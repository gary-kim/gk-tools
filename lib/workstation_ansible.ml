open! Core
open! Async
open Deferred.Or_error.Let_syntax

module Config = struct
  module Url = struct
    type t = Uri.t

    let t_of_sexp sexp = Uri.of_string ([%of_sexp: string] sexp)
    let sexp_of_t t = [%sexp_of: string] (Uri.to_string t)
  end

  type t =
    { url : Url.t
    ; keyring : File_path.t
         [@default File_path.of_string "/etc/gk-workstation-pull/trusted.gpg"]
    ; state_dir : File_path.t
         [@default File_path.of_string "/var/lib/gk-workstation-pull"]
    }
  [@@deriving sexp]

  let default_path = File_path.of_string "/etc/gk-workstation-ansible.sexp"

  let load path =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Filesystem_async.load_as_sexp path ~of_sexp:[%of_sexp: t])
    |> Deferred.Or_error.tag_s ~tag:[%message "cannot load config" (path : File_path.t)]
  ;;
end

module Version = struct
  type t = int [@@deriving compare, sexp_of]

  include functor Comparable.Make_plain

  let of_string str = Int.of_string (String.strip str)
  let to_string = Int.to_string
end

module Trusted_path = struct
  let check_one path ~(kind : Filesystem_types_unix.File_kind.t) =
    let%bind (stats : Filesystem_types_unix.File_stats.t) =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Filesystem_async.lstat (File_path.of_absolute path))
    in
    let euid = Core_unix.geteuid () in
    let group_or_other_writable =
      Filesystem_types_unix.File_permissions.(
        do_intersect stats.permissions (g_w lor o_w))
    in
    if Filesystem_types_unix.File_kind.equal stats.kind kind
       && (stats.user_id = 0 || stats.user_id = euid)
       && not group_or_other_writable
    then return ()
    else
      Deferred.Or_error.error_s
        [%message
          "path must be owned by root or this user and not writable by group or others"
            (path : File_path.Absolute.t)
            ~expected:(kind : Filesystem_types_unix.File_kind.t)
            ~kind:(stats.kind : Filesystem_types_unix.File_kind.t)
            ~user_id:(stats.user_id : int)
            (euid : int)
            ~permissions:(stats.permissions : Filesystem_types_unix.File_permissions.t)]
  ;;

  let ancestors path =
    Sequence.unfold ~init:path ~f:(fun path ->
      File_path.Absolute.dirname path |> Option.map ~f:(fun parent -> parent, parent))
    |> Sequence.to_list
  ;;

  let check path ~kind =
    let%bind resolved =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Filesystem_async.realpath_relative_to_cwd path)
    in
    let%bind () = check_one resolved ~kind in
    let%map () =
      Deferred.Or_error.List.iter
        (ancestors resolved)
        ~how:`Sequential
        ~f:(check_one ~kind:Directory)
    in
    resolved
  ;;
end

module Fetch = struct
  let headers = Cohttp.Header.init_with "User-Agent" "gk-workstation-pull"
  let timeout = Time_ns.Span.of_min 5.

  let ssl_config uri =
    let hostname = Uri.host uri in
    let verify connection =
      Option.value_map hostname ~default:false ~f:(fun hostname ->
        Async_ssl.Ssl.Connection.check_peer_certificate_host connection hostname
        |> Or_error.is_ok)
      |> Deferred.return
    in
    Conduit_async.V2.Ssl.Config.create ?hostname ~verify ()
  ;;

  let get uri =
    let%bind response, body =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Cohttp_async.Client.get ~ssl_config:(ssl_config uri) ~headers uri)
    in
    match Cohttp.Response.status response with
    | `OK ->
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Cohttp_async.Body.to_string body)
    | status ->
      let status = Cohttp.Code.string_of_status status in
      Deferred.Or_error.error_s [%message "unexpected HTTP status" (status : string)]
  ;;

  let http uri =
    match%bind.Deferred Clock_ns.with_timeout timeout (get uri) with
    | `Result result -> Deferred.return result
    | `Timeout ->
      Deferred.Or_error.error_s [%message "timed out" (timeout : Time_ns.Span.t)]
  ;;

  let file uri =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Filesystem_async.read_file (File_path.of_string (Uri.pct_decode (Uri.path uri))))
  ;;

  let fetch uri =
    (match Uri.scheme uri with
     | Some ("https" | "http") -> http uri
     | Some "file" -> file uri
     | scheme ->
       Deferred.Or_error.error_s
         [%message "unsupported URL scheme" (scheme : string option)])
    |> Deferred.Or_error.tag_s
         ~tag:[%message "fetch failed" ~url:(Uri.to_string uri : string)]
  ;;
end

module Gpgv = struct
  let verify ~keyring ~signature ~data =
    let%bind keyring = Trusted_path.check keyring ~kind:Regular in
    Process.run
      ~prog:"gpgv"
      ~args:
        [ "--keyring"
        ; File_path.Absolute.to_string keyring
        ; File_path.to_string signature
        ; File_path.to_string data
        ]
      ()
    |> Deferred.Or_error.ignore_m
    |> Deferred.Or_error.tag_s
         ~tag:
           [%message
             "signature verification failed"
               (signature : File_path.t)
               (data : File_path.t)]
  ;;
end

module Tarball = struct
  let extract tarball ~into =
    let%bind () =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Filesystem_async.mkdir ~parents:true into)
    in
    Process.run
      ~prog:"tar"
      ~args:
        [ "-xzf"
        ; File_path.to_string tarball
        ; "--strip-components=1"
        ; "-C"
        ; File_path.to_string into
        ]
      ()
    |> Deferred.Or_error.ignore_m
  ;;
end

module Playbook = struct
  let run ~tree =
    let%bind tree =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Filesystem_async.make_absolute_under_cwd tree)
    in
    let ansible_config =
      File_path.Absolute.append_part tree (File_path.Part.of_string "ansible.cfg")
    in
    let host = Core_unix.gethostname () in
    let short = String.lsplit2 host ~on:'.' |> Option.value_map ~default:host ~f:fst in
    (* Not [`Share]: Async's stdio is non-blocking, and ansible refuses to start on it. *)
    Process.run_forwarding
      ~working_dir:(File_path.Absolute.to_string tree)
      ~env:(`Extend [ "ANSIBLE_CONFIG", File_path.Absolute.to_string ansible_config ])
      ~prog:"ansible-playbook"
      ~args:
        [ "-c"
        ; "local"
        ; "-l"
        ; [%string "localhost,%{host},%{short},127.0.0.1"]
        ; "-i"
        ; "inventory.yaml"
        ; "playbook.yaml"
        ]
      ()
  ;;
end

module State = struct
  type t = { dir : File_path.t }

  let create ~dir = { dir }
  let try_with f = Deferred.Or_error.try_with ~extract_exn:true f
  let lock_entry = File_path.Part.of_string "lock"
  let current_entry = File_path.Part.of_string "current"
  let incoming_entry = File_path.Part.of_string "incoming"
  let lock t = File_path.append_part t.dir lock_entry
  let current t = File_path.append_part t.dir current_entry
  let current_tmp t = File_path.append_to_basename_exn (current t) ".tmp"
  let incoming t = File_path.append_part t.dir incoming_entry

  let in_incoming t name =
    File_path.append_part (incoming t) (File_path.Part.of_string name)
  ;;

  let incoming_tarball t = in_incoming t "gk-workstation-ansible.tar.gz"
  let incoming_signature t = File_path.append_to_basename_exn (incoming_tarball t) ".sig"
  let incoming_tree t = in_incoming t "tree"
  let version_part version = File_path.Part.of_string (Version.to_string version)

  let with_lock t ~f =
    let%bind () = try_with (fun () -> Filesystem_async.mkdir ~parents:true t.dir) in
    let%bind (_ : File_path.Absolute.t) = Trusted_path.check t.dir ~kind:Directory in
    let lock = lock t in
    match%bind try_with (fun () -> Filesystem_async.try_flock_create lock) with
    | None ->
      Deferred.Or_error.error_s
        [%message "another run holds the lock" (lock : File_path.t)]
    | Some flock -> Monitor.protect f ~finally:(fun () -> Filesystem_async.funlock flock)
  ;;

  let reset_incoming t =
    try_with (fun () ->
      let%bind.Deferred () = Filesystem_async.rm ~recursive:true (incoming t) in
      Filesystem_async.mkdir (incoming t))
  ;;

  let version_of_tree tree =
    let path = File_path.append_part tree (File_path.Part.of_string "VERSION") in
    try_with (fun () ->
      let%map.Deferred contents = Filesystem_async.read_file path in
      Version.of_string contents)
    |> Deferred.Or_error.tag_s ~tag:[%message "cannot read version" (path : File_path.t)]
  ;;

  let incoming_version t = version_of_tree (incoming_tree t)

  let current_exists t =
    try_with (fun () -> Filesystem_async.exists_exn ~follow_symlinks:false (current t))
  ;;

  let applied_version t =
    match%bind current_exists t with
    | false -> return None
    | true ->
      let%map version = version_of_tree (current t) in
      Some version
  ;;

  let current_target t =
    match%bind current_exists t with
    | false -> return None
    | true ->
      let%bind target = try_with (fun () -> Filesystem_async.readlink (current t)) in
      File_path.basename_or_error target |> Or_error.map ~f:Option.some |> Deferred.return
  ;;

  let commit t version =
    let part = version_part version in
    let dest = File_path.append_part t.dir part in
    try_with (fun () ->
      let%bind.Deferred () = Filesystem_async.rm ~recursive:true dest in
      let%bind.Deferred () = Filesystem_async.rename ~src:(incoming_tree t) ~dst:dest in
      let%bind.Deferred () = Filesystem_async.rm (current_tmp t) in
      let%bind.Deferred () =
        Filesystem_async.symlink
          (current_tmp t)
          ~referring_to:(File_path.of_part_relative part)
      in
      Filesystem_async.rename ~src:(current_tmp t) ~dst:(current t))
  ;;

  let prune t =
    let%bind target = current_target t in
    let keep = Option.to_list target @ [ lock_entry; current_entry; incoming_entry ] in
    let%bind entries = try_with (fun () -> Filesystem_async.ls_dir t.dir) in
    List.filter entries ~f:(fun entry ->
      not (List.mem keep entry ~equal:File_path.Part.equal))
    |> Deferred.Or_error.List.iter ~how:`Sequential ~f:(fun entry ->
      try_with (fun () ->
        Filesystem_async.rm ~recursive:true (File_path.append_part t.dir entry)))
  ;;
end

let signature_url url = Uri.with_path url (Uri.path url ^ ".sig")

let write path ~contents =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    Filesystem_async.write_file path ~contents)
;;

let fetch_and_verify state ~url ~keyring =
  let tarball = State.incoming_tarball state in
  let signature = State.incoming_signature state in
  let%bind tarball_contents = Fetch.fetch url
  and signature_contents = Fetch.fetch (signature_url url) in
  let%bind () = write tarball ~contents:tarball_contents
  and () = write signature ~contents:signature_contents in
  [%log.info "fetched" ~url:(Uri.to_string url : string)];
  let%map () = Gpgv.verify ~keyring ~signature ~data:tarball in
  [%log.info "verified"];
  tarball
;;

let apply state version =
  [%log.info "applying" (version : Version.t)];
  let%bind () = Playbook.run ~tree:(State.incoming_tree state) in
  let%map () = State.commit state version in
  [%log.info "applied" (version : Version.t)]
;;

let run ~config_path =
  let%bind ({ url; keyring; state_dir } : Config.t) = Config.load config_path in
  let state = State.create ~dir:state_dir in
  State.with_lock state ~f:(fun () ->
    let%bind () = State.reset_incoming state in
    let%bind tarball = fetch_and_verify state ~url ~keyring in
    let%bind () = Tarball.extract tarball ~into:(State.incoming_tree state) in
    let%bind version = State.incoming_version state
    and applied = State.applied_version state in
    let%bind.Deferred outcome =
      match applied with
      | Some applied when Version.(version < applied) ->
        Deferred.Or_error.error_s
          [%message "refusing to roll back" (applied : Version.t) (version : Version.t)]
      | Some applied when Version.equal version applied ->
        [%log.info "already applied" (version : Version.t)];
        return ()
      | Some _ | None -> apply state version
    in
    Deferred.Or_error.combine_errors_unit [ Deferred.return outcome; State.prune state ])
;;

let pull_command =
  Command.async_or_error
    ~extract_exn:true
    ~summary:"Fetch, verify, and apply the gk-workstation-ansible tarball"
    (let%map_open.Command config_path =
       flag
         "config"
         (optional_with_default Config.default_path File_path.arg_type)
         ~doc:[%string "PATH config file (default: %{Config.default_path#File_path})"]
     and () = Log.Global.set_level_via_param () in
     fun () -> run ~config_path)
;;

let command =
  Command.group ~summary:"gk-workstation-ansible subcommands" [ "pull", pull_command ]
;;
