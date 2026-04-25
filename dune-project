(lang dune 3.15)

(name gk-tools)

(cram enable)

(generate_opam_files true)

(authors "Gary Kim <gary@garykim.dev>")

(maintainers "Gary Kim <gary@garykim.dev>")

(package
 (name gk-tools)
 (synopsis "Personal tools binary")
 (description
  "General purpose tools binary for system administration and automation")
 (depends
  (ocaml-variants
   (= 5.2.0+ox))
  dune
  (core
   (>= 0.17.0))
  (core_unix
   (>= 0.17.0))
  (ppx_jane
   (>= 0.17.0))
  (async
   (>= 0.17.0))
  (cohttp
   (>= 6.0.0))
  (cohttp-async
   (>= 6.0.0))
  (uri
   (>= 4.0.0))
  (sexp_macro
   (>= 0.17.0))
  (xdg
   (>= 3.16.0))
  (patdiff
   (>= 0.17.0))
  (file_path
   (>= 0.17.0))
  (filesystem
   (>= 0.17.0))
  ocamlformat
  dune-build-info))
