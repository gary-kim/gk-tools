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
  core
  core_unix
  ppx_jane
  async
  async_ssl
  cohttp
  cohttp-async
  re
  uri
  sexp_macro
  xdg
  patdiff
  file_path
  filesystem
  jsonaf
  ppx_jsonaf_conv
  async_interactive
  (ocamlformat :with-dev-setup)))
