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
  (oxcaml-compiler
   (>= 5.4.0-ox1))
  dune
  core
  core_unix
  ppx_jane
  async
  async_ssl
  base64
  cohttp
  cohttp-async
  posixat
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
  email_message
  (ocamlformat :with-dev-setup)))
