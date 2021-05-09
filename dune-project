(lang dune 2.8)

(name tadpole)

(version 1.0.3)
(authors "H11 <nirisix@gmail.com>")
(maintainers "H11 <nirisix@gmail.com>")
(source
 (github dhcmrlchtdj/tadpole))
(license MIT)

(package
 (name tadpole)
 (synopsis "WASM/WAT toolkit")
 (depends
  (containers           (and (>= 3.4)))
  (dune                 (and :build (>= 2.8.5)))
  (ppx_deriving         (and :build (>= 5.2.1)))
  (js_of_ocaml          (and :dev (>= 3.9.0)))
  (js_of_ocaml-compiler (and :dev (>= 3.9.0)))
  (js_of_ocaml-ppx      (and :dev (>= 3.9.0)))
  (ocamlformat          (and :dev (>= 0.18.0)))
  ))

(generate_opam_files true)
