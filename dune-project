(lang dune 2.0)

(name tadpole)

(version 1.0.2)
(authors "H11 <nirisix@gmail.com>")
(maintainers "H11 <nirisix@gmail.com>")
(source
 (github dhcmrlchtdj/tadpole))
(license MIT)

(package
 (name tadpole)
 (synopsis "WASM/WAT toolkit")
 (depends
  (containers           (and (>= 2.8)))
  (ppx_deriving         (and :build (>= 4.4)))
  (dune                 (and :build (>= 2.0.0)))
  (js_of_ocaml          (and :dev (>= 3.5.1)))
  (js_of_ocaml-compiler (and :dev (>= 3.5.1)))
  (js_of_ocaml-ppx      (and :dev (>= 3.5.1)))
  (ocamlformat          (and :dev (>= 0.12)))
  ))

(generate_opam_files true)
