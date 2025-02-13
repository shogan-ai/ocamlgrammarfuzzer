(lang dune 3.17)

(name ocamlgrammarfuzzer)

(generate_opam_files true)

(authors "Frédéric Bour <frederic.bour@lakaban.net>")

(maintainers "Frédéric Bour <frederic.bour@lakaban.net")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ocamlgrammarfuzzer)
 (synopsis "A fuzzer for OCamlformat")
 (depends ocaml))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
