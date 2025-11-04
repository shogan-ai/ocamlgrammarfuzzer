(lang dune 3.17)

(name ocamlgrammarfuzzer)

(generate_opam_files true)

(authors "Frédéric Bour <fred@shogan.ai>")
(maintainers "Frédéric Bour <fred@shogan.ai>")

(license MIT)

(package
 (name ocamlgrammarfuzzer)
 (synopsis "A fuzzer for OCamlformat")
 (homepage "https://github.com/shogan-ai/ocamlgrammarfuzzer")
 (bug_reports "https://github.com/shogan-ai/ocamlgrammarfuzzer/issues")
 (documentation "https://github.com/shogan-ai/ocamlgrammarfuzzer/blob/main/README.md")
 (depends ocaml cmon menhir))

(using menhir 3.0)
