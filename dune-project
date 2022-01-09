(lang dune 2.9)
(name ArtiLang)
(using menhir 2.1)

(version 0.0.1)

(license "GPL-3.0")

(authors "The ArtiLang programmers")

(maintainers "lelolartichaut@laposte.net")

(source
 (github ArtichOwO/ArtiLang))

(bug_reports "https://github.com/ArtichOwO/ArtiLang/issues")

(homepage "https://github.com/ArtichOwO/ArtiLang")

(generate_opam_files true)

(package
 (name ArtiLang)
 (synopsis "8086 real mode programming language")
 (description
  "ArtiLang is a 8086 real mode programming language which supports segmentation.")
 (depends
  dune
  (ocaml
   (>= 4.12))
  (menhir
   (>= 20210419))))
