(lang dune 2.9)
(name r86)
(using menhir 2.1)

(version 0.1.0)

(license "GPL-3.0")

(authors "The r86 programmers")

(maintainers "lelolartichaut@laposte.net")

(source
 (github ArtichOwO/r86))

(bug_reports "https://github.com/ArtichOwO/r86/issues")

(homepage "https://github.com/ArtichOwO/r86")

(generate_opam_files true)

(package
 (name r86)
 (synopsis "8086 real mode programming language")
 (description
  "r86 is a 8086 real mode programming language which supports segmentation.")
 (depends
  dune
  (ocaml
   (>= 4.12))
  (menhir
   (>= 20210419))))
