(lang dune 3.14)

(name major)
(generate_opam_files true)

(source
 (github kronkaz/major-server))

(authors "krønkaz")
(maintainers "krønkaz")
(documentation https://github.com/kronkaz/major-server/blob/main/documentation.pdf)
(license GPL3)

(package
 (name major)
 (synopsis "Majority judgment voting system")
 (description "A voting server following the majority judgment election method")
 (depends ocaml dune decimal base64 jose sqlite3 dream ounit2)
 (tags
  ("web server" api vote)))
