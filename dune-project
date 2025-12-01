(lang dune 3.17)
(using menhir 3.0)

(name rstt)

(version 0.1.0)

(generate_opam_files true)

(authors
 "Mickael Laurent"
)

(maintainers
 "Mickael Laurent"
)

(homepage "https://github.com/E-Sh4rk/rstt")

(bug_reports "https://github.com/E-Sh4rk/rstt")

(license "MIT")

(package
 (name rstt)
 (synopsis "R Set-Theoretic Types library")
 (depends (ocaml (>= 5.3.0)) sstt dune zarith)
)

(package
 (name rstt-repl)
 (synopsis "R Set-Theoretic Types REPL library")
 (depends rstt menhir)
)

(package
 (name rstt-bin)
 (synopsis "R Set-Theoretic Types executable")
 (depends rstt-repl yojson)
)
