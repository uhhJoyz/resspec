(lang dune 3.20)

(name resspec)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name resspec)
 (synopsis "A resume generation program.")
 (description "A program which uses a tag-based system to generate one-page resumes based on your existing longer resumes.")
 (depends ocaml str unix minttea)
 (tags
  ("resume generation" "knapsack problem")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
