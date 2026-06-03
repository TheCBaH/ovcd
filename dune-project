(lang dune 3.0)
(name ovcd)
(using menhir 2.1)
(generate_opam_files false)

(source
 (github TheCBaH/ovcd))

(authors "Vladimir Silyaev")

(maintainers "Vladimir Silyaev <thecbah@gmail.com>")

(license ISC)

(documentation https://github.com/TheCBaH/ovcd)

(package
 (name ovcd)
 (version 0.1)
 (synopsis "VCD waveform file parser")
 (description
  "A library and tools for parsing Value Change Dump (VCD) waveform files. Provides a streaming parser, a resolver for hierarchical signal names, and the vcd_dump and vcd_tool command-line utilities.")
 (depends
  (ocaml (>= 4.14))
  (menhir (>= 20211230))
  (menhirLib (>= 20211230))
  re
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  (vcd waveform parser simulation eda)))
