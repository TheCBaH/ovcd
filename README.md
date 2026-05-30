# OCaml VCD Parser

[![build](https://github.com/TheCBaH/ocaml-devcontainer/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/TheCBaH/ocaml-devcontainer/actions/workflows/build.yml)

Streaming [VCD (Value Change Dump)](https://en.wikipedia.org/wiki/Value_change_dump) parser for OCaml.
The header is parsed with Menhir; simulation events are streamed as a lazy `Seq.t` via ocamllex.

## Get started

```
make        # build
make test   # run tests
make bench  # benchmark on large real-world VCD files (default 10 runs each)
```

Open in GitHub Codespaces: [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=628173356)
