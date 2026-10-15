# Writing a C Compiler

A compiler for a subset of C, based on Nora Sandler's book [Writing a C Compiler](https://norasandler.com/book/).

I'm also using this project as a chance to play around with [jujutsu](https://docs.jj-vcs.dev/latest/).
Clone the repo with `jj git clone`, and clone [Nora Sandler's test suite](https://github.com/nlsandler/writing-a-c-compiler-tests/), since `jj` doesn't support submodules yet...

## Build

Ensure that [OxCaml](https://oxcaml.org/get-oxcaml/) is set up as the active `opam switch`.
The following libraries must be installed (through `opam`):

- `core >= v0.18~preview.130.106+341`
- `core_unix >= v0.18~preview.130.106+341`
- `ppx_jane >= v0.18~preview.130.106+341`
- `vec >= v0.18~preview.130.106+341`

Then, in a terminal, execute:

```shell
$ dune build @default
```

The compiled binary can be found at `./_build/default/bin/cc.exe`.

## Test

To run Sandler's test suite, clone her test suite and follow the instructions there.
