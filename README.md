# What is this?

This repository contains property-based tests of a subset of the [OCaml](https://ocaml.org) standard library.  The tests use the [crowbar](https://github.com/stedolan/crowbar) framework and are intended for use with the [American Fuzzy Lop](https://lcamtuf.coredump.cx/afl) program fuzzer.

# What are these tests for?

A crash-free test run cannot comprehensively prove that each tested property is strictly true, and these tests are not a substitute for proof or verification work.  However, these tests *can* easily be run against the actual implementations used by OCaml programmers, can be run unattended, and are easily adaptable over a wide range of input.  Violations of a property (reported by afl-fuzz as crashes) are counterexamples which can be used to fix bugs, create regression tests, and generally help developers improve the quality of software.

# How can I run these tests?

Crowbar tests have two modes:

* a simple quickcheck-like mode for testing propositions against totally random input
* a mode using [afl-persistent](https://github.com/stedolan/ocaml-afl-persistent) to get good performance from `afl-fuzz` with OCaml's instrumentation enabled

## Universal Requirements

* use an OCaml compiler with afl instrumentation enabled by default (any compiler with `+afl` from `opam`), for convenience.
* install [crowbar](https://github.com/stedolan/crowbar).  The package has not yet been released in `opam-repository` but does have an `opam` file, so pinning it should result in a successful installation (e.g. `opam pin add crowbar https://github.com/stedolan/crowbar`).

### Building tests

```
$ cd ocaml-test-stdlib/
$ cd test
$ make alltests
ocamlfind ocamlopt -package crowbar \
  -package fmt -package fmt \
  -linkpkg \
  shims.ml test_maps.ml test_sets.ml -o alltests
```

## fully random test mode

If you wish to use the quickcheck-like, fully random mode to run all tests distributed here, build the tests as above and then run the `alltests` binary with no arguments.

```
$ ./alltests | head -5
max_binding = min_binding implies all elements are equal: PASS

removing a key that isn't bound preserves physical equality: PASS

filtering which keeps all elements retains physical equality: PASS
```

### AFL mode requirements

To run the tests in AFL mode, you'll need to install American Fuzzy Lop ([latest source tarball](http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz), although your distribution may also have a package available).

Once `afl-fuzz` is available on your system, create an `input` directory with a non-empty file in it (or use `tests/input`, conveniently provided in this repository), and an `output` directory for `afl-fuzz` to store its findings:

```
afl-fuzz -i input -o output ./alltests @@
```

This will launch AFL, which will generate new test cases and track the exploration of the state space.  When inputs are discovered which cause a property not to hold, they will be reported as crashes (along with actual crashes, although in the OCaml standard library these are rare).  See the [afl-fuzz documentation](https://lcamtuf.coredump.cx/afl/status_screen.txt) for more on AFL's excellent interface.

### Whales

A Dockerfile is included for the convenience of users who enjoy running `docker` commands.  It uses the experimental, unreleased `bun` project to launch and manage the appropriate number of afl-fuzz invocations.

# Have you found any bugs?

Not yet!
