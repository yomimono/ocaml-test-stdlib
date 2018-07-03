[![travis CI build status badge](https://travis-ci.org/yomimono/ocaml-test-stdlib.svg?branch=primary)](https://travis-ci.org/yomimono/ocaml-test-stdlib/)

# What is this?

This repository contains property-based tests of a subset of the [Base](https://github.com/janestreet/base) standard library replacement for [OCaml](https://ocaml.org).  The tests use the [crowbar](https://github.com/stedolan/crowbar) framework and are intended for use with the [American Fuzzy Lop](https://lcamtuf.coredump.cx/afl) program fuzzer.

# What are these tests for?

A crash-free test run cannot comprehensively prove that each tested property is strictly true, and these tests are not a substitute for proof or verification work.  However, these tests *can* easily be run against the actual implementations used by OCaml programmers, can be run unattended, and are easily adaptable over a wide range of input.  Violations of a property (reported by afl-fuzz as crashes) are counterexamples which can be used to fix bugs, create regression tests, and generally help developers improve the quality of software.

# How can I build these tests?

```
$ cd ocaml-test-base/
$ jbuilder build test/basetests.exe
```

# How can I run these tests?

Crowbar tests have two modes:

* a simple quickcheck-like mode for testing propositions against totally random input
* a mode using [afl-persistent](https://github.com/stedolan/ocaml-afl-persistent) to get good performance from `afl-fuzz` with OCaml's instrumentation enabled

## Universal Requirements

* use an OCaml compiler with afl instrumentation enabled by default (any compiler with `+afl` from `opam`), for convenience.
* install [crowbar](https://github.com/stedolan/crowbar) and [base](https://github.com/janestreet/base).  Both are available via the `opam` package manager.

## fully random test mode

If you wish to use the quickcheck-like, fully random mode to run all tests distributed here, build the tests as above and then run the binary with no arguments.

```
$ _build/default/test/basetests.exe | head -5
max_binding = min_binding implies all elements are equal: PASS

removing a key that isn't bound preserves physical equality: PASS

filtering which keeps all elements retains physical equality: PASS
```

## AFL mode the easy way

For convenience, there is an alias defined for fuzzing `basetests.exe` in `test/jbuild` using [bun](https://github.com/yomimono/ocaml-bun).  It is recommended to invoke this alias with `--no-buffer`, as otherwise the user gets no feedback while the tests are running (apart from fan noise!):

```
jbuilder build @test/fuzz --no-buffer
```

Please note that when invoked in this way, the tests will attempt to use all available CPU cores.  To run the AFL tests in a more considerate manner, please see the next section.

## AFL by hand

To run the tests in AFL mode, you'll need to install American Fuzzy Lop. It's best to use a version more recent than many distributions make available via their own package managers. You can get AFL directly from the [latest source tarball](http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz) or via `opam`, where the package `afl` is available.

Once `afl-fuzz` is available on your system, create an `input` directory with a non-empty file in it (or use `test/input`, conveniently provided in this repository), and an `output` directory for `afl-fuzz` to store its findings:

```
afl-fuzz -i test/input -o output _build/default/test/basetests.exe @@
```

This will launch AFL, which will generate new test cases and track the exploration of the state space.  When inputs are discovered which cause a property not to hold, they will be reported as crashes (along with actual crashes, although in the OCaml standard library these are rare).  See the [afl-fuzz documentation](https://lcamtuf.coredump.cx/afl/status_screen.txt) for more on AFL's excellent interface.

# What tests are here?

Currently, the Map and Set modules have tests which borrow from the [ocaml-test-stdlib](https://github.com/yomimono/ocaml-test-stdlib) tests for generating those data structures.  These tests use Base's provided `invariants` function to check whether the arbitrary data structure generated using Base's API (driven by AFL and Crowbar) is valid.

These tests use several modules from Base as key/value (for Map) or element (for Set) data types.  Int, Float, String, Char, Nativeint, and Uchar are all potential candidates for Map keys or values, as well as Set elements.  Tests for all of these modules (although not all combinations for Map keys and values) are generated, and have an equal chance of being chosen for execution.

To see the tests themselves, have a look at `test_maps.ml` and `test_sets.ml` in the `tests/` directory.
