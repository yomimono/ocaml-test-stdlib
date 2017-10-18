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

## What tests are here?

Currently, the Map and Set functors have a nontrivial number of tests; Map has more tests than Set.

These tests use several modules from the standard library as inputs for the Make functors of Map and Set.  Int, String, Char, Nativeint, and Uchar are all potential candidates for Map keys or values, as well as Set elements.  Tests for all of these modules (although not all combinations for Map keys and values) are generated, and have an equal chance of being chosen for execution.

To see the tests themselves, have a look at `test_maps.ml` and `test_sets.ml` in the `tests/` directory.

# Have you found any bugs?

Not in 4.05 or 4.06!  When run against 4.04.0, known problems with the Set module (fixed in 4.04.1) are discovered by these tests.

## How hard are you looking?

As of commit `4018c4`, no bugs in OCaml 4.06.0+beta2 had been found (although a test run is likely in progress this very moment).

To get a better idea of what that means, here's how the test run in progress at the time of this document's writing was faring on a machine with 7 cores available.  Five of the fuzzer processes have determined there is no more interesting work for them to do and closed down, while two still think there are interesting paths to explore:

```
Individual fuzzers
==================

>>> 1 (0 days, 2 hrs) <<<

  cycle 1, lifetime speed 928 execs/sec, path 2241/2910 (77%)
  pending 9/2676, coverage 3.02%, no crashes yet

>>> 2 (0 days, 2 hrs) <<<

  Instance is dead or running remotely, skipping.

>>> 3 (0 days, 2 hrs) <<<

  cycle 37, lifetime speed 4128 execs/sec, path 3191/4313 (73%)
  pending 0/9, coverage 3.02%, no crashes yet

>>> 4 (0 days, 2 hrs) <<<

  Instance is dead or running remotely, skipping.

>>> 5 (0 days, 2 hrs) <<<

  Instance is dead or running remotely, skipping.

>>> 6 (0 days, 2 hrs) <<<

  Instance is dead or running remotely, skipping.

>>> 7 (0 days, 2 hrs) <<<

  Instance is dead or running remotely, skipping.

Summary stats
=============

       Fuzzers alive : 2
      Dead or remote : 5 (excluded from stats)
      Total run time : 0 days, 4 hours
         Total execs : 36 million
    Cumulative speed : 5056 execs/sec
       Pending paths : 9 faves, 2685 total
  Pending per fuzzer : 4 faves, 1342 total (on average)
       Crashes found : 0 locally unique

```
