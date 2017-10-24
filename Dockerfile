FROM yomimono/bun:6bcbfc3a6dc003719756b6d96b078061af0de481 as bun

FROM yomimono/afl:2.51b as afl

FROM yomimono/opam:debian-9_ocaml-4.06.0_beta2_afl as opam
RUN opam config exec -- opam install -y fmt
RUN opam config exec -- opam pin add -y crowbar https://github.com/yomimono/crowbar.git#greatest_justice

COPY --from=bun /home/opam/bun /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-fuzz /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-gotcpu /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-whatsup /usr/local/bin/

ADD . /home/opam/ocaml-test-stdlib
RUN sudo chown -R opam /home/opam/ocaml-test-stdlib
WORKDIR /home/opam/ocaml-test-stdlib/test
RUN opam config exec -- make alltests
ENTRYPOINT opam config exec -- timeout --preserve-status 25m bun -i input -o output -v ./alltests
