FROM yomimono/bun:0cdbbd1fd0926a816e45b13eb692b517fdf5717f as bun

FROM yomimono/afl:2.52b as afl

FROM yomimono/opam:debian-9_ocaml-4.06.0_afl as opam
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
