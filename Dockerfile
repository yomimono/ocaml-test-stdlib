FROM yomimono/bun:2d36a58a445c493c4b47a2a65b07c2f1327a5fdc as bun

FROM yomimono/afl:2.52b as afl

FROM yomimono/opam:debian-9_ocaml-4.06.0_afl as opam
RUN opam config exec -- opam install -y fmt
RUN opam config exec -- opam pin add -y crowbar https://github.com/yomimono/crowbar.git#stack-overflow

COPY --from=bun /home/opam/bun /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-fuzz /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-gotcpu /usr/local/bin/
COPY --from=afl /usr/local/bin/afl-whatsup /usr/local/bin/

ADD . /home/opam/ocaml-test-stdlib
RUN sudo chown -R opam /home/opam/ocaml-test-stdlib
WORKDIR /home/opam/ocaml-test-stdlib
ENTRYPOINT opam config exec -- timeout --preserve-status 25m jbuilder build @test/fuzz --no-buffer
