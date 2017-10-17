TESTS = map maps sets
PKG_map =
PKG_maps = fmt
PKG_sets = fmt

.PHONY: all clean
all: $(TESTS)
clean:
	rm -f $(TESTS) alltests

$(TESTS): %: shims.ml test_%.ml
	ocamlfind ocamlopt -package crowbar,$(PKG_$*) -linkpkg $^ -o $@ 

ALL_PKG = $(foreach v,$(TESTS),$(PKG_$(v)))
alltests: shims.ml $(TESTS:%=test_%.ml)
	ocamlfind ocamlopt -package crowbar \
	  $(ALL_PKG:%=-package %) \
	  -linkpkg \
	  $^ -o $@

.gitignore: Makefile
	{ \
	   echo '*.cm*'; \
	   echo '*.o'; \
	   echo output; \
	   for i in $(TESTS) alltests; do echo $$i; done \
	} > $@
