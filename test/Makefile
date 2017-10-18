TESTS = maps_base sets_base
COMPILER_406 = maps

BASE_PACKAGES = crowbar,fmt

.PHONY: all clean
all: $(TESTS) $(COMPILER_406)
clean:
	rm -f $(TESTS) alltests

$(COMPILER_406): %: shims.ml test_%_base.ml test_%_406.ml
	ocamlfind ocamlopt -w +A-4-41-42-44-45 -package $(BASE_PACKAGES) $(PKG_$*) -linkpkg $^ -o $@

$(TESTS): %: shims.ml test_%.ml
	ocamlfind ocamlopt -w +A-4-41-42-44-45 -package $(BASE_PACKAGES) $(PKG_$*) -linkpkg $^ -o $@

ALL_PKG = $(foreach v,$(TESTS),$(PKG_$(v)))
alltests: shims.ml $(TESTS:%=test_%.ml) $(COMPILER_406:%=test_%_406.ml)
	ocamlfind ocamlopt -package $(BASE_PACKAGES) \
	  $(ALL_PKG:%=-package %) \
	  -linkpkg \
	  $^ -o $@
406tests: alltests
basetests: shims.ml $(TESTS:%=test_%.ml)
	ocamlfind ocamlopt -package $(BASE_PACKAGES) \
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
