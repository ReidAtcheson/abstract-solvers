

OCB_FLAGS := -I interfaces -I examples -I tests

OCB := ocamlbuild


all_tests.native : all_tests.ml
	$(OCB) $(OCB_FLAGS) all_tests.native

.PHONY : clean


clean :
	ocamlbuild -clean
