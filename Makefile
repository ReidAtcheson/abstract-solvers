

OCB_FLAGS := -I interfaces -I examples

OCB := ocamlbuild

test.native : test.ml
	$(OCB) $(OCB_FLAGS) test.native


.PHONY : clean


clean :
	ocamlbuild -clean
