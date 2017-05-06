

OCB_FLAGS := -I interfaces

OCB := ocamlbuild

test.native : test.ml
	$(OCB) $(OCB_FLAGS) test.native


.PHONY : clean


clean :
	ocamlbuild -clean
