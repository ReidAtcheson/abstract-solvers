


main.native : main.ml
	ocamlbuild main.native



.PHONY : clean



clean :
	ocamlbuild -clean
