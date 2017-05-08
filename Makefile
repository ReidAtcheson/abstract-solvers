

PWD := $(shell pwd)

TPATH := $(PWD)/examples/C/libfd.a
OCB_FLAGS := -I interfaces -I examples -I tests -pkg core -pkg ctypes.foreign -lflags "-cclib -force_load $(TPATH)"

OCB := ocamlbuild


all_tests.native : all_tests.ml libfd.a
	$(OCB) $(OCB_FLAGS) all_tests.native

libfd.a : 
	make -C examples/C/


.PHONY : clean


clean :
	ocamlbuild -clean
	make clean -C examples/C/
