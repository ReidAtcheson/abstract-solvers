

PWD := $(shell pwd)

TPATH := $(PWD)/examples/C/libfd.a
OCB_FLAGS := -I results -I interfaces -I examples -I tests  -pkg core -pkg ctypes.foreign -lflags "-cclib  -pthread -cclib -force_load $(TPATH)"

OCB := ocamlbuild


all : all_tests.native all_results.native

test : all_tests.native

results : all_results.native
	./all_results.native
	make -C ./results

all_results.native : all_results.ml results/one_cg_result.ml results/abstract_cg_result.ml
	$(OCB) $(OCB_FLAGS) all_results.native

all_tests.native : all_tests.ml libfd.a
	$(OCB) $(OCB_FLAGS) all_tests.native

libfd.a : 
	make -C examples/C/


.PHONY : clean


clean :
	ocamlbuild -clean
	make clean -C ./examples/C/
	make clean -C ./results
