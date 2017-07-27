

CFLAGS := -O3

libfd.a : fd.o
	ar rcs libfd.a fd.o

fd.o : fd.c fd.h
	$(CC) $(CFLAGS) -o fd.o -c fd.c



.PHONY : clean


clean :
	rm -rf ./libfd.a
	rm -rf ./fd.o
