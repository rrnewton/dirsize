

all: dirtree dirsize 


dirtree: dirtree.ml dirtree.mli
	ocamlc -c utils.cma unix.cma dirtree.mli dirtree.ml
	ocamlc -a -o dirtree.cma dirtree.cmo
	ocamlopt -c utils.cmxa unix.cmxa dirtree.mli dirtree.ml
	ocamlopt -a -o dirtree.cmxa dirtree.cmx

dirsize: dirtree dirsize.ml 
#	ocamlopt -c dirsize.ml
	ocamlopt -o dirsize unix.cmxa utils.cmxa dirtree.cmxa dirsize.ml 


exec:
	ocamlopt unix.cmxa utils.cmxa dirtree.mli dirtree.ml

top:
	ocamlmktop unix.cma utils.cma ./dirtree.cma

install:
	cp dirtree.a dirtree.cmxa dirtree.cma `ocamlc -where`
	cp dirtree.cmi dirtree.mli `ocamlc -where`
	which ocamlc | dirname | cp dirsize -

cleanup: 
	rm -f *.cmi *.cmx *.cmo *.o *~ a.out

clean: cleanup
	rm -f *.cmxa *.cma *.a dirsize