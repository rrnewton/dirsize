

all: dirtree dirsize 
	chmod o+r *

dirtree: dirtree.ml dirtree.mli
	ocamlc -c rutils.cma unix.cma dirtree.mli dirtree.ml
	ocamlc -a -o dirtree.cma dirtree.cmo
	ocamlopt -c rutils.cmxa unix.cmxa dirtree.mli dirtree.ml
	ocamlopt -a -o dirtree.cmxa dirtree.cmx

dirsize: dirtree dirsize.ml 
#	ocamlopt -c dirsize.ml
	ocamlopt -o dirsize unix.cmxa rutils.cmxa dirtree.cmxa dirsize.ml 

debug: 	dirtree dirsize.ml
	ocamlc -g -custom -ccopt -g -o debug unix.cma rutils.cma dirtree.cma dirsize.ml 

exec:
	ocamlopt unix.cmxa rutils.cmxa dirtree.mli dirtree.ml

top:
	ocamlmktop unix.cma rutils.cma ./dirtree.cma

install:
	cp dirtree.a dirtree.cmxa dirtree.cma `ocamlc -where`
	cp dirtree.cmi dirtree.mli `ocamlc -where`
	dirname `which ocamlc` | cp dirsize -

cleanup: 
	rm -f *.cmi *.cmx *.cmo *.o *~ a.out

clean: cleanup
	rm -f *.cmxa *.cma *.a dirsize debug