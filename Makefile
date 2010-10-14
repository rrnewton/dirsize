

all: rutils graphing dirtree dirsize mods
	chmod o+r *

rutils:
	ocamlc unix.cma -c rutils.mli rutils.ml
	ocamlc  -a -o rutils.cma rutils.cmo
	ocamlopt unix.cmxa -c rutils.mli rutils.ml
	ocamlopt  -a -o rutils.cmxa rutils.cmx

graphing:
	ocamlc unix.cma rutils.cma -c graphs.mli graphs.ml
	ocamlc  -a -o graphs.cma graphs.cmo
	ocamlopt unix.cmxa rutils.cmxa -c graphs.mli graphs.ml
	ocamlopt -a -o graphs.cmxa graphs.cmx


dirtree: dirtree.ml dirtree.mli
	ocamlc -c rutils.cma unix.cma dirtree.mli dirtree.ml
	ocamlc -a -o dirtree.cma dirtree.cmo
	ocamlopt -c rutils.cmxa unix.cmxa dirtree.mli dirtree.ml
	ocamlopt -a -o dirtree.cmxa dirtree.cmx

dirsize: dirtree dirsize.ml 
#	ocamlopt -c dirsize.ml
	ocamlopt -o dirsize unix.cmxa rutils.cmxa dirtree.cmxa dirsize.ml 

mods: dirsize modtimes.ml
#	ocamlopt -c dirsize.ml
	ocamlopt -o mods unix.cmxa graphics.cmxa rutils.cmxa graphs.cmxa dirtree.cmxa modtimes.ml 

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
	dirname `which ocamlc` | xargs cp mods 

cleanup: 
	rm -f *.cmi *.cmx *.cmo *.o *~ a.out

clean: cleanup
	rm -f *.cmxa *.cma *.a dirsize debug mods
