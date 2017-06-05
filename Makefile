SCRIPTNAME = conwiki2slide
PATHBIN = ../../bin/

build:
	mkdir -p extraction
	$(MAKE) -f Makefile.coq
	cd extraction && ocamlbuild main.native -use-ocamlfind -package io-system

init:
	coq_makefile Main.v -o Makefile.coq

clean:
	cd extraction && ocamlbuild -clean
	cd extraction && $(RM) main.ml main.mli
	ls Main* | $(RM) `grep -v "^Main.v$$"`
	$(RM) .Main.aux

install:
	cp -av extraction/main.native $(PATHBIN)$(SCRIPTNAME) 
