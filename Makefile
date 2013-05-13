OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

example:
	$(OCAMLBUILD) example.$(TARGET)


clean:
	rm -rf *\~
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
