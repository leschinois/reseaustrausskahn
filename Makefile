OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

example:
	ocamlc -o ex -I tkahn -I skahn -thread unix.cma threads.cma tkahn/i.ml skahn/kahn.ml example.ml
	
## $(OCAMLBUILD) example.$(TARGET)


clean:
	rm -f *.cm[oi]

realclean: clean
	rm -f *~ */*~

cleanall: realclean
