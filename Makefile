OC=ocamlopt
LIBS=-thread unix.cmxa threads.cmxa

example: example.exe

.PHONY: example

%.exe: %.ml skahn/kahn.ml pkahn/kahn.ml nkahn/kahn.ml tkahn/i.cmo
	$(OC) $(LIBS) -o $@.n -I tkahn -I nkahn\
 tkahn/i.ml nkahn/server.ml nkahn/client.ml nkahn/kahn.ml $<
	$(OC) $(LIBS) -o $@.s -I tkahn -I skahn\
 tkahn/i.ml skahn/kahn.ml $<
	$(OC) $(LIBS) -o $@.p -I tkahn -I pkahn\
 tkahn/i.ml pkahn/kahn.ml $<

%.cmo: %.ml
	$(OC) -c -o $@ $(LIBS) $<

report: report/report.tex
	pdflatex report/report.tex

archive:
	tar -czf cordero-xia.tgz Makefile example.ml

clean:
	rm -f *.cm[xoi] */*.cm[xoi] */*.o

realclean: clean
	rm -f *~ */*~ *.exe

cleanall: realclean
