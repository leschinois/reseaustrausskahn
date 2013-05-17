OC=ocamlc
LIBS=-thread unix.cma threads.cma

example: example.exe

%.exe: %.ml skahn/kahn.ml pkahn/kahn.ml tkahn/i.cmo
	$(OC) $(LIBS) -o $@ -I tkahn -I skahn\
 tkahn/i.ml skahn/kahn.ml $<
	$(OC) $(LIBS) -o $@ -I tkahn -I pkahn\
 tkahn/i.ml pkahn/kahn.ml $<

%.cmo: %.ml
	$(OC) -c -o $@ $(LIBS) $<

report: report/report.tex
	pdflatex report/report.tex

clean:
	rm -f *.cm[xoi] */*.cm[xoi] */*.o

realclean: clean
	rm -f *~ */*~ *.exe

cleanall: realclean
