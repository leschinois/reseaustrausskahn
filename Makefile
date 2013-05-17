OC=ocamlc
LIBS=-thread unix.cma threads.cma

example: example.exe

%.exe: skahn/kahn.cmo tkahn/i.cmo %.ml
	$(OC) $(LIBS) -o $@ -I skahn -I tkahn \
$^

%.cmo: %.ml
	$(OC) -c -o $@ $(LIBS) $<

report: report/report.tex
	pdflatex report/report.tex

clean:
	rm -f *.cm[xoi] */*.cm[xoi] */*.o

realclean: clean
	rm -f *~ */*~ *.exe

cleanall: realclean
