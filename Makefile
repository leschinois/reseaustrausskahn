OC=ocamlc
LIBS=-thread unix.cma threads.cma
SFILE=-I skahn skahn/kahn.ml
TFILE=-I tkahn tkahn/kahn.ml
PFILE=-I pkahn pkahn/kahn.ml
NFILE=-I nkahn nkahn/server.ml nkahn/client.ml nkahn/kahn.ml
FILE=$(NFILE)

example: example.socket

.PHONY: example

cc:
	$(OC) -o $@ $(LIBS) $(FILE)

%.seq: %.ml
	FILE="$(SFILE)"
	$(OC) -o $@ $(LIBS) $(FILE) i.ml $<

%.th: %.ml
	FILE="$(TFILE)"
	$(OC) -o $@ $(LIBS) $(FILE) i.ml $<

%.pipe: %.ml
	FILE="$(PFILE)"
	$(OC) -o $@ $(LIBS) $(FILE) i.ml $<

%.socket: %.ml
	FILE="$(NFILE)"
	$(OC) -o $@ $(LIBS) $(FILE) i.ml $<

report: report/report.pdf

report.pdf: report/report.tex
	pdflatex report/report.tex

archive: report.pdf
	tar -czf cordero-xia.tgz Makefile example.ml eratosthenes.ml \
	nkahn skahn pkahn tkahn report.pdf demo

clean:
	rm -f *.cm[xoi] */*.cm[xoi] */*.o *.o */*.cmxa */*.a *.aux *.log 

realclean: clean
	rm -f *~ */*~ *.exe

cleanall: realclean
