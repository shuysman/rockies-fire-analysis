# Makefile
#
# Builds pdf and docx output

SOURCE_DOC = report.tex

EXPORTED_DOCS=\
 $(SOURCE_DOC:.tex=.html) \
 $(SOURCE_DOC:.tex=.docx) \
 $(SOURCE_DOC:.tex=.pdf)

RM=/bin/rm

PANDOC=/usr/bin/pandoc

PANDOC_OPTIONS= --filter pandoc-crossref --bibliography=/home/steve/OneDrive/org/library.bib --citeproc --metadata link-citations=true --standalone

PANDOC_DOCX_OPTIONS=

PDFLATEX=/usr/bin/pdflatex
BIBER=/usr/bin/biber

%.docx : %.tex
	$(PANDOC) $(PANDOC_OPTIONS) $(PANDOC_DOCX_OPTIONS) -o $@ $<

%.html : %.tex
	$(PANDOC) $(PANDOC_OPTIONS) -o $@ $<

%.pdf : %.tex
	$(PDFLATEX) $<
	$(BIBER) report
	$(PDFLATEX) $<
	$(PDFLATEX) $<
	latexmk -c

# Targets and dependencies

.PHONY: all clean

all : $(EXPORTED_DOCS)

clean:
	- $(RM) $(EXPORTED_DOCS)
	- latexmk -c
