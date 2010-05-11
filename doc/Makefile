# Makefile for icalendar documentation

.PHONY: all clean html info pdf clean

TEXINFO = makeinfo
FILES=cl-icalendar.texi fdl.texi
RM=rm

all: info

html: cl-icalendar/
info: cl-icalendar.info
pdf: cl-icalendar.pdf

cl-icalendar/: $(FILES)
	$(TEXINFO) --html $<

cl-icalendar.info: $(FILES)
	$(TEXINFO) $<

cl-icalendar.pdf: $(FILES)
	texi2pdf $<

clean:
	-$(RM) -rf cl-icalendar/
	-$(RM) cl-icalendar.info
	-$(RM) cl-icalendar.pdf
	-$(RM) *.aux *.cp *.cpx *.fn *.fns *.ky *.pg *.toc *.tp *.tps *.vr
