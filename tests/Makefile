# $Id$

RUNTEST=./runtest.sh

all:
	@for f in test???.xml; do \
	    $(RUNTEST) $$f; \
	    if [ $$? -eq 2 ]; then exit; fi; \
	done

listtests:
	@for f in test???.xml; do \
	    echo $$f; \
	    xsltproc --novalid description.xsl $$f \
	    	| awk -v mode=description -f description.awk \
		| sed 's/^/  /'; \
	done

clean:
	-@rm test???-flip.xml test???.log test???-expected.log test???.diff \
	     test???-uppaal.log

