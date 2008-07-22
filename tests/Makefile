# $Id$

RUNTEST=./runtest.sh

all:
	@for f in test???.xml; do \
	    $(RUNTEST) $$f; \
	    if [ $$? -eq 2 ]; then exit; fi; \
	done

clean:
	-@rm test???-flip.xml test???.log test???-expected.log test???.diff \
	     test???-uppaal.log

