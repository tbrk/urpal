# $Id$

RUNTEST=./runtest.sh

all:
	for f in test???.xml; do $(RUNTEST) $$f; done

clean:
	-@rm test???-flip.xml test???.log test???-expected.log test???.diff

