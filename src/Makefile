# $Id$
#
URPAL_VERSION=0.9.0
HEAP_SUFFIX=x86-bsd

MLYACC=mlyacc
MLLEX=mllex
MLTON=mlton
HEAP2EXEC=heap2exec

SKIPCM=-type d -name '.cm' -prune -type f -o
SRC!=find . $(SKIPCM) -name '*sml' -o -name '*sig'
SRCDIRS=. ./cmdlang ./config ./graphviz ./graphviz ./lib \
	./maketest ./parse ./xml

urpal: $(SRC) version.sml
	ml-build sources.cm Urpal.main urpal
	$(HEAP2EXEC) urpal.$(HEAP_SUFFIX) urpal
	@rm urpal.$(HEAP_SUFFIX)

withmlton: cmdlang/cmdlang.grm.sml cmdlang/cmdlang.lex.sml \
	   parse/uppaal.grm.sml parse/uppaal.lex.sml \
	   config/config.lex.sml version.sml
	$(MLTON) -verbose 1  urpal.mlb

cmdlang/cmdlang.grm.sml: cmdlang/cmdlang.grm
	$(MLYACC) cmdlang/cmdlang.grm

cmdlang/cmdlang.lex.sml: cmdlang/cmdlang.lex
	$(MLLEX)  cmdlang/cmdlang.lex

config/config.lex.sml: config/config.lex
	$(MLLEX)  config/config.lex

parse/uppaal.grm.sml: parse/uppaal.grm
	$(MLYACC) parse/uppaal.grm

parse/uppaal.lex.sml: parse/uppaal.lex
	$(MLLEX)  parse/uppaal.lex

version.sml:
	(echo "structure Version = struct ";   \
	 echo "  val version=\"$(URPAL_VERSION)\";"; \
	 echo "  val svnversion=\"`svnversion`\";";  \
	 echo "end") > version.sml

tags: $(SRC)
	exctags -R --exclude=.cm --exclude=*.grm.sml *

clean:
	-@find . \( $(SKIPCM) -name *.grm.sml -o -name *.lex.sml \) \
	 	-print -exec rm {} \;
	-@for f in $(SRCDIRS); do rm -r $$f/.cm; done

clobber: clean
	-@rm version.sml urpal
	-@rm tags

