# $Id$
#
URPAL_VERSION=0.9.0
HEAP_SUFFIX=x86-bsd
MLTONOPT=-loop-passes 1

MLYACC=mlyacc
MLLEX=mllex
MLTON=mlton
HEAP2EXEC=heap2exec
STRIP=strip

SKIPCM=-type d -name '.cm' -prune -type f -o
SRC!=find . $(SKIPCM) -name '*sml' -o -name '*sig'
SRCDIRS=. ./cmdlang ./config ./graphviz ./graphviz ./lib \
	./maketest ./uppaal ./xml ./mcs51 ./layout

urpal: $(SRC) version.sml
	ml-build sources.cm Urpal.main urpal
	$(HEAP2EXEC) urpal.$(HEAP_SUFFIX) urpal
	@rm urpal.$(HEAP_SUFFIX)

withmlton: cmdlang/cmdlang.grm.sml cmdlang/cmdlang.lex.sml \
	   uppaal/uppaal.grm.sml uppaal/uppaal.lex.sml \
	   mcs51/mcs51.grm.sml mcs51/mcs51.lex.sml \
	   config/config.lex.sml version.sml
	$(MLTON) $(MLTONOPT) -mlb-path-map unix-path-map -verbose 1  urpal.mlb
	$(STRIP) urpal

mingw32: cmdlang/cmdlang.grm.sml cmdlang/cmdlang.lex.sml \
	 uppaal/uppaal.grm.sml uppaal/uppaal.lex.sml \
	 mcs51/mcs51.grm.sml mcs51/mcs51.lex.sml \
	 config/config.lex.sml version.sml
	$(MLTON) -target mingw32 -cc mingw32-gcc \
		 $(MLTONOPT) -mlb-path-map unix-path-map -verbose 1 \
		 urpal.mlb
	mv urpal urpal.exe
	mingw32-strip urpal.exe

winmlton: cmdlang/cmdlang.grm.sml cmdlang/cmdlang.lex.sml \
	  uppaal/uppaal.grm.sml uppaal/uppaal.lex.sml \
	  mcs51/mcs51.grm.sml mcs51/mcs51.lex.sml \
	  config/config.lex.sml version.sml
	$(MLTON) -mlb-path-map win-path-map -verbose 1 urpal.mlb


cmdlang/cmdlang.grm.sml: cmdlang/cmdlang.grm
	$(MLYACC) cmdlang/cmdlang.grm

cmdlang/cmdlang.lex.sml: cmdlang/cmdlang.lex
	$(MLLEX)  cmdlang/cmdlang.lex

config/config.lex.sml: config/config.lex
	$(MLLEX)  config/config.lex

uppaal/uppaal.grm.sml: uppaal/uppaal.grm
	$(MLYACC) uppaal/uppaal.grm

uppaal/uppaal.lex.sml: uppaal/uppaal.lex
	$(MLLEX)  uppaal/uppaal.lex

mcs51/mcs51.grm.sml: mcs51/mcs51.grm
	$(MLYACC) mcs51/mcs51.grm

mcs51/mcs51.lex.sml: mcs51/mcs51.lex
	$(MLLEX)  mcs51/mcs51.lex

version.sml:
	(echo "structure Version = struct ";   \
	 echo "  val version=\"$(URPAL_VERSION)\";"; \
	 echo "  val svnversion=\"`svnversion`\";";  \
	 echo "end") > version.sml

tags: $(SRC)
	exctags -R --exclude=.cm --exclude=*.grm.sml *

clean:
	-@find . \( $(SKIPCM) -name *.grm.sml -o -name *.lex.sml \
		    -o -name *.grm.sig -o -name *.grm.desc \)    \
	 	-print -exec rm {} \;
	-@for f in $(SRCDIRS); do rm -r $$f/.cm; done

clobber: clean
	-@rm version.sml urpal
	-@rm tags

