# $Id$
#
MAKE=make

.include "src/Makefile"

FREEBSD=urpal-$(URPAL_VERSION).x86-freebsd
LINUX=urpal-$(URPAL_VERSION).x86-linux
WIN32=urpal-$(URPAL_VERSION).x86-win32
SRCDIST=urpal-$(URPAL_VERSION)

CHECKBIN=readelf -e src/urpal | sed -n -e 's| *OS/ABI: *\(.*\)|\1|p'
SKIPSVN=-type d -name '.svn' -prune -type f -o \! -name .svn -a
STRIPDOT=sed -ne 's:^\./\(.*\):\1:p'
all:
	make -C ./src

distfiles: freebsd linux win32 src

freebsd: dist/$(FREEBSD)
linux: dist/$(LINUX)
win32: dist/$(WIN32)
src: dist/$(SRCDIST)

dist/$(SRCDIST): dist
	@make -C ./src clobber
	@make -C ./doc clobber
	-@mkdir dist/$(SRCDIST)
.for srcdir in ./src ./tests ./doc
	@(for d in `find $(srcdir) $(SKIPSVN) -type d | $(STRIPDOT)`; do \
	    mkdir dist/$(SRCDIST)/$$d; \
	 done)
	@(for f in `find $(srcdir) $(SKIPSVN) -type f | $(STRIPDOT)`; do \
	    cp $$f dist/$(SRCDIST)/$$f; \
	 done)
	@(for f in `find $(srcdir) $(SKIPSVN) -type l | $(STRIPDOT)`; do \
	    cp -RP $$f dist/$(SRCDIST)/$$f; \
	 done)
.endfor
	@cp README LICENSE dist/$(SRCDIST)/
	tar czf dist/$(SRCDIST).tar.gz -C dist $(SRCDIST)

dist/$(FREEBSD): dist
	make -C ./src clobber
	make -C ./src withmlton
	test "`$(CHECKBIN)`" = 'UNIX - FreeBSD'
	-mkdir dist/$(FREEBSD)
	cp src/urpal doc/urpal.1 README LICENSE dist/$(FREEBSD)
	(echo 'dtd_path="flat-1_1.dtd"'; \
	 echo 'graphviz {'; \
	 echo '    path="/usr/local"'; \
	 echo '    engine=neato'; \
	 echo '}') > dist/$(FREEBSD)/urpalrc
	tar czf dist/$(FREEBSD).tar.gz -C dist $(FREEBSD)

dist/$(WIN32): dist
	make -C ./doc urpal.1.pdf
	make -C ./src clobber
	make -C ./src mingw32
	-mkdir dist/$(WIN32)
	cp src/urpal.exe doc/urpal.1.pdf dist/$(WIN32)
	(echo 'dtd_path="flat-1_1.dtd"'; \
	 echo 'graphviz {'; \
	 echo '    path="C:\Program Files\Graphviz2.16"'; \
	 echo '    engine=neato'; \
	 echo '}') | unix2dos > dist/$(WIN32)/urpalrc
	cat README | unix2dos > dist/$(WIN32)/README.txt
	cat LICENSE | unix2dos > dist/$(WIN32)/LICENSE
	(cd dist; zip -r $(WIN32).zip $(WIN32))

dist/$(LINUX): dist
	# make -C ./src clobber
	# make -C ./src withmlton
	test "`$(CHECKBIN)`" = 'UNIX - Linux'
	-mkdir dist/$(LINUX)
	cp src/urpal doc/urpal.1 README LICENSE dist/$(LINUX)
	(echo 'dtd_path="flat-1_1.dtd"'; \
	 echo 'graphviz {'; \
	 echo '    path="/usr"'; \
	 echo '    engine=neato'; \
	 echo '}') > dist/$(LINUX)/urpalrc
	tar czf dist/$(LINUX).tar.gz -C dist $(LINUX)

dist:
	mkdir dist

