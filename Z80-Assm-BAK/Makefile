OOC=oo2c
OOC=/usr/local/bin/oo2c
OFLAGS=
PKG=Asm8080
# PKG=test

$(PKG):
	$(OOC) --build-package $(PKG)
	$(OOC) --build-pkg-doc $(PKG)
	# oo2c -Mv -r . src/$(PKG).Mod | ooef -l

configure: configure.ac
	autoconf

clean:
	rm -Rf sym obj bin oocdoc

distclean: clean
	rm -f Makefile pkginfo.xml config.log config.status
	rm -Rf autom4te.cache
