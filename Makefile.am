SUBDIRS = cobpp cobc libcob doc
EXTRA_DIST = VERSION

DISTCLEANFILES = defaults.h
MAINTAINERCLEANFILES = config.guess config.sub config.log \
	Makefile.in aclocal.m4 config.h.in stamp-h.in depcomp ltmain.sh \
	INSTALL configure missing mkinstalldirs install-sh texinfo.tex

rpmdir = /home/knishida/redhat

rpm: dist
	cp $(distdir).tar.gz $(rpmdir)/SOURCES/
	rpm -ba rpm/open-cobol.spec

test:
	@(cd test_suite && $(MAKE))
