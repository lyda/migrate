SUBDIRS = cobpp cobc libcob doc

DISTCLEANFILES = defaults.h
MAINTAINERCLEANFILES = config.guess config.sub config.log \
	Makefile.in aclocal.m4 config.h.in stamp-h.in depcomp ltmain.sh \
	INSTALL configure missing mkinstalldirs install-sh texinfo.tex

test:
	@(cd test_suite && $(MAKE))
