bin_SCRIPTS=cob-config

aclocaldir = $(datadir)/aclocal
aclocal_DATA = cob.m4

SUBDIRS = cobpp cobc libcob texi
EXTRA_DIST = VERSION cob.m4

MAINTAINERCLEANFILES = config.guess config.sub config.log \
	Makefile.in aclocal.m4 config.h.in stamp-h.in depcomp ltmain.sh \
	INSTALL configure missing mkinstalldirs install-sh texinfo.tex

rpmdir = /home/knishida/redhat

rpm: dist
	cp $(distdir).tar.gz $(rpmdir)/SOURCES/
	rpm -ba redhat/open-cobol.spec
	cp $(rpmdir)/RPMS/i386/$(distdir)-$(RELEASE).i386.rpm .
	cp $(rpmdir)/SRPMS/$(distdir)-$(RELEASE).src.rpm .

test:
	@(cd testsuite && $(MAKE))
