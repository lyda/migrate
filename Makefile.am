bin_SCRIPTS=cob-config

aclocaldir = $(datadir)/aclocal
aclocal_DATA = cob.m4

include_HEADERS = libcob.h

SUBDIRS = libcob cobpp cobc texi
EXTRA_DIST = cob.m4 redhat debian

MAINTAINERCLEANFILES = config.guess config.sub config.status config.log \
	Makefile.in aclocal.m4 config.h.in stamp-h.in depcomp ltmain.sh \
	INSTALL configure missing mkinstalldirs install-sh

rpmdir = /home/knishida/redhat

rpm: dist
	cp $(distdir).tar.gz $(rpmdir)/SOURCES/
	rpm -ba redhat/open-cobol.spec

deb: dist
	tar xfz $(distdir).tar.gz
	(cd $(distdir) && dpkg-buildpackage -rfakeroot -us -uc)
	rm -rf $(distdir)
