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

upload:
	if test -e $(distdir).tar.gz; then \
	  ncftpput upload.sourceforge.net /incoming $(distdir).tar.gz; \
	fi
	if test -e $(rpmdir)/RPMS/i386/$(distdir)-$(RELEASE).i386.rpm; then \
	  ncftpput upload.sourceforge.net /incoming \
	    $(rpmdir)/RPMS/i386/$(distdir)-$(RELEASE).i386.rpm \
	    $(rpmdir)/SRPMS/$(distdir)-$(RELEASE).src.rpm; \
	fi
	if test -e $(PACKAGE)_$(VERSION)-$(RELEASE)_i386.deb; then \
	  ncftpput upload.sourceforge.net /incoming \
	    $(PACKAGE)_$(VERSION)-$(RELEASE).tar.gz \
	    $(PACKAGE)_$(VERSION)-$(RELEASE)_i386.deb; \
	fi
