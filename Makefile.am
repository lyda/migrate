bin_SCRIPTS=cob-config

aclocaldir = $(datadir)/aclocal
aclocal_DATA = cob.m4

SUBDIRS = cobpp cobc libcob texi
EXTRA_DIST = VERSION cob.m4 debian

MAINTAINERCLEANFILES = config.guess config.sub config.log \
	Makefile.in aclocal.m4 config.h.in stamp-h.in depcomp ltmain.sh \
	INSTALL configure missing mkinstalldirs install-sh texinfo.tex

upload-src: dist
	ncftpput upload.sourceforge.net /incoming $(distdir).tar.gz

rpmdir = /home/knishida/redhat

rpm: dist
	cp $(distdir).tar.gz $(rpmdir)/SOURCES/
	rpm -ba redhat/open-cobol.spec

upload-rpm: rpm
	ncftpput upload.sourceforge.net /incoming \
	  $(rpmdir)/RPMS/i386/$(distdir)-$(RELEASE).i386.rpm \
	  $(rpmdir)/SRPMS/$(distdir)-$(RELEASE).src.rpm

deb:
	dpkg-buildpackage -rfakeroot -us -uc

upload-deb: deb
	ncftpput upload.sourceforge.net /incoming \
	  ../$(PACKAGE)_$(VERSION)-$(RELEASE).tar.gz \
	  ../$(PACKAGE)_$(VERSION)-$(RELEASE)_i386.deb

test:
	@(cd testsuite && $(MAKE))
