VERSION = 0.2

distdir = open-cobol-testsuite-$(VERSION)
DISTFILES = README Makefile nc_exec85.pl nc_extr.pl report.pl \
	    make_pop_linux.txt

all: NC

check: all
	(cd NC; perl ../report.pl)
	diff report.old report.log

dist:
	rm -rf $(distdir)
	mkdir -p $(distdir)/nist
	cp $(DISTFILES) $(distdir)/nist
	tar cfz $(distdir).tar.gz $(distdir)
	rm -rf $(distdir)

NC: EXEC85
	./EXEC85
	perl nc_extr.pl XXXXX002

EXEC85: EXEC85.cob
	cobc EXEC85.cob

EXEC85.cob: newcob.val make_pop_linux.txt
	perl nc_exec85.pl newcob.val
