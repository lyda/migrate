VERSION = 0.4

distdir = open-cobol-testsuite-$(VERSION)
DISTFILES = README Makefile EXEC85.conf.in expand.pl report.pl summary.pl

#MODULES = CM DB IC IF IX NC OB RL RW SG SM SQ ST
MODULES = NC SM IC SQ RL IX

all: $(MODULES)

clean:
	rm -rf $(MODULES) copy EXEC85 EXEC85.cob

check: all
	for m in $(MODULES); do \
	  (cd $$m && make check) \
	done
	make summary

summary:
	perl summary.pl $(MODULES) > summary.log

snap:
	for m in $(MODULES); do \
	  (cp $$m/report.log $$m.txt) \
	done
	cp summary.log summary.txt

diff:
	for m in $(MODULES); do \
	  (diff $$m.txt $$m/report.log) \
	done
	diff summary.txt summary.log

dist:
	rm -rf $(distdir)
	mkdir -p $(distdir)/nist
	cp $(DISTFILES) $(distdir)/nist
	tar cfz $(distdir).tar.gz $(distdir)
	rm -rf $(distdir)

$(MODULES): newcob.val EXEC85 EXEC85.conf.in
	@sed 's/@MODULE@/$@/' EXEC85.conf.in > EXEC85.conf
	./EXEC85
	@perl expand.pl newcob.tmp
	@rm -f newcob.tmp EXEC85.conf
	@echo "check:"              > $@/Makefile
	@echo "	perl ../report.pl" >> $@/Makefile
	@echo

EXEC85: EXEC85.cob
	cobc EXEC85.cob

EXEC85.cob: newcob.val
	perl -pe 'BEGIN{<>; <>;} exit if /^\*END/;' newcob.val		     \
	| sed -e 's/XXXXX001.*/"newcob.val" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX002.*/"newcob.tmp" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX003.*/"unused"./'				     \
	      -e 's/XXXXX055.*/"\/dev\/tty"./'				     \
	      -e 's/XXXXX058.*/"EXEC85.conf" ORGANIZATION LINE SEQUENTIAL./' \
	      -e 's/ORGANIZATION SEQUENTIAL.*//'			     \
	    > EXEC85.cob
