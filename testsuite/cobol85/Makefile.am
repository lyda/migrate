#MODULES = CM DB IC IF IX NC OB RL RW SG SM SQ ST
MODULES = NC SM IC SQ RL IX

EXTRA_DIST = EXEC85.conf.in expand.pl report.pl summary.pl
CLEANFILES = $(MODULES) copy EXEC85 EXEC85.cob

check: $(MODULES)
	for m in $(MODULES); do \
	  (cd $$m && make check) \
	done
	perl summary.pl $(MODULES) > summary.log
	diff summary.txt summary.log

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

$(MODULES): newcob.val EXEC85 EXEC85.conf.in
	@echo -n "Building module $@..."
	@sed 's/@MODULE@/$@/' EXEC85.conf.in > EXEC85.conf
	@./EXEC85
	@perl expand.pl newcob.tmp
	@rm -f newcob.tmp newcob.log EXEC85.conf
	@echo "check:"                      > $@/Makefile
	@echo "	perl ../report.pl"         >> $@/Makefile
	@echo "	diff ../$@.txt report.log" >> $@/Makefile
	@echo "done"

EXEC85: EXEC85.cob
	cobc EXEC85.cob

EXEC85.cob: newcob.val
	@perl -pe 'BEGIN{<>; <>;} exit if /^\*END/;' newcob.val		     \
	| sed -e 's/XXXXX001.*/"newcob.val" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX002.*/"newcob.tmp" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX003.*/"unused"./'				     \
	      -e 's/XXXXX055.*/"newcob.log"./'				     \
	      -e 's/XXXXX058.*/"EXEC85.conf" ORGANIZATION LINE SEQUENTIAL./' \
	      -e 's/ORGANIZATION SEQUENTIAL.*//'			     \
	    > EXEC85.cob
