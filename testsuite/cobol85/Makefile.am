#MODULES = CM DB IC IF IX NC OB RL RW SG SM SQ ST
MODULES = NC SM IC SQ RL IX

EXTRA_DIST = EXEC85.conf.in expand.pl report.pl summary.pl
CLEANFILES = EXEC85

test: $(MODULES)
	@for m in $(MODULES); do \
	  (cd $$m && make test) \
	done
	perl summary.pl $(MODULES) > summary.txt
	test -e summary.old && diff summary.old summary.txt

save:
	@for m in $(MODULES); do \
	  (cp $$m/report.txt $$m.txt) \
	done
	cp summary.txt summary.old

diff:
	@for m in $(MODULES); do \
	  (diff $$m.txt $$m/report.txt) \
	done
	diff summary.old summary.txt

clean: clean-am
	rm -rf $(MODULES)

$(MODULES): newcob.val EXEC85 EXEC85.conf.in
	@echo -n "Building module $@..."
	@sed 's/@MODULE@/$@/' EXEC85.conf.in > EXEC85.conf
	@./EXEC85
	@perl expand.pl newcob.tmp
	@rm -f newcob.tmp newcob.log EXEC85.conf
	@echo "test:"                       > $@/Makefile
	@echo "	perl ../report.pl"         >> $@/Makefile
	@echo "	diff ../$@.txt report.txt" >> $@/Makefile
	@echo "done"

EXEC85: newcob.val
	@perl -pe 'BEGIN{<>; <>;} exit if /^\*END/;' newcob.val		     \
	| sed -e 's/XXXXX001.*/"newcob.val" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX002.*/"newcob.tmp" ORGANIZATION LINE SEQUENTIAL./'  \
	      -e 's/XXXXX003.*/"unused"./'				     \
	      -e 's/XXXXX055.*/"newcob.log"./'				     \
	      -e 's/XXXXX058.*/"EXEC85.conf" ORGANIZATION LINE SEQUENTIAL./' \
	      -e 's/ORGANIZATION SEQUENTIAL.*//'			     \
	    > EXEC85.cob
	cobc -main EXEC85.cob
	@rm -f EXEC85.cob
