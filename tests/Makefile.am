SUBDIRS = cobol85

TESTSUITE = $(srcdir)/redef $(srcdir)/subref

EXTRA_DIST = $(TESTSUITE:=.at) $(TESTSUITE)

check-local: atconfig atlocal $(TESTSUITE)
	@for x in $(TESTSUITE); do \
	  $(SHELL) $${x}; \
	done

distclean-local:
	rm -f atconfig
	rm -rf redef.dir redef.log
	rm -rf subref.dir subref.log

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	@echo "creating $(srcdir)/package.m4"
	@{								   \
	  echo '# Signature of the current package.';			   \
	  echo 'm4_define([AT_PACKAGE_NAME],	  [@PACKAGE_NAME@])';	   \
	  echo 'm4_define([AT_PACKAGE_TARNAME],	  [@PACKAGE_TARNAME@])';   \
	  echo 'm4_define([AT_PACKAGE_VERSION],	  [@PACKAGE_VERSION@])';   \
	  echo 'm4_define([AT_PACKAGE_STRING],	  [@PACKAGE_STRING@])';    \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [@PACKAGE_BUGREPORT@])'; \
	} > $(srcdir)/package.m4

$(srcdir)/%: $(srcdir)/%.at $(srcdir)/package.m4
	autom4te --language=autotest -I $(srcdir) $< -o $@.tmp
	mv $@.tmp $@
