SUBDIRS = cobol85

TESTS =	$(srcdir)/testsuite

TESTSUITE_AT = \
	testsuite.at \
	fundamental.at \
	subscripts.at \
	ref-mod.at \
	redefines.at \
	usage.at \
	value.at \
	initialize.at \
	move.at \
	multiply.at \
	definition.at \
	extension.at \
	entry.at \
	misc.at

EXTRA_DIST = $(srcdir)/package.m4 $(TESTS) $(TESTSUITE_AT)

DISTCLEANFILES = atconfig

clean-local:
	@for x in $(TESTS); do \
	  test -e $${x} && $(SHELL) $${x} -c; true; \
	done

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

$(srcdir)/%: $(srcdir)/%.at $(srcdir)/package.m4 atlocal.in
	autom4te --language=autotest -I $(srcdir) $< -o $@
	chmod +x $@

$(srcdir)/testsuite: $(TESTSUITE_AT)
