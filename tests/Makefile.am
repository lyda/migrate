SUBDIRS = cobol85

TESTS =	$(srcdir)/cobol2002-syntax \
	$(srcdir)/cobol2002-run \
	$(srcdir)/cobol2002-run-O \
	$(srcdir)/data-rep \
	$(srcdir)/data-rep-O \
	$(srcdir)/extension \
	$(srcdir)/extension-O

cobol2002_syntax_DEPENDENCIES = \
	cobol2002-syntax.src/definition.at \
	cobol2002-syntax.src/multiply.at \
	cobol2002-syntax.src/redefines.at \
	cobol2002-syntax.src/value.at \
	cobol2002-syntax.src/move.at \
	cobol2002-syntax.src/occurs.at \
	cobol2002-syntax.src/subscripts.at

cobol2002_run_DEPENDENCIES = \
	cobol2002-run.src/fundamental.at \
	cobol2002-run.src/copy.at \
	cobol2002-run.src/initialize.at \
	cobol2002-run.src/misc.at \
	cobol2002-run.src/ref-mod.at \
	cobol2002-run.src/subscripts.at

data_rep_DEPENDENCIES = \
	data-rep.src/binary.at \
	data-rep.src/display.at \
	data-rep.src/packed.at \
	data-rep.src/numeric-dump.cob

extension_DEPENDENCIES = \
	extension.src/extensions.at \
	extension.src/return-code.at

EXTRA_DIST = $(srcdir)/package.m4 $(TESTS) \
	$(cobol2002_syntax_DEPENDENCIES) \
	$(cobol2002_run_DEPENDENCIES) \
	$(data_rep_DEPENDENCIES) \
	$(extension_DEPENDENCIES)

DISTCLEANFILES = atconfig

all: $(TESTS)

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

$(srcdir)/%: $(srcdir)/%.at $(srcdir)/package.m4
	autom4te --language=autotest -I $(srcdir) -I $(srcdir)/$@.src -o $@ $<
	chmod +x $@

$(srcdir)/cobol2002-syntax: $(cobol2002_syntax_DEPENDENCIES)
$(srcdir)/cobol2002-run: $(cobol2002_run_DEPENDENCIES)
$(srcdir)/data-rep: $(data_rep_DEPENDENCIES)
$(srcdir)/extension: $(extension_DEPENDENCIES)
