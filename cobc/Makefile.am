bin_PROGRAMS = cobc
cobc_SOURCES = cobc.c cobc.h ppparse.c ppparse.h pplex.c tree.c tree.h \
	codegen.c reserved.c reserved.h parser.c parser.h scanner.c \
	warning.def

cobc_CFLAGS = $(COBC_CFLAGS) -I$(top_srcdir)
cobc_LDADD = $(COBC_LIBS) $(top_builddir)/lib/libsupport.a \
	$(top_builddir)/libcob/libcob.la $(LIBCOB_LIBS)

BUILT_SOURCES = parser.c parser.h scanner.c ppparse.c ppparse.h pplex.c
EXTRA_DIST = ppparse.y pplex.l parser.y scanner.l
CLEANFILES = parser.output ppparse.output

AM_CFLAGS = -Wall
AM_YFLAGS = -d -v
AM_LFLAGS = -s

%.c: %.l
	$(LEX) $(AM_LFLAGS) -o$@ $<

%.c %.h: %.y
	$(YACC) $(AM_YFLAGS) -o $@ $<
