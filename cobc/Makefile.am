bin_PROGRAMS = cobc
cobc_SOURCES = cobc.c cobc.h ppparse.c ppparse.h pplex.c tree.c tree.h \
	parser.c parser.h scanner.c codegen.c reserved.c reserved.h \
	usage.def flag.def warning.def

AM_CFLAGS = -Wall -Wwrite-strings -Wmissing-prototypes
cobc_CFLAGS = $(AM_CFLAGS) $(COBC_CFLAGS) -I$(top_srcdir)
cobc_LDADD = $(COBC_LIBS) $(top_builddir)/lib/libsupport.a

BUILT_SOURCES = parser.c parser.h scanner.c ppparse.c ppparse.h pplex.c
EXTRA_DIST = ppparse.y pplex.l parser.y scanner.l
CLEANFILES = parser.output ppparse.output

%.c: %.l
	flex -s -o$@ $<

%.c: %.y
	bison -o $@ $<
