bin_PROGRAMS = cobc
cobc_SOURCES = cobc.c cobc.h tree.c tree.h codegen.c codegen.h \
	reserved.c reserved.h parser.y scanner.l scanner.h
EXTRA_cobc_SOURCES = inline.c
cobc_CFLAGS = -I$(top_srcdir)
cobc_LDADD = $(top_builddir)/lib/libsupport.a \
	-L$(top_builddir)/libcob -lcob $(LIBCOB_LIBS)

BUILT_SOURCES = parser.c parser.h scanner.c
CLEANFILES = parser.output

AM_CFLAGS = -Wall
AM_YFLAGS = -d -v
AM_LFLAGS = -d -s
