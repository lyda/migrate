bin_PROGRAMS = cobc
cobc_SOURCES = cobc.c cobc.h tree.c tree.h codegen.c codegen.h functions.h \
	reserved.c reserved.h parser.y scanner.l scanner.h
cobc_CFLAGS = -I$(top_srcdir)/lib
cobc_LDADD = -L$(top_srcdir)/libcob -lcob
cobc_LIBS = $(top_srcdir)/lib/libsupport.a
EXTRA_cobc_SOURCES = inline.c

BUILT_SOURCES = parser.c parser.h scanner.c
CLEANFILES = parser.c parser.h scanner.c y.output
MAINTAINERCLEANFILES = Makefile.in

AM_CFLAGS = -Wall
AM_YFLAGS = -d -v
AM_LFLAGS = -d -s
