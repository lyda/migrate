bin_PROGRAMS = cobpp
cobpp_SOURCES = cobpp.c cobpp.h parser.y scanner.l scanner.h
cobpp_CFLAGS = -I$(top_srcdir)
cobpp_LDADD = $(top_builddir)/lib/libsupport.a @LTLIBINTL@

BUILT_SOURCES = parser.c parser.h scanner.c

AM_CFLAGS = -Wall
AM_YFLAGS = -d
AM_LFLAGS = -s
