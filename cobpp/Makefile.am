bin_PROGRAMS = cobpp
cobpp_SOURCES = cobpp.c cobpp.h parser.y scanner.l scanner.h
cobpp_CFLAGS = $(COBPP_CFLAGS) -I$(top_srcdir)
cobpp_LDADD = $(COBPP_LIBS) $(top_builddir)/lib/libsupport.a

BUILT_SOURCES = parser.c parser.h scanner.c

AM_CFLAGS = -Wall
AM_YFLAGS = -d
AM_LFLAGS = -s
