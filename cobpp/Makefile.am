bin_PROGRAMS = cobpp
cobpp_SOURCES = cobpp.c cobpp.h parser.y scanner.l scanner.h
cobpp_CFLAGS = -I$(top_srcdir)/lib
cobpp_LDADD = @LTLIBINTL@
cobpp_LIBS = $(top_srcdir)/lib/libsupport.a

CLEANFILES = parser.c parser.h scanner.c
MAINTAINERCLEANFILES = Makefile.in

AM_CFLAGS = -Wall
AM_YFLAGS = -d
AM_LFLAGS = -s
