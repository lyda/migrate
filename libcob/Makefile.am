lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c \
	termio.c fileio.c strings.c call.c
libcob_la_CFLAGS = $(LIBCOB_CFLAGS) -I$(top_srcdir)/lib
libcob_la_LIBADD = $(LIBCOB_LIBS) @LTLIBINTL@
libcob_la_LDFLAGS = -version-info 0:0:0

pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = cobconfig.h common.h move.h numeric.h \
	termio.h fileio.h strings.h call.h support.h

EXTRA_DIST = cobconfig.h.in
DISTCLEANFILES = cobconfig.h
MAINTAINERCLEANFILES = Makefile.in
