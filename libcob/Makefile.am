lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c \
	termio.c fileio.c screenio.c strings.c call.c
libcob_la_CFLAGS = $(LIBCOB_CFLAGS) -I$(top_srcdir)
libcob_la_LIBADD = $(LIBCOB_LIBS)
libcob_la_LDFLAGS = -version-info 0:0:0

pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h move.h numeric.h \
	termio.h fileio.h screenio.h strings.h call.h

AM_CFLAGS = -Wall
