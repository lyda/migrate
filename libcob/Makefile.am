lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c \
	termio.c fileio.c screenio.c strings.c call.c

AM_CFLAGS = -Wall -Wstrict-prototypes -Wmissing-prototypes
libcob_la_CFLAGS = $(AM_CFLAGS) $(LIBCOB_CFLAGS) -I$(top_srcdir)
libcob_la_LIBADD = $(LIBCOB_LIBS)
libcob_la_LDFLAGS = -version-info 1:0:0

pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = common.h move.h numeric.h exception.def \
	termio.h fileio.h screenio.h strings.h call.h
