lib_LTLIBRARIES = libcob.la
libcob_la_SOURCES = common.c move.c numeric.c intrinsic.c \
	termio.c fileio.c screenio.c strings.c call.c

AM_CFLAGS = -fsigned-char -Wall -Wstrict-prototypes -Wmissing-prototypes -Wno-format-y2k -DSUPER_OPTIMIZE
libcob_la_CFLAGS = $(AM_CFLAGS) $(LIBCOB_CFLAGS) -I$(top_srcdir)
libcob_la_LIBADD = $(LIBCOB_LIBS)
libcob_la_LDFLAGS = -version-info 1:0:0 -no-undefined
if COBCRUN_SHARED
AM_LDFLAGS = -Wl,--export-all-symbols -Wl,--enable-auto-import
endif

pkgincludedir = $(includedir)/libcob
pkginclude_HEADERS = byteswap.h common.h move.h numeric.h exception.def \
	termio.h fileio.h screenio.h strings.h call.h intrinsic.h
