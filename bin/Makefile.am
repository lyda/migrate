bin_PROGRAMS = cobcrun
cobcrun_SOURCES = cobcrun.c

AM_CFLAGS = -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k
AM_LDFLAGS = $(COB_EXPORT_DYN)
cobcrun_CFLAGS = $(AM_CFLAGS) $(COBC_CFLAGS) -I$(top_srcdir)
if COBCRUN_SHARED
cobcrun_LDADD = $(LIBCOB_LIBS) -L$(top_builddir)/libcob -lcob
else
cobcrun_LDADD = $(LIBCOB_LIBS) \
$(top_builddir)/libcob/.libs/libcob_la-call.o \
$(top_builddir)/libcob/.libs/libcob_la-numeric.o \
$(top_builddir)/libcob/.libs/libcob_la-common.o \
$(top_builddir)/libcob/.libs/libcob_la-screenio.o \
$(top_builddir)/libcob/.libs/libcob_la-fileio.o \
$(top_builddir)/libcob/.libs/libcob_la-strings.o \
$(top_builddir)/libcob/.libs/libcob_la-move.o \
$(top_builddir)/libcob/.libs/libcob_la-termio.o
endif

