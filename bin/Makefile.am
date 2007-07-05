bin_PROGRAMS = cobcrun
cobcrun_SOURCES = cobcrun.c

AM_LDFLAGS = $(COB_EXPORT_DYN)
cobcrun_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir)
cobcrun_LDADD = -L$(top_builddir)/libcob -lcob $(LIBCOB_LIBS)
