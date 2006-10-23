bin_PROGRAMS = cobcrun
cobcrun_SOURCES = cobcrun.c

# AM_CFLAGS = -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k
AM_LDFLAGS = $(COB_EXPORT_DYN)
cobcrun_CFLAGS = $(AM_CFLAGS) -I$(top_srcdir)
cobcrun_LDADD = $(LIBCOB_LIBS) -L$(top_builddir)/libcob -lcob
