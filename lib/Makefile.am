if HAVE_GETOPT_LONG_ONLY
getopt_SOURCES = 
else
getopt_SOURCES = getopt.c getopt1.c getopt.h
endif

noinst_LIBRARIES = libsupport.a
libsupport_a_SOURCES = $(getopt_SOURCES)

noinst_HEADERS = gettext.h

EXTRA_DIST = getopt.c getopt1.c getopt.h
MAINTAINERCLEANFILES = Makefile.in
