if HAVE_GETOPT_LONG_ONLY
getopt_SOURCES = 
else
getopt_SOURCES = getopt.c getopt1.c getopt.h
endif

bin_PROGRAMS = cobpp
cobpp_SOURCES = cobpp.c cobpp.h parser.y parser.h scanner.l scanner.h \
	$(getopt_SOURCES)
BUILT_SOURCES = parser.c parser.h scanner.c
EXTRA_DIST = getopt.c getopt1.c getopt.h
MAINTAINERCLEANFILES = Makefile.in

AM_CFLAGS = -Wall
AM_YFLAGS = -d
AM_LFLAGS = -s
