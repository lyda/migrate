pkglibexecdir = $(libexecdir)/@PACKAGE@
pkglibexec_PROGRAMS = cobpp
cobpp_SOURCES = cobpp.c cobpp.h parser.y parser.h scanner.l
BUILT_SOURCES = parser.c parser.h scanner.c
MAINTAINERCLEANFILES = Makefile.in

AM_CFLAGS = -Wall
AM_YFLAGS = -d
