# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.50)

AC_INIT
AC_CONFIG_SRCDIR(cobc/cobc.c)
. $srcdir/VERSION
AM_INIT_AUTOMAKE($PACKAGE, $VERSION)
AM_MAINTAINER_MODE
AM_CONFIG_HEADER(config.h)
AC_SUBST(RELEASE)

# Configure options.
AC_ARG_ENABLE(debug,
	[  --enable-debug           enable debug features],
	[ AC_DEFINE(COB_DEBUG) ])

AC_ARG_ENABLE(maintainer-mode,
	[  --enable-maintainer-mode enable maintainer mode],
	[ AC_DEFINE(COB_DEBUG)
	  AC_CONFIG_FILES([rpm/open-cobol.spec]) ])

save_libs="$LIBS"

# Checks for programs.
AC_PROG_CC
AM_PROG_LEX
AC_PROG_YACC
AC_PROG_LN_S
AC_PROG_INSTALL
AM_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([alloca.h errno.h fcntl.h limits.h malloc.h stdlib.h \
		  string.h sys/param.h sys/time.h termios.h unistd.h])

# Checks for dl
AC_CHECK_HEADERS([dlfcn.h], , AC_MSG_ERROR(dlfcn.h is required))
AC_CHECK_LIB([dl], [dlopen], , AC_MSG_ERROR(dl is required))

# Checks for db.
AC_CHECK_HEADERS([db1/db.h], , AC_MSG_ERROR(db1 is required))
AC_CHECK_LIB([db1], [dbopen], , AC_MSG_ERROR(db1 is required))

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp is required))

# Checks for readline.
LIBRL=""
AC_CHECK_HEADERS([readline/readline.h readline/history.h])
AC_CHECK_LIB([readline], [readline], LIBRL="-lreadline")

# Checks for ncurses.
AC_CHECK_HEADERS([ncurses.h], , AC_MSG_ERROR(ncurses is required))
AC_CHECK_LIB([ncurses], [addch], , AC_MSG_ERROR(ncurses is required))
AC_CHECK_FUNCS([color_set attr_get])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_STAT
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([atexit memmove memset strcasecmp strchr strdup \
		strerror strrchr strstr strtol])

COB_CC="cc"
COB_COBPP="$libexecdir/$PACKAGE/cobpp"
COB_CFLAGS="-I$includedir"
COB_LIBADD="-L$libdir -lcob"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE"
LIBCOB_CFLAGS="${INCLTDL}"
LIBCOB_LIBADD="-ldl -ldb1 -lgmp -lncurses ${LIBRL}"
LIBS="$save_libs"

AC_SUBST(COB_CC)
AC_SUBST(COB_COBPP)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LIBADD)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBADD)

AC_CONFIG_FILES([Makefile cobpp/Makefile cobc/Makefile libcob/Makefile
		texi/Makefile cob-config])
AC_OUTPUT
