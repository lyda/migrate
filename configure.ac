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

# Checks for programs.
AC_PROG_CC
AM_PROG_LEX
AC_PROG_YACC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for libraries.
AC_LIB_LTDL
AC_CHECK_LIB([m], [pow])

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([alloca.h errno.h fcntl.h limits.h malloc.h stdlib.h \
		  string.h sys/param.h sys/time.h termios.h unistd.h])

# Checks for db.
AC_CHECK_DB

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(gmp is required))

# Checks for readline.
AC_CHECK_HEADERS([readline/readline.h readline/history.h])
AC_CHECK_LIB([readline], [readline])

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

COB_LDADD="-L\${libdir} -lcob -lncurses -ldl -lm -lgmp $DB_LIBS"
COB_LDFLAGS=

AC_SUBST(COB_LDADD)
AC_SUBST(COB_LDFLAGS)

AC_CONFIG_FILES([Makefile cobpp/Makefile cobc/Makefile libcob/Makefile
		doc/Makefile])
AC_OUTPUT
