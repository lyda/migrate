# Process this file with autoconf to produce a configure script.
AC_INIT(open-cobol, 0.9.4)
AC_CONFIG_SRCDIR(cobc/cobc.c)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

# enable debug mode by default for now
AC_DEFINE(COB_DEBUG)

# Configure options.
AC_ARG_ENABLE(debug,
  [  --enable-debug           enable debug features],
  [ AC_DEFINE(COB_DEBUG) ])

AC_ARG_ENABLE(maintainer-mode,
  [  --enable-maintainer-mode enable maintainer mode],
  [ AC_DEFINE(COB_DEBUG)
    AC_CONFIG_FILES([redhat/open-cobol.spec])])

save_libs="$LIBS"

# Checks for programs.
AC_PROG_CC
AM_PROG_LEX
AC_PROG_YACC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h malloc.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_STAT
AC_FUNC_ALLOCA
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp \
		strcasecmp strchr strrchr strdup strstr strtol])

# Checks for getopt_long
AC_CHECK_FUNC([getopt_long_only], getopt=true, getopt=false)
AM_CONDITIONAL(HAVE_GETOPT_LONG_ONLY, test x$getopt = xtrue)

# Checks for db.
LIBDB="-ldb"
AC_CHECK_HEADERS([db.h], , AC_MSG_ERROR(db.h is required))
AC_CHECK_LIB([db], [db_create], , AC_MSG_ERROR(DB 3.0 or later is required))

# Checks for gmp.
LIBGMP="-lgmp"
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(gmp3 or higher is required))

# Checks for ltdl.
LIBLTDL="-lltdl"
AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(ltdl is required))

# Checks for readline.
LIBRL=""
AC_CHECK_HEADERS([readline/readline.h readline/history.h])
AC_CHECK_LIB([readline], [readline], LIBRL="-lreadline")

# Autoheader templates
AH_TEMPLATE([COB_DEBUG], [Enable debugging features in the compiler])

COB_CC="gcc"
COB_COBPP="cobpp"
COB_CFLAGS="-I$includedir"
COB_LIBS="-L$libdir -lcob ${LIBDB} ${LIBGMP} ${LIBLTDL} ${LIBRL}"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE"
LIBCOB_CFLAGS=""
LIBCOB_LIBS="${LIBDB} ${LIBGMP} ${LIBLTDL} ${LIBRL}"
LIBS="$save_libs"

AC_SUBST(COB_CC)
AC_SUBST(COB_COBPP)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)

AC_CONFIG_FILES([Makefile libcob/Makefile cobpp/Makefile cobc/Makefile
		texi/Makefile cob-config])
AC_OUTPUT
