# Process this file with autoconf to produce a configure script.
AC_INIT(open-cobol, 0.9.6.1)
AC_CONFIG_SRCDIR(cobc/cobc.c)
AM_CONFIG_HEADER(config.h)
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

# enable debug mode by default for now
AC_DEFINE(COB_DEBUG)

# Configure options.
COB_CC=""
AC_ARG_WITH(cc,
  [  --with-cc=<gcc>         specify C compiler],
  [ COB_CC=$withval ])
if test "x${COB_CC}" = "x"; then
  AC_CHECK_PROG([COB_CC], [gcc], [gcc])
fi
if test "x${COB_CC}" = "x"; then
 AC_CHECK_PROG([COB_CC], [cc], [cc])
fi

AC_ARG_ENABLE(debug,
  [  --enable-debug          enable debugging options],
  [ AC_DEFINE(COB_DEBUG) ])

# Autoheader templates
AH_TEMPLATE([COB_DEBUG], [Enable debugging features in the compiler])

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
AC_C_BIGENDIAN
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

# Checks for gettext
AM_GNU_GETTEXT([external])

# Checks for getopt_long
AC_CHECK_FUNC([getopt_long_only], getopt=true, getopt=false)
AM_CONDITIONAL(HAVE_GETOPT_LONG_ONLY, test x$getopt = xtrue)

# Checks for db.
LIBDB="-ldb"
AC_CHECK_HEADERS([db.h], , AC_MSG_ERROR(db.h is required))
AC_CHECK_LIB([db], [db_version], , AC_MSG_ERROR(DB 2.0 or later is required))

# Checks for gmp.
LIBGMP="-lgmp"
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(gmp3 is required))

# Checks for ltdl.
LIBLTDL="-lltdl"
AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(libltdl is required))

# Checks for readline.
LIBRL=""
USE_READLINE="no"
AC_ARG_WITH(readline,
  [  --with-readline          use readline for terminal input],
  [AC_CHECK_HEADERS([readline/readline.h readline/history.h])
   AC_CHECK_LIB([readline], [readline],
                [LIBRL="-lreadline"; USE_READLINE="yes"],
		[AC_MSG_ERROR(libreadline is required)])
  ])

# Checks for GCC's computed-goto extension
AC_CACHE_CHECK([for computed-goto], [ac_computed_goto],
[AC_TRY_COMPILE([], [
int foo(int);
int foo(i)
int i; { 
static void *label[] = { &&l1, &&l2 };
goto *label[i];
l1: return 1;
l2: return 2;
}
],
ac_computed_goto=yes,
ac_computed_goto=no)])

COMPUTED_GOTO=0
if test "$ac_computed_goto" = yes; then
  COMPUTED_GOTO=1
fi
AC_SUBST(COMPUTED_GOTO)

# OpenCOBOL Configuration

LIBCOB_CFLAGS=""
LIBCOB_LIBS="-lm ${LIBDB} ${LIBGMP} ${LIBLTDL} ${LIBINTL} ${LIBRL}"
COB_COBPP="$bindir/cobpp"
COB_CFLAGS="-I$includedir"
COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_CONFIG_FILE="$datadir/$PACKAGE/libcob.conf"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE"
COB_MODULE_EXT=`echo "$shlibext" | sed 's/dll\.a/dll/'`
LIBS="$save_libs"

AC_SUBST(COB_CC)
AC_SUBST(COB_COBPP)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_CONFIG_FILE)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(COB_MODULE_EXT)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)

AC_CONFIG_FILES([cob-config libcob/cobconfig.h
		Makefile lib/Makefile libcob/Makefile
		cobpp/Makefile cobc/Makefile po/Makefile.in texi/Makefile
		testsuite/Makefile testsuite/cobol85/Makefile])
if test "$USE_MAINTAINER_MODE" = "yes"; then
  AC_CONFIG_FILES([redhat/open-cobol.spec])
fi
AC_OUTPUT

echo ""
echo "OpenCOBOL Configuration:"
echo ""
echo "  COB_CC               ${COB_CC}"
echo "  COB_COBPP            ${COB_COBPP}"
echo "  COB_CFLAGS           ${COB_CFLAGS}"
echo "  COB_LIBS             ${COB_LIBS}"
echo "  COB_CONFIG_FILE      ${COB_CONFIG_FILE}"
echo "  COB_LIBRARY_PATH     ${COB_LIBRARY_PATH}"
echo "  COB_MODULE_EXT       ${COB_MODULE_EXT}"
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
echo "  Use readline for terminal input:             ${USE_READLINE}"
echo "  Use computed-goto to implement PERFORM:      ${ac_computed_goto}"
echo ""
