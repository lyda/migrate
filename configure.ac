#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2001-2003 Keisuke Nishida
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_PREREQ(2.57)

AC_INIT([OpenCOBOL],[0.20],[open-cobol-list@lists.sourceforge.net],[open-cobol])
AC_CONFIG_SRCDIR([libcob.h])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# enable debug mode by default for now
AC_DEFINE(COB_DEBUG)

COBC_LIBS=""
COBC_CFLAGS=""
LIBCOB_LIBS="-lm"
LIBCOB_CFLAGS=""

# Configure options.
COB_CC=""
AC_ARG_WITH(cc,
  [  --with-cc=<gcc>         specify C compiler],
  [ COB_CC=$withval ])
if test "x${COB_CC}" = "x"; then
  AC_CHECK_PROG([COB_CC], [gcc], [gcc])
fi
if test "x${COB_CC}" = "x"; then
  AC_CHECK_PROG([COB_CC], [cc], [cc], AC_MSG_ERROR(cc is required))
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
AC_CHECK_HEADERS([fcntl.h malloc.h libintl.h locale.h stddef.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp pow setlocale strerror \
		strcasecmp strchr strrchr strdup strstr strtol])

# Checks for getopt_long.
AC_CHECK_FUNC([getopt_long_only], ,
  [AC_LIBOBJ(getopt)
   AC_LIBOBJ(getopt1)
   AC_LIBSOURCE(getopt.h)])

# Checks for regex.
AC_CHECK_LIB([regex], [regcomp], [LIBCOB_LIBS="$LIBCOB_LIBS -lregex"])

# Checks for db.
AC_CHECK_HEADERS([db1/db.h db_185.h db.h])
AC_CHECK_FUNCS([dbopen], ,
  [AC_CHECK_LIB([db1], [dbopen],
    [LIBCOB_LIBS="$LIBCOB_LIBS -ldb1"],
    [AC_CHECK_LIB([db], [__db_dbopen],
      [LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
      [AC_MSG_ERROR(libdb1 is required)])])])

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(GMP 3 or later is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"

# Checks for ltdl.
AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(libltdl is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lltdl"

# Checks for gettext
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.11.5)
if test "x$LTLIBINTL" != "x"; then
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for ncurses.
USE_NCURSES="no"
AC_CHECK_HEADERS([curses.h])
AC_CHECK_LIB([ncurses], [initscr])
if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
  USE_NCURSES="yes"
  LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
fi

# Checks for readline.
USE_READLINE="no"
AC_ARG_WITH(readline,
  [  --with-readline          use readline for terminal input],
  [AC_CHECK_HEADERS([readline/readline.h readline/history.h])
   AC_CHECK_LIB([readline], [readline])])
if test "x$ac_cv_lib_readline_readline" = "xyes"; then
  USE_READLINE="yes"
  LIBCOB_LIBS="$LIBCOB_LIBS -lreadline"
fi

# OpenCOBOL Configuration

if test "x$prefix" = "x/usr" \
     -a "x$includedir" = "x\${prefix}/include"; then
  COB_CFLAGS=""
else
  COB_CFLAGS="-I$includedir"
fi

COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_CONFIG_FILE="$sysconfdir/libcob.conf"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE_TARNAME"
COB_MODULE_EXT=`echo "$shlibext" | sed 's/dll\.a/dll/'`
LIBS="$save_libs"

AC_SUBST(COB_CC)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_CONFIG_FILE)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(COB_MODULE_EXT)
AC_SUBST(COBC_CFLAGS)
AC_SUBST(COBC_LIBS)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)

AC_CONFIG_FILES([cob.pc cob-config
		Makefile lib/Makefile libcob/Makefile
		cobc/Makefile po/Makefile.in texi/Makefile
		tests/atlocal tests/Makefile tests/cobol85/Makefile])
AC_OUTPUT

echo ""
echo "OpenCOBOL Configuration:"
echo ""
echo "  COB_CC               ${COB_CC}"
echo "  COB_CFLAGS           ${COB_CFLAGS}"
echo "  COB_LIBS             ${COB_LIBS}"
echo "  COB_CONFIG_FILE      ${COB_CONFIG_FILE}"
echo "  COB_LIBRARY_PATH     ${COB_LIBRARY_PATH}"
echo "  COB_MODULE_EXT       ${COB_MODULE_EXT}"
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
echo "  Use ncurses for screen I/O:                  ${USE_NCURSES}"
echo "  Use readline for terminal input:             ${USE_READLINE}"
echo ""
