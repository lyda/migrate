#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2001-2004 Keisuke Nishida
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

AC_PREREQ(2.59)

AC_INIT([OpenCOBOL],[0.33],[open-cobol-list@lists.sourceforge.net],[open-cobol])
AC_CONFIG_SRCDIR([libcob.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([WITH_DB],   [Compile with the Berkeley DB library])
AH_TEMPLATE([WITH_LFS64], [Compile with large file system 64])
AH_TEMPLATE([COB_DEBUG], [Enable debugging features in the compiler])
AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_SHARED_OPT], [Compile/link option for shared code])
AH_TEMPLATE([COB_STRIP_CMD], [Strip command])

# enable debug mode by default for now
AC_DEFINE(COB_DEBUG)

COBC_LIBS=""
COBC_CFLAGS=""
LIBCOB_LIBS="-lm"
LIBCOB_CFLAGS="$lt_prog_compiler_pic"

# Configure options.
COB_CC=""
AC_ARG_WITH(cc,
  [  --with-cc=<cc>          specify the C compiler used by cobc],
  [ COB_CC=$withval ])
if test "x${COB_CC}" = "x"; then
  AC_CHECK_PROG([COB_CC], [gcc], [gcc])
fi
if test "x${COB_CC}" = "x"; then
  AC_CHECK_PROG([COB_CC], [cc], [cc], AC_MSG_ERROR(cc is required))
fi

LIBDB=""
AC_ARG_WITH(db1,
  [  --with-db1              use Berkeley DB 1.85 (libdb-1.85)],
  [ LIBDB="1"
    AC_DEFINE(WITH_DB, 1) ])

AC_ARG_WITH(db,
  [  --with-db               use Berkeley DB 3.0 or later (libdb)],
  [ LIBDB="3"
    AC_DEFINE(WITH_DB, 3) ])

AC_ARG_WITH(lfs64,
  [  --with-lfs64            use large file system for file I/O],
  [ AC_DEFINE(WITH_LFS64) ])

AC_ARG_ENABLE(debug,
  [  --enable-debug          enable debugging options],
  [ AC_DEFINE(COB_DEBUG) ])

save_libs="$LIBS"

# Checks for getopt_long.
AC_CHECK_FUNC([getopt_long_only], ,
  [AC_LIBOBJ(getopt)
   AC_LIBOBJ(getopt1)
   AC_LIBSOURCE(getopt.h)])

# Checks for regex.
AC_CHECK_LIB([regex], [regcomp], [LIBCOB_LIBS="$LIBCOB_LIBS -lregex"])

# Checks for db.
USE_DB="no"
AC_CHECK_FUNCS([dbopen],
  [USE_DB="yes" AC_DEFINE(HAVE_DBOPEN)],
  [case $LIBDB in
    1) AC_CHECK_HEADERS([db1/db.h],
         [AC_DEFINE(HAVE_DB1_DB_H)],
         [AC_MSG_ERROR([db1/db.h is required])])
       AC_CHECK_LIB([db1], [dbopen],
         [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb1"],
         [AC_MSG_ERROR(libdb1 is required)])
       ;;
    3) AC_CHECK_HEADERS([db.h db_185.h db3/db_185.h])
       AC_CHECK_LIB([db], [__db_open],
         [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
         [AC_MSG_ERROR(libdb is required)])
       ;;
   esac])

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(GMP 3 or later is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"

# Checks for ltdl.
AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(libltdl is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lltdl"

# Checks for gettext.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.13)
if test "x$LTLIBINTL" != "x"; then
  COBC_LIBS="$COBC_LIBS $LTLIBINTL"
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for ncurses.
USE_NCURSES="no"
AC_CHECK_HEADERS([ncurses.h curses.h])
AC_CHECK_LIB([ncurses], [initscr])
if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
  USE_NCURSES="yes"
  LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
fi

# Checks for programs.
AC_PROG_CC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h malloc.h locale.h stddef.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp pow setlocale fcntl strerror \
		strcasecmp strchr strrchr strdup strstr strtol])

# OpenCOBOL Configuration

if test "x$prefix" = "x/usr" \
     -a "x$includedir" = "x\${prefix}/include"; then
  COB_CFLAGS=""
else
  COB_CFLAGS="-I$includedir"
fi

COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE_TARNAME"
COB_MODULE_EXT=`echo "$shlibext" | sed 's/dll\.a/dll/'`
LIBS="$save_libs"
COB_EXPORT_DYN="`eval echo $export_dynamic_flag_spec`"
COB_PIC_FLAGS="$lt_prog_compiler_pic"
COB_SHARED_OPT="-shared"

case $host_os in
  cygwin* | mingw*)
	cobcruns=true
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	;;
  darwin* | rhapsody*)
	cobcruns=false
	COB_SHARED_OPT="-dynamiclib"
	;;
  *)
	cobcruns=false
	;;
esac
AM_CONDITIONAL(COBCRUN_SHARED, test x$cobcruns = xtrue)
AC_DEFINE_UNQUOTED(COB_EXPORT_DYN, "$COB_EXPORT_DYN")
AC_DEFINE_UNQUOTED(COB_PIC_FLAGS, "$COB_PIC_FLAGS")
AC_DEFINE_UNQUOTED(COB_SHARED_OPT, "$COB_SHARED_OPT")
if test "x$striplib" != "x"; then
 AC_DEFINE_UNQUOTED(COB_STRIP_CMD, "$striplib")
 AC_SUBST(COB_STRIP_CMD)
fi
AC_SUBST(COB_CC)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_CONFIG_DIR)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(COB_MODULE_EXT)
AC_SUBST(COBC_CFLAGS)
AC_SUBST(COBC_LIBS)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)
AC_SUBST(COB_EXPORT_DYN)
AC_SUBST(COB_PIC_FLAGS)
AC_SUBST(COB_SHARED_OPT)

AC_CONFIG_FILES([cob-config
		Makefile lib/Makefile libcob/Makefile
		cobc/Makefile bin/Makefile po/Makefile.in texi/Makefile config/Makefile
		tests/atlocal tests/Makefile tests/cobol85/Makefile])
AC_OUTPUT

echo ""
echo "OpenCOBOL Configuration:"
echo ""
echo "  COB_CC               ${COB_CC}"
echo "  CFLAGS               ${CFLAGS}"
echo "  COB_CFLAGS           ${COB_CFLAGS}"
echo "  COB_LIBS             ${COB_LIBS}"
echo "  COB_CONFIG_DIR       ${COB_CONFIG_DIR}"
echo "  COB_LIBRARY_PATH     ${COB_LIBRARY_PATH}"
echo "  COB_MODULE_EXT       ${COB_MODULE_EXT}"
echo "  COB_SHARED_OPT       ${COB_SHARED_OPT}"
echo "  COB_PIC_FLAGS        ${COB_PIC_FLAGS}"
echo "  COB_EXPORT_DYN       ${COB_EXPORT_DYN}"
if test "x$striplib" != "x"; then
 echo "  COB_STRIP_CMD        ${striplib}"
fi
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
echo "  Use Berkeley DB for file I/O:                ${USE_DB}"
echo "  Use fcntl for file locking:                  ${ac_cv_func_fcntl}"
echo "  Use ncurses for screen I/O:                  ${USE_NCURSES}"
echo ""

case $LIBDB in
  1) echo "NOTICE: You chose to link Berkeley DB 1.85, which is distributed"
     echo "NOTICE: under the BSD license.  In accordance with the license,"
     echo "NOTICE: all advertising materials mentioning features or use of"
     echo "NOTICE: Berkeley DB must display the following acknowledgement:"
     echo ""
     echo "    This product includes software developed by the University"
     echo "    of California, Berkeley and its contributors."
     echo ""
     ;;
  3) echo "NOTICE: You chose to link Berkeley DB 3.0 or later, which is"
     echo "NOTICE: distributed under their own open-source license.  In"
     echo "NOTICE: accordance with the license, you must distribute the"
     echo "NOTICE: source code of your software along with your software,"
     echo "NOTICE: or you have to pay royalty to Sleepycat Software, Inc."
     echo "NOTICE: See http://www.sleepycat.com/ for details."
     echo ""
     ;;
esac
