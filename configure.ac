#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2001-2006 Keisuke Nishida
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

# Hack to stop CXX/F77 checks - Any better way ?
tagnames=

# Note for SUN Solaris (gcc)
# export/setenv CFLAGS "-m64 -mptr64"
# export/setenv LDFLAGS "-m64 -mptr64 -L/usr/local/lib/sparcv9"
# ./configure --libdir=/usr/local/lib/sparcv9
#
# Hack for AIX 64 bit (gcc)
# Required -
# export/setenv CFLAGS=-maix64
# export/setenv LDFLAGS=-maix64

if echo $CFLAGS | grep 'aix64' 1>/dev/null 2>&1
then
	NM="nm -X64"
	AR="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([WITH_DB], [Compile with the Berkeley DB library])
AH_TEMPLATE([WITH_LFS64], [Compile with large file system 64])
AH_TEMPLATE([COB_DEBUG], [Enable debugging features in the compiler])
AH_TEMPLATE([COB_EXPORT_DYN], [Compile/link option for exporting symbols])
AH_TEMPLATE([COB_PIC_FLAGS], [Compile/link option for PIC code])
AH_TEMPLATE([COB_SHARED_OPT], [Compile/link option for shared code])
AH_TEMPLATE([COB_STRIP_CMD], [Strip command])
AH_TEMPLATE([USE_LIBDL], [Use system dynamic loader])
AH_TEMPLATE([PATCH_LEVEL], [Define a patch level])
AH_TEMPLATE([WITH_VARSEQ], [Define variable sequential file format])
# AH_TEMPLATE([DLLHIDDEN], [Has visibility attribute])
AH_TEMPLATE([HAVE_PSIGN_OPT], [Has -Wno-pointer-sign])
AH_TEMPLATE([HAVE_TIMEZONE], [Has timezone variable])
AH_TEMPLATE([COB_EXEEXT], [Executable extension])
AH_TEMPLATE([COB_EXTRA_FLAGS], [Extra Compiler Flags])
AH_TEMPLATE([__USE_STRING_INLINES], [Compiler optimization])

# enable debug mode by default for now
AC_DEFINE(COB_DEBUG)
AC_DEFINE(__USE_STRING_INLINES)

LIBCOB_LIBS="-lm"
LIBCOB_CFLAGS="$lt_prog_compiler_pic"

AC_PROG_CC
AC_PROG_CPP
AC_PROG_EGREP

# Configure options.
COB_CC=""
AC_ARG_WITH(cc,
  [  --with-cc=<cc>          (OpenCOBOL) specify the C compiler used by cobc],
  [ COB_CC=$withval ], [ COB_CC=$CC] )

#if test "x${COB_CC}" = "x"; then
#  AC_CHECK_PROG([COB_CC], [gcc], [gcc])
#fi
#if test "x${COB_CC}" = "x"; then
#  AC_CHECK_PROG([COB_CC], [cc], [cc], AC_MSG_ERROR(cc is required))
#fi

LIBDB=""
AC_ARG_WITH(db1,
  [  --with-db1              (OpenCOBOL) use Berkeley DB 1.85 (libdb-1.85)],
  [ LIBDB="1"
    AC_DEFINE(WITH_DB, 1) ],
	[ AC_ARG_WITH(db,
		[  --with-db               (OpenCOBOL) use Berkeley DB 3.0 or later (libdb)(default)],
		[ if test "$with_db" = "yes"
		  then
		  LIBDB="3"
		  AC_DEFINE(WITH_DB, 3)
		  fi ],
		[ LIBDB="3"
		  AC_DEFINE(WITH_DB, 3) ]) ])

#AC_ARG_WITH(db,
#  [  --with-db               (OpenCOBOL) use Berkeley DB 3.0 or later (libdb)],
#  [ LIBDB="3"
#    AC_DEFINE(WITH_DB, 3) ])

AC_ARG_WITH(lfs64,
  [  --with-lfs64            (OpenCOBOL) use large file system for file I/O (default)],
  [ if test "$with_lfs64" = "yes"
    then
	AC_DEFINE(WITH_LFS64)
    fi ],
  [ AC_DEFINE(WITH_LFS64) ])

DEFINE_DL="no"
COB_TRY_DL="yes"

AC_ARG_WITH(dl,
  [  --with-dl               (OpenCOBOL) use system dynamic loader (default)],
  [case $with_dl in  
    yes)	COB_TRY_DL="yes"
		;;
    no)		COB_TRY_DL="no"
		;;
    *)		AC_MSG_ERROR(["--with/without-dl can not have a value"])
		;;
  esac])

GEN_DEBUG_OPT="no"

AC_ARG_ENABLE(debug,
  [  --enable-debug          (OpenCOBOL) enable debugging options],
  [ AC_DEFINE(COB_DEBUG) GEN_DEBUG_OPT="yes"])

PATCH_LEVEL=0
AC_ARG_WITH(patch-level, [  --with-patch-level      (OpenCOBOL) define a patch level (default 0)],
[case $with_patch_level in  
    yes)	AC_MSG_ERROR(["You must give --with-patch-level an argument."])
		;;
    no)		AC_MSG_ERROR(["--without-patch-level not supported."])
		;;
    [[0-9]]*)	PATCH_LEVEL=$with_patch_level
		;;
    *)		AC_MSG_ERROR(["You must use a numeric patch level"])
		;;
esac])
AC_DEFINE_UNQUOTED(PATCH_LEVEL, $PATCH_LEVEL)

WITH_VARSEQ=0
AC_ARG_WITH(varseq, [  --with-varseq           (OpenCOBOL) define variable sequential format (default 0)],
[case $with_varseq in  
    yes)	AC_MSG_ERROR(["You must give --with-varseq an argument."])
		;;
    no)		AC_MSG_ERROR(["--without-varseq not supported."])
		;;
    [[0-2]])	WITH_VARSEQ=$with_varseq
		;;
    *)		AC_MSG_ERROR(["Invalid --with-varseq argument"])
		;;
esac])
AC_DEFINE(WITH_VARSEQ)

save_libs="$LIBS"

# Checks for getopt_long.
# Cygwin has getopt_long_only prototype in kpathsea/getopt.h
# FreeBSD has gnugetopt library - needs checking

AC_CHECK_FUNC([getopt_long_only],
  [AC_CHECK_HEADERS([kpathsea/getopt.h getopt.h], [], [])],
  [AC_LIBOBJ(getopt)
   AC_LIBOBJ(getopt1)
   AC_LIBSOURCE(getopt.h)])

# Checks for regex. (Not used anymore)
# AC_CHECK_LIB([regex], [regcomp], [LIBCOB_LIBS="$LIBCOB_LIBS -lregex"])

# Checks for db.
USE_DB="no"
if test "$LIBDB" = "1"
then
       AC_CHECK_HEADERS([db1/db.h],
         [AC_DEFINE(HAVE_DB1_DB_H)],
         [AC_MSG_ERROR([db1/db.h is required])])
       AC_CHECK_LIB([db1], [dbopen],
         [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb1"],
         [AC_MSG_ERROR(libdb1 is required)])
elif test "$LIBDB" = "3"
then
	AC_CHECK_HEADERS([db.h db_185.h db3/db_185.h db4/db_185.h db4.1/db_185.h db4.2/db_185.h db4.3/db_185.h db4.4/db_185.h db4.5/db_185.h])
	AC_CHECK_LIB([db-4.5], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.5"],
	  [AC_CHECK_LIB([db-4.5], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.5"], [])])
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db-4.4], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.4"],
	  [AC_CHECK_LIB([db-4.4], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.4"], [])])
	fi
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db-4.3], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.3"],
	  [AC_CHECK_LIB([db-4.3], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.3"], [])])
	fi
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db-4.2], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.2"],
	  [AC_CHECK_LIB([db-4.2], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.2"], [])])
	fi
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db-4.1], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.1"],
	  [AC_CHECK_LIB([db-4.1], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4.1"], [])])
	fi
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db-4], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4"],
	  [AC_CHECK_LIB([db-4], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb-4"], [])])
	fi
	
	if test "$USE_DB" != "yes"
	then
	AC_CHECK_LIB([db], [__db_open],
	[USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
	  [AC_CHECK_LIB([db], [dbopen],
	    [USE_DB="yes" LIBCOB_LIBS="$LIBCOB_LIBS -ldb"],
		[AC_MSG_ERROR(libdb is required)])])
	fi
fi

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
AC_CHECK_LIB([gmp], [__gmp_rand], , AC_MSG_ERROR(GMP 3 or later is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"

# Checks for gettext.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.13)
if test "x$LTLIBINTL" != "x"; then
  COBC_LIBS="$COBC_LIBS $LTLIBINTL"
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for ncurses.
USE_NCURSES="no"
AC_CHECK_HEADERS([ncurses.h pdcurses.h ncurses/ncurses.h curses.h])
AC_CHECK_LIB([ncurses], [initscr])
if test "x$ac_cv_lib_ncurses_initscr" = "xyes"; then
  USE_NCURSES="yes"
  LIBCOB_LIBS="$LIBCOB_LIBS -lncurses"
else
  AC_CHECK_LIB([pdcurses], [initscr])
  if test "x$ac_cv_lib_pdcurses_initscr" = "xyes"; then
    USE_NCURSES="yes"
    LIBCOB_LIBS="$LIBCOB_LIBS -lpdcurses"
  fi
fi

# Checks for programs.
AC_PROG_LN_S
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdint.h sys/types.h signal.h fcntl.h malloc.h locale.h \
	stddef.h wchar.h dlfcn.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
AX_C___ATTRIBUTE__

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp pow setlocale fcntl strerror \
		strcasecmp strchr strrchr strdup strstr strtol gettimeofday])

# AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint __attribute__ ((visibility ("hidden")));]])],
# 	[AC_DEFINE(DLLHIDDEN, __attribute__ ((visibility ("hidden"))))],
# 	[AC_DEFINE(DLLHIDDEN, ,Has visibility attribute)])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[timezone = 3600;]])],
	[AC_DEFINE(HAVE_TIMEZONE, 1)],
	[])

# Checks for dl/ltdl.
AC_RUN_IFELSE([AC_LANG_PROGRAM([[]], [[
  #ifdef _WIN32
   return 0;
  #else
   return 1;
  #endif]])],
  [DEFINE_DL="yes"],
  [],
  [])

if test "$DEFINE_DL" = "no" -a "$COB_TRY_DL" = "yes"
then
  if test "x$ac_cv_header_dlfcn_h" = "xyes"
  then
    AC_CHECK_LIB([c], [dlopen], [DEFINE_DL="yes"], [])
    if test "$DEFINE_DL" = "no"
    then
      AC_CHECK_LIB([dl], [dlopen], [DEFINE_DL="yes"], [])
      if test "$DEFINE_DL" = "yes"
      then
	AC_DEFINE(USE_LIBDL)
	LIBCOB_LIBS="$LIBCOB_LIBS -ldl"
      fi
    else
	AC_DEFINE(USE_LIBDL)
    fi
  fi
fi
if test "$DEFINE_DL" = "no"
then
  AC_CHECK_HEADERS([ltdl.h], , AC_MSG_ERROR(ltdl.h is required))
  AC_CHECK_LIB([ltdl], [lt_dlopen], , AC_MSG_ERROR(libltdl is required))
  LIBCOB_LIBS="$LIBCOB_LIBS -lltdl"
fi

# OpenCOBOL Configuration

if test "x$prefix" = "x/usr" \
     -a "x$includedir" = "x\${prefix}/include"; then
  COB_CFLAGS=""
else
  COB_CFLAGS="-I$includedir"
fi
if test "x$CPPFLAGS" != "x"
then
  COB_CFLAGS="$CPPFLAGS $COB_CFLAGS"
fi

COB_LIBS="-L$libdir -lcob ${LIBCOB_LIBS}"
COB_LDFLAGS=""
if test "x$LDFLAGS" != "x"
then
  COB_LIBS="$LDFLAGS $COB_LIBS"
  COB_LDFLAGS="$LDFLAGS"
fi

COB_CONFIG_DIR="$datadir/$PACKAGE_TARNAME/config"
COB_LIBRARY_PATH=".:$libdir/$PACKAGE_TARNAME"
COB_MODULE_EXT=`echo "$shlibext" | sed 's/dll\.a/dll/'`
LIBS="$save_libs"
COB_EXPORT_DYN="`eval echo $export_dynamic_flag_spec`"
COB_PIC_FLAGS="$lt_prog_compiler_pic"
COB_SHARED_OPT="-shared"
COB_EXEEXT="$EXEEXT"
COB_EXTRA_FLAGS=""

if test "$GEN_DEBUG_OPT" = "no"
then
	CFLAGS=`echo "$CFLAGS " | sed -e 's/-g //'`
fi

# Include CFLAGS in COB_CFLAGS without optimization/debug options
cob_temp_flags=`echo "$CFLAGS " | sed -e 's/-O2 //' | sed -e 's/-O //' | sed -e 's/-g //'`
COB_CFLAGS="$COB_CFLAGS $cob_temp_flags"

if test "$GEN_DEBUG_OPT" = "no"
then
	if test "`basename $COB_CC`" = "gcc"
	then
		cob_temp_flags="`${COB_CC} ${srcdir}/cpucheck.c -o cpucheck 1>/dev/null 2>&1 && ./cpucheck 2>/dev/null`"
		rm -f cpucheck${EXEEXT}
		if test "x$cob_temp_flags" != "x"
		then
			CFLAGS="$CFLAGS $cob_temp_flags -finline-functions -fomit-frame-pointer"
			COB_EXTRA_FLAGS=" $cob_temp_flags"
		else
			CFLAGS="$CFLAGS -finline-functions -fomit-frame-pointer"
		fi
	fi
fi

MYOLDCFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -Wno-pointer-sign"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int testint;]])],
	[AC_DEFINE(HAVE_PSIGN_OPT, 1)],
	[])
CFLAGS="$MYOLDCFLAGS"

# Special stuff

case $host_os in
  cygwin* | mingw*)
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	;;
  darwin* | rhapsody*)
	COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	;;
esac

# Generate the output

AC_DEFINE_UNQUOTED(COB_EXPORT_DYN, "$COB_EXPORT_DYN")
AC_DEFINE_UNQUOTED(COB_PIC_FLAGS, "$COB_PIC_FLAGS")
AC_DEFINE_UNQUOTED(COB_SHARED_OPT, "$COB_SHARED_OPT")
AC_DEFINE_UNQUOTED(COB_EXEEXT, "$COB_EXEEXT")
AC_DEFINE_UNQUOTED(COB_EXTRA_FLAGS, "$COB_EXTRA_FLAGS")
if test "x$striplib" != "x"
then
  AC_DEFINE_UNQUOTED(COB_STRIP_CMD, "$striplib")
# AC_SUBST(COB_STRIP_CMD)
fi
AC_SUBST(COB_CC)
AC_SUBST(COB_CFLAGS)
AC_SUBST(COB_LDFLAGS)
AC_SUBST(COB_LIBS)
AC_SUBST(COB_CONFIG_DIR)
AC_SUBST(COB_LIBRARY_PATH)
AC_SUBST(COB_MODULE_EXT)
AC_SUBST(COBC_LIBS)
AC_SUBST(LIBCOB_CFLAGS)
AC_SUBST(LIBCOB_LIBS)
AC_SUBST(COB_EXPORT_DYN)
AC_SUBST(COB_PIC_FLAGS)
AC_SUBST(COB_SHARED_OPT)
COB_BIGENDIAN="$ac_cv_c_bigendian"
AC_SUBST(COB_BIGENDIAN)

AC_CONFIG_FILES([cob-config
		Makefile lib/Makefile libcob/Makefile
		cobc/Makefile bin/Makefile po/Makefile.in texi/Makefile config/Makefile
		tests/atlocal tests/Makefile tests/cobol85/Makefile])
AC_OUTPUT

echo ""
echo "OpenCOBOL Configuration:"
echo ""
echo "  CC                   ${CC}"
echo "  COB_CC               ${COB_CC}"
echo "  CFLAGS               ${CFLAGS}"
echo "  COB_CFLAGS           ${COB_CFLAGS}"
echo "  COB_EXTRA_FLAGS      ${COB_EXTRA_FLAGS}"
echo "  LDFLAGS              ${LDFLAGS}"
echo "  COB_LDFLAGS          ${COB_LDFLAGS}"
if test "x$COBC_LIBS" != "x"
then
  echo "  COBC_LIBS            ${COBC_LIBS}"
fi
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
if test "${DEFINE_DL}" = "yes" 
then
  echo "  Dynamic loading      System"
else
  echo "  Dynamic loading      Libtool"
fi
echo ""
echo "  Use gettext for international messages:      ${USE_NLS}"
echo "  Use Berkeley DB for file I/O:                ${USE_DB}"
echo "  Use fcntl for file locking:                  ${ac_cv_func_fcntl}"
echo "  Use ncurses/pdcurses for screen I/O:         ${USE_NCURSES}"
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
