#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2001-2007 Keisuke Nishida
# Copyright (C) 2007 Roger While
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
# along with this program; if not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor
# Boston, MA 02110-1301 USA

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
	if test -f /usr/ccs/bin/ar
	then
		AR="/usr/ccs/bin/ar -X64"
	else
		AR="ar -X64"
	fi
	NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([WITH_DB], [Compile with the Berkeley DB library])
AH_TEMPLATE([USE_DB41], [Use Berkeley DB library new features])
AH_TEMPLATE([WITH_LFS64], [Compile with large file system 64])
AH_TEMPLATE([COB_DEBUG], [Enable debugging features in the compiler])
AH_TEMPLATE([COB_EXPERIMENTAL], [Enable experimemental code (Developers only!)])
AH_TEMPLATE([COB_PARAM_CHECK], [Enable CALL parameter checking])
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
AH_TEMPLATE([COB_HAS_INLINE], [Can use inline keyword])
AH_TEMPLATE([COB_NO_SELFOPEN], [Can not dlopen self])
AH_TEMPLATE([COB_STRFTIME], [Can use strftime for timezone])

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

AC_ARG_ENABLE(experimental,
  [  --enable-experimental   (OpenCOBOL) enable experimental code (Developers only!)],
  [ AC_DEFINE(COB_EXPERIMENTAL)])

AC_ARG_ENABLE(param-check,
  [  --disable-param-check   (OpenCOBOL) disable CALL parameter checking],,
  [ AC_DEFINE(COB_PARAM_CHECK)])

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

# Checks for gmp.
AC_CHECK_HEADERS([gmp.h], , AC_MSG_ERROR(gmp.h is required))
# MYOLDLDFLAGS="$LDFLAGS"
# LDFLAGS="$LDFLAGS -lgmp"
# AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>]], [[
#     mpz_t  myvar;
#     mpz_init (myvar);
#   ]])],
#   [],
#   [AC_MSG_ERROR(GMP 3 or later is required)],
#   [])
# LDFLAGS="$MYOLDLDFLAGS"
AC_CHECK_LIB([gmp], [__gmp_randinit], , AC_MSG_ERROR(GMP 3 or later is required))
LIBCOB_LIBS="$LIBCOB_LIBS -lgmp"

# Checks for gettext.
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.13)
if test "x$LTLIBINTL" != "x"; then
  COBC_LIBS="$COBC_LIBS $LTLIBINTL"
  LIBCOB_LIBS="$LIBCOB_LIBS $LTLIBINTL"
fi

# Checks for ncurses/pdcurses/curses.
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
  else
    AC_CHECK_LIB([curses], [initscr])
    if test "x$ac_cv_lib_curses_initscr" = "xyes"; then
      USE_NCURSES="yes"
      LIBCOB_LIBS="$LIBCOB_LIBS -lcurses"
    fi
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
	stddef.h wchar.h dlfcn.h sys/time.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
# AX_C___ATTRIBUTE__
AC_C_INLINE
if test "$ac_cv_c_inline" != no ; then
	AC_DEFINE(COB_HAS_INLINE,1)
fi


# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp setlocale fcntl strerror \
		strcasecmp strchr strrchr strdup strstr strtol gettimeofday])

# Check for timezone
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[timezone = 3600;]])],
	[AC_DEFINE(HAVE_TIMEZONE, 1)],
	[])

# Checks for internationalization stuff
AM_ICONV
AM_LANGINFO_CODESET

# Checks for db.
USE_DB="no"
HAS_DB41="no"
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
	AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
		#include <stdlib.h>
		#include <db.h>]], [[
		printf("checking for BDB db.h version >= 4.1 ... ");
		#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 0))
		printf("yes (%d.%d)\n", DB_VERSION_MAJOR, DB_VERSION_MINOR);
		return 0;
		#else
		printf("no (%d.%d)\n", DB_VERSION_MAJOR, DB_VERSION_MINOR);
		return 1;
		#endif]])],
		[HAS_DB41="yes"],
		[],
		[])
	if test "$HAS_DB41" = "yes"
	then
		AC_DEFINE(USE_DB41)
		MYOLDLIBS="$LIBS"
		export MYOCLIBS
		for MYOCLIBS in db db-4.5 db-4.4 db-4.3 db-4.2 db-4.1
		do
			LIBS="$MYOLDLIBS -l$MYOCLIBS"
			AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
			#include <stdlib.h>
			#include <db.h>]], [[
			int major, minor, patch;
			printf("checking for BDB 4.1 compatibility in %s ... ", getenv("MYOCLIBS"));
			db_version (&major, &minor, &patch);
			if (major == DB_VERSION_MAJOR && minor == DB_VERSION_MINOR) {
				printf("yes\n");
				return 0;
			}
			printf("no\n");
			return 1;]])],
			[USE_DB="yes"],
			[],
			[])
			if test "$USE_DB" != "no"
			then
				LIBCOB_LIBS="$LIBCOB_LIBS -l$MYOCLIBS"
				break
			fi
		done
		LIBS="$MYOLDLIBS"
		if test "$USE_DB" != "yes"
		then
			AC_MSG_ERROR(Include file db.h implies version >=4.1 but no library found)
		fi
	else
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
#			if test "$HAS_DB41" != "no"
#			then
#				AC_MSG_ERROR(Include file db.h implies version >=4.1 but no library found)
#			fi
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
fi

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

AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
	#include <time.h>]], [[
	time_t	ticks;
	int	ret;
	char	buff[16];
	ticks = time(NULL);
	ret = strftime(buff, 15, "%z", localtime(&ticks));
	if (ret != 5) {
		return 1;
	}
	if (buff[0] != '+' && buff[0] != '-') {
		return 1;
	}
	return 0;]])],
	[AC_DEFINE(COB_STRFTIME)],
	[],
	[])

# OpenCOBOL Configuration

if test "x$prefix" = "xNONE"
then
  if test "x$includedir" = "x\${prefix}/include"
  then
    COB_CFLAGS="-I${ac_default_prefix}/include"
  else
    COB_CFLAGS="-I${includedir}"
  fi
else
  if test "x$includedir" = "x\${prefix}/include"
  then
    if test "x$prefix" = "x/usr"
    then
      COB_CFLAGS=""
    else
      COB_CFLAGS="-I${prefix}/include"
    fi
  else
    COB_CFLAGS="-I${includedir}"
  fi
fi

if test "x$CPPFLAGS" != "x"
then
  COB_CFLAGS="$COB_CFLAGS $CPPFLAGS"
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
	CFLAGS=`echo "$CFLAGS " | sed -e 's/-g / /'`
fi

# Include CFLAGS in COB_CFLAGS without optimization/debug options
cob_temp_flags=`echo "$CFLAGS " | sed -e 's/-O2 / /' | sed -e 's/-O / /' | sed -e 's/-g / /'`
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

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

case $host_os in
  mingw*)
	if test "$prefix" = "NONE"
	then
	COB_CONFIG_DIR="`cd /usr && pwd -W`/local/share/$PACKAGE_TARNAME/config"
	fi
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	;;
  cygwin*)
	COB_EXPORT_DYN="-Wl,--export-all-symbols -Wl,--enable-auto-import"
	;;
  darwin* | rhapsody*)
	COB_SHARED_OPT="-bundle -flat_namespace -undefined suppress"
	;;
  hpux*)
	if test "`basename $CC`" != "gcc"
	then
		COB_SHARED_OPT="-b"
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED],1)
		CFLAGS="$CFLAGS +ESlit -O"
		COB_CFLAGS="$COB_CFLAGS +ESlit -w"
	fi
	;;
  aix*)
	COB_EXPORT_DYN="-Wl,bexpfull"
	if test "`basename $CC`" != "gcc"
	then
		COB_SHARED_OPT="-G"
	fi
	;;
esac

if test "`basename $CC`" = "gcc"
then
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

if test "x$lt_cv_dlopen_self" != "xyes"; then
	AC_DEFINE(COB_NO_SELFOPEN,1)
fi

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
AC_SUBST(COB_HAS_INLINE)

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
echo "  Use enhanced Berkeley DB (>= 4.1)            ${HAS_DB41}"
echo "  Use fcntl for file locking:                  ${ac_cv_func_fcntl}"
echo "  Use ncurses/pdcurses/curses for screen I/O:  ${USE_NCURSES}"
echo ""
