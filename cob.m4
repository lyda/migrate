# Check for OpenCOBOL
# Usage: AM_PATH_COB([VERSION], [IF-EXITS], [IF-NOT-EXISTS])
AC_DEFUN([AM_PATH_COB],[
  AC_MSG_CHECKING(for OpenCOBOL)
  cob_exists=yes
  cob-config --version > /dev/null 2> /dev/null || cob_exists=no
  if test "x$cob_exists" = "xyes"; then
    COB_CFLAGS="`cob-config --cflags`"
    COB_LIBS="`cob-config --libs`"
    AC_MSG_RESULT(yes)
    ifelse([$2], , :, [$2])
  else
    AC_MSG_RESULT(no)
    ifelse([$3], , :, [$3])
  fi
  AC_SUBST(COB_CFLAGS)
  AC_SUBST(COB_LIBS)
])
