# check for library db headers
# ----------------------------
AC_DEFUN([AC_HEADER_DB],[
# Step 0: check for library db headers db.h or db_185.h
echo  "Beginning DB library header search sequence "
db_header="0"
db_version="0"
#
# Force the use of library db 2 or 3 
AC_ARG_WITH(with-libdb,
	[  --with-libdb=[ARG]      use DB library version [2 3]],
        ,
        ,
)
#echo  "debug 5a: DB library header option with_libdb:$with_libdb:"
if test "x$with_libdb" != "x"; then
  if test "x$with_libdb" != "x2" && test "x$with_libdb" != "x3"; then
     AC_MSG_ERROR(invalid library db option selected... aborting)
  fi
fi
#echo  "debug 5b: DB library header option with_libdb:$with_libdb:"
if test "x$with_libdb" = "x2"; then
   AC_CHECK_HEADERS("db2/db_185.h", 
           [db_header="2" ],
           AC_MSG_ERROR(header db2/db_185.h for library db version 2.x not found... aborting)
   ) 
fi
if test "x$with_libdb" = "x3"; then
   AC_CHECK_HEADERS("db3/db_185.h", 
           [db_header="3" ],
           AC_MSG_ERROR(header db3/db_185.h for library db version 3.x not found... aborting)
   ) 
fi

# Step 1: check for library db header db_185.h for version 2.x or 3.x
if test "${db_header}" = "0" ; then
   AC_CHECK_HEADERS("db_185.h",
	[ db_header="23"  ], 
	)
fi

# Step 2: if header db_185.h not found check for header db1/db.h for version 1.x
if test "${db_header}" = "0" ; then
   AC_CHECK_HEADERS("db1/db.h", 
                   [ db_header="1" ],
                   ) 
fi

# Step 3: if header db1/db.h not found check for header db3/db_185.h for version 3.x
if test "${db_header}" = "0" ; then
   AC_CHECK_HEADERS("db3/db_185.h", 
                   [db_header="3" ],
                   ) 
fi

# Step 4: if header db3/db_185.h not found check for header db2/db_185.h for version 2.x
if test "${db_header}" = "0" ; then
   AC_CHECK_HEADERS("db2/db_185.h", 
                   [db_header="2" ],
                   ) 
fi

# Step 5: if header db2/db_185.h not found check for header db.h for version 1.x
if test "${db_header}" = "0" ; then
   AC_CHECK_HEADERS("db.h", 
                   [ db_header="10" ],
                   ) 
fi

# Step 6: if no DB headers found abort
if test "${db_header}" = "0" ; then
   AC_MSG_ERROR(library headers (db.h or db_185.h) not found... aborting)
#else
#  echo  "DB library header found db_header state is ${db_header}"
fi

# Step 7: If header db.h found check library db is version 1.x (1.85-2.0)
echo  "Beginning DB library test link sequence "
if test "${db_header}" = "10" ; then

  AC_MSG_CHECKING(if db.h header belongs to version 1.85)
  AC_EGREP_HEADER([dbopen], [db.h],
                 [ AC_MSG_RESULT(yes) ],
                 [ AC_MSG_RESULT(no); 
                   AC_MSG_ERROR(header db.h for library db version 1.85 not found... aborting) ]
                 ) 
                 
  AC_CHECK_LIB(db, dbopen,  
               M_LIBS="$M_LIBS"; db_version="10",
               )
fi
# 

# Step 7a: If header db.h found check library db is version 1.x (1.85-2.0)
if test "${db_header}" = "1" ; then

  AC_MSG_CHECKING(if db.h header belongs to version 1.85)
  AC_EGREP_HEADER([dbopen], [db1/db.h],
                 [ AC_MSG_RESULT(yes) ],
                 [ AC_MSG_RESULT(no); 
                   AC_MSG_ERROR(header db.h for library db version 1.85 not found... aborting) ]
                 ) 
                 
  AC_CHECK_LIB(db1, dbopen,
               [M_LIBS="$M_LIBS"; db_version="1"],
	       [AC_MSG_ERROR(library test link failed for db and/or compatibility API to version 1.85 not found... aborting)]
               )
fi
# 

# Step 7.5: Backup library paths 
LIBS_BK="$LIBS"

# Step 8: If header db_185.h found check library db is version 2.x
if test "${db_header}" = "2" ; then
  echo -n "checking for -ldb2 (with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb2"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db2/db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="2" ], 
            [ echo " no";  db_header="21" ] 
            ) 
fi

# Step 9: If header db_185.h found check library db is version 3.x
if test "${db_header}" = "3" ; then
  echo -n "checking for -ldb3 (with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb3"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db3/db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="3" ], 
            [ echo " no";  db_header="31" ] 
            ) 
fi

# Step 10: If header db_185.h found check if library db is version 2.x
if test "${db_header}" = "23" ; then
  echo -n "checking for -ldb (version 2.x or 3.x with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="23" ], 
            [ echo " no";  db_header="32" ] 
            ) 
fi

# Step 10a: If header db_185.h found check if library db is version 3.x
if test "${db_header}" = "32" ; then
  echo -n "checking for -ldb3 (with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb3"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="32" ], 
            [ echo " no";  db_header="123" ] 
            ) 
fi

# Step 10b: If header db_185.h found check if library db is version 2.x or 3.x called db1
if test "${db_header}" = "123" ; then
  echo -n "checking for -ldb2 (with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb2"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="123" ], 
            [ echo " no"; AC_MSG_ERROR(library test link failed for db3 or db2 and/or compatibility API to version 1.85 not found... aborting) ] 
            ) 
fi

# Step 11: If header db_185.h found check if library db is version 3.x
if test "${db_header}" = "31" ; then
  echo -n "checking for -ldb (version 3.x with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db3/db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="31" ], 
            [ echo " no"; AC_MSG_ERROR(library test link failed for db and/or compatibility API to version 1.85 not found... aborting) ] 
            ) 
fi

# Step 12: If header db_185.h found check if library db is version 3.x
if test "${db_header}" = "21" ; then
  echo -n "checking for -ldb (version 2.x with 1.85 API compatibility)..."
  LIBS="$LIBS_BK -ldb"
AC_TRY_LINK(#define DB_LIBRARY_COMPATIBILITY_API 1
#include <db2/db_185.h>
const char *c1;
int i1;
int i2;
DBTYPE dbv1;
const void *vv;
,  dbopen(c1, i1, i2, dbv1, vv); ,
            [ echo " yes"; db_version="21" ], 
            [ echo " no"; AC_MSG_ERROR(library test link failed for db and/or compatibility API to version 1.85 not found... aborting) ] 
            ) 
fi

#
htg_ld_args_default2=""
htg_ld_args_curses=""
htg_ld_args_db=""
htg_ld_args_readline=""
htg_ld_args_dl=""

# Step 13: Define which header to use 
#echo  "DB library db_version state is ${db_version}"
if test "${db_version}" = "1" ; then
  AC_DEFINE(USE_DB_1) 
  htg_ld_args_db="-ldb1"
fi

if test "${db_version}" = "2" ; then
  AC_DEFINE(USE_DB_2) 
  htg_ld_args_db="-ldb2"
fi

if test "${db_version}" = "23" ; then
  AC_DEFINE(USE_DB_23) 
  htg_ld_args_db="-ldb2"
fi

if test "${db_version}" = "3" ; then
  AC_DEFINE(USE_DB_3) 
  htg_ld_args_db="-ldb3"
fi

if test "${db_version}" = "32" ; then
  AC_DEFINE(USE_DB_23) 
  htg_ld_args_db="-ldb3"
fi

if test "${db_version}" = "10" ; then
  AC_DEFINE(USE_DB) 
  htg_ld_args_db="-ldb"
fi

if test "${db_version}" = "123" ; then
  AC_DEFINE(USE_DB_23) 
  htg_ld_args_db="-ldb"
fi

if test "${db_version}" = "21" ; then
  AC_DEFINE(USE_DB_2) 
  htg_ld_args_db="-ldb"
fi

if test "${db_version}" = "31" ; then
  AC_DEFINE(USE_DB_3) 
  htg_ld_args_db="-ldb"
fi
])
