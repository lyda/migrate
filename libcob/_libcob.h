// Copyright (C) 2001, 2000, 1999,  Rildo Pragana, Jim Noeth, 
//               Andrew Cameron, David Essex.
// Copyright (C) 1993, 1991  Rildo Pragana.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// as published by the Free Software Foundation; either version 2.1,
// or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; see the file COPYING.LIB.  If
// not, write to the Free Software Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA

#include "config.h"
#include "libcob.h"

#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <time.h>
#include <stdarg.h>
#include <sys/types.h>
#include <fcntl.h>

/* Set library db headers and version 1.85 compatibility API for versions >= 2.xx */
#ifdef USE_DB
#include <db.h>
#endif

#ifdef USE_DB_1
#include <db1/db.h>
#endif

#ifdef USE_DB_23
#define DB_LIBRARY_COMPATIBILITY_API
#include <db_185.h>
#endif

#ifdef USE_DB_2
#define DB_LIBRARY_COMPATIBILITY_API
#include <db2/db_185.h>
#endif

#ifdef USE_DB_3
#define DB_LIBRARY_COMPATIBILITY_API
#include <db3/db_185.h>
#endif

#include "decimal.h"

union numeric_type {
  double n_double;
  decimal n_decimal;
};

#define min(x,y) ((x)<(y) ? (x) : (y))

#ifdef WANT_DYNAMIC_LIBS
#include <dlfcn.h>
#endif


#define RTL_FILE_VERSION	3

#define KEY_RETURN		10
#define CHR_POSITIVE		'+'
#define CHR_NEGATIVE		'-'
// #define CHR_DECIMAL          '.'
// #define CHR_DEC_GROUP_DEL    ','
#define CHR_DECIMAL		','
#define CHR_DEC_GROUP_DEL	'.'
#define CHR_BLANK		' '
#define CHR_ZERO		'0'

#define DTYPE_DISPLAY           '9'
#define DTYPE_ALPHA             'A'
#define DTYPE_BININT            'B'
#define DTYPE_PACKED            'C'
#define DTYPE_ACCEPT_DISPLAY    'D'
#define DTYPE_EDITED            'E'
#define DTYPE_GROUP		'G'
#define DTYPE_FLOAT             'U'
#define DTYPE_ALPHANUMERIC      'X'

#define DTYPE_COUNT             7
#define MAX_DIGITS              18
#define MAX_INTEGERS            18                          
#define MAX_DECIMALS            18                          

#define RLBUF_SIZE              8192

#define move_bytes 		memmove

#define ORG_INDEXED		1
#define ORG_SEQUENTIAL 		2
#define ORG_RELATIVE 		3
#define ORG_LINESEQUENTIAL 	4

#define ACC_SEQUENTIAL 		1
#define ACC_DYNAMIC 		2
#define ACC_RANDOM 		3
/* 
 The following are used by file open IO routine.
 They indicate that the filename is EXTERNAL.
 The stored filename string is then used as an environment 
 variable name. It is used to determined the actual filename.
 If no environment variable is defined, then the filename
 defaults to the stored name.
*/
#define ACCEV_ENVAR 		5
#define ACCEV_SEQUENTIAL 	6
#define ACCEV_DYNAMIC 		7
#define ACCEV_RANDOM 		8

#define FMOD_INPUT 		1
#define FMOD_IO 		2
#define FMOD_OUTPUT 		3
#define FMOD_EXTEND 		4

/* inspect options */
#define INSPECT_CHARACTERS 	1
#define INSPECT_ALL        	2
#define INSPECT_LEADING    	3
#define INSPECT_FIRST      	4

/* screen attributes */
#define SCR_BLANK_WHEN_ZERO  0x00000001
#define SCR_HIGHLIGHT        0x00000002
#define SCR_LOWLIGHT         0x00000004
#define SCR_UNDERLINE        0x00000008
#define SCR_REVERSE_VIDEO    0x00000010
#define SCR_BLINK            0x00000020
#define SCR_JUST_LEFT        0x00000040
#define SCR_JUST_RIGHT       0x00000080
#define SCR_AUTO             0x00000100
#define SCR_SECURE           0x00000200
#define SCR_REQUIRED         0x00000400
#define SCR_FULL             0x00000800
#define SCR_SIGN_LEADING     0x00001000
#define SCR_SIGN_SEPARATE    0x00002000
#define SCR_SIGN_PRESENT     0x00004000
#define SCR_BELL             0x00008000
#define SCR_BLANK_SCREEN     0x00010000
#define SCR_BLANK_LINE       0x00020000
#define SCR_DISPLAY          0x00040000	/* if set display, else accept */
#define SCR_NOECHO	     0x00080000
#define SCR_UPDATE	     0x00100000

#define RTERR_INVALID_DATA      1
#define RTERR_INVALID_PIC       2
#define RTERR_NO_MEM            3
#define RTERR_DBG_TRACE		4

/* CONDITIONAL */

#define EQUAL   1
#define LESS    2
#define GREATER 4
#define GEQ     5
#define LEQ     3
#define NEQ     6

#pragma pack(1)
struct fld_desc
{
  unsigned long int len;
  char type;
  unsigned char decimals;
  unsigned int all:1;
  unsigned int just_r:1;
  unsigned int separate_sign:1;
  unsigned int leading_sign:1;
  unsigned int reserved:4;
  char *pic;
};

struct file_desc
{
  unsigned char vers_id;
  struct fld_desc *fname_desc;
  short signed reclen;		/* length of record */

  /* 1=INDEXED,2=SEQUENTIAL,3=RELATIVE 4=LINESEQUENTIAL */
  unsigned char organization;

  /* 1=SEQUENTIAL,2=DYNAMIC,3=RANDOM */
  unsigned char access_mode;

  int open_mode;
  DB *dbp;			/* pointer for libdb operations */
  char *start_record;		/* record for start verb control (Andrew Cameron) */
  unsigned int optional:1;
  unsigned int file_missing:1;
  unsigned int with_advancing:1;
  unsigned int adv_before:1;
  unsigned int reserved:4;
	/******* from now on, only present for indexed files *********/
  short unsigned rec_index;	/* offset of index field in record */
  struct fld_desc *ixd_desc;	/* offset (DGROUP) index field descriptor */
  struct altkey_desc *key_in_use;
};

struct altkey_desc
{
  short int offset;		/* offset of alternate key field in record */
  struct fld_desc *descriptor;	/* descriptor for this field */
  short int duplicates;		/* = 1 if duplicates allowed */
  DB *alt_dbp;			/* handle for the alternate key file */
};

struct scr_desc
{
  int attr;
  int line;
  int column;
  short int foreground;
  short int background;
  void (*process_scr) ();
};
#pragma pack()


/* mccntrl.s */
void exit_paragraph (unsigned, unsigned, unsigned);

/* cobmove.c */
void cob_move (struct fld_desc *f1, char *s1, struct fld_desc *f2, char *s2);
void move_edited (struct fld_desc *f1, char *s1, struct fld_desc *f2,
		  char *s2);
void _DUMP_ (unsigned char *caData, char *szCount, char *caOut);
void _FLDDUMP_(struct fld_desc *f, char *c, char *szMsg);
char *mc_picexpand (struct fld_desc *f);
void float2all (struct fld_desc *f1, char *s1, struct fld_desc *f2, char *s2);

/* general.c */
void stop_run (void);
int fldLength (struct fld_desc *f);
int picCompLength (struct fld_desc *f);

/* mcmath.c */
extern char extract_sign (struct fld_desc *f, char *s);
extern void put_sign (struct fld_desc *f, char *s, char sign);
extern int get_index (struct fld_desc *f, char *s);

/* basicio.c */
void newline (int dupon);
void display (struct fld_desc *f, char *s, int dupon);
void display_erase (int dupon);
int accept_time (char *buffer);
int accept_date (char *buffer);
int accept_day (char *buffer);
int accept_day_of_week (char *buffer);
int accept_std (char *buffer, struct fld_desc *f, int echo);
int accept_cmd_line1 (int ac, char **av, struct fld_desc *f, char *buffer);
int accept_env_var1 (struct fld_desc *f, char *buffer);
int accept_cmd_line (int ac, char **av, struct fld_desc *f, char *buffer);
int accept_env_var (struct fld_desc *f, char *buffer, char *evname);

/* fileio.c */
int cob_open (struct file_desc *f, char *record, char *fname, int mode);
int cob_close (struct file_desc *f, char *record);
int cob_read (struct file_desc *f, char *record, ...);
int cob_read_into (struct file_desc *f, char *record, char *buf, ...);
int cob_read_next (struct file_desc *f, char *record, ...);
int cob_read_prev (struct file_desc *f, char *record, ...);
int cob_read_next_into (struct file_desc *f, char *record, char *buf);
int cob_read_prev_into (struct file_desc *f, char *record, char *buf);
int cob_write (struct file_desc *f, char *record, ...);
int cob_save_status (char *status, int rt);

/* screenio.c */
void cob_accept_screen ();
void cob_display_screen ();
void cob_scr_process (int iAttr, int iLine, int iColumn,
		      int iFgColor, int iBgColor,
		      struct fld_desc *fldScr, char *caScr, void *pInfo, ...);
void cob_scr_initialize ();
void cob_init_screen (void);
void do_scrio_finish ();

/* Run Time Error Routines */
void runtime_error (int iErrorNbr, struct fld_desc *pFld, void *pData);

/* EOF _libcob.h */
