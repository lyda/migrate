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

#include <db1/db.h>

#define min(x,y) ((x)<(y) ? (x) : (y))

#define RTL_FILE_VERSION	3

#define KEY_RETURN		10

#define DTYPE_DISPLAY           '9'
#define DTYPE_ALPHA             'A'
#define DTYPE_BININT            'B'
#define DTYPE_PACKED            'C'
#define DTYPE_ACCEPT_DISPLAY    'D'
#define DTYPE_EDITED            'E'
#define DTYPE_GROUP		'G'
#define DTYPE_FLOAT             'U'
#define DTYPE_ALPHANUMERIC      'X'

#define MAX_DIGITS              18

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

struct fld_desc
{
  unsigned long len;
  char type;
  char decimals;
  char all           : 1;
  char just_r        : 1;
  char separate_sign : 1;
  char leading_sign  : 1;
  char reserved      : 4;
  char *pic;
} __attribute__ ((packed));

#define FIELD_BASE(f) \
  ((f).data + (((f).desc->separate_sign && (f).desc->leading_sign) ? 1 : 0))
#define FIELD_LENGTH(f) \
  ((f).desc->len - ((f).desc->separate_sign ? 1 : 0))


struct file_desc
{
  unsigned char vers_id;
  struct fld_desc *fname_desc;
  signed long reclen;		/* length of record */

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
} __attribute__ ((packed));

struct altkey_desc
{
  short int offset;		/* offset of alternate key field in record */
  struct fld_desc *descriptor;	/* descriptor for this field */
  short int duplicates;		/* = 1 if duplicates allowed */
  DB *alt_dbp;			/* handle for the alternate key file */
} __attribute__ ((packed));

struct scr_desc
{
  int attr;
  int line;
  int column;
  short int foreground;
  short int background;
  void (*process_scr) ();
} __attribute__ ((packed));

struct cob_field {
  struct fld_desc *desc;
  unsigned char *data;
} __attribute__ ((packed));

extern int decimal_comma;
extern unsigned char cCurrencySymbol;
extern long long cob_exp10[19];

extern int extract_sign (struct cob_field f);
extern void put_sign (struct cob_field f, int sign);
extern int picCompLength (const char *pic);
extern int picCompDecimals (const char *pic);

extern void cob_move (struct cob_field f1, struct cob_field f2);
extern void cob_move_2 (struct fld_desc *f1, char *s1, struct fld_desc *f2, char *s2);
extern int get_index (struct cob_field f);
