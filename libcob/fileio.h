/*
 * Copyright (C) 2002 Keisuke Nishida
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 */

#ifndef COB_FILEIO_H
#define COB_FILEIO_H

#include <stdio.h>
#include <libcob/common.h>

#define COB_EQ	1 	/* x == y */
#define COB_LT	2 	/* x <  y */
#define COB_LE	3 	/* x <= y */
#define COB_GT	4 	/* x >  y */
#define COB_GE	5 	/* x >= y */
#define COB_NE	6 	/* x != y */

#define COB_ASCENDING		1
#define COB_DESCENDING		2

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3
#define COB_ORG_SORT		4
#define COB_ORG_MAX		5

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

#define COB_OPEN_INPUT 		1
#define COB_OPEN_OUTPUT		2
#define COB_OPEN_I_O 		3
#define COB_OPEN_EXTEND		4

#define COB_CLOSE_NORMAL	0
#define COB_CLOSE_REEL		1
#define COB_CLOSE_REEL_REMOVAL	2
#define COB_CLOSE_UNIT		3
#define COB_CLOSE_UNIT_REMOVAL	4
#define COB_CLOSE_NO_REWIND	5
#define COB_CLOSE_LOCK		6

#define COB_FILE_MODE		0644

#define COB_FILE_SUCCEED		00
#define COB_FILE_SUCCEED_OPTIONAL	05
#define COB_FILE_SUCCEED_NO_REEL	07
#define COB_FILE_END_OF_FILE		10
#define COB_FILE_OUT_OF_KEY_RANGE	14
#define COB_FILE_KEY_INVALID		21
#define COB_FILE_KEY_EXISTS		22
#define COB_FILE_KEY_NOT_EXISTS		23
#define COB_FILE_PERMANENT_ERROR	30
#define COB_FILE_NOT_EXISTS		35
#define COB_FILE_PERMISSION_DENIED	37
#define COB_FILE_ALREADY_OPEN		41
#define COB_FILE_NOT_OPEN		42
#define COB_FILE_READ_NOT_DONE		43
#define COB_FILE_RECORD_OVERFLOW	44
#define COB_FILE_READ_ERROR		46
#define COB_FILE_INPUT_DENIED		47
#define COB_FILE_OUTPUT_DENIED		48
#define COB_FILE_I_O_DENIED		49

typedef struct {
  cob_field *field;		/* key field */
  int flag;			/* WITH DUPLICATES (for RELATIVE/INDEXED) */
				/* ASCENDING/DESCENDING (for SORT) */
} cob_file_key;

typedef struct {
  int organization;		/* ORGANIZATION */
  char access_mode;		/* ACCESS MODE */
  char open_mode;		/* OPEN MODE */
  char *file_status;		/* FILE STATUS */
  cob_field *assign;		/* ASSIGN TO */
  cob_field *record;		/* record area */
  cob_field *record_size;	/* record size depending on */
  size_t record_min;		/* record min size */
  size_t record_max;		/* record max size */
  struct {
    char optional    : 1;	/* OPTIONAL */
    char opened      : 1;	/* successfully opened */
    char nonexistent : 1;	/* nonexistent file */
    char end_of_file : 1;	/* reached the end of file */
    char first_read  : 1;	/* first READ after OPEN or START */
    char read_done   : 1;	/* last READ successfully done */
  } f;
  int nkeys;			/* the number of keys */
  cob_file_key *keys;		/* RELATIVE/RECORD keys */
  void *file;			/* file type specific data pointer */
} cob_file;

typedef struct {
  int (*open) (cob_file *f, char *filename, int mode);
  int (*close) (cob_file *f, int opt);
  int (*start) (cob_file *f, int cond, cob_field *key);
  int (*read) (cob_file *f, cob_field *key);
  int (*read_next) (cob_file *f);
  int (*write) (cob_file *f);
  int (*rewrite) (cob_file *f, cob_field *rec);
  int (*delete) (cob_file *f);
} cob_fileio_funcs;

extern cob_file *cob_error_file;
extern char cob_dummy_status[];

extern void cob_init_fileio (void);
extern void cob_default_error_handle (cob_file *f);
extern void cob_open (cob_file *f, int mode);
extern void cob_close (cob_file *f, int opt);
extern void cob_read (cob_file *f, cob_field *key);
extern void cob_write (cob_file *f, cob_field *rec);
extern void cob_write_page (cob_file *f);
extern void cob_write_lines (cob_file *f, int lines);
extern void cob_rewrite (cob_file *f, cob_field *rec);
extern void cob_delete (cob_file *f);
extern void cob_start (cob_file *f, int cond, cob_field *key);

extern void cob_sort_init (cob_file *sort_file, int nkeys, cob_file_key *keys);
extern void cob_sort_using (cob_file *sort_file, cob_file *data_file);
extern void cob_sort_giving (cob_file *sort_file, cob_file *data_file);

#endif /* COB_FILEIO_H_ */
