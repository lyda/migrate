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
#include <db.h>
#include <libcob/common.h>

#define COB_EQ	1 	/* x == y */
#define COB_LT	2 	/* x <  y */
#define COB_LE	3 	/* x <= y */
#define COB_GT	4 	/* x >  y */
#define COB_GE	5 	/* x >= y */
#define COB_NE	6 	/* x != y */

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3

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

#define COB_FILE_SUCCEED		00
#define COB_FILE_SUCCEED_OPTIONAL	05
#define COB_FILE_SUCCEED_NO_REEL	07
#define COB_FILE_END_OF_FILE		10
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

struct cob_file {
  int organization;		/* ORGANIZATION */
  char access_mode;		/* ACCESS MODE */
  char open_mode;		/* OPEN MODE */
  struct cob_field assign;	/* ASSIGN */
  char *file_status;		/* FILE STATUS */
  size_t record_size;		/* record size */
  unsigned char *record_data;	/* record data address */
  int record_min, record_max;	/* record min/max size */
  struct cob_field record_depending; /* record size depending on */
  union {
    int fd;
    FILE *fp;
    DB *db;
  } file;			/* file data pointer */
  /* flags */
  struct {
    char optional    : 1;	/* OPTIONAL */
    char nonexistent : 1;	/* nonexistent file */
    char end_of_file : 1;	/* reached the end of file */
    char first_read  : 1;	/* first READ after OPEN or START */
    char read_done   : 1;	/* last READ successfully done */
    char secondary   : 1;	/* alternative key is in use (INDEXED files) */
  } f;
  /* fields used in RELATIVE files */
  struct cob_field relative_key; /* RELATIVE KEY */
  /* fields used in INDEXED files */
  DBC *cursor;
  struct cob_key {
    struct cob_field field;	/* key field */
    int duplicates;		/* WITH DUPLICATES */
    DB *db;			/* database handler */
  } *keys;
  int nkeys;			/* the number of keys */
  unsigned char *last_key;	/* the last key written */
};

struct cob_fileio_funcs {
  int (*open) (struct cob_file *f, char *filename, int mode);
  int (*close) (struct cob_file *f, int opt);
  int (*start) (struct cob_file *f, int cond, struct cob_field key);
  int (*read) (struct cob_file *f, struct cob_field key);
  int (*read_next) (struct cob_file *f);
  int (*write) (struct cob_file *f, struct cob_field rec);
  int (*rewrite) (struct cob_file *f, struct cob_field rec);
  int (*delete) (struct cob_file *f);
};

extern struct cob_file *cob_last_file;
extern char cob_dummy_status[];

extern void cob_init_fileio (void);
extern void cob_default_error_handle (struct cob_file *f);
extern void cob_open (struct cob_file *f, int mode);
extern void cob_close (struct cob_file *f, int opt);
extern void cob_read (struct cob_file *f, struct cob_field key);
extern void cob_read_next (struct cob_file *f);
extern void cob_write (struct cob_file *f, struct cob_field rec);
extern void cob_write_page (struct cob_file *f);
extern void cob_write_lines (struct cob_file *f, int lines);
extern void cob_rewrite (struct cob_file *f, struct cob_field rec);
extern void cob_delete (struct cob_file *f);
extern void cob_start (struct cob_file *f, int cond, struct cob_field key);

#endif /* COB_FILEIO_H_ */
