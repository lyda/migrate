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

struct cob_file_desc {
  int organization;		/* ORGANIZATION */
  char access_mode;		/* ACCESS MODE */
  char open_mode;		/* OPEN MODE */
  char *file_status;		/* FILE STATUS */
  int record_size;		/* record size */
  unsigned char *record_data;	/* record data address */
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
};

struct cob_fileio_funcs {
  void (*open) (struct cob_file_desc *f, struct cob_field name, int mode);
  void (*close) (struct cob_file_desc *f);
  void (*read) (struct cob_file_desc *f);
  void (*read_next) (struct cob_file_desc *f);
  void (*write) (struct cob_file_desc *f);
  void (*rewrite) (struct cob_file_desc *f);
  void (*delete) (struct cob_file_desc *f);
  void (*start) (struct cob_file_desc *f, int cond, struct cob_field key);
};

extern char cob_dummy_status[];

extern void cob_init_fileio (void);
extern void cob_open (struct cob_file_desc *f, struct cob_field name, int mode);
extern void cob_close (struct cob_file_desc *f);
extern void cob_read (struct cob_file_desc *f);
extern void cob_read_next (struct cob_file_desc *f);
extern void cob_write (struct cob_file_desc *f);
extern void cob_rewrite (struct cob_file_desc *f);
extern void cob_delete (struct cob_file_desc *f);
extern void cob_start (struct cob_file_desc *f, int cond, struct cob_field key);

#endif /* COB_FILEIO_H_ */
