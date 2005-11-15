/*
 * Copyright (C) 2002-2005 Keisuke Nishida
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

#include <libcob/common.h>

#define COB_EQ			1 	/* x == y */
#define COB_LT			2 	/* x <  y */
#define COB_LE			3 	/* x <= y */
#define COB_GT			4 	/* x >  y */
#define COB_GE			5 	/* x >= y */
#define COB_NE			6 	/* x != y */

#define COB_ASCENDING		1
#define COB_DESCENDING		2

#define COB_FILE_MODE		0644

/* Organization */

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3
#define COB_ORG_SORT		4
#define COB_ORG_MAX		5

/* Access mode */

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

/* Open mode */

#define COB_OPEN_CLOSED		0
#define COB_OPEN_INPUT 		1
#define COB_OPEN_OUTPUT		2
#define COB_OPEN_I_O 		3
#define COB_OPEN_EXTEND		4
#define COB_OPEN_LOCKED		5

/* Close options */

#define COB_CLOSE_NORMAL	0
#define COB_CLOSE_LOCK		1
#define COB_CLOSE_NO_REWIND	2
#define COB_CLOSE_UNIT		3
#define COB_CLOSE_UNIT_REMOVAL	4

/* Write options */

#define COB_WRITE_MASK		0x0000ffff
#define COB_WRITE_LINES		0x00010000
#define COB_WRITE_PAGE		0x00020000
#define COB_WRITE_AFTER		0x00100000
#define COB_WRITE_BEFORE	0x00200000
#define COB_WRITE_EOP		0x00400000

/* I-O status */

#define COB_STATUS_00_SUCCESS			00
#define COB_STATUS_02_SUCCESS_DUPLICATE		02
#define COB_STATUS_04_SUCCESS_INCOMPLETE	04
#define COB_STATUS_05_SUCCESS_OPTIONAL		05
#define COB_STATUS_07_SUCCESS_NO_UNIT		07
#define COB_STATUS_10_END_OF_FILE		10
#define COB_STATUS_14_OUT_OF_KEY_RANGE		14
#define COB_STATUS_21_KEY_INVALID		21
#define COB_STATUS_22_KEY_EXISTS		22
#define COB_STATUS_23_KEY_NOT_EXISTS		23
#define COB_STATUS_30_PERMANENT_ERROR		30
#define COB_STATUS_31_INCONSISTENT_FILENAME	31
#define COB_STATUS_34_BOUNDARY_VIOLATION	34
#define COB_STATUS_35_NOT_EXISTS		35
#define COB_STATUS_37_PERMISSION_DENIED		37
#define COB_STATUS_38_CLOSED_WITH_LOCK		38
#define COB_STATUS_39_CONFLICT_ATTRIBUTE	39
#define COB_STATUS_41_ALREADY_OPEN		41
#define COB_STATUS_42_NOT_OPEN			42
#define COB_STATUS_43_READ_NOT_DONE		43
#define COB_STATUS_44_RECORD_OVERFLOW		44
#define COB_STATUS_46_READ_ERROR		46
#define COB_STATUS_47_INPUT_DENIED		47
#define COB_STATUS_48_OUTPUT_DENIED		48
#define COB_STATUS_49_I_O_DENIED		49
#define COB_STATUS_52_EOP			52
#define COB_STATUS_57_I_O_LINAGE		57
#define COB_STATUS_61_FILE_SHARING		61


/* File connector */

typedef struct {
	cob_field	*field;	/* key field */
	int		flag;	/* WITH DUPLICATES (for RELATIVE/INDEXED) */
				/* ASCENDING/DESCENDING (for SORT) */
} cob_file_key;

typedef struct {
	char		organization;		/* ORGANIZATION */
	char		access_mode;		/* ACCESS MODE */
	char		open_mode;		/* OPEN MODE */
	char		flag_optional;		/* OPTIONAL */
	char		*file_status;		/* FILE STATUS */
	cob_field	*assign;		/* ASSIGN TO */
	cob_field	*record;		/* record area */
	cob_field	*record_size;		/* record size depending on */
	size_t		record_min;		/* record min size */
	size_t		record_max;		/* record max size */
	int		nkeys;			/* the number of keys */
	cob_file_key	*keys;			/* RELATIVE/RECORD/SORT keys */
	void		*file;			/* file type specific data pointer */
	cob_field	*linage;		/* LINAGE */
	cob_field	*linage_ctr;		/* LINAGE-COUNTER */
	cob_field	*latfoot;		/* LINAGE FOOTING */
	cob_field	*lattop;		/* LINAGE AT TOP */
	cob_field	*latbot;		/* LINAGE AT BOTTOM */
	int		lin_lines;		/* Current Linage */
	int		lin_foot;		/* Current Footage */
	int		lin_top;		/* Current Top */
	int		lin_bot;		/* Current Bottom */
	char		last_open_mode;		/* open mode given by OPEN */
	char		flag_nonexistent;	/* nonexistent file */
	char		flag_end_of_file;	/* reached the end of file */
	char		flag_first_read;	/* first READ after OPEN or START */
	char		flag_read_done;		/* last READ successfully done */
	char		flag_has_status;	/* has FILE STATUS clause */
	char		flag_needs_nl;		/* LS file needs NL at close */
	char		flag_needs_top;		/* Linage needs top */
} cob_file;

/* File I-O functions */

typedef struct {
	int	(*open) (cob_file *f, char *filename, int mode, int opt);
	int	(*close) (cob_file *f, int opt);
	int	(*start) (cob_file *f, int cond, cob_field *key);
	int	(*read) (cob_file *f, cob_field *key);
	int	(*read_next) (cob_file *f);
	int	(*write) (cob_file *f, int opt);
	int	(*rewrite) (cob_file *f);
	int	(*delete) (cob_file *f);
} cob_fileio_funcs;

extern cob_file	*cob_error_file;

extern int	cob_check_eop;

extern void cob_init_fileio (void);
extern void cob_exit_fileio (void);
extern void cob_default_error_handle (void);
extern void cob_open (cob_file *f, int mode, int opt);
extern void cob_close (cob_file *f, int opt);
extern void cob_read (cob_file *f, cob_field *key);
extern void cob_write (cob_file *f, cob_field *rec, int opt);
extern void cob_rewrite (cob_file *f, cob_field *rec);
extern void cob_delete (cob_file *f);
extern void cob_start (cob_file *f, int cond, cob_field *key);

extern void cob_sort_init (cob_file *f, int nkeys, const unsigned char *collating_sequence);
extern void cob_sort_finish (cob_file *f);
extern void cob_sort_init_key (cob_file *f, int flag, cob_field *field);
extern void cob_sort_using (cob_file *sort_file, cob_file *data_file);
extern void cob_sort_giving (cob_file *sort_file, cob_file *data_file);

#endif /* COB_FILEIO_H_ */
