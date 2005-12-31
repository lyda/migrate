/*
 * Copyright (C) 2002-2006 Keisuke Nishida
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

#include "config.h"

#if WITH_LFS64
#define _LFS64_LARGEFILE     	1
#define _LFS64_STDIO         	1
#define _FILE_OFFSET_BITS	64
#define _LARGEFILE64_SOURCE     1
#endif

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK	1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#ifdef	HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#define	fsync	_commit
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if HAVE_DB1_DB_H
#include <db1/db.h>
#elif HAVE_DB_185_H
#include <db_185.h>
#elif HAVE_DB3_DB_185_H
#include <db3/db_185.h>
#elif HAVE_DB4_DB_185_H
#include <db4/db_185.h>
#elif HAVE_DB4_1_DB_185_H
#include <db4.1/db_185.h>
#elif HAVE_DB4_2_DB_185_H
#include <db4.2/db_185.h>
#elif HAVE_DB4_3_DB_185_H
#include <db4.3/db_185.h>
#elif HAVE_DB4_4_DB_185_H
#include <db4.4/db_185.h>
#elif HAVE_DB4_5_DB_185_H
#include <db4.5/db_185.h>
#elif HAVE_DB_H
#include <db.h>
#endif

#include "common.h"
#include "move.h"
#include "numeric.h"
#include "fileio.h"
#include "byteswap.h"
#include "lib/gettext.h"

/*
#ifdef _WIN32
*/
#if !defined(__linux__)
#define SEEK_INIT(f)	fseek ((FILE *)f->file, (off_t)0, SEEK_CUR)
#else
#define SEEK_INIT(f)
#endif

#ifdef _WIN32
#define INITIAL_FLAGS	O_BINARY
#else
#define INITIAL_FLAGS	0
#endif

cob_file		*cob_error_file;

static int		eop_status = 0;
int			cob_check_eop = 0;

static int		cob_do_sync = 0;
static int		cob_first_in = 0;

static struct file_list {
	struct file_list	*next;
	cob_file		*file;
} *file_cache = NULL;

/* Need some value that does not conflict with errno for OPEN/LINAGE */
#define	COB_LINAGE_INVALID	16384
/* Need value that does not conflict with errno 30 (EROFS) for OPEN */
#define	COB_NOT_CONFIGURED	32768

#define RETURN_STATUS(x)	do { save_status (f, x); return; } while (0)

static const int	status_exception[] = {
	0,				/* 0x */
	COB_EC_I_O_AT_END,		/* 1x */
	COB_EC_I_O_INVALID_KEY,		/* 2x */
	COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
	COB_EC_I_O_LOGIC_ERROR,		/* 4x */
	COB_EC_I_O_RECORD_OPERATION,	/* 5x */
	COB_EC_I_O_FILE_SHARING,	/* 6x */
	COB_EC_I_O,			/* unused */
	COB_EC_I_O,			/* unused */
	COB_EC_I_O_IMP			/* 9x */
};


static int dummy_rn_rew_del (cob_file *f);
static int dummy_read (cob_file *f, cob_field *key);
static int dummy_start (cob_file *f, int cond, cob_field *key);
static int file_open (cob_file *f, char *filename, int mode, int opt);
static int file_close (cob_file *f, int opt);
static int file_write_opt (cob_file *f, int opt);
static int sequential_read (cob_file *f);
static int sequential_write (cob_file *f, int opt);
static int sequential_rewrite (cob_file *f);
static int lineseq_read (cob_file *f);
static int lineseq_write (cob_file *f, int opt);
static int relative_start (cob_file *f, int cond, cob_field *k);
static int relative_read (cob_file *f, cob_field *k);
static int relative_read_next (cob_file *f);
static int relative_write (cob_file *f, int opt);
static int relative_rewrite (cob_file *f);
static int relative_delete (cob_file *f);

#ifdef	WITH_DB

#define DB_PUT(db,flags)	db->put (db, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->seq (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, key, flags)
#define DB_CLOSE(db)		db->close (db)
#define DB_SYNC(db)		db->sync (db, 0)

#define DBT_SET(key,fld)			\
  key.data = fld->data;				\
  key.size = fld->size;

struct indexed_file {
	int key_index;
	unsigned char *last_key;	/* the last key written */
	DB **db;		/* database handlers */
	DBT key, data;
};
static int indexed_open (cob_file *f, char *filename, int mode, int flag);
static int indexed_close (cob_file *f, int opt);
static int indexed_start (cob_file *f, int cond, cob_field *key);
static int indexed_read (cob_file *f, cob_field *key);
static int indexed_read_next (cob_file *f);
static int indexed_write_internal (cob_file *f);
static int indexed_write (cob_file *f, int opt);
static int indexed_delete (cob_file *f);
static int indexed_rewrite (cob_file *f);
static int sort_open (cob_file *f, char *filename, int mode, int flag);
static int sort_close (cob_file *f, int opt);
static int sort_read (cob_file *f);
static int sort_write (cob_file *f, int opt);


static const cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};


static const cob_fileio_funcs sort_funcs = {
	sort_open,
	sort_close,
	dummy_start,
	dummy_read,
	sort_read,
	sort_write,
	dummy_rn_rew_del,
	dummy_rn_rew_del
};

#else	/* WITH_DB */

static int
dummy_open (cob_file *f, char *filename, int mode, int opt)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_write_close (cob_file *f, int opt)
{
	return COB_NOT_CONFIGURED;
}


static cob_fileio_funcs indexed_funcs = {
	dummy_open,
	dummy_write_close,
	dummy_start,
	dummy_read,
	dummy_rn_rew_del,
	dummy_write_close,
	dummy_rn_rew_del,
	dummy_rn_rew_del
};
static cob_fileio_funcs sort_funcs = {
	dummy_open,
	dummy_write_close,
	dummy_start,
	dummy_read,
	dummy_rn_rew_del,
	dummy_write_close,
	dummy_rn_rew_del,
	dummy_rn_rew_del
};

#endif	/* WITH_DB */


static const cob_fileio_funcs sequential_funcs = {
	file_open,
	file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_rn_rew_del
};

static const cob_fileio_funcs lineseq_funcs = {
	file_open,
	file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	dummy_rn_rew_del,
	dummy_rn_rew_del
};

static const cob_fileio_funcs relative_funcs = {
	file_open,
	file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete
};

static const cob_fileio_funcs	*fileio_funcs[COB_ORG_MAX] = {
	&sequential_funcs,
	&lineseq_funcs,
	&relative_funcs,
	&indexed_funcs,
	&sort_funcs
};

static void
cob_sync (cob_file *f, int mode)
{
	if ( f->organization == COB_ORG_INDEXED ) {
#ifdef	WITH_DB
		int			i;
		struct indexed_file	*p = f->file;

		for (i = 0; i < f->nkeys; i++) {
			DB_SYNC (p->db[i]);
		}
		if ( mode == 2 ) {
			for (i = 0; i < f->nkeys; i++) {
				fsync (p->db[i]->fd (p->db[i]));
			}
		}
#endif	/* WITH_DB */
		return;
	}
	if ( f->organization != COB_ORG_SORT ) {
		fflush ((FILE *)f->file);
		if ( mode == 2 ) {
			fsync (fileno ((FILE *)f->file));
		}
	}
}

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for ( l = file_cache; l; l = l->next ) {
		if ( f == l->file ) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;
}

static void
save_status (cob_file *f, int status)
{

	if (f->file_status == NULL)
		f->file_status = cob_malloc (2);

	if (status == COB_NOT_CONFIGURED) {
		status = COB_STATUS_30_PERMANENT_ERROR;
	}
	f->file_status[0] = cob_i2d (status / 10);
	f->file_status[1] = cob_i2d (status % 10);
	cob_error_file = f;
	if (status != COB_STATUS_52_EOP) {
		COB_SET_EXCEPTION (status_exception[status / 10]);
	}
}

/*
 * Regular file
 */

#define FILE_WRITE_AFTER(f,opt)			\
  if (opt & COB_WRITE_AFTER) {			\
    int ret = file_write_opt (f, opt);		\
    if ( ret )					\
       return ret;				\
    f->flag_needs_nl = 1;			\
  }

#define FILE_WRITE_BEFORE(f,opt)		\
  if (opt & COB_WRITE_BEFORE) {			\
    int ret = file_write_opt (f, opt);		\
    if ( ret )					\
       return ret;				\
    f->flag_needs_nl = 0;			\
  }

static int
file_linage_check (cob_file *f)
{
	f->lin_lines = cob_get_int (f->linage);
	if (f->lin_lines < 1) {
		return COB_LINAGE_INVALID;
	}
	if (f->latfoot) {
		f->lin_foot = cob_get_int (f->latfoot);
		if (f->lin_foot < 1 || f->lin_foot > f->lin_lines) {
			return COB_LINAGE_INVALID;
		}
	} else {
		f->lin_foot = 0;
	}
	if (f->lattop) {
		f->lin_top = cob_get_int (f->lattop);
		if (f->lin_top < 0) {
			return COB_LINAGE_INVALID;
		}
	} else {
		f->lin_top = 0;
	}
	if (f->latbot) {
		f->lin_bot = cob_get_int (f->latbot);
		if (f->lin_bot < 0) {
			return COB_LINAGE_INVALID;
		}
	} else {
		f->lin_bot = 0;
	}
	return 0;
}

static int
dummy_rn_rew_del (cob_file *f)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_read (cob_file *f, cob_field *key)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_start (cob_file *f, int cond, cob_field *key)
{
	return COB_NOT_CONFIGURED;
}

static int
file_open (cob_file *f, char *filename, int mode, int opt)
{
	FILE *fp = NULL;

	/* open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
#ifndef	_WIN32
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "r");
		else
#endif
			fp = fopen (filename, "rb");
		break;
	case COB_OPEN_OUTPUT:
		if (f->organization == COB_ORG_RELATIVE)
			fp = fopen (filename, "wb+");
#ifndef	_WIN32
		else if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "w");
#endif
		else
			fp = fopen (filename, "wb");
		break;
	case COB_OPEN_I_O:
#ifndef	_WIN32
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "r+");
		else
#endif
			fp = fopen (filename, "rb+");
		break;
	case COB_OPEN_EXTEND:
#ifndef	_WIN32
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "a+");
		else
#endif
			fp = fopen (filename, "ab+");
		break;
	}
	if (fp == NULL)
		return errno;

	if (mode == COB_OPEN_EXTEND)
		fseek (fp, (off_t) 0, SEEK_END);

#if HAVE_FCNTL
	/* lock the file */
	{
		int		ret;
		struct flock	lock;

		own_memset ((unsigned char *)&lock, 0, sizeof (struct flock));
		lock.l_type = (opt || mode == COB_OPEN_OUTPUT) ? F_WRLCK : F_RDLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		if (fcntl (fileno (fp), F_SETLK, &lock) < 0) {
			ret = errno;
			fclose (fp);
			return ret;
		}
	}
#endif

	f->file = fp;
	if (f->linage) {
		if (file_linage_check (f)) {
			cob_set_int (f->linage_ctr, 0);
			return COB_LINAGE_INVALID;
		}
		f->flag_needs_top = 1;
		cob_set_int (f->linage_ctr, 1);
	}
	return 0;
}

static int
file_close (cob_file *f, int opt)
{
	switch (opt) {
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_LOCK:
	case COB_CLOSE_NO_REWIND:
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl && !f->linage) {
				f->flag_needs_nl = 0;
				putc ('\n', (FILE *)f->file);
			}
		}
#if HAVE_FCNTL
		/* unlock the file */
		{
			struct flock lock;

			own_memset ((unsigned char *)&lock, 0, sizeof (struct flock));
			lock.l_type = F_UNLCK;
			lock.l_whence = SEEK_SET;
			lock.l_start = 0;
			lock.l_len = 0;
			fcntl (fileno ((FILE *)f->file), F_SETLK, &lock);
		}
#endif
		/* close the file */
		fclose ((FILE *)f->file);
		if ( opt == COB_CLOSE_NO_REWIND ) {
			f->open_mode = COB_OPEN_CLOSED;
			return COB_STATUS_07_SUCCESS_NO_UNIT;
		}
		return COB_STATUS_00_SUCCESS;
	default:
		fflush ((FILE *)f->file);
		return COB_STATUS_07_SUCCESS_NO_UNIT;
	}
}

static int
file_write_opt (cob_file *f, int opt)
{
	if (opt & COB_WRITE_PAGE) {
		if (f->linage) {
			int i, n;
			i = cob_get_int (f->linage_ctr);
			if (i == 0) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			n = f->lin_lines;
			for (; i < n; i++) {
				putc ('\n', (FILE *)f->file);
			}
			for (i = 0; i < f->lin_bot; i++) {
				putc ('\n', (FILE *)f->file);
			}
			if (file_linage_check (f)) {
				cob_set_int (f->linage_ctr, 0);
				return COB_STATUS_57_I_O_LINAGE;
			}
			for (i = 0; i < f->lin_top; i++) {
				putc ('\n', (FILE *)f->file);
			}
			cob_set_int (f->linage_ctr, 1);
		} else {
			putc ('\f', (FILE *)f->file);
		}
	} else if (opt & COB_WRITE_LINES) {
		int i, n;
		if (f->linage) {
			n = cob_get_int (f->linage_ctr);
			if (n == 0) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			cob_add_int (f->linage_ctr, opt & COB_WRITE_MASK);
			i = cob_get_int (f->linage_ctr);
			if (cob_check_eop && f->lin_foot) {
				if (i >= f->lin_foot) {
					eop_status = 1;
				}
			}
			if (i > f->lin_lines) {
				if (cob_check_eop) {
					eop_status = 1;
				}
				for (; n < f->lin_lines; n++) {
					putc ('\n', (FILE *)f->file);
				}
				for (i = 0; i < f->lin_bot; i++) {
					putc ('\n', (FILE *)f->file);
				}
				if (file_linage_check (f)) {
					cob_set_int (f->linage_ctr, 0);
					return COB_STATUS_57_I_O_LINAGE;
				}
				cob_set_int (f->linage_ctr, 1);
				for (i = 0; i < f->lin_top; i++) {
					putc ('\n', (FILE *)f->file);
				}
			} else {
				for (i = (opt & COB_WRITE_MASK) - 1; i > 0; i--)
					putc ('\n', (FILE *)f->file);
			}
			cob_check_eop = 0;
		} else {
			for (i = opt & COB_WRITE_MASK; i > 0; i--)
				putc ('\n', (FILE *)f->file);
		}
	}
	return 0;
}

/*
 * SEQUENTIAL
 */

static int
sequential_read (cob_file *f)
{
#if	WITH_VARSEQ == 0 || WITH_VARSEQ == 1
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
#endif

	SEEK_INIT (f);

	/* read the record size */
	if (f->record_min != f->record_max) {
#if	WITH_VARSEQ == 2
		if (fread (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
#else
		if (fread (recsize.sbuff, 4, 1, (FILE *)f->file) != 1) {
#endif
			if (ferror ((FILE *)f->file)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				return COB_STATUS_10_END_OF_FILE;
			}
		}
#if	WITH_VARSEQ == 0
#ifdef WORDS_BIGENDIAN
		f->record->size = recsize.sshort[0];
#else
		f->record->size = COB_BSWAP_16 (recsize.sshort[0]);
#endif
#else
#if	WITH_VARSEQ == 1
#ifdef WORDS_BIGENDIAN
		f->record->size = recsize.sint;
#else
		f->record->size = COB_BSWAP_32 (recsize.sint);
#endif
#endif
#endif
	}

	/* read the record */
	if (fread (f->record->data, f->record->size, 1, (FILE *)f->file) != 1) {
		if (ferror ((FILE *)f->file)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		} else {
			return COB_STATUS_10_END_OF_FILE;
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, int opt)
{
#if	WITH_VARSEQ == 0 || WITH_VARSEQ == 1
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
#endif

	SEEK_INIT (f);

	FILE_WRITE_AFTER (f, opt);

	/* write the record size */
	if (f->record_min != f->record_max) {
#if	WITH_VARSEQ == 2
		if (fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
#else
#if	WITH_VARSEQ == 1
#ifdef WORDS_BIGENDIAN
		recsize.sint = f->record->size;
#else
		recsize.sint = COB_BSWAP_32 ((unsigned int)f->record->size);
#endif
#else
		recsize.sint = 0;
#ifdef WORDS_BIGENDIAN
		recsize.sshort[0] = f->record->size;
#else
		recsize.sshort[0] = COB_BSWAP_16 ((unsigned short)f->record->size);
#endif
#endif
		if (fwrite (recsize.sbuff, 4, 1, (FILE *)f->file) != 1) {
#endif
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* write the record */
	if (fwrite (f->record->data, f->record->size, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	FILE_WRITE_BEFORE (f, opt);

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f)
{
	if (fseek ((FILE *)f->file, -(off_t) f->record->size, SEEK_CUR)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (fwrite (f->record->data, f->record->size, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

/*
 * LINE SEQUENTIAL
 */

static int
lineseq_read (cob_file *f)
{
	size_t		i = 0;
	int		n;
	unsigned char	*dataptr;

	dataptr = f->record->data;
	for ( ; ; ) {
		if ( (n = getc((FILE *)f->file)) == EOF ) {
			return COB_STATUS_10_END_OF_FILE;
		}
		if ( n == '\r' ) {
			continue;
		}
		if ( n == '\n' ) {
			break;
		}
		if ( i < f->record->size ) {
			*dataptr++ = n;
			i++;
		}
	}
	if (i < f->record->size) {
		/* fill the record with spaces */
		own_memset ((unsigned char *)f->record->data + i, ' ', f->record->size - i);
	}

	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_write (cob_file *f, int opt)
{
	int i, size;

	if (opt == 0)
		opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;

	/* determine the size to be written */
	for (i = f->record->size - 1; i >= 0; i--)
		if (f->record->data[i] != ' ')
			break;
	size = i + 1;

	if (f->linage && f->flag_needs_top) {
		f->flag_needs_top = 0;
		for (i = 0; i < f->lin_top; i++) {
			putc ('\n', (FILE *)f->file);
		}
	}
	FILE_WRITE_AFTER (f, opt);

	/* write to the file */
	if (size) {
		if (fwrite (f->record->data, size, 1, (FILE *)f->file) != 1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
/* RXW
	for (i = 0; i < size; i++)
		putc (f->record->data[i], (FILE *)f->file);
*/

	if (f->linage) {
		putc ('\n', (FILE *)f->file);
	}

	FILE_WRITE_BEFORE (f, opt);

	if (eop_status) {
		eop_status = 0;
		cob_exception_code = 0x0502;
		return COB_STATUS_52_EOP;
	}
	return COB_STATUS_00_SUCCESS;
}

/*
 * RELATIVE
 */

static int
relative_start (cob_file *f, int cond, cob_field *k)
{
	int	kindex;
	int	relsize;
	off_t	off;

	/* get the index */
	kindex = cob_get_int (k) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	if (cond == COB_LT)
		kindex--;
	else if (cond == COB_GT)
		kindex++;

	/* seek the index */
	while (1) {
		off = kindex * relsize;
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
		    fread (&f->record->size, sizeof (f->record->size),
			   1, (FILE *)f->file) != 1) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}

		/* check if a valid record */
		if (f->record->size > 0) {
			cob_set_int (k, kindex + 1);
			fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);
			return COB_STATUS_00_SUCCESS;
		}

		/* continue */
		switch (cond) {
		case COB_EQ:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		case COB_LT:
		case COB_LE:
			kindex--;
			break;
		case COB_GT:
		case COB_GE:
			kindex++;
			break;
		}
	}
}

static int
relative_read (cob_file *f, cob_field *k)
{
	int	relnum;
	int	relsize;
	off_t	off;

	SEEK_INIT (f);

	relnum = cob_get_int (k) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
	    fread (&f->record->size, sizeof (f->record->size),
		   1, (FILE *)f->file) != 1) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (f->record->size == 0) {
		fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (fread (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_read_next (cob_file *f)
{
	off_t	off;
	int	relsize;
	int	relnum;

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	while (1) {
		if (fread (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
			if (ferror ((FILE *)f->file)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			} else {
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (f->keys[0].field) {
			if (f->flag_first_read) {
				cob_set_int (f->keys[0].field, 1);
				f->flag_first_read = 0;
			} else {
				off = ftell ((FILE *)f->file);
				relnum = (off / relsize) + 1;
				cob_set_int (f->keys[0].field, 0);
				if (cob_add_int (f->keys[0].field, relnum) != 0) {
					fseek ((FILE *)f->file, -(off_t) sizeof (f->record->size),
					       SEEK_CUR);
					return COB_STATUS_14_OUT_OF_KEY_RANGE;
				}
			}
		}

		if (f->record->size > 0) {
			if (fread (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			return COB_STATUS_00_SUCCESS;
		}

		fseek ((FILE *)f->file, (off_t) f->record_max, SEEK_CUR);
	}
}

static int
relative_write (cob_file *f, int opt)
{
	size_t	size;
	int	relsize;
	int	i;
	off_t	off;

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		int kindex = cob_get_int (f->keys[0].field) - 1;

		if (kindex < 0)
			return COB_STATUS_21_KEY_INVALID;
		off = (off_t) (relsize * kindex);
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0)
			return COB_STATUS_21_KEY_INVALID;
	} else {
		off = ftell((FILE *)f->file);
	}

	if (fread (&size, sizeof (size), 1, (FILE *)f->file) > 0) {
		fseek ((FILE *)f->file, -(off_t) sizeof (size), SEEK_CUR);
		if (size > 0)
			return COB_STATUS_22_KEY_EXISTS;
	} else {
		fseek ((FILE *)f->file, off, SEEK_SET);
	}

	if (fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (fwrite (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	/* update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
/*
			off = ftell ((FILE *)f->file);
*/
			off += relsize;
			i = off / relsize;
			cob_set_int (f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f)
{
	int	relsize;
	int	relnum;
	off_t	off;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		fseek ((FILE *)f->file, -(off_t) f->record_max, SEEK_CUR);
	} else {
		relsize = f->record_max + sizeof (f->record->size);
		relnum = cob_get_int (f->keys[0].field) - 1;
		off = relnum * relsize;
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
		    fread (&f->record->size, sizeof (f->record->size),
			   1, (FILE *)f->file) != 1) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
		}
		SEEK_INIT (f);
	}

	if (fwrite (f->record->data, f->record_max, 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
	int	relsize;
	int	relnum;
	off_t	off;

	relnum = cob_get_int (f->keys[0].field) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	off = relnum * relsize;
	if (fseek ((FILE *)f->file, off, SEEK_SET) != 0 ||
	    fread (&f->record->size, sizeof (f->record->size),
		   1, (FILE *)f->file) != 1) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	fseek ((FILE *)f->file, - (off_t) sizeof (f->record->size), SEEK_CUR);

	f->record->size = 0;
	if (fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	fseek ((FILE *)f->file, (off_t) f->record_max, SEEK_CUR);
	return COB_STATUS_00_SUCCESS;
}

/*
 * INDEXED
 */

#ifdef	WITH_DB

static int
indexed_open (cob_file *f, char *filename, int mode, int flag)
{
	int			i, j;
	int			flags = INITIAL_FLAGS;
	struct indexed_file	*p;

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= O_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= O_RDWR | O_CREAT | O_TRUNC;
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
		flags |= O_RDWR | O_CREAT;
		break;
	}

	p = cob_malloc (sizeof (struct indexed_file));
	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	for (i = 0; i < f->nkeys; i++) {
		BTREEINFO	info;
		char		name[COB_SMALL_BUFF];

		/* file name */
		if (i == 0) {
			strcpy (name, filename);
		} else {
			sprintf (name, "%s.%d", filename, i);
		}

		/* btree info */
		own_memset ((unsigned char *)&info, 0, sizeof (info));
		if (f->keys[i].flag) {
			info.flags = R_DUP;
		}

		/* open db */
		p->db[i] = dbopen (name, flags, COB_FILE_MODE, DB_BTREE, &info);
		if (p->db[i] == 0) {
			int	ret;

			ret = errno;
			for (j = 0; j < i; j++) {
				DB_CLOSE (p->db[j]);
			}
			free (p->db);
			free (p);
			return ret;
		}
	}

	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	own_memset ((unsigned char *)&p->key, 0, sizeof (DBT));
	own_memset ((unsigned char *)&p->data, 0, sizeof (DBT));
	DB_SEQ (p->db[p->key_index], R_FIRST);

	return 0;
}

static int
indexed_close (cob_file *f, int opt)
{
	int			i;
	struct indexed_file	*p = f->file;

	/* close DB's */
	for (i = 0; i < f->nkeys; i++) {
		DB_CLOSE (p->db[i]);
	}

	if (p->last_key) {
		free (p->last_key);
	}
	free (p->db);
	free (p);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_start (cob_file *f, int cond, cob_field *key)
{
	int			ret;
	struct indexed_file	*p = f->file;

	/* look up for the key */
	for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++) {
		if (f->keys[p->key_index].field->data == key->data) {
			break;
		}
	}
#if COB_DEBUG
	if (p->key_index == f->nkeys) {
		cob_runtime_error ("cob_start_indexed: key not found "
				   "(should have been detected by cobc)");
		return 99;
	}
#endif

	/* search */
	DBT_SET (p->key, key);
	ret = DB_SEQ (p->db[p->key_index], R_CURSOR);
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = memcmp (p->key.data, key->data, key->size);
		}
		break;
	case COB_LT:
	case COB_LE:
		if (ret != 0) {
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
		} else if (cond == COB_LT
			   || memcmp (p->key.data, key->data, key->size) != 0) {
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
		}
		break;
	case COB_GT:
		while (ret == 0 && memcmp (p->key.data, key->data, key->size) == 0) {
			ret = DB_SEQ (p->db[p->key_index], R_NEXT);
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	}
	if (ret == 0 && p->key_index > 0) {
		p->key = p->data;
		ret = DB_GET (p->db[0], 0);
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_read (cob_file *f, cob_field *key)
{
	struct indexed_file	*p = f->file;
	int			ret = indexed_start (f, COB_EQ, key);

	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	own_memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_read_next (cob_file *f)
{
	struct indexed_file	*p = f->file;

	if (f->flag_first_read) {
		/* data is read in indexed_open or indexed_start */
		if (p->data.data == 0) {
			return COB_STATUS_10_END_OF_FILE;
		}
	} else {
		if (DB_SEQ (p->db[p->key_index], R_NEXT) != 0) {
			return COB_STATUS_10_END_OF_FILE;
		}
		if (p->key_index > 0) {
			p->key = p->data;
			if (DB_GET (p->db[0], 0) != 0) {
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
	}

	f->record->size = p->data.size;
	own_memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_write_internal (cob_file *f)
{
	int			i;
	struct indexed_file	*p = f->file;

	/* write data */
	p->data.data = f->record->data;
	p->data.size = f->record->size;
	if (DB_PUT (p->db[0], R_NOOVERWRITE) != 0) {
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; i++) {
		int flags = f->keys[i].flag ? 0 : R_NOOVERWRITE;
		DBT_SET (p->key, f->keys[i].field);
		if (DB_PUT (p->db[i], flags) != 0) {
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_write (cob_file *f, int opt)
{
	struct indexed_file	*p = f->file;

	/* check record key */
	DBT_SET (p->key, f->keys[0].field);
	if (!p->last_key) {
		p->last_key = cob_malloc (p->key.size);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL
		 && memcmp (p->last_key, p->key.data, p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	own_memcpy (p->last_key, p->key.data, p->key.size);

	return indexed_write_internal (f);
}

static int
indexed_delete (cob_file *f)
{
	int			i, offset;
	struct indexed_file	*p = f->file;
	DBT			prim_key;

	/* find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
		if (DB_GET (p->db[0], 0) != 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}
	prim_key = p->key;

	/* delete the secondary keys */
	offset = p->data.data - (void *)f->record->data;
	for (i = 1; i < f->nkeys; i++) {
		DBT_SET (p->key, f->keys[i].field);
		p->key.data += offset;
		if (!f->keys[i].flag) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;
			if (DB_SEQ (p->db[i], R_CURSOR) == 0) {
				while (sec_key.size == p->key.size
				       && memcmp (p->key.data, sec_key.data, sec_key.size) == 0)
				{
					if (memcmp (p->data.data, prim_key.data, prim_key.size) == 0) {
						DB_DEL (p->db[i], &p->key, R_CURSOR);
					}
					if (DB_SEQ (p->db[i], R_NEXT) != 0) {
						break;
					}
				}
			}
		}
	}

	/* delete the record */
	DB_DEL (p->db[0], &prim_key, 0);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_rewrite (cob_file *f)
{
	struct indexed_file *p = f->file;

	/* delete the current record */
	int ret = indexed_delete (f);

	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	/* write data */
	DBT_SET (p->key, f->keys[0].field);
	ret = indexed_write_internal (f);

	return ret;
}

/*
 * SORT
 */

struct sort_file {
	DB *db;
	DBT key, data;
};

static cob_file	*current_sort_file;

static int
sort_compare (const DBT * k1, const DBT * k2)
{
	int		cmp;
	unsigned int	i;
	cob_file	*f = current_sort_file;

	for (i = 0; i < f->nkeys; i++) {
		cob_field f1 = *(f->keys[i].field);
		cob_field f2 = *(f->keys[i].field);
		f1.data += ((unsigned char *)k1->data) - f->record->data;
		f2.data += ((unsigned char *)k2->data) - f->record->data;
		cmp = cob_cmp (&f1, &f2);
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	return 1;
}

static int
sort_open (cob_file *f, char *filename, int mode, int flag)
{
	BTREEINFO		info;
	struct sort_file	*p = f->file;
	int			flags = INITIAL_FLAGS;

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= O_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= O_RDWR | O_CREAT | O_TRUNC;
		break;
	}

	/* open db */
	own_memset ((unsigned char *)&info, 0, sizeof (info));
	info.flags = R_DUP;
	info.compare = sort_compare;
	p->db = dbopen (filename, flags, COB_FILE_MODE, DB_BTREE, &info);
	if (p->db == NULL) {
		return errno;
	}

	own_memset ((unsigned char *)&p->key, 0, sizeof (DBT));
	own_memset ((unsigned char *)&p->data, 0, sizeof (DBT));
	return 0;
}

static int
sort_close (cob_file *f, int opt)
{
	struct sort_file *p = f->file;
	DB_CLOSE (p->db);
	return COB_STATUS_00_SUCCESS;
}

static int
sort_read (cob_file *f)
{
	struct sort_file	*p = f->file;

	if (DB_SEQ (p->db, f->flag_first_read ? R_FIRST : R_NEXT) != 0) {
		return COB_STATUS_10_END_OF_FILE;
	}

	f->record->size = p->key.size;
	own_memcpy (f->record->data, p->key.data, p->key.size);
	own_memset (f->record->data + p->key.size, ' ', f->record_max - p->key.size);
	return COB_STATUS_00_SUCCESS;
}

static int
sort_write (cob_file *f, int opt)
{
	struct sort_file	*p = f->file;

	current_sort_file = f;
	p->key.data = f->record->data;
	p->key.size = f->record->size;
	if (DB_PUT (p->db, 0)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

#endif	/* WITH_DB */

/*
 * Public interface
 */

void
cob_open (cob_file *f, int mode, int opt)
{
	int	was_not_exist = 0;
	struct	stat st;
	char	filename[COB_MEDIUM_BUFF];

	f->flag_read_done = 0;

	/* file was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		RETURN_STATUS (COB_STATUS_38_CLOSED_WITH_LOCK);
	}

	/* file is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_41_ALREADY_OPEN);
	}

	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_first_read = 1;

	/* obtain the file name */
	cob_field_to_string (f->assign, filename);
	if (cob_current_module->flag_filename_mapping && f->organization != COB_ORG_SORT) {
		char	buff[COB_MEDIUM_BUFF];
		char	*p;
		char	*src = filename;
		char	*dst = buff;
		int	simple = 1;

		/* expand envoronment variables */
		/* ex. "$TMPDIR/foo" -> "/tmp/foo" */
		while (*src) {
			if (!isalnum (*src) && *src != '_') {
				simple = 0;
			}
			if (*src == '$') {
				int	i = 0;
				char	env[COB_SMALL_BUFF];

				while (isalnum (src[++i])) ;
				memcpy (env, src + 1, i - 1);
				env[i - 1] = 0;
				if ((p = getenv (env)) != NULL) {
					strcpy (dst, p);
					dst += strlen (p);
				}
				src += i;
			} else {
				*dst++ = *src++;
			}
		}
		*dst = 0;
		strcpy (filename, buff);

		/* resolve by environment variables */
		/* ex. "TMPFILE" -> DD_TMPFILE, dd_TMPFILE, or TMPFILE */
		if (simple) {
			int	i;
			char	*prefix[] = { "DD_", "dd_", "", 0 };

			for (i = 0; prefix[i]; i++) {
				sprintf (buff, "%s%s", prefix[i], filename);
				if ((p = getenv (buff)) != NULL) {
					strcpy (filename, p);
					break;
				}
			}
		}
	}

	/* check if the file exists */
	if (stat (filename, &st) == -1 && errno == ENOENT) {
		was_not_exist = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	}

	cob_cache_file (f);

	if ( !cob_first_in ) {
		cob_first_in = 1;
		cob_set_signal ();
	}

	/* open the file */
	switch (fileio_funcs[(int)f->organization]->open (f, filename, mode, opt)) {
	case 0:
		f->open_mode = mode;
		if (f->flag_optional && was_not_exist) {
			RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
		} else {
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		}
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
		} else {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	case EACCES:
	case EISDIR:
	case EROFS:
		RETURN_STATUS (COB_STATUS_37_PERMISSION_DENIED);
	case EAGAIN:
		RETURN_STATUS (COB_STATUS_61_FILE_SHARING);
	case COB_LINAGE_INVALID:
		RETURN_STATUS (COB_STATUS_57_I_O_LINAGE);
	default:
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
	}
}

void
cob_close (cob_file *f, int opt)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_42_NOT_OPEN);
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	if (ret == COB_STATUS_00_SUCCESS) {
		switch (opt) {
		case COB_CLOSE_LOCK:
			f->open_mode = COB_OPEN_LOCKED;
			break;
		default:
			f->open_mode = COB_OPEN_CLOSED;
			break;
		}
	}

	RETURN_STATUS (ret);
}

#if 0
void
cob_unlock (cob_file *f)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		RETURN_STATUS (COB_STATUS_42_NOT_OPEN);
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	RETURN_STATUS (ret);
}
#endif

void
cob_start (cob_file *f, int cond, cob_field *key)
{
	int	ret;

	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (f->flag_nonexistent) {
		RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);
	}

	if (f->open_mode == COB_OPEN_CLOSED
	    || f->open_mode == COB_OPEN_OUTPUT
	    || f->open_mode == COB_OPEN_EXTEND
	    || f->access_mode == COB_ACCESS_RANDOM) {
		RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);
	}

	ret = fileio_funcs[(int)f->organization]->start (f, cond, key);
	if (ret == COB_STATUS_00_SUCCESS) {
		f->flag_end_of_file = 0;
		f->flag_first_read = 1;
	}

	RETURN_STATUS (ret);
}

void
cob_read (cob_file *f, cob_field *key)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->flag_nonexistent) {
		if (f->flag_first_read == 0) {
			RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);
		}
		f->flag_first_read = 0;
		RETURN_STATUS (COB_STATUS_10_END_OF_FILE);
	}

	/* sequential read at the end of file is an error */
	if (key == NULL && f->flag_end_of_file) {
		RETURN_STATUS (COB_STATUS_46_READ_ERROR);
	}

	if (f->open_mode == COB_OPEN_CLOSED
	    || f->open_mode == COB_OPEN_OUTPUT
	    || f->open_mode == COB_OPEN_EXTEND) {
		RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);
	}

	if (key) {
		ret = fileio_funcs[(int)f->organization]->read (f, key);
	} else {
		ret = fileio_funcs[(int)f->organization]->read_next (f);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		if (f->record_size) {
			cob_set_int (f->record_size, f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		f->flag_end_of_file = 1;
		break;
	}

	RETURN_STATUS (ret);
}

void
cob_write (cob_file *f, cob_field *rec, int opt)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->open_mode == COB_OPEN_CLOSED
		    || f->open_mode == COB_OPEN_INPUT
		    || f->open_mode == COB_OPEN_I_O) {
			RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
		}
	} else {
		if (f->open_mode == COB_OPEN_CLOSED
		    || f->open_mode == COB_OPEN_INPUT
		    || f->open_mode == COB_OPEN_EXTEND) {
			RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
		}
	}

	if (f->record_size) {
		f->record->size = cob_get_int (f->record_size);
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
	}

	ret = fileio_funcs[(int)f->organization]->write (f, opt);

	if ( cob_do_sync && ret == 0 ) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_rewrite (cob_file *f, cob_field *rec)
{
	int	ret;
	int	read_done = f->flag_read_done;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED || f->open_mode != COB_OPEN_I_O) {
		RETURN_STATUS (COB_STATUS_49_I_O_DENIED);
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);
	}

	if (f->organization == COB_ORG_SEQUENTIAL) {
		if (f->record->size != rec->size) {
			RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
		}

		if (f->record_size) {
			if (f->record->size != cob_get_int (f->record_size)) {
				RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
			}
		}
	}

	ret = fileio_funcs[(int)f->organization]->rewrite (f);

	if ( cob_do_sync && ret == 0 ) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_delete (cob_file *f)
{
	int	ret;
	int	read_done = f->flag_read_done;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED || f->open_mode != COB_OPEN_I_O) {
		RETURN_STATUS (COB_STATUS_49_I_O_DENIED);
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);
	}

	ret = fileio_funcs[(int)f->organization]->delete (f);

	if ( cob_do_sync && ret == 0 ) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

#ifdef	WITH_DB

static const unsigned char *old_sequence;

#ifndef _WIN32
static int	cob_iteration = 0;
static pid_t	cob_process_id = 0;
#endif

void
cob_sort_init (cob_file *f, int nkeys, const unsigned char *collating_sequence)
{
	char	*s;
	char	filename[COB_MEDIUM_BUFF];


#ifdef _WIN32
	char	tmpdir[COB_MEDIUM_BUFF];

	/* get temporary directory */
	if ((s = getenv ("TMPDIR")) != NULL || (s = getenv ("TMP")) != NULL) {
		strcpy (tmpdir, s);
	} else {
		GetTempPath (COB_MEDIUM_BUFF, tmpdir);
	}
	/* get temporary file name */
	GetTempFileName (tmpdir, "cob", 0, filename);
	DeleteFile (filename);
#else
	if ((s = getenv ("TMPDIR")) == NULL && (s = getenv ("TMP")) == NULL) {
		s = "/tmp";
	}
	if ( cob_process_id == 0 ) {
		cob_process_id = getpid();
	}
	sprintf (filename, "%s/cobsort%d_%d", s, cob_process_id, cob_iteration);
	cob_iteration++;
#endif

	f->assign->size = strlen (filename);
	f->assign->data = (ucharptr)strdup (filename);
	f->file = cob_malloc (sizeof (struct sort_file));
	f->keys = cob_malloc (sizeof (cob_file_key) * nkeys);
	f->nkeys = 0;

	old_sequence = cob_current_module->collating_sequence;
	if (collating_sequence) {
		cob_current_module->collating_sequence = collating_sequence;
	}
}

void
cob_sort_finish (cob_file *f)
{
	unlink ((char *)f->assign->data);
	free (f->assign->data);
	free (f->file);
	free (f->keys);
	cob_current_module->collating_sequence = old_sequence;
}

void
cob_sort_init_key (cob_file *f, int flag, cob_field *field)
{
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].field = field;
	f->nkeys++;
}

void
cob_sort_using (cob_file *sort_file, cob_file *data_file)
{
	cob_open (data_file, COB_OPEN_INPUT, 0);
	while (1) {
		cob_read (data_file, 0);
		if (data_file->file_status[0] != '0') {
			break;
		}
		own_memcpy (sort_file->record->data, data_file->record->data,
			sort_file->record->size);
		cob_write (sort_file, sort_file->record, 0);
	}
	cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_sort_giving (cob_file *sort_file, cob_file *data_file)
{
	cob_open (data_file, COB_OPEN_OUTPUT, 0);
	while (1) {
		cob_read (sort_file, 0);
		if (sort_file->file_status[0] != '0') {
			break;
		}
		own_memcpy (data_file->record->data,
			sort_file->record->data, data_file->record->size);
		cob_write (data_file, data_file->record, 0);
	}
	cob_close (data_file, COB_CLOSE_NORMAL);
}

#endif	/* WITH_DB */

void
cob_default_error_handle (void)
{
	const char	*msg = NULL;
	char		*file_status = cob_error_file->file_status;
	int		status = cob_d2i (file_status[0]) * 10 + cob_d2i (file_status[1]);

	switch (status) {
	case COB_STATUS_10_END_OF_FILE:
		msg = N_("End of file");
		break;
	case COB_STATUS_14_OUT_OF_KEY_RANGE:
		msg = N_("Key out of range");
		break;
	case COB_STATUS_21_KEY_INVALID:
		msg = N_("Key order not ascending");
		break;
	case COB_STATUS_22_KEY_EXISTS:
		msg = N_("Record key already exists");
		break;
	case COB_STATUS_23_KEY_NOT_EXISTS:
		msg = N_("Record key does not exist");
		break;
	case COB_STATUS_30_PERMANENT_ERROR:
		msg = N_("Permanent file error");
		break;
	case COB_STATUS_35_NOT_EXISTS:
		msg = N_("File does not exist");
		break;
	case COB_STATUS_37_PERMISSION_DENIED:
		msg = N_("Permission denied");
		break;
	case COB_STATUS_41_ALREADY_OPEN:
		msg = N_("File already open");
		break;
	case COB_STATUS_42_NOT_OPEN:
		msg = N_("File not open");
		break;
	case COB_STATUS_43_READ_NOT_DONE:
		msg = N_("READ must be executed first");
		break;
	case COB_STATUS_44_RECORD_OVERFLOW:
		msg = N_("Record overflow");
		break;
	case COB_STATUS_46_READ_ERROR:
		msg = N_("Failed to read");
		break;
	case COB_STATUS_47_INPUT_DENIED:
		msg = N_("READ/START not allowed");
		break;
	case COB_STATUS_48_OUTPUT_DENIED:
		msg = N_("WRITE not allowed");
		break;
	case COB_STATUS_49_I_O_DENIED:
		msg = N_("DELETE/REWRITE not allowed");
		break;
/*
	case COB_STATUS_52_EOP:
		break;
*/
	case COB_STATUS_57_I_O_LINAGE:
		msg = N_("LINAGE values invalid");
		break;
	default:
		msg = N_("Unknown file error");
		break;
	}

	if (msg) {
		cob_runtime_error ("%s (STATUS=%02d)", gettext (msg), status);
	}
}

void
cob_init_fileio (void)
{
	char	*s;

	if ((s = getenv ("COB_SYNC")) != NULL) {
		if ( *s == 'Y' || *s == 'y' ) {
			cob_do_sync = 1;
		}
		if ( *s == 'P' || *s == 'p' ) {
			cob_do_sync = 2;
		}
	}
}

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	char			filename[COB_MEDIUM_BUFF];

	for ( l = file_cache; l; l = l->next ) {
		if ( l->file->open_mode != COB_OPEN_CLOSED &&
		     l->file->open_mode != COB_OPEN_LOCKED ) {
			cob_field_to_string (l->file->assign, filename);
			cob_close (l->file, 0);
			fprintf (stderr, "WARNING - Implicit CLOSE of %s\n", filename);
			fflush (stderr);
		}
	}
}
