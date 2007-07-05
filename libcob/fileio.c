/*
 * Copyright (C) 2002-2007 Keisuke Nishida
 * Copyright (C) 2007 Roger While
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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#ifdef	WITH_LFS64
#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef _AIX
#define _LARGE_FILES			1
#endif /* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif
#endif /* WITH_LFS64 */

#ifdef __MINGW32__
#define __USE_MINGW_FSEEK	1
#endif /* __MINGW32__ */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#ifdef	HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#include <sys/stat.h>

#ifdef _WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#include <direct.h>
#define	fsync	_commit
#define	getcwd	_getcwd
#define	chdir	_chdir
#define	mkdir	_mkdir
#define	rmdir	_rmdir
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef	WITH_DB
#ifdef	USE_DB41
#include <db.h>
#else
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
#endif	/* USE_DB41 */
#endif	/* WITH_DB */

#ifdef	WITH_CISAM
#include <isam.h>
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
#endif

#if defined(__hpux__) || defined(_AIX) || defined(__sparc)
#define fseek fseeko
#define ftell ftello
#endif

#if WITH_LFS64
#if defined(_MSC_VER)
#define fseek _fseeki64
#define ftell _ftelli64
#define lseek _lseeki64
#define off_t __int64
#endif
#endif

#include "common.h"
#include "coblocal.h"
#include "move.h"
#include "numeric.h"
#include "fileio.h"
#include "byteswap.h"
/*
#include "lib/gettext.h"
*/

#if !defined(__linux__)
#define SEEK_INIT(f)	fseek ((FILE *)f->file, (off_t)0, SEEK_CUR)
#else
#define SEEK_INIT(f)
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#ifndef	O_LARGEFILE
#define	O_LARGEFILE	0
#endif

#ifdef _WIN32
#define INITIAL_FLAGS	O_BINARY
#else
#define INITIAL_FLAGS	0
#endif

/* SORT definitions */

#define COBSORTEND        1
#define COBSORTABORT      2
#define COBSORTFILEERR    3
#define COBSORTNOTOPEN    4

struct cobitem {
	struct cobitem		*next;
	size_t			end_of_block;
	unsigned char		block_byte;
	unsigned char		unique[sizeof(size_t)];
	unsigned char		item[1];
};

struct memory_struct {
	struct cobitem	*first;
	struct cobitem	*last;
	size_t		count;
};

struct file_struct {
	FILE		*fp;
	size_t		count;	/* count of items in temporary files */
};

struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	size_t			unique;
	size_t			retrieving;
	size_t			files_used;
	size_t			size;
	size_t			r_size;
	size_t			w_size;
	size_t			memory;
	int			destination_file;
	int			retrieval_queue;
	struct memory_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */

cob_file		*cob_error_file;

#ifndef _WIN32
static int		cob_iteration = 0;
static pid_t		cob_process_id = 0;
#endif

static int		eop_status = 0;
int			cob_check_eop = 0;

static int		cob_do_sync = 0;
static int		cob_first_in = 0;
static int		cob_sort_memory = 128*1024*1024;
#ifdef	USE_DB41
static int		cob_isam_cache = 8*1024*1024;
static DB_ENV		*bdb_env = NULL;
static unsigned int	bdb_lock_id;
static const char	**bdb_data_dir = NULL;
static void		*record_lock_object;
static int		rlo_size = 0;
#endif

static struct file_list {
	struct file_list	*next;
	cob_file		*file;
} *file_cache = NULL;

static char		*cob_file_path;

/* Need some value that does not conflict with errno for OPEN/LINAGE */
#define	COB_LINAGE_INVALID	16384
/* Need value that does not conflict with errno 30 (EROFS) for OPEN */
#define	COB_NOT_CONFIGURED	32768

#define RETURN_STATUS(x)	do { save_status (f, x, fnstatus); return; } while (0)

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

static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof(prefix) / sizeof(char *)

static const char	parm_msg[] = "CALL to %s requires %d parameters";

static int dummy_rnxt_del (cob_file *f);
static int dummy_rewrite (cob_file *f, int opt);
static int dummy_read (cob_file *f, cob_field *key, int read_opts);
static int dummy_start (cob_file *f, int cond, cob_field *key);
static int file_open (cob_file *f, char *filename, int mode, int opt);
static int file_close (cob_file *f, int opt);
static int file_write_opt (cob_file *f, const int opt);
static int sequential_read (cob_file *f, int read_opts);
static int sequential_write (cob_file *f, int opt);
static int sequential_rewrite (cob_file *f, int opt);
static int lineseq_read (cob_file *f, int read_opts);
static int lineseq_write (cob_file *f, int opt);
static int relative_start (cob_file *f, int cond, cob_field *k);
static int relative_read (cob_file *f, cob_field *k, int read_opts);
static int relative_read_next (cob_file *f, int read_opts);
static int relative_write (cob_file *f, int opt);
static int relative_rewrite (cob_file *f, int opt);
static int relative_delete (cob_file *f);

#if	defined(WITH_DB) || defined(WITH_CISAM) || defined(WITH_VBISAM)

#ifdef	WITH_DB
#ifdef	USE_DB41
#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#define	cob_dbtsize_t		u_int32_t
#else
#define DB_PUT(db,flags)	db->put (db, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->seq (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, key, flags)
#define DB_CLOSE(db)		db->close (db)
#define DB_SYNC(db)		db->sync (db, 0)
#define DB_FIRST		R_FIRST
#define DB_LAST			R_LAST
#define DB_NEXT			R_NEXT
#define DB_PREV			R_PREV
#define	cob_dbtsize_t		size_t
#endif

#define DBT_SET(key,fld)			\
  key.data = fld->data;				\
  key.size = (cob_dbtsize_t) fld->size

struct indexed_file {
	size_t		key_index;
	unsigned char	*last_key;	/* the last key written */
	unsigned char	*temp_key;	/* used for temporary storage */
	DB		**db;		/* database handlers */
	DBT		key;
	DBT		data;
	unsigned char	**last_readkey;	/* the last key read */
	unsigned int	*last_dupno;	/* the last number of duplicates read */
	int		*rewrite_sec_key;
#ifdef	USE_DB41
	DBC		**cursor;
	DB_LOCK		bdb_file_lock;
	char		*filename;	/*needed for record locks*/
	DB_LOCK		bdb_record_lock;
	int		write_cursor_open;
	unsigned int	bdb_lock_id;
	int		record_locked;
	int		filenamelen;
#endif
};
#endif	/* WITH_DB */

static int indexed_open (cob_file *f, char *filename, int mode, int flag);
static int indexed_close (cob_file *f, int opt);
static int indexed_start (cob_file *f, int cond, cob_field *key);
static int indexed_read (cob_file *f, cob_field *key, int read_opts);
static int indexed_read_next (cob_file *f, int read_opts);
static int indexed_write_internal (cob_file *f, int rewrite, int opt);
static int indexed_write (cob_file *f, int opt);
static int indexed_delete_internal (cob_file *f, int rewrite);
static int indexed_delete (cob_file *f);
static int indexed_rewrite (cob_file *f, int opt);

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

#else	/* WITH_DB || WITH_CISAM || WITH_VBISAM */

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
	dummy_rnxt_del,
	dummy_write_close,
	dummy_rewrite,
	dummy_rnxt_del
};

#endif	/* WITH_DB || WITH_CISAM || WITH_VBISAM */


static const cob_fileio_funcs sequential_funcs = {
	file_open,
	file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_rnxt_del
};

static const cob_fileio_funcs lineseq_funcs = {
	file_open,
	file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	dummy_rewrite,
	dummy_rnxt_del
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
	NULL
};

static void COB_NOINLINE
cob_sync (cob_file *f, int mode)
{
#ifdef	WITH_DB
	size_t			i;
	struct indexed_file	*p;
#ifdef	USE_DB41
	int			n;
#endif
#endif
	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		p = f->file;
		for (i = 0; i < f->nkeys; i++) {
			DB_SYNC (p->db[i]);
		}
		if (mode == 2) {
			for (i = 0; i < f->nkeys; i++) {
#ifdef	USE_DB41
				fsync (p->db[i]->fd (p->db[i], &n));
#else
				fsync (p->db[i]->fd (p->db[i]));
#endif
			}
		}
#endif	/* WITH_DB */
		return;
	}
	if (f->organization != COB_ORG_SORT) {
		fflush ((FILE *)f->file);
		if (mode == 2) {
			fsync (fileno ((FILE *)f->file));
		}
	}
}

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (f == l->file) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;
}

static void
save_status (cob_file *f, int status, cob_field *fnstatus)
{

/* File status is an attribute of the program,
   not an attribute of the file structure
	if (f->file_status == NULL)
		f->file_status = cob_malloc (2);
*/

	if (status == COB_NOT_CONFIGURED) {
		status = COB_STATUS_30_PERMANENT_ERROR;
	}
	f->file_status[0] = cob_i2d (status / 10);
	f->file_status[1] = cob_i2d (status % 10);
	if (fnstatus) {
		own_memcpy (fnstatus->data, f->file_status, 2);
	}
	cob_error_file = f;
	if (status != COB_STATUS_52_EOP) {
		cob_set_exception (status_exception[status / 10]);
	}
}

/*
 * Regular file
 */

#define FILE_WRITE_AFTER(f,opt)			\
  if (opt & COB_WRITE_AFTER) {			\
    int ret = file_write_opt (f, opt);		\
    if (ret) {					\
       return ret;				\
    }						\
    f->flag_needs_nl = 1;			\
  }

#define FILE_WRITE_BEFORE(f,opt)		\
  if (opt & COB_WRITE_BEFORE) {			\
    int ret = file_write_opt (f, opt);		\
    if (ret) {					\
       return ret;				\
    }						\
    f->flag_needs_nl = 0;			\
  }

static int COB_NOINLINE
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
dummy_rnxt_del (cob_file *f)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_rewrite (cob_file *f, int opt)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_read (cob_file *f, cob_field *key, int read_opts)
{
	return COB_NOT_CONFIGURED;
}

static int
dummy_start (cob_file *f, int cond, cob_field *key)
{
	return COB_NOT_CONFIGURED;
}

static int COB_NOINLINE
file_open (cob_file *f, char *filename, int mode, int opt)
{
	FILE		*fp = NULL;
#ifdef HAVE_FCNTL
	int		ret;
	struct flock	lock;
#endif

	/* open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "r");
		else
#endif
			fp = fopen (filename, "rb");
		break;
	case COB_OPEN_OUTPUT:
		if (f->organization == COB_ORG_RELATIVE)
			fp = fopen (filename, "wb+");
#if !defined(_WIN32) || defined(_MSC_VER)
		else if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "w");
#endif
		else
			fp = fopen (filename, "wb");
		break;
	case COB_OPEN_I_O:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "r+");
		else
#endif
			fp = fopen (filename, "rb+");
		break;
	case COB_OPEN_EXTEND:
#if !defined(_WIN32) || defined(_MSC_VER)
		if (f->organization == COB_ORG_LINE_SEQUENTIAL)
			fp = fopen (filename, "a+");
		else
#endif
			fp = fopen (filename, "ab+");
		break;
	}
	if (fp == NULL) {
		return errno;
	}

	if (mode == COB_OPEN_EXTEND) {
		fseek (fp, (off_t) 0, SEEK_END);
	}

#ifdef HAVE_FCNTL
	/* lock the file */
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

static int COB_NOINLINE
file_close (cob_file *f, int opt)
{
#ifdef HAVE_FCNTL
	struct flock lock;
#endif

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
#ifdef HAVE_FCNTL
		/* unlock the file */
		own_memset ((unsigned char *)&lock, 0, sizeof (struct flock));
		lock.l_type = F_UNLCK;
		lock.l_whence = SEEK_SET;
		lock.l_start = 0;
		lock.l_len = 0;
		fcntl (fileno ((FILE *)f->file), F_SETLK, &lock);
#endif
		/* close the file */
		fclose ((FILE *)f->file);
		if (opt == COB_CLOSE_NO_REWIND) {
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
file_write_opt (cob_file *f, const int opt)
{
	int	i, n;

	if (unlikely(opt & COB_WRITE_PAGE)) {
		if (unlikely(f->linage)) {
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
		if (unlikely(f->linage)) {
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
sequential_read (cob_file *f, int read_opts)
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
		if (unlikely(fread (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1)) {
#else
		if (unlikely(fread (recsize.sbuff, 4, 1, (FILE *)f->file) != 1)) {
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
	if (unlikely(fread (f->record->data, f->record->size, 1, (FILE *)f->file) != 1)) {
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
		if (unlikely(fwrite (&f->record->size, sizeof (f->record->size), 1, (FILE *)f->file) != 1)) {
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
		if (unlikely(fwrite (recsize.sbuff, 4, 1, (FILE *)f->file) != 1)) {
#endif
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* write the record */
	if (unlikely(fwrite (f->record->data, f->record->size, 1, (FILE *)f->file) != 1)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	FILE_WRITE_BEFORE (f, opt);

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f, int opt)
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
lineseq_read (cob_file *f, int read_opts)
{
	size_t		i = 0;
	int		n;
	unsigned char	*dataptr;

	dataptr = f->record->data;
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (unlikely(n == EOF)) {
			if (!i) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (n == '\r') {
			continue;
		}
		if (n == '\n') {
			break;
		}
		if (likely(i < f->record->size)) {
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
	int	i;
	size_t	size;

	if (opt == 0) {
		opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
	}

	/* determine the size to be written */
	for (i = (int)f->record->size - 1; i >= 0; i--) {
		if (f->record->data[i] != ' ') {
			break;
		}
	}
	size = i + 1;

	if (unlikely(f->linage && f->flag_needs_top)) {
		f->flag_needs_top = 0;
		for (i = 0; i < f->lin_top; i++) {
			putc ('\n', (FILE *)f->file);
		}
	}
	FILE_WRITE_AFTER (f, opt);

	/* write to the file */
	if (size) {
		if (unlikely(fwrite (f->record->data, size, 1, (FILE *)f->file) != 1)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	if (unlikely(f->linage)) {
		putc ('\n', (FILE *)f->file);
	}

	FILE_WRITE_BEFORE (f, opt);

	if (unlikely(eop_status)) {
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
	size_t	relsize;
	off_t	off;

	/* get the index */
	kindex = cob_get_int (k) - 1;
	relsize = f->record_max + sizeof (f->record->size);
	if (cond == COB_LT) {
		kindex--;
	} else if (cond == COB_GT) {
		kindex++;
	}

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
relative_read (cob_file *f, cob_field *k, int read_opts)
{
	int	relnum;
	size_t	relsize;
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
relative_read_next (cob_file *f, int read_opts)
{
	off_t	off;
	size_t	relsize;
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
				relnum = (int)((off / relsize) + 1);
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
	size_t	relsize;
	int	i;
	int	kindex;
	off_t	off;

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int (f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_21_KEY_INVALID;
		}
		off = (off_t) (relsize * kindex);
		if (fseek ((FILE *)f->file, off, SEEK_SET) != 0) {
			return COB_STATUS_21_KEY_INVALID;
		}
	} else {
		off = ftell ((FILE *)f->file);
	}

	if (fread (&size, sizeof (size), 1, (FILE *)f->file) > 0) {
		fseek ((FILE *)f->file, -(off_t) sizeof (size), SEEK_CUR);
		if (size > 0) {
			return COB_STATUS_22_KEY_EXISTS;
		}
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
			i = (int)(off / relsize);
			cob_set_int (f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f, int opt)
{
	size_t	relsize;
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
	size_t	relsize;
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

#ifdef	USE_DB41
static void
join_environment (void)
{
	int		flags, ret;
	char		*home;

	home = getenv ("DB_HOME");
	if (home == NULL) {
		return;
	}
	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error ("Can't join BDB environment, env_create: %d %s\n", ret, db_strerror (ret));
		cob_stop_run (1);
	}
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
#endif
	bdb_env->set_cachesize (bdb_env, 0, cob_isam_cache, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, home, flags, 0);
	if (ret) {
		cob_runtime_error ("Can't join BDB environment, env_open: %d %s\n", ret, db_strerror (ret));
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
	return;
}

static int
lock_record (cob_file *f, char *key, int keylen)
{
	int			ret, len;
	DBT			dbt;
	struct indexed_file	*p = f->file;
	
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	own_memcpy ((char *)record_lock_object, p->filename, p->filenamelen + 1);
	own_memcpy ((char *)record_lock_object + p->filenamelen + 1, key, keylen);
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT, 
				&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
	if (!ret) {
		p->record_locked = 1;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, char *key, int keylen)
{
	int			ret, len;
	DBT			dbt;
	DB_LOCK			test_lock;
	struct indexed_file	*p = f->file;
	
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	own_memcpy ((char *)record_lock_object, p->filename, p->filenamelen + 1);
	own_memcpy ((char *)record_lock_object + p->filenamelen + 1, key, keylen);
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
	ret = bdb_env->lock_get (bdb_env, p->bdb_lock_id, DB_LOCK_NOWAIT, 
				&dbt, DB_LOCK_WRITE, &test_lock);
	if (!ret) {
		bdb_env->lock_put (bdb_env, &test_lock);
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	int			ret;
	struct indexed_file	*p = f->file;
	
	if (p->record_locked == 0) {
		return 0;
	}
	ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
	p->record_locked = 0;
	return ret;
}
#endif	/* USE_DB41 */


static int
indexed_open (cob_file *f, char *filename, int mode, int flag)
{
	size_t			i, j;
#ifdef	USE_DB41
	int			flags = 0;
	int			lock_mode;
	int			handle_created;
#else
	int			flags = INITIAL_FLAGS;
#endif
	int			ret = 0;
	struct indexed_file	*p;
	size_t			maxsize;

	p = cob_malloc (sizeof (struct indexed_file));
#ifdef	USE_DB41
	if (bdb_env != NULL) {
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    f->lock_mode == COB_LOCK_EXCLUSIVE ||
		    (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
		} else {
			lock_mode = DB_LOCK_READ;
		}
		p->key.size = (cob_dbtsize_t) strlen (filename);
		p->key.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT, 
					&p->key, lock_mode, &p->bdb_file_lock);
		if (ret) {
			free (p);
			if (ret == DB_LOCK_NOTGRANTED) {
				ret = COB_STATUS_61_FILE_SHARING;
			}
			return ret;
		}
	}
#endif

	switch (mode) {
	case COB_OPEN_INPUT:
#ifdef	USE_DB41
		flags |= DB_RDONLY;
#else
		flags |= O_RDONLY;
#endif
		break;
	case COB_OPEN_OUTPUT:
#ifdef	USE_DB41
		flags |= DB_CREATE;
#else
		flags |= O_RDWR | O_CREAT | O_TRUNC;
#endif
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
#ifdef	USE_DB41
		flags |= DB_CREATE;
#else
		flags |= O_RDWR | O_CREAT;
#endif
		break;
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
#ifdef	USE_DB41
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
#endif
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = 0;
	for (i = 0; i < f->nkeys; i++) {
		if (f->keys[i].field->size > maxsize) {
			maxsize = f->keys[i].field->size;
		}
	}
	for (i = 0; i < f->nkeys; i++) {
#ifndef	USE_DB41
		BTREEINFO	info;
#endif
		char		name[COB_SMALL_BUFF];

		/* file name */
		if (i == 0) {
			strcpy (name, filename);
		} else {
			sprintf (name, "%s.%d", filename, (int)i);
		}

		/* btree info */
#ifdef	USE_DB41
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					bdb_env->dbremove (bdb_env, NULL, name, NULL, 0);
				} else {
					p->db[i]->remove (p->db[i], name, NULL, 0);
					ret = db_create (&p->db[i], bdb_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].flag) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
#else
		own_memset ((unsigned char *)&info, 0, sizeof (info));
		if (f->keys[i].flag) {
			info.flags = R_DUP;
		}
#endif

		/* open db */
#ifdef	USE_DB41
		if (!ret) {
			ret = p->db[i]->open (p->db[i], NULL, name, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
#else
		p->db[i] = dbopen (name, flags, COB_FILE_MODE, DB_BTREE, &info);
		if (p->db[i] == 0) {
			ret = errno;
		}
#endif
		if (ret) {
			for (j = 0; j < i; j++) {
				DB_CLOSE (p->db[j]);
			}
#ifdef	USE_DB41
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
#endif
			free (p->db);
			free (p->last_readkey);
			free (p->last_dupno);
#ifdef	USE_DB41
			free (p->cursor);
			if (bdb_env != NULL) {
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			}
#endif
			free (p);
			return ret;

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof(unsigned int));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	own_memset ((unsigned char *)&p->key, 0, sizeof (DBT));
	own_memset ((unsigned char *)&p->data, 0, sizeof (DBT));
#ifdef	USE_DB41
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	DBT_SET (p->key, f->keys[0].field);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
#else
	ret = DB_SEQ (p->db[p->key_index], R_FIRST);
#endif
	if (!ret) {
		own_memcpy (p->last_readkey[0], p->key.data, p->key.size);
	} else {
		p->data.data = 0;
	}

	return 0;
}

static int
indexed_close (cob_file *f, int opt)
{
	size_t			i;
	struct indexed_file	*p = f->file;

	/* close DB's */
#ifdef	USE_DB41
	for (i = 0; i < f->nkeys; i++) {
		if (p->cursor[i]) {
			p->cursor[i]->c_close (p->cursor[i]);
		}
	}
#endif
	for (i = 0; i < f->nkeys; i++) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		free (p->last_readkey[i]);
		free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		free (p->last_key);
	}
	free (p->temp_key);
	free (p->db);
	free (p->last_readkey);
	free (p->last_dupno);
#ifdef	USE_DB41
	free (p->filename);
	free (p->cursor);
	if (bdb_env != NULL) {
		unlock_record (f);
		bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
		bdb_env->lock_id_free (bdb_env, p->bdb_lock_id);
	}
#endif
	free (p);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_start_internal (cob_file *f, int cond, cob_field *key, int read_opts, int test_lock)
{
	int			ret;
	unsigned int		dupno;
	struct indexed_file	*p = f->file;

	/* look up for the key */
	for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++) {
		if (f->keys[p->key_index].field->data == key->data) {
			break;
		}
	}
#if COB_DEBUG
	if (unlikely(p->key_index == f->nkeys)) {
		cob_runtime_error ("cob_start_indexed: key not found "
				   "(should have been detected by cobc)");
		return 99;
	}
#endif

	/* search */
	DBT_SET (p->key, key);
#ifdef	USE_DB41
	/* the open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
#else
	ret = DB_SEQ (p->db[p->key_index], R_CURSOR);
#endif
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = memcmp (p->key.data, key->data, key->size);
		}
		break;
	case COB_LT:
		if (ret != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
		} else {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
		}
		break;
	case COB_LE:
		if (ret != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
		} else if (memcmp (p->key.data, key->data, key->size) != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
		} else if (f->keys[p->key_index].flag) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
#else
			while (!ret && memcmp (p->key.data, key->data, key->size) == 0) {
				ret = DB_SEQ (p->db[p->key_index], R_NEXT);
			}
#endif
			if (ret != 0) {
#ifdef	USE_DB41
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
				ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
			} else {
#ifdef	USE_DB41
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
				ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && memcmp (p->key.data, key->data, key->size) == 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
			ret = DB_SEQ (p->db[p->key_index], R_NEXT);
#endif
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* temporarily save alternate key */
		own_memcpy (p->temp_key, p->key.data, f->keys[p->key_index].field->size);
		if (f->keys[p->key_index].flag) {
			own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
		}
		p->key.data = p->data.data;
		p->key.size = f->keys[0].field->size;
		ret = DB_GET (p->db[0], 0);
	}

#ifdef	USE_DB41
	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
	}
#endif

	if (ret == 0) {
		if (p->key_index == 0) {
			own_memcpy (p->last_readkey[0], p->key.data, f->keys[0].field->size);
		} else {
			own_memcpy (p->last_readkey[p->key_index],
				    p->temp_key, f->keys[p->key_index].field->size);
			own_memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}
#endif

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_start (cob_file *f, int cond, cob_field *key)
{
	return (indexed_start_internal (f, cond, key, 0, 0));
}

static int
indexed_read (cob_file *f, cob_field *key, int read_opts)
{
	struct indexed_file	*p = f->file;
	int			ret;
	int			test_lock = 0;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
		test_lock = 1;
	}
#endif

	ret = indexed_start_internal (f, COB_EQ, key, read_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	own_memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_read_next (cob_file *f, int read_opts)
{
	struct indexed_file	*p = f->file;
	int			ret;
	int			read_nextprev;
	int			nextprev = DB_NEXT;
	int			file_changed = 0;
	unsigned int		dupno;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	if (unlikely(read_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
#ifdef	USE_DB41
	/* the open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
#endif

	if (f->flag_first_read) {
		/* data is read in indexed_open or indexed_start */
		if (p->data.data == 0 || (f->flag_first_read == 2 &&
		    nextprev == DB_PREV)) {
#ifdef	USE_DB41
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			p->cursor[p->key_index] = NULL;
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
			}
#endif
			return COB_STATUS_10_END_OF_FILE;
		}
		/* check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
		p->key.data = p->last_readkey[p->key_index];
#ifdef	USE_DB41
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
#else
		ret = DB_GET (p->db[p->key_index], 0) ;
#endif
		if (!ret && p->key_index > 0) {
			if ( f->keys[p->key_index].flag) {
				own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				while (ret == 0 &&
				      memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
				      dupno < p->last_dupno[p->key_index]) {
#ifdef	USE_DB41
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
					ret = DB_SEQ (p->db[p->key_index], R_NEXT); 
#endif
					own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
				}
				if (ret == 0 &&
				   memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
				   dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, f->keys[0].field->size);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) f->keys[0].field->size;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0) ;
			}
		}
		file_changed = ret;	
#ifdef	USE_DB41
		if (bdb_env != NULL && !file_changed) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (read_opts & COB_READ_LOCK) {
				ret = lock_record (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
#endif
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) f->keys[p->key_index].field->size;
			p->key.data = p->last_readkey[p->key_index];
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE); 
#else
			ret = DB_SEQ (p->db[p->key_index], R_CURSOR); 
#endif
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
#ifdef	USE_DB41
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
#endif
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].flag) {
						own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						while (ret == 0 &&
						memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
						dupno < p->last_dupno[p->key_index]) {
#ifdef	USE_DB41
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
							ret = DB_SEQ (p->db[p->key_index], R_NEXT); 
#endif
							own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
#ifdef	USE_DB41
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								p->cursor[p->key_index] = NULL;
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
									p->cursor[0] = NULL;
								}
#endif
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
#else
			ret = DB_SEQ (p->db[p->key_index], nextprev);
#endif
			if (ret != 0) {
#ifdef	USE_DB41
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
#endif
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* temporarily save alternate key */
			own_memcpy (p->temp_key, p->key.data, p->key.size);
			if (f->keys[p->key_index].flag) {
				own_memcpy (&dupno,(ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
			}
			p->key.data = p->data.data;
			p->key.size = f->keys[0].field->size;
			if (DB_GET (p->db[0], 0) != 0) {
#ifdef	USE_DB41
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
#endif
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
#ifdef	USE_DB41
		if (bdb_env != NULL) {
			if (!(read_opts & COB_READ_IGNORE_LOCK)) {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					p->cursor[p->key_index] = NULL;
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
						p->cursor[0] = NULL;
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[p->key_index] = NULL;
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
					p->cursor[0] = NULL;
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
#endif
		if (p->key_index == 0) {
			own_memcpy (p->last_readkey[0], p->key.data, p->key.size);
		} else {
			own_memcpy (p->last_readkey[p->key_index], p->temp_key,
				    f->keys[p->key_index].field->size);
			own_memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, f->keys[0].field->size);
			if (f->keys[p->key_index].flag) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	p->cursor[p->key_index] = NULL;
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
	}
#endif

	f->record->size = p->data.size;
	own_memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
}

/* get the next number in a set of duplicates */
static unsigned int
get_dupno(cob_file *f, int i)
{
	int			ret;
	unsigned int		dupno =  0;
	struct indexed_file	*p = f->file;

	DBT_SET (p->key, f->keys[i].field);
	own_memcpy (p->temp_key, p->key.data, p->key.size);
#ifdef	USE_DB41
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE); 
#else
	ret = DB_SEQ (p->db[i], R_CURSOR); 
#endif
	while (ret == 0 && memcmp (p->key.data, p->temp_key, p->key.size) == 0) {
		own_memcpy (&dupno, (ucharptr)p->data.data + f->keys[0].field->size, sizeof(unsigned int));
#ifdef	USE_DB41
		ret = DB_SEQ (p->cursor[i], DB_NEXT); 
#else
		ret = DB_SEQ (p->db[i], R_NEXT); 
#endif
	}
#ifdef	USE_DB41
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
#endif
	return ++dupno;
}

static int
check_alt_keys (cob_file *f, int rewrite)
{
	size_t		i;
	int			ret;
	struct indexed_file	*p = f->file;

	for (i = 1; i < f->nkeys; i++) {
		if (!f->keys[i].flag) {
			DBT_SET (p->key, f->keys[i].field);
			ret = DB_GET (p->db[i], 0) ;
			if (ret == 0) {
				if (rewrite) {
					if (memcmp(p->data.data,f->keys[0].field->data,f->keys[0].field->size)) {
						return(1);
					}
				}
				else {
					return(1);
				}
			}
		}
	}
	return(0);
}

static int
indexed_write_internal (cob_file *f, int rewrite, int opt)
{
	size_t			i;
	struct indexed_file	*p = f->file;
	int			flags;
	unsigned int		dupno;
#ifdef	USE_DB41
	int			close_cursor;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1; 
		close_cursor = 1;
	}
#endif

	/* check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f,0)) {
#ifdef	USE_DB41
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
#endif
			return COB_STATUS_22_KEY_EXISTS;
		}
		DBT_SET (p->key, f->keys[0].field);
	}

	/* write data */
#ifdef	USE_DB41
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0; 
		}
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	p->cursor[0]->c_put (p->cursor[0], &p->key, &p->data, DB_KEYFIRST);
#else
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	if (DB_PUT (p->db[0], R_NOOVERWRITE) != 0) {
		return COB_STATUS_22_KEY_EXISTS;
	}
#endif

	/* write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; i++) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (f->keys[i].flag) {
			flags =  0;
			dupno = get_dupno(f,i);
			own_memcpy (p->temp_key, f->keys[0].field->data,
				   f->keys[0].field->size);
			own_memcpy (p->temp_key + f->keys[0].field->size, &dupno,
				   sizeof(unsigned int));
			p->data.data = p->temp_key;
			p->data.size = f->keys[0].field->size + sizeof(unsigned int);;
		} else {
#ifdef	USE_DB41
			flags = DB_NOOVERWRITE;
#else
			flags =  R_NOOVERWRITE;
#endif
		}

		DBT_SET (p->key, f->keys[i].field);
		if (DB_PUT (p->db[i], flags) != 0) {
#ifdef	USE_DB41
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
#endif
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

#ifdef	USE_DB41
	if (opt & COB_WRITE_LOCK) {
		DBT_SET (p->key, f->keys[0].field);
		if (lock_record (f, p->key.data, p->key.size)) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
	}
#endif
	return COB_STATUS_00_SUCCESS;
}

static int
indexed_write (cob_file *f, int opt)
{
	struct indexed_file	*p = f->file;

#ifdef	USE_DB41
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	/* check record key */
	DBT_SET (p->key, f->keys[0].field);
	if (!p->last_key) {
		p->last_key = cob_malloc (p->key.size);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL
		 && memcmp (p->last_key, p->key.data, p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	own_memcpy (p->last_key, p->key.data, p->key.size);

	return indexed_write_internal (f, 0, opt);
}

static int
indexed_delete_internal (cob_file *f, int rewrite)
{
	size_t			i;
	size_t			offset;
	struct indexed_file	*p = f->file;
	DBT			prim_key;
#ifdef	USE_DB41
	int			ret, flags, close_cursor;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	if (p->write_cursor_open) {
		close_cursor = 0;
	} else {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
		p->write_cursor_open = 1; 
		close_cursor = 1;
	}
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif
	/* find the primary key */
#ifdef	USE_DB41
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
	}
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->cursor[0] = NULL;
			p->write_cursor_open = 0; 
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (bdb_env != NULL) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->cursor[0] = NULL;
				p->write_cursor_open = 0; 
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
#else
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		DBT_SET (p->key, f->keys[0].field);
		if (DB_GET (p->db[0], 0) != 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}
#endif
	prim_key = p->key;

	/* delete the secondary keys */
	offset = (char *) p->data.data - (char *) f->record->data;
	for (i = 1; i < f->nkeys; i++) {
		DBT_SET (p->key, f->keys[i].field);
		p->key.data = (char *)p->key.data + offset;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			p->rewrite_sec_key[i] = memcmp(p->key.data, f->keys[i].field->data, p->key.size);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].flag) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

#ifdef	USE_DB41
			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
#else
			if (DB_SEQ (p->db[i], R_CURSOR) == 0) {
#endif
				while (sec_key.size == p->key.size
				&& memcmp (p->key.data, sec_key.data,
				sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data,
					prim_key.size) == 0) {
#ifdef	USE_DB41
						p->cursor[i]->c_del (p->cursor[i], 0);
#else
						DB_DEL (p->db[i], &p->key, R_CURSOR);
#endif
					}
#ifdef	USE_DB41
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
#else
					if (DB_SEQ (p->db[i], R_NEXT) != 0) {
#endif
						break;
					}
				}
			}
#ifdef	USE_DB41
			p->cursor[i]->c_close (p->cursor[i]);
			p->cursor[i] = NULL;
#endif
		}
	}

	/* delete the record */
#ifdef	USE_DB41
	p->cursor[0]->c_del (p->cursor[0], 0);
#else
	DB_DEL (p->db[0], &prim_key, 0);
#endif

#ifdef	USE_DB41
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
	}
#endif
	return COB_STATUS_00_SUCCESS;
}

static int
indexed_delete (cob_file *f)
{
	return indexed_delete_internal(f, 0);
}

static int
indexed_rewrite (cob_file *f, int opt)
{
	struct indexed_file *p = f->file;
	int			ret;
#ifdef	USE_DB41
	int			flags;

	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1; 
	if (bdb_env != NULL) {
		unlock_record (f);
	}
#endif

	/* check duplicate alternate keys */
	if (check_alt_keys (f,1)) {
#ifdef	USE_DB41
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
#endif
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* delete the current record */
	ret = indexed_delete_internal (f, 1);

	if (ret != COB_STATUS_00_SUCCESS) {
#ifdef	USE_DB41
		p->cursor[0]->c_close (p->cursor[0]);
		p->cursor[0] = NULL;
		p->write_cursor_open = 0; 
#endif
		return ret;
	}

	/* write data */
	DBT_SET (p->key, f->keys[0].field);
	ret = indexed_write_internal (f, 1, opt);

#ifdef	USE_DB41
	p->cursor[0]->c_close (p->cursor[0]);
	p->cursor[0] = NULL;
	p->write_cursor_open = 0; 
#endif
	return ret;
}

#ifdef	USE_DB41
/* 
 * check if a file exists in bdb data dirs
 */
static int
is_absolute (const char *filename)
{
#ifdef _WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int
bdb_nofile (char *filename)
{
	struct	stat	st;
	int		i;
	char		*p;
	char		bdbname[COB_MEDIUM_BUFF];

	if (is_absolute (filename)) {
		if (stat (filename, &st) == -1 && errno == ENOENT) {
			return 1;
		} else {
			return 0;
		}
	}

	p = getenv ("DB_HOME");
	for (i = 0; bdb_data_dir && bdb_data_dir[i]; ++i) {
		if (is_absolute (bdb_data_dir[i])) {
			sprintf (bdbname, "%s/%s", bdb_data_dir[i], filename);
		} else {
			sprintf (bdbname, "%s/%s/%s", p, bdb_data_dir[i], filename);
		}
		if (stat (bdbname, &st) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	if (i == 0) {
		sprintf (bdbname, "%s/%s", p, filename);
		if (stat (bdbname, &st) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	return 1;
}
#endif

#endif	/* WITH_DB */

/*
 * Public interface
 */

void
cob_open (cob_file *f, int mode, int opt, cob_field *fnstatus)
{
	int		was_not_exist = 0;
	size_t		i;
	char		*p;
	char		*src;
	char		*dst;
	size_t		simple;
	struct stat	st;
	char		env[COB_SMALL_BUFF];
	char		filename[COB_MEDIUM_BUFF];
	char		buff[COB_MEDIUM_BUFF];

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
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;

	if (f->special) {
		if (f->special == 1) {
			if (mode != COB_OPEN_INPUT) {
				RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
			}
			f->file = stdin;
			f->open_mode = mode;
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		} else {
			if (mode != COB_OPEN_OUTPUT) {
				RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
			}
			f->file = stdout;
			f->open_mode = mode;
			RETURN_STATUS (COB_STATUS_00_SUCCESS);
		}
	}

	/* obtain the file name */
	cob_field_to_string (f->assign, filename);
	if (cob_current_module->flag_filename_mapping) {
		src = filename;
		dst = buff;
		simple = 1;
		/* expand envoronment variables */
		/* ex. "$TMPDIR/foo" -> "/tmp/foo" */
		while (*src) {
			if (!isalnum (*src) && *src != '_') {
				simple = 0;
			}
			if (*src == '$') {
				for (i = 1; ;i++) {
					if (!isalnum (src[i]) && src[i] != '_') {
						break;
					}
				}
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
			for (i = 0; i < NUM_PREFIX; i++) {
				sprintf (buff, "%s%s", prefix[i], filename);
				if ((p = getenv (buff)) != NULL) {
					strcpy (filename, p);
					break;
				}
			}
			if (i == NUM_PREFIX && cob_file_path) {
				sprintf (buff, "%s/%s", cob_file_path, filename);
				strcpy (filename, buff);
			}
		}
	}

	/* check if the file exists */
#ifdef	USE_DB41
	if (f->organization == COB_ORG_INDEXED) {
		if ((bdb_env && bdb_nofile (filename)) ||
		     (!bdb_env && stat (filename, &st) == -1 && errno == ENOENT)) {
			was_not_exist = 1;
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
			}
		}
	} else if (stat (filename, &st) == -1 && errno == ENOENT) {
#else
	if (stat (filename, &st) == -1 && errno == ENOENT) {
#endif
		was_not_exist = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	}

	cob_cache_file (f);

	if (!cob_first_in) {
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
			f->flag_begin_of_file = 1;
			RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
		} else {
			RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
		}
	case EACCES:
	case EISDIR:
	case EROFS:
		RETURN_STATUS (COB_STATUS_37_PERMISSION_DENIED);
	case EAGAIN:
	case COB_STATUS_61_FILE_SHARING:
		RETURN_STATUS (COB_STATUS_61_FILE_SHARING);
	case COB_LINAGE_INVALID:
		RETURN_STATUS (COB_STATUS_57_I_O_LINAGE);
	default:
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
	}
}

void
cob_close (cob_file *f, int opt, cob_field *fnstatus)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->special) {
		f->open_mode = COB_OPEN_CLOSED;
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
	}
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
cob_start (cob_file *f, int cond, cob_field *key, cob_field *fnstatus)
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
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	}

	RETURN_STATUS (ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, int read_opts)
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
	if (f->flag_end_of_file && key == NULL &&
	    !(read_opts & COB_READ_PREVIOUS)) {
		RETURN_STATUS (COB_STATUS_46_READ_ERROR);
	}
	if (f->flag_begin_of_file && key == NULL &&
	    (read_opts & COB_READ_PREVIOUS)) {
		RETURN_STATUS (COB_STATUS_46_READ_ERROR);
	}

	if (f->open_mode == COB_OPEN_CLOSED
	    || f->open_mode == COB_OPEN_OUTPUT
	    || f->open_mode == COB_OPEN_EXTEND) {
		RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);
	}

#ifdef	USE_DB41
	if (f->organization == COB_ORG_INDEXED && bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O  ||
		    f->lock_mode == COB_LOCK_EXCLUSIVE) {
			read_opts &= ~COB_READ_LOCK;
		} else if (f->lock_mode == COB_LOCK_AUTOMATIC &&
		   !(read_opts & COB_READ_NO_LOCK)) {
			read_opts |= COB_READ_LOCK; 
		}
	} else {
		read_opts &= ~COB_READ_LOCK;
	}
#endif
	if (key) {
		ret = fileio_funcs[(int)f->organization]->read (f, key, read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->record_size) {
			cob_set_int (f->record_size, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	RETURN_STATUS (ret);
}

void
cob_write (cob_file *f, cob_field *rec, int opt, cob_field *fnstatus)
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

#ifdef	USE_DB41
	if (f->organization != COB_ORG_INDEXED || bdb_env == NULL) {
		opt &= ~COB_WRITE_LOCK;
	}
#endif

	ret = fileio_funcs[(int)f->organization]->write (f, opt);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_rewrite (cob_file *f, cob_field *rec, int opt, cob_field *fnstatus)
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
			if (f->record->size != (size_t)cob_get_int (f->record_size)) {
				RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
			}
		}
	}

#ifdef	USE_DB41
	if (f->organization != COB_ORG_INDEXED || bdb_env == NULL) {
		opt &= ~COB_WRITE_LOCK;
	}
#endif

	ret = fileio_funcs[(int)f->organization]->rewrite (f, opt);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
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

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_default_error_handle (void)
{
	const char	*msg = NULL;
	unsigned char	*file_status = cob_error_file->file_status;
	int		status = cob_d2i (file_status[0]) * 10 + cob_d2i (file_status[1]);
	char		filename[COB_MEDIUM_BUFF];

	switch (status) {
	case COB_STATUS_10_END_OF_FILE:
		msg = "End of file";
		break;
	case COB_STATUS_14_OUT_OF_KEY_RANGE:
		msg = "Key out of range";
		break;
	case COB_STATUS_21_KEY_INVALID:
		msg = "Key order not ascending";
		break;
	case COB_STATUS_22_KEY_EXISTS:
		msg = "Record key already exists";
		break;
	case COB_STATUS_23_KEY_NOT_EXISTS:
		msg = "Record key does not exist";
		break;
	case COB_STATUS_30_PERMANENT_ERROR:
		msg = "Permanent file error";
		break;
	case COB_STATUS_35_NOT_EXISTS:
		msg = "File does not exist";
		break;
	case COB_STATUS_37_PERMISSION_DENIED:
		msg = "Permission denied";
		break;
	case COB_STATUS_41_ALREADY_OPEN:
		msg = "File already open";
		break;
	case COB_STATUS_42_NOT_OPEN:
		msg = "File not open";
		break;
	case COB_STATUS_43_READ_NOT_DONE:
		msg = "READ must be executed first";
		break;
	case COB_STATUS_44_RECORD_OVERFLOW:
		msg = "Record overflow";
		break;
	case COB_STATUS_46_READ_ERROR:
		msg = "Failed to read";
		break;
	case COB_STATUS_47_INPUT_DENIED:
		msg = "READ/START not allowed";
		break;
	case COB_STATUS_48_OUTPUT_DENIED:
		msg = "WRITE not allowed";
		break;
	case COB_STATUS_49_I_O_DENIED:
		msg = "DELETE/REWRITE not allowed";
		break;
	case COB_STATUS_51_RECORD_LOCKED:
		msg = "Record locked by another file connector";
		break;
/*
	case COB_STATUS_52_EOP:
		break;
*/
	case COB_STATUS_57_I_O_LINAGE:
		msg = "LINAGE values invalid";
		break;
	case COB_STATUS_61_FILE_SHARING:
		msg = "File sharing conflict";
		break;
	default:
		msg = "Unknown file error";
		break;
	}

	cob_field_to_string (cob_error_file->assign, filename);
	cob_runtime_error ("%s (STATUS=%02d) File : '%s'", msg,
				status, filename);
}

void
cob_init_fileio (void)
{
	char	*s;
	int	n;

	if ((s = getenv ("COB_SYNC")) != NULL) {
		if (*s == 'Y' || *s == 'y') {
			cob_do_sync = 1;
		}
		if (*s == 'P' || *s == 'p') {
			cob_do_sync = 2;
		}
	}
	if ((s = getenv ("COB_SORT_MEMORY")) != NULL) {
		n = atoi (s);
		if (n >= 1024*1024) {
			cob_sort_memory = n;
		}
	}
	cob_file_path = getenv ("COB_FILE_PATH");
	if (cob_file_path) {
		if (!*cob_file_path || *cob_file_path == ' ') {
			cob_file_path = NULL;
		}
	}

#ifdef	USE_DB41
	join_environment ();
	record_lock_object = cob_malloc (1000);
	rlo_size = 1000;
#endif
}

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	char			filename[COB_MEDIUM_BUFF];

	for (l = file_cache; l; l = l->next) {
		if (l->file->open_mode != COB_OPEN_CLOSED &&
		     l->file->open_mode != COB_OPEN_LOCKED) {
			cob_field_to_string (l->file->assign, filename);
			cob_close (l->file, 0, NULL);
			fprintf (stderr, "WARNING - Implicit CLOSE of %s (\"%s\")\n",
				l->file->select_name, filename);
			fflush (stderr);
		}
	}
#ifdef	USE_DB41
	free (record_lock_object);
	if (bdb_env) {
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
	}
#endif
}

/* System routines */

static void
rationalize_name (char *dest, char *source, const size_t length)
{
	size_t		i;
	int		quote_switch = 0;

	memset (dest, 0, length);
	for (i = 0; i < length; ++i) {
		if (source[i] == '"') {
			quote_switch = !quote_switch;
			continue;
		}
		dest[i] = source[i];
		if (quote_switch) {
			continue;
		}
		if (dest[i] == ' ' || dest[i] == 0) {
			dest[i] = 0;
			break;
		}
	}
}

static int
open_cbl_file (char *file_name, char *file_access, char *file_handle, int file_flags)
{
#ifdef	O_BINARY
	int	flag = O_BINARY;
#else
	int	flag = 0;
#endif
	int	fd;
	char	fn[COB_MEDIUM_BUFF];

	flag |= file_flags;
	rationalize_name (fn, file_name, sizeof (fn));
	switch (*file_access) {
		case 1:
			flag |= O_RDONLY;
			break;
		case 2:
			flag |= O_CREAT | O_TRUNC | O_WRONLY;
			break;
		case 3:
			flag |= O_RDWR;
			break;
		default:
			memset (file_handle, -1, 4);
			return -1;
	}
	fd = open (fn, flag, 0660);
	if (fd < 0) {
		memset (file_handle, -1, 4);
		return 35;
	}
	memcpy (file_handle, &fd, 4);
	return 0;
}

int
CBL_OPEN_FILE (char *file_name, char *file_access, char *file_lock, char *file_dev, char *file_handle)
{
	return open_cbl_file (file_name, file_access, file_handle, 0);
}

int
CBL_CREATE_FILE (char *file_name, char *file_access, char *file_lock, char *file_dev, char *file_handle)
{
	return open_cbl_file (file_name, file_access, file_handle, O_CREAT | O_TRUNC);
}

int
CBL_READ_FILE (char *file_handle, char *file_offset, char *file_len, unsigned char *flags, char *buf)
{
	int		fd;
	long long	off;
	int		len;
	int		rc;
	struct stat	st;

	memcpy (&fd, file_handle, 4);
	memcpy (&off, file_offset, 8);
	memcpy (&len, file_len, 4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) < 0) {
		return -1;
	}
	rc = read (fd, buf, len);
	if (rc < 0) {
		return -1;
	}
	if (rc == 0) {
		return 10;
	}
	if ((*flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		off = st.st_size;
#ifndef	WORDS_BIGENDIAN
		off = COB_BSWAP_64 (off);
#endif
		memcpy (file_offset, &off, 8);
	}
	return 0;
}

int
CBL_WRITE_FILE (char *file_handle, char *file_offset, char *file_len, char *flags, char *buf)
{
	int		fd;
	long long	off;
	int		len;
	int		rc;

	memcpy (&fd, file_handle, 4);
	memcpy (&off, file_offset, 8);
	memcpy (&len, file_len, 4);
#ifndef WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek(fd, (off_t)off, SEEK_SET) < 0) {
		return -1;
	}
	rc = write (fd, buf, len);
	if (rc < 0) {
		return 30;
	}
	return 0;
}

int
CBL_CLOSE_FILE (char *file_handle)
{
	int	fd;

	memcpy (&fd, file_handle, 4);
	return close (fd);
}

int
CBL_FLUSH_FILE (char *file_handle)
{
	return 0;
}

int
CBL_DELETE_FILE (char *file_name)
{
	char	fn[COB_MEDIUM_BUFF];

	rationalize_name (fn, file_name, sizeof (fn));
	return unlink (fn);
}

int
CBL_COPY_FILE (char *fname1, char *fname2)
{
#ifdef	O_BINARY
	int	flag = O_BINARY;
#else
	int	flag = 0;
#endif
	int	ret;
	int	i;
	int	fd1, fd2;
	char	fn1[COB_MEDIUM_BUFF];
	char	fn2[COB_MEDIUM_BUFF];

	rationalize_name (fn1, fname1, sizeof (fn1));
	rationalize_name (fn2, fname2, sizeof (fn2));
	flag |= O_RDONLY;
	fd1 = open (fn1, flag, 0);
	if (fd1 < 0) {
		return -1;
	}
	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (fn2, flag, 0660);
	if (fd2 < 0) {
		close(fd1);
		return -1;
	}
	ret = 0;
	while ((i = read (fd1, fn1, sizeof(fn1))) > 0) {
		if (write (fd2, fn1, i) < 0) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	return ret;
}

int
CBL_CHECK_FILE_EXIST (char *file_name, char *file_info)
{
	long long	sz;
	struct stat	st;
	struct tm	*tm;
	short		y;
	char		d, m, hh, mm, ss;
	char		fn[COB_MEDIUM_BUFF];

	rationalize_name (fn, file_name, sizeof (fn));
	if (stat (fn, &st) < 0) {
		return 35;
	}
	sz = st.st_size;
	tm = localtime(&st.st_mtime);
	d = (char) tm->tm_mday;
	m = (char) tm->tm_mon + 1;
	y = tm->tm_year + 1900;
	hh = (char) tm->tm_hour;
	mm = (char) tm->tm_min;
	ss = (char) tm->tm_sec;

#ifndef WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
	y = COB_BSWAP_16 (y);
#endif
	memcpy (file_info, &sz, 8);
	file_info[8] = d;
	file_info[9] = m;
	memcpy (file_info+10, &y, 2);
	file_info[12] = hh;
	file_info[13] = mm;
	file_info[14] = ss;
	file_info[15] = 0;
	return 0;
}

int
CBL_RENAME_FILE (char *fname1, char *fname2)
{
	char	fn1[COB_MEDIUM_BUFF];
	char	fn2[COB_MEDIUM_BUFF];

	rationalize_name (fn1, fname1, sizeof (fn1));
	rationalize_name (fn2, fname2, sizeof (fn2));
	return rename (fn1, fn2);
}

int
CBL_GET_CURRENT_DIR (int flags, int dir_length, unsigned char *dir)
{
	int	dir_size;
	int	has_space = 0;
	char	dirname[COB_MEDIUM_BUFF];

	if (dir_length < 1) {
		return 128;
	}
	memset (dir, ' ', dir_length);
	if (getcwd (dirname, sizeof (dirname)) == NULL) {
		return 128;
	}
	dir_size = (int) strlen (dirname);
	if (strchr (dirname, ' ')) {
		has_space = 2;
	}
	if (dir_size + has_space > dir_length) {
		return 128;
	}
	if (has_space) {
		*dir = '"';
		memcpy (&dir[1], dirname, dir_size);
		dir[dir_size + 1] = '"';
	} else {
		memcpy (dir, dirname, dir_size);
	}
	return 0;
}

int
CBL_CREATE_DIR (unsigned char *dir)
{
	char	fn[COB_MEDIUM_BUFF];

	rationalize_name (fn, (char *)dir, sizeof (fn));
#ifdef	_WIN32
	return mkdir (fn) == 0 ? 0 : 128;
#else
	return mkdir (fn, 0770) == 0 ? 0 : 128;
#endif
}

int
CBL_CHANGE_DIR (unsigned char *dir)
{
	char	fn[COB_MEDIUM_BUFF];

	rationalize_name (fn, (char *)dir, sizeof (fn));
	return chdir (fn) == 0 ? 0 : 128;
}

int
CBL_DELETE_DIR (unsigned char *dir)
{
	char	fn[COB_MEDIUM_BUFF];

	rationalize_name (fn, (char *)dir, sizeof (fn));
	return rmdir (fn) == 0 ? 0 : 128;
}

int
cob_acuw_mkdir (unsigned char *dir)
{
	char	fn[COB_MEDIUM_BUFF];

	COB_CHK_PARMS (C$MAKEDIR, 1);
	if (cob_current_module->cob_procedure_parameters[0]) {
		cob_field_to_string (cob_current_module->cob_procedure_parameters[0], fn);
		return CBL_CREATE_DIR ((unsigned char *)fn);
	}
	return 128;
}

int
cob_acuw_chdir (unsigned char *dir, unsigned char *status)
{
	int	ret = 128;
	char	fn[COB_MEDIUM_BUFF];

	COB_CHK_PARMS (C$CHDIR, 2);
	if (cob_current_module->cob_procedure_parameters[0]) {
		cob_field_to_string (cob_current_module->cob_procedure_parameters[0], fn);
		ret = CBL_CHANGE_DIR ((unsigned char *)fn);
		cob_set_int (cob_current_module->cob_procedure_parameters[1], ret);
	}
	return ret;
}

int
cob_acuw_copyfile (char *fname1, char *fname2, unsigned char *file_type)
{
	int	ret = 128;
	char	fn1[COB_MEDIUM_BUFF];
	char	fn2[COB_MEDIUM_BUFF];

	/* RXW - Type is not yet evaluated */

	COB_CHK_PARMS (C$COPY, 3);
	if (cob_current_module->cob_procedure_parameters[0]) {
		cob_field_to_string (cob_current_module->cob_procedure_parameters[0], fn1);
		cob_field_to_string (cob_current_module->cob_procedure_parameters[1], fn2);
		ret = CBL_COPY_FILE (fn1, fn2);
		if (ret < 0) {
			ret = 128;
		}
	}
	return ret;
}

int
cob_acuw_file_info (char *file_name, char *file_info)
{
	unsigned long long	sz;
	unsigned int		dt;
	struct stat		st;
	struct tm		*tm;
	short			y;
	short			d, m, hh, mm, ss;
	char			fn[COB_MEDIUM_BUFF];
	char			fn2[COB_MEDIUM_BUFF];

	COB_CHK_PARMS (C$FILEINFO, 2);
	cob_field_to_string (cob_current_module->cob_procedure_parameters[0], fn);
	rationalize_name (fn2, fn, sizeof (fn2));
	if (stat (fn2, &st) < 0) {
		return 35;
	}
	sz = st.st_size;
	tm = localtime(&st.st_mtime);
	d = tm->tm_mday;
	m = tm->tm_mon + 1;
	y = tm->tm_year + 1900;
	hh = tm->tm_hour;
	mm = tm->tm_min;
	ss = tm->tm_sec;

#ifndef WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
#endif
	memcpy (file_info, &sz, 8);
	dt = (y * 10000) + (m * 100) + d;
#ifndef WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 8, &dt, 4);
	dt = (hh * 1000000) + (mm * 10000) + (ss * 100);
#ifndef WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 12, &dt, 4);
	return 0;
}

int
cob_acuw_file_delete (char *file_name, char *file_type)
{
	char			fn[COB_MEDIUM_BUFF];

	/* RXW - Type is not yet evaluated */
	COB_CHK_PARMS (C$DELETE, 2);
	cob_field_to_string (cob_current_module->cob_procedure_parameters[0], fn);
	return CBL_DELETE_FILE (fn);
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
		const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely(col)) {
		for (i = 0; i < size; i++) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
/* RXW
		return memcmp (s1, s2, size);
*/
		for (i = 0; i < size; i++) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static COB_INLINE void
unique_copy (unsigned char *s1, unsigned char *s2)
{
	size_t	size = sizeof(size_t);

	do {
		*s1++ = *s2++;
	} while (--size);
}

static int
cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *pointer)
{
	int		cmp;
	size_t		i;
	cob_file	*f;
	cob_field	f1;
	cob_field	f2;
	size_t		u1;
	size_t		u2;

	f = pointer;
	for (i = 0; i < f->nkeys; i++) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (COB_FIELD_IS_NUMERIC(&f1)) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size, f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	unique_copy ((unsigned char *)&u1, k1->unique);
	unique_copy ((unsigned char *)&u2, k2->unique);
	if (u1 < u2) {
		return -1;
	}
	return 1;
}

static void
cob_free_list (struct cobitem *q)
{
	struct cobitem	*next;

	while (q != NULL) {
		next = q->next;
		free (q);
		q = next;
	}
}

static struct cobitem *
cob_new_item (struct cobsort *hp, size_t size)
{
	struct cobitem *q;

	if (hp->empty != NULL) {
		q = hp->empty;
		hp->empty = q->next;
	} else {
		q = cob_malloc (size);
	}
	return q;
}

static FILE *
cob_tmpfile ()
{
	int		fd;
	FILE		*fp;
	const char	*s;
	char		filename[COB_MEDIUM_BUFF];


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
	fd = _open (filename, _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY, 0660);
#else
	if ((s = getenv ("TMPDIR")) == NULL && (s = getenv ("TMP")) == NULL) {
		s = "/tmp";
	}
	if (cob_process_id == 0) {
		cob_process_id = getpid ();
	}
	sprintf (filename, "%s/cobsort%d_%d", s, cob_process_id, cob_iteration);
	cob_iteration++;
	fd = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY | O_LARGEFILE, 0660);
#endif
	if (fd < 0) {
		return NULL;
	}
#ifdef _WIN32
	_unlink (filename);
	fp = _fdopen (fd, "w+b");
	if (!fp) {
		_close (fd);
	}
#else
	unlink (filename);
	fp = fdopen (fd, "w+b");
	if (!fp) {
		close (fd);
	}
#endif
	return fp;
}

static int
cob_get_temp_file (struct cobsort *hp, int n)
{
	if (hp->file[n].fp == NULL) {
		hp->file[n].fp = cob_tmpfile ();
	} else {
		rewind (hp->file[n].fp);
	}
	hp->file[n].count = 0;
	return hp->file[n].fp == NULL;
}

static int
cob_sort_queues (struct cobsort *hp)
{
	int		source = 0;
	int		destination;
	int		move;
	int		n;
	struct cobitem	*q;
	int		end_of_block[2];

	while (hp->queue[source + 1].count != 0) {
		destination = source ^ 2;
		hp->queue[destination].count = hp->queue[destination + 1].count = 0;
		hp->queue[destination].first = hp->queue[destination + 1].first = NULL;
		while (1) {
			end_of_block[0] = hp->queue[source].count == 0;
			end_of_block[1] = hp->queue[source + 1].count == 0;
			if (end_of_block[0] && end_of_block[1]) {
				break;
			}
			while (!end_of_block[0] || !end_of_block[1]) {
				if (end_of_block[0]) {
					move = 1;
				} else if (end_of_block[1]) {
					move = 0;
				} else {
					n = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = n < 0 ? 0 : 1;
				}
				q = hp->queue[source + move].first;
				if (q->end_of_block) {
					end_of_block[move] = 1;
				}
				hp->queue[source + move].first = q->next;
				if (hp->queue[destination].first == NULL) {
					hp->queue[destination].first = q;
				} else {
					hp->queue[destination].last->next = q;
				}
				hp->queue[destination].last = q;
				hp->queue[source + move].count--;
				hp->queue[destination].count++;
				q->next = NULL;
				q->end_of_block = 0;
			}
			hp->queue[destination].last->end_of_block = 1;
			destination ^= 1;
		}
		source = destination & 2;
	}
	return source;
}

static int
cob_read_item (struct cobsort *hp, int n)
{
	FILE	*fp = hp->file[n].fp;

	if (getc (fp) != 0) {
		hp->queue[n].first->end_of_block = 1;
	} else {
		hp->queue[n].first->end_of_block = 0;
		if (unlikely(fread (hp->queue[n].first->unique, hp->r_size, 1, fp) != 1)) {
			return 1;
		}
	}
	return 0;
}

static int
cob_write_block (struct cobsort *hp, int n)
{
	struct cobitem	*q;
	FILE		*fp = hp->file[hp->destination_file].fp;

	while (1) {
		q = hp->queue[n].first;
		if (q == NULL) {
			break;
		}
		if (unlikely(fwrite (&(q->block_byte), hp->w_size, 1, fp) != 1)) {
			return 1;
		}
		hp->queue[n].first = q->next;
		q->next = hp->empty;
		hp->empty = q;
	}
	hp->queue[n].count = 0;
	hp->file[hp->destination_file].count++;
	if (putc (1, fp) != 1) {
		return 1;
	}
	return 0;
}

static void
cob_copy_check (cob_file *to, cob_file *from)
{
	size_t		tosize;
	size_t		fromsize;
	unsigned char	*toptr;
	unsigned char	*fromptr;

	tosize = to->record->size;
	fromsize = from->record->size;
	toptr = to->record->data;
	fromptr = from->record->data;
	if (unlikely(tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		own_memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}

static int
cob_file_sort_process (struct cobsort *hp)
{
	int	i;
	int	source;
	int	destination;
	int	n;
	int	move;
	int	res;

	hp->retrieving = 1;
	n = cob_sort_queues (hp);
/* RXW - Cannot be true
	if (unlikely(n < 0)) {
		return COBSORTABORT;
	}
*/
	if (likely(!hp->files_used)) {
		hp->retrieval_queue = n;
		return 0;
	}
	if (unlikely(cob_write_block (hp, n))) {
		return COBSORTFILEERR;
	}
	for (i = 0; i < 4; i++) {
		hp->queue[i].first = hp->empty;
		hp->empty = hp->empty->next;
		hp->queue[i].first->next = NULL;
	}
	rewind (hp->file[0].fp);
	rewind (hp->file[1].fp);
	if (unlikely(cob_get_temp_file (hp, 2))) {
		return COBSORTFILEERR;
	}
	if (unlikely(cob_get_temp_file (hp, 3))) {
		return COBSORTFILEERR;
	}
	source = 0;
	while (hp->file[source].count > 1) {
		destination = source ^ 2;
		hp->file[destination].count = 0;
		hp->file[destination + 1].count = 0;
		while (hp->file[source].count > 0) {
			if (unlikely(cob_read_item (hp, source))) {
				return COBSORTFILEERR;
			}
			if (hp->file[source + 1].count > 0) {
				if (unlikely(cob_read_item (hp, source + 1))) {
					return COBSORTFILEERR;
				}
			} else {
				hp->queue[source + 1].first->end_of_block = 1;
			}
			while (!hp->queue[source].first->end_of_block
			       || !hp->queue[source + 1].first->end_of_block) {
				if (hp->queue[source].first->end_of_block) {
					move = 1;
				} else if (hp->queue[source + 1].first->end_of_block) {
					move = 0;
				} else {
					res = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = res < 0 ? 0 : 1;
				}
				if (unlikely(fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, 1,
				    hp->file[destination].fp) != 1)) {
					return COBSORTFILEERR;
				}
				if (unlikely(cob_read_item (hp, source + move))) {
					return COBSORTFILEERR;
				}
			}
			hp->file[destination].count++;
			if (unlikely(putc (1, hp->file[destination].fp) != 1)) {
				return COBSORTFILEERR;
			}
			hp->file[source].count--;
			hp->file[source + 1].count--;
			destination ^= 1;
		}
		source = destination & 2;
		rewind (hp->file[0].fp);
		rewind (hp->file[1].fp);
		rewind (hp->file[2].fp);
		rewind (hp->file[3].fp);
	}
	hp->retrieval_queue = source;
	if (unlikely(cob_read_item (hp, source))) {
		return COBSORTFILEERR;
	}
	if (unlikely(cob_read_item (hp, source + 1))) {
		return COBSORTFILEERR;
	}
	return 0;
}

static int
cob_file_sort_submit (cob_file *f, const unsigned char *p)
{
	struct cobsort		*hp;
/* RXW - See comment lines below
	size_t			i;
*/
	int			n;
	struct cobitem		*q;
	struct memory_struct	*z;

	hp = f->file;
	if (unlikely(!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely(hp->retrieving)) {
		return COBSORTABORT;
		/* put existing items into the empty list */
/* RXW - This was a facility to submit new items after retrieval had begun
		for (i = 0; i < 4; i++) {
			if (hp->queue[i].first != NULL) {
				hp->queue[i].last->next = hp->empty;
				hp->empty = hp->queue[i].first;
				hp->queue[i].first = NULL;
			}
		}
		hp->queue[0].count = hp->queue[1].count = 0;
		hp->destination_file = -1;
		hp->retrieving = 0;
		hp->files_used = 0;
*/
	}
	if (hp->queue[0].count + hp->queue[1].count >= hp->memory) {
		if (!hp->files_used) {
			if (unlikely(cob_get_temp_file (hp, 0))) {
				return COBSORTFILEERR;
			}
			if (unlikely(cob_get_temp_file (hp, 1))) {
				return COBSORTFILEERR;
			}
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
/* RXW - Cannot be true
		if (unlikely(n < 0)) {
			return COBSORTABORT;
		}
*/
		if (unlikely(cob_write_block (hp, n))) {
			return COBSORTFILEERR;
		}
		hp->destination_file ^= 1;
	}
	q = cob_new_item (hp, sizeof (struct cobitem) + hp->size);
	q->end_of_block = 1;
	unique_copy (q->unique, (unsigned char *)&(hp->unique));
	hp->unique++;
	memcpy (q->item, p, hp->size);
	if (hp->queue[0].count <= hp->queue[1].count) {
		z = &hp->queue[0];
	} else {
		z = &hp->queue[1];
	}
	q->next = z->first;
	z->first = q;
	z->count++;
	return 0;
}

static int
cob_file_sort_retrieve (cob_file *f, unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*next;
	struct memory_struct	*z;
	int			move;
	int			source;
	int			res;

	hp = f->file;
	if (unlikely(!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely(!hp->retrieving)) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (unlikely(hp->files_used)) {
		source = hp->retrieval_queue;
		if (hp->queue[source].first->end_of_block) {
			if (hp->queue[source + 1].first->end_of_block) {
				return COBSORTEND;
			}
			move = 1;
		} else if (hp->queue[source + 1].first->end_of_block) {
			move = 0;
		} else {
			res = cob_file_sort_compare (hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
			move = res < 0 ? 0 : 1;
		}
		memcpy (p, hp->queue[source + move].first->item, hp->size);
		if (unlikely(cob_read_item (hp, source + move))) {
			return COBSORTFILEERR;
		}
	} else {
		z = &hp->queue[hp->retrieval_queue];
		if (z->first == NULL) {
			return COBSORTEND;
		}
		memcpy (p, z->first->item, hp->size);
		next = z->first->next;
		z->first->next = hp->empty;
		hp->empty = z->first;
		z->first = next;
	}
	return 0;
}

void
cob_file_sort_using (cob_file *sort_file, cob_file *data_file)
{
	int		ret;

	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	while (1) {
		cob_read (data_file, 0, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		cob_copy_check (sort_file, data_file);
		ret = cob_file_sort_submit (sort_file, sort_file->record->data);
		if (ret) {
			break;
		}
	}
	cob_close (data_file, COB_CLOSE_NORMAL, NULL);
}

void
cob_file_sort_giving (cob_file *sort_file, size_t varcnt, ...)
{
	size_t		i;
	int		ret;
	struct cobsort	*hp;
	va_list		args;
	cob_file	**fbase;

	fbase = cob_malloc (varcnt * sizeof(cob_file *));
	va_start (args, varcnt);
	for (i = 0; i < varcnt; i++) {
		fbase[i] = va_arg (args, cob_file *);
		cob_open (fbase[i], COB_OPEN_OUTPUT, 0, NULL);
	}
	va_end (args);
	while (1) {
		ret = cob_file_sort_retrieve (sort_file, sort_file->record->data);
		if (ret) {
			if (ret == COBSORTEND) {
				sort_file->file_status[0] = '1';
				sort_file->file_status[1] = '0';
			} else {
				hp = sort_file->file;
				*(int *)(hp->sort_return) = 16;
				sort_file->file_status[0] = '3';
				sort_file->file_status[1] = '0';
			}
			break;
		}
		for (i = 0; i < varcnt; i++) {
			cob_copy_check (fbase[i], sort_file);
			cob_write (fbase[i], fbase[i]->record, 0, NULL);
		}
	}
	for (i = 0; i < varcnt; i++) {
		cob_close (fbase[i], COB_CLOSE_NORMAL, NULL);
	}
	free (fbase);
}

void
cob_file_sort_init (cob_file *f, int nkeys, const unsigned char *collating_sequence,
			void *sort_return)
{
	struct cobsort	*p;
	cob_field	*fnstatus = NULL;

	p = cob_malloc (sizeof (struct cobsort));
	p->size = f->record_max;
	p->r_size = f->record_max + sizeof(size_t);
	p->w_size = f->record_max + sizeof(size_t) + 1;
	p->pointer = f;
	p->sort_return = sort_return;
	*(int *)sort_return = 0;
	p->memory = (size_t)cob_sort_memory / (p->size + sizeof(struct cobitem));
	f->file = p;
	f->keys = cob_malloc (sizeof (cob_file_key) * nkeys);
	f->nkeys = 0;
	if (collating_sequence) {
		f->sort_collating = collating_sequence;
	} else {
		f->sort_collating = cob_current_module->collating_sequence;
	}
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, int flag, cob_field *field, size_t offset)
{
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].offset = offset;
	f->nkeys++;
}

void
cob_file_sort_close (cob_file *f)
{
	size_t		i;
	struct cobsort	*hp;
	cob_field	*fnstatus = NULL;

	hp = f->file;
	if (likely(hp)) {
		cob_free_list (hp->empty);
		for (i = 0; i < 4; i++) {
			cob_free_list (hp->queue[i].first);
			if (hp->file[i].fp != NULL) {
				fclose (hp->file[i].fp);
			}
		}
		free (hp);
	}
	f->file = NULL;
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	int		ret;
	struct cobsort	*hp;
	cob_field	*fnstatus = NULL;

	ret = cob_file_sort_submit (f, f->record->data);
	switch (ret) {
	case 0:
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
		break;
	default:
		hp = f->file;
		*(int *)(hp->sort_return) = 16;
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		break;
	}
}

void
cob_file_return (cob_file *f)
{
	int		ret;
	struct cobsort	*hp;
	cob_field	*fnstatus = NULL;

	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		RETURN_STATUS (COB_STATUS_00_SUCCESS);
		break;
	case COBSORTEND:
		RETURN_STATUS (COB_STATUS_10_END_OF_FILE);
		break;
	default:
		hp = f->file;
		*(int *)(hp->sort_return) = 16;
		RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
		break;
	}
}
