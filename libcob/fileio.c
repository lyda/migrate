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
static int		cob_sort_input_cache = 8*1024*1024;
static int		cob_sort_output_cache = 32*1024*1024;
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

static int dummy_rn_rew_del (cob_file *f);
static int dummy_read (cob_file *f, cob_field *key, int read_opts);
static int dummy_start (cob_file *f, int cond, cob_field *key);
static int file_open (cob_file *f, char *filename, int mode, int opt);
static int file_close (cob_file *f, int opt);
static int file_write_opt (cob_file *f, const int opt);
static int sequential_read (cob_file *f, int read_opts);
static int sequential_write (cob_file *f, int opt);
static int sequential_rewrite (cob_file *f);
static int lineseq_read (cob_file *f, int read_opts);
static int lineseq_write (cob_file *f, int opt);
static int relative_start (cob_file *f, int cond, cob_field *k);
static int relative_read (cob_file *f, cob_field *k, int read_opts);
static int relative_read_next (cob_file *f, int read_opts);
static int relative_write (cob_file *f, int opt);
static int relative_rewrite (cob_file *f);
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
	DB		**db;		/* database handlers */
	DBT		key;
	DBT		data;
	unsigned char	**last_readkey;	/* the last key read */
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
static int indexed_write_internal (cob_file *f);
static int indexed_write (cob_file *f, int opt);
static int indexed_delete (cob_file *f);
static int indexed_rewrite (cob_file *f);
static int sort_open (cob_file *f, char *filename, int mode, int flag);
static int sort_close (cob_file *f, int opt);
static int sort_read (cob_file *f, int read_opts);
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

#endif	/* WITH_DB || WITH_CISAM || WITH_VBISAM */


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
	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		size_t			i;
		struct indexed_file	*p = f->file;

		for (i = 0; i < f->nkeys; i++) {
			DB_SYNC (p->db[i]);
		}
		if (mode == 2) {
			for (i = 0; i < f->nkeys; i++) {
#ifdef	USE_DB41
				int	n;

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
		COB_SET_EXCEPTION (status_exception[status / 10]);
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
dummy_read (cob_file *f, cob_field *key, int read_opts)
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
	if (unlikely(opt & COB_WRITE_PAGE)) {
		if (unlikely(f->linage)) {
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
lineseq_read (cob_file *f, int read_opts)
{
	size_t		i = 0;
	int		n;
	unsigned char	*dataptr;

	dataptr = f->record->data;
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (n == EOF) {
			return COB_STATUS_10_END_OF_FILE;
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

	if (f->linage && f->flag_needs_top) {
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
	off_t	off;

	SEEK_INIT (f);

	relsize = f->record_max + sizeof (f->record->size);
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		int kindex = cob_get_int (f->keys[0].field) - 1;

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
relative_rewrite (cob_file *f)
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
/* HMR
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND ||
		    f->lock_mode == COB_LOCK_EXCLUSIVE) {
*/
		if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_EXTEND) {
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
		p->last_readkey[f->nkeys + i] = cob_malloc (f->record_max);
	}

	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	own_memset ((unsigned char *)&p->key, 0, sizeof (DBT));
	own_memset ((unsigned char *)&p->data, 0, sizeof (DBT));
#ifdef	USE_DB41
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy(p->filename, filename);
	p->write_cursor_open = 0;
	p->record_locked = 0;
	if (bdb_env != NULL) {
		bdb_env->lock_id (bdb_env, &p->bdb_lock_id);
	}

	DBT_SET (p->key, f->keys[0].field);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	p->cursor[0]->c_close (p->cursor[0]);
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
	free (p->db);
	free (p->last_readkey);
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
	case COB_LE:
		if (ret != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
#else
			ret = DB_SEQ (p->db[p->key_index], R_LAST);
#endif
		} else if (cond == COB_LT
			   || memcmp (p->key.data, key->data, key->size) != 0) {
#ifdef	USE_DB41
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
#else
			ret = DB_SEQ (p->db[p->key_index], R_PREV);
#endif
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
		own_memcpy (p->last_readkey[f->nkeys], p->key.data, f->keys[p->key_index].field->size);
		p->key = p->data;
		ret = DB_GET (p->db[0], 0);
	}

#ifdef	USE_DB41
	if (ret == 0 && test_lock) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
	if (ret == 0 && (read_opts & COB_READ_LOCK)) {
		ret = lock_record (f, p->key.data, p->key.size);
		if (ret) {
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
			}
			return COB_STATUS_51_RECORD_LOCKED;
		}
	}
#endif

	if (ret == 0) {
		if (p->key_index == 0) {
			own_memcpy (p->last_readkey[0], p->key.data, f->keys[0].field->size);
		} else {
			own_memcpy (p->last_readkey[p->key_index],
				    p->last_readkey[f->nkeys], f->keys[p->key_index].field->size);
			if (f->keys[p->key_index].flag) {
				own_memcpy (p->last_readkey[f->nkeys + p->key_index],
					    p->key.data, f->keys[0].field->size);
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
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
		if (p->data.data == 0 || nextprev == DB_PREV) {
#ifdef	USE_DB41
			p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
			if (p->key_index != 0) {
				p->cursor[0]->c_close (p->cursor[0]);
			}
#endif
			return COB_STATUS_10_END_OF_FILE;
		}
#ifdef	USE_DB41
		if (bdb_env != NULL) {
/* check if previously read data still exists */
			if (DB_GET (p->db[0], 0) != 0) {
				file_changed = 1;
			} else {
				ret = test_record_lock (f, p->key.data, p->key.size);
				if (ret) {
					p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
					}
					return COB_STATUS_51_RECORD_LOCKED;
				}
				if (read_opts & COB_READ_LOCK) {
					ret = lock_record (f, p->key.data, p->key.size);
					if (ret) {
						p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
						if (p->key_index != 0) {
							p->cursor[0]->c_close (p->cursor[0]);
						}
						return COB_STATUS_51_RECORD_LOCKED;
					}
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
					if (p->key_index != 0) {
						p->cursor[0]->c_close (p->cursor[0]);
					}
#endif
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].flag) {
						while (ret == 0 &&
						memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
						memcmp (p->data.data, p->last_readkey[f->nkeys + p->key_index], p->data.size) < 0) {
#ifdef	USE_DB41
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
#else
							ret = DB_SEQ (p->db[p->key_index], R_NEXT); 
#endif
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
#ifdef	USE_DB41
								p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
								if (p->key_index != 0) {
									p->cursor[0]->c_close (p->cursor[0]);
								}
#endif
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], p->key.size) == 0 &&
							memcmp (p->data.data, p->last_readkey[f->nkeys + p->key_index], p->data.size) == 0) {
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
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
				}
#endif
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* temporarily save alternate key */
			own_memcpy (p->last_readkey[f->nkeys], p->key.data, p->key.size);
			p->key = p->data;
			if (DB_GET (p->db[0], 0) != 0) {
#ifdef	USE_DB41
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				p->cursor[0]->c_close (p->cursor[0]);
#endif
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
#ifdef	USE_DB41
		if (bdb_env != NULL) {
			ret = test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = lock_record (f, p->key.data, p->key.size);
			if (ret) {
				p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
				if (p->key_index != 0) {
					p->cursor[0]->c_close (p->cursor[0]);
				}
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}
#endif
		if (p->key_index == 0) {
			own_memcpy (p->last_readkey[0], p->key.data, p->key.size);
		} else {
			own_memcpy (p->last_readkey[p->key_index], p->last_readkey[f->nkeys],
				    f->keys[p->key_index].field->size);
			if (f->keys[p->key_index].flag) {
				own_memcpy (p->last_readkey[f->nkeys + p->key_index],
					    p->key.data, p->key.size);
			}
		}
	}

#ifdef	USE_DB41
	p->cursor[p->key_index]->c_close (p->cursor[p->key_index]);
	if (p->key_index != 0) {
		p->cursor[0]->c_close (p->cursor[0]);
	}
#endif

	f->record->size = p->data.size;
	own_memcpy (f->record->data, p->data.data, p->data.size);

	return COB_STATUS_00_SUCCESS;
}

static int
indexed_write_internal (cob_file *f)
{
	size_t			i;
	struct indexed_file	*p = f->file;
	int			flags;
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

	/* write data */
#ifdef	USE_DB41
	if (p->cursor[0]->c_get (p->cursor[0], &p->key, &p->data, DB_SET) == 0) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
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
#ifdef	USE_DB41
		flags = f->keys[i].flag ? 0 : DB_NOOVERWRITE;
#else
		flags = f->keys[i].flag ? 0 : R_NOOVERWRITE;
#endif

		DBT_SET (p->key, f->keys[i].field);
		if (DB_PUT (p->db[i], flags) != 0) {
#ifdef	USE_DB41
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
				p->write_cursor_open = 0; 
			}
#endif
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

#ifdef	USE_DB41
	if (close_cursor) {
		p->cursor[0]->c_close (p->cursor[0]);
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

	return indexed_write_internal (f);
}

static int
indexed_delete (cob_file *f)
{
	size_t			i;
	int			offset;
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
	ret = DB_SEQ (p->cursor[0], DB_SET_RANGE);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		if (close_cursor) {
			p->cursor[0]->c_close (p->cursor[0]);
			p->write_cursor_open = 0; 
		}
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (bdb_env != NULL) {
		ret = test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			if (close_cursor) {
				p->cursor[0]->c_close (p->cursor[0]);
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
	offset = (int)((char *) p->data.data - (char *) f->record->data);
	for (i = 1; i < f->nkeys; i++) {
		DBT_SET (p->key, f->keys[i].field);
		p->key.data = (char *)p->key.data + offset;
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
		p->write_cursor_open = 0; 
	}
#endif
	return COB_STATUS_00_SUCCESS;
}

static int
indexed_rewrite (cob_file *f)
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

	/* delete the current record */
	ret = indexed_delete (f);

	if (ret != COB_STATUS_00_SUCCESS) {
#ifdef	USE_DB41
		p->cursor[0]->c_close (p->cursor[0]);
		p->write_cursor_open = 0; 
#endif
		return ret;
	}

	/* write data */
	DBT_SET (p->key, f->keys[0].field);
	ret = indexed_write_internal (f);

#ifdef	USE_DB41
	p->cursor[0]->c_close (p->cursor[0]);
	p->write_cursor_open = 0; 
#endif
	return ret;
}

/*
 * SORT
 */

struct sort_file {
	DB	*db;
	DBT	key;
	DBT	data;
#ifdef	USE_DB41
	DBC	*cursor;
#endif
};

static cob_file	*current_sort_file;

static int
#ifdef	USE_DB41
sort_compare (DB *db, const DBT *k1, const DBT *k2)
#else
sort_compare (const DBT *k1, const DBT *k2)
#endif
{
	int		cmp;
	size_t		i;
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
#ifdef	USE_DB41
	int			ret;
#else
	BTREEINFO		info;
#endif
	struct sort_file	*p = f->file;
	int			flags = INITIAL_FLAGS;

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
		flags |= DB_CREATE | DB_TRUNCATE;
#else
		flags |= O_RDWR | O_CREAT | O_TRUNC;
#endif
		break;
	}

	/* open db */
#ifdef	USE_DB41
	ret = db_create (&p->db, NULL, 0);
	if (ret) {
		return ret;
	}
	p->db->set_errfile (p->db, stderr);
	ret = p->db->set_bt_compare (p->db, sort_compare);
	if (mode == COB_OPEN_INPUT) {
		ret = p->db->set_cachesize (p->db, 0, cob_sort_input_cache, 1);
	} else {
		ret = p->db->set_cachesize (p->db, 0, cob_sort_output_cache, 1);
	}
	ret = p->db->set_pagesize (p->db, 64*1024);
	ret = p->db->set_flags (p->db, DB_DUP);
	ret = p->db->open (p->db, NULL, filename, NULL, DB_BTREE,
			   flags, COB_FILE_MODE);
#else
	own_memset ((unsigned char *)&info, 0, sizeof (info));
	info.flags = R_DUP;
	info.compare = sort_compare;
	p->db = dbopen (filename, flags, COB_FILE_MODE, DB_BTREE, &info);
#endif
	if (p->db == NULL) {
		return errno;
	}

#ifdef	USE_DB41
	p->db->cursor (p->db, NULL, &p->cursor, 0);
#endif

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
sort_read (cob_file *f, int read_opts)
{
	struct sort_file	*p = f->file;

#ifdef	USE_DB41
	if (DB_SEQ (p->cursor, f->flag_first_read ? DB_FIRST : DB_NEXT) != 0) {
#else
	if (DB_SEQ (p->db, f->flag_first_read ? R_FIRST : R_NEXT) != 0) {
#endif
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
	p->key.size = (cob_dbtsize_t) f->record->size;
	if (DB_PUT (p->db, 0)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
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
	char	bdbname[COB_MEDIUM_BUFF];
	char	*p;

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
	f->flag_begin_of_file = 0;
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
			size_t	i;

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
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	int	ret;
	int	_read_opts;

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

	_read_opts = read_opts;
#ifdef	USE_DB41
	if (f->organization == COB_ORG_INDEXED && bdb_env != NULL) {
/* HMR
	assume LOCK MODE MANUAL WITH LOCK ON RECORD
	if (f->open_mode != COB_OPEN_I_O)  ||
	f->lock_mode == COB_LOCK_EXCLUSIVE) {
*/
		if (f->open_mode != COB_OPEN_I_O) {
		     _read_opts &= ~COB_READ_LOCK;
		} 
/* HMR
	else if (f->lock_mode == COB_LOCK_AUTOMATIC ) {
	_read_opts |= COB_READ_LOCK; 
	}
*/
	} else {
		_read_opts &= ~COB_READ_LOCK;
	}
#endif
	if (key) {
		ret = fileio_funcs[(int)f->organization]->read (f, key, _read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read_next (f, _read_opts);
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

	ret = fileio_funcs[(int)f->organization]->write (f, opt);

	if (unlikely(cob_do_sync && ret == 0)) {
		cob_sync (f, cob_do_sync);
	}

	RETURN_STATUS (ret);
}

void
cob_rewrite (cob_file *f, cob_field *rec, cob_field *fnstatus)
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

	ret = fileio_funcs[(int)f->organization]->rewrite (f);

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

#ifdef	WITH_DB

static const unsigned char *old_sequence;

#ifndef _WIN32
static int	cob_iteration = 0;
static pid_t	cob_process_id = 0;
#endif

void
cob_sort_init (cob_file *f, int nkeys, const unsigned char *collating_sequence)
{
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
#else
	if ((s = getenv ("TMPDIR")) == NULL && (s = getenv ("TMP")) == NULL) {
		s = "/tmp";
	}
	if (cob_process_id == 0) {
		cob_process_id = getpid ();
	}
	sprintf (filename, "%s/cobsort%d_%d", s, cob_process_id, cob_iteration);
	cob_iteration++;
#endif

	f->assign->size = strlen (filename);
	f->assign->data = (ucharptr)cob_strdup (filename);
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
	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	while (1) {
		cob_read (data_file, 0, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		own_memcpy (sort_file->record->data, data_file->record->data,
			sort_file->record->size);
		cob_write (sort_file, sort_file->record, 0, NULL);
	}
	cob_close (data_file, COB_CLOSE_NORMAL, NULL);
}

void
cob_sort_giving (cob_file *sort_file, cob_file *data_file)
{
	cob_open (data_file, COB_OPEN_OUTPUT, 0, NULL);
	while (1) {
		cob_read (sort_file, 0, NULL, COB_READ_NEXT);
		if (sort_file->file_status[0] != '0') {
			break;
		}
		own_memcpy (data_file->record->data,
			sort_file->record->data, data_file->record->size);
		cob_write (data_file, data_file->record, 0, NULL);
	}
	cob_close (data_file, COB_CLOSE_NORMAL, NULL);
}

#endif	/* WITH_DB */

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
	if ((s = getenv ("COB_SORT_INPUT_CACHE")) != NULL) {
		n = atoi (s);
		if (n >= 1024*1024 && n <= 1024*1024*1024) {
			cob_sort_input_cache = n;
		}
	}
	if ((s = getenv ("COB_SORT_OUTPUT_CACHE")) != NULL) {
		n = atoi (s);
		if (n >= 1024*1024 && n <= 1024*1024*1024) {
			cob_sort_output_cache = n;
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
		struct stat st;

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
