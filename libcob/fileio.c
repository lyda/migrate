/*
 * Copyright (C) 2002-2003 Keisuke Nishida
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

#ifdef __MINGW32__
#define SEEK_INIT(f)	fseek (f->file, 0, SEEK_CUR)
#else
#define SEEK_INIT(f)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/file.h>
#include <sys/stat.h>

#if HAVE_DBOPEN
#include <db.h>
#else
#if HAVE_DB1_DB_H
#include <db1/db.h>
#else
#if HAVE_DB_185_H
#include <db_185.h>
#else
#if HAVE_DB3_DB_185_H
#include <db3/db_185.h>
#endif
#endif
#endif
#endif

#include "move.h"
#include "numeric.h"
#include "fileio.h"
#include "lib/gettext.h"

#ifdef _WIN32
#define INITIAL_FLAGS	O_BINARY
#else
#define INITIAL_FLAGS	0
#endif

char cob_error_open_mode;
cob_file *cob_error_file;

static cob_fileio_funcs *fileio_funcs[COB_ORG_MAX];


/*
 * Regular file
 */

#define FILE_WRITE_AFTER(f,opt)			\
  if (opt & COB_WRITE_AFTER)			\
    file_write_opt (f, opt);

#define FILE_WRITE_BEFORE(f,opt)		\
  if (opt & COB_WRITE_BEFORE)			\
    file_write_opt (f, opt);

static int
file_open (cob_file *f, char *filename, int mode, int opt)
{
  FILE *fp = NULL;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      fp = fopen (filename, "rb");
      break;
    case COB_OPEN_OUTPUT:
      fp = fopen (filename, "wb+");
      break;
    case COB_OPEN_I_O:
      fp = fopen (filename, "rb+");
      break;
    case COB_OPEN_EXTEND:
      fp = fopen (filename, "ab+");
      fseek (fp, 0, SEEK_END);
      break;
    }
  if (fp == NULL)
    return errno;

#ifndef _WIN32
  if (flock (fileno (fp), (opt ? LOCK_EX : LOCK_SH) | LOCK_NB) < 0)
    {
      fclose (fp);
      return errno;
    }
#endif

  f->file = fp;
  return 0;
}

static int
file_close (cob_file *f, int opt)
{
#ifndef _WIN32
  struct flock lock;
#endif

  switch (opt)
    {
    case COB_CLOSE_NORMAL:
    case COB_CLOSE_LOCK:
#ifndef _WIN32
      lock.l_type = F_UNLCK;
      flock (fileno (f->file), LOCK_UN);
#endif
      fclose (f->file);
      return COB_STATUS_00_SUCCESS;
    default:
      return COB_STATUS_07_SUCCESS_NO_UNIT;
    }
}

static void
file_write_opt (cob_file *f, int opt)
{
  if (opt & COB_WRITE_PAGE)
    {
      fputc ('\f', f->file);
    }
  else if (opt & COB_WRITE_LINES)
    {
      int i;
      for (i = opt & COB_WRITE_MASK; i > 0; i--)
	fputc ('\n', f->file);
    }
}


/*
 * SEQUENTIAL
 */

static int
sequential_read (cob_file *f)
{
  SEEK_INIT (f);

  /* read the record size */
  if (f->record_min != f->record_max)
    if (fread (&f->record->size, sizeof (f->record->size), 1, f->file) == 0)
      return COB_STATUS_10_END_OF_FILE;

  /* read the record */
  if (fread (f->record->data, f->record->size, 1, f->file) == 0)
    return COB_STATUS_10_END_OF_FILE;

  return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, int opt)
{
  SEEK_INIT (f);

  FILE_WRITE_AFTER (f, opt);

  /* write the record size */
  if (f->record_min != f->record_max)
    fwrite (&f->record->size, sizeof (f->record->size), 1, f->file);

  /* write the record */
  fwrite (f->record->data, f->record->size, 1, f->file);

  FILE_WRITE_BEFORE (f, opt);

  return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f)
{
  fseek (f->file, -f->record->size, SEEK_CUR);
  fwrite (f->record->data, f->record->size, 1, f->file);
  return COB_STATUS_00_SUCCESS;
}

static cob_fileio_funcs sequential_funcs = {
  file_open,
  file_close,
  0,
  0,
  sequential_read,
  sequential_write,
  sequential_rewrite,
  0
};


/*
 * LINE SEQUENTIAL
 */

static int
lineseq_read (cob_file *f)
{
  size_t i;
  char buff[f->record->size + 1];

  /* read the file */
  if (fgets (buff, f->record->size + 1, f->file) == NULL)
    return COB_STATUS_10_END_OF_FILE;

  /* remove the newline */
  for (i = 0; i < f->record->size; i++)
    if (buff[i] == '\r' || buff[i] == '\n')
      break;
  if (i < f->record->size)
    {
      /* fill the record by spaces */
      memset (buff + i, ' ', f->record->size - i);
    }
  else
    {
      /* discard input until the newline */
      char buff[BUFSIZ];
      while (fgets (buff, BUFSIZ, f->file) != NULL)
	if (strchr (buff, '\n') != NULL)
	  break;
    }

  memcpy (f->record->data, buff, f->record->size);

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

  FILE_WRITE_AFTER (f, opt);

  /* write to the file */
  for (i = 0; i < size; i++)
    fputc (f->record->data[i], f->file);

  FILE_WRITE_BEFORE (f, opt);

  return COB_STATUS_00_SUCCESS;
}

static cob_fileio_funcs lineseq_funcs = {
  file_open,
  file_close,
  0,
  0,
  lineseq_read,
  lineseq_write,
  0,
  0
};


/*
 * RELATIVE
 */

#define RELATIVE_SIZE(f) (f->record_max + sizeof (f->record->size))

#define RELATIVE_SEEK(f,i)						      \
  if (fseek (f->file, RELATIVE_SIZE (f) * (i), SEEK_SET) == -1		      \
      || fread (&f->record->size, sizeof (f->record->size), 1, f->file) == 0) \
    return COB_STATUS_23_KEY_NOT_EXISTS;				      \
  fseek (f->file, - sizeof (f->record->size), SEEK_CUR);

static int
relative_start (cob_file *f, int cond, cob_field *k)
{
  /* get the index */
  int index = cob_get_int (k) - 1;
  if (cond == COB_LT)
    index--;
  else if (cond == COB_GT)
    index++;

  /* seek the index */
  while (1)
    {
      RELATIVE_SEEK (f, index);

      /* check if a valid record */
      if (f->record->size > 0)
	{
	  cob_set_int (k, index + 1);
	  return COB_STATUS_00_SUCCESS;
	}

      /* continue */
      switch (cond)
	{
	case COB_EQ:
	  return COB_STATUS_23_KEY_NOT_EXISTS;
	case COB_LT:
	case COB_LE:
	  index--;
	  break;
	case COB_GT:
	case COB_GE:
	  index++;
	  break;
	}
    }
}

static int
relative_read (cob_file *f, cob_field *k)
{
  RELATIVE_SEEK (f, cob_get_int (k) - 1);

  if (f->record->size == 0)
    return COB_STATUS_23_KEY_NOT_EXISTS;

  fseek (f->file, sizeof (f->record->size), SEEK_CUR);
  fread (f->record->data, f->record_max, 1, f->file);
  return COB_STATUS_00_SUCCESS;
}

static int
relative_read_next (cob_file *f)
{
  SEEK_INIT (f);

  while (1)
    {
      if (fread (&f->record->size, sizeof (f->record->size), 1, f->file) == 0)
	return COB_STATUS_10_END_OF_FILE;

      if (f->keys[0].field)
	{
	  if (f->flag_first_read)
	    {
	      cob_set_int (f->keys[0].field, 1);
	      f->flag_first_read = 0;
	    }
	  else
	    {
	      if (cob_add_int (f->keys[0].field, 1) != 0)
		{
		  fseek (f->file, - sizeof (f->record->size), SEEK_CUR);
		  return COB_STATUS_14_OUT_OF_KEY_RANGE;
		}
	    }
	}

      if (f->record->size > 0)
	{
	  fread (f->record->data, f->record_max, 1, f->file);
	  return COB_STATUS_00_SUCCESS;
	}

      fseek (f->file, f->record_max, SEEK_CUR);
    }
}

static int
relative_write (cob_file *f, int opt)
{
  size_t size;
  FILE *fp = f->file;

  SEEK_INIT (f);

  if (f->access_mode != COB_ACCESS_SEQUENTIAL)
    {
      int index = cob_get_int (f->keys[0].field) - 1;
      if (index < 0
	  || fseek (f->file, RELATIVE_SIZE (f) * index, SEEK_SET) < 0)
	return COB_STATUS_21_KEY_INVALID;
    }

  if (fread (&size, sizeof (size), 1, fp) > 0)
    {
      fseek (f->file, - sizeof (size), SEEK_CUR);
      if (size > 0)
	return COB_STATUS_22_KEY_EXISTS;
    }

  fwrite (&f->record->size, sizeof (f->record->size), 1, f->file);
  fwrite (f->record->data, f->record_max, 1, f->file);

  /* update RELATIVE KEY */
  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (f->keys[0].field)
      cob_set_int (f->keys[0].field, ftell (f->file) / RELATIVE_SIZE (f));

  return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f)
{
  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    {
      fseek (f->file, - f->record_max, SEEK_CUR);
    }
  else
    {
      RELATIVE_SEEK (f, cob_get_int (f->keys[0].field) - 1);
      fseek (f->file, sizeof (f->record->size), SEEK_CUR);
    }

  fwrite (f->record->data, f->record_max, 1, f->file);
  return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
  RELATIVE_SEEK (f, cob_get_int (f->keys[0].field) - 1);

  f->record->size = 0;
  fwrite (&f->record->size, sizeof (f->record->size), 1, f->file);
  fseek (f->file, f->record_max, SEEK_CUR);
  return COB_STATUS_00_SUCCESS;
}

static cob_fileio_funcs relative_funcs = {
  file_open,
  file_close,
  relative_start,
  relative_read,
  relative_read_next,
  relative_write,
  relative_rewrite,
  relative_delete
};


/*
 * INDEXED
 */

#if defined(HAVE_DBOPEN) | defined(WITH_DB)

#define DB_PUT(db,flags)	db->put (db, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->seq (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, key, flags)
#define DB_CLOSE(db)		db->close (db)

#define DBT_SET(key,fld)			\
  key.data = fld->data;				\
  key.size = fld->size;

struct indexed_file {
  int key_index;
  unsigned char *last_key;	/* the last key written */
  DB **db;			/* database handlers */
  DBT key, data;
};

static int
indexed_open (cob_file *f, char *filename, int mode, int flag)
{
  int i, j;
  int flags = INITIAL_FLAGS;
  struct indexed_file *p;

  switch (mode)
    {
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

  p = malloc (sizeof (struct indexed_file));
  p->db = malloc (sizeof (DB *) * f->nkeys);
  for (i = 0; i < f->nkeys; i++)
    {
      BTREEINFO info;
      char name[FILENAME_MAX];

      /* file name */
      if (i == 0)
	strcpy (name, filename);
      else
	sprintf (name, "%s.%d", filename, i);

      /* btree info */
      memset (&info, 0, sizeof (info));
      if (f->keys[i].flag)
	info.flags = R_DUP;

      /* open db */
      p->db[i] = dbopen (name, flags, COB_FILE_MODE, DB_BTREE, &info);
      if (p->db[i] == 0)
	{
	  for (j = 0; j < i; j++)
	    DB_CLOSE (p->db[j]);
	  free (p->db);
	  free (p);
	  return errno;
	}
    }

  f->file = p;
  p->key_index = 0;
  p->last_key = NULL;

  memset (&p->key, 0, sizeof (DBT));
  memset (&p->data, 0, sizeof (DBT));
  DB_SEQ (p->db[p->key_index], R_FIRST);

  return 0;
}

static int
indexed_close (cob_file *f, int opt)
{
  int i;
  struct indexed_file *p = f->file;

  /* close DB's */
  for (i = 0; i < f->nkeys; i++)
    DB_CLOSE (p->db[i]);

  if (p->last_key)
    free (p->last_key);
  free (p->db);
  free (p);

  return COB_STATUS_00_SUCCESS;
}

static int
indexed_start (cob_file *f, int cond, cob_field *key)
{
  int ret;
  struct indexed_file *p = f->file;

  /* look up for the key */
  for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++)
    if (f->keys[p->key_index].field->data == key->data)
      break;
#if COB_DEBUG
  if (p->key_index == f->nkeys)
    {
      cob_runtime_error ("cob_start_indexed: key not found "
			 "(should have been detected by cobc)");
      return 99;
    }
#endif

  /* search */
  DBT_SET (p->key, key);
  ret = DB_SEQ (p->db[p->key_index], R_CURSOR);
  switch (cond)
    {
    case COB_EQ:
      if (ret == 0)
	ret = memcmp (p->key.data, key->data, key->size);
      break;
    case COB_LT:
    case COB_LE:
      if (ret != 0)
	ret = DB_SEQ (p->db[p->key_index], R_LAST);
      else if (cond == COB_LT
	       || memcmp (p->key.data, key->data, key->size) != 0)
	ret = DB_SEQ (p->db[p->key_index], R_PREV);
      break;
    case COB_GT:
      while (ret == 0 && memcmp (p->key.data, key->data, key->size) == 0)
	ret = DB_SEQ (p->db[p->key_index], R_NEXT);
      break;
    case COB_GE:
      /* nothing */
      break;
    }
  if (ret == 0 && p->key_index > 0)
    {
      p->key = p->data;
      ret = DB_GET (p->db[0], 0);
    }

  return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_read (cob_file *f, cob_field *key)
{
  struct indexed_file *p = f->file;

  int ret = indexed_start (f, COB_EQ, key);
  if (ret != COB_STATUS_00_SUCCESS)
    return ret;

  f->record->size = p->data.size;
  memcpy (f->record->data, p->data.data, p->data.size);

  return COB_STATUS_00_SUCCESS;
}

static int
indexed_read_next (cob_file *f)
{
  struct indexed_file *p = f->file;

  if (f->flag_first_read)
    {
      /* data is read in indexed_open or indexed_start */
      if (p->data.data == 0)
	return COB_STATUS_10_END_OF_FILE;
    }
  else
    {
      if (DB_SEQ (p->db[p->key_index], R_NEXT) != 0)
	return COB_STATUS_10_END_OF_FILE;
      if (p->key_index > 0)
	{
	  p->key = p->data;
	  if (DB_GET (p->db[0], 0) != 0)
	    return COB_STATUS_23_KEY_NOT_EXISTS;
	}
    }

  f->record->size = p->data.size;
  memcpy (f->record->data, p->data.data, p->data.size);

  return COB_STATUS_00_SUCCESS;
}

static int
indexed_write_internal (cob_file *f)
{
  int i;
  struct indexed_file *p = f->file;

  /* write data */
  p->data.data = f->record->data;
  p->data.size = f->record->size;
  if (DB_PUT (p->db[0], R_NOOVERWRITE) != 0)
    return COB_STATUS_22_KEY_EXISTS;

  /* write secondary keys */
  p->data = p->key;
  for (i = 1; i < f->nkeys; i++)
    {
      int flags = f->keys[i].flag ? 0 : R_NOOVERWRITE;
      DBT_SET (p->key, f->keys[i].field);
      if (DB_PUT (p->db[i], flags) != 0)
	return COB_STATUS_22_KEY_EXISTS;
    }

  return COB_STATUS_00_SUCCESS;
}

static int
indexed_write (cob_file *f, int opt)
{
  struct indexed_file *p = f->file;

  /* check record key */
  DBT_SET (p->key, f->keys[0].field);
  if (!p->last_key)
    p->last_key = malloc (p->key.size);
  else if (f->access_mode == COB_ACCESS_SEQUENTIAL
	   && memcmp (p->last_key, p->key.data, p->key.size) > 0)
    return COB_STATUS_21_KEY_INVALID;
  memcpy (p->last_key, p->key.data, p->key.size);

  return indexed_write_internal (f);
}

static int
indexed_delete (cob_file *f)
{
  int i, offset;
  struct indexed_file *p = f->file;
  DBT prim_key;

  /* find the primary key */
  if (f->access_mode != COB_ACCESS_SEQUENTIAL)
    {
      DBT_SET (p->key, f->keys[0].field);
      if (DB_GET (p->db[0], 0) != 0)
	return COB_STATUS_23_KEY_NOT_EXISTS;
    }
  prim_key = p->key;

  /* delete the secondary keys */
  offset = p->data.data - (void *) f->record->data;
  for (i = 1; i < f->nkeys; i++)
    {
      DBT_SET (p->key, f->keys[i].field);
      p->key.data += offset;
      if (!f->keys[i].flag)
	{
	  DB_DEL (p->db[i], &p->key, 0);
	}
      else
	{
	  DBT sec_key = p->key;
	  if (DB_SEQ (p->db[i], R_CURSOR) == 0)
	    while (sec_key.size == p->key.size
		   && memcmp (p->key.data, sec_key.data, sec_key.size) == 0)
	      {
		if (memcmp (p->data.data, prim_key.data, prim_key.size) == 0)
		  DB_DEL (p->db[i], &p->key, R_CURSOR);
		if (DB_SEQ (p->db[i], R_NEXT) != 0)
		  break;
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
  if (ret != COB_STATUS_00_SUCCESS)
    return ret;

  /* write data */
  DBT_SET (p->key, f->keys[0].field);
  ret = indexed_write_internal (f);

  return ret;
}

static cob_fileio_funcs indexed_funcs = {
  indexed_open,
  indexed_close,
  indexed_start,
  indexed_read,
  indexed_read_next,
  indexed_write,
  indexed_rewrite,
  indexed_delete
};

#endif /* defined(HAVE_DBOPEN) | defined(WITH_DB) */


/*
 * SORT
 */

#if defined(HAVE_DBOPEN) | defined(WITH_DB)

struct sort_file {
  DB *db;
  DBT key, data;
};

static cob_file *current_sort_file;

static int
sort_compare (const DBT *k1, const DBT *k2)
{
  int cmp;
  unsigned int i;
  cob_file *f = current_sort_file;
  for (i = 0; i < f->nkeys; i++)
    {
      cob_field f1 = *(f->keys[i].field);
      cob_field f2 = *(f->keys[i].field);
      f1.data += ((unsigned char *) k1->data) - f->record->data;
      f2.data += ((unsigned char *) k2->data) - f->record->data;
      cmp = cob_cmp (&f1, &f2);
      if (cmp != 0)
	return (f->keys[i].flag == COB_ASCENDING) ? cmp : - cmp;
    }
  return 0;
}

static int
sort_open (cob_file *f, char *filename, int mode, int flag)
{
  BTREEINFO info;
  struct sort_file *p = f->file;
  int flags = INITIAL_FLAGS;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags |= O_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags |= O_RDWR | O_CREAT | O_TRUNC;
      break;
    }

  /* open db */
  memset (&info, 0, sizeof (info));
  info.flags = R_DUP;
  info.compare = sort_compare;
  p->db = dbopen (filename, flags, COB_FILE_MODE, DB_BTREE, &info);
  if (p->db == NULL)
    return errno;

  memset (&p->key, 0, sizeof (DBT));
  memset (&p->data, 0, sizeof (DBT));
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
  struct sort_file *p = f->file;
  if (DB_SEQ (p->db, f->flag_first_read ? R_FIRST : R_NEXT) != 0)
    return COB_STATUS_10_END_OF_FILE;

  memcpy (f->record->data, p->key.data, p->key.size);
  return COB_STATUS_00_SUCCESS;
}

static int
sort_write (cob_file *f, int opt)
{
  struct sort_file *p = f->file;
  current_sort_file = f;
  p->key.data = f->record->data;
  p->key.size = f->record->size;
  DB_PUT (p->db, 0);
  return COB_STATUS_00_SUCCESS;
}

static cob_fileio_funcs sort_funcs = {
  sort_open,
  sort_close,
  0,
  0,
  sort_read,
  sort_write,
  0,
  0
};

#endif /* defined(HAVE_DBOPEN) | defined(WITH_DB) */


/*
 * Public interface
 */

#define RETURN_STATUS(x)	do { save_status (f, x); return; } while (0)

static void
save_status (cob_file *f, int status)
{
  static int exception[] = {
    0,				/* 0x */
    COB_EC_I_O_AT_END,		/* 1x */
    COB_EC_I_O_INVALID_KEY,	/* 2x */
    COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
    COB_EC_I_O_LOGIC_ERROR,	/* 4x */
    COB_EC_I_O_RECORD_OPERATION,/* 5x */
    COB_EC_I_O_FILE_SHARING,	/* 6x */
    COB_EC_I_O,			/* unused */
    COB_EC_I_O,			/* unused */
    COB_EC_I_O_IMP		/* 9x */
  };

  if (f->file_status == 0)
    f->file_status = malloc (2);

  f->file_status[0] = cob_i2d (status / 10);
  f->file_status[1] = cob_i2d (status % 10);
  cob_error_file = f;
  COB_SET_EXCEPTION (exception[status / 10]);
}

void
cob_open (cob_file *f, int mode, int opt)
{
  int was_not_exist = 0;
  char filename[FILENAME_MAX];
  struct stat st;

  f->flag_read_done = 0;

  /* file was previously closed with lock */
  if (f->open_mode == COB_OPEN_LOCKED)
    RETURN_STATUS (COB_STATUS_38_CLOSED_WITH_LOCK);

  /* file is already open */
  if (f->open_mode != COB_OPEN_CLOSED)
    RETURN_STATUS (COB_STATUS_41_ALREADY_OPEN);

  f->last_open_mode = mode;
  f->flag_nonexistent = 0;
  f->flag_end_of_file = 0;
  f->flag_first_read = 1;

  /* obtain the file name */
  cob_field_to_string (f->assign, filename);
  if (cob_current_module->flag_filename_mapping)
    {
      char buff[FILENAME_MAX];
      char *p;
      char *src = filename;
      char *dst = buff;
      int simple = 1;

      /* expand envoronment variables */
      /* ex. "$TMPDIR/foo" -> "/tmp/foo" */
      while (*src)
	{
	  if (!isalnum (*src) && *src != '_')
	    simple = 0;
	  if (*src == '$')
	    {
	      int i = 0;
	      char env[FILENAME_MAX];
	      while (isalnum (src[++i]));
	      memcpy (env, src + 1, i - 1);
	      env[i - 1] = 0;
	      if ((p = getenv (env)) != NULL)
		{
		  strcpy (dst, p);
		  dst += strlen (p);
		}
	      src += i;
	    }
	  else
	    *dst++ = *src++;
	}
      *dst = 0;
      strcpy (filename, buff);

      /* resolve by envoronment variables */
      /* ex. "TMPFILE" -> DD_TMPFILE, dd_TMPFILE, or TMPFILE */
      if (simple)
	{
	  int i;
	  char *prefix[] = {"DD_", "dd_", "", 0};
	  for (i = 0; prefix[i]; i++)
	    {
	      sprintf (buff, "%s%s", prefix[i], filename);
	      if ((p = getenv (buff)) != NULL)
		{
		  strcpy (filename, p);
		  break;
		}
	    }
	}
    }

  /* check if the file exists */
  if (stat (filename, &st) == -1 && errno == ENOENT)
    {
      was_not_exist = 1;
      if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0)
	RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
    }

  /* open the file */
  switch (fileio_funcs[(int) f->organization]->open (f, filename, mode, opt))
    {
    case 0:
      f->open_mode = mode;
      if (f->flag_optional && was_not_exist)
	RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
      else
	RETURN_STATUS (COB_STATUS_00_SUCCESS);
    case ENOENT:
      if (f->flag_optional)
	{
	  f->open_mode = mode;
	  f->flag_nonexistent = 1;
	  f->flag_end_of_file = 1;
	  RETURN_STATUS (COB_STATUS_05_SUCCESS_OPTIONAL);
	}
      else
	{
	  RETURN_STATUS (COB_STATUS_35_NOT_EXISTS);
	}
    case EACCES:
    case EISDIR:
    case EROFS:
      RETURN_STATUS (COB_STATUS_37_PERMISSION_DENIED);
      //case EACCES:
    case EAGAIN:
      RETURN_STATUS (COB_STATUS_61_FILE_SHARING);
    default:
      RETURN_STATUS (COB_STATUS_30_PERMANENT_ERROR);
    }
}

void
cob_close (cob_file *f, int opt)
{
  int ret;

  f->flag_read_done = 0;

  if (f->open_mode == COB_OPEN_CLOSED)
    RETURN_STATUS (COB_STATUS_42_NOT_OPEN);

  if (f->flag_nonexistent)
    ret = COB_STATUS_00_SUCCESS;
  else
    ret = fileio_funcs[(int) f->organization]->close (f, opt);

  if (ret == COB_STATUS_00_SUCCESS)
    switch (opt)
      {
      case COB_CLOSE_LOCK:
	f->open_mode = COB_OPEN_LOCKED;
	break;
      default:
	f->open_mode = COB_OPEN_CLOSED;
	break;
      }

  RETURN_STATUS (ret);
}

#if 0
void
cob_unlock (cob_file *f)
{
  int ret;

  f->flag_read_done = 0;

  if (f->open_mode == COB_OPEN_CLOSED)
    RETURN_STATUS (COB_STATUS_42_NOT_OPEN);

  if (f->flag_nonexistent)
    ret = COB_STATUS_00_SUCCESS;
  else
    ret = fileio_funcs[(int) f->organization]->close (f, opt);

  RETURN_STATUS (ret);
}
#endif

void
cob_start (cob_file *f, int cond, cob_field *key)
{
  int ret;

  f->flag_read_done = 0;
  f->flag_first_read = 0;

  if (f->flag_nonexistent)
    RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);

  if (f->open_mode == COB_OPEN_CLOSED
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND
      || f->access_mode == COB_ACCESS_RANDOM)
    RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);

  ret = fileio_funcs[(int) f->organization]->start (f, cond, key);
  if (ret == COB_STATUS_00_SUCCESS)
    {
      f->flag_end_of_file = 0;
      f->flag_first_read = 1;
    }

  RETURN_STATUS (ret);
}

void
cob_read (cob_file *f, cob_field *key)
{
  int ret;

  f->flag_read_done = 0;

  if (f->flag_nonexistent)
    {
      if (f->flag_first_read == 0)
	RETURN_STATUS (COB_STATUS_23_KEY_NOT_EXISTS);
      f->flag_first_read = 0;
      RETURN_STATUS (COB_STATUS_10_END_OF_FILE);
    }

  if (f->flag_end_of_file)
    RETURN_STATUS (COB_STATUS_46_READ_ERROR);

  if (f->open_mode == COB_OPEN_CLOSED
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND)
    RETURN_STATUS (COB_STATUS_47_INPUT_DENIED);

  if (key)
    ret = fileio_funcs[(int) f->organization]->read (f, key);
  else
    ret = fileio_funcs[(int) f->organization]->read_next (f);

  switch (ret)
    {
    case COB_STATUS_00_SUCCESS:
      f->flag_first_read = 0;
      f->flag_read_done = 1;
      if (f->record_size)
	cob_set_int (f->record_size, f->record->size);
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
  int ret;

  f->flag_read_done = 0;

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    {
      if (f->open_mode == COB_OPEN_CLOSED
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_I_O)
	RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
    }
  else
    {
      if (f->open_mode == COB_OPEN_CLOSED
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_EXTEND)
	RETURN_STATUS (COB_STATUS_48_OUTPUT_DENIED);
    }

  if (f->record_size)
    f->record->size = cob_get_int (f->record_size);
  else
    f->record->size = rec->size;

  if (f->record->size < f->record_min || f->record_max < f->record->size)
    RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);

  ret = fileio_funcs[(int) f->organization]->write (f, opt);

  RETURN_STATUS (ret);
}

void
cob_rewrite (cob_file *f, cob_field *rec)
{
  int ret;
  int read_done = f->flag_read_done;

  f->flag_read_done = 0;

  if (f->open_mode == COB_OPEN_CLOSED || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_STATUS_49_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done)
    RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);

  if (f->organization == COB_ORG_SEQUENTIAL)
    {
      if (f->record->size != rec->size)
	RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);

      if (f->record_size)
	if (f->record->size != cob_get_int (f->record_size))
	  RETURN_STATUS (COB_STATUS_44_RECORD_OVERFLOW);
    }

  ret = fileio_funcs[(int) f->organization]->rewrite (f);

  RETURN_STATUS (ret);
}

void
cob_delete (cob_file *f)
{
  int ret;
  int read_done = f->flag_read_done;

  f->flag_read_done = 0;

  if (f->open_mode == COB_OPEN_CLOSED || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_STATUS_49_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done)
    RETURN_STATUS (COB_STATUS_43_READ_NOT_DONE);

  ret = fileio_funcs[(int) f->organization]->delete (f);

  RETURN_STATUS (ret);
}


#if defined(HAVE_DBOPEN) | defined(WITH_DB)

static const unsigned char *old_sequence;

void
cob_sort_init (cob_file *f, int nkeys, const unsigned char *collating_sequence)
{
  f->file = malloc (sizeof (struct sort_file));
  f->keys = malloc (sizeof (cob_file_key) * nkeys);
  f->nkeys = 0;

  old_sequence = cob_current_module->collating_sequence;
  if (collating_sequence)
    cob_current_module->collating_sequence = collating_sequence;
}

void
cob_sort_finish (cob_file *f)
{
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
  while (1)
    {
      cob_read (data_file, 0);
      if (data_file->file_status[0] != '0')
	break;
      memcpy (sort_file->record->data, data_file->record->data,
	      sort_file->record->size);
      cob_write (sort_file, sort_file->record, 0);
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_sort_giving (cob_file *sort_file, cob_file *data_file)
{
  cob_open (data_file, COB_OPEN_OUTPUT, 0);
  while (1)
    {
      cob_read (sort_file, 0);
      if (sort_file->file_status[0] != '0')
	break;
      memcpy (data_file->record->data,
	      sort_file->record->data,
	      data_file->record->size);
      cob_write (data_file, data_file->record, 0);
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

#endif /* defined(HAVE_DBOPEN) | defined(WITH_DB) */


void
cob_default_error_handle (void)
{
  const char *msg = NULL;
  char *file_status = cob_error_file->file_status;
  int status = cob_d2i (file_status[0]) * 10 + cob_d2i (file_status[1]);

  switch (status)
    {
    case COB_STATUS_10_END_OF_FILE:
      msg = N_("end of file");
      break;
    case COB_STATUS_14_OUT_OF_KEY_RANGE:
      msg = N_("out of key range");
      break;
    case COB_STATUS_21_KEY_INVALID:
      msg = N_("key order not ascending");
      break;
    case COB_STATUS_22_KEY_EXISTS:
      msg = N_("record key already exists");
      break;
    case COB_STATUS_23_KEY_NOT_EXISTS:
      msg = N_("record key not exists");
      break;
    case COB_STATUS_30_PERMANENT_ERROR:
      msg = N_("permanent file error");
      break;
    case COB_STATUS_35_NOT_EXISTS:
      /* no message */
      break;
    case COB_STATUS_37_PERMISSION_DENIED:
      msg = N_("permission denied");
      break;
    case COB_STATUS_41_ALREADY_OPEN:
      msg = N_("file already open");
      break;
    case COB_STATUS_42_NOT_OPEN:
      msg = N_("file not open");
      break;
    case COB_STATUS_43_READ_NOT_DONE:
      msg = N_("READ must be executed first");
      break;
    case COB_STATUS_44_RECORD_OVERFLOW:
      msg = N_("record overflow");
      break;
    case COB_STATUS_46_READ_ERROR:
      msg = N_("failed to read");
      break;
    case COB_STATUS_47_INPUT_DENIED:
      msg = N_("READ and START not allowed");
      break;
    case COB_STATUS_48_OUTPUT_DENIED:
      msg = N_("WRITE not allowed");
      break;
    case COB_STATUS_49_I_O_DENIED:
      msg = N_("DELETE and REWRITE not allowed");
      break;
    default:
      msg = N_("unknown file error");
      break;
    }

  if (msg)
    cob_runtime_error ("%s (STATUS=%02d)", gettext (msg), status);
}


void
cob_init_fileio (void)
{
  fileio_funcs[COB_ORG_SEQUENTIAL] = &sequential_funcs;
  fileio_funcs[COB_ORG_LINE_SEQUENTIAL] = &lineseq_funcs;
  fileio_funcs[COB_ORG_RELATIVE] = &relative_funcs;
#if defined(HAVE_DBOPEN) | defined(WITH_DB)
  fileio_funcs[COB_ORG_INDEXED] = &indexed_funcs;
  fileio_funcs[COB_ORG_SORT] = &sort_funcs;
#endif
}
