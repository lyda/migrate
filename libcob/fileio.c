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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>

#if HAVE_DB1_DB_H
#include <db1/db.h>
#else
#if HAVE_DB_185_H
#include <db_185.h>
#else
#if HAVE_DB_H
#include <db.h>
#endif
#endif
#endif

#include "move.h"
#include "numeric.h"
#include "fileio.h"
#include "lib/gettext.h"

cob_file *cob_error_file;

static cob_fileio_funcs *fileio_funcs[COB_ORG_MAX];


/*
 * SEQUENTIAL
 */

struct sequential_file {
  int fd;
};

static int
sequential_open (cob_file *f, char *filename, int mode)
{
  int fd;
  int flags = 0;
  struct sequential_file *p;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags = O_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags = O_RDWR | O_CREAT | O_TRUNC;
      break;
    case COB_OPEN_I_O:
      flags = O_RDWR;
      break;
    case COB_OPEN_EXTEND:
      flags = O_RDWR | O_APPEND | (f->flag_optional ? O_CREAT : 0);
      break;
    }

#ifdef __MINGW32__
  flags |= O_BINARY;
#endif

  fd = open (filename, flags, COB_FILE_MODE);
  if (fd == -1)
    return errno;

  p = malloc (sizeof (struct sequential_file));
  p->fd = fd;
  f->file = p;
  return 0;
}

static int
sequential_close (cob_file *f, int opt)
{
  struct sequential_file *p = f->file;

  switch (opt)
    {
    case COB_CLOSE_NORMAL:
    case COB_CLOSE_LOCK:
      close (p->fd);
      free (p);
      return COB_FILE_SUCCEED;
    default:
      return COB_FILE_SUCCEED_NO_REEL;
    }
}

static int
sequential_read (cob_file *f)
{
  struct sequential_file *p = f->file;

  if (f->record_min != f->record_max)
    {
      if (read (p->fd, &f->record->size, sizeof (f->record->size)) <= 0)
	return COB_FILE_END_OF_FILE;
    }

  if (read (p->fd, f->record->data, f->record->size) <= 0)
    return COB_FILE_END_OF_FILE;

  return COB_FILE_SUCCEED;
}

static int
sequential_write (cob_file *f)
{
  struct sequential_file *p = f->file;

  if (f->record_min != f->record_max)
    write (p->fd, &f->record->size, sizeof (f->record->size));

  write (p->fd, f->record->data, f->record->size);
  return COB_FILE_SUCCEED;
}

static int
sequential_rewrite (cob_file *f, cob_field *rec)
{
  struct sequential_file *p = f->file;

  if (rec->size != f->record->size)
    return COB_FILE_RECORD_OVERFLOW;

  if (f->record_size)
    if (f->record->size != cob_to_int (f->record_size))
      return COB_FILE_RECORD_OVERFLOW;

  lseek (p->fd, - f->record->size, SEEK_CUR);
  write (p->fd, f->record->data, f->record->size);
  return COB_FILE_SUCCEED;
}

static cob_fileio_funcs sequential_funcs = {
  sequential_open,
  sequential_close,
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

struct lineseq_file {
  FILE *fp;
};

static int
lineseq_open (cob_file *f, char *filename, int mode)
{
  FILE *fp = NULL;
  struct lineseq_file *p;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      fp = fopen (filename, "r");
      break;
    case COB_OPEN_OUTPUT:
      fp = fopen (filename, "w");
      break;
    case COB_OPEN_I_O:
      fp = fopen (filename, "rw");
      break;
    case COB_OPEN_EXTEND:
      fp = fopen (filename, "w+");
      break;
    }
  if (fp == NULL)
    return errno;

  p = malloc (sizeof (struct lineseq_file));
  p->fp = fp;
  f->file = p;
  return 0;
}

static int
lineseq_close (cob_file *f, int opt)
{
  struct lineseq_file *p = f->file;
  fclose (p->fp);
  free (p);
  return COB_FILE_SUCCEED;
}

static int
lineseq_read (cob_file *f)
{
  size_t i;
  char buff[f->record->size + 1];
  struct lineseq_file *p = f->file;

  /* read the file */
  if (fgets (buff, f->record->size + 1, p->fp) == NULL)
    return COB_FILE_END_OF_FILE;

  /* remove the newline */
  for (i = 0; i < f->record->size; i++)
    if (buff[i] == '\r' || buff[i] == '\n')
      break;
  if (i < f->record->size)
    {
      /* replace the inline newline by spaces */
      for (; i < f->record->size; i++)
	buff[i] = ' ';
    }
  else
    {
      /* discard input until the next newline */
      int c = getc (p->fp);
      while (c != '\r' && c != '\n' && c != EOF)
	c = getc (p->fp);
      if (c == '\r')
	c = getc (p->fp);
      if (c != '\n' && c != EOF)
	ungetc (c, p->fp);
    }

  memcpy (f->record->data, buff, f->record->size);

  return COB_FILE_SUCCEED;
}

static int
lineseq_write (cob_file *f)
{
  int i, size;
  struct lineseq_file *p = f->file;

  /* determine the size to be written */
  for (i = f->record->size - 1; i >= 0; i--)
    if (f->record->data[i] != ' ')
      break;
  size = i + 1;

  /* write to the file */
  for (i = 0; i < size; i++)
    putc (f->record->data[i], p->fp);
  putc ('\n', p->fp);

  return COB_FILE_SUCCEED;
}

static cob_fileio_funcs lineseq_funcs = {
  lineseq_open,
  lineseq_close,
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

#define RELATIVE_SEEK(f,fd,i)						\
  if (lseek (fd, RELATIVE_SIZE (f) * (i), SEEK_SET) == -1		\
      || read (fd, &f->record->size, sizeof (f->record->size)) <= 0)	\
    return COB_FILE_KEY_NOT_EXISTS;					\
  lseek (fd, - sizeof (f->record->size), SEEK_CUR);

static int
relative_open (cob_file *f, char *filename, int mode)
{
  return sequential_open (f, filename, mode);
}

static int
relative_close (cob_file *f, int opt)
{
  return sequential_close (f, opt);
}

static int
relative_start (cob_file *f, int cond, cob_field *k)
{
  struct sequential_file *p = f->file;

  /* get the index */
  int index = cob_to_int (k) - 1;
  if (cond == COB_LT)
    index--;
  else if (cond == COB_GT)
    index++;

  /* seek the index */
  while (1)
    {
      RELATIVE_SEEK (f, p->fd, index);

      /* check if a valid record */
      if (f->record->size > 0)
	{
	  cob_set_int (k, index + 1);
	  return COB_FILE_SUCCEED;
	}

      /* continue */
      switch (cond)
	{
	case COB_EQ:
	  return COB_FILE_KEY_NOT_EXISTS;
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
  struct sequential_file *p = f->file;

  RELATIVE_SEEK (f, p->fd, cob_to_int (k) - 1);

  if (f->record->size == 0)
    return COB_FILE_KEY_NOT_EXISTS;

  lseek (p->fd, sizeof (f->record->size), SEEK_CUR);
  read (p->fd, f->record->data, f->record_max);
  return COB_FILE_SUCCEED;
}

static int
relative_read_next (cob_file *f)
{
  struct sequential_file *p = f->file;

  while (1)
    {
      if (read (p->fd, &f->record->size, sizeof (f->record->size)) <= 0)
	return COB_FILE_END_OF_FILE;

      if (f->keys[0].field)
	{
	  if (f->flag_first_read)
	    {
	      cob_set_int (f->keys[0].field, 1);
	      f->flag_first_read = 0;
	    }
	  else
	    {
	      cob_add_int (f->keys[0].field, 1);
	      if (cob_error_code)
		{
		  lseek (p->fd, - sizeof (f->record->size), SEEK_CUR);
		  return COB_FILE_OUT_OF_KEY_RANGE;
		}
	    }
	}

      if (f->record->size > 0)
	{
	  read (p->fd, f->record->data, f->record_max);
	  return COB_FILE_SUCCEED;
	}

      lseek (p->fd, f->record_max, SEEK_CUR);
    }
}

static int
relative_write (cob_file *f)
{
  size_t size;
  struct sequential_file *p = f->file;

  if (f->access_mode != COB_ACCESS_SEQUENTIAL)
    {
      int index = cob_to_int (f->keys[0].field) - 1;
      lseek (p->fd, RELATIVE_SIZE (f) * index, SEEK_SET);
    }

  if (read (p->fd, &size, sizeof (size)) > 0)
    {
      lseek (p->fd, - sizeof (size), SEEK_CUR);
      if (size > 0)
	return COB_FILE_KEY_EXISTS;
    }

  write (p->fd, &f->record->size, sizeof (f->record->size));
  write (p->fd, f->record->data, f->record_max);

  /* update RELATIVE KEY */
  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (f->keys[0].field)
      cob_set_int (f->keys[0].field,
		   lseek (p->fd, 0, SEEK_CUR) / RELATIVE_SIZE (f));

  return COB_FILE_SUCCEED;
}

static int
relative_rewrite (cob_file *f, cob_field *rec)
{
  struct sequential_file *p = f->file;
  lseek (p->fd, - f->record_max, SEEK_CUR);
  write (p->fd, f->record->data, f->record_max);
  return COB_FILE_SUCCEED;
}

static int
relative_delete (cob_file *f)
{
  struct sequential_file *p = f->file;

  RELATIVE_SEEK (f, p->fd, cob_to_int (f->keys[0].field) - 1);

  f->record->size = 0;
  write (p->fd, &f->record->size, sizeof (f->record->size));
  lseek (p->fd, f->record_max, SEEK_CUR);
  return COB_FILE_SUCCEED;
}

static cob_fileio_funcs relative_funcs = {
  relative_open,
  relative_close,
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
indexed_open (cob_file *f, char *filename, int mode)
{
  int i, j;
  int flags = 0;
  struct indexed_file *p;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags = O_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags = O_RDWR | O_CREAT | O_TRUNC;
      break;
    case COB_OPEN_I_O:
      flags = O_RDWR | O_CREAT;
      break;
    case COB_OPEN_EXTEND:
      flags = O_RDWR | (f->flag_optional ? O_CREAT : 0);
      break;
    }

#ifdef __MINGW32__
  flags |= O_BINARY;
#endif

  p = malloc (sizeof (struct indexed_file));
  p->db = malloc (sizeof (DB *) * f->nkeys);
  for (i = 0; i < f->nkeys; i++)
    {
      BTREEINFO info;
      char name[BUFSIZ];

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

  return COB_FILE_SUCCEED;
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

  return (ret == 0) ? COB_FILE_SUCCEED : COB_FILE_KEY_NOT_EXISTS;
}

static int
indexed_read (cob_file *f, cob_field *key)
{
  struct indexed_file *p = f->file;

  int ret = indexed_start (f, COB_EQ, key);
  if (ret != COB_FILE_SUCCEED)
    return ret;

  f->record->size = p->data.size;
  memcpy (f->record->data, p->data.data, p->data.size);

  return COB_FILE_SUCCEED;
}

static int
indexed_read_next (cob_file *f)
{
  struct indexed_file *p = f->file;

  if (f->flag_first_read)
    {
      /* data is read in indexed_open or indexed_start */
      if (p->data.data == 0)
	return COB_FILE_END_OF_FILE;
    }
  else
    {
      if (DB_SEQ (p->db[p->key_index], R_NEXT) != 0)
	return COB_FILE_END_OF_FILE;
      if (p->key_index > 0)
	{
	  p->key = p->data;
	  if (DB_GET (p->db[0], 0) != 0)
	    return COB_FILE_KEY_NOT_EXISTS;
	}
    }

  f->record->size = p->data.size;
  memcpy (f->record->data, p->data.data, p->data.size);

  return COB_FILE_SUCCEED;
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
    return COB_FILE_KEY_EXISTS;

  /* write secondary keys */
  p->data = p->key;
  for (i = 1; i < f->nkeys; i++)
    {
      int flags = f->keys[i].flag ? 0 : R_NOOVERWRITE;
      DBT_SET (p->key, f->keys[i].field);
      if (DB_PUT (p->db[i], flags) != 0)
	return COB_FILE_KEY_EXISTS;
    }

  return COB_FILE_SUCCEED;
}

static int
indexed_write (cob_file *f)
{
  struct indexed_file *p = f->file;

  /* check record key */
  DBT_SET (p->key, f->keys[0].field);
  if (!p->last_key)
    p->last_key = malloc (p->key.size);
  else if (f->access_mode == COB_ACCESS_SEQUENTIAL
	   && memcmp (p->last_key, p->key.data, p->key.size) > 0)
    return COB_FILE_KEY_INVALID;
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
	return COB_FILE_KEY_NOT_EXISTS;
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

  return COB_FILE_SUCCEED;
}

static int
indexed_rewrite (cob_file *f, cob_field *rec)
{
  struct indexed_file *p = f->file;

  /* delete the current record */
  int ret = indexed_delete (f);
  if (ret != COB_FILE_SUCCEED)
    return ret;

  /* write data */
  DBT_SET (p->key, f->keys[0].field);
  ret = indexed_write_internal (f);

  /* set cursor to the next */
  DB_SEQ (p->db[p->key_index], R_NEXT);

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


/*
 * SORT
 */

struct sort_file {
  DB *db;
  cob_file_key *sort_keys;
  DBT key, data;
};

static cob_file *current_sort_file;

static int
sort_compare (const DBT *k1, const DBT *k2)
{
  int cmp;
  unsigned int i;
  cob_file *f = current_sort_file;
  struct sort_file *p = f->file;
  for (i = 0; i < f->nkeys; i++)
    {
      cob_field f1 = *(p->sort_keys[i].field);
      cob_field f2 = *(p->sort_keys[i].field);
      f1.data += ((unsigned char *) k1->data) - f->record->data;
      f2.data += ((unsigned char *) k2->data) - f->record->data;
      cmp = cob_cmp (&f1, &f2);
      if (cmp != 0)
	return (p->sort_keys[i].flag == COB_ASCENDING) ? cmp : - cmp;
    }
  return 0;
}

static int
sort_open (cob_file *f, char *filename, int mode)
{
  BTREEINFO info;
  struct sort_file *p = f->file;
  int flags = 0;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags = O_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags = O_RDWR | O_CREAT | O_TRUNC;
      break;
    }

#ifdef __MINGW32__
  flags |= O_BINARY;
#endif

  /* open db */
  memset (&info, 0, sizeof (info));
  info.flags = R_DUP;
  info.compare = sort_compare;
  p->db = dbopen (filename, flags, COB_FILE_MODE, DB_BTREE, &info);
  if (p->db == NULL)
    return errno;

  memset (&p->key, 0, sizeof (DBT));
  memset (&p->data, 0, sizeof (DBT));
  DB_SEQ (p->db, R_FIRST);
  return 0;
}

static int
sort_close (cob_file *f, int opt)
{
  struct sort_file *p = f->file;
  DB_CLOSE (p->db);
  return COB_FILE_SUCCEED;
}

static int
sort_read (cob_file *f)
{
  struct sort_file *p = f->file;
  if (f->flag_first_read == 0)
    if (DB_SEQ (p->db, R_NEXT) != 0)
      return COB_FILE_END_OF_FILE;

  memcpy (f->record->data, p->key.data, p->key.size);
  return COB_FILE_SUCCEED;
}

static int
sort_write (cob_file *f)
{
  struct sort_file *p = f->file;
  current_sort_file = f;
  p->key.data = f->record->data;
  p->key.size = f->record->size;
  DB_PUT (p->db, 0);

  return COB_FILE_SUCCEED;
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


/*
 * Public interface
 */

#define RETURN_STATUS(x)	do { save_status (f, x); return; } while (0)

static void
save_status (cob_file *f, int status)
{
  static char dummy_status[2];
  static int error_code[] = {
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
    f->file_status = dummy_status;

  f->file_status[0] = status / 10 + '0';
  f->file_status[1] = status % 10 + '0';
  cob_error_file = f;
  cob_error_code = error_code[status / 10];
}

void
cob_open (cob_file *f, int mode)
{
  int was_not_exist = 0;
  char filename[FILENAME_MAX];
  struct stat st;

  f->flag_read_done = 0;

  /* check if the file is already open */
  if (f->flag_opened)
    RETURN_STATUS (COB_FILE_ALREADY_OPEN);

  cob_field_to_string (f->assign, filename);
  if ((mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND)
      && f->flag_optional != 0
      && stat (filename, &st) == -1)
    was_not_exist = 1;

  f->open_mode = mode;
  f->flag_opened = 0;
  f->flag_nonexistent = 0;
  f->flag_end_of_file = 0;
  f->flag_first_read = 1;

  switch (fileio_funcs[(int) f->organization]->open (f, filename, mode))
    {
    case 0:
      f->flag_opened = 1;
      if (was_not_exist)
	RETURN_STATUS (COB_FILE_SUCCEED_OPTIONAL);
      else
	RETURN_STATUS (COB_FILE_SUCCEED);
    case ENOENT:
      if (f->flag_optional)
	{
	  f->flag_opened = 1;
	  f->flag_nonexistent = 1;
	  f->flag_end_of_file = 1;
	  RETURN_STATUS (COB_FILE_SUCCEED_OPTIONAL);
	}
      else
	{
	  RETURN_STATUS (COB_FILE_NOT_EXISTS);
	}
    case EACCES:
    case EISDIR:
    case EROFS:
      RETURN_STATUS (COB_FILE_PERMISSION_DENIED);
    default:
      RETURN_STATUS (COB_FILE_PERMANENT_ERROR);
    }
}

void
cob_close (cob_file *f, int opt)
{
  int ret;

  f->flag_read_done = 0;

  if (f->flag_opened == 0)
    RETURN_STATUS (COB_FILE_NOT_OPEN);

  if (f->flag_nonexistent)
    {
      f->flag_opened = 0;
      RETURN_STATUS (COB_FILE_SUCCEED);
    }

  ret = fileio_funcs[(int) f->organization]->close (f, opt);
  if (ret == COB_FILE_SUCCEED)
    f->flag_opened = 0;

  RETURN_STATUS (ret);
}

void
cob_start (cob_file *f, int cond, cob_field *key)
{
  int ret;

  f->flag_read_done = 0;
  f->flag_first_read = 0;

  if (f->flag_nonexistent)
    RETURN_STATUS (COB_FILE_KEY_NOT_EXISTS);

  if (f->flag_opened == 0
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND
      || f->access_mode == COB_ACCESS_RANDOM)
    RETURN_STATUS (COB_FILE_INPUT_DENIED);

  ret = fileio_funcs[(int) f->organization]->start (f, cond, key);
  if (ret == COB_FILE_SUCCEED)
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
	RETURN_STATUS (COB_FILE_KEY_NOT_EXISTS);
      f->flag_first_read = 0;
      RETURN_STATUS (COB_FILE_END_OF_FILE);
    }

  if (f->flag_end_of_file)
    RETURN_STATUS (COB_FILE_READ_ERROR);

  if (f->flag_opened == 0
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND)
    RETURN_STATUS (COB_FILE_INPUT_DENIED);

  if (key)
    ret = fileio_funcs[(int) f->organization]->read (f, key);
  else
    ret = fileio_funcs[(int) f->organization]->read_next (f);

  switch (ret)
    {
    case COB_FILE_SUCCEED:
      f->flag_first_read = 0;
      f->flag_read_done = 1;
      if (f->record_size)
	cob_set_int (f->record_size, f->record->size);
      break;
    case COB_FILE_END_OF_FILE:
      f->flag_end_of_file = 1;
      break;
    }

  RETURN_STATUS (ret);
}

void
cob_write (cob_file *f, cob_field *rec)
{
  int ret;

  f->flag_read_done = 0;

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    {
      if (f->flag_opened == 0
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_I_O)
	RETURN_STATUS (COB_FILE_OUTPUT_DENIED);
    }
  else
    {
      if (f->flag_opened == 0
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_EXTEND)
	RETURN_STATUS (COB_FILE_OUTPUT_DENIED);
    }

  if (f->record_size)
    f->record->size = cob_to_int (f->record_size);
  else
    f->record->size = rec->size;

  if (f->record->size < f->record_min || f->record_max < f->record->size)
    RETURN_STATUS (COB_FILE_RECORD_OVERFLOW);

  ret = fileio_funcs[(int) f->organization]->write (f);

  RETURN_STATUS (ret);
}

void
cob_write_page (cob_file *f)
{
  struct sequential_file *p = f->file;

  if (f->flag_opened == 0
      || f->open_mode == COB_OPEN_INPUT
      || f->open_mode == COB_OPEN_I_O)
    return;

  write (p->fd, "\f", 1);
}

void
cob_write_lines (cob_file *f, int lines)
{
  int i;
  struct sequential_file *p = f->file;

  if (f->flag_opened == 0
      || f->open_mode == COB_OPEN_INPUT
      || f->open_mode == COB_OPEN_I_O)
    return;

  for (i = 0; i < lines; i++)
    write (p->fd, "\n", 1);
}

void
cob_rewrite (cob_file *f, cob_field *rec)
{
  int ret;
  int read_done = f->flag_read_done;

  f->flag_read_done = 0;

  if (f->flag_opened == 0 || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_FILE_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (!read_done)
      RETURN_STATUS (COB_FILE_READ_NOT_DONE);

  ret = fileio_funcs[(int) f->organization]->rewrite (f, rec);

  RETURN_STATUS (ret);
}

void
cob_delete (cob_file *f)
{
  int ret;
  int read_done = f->flag_read_done;

  f->flag_read_done = 0;

  if (f->flag_opened == 0 || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_FILE_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (!read_done)
      RETURN_STATUS (COB_FILE_READ_NOT_DONE);

  ret = fileio_funcs[(int) f->organization]->delete (f);

  RETURN_STATUS (ret);
}

void
cob_sort_init (cob_file *sort_file, int nkeys, cob_file_key *keys)
{
  struct sort_file *p = malloc (sizeof (struct sort_file));
  sort_file->file = p;
  sort_file->nkeys = nkeys;
  p->sort_keys = keys;
}

void
cob_sort_using (cob_file *sort_file, cob_file *data_file)
{
  cob_field temp = {0, 0, 0};
  cob_open (data_file, COB_OPEN_INPUT);
  while (1)
    {
      cob_read (data_file, 0);
      if (data_file->file_status[0] != '0')
	break;
      memcpy (sort_file->record->data, data_file->record->data,
	      sort_file->record->size);
      temp.size = sort_file->record->size;
      temp.data = sort_file->record->data;
      cob_write (sort_file, &temp);
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_sort_giving (cob_file *sort_file, cob_file *data_file)
{
  cob_field temp = {0, 0, 0};
  cob_open (data_file, COB_OPEN_OUTPUT);
  while (1)
    {
      cob_read (sort_file, 0);
      if (sort_file->file_status[0] != '0')
	break;
      memcpy (data_file->record->data,
	      sort_file->record->data,
	      data_file->record->size);
      temp.size = data_file->record->size;
      temp.data = data_file->record->data;
      cob_write (data_file, &temp);
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_default_error_handle (cob_file *f)
{
  const char *msg = NULL;
  int status = (f->file_status[0] - '0') * 10 + (f->file_status[1] - '0');
  switch (status)
    {
    case COB_FILE_END_OF_FILE:
      msg = N_("end of file");
      break;
    case COB_FILE_OUT_OF_KEY_RANGE:
      msg = N_("out of key range");
      break;
    case COB_FILE_KEY_INVALID:
      msg = N_("key order not ascending");
      break;
    case COB_FILE_KEY_EXISTS:
      msg = N_("record key already exists");
      break;
    case COB_FILE_KEY_NOT_EXISTS:
      msg = N_("record key not exists");
      break;
    case COB_FILE_PERMANENT_ERROR:
      msg = N_("permanent file error");
      break;
    case COB_FILE_NOT_EXISTS:
      /* no message */
      break;
    case COB_FILE_PERMISSION_DENIED:
      msg = N_("permission denied");
      break;
    case COB_FILE_ALREADY_OPEN:
      msg = N_("file already open");
      break;
    case COB_FILE_NOT_OPEN:
      msg = N_("file not open");
      break;
    case COB_FILE_READ_NOT_DONE:
      msg = N_("READ must be executed first");
      break;
    case COB_FILE_RECORD_OVERFLOW:
      msg = N_("record overflow");
      break;
    case COB_FILE_READ_ERROR:
      msg = N_("failed to read");
      break;
    case COB_FILE_INPUT_DENIED:
      msg = N_("READ and START not allowed");
      break;
    case COB_FILE_OUTPUT_DENIED:
      msg = N_("WRITE not allowed");
      break;
    case COB_FILE_I_O_DENIED:
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
  fileio_funcs[COB_ORG_INDEXED] = &indexed_funcs;
  fileio_funcs[COB_ORG_SORT] = &sort_funcs;
}
