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
#if HAVE_DB_H
#include <db.h>
#endif
#endif

#include "move.h"
#include "numeric.h"
#include "fileio.h"
#include "lib/gettext.h"

struct cob_file *cob_last_file;
char cob_dummy_status[2];

static struct cob_fileio_funcs *fileio_funcs[COB_ORG_MAX];


/*
 * SEQUENTIAL
 */

struct sequential_file {
  int fd;
};

static int
sequential_open (struct cob_file *f, char *filename, int mode)
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
      flags = O_RDWR | O_APPEND | (f->f.optional ? O_CREAT : 0);
      break;
    }

  fd = open (filename, flags, COB_FILE_MODE);
  if (fd == -1)
    return errno;

  p = malloc (sizeof (struct sequential_file));
  p->fd = fd;
  f->file = p;
  return 0;
}

static int
sequential_close (struct cob_file *f, int opt)
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
sequential_read (struct cob_file *f)
{
  struct sequential_file *p = f->file;

  if (f->record_min != f->record_max)
    {
      if (read (p->fd, &f->record_size, sizeof (f->record_size)) <= 0)
	return COB_FILE_END_OF_FILE;
    }

  if (read (p->fd, f->record_data, f->record_size) <= 0)
    return COB_FILE_END_OF_FILE;

  return COB_FILE_SUCCEED;
}

static int
sequential_write (struct cob_file *f)
{
  struct sequential_file *p = f->file;

  if (f->record_min != f->record_max)
    write (p->fd, &f->record_size, sizeof (f->record_size));

  write (p->fd, f->record_data, f->record_size);
  return COB_FILE_SUCCEED;
}

static int
sequential_rewrite (struct cob_file *f, struct cob_field rec)
{
  struct sequential_file *p = f->file;

  if (rec.size != f->record_size)
    return COB_FILE_RECORD_OVERFLOW;

  if (COB_FIELD_IS_VALID (f->record_depending))
    if (f->record_size != cob_to_int (f->record_depending))
      return COB_FILE_RECORD_OVERFLOW;

  lseek (p->fd, - f->record_size, SEEK_CUR);
  write (p->fd, f->record_data, f->record_size);
  return COB_FILE_SUCCEED;
}

static struct cob_fileio_funcs sequential_funcs = {
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
lineseq_open (struct cob_file *f, char *filename, int mode)
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
lineseq_close (struct cob_file *f, int opt)
{
  struct lineseq_file *p = f->file;
  fclose (p->fp);
  free (p);
  return COB_FILE_SUCCEED;
}

static int
lineseq_read (struct cob_file *f)
{
  size_t i;
  char buff[f->record_size + 1];
  struct lineseq_file *p = f->file;

  /* read the file */
  if (fgets (buff, f->record_size + 1, p->fp) == NULL)
    return COB_FILE_END_OF_FILE;

  /* remove the newline */
  for (i = 0; i < f->record_size; i++)
    if (buff[i] == '\r' || buff[i] == '\n')
      break;
  if (i < f->record_size)
    {
      /* replace the inline newline by spaces */
      for (; i < f->record_size; i++)
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

  memcpy (f->record_data, buff, f->record_size);

  return COB_FILE_SUCCEED;
}

static int
lineseq_write (struct cob_file *f)
{
  int i, size;
  struct lineseq_file *p = f->file;

  /* determine the size to be written */
  for (i = f->record_size - 1; i >= 0; i--)
    if (f->record_data[i] != ' ')
      break;
  size = i + 1;

  /* write to the file */
  for (i = 0; i < size; i++)
    putc (f->record_data[i], p->fp);
  putc ('\n', p->fp);

  return COB_FILE_SUCCEED;
}

static struct cob_fileio_funcs lineseq_funcs = {
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

#define RELATIVE_SIZE(f) (f->record_max + sizeof (f->record_size))

#define RELATIVE_SEEK(f,fd,i)						\
  if (lseek (fd, RELATIVE_SIZE (f) * (i), SEEK_SET) == -1		\
      || read (fd, &f->record_size, sizeof (f->record_size)) <= 0)	\
    return COB_FILE_KEY_NOT_EXISTS;					\
  lseek (fd, - sizeof (f->record_size), SEEK_CUR);

static int
relative_open (struct cob_file *f, char *filename, int mode)
{
  return sequential_open (f, filename, mode);
}

static int
relative_close (struct cob_file *f, int opt)
{
  return sequential_close (f, opt);
}

static int
relative_start (struct cob_file *f, int cond, struct cob_field k)
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
      if (f->record_size > 0)
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
relative_read (struct cob_file *f, struct cob_field k)
{
  struct sequential_file *p = f->file;

  RELATIVE_SEEK (f, p->fd, cob_to_int (k) - 1);

  if (f->record_size == 0)
    return COB_FILE_KEY_NOT_EXISTS;

  lseek (p->fd, sizeof (f->record_size), SEEK_CUR);
  read (p->fd, f->record_data, f->record_max);
  return COB_FILE_SUCCEED;
}

static int
relative_read_next (struct cob_file *f)
{
  struct sequential_file *p = f->file;

  while (1)
    {
      if (read (p->fd, &f->record_size, sizeof (f->record_size)) <= 0)
	return COB_FILE_END_OF_FILE;

      if (COB_FIELD_IS_VALID (f->keys[0].field))
	{
	  if (f->f.first_read)
	    {
	      cob_set_int (f->keys[0].field, 1);
	      f->f.first_read = 0;
	    }
	  else
	    {
	      cob_add_int (f->keys[0].field, 1, 0, 0);
	    }
	  if (cob_status != 0)
	    {
	      lseek (p->fd, - sizeof (f->record_size), SEEK_CUR);
	      return 14;
	    }
	}

      if (f->record_size > 0)
	{
	  read (p->fd, f->record_data, f->record_max);
	  return COB_FILE_SUCCEED;
	}

      lseek (p->fd, f->record_max, SEEK_CUR);
    }
}

static int
relative_write (struct cob_file *f)
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

  write (p->fd, &f->record_size, sizeof (f->record_size));
  write (p->fd, f->record_data, f->record_max);

  /* update RELATIVE KEY */
  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (COB_FIELD_IS_VALID (f->keys[0].field))
      cob_set_int (f->keys[0].field,
		   lseek (p->fd, 0, SEEK_CUR) / RELATIVE_SIZE (f));

  return COB_FILE_SUCCEED;
}

static int
relative_rewrite (struct cob_file *f, struct cob_field rec)
{
  struct sequential_file *p = f->file;
  lseek (p->fd, - f->record_max, SEEK_CUR);
  write (p->fd, f->record_data, f->record_max);
  return COB_FILE_SUCCEED;
}

static int
relative_delete (struct cob_file *f)
{
  struct sequential_file *p = f->file;

  RELATIVE_SEEK (f, p->fd, cob_to_int (f->keys[0].field) - 1);

  f->record_size = 0;
  write (p->fd, &f->record_size, sizeof (f->record_size));
  lseek (p->fd, f->record_max, SEEK_CUR);
  return COB_FILE_SUCCEED;
}

static struct cob_fileio_funcs relative_funcs = {
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
  key.data = fld.data;				\
  key.size = fld.size;

struct indexed_file {
  int key_index;
  unsigned char *last_key;	/* the last key written */
  DB **db;			/* database handlers */
  DBT key, data;
};

static int
indexed_open (struct cob_file *f, char *filename, int mode)
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
      flags = O_RDWR | (f->f.optional ? O_CREAT : 0);
      break;
    }

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
      if (f->keys[i].duplicates)
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
indexed_close (struct cob_file *f, int opt)
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
indexed_start (struct cob_file *f, int cond, struct cob_field key)
{
  int ret;
  struct indexed_file *p = f->file;

  /* look up for the key */
  for (p->key_index = 0; p->key_index < f->nkeys; p->key_index++)
    if (f->keys[p->key_index].field.data == key.data)
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
	ret = memcmp (p->key.data, key.data, key.size);
      break;
    case COB_LT:
    case COB_LE:
      if (ret != 0)
	ret = DB_SEQ (p->db[p->key_index], R_LAST);
      else if (cond == COB_LT
	       || memcmp (p->key.data, key.data, key.size) != 0)
	ret = DB_SEQ (p->db[p->key_index], R_PREV);
      break;
    case COB_GT:
      while (ret == 0 && memcmp (p->key.data, key.data, key.size) == 0)
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
indexed_read (struct cob_file *f, struct cob_field key)
{
  struct indexed_file *p = f->file;

  int ret = indexed_start (f, COB_EQ, key);
  if (ret != COB_FILE_SUCCEED)
    return ret;

  f->record_size = p->data.size;
  memcpy (f->record_data, p->data.data, p->data.size);

  return COB_FILE_SUCCEED;
}

static int
indexed_read_next (struct cob_file *f)
{
  struct indexed_file *p = f->file;

  if (!f->f.first_read)
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

  f->record_size = p->data.size;
  memcpy (f->record_data, p->data.data, p->data.size);

  return COB_FILE_SUCCEED;
}

static int
indexed_write_internal (struct cob_file *f)
{
  int i;
  struct indexed_file *p = f->file;

  /* write data */
  p->data.data = f->record_data;
  p->data.size = f->record_size;
  if (DB_PUT (p->db[0], R_NOOVERWRITE) != 0)
    return COB_FILE_KEY_EXISTS;

  /* write secondary keys */
  p->data = p->key;
  for (i = 1; i < f->nkeys; i++)
    {
      int flags = f->keys[i].duplicates ? 0 : R_NOOVERWRITE;
      DBT_SET (p->key, f->keys[i].field);
      if (DB_PUT (p->db[i], flags) != 0)
	return COB_FILE_KEY_EXISTS;
    }

  return COB_FILE_SUCCEED;
}

static int
indexed_write (struct cob_file *f)
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
indexed_delete (struct cob_file *f)
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
  offset = p->data.data - (void *) f->record_data;
  for (i = 1; i < f->nkeys; i++)
    {
      DBT_SET (p->key, f->keys[i].field);
      p->key.data += offset;
      if (!f->keys[i].duplicates)
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
indexed_rewrite (struct cob_file *f, struct cob_field rec)
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

static struct cob_fileio_funcs indexed_funcs = {
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
  struct cob_sort_key {
    int dir;
    struct cob_field field;
  } *sort_keys;
  DBT key, data;
};

static struct cob_file *current_sort_file;

static int
sort_compare (const DBT *k1, const DBT *k2)
{
  int cmp;
  unsigned int i;
  struct cob_file *f = current_sort_file;
  struct sort_file *p = f->file;
  for (i = 0; i < f->nkeys; i++)
    {
      struct cob_field f1 = p->sort_keys[i].field;
      struct cob_field f2 = p->sort_keys[i].field;
      f1.data += ((unsigned char *) k1->data) - f->record_data;
      f2.data += ((unsigned char *) k2->data) - f->record_data;
      if (f1.desc &&
	  (f1.desc->type == COB_DISPLAY || f1.desc->type == COB_BINARY))
	{
	  cob_decimal_set_field (cob_d1, f1);
	  cob_decimal_set_field (cob_d2, f2);
	  cmp = cob_decimal_cmp (cob_d1, cob_d2);
	}
      else
	cmp = cob_cmp_field (f1, f2);
      if (cmp != 0)
	return (p->sort_keys[i].dir == COB_ASCENDING) ? cmp : - cmp;
    }
  return 0;
}

static int
sort_open (struct cob_file *f, char *filename, int mode)
{
  DB *db;
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

  /* open db */
  memset (&info, 0, sizeof (info));
  info.flags = R_DUP;
  info.compare = sort_compare;
  db = dbopen (filename, flags, COB_FILE_MODE, DB_BTREE, &info);
  if (db == NULL)
    return errno;

  p->db = db;
  memset (&p->key, 0, sizeof (DBT));
  memset (&p->data, 0, sizeof (DBT));
  DB_SEQ (p->db, R_FIRST);
  return 0;
}

static int
sort_close (struct cob_file *f, int opt)
{
  struct sort_file *p = f->file;
  DB_CLOSE (p->db);
  return COB_FILE_SUCCEED;
}

static int
sort_read (struct cob_file *f)
{
  struct sort_file *p = f->file;
  if (!f->f.first_read)
    if (DB_SEQ (p->db, R_NEXT) != 0)
      return COB_FILE_END_OF_FILE;

  memcpy (f->record_data, p->key.data, p->key.size);
  return COB_FILE_SUCCEED;
}

static int
sort_write (struct cob_file *f)
{
  struct sort_file *p = f->file;
  current_sort_file = f;
  p->key.data = f->record_data;
  p->key.size = f->record_size;
  DB_PUT (p->db, 0);

  return COB_FILE_SUCCEED;
}

static struct cob_fileio_funcs sort_funcs = {
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

#define RETURN_STATUS(x)			\
  do {						\
    f->file_status[0] = x / 10 + '0';		\
    f->file_status[1] = x % 10 + '0';		\
    cob_last_file = f;				\
    return;					\
  } while (0)

void
cob_open (struct cob_file *f, int mode)
{
  int was_not_exist = 0;
  char filename[FILENAME_MAX];
  struct stat st;

  f->f.read_done = 0;

  /* check if the file is already open */
  if (f->f.opened)
    RETURN_STATUS (COB_FILE_ALREADY_OPEN);

  cob_field_to_string (f->assign, filename);
  if ((mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND)
      && f->f.optional != 0
      && stat (filename, &st) == -1)
    was_not_exist = 1;

  f->open_mode = mode;
  f->f.opened = 0;
  f->f.nonexistent = 0;
  f->f.end_of_file = 0;
  f->f.first_read = 1;

  switch (fileio_funcs[f->organization]->open (f, filename, mode))
    {
    case 0:
      f->f.opened = 1;
      if (was_not_exist)
	RETURN_STATUS (COB_FILE_SUCCEED_OPTIONAL);
      else
	RETURN_STATUS (COB_FILE_SUCCEED);
    case ENOENT:
      if (f->f.optional)
	{
	  f->f.opened = 1;
	  f->f.nonexistent = 1;
	  f->f.end_of_file = 1;
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
cob_close (struct cob_file *f, int opt)
{
  int ret;

  f->f.read_done = 0;

  if (!f->f.opened)
    RETURN_STATUS (COB_FILE_NOT_OPEN);

  if (f->f.nonexistent)
    {
      f->f.opened = 0;
      RETURN_STATUS (COB_FILE_SUCCEED);
    }

  ret = fileio_funcs[f->organization]->close (f, opt);
  if (ret == COB_FILE_SUCCEED)
    f->f.opened = 0;

  RETURN_STATUS (ret);
}

void
cob_start (struct cob_file *f, int cond, struct cob_field key)
{
  int ret;

  f->f.read_done = 0;
  f->f.first_read = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (COB_FILE_KEY_NOT_EXISTS);

  if (!f->f.opened
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND
      || f->access_mode == COB_ACCESS_RANDOM)
    RETURN_STATUS (COB_FILE_INPUT_DENIED);

  ret = fileio_funcs[f->organization]->start (f, cond, key);
  if (ret == COB_FILE_SUCCEED)
    {
      f->f.end_of_file = 0;
      f->f.first_read = 1;
    }

  RETURN_STATUS (ret);
}

static void
read_common (struct cob_file *f, struct cob_field key)
{
  int ret;

  f->f.read_done = 0;

  if (f->f.nonexistent)
    {
      if (!f->f.first_read)
	RETURN_STATUS (COB_FILE_KEY_NOT_EXISTS);
      f->f.first_read = 0;
      RETURN_STATUS (COB_FILE_END_OF_FILE);
    }

  if (f->f.end_of_file)
    RETURN_STATUS (COB_FILE_READ_ERROR);

  if (!f->f.opened
      || f->open_mode == COB_OPEN_OUTPUT
      || f->open_mode == COB_OPEN_EXTEND)
    RETURN_STATUS (COB_FILE_INPUT_DENIED);

  if (COB_FIELD_IS_VALID (key))
    ret = fileio_funcs[f->organization]->read (f, key);
  else
    ret = fileio_funcs[f->organization]->read_next (f);

  switch (ret)
    {
    case COB_FILE_SUCCEED:
      f->f.first_read = 0;
      f->f.read_done = 1;
      if (COB_FIELD_IS_VALID (f->record_depending))
	cob_set_int (f->record_depending, f->record_size);
      break;
    case COB_FILE_END_OF_FILE:
      f->f.end_of_file = 1;
      break;
    }

  RETURN_STATUS (ret);
}

void
cob_read (struct cob_file *f, struct cob_field key)
{
  read_common (f, key);
}

void
cob_read_next (struct cob_file *f)
{
  read_common (f, (struct cob_field) {0, 0});
}

void
cob_write (struct cob_file *f, struct cob_field rec)
{
  int ret;

  f->f.read_done = 0;

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    {
      if (!f->f.opened
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_I_O)
	RETURN_STATUS (COB_FILE_OUTPUT_DENIED);
    }
  else
    {
      if (!f->f.opened
	  || f->open_mode == COB_OPEN_INPUT
	  || f->open_mode == COB_OPEN_EXTEND)
	RETURN_STATUS (COB_FILE_OUTPUT_DENIED);
    }

  if (COB_FIELD_IS_VALID (f->record_depending))
    f->record_size = cob_to_int (f->record_depending);
  else
    f->record_size = rec.size;

  if (f->record_size < f->record_min || f->record_max < f->record_size)
    RETURN_STATUS (COB_FILE_RECORD_OVERFLOW);

  ret = fileio_funcs[f->organization]->write (f);

  RETURN_STATUS (ret);
}

void
cob_write_page (struct cob_file *f)
{
  struct sequential_file *p = f->file;

  if (!f->f.opened
      || f->open_mode == COB_OPEN_INPUT
      || f->open_mode == COB_OPEN_I_O)
    return;

  write (p->fd, "\f", 1);
}

void
cob_write_lines (struct cob_file *f, int lines)
{
  int i;
  struct sequential_file *p = f->file;

  if (!f->f.opened
      || f->open_mode == COB_OPEN_INPUT
      || f->open_mode == COB_OPEN_I_O)
    return;

  for (i = 0; i < lines; i++)
    write (p->fd, "\n", 1);
}

void
cob_rewrite (struct cob_file *f, struct cob_field rec)
{
  int ret;
  int read_done = f->f.read_done;

  f->f.read_done = 0;

  if (!f->f.opened || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_FILE_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (!read_done)
      RETURN_STATUS (COB_FILE_READ_NOT_DONE);

  ret = fileio_funcs[f->organization]->rewrite (f, rec);

  RETURN_STATUS (ret);
}

void
cob_delete (struct cob_file *f)
{
  int ret;
  int read_done = f->f.read_done;

  f->f.read_done = 0;

  if (!f->f.opened || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (COB_FILE_I_O_DENIED);

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (!read_done)
      RETURN_STATUS (COB_FILE_READ_NOT_DONE);

  ret = fileio_funcs[f->organization]->delete (f);

  RETURN_STATUS (ret);
}

void
cob_sort_init (struct cob_file *sort_file, ...)
{
  int tag;
  size_t size = 0;
  struct sort_file *p = malloc (sizeof (struct sort_file));
  va_list ap;

  sort_file->nkeys = 0;
  sort_file->file = p;
  p->sort_keys = NULL;
  va_start (ap, sort_file);
  while ((tag = va_arg (ap, int)) != 0)
    {
      if (size < sort_file->nkeys + 1)
	{
	  size = (size == 0) ? 4 : size * 2;
	  p->sort_keys = realloc (p->sort_keys,
				  size * sizeof (struct cob_sort_key));
	}
      p->sort_keys[sort_file->nkeys].dir = tag;
      p->sort_keys[sort_file->nkeys].field = va_arg (ap, struct cob_field);
      sort_file->nkeys++;
    }
  va_end (ap);
}

void
cob_sort_using (struct cob_file *sort_file, struct cob_file *data_file)
{
  cob_open (data_file, COB_OPEN_INPUT);
  while (1)
    {
      cob_read_next (data_file);
      if (data_file->file_status[0] != '0')
	break;
      memcpy (sort_file->record_data, data_file->record_data,
	      sort_file->record_size);
      cob_write (sort_file, (struct cob_field) {sort_file->record_size, sort_file->record_data, 0});
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_sort_giving (struct cob_file *sort_file, struct cob_file *data_file)
{
  cob_open (data_file, COB_OPEN_OUTPUT);
  while (1)
    {
      cob_read_next (sort_file);
      if (sort_file->file_status[0] != '0')
	break;
      memcpy (data_file->record_data,
	      sort_file->record_data,
	      data_file->record_size);
      cob_write (data_file, (struct cob_field) {data_file->record_size, data_file->record_data, 0});
    };
  cob_close (data_file, COB_CLOSE_NORMAL);
}

void
cob_default_error_handle (struct cob_file *f)
{
  const char *msg = NULL;
  int status = (f->file_status[0] - '0') * 10 + (f->file_status[1] - '0');
  switch (status)
    {
    case COB_FILE_END_OF_FILE:
      msg = N_("end of file");
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
