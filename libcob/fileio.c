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

/*
 * ACCESS MODE |         | INPUT  OUTPUT  I-O  EXTEND
 * ------------+---------+---------------------------
 *             | READ    |   o      x      o     x
 *             | WRITE   |   x      o      x     o
 * SEQUENTIAL  | REWRITE |   x      x      o     x
 *             | START   |   o      x      o     x
 *             | DELETE  |   x      x      o     x
 * ------------+---------+---------------------------
 *             | READ    |   o      x      o     x
 *             | WRITE   |   x      o      o     x
 * RANDOM      | REWRITE |   x      x      o     x
 *             | START   |   x      x      x     x
 *             | DELETE  |   x      x      o     x
 * ------------+---------+---------------------------
 *             | READ    |   o      x      o     x
 *             | WRITE   |   x      o      o     x
 * DYNAMIC     | REWRITE |   x      x      o     x
 *             | START   |   o      x      o     x
 *             | DELETE  |   x      x      o     x
 */

/* FILE STATUS
 *
 * 00 - succeed
 * 02 - succeed, but duplicate keys
 * 05 - succeed, but optional file
 * 07 - succeed, but no reel
 * 10 - end of file
 * 21 - invalid key order
 * 22 - key already exists
 * 23 - key not exists
 * 24 - key out of range
 * 30 - unknown permanent error
 * 34 - file overflow
 * 35 - file not exists
 * 37 - permission defied
 * 38 - file locked
 * 39 - file property mismatch
 * 41 - file already opened
 * 42 - file not opened
 * 43 - READ must be executed
 * 44 - record overflow
 * 46 - READ error
 * 47 - READ, START not permitted
 * 48 - WRITE not permitted
 * 49 - DELETE, REWRITE not permitted
 * 9x - unknown error
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

#include "libcob.h"

#define FILE_OPENED(f)		(f->file.fd != 0)

#define FILE_READABLE(f)			\
  (f->open_mode == COB_OPEN_INPUT		\
   || f->open_mode == COB_OPEN_I_O)

#define FILE_WRITABLE(f)			\
  (f->open_mode == COB_OPEN_OUTPUT		\
   || f->open_mode == COB_OPEN_I_O)

#define FILE_EXTENSIBLE(f)			\
  (f->open_mode == COB_OPEN_OUTPUT		\
   || f->open_mode == COB_OPEN_EXTEND)

#define FILE_STATUS(f)				\
  (f->file_status[0] - '0') * 10 + (f->file_status[1] - '0')

#define RETURN_STATUS(x)			\
  do {						\
    f->file_status[0] = x / 10 + '0';		\
    f->file_status[1] = x % 10 + '0';		\
    return;					\
  } while (0)

#define FILE_PERMISSION 0644

char cob_dummy_status[2];


/*
 * SEQUENTIAL
 */

static int
sequential_open (struct cob_file_desc *f, char *filename, int mode)
{
  int flags;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags = O_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags = O_CREAT | O_RDWR | O_TRUNC;
      break;
    case COB_OPEN_I_O:
      flags = O_CREAT | O_RDWR;
      break;
    case COB_OPEN_EXTEND:
      flags = O_CREAT | O_RDWR | O_APPEND;
      break;
    }

  f->file.fd = open (filename, flags, FILE_PERMISSION);

  return f->file.fd ? 0 : errno;
}

static void
sequential_close (struct cob_file_desc *f, int opt)
{
  switch (opt)
    {
    case COB_CLOSE_NORMAL:
      close (f->file.fd);
      f->file.fd = 0;
      RETURN_STATUS (00);
    case COB_CLOSE_REEL:
    case COB_CLOSE_REEL_REMOVAL:
    case COB_CLOSE_UNIT:
    case COB_CLOSE_UNIT_REMOVAL:
    case COB_CLOSE_NO_REWIND:
    case COB_CLOSE_LOCK:
      RETURN_STATUS (07);
    }
}

static void
sequential_read (struct cob_file_desc *f)
{
  switch (read (f->file.fd, f->record_data, f->record_size))
    {
    case -1:
      RETURN_STATUS (46);
    case 0:
      f->f.end_of_file = 1;
      RETURN_STATUS (10);
    default:
      RETURN_STATUS (00);
    }
}

static void
sequential_write (struct cob_file_desc *f)
{
  if (write (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (30);

  RETURN_STATUS (00);
}

static void
sequential_rewrite (struct cob_file_desc *f)
{
  if (lseek (f->file.fd, - f->record_size, SEEK_CUR) == -1
      || write (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (30);

  RETURN_STATUS (00);
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

static int
lineseq_open (struct cob_file_desc *f, char *filename, int mode)
{
  switch (mode)
    {
    case COB_OPEN_INPUT:
      f->file.fp = fopen (filename, "r");
      break;
    case COB_OPEN_OUTPUT:
      f->file.fp = fopen (filename, "w");
      break;
    case COB_OPEN_I_O:
      f->file.fp = fopen (filename, "rw");
      break;
    case COB_OPEN_EXTEND:
      f->file.fp = fopen (filename, "w+");
      break;
    }

  return f->file.fp ? 0 : errno;
}

static void
lineseq_close (struct cob_file_desc *f, int opt)
{
  fclose (f->file.fp);
  f->file.fp = NULL;
  RETURN_STATUS (00);
}

static void
lineseq_read (struct cob_file_desc *f)
{
  int i;
  char buff[f->record_size + 1];

  /* read the file */
  if (fgets (buff, f->record_size + 1, f->file.fp) == NULL)
    {
      f->f.end_of_file = 1;
      RETURN_STATUS (10);
    }

  /* remove the newline */
  for (i = 0; i < f->record_size; i++)
    if (buff[i] == '\r' || buff[i] == '\n' || buff[i] == '\0')
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
      int c = getc (f->file.fp);
      while (c != '\r' && c != '\n' && c != '\0')
	c = getc (f->file.fp);
      if (c == '\r')
	c = getc (f->file.fp);
      if (c != '\n' && c != '\0')
	ungetc (c, f->file.fp);
    }

  memcpy (f->record_data, buff, f->record_size);

  RETURN_STATUS (00);
}

static void
lineseq_write (struct cob_file_desc *f)
{
  int i, size;

  /* determine the size to be written */
  for (i = f->record_size - 1; i >= 0; i--)
    if (f->record_data[i] != ' ')
      break;
  size = i + 1;

  /* write to the file */
  for (i = 0; i < size; i++)
    putc (f->record_data[i], f->file.fp);
  putc ('\n', f->file.fp);

  RETURN_STATUS (00);
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

static int
relative_open (struct cob_file_desc *f, char *filename, int mode)
{
  f->relative_index = 0;
  return sequential_open (f, filename, mode);
}

static void
relative_close (struct cob_file_desc *f, int opt)
{
  f->relative_index = -1;
  sequential_close (f, opt);
}

static void
relative_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  char c;
  int index;

  /* get the index */
  index = cob_to_int (k) - 1;
  if (cond == COB_LT)
    index--;
  else if (cond == COB_GT)
    index++;

  /* seek the index */
 again:
  if (lseek (f->file.fd, f->record_size * index, SEEK_SET) == -1
      || read (f->file.fd, &c, 1) == -1)
    {
    not_found:
      RETURN_STATUS (23);
    }

  /* check if a valid record */
  if (c != '\0')
    {
      cob_set_int (k, index + 1);
      lseek (f->file.fd, -1, SEEK_CUR);
      RETURN_STATUS (00);
    }

  /* continue */
  switch (cond)
    {
    case COB_EQ:
      goto not_found;
    case COB_LT:
    case COB_LE:
      index--;
      goto again;
    case COB_GT:
    case COB_GE:
      index++;
      goto again;
    }
}

static void
relative_read (struct cob_file_desc *f, struct cob_field k)
{
  int index = cob_to_int (k) - 1;

  if (lseek (f->file.fd, f->record_size * index, SEEK_SET) == -1
      || read (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (23);

  RETURN_STATUS (00);
}

static void
relative_read_next (struct cob_file_desc *f)
{
  do {
      if (lseek (f->file.fd, 0, SEEK_CUR) > 0)
	f->relative_index++;

      switch (read (f->file.fd, f->record_data, f->record_size))
	{
	case 0:
	  RETURN_STATUS (10);
	case -1:
	  RETURN_STATUS (99);
	}

      if (f->relative_key.desc)
	cob_set_int (f->relative_key, f->relative_index + 1);
  } while (f->record_data[0] == '\0');

  RETURN_STATUS (00);
}

static void
relative_write (struct cob_file_desc *f)
{
  if (f->access_mode != COB_ACCESS_SEQUENTIAL)
    {
      int index = cob_to_int (f->relative_key) - 1;
      if (lseek (f->file.fd, f->record_size * index, SEEK_SET) < 0)
	RETURN_STATUS (23);
    }

  write (f->file.fd, f->record_data, f->record_size);

  /* update RELATIVE KEY */
  if (f->relative_key.desc)
    cob_set_int (f->relative_key,
		 lseek (f->file.fd, 0, SEEK_CUR) / f->record_size);

  RETURN_STATUS (00);
}

static void
relative_rewrite (struct cob_file_desc *f)
{
  int index = cob_to_int (f->relative_key) - 1;
  if (lseek (f->file.fd, f->record_size * index, SEEK_SET) == -1
      || write (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (99);

  RETURN_STATUS (00);
}

static void
relative_delete (struct cob_file_desc *f)
{
  int index = cob_to_int (f->relative_key) - 1;
  char buff[f->record_size];
  memset (buff, 0, f->record_size);

  if (lseek (f->file.fd, f->record_size * index, SEEK_SET) == -1
      || write (f->file.fd, buff, f->record_size) == -1)
    RETURN_STATUS (99);

  RETURN_STATUS (00);
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

#define DB_OPEN(db,name,dbname,flags)				\
  db->open (db, name, dbname, DB_BTREE, flags, FILE_PERMISSION)
#define DB_CLOSE(db)						\
  db->close (db, 0)
#define DB_PUT(db,key,data,flags)				\
  db->put (db, NULL, key, data, flags)
#define DB_GET(db,key,data,flags)				\
  db->get (db, NULL, key, data, flags)
#define DB_DEL(db,key,flags)					\
  db->del (db, NULL, key, flags)
#define DB_CURSOR(db,cur)					\
  db->cursor (db, NULL, cur, 0)
#define DBC_GET(cur,key,data,flags)			\
  ({							\
    int ret = cur->c_get (cur, key, data, flags);	\
    if (ret == 0 && f->f.secondary)			\
      {							\
	*key = *data;					\
	ret = DB_GET (f->file.db, key, data, 0);	\
      }							\
    ret;						\
  })

#define DBT_SET(k,base,i)						\
  k.data = COB_FIELD_DATA (f->keys[i].field) - f->record_data + (base);	\
  k.size = COB_FIELD_SIZE (f->keys[i].field);

static DB_ENV *dbenv = NULL;

static int
indexed_open (struct cob_file_desc *f, char *filename, int mode)
{
  int i, j, ret;
  int flags = 0;

  switch (mode)
    {
    case COB_OPEN_INPUT:
      flags = DB_RDONLY;
      break;
    case COB_OPEN_OUTPUT:
      flags = DB_CREATE | DB_TRUNCATE;
      break;
    case COB_OPEN_I_O:
      flags = DB_CREATE;
      break;
    case COB_OPEN_EXTEND:
      flags = 0;
      break;
    }

  for (i = 0; i < f->nkeys; i++)
    {
      char dbname[BUFSIZ];
      if (i == 0)
	strcpy (dbname, filename);
      else
	sprintf (dbname, "%s.%d", filename, i);
      db_create (&f->keys[i].db, dbenv, 0);
      if (f->keys[i].duplicates)
	f->keys[i].db->set_flags (f->keys[i].db, DB_DUP);
      if ((ret = DB_OPEN (f->keys[i].db, dbname, NULL, flags)) != 0)
	{
	  for (j = 0; j <= i; j++)
	    DB_CLOSE (f->keys[j].db);
	  return ret;
	}
    }

  f->file.db = f->keys[0].db;
  return 0;
}

static void
indexed_close (struct cob_file_desc *f, int opt)
{
  int i;

  /* close the cursor */
  if (f->cursor)
    f->cursor->c_close (f->cursor);
  f->cursor = NULL;

  /* close DB's */
  for (i = 0; i < f->nkeys; i++)
    DB_CLOSE (f->keys[i].db);
  f->file.db = NULL;

  RETURN_STATUS (00);
}

static void
indexed_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  int i, ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* look up for the key */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].field.desc == k.desc)
      break;
#if COB_DEBUG
  if (i == f->nkeys)
    cob_runtime_error ("cob_start_indexed: key not found "
		       "(should have been detected by cobc)");
#endif

  /* close the current cursor */
  if (f->cursor)
    f->cursor->c_close (f->cursor);

  /* create a new cursor */
  DB_CURSOR (f->keys[i].db, &f->cursor);
  f->f.secondary = (i > 0);

  /* search */
  DBT_SET (key, f->record_data, i);
  switch (cond)
    {
    case COB_EQ:
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_SET);
      break;
    case COB_LT:
    case COB_LE:
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_SET_RANGE);
      if (ret == 0 && cond == COB_LE)
	if (memcpy (k.data, key.data, key.size) == 0)
	  break;
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_PREV);
      break;
    case COB_GT:
    case COB_GE:
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_SET_RANGE);
      if (ret == 0 && cond == COB_GE)
	if (memcpy (k.data, key.data, key.size) == 0)
	  break;
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_NEXT);
      break;
  }
  
  if (ret == 0)
    RETURN_STATUS (00);
  else
    RETURN_STATUS (23);
}

static void
indexed_read (struct cob_file_desc *f, struct cob_field k)
{
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  indexed_start (f, COB_EQ, k);
  if (FILE_STATUS (f) != 00)
    return;

  if (DBC_GET (f->cursor, &key, &data, DB_CURRENT) != 0)
    RETURN_STATUS (23);

  memcpy (f->record_data, data.data, f->record_size);

  RETURN_STATUS (00);
}

static void
indexed_read_next (struct cob_file_desc *f)
{
  int ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  if (f->cursor)
    {
      ret = DBC_GET (f->cursor, &key, &data, DB_NEXT);
    }
  else
    {
      DB_CURSOR (f->keys[0].db, &f->cursor);
      f->f.secondary = 0;
      ret = DBC_GET (f->cursor, &key, &data, DB_FIRST);
    }

  switch (ret)
    {
    case 0:
      memcpy (f->record_data, data.data, f->record_size);
      RETURN_STATUS (00);
    case DB_NOTFOUND:
      RETURN_STATUS (10);
      break;
    default:
      RETURN_STATUS (99);
    }
}

static void
indexed_write (struct cob_file_desc *f)
{
  int i;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* write data */
  data.data = f->record_data;
  data.size = f->record_size;
  DBT_SET (key, f->record_data, 0);
  switch (DB_PUT (f->keys[0].db, &key, &data, DB_NOOVERWRITE))
    {
    case 0:
      break;
    case DB_KEYEXIST: 
      RETURN_STATUS (22);
    default:
      RETURN_STATUS (99);
    }

  /* write secondary keys */
  data = key;
  for (i = 1; i < f->nkeys; i++)
    {
      DBT_SET (key, f->record_data, i);
      switch (DB_PUT (f->keys[i].db, &key, &data,
		      f->keys[i].duplicates ? 0 : DB_NOOVERWRITE))
	{
	case 0:
	  break;
	case DB_KEYEXIST: 
	  RETURN_STATUS (22);
	default:
	  RETURN_STATUS (99);
	}
    }

  RETURN_STATUS (00);
}

static void
indexed_delete (struct cob_file_desc *f)
{
  int i;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* find the primary key */
  switch (f->access_mode)
    {
    case COB_ACCESS_SEQUENTIAL:
      if (DBC_GET (f->cursor, &key, &data, DB_CURRENT) != 0)
	RETURN_STATUS (43);
      break;
    case COB_ACCESS_RANDOM:
    case COB_ACCESS_DYNAMIC:
      DBT_SET (key, f->record_data, 0);
      if (DB_GET (f->file.db, &key, &data, 0) != 0)
	RETURN_STATUS (23);
      break;
    }

  /* delete the secondary keys */
  for (i = 1; i < f->nkeys; i++)
    {
      DBT skey;
      DBT_SET (skey, data.data, i);
      if (f->keys[i].duplicates)
	{
	  DBC *cursor;
	  DB_CURSOR (f->keys[i].db, &cursor);
	  if (cursor->c_get (cursor, &skey, &data, DB_SET) != 0)
	    RETURN_STATUS (99); /* database broken */
	  do {
	    if (memcmp (data.data, key.data, key.size) == 0)
	      cursor->c_del (cursor, 0);
	  } while (cursor->c_get (cursor, &skey, &data, DB_NEXT_DUP) == 0);
	  cursor->c_close (cursor);
	}
      else
	{
	  DB_DEL (f->keys[i].db, &skey, 0);
	}
    }

  /* delete the record */
  DB_DEL (f->file.db, &key, 0);

  RETURN_STATUS (00);
}

static void
indexed_rewrite (struct cob_file_desc *f)
{
  indexed_delete (f);
  if (FILE_STATUS (f) == 00)
    indexed_write (f);
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
 * Public interface
 */

static struct cob_fileio_funcs *fileio_funcs[4];

void
cob_init_fileio (void)
{
  fileio_funcs[COB_ORG_SEQUENTIAL] = &sequential_funcs;
  fileio_funcs[COB_ORG_LINE_SEQUENTIAL] = &lineseq_funcs;
  fileio_funcs[COB_ORG_RELATIVE] = &relative_funcs;
  fileio_funcs[COB_ORG_INDEXED] = &indexed_funcs;

  db_env_create (&dbenv, 0);
#ifdef COB_DEBUG
  dbenv->set_errpfx (dbenv, "DB");
  dbenv->set_errfile (dbenv, stderr);
#endif
  dbenv->open (dbenv, NULL, DB_CREATE | DB_INIT_MPOOL, 0);
}

void
cob_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  char filename[FILENAME_MAX];

  f->f.read_done = 0;

  /* check if the file is already open */
  if (FILE_OPENED (f))
    RETURN_STATUS (41);

  cob_field_to_string (name, filename);

  f->open_mode = mode;
  f->f.nonexistent = 0;
  f->f.end_of_file = 0;

  switch (fileio_funcs[f->organization]->open (f, filename, mode))
    {
    case 0:
      RETURN_STATUS (00);
    case ENOENT:
      if (f->f.optional)
	{
	  f->f.nonexistent = 1;
	  RETURN_STATUS (05);
	}
      else
	{
	  RETURN_STATUS (35);
	}
    case EACCES:
    case EISDIR:
    case EROFS:
      RETURN_STATUS (37);
    default:
      RETURN_STATUS (30);
    }
}

void
cob_close (struct cob_file_desc *f, int opt)
{
  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (00);

  if (!FILE_OPENED (f))
    RETURN_STATUS (42);

  fileio_funcs[f->organization]->close (f, opt);

  f->f.nonexistent = 0;
  f->f.end_of_file = 0;
}

void
cob_start (struct cob_file_desc *f, int cond, struct cob_field key)
{
  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (23);

  if (f->access_mode == COB_ACCESS_RANDOM)
    RETURN_STATUS (47);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  fileio_funcs[f->organization]->start (f, cond, key);
}

void
cob_read (struct cob_file_desc *f, struct cob_field key)
{
  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  fileio_funcs[f->organization]->read (f, key);

  if (FILE_STATUS (f) == 00)
    f->f.read_done = 1;
}

void
cob_read_next (struct cob_file_desc *f)
{
  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  fileio_funcs[f->organization]->read_next (f);

  if (FILE_STATUS (f) == 00)
    f->f.read_done = 1;
}

void
cob_write (struct cob_file_desc *f)
{
  f->f.read_done = 0;

  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    {
      if (!FILE_OPENED (f) || !FILE_EXTENSIBLE (f))
	RETURN_STATUS (48);
    }
  else
    {
      if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
	RETURN_STATUS (48);
    }

  fileio_funcs[f->organization]->write (f);
}

void
cob_write_page (struct cob_file_desc *f)
{
  if (!FILE_OPENED (f) || !FILE_EXTENSIBLE (f))
    return;

  write (f->file.fd, "\f", 1);
}

void
cob_write_lines (struct cob_file_desc *f, int lines)
{
  int i;

  if (!FILE_OPENED (f) || !FILE_EXTENSIBLE (f))
    return;

  for (i = 0; i < lines; i++)
    write (f->file.fd, "\n", 1);
}

void
cob_rewrite (struct cob_file_desc *f)
{
  int read_done = f->f.read_done;

  f->f.read_done = 0;

  if (!read_done)
    RETURN_STATUS (43);

  if (!FILE_OPENED (f) || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (49);

  fileio_funcs[f->organization]->rewrite (f);
}

void
cob_delete (struct cob_file_desc *f)
{
  int read_done = f->f.read_done;

  f->f.read_done = 0;

  if (!read_done)
    RETURN_STATUS (43);

  if (!FILE_OPENED (f) || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (49);

  fileio_funcs[f->organization]->delete (f);
}
