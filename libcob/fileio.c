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
      flags = O_RDWR;
      break;
    case COB_OPEN_EXTEND:
      flags = O_CREAT | O_RDWR | O_APPEND;
      break;
    }

  f->file.fd = open (filename, flags, FILE_PERMISSION);

  return (f->file.fd > 0) ? 0 : errno;
}

static int
sequential_close (struct cob_file_desc *f, int opt)
{
  switch (opt)
    {
    case COB_CLOSE_NORMAL:
      close (f->file.fd);
      f->file.fd = 0;
      return 00;
    default:
      return 07;
    }
}

static int
sequential_read (struct cob_file_desc *f)
{
  if (f->record_min != f->record_max)
    {
      if (read (f->file.fd, &f->record_size, sizeof (f->record_size)) <= 0)
	return 10;
      if (f->record_depending.desc)
	cob_set_int (f->record_depending, f->record_size);
    }

  if (read (f->file.fd, f->record_data, f->record_size) <= 0)
    return 10;

  return 00;
}

static int
sequential_write (struct cob_file_desc *f, struct cob_field rec)
{
  if (f->record_min != f->record_max)
    {
      if (f->record_depending.desc)
	f->record_size = cob_to_int (f->record_depending);
      else
	f->record_size = COB_FIELD_SIZE (rec);
      write (f->file.fd, &f->record_size, sizeof (f->record_size));
    }

  write (f->file.fd, f->record_data, f->record_size);
  return 00;
}

static int
sequential_rewrite (struct cob_file_desc *f, struct cob_field rec)
{
  if (COB_FIELD_SIZE (rec) != f->record_size)
    return 44;

  lseek (f->file.fd, - f->record_size, SEEK_CUR);
  write (f->file.fd, f->record_data, f->record_size);
  return 00;
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

  return (f->file.fp != NULL) ? 0 : errno;
}

static int
lineseq_close (struct cob_file_desc *f, int opt)
{
  fclose (f->file.fp);
  f->file.fp = NULL;
  return 00;
}

static int
lineseq_read (struct cob_file_desc *f)
{
  int i;
  char buff[f->record_size + 1];

  /* read the file */
  if (fgets (buff, f->record_size + 1, f->file.fp) == NULL)
    return 10;

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

  return 00;
}

static int
lineseq_write (struct cob_file_desc *f, struct cob_field rec)
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

  return 00;
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

#define RELATIVE_SEEK(f,i)						   \
  if (lseek (f->file.fd, RELATIVE_SIZE (f) * (i), SEEK_SET) == -1	   \
      || read (f->file.fd, &f->record_size, sizeof (f->record_size)) <= 0) \
    return 23;								   \
  lseek (f->file.fd, - sizeof (f->record_size), SEEK_CUR);

static int
relative_open (struct cob_file_desc *f, char *filename, int mode)
{
  return sequential_open (f, filename, mode);
}

static int
relative_close (struct cob_file_desc *f, int opt)
{
  return sequential_close (f, opt);
}

static int
relative_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  /* get the index */
  int index = cob_to_int (k) - 1;
  if (cond == COB_LT)
    index--;
  else if (cond == COB_GT)
    index++;

  /* seek the index */
  while (1)
    {
      RELATIVE_SEEK (f, index);

      /* check if a valid record */
      if (f->record_size > 0)
	{
	  cob_set_int (k, index + 1);
	  return 00;
	}

      /* continue */
      switch (cond)
	{
	case COB_EQ:
	  return 23;
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
relative_read (struct cob_file_desc *f, struct cob_field k)
{
  RELATIVE_SEEK (f, cob_to_int (k) - 1);

  if (f->record_size == 0)
    return 23;

  if (f->record_depending.desc)
    cob_set_int (f->record_depending, f->record_size);

  lseek (f->file.fd, sizeof (f->record_size), SEEK_CUR);
  read (f->file.fd, f->record_data, f->record_max);
  return 00;
}

static int
relative_read_next (struct cob_file_desc *f)
{
  while (1)
    {
      if (read (f->file.fd, &f->record_size, sizeof (f->record_size)) <= 0)
	return 10;

      if (f->relative_key.desc)
	{
	  if (f->f.first_read)
	    {
	      cob_set_int (f->relative_key, 1);
	      f->f.first_read = 0;
	    }
	  else
	    cob_add_int (f->relative_key, 1, 0, 0);
	  if (cob_status != 0)
	    {
	      lseek (f->file.fd, - sizeof (f->record_size), SEEK_CUR);
	      return 14;
	    }
	}

      if (f->record_size > 0)
	{
	  if (f->record_depending.desc)
	    cob_set_int (f->record_depending, f->record_size);

	  read (f->file.fd, f->record_data, f->record_max);
	  return 00;
	}

      lseek (f->file.fd, f->record_max, SEEK_CUR);
    }
}

static int
relative_write (struct cob_file_desc *f, struct cob_field rec)
{
  if (f->access_mode != COB_ACCESS_SEQUENTIAL)
    {
      int index = cob_to_int (f->relative_key) - 1;
      lseek (f->file.fd, RELATIVE_SIZE (f) * index, SEEK_SET);
    }

  if (read (f->file.fd, &f->record_size, sizeof (f->record_size)) > 0)
    {
      lseek (f->file.fd, - sizeof (f->record_size), SEEK_CUR);
      if (f->record_size > 0)
	return 22;
    }

  if (f->record_depending.desc)
    f->record_size = cob_to_int (f->record_depending);
  else
    f->record_size = COB_FIELD_SIZE (rec);
  write (f->file.fd, &f->record_size, sizeof (f->record_size));
  write (f->file.fd, f->record_data, f->record_max);

  /* update RELATIVE KEY */
  if (f->access_mode == COB_ACCESS_SEQUENTIAL)
    if (f->relative_key.desc)
      cob_set_int (f->relative_key,
		   lseek (f->file.fd, 0, SEEK_CUR) / RELATIVE_SIZE (f));

  return 00;
}

static int
relative_rewrite (struct cob_file_desc *f, struct cob_field rec)
{
  lseek (f->file.fd, - f->record_max, SEEK_CUR);
  write (f->file.fd, f->record_data, f->record_max);
  return 00;
}

static int
relative_delete (struct cob_file_desc *f)
{
  RELATIVE_SEEK (f, cob_to_int (f->relative_key) - 1);

  f->record_size = 0;
  write (f->file.fd, &f->record_size, sizeof (f->record_size));
  lseek (f->file.fd, f->record_max, SEEK_CUR);
  return 00;
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
  f->last_key = NULL;
  return 0;
}

static int
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

  if (f->last_key)
    free (f->last_key);

  return 00;
}

static int
indexed_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  int i, ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* look up for the key */
  for (i = 0; i < f->nkeys; i++)
    if (f->keys[i].field.data == k.data)
      break;
#if COB_DEBUG
  if (i == f->nkeys)
    {
      cob_runtime_error ("cob_start_indexed: key not found "
			 "(should have been detected by cobc)");
      return 24;
    }
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
      if (ret != 0)
	ret = f->cursor->c_get (f->cursor, &key, &data, DB_LAST);
      else if (cond == COB_LT || memcmp (k.data, key.data, key.size) != 0)
	ret = f->cursor->c_get (f->cursor, &key, &data, DB_PREV_NODUP);
      break;
    case COB_GT:
    case COB_GE:
      ret = f->cursor->c_get (f->cursor, &key, &data, DB_SET_RANGE);
      if (ret == 0)
	if (cond == COB_GT && memcmp (k.data, key.data, key.size) == 0)
	  ret = f->cursor->c_get (f->cursor, &key, &data, DB_NEXT_NODUP);
      break;
  }

  return (ret == 0) ? 00 : 23;
}

static int
indexed_read (struct cob_file_desc *f, struct cob_field k)
{
  int ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  if ((ret = indexed_start (f, COB_EQ, k)) != 00)
    return ret;

  if (DBC_GET (f->cursor, &key, &data, DB_CURRENT) != 0)
    return 23;

  memcpy (f->record_data, data.data, f->record_size);

  return 00;
}

static int
indexed_read_next (struct cob_file_desc *f)
{
  int ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  if (!f->f.first_read)
    {
      ret = DBC_GET (f->cursor, &key, &data, DB_NEXT);
    }
  else if (f->cursor)
    {
      ret = DBC_GET (f->cursor, &key, &data, DB_CURRENT);
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
      return 00;
    case DB_NOTFOUND:
      return 10;
      break;
    default:
      return 99;
    }
}

static int
indexed_write (struct cob_file_desc *f, struct cob_field rec)
{
  int i;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* check record key */
  DBT_SET (key, f->record_data, 0);
  if (!f->last_key)
    f->last_key = malloc (key.size);
  else if (memcmp (f->last_key, key.data, key.size) > 0)
    return 21;
  memcpy (f->last_key, key.data, key.size);

  /* write data */
  data.data = f->record_data;
  data.size = f->record_size;
  if (DB_PUT (f->keys[0].db, &key, &data, DB_NOOVERWRITE) != 0)
    return 22;

  /* write secondary keys */
  data = key;
  for (i = 1; i < f->nkeys; i++)
    {
      DBT_SET (key, f->record_data, i);
      if (DB_PUT (f->keys[i].db, &key, &data,
		  f->keys[i].duplicates ? 0 : DB_NOOVERWRITE) != 0)
	return 22;
    }

  return 00;
}

static int
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
	return 43;
      break;
    case COB_ACCESS_RANDOM:
    case COB_ACCESS_DYNAMIC:
      DBT_SET (key, f->record_data, 0);
      if (DB_GET (f->file.db, &key, &data, 0) != 0)
	return 23;
      break;
    }

  /* delete the secondary keys */
  for (i = 1; i < f->nkeys; i++)
    {
      DBT skey;
      memset (&skey, 0, sizeof (DBT));
      DBT_SET (skey, data.data, i);
      if (f->keys[i].duplicates)
	{
	  DBC *cursor;
	  DB_CURSOR (f->keys[i].db, &cursor);
	  if (cursor->c_get (cursor, &skey, &data, DB_SET) != 0)
	    return 99; /* database broken */
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

  return 00;
}

static int
indexed_rewrite (struct cob_file_desc *f, struct cob_field rec)
{
  int i, ret;
  DBT key, data;

  memset (&key, 0, sizeof (DBT));
  memset (&data, 0, sizeof (DBT));

  /* delete the current record */
  if ((ret = indexed_delete (f)) != 00)
    return ret;

  /* write data */
  DBT_SET (key, f->record_data, 0);
  data.data = f->record_data;
  data.size = f->record_size;
  if (DB_PUT (f->keys[0].db, &key, &data, DB_NOOVERWRITE) != 0)
    return 22;

  /* write secondary keys */
  data = key;
  for (i = 1; i < f->nkeys; i++)
    {
      DBT_SET (key, f->record_data, i);
      if (DB_PUT (f->keys[i].db, &key, &data,
		  f->keys[i].duplicates ? 0 : DB_NOOVERWRITE) != 0)
	return 22;
    }

  return 00;
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

#define RETURN_STATUS(x)			\
  do {						\
    f->file_status[0] = x / 10 + '0';		\
    f->file_status[1] = x % 10 + '0';		\
    return;					\
  } while (0)

static struct cob_fileio_funcs *fileio_funcs[4];

void
cob_init_fileio (void)
{
  fileio_funcs[COB_ORG_SEQUENTIAL] = &sequential_funcs;
  fileio_funcs[COB_ORG_LINE_SEQUENTIAL] = &lineseq_funcs;
  fileio_funcs[COB_ORG_RELATIVE] = &relative_funcs;
  fileio_funcs[COB_ORG_INDEXED] = &indexed_funcs;

#if 0
  db_env_create (&dbenv, 0);
  dbenv->set_errpfx (dbenv, "DB");
  dbenv->set_errfile (dbenv, stderr);
  dbenv->open (dbenv, NULL, DB_CREATE | DB_INIT_MPOOL, 0);
#endif
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
  f->f.first_read = 1;

  switch (fileio_funcs[f->organization]->open (f, filename, mode))
    {
    case 0:
      RETURN_STATUS (00);
    case ENOENT:
      if (f->f.optional)
	{
	  f->f.nonexistent = 1;
	  f->f.end_of_file = 1;
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
  int ret;

  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (00);

  if (!FILE_OPENED (f))
    RETURN_STATUS (42);

  ret = fileio_funcs[f->organization]->close (f, opt);

  f->f.nonexistent = 0;
  f->f.end_of_file = 0;

  RETURN_STATUS (ret);
}

void
cob_start (struct cob_file_desc *f, int cond, struct cob_field key)
{
  int ret;

  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (23);

  if (f->access_mode == COB_ACCESS_RANDOM)
    RETURN_STATUS (47);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  ret = fileio_funcs[f->organization]->start (f, cond, key);
  if (ret == 00)
    {
      f->f.first_read = 1;
    }

  RETURN_STATUS (ret);
}

static void
read_common (struct cob_file_desc *f, struct cob_field key)
{
  int ret;

  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  if (key.desc)
    ret = fileio_funcs[f->organization]->read (f, key);
  else
    ret = fileio_funcs[f->organization]->read_next (f);

  switch (ret)
    {
    case 00:
      f->f.first_read = 0;
      f->f.read_done = 1;
      break;
    case 10:
      f->f.end_of_file = 1;
      break;
    }

  RETURN_STATUS (ret);
}

void
cob_read (struct cob_file_desc *f, struct cob_field key)
{
  read_common (f, key);
}

void
cob_read_next (struct cob_file_desc *f)
{
  read_common (f, (struct cob_field) {0, 0});
}

void
cob_write (struct cob_file_desc *f, struct cob_field rec)
{
  int ret;

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

  ret = fileio_funcs[f->organization]->write (f, rec);

  RETURN_STATUS (ret);
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
cob_rewrite (struct cob_file_desc *f, struct cob_field rec)
{
  int ret;
  int read_done = f->f.read_done;

  f->f.read_done = 0;

  if (!FILE_OPENED (f) || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (49);

  if (!read_done)
    RETURN_STATUS (43);

  ret = fileio_funcs[f->organization]->rewrite (f, rec);

  RETURN_STATUS (ret);
}

void
cob_delete (struct cob_file_desc *f)
{
  int ret;

  f->f.read_done = 0;

  if (!FILE_OPENED (f) || f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (49);

  ret = fileio_funcs[f->organization]->delete (f);

  RETURN_STATUS (ret);
}
