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
   || f->open_mode == COB_OPEN_EXTEND)

#define FILE_READWRITE(f)			\
  (f->open_mode == COB_OPEN_I_O)

#define FILE_STATUS(f)				\
  (f->file_status[0] - '0') * 10 + (f->file_status[1] - '0')

#define RETURN_STATUS(x)			\
  do {						\
    f->file_status[0] = x / 10 + '0';		\
    f->file_status[1] = x % 10 + '0';		\
    return;					\
  } while (0)

#define FILE_PERMISSION 0644

#define OPEN_HEADER				\
  char filename[FILENAME_MAX];			\
						\
  /* check if the file is opened */		\
  if (FILE_OPENED (f))				\
    RETURN_STATUS (41);				\
						\
  cob_field_to_string (name, filename);		\
						\
  f->open_mode = mode;				\
  f->f.nonexistent = 0;				\
  f->f.end_of_file = 0;				\
  f->f.read_done   = 0;

#define OPEN_FOOTER				\
  if (FILE_OPENED (f))				\
      RETURN_STATUS (00);			\
						\
  switch (errno)				\
    {						\
    case ENOENT:				\
      if (!f->f.optional)			\
	RETURN_STATUS (35);			\
      else					\
	{					\
	  f->f.nonexistent = 1;			\
	  RETURN_STATUS (05);			\
	}					\
    case EACCES:				\
    case EISDIR: 				\
    case EROFS:					\
      RETURN_STATUS (37);			\
    default:					\
      RETURN_STATUS (30);			\
    }

char cob_dummy_status[2];


/*
 * SEQUENTIAL
 */

static void
sequential_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  int flags;

  OPEN_HEADER;

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

  OPEN_FOOTER;
}

static void
sequential_close (struct cob_file_desc *f)
{
  if (f->f.nonexistent)
    RETURN_STATUS (00);

  if (!FILE_OPENED (f))
    RETURN_STATUS (42);

  close (f->file.fd);
  f->file.fd = 0;

  RETURN_STATUS (00);
}

static void
sequential_read (struct cob_file_desc *f)
{
  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  switch (read (f->file.fd, f->record_data, f->record_size))
    {
    case -1:
      RETURN_STATUS (46);
    case 0:
      f->f.end_of_file = 1;
      RETURN_STATUS (10);
    default:
      f->f.read_done = 1;
      RETURN_STATUS (00);
    }
}

static void
sequential_write (struct cob_file_desc *f)
{
  f->f.read_done = 0;

  if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
    RETURN_STATUS (48);

  if (write (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (30);

  RETURN_STATUS (00);
}

static void
sequential_rewrite (struct cob_file_desc *f)
{
  if (!FILE_OPENED (f) || !FILE_READWRITE (f))
    RETURN_STATUS (49);

  if (!f->f.read_done)
    RETURN_STATUS (43);

  f->f.read_done = 0;

  if (lseek (f->file.fd, - f->record_size, SEEK_CUR) == -1
      || write (f->file.fd, f->record_data, f->record_size) == -1)
    RETURN_STATUS (30);

  RETURN_STATUS (00);
}

static struct cob_fileio_funcs sequential_funcs = {
  sequential_open,
  sequential_close,
  sequential_read,
  sequential_read,
  sequential_write,
  sequential_rewrite,
  0,
  0
};

void
cob_write_lines (struct cob_file_desc *f, int lines)
{
  int i;

  if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
    return;

  for (i = 0; i < lines; i++)
    write (f->file.fd, "\n", 1);
}

void
cob_write_page (struct cob_file_desc *f)
{
  if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
    return;

  write (f->file.fd, "\f", 1);
}


/*
 * LINE SEQUENTIAL
 */

static void
lineseq_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  OPEN_HEADER;

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

  OPEN_FOOTER;
}

static void
lineseq_close (struct cob_file_desc *f)
{
  if (f->f.nonexistent)
    RETURN_STATUS (00);

  if (!FILE_OPENED (f))
    RETURN_STATUS (42);

  fclose (f->file.fp);
  f->file.fp = NULL;

  RETURN_STATUS (00);
}

static void
lineseq_read (struct cob_file_desc *f)
{
  int i;
  char buff[f->record_size + 1];

  f->f.read_done = 0;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

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

  f->f.read_done = 1;
  RETURN_STATUS (00);
}

static void
lineseq_write (struct cob_file_desc *f)
{
  int i, size;

  f->f.read_done = 0;

  if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
    RETURN_STATUS (48);

  /* determine the size to be written */
  for (i = f->record_size - 1; i >= 0; i--)
    if (f->record_data[i] != ' ')
      break;
  size = i + 1;

  /* write the the file */
  for (i = 0; i < size; i++)
    putc (f->record_data[i], f->file.fp);
  putc ('\n', f->file.fp);

  RETURN_STATUS (00);
}

static struct cob_fileio_funcs lineseq_funcs = {
  lineseq_open,
  lineseq_close,
  lineseq_read,
  lineseq_read,
  lineseq_write,
  0,
  0,
  0
};


/*
 * RELATIVE
 */

static void
relative_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  sequential_open (f, name, mode);
  cob_set_int (f->relative_key, 1);
}

static void
relative_close (struct cob_file_desc *f)
{
  sequential_close (f);
  cob_set_int (f->relative_key, 0);
}

static void
relative_read (struct cob_file_desc *f)
{
  int size, index;

  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  index = cob_to_int (f->relative_key);
  if (lseek (f->file.fd, f->record_size * index, SEEK_SET) == -1)
    RETURN_STATUS (99);

  size = read (f->file.fd, f->record_data, f->record_size);
  if (size == 0)
    RETURN_STATUS (10);
  else if (size == f->record_size)
    RETURN_STATUS (00);
  else if (f->record_data[0] == '\0')
    RETURN_STATUS (23);
  else
    RETURN_STATUS (30);
}

static void
relative_read_next (struct cob_file_desc *f)
{
  if (f->f.nonexistent)
    RETURN_STATUS (10);

  if (!FILE_OPENED (f) || !FILE_READABLE (f))
    RETURN_STATUS (47);

  if (f->f.end_of_file)
    RETURN_STATUS (46);

  do {
    int size;
    size = read (f->file.fd, f->record_data, f->record_size);
    if (size == 0)
      RETURN_STATUS (10);
    else if (size == -1)
      RETURN_STATUS (99);
  } while (f->record_data[0] == '\0');

  RETURN_STATUS (00);
}

static void
relative_write (struct cob_file_desc *f)
{
  int index;

  if (!FILE_OPENED (f) || !FILE_WRITABLE (f))
    RETURN_STATUS (48);

  index = cob_to_int (f->relative_key);
  lseek (f->file.fd, f->record_size * index, SEEK_SET);
  write (f->file.fd, f->record_data, f->record_size);

  RETURN_STATUS (00);
}

static void
relative_rewrite (struct cob_file_desc *f)
{
  int index;

  if (!FILE_OPENED (f) || !FILE_READWRITE (f))
    RETURN_STATUS (49);

  index = cob_to_int (f->relative_key);
  lseek (f->file.fd, f->record_size * index, SEEK_SET);
  write (f->file.fd, f->record_data, f->record_size);

  RETURN_STATUS (00);
}

static void
relative_delete (struct cob_file_desc *f)
{
  int index;
  char buff[f->record_size];

  if (!FILE_OPENED (f) || !FILE_READWRITE (f))
    RETURN_STATUS (49);

  index = cob_to_int (f->relative_key);
  lseek (f->file.fd, f->record_size * index, SEEK_SET);

  memset (buff, 0, f->record_size);
  write (f->file.fd, buff, f->record_size);

  RETURN_STATUS (00);
}

static void
relative_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  int index;

  index = cob_to_int (k);
  lseek (f->file.fd, f->record_size * index, SEEK_SET);
}

static struct cob_fileio_funcs relative_funcs = {
  relative_open,
  relative_close,
  relative_read,
  relative_read_next,
  relative_write,
  relative_rewrite,
  relative_delete,
  relative_start
};


/*
 * INDEXED
 */

#define DB_OPEN(db,name,dbname,flags)					\
  ({									\
    db_create (&db, NULL, 0);						\
    db->open (db, name, dbname, DB_BTREE, flags, FILE_PERMISSION);	\
  })
#define DB_PUT(db,key,data,flags)					\
  db->put (db, NULL, key, data, flags)
#define DB_GET(db,key,data,flags)					\
  db->get (db, NULL, key, data, flags)
#define DB_DEL(db,key,flags)						\
  db->del (db, NULL, key, flags)
#define DB_CURSOR(db,key)						\
  db->cursor (db, NULL, key, 0)
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

#define DBT_SET(key,base,i)						    \
  (key).data = COB_FIELD_DATA (f->keys[i].field) - f->record_data + (base); \
  (key).size = COB_FIELD_SIZE (f->keys[i].field);

static void
indexed_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  int i;
  int flags;

  OPEN_HEADER;

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

  DB_OPEN (f->file.db, filename, "primary", flags);
  f->keys[0].db = f->file.db;

  for (i = 1; i < f->nkeys; i++)
    {
      char buff[BUFSIZ];
      sprintf (buff, "secondary%d", i);
      DB_OPEN (f->keys[i].db, filename, buff, flags);
    }

  OPEN_FOOTER;
}

static void
indexed_close (struct cob_file_desc *f)
{
  int i;

  if (!FILE_OPENED (f))
    RETURN_STATUS (42);

  /* close the cursor */
  if (f->cursor)
    f->cursor->c_close (f->cursor);

  /* close DB's */
  for (i = 0; i < f->nkeys; i++)
    f->keys[i].db->close (f->keys[i].db, 0);
  f->file.db = NULL;

  RETURN_STATUS (00);
}

static void
indexed_read (struct cob_file_desc *f)
{
  DBT key, data;

  if (DBC_GET (f->cursor, &key, &data, DB_SET))
    RETURN_STATUS (23);
  memcpy (f->record_data, data.data, f->record_size);

  RETURN_STATUS (00);
}

static void
indexed_read_next (struct cob_file_desc *f)
{
  DBT key, data;

  if (DBC_GET (f->cursor, &key, &data, DB_NEXT))
    RETURN_STATUS (23);
  memcpy (f->record_data, data.data, f->record_size);

  RETURN_STATUS (00);
}

static void
indexed_write_data (struct cob_file_desc *f, int i, DBT *datap)
{
  DBT key;
  DBT_SET (key, f->record_data, i);
  if (f->keys[i].duplicates)
    DB_PUT (f->keys[i].db, &key, datap, 0);
  else
    DB_PUT (f->keys[i].db, &key, datap, DB_NOOVERWRITE);
}

static void
indexed_write (struct cob_file_desc *f)
{
  int i;
  DBT data;

  /* write data */
  data.data = f->record_data;
  data.size = f->record_size;
  indexed_write_data (f, 0, &data);

  /* write secondary keys */
  DBT_SET (data, f->record_data, 0);
  for (i = 1; i < f->nkeys; i++)
    indexed_write_data (f, i, &data);

  RETURN_STATUS (00);
}

static void
indexed_delete (struct cob_file_desc *f)
{
  int i;
  DBT key, data;

  if (!FILE_OPENED (f) || !FILE_READWRITE (f))
    RETURN_STATUS (49);

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
  if (!FILE_OPENED (f) || !FILE_READWRITE (f))
    RETURN_STATUS (49);

  indexed_delete (f);
  if (FILE_STATUS (f) == 00)
    indexed_write (f);
}

static void
indexed_start (struct cob_file_desc *f, int cond, struct cob_field k)
{
  int i, ret;
  DBT key, data;

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
}

static struct cob_fileio_funcs indexed_funcs = {
  indexed_open,
  indexed_close,
  indexed_read,
  indexed_read_next,
  indexed_write,
  indexed_rewrite,
  indexed_delete,
  indexed_start
};


/*
 * Public interface
 */

static struct cob_fileio_funcs *fileio_funcs[] = {
  &sequential_funcs,		/* COB_ORG_SEQUENTIAL */
  &lineseq_funcs,		/* COB_ORG_LINE_SEQUENTIAL */
  &relative_funcs,		/* COB_ORG_RELATIVE */
  &indexed_funcs		/* COB_ORG_INDEXED */
};

void
cob_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  fileio_funcs[f->organization]->open (f, name, mode);
}

void
cob_close (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->close (f);
}

void
cob_read (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->read (f);
}

void
cob_read_next (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->read_next (f);
}

void
cob_write (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->write (f);
}

void
cob_rewrite (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->rewrite (f);
}

void
cob_delete (struct cob_file_desc *f)
{
  fileio_funcs[f->organization]->delete (f);
}

void
cob_start (struct cob_file_desc *f, int cond, struct cob_field key)
{
  fileio_funcs[f->organization]->start (f, cond, key);
}
