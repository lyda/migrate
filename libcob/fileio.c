// 
// Copyright (C) 2001, 2000, 1999,  Rildo Pragana, Jim Noeth, 
//               Andrew Cameron, David Essex.
// Copyright (C) 1993, 1991  Rildo Pragana.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// as published by the Free Software Foundation; either version 2.1,
// or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; see the file COPYING.LIB.  If
// not, write to the Free Software Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA

// 
// 
//   Cobol Compiler Library -- File Handling Module
// 
// 

// Permissible statements -- table from CD-1.2 (cobol draft) pp. 450
//   ----------------------------------------------------------------- 
//   acc. mode | statement  | -------- open mode ------
//             |            | input output  i-o  extend
//   ----------+------------+--------------------------
//   line      |  read      |   X            
//   sequential|  write     |          X           X
//   ----------+------------+--------------------------
//   sequential|  read      |   X            X
//             |  write     |          X           X
//             |  rewrite   |                X
//   ----------+------------+--------------------------
//   sequential|  delete    |                X
//   (relative |            |
//    & indexed|  start     |   X            X
//    files)   |            |
//   ----------+------------+--------------------------
//   random    |  read      |   X            X
//             |  write     |          X     X
//             |  rewrite   |                X
//             |  start     |
//             |  delete    |                X
//   ----------+------------+--------------------------
//   dynamic   |  read      |   X            X
//             |  write     |          X     X
//             |  rewrite   |                X
//             |  start     |   X            X
//             |  delete    |                X
//   ----------+------------+--------------------------

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

#define RETURN_STATUS(x)			\
  do {						\
    f->status[0] = x / 10 + '0';		\
    f->status[1] = x % 10 + '0';		\
    return;					\
  } while (0)

char cob_dummy_status[2];

void
cob_open (struct cob_file_desc *f, struct cob_field name, int mode)
{
  DBTYPE type = DB_BTREE;
  void *infop = NULL;
  int sflags = S_IRUSR | S_IWUSR;
  int oflags = 0;
  char alt_filename[128];
  char filename[128];
  int alt_key_no;

  BTREEINFO alt_key;
  alt_key.flags = 0;
  alt_key.cachesize = 0;
  alt_key.maxkeypage = 0;
  alt_key.minkeypage = 0;
  alt_key.psize = 0;
  alt_key.compare = NULL;
  alt_key.prefix = NULL;
  alt_key.lorder = 0;

  cob_field_to_string (name, filename);

  /* Check to see if the file is already open. If so return
     File Status 91 in according to the Ansi 74 Standard. */
  if (f->dbp != NULL)
    RETURN_STATUS (91);

  switch (mode)
    {
    case COB_OPEN_INPUT:
      oflags = O_RDONLY;
      break;
    case COB_OPEN_I_O:
      oflags = O_CREAT | O_RDWR;
      if (f->organization == COB_ORG_LINE_SEQUENTIAL)
	{
	  /* Line Sequential does not Support Mode IO */
	  RETURN_STATUS (92);
	}
      break;
    case COB_OPEN_OUTPUT:
      /* libdb doesn't support O_WRONLY */
      oflags = O_CREAT | O_TRUNC | O_RDWR;
      break;
    case COB_OPEN_EXTEND:
      oflags = O_CREAT | O_RDWR | O_APPEND;
      break;
    }
  if (f->organization == COB_ORG_INDEXED)
    {
      type = DB_BTREE;
    }
  else if (f->organization == COB_ORG_RELATIVE)
    {
/*		type= DB_RECNO;  */
    }
  if (f->organization == COB_ORG_INDEXED)
    {
      struct altkey_desc *akd;
      alt_key_no = 1;

      for (akd = f->altkeys; akd->offset != -1; akd++)
	{
	  if (akd->duplicates > 0)
	    {
	      alt_key.flags = R_DUP;
	    }
	  else
	    {
	      alt_key.flags = 0;
	    }
	  sprintf (alt_filename, "%s%d", filename, alt_key_no);
	  akd->alt_dbp =
	    dbopen (alt_filename, oflags, sflags, type, &alt_key);
	  if (!akd->alt_dbp)
	    {
	      if (errno == EINVAL)
		{
		  f->dbp = NULL;
		  RETURN_STATUS (37);
		}
	      if (errno == ENOENT)
		{
		  f->dbp = NULL;
		  RETURN_STATUS (35);
		}
	      f->dbp = NULL;
	      RETURN_STATUS (91);
	    }
	  alt_key_no++;
	}
      f->dbp = dbopen (filename, oflags, sflags, type, infop);
    }
  /* otherwise it is sequential or relative, save its file handle, converted */
  else if ((f->organization == COB_ORG_LINE_SEQUENTIAL) && (mode == COB_OPEN_INPUT))
    f->dbp = (void *) fopen (filename, "r");
  else
    {
      f->dbp = (void *) open (filename, oflags, sflags);
      if ((int) f->dbp == -1)
	f->dbp = 0;
    }

  if (!f->dbp)
    {
      // Check to see if optional
      if (f->optional && mode == COB_OPEN_INPUT)
	{
	  f->dbp = NULL;
	  f->file_missing = 1;
	  f->open_mode = mode;
	  RETURN_STATUS (0);
	}
      if (errno == EINVAL)
	{
	  f->dbp = NULL;
	  RETURN_STATUS (37);
	}
      if (errno == ENOENT)
	{
	  f->dbp = NULL;
	  RETURN_STATUS (35);
	}
      f->dbp = NULL;
      RETURN_STATUS (91);
    }
  /* save mode to check later (read,write,start,...) */
  f->open_mode = mode;
  RETURN_STATUS (0);
}

void
cob_close (struct cob_file_desc *f)
{
  /* Check to see if file is open. If not return File Status 91
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	RETURN_STATUS (0);
      RETURN_STATUS (91);
    }

  /* If there is a start not yet used delete it */

  if (f->start_record != NULL)
    {
      free (f->start_record);
      f->start_record = NULL;
    }

  if (f->organization == COB_ORG_INDEXED)
    {
      struct altkey_desc *akd;
      for (akd = f->altkeys; akd->offset != -1; akd++)
	{
	  akd->alt_dbp->close (akd->alt_dbp);
	}
      f->dbp->close (f->dbp);
    }
  else
    {
      if ((f->organization == COB_ORG_LINE_SEQUENTIAL)
	  && (f->open_mode == COB_OPEN_INPUT))
	fclose ((FILE *) f->dbp);
      else
	close ((int) f->dbp);
    }
  f->dbp = NULL;
  RETURN_STATUS (0);
}

void
cob_read (struct cob_file_desc *f, ...)
{
  int result;
  recno_t recno;
  struct cob_field_desc *fkey;
  char *keybuf;
  off_t file_pos;
  DBT key, data;
  DBT save_key;
  va_list args;
  char *tmpbuf, *s;
  int offset;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	RETURN_STATUS (10);
      RETURN_STATUS (92);
    }

  /* Check the mode the file is opened in to make sure that read
     is Allowed */
  if (f->open_mode != COB_OPEN_INPUT && f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (92);

  /* If there is a start record outstanding use it to fulfill the read */
  if (f->start_record != NULL)
    {
      memmove (f->record, f->start_record, f->reclen);
      free (f->start_record);
      f->start_record = NULL;
      RETURN_STATUS (0);
    }

  switch (f->organization)
    {
    case COB_ORG_RELATIVE:
      va_start (args, f);
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	RETURN_STATUS (23);
      file_pos = lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
      result = read ((int) f->dbp, f->record, f->reclen);
      if (f->record[0] == '\0')
	RETURN_STATUS (23);
      if (result == f->reclen)
	RETURN_STATUS (0);
      if (result == 0)
	RETURN_STATUS (10);
      RETURN_STATUS (30);
    case COB_ORG_INDEXED:
      va_start (args, f);
      fkey = va_arg (args, struct cob_field_desc *);
      keybuf = va_arg (args, char *);
      va_end (args);
      key.data = f->record + f->rec_index;
      save_key.data = f->record + f->rec_index;
      key.size = f->ixd_desc->size;
      save_key.size = f->ixd_desc->size;
      if (fkey == NULL)
	{
	  f->key_in_use = NULL;
	}
      else
	{
	  struct altkey_desc *akd;
	  for (akd = f->altkeys; akd->offset != -1; akd++)
	    {
	      if (akd->descriptor->pic == fkey->pic)
		{
		  f->key_in_use = akd;
		  key.data = keybuf;
		  key.size = akd->descriptor->size;
		  result =
		    akd->alt_dbp->seq (akd->alt_dbp, &key, &data, R_CURSOR);
		  if (result)
		    RETURN_STATUS (23);
		  if (memcmp (key.data, keybuf, key.size) != 0)
		    RETURN_STATUS (23);
		  key.data = data.data;
		  save_key.data = data.data;
		  key.size = f->ixd_desc->size;
		  save_key.size = f->ixd_desc->size;
		}
	    }
	}
      result = f->dbp->seq (f->dbp, &key, &data, R_CURSOR);
      if (result)
	RETURN_STATUS (23);	/* should have a better error info here */
      if (data.size < f->reclen)
	RETURN_STATUS (23);
      if (memcmp (save_key.data, data.data + f->rec_index, save_key.size) != 0)
	RETURN_STATUS (23);
      memmove (f->record, data.data, f->reclen);
      RETURN_STATUS (0);
    case COB_ORG_LINE_SEQUENTIAL:
      /* If the file is LINE SEQUENTIAL we need to read in the <NL> as well */
      tmpbuf = malloc (f->reclen + 2);
      memset (tmpbuf, (int) NULL, (f->reclen) + 2);
      memset (f->record, (int) ' ', f->reclen);
      fgets (tmpbuf, f->reclen + 2, (void *) f->dbp);
      if ((int) tmpbuf[0] == (int) NULL)
	{
	  free (tmpbuf);
	  RETURN_STATUS (10);
	}
      s = strchr (tmpbuf, '\n');
      if (s == NULL)
	{
	  memmove (f->record, tmpbuf, f->reclen);
	}
      else
	{
	  offset = s - tmpbuf;
	  memset (s, ' ', (f->reclen) - offset);
	  memmove (f->record, tmpbuf, f->reclen);
	}
      free (tmpbuf);
      RETURN_STATUS (0);
    default:
      /* sequential files */
      result = read ((int) f->dbp, f->record, f->reclen);
      if (result == f->reclen)
	RETURN_STATUS (0);
      if (result == 0)
	RETURN_STATUS (10);
      RETURN_STATUS (30);
    }
}

void
cob_read_next (struct cob_file_desc *f)
{
  int result;
  int flags = R_NEXT;
  DBT key, data;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	RETURN_STATUS (10);
      RETURN_STATUS (92);
    }

  /* Check the mode the file is opened in to make sure that read
     is Allowed */
  if (((f->open_mode != COB_OPEN_INPUT) && (f->open_mode != COB_OPEN_I_O)))
    RETURN_STATUS (92);

  /* If there is a start record outstanding use it to fulfill the read */
  if (f->start_record != NULL)
    {
      memmove (f->record, f->start_record, f->reclen);
      free (f->start_record);
      f->start_record = NULL;
      RETURN_STATUS (0);
    }

  switch (f->organization)
    {
    case COB_ORG_SEQUENTIAL:
      result = read ((int) f->dbp, f->record, f->reclen);
      if (result <= 0)
	RETURN_STATUS (10);		/* what errors should I return? */
      RETURN_STATUS (0);
    case COB_ORG_RELATIVE:
      result = 1;
      f->record[0] = '\0';
      while (f->record[0] == '\0' && result > 0)
	result = read ((int) f->dbp, f->record, f->reclen);
      if (result <= 0)
	RETURN_STATUS (10);		/* what errors should I return? */
      RETURN_STATUS (0);
    case COB_ORG_INDEXED:
      if (f->key_in_use != NULL)
	{
	  struct altkey_desc *akd;
	  akd = f->key_in_use;
	  result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
	  if (result)
	    RETURN_STATUS (10);
	  key.data = data.data;
	  key.size = f->ixd_desc->size;
	  flags = 0;
	  result = f->dbp->get (f->dbp, &key, &data, flags);
	  if (result)
	    RETURN_STATUS (10);	/* should have a better error info here */
	  if (data.size < f->reclen)
	    RETURN_STATUS (10);
	  memmove (f->record, data.data, f->reclen);
	  RETURN_STATUS (0);
	}
      result = f->dbp->seq (f->dbp, &key, &data, flags);
      if (result)
	RETURN_STATUS (10);	/* should have a better error info here */
      if (data.size < f->reclen)
	RETURN_STATUS (10);
      memmove (f->record, data.data, f->reclen);
      RETURN_STATUS (0);
    }
  RETURN_STATUS (99);
}

void
cob_write (struct cob_file_desc *f, ...)
{
  int result;
  recno_t recno;
  int flags = 0;
  off_t file_pos;
  DBT key, data;
  va_list args;
  char *tmpbuf;
  int offset;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    RETURN_STATUS (92);

  /* Check the mode the file is opened in to make sure that write
     is Allowed */
  if (((f->open_mode != COB_OPEN_OUTPUT) && (f->open_mode != COB_OPEN_I_O)
       && (f->open_mode != COB_OPEN_EXTEND)))
    RETURN_STATUS (92);

  data.data = f->record;
  data.size = f->reclen;
  switch (f->organization)
    {
    case COB_ORG_INDEXED:
      key.data = f->record + f->rec_index;
      key.size = f->ixd_desc->size;
      result = f->dbp->put (f->dbp, &key, &data, R_NOOVERWRITE);
      if (result == 1)
	RETURN_STATUS (22);
      else if (result)
	RETURN_STATUS (99);
      /* If the main write was successfull then we proceed to
         write out the alternate keys. Any Failure will mean that 
         we have to delete the writes we have just done.
       */
      {
	struct altkey_desc *akd;
	for (akd = f->altkeys; akd->offset != -1; akd++)
	  {
	    key.data = f->record + akd->offset;
	    key.size = akd->descriptor->size;
	    data.data = f->record + f->rec_index;
	    data.size = f->ixd_desc->size;
	    result = akd->alt_dbp->put (akd->alt_dbp, &key, &data, flags);
	    /* If an error occurs we need code to back out.
	       Will be done later.
	     */
	  }
      }
      break;
    case COB_ORG_RELATIVE:
      va_start (args, f);
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	RETURN_STATUS (23);
      file_pos = lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    case COB_ORG_LINE_SEQUENTIAL:
      tmpbuf = malloc (f->reclen + 1);
      memmove (tmpbuf, f->record, f->reclen);
      tmpbuf[f->reclen + 1] = (int) '\n';
      if ((tmpbuf[f->reclen]) == (int) NULL);
      tmpbuf[f->reclen] = ' ';
      if ((tmpbuf[f->reclen]) != ' ')
	{
	  result = write ((int) f->dbp, tmpbuf, f->reclen + 1);
	  if (!result)
	    RETURN_STATUS (99);		/* what errors should I return? */
	}
      else
	{
	  for (offset = f->reclen; offset >= 0; offset--)
	    {
	      if ((tmpbuf[f->reclen]) == (int) NULL)
		tmpbuf[f->reclen] = ' ';
	      if (tmpbuf[offset] != ' ')
		{
		  result = write ((int) f->dbp, tmpbuf, offset + 2);
		  if (!result)
		    {
		      free (tmpbuf);
		      RETURN_STATUS (99);	/* what errors should I return? */
		    }
		  break;

		}
	      tmpbuf[offset] = (int) '\n';
	      if (offset == 0)
		{
		  result = write ((int) f->dbp, tmpbuf, 1);
		  if (!result)
		    {
		      free (tmpbuf);
		      RETURN_STATUS (99);	/* what errors should I return? */
		    }
		}
	    }
	}
      free (tmpbuf);
      break;
    default:
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    }
  RETURN_STATUS (0);
}

void
cob_write_lines (struct cob_file_desc *f, int lines)
{
  int i;

  if (f->dbp == NULL)
    return;

  if (f->open_mode != COB_OPEN_OUTPUT
      && f->open_mode != COB_OPEN_I_O
      && f->open_mode != COB_OPEN_EXTEND)
    return;

  for (i = 0; i < lines; i++)
    write ((int) f->dbp, "\n", 1);
}

void
cob_write_page (struct cob_file_desc *f)
{
  if (f->dbp == NULL)
    return;

  if (f->open_mode != COB_OPEN_OUTPUT
      && f->open_mode != COB_OPEN_I_O
      && f->open_mode != COB_OPEN_EXTEND)
    return;

  write ((int) f->dbp, "\f", 1);
}

void
cob_start (struct cob_file_desc *f, int cond, ...)
{
  int result;
  recno_t recno;
  int flags = R_CURSOR;
  off_t file_pos;
  DBT key, data;
  va_list args;
  int alternate = 0;
  struct cob_field_desc *r;
  char *key_ptr;
  char new_record[f->reclen];

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    RETURN_STATUS (92);

  /* Check the mode the file is opened in to make sure that start
     is Allowed */
  if (((f->open_mode != COB_OPEN_INPUT) && (f->open_mode != COB_OPEN_I_O)))
    RETURN_STATUS (92);

  switch (f->organization)
    {
    case COB_ORG_RELATIVE:
      va_start (args, cond);
      recno = va_arg (args, recno_t);
      va_end (args);
      switch (cond)
	{
	case 1:		/* Equal to */
	  file_pos =
	    lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		RETURN_STATUS (23);
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  RETURN_STATUS (0);
		}
	    }
	  break;
	case 3:		/* Less Than */
	  recno = recno - 1;
	  if (recno < 0)
	    RETURN_STATUS (23);
	  file_pos =
	    lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		{
		  result = 1;
		  new_record[0] = '\0';
		  while (new_record[0] == '\0' && result > 0)
		    {
		      recno--;
		      file_pos =
			lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
		      result = read ((int) f->dbp, new_record, f->reclen);
		    }
		  if (result <= 0)
		    RETURN_STATUS (23);
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  RETURN_STATUS (0);
		}
	    }
	  break;
	case 5:		/* Less than or equal to */
	  file_pos =
	    lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		{
		  result = 1;
		  new_record[0] = '\0';
		  while (new_record[0] == '\0' && result > 0)
		    {
		      recno--;
		      file_pos =
			lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
		      result = read ((int) f->dbp, new_record, f->reclen);
		    }
		  if (result <= 0)
		    RETURN_STATUS (23);
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  RETURN_STATUS (0);
		}
	    }
	  break;
	case 2:		/* Greater Than */
	  recno++;
	  file_pos =
	    lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		{
		  result = 1;
		  new_record[0] = '\0';
		  while (new_record[0] == '\0' && result > 0)
		    {
		      result = read ((int) f->dbp, new_record, f->reclen);
		      recno++;
		    }
		  if (result <= 0)
		    RETURN_STATUS (23);
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  RETURN_STATUS (0);
		}
	    }
	  break;
	case 4:		/* Greater than or Equal to */
	  file_pos =
	    lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		{
		  result = 1;
		  new_record[0] = '\0';
		  while (new_record[0] == '\0' && result > 0)
		    {
		      result = read ((int) f->dbp, new_record, f->reclen);
		      recno++;
		    }
		  if (result <= 0)
		    RETURN_STATUS (23);
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  RETURN_STATUS (0);
		}
	    }
	  break;
	default:
	  RETURN_STATUS (99);
	}
      RETURN_STATUS (23);
    case COB_ORG_INDEXED:
      {
	struct altkey_desc *akd;
	va_start (args, cond);
	r = va_arg (args, struct cob_field_desc *);
	key_ptr = va_arg (args, char *);
	va_end (args);
	f->key_in_use = NULL;
	for (akd = f->altkeys; akd->offset != -1; akd++)
	  {
	    if (akd->descriptor == r)
	      {
		f->key_in_use = akd;
		alternate = 1;
		break;
	      }
	  }
	switch (cond)
	  {
	  case 1:		/* Equal To */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->size;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (memcmp (key.data, key_ptr, key.size) != 0)
		  RETURN_STATUS (23);
		key.data = data.data;
		key.size = f->ixd_desc->size;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = f->record + f->rec_index;
		key.size = f->ixd_desc->size;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);	/* should have a better error info here */
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
		if (memcmp (key.data, new_record + f->rec_index, key.size) !=
		    0)
		  RETURN_STATUS (23);
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 3:		/* Less than */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->size;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);
		  }
		/* If result greater than or equal Key read prev record */
		if (memcmp (key.data, key_ptr, key.size) > -1)
		  {
		    flags = R_PREV;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);
		  }
		key.data = data.data;
		key.size = f->ixd_desc->size;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = f->record + f->rec_index;
		key.size = f->ixd_desc->size;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);	/* should have a better error info here */
		  }
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
		/* If result greater than or equal key read prev record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) >
		    -1)
		  {
		    flags = R_PREV;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);	/* should have a better error info here */
		    if (data.size < f->reclen)
		      RETURN_STATUS (23);
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 5:		/* Less than or Equal To */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->size;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);
		  }
		/* If result greater than Key read prev record */
		if (memcmp (key.data, key_ptr, key.size) > 0)
		  {
		    flags = R_PREV;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);
		  }
		key.data = data.data;
		key.size = f->ixd_desc->size;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = f->record + f->rec_index;
		key.size = f->ixd_desc->size;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);	/* should have a better error info here */
		  }
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
		/* If result greater than key read prev record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) >
		    0)
		  {
		    flags = R_PREV;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);	/* should have a better error info here */
		    if (data.size < f->reclen)
		      RETURN_STATUS (23);
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 2:		/* Greater than */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->size;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		/* If result equals Key read next record */
//                              if(memcmp(data.data,key_ptr,akd->descriptor->size)==0)
		if (memcmp (key.data, key_ptr, key.size) == 0)
		  {
		    flags = R_NEXT;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);
		  }
		key.data = data.data;
		key.size = f->ixd_desc->size;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = f->record + f->rec_index;
		key.size = f->ixd_desc->size;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);	/* should have a better error info here */
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
		/* If result equals key read next record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) ==
		    0)
		  {
		    flags = R_NEXT;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      RETURN_STATUS (23);	/* should have a better error info here */
		    if (data.size < f->reclen)
		      RETURN_STATUS (23);
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 4:		/* Greater than or Equal To */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->size;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		key.data = data.data;
		key.size = f->ixd_desc->size;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = f->record + f->rec_index;
		key.size = f->ixd_desc->size;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  RETURN_STATUS (23);	/* should have a better error info here */
		if (data.size < f->reclen)
		  RETURN_STATUS (23);
		memmove (new_record, data.data, f->reclen);
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  default:
	    RETURN_STATUS (99);
	  }
	RETURN_STATUS (0);
      }
    default:			/* sequential files */
      RETURN_STATUS (0);
    }
}

void
cob_rewrite (struct cob_file_desc *f, ...)
{
  int result;
  recno_t recno;
  int flags = 0;
  off_t file_pos;
  DBT key, data;
  char newdata_data[f->reclen];
  int newdata_size;
  va_list args;
  char *NL = "\n";

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    RETURN_STATUS (92);

  /* Check the mode the file is opened in to make sure that rewrite
     is Allowed */
  if (f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (92);

  switch (f->organization)
    {
    case COB_ORG_INDEXED:
      break;
    case COB_ORG_RELATIVE:
      break;
    case COB_ORG_SEQUENTIAL:
      /* Rewrite No longer supported on Line Sequential files */
      RETURN_STATUS (92);
    case COB_ORG_LINE_SEQUENTIAL:
      file_pos = lseek ((int) f->dbp, - (f->reclen + 1), SEEK_CUR);
      break;
    default:
      RETURN_STATUS (30);
    }

  data.data = f->record;
  data.size = f->reclen;
  /* Save the new record */
  memmove (newdata_data, f->record, f->reclen);
  newdata_size = f->reclen;
  switch (f->organization)
    {
    case COB_ORG_INDEXED:
      key.data = f->record + f->rec_index;
      key.size = f->ixd_desc->size;
      /* Get the origional Record so we can delete the
         Alternate Keys is there is any change */
      result = f->dbp->get (f->dbp, &key, &data, flags);
      memmove (f->record, data.data, f->reclen);
      if (result)
	RETURN_STATUS (23);
      {
	struct altkey_desc *akd;
	for (akd = f->altkeys; akd->offset != -1; akd++)
	  {
	    key.data = f->record + akd->offset;
	    key.size = akd->descriptor->size;
	    result = akd->alt_dbp->del (akd->alt_dbp, &key, flags);
	  }
      }
      memmove (f->record, newdata_data, newdata_size);
      key.data = f->record + f->rec_index;
      key.size = f->ixd_desc->size;
      data.data = newdata_data;
      data.size = newdata_size;
      /* Rewrite the Main Record */
      result = f->dbp->put (f->dbp, &key, &data, flags);
      if (result)
	RETURN_STATUS (99);		/* ? error code to be determined */
      result = f->dbp->seq (f->dbp, &key, &data, R_CURSOR);
      {
	/* Rewrite the Alternate Keys */
	struct altkey_desc *akd;
	for (akd = f->altkeys; akd->offset != -1; akd++)
	  {
	    key.data = f->record + akd->offset;
	    key.size = akd->descriptor->size;
	    data.data = f->record + f->rec_index;
	    data.size = f->ixd_desc->size;
	    result = akd->alt_dbp->put (akd->alt_dbp, &key, &data, flags);
	    result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, R_CURSOR);
	  }
      }
      break;
    case COB_ORG_RELATIVE:
      va_start (args, f);
      recno = va_arg (args, recno_t);
      va_end (args);
      file_pos = lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    case COB_ORG_LINE_SEQUENTIAL:
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      result = write ((int) f->dbp, NL, 1);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    default:
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    }
  RETURN_STATUS (0);
}

void
cob_delete (struct cob_file_desc *f, ...)
{
  int result;
  recno_t recno;
  int flags = 0;
  off_t file_pos;
  va_list args;
  DBT key;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    RETURN_STATUS (92);

  /* Check the mode the file is opened in to make sure that delete
     is Allowed */
  if (f->open_mode != COB_OPEN_I_O)
    RETURN_STATUS (92);

  switch (f->organization)
    {
    case COB_ORG_INDEXED:
      {
	struct altkey_desc *akd;
	for (akd = f->altkeys; akd->offset != -1; akd++)
	  {
	    key.data = f->record + akd->offset;
	    key.size = akd->descriptor->size;
	    result = akd->alt_dbp->del (akd->alt_dbp, &key, flags);
	  }
      }
      key.data = f->record + f->rec_index;
      key.size = f->ixd_desc->size;
      result = f->dbp->del (f->dbp, &key, flags);
      if (result != 0)
	RETURN_STATUS (23);
      break;
    case COB_ORG_RELATIVE:
      va_start (args, f);
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	RETURN_STATUS (23);
      file_pos = lseek ((int) f->dbp, recno * f->reclen, SEEK_SET);
      memset (f->record, 0, f->reclen);
      result = write ((int) f->dbp, f->record, f->reclen);
      if (!result)
	RETURN_STATUS (99);		/* what errors should I return? */
      break;
    default:
      RETURN_STATUS (37);
    }
  RETURN_STATUS (0);
}
