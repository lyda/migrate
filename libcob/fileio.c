/* File Handling Module
 *
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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


#include "_libcob.h"

#define bcounter	 5
#if defined(SunOS)
va_list __builtin_va_alist;
#endif

#ifdef WANT_ALL_DYNAMIC_LIBS
DB *db_open_stub (const char *s, int i, int j, DBTYPE d, const void *p);
DB *(*db_open) (const char *, int, int, DBTYPE, const void *) = db_open_stub;

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          db_open_stub                                  |
 |                                                                        |
\*------------------------------------------------------------------------*/

DB *
db_open_stub (const char *s, int i, int j, DBTYPE d, const void *p)
{
  char *libname = "libdb.so";
  void *handle = dlopen (libname, RTLD_LAZY);
  if (!handle)
    {
      fprintf (stderr, "*ERROR* loading %s: %s\n", libname, dlerror ());
      return NULL;
    }
  db_open = dlsym (handle, "dbopen");
  return (*db_open) (s, i, j, d, p);
}
#else
#define db_open dbopen
#endif


/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_check_varying                             |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_check_varying (struct file_desc *f,
		   struct fld_desc *min_desc, char *min_value,
		   struct fld_desc *max_desc, char *max_value,
		   struct fld_desc *reclen_desc, char *reclen_value)
{

  int mi_value;
  int mx_value;
  int rc_len;
  char temp[20];
  struct fld_desc *t;
  char *t_value;
  char *b;

  t = (struct fld_desc *) malloc (sizeof (struct fld_desc *));
  t_value = (char *) malloc (sizeof (char *));
  b = (char *) malloc (sizeof (char *));
  memset (temp, 0, 19);
  if (min_desc == NULL)
    {
      mi_value = 0;
    }
  else
    {
      strncpy (temp, min_value, min_desc->len);
      mi_value = atoi (temp);
    }
  memset (temp, 0, 19);
  strncpy (temp, max_value, max_desc->len);
  mx_value = atoi (temp);
  memset (temp, 0, 19);
  t->len = reclen_desc->len;
  t->type = '9';
  t->decimals = reclen_desc->decimals;
  t->all = reclen_desc->all;
  t->just_r = reclen_desc->just_r;
  t->reserved = reclen_desc->reserved;
  t->pic = b;
  cob_move (reclen_desc, reclen_value, t, t_value);
  strncpy (temp, t_value, t->len);
  rc_len = atoi (temp);
  if ((rc_len <= mx_value) && (rc_len >= mi_value))
    {
      f->reclen = rc_len;
    }
  else
    {
      f->reclen = -1;
      return 99;
    }

  return 0;

}


/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_open                                      |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_open (struct file_desc *f, char *record, char *fname, int mode)
{
  DBTYPE type = DB_BTREE;
  void *infop = NULL;
  int sflags = S_IRUSR | S_IWUSR;
  int oflags;
  char *filename, *evname, *pt;
  char alt_filename[128];
  int alt_key_no;
  int len;

  BTREEINFO alt_key;
  alt_key.flags = 0;
  alt_key.cachesize = 0;
  alt_key.maxkeypage = 0;
  alt_key.minkeypage = 0;
  alt_key.psize = 0;
  alt_key.compare = NULL;
  alt_key.prefix = NULL;
  alt_key.lorder = 0;

  // Check the correct structure
  if (f->vers_id < RTL_FILE_VERSION)
    {
      fprintf (stderr, "You need to recompile your program\n");
      fprintf (stderr, "Version mismatch; structure %x, RTL %x\n",
	       f->vers_id, RTL_FILE_VERSION);
      return 99;
    }
/*
	BEWARE: 
	fname points to a field storage (non null-terminated) 
	we must copy fname to a C string and terminate it with a \0
	and also trim spaces at the end. 

        NOTE: 
        This is a hack to accommodate the 'EXTERNAL' clause in 
        the SELECT statement. 
        
        The 'access_mode' in the file structure 'struct file_desc' is 
        used to indicate how the value stored in 'fname' is used.
        It can be used to define a filename variable or a environment
        variable.
        
        If the access_mode < ACCEV_ENVAR (5), then 'fname' is used to 
        define a filename. Else 'fname' is used as an environment 
        variable which is used to determined the actual filename.
        If no environment variable is found or is empty, then the 
        filename defaults to the value stored in 'fname'.
*/
  if (f->access_mode < ACCEV_ENVAR)
    {
      len = f->fname_desc->len;
      filename = malloc (len + 1);
      memmove (filename, fname, len);
      do
	{
	  filename[len--] = 0;
	}
      while (filename[len] == ' ');
      evname = NULL;
    }
  else
    {
      len = f->fname_desc->len;
      evname = malloc (len + 1);
      memmove (evname, fname, len);
      do
	{
	  evname[len--] = 0;
	}
      while (evname[len] == ' ');

      /* Get environment variable, if it exists */
      if ((pt = getenv (evname)) != NULL)
	{
	  len = strlen (pt);
	  if (len > 0)
	    {
	      filename = malloc (len + 1);
	      strncpy (filename, pt, len);
	    }
	  else
	    {
	      len = strlen (evname);
	      filename = malloc (len + 1);
	      strncpy (filename, evname, len);
	    }
	}
      else
	{
	  len = strlen (evname);
	  filename = malloc (len + 1);
	  strncpy (filename, evname, len);
	}
    }

  /* Check to see if the file is already open. If so return
     File Status 91 in according to the Ansi 74 Standard. */
  if (f->dbp != NULL)
    return 91;

  switch (mode)
    {
    case FMOD_INPUT:
      oflags = O_RDONLY;
      break;
    case FMOD_IO:
      oflags = O_RDWR;
      if (f->organization == ORG_LINESEQUENTIAL)
	{
	  /* Line Sequential does not Support Mode IO */
	  return 92;
	}
      break;
    case FMOD_OUTPUT:
      /* libdb doesn't support O_WRONLY */
      oflags = O_CREAT | O_TRUNC | O_RDWR;
      break;
    case FMOD_EXTEND:
      oflags = O_CREAT | O_RDWR | O_APPEND;
      break;
    }
  if (f->organization == ORG_INDEXED)
    {
      type = DB_BTREE;
    }
  else if (f->organization == ORG_RELATIVE)
    {
/*		type= DB_RECNO;  */
    }
  if (f->organization == ORG_INDEXED)
    {
      struct altkey_desc *akd;
      alt_key_no = 1;

      for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	{
//                      printf("Alternate key: offset=%d, descriptor pic=0x%08x, dupl=%d\n",
//                              akd->offset, (int)akd->descriptor->pic, akd->duplicates);
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
	    db_open (alt_filename, oflags, sflags, type, &alt_key);
	  if (!akd->alt_dbp)
	    {
	      if (errno == EINVAL)
		{
		  f->dbp = NULL;
		  return 37;
		}
	      if (errno == ENOENT)
		{
		  f->dbp = NULL;
		  return 35;
		}
	      f->dbp = NULL;
	      return 91;
	    }
	  alt_key_no++;
	}
      f->dbp = db_open (filename, oflags, sflags, type, infop);
    }
  /* otherwise it is sequential or relative, save its file handle, converted */
  else if ((f->organization == ORG_LINESEQUENTIAL) && (mode == FMOD_INPUT))
    f->dbp = (void *) fopen (filename, "r");
  else
    {
      f->dbp = (void *) open (filename, oflags, sflags);
      if ((int) f->dbp == -1)
	f->dbp = 0;
    }

  free (filename);
  if (evname != NULL)
    free (evname);

  if (!f->dbp)
    {
      // Check to see if optional
      if (f->optional && mode == FMOD_INPUT)
	{
	  f->dbp = NULL;
	  f->file_missing = 1;
	  f->open_mode = mode;
	  return 0;
	}
      if (errno == EINVAL)
	{
	  f->dbp = NULL;
	  return 37;
	}
      if (errno == ENOENT)
	{
	  f->dbp = NULL;
	  return 35;
	}
      f->dbp = NULL;
      return 91;
    }
  /* save mode to check later (read,write,start,...) */
  f->open_mode = mode;
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_close                                     |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_close (struct file_desc *f, char *record)
{
  /* Check to see if file is open. If not return File Status 91
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	return 0;
      return 91;
    }

  /* If there is a start not yet used delete it */

  if (f->start_record != NULL)
    {
      free (f->start_record);
      f->start_record = NULL;
    }

  if (f->organization == ORG_INDEXED)
    {
      struct altkey_desc *akd;
      for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	{
	  akd->alt_dbp->close (akd->alt_dbp);
	}
      f->dbp->close (f->dbp);
    }
  else
    {
//              if ((f->organization == ORG_LINESEQUENTIAL || f->organization == ORG_SEQUENTIAL) && f->open_mode == FMOD_OUTPUT
      if (f->organization == ORG_LINESEQUENTIAL && f->open_mode == FMOD_OUTPUT
	  && f->adv_before == 0)
	{
	  write ((int) f->dbp, "\x0a", 1);
	}
      if ((f->organization == ORG_LINESEQUENTIAL)
	  && (f->open_mode == FMOD_INPUT))
	fclose ((FILE *) f->dbp);
      else
	close ((int) f->dbp);
    }
  f->dbp = NULL;
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_read                                      |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_read (struct file_desc *f, char *record, ...)
{
  int result;
  recno_t recno;
  struct fld_desc *fkey;
  char *keybuf;
  struct fld_desc *reclen_desc;
  char *reclen_buf;
  off_t file_pos;
  DBT key, data;
  DBT save_key;
  va_list args;
  char *tmpbuf, *s;
  int offset;
  char rsize[bcounter];
  int rlen;
  char *temp_record;
  struct fld_desc *t;
  char t_value[bcounter];
  char *b;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	return 10;
      return 92;
    }

  /* Check to see that the record length is valid */
  if (f->reclen == -1)
    return 99;

  /* Check the mode the file is opened in to make sure that read
     is Allowed */
  if (((f->open_mode != FMOD_INPUT) && (f->open_mode != FMOD_IO)))
    return 92;

  /* check if reclen was given */
  va_start (args, record);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);


  /* If there is a start record outstanding use it to fulfill the read */
  if (f->start_record != NULL)
    {
      memmove (record, f->start_record, f->reclen);
      free (f->start_record);
      f->start_record = NULL;
      return 0;
    }

  switch (f->organization)
    {
    case ORG_RELATIVE:
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	return 23;
      file_pos = lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
      result = read ((int) f->dbp, record, f->reclen);
      if (record[0] == '\0')
	return 23;
      if (result == f->reclen)
	return 0;
      if (result == 0)
	return 10;
      if (reclen_desc != NULL)
	{
	  sprintf (reclen_buf, "%0*i", (int) reclen_desc->len, result);
	  return 0;
	}
      return 30;
    case ORG_INDEXED:
      fkey = va_arg (args, struct fld_desc *);
      keybuf = va_arg (args, char *);
      va_end (args);
      key.data = record + f->rec_index;
      save_key.data = record + f->rec_index;
      key.size = f->ixd_desc->len;
      save_key.size = f->ixd_desc->len;
      if (fkey == NULL)
	{
	  f->key_in_use = NULL;
	}
      else
	{
	  struct altkey_desc *akd;
	  for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	    {
	      if (akd->descriptor->pic == fkey->pic)
		{
		  f->key_in_use = akd;
		  key.data = keybuf;
		  key.size = akd->descriptor->len;
		  result =
		    akd->alt_dbp->seq (akd->alt_dbp, &key, &data, R_CURSOR);
		  if (result)
		    return 23;
		  if (memcmp (key.data, keybuf, key.size) != 0)
		    return 23;
		  key.data = data.data;
		  save_key.data = data.data;
		  key.size = f->ixd_desc->len;
		  save_key.size = f->ixd_desc->len;
		}
	    }
	}
      result = f->dbp->seq (f->dbp, &key, &data, R_CURSOR);
      if (result)
	return 23;		/* should have a better error info here */
      if (data.size < f->reclen)
	{
	  if (reclen_desc != NULL)
	    {
	      sprintf (reclen_buf, "%0*i", (int) reclen_desc->len, result);
	      return 0;
	    }
	  else
	    return 23;
	}
      if (memcmp (save_key.data, data.data + f->rec_index, save_key.size) !=
	  0)
	return 23;
      memmove (record, data.data, f->reclen);
      return 0;
    case ORG_LINESEQUENTIAL:
      /* If the file is LINE SEQUENTIAL we need to read in the <NL> as well */
      tmpbuf = malloc (f->reclen + 2);
      memset (tmpbuf, (int) NULL, (f->reclen) + 2);
      memset (record, (int) ' ', f->reclen);
      fgets (tmpbuf, f->reclen + 2, (void *) f->dbp);
      if ((int) tmpbuf[0] == (int) NULL)
	{
	  free (tmpbuf);
	  return 10;
	}
      s = strchr (tmpbuf, '\n');
      if (s == NULL)
	{
	  //              tmpbuf[f->reclen] = ' ';
	  memmove (record, tmpbuf, f->reclen);
	}
      else
	{
	  offset = s - tmpbuf;
	  memset (s, ' ', (f->reclen) - offset);
	  memmove (record, tmpbuf, f->reclen);
	}
      free (tmpbuf);
      return 0;
    default:			/* sequential files */
      if (reclen_desc != NULL)
	{
	  /* Variable Length Sequential File */
	  result = read ((int) f->dbp, rsize, bcounter);
	  if (result == 0)
	    return 10;
	  rlen = atoi (rsize);
	  f->reclen = rlen - bcounter;
	  temp_record = malloc (f->reclen + bcounter);
	  memset (temp_record, ' ', rlen);
	  result = read ((int) f->dbp, temp_record, rlen);
	  if (result == 0)
	    return 10;
	  memmove (record, temp_record, result - bcounter);
	  sprintf (t_value, "%0*i", bcounter, result - bcounter);
	  t = (struct fld_desc *) malloc (sizeof (struct fld_desc *));
//                      t_value=(char *)malloc(sizeof(char *));
	  b = (char *) malloc (sizeof (char *));
	  t->len = bcounter;
	  t->type = '9';
	  t->decimals = 0;
	  t->all = reclen_desc->all;
	  t->just_r = reclen_desc->just_r;
	  t->reserved = reclen_desc->reserved;
	  t->pic = b;
	  cob_move (t, t_value, reclen_desc, reclen_buf);
	  return 0;
	}
      else
	{
	  result = read ((int) f->dbp, record, f->reclen);
	}
      if (result == f->reclen)
	return 0;
      if (result == 0)
	return 10;
      return 30;
    }
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_read_next                                 |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_read_next (struct file_desc *f, char *record, ...)
{
  int result;
  int flags = R_NEXT;
  DBT key, data;
  va_list args;
  struct fld_desc *reclen_desc;
  char *reclen_buf;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	return 10;
      return 92;
    }

  /* Check the mode the file is opened in to make sure that read
     is Allowed */
  if (((f->open_mode != FMOD_INPUT) && (f->open_mode != FMOD_IO)))
    return 92;

  /* check if reclen was given */
  va_start (args, record);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);

  /* If there is a start record outstanding use it to fulfill the read */
  if (f->start_record != NULL)
    {
      memmove (record, f->start_record, f->reclen);
      free (f->start_record);
      f->start_record = NULL;
      return 0;
    }

  switch (f->organization)
    {
    case ORG_SEQUENTIAL:
      result = read ((int) f->dbp, record, f->reclen);
      if (result <= 0)
	return 10;		/* what errors should I return? */
      return 0;
    case ORG_RELATIVE:
      result = 1;
      record[0] = '\0';
      while (record[0] == '\0' && result > 0)
	{
	  result = read ((int) f->dbp, record, f->reclen);
	}
      if (result <= 0)
	return 10;		/* what errors should I return? */
      return 0;
    case ORG_INDEXED:
      if (f->key_in_use != NULL)
	{
	  struct altkey_desc *akd;
	  akd = (struct altkey_desc *) (f->key_in_use);
	  result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
	  if (result)
	    return 10;
	  key.data = data.data;
	  key.size = f->ixd_desc->len;
	  flags = 0;
	  result = f->dbp->get (f->dbp, &key, &data, flags);
	  if (result)
	    return 10;		/* should have a better error info here */
	  if (data.size < f->reclen)
	    return 10;
	  memmove (record, data.data, f->reclen);
	  return 0;
	}
      result = f->dbp->seq (f->dbp, &key, &data, flags);
      if (result)
	return 10;		/* should have a better error info here */
      if (data.size < f->reclen)
	return 10;
      memmove (record, data.data, f->reclen);
      return 0;
    }
  return 99;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_read_prev                                 |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_read_prev (struct file_desc *f, char *record, ...)
{
  int result;
  int flags = R_PREV;
  off_t file_pos;
  DBT key, data;
  va_list args;
  struct fld_desc *reclen_desc;
  char *reclen_buf;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    {
      if (f->optional && f->file_missing)
	return 10;
      return 92;
    }

  /* Check the mode the file is opened in to make sure that read
     is Allowed */
  if (((f->open_mode != FMOD_INPUT) && (f->open_mode != FMOD_IO)))
    return 92;

  /* check if reclen was given */
  va_start (args, record);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);


  /* If there is a start record outstanding use it to fulfill the read */
  if (f->start_record != NULL)
    {
      memmove (record, f->start_record, f->reclen);
      free (f->start_record);
      f->start_record = NULL;
      return 0;
    }

  switch (f->organization)
    {
    case ORG_SEQUENTIAL:
      /* Need some logic here to figure out if at beginning of file */
      /* As a result of the previous Read Previous */
      file_pos = lseek ((int) f->dbp, (2 * ((f->reclen) * -1)), SEEK_CUR);
      result = read ((int) f->dbp, record, f->reclen);
      if (result <= 0)
	return 10;
      return 0;
    case ORG_RELATIVE:
      result = 1;
      record[0] = '\0';
      while (record[0] == '\0' && result > 0)
	{
	  file_pos = lseek ((int) f->dbp, (2 * ((f->reclen) * -1)), SEEK_CUR);
	  if (file_pos == 0)
	    return 10;
	  result = read ((int) f->dbp, record, f->reclen);
	}
      if (result <= 0)
	return 10;
      return 0;
    case ORG_INDEXED:
      if (f->key_in_use != NULL)
	{
	  struct altkey_desc *akd;
	  akd = (struct altkey_desc *) (f->key_in_use);
	  result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
	  if (result)
	    return 10;
	  key.data = data.data;
	  key.size = f->ixd_desc->len;
	  flags = 0;
	  result = f->dbp->get (f->dbp, &key, &data, flags);
	  if (result)
	    return 10;		/* should have a better error info here */
	  if (data.size < f->reclen)
	    return 10;
	  memmove (record, data.data, f->reclen);
	  return 0;
	}
      result = f->dbp->seq (f->dbp, &key, &data, flags);
      if (result)
	return 10;		/* should have a better error info here */
      if (data.size < f->reclen)
	return 10;
      memmove (record, data.data, f->reclen);
      return 0;
    }
  return 99;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_write                                     |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_write (struct file_desc *f, char *record, ...)
{
  int result;
  recno_t recno;
  int flags = 0;
  off_t file_pos;
  DBT key, data;
  va_list args;
  char *tmpbuf;
  int offset;
  struct fld_desc *reclen_desc;
  char *reclen_buf;
  unsigned int slen = 0;
  unsigned int elen = 0;
  char sclen[bcounter];
  char temp_record[f->reclen];
  char eclen[bcounter];

//      t->temp_record=malloc(f->reclen);

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    return 92;

  /* Check to see that the record length is valid */
  if (f->reclen == -1)
    return 99;

  /* Check the mode the file is opened in to make sure that write
     is Allowed */
  if (
      ((f->open_mode != FMOD_OUTPUT) && (f->open_mode != FMOD_IO)
       && (f->open_mode != FMOD_EXTEND)))
    return 92;

  /* check if reclen was given */
  va_start (args, record);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);

  data.data = record;
  data.size = f->reclen;
  switch (f->organization)
    {
    case ORG_INDEXED:
      key.data = record + f->rec_index;
      key.size = f->ixd_desc->len;
      result = f->dbp->put (f->dbp, &key, &data, R_NOOVERWRITE);
      if (result == 1)
	return 22;
      else if (result)
	return 99;
      /* If the main write was successfull then we proceed to
         write out the alternate keys. Any Failure will mean that 
         we have to delete the writes we have just done.
       */
      {
	struct altkey_desc *akd;
	for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	  {
	    key.data = record + akd->offset;
	    key.size = akd->descriptor->len;
	    data.data = record + f->rec_index;
	    data.size = f->ixd_desc->len;
	    result = akd->alt_dbp->put (akd->alt_dbp, &key, &data, flags);
	    /* If an error occurs we need code to back out.
	       Will be done later.
	     */
	  }
      }
      break;
    case ORG_RELATIVE:
//              va_start(args,record);
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	return 23;
      file_pos = lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
      result = write ((int) f->dbp, record, f->reclen);
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    case ORG_LINESEQUENTIAL:
      tmpbuf = malloc (f->reclen + 1);
      memmove (tmpbuf, record, f->reclen);
      tmpbuf[f->reclen + 1] = (int) '\n';
      if ((tmpbuf[f->reclen]) == (int) NULL);
      tmpbuf[f->reclen] = ' ';
      if ((tmpbuf[f->reclen]) != ' ')
	{
	  result = write ((int) f->dbp, tmpbuf, f->reclen + 1);
	  if (!result)
	    return 99;		/* what errors should I return? */
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
		      return 99;	/* what errors should I return? */
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
		      return 99;	/* what errors should I return? */
		    }
		}
	    }
	}
      free (tmpbuf);
      break;
    default:
      if (reclen_desc != NULL)
	{
	  /* If this is a variable length record write out the length */
	  slen = f->reclen + bcounter;
	  elen = slen;
	  sprintf (sclen, "%0*i", bcounter, slen);
	  sprintf (eclen, "%0*i", bcounter, elen);
	  memmove (temp_record, record, f->reclen);
	  result = write ((int) f->dbp, sclen, bcounter);
	  result = write ((int) f->dbp, temp_record, f->reclen);
	  result = write ((int) f->dbp, eclen, bcounter);
	}
      else
	{
	  result = write ((int) f->dbp, record, f->reclen);
	}
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    }
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_delete                                    |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_delete (struct file_desc *f, char *record, ...)
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
    return 92;

  /* Check to see that the record length is valid */
  if (f->reclen == -1)
    return 99;


  /* Check the mode the file is opened in to make sure that delete
     is Allowed */
  if (f->open_mode != FMOD_IO)
    return 92;


  switch (f->organization)
    {
    case ORG_INDEXED:
      {
	struct altkey_desc *akd;
	for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	  {
	    key.data = record + akd->offset;
	    key.size = akd->descriptor->len;
	    result = akd->alt_dbp->del (akd->alt_dbp, &key, flags);
	  }
      }
      key.data = record + f->rec_index;
      key.size = f->ixd_desc->len;
      result = f->dbp->del (f->dbp, &key, flags);
      if (result != 0)
	return 23;
      break;
    case ORG_RELATIVE:
      va_start (args, record);
      recno = va_arg (args, recno_t);
      va_end (args);
      if (recno < 1)
	return 23;
/*		recno = recno - 1; */
      file_pos = lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
      memset (record, 0, f->reclen);
      result = write ((int) f->dbp, record, f->reclen);
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    default:
      return 37;
    }
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_start                                     |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_start (struct file_desc *f, char *record, int cond, ...)
{
  int result;
  recno_t recno;
  int flags = R_CURSOR;
  off_t file_pos;
  DBT key, data;
  va_list args;
  int alternate = 0;
  struct fld_desc *r;
  char *key_ptr;
  char new_record[f->reclen];

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    return 92;

  /* Check to see that the record length is valid */
  if (f->reclen == -1)
    return 99;

  /* Check the mode the file is opened in to make sure that start
     is Allowed */
  if (((f->open_mode != FMOD_INPUT) && (f->open_mode != FMOD_IO)))
    return 92;

  switch (f->organization)
    {
    case ORG_RELATIVE:
      va_start (args, cond);
      recno = va_arg (args, recno_t);
      va_end (args);
      switch (cond)
	{
	case 1:		/* Equal to */
	  file_pos =
	    lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
	  if (file_pos > 0)
	    {
	      result = read ((int) f->dbp, new_record, f->reclen);
	      if (new_record[0] == '\0')
		return 23;
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  return 0;
		}
	    }
	  break;
	case 2:		/* Less Than */
	  recno = recno - 1;
	  if (recno < 0)
	    return 23;
	  file_pos =
	    lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
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
			lseek ((int) f->dbp, ((recno) * ((f->reclen))),
			       SEEK_SET);
		      result = read ((int) f->dbp, new_record, f->reclen);
		    }
		  if (result <= 0)
		    return 23;
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  return 0;
		}
	    }
	  break;
	case 3:		/* Less than or equal to */
	  file_pos =
	    lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
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
			lseek ((int) f->dbp, ((recno) * ((f->reclen))),
			       SEEK_SET);
		      result = read ((int) f->dbp, new_record, f->reclen);
		    }
		  if (result <= 0)
		    return 23;
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  return 0;
		}
	    }
	  break;
	case 4:		/* Greater Than */
	  recno++;
	  file_pos =
	    lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
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
		    return 23;
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  return 0;
		}
	    }
	  break;
	case 5:		/* Greater than or Equal to */
	  file_pos =
	    lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
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
		    return 23;
		}
	      if (result == f->reclen)
		{
		  f->start_record = malloc (f->reclen);
		  memmove (f->start_record, new_record, f->reclen);
		  return 0;
		}
	    }
	  break;
	case 6:
	default:
	  return 99;
	}
      return 23;
    case ORG_INDEXED:
      {
	struct altkey_desc *akd;
	va_start (args, cond);
	r = va_arg (args, struct fld_desc *);
	key_ptr = va_arg (args, char *);
	va_end (args);
	f->key_in_use = NULL;
	for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
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
		key.size = akd->descriptor->len;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (memcmp (key.data, key_ptr, key.size) != 0)
		  return 23;
		key.data = data.data;
		key.size = f->ixd_desc->len;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = record + f->rec_index;
		key.size = f->ixd_desc->len;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  return 23;	/* should have a better error info here */
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
		if (memcmp (key.data, new_record + f->rec_index, key.size) !=
		    0)
		  return 23;
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 2:		/* Less than */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->len;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      return 23;
		  }
		/* If result greater than or equal Key read prev record */
		if (memcmp (key.data, key_ptr, key.size) > -1)
		  {
		    flags = R_PREV;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      return 23;
		  }
		key.data = data.data;
		key.size = f->ixd_desc->len;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = record + f->rec_index;
		key.size = f->ixd_desc->len;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      return 23;	/* should have a better error info here */
		  }
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
		/* If result greater than or equal key read prev record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) >
		    -1)
		  {
		    flags = R_PREV;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      return 23;	/* should have a better error info here */
		    if (data.size < f->reclen)
		      return 23;
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 3:		/* Less than or Equal To */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->len;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      return 23;
		  }
		/* If result greater than Key read prev record */
		if (memcmp (key.data, key_ptr, key.size) > 0)
		  {
		    flags = R_PREV;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      return 23;
		  }
		key.data = data.data;
		key.size = f->ixd_desc->len;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = record + f->rec_index;
		key.size = f->ixd_desc->len;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  {
		    flags = R_LAST;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      return 23;	/* should have a better error info here */
		  }
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
		/* If result greater than key read prev record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) >
		    0)
		  {
		    flags = R_PREV;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      return 23;	/* should have a better error info here */
		    if (data.size < f->reclen)
		      return 23;
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 4:		/* Greater than */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->len;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  return 23;
		/* If result equals Key read next record */
//                              if(memcmp(data.data,key_ptr,akd->descriptor->len)==0)
		if (memcmp (key.data, key_ptr, key.size) == 0)
		  {
		    flags = R_NEXT;
		    result =
		      akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		    if (result)
		      return 23;
		  }
		key.data = data.data;
		key.size = f->ixd_desc->len;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = record + f->rec_index;
		key.size = f->ixd_desc->len;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  return 23;	/* should have a better error info here */
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
		/* If result equals key read next record */
		if (memcmp (key.data, new_record + f->rec_index, key.size) ==
		    0)
		  {
		    flags = R_NEXT;
		    result = f->dbp->seq (f->dbp, &key, &data, flags);
		    if (result)
		      return 23;	/* should have a better error info here */
		    if (data.size < f->reclen)
		      return 23;
		    memmove (new_record, data.data, f->reclen);
		  }
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 5:		/* Greater than or Equal To */
	    if (alternate == 1)
	      {
		key.data = key_ptr;
		key.size = akd->descriptor->len;
		result = akd->alt_dbp->seq (akd->alt_dbp, &key, &data, flags);
		if (result)
		  return 23;
		key.data = data.data;
		key.size = f->ixd_desc->len;
		flags = 0;
		result = f->dbp->get (f->dbp, &key, &data, flags);
		if (result)
		  return 23;
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    else
	      {
		key.data = record + f->rec_index;
		key.size = f->ixd_desc->len;
		result = f->dbp->seq (f->dbp, &key, &data, flags);
		if (result)
		  return 23;	/* should have a better error info here */
		if (data.size < f->reclen)
		  return 23;
		memmove (new_record, data.data, f->reclen);
	      }
	    f->start_record = malloc (f->reclen);
	    memmove (f->start_record, new_record, f->reclen);
	    break;
	  case 6:
	  default:
	    return 99;
	  }
	return 0;
      }
    default:			/* sequential files */
      return 0;
    }
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_save_status                               |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_save_status (char *status, int rt)
{
  status[0] = rt / 10 + '0';
  status[1] = rt % 10 + '0';
  return rt;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_rewrite                                   |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_rewrite (struct file_desc *f, char *record, ...)
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
  struct fld_desc *reclen_desc;
  char *reclen_buf;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    return 92;

  /* Check to see that the record length is valid */
  if (f->reclen == -1)
    return 99;

  /* Check the mode the file is opened in to make sure that rewrite
     is Allowed */
  if (f->open_mode != FMOD_IO)
    return 92;

  /* check if reclen was given */
  va_start (args, record);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);


  switch (f->organization)
    {
    case ORG_INDEXED:
      break;
    case ORG_RELATIVE:
      break;
    case ORG_SEQUENTIAL:
      /* Rewrite No longer supported on Line Sequential files */
      return 92;
//              file_pos = lseek((int)f->dbp, ((f->reclen)* -1), SEEK_CUR);
//              break;
    case ORG_LINESEQUENTIAL:
      file_pos = lseek ((int) f->dbp, (((f->reclen + 1)) * -1), SEEK_CUR);
      break;
    default:
      return 30;
    }

  data.data = record;
  data.size = f->reclen;
  /* Save the new record */
  memmove (newdata_data, record, f->reclen);
  newdata_size = f->reclen;
  switch (f->organization)
    {
    case ORG_INDEXED:
      key.data = record + f->rec_index;
      key.size = f->ixd_desc->len;
      /* Get the origional Record so we can delete the
         Alternate Keys is there is any change */
      result = f->dbp->get (f->dbp, &key, &data, flags);
      memmove (record, data.data, f->reclen);
      if (result)
	return 23;
      {
	struct altkey_desc *akd;
	for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	  {
	    key.data = record + akd->offset;
	    key.size = akd->descriptor->len;
	    result = akd->alt_dbp->del (akd->alt_dbp, &key, flags);
	  }
      }
      memmove (record, newdata_data, newdata_size);
      key.data = record + f->rec_index;
      key.size = f->ixd_desc->len;
      data.data = newdata_data;
      data.size = newdata_size;
      /* Rewrite the Main Record */
      result = f->dbp->put (f->dbp, &key, &data, flags);
      if (result)
	return 99;		/* ? error code to be determined */
      {
	/* Rewrite the Alternate Keys */
	struct altkey_desc *akd;
	for (akd = (struct altkey_desc *) (f + 1); akd->offset != -1; akd++)
	  {
	    key.data = record + akd->offset;
	    key.size = akd->descriptor->len;
	    data.data = record + f->rec_index;
	    data.size = f->ixd_desc->len;
	    result = akd->alt_dbp->put (akd->alt_dbp, &key, &data, flags);
	  }
      }
      break;
    case ORG_RELATIVE:
//              va_start(args,record);
      recno = va_arg (args, recno_t);
      va_end (args);
      file_pos = lseek ((int) f->dbp, ((recno) * ((f->reclen))), SEEK_SET);
      result = write ((int) f->dbp, record, f->reclen);
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    case ORG_LINESEQUENTIAL:
      result = write ((int) f->dbp, record, f->reclen);
      if (!result)
	return 99;		/* what errors should I return? */
      result = write ((int) f->dbp, NL, 1);
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    default:
      result = write ((int) f->dbp, record, f->reclen);
      if (!result)
	return 99;		/* what errors should I return? */
      break;
    }
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_write_adv                                 |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_write_adv (struct file_desc *f, char *record, int opt, ...)
{
  int result;
  va_list args;
  struct fld_desc *cnt_desc;
  char *cnt_buf;
  int lines = 0;
  struct fld_desc *reclen_desc;
  char *reclen_buf;

  /* Check to see if file is open. If not return File Status 92
     In accordance with the Cobol 74 Standard. */

  if (f->dbp == NULL)
    return 92;

  /* Check the mode the file is opened in to make sure that write
     is Allowed */
  if (
      ((f->open_mode != FMOD_OUTPUT) && (f->open_mode != FMOD_IO)
       && (f->open_mode != FMOD_EXTEND)))
    return 92;

  /* check if reclen was given */
  va_start (args, opt);
  reclen_desc = va_arg (args, struct fld_desc *);
  if (reclen_desc != NULL)
    reclen_buf = va_arg (args, char *);

  f->with_advancing = 1;
  f->adv_before = 0;
//      va_start( args, opt );
  if (opt > 0)
    {
      cnt_desc = va_arg (args, struct fld_desc *);
      cnt_buf = va_arg (args, char *);
      lines = get_index (cnt_desc, cnt_buf);
      if (opt == 1)
	{
	  result = write ((int) f->dbp, record, f->reclen);
	  for (; lines; lines--)
	    write ((int) f->dbp, "\x0a", 1);
	  f->adv_before = 1;
	}
      else
	{			/* opt==2 */
	  for (; lines; lines--)
	    write ((int) f->dbp, "\x0a", 1);
	  result = write ((int) f->dbp, record, f->reclen);
	}
    }
  else if (opt == -1)
    {				/* before advancing page */
      result = write ((int) f->dbp, record, f->reclen);
      write ((int) f->dbp, "\x0a\x0c", 2);
    }
  else if (opt == -2)
    {				/* after advancing page */
      write ((int) f->dbp, "\x0a\x0c", 2);
      result = write ((int) f->dbp, record, f->reclen);
    }
  else
    {				/* only normal write (error?) */
      result = write ((int) f->dbp, record, f->reclen);
//              write( (int)f->dbp, "\x0a", 1 );
    }
  if (result == f->reclen)
    return 0;
  return result;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          sort_open                                     |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
sort_open (struct file_desc *f, char *record, char *fname)
{
  DBTYPE type = DB_BTREE;
  int sflags = S_IRUSR | S_IWUSR;
  int oflags = O_CREAT | O_RDWR;
  char *filename;
  int len;
  BTREEINFO b;

  b.flags = R_DUP;
  b.cachesize = 0;
  b.maxkeypage = 0;
  b.minkeypage = 0;
  b.psize = 0;
  b.compare = NULL;
  b.prefix = NULL;
  b.lorder = 0;


  /* beware: fname points to a field storage (non null-terminated) 
     we must copy fname to a C string and terminate it with a \0
     and also trim spaces at the end. */
  len = f->fname_desc->len;
  filename = malloc (len + 1);
  memmove (filename, fname, len);
  do
    {
      filename[len--] = 0;
    }
  while (filename[len] == ' ');

  type = DB_BTREE;
  f->dbp = db_open (NULL, oflags, sflags, type, &b);
  if (!f->dbp)
    {
      if (errno == EINVAL)
	return 37;
      return 30;
    }
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          sort_release                                  |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
sort_release (struct file_desc *f, char *record, char *sd, ...)
{
  int result;
  int flags = 0;
  int key_size = 0;
  int counter;
  int sort_direction;
  int key_ptr = 0;
  DBT key, data;
  va_list args;
  char *fld;
  char key1[f->reclen];
  char fld_new[f->reclen];
  int i = 0;

  data.data = record;
  data.size = f->reclen;

  va_start (args, sd[0]);
  sort_direction = sd[0];
  while (sd[i++])
    {
      fld = va_arg (args, char *);
      key_size = key_size + sd[i];
      move_bytes (&fld_new, fld, sd[i]);
      if (sort_direction == 2)
	{
	  /* If the key is descending then for each character we
	     subtract its value from 255 giving the final value.
	     This way we can combine the Ascending and Descending 
	     Keys into one long key. The result is the final key and
	     saves us from having to run sort. 
	   */
	  for (counter = 0; counter <= sd[i]; counter++)
	    {
	      fld_new[counter] = 255 - fld_new[counter];
	    }
	}
      move_bytes (&key1[key_ptr], fld_new, sd[i]);
      sort_direction = sd[i + 1];
      key_ptr += sd[i++];
    }
  key.data = &key1;
  key.size = key_size;
  result = f->dbp->put (f->dbp, &key, &data, flags);
  if (!result)
    return 0;
  else
    return 99;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          sort_return                                   |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
sort_return (struct file_desc *f, char *record)
{
  int result;
  DBT key, data;
  int flags = R_NEXT;

  result = f->dbp->seq (f->dbp, &key, &data, flags);
  if (result)
    return 10;
  if (data.size < f->reclen)
    return 10;
  move_bytes (record, data.data, f->reclen);
  return 0;

}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_sort_using                                |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_sort_using (struct file_desc *f1, char *fname1, ...)
{
  int result;
  int flags = 0;
  int oflags = 0;
  int sflags = S_IRUSR | S_IWUSR;
  int key_size = 0;
  int counter;
  int sort_direction;
  int key_ptr = 0;
  int fcnt = 0;
  DBT key, data;
  va_list args;
  char *filename;
  int len;
  int i;
  int cnt = 0;
  int file_num = 0;
  int key_num = 0;
  struct file_desc *temp_f;
  char *temp_fname;
  struct file_desc *sort_file;
  char *sd;
  char *record;
  struct file_desc *f[100];
  char *fname[100];
  char *fld[100];
  char key1[f1->reclen];
  char fld_new[f1->reclen];

  va_start (args, *fname1);
  f[file_num] = f1;
  fname[file_num] = fname1;
  temp_f = f1;
  while (temp_f != NULL)
    {
      temp_f = va_arg (args, struct file_desc *);
      if (temp_f == NULL)
	break;
      temp_fname = va_arg (args, char *);
      file_num++;
      f[file_num] = temp_f;
      fname[file_num] = temp_fname;
    }
  sort_file = va_arg (args, struct file_desc *);
  record = va_arg (args, char *);
  sd = va_arg (args, char *);
  for (key_num = 0; key_num < (strlen (sd) / 2); key_num++)
    {
      fld[key_num] = va_arg (args, char *);
    }
  va_end (args);
  for (cnt = 0; cnt <= file_num; cnt++)
    {
      oflags = O_RDONLY;
      /* beware: fname points to a field storage (non null-terminated) 
         we must copy fname to a C string and terminate it with a \0
         and also trim spaces at the end. */
      len = f[cnt]->fname_desc->len;
      filename = malloc (len + 1);
      memmove (filename, fname[cnt], len);
      do
	{
	  filename[len--] = 0;
	}
      while (filename[len] == ' ');

      f[cnt]->dbp = (void *) open (filename, oflags, sflags);
      free (filename);
      if (!f[cnt]->dbp)
	{
	  if (errno == EINVAL)
	    {
	      f[cnt]->dbp = NULL;
	      return 37;
	    }
	  if (errno == ENOENT)
	    {
	      f[cnt]->dbp = NULL;
	      return 35;
	    }
	  f[cnt]->dbp = NULL;
	  return 91;
	}
      result = 0;
      while (result == 0)
	{
	  i = 0;
	  key_size = 0;
	  key_ptr = 0;
	  result = read ((int) f[cnt]->dbp, record, f[cnt]->reclen);
	  if (result == 0)
	    {
	      close ((int) f[cnt]->dbp);
	      f[cnt]->dbp = NULL;
	      result = 99;
	      break;
	    }
	  if (result != f[cnt]->reclen)
	    {
	      close ((int) f[cnt]->dbp);
	      f[cnt]->dbp = NULL;
	      result = 99;
	      return 30;
	    }
	  data.data = record;
	  data.size = f[cnt]->reclen;
	  sort_direction = sd[i];
	  fcnt = 0;
	  while (sd[i++])
	    {
	      key_size = key_size + sd[i];
	      move_bytes (&fld_new, fld[fcnt], sd[i]);
	      if (sort_direction == 2)
		{
		  /* If the key is descending then for each 
		     character we subtract its value from 255
		     giving the final value. This way we can
		     combine the Ascending and Descending keys
		     into one long key. The result is the final
		     key and saves us from having to run sort.
		   */
		  for (counter = 0; counter <= sd[i]; counter++)
		    {
		      fld_new[counter] = 255 - fld_new[counter];
		    }
		}
	      move_bytes (&key1[key_ptr], fld_new, sd[i]);
	      sort_direction = sd[i + 1];
	      key_ptr += sd[i++];
	      fcnt++;
	    }
	  key.data = &key1;
	  key.size = key_size;
	  result = sort_file->dbp->put (sort_file->dbp, &key, &data, flags);
	  if (result)
	    return 99;
	}
    }
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_sort_giving                               |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_sort_giving (struct file_desc *f1, char *fname1, ...)
{
  int result;
  int oflags = 0;
  int sflags = S_IRUSR | S_IWUSR;
  DBT key, data;
  int flags = R_NEXT;
  struct file_desc *f[100];
  char *fname[100];
  int file_num = 0;
  int cnt = 0;
  char *temp_fname;
  struct file_desc *temp_f;
  char *filename;
  va_list args;
  struct file_desc *sort_file;
  char *record;
  int len;

  va_start (args, *fname1);
  f[file_num] = f1;
  fname[file_num] = fname1;
  temp_f = f1;
  while (temp_f != NULL)
    {
      temp_f = va_arg (args, struct file_desc *);
      if (temp_f == NULL)
	break;
      temp_fname = va_arg (args, char *);
      file_num++;
      f[file_num] = temp_f;
      fname[file_num] = temp_fname;
    }
  sort_file = va_arg (args, struct file_desc *);
  record = va_arg (args, char *);
  va_end (args);
  for (cnt = 0; cnt <= file_num; cnt++)
    {
      oflags = O_CREAT | O_TRUNC | O_RDWR;
      /* beware: fname points to a field storage (non null-terminated) 
         we must copy fname to a C string and terminate it with a \0
         and also trim spaces at the end. */
      len = f[cnt]->fname_desc->len;
      filename = malloc (len + 1);
      memmove (filename, fname[cnt], len);
      do
	{
	  filename[len--] = 0;
	}
      while (filename[len] == ' ');

      f[cnt]->dbp = (void *) open (filename, oflags, sflags);
      free (filename);
      if (!f[cnt]->dbp)
	{
	  if (errno == EINVAL)
	    {
	      f[cnt]->dbp = NULL;
	      return 37;
	    }
	  if (errno == ENOENT)
	    {
	      f[cnt]->dbp = NULL;
	      return 35;
	    }
	  f[cnt]->dbp = NULL;
	  return 91;
	}
      result = 0;
    }
  result = 0;
  while (result == 0)
    {
      result = sort_file->dbp->seq (sort_file->dbp, &key, &data, flags);
      if (result)
	{
	  result = 10;
	  break;
	}
      if (data.size < sort_file->reclen)
	{
	  result = 10;
	  break;
	}
      memmove (record, data.data, sort_file->reclen);
      for (cnt = 0; cnt <= file_num; cnt++)
	{
	  result = write ((int) f[cnt]->dbp, record, f[cnt]->reclen);
	  if (!result)
	    return 99;
	  else
	    result = 0;
	}
    }
  if (result != 10)
    return 99;
  for (cnt = 0; cnt <= file_num; cnt++)
    {
      close ((int) f[cnt]->dbp);
      f[cnt]->dbp = NULL;
    }
  return 0;
}

/* EOF fileio.c */
