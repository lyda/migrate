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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "_libcob.h"

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

#define match(s1,s2,size) \
  ((*(s1) == *(s2)) && ((size) == 1 || memcmp ((s1), (s2), (size)) == 0))


/*
 * INSPECT
 */

static va_list
inspect_get_region (struct cob_field var, va_list ap, int *offset, int *len)
{
  int type;
  unsigned char *p;
  unsigned char *start = FIELD_DATA (var);
  unsigned char *end   = FIELD_DATA (var) + FIELD_SIZE (var);

  while ((type = va_arg (ap, int)) != INSPECT_END)
    {
      struct cob_field str = va_arg (ap, struct cob_field);
      unsigned char *s = FIELD_DATA (str);
      int size = FIELD_SIZE (str);
      switch (type)
	{
	case INSPECT_BEFORE:
	case INSPECT_AFTER:
	  for (p = start; p < end - size; p++)
	    if (match (p, s, size))
	      {
		if (type == INSPECT_BEFORE)
		  end = p;
		else
		  start = p + size;
		break;
	      }
	  break;

	default:
	  /* fatal error */
	  fputs ("INSPECT data broken!\n", stderr);
	  abort ();
	}
    }

  *offset = start - FIELD_DATA (var);
  *len = end - start;
  return ap;
}

static void
inspect_add_counter (struct cob_field f, int n)
{
  if (n > 0)
    {
      cob_push_decimal (f);
      cob_push_int (n);
      cob_add ();
      cob_set (f, 0);
    }
}

int
cob_inspect_tallying (struct cob_field var, ...)
{
  int type;
  char mark[FIELD_SIZE (var)];
  unsigned char *var_data = FIELD_DATA (var);
  va_list ap;

  va_start (ap, var);
  memset (mark, 0, FIELD_SIZE (var));

  while ((type = va_arg (ap, int)) != INSPECT_END)
    {
      int offset, len;
      switch (type)
	{
	case INSPECT_CHARACTERS:
	  {
	    struct cob_field dst = va_arg (ap, struct cob_field);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, n = 0;
		for (i = 0; i < len; i++)
		  if (mark[offset + i] == 0)
		    {
		      n++;
		      mark[offset + i] = 1;
		    }
		inspect_add_counter (dst, n);
	      }
	    break;
	  }

	case INSPECT_ALL:
	case INSPECT_LEADING:
	  {
	    struct cob_field dst = va_arg (ap, struct cob_field);
	    struct cob_field str = va_arg (ap, struct cob_field);
	    unsigned char *str_data = FIELD_DATA (str);
	    int size = FIELD_SIZE (str);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, j, n = 0;
		for (i = 0; i < len - size + 1; i++)
		  {
		    /* find matching substring */
		    if (match (var_data + offset + i, str_data, size))
		      {
			/* check if it is already marked */
			for (j = 0; j < size; j++)
			  if (mark[offset + i + j])
			    break;
			/* if not, mark and count it */
			if (j == size)
			  {
			    n++;
			    memset (&mark[offset + i], 1, size);
			    continue;
			  }
		      }
		    /* not found */
		    if (type == INSPECT_LEADING)
		      break;
		  }
		inspect_add_counter (dst, n);
	      }
	    break;
	  }

	default:
	  /* fatal error */
	  fputs ("INSPECT data broken!\n", stderr);
	  abort ();
	}
    }

  va_end (ap);
  return 0;
}

int
cob_inspect_replacing (struct cob_field var, ...)
{
  int type;
  char mark[FIELD_SIZE (var)];
  unsigned char *var_data = FIELD_DATA (var);
  va_list ap;

  va_start (ap, var);
  memset (mark, 0, FIELD_SIZE (var));

  while ((type = va_arg (ap, int)) != INSPECT_END)
    {
      int offset, len;
      switch (type)
	{
	case INSPECT_CHARACTERS:
	  {
	    struct cob_field new = va_arg (ap, struct cob_field);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i;
		for (i = 0; i < len; i++)
		  if (mark[offset + i] == 0)
		    {
		      var_data[offset + i] = FIELD_DATA (new)[0];
		      mark[offset + i] = 1;
		    }
	      }
	    break;
	  }

	case INSPECT_ALL:
	case INSPECT_LEADING:
	case INSPECT_FIRST:
	  {
	    struct cob_field old = va_arg (ap, struct cob_field);
	    struct cob_field new = va_arg (ap, struct cob_field);
	    unsigned char *old_data = FIELD_DATA (old);
	    unsigned char *new_data = FIELD_DATA (new);
	    int size = FIELD_SIZE (old);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, j;
		for (i = 0; i < len - size + 1; i++)
		  {
		    /* find matching substring */
		    if (match (var_data + offset + i, old_data, size))
		      {
			/* check if it is already marked */
			for (j = 0; j < size; j++)
			  if (mark[offset + i + j])
			    break;
			/* if not, mark and replace it */
			if (j == size)
			  {
			    memcpy (var_data + offset + i, new_data, size);
			    memset (&mark[offset + i], 1, size);
			    if (type == INSPECT_FIRST)
			      break;
			    continue;
			  }
		      }
		    /* not found */
		    if (type == INSPECT_LEADING)
		      break;
		  }
	      }
	    break;
	  }

	default:
	  /* fatal error */
	  fputs ("INSPECT data broken!\n", stderr);
	  abort ();
	}
    }

  va_end (ap);
  return 0;
}

int
cob_inspect_converting (struct cob_field var, ...)
{
  int type;
  unsigned char *var_data = FIELD_DATA (var);
  va_list ap;
  va_start (ap, var);

  while ((type = va_arg (ap, int)) != INSPECT_END)
    {
      int offset, len;
      switch (type)
	{
	case INSPECT_CONVERTING:
	  {
	    struct cob_field old = va_arg (ap, struct cob_field);
	    struct cob_field new = va_arg (ap, struct cob_field);
	    unsigned char *old_data = FIELD_DATA (old);
	    unsigned char *new_data = FIELD_DATA (new);
	    int size = FIELD_SIZE (old);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, j;
		for (i = 0; i < len; i++)
		  for (j = 0; j < size; j++)
		    if (var_data[offset + i] == old_data[j])
		      var_data[offset + i] = new_data[j];
	      }
	    break;
	  }

	default:
	  /* fatal error */
	  fputs ("INSPECT data broken!\n", stderr);
	  abort ();
	}
    }

  va_end (ap);
  return 0;
}


/*
 * STRING
 */

static void
set_pointer (struct cob_field f, int n)
{
  int saved_status = cob_status;
  cob_status = COB_STATUS_SUCCESS;
  cob_push_int (n);
  cob_set (f, 0);
  cob_status = saved_status;
}

int
cob_string (struct cob_field dst, ...)
{
  int i, type, offset = 0;
  struct cob_field ptr = {0, 0}, dlm, src;
  int dlm_size, src_size, dst_size;
  unsigned char *dlm_data, *src_data, *dst_data;
  va_list ap;

  va_start (ap, dst);
  dlm_size = 0;
  dst_size = FIELD_SIZE (dst);
  dst_data = FIELD_DATA (dst);

  while ((type = va_arg (ap, int)) != STRING_END)
    switch (type)
      {
      case STRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = get_index (ptr) - 1;
	if (offset < -1 || offset >= dst_size)
	  goto overflow;
	break;

      case STRING_DELIMITED_NAME:
	dlm = va_arg (ap, struct cob_field);
	dlm_size = FIELD_SIZE (dlm);
	dlm_data = FIELD_DATA (dlm);
	break;

      case STRING_DELIMITED_SIZE:
	dlm_size = 0;
	break;

      case STRING_CONCATENATE:
	src = va_arg (ap, struct cob_field);
	src_size = FIELD_SIZE (src);
	src_data = FIELD_DATA (src);
	if (dlm_size > 0)
	  for (i = 0; i < src_size - dlm_size + 1; i++)
	    if (match (src_data + i, dlm_data, dlm_size))
	      {
		src_size = i;
		break;
	      }
	if (src_size <= dst_size - offset)
	  {
	    memcpy (dst_data + offset, src_data, src_size);
	    offset += src_size;
	  }
	else
	  {
	    int len = dst_size - offset;
	    memcpy (dst_data + offset, src_data, len);
	    offset += len;
	    goto overflow;
	  }
	break;

      default:
	/* fatal error */
	fputs ("STRING data broken!\n", stderr);
	abort ();
      }

  cob_status = COB_STATUS_SUCCESS;
  goto end;

 overflow:
  cob_status = COB_STATUS_OVERFLOW;

 end:
  va_end (ap);
  if (ptr.data)
    set_pointer (ptr, offset + 1);
  return cob_status;
}


/*
 * UNSTRING
 */

static int
offset_substr (char *s1, char *s2, int n1, int n2)
{
  int i, j;
  for (i = 0; i < n1; i++)
    {
      for (j = 0; j < n2; j++)
	{
	  if (i + j > n1)
	    break;		/* past the first string, ignore */
	  if (s1[i + j] != s2[j])
	    break;
	}
      if (j == n2)
	break;			/* found! */
    }
  return i;
}

static void
put_integer (struct cob_field f, int v)
{
  struct fld_desc fld = { 4, 'B', 0, 0, 0, 0, 0, 0, 0, "S9\x9" };
  cob_move ((struct cob_field) {&fld, (char *) &v}, f);
}

int
cob_unstring (struct fld_desc *fvar, char *svar, ...)
{
  struct fld_desc *fptr, *ftally;
  char *sptr, *stally;
  struct fld_desc **p;
  struct fld_desc *fdelim, *fdest, *fdltr, *fcnt;
  char *sdelim, *sdest, *sdltr, *scnt;
  char *delimbuf, *s1;
  int all, n, n1, len, partlen, delimlen, delimall, nfields;
  int i, ip;		// assorted indexes
  va_list args;

  /* first receive all arguments */
  va_start (args, svar);
  if ((fptr = va_arg (args, struct fld_desc *)))
    {
      sptr = va_arg (args, char *);
    }
  if ((ftally = va_arg (args, struct fld_desc *)))
    {
      stally = va_arg (args, char *);
    }
  /* setup indirect pointer to the start of delimiters array */
  i = 0;
  len = 16;
  p = malloc (sizeof (struct fld_desc) * len);
  p[i] = va_arg (args, struct fld_desc *);
  while (p[i])
    {
      if (i + 3 >= len)
	{
	  len *= 2;
	  p = realloc (p, sizeof (struct fld_desc) * len);
	}
      p[++i] = va_arg (args, struct fld_desc *);
      p[++i] = va_arg (args, struct fld_desc *);
      p[++i] = va_arg (args, struct fld_desc *);
    }

  /* now execute the actual unstring command */
  len = fvar->len;
  i = 0;
  if (fptr)
    {
      /* if there is a pointer, skip some length at svar */
      /* get the integer value of this */
      n = get_index ((struct cob_field) {fptr, sptr}) - 1;
      if (n >= (len - i) || n < 0)
	goto error;		/* overflow at the pointer */
      i += n;
    }
  nfields = 0;
  for (fdest = va_arg (args, struct fld_desc *);
       fdest;
       fdest = va_arg (args, struct fld_desc *))
    {
      sdest = va_arg (args, char *);
      if ((fdltr = va_arg (args, struct fld_desc *)))
	{
	  sdltr = va_arg (args, char *);
	}
      if ((fcnt = va_arg (args, struct fld_desc *)))
	{
	  scnt = va_arg (args, char *);
	}
      if ((len - i) <= 0)	/* check if overflow found */
	goto error;
      /* find the nearest delimiter */
      delimall = 0;
      delimlen = len - i;
      delimbuf = NULL;
      ip = 0;
      fdelim = p[ip++];
      partlen = (!fdelim) ? (fdest->len) : (len - i);
      for (; fdelim; fdelim = p[ip++])
	{
	  sdelim = (char *) p[ip++];
	  all = (int) p[ip++];
	  n1 = offset_substr (svar + i, sdelim, partlen, fdelim->len);
	  if (n1 < partlen)
	    {
	      partlen = n1;
	      delimlen = fdelim->len;
	      delimbuf = sdelim;
	      delimall = all;
	    }
	}
      /* this should be a call to our cob_move function, 
         but it's unfinished yet */
      memmove (sdest, svar + i, min (fdest->len, partlen));
      if (fdest->len > partlen)
	memset (sdest + partlen, ' ', fdest->len - partlen);
      /* adjust for the partial string processed */
      i += partlen;
      if (delimbuf)
	{			/* adjust for delimiter too */
	  i += delimlen;
	  if (fdltr)
	    {			/* if delimiter storage requested */
	      memset (sdltr, ' ', fdltr->len);
	      memmove (sdltr, delimbuf, min (fdltr->len, delimlen));
	      n1 = fdltr->len - delimlen;
	      s1 = sdltr + delimlen;
	    }
	}
      if (fcnt)
	put_integer ((struct cob_field) {fcnt, scnt}, partlen);
      if (delimall)
	{			/* remove all copies of delimiter */
	  while ((len - i)
		 && !offset_substr (svar + i, delimbuf, len - i, delimlen))
	    {
	      i += delimlen;
	      if (n1 && fdltr)
		{
		  memmove (s1, delimbuf, min (n1, delimlen));
		  n1 -= delimlen;
		  s1 += delimlen;
		}
	    }
	  if (fdltr)
	    memset (s1, ' ', n1);
	}
      if (fcnt)
	put_integer ((struct cob_field) {fcnt, scnt}, partlen);
      nfields++;
    }
  if (ftally)
    put_integer ((struct cob_field) {ftally, stally},
		 nfields + get_index ((struct cob_field) {ftally, stally}));
  if (fptr)
    put_integer ((struct cob_field) {fptr, sptr}, i + 1);
  if (len - i)			/* another way to overflow */
    {
    error:
      free (p);
      va_end (args);
      cob_status = COB_STATUS_OVERFLOW;
      return cob_status;
    }
  free (p);
  va_end (args);
  cob_status = COB_STATUS_SUCCESS;
  return cob_status;
}
