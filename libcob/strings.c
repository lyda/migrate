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
#include <regex.h>

#include "_libcob.h"

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

#define match(s1,s2,size) \
  ((*(s1) == *(s2)) && ((size) == 1 || memcmp ((s1), (s2), (size)) == 0))

static void
fatal_error (const char *func)
{
  fprintf (stderr, "%s: fatal error\n", func);
  abort ();
}


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
	  fatal_error ("inspect_get_region");
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
	  fatal_error ("cob_inspect_tallying");
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
	  fatal_error ("cob_inspect_replacing");
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
	  fatal_error ("cob_inspect_converting");
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
	fatal_error ("cob_string");
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

int
cob_unstring (struct cob_field src, ...)
{
  int i, type, offset = 0, count = 0, delms = 0;
  struct cob_field ptr = {0, 0};
  int src_size, delm_size;
  unsigned char *src_data, *delm_data;
  regex_t reg;
  int reg_inited = 0, match_size = 0;
  char regexp[256] = ""; /* FIXME: to be dynamic */
  va_list ap;

  va_start (ap, src);
  src_size = FIELD_SIZE (src);
  src_data = FIELD_DATA (src);

  while ((type = va_arg (ap, int)) != UNSTRING_END)
    switch (type)
      {
      case UNSTRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = get_index (ptr) - 1;
	if (offset < -1 || offset >= src_size)
	  goto overflow;
	break;

      case UNSTRING_DELIMITED_BY:
      case UNSTRING_DELIMITED_ALL:
	{
	  int i;
	  char *p;
	  struct cob_field dlm = va_arg (ap, struct cob_field);
	  int size = FIELD_SIZE (dlm);
	  unsigned char *data = FIELD_DATA (dlm);
	  if (delms > 0)
	    strcat (regexp, "\\|");
	  strcat (regexp, "\\(");
	  /* copy deliminator with regexp quote */
	  p = regexp + strlen (regexp);
	  for (i = 0; i < size; i++)
	    {
	      int c = data[i];
	      if (c == '.' || c == '\\')
		*p++ = '\\';
	      *p++ = c;
	    }
	  *p = 0;
	  strcat (regexp, "\\)");
	  if (type == UNSTRING_DELIMITED_ALL)
	    strcat (regexp, "\\+");
	  delms++;
	  reg_inited = 0;
	  break;
	}

      case UNSTRING_INTO:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  unsigned char *start = src_data + offset;
	  regmatch_t *match;
	  if (offset >= src_size)
	    break;
	  if (delms == 0)
	    {
	      match_size = MIN (FIELD_LENGTH (f), src_size - offset);
	      cob_mem_move (f, start, match_size);
	      offset += match_size;
	    }
	  else
	    {
	      /* delimit using regexec */
	      if (!reg_inited)
		{
		  regcomp (&reg, regexp, 0);
		  match = alloca ((delms + 1) * sizeof (regmatch_t));
		  reg_inited = 1;
		}
	      if (regexec (&reg, start, delms + 1, match, 0) == 0
		  && match[0].rm_so <= src_size - offset)
		{
		  match_size = match[0].rm_so;
		  cob_mem_move (f, start, match_size);
		  offset += match[0].rm_eo;
		  for (i = 1; i <= delms; i++)
		    if (match[i].rm_so >= 0)
		      {
			delm_data = start + match[i].rm_so;
			delm_size = match[i].rm_eo - match[i].rm_so;
			break;
		      }
		}
	      else
		{
		  match_size = src_size - offset;
		  cob_mem_move (f, start, match_size);
		  offset = src_size;
		  delm_data = NULL;
		}
	    }
	  count++;
	  break;
	}

      case UNSTRING_DELIMITER:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  if (delm_data)
	    cob_mem_move (f, delm_data, delm_size);
	  else if (FIELD_TYPE (f) == '9')
	    cob_move_zero (f);
	  else
	    cob_move_space (f);
	  break;
	}

      case UNSTRING_COUNT:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  set_pointer (f, match_size);
	  break;
	}

      case UNSTRING_TALLYING:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  inspect_add_counter (f, count);
	  break;
	}

      default:
	fatal_error ("cob_unstring");
      }

  if (offset < src_size)
    goto overflow;

  cob_status = COB_STATUS_SUCCESS;
  goto end;

 overflow:
  cob_status = COB_STATUS_OVERFLOW;

 end:
  va_end (ap);
  if (reg_inited)
    regfree (&reg);
  if (ptr.data)
    set_pointer (ptr, offset + 1);
  return cob_status;
}
