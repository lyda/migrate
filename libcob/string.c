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
#include <sys/types.h>
#include <regex.h>

#include "libcob.h"

#define MIN(x,y) ({int _x = (x), _y = (y); (_x < _y) ? _x : _y; })
#define MAX(x,y) ({int _x = (x), _y = (y); (_x > _y) ? _x : _y; })

#define match(s1,s2,size) \
  ((*(s1) == *(s2)) && ((size) == 1 || memcmp ((s1), (s2), (size)) == 0))

static void
set_int (struct cob_field f, int n)
{
  int saved_status = cob_status;
  cob_status = COB_STATUS_SUCCESS;
  cob_set_int (f, n);
  cob_status = saved_status;
}

static void
add_int (struct cob_field f, int n)
{
  int saved_status;
  if (n == 0) return;
  saved_status = cob_status;
  cob_status = COB_STATUS_SUCCESS;
  cob_add_int (f, n, 0, 0);
  cob_status = saved_status;
}


/*
 * INSPECT
 */

static va_list
inspect_get_region (struct cob_field var, va_list ap, int *offset, int *len)
{
  int type;
  unsigned char *p;
  unsigned char *start = COB_FIELD_DATA (var);
  unsigned char *end   = COB_FIELD_DATA (var) + COB_FIELD_SIZE (var);

  while ((type = va_arg (ap, int)) != COB_INSPECT_END)
    {
      struct cob_field str = va_arg (ap, struct cob_field);
      unsigned char *s = COB_FIELD_DATA (str);
      int size = COB_FIELD_SIZE (str);
      switch (type)
	{
	case COB_INSPECT_BEFORE:
	case COB_INSPECT_AFTER:
	  for (p = start; p < end - size; p++)
	    if (match (p, s, size))
	      {
		if (type == COB_INSPECT_BEFORE)
		  end = p;
		else
		  start = p + size;
		break;
	      }
	  break;
	}
    }

  *offset = start - COB_FIELD_DATA (var);
  *len = end - start;
  return ap;
}

static void
inspect_internal (struct cob_field var, va_list ap, int replacing)
{
  int type;
  char mark[COB_FIELD_SIZE (var)];
  unsigned char *var_data = COB_FIELD_DATA (var);

  memset (mark, 0, COB_FIELD_SIZE (var));
  while ((type = va_arg (ap, int)) != COB_INSPECT_END)
    {
      int offset, len;
      switch (type)
	{
	case COB_INSPECT_CHARACTERS:
	  {
	    struct cob_field f1 = va_arg (ap, struct cob_field);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, n = 0;
		for (i = 0; i < len; i++)
		  if (mark[offset + i] == 0)
		    {
		      n++;
		      mark[offset + i] = 1;
		      if (replacing)
			var_data[offset + i] = COB_FIELD_DATA (f1)[0];
		    }
		if (!replacing)
		  add_int (f1, n);
	      }
	    break;
	  }

	case COB_INSPECT_ALL:
	case COB_INSPECT_LEADING:
	case COB_INSPECT_FIRST:
	  {
	    struct cob_field f1 = va_arg (ap, struct cob_field);
	    struct cob_field f2 = va_arg (ap, struct cob_field);
	    int size = COB_FIELD_SIZE (f2);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, j, n = 0;
		for (i = 0; i < len - size + 1; i++)
		  {
		    /* find matching substring */
		    if (match (var_data + offset + i, COB_FIELD_DATA (f2), size))
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
			    if (replacing)
			      memcpy (var_data + offset + i,
				      COB_FIELD_DATA (f1), size);
			    if (type == COB_INSPECT_FIRST)
			      break;
			    continue;
			  }
		      }
		    /* not found */
		    if (type == COB_INSPECT_LEADING)
		      break;
		  }
		if (!replacing)
		  add_int (f1, n);
	      }
	    break;
	  }
	}
    }
}

void
cob_inspect_tallying (struct cob_field var, ...)
{
  va_list ap;
  va_start (ap, var);
  inspect_internal (var, ap, 0);
  va_end (ap);
}

void
cob_inspect_replacing (struct cob_field var, ...)
{
  va_list ap;
  va_start (ap, var);
  inspect_internal (var, ap, 1);
  va_end (ap);
}

void
cob_inspect_converting (struct cob_field var, ...)
{
  int type;
  unsigned char *var_data = COB_FIELD_DATA (var);
  va_list ap;
  va_start (ap, var);

  while ((type = va_arg (ap, int)) != COB_INSPECT_END)
    {
      int offset, len;
      switch (type)
	{
	case COB_INSPECT_CONVERT:
	  {
	    struct cob_field old = va_arg (ap, struct cob_field);
	    struct cob_field new = va_arg (ap, struct cob_field);
	    ap = inspect_get_region (var, ap, &offset, &len);
	    if (len > 0)
	      {
		int i, j;
		for (i = 0; i < len; i++)
		  for (j = 0; j < COB_FIELD_SIZE (old); j++)
		    if (var_data[offset + i] == COB_FIELD_DATA (old)[j])
		      var_data[offset + i] = COB_FIELD_DATA (new)[j];
	      }
	    break;
	  }
	}
    }

  va_end (ap);
}


/*
 * STRING
 */

void
cob_string (struct cob_field dst, ...)
{
  int i, type, offset = 0;
  struct cob_field ptr = {0, 0}, dlm, src;
  int dlm_size, src_size, dst_size;
  unsigned char *dlm_data, *src_data, *dst_data;
  va_list ap;

  va_start (ap, dst);
  dlm_size = 0;
  dst_size = COB_FIELD_SIZE (dst);
  dst_data = COB_FIELD_DATA (dst);

  while ((type = va_arg (ap, int)) != COB_STRING_END)
    switch (type)
      {
      case COB_STRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = cob_to_int (ptr) - 1;
	if (offset < -1 || offset >= dst_size)
	  goto overflow;
	break;

      case COB_STRING_DELIMITED_NAME:
	dlm = va_arg (ap, struct cob_field);
	dlm_size = COB_FIELD_SIZE (dlm);
	dlm_data = COB_FIELD_DATA (dlm);
	break;

      case COB_STRING_DELIMITED_SIZE:
	dlm_size = 0;
	break;

      case COB_STRING_CONCATENATE:
	src = va_arg (ap, struct cob_field);
	src_size = COB_FIELD_SIZE (src);
	src_data = COB_FIELD_DATA (src);
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
      }

  cob_status = COB_STATUS_SUCCESS;
  goto end;

 overflow:
  cob_status = COB_STATUS_OVERFLOW;

 end:
  va_end (ap);
  if (ptr.data)
    set_int (ptr, offset + 1);
}


/*
 * UNSTRING
 */

void
cob_unstring (struct cob_field src, ...)
{
  int i, type, offset = 0, count = 0, delms = 0;
  struct cob_field ptr = {0, 0};
  int src_size, delm_size;
  unsigned char *src_data, *delm_data;
  regex_t reg;
  int reg_inited = 0, match_size = 0;
  char regexp[256] = ""; /* FIXME: should be dynamic */
  va_list ap;

  va_start (ap, src);
  src_size = COB_FIELD_SIZE (src);
  src_data = COB_FIELD_DATA (src);

  while ((type = va_arg (ap, int)) != COB_UNSTRING_END)
    switch (type)
      {
      case COB_UNSTRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = cob_to_int (ptr) - 1;
	if (offset < -1 || offset >= src_size)
	  goto overflow;
	break;

      case COB_UNSTRING_DELIMITED_BY:
      case COB_UNSTRING_DELIMITED_ALL:
	{
	  int i;
	  char *p;
	  struct cob_field dlm = va_arg (ap, struct cob_field);
	  int size = COB_FIELD_SIZE (dlm);
	  unsigned char *data = COB_FIELD_DATA (dlm);
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
	  if (type == COB_UNSTRING_DELIMITED_ALL)
	    strcat (regexp, "\\+");
	  delms++;
	  reg_inited = 0;
	  break;
	}

      case COB_UNSTRING_INTO:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  unsigned char *start = src_data + offset;
	  regmatch_t *match;
	  if (offset >= src_size)
	    break;
	  if (delms == 0)
	    {
	      match_size = MIN (COB_FIELD_LENGTH (f), src_size - offset);
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

      case COB_UNSTRING_DELIMITER:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  if (delm_data)
	    cob_mem_move (f, delm_data, delm_size);
	  else if (COB_FIELD_TYPE (f) == '9')
	    cob_move (cob_zero, f);
	  else
	    cob_move (cob_space, f);
	  break;
	}

      case COB_UNSTRING_COUNT:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  set_int (f, match_size);
	  break;
	}

      case COB_UNSTRING_TALLYING:
	{
	  struct cob_field f = va_arg (ap, struct cob_field);
	  add_int (f, count);
	  break;
	}
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
    set_int (ptr, offset + 1);
}
