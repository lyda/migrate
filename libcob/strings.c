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

#include "move.h"
#include "numeric.h"
#include "strings.h"

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

void
cob_inspect (struct cob_field var, ...)
{
  int type;
  int var_sign = cob_get_sign (var);
  size_t var_size = COB_FIELD_LENGTH (var);
  unsigned char *var_data = COB_FIELD_BASE (var);
  unsigned char *region_start = 0, *region_end = 0;
  unsigned char region_mark[var_size];
  int replacing = 0;
  va_list ap;

  va_start (ap, var);
  memset (region_mark, 0, var_size);

  while ((type = va_arg (ap, int)) != COB_INSPECT_END)
    switch (type)
      {
      case COB_INSPECT_INIT:
	{
	  /* initialize the region inspected */
	  region_start = var_data;
	  region_end   = var_data + var_size;
	  break;
	}

      case COB_INSPECT_BEFORE:
      case COB_INSPECT_AFTER:
	{
	  /* determine the region inspected */
	  struct cob_field str = va_arg (ap, struct cob_field);
	  unsigned char *p;
	  for (p = region_start; p < region_end - str.size; p++)
	    if (match (p, str.data, str.size))
	      {
		if (type == COB_INSPECT_BEFORE)
		  region_end = p;
		else
		  region_start = p + str.size;
		goto done;
	      }
	  if (type == COB_INSPECT_AFTER)
	    region_start = region_end;
	done:
	  break;
	}

      case COB_INSPECT_TALLYING:
	replacing = 0;
	break;

      case COB_INSPECT_REPLACING:
	replacing = 1;
	break;

      case COB_INSPECT_CHARACTERS:
	{
	  struct cob_field f1 = va_arg (ap, struct cob_field);
	  unsigned char *mark = &region_mark[region_start - var_data];
	  int len = region_end - region_start;
	  if (len > 0)
	    {
	      int i, n = 0;
	      for (i = 0; i < len; i++)
		if (mark[i] == 0)
		  {
		    n++;
		    if (replacing)
		      mark[i] = f1.data[0];
		    else
		      mark[i] = 1;
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
	  unsigned char *mark = &region_mark[region_start - var_data];
	  int len = region_end - region_start;
	  if (len > 0)
	    {
	      int i, n = 0;
	      for (i = 0; i < len - f2.size + 1; i++)
		{
		  /* find matching substring */
		  if (match (region_start + i, f2.data, f2.size))
		    {
		      int j;
		      /* check if it is already marked */
		      for (j = 0; j < f2.size; j++)
			if (mark[i + j])
			  break;
		      /* if not, mark and count it */
		      if (j == f2.size)
			{
			  n++;
			  if (replacing)
			    memcpy (mark + i, f1.data, f2.size);
			  else
			    memset (mark + i, 1, f2.size);
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

      case COB_INSPECT_CONVERTING:
	{
	  int i, j;
	  struct cob_field old = va_arg (ap, struct cob_field);
	  struct cob_field new = va_arg (ap, struct cob_field);
	  for (i = 0; i < region_end - region_start; i++)
	    for (j = 0; j < old.size; j++)
	      if (region_start[i] == old.data[j])
		region_start[i] = new.data[j];
	  break;
	}
      }

  /* do replacement */
  if (replacing)
    {
      int i;
      for (i = 0; i < var_size; i++)
	if (region_mark[i])
	  var_data[i] = region_mark[i];
    }

  cob_put_sign (var, var_sign);
}


/*
 * STRING
 */

void
cob_string (struct cob_field dst, ...)
{
  int i, type, offset = 0;
  struct cob_field ptr = {0}, dlm = {0}, src;
  va_list ap;

  va_start (ap, dst);

  while ((type = va_arg (ap, int)) != COB_STRING_END)
    switch (type)
      {
      case COB_STRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = cob_to_int (ptr) - 1;
	if (offset < -1 || offset >= dst.size)
	  goto overflow;
	break;

      case COB_STRING_DELIMITED_NAME:
	dlm = va_arg (ap, struct cob_field);
	break;

      case COB_STRING_DELIMITED_SIZE:
	dlm.size = 0;
	break;

      case COB_STRING_CONCATENATE:
	src = va_arg (ap, struct cob_field);
	if (dlm.size > 0)
	  {
	    int size = src.size - dlm.size + 1;
	    for (i = 0; i < size; i++)
	      if (match (src.data + i, dlm.data, dlm.size))
		{
		  src.size = i;
		  break;
		}
	  }
	if (src.size <= dst.size - offset)
	  {
	    memcpy (dst.data + offset, src.data, src.size);
	    offset += src.size;
	  }
	else
	  {
	    int size = dst.size - offset;
	    memcpy (dst.data + offset, src.data, size);
	    offset += size;
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
  if (ptr.size)
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
  int delm_size;
  unsigned char *delm_data;
  regex_t reg;
  int reg_inited = 0, match_size = 0;
  char regexp[256] = ""; /* FIXME: should be dynamic */
  va_list ap;

  va_start (ap, src);

  while ((type = va_arg (ap, int)) != COB_UNSTRING_END)
    switch (type)
      {
      case COB_UNSTRING_WITH_POINTER:
	ptr = va_arg (ap, struct cob_field);
	offset = cob_to_int (ptr) - 1;
	if (offset < -1 || offset >= src.size)
	  goto overflow;
	break;

      case COB_UNSTRING_DELIMITED_BY:
      case COB_UNSTRING_DELIMITED_ALL:
	{
	  int i;
	  char *p;
	  struct cob_field dlm = va_arg (ap, struct cob_field);
	  if (delms > 0)
	    strcat (regexp, "\\|");
	  strcat (regexp, "\\(");
	  /* copy deliminator with regexp quote */
	  p = regexp + strlen (regexp);
	  for (i = 0; i < dlm.size; i++)
	    {
	      int c = dlm.data[i];
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
	  unsigned char *start = src.data + offset;
	  regmatch_t *match;
	  if (offset >= src.size)
	    break;
	  if (delms == 0)
	    {
	      match_size = MIN (COB_FIELD_LENGTH (f), src.size - offset);
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
		  && match[0].rm_so <= src.size - offset)
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
		  match_size = src.size - offset;
		  cob_mem_move (f, start, match_size);
		  offset = src.size;
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

  if (offset < src.size)
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
