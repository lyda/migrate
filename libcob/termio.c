/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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
#include <string.h>
#include <stdlib.h>
#include <time.h>

#include "move.h"
#include "termio.h"


/*
 * DISPLAY
 */

static void
display (cob_field *f, FILE *fp)
{
  if (COB_FIELD_IS_NUMERIC (f))
    {
      if (cob_current_module->flag_pretty_display)
	{
	  int i;
	  int digits = COB_FIELD_DIGITS (f);
	  int scale = COB_FIELD_SCALE (f);
	  int size = (digits
		      + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0)
		      + (scale > 0 ? 1 : 0));
	  unsigned char pic[9], *p = pic;
	  unsigned char data[size];
	  cob_field_attr attr = {COB_TYPE_NUMERIC_EDITED, digits, scale};
	  cob_field temp = {size, data, &attr};
	  attr.pic = pic;

	  if (COB_FIELD_HAVE_SIGN (f))
	    p += sprintf (p, "+\001");
	  if (scale > 0)
	    {
	      p += sprintf (p, "9%c", digits - scale);
	      p += sprintf (p, "%c%c", cob_current_module->decimal_point, 1);
	      p += sprintf (p, "9%c", scale);
	    }
	  else
	    {
	      p += sprintf (p, "9%c", digits);
	    }

	  cob_move (f, &temp);
	  for (i = 0; i < size; i++)
	    fputc (data[i], fp);
	}
      else
	{
	  int i;
	  int digits = COB_FIELD_DIGITS (f);
	  int scale = COB_FIELD_SCALE (f);
	  int size = digits + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0);
	  unsigned char data[size];
	  cob_field_attr attr = {COB_TYPE_NUMERIC_DISPLAY, digits, scale};
	  cob_field temp = {size, data, &attr};

	  if (COB_FIELD_HAVE_SIGN (f))
	    {
	      attr.flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
	      if (COB_FIELD_SIGN_LEADING (f))
		attr.flags |= COB_FLAG_SIGN_LEADING;
	    }

	  cob_move (f, &temp);
	  for (i = 0; i < size; i++)
	    fputc (data[i], fp);
	}
    }
  else
    {
      size_t i;
      for (i = 0; i < f->size; i++)
	fputc (f->data[i], fp);
    }
}

void
cob_display (cob_field *f)
{
  display (f, stdout);
}

void
cob_display_error (cob_field *f)
{
  display (f, stderr);
}

void
cob_newline (void)
{
  fputc ('\n', stdout);
  fflush (stdout);
}

void
cob_newline_error (void)
{
  fputc ('\n', stderr);
  fflush (stderr);
}

void
cob_field_print (cob_field *f)
{
  cob_display (f);
  cob_newline ();
}


/*
 * ACCEPT
 */

void
cob_accept (cob_field *f)
{
  size_t size;
  char buff[FILENAME_MAX];
  fgets (buff, FILENAME_MAX, stdin);
  size = strlen (buff) - 1;
  if (size > f->size)
    size = f->size;
  memcpy (f->data, buff, size);
  memset (f->data + size, ' ', f->size - size);
}

void
cob_accept_date (cob_field *f)
{
  char s[7];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d", tm->tm_year % 100, tm->tm_mon + 1, tm->tm_mday);
  cob_memcpy (f, s, 6);
}

void
cob_accept_day (cob_field *f)
{
  char s[6];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%03d", tm->tm_year % 100, tm->tm_yday + 1);
  cob_memcpy (f, s, 5);
}

void
cob_accept_day_of_week (cob_field *f)
{
  char s[2];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%01d", ((tm->tm_wday + 6) % 7) + 1);
  cob_memcpy (f, s, 1);
}

void
cob_accept_time (cob_field *f)
{
  char s[9];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d%02d", tm->tm_hour, tm->tm_min, tm->tm_sec, 0);
  cob_memcpy (f, s, 8);
}

void
cob_accept_command_line (cob_field *f)
{
  int i, size = 0;
  char buff[FILENAME_MAX] = "";

  for (i = 1; i < cob_argc; i++)
    {
      int len = strlen (cob_argv[i]);
      if (size + len >= FILENAME_MAX)
	/* overflow */
	break;
      memcpy (buff + size, cob_argv[i], len);
      size += len;
      buff[size++] = ' ';
    }

  cob_memcpy (f, buff, size);
}


/*
 * Environment variable
 */

static char env[FILENAME_MAX] = "";

void
cob_display_environment (cob_field *f)
{
  cob_field_to_string (f, env);
}

void
cob_accept_environment (cob_field *f)
{
  char *p = getenv (env);
  if (!p) p = "";
  cob_memcpy (f, p, strlen (p));
}


void
cob_init_termio (void)
{
}
