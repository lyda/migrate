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

#ifdef HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#endif
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif

void
cob_init_termio (void)
{
}


/*
 * DISPLAY
 */

void
cob_display (cob_field *f)
{
  if (COB_FIELD_IS_NUMERIC (f))
    {
      int i;
      int digits = COB_FIELD_DIGITS (f);
      int expt = COB_FIELD_EXPT (f);
      int size = (digits
		  + (COB_FIELD_HAVE_SIGN (f) ? 1 : 0)
		  + (expt < 0 ? 1 : 0));
      unsigned char pic[9], *p = pic;
      unsigned char data[size];
      cob_field_attr attr = {COB_TYPE_NUMERIC_EDITED, digits, expt};
      cob_field temp = {size, data, &attr};
      attr.pic = pic;
      if (COB_FIELD_HAVE_SIGN (f))
	p += sprintf (p, "+\001");
      if (expt < 0)
	sprintf (p, "9%c.%c9%c", digits + expt, 1, - expt);
      else
	sprintf (p, "9%c", digits);
      cob_move (f, &temp);
      for (i = 0; i < size; i++)
	putchar (data[i]);
    }
  else
    {
      size_t i;
      for (i = 0; i < f->size; i++)
	putchar (f->data[i]);
    }
}

void
cob_newline ()
{
  putchar ('\n');
  fflush (stdout);
}

#ifdef COB_DEBUG
void
cob_debug_print (cob_field *f)
{
  cob_display (f);
  cob_newline ();
}
#endif


/*
 * ACCEPT
 */

void
cob_accept (cob_field *f)
{
  size_t size;

#ifdef HAVE_LIBREADLINE
  if (isatty (fileno (stdin)))
    {
      char *p = readline ("");
      add_history (p);
      size = strlen (p);
      if (size > f->size)
	size = f->size;
      memcpy (f->data, p, size);
      memset (f->data + size, ' ', f->size - size);
      free (p);
    }
  else
#endif
    {
      char buff[FILENAME_MAX];
      fgets (buff, FILENAME_MAX, stdin);
      size = strlen (buff) - 1;
      if (size > f->size)
	size = f->size;
      memcpy (f->data, buff, size);
      memset (f->data + size, ' ', f->size - size);
    }

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
  char buff[FILENAME_MAX];

  for (i = 0; i < cob_argc; i++)
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

void
cob_accept_environment (cob_field *f, cob_field *env)
{
  char buff[env->size + 1];
  char *p = getenv (cob_field_to_string (env, buff));
  if (!p) p = "";
  cob_memcpy (f, p, strlen (p));
}
