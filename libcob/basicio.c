/*
 * Copyright (C) 2001, 2000, 1999,  Rildo Pragana, Jim Noeth, 
 *               Andrew Cameron, David Essex.
 * Copyright (C) 1993, 1991  Rildo Pragana.
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
#include <unistd.h>
#include <time.h>
#include <termios.h>

#include "libcob.h"

#ifdef HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#endif
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif


/*
 * DISPLAY
 */

void
cob_display (struct cob_field f)
{
  if (FIELD_NUMERIC_P (f))
    {
      int i;
      int size = (f.desc->digits
		  + (f.desc->have_sign ? 1 : 0)
		  + (f.desc->decimals > 0 ? 1 : 0));
      unsigned char pic[9], *p = pic;
      unsigned char data[size];
      struct cob_field_desc desc =
	{size, '0', f.desc->digits, f.desc->decimals};
      struct cob_field temp = {&desc, data};
      desc.pic = pic;
      if (f.desc->have_sign)
	p += sprintf (p, "+\001");
      if (f.desc->decimals > 0)
	sprintf (p, "9%c.%c9%c", f.desc->digits - f.desc->decimals,
		 1, f.desc->decimals);
      else
	sprintf (p, "9%c", f.desc->digits);
      cob_move (f, temp);
      for (i = 0; i < size; i++)
	fputc (data[i], stdout);
    }
  else
    {
      int i;
      int size = FIELD_SIZE (f);
      unsigned char *data = FIELD_DATA (f);
      for (i = 0; i < size; i++)
	fputc (data[i], stdout);
    }
}

void
cob_newline (void)
{
  putc ('\n', stdout);
  fflush (stdout);
}

void
cob_debug_print (struct cob_field f)
{
  cob_display (f);
  cob_newline ();
}


/*
 * ACCEPT
 */

void
cob_accept (struct cob_field f)
{
#ifdef HAVE_LIBREADLINE
  if (isatty (fileno (stdin)))
    {
      char *p = readline ("");
      add_history (p);
      cob_mem_move (f, p, strlen (p));
      free (p);
      return;
    }
  else
#endif
    {
      char buff[BUFSIZ];
      fgets (buff, BUFSIZ, stdin);
      cob_mem_move (f, buff, strlen (buff) - 1);
    }

}

int
cob_accept_date (struct cob_field f)
{
  char s[7];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d", tm->tm_year % 100, tm->tm_mon + 1, tm->tm_mday);
  cob_mem_move (f, s, 6);
  return 0;
}

int
cob_accept_day (struct cob_field f)
{
  char s[6];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%03d", tm->tm_year % 100, tm->tm_yday + 1);
  cob_mem_move (f, s, 5);
  return 0;
}

int
cob_accept_day_of_week (struct cob_field f)
{
  char s[2];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%01d", ((tm->tm_wday + 6) % 7) + 1);
  cob_mem_move (f, s, 1);
  return 0;
}

int
cob_accept_time (struct cob_field f)
{
  char s[9];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d%02d", tm->tm_hour, tm->tm_min, tm->tm_sec, 0);
  cob_mem_move (f, s, 8);
  return 0;
}

int
cob_accept_command_line (struct cob_field f)
{
  int i, size = 0;
  char buff[BUFSIZ];

  for (i = 0; i < cob_argc; i++)
    {
      int len = strlen (cob_argv[i]);
      if (size + len >= BUFSIZ)
	/* overflow */
	return 1;
      memcpy (buff + size, cob_argv[i], len);
      size += len;
      buff[size++] = ' ';
    }

  cob_mem_move (f, buff, size);
  return 0;
}

void
cob_accept_environment (struct cob_field f, struct cob_field env)
{
  char buff[FIELD_SIZE (env) + 1];
  char *p = getenv (cob_field_to_string (env, buff));
  if (!p) p = "";
  cob_mem_move (f, p, strlen (p));
}
