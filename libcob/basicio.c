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

#include "_libcob.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <termios.h>

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
  char *buffer;
  struct fld_desc ftmp;
  char pictmp[64];
  char szSigned[3];
  int i, len;

  if (FIELD_NUMERIC_P (f))
    {
      int decimals = FIELD_DECIMALS (f);
      len = picCompLength (f.desc->pic);
      if (FIELD_SIGNED_P (f))
	{
	  szSigned[0] = '+';
	  szSigned[1] = (char) 1;
	  szSigned[2] = '\0';
	}
      else
	{
	  szSigned[0] = '\0';
	}

      buffer = alloca (len + 2);
      memcpy (&ftmp, f.desc, sizeof (ftmp));
      if (decimals > 0)
	{
	  if (f.desc->pic[0] == 'P' || f.desc->pic[2] == 'P')
	    sprintf (pictmp, "%s.%c9%c", szSigned, 1, decimals);
	  else
	    sprintf (pictmp, "%s9%c%c%c9%c", szSigned,
		     len - decimals, cob_decimal_point, 1, decimals);
	}
      else
	{
	  sprintf (pictmp, "%s9%c", szSigned, len);
	}
      ftmp.type = 'E';
      ftmp.pic = pictmp;
      ftmp.len = len;
      if (strlen (szSigned))
	{
	  ++ftmp.len;
	  ++len;
	}
      if (decimals > 0)
	len++;
      cob_move (f, (struct cob_field) {&ftmp, buffer});
    }
  else
    {
      len = f.desc->len;
      buffer = f.data;
    }

  for (i = 0; i < len; i++)
    fputc (buffer[i], stdout);
}

void
cob_newline (void)
{
  fputc ('\n', stdout);
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

int
cob_accept_environment (struct fld_desc *f, char *buffer, char *ptevname)
{
  int len, r = 0;
  char *pt1;

  // Padd variable with blanks 
  memset (buffer, ' ', f->len);

  // Get environment variable, if it exists
  if ((pt1 = getenv (ptevname)) == NULL)
    r = 1;
  else
    {
      len = strlen (pt1);
      if (f->len < len)
	{
	  len = f->len;
	  r = 2;
	}
      memmove (buffer, pt1, len);
    }

  return r;
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
