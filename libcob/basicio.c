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

static char *rlbuf = NULL;

#define port(dupon) ((dupon == 1) ? stdout : stderr)


/*
 * DISPLAY
 */

int
cob_display (struct cob_field f, int dupon)
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
    putc (buffer[i], port (dupon));

  return 0;
}

int
cob_newline (int dupon)
{
  fputc ('\n', port (dupon));
  fflush (port (dupon));
  return 0;
}

int
cob_erase (int dupon)
{
  fputc ('\f', port (dupon));
  fflush (port (dupon));
  return 0;
}

void
cob_debug_print (struct cob_field f)
{
  cob_display (f, 1);
  cob_newline (1);
}


/*
 * ACCEPT
 */

int
cob_accept_std (char *buffer, struct fld_desc *f, int flags)
{
  struct termios attr;
  int r;
  char pic1[3] = "X\000";
  struct fld_desc f1 = { 0, 'X', 0, 0, 0, 0, 0, 0, 0, pic1 };

  if ((flags & SCR_NOECHO) != 0)
    {
      // Get terminal attributes
      if (tcgetattr (STDIN_FILENO, &attr) != 0)
	return (-1);

      // Turn off echo flag 
      attr.c_lflag &= ~(ECHO);

      // Set terminal attributes 
      // Discard any typed but un-read characters
      if (tcsetattr (STDIN_FILENO, TCSAFLUSH, &attr) != 0)
	return (-1);
    }

#ifdef HAVE_LIBREADLINE
  if (isatty (fileno (stdin)))
    {
      rlbuf = readline ("");
    }
  else
    {
      fgets (rlbuf, BUFSIZ, stdin);
      rlbuf[strlen (rlbuf) - 1] = 0;
    }
#else
  /* we alloc the line buffer only at the first time */
  if (!rlbuf)
    rlbuf = malloc (8192);
  fgets (rlbuf, BUFSIZ, stdin);
  rlbuf[strlen (rlbuf) - 1] = 0;
#endif

  r = strlen (rlbuf) ? 0 : -1;	/* it's not really "on escape", but... */
  pic1[1] = f1.len = strlen (rlbuf);

#ifdef HAVE_LIBREADLINE
  if (f1.len)
    add_history (rlbuf);
#endif

  cob_move_2 (&f1, rlbuf, f, buffer);

#ifdef HAVE_LIBREADLINE
  /* free the buffer only if it came from a readline call */
  if (isatty (fileno (stdin)))
    free (rlbuf);
#endif

  if ((flags & SCR_NOECHO) != 0)
    {
      // Turn on echo flag 
      attr.c_lflag |= ECHO;

      // Set terminal attributes 
      if (tcsetattr (STDIN_FILENO, TCSANOW, &attr) != 0)
	return (-1);
    }

  return r;

}

int
cob_accept_cmd_line (struct cob_field f, int argc, char **argv)
{
  int i, size = 0;
  char buff[BUFSIZ];

  for (i = 0; i < argc; i++)
    {
      int len = strlen (argv[i]);
      if (size + len >= BUFSIZ)
	/* overflow */
	return 1;
      memcpy (buff + size, argv[i], len);
      size += len;
      buff[size++] = ' ';
    }

  cob_mem_move (f, buff, size);
  return 0;
}

int
cob_accept_env_var (struct fld_desc *f, char *buffer, char *ptevname)
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
cob_accept_date (char *buffer)
{
  char s[7];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d", tm->tm_year % 100, tm->tm_mon + 1, tm->tm_mday);
  memcpy (buffer, s, 6);
  return 0;
}

int
cob_accept_day (char *buffer)
{
  char s[6];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%03d", tm->tm_year % 100, tm->tm_yday + 1);
  memcpy (buffer, s, 5);
  return 0;
}

int
cob_accept_day_of_week (char *buffer)
{
  char s[2];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%01d", ((tm->tm_wday + 6) % 7) + 1);
  memcpy (buffer, s, 1);
  return 0;
}

int
cob_accept_time (char *buffer)
{
  char s[9];
  time_t t = time (NULL);
  struct tm *tm = localtime (&t);
  sprintf (s, "%02d%02d%02d%02d", tm->tm_hour, tm->tm_min, tm->tm_sec, 0);
  memcpy (buffer, s, 8);
  return 0;
}
