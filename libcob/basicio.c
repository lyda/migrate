/* Accept/Display basic I/O functions
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

#include "_libcob.h"

#include <termios.h>

#ifdef HAVE_READLINE_READLINE_H
#include <readline/readline.h>
#endif
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#endif

char *rlbuf = NULL;
extern int decimal_comma;

#define port(dupon) ((dupon == 1) ? stdout : stderr)

void
newline (int dupon)
{
  fputc ('\n', port(dupon));
  fflush (port(dupon));
}

void
display (struct fld_desc *f, char *s, int dupon)
{
  char *buffer;
  struct fld_desc ftmp;
  char pictmp[64];
  char szSigned[3];
  int moved = 0;
  int i, len;
  char decimals;
  int decimal_char;

  if ((f->type == '9') || (f->type == 'C') || (f->type == 'B')
      || (f->type == 'U'))
    {
      len = picCompLength (f);
      if ((f->pic[0] == 'S') || (f->pic[0] == 's'))
	{
	  szSigned[0] = '-';
	  szSigned[1] = (char) 1;
	  szSigned[2] = '\0';
	}
      else
	{
	  szSigned[0] = '\0';
	}

      decimal_char = (decimal_comma) ? ',' : '.';
      buffer = malloc (len + 2);
      memmove (&ftmp, f, sizeof (ftmp));
      decimals = f->decimals;
      if (decimals > 0)
	{
	  if (f->pic[0] == 'P' || f->pic[2] == 'P')
	    {
	      sprintf (pictmp, "%s.%c9%c", szSigned, 1,
		       (unsigned char) decimals);
	    }
	  else
	    {
	      sprintf (pictmp, "%s9%c%c%c9%c", szSigned,
		       (unsigned char) (len - decimals),
		       (unsigned char) decimal_char, 1,
		       (unsigned char) decimals);
	    }
	}
      else
	{			// decimals <= 0;
	  sprintf (pictmp, "%s9%c", szSigned, (unsigned char) len);
	}
      ftmp.type = 'E';
      ftmp.pic = pictmp;
      ftmp.len = len;
      if (strlen (szSigned))
	{
	  ++ftmp.len;
	  ++len;
	}
      if (decimals)
	len++;			/* accounts for the decimal point */
      cob_move (f, s, &ftmp, buffer);
      moved++;
    }
  else
    {
      len = f->len;
      buffer = s;
    }

  for (i = 0; i < len; i++)
    putc (buffer[i], port(dupon));

  if (moved)
    free (buffer);
}

void
display_erase (int dupon)
{
  putc ('\f', port (dupon));
}

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          accept_time                                    |
 |  Accepts the current time in the form 'HHMMSScc'.                       |
 |    HH is the hour (0-23), MM is the minute, SS is the second,           |
 |    cc is hundredths of a second (currently just 00)                     |
 |                                                                         |
\*-------------------------------------------------------------------------*/

int
accept_time (char *buffer)
{
  time_t tnow;
  struct tm *timep;
  char s[9];
  time (&tnow);
  timep = localtime (&tnow);
  sprintf (s, "%02d%02d%02d%02d", timep->tm_hour, timep->tm_min,
	   timep->tm_sec, 0);
  memmove (buffer, s, 8);
  return 0;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          accept_date                                    |
 |  Accepts the current date in the form 'YYMMDD'.                         |
 |    YY is the year, MM is the month (January=1),  DD is the day.         |
 |                                                                         |
\*-------------------------------------------------------------------------*/

int
accept_date (char *buffer)
{
  time_t tnow;
  struct tm *timep;
  char s[7];
  time (&tnow);
  timep = localtime (&tnow);
  sprintf (s, "%02d%02d%02d", (timep->tm_year) % 100, (timep->tm_mon) + 1,
	   timep->tm_mday);
  memmove (buffer, s, 6);
  return 0;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          accept_day                                     |
 |  Accepts the current day of the year in the form 'YYDDD'.               |
 |    YY is the year, DDD is the day of the year.                          |
 |                                                                         |
\*-------------------------------------------------------------------------*/

int
accept_day (char *buffer)
{
  time_t tnow;
  struct tm *timep;
  char s[6];
  time (&tnow);
  timep = localtime (&tnow);
  sprintf (s, "%02d%03d", (timep->tm_year) % 100, (timep->tm_yday) + 1);
  memmove (buffer, s, 5);
  return 0;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          accept_day_of_week                             |
 |  Accepts the day of the week into a single character.                   |
 |  The reason for the odd calculation is that C's representation of       |
 |    Sunday is 0, while in COBOL it is 7.  The rest of the week is the    |
 |    same in both languages (Monday=1 ... Saturday=6).                    |
 |                                                                         |
\*-------------------------------------------------------------------------*/

int
accept_day_of_week (char *buffer)
{
  time_t tnow;
  struct tm *timep;
  char s[2];
  time (&tnow);
  timep = localtime (&tnow);
  sprintf (s, "%01d", (((timep->tm_wday) + 6) % 7) + 1);
  memmove (buffer, s, 1);
  return 0;
}

int
accept_std (char *buffer, struct fld_desc *f, int flags)
{
  struct termios attr;
  int r;
  char pic1[3] = "X\000";
  struct fld_desc f1 = { 0, 'X', 0, 0, 0, 0, 0, 0, pic1 };

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
      fgets (rlbuf, RLBUF_SIZE, stdin);
      rlbuf[strlen (rlbuf) - 1] = 0;
    }
#else
  /* we alloc the line buffer only at the first time */
  if (!rlbuf)
    rlbuf = malloc (8192);
  fgets (rlbuf, RLBUF_SIZE, stdin);
  rlbuf[strlen (rlbuf) - 1] = 0;
#endif

  r = strlen (rlbuf) ? 0 : -1;	/* it's not really "on escape", but... */
  pic1[1] = f1.len = strlen (rlbuf);

#ifdef HAVE_LIBREADLINE
  if (f1.len)
    add_history (rlbuf);
#endif

  cob_move (&f1, rlbuf, f, buffer);

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
accept_cmd_line1 (int ac, char **av, struct fld_desc *f, char *buffer)
{

  int len = 0, cmderr, cmdac, cmdmaxlen, cmdmaxnum;
  int i, j, r = 0;
  char *pt, *pt1;

  // Test actual buffer length(f->len) is less than min.
  if (f->len < 21)
    {
      fprintf (stderr,
	       "run time error: basic.c @ accept_cmd_line: unacceptable length(%d<21) in command line parms\n",
	       (int) f->len);
      return -1;
    }

  sscanf (buffer, "%04d %04d %04d %04d", &cmderr, &cmdac, &cmdmaxnum,
	  &cmdmaxlen);

  // Test actual buffer length(f->len) and expected length from copybook are equal.
  len = (cmdmaxlen * cmdmaxnum) + 20;

  if (f->len != len)
    {
      fprintf (stderr,
	       "run time error: basic.c @ accept_cmd_line: actual length=%d not equal expected length=%d, in command line parms\n",
	       (int) f->len, len);
      sprintf (buffer, "%04d %04d %04d %04d", 1, cmdac, cmdmaxnum, cmdmaxlen);
      return -1;
    }


  // Test if number if input parms exceeds copybook expected max.
  if (ac > cmdmaxnum)
    {
      fprintf (stderr,
	       "run time error: basic.c @ accept_cmd_line: command line arguments overflow, max=%d actual=%d\n",
	       ac, cmdmaxnum);
      sprintf (buffer, "%04d %04d %04d %04d", 1, cmdac, cmdmaxnum, cmdmaxlen);
      return -1;
    }


  // Process input parms 
  // Padd copybook array with spaces.
  // Truncate parms with excess length
  cmderr = 0;
  pt = buffer + 20;
  for (i = 0, pt; i < ac; i++, pt = (buffer + 20) + i * cmdmaxlen)
    {
      len = strlen (av[i]);
      if (len <= cmdmaxlen)
	{
	  sprintf (pt, "%s", av[i]);
	  pt = pt + len;
	  for (j = len; j < cmdmaxlen; j++, pt++)
	    {
	      *pt = ' ';
	    }
	}
      else
	{
	  pt1 = av[i];
	  for (j = 0; j < cmdmaxlen; j++, pt++, pt1++)
	    {
	      *pt = *pt1;
	    }
	  cmderr = 1;
	}
    }

  cmdac = ac;
  sprintf (buffer, "%04d %04d %04d %04d", cmderr, cmdac, cmdmaxnum,
	   cmdmaxlen);
  pt = buffer + 19;
  *pt = ' ';

  return r;
}

int
accept_env_var1 (struct fld_desc *f, char *buffer)
{

  int len = 0, env_err, env_name_maxlen, env_var_maxlen;
  int i, j, r = 0;
  char *pt, *pt1, *envpt;

  // Test actual buffer length(f->len) is less than min.
  if (f->len < 16)
    {
      fprintf (stderr,
	       "run time error: basicio.c @ accept_env_var: unacceptable length(%d<16) in get environment parms\n",
	       (int) f->len);
      return -1;
    }

  sscanf (buffer, "%04d %04d %04d ", &env_err, &env_name_maxlen,
	  &env_var_maxlen);

  // Test actual buffer length(f->len) and expected length from copybook are equal.
  len = env_name_maxlen + env_var_maxlen + 15;

  if (f->len != len)
    {
      fprintf (stderr,
	       "run time error: basicio.c @ accept_env_var: actual length=%d not equal expected length=%d, in get environment parms\n",
	       (int) f->len, len);
      sprintf (buffer, "%04d %04d %04d ", 100, env_name_maxlen,
	       env_var_maxlen);
      return -1;
    }

  // Process input parms 
  // Determine variable name length and allocate memory

  env_err = 0;
  pt = buffer + 15;
  for (i = 0, pt; i < env_name_maxlen; i++, pt++)
    {
      if (*pt == CHR_BLANK)
	{
	  j = i;
	  if ((envpt = malloc (j)) == NULL)
	    {
	      fprintf (stderr,
		       "run time error: basicio.c @ accept_env_var: memory allocation error, in get environment parms\n");
	      sprintf (buffer, "%04d %04d %04d ", 100, env_name_maxlen,
		       env_var_maxlen);
	      return -1;
	    }
	  i = env_name_maxlen + 1;
	}
    }

  // Copy to envpt(null terminated string)
  pt = buffer + 15;
  pt1 = envpt;
  for (i = 0, pt; i < j; i++, pt++, pt1++)
    {
      *pt1 = *pt;
    }
  *pt1 = '\0';

  // Get environment variable, if it exists
  if ((pt = getenv (envpt)) == NULL)
    {
      sprintf (buffer, "%04d %04d %04d ", 1, env_name_maxlen, env_var_maxlen);
      r = 1;
    }
  else
    {
      len = strlen (pt);
      pt1 = buffer + 15 + env_name_maxlen;
      for (i = 0; i < env_var_maxlen; i++)
	{
	  if (i < len)
	    {
	      *pt1 = *pt;
	      pt++;
	      pt1++;
	    }
	  else
	    {
	      *pt1 = CHR_BLANK;
	      pt1++;
	    }
	}
      if (len > env_var_maxlen)
	{
	  sprintf (buffer, "%04d %04d %04d ", 2, env_name_maxlen,
		   env_var_maxlen);
	  r = 2;
	}
      else
	{
	  sprintf (buffer, "%04d %04d %04d ", 0, env_name_maxlen,
		   env_var_maxlen);
	}
    }

  free (envpt);

  return r;
}

int
accept_cmd_line (int ac, char **av, struct fld_desc *f, char *buffer)
{
  int len = 0, i, j, totlen, r = 0;

  // Padd variable with blanks 
  memset (buffer, ' ', f->len);

  // Process input parms 
  j = 0;
  totlen = 0;
  for (i = 0; i < ac; i++)
    {
      len = strlen (av[i]);
      j += len;
      if (f->len >= j)
	{
	  memmove (&buffer[totlen], av[i], len);
	  totlen += len;
	  // Add blank delimiter 
	  if (f->len >= ++j)
	    buffer[++totlen] = ' ';
	}
      else
	{
	  i = ac;
	  r = 1;
	}
    }

  return r;
}

int
accept_env_var (struct fld_desc *f, char *buffer, char *ptevname)
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
