/* Screen I/O functions
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

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <ncurses.h>

#include "_libcob.h"
#include "screenio.h"

#ifndef HAVE_COLOR_SET
#define color_set(x,y) start_color ()
#endif

/* control keys for RM/Cobol compatibility (sent by Harold Norris) */
#define COBKEY_TAB 9
#define COBKEY_FORWARD 12
#define COBKEY_HOMECLR 24

static int _scrio_init_ = 0;
static int _decimal_char_ = '.';
static struct ScrFld *_Fields_ = (struct ScrFld *) 0;
static struct Colors *_colors_ = (struct Colors *) 0;
static attr_t _iDefAttr_ = 0;
static short _iDefPair_ = 0;
static char *rlbuff = NULL;

static void cob_init_screen (void);

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                     accept_curses                                       |
 |                                                                         |
 |   Similar to accept_std (basicio.c) but only called when position       |
 |   options or on exception are given.                                    |
 |                                                                         |
\*-------------------------------------------------------------------------*/
int
accept_curses (char *buffer, struct fld_desc *f, int flags)
{
  int i, r, x, y, x0, y0, endloop, first = 1;
  int tmp, len = f->len;
  int ib;			// rlbuff offset
  char pic1[3] = "X\000";
  struct fld_desc f1 = { 0, 'X', 0, 0, 0, 0, 0, 0, pic1 };

  cob_init_screen ();
  raw ();
  noecho ();
  /* we alloc the line buffer only at the first time */
  if (!rlbuff)
    rlbuff = malloc (BUFSIZ);
  /* check if UPDATE was given (copy input field to rlbuff and show it) */
  getyx (stdscr, y0, x0);
  if (flags & SCR_UPDATE)
    {
      strncpy (rlbuff, buffer, len);
      getyx (stdscr, y, x);
      for (i = 0; i < len; i++)
	addch (buffer[i]);
      move (y, x);
    }
  else
    {
      if (f->type == 'X')
	memset (rlbuff, ' ', len);
      else
	memset (rlbuff, '0', len);
    }

  /* do the accept */
  rlbuff[len] = '\0';
  ib = 0;
  endloop = 0;
  while (!endloop)
    {
      r = getch ();
      switch (r)
	{
	case KEY_BACKSPACE:
	  if (ib > 0)
	    {
	      ib--;
	      if (ib < len)
		memmove (rlbuff + ib, rlbuff + ib + 1, len - ib);
	      rlbuff[len - 1] = ' ';
	      getyx (stdscr, y, x);
	      x--;
	      move (y, x);
	      for (tmp = ib; tmp < len; tmp++)
		addch (rlbuff[tmp]);
	      move (y, x);
	    }
	  break;
	case COBKEY_FORWARD:
	  if (ib < len)
	    {
	      ib++;
	      getyx (stdscr, y, x);
	      x++;
	      move (y, x);
	    }
	  break;
	case COBKEY_TAB:
	  while (ib < len)
	    {
	      rlbuff[ib++] = ' ';
	      addch (' ');
	    }
	  r = 0x0d;		/* simulate a <Return> key */
	  endloop++;
	  break;
	case COBKEY_HOMECLR:
	  /*getyx(stdscr,y,x);
	     x -= ib; */
	  ib = 0;
	  if (f->type == 'X')
	    memset (rlbuff, ' ', len);
	  else
	    memset (rlbuff, '0', len);
	  move (y0, x0);
	  for (i = 0; i < len; i++)
	    addch (rlbuff[i]);
	  move (y0, x0);
	  break;
	default:
	  /* allow accented (portuguese, french, ...) chars as well */
	  if (r < ' ' || r > 0xff || r == 0x7f)
	    {
	      if (first)
		ib = len;
	      endloop++;
	    }
	  else
	    {
	      first = 0;
	      if (ib < len)
		{
		  rlbuff[ib++] = r;
		  addch (r);
		}
	      if ((ib >= len) && (flags & SCR_AUTO))
		{
		  endloop++;
		  r = 0x0d;	/* simulate a <Return> key */
		}
	    }
	}
    }
  if (r == 0x0d || r == 0x0a)
    {
      r = 0;
    }
  if (flags & SCR_JUST_RIGHT)
    {
      /* trim right blanks */
      ib = len - 1;
      for (ib = len - 1; (rlbuff[ib] == ' ') && (ib > 0); ib--);
      ib++;
      memmove (rlbuff + len - ib, rlbuff, ib);
      if (f->type == 'X')
	memset (rlbuff, ' ', len - ib);
      else
	memset (rlbuff, '0', len - ib);
      /* update screen with the field justified right */
      move (y0, x0);
      for (i = 0; i < len; i++)
	addch (rlbuff[i]);
    }
  pic1[1] = f1.len = strlen (rlbuff);
  cob_move_2 (&f1, rlbuff, f, buffer);
  return r;
}

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                     display_curses                                      |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
display_curses (struct fld_desc *f, char *s, int dspflags)
{
  char *buffer;
  int i;
  struct fld_desc ftmp;
  char pictmp[64];
  char szSigned[3];
  int domove = 0;
  int decimal_char;
  int x, y;
  int len = f->len;
  char *pic = f->pic;
  char decimals;

  if (dspflags & 2)
    {				/* erase? */
      getyx (stdscr, y, x);
      erase ();
      move (y, x);
    }
  if (dspflags & 4)
    {				/* erase to end of line? */
      getyx (stdscr, y, x);
      clrtoeol ();
      move (y, x);
    }
  if ((f->type == '9') || (f->type == 'C') || (f->type == 'B'))
    {
      len = picCompLength (f->pic);
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
      domove++;
    }
  else
    {
      buffer = s;
    }
  if (domove)
    {
      decimal_char = (decimal_comma) ? ',' : '.';
      buffer = malloc (len + 2);
      memmove (&ftmp, f, sizeof (ftmp));
      decimals = f->decimals;
      if (decimals > 0)
	{
	  if (pic[0] == 'P' || pic[2] == 'P')
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
	{
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
      cob_move_2 (f, s, &ftmp, buffer);
    }
  for (i = 0; i < len; i++)
    {
      addch (buffer[i]);
    }
  if ((dspflags & 1) == 0)
    {				/* no advancing? */
      addch ('\n');
    }
  refresh ();
  if (domove)
    free (buffer);
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          cob_goxy_expr                                  |
 |                                                                         |
 |   Position cursor at the screen with ncurses. Called only if DISPLAY    |
 |   or ACCEPT statements generated with position-spec is found.           |
 |   Uses double coords because allow any arbitrary expressions.           |
 |                                                                         |
\*-------------------------------------------------------------------------*/
void
cob_goxy_expr (double x, double y)
{
  cob_init_screen ();
  move ((int) y - 1, (int) x - 1);
  refresh ();
}

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          cob_accept_screen                              |
 |  Calls to this routine are generated by the compiler when an ACCEPT     |
 |  verb with an assocated screeen section name is encountered.  The       |
 |  routine takes no arguments, but assumes that the routine               |
 |  cob_scr_process has been called once for each field in the referenced  |
 |  screen. The cob_scr_process builds a linked list of ScrFld structures  |
 |  with one structure per field in the screen. This linked list is        |
 |  referenced to by the external variable _Fields_.  Upon exit from this  |
 |  routine, all memory associated with these structures is freed.         |
 |                                                                         |
 |  Input is accepted from any field that has a destination or "To"        |
 |  field associated with it. The routine accepts keyboard input and that  |
 |  input is edited to insure that it is valid for the type of field       |
 |  that was described in the screen section.                              |
 |                                                                         |
 |  Control stays in this routine until a carrage return is received,      |
 |  then, corresponding data is moved to all receiving fields, memory      |
 |  is freed, the screen mode is reset and the routine exits back to the   |
 |  calling mainline program.                                              |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
cob_accept_screen ()
{
  int i, j, k;
  int iKey;
  int iSign;
  int bAtEnd;
  char cWk;
  struct ScrFld *pFld;
  struct ScrFld *pFldWk;

  cob_init_screen ();

  pFldWk = (struct ScrFld *) 0;
  for (pFld = _Fields_; pFld; pFld = pFld->pNext)
    {
      if (pFld->fldFrom)
	/* if we have a from field */
	cob_move_2 (pFld->fldFrom, pFld->caFrom, &(pFld->fldScr), pFld->caScr);
      if (pFld->fldTo)
	{			/* cursor stop */
	  if (!pFldWk)
	    pFldWk = pFld;
	}
    }

  if (!pFldWk)
    return;

  pFld = pFldWk;
  while (pFld)
    {
      move (pFld->iLine, pFld->iCol + pFld->iScrPos);
      attron (_GetAttributes (pFld));
      color_set (_GetColor (pFld), (void *) 0);

      iKey = getch ();
      move (pFld->iLine, pFld->iCol + pFld->iScrPos);
      switch (iKey)
	{
	case KEY_DOWN:		/* move cursor to next input field */
	case '\t':
	  pFld = pFld->pNext;	/* find next input field in chain */
	  while (pFld != pFldWk)
	    {
	      if (!pFld)
		pFld = _Fields_;
	      if (pFld->fldTo)
		break;
	      pFld = pFld->pNext;
	    }
	  break;
	case KEY_UP:		/* move cursor to prev input field */
	case KEY_BTAB:
	  pFldWk = pFld;	/* find prev input field in chain */
	  pFld = pFld->pNext;
	  while (TRUE)
	    {
	      if (!pFld)
		pFld = _Fields_;
	      if (pFld->pNext == pFldWk)
		{
		  if (pFld->fldTo)
		    break;
		  else
		    pFldWk = pFld;
		}
	      if ((pFld->pNext == (struct ScrFld *) 0)
		  && (pFldWk == _Fields_))
		{
		  if (pFld->fldTo)
		    break;
		  else
		    pFldWk = pFld;
		}
	      pFld = pFld->pNext;
	    }
	  break;
	case KEY_LEFT:		/* move cursor to left */
	  if (pFld->iFldPos)
	    {
	      --pFld->iFldPos;
	      --pFld->iScrPos;
	      if ((pFld->iScrPos) && (pFld->fldScr.type == DTYPE_EDITED))
		{
		  cWk = pFld->caScr[pFld->iScrPos];
		  while (pFld->iScrPos)
		    {
		      if ((cWk != '.') && (cWk != ','))
			break;
		      --pFld->iScrPos;
		      cWk = pFld->caScr[pFld->iScrPos];
		    }
		}
	      move (pFld->iLine, pFld->iCol + pFld->iScrPos);
	    }
	  else
	    beep ();
	  break;
	case KEY_RIGHT:	/* move cursor to right */
	  if (pFld->iFldPos < (pFld->fldWk.len - 1))
	    {
	      ++pFld->iScrPos;
	      ++pFld->iFldPos;
	      if (pFld->fldScr.type == DTYPE_EDITED)
		{
		  cWk = pFld->caScr[pFld->iScrPos];
		  while (pFld->iScrPos < (pFld->fldScr.len - 1))
		    {
		      if ((cWk != '.') && (cWk != ','))
			break;
		      ++pFld->iScrPos;
		      cWk = pFld->caScr[pFld->iScrPos];
		    }
		}
	      move (pFld->iLine, pFld->iCol + pFld->iScrPos);
	      if ((pFld->iFldPos == (pFld->fldWk.len - 1))
		  && ((pFld->iAttributes & SCR_AUTO) == SCR_AUTO))
		{
		  pFld = pFld->pNext;
		  while (TRUE)
		    {
		      if (!pFld)
			pFld = _Fields_;
		      if (pFld->fldTo)
			break;
		      pFld = pFld->pNext;
		    }
		}
	    }
	  else
	    beep ();
	  break;
	case '\r':		/* we're done */
	  pFld = (struct ScrFld *) 0;
	  break;
	case KEY_BACKSPACE:
	  if (pFld->iFldPos)
	    {
	      --pFld->iFldPos;
	      --pFld->iScrPos;
	      if ((pFld->iScrPos) && (pFld->fldScr.type == DTYPE_EDITED))
		{
		  cWk = pFld->caScr[pFld->iScrPos];
		  while (pFld->iScrPos)
		    {
		      if ((cWk != '.') && (cWk != ','))
			break;
		      --pFld->iScrPos;
		      cWk = pFld->caScr[pFld->iScrPos];
		    }
		}
	      move (pFld->iLine, pFld->iCol + pFld->iScrPos);
	    }
	case KEY_DC:		/* delete character under cursor */
	  if (pFld->fldScr.type == DTYPE_ALPHANUMERIC)
	    {
	      j = pFld->fldWk.len - pFld->iFldPos - 1;
	      k = pFld->iFldPos;
	      for (i = 0; i < j; ++i)
		{
		  pFld->caWk[k + i] = pFld->caWk[k + i + 1];
		}
	      pFld->caWk[k + i] = ' ';
	    }
	  else
	    {
	      iSign = get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
	      k = pFld->iFldPos;
	      for (i = k; i > 0; --i)
		{
		  pFld->caWk[i] = pFld->caWk[i - 1];
		}
	      pFld->caWk[i] = '0';
	      put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, iSign);
	    }
	  _DisplayField (pFld);
	  break;
	case KEY_IC:		/* insert a character before cursor */
	  if (pFld->fldScr.type == DTYPE_ALPHANUMERIC)
	    {
	      j = pFld->fldWk.len - pFld->iFldPos - 1;
	      k = pFld->iFldPos;
	      for (i = j; i > 0; --i)
		{
		  pFld->caWk[k + i] = pFld->caWk[k + i - 1];
		}
	      pFld->caWk[k + i] = ' ';
	    }
	  else
	    {
	      iSign = get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
	      k = pFld->iFldPos;
	      for (i = 0; i < k; ++i)
		{
		  pFld->caWk[i] = pFld->caWk[i + 1];
		}
	      pFld->caWk[k] = '0';
	      put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, iSign);
	    }
	  _DisplayField (pFld);
	  break;
	default:		/* replace character under cursor */
	  bAtEnd = 0;
	  if ((iKey & 0400) || (iKey < ' '))
	    {			/* non data key pressed */
	      beep ();
	    }
	  else
	    {
	      if (pFld->fldScr.type == DTYPE_ALPHANUMERIC)
		{
		  pFld->caWk[pFld->iFldPos] = (char) iKey;
		  _DisplayField (pFld);
		  if (pFld->iFldPos < (pFld->fldScr.len - 1))
		    {
		      ++pFld->iFldPos;
		      ++pFld->iScrPos;
		    }
		  else
		    bAtEnd = 1;
		  move (pFld->iLine, pFld->iCol + pFld->iScrPos);
		}
	      else
		{		/* numeric only data accepted */
		  if ((iKey < '0') || (iKey > '9'))
		    {
		      if (iKey == '-')
			{
			  get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
			  put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, 1);
			  _DisplayField (pFld);
			}
		      else if (iKey == '+')
			{
			  get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
			  put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, 0);
			  _DisplayField (pFld);
			}
		      else
			beep ();
		    }
		  else
		    {
		      iSign = get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
		      pFld->caWk[pFld->iFldPos] = (char) iKey;
		      put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, iSign);
		      _DisplayField (pFld);
		      if (pFld->iFldPos < (pFld->fldWk.len - 1))
			{
			  ++pFld->iFldPos;
			  ++pFld->iScrPos;
			  cWk = pFld->caScr[pFld->iScrPos];
			  while (pFld->iScrPos < (pFld->fldScr.len - 1))
			    {
			      if ((cWk != ',') && (cWk != '.'))
				break;
			      ++pFld->iScrPos;
			      cWk = pFld->caScr[pFld->iScrPos];
			    }
			}
		      else
			bAtEnd = 1;
		    }
		}
	      if ((bAtEnd) && ((pFld->iAttributes & SCR_AUTO) == SCR_AUTO))
		{
		  pFld = pFld->pNext;
		  while (TRUE)
		    {
		      if (!pFld)
			pFld = _Fields_;
		      if (pFld->fldTo)
			break;
		      pFld = pFld->pNext;
		    }
		}
	    }
	  break;
	}
    }

  while (_Fields_)
    {
      pFld = (struct ScrFld *) &_Fields_;
      while (pFld->pNext->pNext)
	pFld = (struct ScrFld *) pFld->pNext;
      if (pFld->pNext->fldTo)
	cob_move_2 (&pFld->pNext->fldWk, pFld->pNext->caWk,
		    pFld->pNext->fldTo, pFld->pNext->caTo);
      if (pFld->pNext->caWk)
	free (pFld->pNext->caWk);
      free (pFld->pNext);
      pFld->pNext = (struct ScrFld *) 0;
    }
}



/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          cob_display_screen                             |
 |  Calls to this routine are generated by the compiler when an DISPLAY    |
 |  verb with an assocated screeen section name is encountered.  The       |
 |  routine takes no arguments, but assumes that the routine               |
 |  cob_scr_process has been called once for each field in the referenced  |
 |  screen. The cob_scr_process builds a linked list of ScrFld structures  |
 |  with one structure per field in the screen. This linked list is        |
 |  referenced to by the external variable Fields.  Upon exit from this    |
 |  routine, all memory associated with these structures is freed.         |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
cob_display_screen ()
{
  struct ScrFld *pFld;

  cob_init_screen ();

  pFld = _Fields_;
  while (pFld)
    {
      if (pFld->fldFrom)
	/* if we have a from field */
	cob_move_2 (pFld->fldFrom, pFld->caFrom, &(pFld->fldScr), pFld->caScr);
      _DisplayField (pFld);
      pFld = pFld->pNext;
    }

  while (_Fields_)
    {				/* we're done with the field structs */
      pFld = (struct ScrFld *) &_Fields_;
      while (pFld->pNext->pNext)
	pFld = (struct ScrFld *) pFld->pNext;
      if (pFld->pNext->caWk)
	free (pFld->pNext->caWk);
      free (pFld->pNext);
      pFld->pNext = (struct ScrFld *) 0;
    }
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                               cob_scr_process                           |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
cob_scr_process (int iAttr, int iLine, int iColumn,
		 int iFgColor, int iBgColor,
		 struct fld_desc *fldScr, char *caScr, void *pInfo, ...)
{
  va_list ap;
  int i;
  int bIsAlpha;
  int bIsDisplay;
  int bIsEdited;
  int bDecPoint;
  int iCharCnt;
  int iDecCnt;
  struct ScrFld *pFld;
  void *pWk;

  pFld = (struct ScrFld *) &_Fields_;
  for (i = 0; pFld->pNext; i++)
    pFld = pFld->pNext;
  pFld->pNext = malloc (sizeof (struct ScrFld));
  pFld = pFld->pNext;
  pFld->pNext = 0;

  va_start (ap, pInfo);
  pWk = pInfo;
  pFld->fldFrom = (struct fld_desc *) pWk;
  if (pWk)
    {
      pWk = va_arg (ap, void *);
      pFld->caFrom = (char *) pWk;
    }
  else
    pFld->caFrom = (char *) 0;
  pWk = va_arg (ap, void *);
  pFld->fldTo = (struct fld_desc *) pWk;
  if (pWk)
    {
      pWk = va_arg (ap, void *);
      pFld->caTo = (char *) pWk;
    }
  else
    pFld->caTo = (char *) 0;

  pFld->iAttributes = iAttr;
  pFld->iLine = iLine - 1;
  pFld->iCol = iColumn - 1;
  pFld->iFgColor = iFgColor;
  pFld->iBgColor = iBgColor;
  memcpy (&(pFld->fldScr), fldScr, sizeof (struct fld_desc));
  pFld->caScr = caScr;
  bIsAlpha = 0;
  bIsDisplay = 0;
  bIsEdited = 0;
  bDecPoint = 0;
  iCharCnt = 0;
  iDecCnt = 0;
  for (i = 0; pFld->fldScr.pic[i]; i += 2)
    {
      switch (pFld->fldScr.pic[i])
	{
	case 'A':
	case 'X':
	  ++bIsAlpha;
	  iCharCnt += (int) pFld->fldScr.pic[i + 1];
	  break;
	case '9':
	  iCharCnt += (int) pFld->fldScr.pic[i + 1];
	  if (bDecPoint)
	    iDecCnt += (int) pFld->fldScr.pic[i + 1];
	  break;
	case 'V':
	  ++bDecPoint;
	  break;
	case '.':
	case ',':
	  if (pFld->fldScr.pic[i] == _decimal_char_)
	    ++bDecPoint;
	  ++bIsEdited;
	  break;
	case '+':
	case '-':
	case 'Z':
	case '*':
	case '$':
	  ++bIsEdited;
	  iCharCnt += (int) pFld->fldScr.pic[i + 1];
	  if (bDecPoint)
	    iDecCnt += (int) pFld->fldScr.pic[i + 1];
	  break;
	default:
	  ++bIsEdited;
	}
    }
  if (bIsEdited)
    {				/* change field type to something appropriate */
      pFld->fldScr.type = DTYPE_EDITED;
    }
  else if (!bIsAlpha)
    {
      pFld->fldScr.type = DTYPE_DISPLAY;
    }
  else
    {
      pFld->fldScr.type = DTYPE_ALPHANUMERIC;
    }
  if (pFld->fldTo)
    {				/* if we have a destination field */
      pFld->fldWk.len = iCharCnt;
      if (bIsEdited)
	{
	  pFld->fldWk.type = DTYPE_DISPLAY;
	}
      else if (!bIsAlpha)
	{
	  pFld->fldWk.type = DTYPE_DISPLAY;
	}
      else
	{
	  pFld->fldWk.type = DTYPE_ALPHANUMERIC;
	}
      pFld->fldWk.decimals = (unsigned char) iDecCnt;
      pFld->fldWk.all = 0;
      pFld->fldWk.pic = pFld->caPicWk;
      if ((pFld->fldWk.type == DTYPE_DISPLAY)
	  || (pFld->fldWk.type == DTYPE_EDITED))
	{
	  pFld->caPicWk[0] = 'S';
	  pFld->caPicWk[1] = 1;
	  pFld->caPicWk[2] = '9';
	  pFld->caPicWk[3] = iCharCnt - iDecCnt;
	  if (iDecCnt)
	    {
	      pFld->caPicWk[4] = 'V';
	      pFld->caPicWk[5] = 1;
	      pFld->caPicWk[6] = '9';
	      pFld->caPicWk[7] = iDecCnt;
	      pFld->caPicWk[8] = 0;
	    }
	  else
	    {
	      pFld->caPicWk[4] = 0;
	    }
	}
      else
	{
	  pFld->caPicWk[0] = 'X';
	  pFld->caPicWk[1] = iCharCnt;
	  pFld->caPicWk[2] = 0;
	}
      pFld->caWk = (char *) malloc (iCharCnt);
      if (pFld->fldFrom)
	cob_move_2 (pFld->fldFrom, pFld->caFrom, &pFld->fldWk, pFld->caWk);
      else
	cob_move_2 (&pFld->fldScr, pFld->caScr, &pFld->fldWk, pFld->caWk);
    }
  else
    {
      pFld->caWk = (char *) 0;
    }
  pFld->iFldPos = 0;
  pFld->iScrPos = 0;
  return;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                           cob_scr_initialize                            |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
cob_scr_initialize ()
{
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                            do_putch_terminal                            |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
do_putch_terminal (char c)
{
  int x, y;
  int mx, my;

  getyx (stdscr, y, x);
  getmaxyx (stdscr, my, mx);

  if (c > 31)
    addch (c);
  else if (c == '\t')
    {
      do
	{
	  addch (' ');
	  x++;
	  if (x == mx)
	    {
	      x = 0;
	      if (y < my)
		y++;
	      move (y, x);
	      return;
	    }
	}
      while (x % 8);
    }
  else if (c == '\n')
    {
      if (y < my)
	move (y + 1, x);
    }
  else if (c == '\r')
    {
      move (y, 0);
    }
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             cob_init_screen                             |
 |                                                                         |
\*-------------------------------------------------------------------------*/

static void
cob_init_screen (void)
{
  int i, j;

  if (_scrio_init_)
    return;

  if (decimal_comma)
    _decimal_char_ = ',';
  initscr ();
  noecho ();
  cbreak ();
  keypad (stdscr, TRUE);
  scrollok (stdscr, TRUE);
  nonl ();
  start_color ();

#ifdef HAVE_ATTR_GET
  attr_get (&_iDefAttr_, &_iDefPair_, (void *) 0);
#endif

  j = COLOR_PAIRS * sizeof (struct Colors);
  _colors_ = malloc (j);
  for (i = 0; i < COLOR_PAIRS; i++)
    _colors_[i].iPairNbr = -1;
  _colors_[0].iPairNbr = 0;
  pair_content (1, &_colors_[0].iFgColor, &_colors_[0].iBgColor);
  _scrio_init_++;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             display_terminal                            |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
display_terminal (char *buf, int len)
{
  int i;
//      char *tbuf=buf;

//      while(*buf)
  for (i = 0; i < len; i++)
    do_putch_terminal (buf[i]);
  refresh ();
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             newline_terminal                            |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
newline_terminal (void)
{
  do_putch_terminal ('\n');
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                          display_erase_terminal                         |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
display_erase_terminal (void)
{
  clear ();
  refresh ();
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             do_scrio_finish                             |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
do_scrio_finish (void)
{
  if (_scrio_init_)
    {
      endwin ();
      _scrio_init_ = 0;
    }
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             outit_terminal                              |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
outit_terminal (void)
{
  write (2, "BRK", 3);
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             outeek_terminal                             |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
outeek_terminal (void)
{
  write (2, "CNT", 3);
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                           ep_debug_terminal                             |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
ep_debug_terminal (void *addr, void *low, void *high, void *ret)
{
  printf ("ADDR=%p LOW=%p HIGH=%p RET=%p\n", addr, low, high, ret);
  fflush (stdout);
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                              backspace                                  |
 |                                                                         |
\*-------------------------------------------------------------------------*/

static void
backspace (void)
{
  int y, x;
  getyx (stdscr, y, x);
  if (x)
    x--;
  move (y, x);
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                             cob_accept                                  |
 |                                                                         |
\*-------------------------------------------------------------------------*/

int
cob_accept (char *buffer, struct fld_desc *f, int echo)
{
  int y, x;
  int cnt, digcnt, i;
  int ib, it;			// buffer offsets
  /* cnt          = how many chars need be entered
     ** digcnt    = how many already entered 
   */
  int c, sign = 0;
  int decimal_flg = 0;

  getyx (stdscr, y, x);
  digcnt = 0;
  cnt = f->len;
  /*
     ** echo & 4 --> DARK,
     ** echo == 0 --> NO ECHO
   */
  if (echo & 2)
    {				/* WITH FILLER (this isn't standard, 
				   I've invented it, but...) */
      for (i = 0; i < cnt; i++)
	addch ('_');
      if (f->decimals)
	addch (_decimal_char_);
      refresh ();
    }
  if (echo & 8)
    {				/* WITH UPDATE */
      for (i = 0; i < cnt; i++)
	addch (buffer[i]);
      move (y, x);
      refresh ();
    }
  ib = 0;
  while (1)
    {
      c = getch ();
      if (c == '-' && ib == 0)
	{
	  sign = 1;
	  if (echo & 3)
	    addch ('-');
	  else if (echo & 4)
	    addch ('*');
	  continue;
	}
      if (c == KEY_BACKSPACE)
	{
	  for (it = 0; it != ib; it++)
	    {
	      backspace ();
	      if (echo & 2)
		addch ('_');
	      else if (echo & 8)
		addch (buffer[it]);
	      else if (echo)
		addch (' ');
	    }
	  if (decimal_flg)
	    {
	      if (echo & 2)
		addch (_decimal_char_);
	      else if (echo & 8)
		{
		}		/* nothing to be done */
	      else if (echo)
		addch (' ');
	    }
	  refresh ();
	  decimal_flg = 0;
	  cnt = f->len;
	  digcnt = 0;
	  ib = 0;
	  continue;
	}
      if (c == 8)
	{
	  if (ib > 0)
	    {
	      ib--;
	      if (echo)
		backspace ();
	      if (cnt == f->decimals)
		{
		  decimal_flg = 0;
		  if (echo)
		    backspace ();
		}
	      else if (digcnt == 1 && sign)
		{
		  sign = 0;
		  if (echo)
		    backspace ();
		}
	      cnt++;
	      digcnt--;
	    }
	  continue;
	}
      if (c == '\n')		/* curses return '\n' as the return key */
	goto final;
      if (((f->type == 'A') && (isalpha (c) || c == ' ')) ||
	  ((f->type == 'X' || f->type == 'G')
	   && (isprint (c) || c == ' ')) || ((f->type == '9') && isdigit (c)))
	{
	  buffer[ib++] = c;
	  if (echo & 4)
	    addch ('*');
	  else if (echo)
	    addch (c);
	  if (cnt)
	    {
	      cnt--;
	      digcnt++;
	    }
	  if (cnt == f->decimals && cnt)
	    {
	      if (echo & 4)
		addch ('*');
	      else if (echo)
		addch (_decimal_char_);
	      decimal_flg++;
	    }
	  continue;
	}
      if (c == _decimal_char_ && f->type == '9' && !decimal_flg)
	{
	  decimal_flg++;
	  cnt = f->decimals;
	  if (echo & 4)
	    addch ('*');
	  else if (echo)
	    addch (_decimal_char_);
	  continue;
	}
    }
final:if (f->type == '9')
    {
      if (!decimal_flg)
	{
	  /* align field (integer part) at the right */
	  if (f->len - cnt)
	    memmove (buffer + cnt - f->decimals, buffer, digcnt);
	  /* fill rest of field at the left with zeros */
	  if (cnt > f->decimals)
	    memset (buffer, '0', cnt - f->decimals);
	  /* fill decimal digits with zeros at the right */
	  memset (buffer + f->len - f->decimals, '0', f->decimals);
	}
      else
	{
	  while (cnt)
	    {
	      buffer[ib++] = '0';
	      cnt--;
	      digcnt++;
	    }
	  memmove (buffer + f->len - digcnt, buffer, digcnt);
	  memset (buffer, '0', f->len - digcnt);
	}
    }
  else
    memset (buffer + ib, ' ', cnt);
  if (f->type == '9')
    put_sign ((struct cob_field) {f, buffer}, sign);
  printw ("\n");
  return ib;
}


/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                            _DisplayField                                |
 |                                                                         |
\*-------------------------------------------------------------------------*/

void
_DisplayField (struct ScrFld *pFld)
{
  int i;
  attr_t iAttr;

  iAttr = _GetAttributes (pFld);
  attron (iAttr);
  color_set (_GetColor (pFld), (void *) 0);

  if (pFld->fldTo)
    cob_move_2 (&pFld->fldWk, pFld->caWk, &pFld->fldScr, pFld->caScr);
  move (pFld->iLine, pFld->iCol);
  if ((pFld->iAttributes & SCR_BLANK_LINE) == SCR_BLANK_LINE)
    clrtoeol ();
  if ((pFld->iAttributes & SCR_SECURE) == SCR_SECURE)
    {
      for (i = 0; i < pFld->fldScr.len; ++i)
	addch ('*');
    }
  else
    {
      if ((pFld->iAttributes & SCR_BLANK_WHEN_ZERO) != SCR_BLANK_WHEN_ZERO)
	{
	  addnstr (pFld->caScr, pFld->fldScr.len);
	}
      else
	{
	  move (pFld->iLine, pFld->iCol);
	  i = get_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk});
	  for (i = 0; i < pFld->fldWk.len; ++i)
	    {
	      if (pFld->caWk[i] != '0')
		break;
	    }
	  put_sign ((struct cob_field) {&pFld->fldWk, pFld->caWk}, i);
	  if (i == pFld->fldWk.len)
	    {
	      for (i = 0; i < pFld->fldScr.len; ++i)
		addch (' ');
	    }
	  else
	    addnstr (pFld->caScr, pFld->fldScr.len);
	}
    }
  attrset (_iDefAttr_);
  color_set (_iDefPair_, (void *) 0);
}

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                            _GetAttributes                               |
 |                                                                         |
\*-------------------------------------------------------------------------*/

attr_t
_GetAttributes (struct ScrFld *pFld)
{
  attr_t iAttr;

  iAttr = 0;
  if ((pFld->iAttributes & SCR_HIGHLIGHT) == SCR_HIGHLIGHT)
    iAttr |= A_BOLD;
  if ((pFld->iAttributes & SCR_LOWLIGHT) == SCR_LOWLIGHT)
    iAttr |= A_DIM;
  if ((pFld->iAttributes & SCR_UNDERLINE) == SCR_UNDERLINE)
    iAttr |= A_UNDERLINE;
  if ((pFld->iAttributes & SCR_REVERSE_VIDEO) == SCR_REVERSE_VIDEO)
    iAttr |= A_REVERSE;
  if ((pFld->iAttributes & SCR_BLINK) == SCR_BLINK)
    iAttr |= A_BLINK;
  if ((pFld->iAttributes & SCR_BELL) == SCR_BELL)
    beep ();
  if ((pFld->iAttributes & SCR_BLANK_SCREEN) == SCR_BLANK_SCREEN)
    clear ();
  return (iAttr);
}

/*-------------------------------------------------------------------------*\
 |                                                                         |
 |                            _GetColor                                    |
 |                                                                         |
\*-------------------------------------------------------------------------*/

short
_GetColor (struct ScrFld *pFld)
{
  int i;
  short int iFgColor;
  short int iBgColor;

  if ((pFld->iFgColor != 0) || (pFld->iBgColor != 0))
    {
      for (i = 0; i < COLOR_PAIRS; ++i)
	{
	  if ((_colors_[i].iFgColor == pFld->iFgColor)
	      && (_colors_[i].iBgColor == pFld->iBgColor))
	    break;
	}
      if (i == COLOR_PAIRS)
	{
	  for (i = 0; i < COLOR_PAIRS; ++i)
	    {
	      if (_colors_[i].iPairNbr == -1)
		break;
	    }
	  if (i < COLOR_PAIRS)
	    {
	      iBgColor = pFld->iBgColor % COLORS;
	      iFgColor = pFld->iFgColor % COLORS;
	      _colors_[i].iPairNbr = i;
	      _colors_[i].iFgColor = iFgColor;
	      _colors_[i].iBgColor = iBgColor;
	      init_pair (i, iFgColor, iBgColor);
	      color_set (i, (void *) 0);
	    }
	  else
	    return (_iDefPair_);
	}
      else
	{
	  return (i);
	}
    }
  return (_iDefPair_);
}


/* screenio.c */
