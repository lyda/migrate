/*
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

struct ScrFld
{
  struct ScrFld *pNext;		/* pointer to next field in list */
  int iAttributes;		/* field attributs */
  int iFldPos;			/* horizontal position in field */
  int iScrPos;			/* horizontal position on screen */
  short int iLine;		/* Line number of start of field */
  short int iCol;		/* column number of start of field */
  short int iFgColor;		/* foreground color */
  short int iBgColor;		/* background color */
  struct fld_desc *fldFrom;	/* field description of source field */
  char *caFrom;			/* data area of source field */
  struct fld_desc *fldTo;	/* field description of dest. field */
  char *caTo;			/* data are of destination field */
  struct fld_desc fldScr;	/* field description of screen field */
  char *caScr;			/* data are of screen field */
  struct fld_desc fldWk;	/* work field desc for input fields */
  char *caWk;			/* work data area for input fields */
  char caPicWk[20];		/* area for picture definition */
};
struct Colors
{
  short iPairNbr;
  short iBgColor;
  short iFgColor;
};

void _DisplayField (struct ScrFld *pFld);
attr_t _GetAttributes (struct ScrFld *pFld);
short _GetColor (struct ScrFld *pFld);
