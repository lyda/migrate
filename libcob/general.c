/* Genaral functions
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

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          stop_run                                      |
 |                                                                        |
\*------------------------------------------------------------------------*/

void
stop_run ()
{
//              do_scrio_finish();
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          fldLength                                     |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
fldLength (struct fld_desc *f)
{
  int len;

  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1:
	  len = 3;
	  break;
	case 2:
	  len = 5;
	  break;
	case 4:
	  len = 10;
	  break;
	default:
	  len = 18;
	  break;
	}
      break;

    case 'U':
      if (f->len == 4)
	len = 14;
      else
	len = 30;
      break;

    default:
      len = f->len;
      break;
    }

  return len;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          picCompLength                                 |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
picCompLength (struct fld_desc *f)
{
  int len = 0, i;
  unsigned char *pic = f->pic;
  for (i = 0; pic[i]; i++)
    {
      if (pic[i] == '9' || pic[i] == 'P')
	len += pic[++i];
    }
  return len;
}
