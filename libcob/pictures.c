/* Pictures Expand
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
 |                          mc_picexpand                                  |
 |  expand a picture returning a malloc'ed string                         |
 |  from: "XAYBZC" (where X, Y, Z are picture characters and              |
 |                  A, B, C are binary counts for each character)         |
 |    to: "XXXXYYYZZZZZ"                                                  |
 |                                                                        |
\*------------------------------------------------------------------------*/

char *
mc_picexpand (struct fld_desc *f)
{
  char *pic, *result;
  int i = 0, j, k = 0;

  pic = f->pic;
  for (j = 0; pic[j]; j++)
    i += (unsigned char) pic[++j];
  result = malloc (i + 1);
  for (j = 0; pic[j]; j += 2)
    {
      for (i = 0; i < (unsigned char) pic[j + 1]; i++)
	result[k++] = pic[j];
    }
  result[k] = 0;
/*
#ifdef PICTURE_TESTING
	printf("picexpand: %s\n", result);
	free(result);
	return NULL;
#else
	return result;
#endif
*/
  return result;
}
