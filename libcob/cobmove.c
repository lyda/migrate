/* Move Module
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define binFldSize(f) (f->len)

static unsigned long long _iIntValues_[18] = {
  100000000000000000,
  10000000000000000,
  1000000000000000,
  100000000000000,
  10000000000000,
  1000000000000,
  100000000000,
  10000000000,
  1000000000,
  100000000,
  10000000,
  1000000,
  100000,
  10000,
  1000,
  100,
  10,
  1
};

static void move_edited (struct fld_desc *pSrcFld, char *pSrcData,
			 struct fld_desc *pDstFld, char *pDstData);
static void float2all (struct fld_desc *pSrcFld, char *pSrcData,
		       struct fld_desc *pDstFld, char *pDstData);

void
cob_move_zero (struct fld_desc *f, char *s)
{
  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: *((char *) s) = 0; return;
	case 2: *((short *) s) = 0; return;
	case 4: *((long *) s) = 0; return;
	default: *((long long *) s) = 0; return;
	}

    case '9':
      memset (s, '0', f->len);
      put_sign (f, s, 0);
      return;

    default:
      {
	static struct fld_desc fld  = { 1, '9', 0, 1, 0, 0, 0, 0, "9\001"};
	cob_move (&fld, "0", f, s);
	return;
      }
    }
}

void
cob_move_space (struct fld_desc *f, char *s)
{
  static struct fld_desc fld = { 1, 'X', 0, 1, 0, 0, 0, 0, "X\001"};
  cob_move (&fld, " ", f, s);
}

static int
fldLength (struct fld_desc *f)
{
  switch (f->type)
    {
    case 'B':
      switch (f->len)
	{
	case 1: return 3;
	case 2: return 5;
	case 4: return 10;
	default: return 18;
	}
    case 'U':
      if (f->len == 4)
	return 14;
      else
	return 30;
    default:
      return f->len;
    }
}

/*--------------------------------------------------------------------------*\
 |                                                                           |
 |       void cob_move( struct fld_desc *FieldDescSrc,                       |
 |                      char            *caSrcBuffer,                        |
 |                      struct fld_desc *FieldDescDest,                      |
 |                      char            *caDestBuffer)                       |
 |                                                                           |
 |   Copy the contents of the field described by FieldDescSrc (data is       |
 |   pointed to by caSrcBuffer) to the field described by FieldDescDest      |
 |   (data will be stored in memory pointed to by caDescBuffer).             |
 |                                                                           |
 |   This routine will appropriately convert data (or format it) as it       |
 |   potentially moves from one data type to another. Truncation and         |
 |   padding will also be done in this routine if field sizes differ.        |
 |   Data types that this routine will handle are:                           |
 |                                                                           |
 |   Type  Source Type            Destination Type       Defined As          |
 |    '9'  Numeric Display	  Numeric Display	 DTYPE_DISPLAY       |
 |    'D'  Accept Display	  Accept Display	 DTYPE_ACCEP_DISPLAY |
 |    'C'  Numeric Packed Decimal Numeric Packed Decimal DTYPE_PACKED        |
 |    'A'  Alpha		  Alpha 		 DTYPE_ALPHA         |
 |    'X'  AlphaNumeric 	  AlphaNumeric  	 DTYPE_ALPHANUMERIC  |
 |    'E'  AlphaNumeric 	  Numeric Edited	 DTYPE_EDITED        |
 |    'G'  AlphaNumeric 	  AlphaNumeric  	 DTYPE_GROUP         |
 |    'B'  Numeric Bin Int	  Numeric Bin Int	 DTYPE_BININT        |
 |    'U'  Floating Point	  Floating Point	 DTYPE_FLOAT         |
 |                                                                           |
\*--------------------------------------------------------------------------*/

void
cob_move (struct fld_desc *f1desc, char *f1data,
	  struct fld_desc *f2desc, char *f2data)
{
  int i;
  int iPadLength;
  int iSrcLength, iDestLength;
  char iSrcDecimals, iDestDecimals;
  char *pSrcData, *pDstData;
  char caWrkData[20];
  char caPic[20];
  char cWork;

  struct fld_desc *pSrcFld, *pDstFld;
  struct fld_desc FldWrk;

  iSrcLength = fldLength (f1desc);
  iDestLength = fldLength (f2desc);
  iSrcDecimals = f1desc->decimals;
  iDestDecimals = f2desc->decimals;
  pSrcFld = f1desc;		/* may be changed to point to work area */
  pDstFld = f2desc;		/* may be changed to point to work area */
  pSrcData = f1data;		/* may be changed to point to work area */
  pDstData = f2data;		/* may be changed to point to work area */
  FldWrk.pic = caPic;

  switch (pSrcFld->type)
    {				/* source field type */
    case DTYPE_ALPHA:
    case DTYPE_ALPHANUMERIC:
    case DTYPE_EDITED:
    case DTYPE_GROUP:
    case DTYPE_ACCEPT_DISPLAY:
      if ((pSrcFld->all)	/* handle high/low values for */
	  && ((pDstFld->type == DTYPE_DISPLAY)
	      || (pDstFld->type == DTYPE_PACKED)))
	{
	  if ((pSrcData[0] == '\0') || (pSrcData[0] == '\xFF'))
	    {
	      cWork = pSrcFld->type;
	      pSrcFld->type = DTYPE_DISPLAY;
	      cob_move (pSrcFld, pSrcData, pDstFld, pDstData);
	      pSrcFld->type = cWork;
	      return;
	    }
	}
      switch (pDstFld->type)
	{			/* destination field type */
	case DTYPE_ALPHA:
	case DTYPE_ALPHANUMERIC:
	case DTYPE_GROUP:
	case DTYPE_ACCEPT_DISPLAY:
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Just move the source data to the destination area |
                     |  left to right, padding with spaces on the right   |
                     |  if the destination is larger than the source, or  |
                     |  truncating on the right if the destination is     |
                     |  smaller.  There is also some special logic that   |
                     |  checks to see if the 'all' attribute of the       |
                     |  source field is non zero. If it is, the source    |
                     |  contents are repeated as many times as necessary  |
                     |  to completely fill the destination area.          |
                     |                                                    |
                    \*----------------------------------------------------*/

	  if (iSrcLength >= iDestLength)
	    {
	      if (pDstFld->just_r == 0)
		{
		  memcpy (pDstData, pSrcData, iDestLength);
		}
	      else
		{
		  /* right justification */
		  iPadLength = iSrcLength - iDestLength;
		  memcpy (pDstData, pSrcData + iPadLength, iDestLength);
		}
	    }
	  else if (pSrcFld->all == 0)
	    {
	      iPadLength = iDestLength - iSrcLength;
	      if (pDstFld->just_r == 0)
		{
		  memcpy (pDstData, pSrcData, iSrcLength);
		  memset (pDstData + iSrcLength, ' ', iPadLength);
		}
	      else
		{
		  /* right justification */
		  memcpy (pDstData + iPadLength, pSrcData, iSrcLength);
		  memset (pDstData, ' ', iPadLength);	/* pad rest */
		}
	    }
	  else
	    {
	      while (iDestLength)
		{
		  i = min (iSrcLength, iDestLength);
		  memcpy (pDstData, pSrcData, i);
		  pDstData += i;
		  iDestLength -= i;
		}
	    }
	  break;
	case DTYPE_DISPLAY:
	  {
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Edit and move alphanumeric contents into a        |
                     |  numeric display destination field.  If the        |
                     |  source field contains non numeric data, a runtime |
                     |  error is generated, and the destination field     |
                     |  is cleared to all zeroes.  The source field may   |
                     |  contain leading or trailing plus (+) or minus (-) |
                     |  signs, and if the receiving field is signed, its  |
                     |  sign will be set accordingly. If no sign is       |
                     |  present in the source data, it is assumed that    |
                     |  a positive number is represented.  Spaces and     |
                     |  tabs are ignored in the conversion.               |
                     |                                                    |
                    \*----------------------------------------------------*/

	    char cSign;
	    char cDecimalPoint;
	    char cChar;
	    char *pDecPortion;

	    int iIntCount;
	    int iDecCount;
	    int iWork;
	    int iSrcPtr;
	    int iWrkLength;
	    int bInDecPortion;
	    int bIsSigned;
	    int bLeadingWhite;
	    int bTrailingWhite;

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  If the source field is scaled (has 'P's in the    |
                     |  picture clause, move the source data to a like    |
                     |  sized non scaled numeric field, then call move    |
                     |  again using this non scaled field as the source   |
                     |  and the original destination field as the         |
                     |  destination.  This isn't the most efficient way   | 
                     |  to handle this type of move, but scaled fields    |
                     |  aren't heavily used.                              |
                     |                                                    |
                    \*----------------------------------------------------*/

	    if (iDestDecimals < 0)
	      {			/* integer scaling */
		//FldWrk.len = pDstFld->len;
		FldWrk.len = fldLength (pDstFld);
		FldWrk.len += ((char) pDstFld->decimals * -1);
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = 0;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.len;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }
	    if (iDestLength < iDestDecimals)
	      {			/* fractional scaling */
		FldWrk.len = pDstFld->decimals;
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = pDstFld->decimals;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = 'V';
		FldWrk.pic[i++] = (char) 1;
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.len;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }

	    iDestDecimals = (int) pDstFld->decimals;
	    iWrkLength = iSrcLength;
	    iDecCount = 0;
	    iIntCount = 0;
	    iSrcPtr = 0;
	    bInDecPortion = 0;
	    bIsSigned = 0;
	    bLeadingWhite = 1;
	    bTrailingWhite = 0;
	    pDecPortion = (char *) 0;
	    cDecimalPoint = (decimal_comma) ? ',' : '.';
	    cSign = 0;
	    for (i = 0; i < iSrcLength; ++i)
	      {
		cChar = pSrcData[i];
		switch (cChar)
		  {
		  case '+':
		    cSign = 0;
		    break;
		  case '-':
		    cSign = 1;
		    break;
		  case '.':
		  case ',':
		    if (cChar == cDecimalPoint)
		      {
			if ((!bInDecPortion) && ((i + 1) < iSrcLength))
			  pDecPortion = &caWrkData[iSrcPtr];
			bInDecPortion = 1;
		      }
		    break;
		  case '0':
		  case '1':
		  case '2':
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':
		  case '8':
		  case '9':
		    if (!bInDecPortion)
		      iIntCount++;
		    else
		      iDecCount++;
		    caWrkData[iSrcPtr++] = cChar;
		    break;
		  case ' ':
		  case '\t':
		    break;
		  default:
		    runtime_error (RTERR_INVALID_DATA, f1desc,
				   (void *) pSrcData);
		    //memset(pDstData, '0', pDstFld->len);
		    memset (pDstData, '0', fldLength (pDstFld));
		    return;
		  }
	      }

	    i = 0;
	    iSrcLength = iSrcPtr;
	    iSrcPtr = 0;
	    if (iDestLength > iDestDecimals)
	      {
		iWork = iDestLength - iDestDecimals;
		if (iIntCount < iWork)
		  {		/* move pad first */
		    i = iWork - iIntCount;
		    memset (f2data, '0', i);
		  }
		if (iIntCount > iWork)	/* truncate */
		  iSrcPtr = iIntCount - iWork;
		memmove (&pDstData[i], &caWrkData[iSrcPtr],
			 iIntCount - iSrcPtr);
		i += iIntCount - iSrcPtr;
	      }
	    if (iDecCount > iDestDecimals)
	      iDecCount = iDestDecimals;

	    memmove (&pDstData[i], pDecPortion, iDecCount);
	    i += iDecCount;
	    memset (&pDstData[i], '0', iDestDecimals - iDecCount);

	    if (pDstFld->pic[0] == 'S')
	      put_sign (pDstFld, pDstData, (int) cSign);
	    break;
	  }
	case DTYPE_PACKED:
	  {
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Moving alphanumeric data to a packed field is a   |
                     |  two step process, the first process moves it to   |
                     |  a work area defined like the destination field,   |
                     |  except that it will be display and contain no     |
                     |  scaling.  Then the contents of this work area     |
                     |  will then be moved to the original destination.   |
                     |  This is pretty inefficient, but, it will work     | 
                     |  for a first cut.                                  |
                     |                                                    |
                    \*----------------------------------------------------*/

	    memcpy (&FldWrk, pSrcFld, sizeof (FldWrk));
	    FldWrk.pic = caPic;
	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.pic[0] = 'S';
	    FldWrk.pic[1] = 1;
	    i = 2;
	    if (iSrcDecimals < 0)
	      {
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) ((int) FldWrk.decimals * -1);
	      }
	    else
	      {
		if (FldWrk.len > FldWrk.decimals)
		  {
		    FldWrk.pic[i++] = '9';
		    FldWrk.pic[i++] = (char) (FldWrk.len - FldWrk.decimals);
		  }
		if (FldWrk.decimals)
		  {
		    FldWrk.pic[i++] = 'V';
		    FldWrk.pic[i++] = (char) 1;
		    FldWrk.pic[i++] = '9';
		    FldWrk.pic[i++] = (char) FldWrk.decimals;
		  }
	      }
	    FldWrk.pic[i] = '\0';
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
	    cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
	    break;
	  }
	case DTYPE_EDITED:
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  We are handling edited moves in their own routine | 
                     |  since the logic involved is very complicated, and |
                     |  will be used in a similar fashion regardless of   |
                     |  source data type.                                 |
                    \*----------------------------------------------------*/

	  move_edited (pSrcFld, pSrcData, pDstFld, pDstData);
	  break;

	case DTYPE_FLOAT:
	  {
		    /*----------------------------------------------------*\
		     |  DTYPE_ALPHA                                       |
		     |  DTYPE_ALPHANUMERIC                                |
		     |  DTYPE_EDITED                                      |
		     |  DTYPE_GROUP                                       |
		     |  DTYPE_ACCEPT_DISPLAY                              |
                     |   -> DTYPE_FLOAT                                   |
                     |  Convert the source to a normalized DISPLAY type.  |
                     |  Then convert the contents of the normalized       |
                     |  data area to float or double.                     |
                    \*----------------------------------------------------*/
	    float fWork;
	    double dWork;
	    int bIsNegative, i, k;
	    char caWork[32], caWork1[2];

	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    if (pSrcFld->decimals != 0)
	      {
		FldWrk.decimals += 2;
		FldWrk.len += 2;
	      }

	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    caWork[FldWrk.len] = '\0';

	    bIsNegative = extract_sign (&FldWrk, caWork);

	    // Add the decimal point
	    if (FldWrk.decimals != 0)
	      {
		caWork1[0] = '.';
		caWork1[1] = '\0';
		k = FldWrk.len - FldWrk.decimals;
		for (i = 0; i < FldWrk.len; i++)
		  {
		    if (i >= k)
		      {
			caWork1[1] = caWork[i];
			caWork[i] = caWork1[0];
			caWork1[0] = caWork1[1];
			caWork1[1] = '\0';
		      }
		  }
		caWork[i] = caWork1[0];
		i++;
		caWork[i] = '\0';
	      }

	    if (pDstFld->len == 4)
	      {
		sscanf (caWork, "%f", &fWork);
		if (bIsNegative)
		  fWork *= -1;
		*(float *) pDstData = fWork;
	      }
	    else
	      {
		sscanf (caWork, "%lf", &dWork);
		if (bIsNegative)
		  dWork *= -1;
		*(double *) pDstData = dWork;
	      }

	    break;
	  }

	case DTYPE_BININT:
	  {
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Call cobmove to move the alphanumeric source into |
                     |  a work area of type display with an implied       |
                     |  picture of destination field, then convert the    |
                     |  value in the work area to a 8 byte binary integer |
                     |  and store in the receiving field.                 |
                     |                                                    |
                    \*----------------------------------------------------*/

	    long long iWork;
	    int bIsNegative;
	    char caWork[18];

	    //FldWrk.len = pDstFld->len;
	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    bIsNegative = extract_sign (&FldWrk, caWork);
	    iWork = 0;
	    for (i = 0; i < FldWrk.len; ++i)
	      {
		iWork *= 10;
		iWork += (int) (caWork[i] - '0');
	      }
	    if (bIsNegative)
	      iWork *= -1;
	    switch (binFldSize (pDstFld))
	      {
	      case 1:
		*(char *) pDstData = iWork;
		break;
	      case 2:
		*(short int *) pDstData = iWork;
		break;
	      case 4:
		*(int *) pDstData = iWork;
		break;
	      case 8:
		*(long long int *) pDstData = iWork;
		break;
	      }
	    break;
	  }
	}
      break;


/* Source type is Display */
    case DTYPE_DISPLAY:
      switch (f2desc->type)
	{
	case DTYPE_ALPHA:
	case DTYPE_GROUP:
	case DTYPE_ALPHANUMERIC:
	case DTYPE_ACCEPT_DISPLAY:
	  {
	    int j;

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  If the source field is scaled (has 'P's in the    |
                     |  picture clause, move the source data to a like    |
                     |  sized non scaled numeric field, then call move    |
                     |  again using this non scaled field as the source   |
                     |  and the original destination field as the         |
                     |  destination.  This isn't the most efficient way   | 
                     |  to handle this type of move, but scaled fields    |
                     |  aren't heavily used.                              |
                     |                                                    |
                    \*----------------------------------------------------*/

	    if (iDestDecimals < 0)
	      {			/* integer scaling */
		FldWrk.len = fldLength (pDstFld);
		FldWrk.len += ((char) pDstFld->decimals * -1);
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = 0;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.len;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }
	    if (iDestLength < iDestDecimals)
	      {			/* fractional scaling */
		FldWrk.len = pDstFld->decimals;
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = pDstFld->decimals;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = 'V';
		FldWrk.pic[i++] = (char) 1;
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.decimals;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Else, just move the data left to right,           |
                     |  truncating or padding with spaces on the right    |
                     |  side of the destination field.                    |
                     |                                                    |
                    \*----------------------------------------------------*/

	    if (iSrcLength >= iDestLength)
	      {
		memcpy (pDstData, pSrcData, iDestLength);
	      }
	    else if (pSrcFld->all == 0)
	      {
		memcpy (pDstData, pSrcData, iSrcLength);
		iPadLength = iDestLength - iSrcLength;
		memset (pDstData + iSrcLength, ' ', iPadLength);
	      }
	    else
	      {
		j = 0;
		for (i = 0; i < iDestLength; ++i)
		  {
		    pDstData[i] = pSrcData[j++];
		    if (j == iSrcLength)
		      j = 0;
		  }
	      }
	    break;
	  }

	case DTYPE_DISPLAY:
	  {
	    int j;
	    int iSrcSign;
	    int iSrcIntDigits;
	    int iDestIntDigits;
	    char caWork[MAX_DIGITS * 2];

		    /*--------------------------------------------*\
                     |                                            |
                     |   If source field is signed, extract the   |
                     |   value of the sign (zero is positive and  |
                     |   non zero is negative).  The extract      |
                     |   routine also 'unsigns' the overpuched    |
                     |   last digit of the field, so we will      |
                     |   need to re-sign the field later.         |
                     |   if the 'all' flag is set on, then set    | 
                     |   the dest type to alphanumeric, call      |
                     |   ourselves again, then change the type    |
                     |   back to numeric.  In the case of the     |
                     |   'all' flag, scaling and decimal points   |
                     |   are ignored. The 'all' flag is used      |
                     |   primarily for zero, high and low vales.  |
                     |                                            |
                    \*--------------------------------------------*/

	    iSrcSign = (pSrcFld->pic[0] == 'S') ?
	      extract_sign (pSrcFld, pSrcData) : 0;

	    if (pSrcFld->all != 0)
	      {
		pDstFld->type = DTYPE_ALPHANUMERIC;
		j = 0;
		if (pSrcData[0] == '\0')
		  {		/* low values */
		    j = (int) pSrcData[0] | 0x100;
		    pSrcData[0] = '0';
		  }
		else if (pSrcData[0] == '\xFF')
		  {		/* high values */
		    j = pSrcData[0];
		    pSrcData[0] = '9';
		  }
		cob_move (pSrcFld, pSrcData, pDstFld, pDstData);
		pDstFld->type = DTYPE_DISPLAY;
		if (j)
		  pSrcData[0] = (char) j;
		if (pSrcFld->pic[0] == 'S')
		  put_sign (pSrcFld, pSrcData, iSrcSign);
		if (pDstFld->pic[0] == 'S')
		  put_sign (pDstFld, pDstData, 0);
		return;
	      }

		    /*--------------------------------------------*\
                     |                                            |
                     |  Move the contents of the source field to  |
                     |  our work area to create a normalized      | 
                     |  9(18)v9(18) view of the data.             |
                     |                                            |
                    \*--------------------------------------------*/


	    memset (caWork, '0', sizeof (caWork));

	    j = MAX_DIGITS + iSrcDecimals - iSrcLength;
	    iSrcIntDigits = iSrcLength;
	    if (iSrcDecimals > 0)
	      iSrcIntDigits -= iSrcDecimals;
	    if (iSrcIntDigits > 0)
	      memmove (&caWork[j], pSrcData, iSrcIntDigits);

	    if (iSrcDecimals > 0)
	      {
		j = MAX_DIGITS;
		if (iSrcDecimals > iSrcLength)
		  j += iSrcDecimals - 1;
		memmove (&caWork[j], &pSrcData[iSrcIntDigits], iSrcDecimals);
	      }
	    if (pSrcFld->pic[0] == 'S')
	      put_sign (pSrcFld, pSrcData, iSrcSign);

		    /*--------------------------------------------*\
                     |                                            |
                     |  Now move the contents of our normalized   |
                     |  data area to the receiving field.         |
                     |                                            |
                    \*--------------------------------------------*/

	    j = MAX_DIGITS + iDestDecimals - iDestLength;
	    iDestIntDigits = iDestLength;
	    if (iDestDecimals > 0)
	      iDestIntDigits -= iDestDecimals;

	    /* Zero the destination */
	    if (iSrcIntDigits + iSrcDecimals > 0)
	      memset (pDstData, '0', min (iSrcIntDigits + iSrcDecimals,
					  fldLength (pDstFld)));
	    else
	      memset (pDstData, '0', fldLength (pDstFld));

	    /* Fill in the destination integer part */
	    if (iDestIntDigits > 0)
	      memmove (pDstData, &caWork[j], iDestIntDigits);

	    if (iDestDecimals > 0)
	      {
		if (iDestDecimals > iDestLength)
		  {
		    j = MAX_DIGITS - iDestIntDigits;
		    /* Source decimal part exceeds destination width, 
		       insert those digits that
		       fit into the destination decimal part */
		    memmove (pDstData, &caWork[j], iDestLength);
		  }
		else
		  {
		    j = MAX_DIGITS;
		    /* Source decimal part fits into the destination width,
		       copy it to the destination */
		    memmove (&pDstData[iDestIntDigits],
			     &caWork[j], iDestDecimals);
		  }
	      }
	    if (pDstFld->pic[0] == 'S')
	      put_sign (pDstFld, pDstData, iSrcSign);
	    break;
	  }

	case DTYPE_PACKED:
	  {
	    int temp1;
	    int j, k;
	    int iSrcSign;
	    int iSrcIntDigits;
	    int iDestIntDigits;
	    char caWork[MAX_DIGITS * 2];
	    unsigned char cPack;

		    /*--------------------------------------------*\
                     |                                            |
                     |   If source field is signed, extract the   |
                     |   value of the sign (zero is positive and  |
                     |   non zero is negative).  The extract      |
                     |   routine also 'unsigns' the overpunched   |
                     |   last digit of the field, so we will      |
                     |   need to re-sign the field later.         |
                     |   If the source field has the 'all' flag   |
                     |   on, move to work area as type display,   |
                     |   then change work area type to alpha,     |
                     |   and call move again, change type back    |
                     |   to display, then call move using the     |
                     |   work area as source and the original     |
                     |   destination. This facilitates moving     |
                     |   of zero, high and low values.            |
                     |                                            |
                    \*--------------------------------------------*/


	    iSrcSign = (pSrcFld->pic[0] == 'S') ?
	      extract_sign (pSrcFld, pSrcData) : 0;

	    if (pSrcFld->all != 0)
	      {
		memcpy (&FldWrk, pDstFld, sizeof (FldWrk));
		FldWrk.type = DTYPE_ALPHANUMERIC;
		j = 0;
		if (pSrcData[0] == '\0')
		  {		/* low values */
		    j = (int) pSrcData[0] | 0x100;
		    pSrcData[0] = '0';
		  }
		else if (pSrcData[0] == '\xFF')
		  {		/* high values */
		    j = pSrcData[0];
		    pSrcData[0] = '9';
		  }
		FldWrk.pic[0] = '9';	// mark it as unsigned
		cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
		FldWrk.type = DTYPE_DISPLAY;
		cob_move (&FldWrk, caWork, pDstFld, pDstData);
		if (j)
		  pSrcData[0] = (char) j;
		return;
	      }

		    /*--------------------------------------------*\
                     |                                            |
                     |  Move the contents of the source field to  |
                     |  our work area to create a normalized      | 
                     |  9(18)v9(18) view of the data.             |
                     |                                            |
                    \*--------------------------------------------*/

	    memset (caWork, '0', sizeof (caWork));
	    /*
	       for(i=0; i<sizeof(caWork); i++)
	       caWork[i] = '0';
	     */

	    j = MAX_DIGITS + iSrcDecimals - iSrcLength;
	    iSrcIntDigits = iSrcLength;
	    if (iSrcDecimals > 0)
	      iSrcIntDigits -= iSrcDecimals;
	    memmove (&caWork[j], pSrcData, iSrcIntDigits);

	    if (iSrcDecimals > 0)
	      {
		j = MAX_DIGITS;
		if (iSrcDecimals > iSrcLength)
		  j += iSrcDecimals - 1;
		memmove (&caWork[j], &pSrcData[iSrcIntDigits], iSrcDecimals);
	      }
	    if (pSrcFld->pic[0] == 'S')
	      put_sign (pSrcFld, pSrcData, iSrcSign);


		    /*--------------------------------------------*\
                     |                                            |
                     |  Now move the contents of our normalized   |
                     |  data area to the receiving field, packing |
                     |  the data as we go.  Packed fields are (at |
                     |  least at this time) always considered to  |
                     |  be signed, so we will also move a         |
                     |  trailing sign to the receiving area. A    |
                     |  value of 0xC represents a positive value  |
                     |  and 0xD represents a negative value.      |
                     |                                            |
                    \*--------------------------------------------*/

	    k = 0;
	    cPack = 0;
	    j = MAX_DIGITS + iDestDecimals - iDestLength - 1;
	    /* j is pointer into work area */
	    iDestIntDigits = iDestLength;
	    if (iDestDecimals >= 0)
	      {
		iDestIntDigits -= iDestDecimals;
		j++;
	      }
	    /* zero fill destination first */
	    for (i = 0; i < (iSrcIntDigits + iSrcDecimals); ++i)
	      {
		//if(i == pDstFld->len)
		if (i == fldLength (pDstFld))
		  break;
		if (i & 1)	/* if lower (right) nibble */
		  pDstData[k++] = 0;
	      }
	    if (i & 1)
	      pDstData[k] = '\0';

	    for (i = 0; i < iDestIntDigits; i++)
	      {
		if (i & 1)
		  {		/* if lower (right) nibble */
		    cPack = cPack | ((unsigned char) caWork[j++] & 0xF);
		    pDstData[i >> 1] = cPack;
		  }
		else
		  {
		    cPack = (unsigned char) caWork[j++] << 4;
		  }
	      }
	    k = i;

	    if (iDestDecimals > 0)
	      {
		j = MAX_DIGITS;
		temp1 = min (iDestLength, iDestDecimals);
		if (iDestDecimals > iDestLength)
		  j += iDestDecimals - iDestLength - 1;
		for (i = 0; i < temp1; i++)
		  {
		    if (k & 1)
		      {
			cPack |= ((unsigned char) caWork[j++] & 0xF);
			pDstData[k >> 1] = cPack;
		      }
		    else
		      {
			cPack = (unsigned char) caWork[j++] << 4;
		      }
		    k++;
		  }
	      }
	    if (k & 1)
	      {
		cPack |= ((unsigned char) caWork[j++] & 0xF);
		pDstData[k >> 1] = cPack;
	      }
	    put_sign (pDstFld, pDstData, iSrcSign);
	    break;
	  }

	case DTYPE_EDITED:
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  We are handling edited moves in their own routine | 
                     |  since the logic involved is very complicated, and |
                     |  will be used in a similar fashion regardless of   |
                     |  source data type.                                 |
                    \*----------------------------------------------------*/

	  move_edited (pSrcFld, pSrcData, pDstFld, pDstData);
	  break;

	case DTYPE_FLOAT:
	  {
		    /*----------------------------------------------------*\
                     |  DTYPE_DISPLAY -> DTYPE_FLOAT                      |
                     |  Convert the source to a normalized DISPLAY type.  |
                     |  Then convert the contents of the normalized       |
                     |  data area to float or double.                     |
                    \*----------------------------------------------------*/
	    float fWork;
	    double dWork;
	    int bIsNegative, i, k;
	    char caWork[32], caWork1[2];

	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    if (pDstFld->decimals != 0)
	      {
		FldWrk.decimals += 2;
		FldWrk.len += 2;
	      }

	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    caWork[FldWrk.len] = '\0';

	    bIsNegative = extract_sign (&FldWrk, caWork);

	    // Add the decimal point
	    if (FldWrk.decimals != 0)
	      {
		caWork1[0] = '.';
		caWork1[1] = '\0';
		k = FldWrk.len - FldWrk.decimals;
		for (i = 0; i < FldWrk.len; i++)
		  {
		    if (i >= k)
		      {
			caWork1[1] = caWork[i];
			caWork[i] = caWork1[0];
			caWork1[0] = caWork1[1];
			caWork1[1] = '\0';
		      }
		  }
		caWork[i] = caWork1[0];
		i++;
		caWork[i] = '\0';
	      }

	    if (pDstFld->len == 4)
	      {
		sscanf (caWork, "%f", &fWork);
		if (bIsNegative)
		  fWork *= -1;
		*(float *) pDstData = fWork;
	      }
	    else
	      {
		sscanf (caWork, "%lf", &dWork);
		if (bIsNegative)
		  dWork *= -1;
		*(double *) pDstData = dWork;
	      }

	    break;
	  }

	case DTYPE_BININT:
	  {
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Just scan through the integer portion of the      |
                     |  source field and build the resulting 4 byte       |
                     |  binary integer (which will be move to the         |
                     |  receiving field.  The picture of the receiving    |
                     |  field is ignored.                                 |
                     |                                                    |
                    \*----------------------------------------------------*/

	    long long iWork;
	    int bIsNegative;
	    char caWork[18];

	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    bIsNegative = extract_sign (&FldWrk, caWork);
	    iWork = 0;
	    for (i = 0; i < FldWrk.len; ++i)
	      {
		iWork *= 10;
		iWork += (int) (caWork[i] - '0');
	      }
	    if (bIsNegative)
	      iWork *= -1;
	    switch (binFldSize (pDstFld))
	      {
	      case 1:
		*(char *) pDstData = iWork;
		break;
	      case 2:
		*(short int *) pDstData = iWork;
		break;
	      case 4:
		*(int *) pDstData = iWork;
		break;
	      case 8:
		*(long long int *) pDstData = iWork;
		break;
	      }
	  }
	}
      break;

/* Source type is Packed */
    case DTYPE_PACKED:
      switch (f2desc->type)
	{			/* destination field type */
	case DTYPE_ALPHA:
	case DTYPE_GROUP:
	case DTYPE_ACCEPT_DISPLAY:
	case DTYPE_ALPHANUMERIC:
	  {
	    int j;
	    unsigned char cWork;

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  If the source field is scaled (has 'P's in the    |
                     |  picture clause, move the source data to a like    |
                     |  sized non scaled numeric field, then call move    |
                     |  again using this non scaled field as the source   |
                     |  and the original destination field as the         |
                     |  destination.  This isn't the most efficient way   | 
                     |  to handle this type of move, but scaled fields    |
                     |  aren't heavily used.                              |
                     |                                                    |
                    \*----------------------------------------------------*/

	    if (iDestDecimals < 0)
	      {			/* integer scaling */
		FldWrk.len = fldLength (pDstFld);
		FldWrk.len += ((char) pDstFld->decimals * -1);
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = 0;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.len;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }
	    if (iDestLength < iDestDecimals)
	      {			/* fractional scaling */
		FldWrk.len = pDstFld->decimals;
		FldWrk.type = DTYPE_DISPLAY;
		FldWrk.decimals = pDstFld->decimals;
		FldWrk.all = 0;
		i = 0;
		if (pDstFld->pic[0] == 'S')
		  {
		    FldWrk.pic[0] = 'S';
		    FldWrk.pic[1] = 1;
		    i = 2;
		  }
		FldWrk.pic[i++] = 'V';
		FldWrk.pic[i++] = (char) 1;
		FldWrk.pic[i++] = '9';
		FldWrk.pic[i++] = (char) FldWrk.decimals;
		FldWrk.pic[i] = '\0';
		cob_move (pSrcFld, pSrcData, &FldWrk, caWrkData);
		cob_move (&FldWrk, caWrkData, pDstFld, pDstData);
		return;
	      }

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Else, just move the data left to right,           |
                     |  truncating or padding with spaces on the right    |
                     |  side of the destination field.                    |
                     |                                                    |
                    \*----------------------------------------------------*/

	    j = min (iSrcLength, iDestLength);
	    for (i = 0; i < j; i++)
	      {
		cWork = (unsigned char) pSrcData[i >> 1];
		cWork = (i & 1) ? (cWork & 0x0f) : (cWork >> 4);
		pDstData[i] = cWork + '0';
	      }
	    memset (&pDstData[j], ' ', iDestLength - j);
	    break;
	  }

	case DTYPE_DISPLAY:
	  {
	    int j;
	    int iSrcSign;
	    int iSrcIntDigits;
	    int iDestIntDigits;
	    char caWork[MAX_DIGITS * 2];
	    unsigned char cWork;

		    /*--------------------------------------------------*\
                     |                                                  |
                     |   Currently, packed fields are assumed to be     |
                     |   signed, so extract the value of the sign from  |
                     |   the source field (zero represents positive,    | 
                     |   and non zero represents negative).             |
                     |                                                  |
                    \*--------------------------------------------------*/

	    iSrcSign = extract_sign (pSrcFld, pSrcData);

		    /*--------------------------------------------*\
                     |                                            |
                     |  Move the contents of the source field to  |
                     |  our work area to create a normalized      | 
                     |  9(18)v9(18) view of the data.             |
                     |                                            |
                    \*--------------------------------------------*/

	    memset (caWork, '0', sizeof (caWork));

	    j = MAX_DIGITS + iSrcDecimals - iSrcLength;
	    iSrcIntDigits = iSrcLength;
	    if (iSrcDecimals > 0)
	      iSrcIntDigits -= iSrcDecimals;
	    for (i = 0; i < iSrcIntDigits; i++)
	      {
		cWork = (unsigned char) pSrcData[i >> 1];
		cWork = (i & 1) ? (cWork & 0xF) : (cWork >> 4);
		caWork[j++] = cWork + '0';
	      }
	    if (iSrcDecimals > 0)
	      {
		j = MAX_DIGITS;
		if (iSrcDecimals > iSrcLength)
		  j += iSrcDecimals - iSrcLength;
		for (; i < iSrcLength; i++)
		  {
		    cWork = (unsigned char) pSrcData[i >> 1];
		    cWork = (i & 1) ? (cWork & 0xF) : (cWork >> 4);
		    caWork[j++] = cWork + '0';
		  }
	      }

		    /*--------------------------------------------*\
                     |                                            |
                     |  Now move the contents of our normalized   |
                     |  data area to the receiving field.         |
                     |                                            |
                    \*--------------------------------------------*/

	    j = MAX_DIGITS + iDestDecimals - iDestLength;
	    iDestIntDigits = iDestLength;
	    if (iDestDecimals > 0)
	      iDestIntDigits -= iDestDecimals;

	    memset (pDstData, '0', min (iSrcLength, fldLength (pDstFld)));
	    memmove (pDstData, &caWork[j], iDestIntDigits);

	    if (iDestDecimals > 0)
	      {
		if (iDestDecimals > iDestLength)
		  {
		    j = MAX_DIGITS - iDestIntDigits;
		    memmove (pDstData, &caWork[j], iDestLength);
		  }
		else
		  {
		    j = MAX_DIGITS;
		    memmove (&pDstData[iDestIntDigits],
			     &caWork[j], iDestDecimals);
		  }
	      }
	    if (pDstFld->pic[0] == 'S')
	      put_sign (pDstFld, pDstData, iSrcSign);
	    break;
	  }

	case DTYPE_PACKED:
	  {
	    int temp1;
	    int j, k;
	    int iSrcSign;
	    int iSrcIntDigits;
	    int iDestIntDigits;
	    char caWork[MAX_DIGITS * 2];
	    unsigned char cPack;

		    /*--------------------------------------------------*\
                     |                                                  |
                     |   Currently, packed fields are assumed to be     |
                     |   signed, so extract the value of the sign from  |
                     |   the source field (zero represents positive,    | 
                     |   and non zero represents negative).             |
                     |                                                  |
                    \*--------------------------------------------------*/

	    iSrcSign = extract_sign (pSrcFld, pSrcData);

		    /*--------------------------------------------*\
                     |                                            |
                     |  Move the contents of the source field to  |
                     |  our work area to create a normalized      | 
                     |  9(18)v9(18) view of the data.             |
                     |                                            |
                    \*--------------------------------------------*/

	    memset (caWork, '0', sizeof (caWork));

	    j = MAX_DIGITS + iSrcDecimals - iSrcLength;
	    iSrcIntDigits = iSrcLength;
	    if (iSrcDecimals >= 0)
	      iSrcIntDigits -= iSrcDecimals;
	    for (i = 0; i < iSrcIntDigits; ++i)
	      {
		cPack = (unsigned char) pSrcData[i >> 1];
		cPack = ((i & 1) ? (cPack & 0xF) : (cPack >> 4)) + '0';
		caWork[j++] = cPack;
	      }
	    if (iSrcDecimals > 0)
	      {
		j = MAX_DIGITS;
		if (iSrcDecimals > iSrcLength)
		  j += iSrcDecimals - iSrcLength;
		for (; i < iSrcLength; ++i)
		  {
		    cPack = (unsigned char) pSrcData[i >> 1];
		    cPack = ((i & 1) ? (cPack & 0xF) : (cPack >> 4)) + '0';
		    caWork[j++] = cPack;
		  }
	      }

		    /*--------------------------------------------*\
                     |                                            |
                     |  Now move the contents of our normalized   |
                     |  data area to the receiving field, packing |
                     |  the data as we go.  Packed fields are (at |
                     |  least at this time) always considered to  |
                     |  be signed, so we will also move a         |
                     |  trailing sign to the receiving area. A    |
                     |  value of 0xC represents a positive value  |
                     |  and 0xD represents a negative value.      |
                     |                                            |
                    \*--------------------------------------------*/

	    k = 0;
	    cPack = 0;
	    j = MAX_DIGITS + iDestDecimals - iDestLength;
	    iDestIntDigits = iDestLength;
	    if (iDestDecimals >= 0)
	      iDestIntDigits -= iDestDecimals;
	    /* zero fill destination first */
	    for (i = 0; i < iSrcLength; ++i)
	      {
		if (i == fldLength (pDstFld))
		  break;
		if (i & 1)	/* if lower (right) nibble */
		  pDstData[k++] = '\0';
	      }
	    if (i & 1)
	      pDstData[k] = '\0';

	    for (i = 0; i < iDestIntDigits; i++)
	      {
		if (i & 1)
		  {		/* if lower (right) nibble */
		    cPack = cPack | ((unsigned char) caWork[j++] & 0xF);
		    pDstData[i >> 1] = cPack;
		  }
		else
		  {
		    cPack = (unsigned char) caWork[j++] << 4;
		  }
	      }
	    k = i;

	    if (iDestDecimals > 0)
	      {
		temp1 = min (iDestLength, iDestDecimals);
		j = MAX_DIGITS;
		if (iDestDecimals > iDestLength)
		  j += iDestDecimals - iDestLength;
		for (i = 0; i < temp1; i++)
		  {
		    if (k & 1)
		      {
			cPack |= ((unsigned char) caWork[j++] & 0xF);
			pDstData[k >> 1] = cPack;
		      }
		    else
		      {
			cPack = (unsigned char) caWork[j++] << 4;
		      }
		    k++;
		  }
	      }
	    if (k & 1)
	      pDstData[k >> 1] = cPack;
	    put_sign (pDstFld, pDstData, iSrcSign);
	    break;
	  }

	case DTYPE_EDITED:
		    /*----------------------------------------------------*\
                     |                                                    |
                     |  We are handling edited moves in their own routine | 
                     |  since the logic involved is very complicated, and |
                     |  will be used in a similar fashion regardless of   |
                     |  source data type.                                 |
                    \*----------------------------------------------------*/

	  move_edited (pSrcFld, pSrcData, pDstFld, pDstData);
	  break;

	case DTYPE_FLOAT:
	  {
		    /*----------------------------------------------------*\
                     |  DTYPE_PACKED -> DTYPE_FLOAT                       |
                     |  Convert the source to a normalized DISPLAY type.  |
                     |  Then convert the contents of the normalized       |
                     |  data area to float or double.                     |
                    \*----------------------------------------------------*/
	    float fWork;
	    double dWork;
	    int bIsNegative, i, k;
	    char caWork[32], caWork1[2];

	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    if (pDstFld->decimals != 0)
	      {
		FldWrk.decimals += 2;
		FldWrk.len += 2;
	      }

	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    caWork[FldWrk.len] = '\0';
	    bIsNegative = extract_sign (&FldWrk, caWork);

	    // Add the decimal point
	    if (FldWrk.decimals != 0)
	      {
		caWork1[0] = '.';
		caWork1[1] = '\0';
		k = FldWrk.len - FldWrk.decimals;
		for (i = 0; i < FldWrk.len; i++)
		  {
		    if (i >= k)
		      {
			caWork1[1] = caWork[i];
			caWork[i] = caWork1[0];
			caWork1[0] = caWork1[1];
			caWork1[1] = '\0';
		      }
		  }
		caWork[i] = caWork1[0];
		i++;
		caWork[i] = '\0';
	      }

	    if (pDstFld->len == 4)
	      {
		sscanf (caWork, "%f", &fWork);
		if (bIsNegative)
		  fWork *= -1;
		*(float *) pDstData = fWork;
	      }
	    else
	      {
		sscanf (caWork, "%lf", &dWork);
		if (bIsNegative)
		  dWork *= -1;
		*(double *) pDstData = dWork;
	      }
	    break;
	  }

	case DTYPE_BININT:
	  {

		    /*----------------------------------------------------*\
                     |                                                    |
                     |  Call cobmove to move the packed  source into      |
                     |  a work area of type display.                      |
                     |                                                    |
                    \*----------------------------------------------------*/

	    long long iWork;
	    int bIsNegative;
	    char caWork[18];

	    FldWrk.len = picCompLength (pDstFld);
	    FldWrk.decimals = pDstFld->decimals;
	    FldWrk.type = DTYPE_DISPLAY;
	    FldWrk.all = 0;
	    strcpy (FldWrk.pic, pDstFld->pic);
	    cob_move (pSrcFld, pSrcData, &FldWrk, caWork);
	    bIsNegative = extract_sign (&FldWrk, caWork);
	    iWork = 0;
	    for (i = 0; i < FldWrk.len; ++i)
	      {
		iWork *= 10;
		iWork += (int) (caWork[i] - '0');
	      }
	    if (bIsNegative)
	      iWork *= -1;
	    switch (binFldSize (pDstFld))
	      {
	      case 1:
		*(char *) pDstData = iWork;
		break;
	      case 2:
		*(short int *) pDstData = iWork;
		break;
	      case 4:
		*(int *) pDstData = iWork;
		break;
	      case 8:
		*(long long int *) pDstData = iWork;
		break;
	      }
	    break;
	  }
	}
      break;


/* Source type is floating point */
/*----------------------------------------------------*\
 |  DTYPE_FLOAT -> All                                |
 |  Convert the source to a normalized DISPLAY type.  |
 |  Then convert the contents of the normalized       |
 |  data area to sestination field type  	      |
 |						      |
\*----------------------------------------------------*/
    case DTYPE_FLOAT:
      float2all (pSrcFld, pSrcData, pDstFld, pDstData);
      break;

/* source is binary integer */
    case DTYPE_BININT:
      {
	long long iWork;
	int j, k;
	int bIsNegative;
	char caWork[19];

	switch (binFldSize (pSrcFld))
	  {
	  case 1:
	    iWork = *(char *) pSrcData;
	    break;
	  case 2:
	    iWork = *(short int *) pSrcData;
	    break;
	  case 4:
	    iWork = *(int *) pSrcData;
	    break;
	  case 8:
	    iWork = *(long long int *) pSrcData;
	    break;
	  }
	if ((pDstFld->type == DTYPE_BININT)
	    && (pDstFld->decimals == pSrcFld->decimals)
	    && (fldLength (pDstFld) == fldLength (pDstFld)))
	  {
	    switch (binFldSize (pDstFld))
	      {
	      case 1:
		*(char *) pDstData = iWork;
		break;
	      case 2:
		*(short int *) pDstData = iWork;
		break;
	      case 4:
		*(int *) pDstData = iWork;
		break;
	      case 8:
		*(long long int *) pDstData = iWork;
		break;
		return;
	      }
	  }

	if (iWork < 0)
	  {
	    bIsNegative = 1;
	    iWork *= -1;
	  }
	else
	  bIsNegative = 0;

	k = 18 - fldLength (pSrcFld);
	for (i = 0; i < fldLength (pSrcFld); ++i)
	  {
	    if (iWork >= _iIntValues_[i + k])
	      {
		j = iWork / _iIntValues_[i + k];
		caWork[i] = (char) (j + '0');
		iWork -= (_iIntValues_[i + k] * j);
	      }
	    else
	      caWork[i] = '0';
	  }

	FldWrk.len = fldLength (pSrcFld);
	FldWrk.decimals = pSrcFld->decimals;
	FldWrk.type = DTYPE_DISPLAY;
	FldWrk.all = 0;
	strcpy (FldWrk.pic, pSrcFld->pic);
	if (FldWrk.pic[0] == 'S')
	  put_sign (&FldWrk, caWork, bIsNegative);
	cob_move (&FldWrk, caWork, pDstFld, pDstData);
      }
    }
  return;
}

/*-----------------------------------------------------------------------*\
 |                                                                       |
 |       char *pic_expand( struct fld_desc *FieldDesc)                   |
 |                                                                       |
 |  Return a string containing the picture clause for the field          |
 |  described in the field description structure pointed to by the       |
 |  argument FieldDesc.                                                  |
 |                                                                       |
 |  The string pointed to by the field 'pic' in the fld_desc structure   |
 |  has the following format:                                            |
 |         char           cPictChar;                                     |
 |         unsigned char  cLength;                                       |
 |            . . .                                                      |
 |            . . .                                                      |
 |         unsigned char(0);                                             |
 |  Where cPictChar is a character of the picture, for example 'X', '9', |
 |  'Z', etc. and cLength is the number of times that that character is  |
 |  repeated. These two fields repeat as many times as necessary to      |
 |  describe the entire picture clause.                                  |
 |                                                                       |
 |  Example:      PIC   Z(3),Z(2)9.99CR.                                 |
 |  Would be encoded as:                                                 |
 |         char('Z');                                                    |
 |         unsigned char(3);      the 'Z' occurrs 3 times                |
 |         char(',');                                                    |
 |         unsigned char(1);      comma occurs once                      |
 |         char('Z');                                                    |
 |         unsigned char(2);      second Z occurs 2 times                |
 |         char('9');                                                    |
 |         unsigned char(1);      followed by one 9                      |
 |         char('.');                                                    |
 |         unsigned char(1);      one decimal point                      |
 |         char('9');                                                    |
 |         unsigned char(2);      two digits following dec point         |
 |         char('C');                                                    |
 |         unsigned char(1);                                             |
 |         char('R');                                                    |
 |         unsigned char(1);                                             |
 |         unsigned char(0);      terminator for the string              |
 |                                                                       |
\*-----------------------------------------------------------------------*/

char *
pic_expand (struct fld_desc *pfldDesc)
{
  char *pic, *result;
  int sum, j, count;

  pic = pfldDesc->pic;
  sum = 0;
  for (j = 0; pic[j]; j++)
    sum += (unsigned char) pic[++j];

  result = malloc (sum + 1);	/* sum has length of encoded pic string */
  if (!result)
    {				/* if malloc failed, do something ? */
      runtime_error (RTERR_INVALID_PIC, pfldDesc, (void *) 0);
      return ((char *) 0);
    }

  sum = 0;
  for (j = 0; pic[j]; j += 2)
    {				/* decode the picture from fld_struct */
      count = (unsigned char) pic[j + 1];
      memset (&result[sum], pic[j], count);
      sum += count;
    }
  result[sum] = '\0';		/* null terminator for decoded pic string */

  return result;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          runtime_error                                 |
 |                                                                        |
\*------------------------------------------------------------------------*/

static int _iRtErrorNbr[] = {
  RTERR_INVALID_DATA,
  RTERR_INVALID_PIC,
  -1
};

static char *_szRtErrorDesc[] = {
  "Invalid Data Content",
  "Invalid Picture Structure",
  (char *) 0
};

void
runtime_error (int iErrorNum, struct fld_desc *pField, void *pData)
{
  int i, j;

  for (i = 0; _iRtErrorNbr[i] != -1; ++i)
    if (iErrorNum == _iRtErrorNbr[i])
      break;

  fprintf (stderr, "\n\nRun Time Error - ");
  if (_iRtErrorNbr[i] != -1)
    fprintf (stderr, "%s\n", _szRtErrorDesc[i]);
  else
    fprintf (stderr, "Unknown Error\n");

  if (pField)
    {
      fprintf (stderr, "Field Description: ");
      fprintf (stderr, "len = %ld, type = %c, decimals = %d, all = %d\n",
	       pField->len, pField->type, (char) pField->decimals,
	       pField->all);
    }
  if (!pData)
    {
      fprintf (stderr, "No data pointer provided\n");
      return;
    }

  fprintf (stderr, "Data Dump, Address = %08lX", (unsigned long) pData);
  j = 0;
  while (j < fldLength (pField))
    {
      fprintf (stderr, "\n%04X: ", (unsigned) j);
      for (i = 0; i < 24; ++i)
	{
	  if ((i + j) == fldLength (pField))
	    break;
	  fprintf (stderr, "%02X ", ((unsigned char *) pData)[i]);
	}
      fprintf (stderr, "\n      ");
      for (i = 0; i < 24; ++i)
	{
	  if ((i + j) == pField->len)
	    if ((i + j) == fldLength (pField))
	      break;
	  if ((((unsigned char *) pData)[i] >= ' ')
	      && (((unsigned char *) pData)[i] < 0x7f))
	    fprintf (stderr, " %c ", ((unsigned char *) pData)[i]);
	  else
	    fprintf (stderr, "   ");
	}
      j += 24;
    }
  fprintf (stderr, "\n\n");
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          move_edited                                   |
 |                                                                        |
\*------------------------------------------------------------------------*/

static void
move_edited (struct fld_desc *pSrcFld, char *pSrcData,
	     struct fld_desc *pDstFld, char *pDstData)
{
  int i, k;
  int iSrcPtr;
  int iDstPtr;
  int iDecimalPos;
  int bEndSuppression;
  int bIsNegative;
  int bInFraction;
  int bIsAlphaEdited;
  int bIsBlankWhenZero;

  char *caWorkData;
  char *pPic;
  char caPic[3];
  char cChar;
  char cFloatChar;
  char cFillChar;
  char cDecimalPoint;
  char cComma;

  struct fld_desc FldWrk;

  caPic[0] = 'S';
  caPic[1] = (char) 1;
  caPic[2] = '\0';
  FldWrk.pic = caPic;
  FldWrk.len = 0;
  FldWrk.decimals = 0;
  pPic = pDstFld->pic;
  bEndSuppression = 0;
  bIsNegative = 0;
  bInFraction = 0;
  bIsAlphaEdited = 0;
  bIsBlankWhenZero = 1;
  cFloatChar = '\0';
  if (decimal_comma)
    {
      cComma = '.';
      cDecimalPoint = ',';
    }
  else
    {
      cComma = ',';
      cDecimalPoint = '.';
    }

  for (i = 0; pPic[i]; i += 2)
    {
      cChar = pPic[i];
      k = pPic[i + 1];
      if ((cChar == 'X') || (cChar == '9') || (cChar == 'A'))
	{
	  if (cChar != '9')
	    bIsAlphaEdited = 1;
	  bIsBlankWhenZero = 0;
	  FldWrk.len += (long) k;
	  if (bInFraction)
	    FldWrk.decimals += k;
	  bEndSuppression = 1;
	  continue;
	}
      if (cChar == 'Z')
	{
	  FldWrk.len += (long) k;
	  if (bInFraction)
	    FldWrk.decimals += k;
	  continue;
	}
      if (cChar == '*')
	{
	  FldWrk.len += (long) k;
	  if (bInFraction)
	    FldWrk.decimals += k;
	  bIsBlankWhenZero = 0;
	  continue;
	}
      if (cChar == cCurrencySymbol)
	{
	  if (cFloatChar)
	    {
	      if (cFloatChar == cCurrencySymbol)
		{
		  FldWrk.len += (long) k;
		  if (bInFraction)
		    FldWrk.decimals += k;
		}
	      continue;
	    }
	  if (k > 1)
	    {
	      cFloatChar = cChar;
	      FldWrk.len += (k - 1);
	      if (bInFraction)
		FldWrk.decimals += (k - 1);
	      continue;
	    }
	  if (pPic[i + 2] == cComma)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  if (pPic[i + 2] == cDecimalPoint)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  bIsBlankWhenZero = 0;
	  continue;
	}
      if (cChar == '+')
	{
	  if (cFloatChar)
	    {
	      if (cFloatChar == '+')
		{
		  FldWrk.len += (long) k;
		  if (bInFraction)
		    FldWrk.decimals += k;
		}
	      continue;
	    }
	  if (k > 1)
	    {
	      cFloatChar = cChar;
	      FldWrk.len += (k - 1);
	      if (bInFraction)
		FldWrk.decimals += (k - 1);
	      continue;
	    }
	  if (pPic[i + 2] == cComma)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  if (pPic[i + 2] == cDecimalPoint)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  bIsBlankWhenZero = 0;
	  continue;
	}
      if (cChar == '-')
	{
	  if (cFloatChar)
	    {
	      if (cFloatChar == '-')
		{
		  FldWrk.len += (long) k;
		  if (bInFraction)
		    FldWrk.decimals += k;
		}
	      continue;
	    }
	  if (k > 1)
	    {
	      cFloatChar = cChar;
	      FldWrk.len += (k - 1);
	      if (bInFraction)
		FldWrk.decimals += (k - 1);
	      continue;
	    }
	  if (pPic[i + 2] == cComma)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  if (pPic[i + 2] == cDecimalPoint)
	    {
	      cFloatChar = cChar;
	      continue;
	    }
	  bIsBlankWhenZero = 0;
	  continue;
	}
      if (cChar == cComma)
	{
	  continue;
	}
      if (cChar == cDecimalPoint)
	{
	  bInFraction = 1;
	  continue;
	}
      if (cChar == 'V')
	{
	  bInFraction = 1;
	  continue;
	}
      bIsBlankWhenZero = 0;
    }

  if (!FldWrk.len)
    return;

  caWorkData = malloc (FldWrk.len);
  if (!caWorkData)
    return;
  if (bIsAlphaEdited)
    FldWrk.type = DTYPE_ALPHANUMERIC;
  else
    FldWrk.type = DTYPE_DISPLAY;
  cob_move (pSrcFld, pSrcData, &FldWrk, caWorkData);
  if (!bIsAlphaEdited)
    bIsNegative = extract_sign (&FldWrk, caWorkData);

  if (bIsBlankWhenZero)
    {
      for (i = 0; i < FldWrk.len; ++i)
	if (caWorkData[i] != '0')
	  break;
      if (i == FldWrk.len)
	{			/* it's zero, blank it out */
	  memset (pDstData, ' ', fldLength (pDstFld));
	  return;
	}
    }

  bEndSuppression = 0;
  bInFraction = 0;
  cFloatChar = '\0';
  cFillChar = ' ';
  iSrcPtr = 0;
  iDstPtr = 0;
  iDecimalPos = 0;

  pPic = pic_expand (pDstFld);
  if (!pPic)
    {
      free (caWorkData);
      return;
    }
  for (i = 0; pPic[i]; ++i)
    {
      cChar = pPic[i];
      if ((cChar == 'X') || (cChar == '9') || (cChar == 'A'))
	{
	  if (!bEndSuppression)
	    {			/* all suppressed so far */
	      if (i != 0)
		{		/* not first character */
		  if (cFloatChar)
		    pDstData[iDstPtr - 1] = cFloatChar;
		  else if (pPic[i - 1] == cComma)
		    pDstData[iDstPtr - 2] = cFillChar;
		  else if (pPic[i - 1] != cDecimalPoint)
		    pDstData[iDstPtr - 1] = cFillChar;
		}
	    }
	  pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
	  bEndSuppression = 1;
	  continue;
	}
      if ((cChar == 'Z') || (cChar == '*'))
	{
	  if (bEndSuppression)
	    {
	      pDstData[iDstPtr++] = caWorkData[iSrcPtr];
	    }
	  else if (caWorkData[iSrcPtr] != '0')
	    {
	      pDstData[iDstPtr++] = caWorkData[iSrcPtr];
	      bEndSuppression = 1;
	    }
	  else if (cChar == 'Z')
	    {
	      pDstData[iDstPtr++] = ' ';
	    }
	  else
	    {
	      cFillChar = '*';
	      pDstData[iDstPtr++] = cChar;
	    }
	  iSrcPtr++;
	  continue;
	}
      if (cChar == cCurrencySymbol)
	{
	  if ((cChar != cFloatChar) && (i))
	    {
	      if ((pPic[i - 1] != cChar)
		  && (pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = cChar;
		  continue;
		}
	    }
	  if ((cChar != cFloatChar) && (!i))
	    {
	      if ((pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = cChar;
		  continue;
		}
	    }
	  if (bEndSuppression)
	    {
	      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
	      continue;
	    }
	  if (cFloatChar)
	    {
	      if (cFloatChar == cCurrencySymbol)
		{
		  if (caWorkData[iSrcPtr] != '0')
		    {
		      pDstData[iDstPtr - 1] = cChar;
		      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
		      bEndSuppression = 1;
		    }
		  else
		    {
		      if (pPic[i + 1] == cDecimalPoint)
			{
			  pDstData[iDstPtr++] = cChar;
			  bEndSuppression = 1;
			  ++iSrcPtr;
			  continue;
			}
		      pDstData[iDstPtr++] = ' ';
		      iSrcPtr++;
		    }
		}
	      else
		pDstData[iDstPtr++] = cChar;
	      continue;
	    }
	  if ((pPic[i + 1] == cChar) || (pPic[i + 1] == cComma))
	    {
	      cFloatChar = cChar;
	      pDstData[iDstPtr++] = ' ';
	      continue;
	    }
	  pDstData[iDstPtr++] = cChar;
	  continue;
	}
      if (cChar == '+')
	{
	  if ((cChar != cFloatChar) && (i != 0))
	    {
	      if ((pPic[i - 1] != cChar)
		  && (pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = (bIsNegative) ? '-' : '+';
		  continue;
		}
	    }
	  if ((cChar != cFloatChar) && (i == 0))
	    {
	      if ((pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = (bIsNegative) ? '-' : '+';
		  continue;
		}
	    }
	  if (bEndSuppression)
	    {
	      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
	      continue;
	    }
	  if (cFloatChar)
	    {
	      if (cFloatChar == '+')
		{
		  if (caWorkData[iSrcPtr] != '0')
		    {
		      pDstData[iDstPtr - 1] = (bIsNegative) ? '-' : '+';
		      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
		      bEndSuppression = 1;
		    }
		  else
		    {
		      if (pPic[i + 1] == cDecimalPoint)
			{
			  pDstData[iDstPtr++] = (bIsNegative) ? '-' : '+';
			  iSrcPtr++;
			  bEndSuppression = 1;
			  continue;
			}
		      pDstData[iDstPtr++] = ' ';
		      iSrcPtr++;
		    }
		}
	      else
		pDstData[iDstPtr++] = cChar;
	      continue;
	    }
	  if ((pPic[i + 1] == cChar) || (pPic[i + 1] == cComma))
	    {
	      cFloatChar = cChar;
	      pDstData[iDstPtr++] = ' ';
	      continue;
	    }
	  pDstData[iDstPtr++] = cChar;
	  continue;
	}
      if (cChar == '-')
	{
	  if ((cChar != cFloatChar) && (i))
	    {
	      if ((pPic[i - 1] != cChar)
		  && (pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = (bIsNegative) ? cChar : ' ';
		  continue;
		}
	    }
	  if ((cChar != cFloatChar) && (!i))
	    {
	      if ((pPic[i + 1] != cChar) && (pPic[i + 1] != cComma))
		{
		  bEndSuppression = 1;
		  pDstData[iDstPtr++] = (bIsNegative) ? cChar : ' ';
		  continue;
		}
	    }
	  if (bEndSuppression)
	    {
	      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
	      continue;
	    }
	  if (cFloatChar)
	    {
	      if (cFloatChar == '-')
		{
		  if (caWorkData[iSrcPtr] != '0')
		    {
		      pDstData[iDstPtr - 1] = (bIsNegative) ? '-' : ' ';
		      pDstData[iDstPtr++] = caWorkData[iSrcPtr++];
		      bEndSuppression = 1;
		    }
		  else
		    {
		      if (pPic[i + 1] == cDecimalPoint)
			{
			  pDstData[iDstPtr++] = (bIsNegative) ? '-' : ' ';
			  iSrcPtr++;
			  bEndSuppression = 1;
			  continue;
			}
		      pDstData[iDstPtr++] = ' ';
		      iSrcPtr++;
		    }
		}
	      else
		pDstData[iDstPtr++] = cChar;
	      continue;
	    }
	  if ((pPic[i + 1] == cChar) || (pPic[i + 1] == cComma))
	    {
	      cFloatChar = cChar;
	      pDstData[iDstPtr++] = ' ';
	      continue;
	    }
	  pDstData[iDstPtr++] = cChar;
	  continue;
	}
      if (cChar == cComma)
	{
	  pDstData[iDstPtr++] = (bEndSuppression) ? cChar : cFillChar;
	  continue;
	}
      if (cChar == cDecimalPoint)
	{
	  pDstData[iDstPtr++] = '.';
	  iDecimalPos = iDstPtr;
	  continue;
	}
      if ((cChar == 'C') && (pPic[i + 1] == 'R'))
	{
	  if (bIsNegative)
	    {
	      pDstData[iDstPtr++] = 'C';
	      pDstData[iDstPtr++] = 'R';
	    }
	  else
	    {
	      pDstData[iDstPtr++] = ' ';
	      pDstData[iDstPtr++] = ' ';
	    }
	  i++;
	  continue;
	}
      if ((cChar == 'D') && (pPic[i + 1] == 'B'))
	{
	  if (bIsNegative)
	    {
	      pDstData[iDstPtr++] = 'D';
	      pDstData[iDstPtr++] = 'B';
	    }
	  else
	    {
	      pDstData[iDstPtr++] = ' ';
	      pDstData[iDstPtr++] = ' ';
	    }
	  i++;
	  continue;
	}
      if (cChar == 'B')
	{
	  pDstData[iDstPtr++] = ' ';
	  continue;
	}
      if (cChar == 'V')
	{
	  continue;
	}
      pDstData[iDstPtr++] = cChar;
    }
  if ((iDecimalPos) && (bEndSuppression))
    {
      for (i = iDecimalPos; i < fldLength (pDstFld); ++i)
	{
	  if ((pDstData[i] >= '0') && (pDstData[i] <= '9'))
	    break;
	  pDstData[i] = '0';
	}
    }
  free (pPic);
  free (caWorkData);
  return;
}


/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          _DUMP_                                        |
 |                                                                        |
\*------------------------------------------------------------------------*/

void
_DUMP_ (unsigned char *caData, char *szCount, char *caOut)
{
  int i, k;
  unsigned char c;

  k = 0;
  for (i = 0; i < 4; ++i)
    {
      if (szCount[i] == '\0')
	break;
      k *= 10;
      k += (szCount[i] - '0');
    }

  for (i = 0; i < k; ++i)
    {
      c = (caData[i] >> 4) + '0';
      if (c > '9')
	c += 7;
      caOut[i * 2] = c;

      c = (caData[i] & 0xf) + '0';
      if (c > '9')
	c += 7;
      caOut[(i * 2) + 1] = c;
    }
}


/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          move float                                    |
 |  DTYPE_FLOAT -> All                                                    |
 |  Convert the source to a normalized DISPLAY type.                      |
 |  Then convert the contents of the normalized                           |
 |  data area to sestination field type  	                          |
 |                                                                        |
\*------------------------------------------------------------------------*/

static void
float2all (struct fld_desc *pSrcFld, char *pSrcData, struct fld_desc *pDstFld,
	   char *pDstData)
{

  double dWork;
  int j, k, i, typesw, slen;
  int bIsNegative;
  char caWork[19], caPic[16];
  struct fld_desc FldWrk;

  FldWrk.pic = caPic;

  if (pDstFld->type == DTYPE_FLOAT)
    {
      typesw = (10 * binFldSize (pSrcFld)) + binFldSize (pDstFld);
      switch (typesw)
	{
	case 44:
	  *(float *) pDstData = *(float *) pSrcData;
	  break;
	case 48:
	  *(double *) pDstData = *(float *) pSrcData;
	  break;
	case 84:
	  *(float *) pDstData = *(double *) pSrcData;
	  break;
	case 88:
	  *(double *) pDstData = *(double *) pSrcData;
	  break;
	}
    }
  else
    {
      typesw = binFldSize (pSrcFld);

      if (typesw == 4)
	dWork = (double) *((float *) pSrcData);
      else
	dWork = (double) *((double *) pSrcData);

      if (dWork < 0)
	{
	  bIsNegative = 1;
	  dWork *= -1;
	}
      else
	{
	  bIsNegative = 0;
	}
      sprintf (caWork, "%f", dWork);

      // Remove padding zeros after decimal point
      slen = strlen (caWork);
      for (i = slen - 1; i >= 0; i--)
	{
	  if (caWork[i] == '0')
	    caWork[i] = '\0';
	  else
	    i = -1;
	}

      j = 0;			// Location of decimal point 
      k = 0;			// Decimal point switch
      // Determine location and remove decimal point 
      slen = strlen (caWork);
      for (i = 0; i < slen; i++)
	{
	  if (k == 0)
	    {
	      if (caWork[i] == '.')
		{
		  j = i;
		  k = 1;
		}
	    }
	  else
	    {
	      caWork[i - 1] = caWork[i];
	    }
	}
      if (k != 0)
	caWork[i - 1] = '\0';

      slen = strlen (caWork);
      FldWrk.len = slen;
      if (j == 0)
	FldWrk.decimals = 0;
      else
	FldWrk.decimals = slen - j;
      FldWrk.type = DTYPE_DISPLAY;
      FldWrk.all = 0;

      strcpy (FldWrk.pic, pSrcFld->pic);
      if (FldWrk.pic[0] == 'S')
	put_sign (&FldWrk, caWork, bIsNegative);
      cob_move (&FldWrk, caWork, pDstFld, pDstData);

    }
}
