/* Strings Module
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
#if defined(SunOS)
va_list __builtin_va_alist;
#endif

int offset_substr (char *s1, char *s2, int n1, int n2);
static void cob_put_integer (struct fld_desc *fdesc, char *sbuf, int value);
struct comparand *alloc_comparand (int opt, struct comparand **list);
void free_comparands (struct comparand *cmps);

/*
 * auxiliary comparands list to walk several times through comparands
 * in cob_inspect_replacing function.
 */
struct comparand
{
  struct comparand *next;
  int opt;
  struct fld_desc *ffor, *fby, *fbefore, *fafter;
  char *sfor, *sby, *sbefore, *safter;
  int stop;			/* -1 -> not yet (only if "after" found), 
				   0 -> go, 1 -> stop */
};

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          alloc_comparand                               |
 |                                                                        |
\*------------------------------------------------------------------------*/

struct comparand *
alloc_comparand (int opt, struct comparand **list)
{
  struct comparand *anew, *tmp;
  anew = (struct comparand *) malloc (sizeof (struct comparand));
  if ((tmp = *list))
    {
      while (tmp->next)
	tmp = tmp->next;
      tmp->next = anew;
    }
  else
    *list = anew;
  anew->next = NULL;
  anew->opt = opt;
  anew->ffor = anew->fby = anew->fbefore = anew->fafter = NULL;
  anew->sfor = anew->sby = anew->sbefore = anew->safter = NULL;
  anew->stop = -1;
  return anew;
};

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          free_comparands                               |
 |                                                                        |
\*------------------------------------------------------------------------*/

void
free_comparands (struct comparand *cmps)
{
  struct comparand *tmp;
  while (cmps)
    {
      tmp = cmps;
      cmps = cmps->next;
      free (tmp);
    }
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_inspect_converting                        |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_inspect_converting (struct fld_desc *fvar, char *svar,
			struct fld_desc *ffrom, char *sfrom,
			struct fld_desc *fto, char *sto, ...)
{
  /* will receive optional before/after variables, like other
     inpect statements */
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_inspect_tallying                          |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_inspect_tallying (struct fld_desc *fvar, char *svar, ...)
{
  struct fld_desc *fcnt;
  struct comparand *cmp, *comparands;
  char *scnt, *tscnt;
  int opt, cnt, cnt_incr, len, clen, rsize;
  va_list args;

  va_start (args, svar);
  comparands = NULL;
  while ((fcnt = va_arg (args, struct fld_desc *)))
    {
      scnt = va_arg (args, char *);
      clen = fcnt->len;
      while ((opt = va_arg (args, int)))
	{
	  cmp = alloc_comparand (opt, &comparands);
	  if (opt != INSPECT_CHARACTERS)
	    {
	      if ((cmp->ffor = va_arg (args, struct fld_desc *)))
		{
		  cmp->sfor = va_arg (args, char *);
		}
	    }
	  if ((cmp->fbefore = va_arg (args, struct fld_desc *)))
	    {
	      cmp->sbefore = va_arg (args, char *);
	    }
	  if ((cmp->fafter = va_arg (args, struct fld_desc *)))
	      cmp->safter = va_arg (args, char *);
	  else
	    cmp->stop = 0;

	}
    }

  va_end (args);
  len = fvar->len;
  cnt = 0;
  /* do the actual processing */
  while (len)
    {
      cnt_incr = 1;
      rsize = 1;
      for (cmp = comparands; cmp; cmp = cmp->next)
	{			/* reposition comparand list */
	  if (cmp->stop < 0)
	    {
	      if (!offset_substr (svar, cmp->safter, len, cmp->fafter->len))
		cmp->stop = 0;
	    }
	  if (cmp->fbefore != NULL)
	    {
	      if (!offset_substr (svar, cmp->sbefore, len, cmp->fbefore->len))
		cmp->stop = 1;
	    }
	  if (!cmp->stop)
	    {
	      if (cmp->opt == INSPECT_CHARACTERS)
		{
		  cnt += cnt_incr;
		  break;
		}
	      if (!offset_substr (svar, cmp->sfor, len, cmp->ffor->len))
		{
		  cnt += cnt_incr;
		  rsize = cmp->ffor->len;
		  if (cmp->opt == INSPECT_FIRST)
		    cmp->stop = 1;
		  break;
		}
	      else if (cmp->opt == INSPECT_LEADING)
		{
		  cmp->stop = 1;
		}
	    }
	}
      svar += rsize;
      len -= rsize;
    }
  tscnt = malloc (clen + 1);
  sprintf (tscnt, "%0*d", clen, cnt);
  memmove (scnt, tscnt, clen);
  free (tscnt);
  free (comparands);
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_inspect_replacing                         |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_inspect_replacing (struct fld_desc *fvar, char *svar, ...)
{
  struct comparand *cmp, *comparands;
  int opt, len, rsize;
  va_list args;

  va_start (args, svar);
  comparands = NULL;
  while ((opt = va_arg (args, int)))
    {
      cmp = alloc_comparand (opt, &comparands);
      if (opt != INSPECT_CHARACTERS)
	{
	  if ((cmp->ffor = va_arg (args, struct fld_desc *)))
	    {
	      cmp->sfor = va_arg (args, char *);
	    }
	}
      if ((cmp->fby = va_arg (args, struct fld_desc *)))
	{
	  cmp->sby = va_arg (args, char *);
	}
      if ((cmp->fbefore = va_arg (args, struct fld_desc *)))
	{
	  cmp->sbefore = va_arg (args, char *);
	}
      if ((cmp->fafter = va_arg (args, struct fld_desc *)))
	{
	  cmp->safter = va_arg (args, char *);
	}
      else
	cmp->stop = 0;
    }
  va_end (args);
  len = fvar->len;
  /* do the actual processing */
  while (len)
    {
      rsize = 1;
      for (cmp = comparands; cmp; cmp = cmp->next)
	{			/* reposition comparand list */
	  if (cmp->stop < 0)
	    {
	      if (!offset_substr (svar, cmp->safter, len, cmp->fafter->len))
		cmp->stop = 0;
	    }
	  if (cmp->fbefore)
	    {
	      if (!offset_substr (svar, cmp->sbefore, len, cmp->fbefore->len))
		cmp->stop = 1;
	    }
	  if (!cmp->stop)
	    {
	      if (cmp->opt == INSPECT_CHARACTERS)
		{
		  memmove (svar, cmp->sby, 1);
		  break;
		}
	      if (!offset_substr (svar, cmp->sfor, len, cmp->ffor->len))
		{
		  rsize = cmp->ffor->len;
		  memmove (svar, cmp->sby, rsize);
		  if (cmp->opt == INSPECT_FIRST)
		    cmp->stop = 1;
		  break;
		}
	      else if (cmp->opt == INSPECT_LEADING)
		{
		  cmp->stop = 1;
		}
	    }
	}
      svar += rsize;
      len -= rsize;
    }
  free (comparands);
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_unstring                                  |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_unstring (struct fld_desc *fvar, char *svar, ...)
{
  struct fld_desc *fptr, *ftally;
  char *sptr, *stally;
  struct fld_desc **p;
  struct fld_desc *fdelim, *fdest, *fdltr, *fcnt;
  char *sdelim, *sdest, *sdltr, *scnt;
  char *delimbuf, *s1;
  int all, n, n1, len, partlen, delimlen, delimall, nfields;
  int i, ip;		// assorted indexes
  va_list args;

  /* first receive all arguments */
  va_start (args, svar);
  if ((fptr = va_arg (args, struct fld_desc *)))
    {
      sptr = va_arg (args, char *);
    }
  if ((ftally = va_arg (args, struct fld_desc *)))
    {
      stally = va_arg (args, char *);
    }
  /* setup indirect pointer to the start of delimiters array */
  i = 0;
  len = 16;
  p = malloc (sizeof (struct fld_desc) * len);
  p[i] = va_arg (args, struct fld_desc *);
  while (p[i])
    {
      if (i + 3 >= len)
	{
	  len *= 2;
	  p = realloc (p, sizeof (struct fld_desc) * len);
	}
      p[++i] = va_arg (args, struct fld_desc *);
      p[++i] = va_arg (args, struct fld_desc *);
      p[++i] = va_arg (args, struct fld_desc *);
    }

  /* now execute the actual unstring command */
  len = fvar->len;
  i = 0;
  if (fptr)
    {				/* if there is a pointer, skip some length at svar */
      n = get_index (fptr, sptr) - 1;	/* get the integer value of this */
      if (n >= (len - i) || n < 0)
	goto error;		/* overflow at the pointer */
      i += n;
    }
  nfields = 0;
  for (fdest = va_arg (args, struct fld_desc *);
       fdest;
       fdest = va_arg (args, struct fld_desc *))
    {
      sdest = va_arg (args, char *);
      if ((fdltr = va_arg (args, struct fld_desc *)))
	{
	  sdltr = va_arg (args, char *);
	}
      if ((fcnt = va_arg (args, struct fld_desc *)))
	{
	  scnt = va_arg (args, char *);
	}
      if ((len - i) <= 0)	/* check if overflow found */
	goto error;
      /* find the nearest delimiter */
      delimall = 0;
      delimlen = len - i;
      delimbuf = NULL;
      ip = 0;
      fdelim = p[ip++];
      partlen = (!fdelim) ? (fdest->len) : (len - i);
      for (; fdelim; fdelim = p[ip++])
	{
	  sdelim = (char *) p[ip++];
	  all = (int) p[ip++];
	  n1 = offset_substr (svar + i, sdelim, partlen, fdelim->len);
	  if (n1 < partlen)
	    {
	      partlen = n1;
	      delimlen = fdelim->len;
	      delimbuf = sdelim;
	      delimall = all;
	    }
	}
      /* this should be a call to our cob_move function, 
         but it's unfinished yet */
      memmove (sdest, svar + i, min (fdest->len, partlen));
      if (fdest->len > partlen)
	memset (sdest + partlen, ' ', fdest->len - partlen);
      /* adjust for the partial string processed */
      i += partlen;
      if (delimbuf)
	{			/* adjust for delimiter too */
	  i += delimlen;
	  if (fdltr)
	    {			/* if delimiter storage requested */
	      memset (sdltr, ' ', fdltr->len);
	      memmove (sdltr, delimbuf, min (fdltr->len, delimlen));
	      n1 = fdltr->len - delimlen;
	      s1 = sdltr + delimlen;
	    }
	}
      if (fcnt)
	{
	  cob_put_integer (fcnt, scnt, partlen);
	}
      if (delimall)
	{			/* remove all copies of delimiter */
	  while ((len - i)
		 && !offset_substr (svar + i, delimbuf, len - i, delimlen))
	    {
	      i += delimlen;
	      if (n1 && fdltr)
		{
		  memmove (s1, delimbuf, min (n1, delimlen));
		  n1 -= delimlen;
		  s1 += delimlen;
		}
	    }
	  if (fdltr)
	    memset (s1, ' ', n1);
	}
      if (fcnt)
	{			/* if count requested */
	  cob_put_integer (fcnt, scnt, partlen);
	}
      nfields++;
    }
  if (ftally)
    {
      cob_put_integer (ftally, stally, nfields + get_index (ftally, stally));
    }
  if (fptr)
    {
      cob_put_integer (fptr, sptr, i + 1);
    }
  if (len - i)			/* another way to overflow */
    {
    error:
      free (p);
      va_end (args);
      return -1;
    }
  free (p);
  va_end (args);
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_string                                    |
 |  Cobol string statement.                                               |
 |  The variables are (in that order):                                    |
 |     receiving var, pointer (with pointer clause),                      |
 |     1st. sending var, 2nd sending var,...                              |
 |  Each variable have it's field descriptor (struct fld_desc) and it's   |
 |  buffer, except if it's non-existent. In such case, only a NULL is     |
 |  passed as argument and must be skipped. (not 2 stack positions ever)  |
 |  The last sending variable is a NULL.                                  |
 |                                                                        | 
 |  This function returns -1 in case of overflow found, or 0 if ok.       |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
cob_string (struct fld_desc *fdst, char *sdst, ...)
{
  struct fld_desc *fptr, *fsrc, *fdelim;
  char *sptr, *ssrc, *sdelim;
  int n, len, i = 0;
  va_list args;

  len = fdst->len;
  va_start (args, sdst);
  fptr = va_arg (args, struct fld_desc *);
  if (fptr)
    {
      sptr = va_arg (args, char *);
      i += get_index (fptr, sptr);	/* get the integer value of this */
    }
  fsrc = va_arg (args, struct fld_desc *);
  while (fsrc)
    {				/* while there are variables to move */
      ssrc = va_arg (args, char *);
      fdelim = va_arg (args, struct fld_desc *);
      if (fdelim)
	{			/* if there is a delimiter, get it's buffer */
	  sdelim = va_arg (args, char *);
	}
      n = fsrc->len;
      if (fdelim)
	{
	  n = offset_substr (ssrc, sdelim, n, fdelim->len);
	}
      if ((len - i) >= n)
	{
	  memmove (sdst + i, ssrc, n);
	  i += n;
	}
      else
	{
	  return -1;
	}
      fsrc = va_arg (args, struct fld_desc *);
    }
  va_end (args);
  memset (sdst + i, ' ', len - i);
  return 0;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          offset_substr                                 |
 |  return number of characters before found s2 in s1                     |
 |  (C string functions are not useful here, because                      |
 |  the strings are _not_ NULL-terminated)                                |
 |  I would like to see here a better algorithm, but this                 |
 |  "brute-force" method is easier to code now.                           |
 |                                                                        |
\*------------------------------------------------------------------------*/

int
offset_substr (char *s1, char *s2, int n1, int n2)
{
  int i, j;
  for (i = 0; i < n1; i++)
    {
      for (j = 0; j < n2; j++)
	{
	  if (i + j > n1)
	    break;		/* past the first string, ignore */
	  if (s1[i + j] != s2[j])
	    break;
	}
      if (j == n2)
	break;			/* found! */
    }
  return i;
}

/*------------------------------------------------------------------------*\
 |                                                                        |
 |                          cob_put_integer                               |
 |  this is not the most generic implementation, as we should use a call  |
 |  to cob_move in the future, but it's better than none                  |
 |                                                                        |
\*------------------------------------------------------------------------*/

static void
cob_put_integer (struct fld_desc *fdesc, char *sbuf, int value)
{
  struct fld_desc fld = { 4, 'B', 0, 0, 0, 0, 0, 0, "S9\x9" };
  cob_move (&fld, (char *) &value, fdesc, sbuf);
/*	char *s;
	s = malloc(fdesc->len+1);
	sprintf(s,"%0*d",(int)fdesc->len,value);
	memmove(sbuf,s,fdesc->len);
	free(s);*/
}

/* end of strings.c */
