/*
 * Copyright (C) 2001-2004 Keisuke Nishida
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
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#ifdef	__MINGW32__
#include <io.h>
#include <fcntl.h>
#endif

#include "common.h"
#include "move.h"
#include "numeric.h"
#include "termio.h"
#include "fileio.h"
#include "call.h"
#include "lib/gettext.h"

int cob_argc = 0;
char **cob_argv = NULL;

int cob_initialized = 0;
int cob_exception_code = 0;

cob_module *cob_current_module = NULL;

const char *cob_source_file = NULL;
unsigned int cob_source_line = 0;
const char *cob_source_statement = NULL;

int cob_linage_counter = 0;
int cob_call_params = 0;

static cob_field_attr all_attr = {COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL};

cob_field cob_zero =  {1, "0",    &all_attr};
cob_field cob_space = {1, " ",    &all_attr};
cob_field cob_high =  {1, "\xff", &all_attr};
cob_field cob_low =   {1, "\0",   &all_attr};
cob_field cob_quote = {1, "\"",   &all_attr};

const int cob_exp10[10] = {
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000
};

const long long cob_exp10LL[19] = {
  1LL,
  10LL,
  100LL,
  1000LL,
  10000LL,
  100000LL,
  1000000LL,
  10000000LL,
  100000000LL,
  1000000000LL,
  10000000000LL,
  100000000000LL,
  1000000000000LL,
  10000000000000LL,
  100000000000000LL,
  1000000000000000LL,
  10000000000000000LL,
  100000000000000000LL,
  1000000000000000000LL
};

const unsigned char cob_a2e[256] = {
  0x00, 0x01, 0x02, 0x03, 0x1d, 0x19, 0x1a, 0x1b, 
  0x0f, 0x04, 0x16, 0x06, 0x07, 0x08, 0x09, 0x0a, 
  0x0b, 0x0c, 0x0d, 0x0e, 0x1e, 0x1f, 0x1c, 0x17, 
  0x10, 0x11, 0x20, 0x18, 0x12, 0x13, 0x14, 0x15, 
  0x21, 0x27, 0x3a, 0x36, 0x28, 0x30, 0x26, 0x38, 
  0x24, 0x2a, 0x29, 0x25, 0x2f, 0x2c, 0x22, 0x2d, 
  0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 
  0x7b, 0x7c, 0x35, 0x2b, 0x23, 0x39, 0x32, 0x33, 
  0x37, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 
  0x5e, 0x5f, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 
  0x67, 0x68, 0x69, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 
  0x70, 0x71, 0x72, 0x7d, 0x6a, 0x7e, 0x7f, 0x31, 
  0x34, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 
  0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 
  0x4a, 0x4b, 0x4c, 0x4e, 0x4f, 0x50, 0x51, 0x52, 
  0x53, 0x54, 0x55, 0x56, 0x2e, 0x60, 0x4d, 0x05, 
  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 
  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 
  0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 
  0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 
  0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 
  0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 
  0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 
  0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 
  0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 
  0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 
  0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 
  0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

/* Not needed at the moment - comment out
const unsigned char cob_e2a[256] = {
  0x00, 0x01, 0x02, 0x03, 0x9c, 0x09, 0x86, 0x7f,
  0x97, 0x8d, 0x8e, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, 0x9d, 0x85, 0x08, 0x87,
  0x18, 0x19, 0x92, 0x8f, 0x1c, 0x1d, 0x1e, 0x1f,
  0x80, 0x81, 0x82, 0x83, 0x84, 0x0a, 0x17, 0x1b,
  0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x05, 0x06, 0x07,
  0x90, 0x91, 0x16, 0x93, 0x94, 0x95, 0x96, 0x04,
  0x98, 0x99, 0x9a, 0x9b, 0x14, 0x15, 0x9e, 0x1a,
  0x20, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6,
  0xa7, 0xa8, 0x5b, 0x2e, 0x3c, 0x28, 0x2b, 0x21,
  0x26, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  0xb0, 0xb1, 0x5d, 0x24, 0x2a, 0x29, 0x3b, 0x5e,
  0x2d, 0x2f, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  0xb8, 0xb9, 0x7c, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,
  0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1,
  0xc2, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,
  0xc3, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9,
  0xca, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70,
  0x71, 0x72, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0,
  0xd1, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
  0x79, 0x7a, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed,
  0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,
  0x51, 0x52, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3,
  0x5c, 0x9f, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
  0x59, 0x5a, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};
end of comment out */

struct cob_exception cob_exception_table[] = {
  {0, 0, 0},		/* COB_EC_ZERO */
#undef COB_EXCEPTION
#define COB_EXCEPTION(CODE,TAG,NAME,CRITICAL) { 0x##CODE, NAME, CRITICAL },
#include "exception.def"
  {0, 0, 0}		/* COB_EC_MAX */
};


/*
 * General functions
 */

void
cob_init (int argc, char **argv)
{
  cob_argc = argc;
  cob_argv = argv;

  if (!cob_initialized)
    {
#if ENABLE_NLS
      setlocale (LC_ALL, "");
      bindtextdomain (PACKAGE, LOCALEDIR);
      textdomain (PACKAGE);
#endif

/* Dirty hack until we implement something better */
#ifdef	__MINGW32__
	_setmode(_fileno(stdin), _O_BINARY);
	_setmode(_fileno(stdout), _O_BINARY);
	_setmode(_fileno(stderr), _O_BINARY);
#endif

      cob_init_numeric ();
      cob_init_termio ();
      cob_init_fileio ();
      cob_init_call ();

      cob_initialized = 1;
    }
}

void
cob_module_enter (cob_module *module)
{
  if (!cob_initialized)
    {
      fputs (_("warning: cob_init expected in the main program\n"), stderr);
      cob_init (0, NULL);
    }

  module->next = cob_current_module;
  cob_current_module = module;
}

void
cob_module_leave (cob_module *module)
{
  cob_current_module = cob_current_module->next;
}

void
cob_stop_run (int status)
{
  exit (status);
}


/*
 * Sign
 */

/* SIGN */

/*
 * positive: 0123456789
 * negative: pqrstuvwxy
 */
#define GET_SIGN_ASCII(x) x -= 0x40
#define PUT_SIGN_ASCII(x) x += 0x40

/*
 * positive: 0123456789
 * negative: @ABCDEFGHI
 */
#define GET_SIGN_ASCII10(x) x -= 0x10
#define PUT_SIGN_ASCII10(x) x += 0x10

int
cob_real_get_sign (cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	unsigned char *p;

	/* locate sign */
	if (COB_FIELD_SIGN_LEADING (f))
	  p = f->data;
	else
	  p = f->data + f->size - 1;

	/* get sign */
	if (COB_FIELD_SIGN_SEPARATE (f))
	  {
	    return (*p == '+') ? 1 : -1;
	  }
	else
	  {
	    if (*p <= '9')
	      return 1;
	    switch (cob_current_module->display_sign)
	      {
	      case COB_DISPLAY_SIGN_ASCII: GET_SIGN_ASCII (*p); break;
	      case COB_DISPLAY_SIGN_ASCII10: GET_SIGN_ASCII10 (*p); break;
	      default: abort ();
	      }
	    return -1;
	  }
      }
    case COB_TYPE_NUMERIC_PACKED:
      {
	unsigned char *p = f->data + f->attr->digits / 2;
	return ((*p & 0x0f) == 0x0d) ? -1 : 1;
      }
    default:
      return 0;
    }
}

void
cob_real_put_sign (cob_field *f, int sign)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	unsigned char *p;

	/* locate sign */
	if (COB_FIELD_SIGN_LEADING (f))
	  p = f->data;
	else
	  p = f->data + f->size - 1;

	/* put sign */
	if (COB_FIELD_SIGN_SEPARATE (f))
	  {
	    int c = (sign < 0) ? '-' : '+';
	    if (*p != c)
	      *p = c;
	  }
	else if (sign < 0)
	  {
	    switch (cob_current_module->display_sign)
	      {
	      case COB_DISPLAY_SIGN_ASCII: PUT_SIGN_ASCII (*p); break;
	      case COB_DISPLAY_SIGN_ASCII10: PUT_SIGN_ASCII10 (*p); break;
	      default: abort ();
	      }
	  }
	return;
      }
    case COB_TYPE_NUMERIC_PACKED:
      {
	unsigned char *p = f->data + f->attr->digits / 2;
	if (sign < 0)
	  *p = (*p & 0xf0) | 0x0d;
	else
	  *p = (*p & 0xf0) | 0x0c;
	return;
      }
    default:
      return;
    }
}


char *
cob_field_to_string (cob_field *f, char *s)
{
  int i;
  memcpy (s, f->data, f->size);
  for (i = f->size - 1; i >= 0; i--)
    if (s[i] != ' ')
      break;
  s[i + 1] = '\0';
  return s;
}


/*
 * Switch
 */

static int cob_switch[8] = {1, 0, 1, 1, 1, 1, 1, 1};

int
cob_get_switch (int n)
{
  return cob_switch[n];
}

void
cob_set_switch (int n, int flag)
{
  cob_switch[n] = flag;
}


/*
 * Comparison
 */

static int
cmpc (unsigned char *s1, unsigned char c, size_t size)
{
  size_t i;
  int ret = 0;
  const unsigned char *s = cob_current_module->collating_sequence;
  if (s)
    {
      for (i = 0; i < size; i++)
	if ((ret = s[s1[i]] - s[c]) != 0)
	  return ret;
    }
  else
    {
      for (i = 0; i < size; i++)
	if ((ret = s1[i] - c) != 0)
	  return ret;
    }
  return ret;
}

static int
cmps (unsigned char *s1, unsigned char *s2, size_t size)
{
  size_t i;
  int ret = 0;
  const unsigned char *s = cob_current_module->collating_sequence;
  if (s)
    {
      for (i = 0; i < size; i++)
	if ((ret = s[s1[i]] - s[s2[i]]) != 0)
	  return ret;
    }
  else
    {
      for (i = 0; i < size; i++)
	if ((ret = s1[i] - s2[i]) != 0)
	  return ret;
    }
  return ret;
}

static int
cob_cmp_char (cob_field *f, unsigned char c)
{
  int sign = cob_get_sign (f);
  int ret = cmpc (f->data, c, f->size);
  cob_put_sign (f, sign);
  return ret;
}

static int
cob_cmp_all (cob_field *f1, cob_field *f2)
{
  int ret = 0;
  int sign = cob_get_sign (f1);
  size_t size = f1->size;
  unsigned char *data = f1->data;

  while (size >= f2->size)
    {
      if ((ret = cmps (data, f2->data, f2->size)) != 0)
	goto end;
      size -= f2->size;
      data += f2->size;
    }
  if (size > 0)
    ret = cmps (data, f2->data, size);

 end:
  cob_put_sign (f1, sign);
  return ret;
}

static int
cob_cmp_alnum (cob_field *f1, cob_field *f2)
{
  int ret = 0;
  int sign1 = cob_get_sign (f1);
  int sign2 = cob_get_sign (f2);
  size_t min = (f1->size < f2->size) ? f1->size : f2->size;

  /* compare common substring */
  if ((ret = cmps (f1->data, f2->data, min)) != 0)
    goto end;

  /* compare the rest (if any) with spaces */
  if (f1->size > f2->size)
    {
      if ((ret = cmpc (f1->data + min, ' ', f1->size - min)) != 0)
	goto end;
    }
  else
    {
      if ((ret = -cmpc (f2->data + min, ' ', f2->size - min)) != 0)
	goto end;
    }

 end:
  cob_put_sign (f1, sign1);
  cob_put_sign (f2, sign2);
  return ret;
}

int
cob_cmp (cob_field *f1, cob_field *f2)
{
  if (COB_FIELD_TYPE (f2) == COB_TYPE_ALPHANUMERIC_ALL)
    {
      if (f2 == &cob_zero && COB_FIELD_IS_NUMERIC (f1))
	return cob_cmp_int (f1, 0);
      else if (f2->size == 1)
	return cob_cmp_char (f1, f2->data[0]);
      else
	return cob_cmp_all (f1, f2);
    }
  else if (COB_FIELD_TYPE (f1) == COB_TYPE_ALPHANUMERIC_ALL)
    {
      if (f1 == &cob_zero && COB_FIELD_IS_NUMERIC (f2))
	return -cob_cmp_int (f2, 0);
      else if (f1->size == 1)
	return -cob_cmp_char (f2, f1->data[0]);
      else
	return -cob_cmp_all (f2, f1);
    }
  else
    {
      cob_field temp;
      cob_field_attr attr;
      unsigned char buff[18];
      if (COB_FIELD_IS_NUMERIC (f1) && COB_FIELD_IS_NUMERIC (f2))
	return cob_numeric_cmp (f1, f2);
      if (COB_FIELD_IS_NUMERIC (f1)
	  && COB_FIELD_TYPE (f1) != COB_TYPE_NUMERIC_DISPLAY)
	{
	  temp = (cob_field) {f1->attr->digits, buff, &attr};
	  attr = *f1->attr;
	  attr.type = COB_TYPE_NUMERIC_DISPLAY;
	  attr.flags &= ~COB_FLAG_HAVE_SIGN;
	  cob_move (f1, &temp);
	  f1 = &temp;
	}
      if (COB_FIELD_IS_NUMERIC (f2)
	  && COB_FIELD_TYPE (f2) != COB_TYPE_NUMERIC_DISPLAY)
	{
	  temp = (cob_field) {f2->attr->digits, buff, &attr};
	  attr = *f2->attr;
	  attr.type = COB_TYPE_NUMERIC_DISPLAY;
	  attr.flags &= ~COB_FLAG_HAVE_SIGN;
	  cob_move (f2, &temp);
	  f2 = &temp;
	}
      return cob_cmp_alnum (f1, f2);
    }
}

int
cob_cmp_int (cob_field *f1, int n)
{
  cob_field_attr attr = {COB_TYPE_NUMERIC_BINARY, 9, 0, COB_FLAG_HAVE_SIGN, NULL};
  cob_field temp = {sizeof (int), (unsigned char *) &n, &attr};
  return cob_numeric_cmp (f1, &temp);
}


/*
 * Class check
 */

int
cob_is_numeric (cob_field *f)
{
  switch (COB_FIELD_TYPE (f))
    {
    case COB_TYPE_NUMERIC_BINARY:
    case COB_TYPE_NUMERIC_FLOAT:
    case COB_TYPE_NUMERIC_DOUBLE:
      return 1;
    case COB_TYPE_NUMERIC_PACKED:
      {
	int i;
	int sign;
	/* check digits */
	for (i = 0; i < f->size - 1; i++)
	  if ((f->data[i] & 0xf0) > 0x90 || (f->data[i] & 0x0f) > 0x09)
	    return 0;
	if ((f->data[i] & 0xf0) > 0x90)
	  return 0;
	/* check sign */
	sign = f->data[i] & 0x0f;
	if (sign == 0x0f)
	  return 1;
	if (COB_FIELD_HAVE_SIGN (f))
	  if (sign == 0x0c || sign == 0x0d)
	    return 1;
	return 0;
      }
    case COB_TYPE_NUMERIC_DISPLAY:
      {
	int i;
	int sign = cob_get_sign (f);
	int size = COB_FIELD_SIZE (f);
	unsigned char *data = COB_FIELD_DATA (f);
	for (i = 0; i < size; i++)
	  if (!isdigit (data[i]))
	    {
	      cob_put_sign (f, sign);
	      return 0;
	    }
	cob_put_sign (f, sign);
	return 1;
      }
    default:
      {
	size_t i;
	for (i = 0; i < f->size; i++)
	  if (!isdigit (f->data[i]))
	    return 0;
	return 1;
      }
    }
}

int
cob_is_alpha (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !isalpha (f->data[i]))
      return 0;
  return 1;
}

int
cob_is_upper (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !isupper (f->data[i]))
      return 0;
  return 1;
}

int
cob_is_lower (cob_field *f)
{
  size_t i;
  for (i = 0; i < f->size; i++)
    if (!isspace (f->data[i]) && !islower (f->data[i]))
      return 0;
  return 1;
}


/*
 * Table sort
 */

static int sort_nkeys;
static cob_file_key *sort_keys;
static cob_field *sort_base;

static int
sort_compare (const void *data1, const void *data2)
{
  int i, cmp;
  for (i = 0; i < sort_nkeys; i++)
    {
      cob_field f1 = *sort_keys[i].field;
      cob_field f2 = *sort_keys[i].field;
      f1.data += ((unsigned char *) data1) - sort_base->data;
      f2.data += ((unsigned char *) data2) - sort_base->data;
      cmp = cob_cmp (&f1, &f2);
      if (cmp != 0)
	return (sort_keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
    }
  return 0;
}

void
cob_table_sort_init (int nkeys)
{
  sort_nkeys = 0;
  sort_keys = malloc (nkeys * sizeof (cob_file_key));
}

void
cob_table_sort_init_key (int flag, cob_field *field)
{
  sort_keys[sort_nkeys].flag = flag;
  sort_keys[sort_nkeys].field = field;
  sort_nkeys++;
}

void
cob_table_sort (cob_field *f, int n)
{
  sort_base = f;
  qsort (f->data, (size_t)n, f->size, sort_compare);
}


/*
 * Run-time error checking
 */

void
cob_runtime_error (const char *fmt, ...)
{
  va_list ap;

  /* prefix */
  if (cob_source_file)
    fprintf (stderr, "%s:%d: ", cob_source_file, cob_source_line);
  fputs ("libcob: ", stderr);

  /* body */
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);

  /* postfix */
  fputs ("\n", stderr);
  fflush (stderr);
}

void
cob_check_numeric (cob_field *f, const char *name)
{
  if (!cob_is_numeric (f))
    {
      size_t i;
      unsigned char *data = f->data;
      char buff[f->size * 4 + 1];
      char *p = buff;
      for (i = 0; i < f->size; i++)
	if (isprint (data[i]))
	  *p++ = data[i];
	else
	  p += sprintf (p, "\\%03o", data[i]);
      *p = '\0';
      cob_runtime_error (_("'%s' not numeric: '%s'"), name, buff);
      exit (1);
    }
}

void
cob_check_odo (int i, int min, int max, const char *name)
{
  /* check the OCCURS DEPENDING ON item */
  if (i < min || max < i)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_ODO);
      cob_runtime_error (_("OCCURS DEPENDING ON '%s' out of bounds: %d"),
			 name, i);
      exit (1);
    }
}

void
cob_check_subscript (int i, int min, int max, const char *name)
{
  /* check the subscript */
  if (i < min || max < i)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_SUBSCRIPT);
      cob_runtime_error (_("subscript of '%s' out of bounds: %d"), name, i);
      exit (1);
    }
}

void
cob_check_ref_mod (int offset, int length, int size, const char *name)
{
  /* check the offset */
  if (offset < 1 || offset > size)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_REF_MOD);
      cob_runtime_error (_("offset of '%s' out of bounds: %d"),
			 name, offset);
      exit (1);
    }

  /* check the length */
  if (length < 1 || offset + length - 1 > size)
    {
      COB_SET_EXCEPTION (COB_EC_BOUND_REF_MOD);
      cob_runtime_error (_("length of '%s' out of bounds: %d"),
			 name, length);
      exit (1);
    }
}

char *
cob_external_addr (char *exname, int exlength)
{
	static cob_external *basext = NULL;

	cob_external	*eptr;

	for ( eptr = basext; eptr; eptr = eptr->next ) {
		if ( !strcmp(exname, eptr->ename) ) {
			if ( exlength > eptr->esize ) {
				cob_runtime_error (_("EXTERNAL item '%s' has size > %d"), exname, exlength);
				exit (1);
			}
			return eptr->ext_alloc;
		}
	}
	eptr = (cob_external *)malloc(sizeof(cob_external));
	eptr->next = basext;
	eptr->esize = exlength;
	eptr->ename = malloc(strlen(exname) + 1);
	strcpy(eptr->ename, exname);
	eptr->ext_alloc = malloc(exlength);
	basext = eptr;
	return eptr->ext_alloc;
}
