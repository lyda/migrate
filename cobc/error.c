/*
 * Copyright (C) 2003 Keisuke Nishida
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "cobc.h"

static void
print_error (char *file, int line, const char *prefix, const char *fmt, va_list ap)
{
  static struct cb_label *last_section = NULL;
  static struct cb_label *last_paragraph = NULL;

  file = file ? file : cb_source_file;
  line = line ? line : cb_source_line;

  /* print the paragraph or section name */
  if (current_section != last_section
      || current_paragraph != last_paragraph)
    {
      if (current_paragraph)
	fprintf (stderr, _("%s: In paragraph `%s':\n"),
		 file, current_paragraph->name);
      else
	fprintf (stderr, _("%s: In section `%s':\n"),
		 file, current_section->name);
      last_section = current_section;
      last_paragraph = current_paragraph;
    }

  /* print the error */
  fprintf (stderr, "%s:%d: %s", file, line, prefix);
  vfprintf (stderr, fmt, ap);
  fputs ("\n", stderr);
}

void
cb_warning (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  print_error (0, 0, "warning: ", fmt, ap);
  va_end (ap);

  warningcount++;
}

void
cb_error (const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  print_error (0, 0, "", fmt, ap);
  va_end (ap);

  errorcount++;
}

void
cb_warning_x (cb_tree x, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  print_error (x->source_file, x->source_line, "warning: ", fmt, ap);
  va_end (ap);

  warningcount++;
}

void
cb_error_x (cb_tree x, const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  print_error (x->source_file, x->source_line, "", fmt, ap);
  va_end (ap);

  errorcount++;
}

void
cb_archaic (const char *feature)
{
  if (cb_warn_archaic)
    cb_warning (_("%s is archaic in %s"), feature, cb_standard_name);
}

static void
cb_obsolete (const char *feature)
{
  if (cb_warn_obsolete)
    cb_warning (_("%s is obsolete in %s"), feature, cb_standard_name);
}

static void
cb_unconformable (const char *feature)
{
  cb_error (_("%s not conform to %s"), feature, cb_standard_name);
}

void
cb_obsolete_85 (const char *feature)
{
  if (cb_standard == CB_STANDARD_COBOL2002)
    cb_unconformable (feature);
  else if (cb_standard == CB_STANDARD_COBOL85)
    cb_obsolete (feature);
}

void
cb_obsolete_2002 (const char *feature)
{
  if (cb_standard == CB_STANDARD_COBOL2002)
    cb_obsolete (feature);
}


void
redefinition_error (cb_tree x)
{
  struct cb_word *w = CB_REFERENCE (x)->word;
  cb_error_x (x, _("redefinition of `%s'"), w->name);
  cb_error_x (CB_VALUE (w->items), _("`%s' previously defined here"), w->name);
}

void
undefined_error (cb_tree x)
{
  struct cb_reference *r = CB_REFERENCE (x);
  if (r->chain)
    cb_error_x (x, _("`%s' undefined in `%s'"),
		r->word->name, CB_REFERENCE (r->chain)->word->name);
  else
    cb_error_x (x, _("`%s' undefined"), r->word->name);
}

void
ambiguous_error (cb_tree x)
{
  struct cb_word *w = CB_REFERENCE (x)->word;
  if (w->error == 0)
    {
      cb_tree list;

      /* display error on the first time */
      cb_error_x (x, _("`%s' ambiguous; need qualification"), w->name);
      w->error = 1;

      /* display all fields with the same name */
      for (list = w->items; list; list = CB_CHAIN (list))
	{
	  char buff[FILENAME_MAX];
	  cb_tree x = CB_LIST (list)->value;
	  sprintf (buff, "`%s' ", w->name);
	  switch (CB_TREE_TAG (x))
	    {
	    case CB_TAG_FIELD:
	      {
		struct cb_field *p;
		for (p = CB_FIELD (x)->parent; p; p = p->parent)
		  {
		    strcat (buff, "in `");
		    strcat (buff, p->name);
		    strcat (buff, "' ");
		  }
		break;
	      }
	    case CB_TAG_LABEL:
	      {
		struct cb_label *l = CB_LABEL (x);
		if (l->section)
		  {
		    strcat (buff, "in `");
		    strcat (buff, l->section->name);
		    strcat (buff, "' ");
		  }
		break;
	      }
	    default:
	      break;
	    }
	  strcat (buff, _("defined here"));
	  cb_error_x (x, buff);
	}
    }
}
