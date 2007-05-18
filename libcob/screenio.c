/*
 * Copyright (C) 2001-2007 Keisuke Nishida
 * Copyright (C) 2007 Roger While
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
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if HAVE_NCURSES_H
#include <ncurses.h>
#elif HAVE_NCURSES_NCURSES_H
#include <ncurses/ncurses.h>
#elif HAVE_PDCURSES_H
#include <pdcurses.h>
#define mvgetnstr(w, x, y, z)	mvgetstr(w, x, y)
#elif HAVE_CURSES_H
#include <curses.h>
#endif

#include "move.h"
#include "coblocal.h"
#include "screenio.h"

/*
#define SCREEN_LINE_POS(s) \
  ((COB_SCREEN_LINE_CONST ? s->line.val : cob_get_int (s->line.ptr)) - 1)
#define SCREEN_COLUMN_POS(s) \
  ((COB_SCREEN_COLUMN_CONST ? s->column.val : cob_get_int (s->column.ptr)) - 1)
*/
#define SCREEN_LINE_POS(s)	(cob_get_int (s->line) - 1)
#define SCREEN_COLUMN_POS(s)	(cob_get_int (s->column) - 1)

#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES || HAVE_LIBCURSES
static int	screen_initialized = 0;
#endif

int	cob_screen_mode = 0;
int	cob_current_y = 0;
int	cob_current_x = 0;

#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES || HAVE_LIBCURSES

static int
get_line_column (cob_field *fline, cob_field *fcol, int *line, int *col)
{
	int	l = 0;
	int	c = 0;
	int	p = 0;

	if (fline == NULL) {
		return -1;
	}

	p = cob_get_int (fline);

	if (fcol == NULL) {
		p = cob_get_int (fline);
		if (fline->size == 4) {
			l = p / 100;
			c = p % 100;
		} else {
			l = p / 1000;
			c = p % 1000;
		}
	} else {
		l = p;
		c = cob_get_int (fcol);
	}
	if (l > 0) {
		l--;
	}
	if (c > 0) {
		c--;
	}
	*line = l;
	*col = c;
	return 0;
}

void
cob_screen_init (void)
{
	if (!screen_initialized) {
		initscr ();
		keypad (stdscr, TRUE);
		nonl ();
		cbreak ();
		echo ();
		screen_initialized = 1;
	}
}

void
cob_screen_terminate (void)
{
	if (screen_initialized) {
		endwin ();
	}
}

void
cob_screen_attr (int line, int column, long attr)
{
}

void
cob_screen_puts (const char *data, size_t size, int line, int column, long attr)
{
	if (!screen_initialized) {
		cob_screen_init ();
	}
	mvaddnstr (line, column, data, size);
	refresh ();
}

void
cob_screen_gets (char *data, size_t size, int line, int column, long attr)
{
	mvgetnstr (line, column, data, size);
}


void
cob_field_display (cob_field *f, cob_field *line, cob_field *column)
{
	int sline;
	int scolumn;

	if (!screen_initialized) {
		cob_screen_init ();
	}

	get_line_column (line, column, &sline, &scolumn);
	mvaddnstr (sline, scolumn, (char *)f->data, f->size);

	refresh ();
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column)
{
	int sline;
	int scolumn;

	if (!screen_initialized) {
		cob_screen_init ();
	}

	get_line_column (line, column, &sline, &scolumn);
	mvgetnstr (sline, scolumn, (char *)f->data, f->size);
	refresh ();
}

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column)
{
	int sline;
	int scolumn;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		for (s = s->child; s; s = s->next) {
			cob_screen_display (s, line, column);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		if (s->from) {
			sline = SCREEN_LINE_POS (s);
			scolumn = SCREEN_COLUMN_POS (s);
			cob_move (s->from, s->field);
			cob_screen_puts ((char *)s->field->data, s->field->size,
					 sline, scolumn, s->attr);
		}
		break;
	case COB_SCREEN_TYPE_VALUE:
		sline = SCREEN_LINE_POS (s);
		scolumn = SCREEN_COLUMN_POS (s);
		cob_screen_puts ((char *)(s->value->data), strlen ((char *)(s->value->data)), sline, scolumn, s->attr);
		break;
	case COB_SCREEN_TYPE_ATTRIBUTE:
		sline = SCREEN_LINE_POS (s);
		scolumn = SCREEN_COLUMN_POS (s);
		cob_screen_attr (sline, scolumn, s->attr);
		break;
	}
	refresh ();
}

void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column)
{
	int sline;
	int scolumn;

	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
		for (s = s->child; s; s = s->next) {
			cob_screen_accept (s, line, column);
		}
		break;
	case COB_SCREEN_TYPE_FIELD:
		if (s->to) {
			sline = SCREEN_LINE_POS (s);
			scolumn = SCREEN_COLUMN_POS (s);
			cob_screen_gets ((char *)s->field->data, s->field->size,
					 sline, scolumn, s->attr);
			cob_move (s->field, s->to);
		}
		break;
	case COB_SCREEN_TYPE_VALUE:
	case COB_SCREEN_TYPE_ATTRIBUTE:
		/* nothing to accept */
		break;
	}
}

#else

void
cob_screen_init (void)
{
}

void
cob_screen_terminate (void)
{
}

void
cob_screen_attr (int line, int column, long attr)
{
}

void
cob_screen_puts (const char *data, size_t size, int line, int column, long attr)
{
}

void
cob_screen_gets (char *data, size_t size, int line, int column, long attr)
{
}

void
cob_field_display (cob_field *f, cob_field *line, cob_field *column)
{
}

void
cob_field_accept (cob_field *f, cob_field *line, cob_field *column)
{
}

void
cob_screen_display (cob_screen *s, cob_field *line, cob_field *column)
{
}

void
cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column)
{
}

#endif
