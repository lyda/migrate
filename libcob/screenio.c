/*
 * Copyright (C) 2001-2006 Keisuke Nishida
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

#include <string.h>

#if HAVE_NCURSES_H
#include <ncurses.h>
#elif HAVE_NCURSES_NCURSES_H
#include <ncurses/ncurses.h>
#elif HAVE_CURSES_H
#include <curses.h>
#elif HAVE_PDCURSES_H
#include <pdcurses.h>
#define mvgetnstr(w, x, y, z)	mvgetstr(w, x, y)
#endif

#include "move.h"
#include "screenio.h"

/*
#define SCREEN_LINE_POS(s) \
  ((COB_SCREEN_LINE_CONST ? s->line.val : cob_get_int (s->line.ptr)) - 1)
#define SCREEN_COLUMN_POS(s) \
  ((COB_SCREEN_COLUMN_CONST ? s->column.val : cob_get_int (s->column.ptr)) - 1)
*/
#define SCREEN_LINE_POS(s)	(cob_get_int (s->line) - 1)
#define SCREEN_COLUMN_POS(s)	(cob_get_int (s->column) - 1)

#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
static int	screen_initialized = 0;
#endif

int	cob_screen_mode = 0;
int	cob_current_y = 0;
int	cob_current_x = 0;

#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
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
	sline = cob_get_int (line);
	scolumn = cob_get_int (column);
	mvaddnstr (sline, scolumn, f->data, f->size);
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
	sline = cob_get_int (line);
	scolumn = cob_get_int (column);
	mvgetnstr (sline, scolumn, f->data, f->size);
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
		cob_screen_puts (s->value->data, strlen (s->value->data), sline, scolumn, s->attr);
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
cob_field_display (cob_field *f, int line, int column)
{
}

void
cob_field_accept (cob_field *f, int line, int column)
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
