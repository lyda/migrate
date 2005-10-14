/*
 * Copyright (C) 2001-2005 Keisuke Nishida
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
#else
#if HAVE_PDCURSES_H
#include <pdcurses.h>
#else
#if HAVE_CURSES_H
#include <curses.h>
#endif
#endif
#endif

#include "move.h"
#include "screenio.h"

#define SCREEN_LINE_POS(s) \
  ((COB_SCREEN_LINE_CONST ? s->line.val : cob_get_int (s->line.ptr)) - 1)
#define SCREEN_COLUMN_POS(s) \
  ((COB_SCREEN_COLUMN_CONST ? s->column.val : cob_get_int (s->column.ptr)) - 1)

#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
static int	screen_initialized = 0;
#endif

void
cob_screen_init (void)
{
#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
	if (!screen_initialized) {
		initscr ();
		keypad (stdscr, TRUE);
		nonl ();
		cbreak ();
		echo ();
		screen_initialized = 1;
	}
#endif
}

void
cob_screen_terminate (void)
{
#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
	if (screen_initialized) {
		endwin ();
	}
#endif
}

void
cob_screen_attr (int line, int column, long attr)
{
}

void
cob_screen_puts (const char *data, size_t size, int line, int column, long attr)
{
#if HAVE_LIBNCURSES || HAVE_LIBPDCURSES
	mvaddnstr (line, column, data, size);
#endif
}

void
cob_screen_gets (char *data, size_t size, int line, int column, long attr)
{
#if HAVE_LIBNCURSES
	mvgetnstr (line, column, data, size);
#endif
#if HAVE_LIBPDCURSES
	mvgetstr (line, column, data);
#endif
}

void
cob_screen_display (cob_screen *s, int line, int column)
{
	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
	{
		for (s = s->data.child; s; s = s->next) {
			cob_screen_display (s, line, column);
		}
		break;
	}
	case COB_SCREEN_TYPE_FIELD:
	{
		if (s->from) {
			int line = SCREEN_LINE_POS (s);
			int column = SCREEN_COLUMN_POS (s);

			cob_move (s->from, s->data.field);
			cob_screen_puts ((char *)s->data.field->data, s->data.field->size,
					 line, column, s->attr);
		}
		break;
	}
	case COB_SCREEN_TYPE_VALUE:
	{
		int line = SCREEN_LINE_POS (s);
		int column = SCREEN_COLUMN_POS (s);

		cob_screen_puts (s->data.value, strlen (s->data.value), line, column, s->attr);
		break;
	}
	case COB_SCREEN_TYPE_ATTRIBUTE:
	{
		int line = SCREEN_LINE_POS (s);
		int column = SCREEN_COLUMN_POS (s);

		cob_screen_attr (line, column, s->attr);
		break;
	}
	}
}

void
cob_screen_accept (cob_screen *s, int line, int column)
{
	switch (s->type) {
	case COB_SCREEN_TYPE_GROUP:
	{
		for (s = s->data.child; s; s = s->next) {
			cob_screen_accept (s, line, column);
		}
		break;
	}
	case COB_SCREEN_TYPE_FIELD:
	{
		if (s->to) {
			int line = SCREEN_LINE_POS (s);
			int column = SCREEN_COLUMN_POS (s);

			cob_screen_gets ((char *)s->data.field->data, s->data.field->size,
					 line, column, s->attr);
			cob_move (s->data.field, s->to);
		}
		break;
	}
	case COB_SCREEN_TYPE_VALUE:
	case COB_SCREEN_TYPE_ATTRIBUTE:
	{
		/* nothing to accept */
		break;
	}
	}
}
