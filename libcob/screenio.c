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
static short	fore_color;
static short	back_color;
static size_t	cob_has_color = 0;
#endif

int	screen_initialized = 0;
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

static void
cob_screen_attr (long attr)
{
	chtype		styles;
	size_t		i;
	short		fgcolor;
	short		bgcolor;
	short		fgdef;
	short		bgdef;

	styles = 0;
	attrset (A_NORMAL);
	if (attr & COB_SCREEN_BLANK_SCREEN) {
		clear ();
	}
	if (attr & COB_SCREEN_ERASE_EOL) {
		clrtoeol ();
	}
	if (attr & COB_SCREEN_ERASE_EOS) {
		clrtobot ();
	}
	if (attr & COB_SCREEN_BELL) {
		beep ();
	}
	if (attr & COB_SCREEN_REVERSE) {
		styles |= A_REVERSE;
	}
	if (attr & COB_SCREEN_HIGHLIGHT) {
		styles |= A_BOLD;
	}
	if (attr & COB_SCREEN_BLINK) {
		styles |= A_BLINK;
	}
	if (attr & COB_SCREEN_UNDERLINE) {
		styles |= A_UNDERLINE;
	}
	attron (styles);
	if (cob_has_color) {
		fgcolor = fore_color;
		bgcolor = back_color;
		if (attr & COB_SCREEN_FG_MASK) {
			switch(attr & COB_SCREEN_FG_MASK) {
			case COB_SCREEN_FG_BLACK:
				fgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_FG_BLUE:
				fgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_FG_GREEN:
				fgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_FG_CYAN:
				fgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_FG_RED:
				fgcolor = COLOR_RED;
				break;
			case COB_SCREEN_FG_MAGENTA:
				fgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_FG_YELLOW:
				fgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_FG_WHITE:
				fgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		if (attr & COB_SCREEN_BG_MASK) {
			switch(attr & COB_SCREEN_BG_MASK) {
			case COB_SCREEN_BG_BLACK:
				bgcolor = COLOR_BLACK;
				break;
			case COB_SCREEN_BG_BLUE:
				bgcolor = COLOR_BLUE;
				break;
			case COB_SCREEN_BG_GREEN:
				bgcolor = COLOR_GREEN;
				break;
			case COB_SCREEN_BG_CYAN:
				bgcolor = COLOR_CYAN;
				break;
			case COB_SCREEN_BG_RED:
				bgcolor = COLOR_RED;
				break;
			case COB_SCREEN_BG_MAGENTA:
				bgcolor = COLOR_MAGENTA;
				break;
			case COB_SCREEN_BG_YELLOW:
				bgcolor = COLOR_YELLOW;
				break;
			case COB_SCREEN_BG_WHITE:
				bgcolor = COLOR_WHITE;
				break;
			default:
				break;
			}
		}
		for (i = 0; i < COLOR_PAIRS; i++) {
			pair_content (i, &fgdef, &bgdef);
			if (fgdef == fgcolor && bgdef == bgcolor) {
				break;
			}
			if (fgdef == 0 && bgdef == 0) {
				init_pair (i, fgcolor, bgcolor);
				break;
			}
		}
		if (i != COLOR_PAIRS) {
			attrset (COLOR_PAIR(i));
			bkgdset (COLOR_PAIR(i));
		} else {
			attrset (A_NORMAL);
		}
	}
}

void
cob_screen_init (void)
{
	if (!screen_initialized) {
		if (!initscr ()) {
			cob_runtime_error ("Failed to initialize curses");
			cob_stop_run (1);
		}
		cbreak ();
		keypad (stdscr, TRUE);
		nonl ();
		echo ();
/* RXW
		noecho ();
		scrollok (stdscr, TRUE);
*/
		if (has_colors ()) {
			start_color ();
			pair_content (0, &fore_color, &back_color);
			if (COLOR_PAIRS) {
				cob_has_color = 1;
			}
		}
		attrset (A_NORMAL);
		screen_initialized = 1;
	}
}

void
cob_screen_terminate (void)
{
	if (screen_initialized) {
		screen_initialized = 0;
		endwin ();
	}
}

void
cob_screen_puts (const char *data, size_t size, int line, int column, long attr)
{
	if (!screen_initialized) {
		cob_screen_init ();
	}
	cob_screen_attr (attr);
	mvaddnstr (line, column, data, size);
	refresh ();
}

void
cob_screen_gets (char *data, size_t size, int line, int column, long attr)
{
	cob_screen_attr (attr);
	mvgetnstr (line, column, data, size);
}


void
cob_field_display (cob_field *f, cob_field *line, cob_field *column, long attr)
{
	int sline;
	int scolumn;

	if (!screen_initialized) {
		cob_screen_init ();
	}

	get_line_column (line, column, &sline, &scolumn);
	cob_screen_attr (attr);
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
		cob_screen_attr (s->attr);
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
cob_field_display (cob_field *f, cob_field *line, cob_field *column, long attr)
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
