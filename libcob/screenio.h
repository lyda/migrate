/*
 * Copyright (C) 2002-2007 Keisuke Nishida
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

#ifndef COB_SCREENIO_H
#define COB_SCREENIO_H

#include <libcob/common.h>

#define COB_SCREEN_BG_BLACK	0x00000000
#define COB_SCREEN_BG_BLUE	0x00000001
#define COB_SCREEN_BG_GREEN	0x00000002
#define COB_SCREEN_BG_CYAN	0x00000003
#define COB_SCREEN_BG_RED	0x00000004
#define COB_SCREEN_BG_MAGENTA	0x00000005
#define COB_SCREEN_BG_YELLOW	0x00000006
#define COB_SCREEN_BG_WHITE	0x00000007
#define COB_SCREEN_BG_NONE	0x00000008
#define COB_SCREEN_BG_MASK	0x0000000f

#define COB_SCREEN_FG_BLACK	0x00000000
#define COB_SCREEN_FG_BLUE	0x00000010
#define COB_SCREEN_FG_GREEN	0x00000020
#define COB_SCREEN_FG_CYAN	0x00000030
#define COB_SCREEN_FG_RED	0x00000040
#define COB_SCREEN_FG_MAGENTA	0x00000050
#define COB_SCREEN_FG_YELLOW	0x00000060
#define COB_SCREEN_FG_WHITE	0x00000070
#define COB_SCREEN_FG_NONE	0x00000080
#define COB_SCREEN_FG_MASK	0x000000f0

#define COB_SCREEN_LINE_ABS	0x00000000
#define COB_SCREEN_LINE_PLUS	0x00000100
#define COB_SCREEN_LINE_MINUS	0x00000200
#define COB_SCREEN_LINE_MASK	0x00000300
#define COB_SCREEN_LINE_CONST	0x00000400

#define COB_SCREEN_COLUMN_ABS	0x00000000
#define COB_SCREEN_COLUMN_PLUS	0x00001000
#define COB_SCREEN_COLUMN_MINUS	0x00002000
#define COB_SCREEN_COLUMN_MASK	0x00003000
#define COB_SCREEN_COLUMN_CONST	0x00004000

#define COB_SCREEN_AUTO		0x00010000
#define COB_SCREEN_BELL		0x00020000
#define COB_SCREEN_BLANK_LINE	0x00040000
#define COB_SCREEN_BLANK_SCREEN	0x00080000
#define COB_SCREEN_BLINK	0x00100000
#define COB_SCREEN_ERASE_EOL	0x00200000
#define COB_SCREEN_ERASE_EOS	0x00400000
#define COB_SCREEN_FULL		0x00800000
#define COB_SCREEN_HIGHLIGHT	0x01000000
#define COB_SCREEN_LOWLIGHT	0x02000000
#define COB_SCREEN_REQUIRED	0x04000000
#define COB_SCREEN_REVERSE	0x08000000
#define COB_SCREEN_SECURE	0x10000000
#define COB_SCREEN_UNDERLINE	0x20000000
#define COB_SCREEN_OVERLINE	0x40000000

typedef struct __cob_screen cob_screen;

typedef enum {
	COB_SCREEN_TYPE_GROUP,
	COB_SCREEN_TYPE_FIELD,
	COB_SCREEN_TYPE_VALUE,
	COB_SCREEN_TYPE_ATTRIBUTE
} cob_screen_type;

struct __cob_screen {
	cob_screen_type		type;
	cob_screen		*child;		/* for COB_SCREEN_TYPE_GROUP */
	cob_field		*field;		/* for COB_SCREEN_TYPE_FIELD */
	cob_field		*value;		/* for COB_SCREEN_TYPE_VALUE */
	cob_screen		*next;
	cob_field		*from;
	cob_field		*to;
	cob_field		*line;
	cob_field		*column;
	long			attr;		/* for COB_SCREEN_TYPE_ATTRIBUTE */
};

DLL_EXPIMP extern int	cob_screen_mode;
DLL_EXPIMP extern int	cob_current_x;
DLL_EXPIMP extern int	cob_current_y;

extern void cob_screen_init (void);
extern void cob_screen_terminate (void);
extern void cob_screen_puts (const char *data, size_t size, int line, int column, long attr);
extern void cob_screen_gets (char *data, size_t size, int line, int column, long attr);
extern void cob_screen_display (cob_screen *s, cob_field *line, cob_field *column);
extern void cob_screen_accept (cob_screen *s, cob_field *line, cob_field *column);
extern void cob_field_display (cob_field *f, cob_field *line, cob_field *column, long attr);
extern void cob_field_accept (cob_field *f, cob_field *line, cob_field *column);

#endif /* COB_SCREENIO_H */
