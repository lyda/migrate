/*
 * Copyright (C) 2001-2002 Keisuke Nishida
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

#ifndef COBPP_SCANNER_H
#define COBPP_SCANNER_H

#include <stdio.h>

struct replacement {
  const char *old_text;
  const char *new_text;
  struct replacement *next;
};

extern FILE *yyin, *yyout;
extern char *yytext;
extern int yylineno;
extern const char *yyfilename;

extern int yylex (void);
extern void yywarn (const char *fmt, ...);
extern void yyerror (const char *fmt, ...);
extern void open_buffer (const char *name, struct replacement *replacing);
extern void include_copybook (const char *name, const char *lib, struct replacement *replacing);
extern struct replacement *add_replacement (struct replacement *replacing, const char *old_text, const char *new_text);
extern void remove_replacements (void);

#endif /* COBPP_SCANNER_H */
