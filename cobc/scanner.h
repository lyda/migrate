/*
 * Copyright (C) 2001  Keisuke Nishida
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

#ifndef _SCANNER_H_
#define _SCANNER_H_

#include "tree.h"

extern char *cobc_source_file;
extern int cobc_source_line;
extern int cobc_last_line;
extern char cobc_last_text[];

extern int cobc_skip_comment;
extern int cobc_in_procedure;

extern int yylex (void);
extern struct cobc_picture *yylex_picture (char *str);

extern void yywarn (char *fmt, ...);
extern void yyerror (char *fmt, ...);
extern void yywarn_loc (YYLTYPE *loc, char *fmt, ...);
extern void yyerror_loc (YYLTYPE *loc, char *fmt, ...);
extern void yywarn_tree (cobc_tree x, char *fmt, ...);
extern void yyerror_tree (cobc_tree x, char *fmt, ...);

#endif /* _SCANNER_H_ */
