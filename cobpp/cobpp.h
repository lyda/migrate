/* 
 * Copyright (C) 1999, 2000  David Essex.
 * Copyright (C) 1998, 1999  Laura Tweedy.
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

#ifndef _COBPP_H
#define _COBPP_H

#include <stdlib.h>
#include <stdio.h>

#define PATHMAX 		120
#define PATHMAX1 		1024
#define PATHMAX2 		256
#define CHR_SQUOTE		'\''
#define CHR_DQUOTE		'"'
#define CHR_SLASH		'/'
#define CHR_EOS			'\0'
#define CHR_ASTERIX		'*'
#define CHR_COLEN		':'
#define CHR_CR			'\r'
#define CHR_LF			'\n'
#define STR_DASH		'-'
#define STR_UNDERSCORE		'_'
#define STR_COLEN		":"
#define STR_SEARCH_SUFFIXES	".cpy:.CPY:.cob:.COB:.cbl:.CBL::"

#define STATE_FLIT_LNONE	1
#define STATE_FNAME_LNONE	2
#define STATE_FLIT_LLIT		3
#define STATE_FLIT_LNAME	4
#define STATE_FNAME_LLIT	5
#define STATE_FNAME_LNAME	6

extern char HTG_COPYDIR[];

typedef short bool;

struct s_Env
{
  bool codeFormat;		/* 1 is free , 0 is fixed */
  int debugFlag;
  int errFlag;
  int verboseFlag;
  int tab2space;
  char *progName;
  char *ifname;
  char *ofname;
  char *lfname;
};

typedef struct s_Env Env;

Env globalEnv;
Env *globalEnvPtr;

/* htcobol.c */
void yyerror (char *s);
int yyparse (void);

/* scan.c */
int yylex (void);
int yywrap (void);
void yyrestart (FILE * input_file);
int lex_fgets (char *buf, int maxsize);

extern void setup_scanner_state ();
extern char include_full_filename[];
int find_copybook_file (char *fname, char *lname);
int find_copybook_file2 (char *fname, char *lname);
int find_filename_text (char *fname, char *fp, char *fs);
int find_filename_literal (char *fname, char *fp);
char *find_env_variable (char *ev);
char *find_token (char *p, const char *d, int sw);

int setOptions (Env *, int, char **);
void setDefaults (void);
void printVersion (void);
void printHelp (void);
void CleanUp (void);

#endif
