/* COBOL scanner 					-*- c -*-
 *
 * Copyright (C) 2001  Keisuke Nishida
 * Copyright (C) 2000  Rildo Pragana, Alan Cox, Andrew Cameron,
 *		      David Essex, Glen Colbert, Jim Noeth.
 * Copyright (C) 1999  Rildo Pragana, Alan Cox, Andrew Cameron, David Essex.
 * Copyright (C) 1991, 1993  Rildo Pragana.
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

%option noyywrap

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cobc.h"
#include "codegen.h"
#include "parser.h"
#include "reserved.h"
#include "_libcob.h"

#define MAX_COPY_DEPTH          10
#define MAX_CPYNAME_LEN         65
#define MAXLINEBUF              256

#ifdef COB_DEBUG
static void trace_print (char *fmt, ...);
#define TRACE_PRINT(args)	trace_print args
#else
#define TRACE_PRINT(args)
#endif

extern FILE *o_lst;
static int lex_fgets (char *buf, int maxsize);

#define YY_INPUT(buf,result,max_size) \
  result = lex_fgets (buf, max_size);

unsigned int varnumber;
int previous_state = 0;
int stabs_started=0;

static int read_literal (char mark);
static int read_literal_x (char mark);
static int scan_word (char *text);
static int scan_reserved_word (char *text);
%}

digit [0-9]
relop [<>=]
white [\r\t ]
sdelim \"
ldelim \'
any [!-~]

word [A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9]+)?

%x IDENT_ST ENVIR_ST DATA_ST COMMENT_ST FD_ST REDEF_ST
%x SUBSCRIPTS_ST EXCEPTION_ST
%x PIC_ST PIC_ST2

%%

%{
        extern int curr_division;
        extern int need_subscripts;

        switch (curr_division) {
        case CDIV_IDENT:
                TRACE_PRINT (("-> IDENT_ST\n"));
                BEGIN IDENT_ST;
                break;
        case CDIV_ENVIR:
                TRACE_PRINT (("-> ENVIR_ST\n"));
                BEGIN ENVIR_ST;
                break;
        case CDIV_DATA:
                TRACE_PRINT (("-> DATA_ST\n"));
                BEGIN DATA_ST;
                break;
        case CDIV_PROC:
                //stabs_started=1;
                TRACE_PRINT (("-> INITIAL\n"));
                BEGIN INITIAL;
                break;
        case CDIV_COMMENT:
                TRACE_PRINT (("-> COMMENT_ST\n"));
                BEGIN COMMENT_ST;
                break;
        case CDIV_FD:
                TRACE_PRINT (("-> FD_ST\n"));
                BEGIN FD_ST;
                break;
        case CDIV_REDEF:
                TRACE_PRINT (("-> REDEF_ST\n"));
                BEGIN REDEF_ST;
                break;
        case CDIV_SUBSCRIPTS:
                TRACE_PRINT (("-> SUBSCRIPTS_ST\n"));
                BEGIN SUBSCRIPTS_ST;
                break;
        case CDIV_EXCEPTION:
                TRACE_PRINT (("-> EXCEPTION_ST\n"));
                BEGIN EXCEPTION_ST;
                break;
        case CDIV_PIC:
                TRACE_PRINT (("-> PIC_ST\n"));
                BEGIN PIC_ST;
                break;
        case CINITIAL:
                TRACE_PRINT (("-> INITIAL\n"));
                BEGIN INITIAL;
                break;
        }
        curr_division=0; /* avoid new state switch */
%}

<*>^"#".* {
  char *endp;
  cob_orig_lineno = strtol (yytext + 2, &endp, 10) - 1;
  if (cob_orig_filename)
    free (cob_orig_filename);
  cob_orig_filename = strdup (strchr (endp, '"') + 1);
  strrchr (cob_orig_filename, '"')[0] = '\0';
}

<*>^"D ".* {
  if (cob_debug_flag)
    yyless(2);
  else
    TRACE_PRINT (("<comment>: %s:\n", yytext));
}

"," {
  return ',';
}

"(" {
  if (need_subscripts) {
    TRACE_PRINT (("<LPAR>:\n"));
    need_subscripts=0;
    return (LPAR);
  }
  else
    return ('(');
}

{white}+ { /* ignore */ }

<INITIAL,SUBSCRIPTS_ST>{word} {
  struct sym *symbol;
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  /* if symbol was defined at the data division */
  if ((symbol = lookup_symbol (yytext)) != NULL)
    {
      yylval.sval = symbol;
      if (symbol->type == '8')
	{
	  TRACE_PRINT (("<VARCOND>: %s\n", yytext));
	  update_xreflist (yylval.sval);
	  return (VARCOND);
	}
      else
	{
	  if (is_subscripted (symbol))
	    {
	      TRACE_PRINT (("<SUBSCVAR>: %s\n", yytext));
	      return (SUBSCVAR);
	    }
	  TRACE_PRINT (("<VARIABLE>: %s\n", yytext));
	  update_xreflist (yylval.sval);
	  return (VARIABLE);
	}
    }
  /* otherwise, it should be a label */
  else
    {
      yylval.sval = install_label (yytext);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
}

<ENVIR_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  TRACE_PRINT (("<STRING>: %s\n", yytext));
  yylval.sval = install (yytext, SYTB_VAR, 0);
  return (STRING);
}

<EXCEPTION_ST>{word} {
  struct sym *symbol;
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  /* if symbol was defined at the data division */
  if ((symbol = lookup_symbol (yytext)) != NULL)
    {
      yylval.sval = symbol;
      if (symbol->type == '8')
	{
	  TRACE_PRINT (("<VARCOND>: %s\n", yytext));
	  update_xreflist (yylval.sval);
	  return (VARCOND);
	}
      else
	{
	  TRACE_PRINT (("<VARIABLE>: %s\n", yytext));
	  update_xreflist (yylval.sval);
	  return (VARIABLE);
	}

    }
  /* otherwise, it should be a label */
  else
    {
      yylval.sval = install_label (yytext);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
}


<FD_ST>{
  {word} {
    int tok = scan_word (yytext);
    if (tok)
      return tok;

    TRACE_PRINT (("<STRING>: %s\n", yytext));
    if ((yylval.sval = lookup_symbol (yytext)) == NULL)
      yyerror ("undefined %s at FD/SD", yytext);
    return (STRING);
  }

  [ \t]* { }
}

<REDEF_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  TRACE_PRINT (("<STRING>: %s\n", yytext));
  /* don't clone redefined var */
  yylval.sval = install (yytext, SYTB_VAR, 0);
  update_xreflist (yylval.sval);
  return (STRING);
}

<DATA_ST>{word} {
  int tok = scan_word (yytext);
  if (tok)
    return tok;

  TRACE_PRINT (("<STRING>: %s\n", yytext));
  /* install w/clone if neccessary */
  yylval.sval = install (yytext, SYTB_VAR, 1);
  update_xreflist (yylval.sval);
  return (STRING);
}

<SUBSCRIPTS_ST>{
  {any}   {
    TRACE_PRINT (("<0: any>: %s\n", yytext));
    return(yytext[0]); 
  }
  {white}+ { }
}

<IDENT_ST>{
  [ \t]* { }
  "."/[ \t\n] { 
    TRACE_PRINT (("<EOS>:\n"));
    return(EOS); 
  }

  {word} {
    int tok = scan_word (yytext);
    if (tok)
      return tok;

    TRACE_PRINT ((" <IDSTRING>: %s\n", yytext));
    yylval.str = strdup (yytext);
    return IDSTRING;
  }

  {any}   {
    TRACE_PRINT (("<0: any>: %s\n", yytext));
    return(yytext[0]); 
  }

  "\n" { }
} /* end of <IDENT_ST> */

<INITIAL,ENVIR_ST,DATA_ST,REDEF_ST,EXCEPTION_ST>{

  "."/[ \t\n] { 
		  TRACE_PRINT (("<EOS>:\n"));
		  return(EOS); 
	  }

  {ldelim}        { return read_literal ('\''); }
  {sdelim}        { return read_literal ('\"'); }
  X{ldelim}       { return read_literal_x ('\''); }
  X{sdelim}       { return read_literal_x ('\"'); }

  [0-9]*[.,]?[0-9]+ {
    TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
    yylval.lval = install_literal (yytext);
    return(NLITERAL);
  }

  {white}+ { }
  "\n"   { }

  {relop}{relop}? {
    int token;
    if ((token = scan_reserved_word (yytext)) != 0)
      return token;
    yyerror("unknown operator: `%s'", yytext);
  }

  "(" {
    TRACE_PRINT (("<0: lpar>: %s\n", yytext));
    return(yytext[0]); 
  }
  "**" {
    TRACE_PRINT (("<0: POW_OP>: %s\n", yytext));
    return(POW_OP); 
  }
  {any} {
    TRACE_PRINT (("<0: any>: %s\n", yytext));
    return(yytext[0]); 
  }

} /* end of <INITIAL,DATA_ST,ENVIR_ST> */

<COMMENT_ST>{
  .*      { 
	   TRACE_PRINT ((" <COMMENT_ST>: %s\n", yytext));
	  }
  "\n"   { BEGIN INITIAL; }
} /* end of COMMENT_ST */

<PIC_ST>{white}* { 
                BEGIN PIC_ST2;
                TRACE_PRINT (("<PICS -> PIC_ST2>:\n"));
        }

<PIC_ST2>{
  {white}* { 
		  BEGIN DATA_ST;
		  TRACE_PRINT (("<PICS -> DATA_ST>:\n"));
	  }
  "."/[ \t\n] { 
		  BEGIN DATA_ST;
		  TRACE_PRINT (("<PICS -> DATA_ST>: EOS\n"));
		  return EOS;
	  }
}

<PIC_ST,PIC_ST2>{
  "("[0-9]+")" {
    {
      char *p; 
      int val = 0;
      for (p = yytext + 1; *p != ')'; p++)
	val = 10 * val + *p - '0';
      TRACE_PRINT (("<PICS: MULTIPLIER>: %d\n", val));
      yylval.ival = val;
      return MULTIPLIER;
    }
  }
  {word} {
    int token;
    if ((token = scan_reserved_word (yytext)) != 0) 
      {
	BEGIN PIC_ST;
	TRACE_PRINT (("<PIC_ST2-> PIC_ST>:\n"));
	return token;
      }
    REJECT;
  }
  {any} { 
    TRACE_PRINT (("<PICS: any char>: %c\n", yytext[0]));
    yylval.ival=(int)yytext[0];
    return(CHAR);
  }
  "\n" {
  }
}

<<EOF>> {
  yyterminate();
}

%%

static int
read_literal (char mark)
{
  int c;
  char buff[BUFSIZ];
  char *p = buff;

  while ((c = input ()) != EOF)
    {
      *p++ = c;
      if (c == mark && (c = input ()) != mark)
	{
	  p--;
	  unput (c);
	  break;
	}
      if (p - buff >= BUFSIZ - 1)
	yyerror ("too large literal");
    }
  *p = 0;
  TRACE_PRINT (("<CLITERAL>: %s\n", buff));
  yylval.lval = install_literal (buff);
  return CLITERAL;
}

static int
read_literal_x (char mark)
{
  unsigned char hnum[512];
  unsigned char *s = hnum;
  char toktext[1024];
  char *tp = toktext;
  while ((*tp = toupper (input ())) != mark)
    {
      if (tp >= toktext + 1024)
	{
	  yyerror ("scanner error: token too large");
	  while (input () != mark);
	  *tp = 0;
	  TRACE_PRINT (("<CLITERAL>: %s\n", toktext));
	  yylval.lval = install_literal (toktext);
	  return (CLITERAL);
	}
      *s = (*tp > '9') ? (*tp - 'A' + 0x0a) : (*tp - '0');
      if ((*tp = toupper (input ())) == mark)
	{
	  break;
	}
      *s++ = *s * 0x10 + ((*tp > '9') ? (*tp - 'A' + 0x0a) : (*tp - '0'));
    }
  {
    char buff[1024];
    strncpy (buff, hnum, s - hnum);
    buff[s - hnum] = '\0';
    yylval.lval = install_literal (buff);
  }
  TRACE_PRINT (("<CLITERAL>: %s\n", toktext));
  return CLITERAL;
}

static int
is_integer_token (const char *tok)
{
  while (*tok)
    if (!isdigit (*tok++))
      return 0;
  return 1;
}

static int
is_valid_identifier (const char *tok)
{
  while (*tok)
    if (isalpha (*tok++))
      return 1;
  return 0;
}

static int
scan_word (char *text)
{
  int token;

  if (is_integer_token (text))
    {
      yylval.lval = install_literal (text);
      TRACE_PRINT (("<NLITERAL>: %s\n", text));
      return (NLITERAL);
    }
  else if (!is_valid_identifier (text))
    {
      yylval.sval = install_label (text);
      TRACE_PRINT (("<LABELSTR>: %s\n", text));
      return (LABELSTR);
    }
  else if ((token = scan_reserved_word (text)) != 0)
    return token;
  else
    return 0;
}

static int
scan_reserved_word (char *s)
{
  struct reserved_word *p = lookup_reserved_word (s);
  if (!p)
    return 0;
  TRACE_PRINT ((" <reserved>: %s\n", p->name));
  yylval.ival = p->minor;
  return p->token;
}

static int
lex_fgets (char *buf, int maxsize)
{
  if (fgets (buf, maxsize, yyin) == NULL)
    return YY_NULL;

  cob_orig_lineno++;
  return strlen (buf);
}

#ifdef COB_DEBUG
static void
trace_print (char *fmt, ...)
{
  if (cob_trace_scanner)
    {
      va_list args;
      va_start (args, fmt);
      fprintf (stderr, "SCAN(%4d):", cob_orig_lineno);
      vfprintf (stderr, fmt, args);
      va_end (args);
    }
}
#endif

void
dummy ()
{
  /* This is to avoid warnings */
  yy_flex_realloc (0, 0);
}
