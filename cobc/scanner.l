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
int lex_fgets( char *buf, int maxsize );

#define YY_INPUT(buf,result,max_size) \
        { \
                result = lex_fgets( buf, max_size ); \
        }

unsigned int varnumber;
int reserved_token;
int previous_state = 0;
short function_seen = 0;
int eos_detect=0;
int stabs_started=0;

struct {
        char *name;
        int token;
        unsigned minor;
} reserved_symbols[] = {
        {"GLOBAL",GLOBAL,0},{"EXTERNAL",EXTERNAL,0},
        {"SEPARATE",SEPARATE,0},{"LEFT",LEFT,0},{"TRAILING",TRAILING,0},
        {"CHARACTER",CHARACTER,0},{"PIC",PIC,0},{"PICTURE",PIC,0},
        {"SCREEN",SCREEN,0},{"REVERSE-VIDEO",REVERSEVIDEO,0},
        {"NUMBER",NUMBERTOK,0},{"PLUS",PLUS,0},{"MINUS",MINUS,0},
        {"FOREGROUND-COLOR",FOREGROUNDCOLOR,0},
        {"BACKGROUND-COLOR",BACKGROUNDCOLOR,0},{"COL",COLUMN,0},
        {"COLUMN",COLUMN,0},{"BELL",BELL,0},{"UNDERLINE",UNDERLINE,0},
        {"HIGHLIGHT",HIGHLIGHT,0},{"LOWLIGHT",LOWLIGHT,0},
        {"BLINK",BLINK,0},{"SECURE",SECURE,0},
        {"JUST",JUST,0},{"JUSTIFIED",JUST,0},{"RIGHT",RIGHT,0},{"AUTO",AUTO,0},
        {"REQUIRED",REQUIRED,0},{"FULL",FULL,0},
        {"ASCENDING",DIRECTION,ASCENDING},{"DESCENDING",DIRECTION,DESCENDING},
        {"OR",OR,0},{"AND",AND,0},{"UPDATE",UPDATE,0},{"SORT",SORT,0},
        {"DATE",DATE_TIME,DATE},{"TIME",DATE_TIME,TIME},
        {"DAY",DATE_TIME,DAY},{"DAY-OF-WEEK",DATE_TIME,DAY_OF_WEEK},
        {"COMMAND-LINE",CMD_LINE,0},{"ENVIRONMENT-VARIABLE",ENVIRONMENT_VARIABLE,0},
        {"INKEY",DATE_TIME,INKEY},{"INSPECT",INSPECT,0},
        {"TALLYING",TALLYING,0},{"REPLACING",REPLACING,0},{"LEADING",LEADING,0},
        {"INITIAL",INITIALTOK,0},{"COMMON",COMMONTOK,0},
        {"FIRST",FIRSTTOK,0},{"ALL",ALL,0},
        {"EXCEPTION",EXCEPTION,0},{"MOVE",MOVE,0},{"ACCEPT",ACCEPT,0},
        {"FD",FD,0},{"SD",SD,0},{"ECHO",ECHOT,0},{"REDEFINES",REDEFINES,0},
        {"FILLER",FILLER,0},{"OCCURS",OCCURS,0},{"TIMES",TIMES,0},
        {"GIVING",GIVING,0},{"ERASE",ERASE,0},
        {"COMP",USAGENUM,COMP},{"COMPUTATIONAL",USAGENUM,COMP},
        {"INDEX",USAGENUM,INDEX},{"BINARY",USAGENUM,COMP},
        {"COMP-3",USAGENUM,COMP3},{"COMPUTATIONAL-3",USAGENUM,COMP3},
        {"PACKED-DECIMAL",USAGENUM,COMP3},
        {"COMP-5",USAGENUM,COMP},{"COMPUTATIONAL-5",USAGENUM,COMP},
        {"DISPLAY",DISPLAY,0},{"UPON",UPON,0},{"CONSOLE",CONSOLE,0},
        {"STD-OUTPUT",STD_OUTPUT,0},{"STD-ERROR",STD_ERROR,0},
        {"TRACE",TRACE,0},{"READY",READY,0},{"RESET",RESET,0},
        {"PROGRAM",PROGRAM,0},
        {"PROGRAM-ID",PROGRAM_ID,0},
        {"IDENTIFICATION",IDENTIFICATION_TOK,0},
        {"ENVIRONMENT",ENVIRONMENT_TOK,0},
        {"DATA",DATA_TOK,0},
        {"PROCEDURE",PROCEDURE_TOK,0},
        {"DIVISION",DIVISION,0},{"SECTION",SECTION,0},
        {"TO",TO,0},{"EXIT",EXIT,0},{"OF",OF,0},{"IN",IN,0},
        {"AUTHOR",AUTHOR_TOK,0},
        {"CONFIGURATION",CONFIGURATION,0},
        {"SAME",SAME,0},{"FOR",FOR,0},
        {"INPUT-OUTPUT",INPUT_OUTPUT,0},{"SPECIAL-NAMES",SPECIAL_NAMES,0},
        {"FILE-CONTROL",FILE_CONTROL,0},{"FILE-ID",FILE_ID,0},
        {"I-O-CONTROL",I_O_CONTROL,0},
        {"IS",IS,0},{"ARE",ARE,0},{"START",START,0},
        {"THROUGH",THRU,0},{"THRU",THRU,0},
        {"WORKING-STORAGE",WORKING_STORAGE,0},{"AREA",AREA,0},
        {"LINKAGE",LINKAGE,0},{"COMMA",COMMA,0},{"INVALID",INVALID,0},
        {"DECIMAL-POINT",DECIMAL_POINT,0},{"FILE",FILEN,0},{"BY",BY,0},
        {"GO",GO,0},{"USAGE",USAGE,0},{"BLANK",BLANK,0},{"SIGN",SIGN,0},
        {"VALUE",VALUE,0},{"VALUES",VALUE,0},
        {"OTHER",OTHER,0},{"CONTINUE",CONTINUE,0},
        {"THEN",THEN,0},{"EVALUATE",EVALUATE,0},{"ALSO",ALSO,0},
        {"END-ADD",END_ADD,0},{"END-CALL",END_CALL,0},
        {"END-COMPUTE",END_COMPUTE,0},{"END-DELETE",END_DELETE,0},
        {"END-DIVIDE",END_DIVIDE,0},{"END-EVALUATE",END_EVALUATE,0},
        {"END-IF",END_IF,0},{"END-MULTIPLY",END_MULTIPLY,0},{"END-PERFORM",END_PERFORM,0},
        {"END-READ",END_READ,0},{"END-REWRITE",END_REWRITE,0},{"END-SEARCH",END_SEARCH,0},
        {"END-START",END_START,0},{"END-STRING",END_STRINGCMD,0},
        {"END-SUBTRACT",END_SUBTRACT,0},{"END-UNSTRING",END_UNSTRING,0},
        {"END-WRITE",END_WRITE,0},{"END-RETURN",END_RETURN,0},
        {"TEST",TEST,0},
        {"SEARCH",SEARCH,0},{"WHEN",WHEN,0},
        {"LABEL",LABEL,0},{"RECORD",RECORD,0},{"OMITTED",OMITTED,0},
        {"STANDARD",STANDARD,0},{"RECORDS",RECORDS,0},{"BLOCK",BLOCK,0},
        {"CONTAINS",CONTAINS,0},{"CHARACTERS",CHARACTERS,0},
        {"ADD",ADD,0},{"SUBTRACT",SUBTRACT,0},{"DELETE",DELETE,0},
        {"MULTIPLY",MULTIPLY,0},
        {"DIVIDE",DIVIDE,0},{"ROUNDED",ROUNDED,0},{"REMAINDER",REMAINDER,0},
        {"COMPUTE",COMPUTE,0},{"STOP",STOP,0},{"RUN",RUN,0},
        {"PERFORM",PERFORM,0},{"VARYING",VARYING,0},{"FROM",FROM,0},
        {"UNTIL",UNTIL,0},{"IF",IF,0},
        {"NEXT",NEXT,0},{"PREV",PREVIOUS,0},{"PREVIOUS",PREVIOUS,0},
        {"ELSE",ELSE,0},{"LINE",LINE,0},{"LINES",LINE,0},{"PAGE",PAGETOK,0},
        {"SELECT",SELECT,0},{"ASSIGN",ASSIGN,0},{"SENTENCE",SENTENCE,0},
        {"EQUAL",CONDITIONAL,EQUAL},{"LESS",CONDITIONAL,LESS},
        {"THAN",THAN,0},
        {"=",CONDITIONAL,EQUAL},{"<",CONDITIONAL,LESS},{">",CONDITIONAL,GREATER},
        {"<=",CONDITIONAL,LEQ},{">=",CONDITIONAL,GEQ},{"<>",CONDITIONAL,NEQ},
        {"GREATER",CONDITIONAL,GREATER},{"OPEN",OPEN,0},{"CLOSE",CLOSE,0},
        {"READ",READ,0},{"RETURN",RETURN_TOK,0},
        {"WRITE",WRITE,0},{"RELEASE",WRITE,1},{"REWRITE",REWRITE,0},
        {"AFTER",AFTER,0},{"BEFORE",BEFORE,0},
        {"ADVANCING",ADVANCING,0},
        {"INTO",INTO,0},{"AT",AT,0},{"END",END,0},{"NEGATIVE",NEGATIVE,0},
        {"ZERO",ZERONUM,ZERO},{"ZEROS",ZERONUM,ZEROS},{"ZEROES",ZERONUM,ZEROES},
        {"SPACE",SPACES,0},{"SPACES",SPACES,0},
        {"HIGH-VALUE",HIGHVALUES,0},{"HIGH-VALUES",HIGHVALUES,0},
        {"LOW-VALUE",LOWVALUES,0},{"LOW-VALUES",LOWVALUES,0},
        {"QUOTE",QUOTES,0},{"QUOTES",QUOTES,0},
        {"POSITIVE",POSITIVE,0},{"NOT",NOT,0},
        {"NO",NO,0},
        {"INPUT",INPUT,0},{"I-O",I_O,0},
        {"OUTPUT",OUTPUT,0},{"EXTEND",EXTEND,0},
        {"CALL",CALL,0},{"USING",USING,0},{"WITH",WITH,0},{"CONTENT",CONTENT,0},
        {"DARK",DARK,0},
        {"EOL",EOL_TOK,0},{"EOS",EOS_TOK,0},
        {"SET",SET,0},{"UP",UP,0},{"DOWN",DOWN,0},
        {"SOURCE-COMPUTER",COMMENTING,SOURCE_COMPUTER},
        {"OBJECT-COMPUTER",COMMENTING,OBJECT_COMPUTER},
        {"DATE-WRITTEN",DATE_WRITTEN_TOK,0},
        {"DATE-COMPILED",DATE_COMPILED_TOK,0},
        {"INSTALLATION",INSTALLATION_TOK,0},
        {"SECURITY",SECURITY_TOK,0},
        {"ORGANIZATION",ORGANIZATION,0},{"ACCESS",ACCESS,0},{"MODE",MODE,0},
        {"KEY",KEY,0},{"STATUS",STATUS,0},{"SEQUENTIAL",SEQUENTIAL,0},
        {"INDEXED",INDEXED,0},{"DYNAMIC",DYNAMIC,0},
        {"RANDOM",RANDOM,0},{"RELATIVE",RELATIVE,0},
        {"COM1",PORTNUM,8},{"COM2",PORTNUM,1},{"COM3",PORTNUM,2},
        {"COM4",PORTNUM,3},{"LPT1",PORTNUM,4},{"LPT2",PORTNUM,5},
        {"LPT3",PORTNUM,6},{"LPT4",PORTNUM,7},
        {"DISK",PORTNUM,0},{"PRINTER",PORTNUM,4},
        {"STRING",STRINGCMD,0},{"UNSTRING",UNSTRING,0},
        {"ON",ONTOK,0},{"ERROR",ERRORTOK,0},{"SIZE",SIZE,0},
        {"POINTER",POINTER,0},
        {"COMP-1",COMP1,0},{"COMPUTATIONAL-1",COMP1,0},{"FLOAT-SHORT",COMP1,0},
        {"COMP-2",COMP2,0},{"COMPUTATIONAL-2",COMP2,0},{"FLOAT-LONG",COMP2,0},
        {"DELIMITED",DELIMITED,0},
        {"OVERFLOW",OVERFLOWTK,0},{"DELIMITER",DELIMITER,0},
        {"COUNT",COUNT,0},{"DUPLICATES",DUPLICATES,0},
        {"ALTERNATE",ALTERNATE,0},{"CURRENCY",CURRENCY,0},
        {"REFERENCE",REFERENCE,0},{"INITIALIZE",INITIALIZE,0},
        {"NUMERIC",NUMERIC,0},{"ALPHABETIC",ALPHABETICTOK,0},
        {"ALPHABETIC-LOWER",ALPHABETICLOWER,0},
        {"ALPHABETIC-UPPER",ALPHABETICUPPER,0},
        {"RETURNING",RETURNING,0},{"TRUE",TOKTRUE,0},{"FALSE",TOKFALSE,0},
        {"FUNCTION",FUNCTION,0},{"SYNCHRONIZED",SYNC,0},{"SYNC",SYNC,0},
        {"ANY",ANY,0},{"REPORT",REPORT,0},{"RD",TOKRD,0},{"CODE",TOKCODE,0},
        {"CONTROL",CONTROL,0},{"CONTROLS",CONTROL,1},
        {"LIMIT",LIMIT,0},{"FINAL",FINAL,0},{"HEADING",HEADING,0},
        {"FOOTING",FOOTING,0},{"LAST",TOKLAST,0},{"DETAIL",DETAIL,0},
        {"GROUP",GROUP,0},{"INDICATE",INDICATE,0},{"SUM",TOKSUM,0},
        {"POSITION",TOKPOSITION,0},{"DEPENDING",DEPENDING,0},{"TYPE",TYPETOK,0},
        {"SOURCE",TOKSOURCE,0}, {"INITIATE",INITIATE,0}, {"TERMINATE",TERMINATE,0},
        {"GENERATE",GENERATE,0},{"ADDRESS",ADDRESS,0},{"NULL",NULLTOK,0},
        {"NO-ECHO",NOECHO,0},{"CORRESPONDING",CORRESPONDING,0},
        {"CORR",CORRESPONDING,0},{"LENGTH",LENGTH,0},
        {"BINARY-CHAR",USAGENUM,USAGE_BINARY_CHAR}, 
        {"BINARY-SHORT",USAGENUM,USAGE_BINARY_SHORT},
        {"BINARY-LONG",USAGENUM,USAGE_BINARY_LONG}, 
        {"BINARY-DOUBLE",USAGENUM,USAGE_BINARY_DOUBLE},
        {"CONVERTING",CONVERTING,0},{"OPTIONAL",OPTIONAL,0},
        {"",0,0}
};
struct reserved_sym {
        char *name;
        struct reserved_sym *next;
        int i; /* index on reserved symbol table */
};
#define HASHLEN 100
static struct reserved_sym *reserved_tab[ HASHLEN ]={NULL};

#define ANY_NUMBER 32767
struct {
        char *name;
        char function_type;
        short args;
} intrinsic_symbols[] = {
        {"ABS",'*',1},
	{"LENGTH",'9',ANY_NUMBER},
	{"MAX",'*',ANY_NUMBER},
	{"MIN",'*',ANY_NUMBER},
        {"RANDOM",'*',-1},
        {"",0,0}
};
struct intrinsic_sym {
        char *name;
        struct intrinsic_sym *next;
        int i; /* index on intrinsic symbol table */
};
static struct intrinsic_sym *intrinsic_tab[ HASHLEN ]={NULL};

static int read_literal (char mark);
static int read_literal_x (char mark);
static int is_integer_token (const char *tok);
static int is_valid_identifier (const char *tok);
%}

letters [A-Za-z]
alphanum [A-Za-z0-9]
exletter {letters}|-
digit [0-9]
pic_ch [9XVSBZAP.,-+*/$]
relop [<>=]
white [\r\t ]
lpar \(
rpar \)
sdelim \"
ldelim \'
any [!-~]
int {digit}({digit})*
decdelim [.,]
sign [+-]

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

^"*".* { 
  TRACE_PRINT (("<comment>: %s:\n", yytext));
}

^"D ".* {
  if (cob_debug_flag)
    yyless(2);
  else
    TRACE_PRINT (("<comment>: %s:\n", yytext));
}

"," {
  return ',';
}

{lpar} {
  if (need_subscripts) {
    TRACE_PRINT (("<LPAR>:\n"));
    need_subscripts=0;
    return (LPAR);
  }
  else
    return ('(');
}

{white}+ { }

"EJECT"|"SKIP1"|"SKIP2"|"SKIP3" { /* throw away this */ }

<INITIAL,SUBSCRIPTS_ST>{word} {
  struct sym *symbol;
  if (is_integer_token (yytext) && !eos_detect)
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  eos_detect = 0;
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  if (reserved_token == FUNCTION)
	    function_seen = 1;
	  /* special case of 4 intrinsics which are also reserved */
	  if (function_seen && lookup_intrinsic (yytext) != NULL)
	    {
	      yylval.sval = install (yytext, SYTB_LAB, 0);
	      TRACE_PRINT ((" <LABELSTR>: %s\n", yytext));
	      return (LABELSTR);
	    }
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      /* if symbol was defined at the data division */
      if ((symbol = lookup (yytext, 2)) != NULL)
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
	  yylval.sval = install (yytext, SYTB_LAB, 0);
	  TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
	  return (LABELSTR);
	}
    }
}

<ENVIR_ST>{word} {
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      TRACE_PRINT (("<STRING>: %s\n", yytext));
      yylval.sval = install (yytext, SYTB_VAR, 0);
      return (STRING);
    }
}

<EXCEPTION_ST>{word} {
  struct sym *symbol;
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  if (reserved_token == NOT)
	    reserved_token = NOTEXCEP;
	  return reserved_token;
	}
      /* if symbol was defined at the data division */
      if ((symbol = lookup (yytext, 2)) != NULL)
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
	  yylval.sval = install (yytext, SYTB_LAB, 0);
	  TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
	  return (LABELSTR);
	}
    }
}


<FD_ST>{
{word} {
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      TRACE_PRINT (("<STRING>: %s\n", yytext));
      /* install w/clone if neccessary */
      if ((yylval.sval = lookup (yytext, SYTB_VAR)) == NULL)
	yyerror ("undefined %s at FD/SD", yytext);
      return (STRING);
    }
}

[ \t]* { }
}

<REDEF_ST>{word} {
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      TRACE_PRINT (("<STRING>: %s\n", yytext));
      /* don't clone redefined var */
      yylval.sval = install (yytext, SYTB_VAR, 0);
      update_xreflist (yylval.sval);
      return (STRING);
    }
}

<DATA_ST>{word} {
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      TRACE_PRINT (("<STRING>: %s\n", yytext));
      /* install w/clone if neccessary */
      yylval.sval = install (yytext, SYTB_VAR, 1);
      update_xreflist (yylval.sval);
      return (STRING);
    }
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
  if (is_integer_token (yytext))
    {
      TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
      yylval.lval = install_literal (yytext);
      return (NLITERAL);
    }
  if (!is_valid_identifier (yytext))
    {
      yylval.sval = install (yytext, SYTB_LAB, 0);
      TRACE_PRINT (("<LABELSTR>: %s\n", yytext));
      return (LABELSTR);
    }
  else
    {
      if ((reserved_token = reserved (yytext)) != 0)
	{
	  TRACE_PRINT ((" <reserved>: %s\n", yytext));
	  return reserved_token;
	}
      TRACE_PRINT ((" <IDSTRING>: %s\n", yytext));
      yylval.str = strdup (yytext);
      return IDSTRING;
    }
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
                eos_detect++;
                return(EOS); 
        }

{ldelim}        { return read_literal ('\''); }
{sdelim}        { return read_literal ('\"'); }
X{ldelim}       { return read_literal_x ('\''); }
X{sdelim}       { return read_literal_x ('\"'); }

{digit}*{decdelim}?{int}+       {
                TRACE_PRINT (("<NLITERAL>: %s\n", yytext));
                yylval.lval = install_literal (yytext);
                return(NLITERAL);
        }

{white}+ { }


{relop}{relop}? {
                if ((reserved_token=reserved(yytext))!=0) {
                        TRACE_PRINT ((" <reserved>: %s\n", yytext));
                        return reserved_token;
                }
                yyerror("relational operator unknown");
        }

{lpar}  {
                if (function_seen) {
                        function_seen = 0;
                        }
                TRACE_PRINT (("<0: lpar>: %s\n", yytext));
                return(yytext[0]); 
        }
"**"    {
                TRACE_PRINT (("<0: POW_OP>: %s\n", yytext));
                return(POW_OP); 
        }
{any}   {
                TRACE_PRINT (("<0: any>: %s\n", yytext));
                return(yytext[0]); 
        }

"\n"   { }      

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
{lpar}{digit}({digit})*{rpar} { 
                { 
                char *s;
                yylval.ival = 0;
                s = yytext+1; /* bypass lpar */
                while (*s!=')')
                        yylval.ival = 10 * yylval.ival + *s++ - '0';
                TRACE_PRINT (("<PICS: MULTIPLIER>: %d\n", yylval.ival));
                return(MULTIPLIER); 
                } 
        }
{word} {
                if ((reserved_token=reserved(yytext))!=0) 
                {
                        TRACE_PRINT ((" <reserved>: %s\n", yytext));
                        BEGIN PIC_ST;
                        TRACE_PRINT (("<PIC_ST2-> PIC_ST>:\n"));
                        return reserved_token;
                }
                REJECT;
        }
{any}   { 
                        TRACE_PRINT (("<PICS: any char>: %c\n", yytext[0]));
                yylval.ival=(int)yytext[0];
                return(CHAR); }
"\n" { }
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

int
lex_fgets (char *buf, int maxsize)
{
  int result;
  char *s;
  s = fgets (buf, maxsize, yyin);
  result = (s == NULL) ? YY_NULL : strlen (s);
  if (result)
    {
      result = strlen (buf);
      ++cob_orig_lineno;
    }
  return result;
}

int hash (char *name);

struct reserved_sym *
lookup_reserved (char *s)
{
  struct reserved_sym *as;
  for (as = reserved_tab[hash (s)]; as != NULL; as = as->next)
    if (strcasecmp (s, as->name) == 0)
      {
	return (as);
      }
  return (NULL);
}

void
install_reserved (void)
{
  struct reserved_sym *as;
  struct intrinsic_sym *is;
  int val, i;
  for (i = 0; strcmp (reserved_symbols[i].name, ""); i++)
    {
      as = (struct reserved_sym *) malloc (sizeof (struct reserved_sym));
      as->name = strdup (reserved_symbols[i].name);
      val = hash (as->name);
      as->next = reserved_tab[val];
      reserved_tab[val] = as;
      as->i = i;
    }
  for (i = 0; strcmp (intrinsic_symbols[i].name, ""); i++)
    {
      is = (struct intrinsic_sym *) malloc (sizeof (struct intrinsic_sym));
      is->name = strdup (intrinsic_symbols[i].name);
      val = hash (is->name);
      is->next = intrinsic_tab[val];
      intrinsic_tab[val] = is;
      is->i = i;
    }
}

int
reserved (char *s)
{
  struct reserved_sym *r;
  if ((r = lookup_reserved (s)) != NULL)
    {
      yylval.ival = reserved_symbols[r->i].minor;
      return reserved_symbols[r->i].token;
    }
  return 0;
}

struct intrinsic_sym *
lookup_intrinsic (char *s)
{
  struct intrinsic_sym *is;
  for (is = intrinsic_tab[hash (s)]; is != NULL; is = is->next)
    if (strcasecmp (s, is->name) == 0)
      {
	return (is);
      }
  return (NULL);
}

#if defined(SunOS)
va_list __builtin_va_alist;
#endif

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
  /* This avoids warnings */
  yy_flex_realloc (0, 0);
}
