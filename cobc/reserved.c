/*
 * Copyright (C) 2001 Keisuke Nishida
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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "codegen.h"
#include "parser.h"
#include "reserved.h"

#define HASH_SIZE 133

struct word {
  const char *name;
  int token;
  struct word *next;
} *reserved_table[HASH_SIZE];

static struct word builtin_words[] = {
  {"STDIN",		BUILTIN_TOK},
  {"STDOUT",		BUILTIN_TOK},
  {"STDERR",		BUILTIN_TOK},
  {"STANDARD-INPUT",	BUILTIN_TOK},
  {"STANDARD-OUTPUT",	BUILTIN_TOK},
  {"STANDARD-ERROR",	BUILTIN_TOK},
  {"SW1",		BUILTIN_TOK},
  {"SW2",		BUILTIN_TOK},
  {"SW3", 	       	BUILTIN_TOK},
  {"SW4",		BUILTIN_TOK},
  {"SW5",		BUILTIN_TOK},
  {"SW6",		BUILTIN_TOK},
  {"SW7",		BUILTIN_TOK},
  {"SW8",		BUILTIN_TOK},
  {"SWITCH-1",		BUILTIN_TOK},
  {"SWITCH-2",		BUILTIN_TOK},
  {"SWITCH-3", 	       	BUILTIN_TOK},
  {"SWITCH-4",		BUILTIN_TOK},
  {"SWITCH-5",		BUILTIN_TOK},
  {"SWITCH-6",		BUILTIN_TOK},
  {"SWITCH-7",		BUILTIN_TOK},
  {"SWITCH-8",		BUILTIN_TOK},
  {0, 0}
};

static struct word reserved_words[] = {
  {"ACCEPT",		ACCEPT},
  {"ACCESS",		ACCESS},
  {"ADD",		ADD},
  {"ADVANCING",		ADVANCING},
  {"AFTER",		AFTER},
  {"ALL",		ALL},
  {"ALPHABET",          ALPHABET},
  {"ALPHABETIC",	ALPHABETIC},
  {"ALPHABETIC-LOWER",	ALPHABETIC_LOWER},
  {"ALPHABETIC-UPPER",	ALPHABETIC_UPPER},
  {"ALPHANUMERIC",	ALPHANUMERIC},
  {"ALPHANUMERIC-EDITED", ALPHANUMERIC_EDITED},
  {"ALSO",		ALSO},
  {"ALTER",		ALTER},
  {"ALTERNATE",		ALTERNATE},
  {"AND",		AND},
  {"ANY",		ANY},
  {"ARE",		ARE},
  {"AREA",		AREA},
  {"ASCENDING",		ASCENDING},
  {"ASSIGN",		ASSIGN},
  {"AT",		AT},
  {"AUTHOR",		AUTHOR},
  {"BEFORE",		BEFORE},
  {"BINARY",		BINARY},
  {"BLANK",		BLANK},
  {"BLOCK",		BLOCK},
  {"BY",		BY},
  {"CALL",		CALL},
  {"CANCEL",		CANCEL},
  {"CHARACTER",		CHARACTER},
  {"CHARACTERS",	CHARACTERS},
  {"CLASS",		CLASS},
  {"CLOSE",		CLOSE},
  {"COL",		COLUMN},
  {"COLLATING",		COLLATING},
  {"COLUMN",		COLUMN},
  {"COM1",		PORT},
  {"COM2",		PORT},
  {"COM3",		PORT},
  {"COM4",		PORT},
  {"COMMA",		COMMA},
  {"COMMAND-LINE",	COMMAND_LINE},
  {"COMMON",		COMMON},
  {"COMP",		BINARY},
  {"COMP-1",		FLOAT_SHORT},
  {"COMP-2",		FLOAT_LONG},
  {"COMP-3",		PACKED_DECIMAL},
  {"COMP-5",		BINARY},
  {"COMPUTATIONAL",	BINARY},
  {"COMPUTATIONAL-1",	FLOAT_SHORT},
  {"COMPUTATIONAL-2",	FLOAT_LONG},
  {"COMPUTATIONAL-3",	PACKED_DECIMAL},
  {"COMPUTATIONAL-5",	BINARY},
  {"COMPUTE",		COMPUTE},
  {"CONFIGURATION",	CONFIGURATION},
  {"CONSOLE",		CONSOLE},
  {"CONTAINS",		CONTAINS},
  {"CONTENT",		CONTENT},
  {"CONTINUE",		CONTINUE},
  {"CONTROL",		CONTROL},
  {"CONTROLS",		CONTROL},
  {"CONVERTING",	CONVERTING},
  {"CORR",		CORRESPONDING},
  {"CORRESPONDING",	CORRESPONDING},
  {"COUNT",		COUNT},
  {"CURRENCY",		CURRENCY},
  {"DATA",		DATA},
  {"DATE",		DATE},
  {"DATE-COMPILED",	DATE_COMPILED},
  {"DATE-WRITTEN",	DATE_WRITTEN},
  {"DAY",		DAY},
  {"DAY-OF-WEEK",	DAY_OF_WEEK},
  {"DEBUGGING",		DEBUGGING},
  {"DECIMAL-POINT",	DECIMAL_POINT},
  {"DELETE",		DELETE},
  {"DELIMITED",		DELIMITED},
  {"DELIMITER",		DELIMITER},
  {"DEPENDING",		DEPENDING},
  {"DESCENDING",	DESCENDING},
  {"DETAIL",		DETAIL},
  {"DISK",		PORT},
  {"DISPLAY",		DISPLAY},
  {"DIVIDE",		DIVIDE},
  {"DIVISION",		DIVISION},
  {"DOWN",		DOWN},
  {"DUPLICATES",	DUPLICATES},
  {"DYNAMIC",		DYNAMIC},
  {"ELSE",		ELSE},
  {"END",		END},
  {"END-ADD",		END_ADD},
  {"END-CALL",		END_CALL},
  {"END-COMPUTE",	END_COMPUTE},
  {"END-DELETE",	END_DELETE},
  {"END-DIVIDE",	END_DIVIDE},
  {"END-EVALUATE",	END_EVALUATE},
  {"END-IF",		END_IF},
  {"END-MULTIPLY",	END_MULTIPLY},
  {"END-PERFORM",	END_PERFORM},
  {"END-READ",		END_READ},
  {"END-RETURN",	END_RETURN},
  {"END-REWRITE",	END_REWRITE},
  {"END-SEARCH",	END_SEARCH},
  {"END-START",		END_START},
  {"END-STRING",	END_STRING},
  {"END-SUBTRACT",	END_SUBTRACT},
  {"END-UNSTRING",	END_UNSTRING},
  {"END-WRITE",		END_WRITE},
  {"ENVIRONMENT",	ENVIRONMENT},
  {"ENVIRONMENT-VARIABLE", ENVIRONMENT_VARIABLE},
  {"EQUAL",		EQUAL},
  {"ERROR",		ERROR},
  {"EVALUATE",		EVALUATE},
  {"EXCEPTION",		EXCEPTION},
  {"EXIT",		EXIT},
  {"EXTEND",		EXTEND},
  {"EXTERNAL",		EXTERNAL},
  {"FALSE",		FALSE},
  {"FD",		FD},
  {"FILE",		FILEN},
  {"FILE-CONTROL",	FILE_CONTROL},
  {"FILE-ID",		FILE_ID},
  {"FILLER",		FILLER},
  {"FINAL",		FINAL},
  {"FIRST",		FIRST},
  {"FLOAT-LONG",	FLOAT_LONG},
  {"FLOAT-SHORT",	FLOAT_SHORT},
  {"FOOTING",		FOOTING},
  {"FOR",		FOR},
  {"FROM",		FROM},
  {"FUNCTION",		FUNCTION},
  {"GENERATE",		GENERATE},
  {"GIVING",		GIVING},
  {"GLOBAL",		GLOBAL},
  {"GO",		GO},
  {"GREATER",		GREATER},
  {"HEADING",		HEADING},
  {"HIGH-VALUE",	HIGH_VALUES},
  {"HIGH-VALUES",	HIGH_VALUES},
  {"I-O",		I_O},
  {"I-O-CONTROL",	I_O_CONTROL},
  {"IDENTIFICATION",	IDENTIFICATION},
  {"IF",		IF},
  {"IN",		IN},
  {"INDEX",		INDEX},
  {"INDEXED",		INDEXED},
  {"INITIAL",		TOK_INITIAL},
  {"INITIALIZE",	INITIALIZE},
  {"INITIATE",		INITIATE},
  {"INPUT",		INPUT},
  {"INPUT-OUTPUT",	INPUT_OUTPUT},
  {"INSPECT",		INSPECT},
  {"INSTALLATION",	INSTALLATION},
  {"INTO",		INTO},
  {"INVALID",		INVALID},
  {"IS",		IS},
  {"JUST",		JUSTIFIED},
  {"JUSTIFIED",		JUSTIFIED},
  {"KEY",		KEY},
  {"LABEL",		LABEL},
  {"LAST",		LAST},
  {"LEADING",		LEADING},
  {"LEFT",		LEFT},
  {"LESS",		LESS},
  {"LIMIT",		LIMIT},
  {"LINE",		LINE},
  {"LINES",		LINE},
  {"LINKAGE",		LINKAGE},
  {"LOW-VALUE",		LOW_VALUES},
  {"LOW-VALUES",	LOW_VALUES},
  {"LPT1",		PORT},
  {"LPT2",		PORT},
  {"LPT3",		PORT},
  {"LPT4",		PORT},
  {"MEMORY",		MEMORY},
  {"MODE",		MODE},
  {"MOVE",		MOVE},
  {"MULTIPLY",		MULTIPLY},
  {"NEGATIVE",		NEGATIVE},
  {"NEXT",		NEXT},
  {"NATIVE",		NATIVE},
  {"NATIONAL",		NATIONAL},
  {"NATIONAL-EDITED",	NATIONAL_EDITED},
  {"NO",		NO},
  {"NOT",		NOT},
  {"NUMBER",		NUMBER},
  {"NUMERIC",		NUMERIC},
  {"NUMERIC-EDITED",	NUMERIC_EDITED},
  {"OBJECT-COMPUTER",	OBJECT_COMPUTER},
  {"OCCURS",		OCCURS},
  {"OF",		OF},
  {"OFF",		OFF},
  {"OMITTED",		OMITTED},
  {"ON",		ON},
  {"OPEN",		OPEN},
  {"OPTIONAL",		OPTIONAL},
  {"OR",		OR},
  {"ORGANIZATION",	ORGANIZATION},
  {"OTHER",		OTHER},
  {"OUTPUT",		OUTPUT},
  {"OVERFLOW",		OVERFLOW},
  {"PACKED-DECIMAL",	PACKED_DECIMAL},
  {"PAGE",		PAGE},
  {"PERFORM",		PERFORM},
  {"PLUS",		PLUS},
  {"POINTER",		POINTER},
  {"POSITIVE",		POSITIVE},
  {"PREV",		PREVIOUS},
  {"PREVIOUS",		PREVIOUS},
  {"PRINTER",		PORT},
  {"PROCEDURE",		PROCEDURE},
  {"PROGRAM",		PROGRAM},
  {"PROGRAM-ID",	PROGRAM_ID},
  {"QUOTE",		QUOTES},
  {"QUOTES",		QUOTES},
  {"RANDOM",		RANDOM},
  {"RD",		RD},
  {"READ",		READ},
  {"READY",		READY},
  {"RECORD",		RECORD},
  {"RECORDS",		RECORDS},
  {"REDEFINES",		REDEFINES},
  {"REFERENCE",		REFERENCE},
  {"RELATIVE",		RELATIVE},
  {"RELEASE",		RELEASE},
  {"REMAINDER",		REMAINDER},
  {"RENAMES",		RENAMES},
  {"REPLACING",		REPLACING},
  {"REPORT",		REPORT},
  {"RESET",		RESET},
  {"RETURN",		RETURN},
  {"RETURNING",		RETURNING},
  {"REWRITE",		REWRITE},
  {"RIGHT",		RIGHT},
  {"ROUNDED",		ROUNDED},
  {"RUN",		RUN},
  {"SAME",		SAME},
  {"SD",		SD},
  {"SEARCH",		SEARCH},
  {"SECTION",		SECTION},
  {"SECURITY",		SECURITY},
  {"SELECT",		SELECT},
  {"SENTENCE",		SENTENCE},
  {"SEPARATE",		SEPARATE},
  {"SEQUENCE",		SEQUENCE},
  {"SEQUENTIAL",	SEQUENTIAL},
  {"SET",		SET},
  {"SIGN",		SIGN},
  {"SIZE",		SIZE},
  {"SORT",		SORT},
  {"SORT-MERGE",	SORT_MERGE},
  {"SOURCE",		SOURCE},
  {"SOURCE-COMPUTER",	SOURCE_COMPUTER},
  {"SPACE",		SPACES},
  {"SPACES",		SPACES},
  {"SPECIAL-NAMES",	SPECIAL_NAMES},
  {"STANDARD",		STANDARD},
  {"STANDARD-1",	STANDARD_1},
  {"STANDARD-2",	STANDARD_2},
  {"START",		START},
  {"STATUS",		STATUS},
  {"STOP",		STOP},
  {"STRING",		STRING},
  {"SUBTRACT",		SUBTRACT},
  {"SUM",		SUM},
  {"SYMBOLIC",		SYMBOLIC},
  {"SYNC",		SYNCHRONIZED},
  {"SYNCHRONIZED",      SYNCHRONIZED},
  {"TALLYING",	    	TALLYING},
  {"TERMINATE",	    	TERMINATE},
  {"TEST",	    	TEST},
  {"THAN",	    	THAN},
  {"THEN",	    	THEN},
  {"THROUGH",	    	THRU},
  {"THRU",	    	THRU},
  {"TIME",	    	TIME},
  {"TIMES",	    	TIMES},
  {"TO",	    	TO},
  {"TRACE",	    	TRACE},
  {"TRAILING",	    	TRAILING},
  {"TRUE",	    	TRUE},
  {"TYPE",	    	TYPE},
  {"UNSTRING",	    	UNSTRING},
  {"UNTIL",	    	UNTIL},
  {"UP",       	    	UP},
  {"UPON",     	       	UPON},
  {"USAGE",		USAGE},
  {"USING",		USING},
  {"VALUE",		VALUE},
  {"VALUES",		VALUE},
  {"VARYING",		VARYING},
  {"WHEN",		WHEN},
  {"WITH",		WITH},
  {"WORKING-STORAGE",	WORKING_STORAGE},
  {"WRITE",		WRITE},
  {"ZERO",		ZEROS},
  {"ZEROES",		ZEROS},
  {"ZEROS",		ZEROS},
  {0, 0}
};

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return val % HASH_SIZE;
}

int
lookup_reserved_word (const char *name)
{
  struct word *p;
  for (p = reserved_table[hash (name)]; p; p = p->next)
    if (strcasecmp (name, p->name) == 0)
      return p->token;
  return 0;
}

void
init_reserved_words (void)
{
  int i;
  for (i = 0; i < HASH_SIZE; i++)
    reserved_table[i] = NULL;

  /* install builtin words */
  for (i = 0; builtin_words[i].name != 0; i++)
    {
      int val = hash (builtin_words[i].name);
      builtin_words[i].next = reserved_table[val];
      reserved_table[val] = &builtin_words[i];
    }

  /* install reserved words */
  for (i = 0; reserved_words[i].name != 0; i++)
    {
      int val = hash (reserved_words[i].name);
      reserved_words[i].next = reserved_table[val];
      reserved_table[val] = &reserved_words[i];
    }
}
