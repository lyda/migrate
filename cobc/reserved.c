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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "tree.h"
#include "parser.h"
#include "reserved.h"

#define HASH_SIZE 133

static struct builtin {
  char *name;
  int token;
} builtin_words[] = {
  {"CONSOLE",		BUILTIN_STDOUT},
  {"SYSIN",		BUILTIN_STDIN},
  {"SYSOUT",		BUILTIN_STDOUT},
  {"SYSERR",		BUILTIN_STDERR},
  {"STANDARD-INPUT",	BUILTIN_STDIN},
  {"STANDARD-OUTPUT",	BUILTIN_STDOUT},
  {"STANDARD-ERROR",	BUILTIN_STDERR},
  {"SWITCH-1",		BUILTIN_SWITCH_1},
  {"SWITCH-2",		BUILTIN_SWITCH_2},
  {"SWITCH-3", 	       	BUILTIN_SWITCH_3},
  {"SWITCH-4",		BUILTIN_SWITCH_4},
  {"SWITCH-5",		BUILTIN_SWITCH_5},
  {"SWITCH-6",		BUILTIN_SWITCH_6},
  {"SWITCH-7",		BUILTIN_SWITCH_7},
  {"SWITCH-8",		BUILTIN_SWITCH_8},
  {0, 0}
};

static struct reserved {
  char *name;
  int token;
  struct reserved *next;
} *reserved_table[HASH_SIZE];

static struct reserved reserved_words[] = {
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
  {"AREAS",		AREA},
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
  {"CODE-SET",		CODE_SET},
  {"COLLATING",		COLLATING},
  {"COMMA",		COMMA},
  {"COMMAND-LINE",	COMMAND_LINE},
  {"COMMON",		COMMON},
  {"COMP",		BINARY},
  {"COMP-3",		PACKED_DECIMAL},
  {"COMPUTATIONAL",	BINARY},
  {"COMPUTATIONAL-3",	PACKED_DECIMAL},
  {"COMPUTE",		COMPUTE},
  {"CONFIGURATION",	CONFIGURATION},
  {"CONTAINS",		CONTAINS},
  {"CONTENT",		CONTENT},
  {"CONTINUE",		CONTINUE},
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
  {"DECLARATIVES",	DECLARATIVES},
  {"DECIMAL-POINT",	DECIMAL_POINT},
  {"DELETE",		DELETE},
  {"DELIMITED",		DELIMITED},
  {"DELIMITER",		DELIMITER},
  {"DEPENDING",		DEPENDING},
  {"DESCENDING",	DESCENDING},
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
  {"FILE",		TOK_FILE},
  {"FILE-CONTROL",	FILE_CONTROL},
  {"FILLER",		FILLER},
  {"FIRST",		FIRST},
  {"FOR",		FOR},
  {"FROM",		FROM},
  {"GIVING",		GIVING},
  {"GLOBAL",		GLOBAL},
  {"GO",		GO},
  {"GREATER",		GREATER},
  {"HIGH-VALUE",	HIGH_VALUE},
  {"HIGH-VALUES",	HIGH_VALUE},
  {"I-O",		I_O},
  {"I-O-CONTROL",	I_O_CONTROL},
  {"IDENTIFICATION",	IDENTIFICATION},
  {"IF",		IF},
  {"IN",		IN},
  {"INDEX",		INDEX},
  {"INDEXED",		INDEXED},
  {"INITIAL",		TOK_INITIAL},
  {"INITIALIZE",	INITIALIZE},
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
  {"LEADING",		LEADING},
  {"LEFT",		LEFT},
  {"LESS",		LESS},
  {"LINE",		LINE},
  {"LINES",		LINE},
  {"LINKAGE",		LINKAGE},
  {"LOW-VALUE",		LOW_VALUE},
  {"LOW-VALUES",	LOW_VALUE},
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
  {"POINTER",		POINTER},
  {"POSITIVE",		POSITIVE},
  {"PROCEED",		PROCEED},
  {"PROCEDURE",		PROCEDURE},
  {"PROGRAM",		PROGRAM},
  {"PROGRAM-ID",	PROGRAM_ID},
  {"QUOTE",		QUOTE},
  {"QUOTES",		QUOTE},
  {"RANDOM",		RANDOM},
  {"READ",		READ},
  {"RECORD",		RECORD},
  {"RECORDS",		RECORDS},
  {"REDEFINES",		REDEFINES},
  {"REFERENCE",		REFERENCE},
  {"RELATIVE",		RELATIVE},
  {"REMAINDER",		REMAINDER},
  {"RENAMES",		RENAMES},
  {"REPLACING",		REPLACING},
  {"RESERVE",		RESERVE},
  {"RETURNING",		RETURNING},
  {"REWRITE",		REWRITE},
  {"RIGHT",		RIGHT},
  {"ROUNDED",		ROUNDED},
  {"RUN",		RUN},
  {"SAME",		SAME},
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
  {"SOURCE-COMPUTER",	SOURCE_COMPUTER},
  {"SPACE",		SPACE},
  {"SPACES",		SPACE},
  {"SPECIAL-NAMES",	SPECIAL_NAMES},
  {"STANDARD",		STANDARD},
  {"STANDARD-1",	STANDARD_1},
  {"STANDARD-2",	STANDARD_2},
  {"START",		START},
  {"STATUS",		STATUS},
  {"STOP",		STOP},
  {"STRING",		STRING},
  {"SUBTRACT",		SUBTRACT},
  {"SYMBOLIC",		SYMBOLIC},
  {"SYNC",		SYNCHRONIZED},
  {"SYNCHRONIZED",      SYNCHRONIZED},
  {"TALLYING",	    	TALLYING},
  {"TEST",	    	TEST},
  {"THAN",	    	THAN},
  {"THEN",	    	THEN},
  {"THROUGH",	    	THRU},
  {"THRU",	    	THRU},
  {"TIME",	    	TIME},
  {"TIMES",	    	TIMES},
  {"TO",	    	TO},
  {"TRAILING",	    	TRAILING},
  {"TRUE",	    	TRUE},
  {"UNSTRING",	    	UNSTRING},
  {"UNTIL",	    	UNTIL},
  {"UP",       	    	UP},
  {"UPON",     	       	UPON},
  {"USAGE",		USAGE},
  {"USE",		USE},
  {"USING",		USING},
  {"VALUE",		VALUE},
  {"VALUES",		VALUE},
  {"VARYING",		VARYING},
  {"WHEN",		WHEN},
  {"WITH",		WITH},
  {"WORKING-STORAGE",	WORKING_STORAGE},
  {"WRITE",		WRITE},
  {"ZERO",		ZERO},
  {"ZEROES",		ZERO},
  {"ZEROS",		ZERO},
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
lookup_builtin_word (const char *name)
{
  int i;
  for (i = 0; builtin_words[i].name != 0; i++)
    if (strcasecmp (name, builtin_words[i].name) == 0)
      return builtin_words[i].token;
  return BUILTIN_ZERO;
}

int
lookup_reserved_word (const char *name)
{
  struct reserved *p;
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

  for (i = 0; reserved_words[i].name != 0; i++)
    {
      int val = hash (reserved_words[i].name);
      reserved_words[i].next = reserved_table[val];
      reserved_table[val] = &reserved_words[i];
    }
}
