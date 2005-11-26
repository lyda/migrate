/*
 * Copyright (C) 2001-2005 Keisuke Nishida
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

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "tree.h"
#include "parser.h"

#define HASH_SIZE 133

static struct {
	const char			*name;
	enum cb_system_name_category	category;
	int				token;
	cb_tree				node;
} system_table[] = {
  {"SYSIN",		CB_DEVICE_NAME,  CB_DEVICE_SYSIN, 0},
  {"SYSIPT",		CB_DEVICE_NAME,  CB_DEVICE_SYSIN, 0},
  {"SYSOUT",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, 0},
  {"SYSLIST",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, 0},
  {"SYSLST",		CB_DEVICE_NAME,  CB_DEVICE_SYSOUT, 0},
  {"SYSERR",		CB_DEVICE_NAME,  CB_DEVICE_SYSERR, 0},
  {"CONSOLE",		CB_DEVICE_NAME,  CB_DEVICE_CONSOLE, 0},
  {"C01",		CB_FEATURE_NAME, CB_FEATURE_FORMFEED, 0},
  {"FORMFEED",		CB_FEATURE_NAME, CB_FEATURE_FORMFEED, 0},
  {"SWITCH-1",		CB_SWITCH_NAME,  CB_SWITCH_1, 0},
  {"SWITCH-2",		CB_SWITCH_NAME,  CB_SWITCH_2, 0},
  {"SWITCH-3", 	       	CB_SWITCH_NAME,  CB_SWITCH_3, 0},
  {"SWITCH-4",		CB_SWITCH_NAME,  CB_SWITCH_4, 0},
  {"SWITCH-5",		CB_SWITCH_NAME,  CB_SWITCH_5, 0},
  {"SWITCH-6",		CB_SWITCH_NAME,  CB_SWITCH_6, 0},
  {"SWITCH-7",		CB_SWITCH_NAME,  CB_SWITCH_7, 0},
  {"SWITCH-8",		CB_SWITCH_NAME,  CB_SWITCH_8, 0},
  {0, 0, 0}
};

static struct reserved {
	const char	*name;
	int		token;
	struct reserved *next;
} *reserved_table[HASH_SIZE];

static struct reserved reserved_words[] = {
  {"ACCEPT",		ACCEPT, 0},			/* 2002 */
  {"ACCESS",		ACCESS, 0},			/* 2002 */
  {"ACTIVE-CLASS",	-1, 0},				/* 2002 */
  {"ADD",		ADD, 0},			/* 2002 */
  {"ADDRESS",		ADDRESS, 0},			/* 2002 */
  {"ADVANCING",		ADVANCING, 0},			/* 2002 */
  {"AFTER",		AFTER, 0},			/* 2002 */
  {"ALIGNED",		-1, 0},				/* 2002 */
  {"ALL",		ALL, 0},			/* 2002 */
  {"ALLOCATE",		-1, 0},				/* 2002 */
  {"ALPHABET",		ALPHABET, 0},			/* 2002 */
  {"ALPHABETIC",	ALPHABETIC, 0},			/* 2002 */
  {"ALPHABETIC-LOWER",	ALPHABETIC_LOWER, 0},		/* 2002 */
  {"ALPHABETIC-UPPER",	ALPHABETIC_UPPER, 0},		/* 2002 */
  {"ALPHANUMERIC",	ALPHANUMERIC, 0},		/* 2002 */
  {"ALPHANUMERIC-EDITED", ALPHANUMERIC_EDITED, 0},	/* 2002 */
  {"ALSO",		ALSO, 0},			/* 2002 */
  {"ALTER",		ALTER, 0},			/* 85 */
  {"ALTERNATE",		ALTERNATE, 0},			/* 2002 */
  {"AND",		AND, 0},			/* 2002 */
  {"ANY",		ANY, 0},			/* 2002 */
  {"ANYCASE",		-1, 0},				/* 2002 */
  {"ARE",		ARE, 0},			/* 2002 */
  {"AREA",		AREA, 0},			/* 2002 */
  {"AREAS",		AREA, 0},			/* 2002 */
  {"ARGUMENT-NUMBER",	ARGUMENT_NUMBER, 0},		/* extension */
  {"ARGUMENT-VALUE",	ARGUMENT_VALUE, 0},		/* extension */
  {"ARITHMETIC",	-1, 0},				/* 2002 (C/S) */
  {"AS",		AS, 0},				/* 2002 */
  {"ASCENDING",		ASCENDING, 0},			/* 2002 */
  {"ASSIGN",		ASSIGN, 0},			/* 2002 */
  {"AT",		AT, 0},				/* 2002 */
  {"ATTRIBUTE",		-1, 0},				/* 2002 (C/S) */
  {"AUTO",		AUTO, 0},			/* 2002 (C/S) */
  {"AUTOMATIC",		AUTOMATIC, 0},			/* extension */
  {"B-AND",		-1, 0},				/* 2002 */
  {"B-NOT",		-1, 0},				/* 2002 */
  {"B-OR",		-1, 0},				/* 2002 */
  {"B-XOR",		-1, 0},				/* 2002 */
  {"BACKGROUND-COLOR",	BACKGROUND_COLOR, 0},		/* 2002 (C/S) */
  {"BASED",		-1, 0},				/* 2002 */
  {"BEFORE",		BEFORE, 0},			/* 2002 */
  {"BELL",		BELL, 0},			/* 2002 (C/S) */
  {"BINARY",		BINARY, 0},			/* 2002 */
  {"BINARY-CHAR",	-1, 0},				/* 2002 */
  {"BINARY-DOUBLE",	-1, 0},				/* 2002 */
  {"BINARY-LONG",	-1, 0},				/* 2002 */
  {"BINARY-SHORT",	-1, 0},				/* 2002 */
  {"BIT",		-1, 0},				/* 2002 */
  {"BLANK",		BLANK, 0},			/* 2002 */
  {"BLINK",		BLINK, 0},			/* 2002 (C/S) */
  {"BLOCK",		BLOCK, 0},			/* 2002 */
  {"BOOLEAN",		-1, 0},				/* 2002 */
  {"BOTTOM",		BOTTOM, 0},			/* 2002 */
  {"BY",		BY, 0},				/* 2002 */
  {"BYTE-LENGTH",	-1, 0},				/* 2002 (C/S) */
  {"CALL",		CALL, 0},			/* 2002 */
  {"CANCEL",		CANCEL, 0},			/* 2002 */
  {"CD",		-1, 0},				/* 2002 */
  {"CENTER",		-1, 0},				/* 2002 (C/S) */
  {"CF",		-1, 0},				/* 2002 */
  {"CH",		-1, 0},				/* 2002 */
  {"CHARACTER",		CHARACTER, 0},			/* 2002 */
  {"CHARACTERS",	CHARACTERS, 0},			/* 2002 */
  {"CLASS",		CLASS, 0},			/* 2002 */
  {"CLASS-ID",		-1, 0},				/* 2002 */
  {"CLASSIFICATION",	-1, 0},				/* 2002 (C/S) */
  {"CLOSE",		CLOSE, 0},			/* 2002 */
  {"CODE",		-1, 0},				/* 2002 */
  {"CODE-SET",		CODE_SET, 0},			/* 2002 */
  {"COL",		COLUMN, 0},			/* 2002 */
  {"COLLATING",		COLLATING, 0},			/* 2002 */
  {"COLS",		COLUMN, 0},			/* 2002 */
  {"COLUMN",		COLUMN, 0},			/* 2002 */
  {"COLUMNS",		COLUMN, 0},			/* 2002 */
  {"COMMA",		COMMA, 0},			/* 2002 */
  {"COMMAND-LINE",	COMMAND_LINE, 0},		/* extension */
  {"COMMON",		COMMON, 0},			/* 2002 */
  {"COMMUNICATION",	-1, 0},				/* 2002 */
  {"COMP",		COMP, 0},			/* 2002 */
  {"COMP-1",		COMP_1, 0},			/* extension */
  {"COMP-2",		COMP_2, 0},			/* extension */
  {"COMP-3",		COMP_3, 0},			/* extension */
  {"COMP-4",		COMP_4, 0},			/* extension */
  {"COMP-5",		COMP_5, 0},			/* extension */
  {"COMP-X",		COMP_X, 0},			/* extension */
  {"COMPUTATIONAL",	COMP, 0},			/* 2002 */
  {"COMPUTATIONAL-1",	COMP_1, 0},			/* extension */
  {"COMPUTATIONAL-2",	COMP_2, 0},			/* extension */
  {"COMPUTATIONAL-3",	COMP_3, 0},			/* extension */
  {"COMPUTATIONAL-4",	COMP_4, 0},			/* extension */
  {"COMPUTATIONAL-5",	COMP_5, 0},			/* extension */
  {"COMPUTATIONAL-X",	COMP_X, 0},			/* extension */
  {"COMPUTE",		COMPUTE, 0},			/* 2002 */
  {"CONDITION",		-1, 0},				/* 2002 */
  {"CONFIGURATION",	CONFIGURATION, 0},		/* 2002 */
  {"CONSTANT",		-1, 0},				/* 2002 */
  {"CONTAINS",		CONTAINS, 0},			/* 2002 */
  {"CONTENT",		CONTENT, 0},			/* 2002 */
  {"CONTINUE",		CONTINUE, 0},			/* 2002 */
  {"CONTROL",		-1, 0},				/* 2002 */
  {"CONTROLS",		-1, 0},				/* 2002 */
  {"CONVERTING",	CONVERTING, 0},			/* 2002 */
  {"COPY",		0, 0},				/* 2002 */
  {"CORR",		CORRESPONDING, 0},		/* 2002 */
  {"CORRESPONDING",	CORRESPONDING, 0},		/* 2002 */
  {"COUNT",		COUNT, 0},			/* 2002 */
  {"CRT",		CRT, 0},			/* 2002 */
  {"CURRENCY",		CURRENCY, 0},			/* 2002 */
  {"CURSOR",		CURSOR, 0},			/* 2002 */
  {"CYCLE",		CYCLE, 0},			/* 2002 (C/S) */
  {"DATA",		DATA, 0},			/* 2002 */
  {"DATA-POINTER",	-1, 0},				/* 2002 */
  {"DATE",		DATE, 0},			/* 2002 */
  {"DAY",		DAY, 0},			/* 2002 */
  {"DAY-OF-WEEK",	DAY_OF_WEEK, 0},		/* 2002 */
  {"DE",		-1, 0},				/* 2002 */
  {"DEBUGGING",		DEBUGGING, 0},			/* 2002 */
  {"DECIMAL-POINT",	DECIMAL_POINT, 0},		/* 2002 */
  {"DECLARATIVES",	DECLARATIVES, 0},		/* 2002 */
  {"DEFAULT",		DEFAULT, 0},			/* 2002 */
  {"DELETE",		DELETE, 0},			/* 2002 */
  {"DELIMITED",		DELIMITED, 0},			/* 2002 */
  {"DELIMITER",		DELIMITER, 0},			/* 2002 */
  {"DEPENDING",		DEPENDING, 0},			/* 2002 */
  {"DESCENDING",	DESCENDING, 0},			/* 2002 */
  {"DESTINATION",	-1, 0},				/* 2002 */
  {"DETAIL",		-1, 0},				/* 2002 */
  {"DISABLE",		-1, 0},				/* 2002 */
  {"DISPLAY",		DISPLAY, 0},			/* 2002 */
  {"DIVIDE",		DIVIDE, 0},			/* 2002 */
  {"DIVISION",		DIVISION, 0},			/* 2002 */
  {"DOWN",		DOWN, 0},			/* 2002 */
  {"DUPLICATES",	DUPLICATES, 0},			/* 2002 */
  {"DYNAMIC",		DYNAMIC, 0},			/* 2002 */
  {"EBCDIC",		EBCDIC, 0},			/* extension */
  {"EC",		-1, 0},				/* 2002 */
  {"EGI",		-1, 0},				/* 2002 */
  {"ELSE",		ELSE, 0},			/* 2002 */
  {"EMI",		-1, 0},				/* 2002 */
  {"ENABLE",		-1, 0},				/* 2002 */
  {"END",		END, 0},			/* 2002 */
  {"END-ACCEPT",	END_ACCEPT, 0},			/* 2002 */
  {"END-ADD",		END_ADD, 0},			/* 2002 */
  {"END-CALL",		END_CALL, 0},			/* 2002 */
  {"END-COMPUTE",	END_COMPUTE, 0},		/* 2002 */
  {"END-DELETE",	END_DELETE, 0},			/* 2002 */
  {"END-DISPLAY",	END_DISPLAY, 0},		/* 2002 */
  {"END-DIVIDE",	END_DIVIDE, 0},			/* 2002 */
  {"END-EVALUATE",	END_EVALUATE, 0},		/* 2002 */
  {"END-IF",		END_IF, 0},			/* 2002 */
  {"END-MULTIPLY",	END_MULTIPLY, 0},		/* 2002 */
  {"END-OF-PAGE",	EOP, 0},			/* 2002 */
  {"END-PERFORM",	END_PERFORM, 0},		/* 2002 */
  {"END-READ",		END_READ, 0},			/* 2002 */
  {"END-RECEIVE",	-1, 0},				/* 2002 */
  {"END-RETURN",	END_RETURN, 0},			/* 2002 */
  {"END-REWRITE",	END_REWRITE, 0},		/* 2002 */
  {"END-SEARCH",	END_SEARCH, 0},			/* 2002 */
  {"END-START",		END_START, 0},			/* 2002 */
  {"END-STRING",	END_STRING, 0},			/* 2002 */
  {"END-SUBTRACT",	END_SUBTRACT, 0},		/* 2002 */
  {"END-UNSTRING",	END_UNSTRING, 0},		/* 2002 */
  {"END-WRITE",		END_WRITE, 0},			/* 2002 */
  {"ENTRY",		ENTRY, 0},			/* extension */
  {"ENTRY-CONVENTION",	-1, 0},				/* 2002 (C/S) */
  {"ENVIRONMENT",	ENVIRONMENT, 0},		/* 2002 */
  {"ENVIRONMENT-NAME",	ENVIRONMENT_NAME, 0},		/* extension */
  {"ENVIRONMENT-VALUE",	ENVIRONMENT_VALUE, 0},		/* extension */
  {"EO",		-1, 0},				/* 2002 */
  {"EOL",		EOL, 0},			/* 2002 (C/S) */
  {"EOP",		EOP, 0},			/* 2002 */
  {"EOS",		EOS, 0},			/* 2002 (C/S) */
  {"EQUAL",		EQUAL, 0},			/* 2002 */
  {"ERASE",		ERASE, 0},			/* 2002 (C/S) */
  {"ERROR",		ERROR, 0},			/* 2002 */
  {"ESI",		-1, 0},				/* 2002 */
  {"EVALUATE",		EVALUATE, 0},			/* 2002 */
  {"EXCEPTION",		EXCEPTION, 0},			/* 2002 */
  {"EXCEPTION-OBJECT",	-1, 0},				/* 2002 */
  {"EXCLUSIVE",		EXCLUSIVE, 0},			/* extension */
  {"EXIT",		EXIT, 0},			/* 2002 */
  {"EXPANDS",		-1, 0},				/* 2002 (C/S) */
  {"EXTEND",		EXTEND, 0},			/* 2002 */
  {"EXTERNAL",		EXTERNAL, 0},			/* 2002 */
  {"FACTORY",		-1, 0},				/* 2002 */
  {"FALSE",		TOK_FALSE, 0},			/* 2002 */
  {"FD",		FD, 0},				/* 2002 */
  {"FILE",		TOK_FILE, 0},			/* 2002 */
  {"FILE-CONTROL",	FILE_CONTROL, 0},		/* 2002 */
  {"FILLER",		FILLER, 0},			/* 2002 */
  {"FINAL",		-1, 0},				/* 2002 */
  {"FIRST",		FIRST, 0},			/* 2002 */
  {"FLOAT-EXTENDED",	-1, 0},				/* 2002 */
  {"FLOAT-LONG",	COMP_2, 0},			/* 2002 */
  {"FLOAT-SHORT",	COMP_1, 0},			/* 2002 */
  {"FOOTING",		FOOTING, 0},			/* 2002 */
  {"FOR",		FOR, 0},			/* 2002 */
  {"FOREGROUND-COLOR",	FOREGROUND_COLOR, 0},		/* 2002 (C/S) */
  {"FOREVER",		-1, 0},				/* 2002 (C/S) */
  {"FORMAT",		-1, 0},				/* 2002 */
  {"FREE",		-1, 0},				/* 2002 */
  {"FROM",		FROM, 0},			/* 2002 */
  {"FULL",		FULL, 0},			/* 2002 (C/S) */
  {"FUNCTION",		0, 0},				/* 2002 */
  {"FUNCTION-ID",	-1, 0},				/* 2002 */
  {"GENERATE",		-1, 0},				/* 2002 */
  {"GET",		-1, 0},				/* 2002 */
  {"GIVING",		GIVING, 0},			/* 2002 */
  {"GLOBAL",		GLOBAL, 0},			/* 2002 */
  {"GO",		GO, 0},				/* 2002 */
  {"GOBACK",		GOBACK, 0},			/* 2002 */
  {"GREATER",		GREATER, 0},			/* 2002 */
  {"GROUP",		-1, 0},				/* 2002 */
  {"GROUP-USAGE",	-1, 0},				/* 2002 */
  {"HEADING",		-1, 0},				/* 2002 */
  {"HIGH-VALUE",	HIGH_VALUE, 0},			/* 2002 */
  {"HIGH-VALUES",	HIGH_VALUE, 0},			/* 2002 */
  {"HIGHLIGHT",		HIGHLIGHT, 0},			/* 2002 (C/S) */
  {"I-O",		I_O, 0},			/* 2002 */
  {"I-O-CONTROL",	I_O_CONTROL, 0},		/* 2002 */
  {"ID",		IDENTIFICATION, 0},		/* extension */
  {"IDENTIFICATION",	IDENTIFICATION, 0},		/* 2002 */
  {"IF",		IF, 0},				/* 2002 */
  {"IGNORING",		-1, 0},				/* 2002 (C/S) */
  {"IMPLEMENTS",	-1, 0},				/* 2002 (C/S) */
  {"IN",		IN, 0},				/* 2002 */
  {"INDEX",		INDEX, 0},			/* 2002 */
  {"INDEXED",		INDEXED, 0},			/* 2002 */
  {"INDICATE",		-1, 0},				/* 2002 */
  {"INHERITS",		-1, 0},				/* 2002 */
  {"INITIAL",		TOK_INITIAL, 0},		/* 2002 */
  {"INITIALIZE",	INITIALIZE, 0},			/* 2002 */
  {"INITIALIZED",	-1, 0},				/* 2002 (C/S) */
  {"INITIATE",		-1, 0},				/* 2002 */
  {"INPUT",		INPUT, 0},			/* 2002 */
  {"INPUT-OUTPUT",	INPUT_OUTPUT, 0},		/* 2002 */
  {"INSPECT",		INSPECT, 0},			/* 2002 */
  {"INTERFACE",		-1, 0},				/* 2002 */
  {"INTERFACE-ID",	-1, 0},				/* 2002 */
  {"INTRINSIC",		-1, 0},				/* 2002 (C/S) */
  {"INTO",		INTO, 0},			/* 2002 */
  {"INVALID",		INVALID, 0},			/* 2002 */
  {"INVOKE",		-1, 0},				/* 2002 */
  {"IS",		IS, 0},				/* 2002 */
  {"JUST",		JUSTIFIED, 0},			/* 2002 */
  {"JUSTIFIED",		JUSTIFIED, 0},			/* 2002 */
  {"KEY",		KEY, 0},			/* 2002 */
  {"LABEL",		LABEL, 0},			/* 85 */
  {"LAST",		-1, 0},				/* 2002 */
  {"LC_ALL",		-1, 0},				/* 2002 (C/S) */
  {"LC_COLLATE",	-1, 0},				/* 2002 (C/S) */
  {"LC_CTYPE",		-1, 0},				/* 2002 (C/S) */
  {"LC_MESSAGES",	-1, 0},				/* 2002 (C/S) */
  {"LC_MONETARY",	-1, 0},				/* 2002 (C/S) */
  {"LC_NUMERIC",	-1, 0},				/* 2002 (C/S) */
  {"LC_TIME",		-1, 0},				/* 2002 (C/S) */
  {"LEADING",		LEADING, 0},			/* 2002 */
  {"LEFT",		LEFT, 0},			/* 2002 */
  {"LENGTH",		LENGTH, 0},			/* 2002 */
  {"LESS",		LESS, 0},			/* 2002 */
  {"LIMIT",		-1, 0},				/* 2002 */
  {"LIMITS",		-1, 0},				/* 2002 */
  {"LINAGE",		LINAGE, 0},			/* 2002 */
  {"LINAGE-COUNTER",	LINAGE_COUNTER, 0},		/* 2002 */
  {"LINE",		LINE, 0},			/* 2002 */
  {"LINE-COUNTER",	-1, 0},				/* 2002 */
  {"LINES",		LINES, 0},			/* 2002 */
  {"LINKAGE",		LINKAGE, 0},			/* 2002 */
  {"LOCAL-STORAGE",	LOCAL_STORAGE, 0},		/* 2002 */
  {"LOCALE",		-1, 0},				/* 2002 */
  {"LOCK",		LOCK, 0},			/* 2002 */
  {"LOW-VALUE",		LOW_VALUE, 0},			/* 2002 */
  {"LOW-VALUES",	LOW_VALUE, 0},			/* 2002 */
  {"LOWLIGHT",		LOWLIGHT, 0},			/* 2002 (C/S) */
  {"MANUAL",		MANUAL, 0},			/* extension */
  {"MEMORY",		MEMORY, 0},			/* 85 */
  {"MERGE",		MERGE, 0},			/* 2002 */
  {"MESSAGE",		-1, 0},				/* 2002 */
  {"METHOD",		-1, 0},				/* 2002 */
  {"METHOD-ID",		-1, 0},				/* 2002 */
  {"MINUS",		MINUS, 0},			/* 2002 */
  {"MODE",		MODE, 0},			/* 2002 */
  {"MOVE",		MOVE, 0},			/* 2002 */
  {"MULTIPLE",		MULTIPLE, 0},			/* 85 */
  {"MULTIPLY",		MULTIPLY, 0},			/* 2002 */
  {"NATIONAL",		NATIONAL, 0},			/* 2002 */
  {"NATIONAL-EDITED",	NATIONAL_EDITED, 0},		/* 2002 */
  {"NATIVE",		NATIVE, 0},			/* 2002 */
  {"NEGATIVE",		NEGATIVE, 0},			/* 2002 */
  {"NESTED",		-1, 0},				/* 2002 */
  {"NEXT",		NEXT, 0},			/* 2002 */
  {"NO",		NO, 0},				/* 2002 */
  {"NONE",		-1, 0},				/* 2002 (C/S) */
  {"NORMAL",		-1, 0},				/* 2002 (C/S) */
  {"NOT",		NOT, 0},			/* 2002 */
  {"NULL",		TOK_NULL, 0},			/* 2002 */
  {"NULLS",		TOK_NULL, 0},			/* extension */
  {"NUMBER",		NUMBER, 0},			/* 2002 */
  {"NUMBERS",		NUMBER, 0},			/* 2002 (C/S) */
  {"NUMERIC",		NUMERIC, 0},			/* 2002 */
  {"NUMERIC-EDITED",	NUMERIC_EDITED, 0},		/* 2002 */
  {"OBJECT",		-1, 0},				/* 2002 */
  {"OBJECT-COMPUTER",	OBJECT_COMPUTER, 0},		/* 2002 */
  {"OBJECT-REFERENCE",	-1, 0},				/* 2002 */
  {"OCCURS",		OCCURS, 0},			/* 2002 */
  {"OF",		OF, 0},				/* 2002 */
  {"OFF",		OFF, 0},			/* 2002 */
  {"OMITTED",		OMITTED, 0},			/* 2002 */
  {"ON",		ON, 0},				/* 2002 */
  {"ONLY",		ONLY, 0},			/* 2002 (C/S) */
  {"OPEN",		OPEN, 0},			/* 2002 */
  {"OPTIONAL",		OPTIONAL, 0},			/* 2002 */
  {"OPTIONS",		-1, 0},				/* 2002 */
  {"OR",		OR, 0},				/* 2002 */
  {"ORDER",		ORDER, 0},			/* 2002 */
  {"ORGANIZATION",	ORGANIZATION, 0},		/* 2002 */
  {"OTHER",		OTHER, 0},			/* 2002 */
  {"OUTPUT",		OUTPUT, 0},			/* 2002 */
  {"OVERFLOW",		OVERFLOW, 0},			/* 2002 */
  {"OVERRIDE",		-1, 0},				/* 2002 */
  {"PACKED-DECIMAL",	PACKED_DECIMAL, 0},		/* 2002 */
  {"PADDING",		PADDING, 0},			/* 2002 */
  {"PAGE",		PAGE, 0},			/* 2002 */
  {"PAGE-COUNTER",	-1, 0},				/* 2002 */
  {"PARAGRAPH",		-1, 0},				/* 2002 (C/S) */
  {"PERFORM",		PERFORM, 0},			/* 2002 */
  {"PF",		-1, 0},				/* 2002 */
  {"PH",		-1, 0},				/* 2002 */
  {"PIC",		0, 0},				/* 2002 */
  {"PICTURE",		0, 0},				/* 2002 */
  {"PLUS",		PLUS, 0},			/* 2002 */
  {"POINTER",		POINTER, 0},			/* 2002 */
  {"POSITION",		POSITION, 0},			/* 85 */
  {"POSITIVE",		POSITIVE, 0},			/* 2002 */
  {"PRESENT",		-1, 0},				/* 2002 */
  {"PREVIOUS",		-1, 0},				/* 2002 (C/S) */
  {"PRINTING",		-1, 0},				/* 2002 */
  {"PROCEDURE",		PROCEDURE, 0},			/* 2002 */
  {"PROCEDURE-POINTER",	-1, 0},				/* extension */
  {"PROCEDURES",	PROCEDURES, 0},			/* extension */
  {"PROCEED",		PROCEED, 0},			/* 85 */
  {"PROGRAM",		PROGRAM, 0},			/* 2002 */
  {"PROGRAM-ID",	PROGRAM_ID, 0},			/* 2002 */
  {"PROPERTY",		-1, 0},				/* 2002 */
  {"PROTOTYPE",		-1, 0},				/* 2002 */
  {"PURGE",		-1, 0},				/* 2002 */
  {"QUEUE",		-1, 0},				/* 2002 */
  {"QUOTE",		QUOTE, 0},			/* 2002 */
  {"QUOTES",		QUOTE, 0},			/* 2002 */
  {"RAISE",		-1, 0},				/* 2002 */
  {"RAISING",		-1, 0},				/* 2002 */
  {"RANDOM",		RANDOM, 0},			/* 2002 */
  {"RD",		-1, 0},				/* 2002 */
  {"READ",		READ, 0},			/* 2002 */
  {"RECEIVE",		-1, 0},				/* 2002 */
  {"RECORD",		RECORD, 0},			/* 2002 */
  {"RECORDING",		RECORDING, 0},			/* extension */
  {"RECORDS",		RECORDS, 0},			/* 2002 */
  {"RECURSIVE",		RECURSIVE, 0},			/* 2002 (C/S) */
  {"REDEFINES",		REDEFINES, 0},			/* 2002 */
  {"REEL",		REEL, 0},			/* 2002 */
  {"REFERENCE",		REFERENCE, 0},			/* 2002 */
  {"RELATION",		-1, 0},				/* 2002 (C/S) */
  {"RELATIVE",		RELATIVE, 0},			/* 2002 */
  {"RELEASE",		RELEASE, 0},			/* 2002 */
  {"REMAINDER",		REMAINDER, 0},			/* 2002 */
  {"REMOVAL",		REMOVAL, 0},			/* 2002 */
  {"RENAMES",		RENAMES, 0},			/* 2002 */
  {"REPLACE",		-1, 0},				/* 2002 */
  {"REPLACING",		REPLACING, 0},			/* 2002 */
  {"REPORT",		-1, 0},				/* 2002 */
  {"REPORTING",		-1, 0},				/* 2002 */
  {"REPORTS",		-1, 0},				/* 2002 */
  {"REPOSITORY",	-1, 0},				/* 2002 */
  {"REQUIRED",		REQUIRED, 0},			/* 2002 (C/S) */
  {"RESERVE",		RESERVE, 0},			/* 2002 */
  {"RESET",		-1, 0},				/* 2002 */
  {"RESUME",		-1, 0},				/* 2002 */
  {"RETRY",		-1, 0},				/* 2002 */
  {"RETURN",		RETURN, 0},			/* 2002 */
  {"RETURNING",		RETURNING, 0},			/* 2002 */
  {"REVERSE-VIDEO",	REVERSE_VIDEO, 0},		/* 2002 (C/S) */
  {"REWIND",		REWIND, 0},			/* 2002 */
  {"REWRITE",		REWRITE, 0},			/* 2002 */
  {"RF",		-1, 0},				/* 2002 */
  {"RH",		-1, 0},				/* 2002 */
  {"RIGHT",		RIGHT, 0},			/* 2002 */
  {"ROLLBACK",		ROLLBACK, 0},			/* extension */
  {"ROUNDED",		ROUNDED, 0},			/* 2002 */
  {"RUN",		RUN, 0},			/* 2002 */
  {"SAME",		SAME, 0},			/* 2002 */
  {"SCREEN",		SCREEN, 0},			/* 2002 */
  {"SD",		SD, 0},				/* 2002 */
  {"SEARCH",		SEARCH, 0},			/* 2002 */
  {"SECONDS",		-1, 0},				/* 2002 (C/S) */
  {"SECTION",		SECTION, 0},			/* 2002 */
  {"SECURE",		SECURE, 0},			/* 2002 (C/S) */
  {"SEGMENT",		-1, 0},				/* 2002 */
  {"SELECT",		SELECT, 0},			/* 2002 */
  {"SELF",		-1, 0},				/* 2002 */
  {"SEND",		-1, 0},				/* 2002 */
  {"SENTENCE",		SENTENCE, 0},			/* 2002 */
  {"SEPARATE",		SEPARATE, 0},			/* 2002 */
  {"SEQUENCE",		SEQUENCE, 0},			/* 2002 */
  {"SEQUENTIAL",	SEQUENTIAL, 0},			/* 2002 */
  {"SET",		SET, 0},			/* 2002 */
  {"SHARING",		SHARING, 0},			/* 2002 */
  {"SIGN",		SIGN, 0},			/* 2002 */
  {"SIGNED",		-1, 0},				/* 2002 (C/S) */
  {"SIZE",		SIZE, 0},			/* 2002 */
  {"SORT",		SORT, 0},			/* 2002 */
  {"SORT-MERGE",	SORT_MERGE, 0},			/* 2002 */
  {"SOURCE",		-1, 0},				/* 2002 */
  {"SOURCE-COMPUTER",	SOURCE_COMPUTER, 0},		/* 2002 */
  {"SOURCES",		-1, 0},				/* 2002 */
  {"SPACE",		SPACE, 0},			/* 2002 */
  {"SPACES",		SPACE, 0},			/* 2002 */
  {"SPECIAL-NAMES",	SPECIAL_NAMES, 0},		/* 2002 */
  {"STANDARD",		STANDARD, 0},			/* 2002 */
  {"STANDARD-1",	STANDARD_1, 0},			/* 2002 */
  {"STANDARD-2",	STANDARD_2, 0},			/* 2002 */
  {"START",		START, 0},			/* 2002 */
  {"STATEMENT",		-1, 0},				/* 2002 (C/S) */
  {"STATUS",		STATUS, 0},			/* 2002 */
  {"STEP",		-1, 0},				/* 2002 (C/S) */
  {"STOP",		STOP, 0},			/* 2002 */
  {"STRING",		STRING, 0},			/* 2002 */
  {"STRONG",		-1, 0},				/* 2002 (C/S) */
  {"SUB-QUEUE-1",	-1, 0},				/* 2002 */
  {"SUB-QUEUE-2",	-1, 0},				/* 2002 */
  {"SUB-QUEUE-3",	-1, 0},				/* 2002 */
  {"SUBTRACT",		SUBTRACT, 0},			/* 2002 */
  {"SUM",		-1, 0},				/* 2002 */
  {"SUPER",		-1, 0},				/* 2002 */
  {"SUPPRESS",		-1, 0},				/* 2002 */
  {"SYMBOL",		-1, 0},				/* 2002 (C/S) */
  {"SYMBOLIC",		SYMBOLIC, 0},			/* 2002 */
  {"SYNC",		SYNCHRONIZED, 0},		/* 2002 */
  {"SYNCHRONIZED",	SYNCHRONIZED, 0},		/* 2002 */
  {"SYSTEM-DEFAULT",	-1, 0},				/* 2002 */
  {"TABLE",		-1, 0},				/* 2002 */
  {"TALLYING",		TALLYING, 0},			/* 2002 */
  {"TAPE",		TAPE, 0},			/* 85 */
  {"TERMINAL",		-1, 0},				/* 2002 */
  {"TERMINATE",		-1, 0},				/* 2002 */
  {"TEST",		TEST, 0},			/* 2002 */
  {"TEXT",		-1, 0},				/* 2002 */
  {"THAN",		THAN, 0},			/* 2002 */
  {"THEN",		THEN, 0},			/* 2002 */
  {"THROUGH",		THRU, 0},			/* 2002 */
  {"THRU",		THRU, 0},			/* 2002 */
  {"TIME",		TIME, 0},			/* 2002 */
  {"TIMES",		TIMES, 0},			/* 2002 */
  {"TO",		TO, 0},				/* 2002 */
  {"TOP",		TOP, 0},			/* 2002 */
  {"TRAILING",		TRAILING, 0},			/* 2002 */
  {"TRUE",		TOK_TRUE, 0},			/* 2002 */
  {"TYPE",		-1, 0},				/* 2002 */
  {"TYPEDEF",		-1, 0},				/* 2002 */
  {"UCS-4",		-1, 0},				/* 2002 (C/S) */
  {"UNDERLINE",		UNDERLINE, 0},			/* 2002 (C/S) */
  {"UNIT",		UNIT, 0},			/* 2002 */
  {"UNIVERSAL",		-1, 0},				/* 2002 */
  {"UNLOCK",		-1, 0},				/* 2002 */
  {"UNSIGNED",		-1, 0},				/* 2002 (C/S) */
  {"UNSTRING",		UNSTRING, 0},			/* 2002 */
  {"UNTIL",		UNTIL, 0},			/* 2002 */
  {"UP",		UP, 0},				/* 2002 */
  {"UPON",		UPON, 0},			/* 2002 */
  {"USAGE",		USAGE, 0},			/* 2002 */
  {"USE",		USE, 0},			/* 2002 */
  {"USER-DEFAULT",	-1, 0},				/* 2002 */
  {"USING",		USING, 0},			/* 2002 */
  {"UTF-16",		-1, 0},				/* 2002 (C/S) */
  {"UTF-8",		-1, 0},				/* 2002 (C/S) */
  {"VAL-STATUS",	-1, 0},				/* 2002 */
  {"VALID",		-1, 0},				/* 2002 */
  {"VALIDATE",		-1, 0},				/* 2002 */
  {"VALIDATE-STATUS",	-1, 0},				/* 2002 */
  {"VALUE",		VALUE, 0},			/* 2002 */
  {"VALUES",		VALUE, 0},			/* 2002 */
  {"VARYING",		VARYING, 0},			/* 2002 */
  {"WHEN",		WHEN, 0},			/* 2002 */
  {"WITH",		WITH, 0},			/* 2002 */
  {"WORKING-STORAGE",	WORKING_STORAGE, 0},		/* 2002 */
  {"WRITE",		WRITE, 0},			/* 2002 */
  {"YYYYDDD",		YYYYDDD, 0},			/* 2002 (C/S) */
  {"YYYYMMDD",		YYYYMMDD, 0},			/* 2002 (C/S) */
  {"ZERO",		ZERO, 0},			/* 2002 */
  {"ZEROES",		ZERO, 0},			/* 2002 */
  {"ZEROS",		ZERO, 0},			/* 2002 */
  {0, 0, 0}
};

/* FUNCTION List */
/* Name, Arguments, Implemented, Enum intrinsic, Routine, Field, Category */
static struct cb_intrinsic_table function_list[] = {
  { "ABS",			 1, 1, CB_INTR_ABS,
				 "cob_intr_abs",
				 CB_CATEGORY_NUMERIC },
  { "ACOS",			 1, 1, CB_INTR_ACOS,
				 "cob_intr_acos",
				 CB_CATEGORY_NUMERIC },
  { "ANNUITY",			 2, 1, CB_INTR_ANNUITY,
				 "cob_intr_annuity",
				 CB_CATEGORY_NUMERIC },
  { "ASIN",			 1, 1, CB_INTR_ASIN,
				 "cob_intr_asin",
				 CB_CATEGORY_NUMERIC },
  { "ATAN",			 1, 1, CB_INTR_ATAN,
				 "cob_intr_atan",
				 CB_CATEGORY_NUMERIC },
  { "BOOLEAN-OF-INTEGER",	 2, 0, CB_INTR_BOOLEAN_OF_INTEGER,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "BYTE-LENGTH",		 1, 0, CB_INTR_BYTE_LENGTH,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "CHAR",			 1, 1, CB_INTR_CHAR,
				 "cob_intr_char",
				 CB_CATEGORY_ALPHANUMERIC },
  { "CHAR-NATIONAL",		 1, 0, CB_INTR_CHAR_NATIONAL,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "COS",			 1, 1, CB_INTR_COS,
				 "cob_intr_cos",
				 CB_CATEGORY_NUMERIC },
  { "CURRENT-DATE",		 0, 1, CB_INTR_CURRENT_DATE,
				 "cob_intr_current_date",
				 CB_CATEGORY_ALPHANUMERIC },
  { "DATE-OF-INTEGER",		 1, 1, CB_INTR_DATE_OF_INTEGER,
				 "cob_intr_date_of_integer",
				 CB_CATEGORY_NUMERIC },
  { "DATE-TO-YYYYMMDD",		-1, 1, CB_INTR_DATE_TO_YYYYMMDD,
				 "cob_intr_date_to_yyyymmdd",
				 CB_CATEGORY_NUMERIC },
  { "DAY-OF-INTEGER",		 1, 1, CB_INTR_DAY_OF_INTEGER,
				 "cob_intr_day_of_integer",
				 CB_CATEGORY_NUMERIC },
  { "DAY-TO-YYYYDDD",		-1, 1, CB_INTR_DAY_TO_YYYYDDD,
				 "cob_intr_day_to_yyyyddd",
				 CB_CATEGORY_NUMERIC },
  { "DISPLAY-OF",		-1, 0, CB_INTR_DISPLAY_OF,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "E",			 0, 1, CB_INTR_E,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "EXCEPTION-FILE",		 0, 0, CB_INTR_EXCEPTION_FILE,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXCEPTION-FILE-N",		 0, 0, CB_INTR_EXCEPTION_FILE_N,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXCEPTION-LOCATION",	 0, 0, CB_INTR_EXCEPTION_LOCATION,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXCEPTION-LOCATION-N",	 0, 0, CB_INTR_EXCEPTION_LOCATION_N,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXCEPTION-STATEMENT",	 0, 0, CB_INTR_EXCEPTION_STATEMENT,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXCEPTION-STATUS",		 0, 0, CB_INTR_EXCEPTION_STATUS,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "EXP",			 1, 1, CB_INTR_EXP,
				 "cob_intr_exp",
				 CB_CATEGORY_NUMERIC },
  { "EXP10",			 1, 1, CB_INTR_EXP10,
				 "cob_intr_exp10",
				 CB_CATEGORY_NUMERIC },
  { "FACTORIAL",		 1, 1, CB_INTR_FACTORIAL,
				 "cob_intr_factorial",
				 CB_CATEGORY_NUMERIC },
  { "FRACTION-PART",		 1, 1, CB_INTR_FRACTION_PART,
				 "cob_intr_fraction_part",
				 CB_CATEGORY_NUMERIC },
  { "HIGHEST-ALGEBRAIC",	 1, 0, CB_INTR_HIGHEST_ALGEBRAIC,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "INTEGER",			 1, 1, CB_INTR_INTEGER,
				 "cob_intr_integer",
				 CB_CATEGORY_NUMERIC },
  { "INTEGER-OF-BOOLEAN",	 1, 0, CB_INTR_INTEGER_OF_BOOLEAN,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "INTEGER-OF-DATE",		 1, 1, CB_INTR_INTEGER_OF_DATE,
				 "cob_intr_integer_of_date",
				 CB_CATEGORY_NUMERIC },
  { "INTEGER-OF-DAY",		 1, 1, CB_INTR_INTEGER_OF_DAY,
				 "cob_intr_integer_of_day",
				 CB_CATEGORY_NUMERIC },
  { "INTEGER-PART",		 1, 1, CB_INTR_INTEGER_PART,
				 "cob_intr_integer_part",
				 CB_CATEGORY_NUMERIC },
  { "LENGTH",			 1, 1, CB_INTR_LENGTH,
				 "cob_intr_length",
				 CB_CATEGORY_NUMERIC },
  { "LOCALE-COMPARE",		-1, 0, CB_INTR_LOCALE_COMPARE,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "LOCALE-DATE",		-1, 0, CB_INTR_LOCALE_DATE,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "LOCALE-TIME",		-1, 0, CB_INTR_LOCALE_TIME,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "LOG",			 1, 1, CB_INTR_LOG,
				 "cob_intr_log",
				 CB_CATEGORY_NUMERIC },
  { "LOG10",			 1, 1, CB_INTR_LOG10,
				 "cob_intr_log10",
				 CB_CATEGORY_NUMERIC },
  { "LOWER-CASE",		 1, 1, CB_INTR_LOWER_CASE,
				 "cob_intr_lower_case",
				 CB_CATEGORY_NUMERIC },
  { "LOWEST-ALGEBRAIC",		 1, 0, CB_INTR_LOWEST_ALGEBRAIC,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "MAX",			-1, 1, CB_INTR_MAX,
				 "cob_intr_max",
				 CB_CATEGORY_NUMERIC },
  { "MEAN",			-1, 1, CB_INTR_MEAN,
				 "cob_intr_mean",
				 CB_CATEGORY_NUMERIC },
  { "MEDIAN",			-1, 1, CB_INTR_MEDIAN,
				 "cob_intr_median",
				 CB_CATEGORY_NUMERIC },
  { "MIDRANGE",			-1, 1, CB_INTR_MIDRANGE,
				 "cob_intr_midrange",
				 CB_CATEGORY_NUMERIC },
  { "MIN",			-1, 1, CB_INTR_MIN,
				 "cob_intr_min",
				 CB_CATEGORY_NUMERIC },
  { "MOD",			 2, 1, CB_INTR_MOD,
				 "cob_intr_mod",
				 CB_CATEGORY_NUMERIC },
  { "NATIONAL-OF",		-1, 0, CB_INTR_NATIONAL_OF,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "NUMVAL",			 1, 1, CB_INTR_NUMVAL,
				 "cob_intr_numval",
				 CB_CATEGORY_NUMERIC },
  { "NUMVAL-C",			-1, 1, CB_INTR_NUMVAL_C,
				 "cob_intr_numval_c",
				 CB_CATEGORY_NUMERIC },
  { "NUMVAL-F",			 1, 0, CB_INTR_NUMVAL_F,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "ORD",			 1, 1, CB_INTR_ORD,
				 "cob_intr_ord",
				 CB_CATEGORY_NUMERIC },
  { "ORD-MAX",			-1, 1, CB_INTR_ORD_MAX,
				 "cob_intr_ord_max",
				 CB_CATEGORY_NUMERIC },
  { "ORD-MIN",			-1, 1, CB_INTR_ORD_MIN,
				 "cob_intr_ord_min",
				 CB_CATEGORY_NUMERIC },
  { "PI",			 0, 1, CB_INTR_PI,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "PRESENT-VALUE",		-1, 1, CB_INTR_PRESENT_VALUE,
				 "cob_intr_present_value",
				 CB_CATEGORY_NUMERIC },
  { "RANDOM",			-1, 1, CB_INTR_RANDOM,
				 "cob_intr_random",
				 CB_CATEGORY_NUMERIC },
  { "RANGE",			-1, 1, CB_INTR_RANGE,
				 "cob_intr_range",
				 CB_CATEGORY_NUMERIC },
  { "REM",			 2, 1, CB_INTR_REM,
				 "cob_intr_rem",
				 CB_CATEGORY_NUMERIC },
  { "REVERSE",			 1, 1, CB_INTR_REVERSE,
				 "cob_intr_reverse",
				 CB_CATEGORY_NUMERIC },
  { "SIGN",			 1, 1, CB_INTR_SIGN,
				 "cob_intr_sign",
				 CB_CATEGORY_NUMERIC },
  { "SIN",			 1, 1, CB_INTR_SIN,
				 "cob_intr_sin",
				 CB_CATEGORY_NUMERIC },
  { "SQRT",			 1, 1, CB_INTR_SQRT,
				 "cob_intr_sqrt",
				 CB_CATEGORY_NUMERIC },
  { "STANDARD-COMPARE",		-1, 0, CB_INTR_STANDARD_COMPARE,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "STANDARD-DEVIATION",	-1, 1, CB_INTR_STANDARD_DEVIATION,
				 "cob_intr_standard_deviation",
				 CB_CATEGORY_NUMERIC },
  { "SUM",			-1, 1, CB_INTR_SUM,
				 "cob_intr_sum",
				 CB_CATEGORY_NUMERIC },
  { "TAN",			 1, 1, CB_INTR_TAN,
				 "cob_intr_tan",
				 CB_CATEGORY_NUMERIC },
  { "TEST-DATE-YYYYMMDD",	 1, 1, CB_INTR_TEST_DATE_YYYYMMDD,
				 "cob_intr_test_date_yyyymmdd",
				 CB_CATEGORY_NUMERIC },
  { "TEST-DAY-YYYYDDD",		 1, 1, CB_INTR_TEST_DAY_YYYYDDD,
				 "cob_intr_test_day_yyyyddd",
				 CB_CATEGORY_NUMERIC },
  { "TEST-NUMVAL",		 1, 0, CB_INTR_TEST_NUMVAL,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "TEST-NUMVAL-C",		-1, 0, CB_INTR_TEST_NUMVAL_C,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "TEST-NUMVAL-F",		 1, 0, CB_INTR_TEST_NUMVAL_F,
				 NULL,
				 CB_CATEGORY_NUMERIC },
  { "UPPER-CASE",		 1, 1, CB_INTR_UPPER_CASE,
				 "cob_intr_upper_case",
				 CB_CATEGORY_NUMERIC },
  { "VARIANCE",			-1, 1, CB_INTR_VARIANCE, 
				 "cob_intr_variance",
				 CB_CATEGORY_NUMERIC },
  { "WHEN-COMPILED",		 0, 1, CB_INTR_WHEN_COMPILED,
				 NULL,
				 CB_CATEGORY_ALPHANUMERIC },
  { "YEAR-TO-YYYY",		-1, 1, CB_INTR_YEAR_TO_YYYY,
				 "cob_intr_year_to_yyyy",
				 CB_CATEGORY_NUMERIC }
};
#define	NUM_INTRINSICS	sizeof(function_list) / sizeof(struct cb_intrinsic_table)

static int
hash (const char *s)
{
	int	val = 0;

	for (; *s; s++) {
		val += toupper (*s);
	}
	return val % HASH_SIZE;
}

cb_tree
lookup_system_name (const char *name)
{
	int	i;

	for (i = 0; system_table[i].name != 0; i++) {
		if (strcasecmp (name, system_table[i].name) == 0) {
			return system_table[i].node;
		}
	}
	return cb_error_node;
}

int
lookup_reserved_word (const char *name)
{
  struct reserved	*p;

  for (p = reserved_table[hash (name)]; p; p = p->next) {
    if (strcasecmp (name, p->name) == 0) {
	if (p->token != -1) {
	  struct noreserve *noresptr;

	  for ( noresptr = norestab; noresptr; noresptr = noresptr->next) {
		if (strcasecmp (name, noresptr->noresword) == 0) {
			return 0;
		}
	  }
	  return p->token;
	}
	cb_error (_("`%s' reserved word, but not supported yet"), name);
	return 0;
    }
  }
  return 0;
}

struct cb_intrinsic_table *
lookup_intrinsic (const char *name)
{
	int				i;
	struct cb_intrinsic_table	*cbp;

	i = 0;
	cbp = &function_list[0];
	for ( ; i < NUM_INTRINSICS; i++, cbp++ ) {
		if ( strcasecmp(name, cbp->name) == 0 ) {
			if ( cbp->implemented ) {
				return cbp;
			} else {
				return NULL;
			}
		}
	}
	return NULL;
}

void
cb_list_reserved (void)
{
	int	i;

	for (i = 0; reserved_words[i].name; i++) {
		puts (reserved_words[i].name);
	}
}

void
cb_init_reserved (void)
{
	int	i;

	/* build system-name table */
	for (i = 0; system_table[i].name != 0; i++) {
		system_table[i].node =
		  cb_build_system_name (system_table[i].category, system_table[i].token);
	}

	/* initialize reserved-word table */
	for (i = 0; i < HASH_SIZE; i++) {
		reserved_table[i] = NULL;
	}

	/* build reserved-word table */
	for (i = 0; reserved_words[i].name != 0; i++) {
		int	val = hash (reserved_words[i].name);

		reserved_words[i].next = reserved_table[val];
		reserved_table[val] = &reserved_words[i];
	}
}
