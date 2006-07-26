/*
 * Copyright (C) 2001-2006 Keisuke Nishida
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

%expect 114

%defines
%verbose

%{
#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <libcob.h>

#include "cobc.h"
#include "tree.h"

#define yyerror			cb_error
#define YYDEBUG			COB_DEBUG
#define YYERROR_VERBOSE		1

#define PENDING(x)		cb_warning (_("'%s' not implemented"), x)

#define emit_statement(x) \
  current_program->exec_list = cb_cons (x, current_program->exec_list)

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

#define BEGIN_STATEMENT(name)					\
  current_statement = cb_build_statement ((char *)name);		\
  CB_TREE (current_statement)->source_file = (unsigned char *)cb_source_file;	\
  CB_TREE (current_statement)->source_line = cb_source_line;	\
  emit_statement (CB_TREE (current_statement));			\
  main_statement = current_statement

#define BEGIN_IMPLICIT_STATEMENT()					\
  current_statement = cb_build_statement (NULL);			\
  main_statement->body = cb_list_add (main_statement->body,		\
				      CB_TREE (current_statement))

static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_file		*current_file;

enum cb_storage			current_storage;

static cb_tree			call_mode;

static cb_tree			perform_stack = NULL;

static int			next_label_id = 0;
static int			current_linage = 0;
static int			eval_level = 0;
static int			eval_inc = 0;
static int			eval_inc2 = 0;
static int			prog_end = 0;
static int			depth = 0;
static int			samearea = 1;
static struct cb_file		*linage_file;
static cb_tree			next_label_list = NULL;
static int			eval_check[64][64];

static void emit_entry (const char *name, const int encode, cb_tree using_list);
static void terminator_warning (void);
static void terminator_error (void);
static int literal_value (cb_tree x);
%}

%token WORD LITERAL PICTURE MNEMONIC_NAME FUNCTION_NAME

%token ACCEPT ADD ADDRESS CALL CANCEL CLOSE COMPUTE DELETE DISPLAY DIVIDE ENTRY
%token EVALUATE IF INITIALIZE INSPECT MERGE MOVE MULTIPLY OPEN PERFORM
%token READ RELEASE RETURN REWRITE SEARCH SET SORT START STRING
%token SUBTRACT UNSTRING WRITE WORKING_STORAGE ZERO PACKED_DECIMAL RECURSIVE
%token LINAGE FOOTING TOP BOTTOM SHARING ONLY RECORDING LOCAL_STORAGE
%token ACCESS ADVANCING AFTER ALL ALPHABET ALPHABETIC ALPHABETIC_LOWER AS
%token ALPHABETIC_UPPER ALPHANUMERIC ALPHANUMERIC_EDITED ALSO ALTER ALTERNATE
%token AND ANY ARE AREA ARGUMENT_NUMBER ARGUMENT_VALUE ASCENDING ASSIGN
%token AT AUTO BACKGROUND_COLOR BEFORE BELL
%token BINARY BLANK BLINK BLOCK BY CHARACTER CHARACTERS CLASS CODE_SET CYCLE
%token COLLATING COLUMN COMMA COMMAND_LINE COMMON CONFIGURATION CONTAINS
%token CONTENT CONTINUE CONVERTING CORRESPONDING COUNT CRT CURRENCY CURSOR
%token DATA DATE DAY DAY_OF_WEEK DEBUGGING DECIMAL_POINT DECLARATIVES DEFAULT
%token DELIMITED DELIMITER DEPENDING DESCENDING DIVISION DOWN DUPLICATES
%token DYNAMIC ELSE END END_ACCEPT END_ADD END_CALL END_COMPUTE END_DELETE
%token END_DISPLAY END_DIVIDE END_EVALUATE END_IF END_MULTIPLY END_PERFORM
%token END_READ END_RETURN END_REWRITE END_SEARCH END_START END_STRING
%token END_SUBTRACT END_UNSTRING END_WRITE ENVIRONMENT
%token EBCDIC ENVIRONMENT_NAME ENVIRONMENT_VALUE YYYYMMDD YYYYDDD
%token EOL EOS EOP EQUAL ERASE ERROR EXCEPTION EXIT EXTEND EXTERNAL FD GOBACK
%token FILE_CONTROL FILLER FIRST FOR FOREGROUND_COLOR FROM FULL GE GIVING
%token GLOBAL GO GREATER HIGHLIGHT HIGH_VALUE IDENTIFICATION IN INDEX INDEXED
%token INPUT INPUT_OUTPUT INTO INVALID IS I_O I_O_CONTROL JUSTIFIED KEY LABEL
%token LE LEADING LEFT LENGTH LESS LINE LINES LINKAGE LOCK LOWLIGHT LOW_VALUE
%token MEMORY MINUS MODE MULTIPLE NATIONAL NATIONAL_EDITED NATIVE NE NEGATIVE
%token NEXT NO NOT NUMBER NUMERIC NUMERIC_EDITED OBJECT_COMPUTER OCCURS OF OFF
%token OMITTED ON OPTIONAL OR ORDER ORGANIZATION OTHER OUTPUT OVERFLOW PADDING
%token PAGE PLUS POINTER POSITION POSITIVE PROCEDURE PROCEDURES PROCEED PROGRAM
%token PROGRAM_ID QUOTE RANDOM RECORD RECORDS REDEFINES REEL REFERENCE
%token RELATIVE REMAINDER REMOVAL RENAMES REPLACING REQUIRED RESERVE RETURNING
%token REVERSE_VIDEO REWIND RIGHT ROUNDED RUN SAME SCREEN SD SECTION SECURE
%token SELECT SENTENCE SEPARATE SEQUENCE SEQUENTIAL SIGN SIZE SORT_MERGE
%token SOURCE_COMPUTER SPACE SPECIAL_NAMES STANDARD STANDARD_1 STANDARD_2
%token STATUS STOP SYMBOLIC SYNCHRONIZED TALLYING TAPE TEST THAN THEN THRU
%token TIME TIMES TO TOK_FILE TOK_INITIAL TOK_TRUE TOK_FALSE TOK_NULL TRAILING
%token UNDERLINE UNIT UNTIL UP UPON USAGE USE USING VALUE VARYING WHEN WITH
%token MANUAL AUTOMATIC EXCLUSIVE ROLLBACK OVERLINE PROMPT UPDATE ESCAPE
%token COMP COMP_1 COMP_2 COMP_3 COMP_4 COMP_5 COMP_X
%token SOURCE SCREEN_CONTROL EVENT_STATUS
%token SIGNED_SHORT SIGNED_INT SIGNED_LONG UNSIGNED_SHORT UNSIGNED_INT UNSIGNED_LONG
%token BINARY_CHAR BINARY_SHORT BINARY_LONG BINARY_DOUBLE SIGNED UNSIGNED
%token LINAGE_COUNTER PROGRAM_POINTER CHAINING BLANK_SCREEN BLANK_LINE
%token NOT_EXCEPTION SIZE_ERROR NOT_SIZE_ERROR NOT_OVERFLOW NOT_EOP
%token INVALID_KEY NOT_INVALID_KEY COMMA_DELIM DISK NO_ADVANCING
%token PREVIOUS UNLOCK ALLOCATE INITIALIZED FREE BASED

%left '+' '-'
%left '*' '/'
%left UNARY_SIGN
%right '^'


%%
/*****************************************************************************
 * COBOL Compilation Unit
 *****************************************************************************/

start:
  {
	perform_stack = NULL;
	next_label_id = 0;
	current_linage = 0;
	current_storage = 0;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	prog_end = 0;
	depth = 0;
	samearea = 1;
	memset ((char *)eval_check, 0, sizeof(eval_check));
	entry_number = 0;
	linage_file = NULL;
	next_label_list = NULL;
	current_program = cb_build_program ();
	cb_build_registers ();
	current_program->gen_main = cb_flag_main;
  }
  /* program_definition */
  nested_list
  {
	if (!current_program->validated) {
		current_program->validated = 1;
		cb_validate_program_body (current_program);
	}
	if (errorcount > 0)
		YYABORT;
	if (!current_program->entry_list) {
		emit_entry (current_program->program_id, 0, NULL);
	}
  }
;

nested_list:
  program_definition
| nested_list program_definition
;

program_definition:
  identification_division
  environment_division	{ cb_validate_program_environment (current_program); }
  data_division		{ cb_validate_program_data (current_program); }
  procedure_division
  nested_prog
  end_program
;

nested_prog:
| program_definition
| nested_prog program_definition
;

end_program:
| END PROGRAM program_name '.'
  {
	if (depth) {
		depth--;
	}
	if (!current_program->validated) {
		current_program->validated = 1;
		cb_validate_program_body (current_program);
	}
  }
;


/*****************************************************************************
 * Identification division
 *****************************************************************************/

identification_division:
  PROGRAM_ID '.' program_name as_literal program_type dot
  {
	if (prog_end) {
		struct cb_program	*newx;

		if (!current_program->validated) {
			current_program->validated = 1;
			cb_validate_program_body (current_program);
		}
		perform_stack = NULL;
		next_label_id = 0;
		current_linage = 0;
		current_storage = 0;
		eval_level = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		samearea = 1;
		memset ((char *)eval_check, 0, sizeof(eval_check));
		entry_number = 0;
		linage_file = NULL;
		next_label_list = NULL;
		newx = current_program;
		current_program = cb_build_program ();
		current_program->next_program = newx;
		cb_build_registers ();
		current_program->static_func = depth;
	} else {
		prog_end = 1;
	}
	depth++;
	current_program->program_id = cb_build_program_id ($3, $4);
  }
;

program_name:
  WORD
| LITERAL
;

as_literal:
  /* empty */			{ $$ = NULL; }
| AS LITERAL			{ $$ = $2; }
;

program_type:
| _is COMMON _program		{ current_program->flag_common = 1; }
| _is TOK_INITIAL _program	{ current_program->flag_initial = 1; }
| _is RECURSIVE _program	{ current_program->flag_recursive = 1; }
;

dot: | '.' ;


/*****************************************************************************
 * Environment division
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION '.'
  configuration_section
  input_output_section
;


/*******************
 * Configuration section
 *******************/

configuration_section:
| CONFIGURATION SECTION '.'
  configuration_list
;

configuration_list:
| configuration_list configuration_paragraph
;

configuration_paragraph:
  source_computer_paragraph
| object_computer_paragraph
| special_names_paragraph
;


/*
 * SOURCE-COMPUTER paragraph
 */

source_computer_paragraph:
  SOURCE_COMPUTER '.' source_computer_entry
;

source_computer_entry:
| '.'
| computer_name '.'
| computer_name with_debugging_mode '.'
| with_debugging_mode '.'
;

with_debugging_mode:
  _with DEBUGGING MODE
  {
    cb_verify (cb_debugging_line, "DEBUGGING MODE");
  }
;

computer_name:
  WORD { }
;


/*
 * OBJECT-COMPUTER paragraph
 */

object_computer_paragraph:
  OBJECT_COMPUTER '.' object_computer_entry
;

object_computer_entry:
| '.'
| computer_name '.'
| computer_name object_computer_phrase_sequence '.'
| object_computer_phrase_sequence '.'
;

object_computer_phrase_sequence:
| object_computer_phrase_sequence object_computer_phrase
;

object_computer_phrase:
  _program _collating SEQUENCE _is reference
  {
    current_program->collating_sequence = $5;
  }
| MEMORY SIZE _is integer CHARACTERS
  {
    cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
;


/*
 * SPECIAL-NAMES paragraph
 */

special_names_paragraph:
  SPECIAL_NAMES '.' opt_special_names
;

opt_special_names:
| special_name_list '.'
;

special_name_list:
  special_name
| special_name_list special_name
;

special_name:
  mnemonic_name_clause
| alphabet_name_clause
| symbolic_characters_clause
| class_name_clause
| currency_sign_clause
| decimal_point_clause
| cursor_clause
| crt_status_clause
| screen_control
| event_status
;


/* Mnemonic name clause */

mnemonic_name_clause:
  WORD
  {
    $$ = lookup_system_name (CB_NAME ($1));
    if ($$ == cb_error_node)
      cb_error_x ($1, _("Unknown system-name '%s'"), CB_NAME ($1));
  }
  special_name_mnemonic_define
  special_name_mnemonic_on_off
;

special_name_mnemonic_define:
| IS CRT
  {
	if (CB_SYSTEM_NAME($0)->token != CB_DEVICE_CONSOLE) {
		cb_error_x ($0, _("Invalid CRT clause"));
	}
	/* current_program->flag_screen = 1; */
  }
| IS undefined_word		{ cb_define ($2, $0); }
;

special_name_mnemonic_on_off:
| special_name_mnemonic_on_off
  on_or_off _status _is undefined_word
  {
    cb_define_switch_name ($5, $-1, $2, $-2);
  }
;

on_or_off:
  ON				{ $$ = cb_int1; }
| OFF				{ $$ = cb_int0; }
;


/* Alphabet name clause */

alphabet_name_clause:
  ALPHABET undefined_word _is alphabet_definition
;

alphabet_definition:
  NATIVE	{ cb_build_alphabet_name ($-1, CB_ALPHABET_NATIVE); }
| STANDARD_1	{ cb_build_alphabet_name ($-1, CB_ALPHABET_STANDARD_1); }
| STANDARD_2	{ cb_build_alphabet_name ($-1, CB_ALPHABET_STANDARD_2); }
| EBCDIC	{ cb_build_alphabet_name ($-1, CB_ALPHABET_EBCDIC); }
| alphabet_literal_list
  {
    cb_tree x = cb_build_alphabet_name ($-1, CB_ALPHABET_CUSTOM);
    CB_ALPHABET_NAME (x)->custom_list = $1;
    current_program->alphabet_name_list =
      cb_list_add (current_program->alphabet_name_list, x);
  }
;

alphabet_literal_list:
  alphabet_literal		{ $$ = cb_list ($1); }
| alphabet_literal_list
  alphabet_literal		{ $$ = cb_list_add ($1, $2); }
;

alphabet_literal:
  LITERAL			{ $$ = $1; }
| LITERAL THRU LITERAL		{ $$ = cb_build_pair ($1, $3); }
| LITERAL ALSO			{ $$ = cb_list ($1); }
  alphabet_also_sequence	{ $$ = $3; }
;

alphabet_also_sequence:
  alphabet_also_literal
| alphabet_also_sequence ALSO	{ $$ = $0; }
  alphabet_also_literal
;

alphabet_also_literal:
  LITERAL			{ cb_list_add ($0, $1); }
| SPACE				{ /* ignore */ }
| ZERO				{ /* ignore */ }
| QUOTE				{ /* ignore */ }
| HIGH_VALUE			{ cb_high = CB_VALUE ($0); }
| LOW_VALUE			{ cb_low = CB_VALUE ($0); }
;


/* Symbolic characters clause */

symbolic_characters_clause:
  SYMBOLIC _characters symbolic_characters_list
  {
    PENDING ("SYMBOLIC CHARACTERS");
  }
;

symbolic_characters_list:
  symbolic_characters
| symbolic_characters_list symbolic_characters
;

symbolic_characters:
  char_list is_are integer_list
;

char_list:
  WORD { }
| char_list WORD { }
;

integer_list:
  integer { }
| integer_list integer { }
;


/* Class name clause */

class_name_clause:
  CLASS undefined_word _is class_item_list
  {
    current_program->class_name_list =
      cb_list_add (current_program->class_name_list,
		   cb_build_class_name ($2, $4));
  }
;

class_item_list:
  class_item			{ $$ = cb_list ($1); }
| class_item_list class_item	{ $$ = cb_list_add ($1, $2); }
;

class_item:
  literal			{ $$ = $1; }
| literal THRU literal
  {
    /* if (CB_LITERAL ($1)->data[0] < CB_LITERAL ($3)->data[0]) */
    if (literal_value ($1) < literal_value ($3))
      $$ = cb_build_pair ($1, $3);
    else
      $$ = cb_build_pair ($3, $1);
  }
;


/* CURRENCY SIGN clause */

currency_sign_clause:
  CURRENCY _sign _is LITERAL
  {
    unsigned char *s = CB_LITERAL ($4)->data;
    if (CB_LITERAL ($4)->size != 1)
      cb_error_x ($4, _("Invalid currency sign '%s'"), s);
    switch (*s) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case 'a':
    case 'A':
    case 'b':
    case 'B':
    case 'c':
    case 'C':
    case 'd':
    case 'D':
    case 'e':
    case 'E':
    case 'n':
    case 'N':
    case 'p':
    case 'P':
    case 'r':
    case 'R':
    case 's':
    case 'S':
    case 'v':
    case 'V':
    case 'x':
    case 'X':
    case 'z':
    case 'Z':
    case '+':
    case '-':
    case ',':
    case '.':
    case '*':
    case '/':
    case ';':
    case '(':
    case ')':
    case '=':
    case '"':
    case ' ':
	cb_error_x ($4, _("Invalid currency sign '%s'"), s);
	break;
    default:
	break;
    }
    current_program->currency_symbol = s[0];
  }
;


/* DECIMAL-POINT clause */

decimal_point_clause:
  DECIMAL_POINT _is COMMA
  {
    current_program->decimal_point = ',';
    current_program->numeric_separator = '.';
  }
;


/* CURSOR clause */

cursor_clause:
  CURSOR _is reference		{ current_program->cursor_pos = $3; }
;


/* CRT STATUS clause */

crt_status_clause:
  CRT STATUS _is reference	{ current_program->crt_status = $4; }
;


/* SCREEN CONTROL */

screen_control:
  SCREEN_CONTROL _is reference	{  PENDING ("SCREEN CONTROL"); }
;

/* EVENT STATUS */

event_status:
  EVENT_STATUS _is reference	{  PENDING ("EVENT STATUS"); }
;

/*******************
 * INPUT-OUTPUT SECTION
 *******************/

input_output_section:
| INPUT_OUTPUT SECTION '.'
  file_control_paragraph
  i_o_control_paragraph
;


/*
 * FILE-CONTROL paragraph
 */

file_control_paragraph:
| FILE_CONTROL '.' file_control_sequence
;

file_control_sequence:
| file_control_sequence file_control_entry
;

file_control_entry:
  SELECT flag_optional undefined_word
  {
    if ($3 == cb_error_node)
      YYERROR;

    /* build new file */
    current_file = build_file ($3);
    current_file->optional = CB_INTEGER ($2)->val;

    /* register the file */
    current_program->file_list =
      cb_cons (CB_TREE (current_file), current_program->file_list);
  }
  select_clause_sequence '.'
  {
    validate_file (current_file, $3);
  }
;

select_clause_sequence:
| select_clause_sequence select_clause
;

select_clause:
  assign_clause
| access_mode_clause
| alternative_record_key_clause
| collating_sequence_clause
| file_status_clause
| lock_mode_clause
| organization_clause
| padding_character_clause
| record_delimiter_clause
| record_key_clause
| relative_key_clause
| reserve_clause
| sharing_clause
;


/* ASSIGN clause */

assign_clause:
  ASSIGN _to _ext_clause _disk assignment_name
  {
    current_file->assign = cb_build_assignment_name ($5);
  }
;

_ext_clause:
| EXTERNAL
| DYNAMIC
;

assignment_name:
  alnum_literal
| qualified_word
;


/* ACCESS MODE clause */

access_mode_clause:
  ACCESS _mode _is access_mode
;

access_mode:
  SEQUENTIAL		{ current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
| DYNAMIC		{ current_file->access_mode = COB_ACCESS_DYNAMIC; }
| RANDOM		{ current_file->access_mode = COB_ACCESS_RANDOM; }
;


/* ALTERNATIVE RECORD KEY clause */

alternative_record_key_clause:
  ALTERNATE RECORD _key _is reference opt_splitk flag_duplicates
  {
    struct cb_alt_key *p = cobc_malloc (sizeof (struct cb_alt_key));
    p->key = $5;
    p->duplicates = CB_INTEGER ($7)->val;
    p->next = NULL;

    /* add to the end of list */
    if (current_file->alt_key_list == NULL)
      current_file->alt_key_list = p;
    else
      {
	struct cb_alt_key *l = current_file->alt_key_list;
	for (; l->next; l = l->next);
	l->next = p;
      }
  }
;


/* COLLATING SEQUENCE clause */

collating_sequence_clause:
  _collating SEQUENCE _is WORD
  {
    PENDING ("COLLATING SEQUENCE");
  }
;


/* FILE STATUS clause */

file_status_clause:
  _file STATUS _is reference opt_reference
  {
    current_file->file_status = $4;
    if ($5)
      PENDING ("2nd FILE STATUS");
  }
;


/* LOCK MODE clause */

lock_mode_clause:
  LOCK _mode _is lock_mode	{ PENDING ("LOCK MODE"); }
;

lock_mode:
  MANUAL lock_with
| AUTOMATIC lock_with
| EXCLUSIVE
;

lock_with:
| WITH LOCK ON _multiple records
| WITH ROLLBACK
;


/* ORGANIZATION clause */

organization_clause:
  ORGANIZATION _is organization
| organization
;

organization:
  INDEXED		{ current_file->organization = COB_ORG_INDEXED; }
| SEQUENTIAL		{ current_file->organization = COB_ORG_SEQUENTIAL; }
| RELATIVE		{ current_file->organization = COB_ORG_RELATIVE; }
| LINE SEQUENTIAL	{ current_file->organization = COB_ORG_LINE_SEQUENTIAL; }
;


/* PADDING CHARACTER clause */

padding_character_clause:
  PADDING _character _is reference_or_literal
  {
    cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
;


/* RECORD DELIMITER clause */

record_delimiter_clause:
  RECORD DELIMITER _is STANDARD_1	{ /* ignored */ }
;


/* RECORD KEY clause */

record_key_clause:
  RECORD _key _is reference opt_splitk	{ current_file->key = $4; }
;

opt_splitk:
| '=' reference_list			{ PENDING ("SPLIT KEYS"); }
| SOURCE _is reference_list		{ PENDING ("SPLIT KEYS"); }
;

/* RELATIVE KEY clause */

relative_key_clause:
  RELATIVE _key _is reference	{ current_file->key = $4; }
;


/* RESERVE clause */

reserve_clause:
  RESERVE integer _area		{ /* ignored */ }
;


/* SHARING clause */

sharing_clause:
  SHARING _with sharing_option	{ current_file->sharing = $3; }
;

sharing_option:
  ALL _other			{ $$ = NULL; PENDING ("SHARING ALL OTHER"); }
| NO _other			{ $$ = cb_int1; }
| READ ONLY			{ $$ = cb_int0; }
;


/*
 * I-O-CONTROL paragraph
 */

i_o_control_paragraph:
| I_O_CONTROL '.' opt_i_o_control
;

opt_i_o_control:
| i_o_control_list '.'
;

i_o_control_list:
  i_o_control_clause
| i_o_control_list i_o_control_clause
;

i_o_control_clause:
  same_clause
| multiple_file_tape_clause
;

/* SAME clause */

same_clause:
  SAME same_option _area _for file_name_list
  {
    cb_tree l;
    switch (CB_INTEGER ($2)->val)
      {
      case 0:
	/* SAME AREA */
	break;
      case 1:
	/* SAME RECORD */
	for (l = $5; l; l = CB_CHAIN (l))
	{
	if (CB_VALUE (l) != cb_error_node)
		CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = samearea;
	}
	samearea++;
	break;
      case 2:
	/* SAME SORT-MERGE */
	break;
      }
  }
;

same_option:
  /* empty */			{ $$ = cb_int0; }
| RECORD			{ $$ = cb_int1; }
| SORT				{ $$ = cb_int2; }
| SORT_MERGE			{ $$ = cb_int2; }
;

/* MULTIPLE FILE TAPE clause */

multiple_file_tape_clause:
  MULTIPLE _file _tape
  {
    cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
  }
  _contains multiple_file_list
;

multiple_file_list:
  multiple_file
| multiple_file_list multiple_file
;

multiple_file:
  file_name multiple_file_position { }
;

multiple_file_position:
| POSITION integer
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION '.'
  file_section
  working_storage_section
  local_storage_section
  linkage_section
  screen_section
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| TOK_FILE SECTION '.'		{ current_storage = CB_STORAGE_FILE; }
  file_description_sequence
| file_type
  {
    /* hack for MF compatibility */
    if (cb_relaxed_syntax_check) {
       cb_warning ("FILE SECTION header missing - assumed");
    } else {
       cb_error ("FILE SECTION header missing");
    }
    current_storage = CB_STORAGE_FILE;
  }
  file_description_sequence_without_type
;

file_description_sequence:
| file_description_sequence file_description
;

file_description:
  file_type file_description_entry
  record_description_list
  {
    finalize_file (current_file, CB_FIELD ($3));
  }
;

file_description_sequence_without_type:
  file_description_entry
  record_description_list
  {
    finalize_file (current_file, CB_FIELD ($2));
  }
| file_description_sequence_without_type file_description
;

file_type:
  FD                           { $$ = cb_int0; }
| SD                           { $$ = cb_int1; }
;


/*
 * File description entry
 */

file_description_entry:
  file_name
  {
    if ($1 == cb_error_node)
      YYERROR;

    current_file = CB_FILE (cb_ref ($1));
    if ($0 == cb_int1)
      current_file->organization = COB_ORG_SORT;
  }
  file_description_clause_sequence '.'
;

file_description_clause_sequence:
| file_description_clause_sequence file_description_clause
;

file_description_clause:
  _is EXTERNAL			{ current_file->external = 1; }
| _is GLOBAL			{ PENDING ("GLOBAL"); }
| block_contains_clause
| record_clause
| label_records_clause
| value_of_clause
| data_records_clause
| linage_clause
| recording_mode_clause
| code_set_clause
;


/* BLOCK CONTAINS clause */

block_contains_clause:
  BLOCK _contains integer opt_to_integer _records_or_characters
  { /* ignored */ }
;
_records_or_characters: | RECORDS | CHARACTERS ;


/* RECORD clause */

record_clause:
  RECORD _contains integer _characters
  {
    current_file->record_max = cb_get_int ($3);
  }
| RECORD _contains integer TO integer _characters
  {
    current_file->record_min = cb_get_int ($3);
    current_file->record_max = cb_get_int ($5);
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  record_depending
  {
    current_file->record_min = $6 ? cb_get_int ($6) : 0;
    current_file->record_max = $7 ? cb_get_int ($7) : 0;
  }
;

record_depending:
| DEPENDING _on reference
  {
    current_file->record_depending = $3;
  }
;

opt_from_integer:
  /* empty */			{ $$ = NULL; }
| _from integer			{ $$ = $2; }
;

opt_to_integer:
  /* empty */			{ $$ = NULL; }
| TO integer			{ $$ = $2; }
;


/* LABEL RECORDS clause */

label_records_clause:
  LABEL records label_option
  {
    cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
;

label_option:
  STANDARD
| OMITTED
;


/* VALUE OF clause */

value_of_clause:
  WORD _is WORD
  {
    cb_verify (cb_value_of_clause, "VALUE OF");
  }
;


/* DATA RECORDS clause */

data_records_clause:
  DATA records reference_list
  {
    cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
;


/* LINAGE clause */

linage_clause:
  LINAGE _is reference_or_literal _lines
  linage_sequence
  {
    if (current_file->organization != COB_ORG_LINE_SEQUENTIAL
	&& current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error ("LINAGE clause with wrong file type");
		$$ = cb_error_node;
    } else {
	current_file->linage = $3;
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if (current_linage == 0) {
		linage_file = current_file;
	}
	current_linage++;
    }
  }
;

linage_sequence:
| linage_sequence linage_lines
;

linage_lines:
  linage_footing
| linage_top
| linage_bottom
;

linage_footing:
  _with FOOTING _at reference_or_literal _lines
  {
    current_file->latfoot = $4;
  }
;

linage_top:
  _at TOP reference_or_literal _lines
  {
    current_file->lattop = $3;
  }
;

linage_bottom:
  _at BOTTOM reference_or_literal
  {
    current_file->latbot = $3;
  }
;


/* RECORDING MODE clause */

recording_mode_clause:
  RECORDING _mode _is WORD	{ /* ignore */ }
;


/* CODE-SET clause */

code_set_clause:
  CODE_SET _is WORD
  {
    if ($3 != cb_error_node)
      {
	cb_tree x = cb_ref ($3);
	if (!CB_ALPHABET_NAME_P (x))
	  cb_error_x ($3, _("Alphabet-name is expected '%s'"), cb_name ($3));
	else if (CB_ALPHABET_NAME (x)->custom_list)
	  PENDING ("CODE-SET");
      }
  }
;


/*******************
 * WORKING-STORAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION '.'	{ current_storage = CB_STORAGE_WORKING; }
  record_description_list
  {
	if ($5) {
		current_program->working_storage =
			cb_field_add (current_program->working_storage, CB_FIELD ($5));
	}
  }
;

record_description_list:
  /* empty */			{ $$ = NULL; }
| record_description_list_1	{ $$ = $1; }
;

record_description_list_1:
  {
    current_field = NULL;
  }
  record_description_list_2
  {
    struct cb_field *p;
    for (p = CB_FIELD ($2); p; p = p->sister)
      cb_validate_field (p);
    $$ = $2;
  }
;

record_description_list_2:
  data_description '.'		{ $$ = $1; }
| record_description_list_2
  data_description '.'		{ $$ = $1; }
| record_description_list_2 '.' { $$ = $1; }
;

data_description:
  level_number entry_name
  {
    cb_tree x = cb_build_field_tree ($1, $2, current_field, current_storage, current_file);
    if (x == cb_error_node)
      YYERROR;
    else
      current_field = CB_FIELD (x);
  }
  data_description_clause_sequence
  {
	if (current_field->level == 88) {
		cb_validate_88_item (current_field);
	}
	if (current_field->level == 78) {
		cb_validate_78_item (current_field);
	}
	$$ = CB_TREE (current_field);
  }
;

level_number:
  WORD
;

entry_name:
  /* empty */			{ $$ = cb_build_filler (); }
| FILLER			{ $$ = cb_build_filler (); }
| WORD				{ $$ = $1; }
;

data_description_clause_sequence:
  /* empty */			{ $$ = NULL; }
| data_description_clause_sequence
  data_description_clause	{ $$ = cb_true; }
;

data_description_clause:
  redefines_clause
| external_clause
| global_clause
| picture_clause
| usage_clause
| sign_clause
| occurs_clause
| justified_clause
| synchronized_clause
| blank_clause
| based_clause
| value_clause
| renames_clause
;


/* REDEFINES clause */

redefines_clause:
  REDEFINES identifier_1
  {
    if ($0 != NULL)
      cb_error_x ($2, _("REDEFINES clause must follow entry-name"));

    current_field->redefines = cb_resolve_redefines (current_field, $2);
    if (current_field->redefines == NULL)
      YYERROR;
  }
;


/* EXTERNAL clause */

external_clause:
  _is EXTERNAL as_extname
  {
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error ("EXTERNAL not allowed here");
		$$ = cb_error_node;
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error ("EXTERNAL only allowed at 01/77 level");
		$$ = cb_error_node;
	} else {
		current_field->flag_external = 1;
		has_external = 1;
	}
  }
;

as_extname:
  /* empty */			{ current_field->ename = NULL; }
| AS LITERAL
 {
      struct cb_field *x = CB_FIELD(cb_build_field (cb_build_reference ((char *)(CB_LITERAL ($2)->data))));
      current_field->ename = x->name;
 }
;

/* GLOBAL clause */

global_clause:
  _is GLOBAL			{ PENDING ("GLOBAL"); }
;


/* PICTURE clause */

picture_clause:
  PICTURE			{ current_field->pic = CB_PICTURE ($1); }
;


/* USAGE clause */

usage_clause:
  usage
| USAGE _is usage
;

usage:
  BINARY			{ current_field->usage = CB_USAGE_BINARY; }
| COMP				{ current_field->usage = CB_USAGE_BINARY; }
| COMP_1			{ current_field->usage = CB_USAGE_FLOAT; }
| COMP_2			{ current_field->usage = CB_USAGE_DOUBLE; }
| COMP_3			{ current_field->usage = CB_USAGE_PACKED; }
| COMP_4			{ current_field->usage = CB_USAGE_BINARY; }
| COMP_5			{ current_field->usage = CB_USAGE_COMP_5; }
| COMP_X			{ current_field->usage = CB_USAGE_COMP_X; }
| DISPLAY			{ current_field->usage = CB_USAGE_DISPLAY; }
| INDEX				{ current_field->usage = CB_USAGE_INDEX; }
| PACKED_DECIMAL		{ current_field->usage = CB_USAGE_PACKED; }
| POINTER			{ current_field->usage = CB_USAGE_POINTER; }
| PROGRAM_POINTER		{ current_field->usage = CB_USAGE_PROGRAM_POINTER; }
| SIGNED_SHORT			{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| SIGNED_INT			{ current_field->usage = CB_USAGE_SIGNED_INT; }
| SIGNED_LONG			{ current_field->usage = CB_USAGE_SIGNED_LONG; }
| UNSIGNED_SHORT		{ current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
| UNSIGNED_INT			{ current_field->usage = CB_USAGE_UNSIGNED_INT; }
| UNSIGNED_LONG			{ current_field->usage = CB_USAGE_UNSIGNED_LONG; }
| BINARY_CHAR SIGNED		{ current_field->usage = CB_USAGE_SIGNED_CHAR; }
| BINARY_CHAR UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_CHAR; }
| BINARY_CHAR			{ current_field->usage = CB_USAGE_SIGNED_CHAR; }
| BINARY_SHORT SIGNED		{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| BINARY_SHORT UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_SHORT; }
| BINARY_SHORT			{ current_field->usage = CB_USAGE_SIGNED_SHORT; }
| BINARY_LONG SIGNED		{ current_field->usage = CB_USAGE_SIGNED_INT; }
| BINARY_LONG UNSIGNED		{ current_field->usage = CB_USAGE_UNSIGNED_INT; }
| BINARY_LONG			{ current_field->usage = CB_USAGE_SIGNED_INT; }
| BINARY_DOUBLE SIGNED		{ current_field->usage = CB_USAGE_SIGNED_LONG; }
| BINARY_DOUBLE UNSIGNED	{ current_field->usage = CB_USAGE_UNSIGNED_LONG; }
| BINARY_DOUBLE			{ current_field->usage = CB_USAGE_SIGNED_LONG; }
;


/* SIGN clause */

sign_clause:
  _sign_is LEADING flag_separate
  {
    current_field->flag_sign_separate = CB_INTEGER ($3)->val;
    current_field->flag_sign_leading  = 1;
  }
| _sign_is TRAILING flag_separate
  {
    current_field->flag_sign_separate = CB_INTEGER ($3)->val;
    current_field->flag_sign_leading  = 0;
  }
;


/* OCCURS clause */

occurs_clause:
  OCCURS integer occurs_to_integer _times
  occurs_depending occurs_keys occurs_indexed
  {
    current_field->occurs_min = $3 ? cb_get_int ($2) : 1;
    current_field->occurs_max = $3 ? cb_get_int ($3) : cb_get_int ($2);
    current_field->indexes++;
    current_field->flag_occurs = 1;
  }
;

occurs_to_integer:
  /* empty */			{ $$ = NULL; }
| TO integer			{ $$ = $2; }
;

occurs_depending:
| DEPENDING _on reference
  {
    current_field->occurs_depending = $3;
  }
;

occurs_keys:
  occurs_key_list
  {
    if ($1)
      {
	int i, nkeys = cb_list_length ($1);
	struct cb_key *keys = cobc_malloc (sizeof (struct cb_key) * nkeys);
	cb_tree l = $1;
	for (i = 0; i < nkeys; i++)
	  {
	    keys[i].dir = CB_PURPOSE_INT (l);
	    keys[i].key = CB_VALUE (l);
	    l = CB_CHAIN (l);
	  }
	current_field->keys = keys;
	current_field->nkeys = nkeys;
      }
  }
;

occurs_key_list:
  /* empty */			{ $$ = NULL; }
| occurs_key_list
  ascending_or_descending _key _is reference_list
  {
    cb_tree l;
    for (l = $5; l; l = CB_CHAIN (l))
      CB_PURPOSE (l) = $2;
    $$ = cb_list_append ($1, $5);
  }
;

ascending_or_descending:
  ASCENDING			{ $$ = cb_int (COB_ASCENDING); }
| DESCENDING			{ $$ = cb_int (COB_DESCENDING); }
;

occurs_indexed:
| INDEXED _by occurs_index_list	{ current_field->index_list = $3; }
;

occurs_index_list:
  occurs_index			{ $$ = cb_list ($1); }
| occurs_index_list
  occurs_index			{ $$ = cb_list_add ($1, $2); }
;

occurs_index:
  WORD
	{
		$$ = cb_build_index ($1);
		if ($$ != cb_error_node) {
			CB_FIELD (cb_ref ($$))->values = cb_list (cb_int1);
		}
	}
;


/* JUSTIFIED clause */

justified_clause:
  JUSTIFIED _right		{ current_field->flag_justified = 1; }
;


/* SYNCHRONIZED clause */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ current_field->flag_synchronized = 1; }
;

left_or_right:
| LEFT
| RIGHT
;


/* BLANK clause */

blank_clause:
  BLANK _when ZERO		{ current_field->flag_blank_zero = 1; }
;


/* BASED clause */

based_clause:
  BASED					{ current_field->flag_item_based = 1; }
;

/* VALUE clause */

value_clause:
  VALUE _is_are value_item_list	{ current_field->values = $3; }
  _when _set _to false_is
;

value_item_list:
  value_item			{ $$ = cb_list ($1); }
| value_item_list value_item	{ $$ = cb_list_add ($1, $2); }
;

value_item:
  literal			{ $$ = $1; }
| literal THRU literal		{ $$ = cb_build_pair ($1, $3); }
;

false_is:
| TOK_FALSE _is literal
	{
		if (current_field->level != 88) {
			cb_error ("FALSE clause only allowed for 88 level");
		}
		current_field->false_88 = cb_list($3);
	}
;


/* RENAMES clause */

renames_clause:
  RENAMES qualified_word
  {
	if (cb_ref ($2) != cb_error_node) {
		if (CB_FIELD (cb_ref ($2))->level == 01 ||
		    CB_FIELD (cb_ref ($2))->level > 50) {
			cb_error ("RENAMES may not reference a level 01 or > 50");
		} else {
			current_field->redefines = CB_FIELD (cb_ref ($2));
			current_field->pic = current_field->redefines->pic;
		}
	}
  }
| RENAMES qualified_word THRU qualified_word
  {
	if (cb_ref ($2) != cb_error_node && cb_ref ($4) != cb_error_node) {
		if (CB_FIELD (cb_ref ($2))->level == 01 ||
		    CB_FIELD (cb_ref ($2))->level > 50) {
			cb_error ("RENAMES may not reference a level 01 or > 50");
		} else if (CB_FIELD (cb_ref ($4))->level == 01 ||
		    CB_FIELD (cb_ref ($4))->level > 50) {
			cb_error ("RENAMES may not reference a level 01 or > 50");
		} else {
			current_field->redefines = CB_FIELD (cb_ref ($2));
			current_field->rename_thru = CB_FIELD (cb_ref ($4));
		}
	}
  }
;


/*******************
 * LOCAL-STORAGE SECTION
 *******************/

local_storage_section:
| LOCAL_STORAGE SECTION '.'	{ current_storage = CB_STORAGE_LOCAL; }
  record_description_list
  {
    if ($5)
      current_program->local_storage = CB_FIELD ($5);
  }
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION '.'		{ current_storage = CB_STORAGE_LINKAGE; }
  record_description_list
  {
    if ($5)
      current_program->linkage_storage = CB_FIELD ($5);
  }
;


/*******************
 * SCREEN SECTION
 *******************/

screen_section:
| SCREEN SECTION '.'		{ current_storage = CB_STORAGE_SCREEN; }
  {
	current_field = NULL;
  }
  opt_screen_description_list
  {
	if ($6) {
		struct cb_field *p;
		for (p = CB_FIELD ($6); p; p = p->sister)
			cb_validate_field (p);
		current_program->screen_storage = CB_FIELD ($6);
		current_program->flag_screen = 1;
	}
  }
;

opt_screen_description_list:
  /* empty */			{ $$ = NULL; }
| screen_description_list	{ $$ = $1; }
;

screen_description_list:
  screen_description		{ $$ = $1; }
| screen_description_list
  screen_description		{ $$ = $1; }
;

screen_description:
  level_number entry_name
  {
    cb_tree x = cb_build_field_tree ($1, $2, current_field, current_storage, current_file);
    if (x == cb_error_node)
      YYERROR;

    current_field = CB_FIELD (x);
    current_field->screen_flag |= COB_SCREEN_FG_NONE;
    current_field->screen_flag |= COB_SCREEN_BG_NONE;
    if (current_field->parent)
      current_field->screen_flag |= current_field->parent->screen_flag;
  }
  screen_options '.'
  {
    if (!current_field->screen_line)
      {
	current_field->screen_line = cb_zero;
	current_field->screen_flag |= COB_SCREEN_LINE_CONST;
      }
    if (!current_field->screen_column)
      {
	current_field->screen_column = cb_zero;
	current_field->screen_flag |= COB_SCREEN_COLUMN_CONST;
      }
    $$ = CB_TREE (current_field);
  }
;

screen_options:
| screen_options screen_option
;

screen_option:
  BLANK_LINE	{ current_field->screen_flag |= COB_SCREEN_BLANK_LINE; }
| BLANK_SCREEN	{ current_field->screen_flag |= COB_SCREEN_BLANK_SCREEN; }
| BELL		{ current_field->screen_flag |= COB_SCREEN_BELL; }
| BLINK		{ current_field->screen_flag |= COB_SCREEN_BLINK; }
| ERASE EOL	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOL; }
| ERASE EOS	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOS; }
| HIGHLIGHT	{ current_field->screen_flag |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ current_field->screen_flag |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ current_field->screen_flag |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ current_field->screen_flag |= COB_SCREEN_UNDERLINE; }
| OVERLINE	{ current_field->screen_flag |= COB_SCREEN_OVERLINE; }
| AUTO		{ current_field->screen_flag |= COB_SCREEN_AUTO; }
| SECURE	{ current_field->screen_flag |= COB_SCREEN_SECURE; }
| REQUIRED	{ current_field->screen_flag |= COB_SCREEN_REQUIRED; }
| FULL		{ current_field->screen_flag |= COB_SCREEN_FULL; }
| PROMPT CHARACTER _is literal
  {
	/* Nothing yet */
  }
| LINE _number _is screen_plus_minus x
  {
    current_field->screen_line = $5;
    if (CB_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| COLUMN _number _is screen_plus_minus x
  {
    current_field->screen_column = $5;
    if (CB_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| FOREGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_FG_MASK;
    switch (cb_get_int ($3))
      {
      case 0: current_field->screen_flag |= COB_SCREEN_FG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_FG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_FG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_FG_CYAN; break;
      case 4: current_field->screen_flag |= COB_SCREEN_FG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_FG_MAGENTA; break;
      case 6: current_field->screen_flag |= COB_SCREEN_FG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_FG_WHITE; break;
      default:
	cb_error (_("Invalid color '%d'"), cb_get_int ($3));
      }
  }
| BACKGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_BG_MASK;
    switch (cb_get_int ($3))
      {
      case 0: current_field->screen_flag |= COB_SCREEN_BG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_BG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_BG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_BG_CYAN; break;
      case 4: current_field->screen_flag |= COB_SCREEN_BG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_BG_MAGENTA; break;
      case 6: current_field->screen_flag |= COB_SCREEN_BG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_BG_WHITE; break;
      default:
	cb_error (_("Invalid color '%d'"), cb_get_int ($3));
      }
  }
| usage_clause
| blank_clause
| justified_clause
| sign_clause
| value_clause
| picture_clause
| USING identifier
  {
    current_field->screen_from = $2;
    current_field->screen_to = $2;
  }
| FROM id_or_lit
  {
    current_field->screen_from = $2;
  }
| TO identifier
  {
    current_field->screen_to = $2;
  }
;

screen_plus_minus:
  /* empty */
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_ABS;
  }
| PLUS
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_PLUS;
  }
| MINUS
  {
    current_field->screen_flag &= ~COB_SCREEN_COLUMN_MASK;
    current_field->screen_flag |= ~COB_SCREEN_COLUMN_MINUS;
  }
;


/*****************************************************************************
 * PROCEDURE DIVISION
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION using_chaining '.'
  {
    current_section = NULL;
    current_paragraph = NULL;

    if (cb_device_predefine)
      {
	cb_define_system_name ("CONSOLE");
	cb_define_system_name ("SYSIN");
	cb_define_system_name ("SYSOUT");
	cb_define_system_name ("SYSERR");
      }
  }
  procedure_declaratives
  {
    if (current_program->gen_main && !current_program->is_chained && $3) {
	cb_error ("Executable program requested but PROCEDURE/ENTRY has USING clause");
    }
    emit_entry (current_program->program_id, 0, $3); /* main entry point */
    if (current_program->source_name) {
	emit_entry (current_program->source_name, 1, $3);
    }
  }
  procedure_list
  {
    if (current_paragraph)
      emit_statement (cb_build_perform_exit (current_paragraph));
    if (current_section)
      emit_statement (cb_build_perform_exit (current_section));
  }
;

procedure_declaratives:
| DECLARATIVES '.'
  procedure_list
  END DECLARATIVES '.'
;


/*******************
 * Procedure list
 *******************/

procedure_list:
| procedure_list procedure
;

procedure:
  section_header
| paragraph_header
| invalid_statement
| statements '.'
  {
    if (next_label_list)
      {
	char name[16];
	cb_tree label;
	sprintf (name, "L$%d", next_label_id);
	label = cb_build_reference (name);
	emit_statement (cb_build_label (label, 0));
	current_program->label_list =
	  cb_list_append (current_program->label_list, next_label_list);
	next_label_list = NULL;
	next_label_id++;
      }
  }
| error '.'
| '.'
;


/*******************
 * Section/Paragraph
 *******************/

section_header:
  section_name SECTION opt_segment '.'
  {
    if ($1 == cb_error_node)
      YYERROR;

    /* Exit the last section */
    if (current_paragraph)
      emit_statement (cb_build_perform_exit (current_paragraph));
    if (current_section)
      emit_statement (cb_build_perform_exit (current_section));

    /* Begin a new section */
    current_section = CB_LABEL (cb_build_label ($1, NULL));
    current_section->is_section = 1;
    current_paragraph = NULL;
    emit_statement (CB_TREE (current_section));
  }
;

paragraph_header:
  WORD '.'
  {
    $$ = cb_build_section_name ($1, 1);
    /* if ($1 == cb_error_node) */
    if ($$ == cb_error_node)
      YYERROR;

    /* Exit the last paragraph */
    if (current_paragraph)
      emit_statement (cb_build_perform_exit (current_paragraph));

    /* Begin a new paragraph */
    current_paragraph = CB_LABEL (cb_build_label ($$, current_section));
    if (current_section)
      current_section->children =
	cb_cons (CB_TREE (current_paragraph), current_section->children);
    emit_statement (CB_TREE (current_paragraph));
  }
;

invalid_statement:
  section_name
  {
    if ($1 != cb_error_node)
      cb_error_x ($1, _("Unknown statement '%s'"), CB_NAME ($1));
    YYERROR;
  }
;

section_name:
  WORD				{ $$ = cb_build_section_name ($1, 0); }
;

opt_segment:
| x				{ /* ignore */ }
;


/*******************
 * Statements
 *******************/

statement_list:
  {
    $$ = current_program->exec_list;
    current_program->exec_list = NULL;
  }
  {
    $$ = CB_TREE (current_statement);
    current_statement->need_terminator = 1;
    current_statement = NULL;
  }
  statements
  {
    $$ = cb_list_reverse (current_program->exec_list);
    current_program->exec_list = $1;
    current_statement = CB_STATEMENT ($2);
  }
;

statements:
  statement
| statements statement
;

statement:
  accept_statement
| add_statement
| allocate_statement
| alter_statement
| call_statement
| cancel_statement
| close_statement
| compute_statement
| continue_statement
| delete_statement
| display_statement
| divide_statement
| entry_statement
| evaluate_statement
| exit_statement
| free_statement
| goto_statement
| goback_statement
| if_statement
| initialize_statement
| inspect_statement
| merge_statement
| move_statement
| multiply_statement
| open_statement
| perform_statement
| read_statement
| release_statement
| return_statement
| rewrite_statement
| search_statement
| set_statement
| sort_statement
| start_statement
| stop_statement
| string_statement
| subtract_statement
| unlock_statement
| unstring_statement
| use_statement
| write_statement
| NEXT SENTENCE
  {
    if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE"))
      {
	cb_tree label;
	char	name[16];

	BEGIN_STATEMENT ("NEXT SENTENCE");
	sprintf (name, "L$%d", next_label_id);
	label = cb_build_reference (name);
	next_label_list = cb_list_add (next_label_list, label);
	emit_statement (cb_build_goto (label, 0));
      }
  }
;


/*
 * ACCEPT statement
 */

accept_statement:
  ACCEPT			{ BEGIN_STATEMENT ("ACCEPT"); }
  accept_body
  end_accept
;

accept_body:
  identifier opt_at_line_column opt_accp_attr		{ cb_emit_accept ($1, $2); }
| identifier FROM ESCAPE KEY
	{
		PENDING ("ACCEPT .. FROM ESCAPE KEY");
	}
| identifier FROM DATE					{ cb_emit_accept_date ($1); }
| identifier FROM DATE YYYYMMDD				{ cb_emit_accept_date_yyyymmdd ($1); }
| identifier FROM DAY					{ cb_emit_accept_day ($1); }
| identifier FROM DAY YYYYDDD				{ cb_emit_accept_day_yyyyddd ($1); }
| identifier FROM DAY_OF_WEEK				{ cb_emit_accept_day_of_week ($1); }
| identifier FROM TIME					{ cb_emit_accept_time ($1); }
| identifier FROM COMMAND_LINE				{ cb_emit_accept_command_line ($1); }
| identifier FROM ENVIRONMENT_VALUE on_accp_exception	{ cb_emit_accept_environment ($1); }
| identifier FROM ENVIRONMENT simple_value on_accp_exception
	{ 
		cb_emit_display (cb_list ($4), cb_true, NULL, NULL);
		cb_emit_accept_environment ($1);
	}
| identifier FROM ARGUMENT_NUMBER			{ cb_emit_accept_arg_number ($1); }
| identifier FROM ARGUMENT_VALUE on_accp_exception	{ cb_emit_accept_arg_value ($1); }
| identifier FROM mnemonic_name				{ cb_emit_accept_mnemonic ($1, $3); }
| identifier FROM WORD					{ cb_emit_accept_name ($1, $3); }
;

opt_at_line_column:
  /* empty */			{ $$ = NULL; }
| _at line_number column_number { $$ = cb_build_pair ($2, $3); }
| _at column_number line_number { $$ = cb_build_pair ($3, $2); }
| AT simple_value		{ $$ = $2; }
;

line_number:
  LINE _number id_or_lit	{ $$ = $3; }
;

column_number:
  COLUMN _number id_or_lit	{ $$ = $3; }
;

opt_accp_attr:
| WITH accp_attrs
;

accp_attrs:
  accp_attr
| accp_attrs accp_attr
;

accp_attr:
  AUTO
| BELL
| BLINK
| HIGHLIGHT
| LOWLIGHT
| REVERSE_VIDEO
| SECURE
| UNDERLINE
| UPDATE
| FOREGROUND_COLOR _is integer
| BACKGROUND_COLOR _is integer
;

end_accept:
| END_ACCEPT
;


/*
 * ADD statement
 */

add_statement:
  ADD				{ BEGIN_STATEMENT ("ADD"); }
  add_body
  end_add
;

add_body:
  x_list TO arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($3, '+', cb_build_binary_list ($1, '+'));
  }
| x_list add_to GIVING arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($4, 0, cb_build_binary_list ($1, '+'));
  }
| CORRESPONDING identifier TO identifier flag_rounded on_size_error
  {
    cb_emit_corresponding (cb_build_add, $4, $2, $5);
  }
;

add_to:
| TO x				{ cb_list_add ($0, $2); }
;

end_add:
  /* empty */			{ terminator_warning (); }
| END_ADD
;


/*
 * ALLOCATE statement
 */

allocate_statement:
  ALLOCATE			{ BEGIN_STATEMENT ("ALLOCATE"); }
  allocate_body
;

allocate_body:
  allocate_expr_or_x CHARACTERS flag_initialized RETURNING target_x
  {
    cb_emit_allocate (NULL, $5, $1, $3);
  }
| target_x flag_initialized allocate_returning
  {
    cb_emit_allocate ($1, $3, NULL, $2);
  }
;

allocate_expr_or_x:
  expr					{ $$ = $1; }
| x						{ $$ = $1; }

allocate_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING target_x	{ $$ = $2; }
;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options
  {
    cb_verify (cb_alter_statement, "ALTER");
  }
;

alter_options:
| alter_options
  procedure_name TO _proceed_to procedure_name
;

_proceed_to: | PROCEED TO ;


/*
 * CALL statement
 */

call_statement:
  CALL	 			{ BEGIN_STATEMENT ("CALL"); }
  id_or_lit call_using call_returning
  call_on_exception call_not_on_exception
  end_call
  {
    cb_emit_call ($3, $4, $5, $6, $7);
  }
;

using_chaining:
  /* empty */			{ $$ = NULL; }
| USING				{ call_mode = cb_int (CB_CALL_BY_REFERENCE); }
  call_param_list		{ $$ = $3; }
| CHAINING
	{
		call_mode = cb_int (CB_CALL_BY_REFERENCE);
		current_program->is_chained = 1;
	}
  call_param_list		{ $$ = $3; }
;

call_using:
  /* empty */			{ $$ = NULL; }
| USING				{ call_mode = cb_int (CB_CALL_BY_REFERENCE); }
  call_param_list		{ $$ = $3; }
;

call_param_list:
  call_param			{ $$ = $1; }
| call_param_list
  call_param			{ $$ = cb_list_append ($1, $2); }
;

call_param:
  x				{ $$ = cb_build_pair (call_mode, $1); }
| _by call_mode x		{ $$ = cb_build_pair (call_mode, $3); }
;

call_mode:
  REFERENCE
	{
		call_mode = cb_int (CB_CALL_BY_REFERENCE);
	}
| CONTENT
	{
		if (current_program->is_chained) {
			cb_error ("BY CONTENT not allowed in CHAINED program");
		} else {
			call_mode = cb_int (CB_CALL_BY_CONTENT);
		}
	}
| VALUE
	{
		if (current_program->is_chained) {
			cb_error ("BY VALUE not allowed in CHAINED program");
		} else {
			call_mode = cb_int (CB_CALL_BY_VALUE);
		}
	}
;

call_returning:
  /* empty */			{ $$ = NULL; }
| RETURNING x			{ $$ = $2; }
| GIVING x			{ $$ = $2; }
;

call_on_exception:
  /* empty */			{ $$ = NULL; }
| OVERFLOW statement_list	{ $$ = $2; }
| EXCEPTION statement_list	{ $$ = $2; }
;

call_not_on_exception:
  /* empty */			{ $$ = NULL; }
| NOT_EXCEPTION
  statement_list		{ $$ = $2; }
;

end_call:
  /* empty */			{ terminator_warning (); }
| END_CALL
;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL			{ BEGIN_STATEMENT ("CANCEL"); }
  cancel_list
;

cancel_list:
| cancel_list x
  {
    cb_emit_cancel ($2);
  }
;


/*
 * CLOSE statement
 */

close_statement:
  CLOSE				{ BEGIN_STATEMENT ("CLOSE"); }
  close_list
;

close_list:
| close_list
  file_name close_option
  {
	BEGIN_IMPLICIT_STATEMENT ();
	cb_emit_close ($2, $3);
  }
;

close_option:
  /* empty */			{ $$ = cb_int (COB_CLOSE_NORMAL); }
| reel_or_unit			{ $$ = cb_int (COB_CLOSE_UNIT); }
| reel_or_unit _for REMOVAL	{ $$ = cb_int (COB_CLOSE_UNIT_REMOVAL); }
| _with NO REWIND		{ $$ = cb_int (COB_CLOSE_NO_REWIND); }
| _with LOCK			{ $$ = cb_int (COB_CLOSE_LOCK); }
;

reel_or_unit: REEL | UNIT ;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE			{ BEGIN_STATEMENT ("COMPUTE"); }
  compute_body
  end_compute
;

compute_body:
  arithmetic_x_list comp_equal expr on_size_error
  {
    cb_emit_arithmetic ($1, 0, $3);
  }
;

end_compute:
  /* empty */			{ terminator_warning (); }
| END_COMPUTE
;

comp_equal: '=' | EQUAL;

/*
 * CONTINUE statement
 */

continue_statement:
  CONTINUE
	{
		BEGIN_STATEMENT ("CONTINUE");
		cb_emit_continue ();
	}
;


/*
 * DELETE statement
 */

delete_statement:
  DELETE			{ BEGIN_STATEMENT ("DELETE"); }
  file_name _record opt_invalid_key
  end_delete
  {
    cb_emit_delete ($3);
  }
;

end_delete:
  /* empty */			{ terminator_warning (); }
| END_DELETE
;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY			{ BEGIN_STATEMENT ("DISPLAY"); }
  x_list opt_at_line_column display_upon with_clause on_disp_exception
  end_display
  {
    cb_emit_display ($3, $5, $6, $4);
  }
;

display_upon:
  /* empty */			{ $$ = cb_int1; }
| UPON mnemonic_name		{ $$ = cb_build_display_upon ($2); }
| UPON WORD			{ $$ = cb_build_display_upon_direct ($2); }
| UPON CRT			{ $$ = cb_int1; }
| UPON ENVIRONMENT_NAME		{ $$ = cb_true; }
| UPON ENVIRONMENT_VALUE	{ $$ = cb_int3; }
| UPON ARGUMENT_NUMBER		{ $$ = cb_int4; }
;

with_clause:
  /* empty */			{ $$ = cb_int0; }
| _with NO_ADVANCING		{ $$ = cb_int1; }
| _with disp_attrs		{ $$ = cb_int0; }
;

disp_attrs:
  disp_attr
| disp_attrs disp_attr
;

disp_attr:
  BELL
| BLINK
| ERASE EOL
| ERASE EOS
| HIGHLIGHT
| LOWLIGHT
| REVERSE_VIDEO
| UNDERLINE
| FOREGROUND_COLOR _is integer
| BACKGROUND_COLOR _is integer
| BLANK_LINE
| BLANK_SCREEN
;

end_display:
| END_DISPLAY
;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE			{ BEGIN_STATEMENT ("DIVIDE"); }
  divide_body
  end_divide
;

divide_body:
  x INTO arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($3, '/', $1);
  }
| x INTO x GIVING arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($5, 0, cb_build_binary_op ($3, '/', $1));
  }
| x BY x GIVING arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($5, 0, cb_build_binary_op ($1, '/', $3));
  }
| x INTO x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error
  {
    cb_emit_divide ($3, $1, $5, $7);
  }
| x BY x GIVING arithmetic_x REMAINDER arithmetic_x on_size_error
  {
    cb_emit_divide ($1, $3, $5, $7);
  }
;

end_divide:
  /* empty */			{ terminator_warning (); }
| END_DIVIDE
;


/*
 * ENTRY statement
 */

entry_statement:
  ENTRY				{ BEGIN_STATEMENT ("ENTRY"); }
  literal call_using
  {
    if (cb_verify (cb_entry_statement, "ENTRY"))
      emit_entry ((char *)(CB_LITERAL ($3)->data), 1, $4);
  }
;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE
  {
	BEGIN_STATEMENT ("EVALUATE");
	eval_level++;
	eval_inc = 0;
	eval_inc2 = 0;
  }
  evaluate_subject_list evaluate_case_list
  end_evaluate
  {
    cb_emit_evaluate ($3, $4);
    eval_level--;
  }
;

evaluate_subject_list:
  evaluate_subject		{ $$ = cb_list ($1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $$ = cb_list_add ($1, $3); }
;

evaluate_subject:
  expr
  {
	$$ = $1;
	if (CB_REFERENCE_P($1)) {
		eval_check[eval_level][eval_inc++] = 1;
	} else {
		eval_check[eval_level][eval_inc++] = 0;
	}
  }
| TOK_TRUE			{ $$ = cb_true; eval_check[eval_level][eval_inc++] = 0; }
| TOK_FALSE			{ $$ = cb_false; eval_check[eval_level][eval_inc++] = 0; }
;

evaluate_case_list:
  /* empty */			{ $$ = NULL; }
| evaluate_case_list
  evaluate_case			{ $$ = cb_list_add ($1, $2); }
;

evaluate_case:
  evaluate_when_list
  statement_list		{ $$ = cb_cons ($2, $1); eval_inc2 = 0; }
| WHEN OTHER
  statement_list		{ $$ = cb_cons ($3, NULL); eval_inc2 = 0; }
;

evaluate_when_list:
  WHEN evaluate_object_list	{ $$ = cb_list ($2); }
| evaluate_when_list
  WHEN evaluate_object_list	{ $$ = cb_list_add ($1, $3); }
;

evaluate_object_list:
  evaluate_object		{ $$ = cb_list ($1); }
| evaluate_object_list ALSO
  evaluate_object		{ $$ = cb_list_add ($1, $3); }
;

evaluate_object:
  partial_expr opt_evaluate_thru_expr
  {
    cb_tree not = cb_int0;
    cb_tree e1;
    cb_tree e2 = $2;

    /* in case the first token is NOT */
    if (CB_PURPOSE_INT ($1) == '!') {
      not = cb_int1;
      $1 = CB_CHAIN ($1);
    }

    /* build expr now */
    e1 = cb_build_expr ($1);

    if (e2 == NULL) {
      /*
       * WHEN expr
       */
      if (eval_check[eval_level][eval_inc2] && CB_REFERENCE_P(e1) &&
	   CB_FIELD_P(CB_REFERENCE(e1)->value)) {
	if (CB_FIELD(CB_REFERENCE(e1)->value)->level == 88) {
	  cb_error_x (e1, _("88 level invalid here"));
	}
      }
      eval_inc2++;
      if (not == cb_int1
	  && CB_BINARY_OP_P (e1)
	  && (CB_BINARY_OP (e1)->op == '&' || CB_BINARY_OP (e1)->op == '|'))
	{
	  cb_error_x (e1, _("FIXME: failed to compile \"WHEN NOT ... AND/OR ...\""));
	  cb_error_x (e1, _("FIXME: change it into \"WHEN (NOT ... AND/OR ...)\" for now"));
	}
      $$ = cb_build_pair (not, cb_build_pair (e1, 0));
    } else {
      /*
       * WHEN expr THRU expr
       */
      $$ = cb_build_pair (not, cb_build_pair (e1, e2));
      eval_inc2++;
    }
  }
| ANY				{ $$ = cb_any; eval_inc2++; }
| TOK_TRUE			{ $$ = cb_true; eval_inc2++; }
| TOK_FALSE			{ $$ = cb_false; eval_inc2++; }
;
opt_evaluate_thru_expr:
  /* empty */			{ $$ = NULL; }
| THRU expr			{ $$ = $2; }
;

end_evaluate:
  /* empty */			{ terminator_warning (); }
| END_EVALUATE
;


/*
 * EXIT statement
 */

exit_statement:
  EXIT				{ BEGIN_STATEMENT ("EXIT"); }
  exit_body
;

exit_body:
  /* empty */			{ /* nothing */ }
| PROGRAM			{ cb_emit_exit (); }
| PERFORM
  {
    char name[256];
    struct cb_perform *p = CB_PERFORM (CB_VALUE (perform_stack));
    sprintf (name, "PERFORML-EXIT%d", cb_id++);
    p->exit_label = cb_build_reference (name);
    CB_LABEL (cb_build_label (p->exit_label, 0))->need_begin = 1;
    cb_emit_goto (cb_list (p->exit_label), 0);
  }
| PERFORM CYCLE
  {
    char name[256];
    struct cb_perform *p = CB_PERFORM (CB_VALUE (perform_stack));
    sprintf (name, "PERFORML-CYCLE%d", cb_id++);
    p->cycle_label = cb_build_reference (name);
    CB_LABEL (cb_build_label (p->cycle_label, 0))->need_begin = 1;
    cb_emit_goto (cb_list (p->cycle_label), 0);
  }
;


/*
 * FREE statement
 */

free_statement:
  FREE			{ BEGIN_STATEMENT ("FREE"); }
  target_x_list
  {
    cb_emit_free ($3)
  }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to			{ BEGIN_STATEMENT ("GO TO"); }
  procedure_name_list goto_depending
  {
    cb_emit_goto ($4, $5);
  }
;

goto_depending:
  /* empty */			{ $$ = NULL; }
| DEPENDING _on x		{ $$ = $3; }
;


/*
 * GOBACK statement
 */

goback_statement:
  GOBACK			{ BEGIN_STATEMENT ("GOBACK"); }
  {
    cb_emit_exit ();
  }
;


/*
 * IF statement
 */

if_statement:
  IF				{ BEGIN_STATEMENT ("IF"); }
  condition _then statement_list if_else_sentence
  end_if
  {
    cb_emit_if ($3, $5, $6);
  }
| IF error END_IF
;

if_else_sentence:
  /* empty */			{ $$ = NULL; }
| ELSE statement_list		{ $$ = $2; }
;

end_if:
  /* empty */			{ terminator_warning (); }
| END_IF
;


/*
 * INITIALIZE statement
 */

initialize_statement:
  INITIALIZE			{ BEGIN_STATEMENT ("INITIALIZE"); }
  target_x_list initialize_filler initialize_value initialize_replacing initialize_default
  {
    cb_emit_initialize ($3, $4, $5, $6, $7);
  }
;

initialize_filler:
  /* empty */			{ $$ = NULL; }
| _with FILLER			{ $$ = cb_true; }
;

initialize_value:
  /* empty */			{ $$ = NULL; }
| ALL _to VALUE			{ $$ = cb_true; }
| initialize_category _to VALUE	{ $$ = $1; }
;

initialize_replacing:
  /* empty */			{ $$ = NULL; }
| REPLACING
  initialize_replacing_list	{ $$ = $2; }
;

initialize_replacing_list:
  initialize_replacing_item	{ $$ = $1; }
| initialize_replacing_list
  initialize_replacing_item	{ $$ = cb_list_append ($1, $2); }
;

initialize_replacing_item:
  initialize_category _data BY x { $$ = cb_build_pair ($1, $4); }
;

initialize_category:
  ALPHABETIC		{ $$ = cb_int (CB_CATEGORY_ALPHABETIC); }
| ALPHANUMERIC		{ $$ = cb_int (CB_CATEGORY_ALPHANUMERIC); }
| NUMERIC		{ $$ = cb_int (CB_CATEGORY_NUMERIC); }
| ALPHANUMERIC_EDITED	{ $$ = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
| NUMERIC_EDITED	{ $$ = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
| NATIONAL		{ $$ = cb_int (CB_CATEGORY_NATIONAL); }
| NATIONAL_EDITED	{ $$ = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
;

initialize_default:
  /* empty */			{ $$ = NULL; }
| DEFAULT			{ $$ = cb_true; }
;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT			{ BEGIN_STATEMENT ("INSPECT"); }
  identifier inspect_list
;

inspect_list:
| inspect_list inspect_item
;

inspect_item:
  inspect_tallying		{ cb_emit_inspect ($-1, $1, cb_int0); }
| inspect_replacing		{ cb_emit_inspect ($-1, $1, cb_int1); }
| inspect_converting		{ cb_emit_inspect ($-1, $1, cb_int0); }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING			{ cb_init_tarrying (); }
  tallying_list			{ $$ = $3; }
;

tallying_list:
  tallying_item			{ $$ = $1; }
| tallying_list tallying_item	{ $$ = cb_list_append ($1, $2); }
;

tallying_item:
  simple_value FOR		{ $$ = cb_build_tarrying_data ($1); }
| CHARACTERS inspect_region	{ $$ = cb_build_tarrying_characters ($2); }
| ALL				{ $$ = cb_build_tarrying_all (); }
| LEADING			{ $$ = cb_build_tarrying_leading (); }
| TRAILING			{ $$ = cb_build_tarrying_trailing (); }
| simple_value inspect_region	{ $$ = cb_build_tarrying_value ($1, $2); }
;

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_list	{ $$ = $2; }
;

replacing_list:
  replacing_item		{ $$ = $1; }
| replacing_list replacing_item	{ $$ = cb_list_append ($1, $2); }
;

replacing_item:
  CHARACTERS BY x inspect_region	{ $$ = cb_build_replacing_characters ($3, $4); }
| ALL x BY x inspect_region		{ $$ = cb_build_replacing_all ($2, $4, $5); }
| LEADING x BY x inspect_region		{ $$ = cb_build_replacing_leading ($2, $4, $5); }
| FIRST x BY x inspect_region		{ $$ = cb_build_replacing_first ($2, $4, $5); }
| TRAILING x BY x inspect_region	{ $$ = cb_build_replacing_trailing ($2, $4, $5); }
;

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING x TO x inspect_region { $$ = cb_build_converting ($2, $4, $5); }
;

/* INSPECT BEFORE/AFTER */

inspect_region:
  /* empty */			{ $$ = cb_build_inspect_region_start (); }
| inspect_region
  before_or_after _initial x	{ $$ = cb_build_inspect_region ($1, $2, $4); }
;

_initial: | TOK_INITIAL ;


/*
 * MERGE statement
 */

merge_statement:
  MERGE				{ BEGIN_STATEMENT ("MERGE"); }
  sort_body
;


/*
 * MOVE statement
 */

move_statement:
  MOVE				{ BEGIN_STATEMENT ("MOVE"); }
  move_body
;

move_body:
  x TO target_x_list
  {
    cb_emit_move ($1, $3);
  }
| CORRESPONDING x TO target_x_list
  {
    cb_emit_move_corresponding ($2, $4);
  }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY			{ BEGIN_STATEMENT ("MULTIPLY"); }
  multiply_body
  end_multiply
;

multiply_body:
  x BY arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($3, '*', $1);
  }
| x BY x GIVING arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($5, 0, cb_build_binary_op ($1, '*', $3));
  }
;

end_multiply:
  /* empty */			{ terminator_warning (); }
| END_MULTIPLY
;


/*
 * OPEN statement
 */

open_statement:
  OPEN				{ BEGIN_STATEMENT ("OPEN"); }
  open_list
;

open_list:
| open_list
  open_mode open_sharing file_name_list open_option
  {
    cb_tree l;
    for (l = $4; l; l = CB_CHAIN (l))
      {
	if (CB_VALUE (l) != cb_error_node) {
		BEGIN_IMPLICIT_STATEMENT ();
		cb_emit_open (CB_VALUE (l), $2, $3);
	}
      }
  }
;

open_mode:
  INPUT				{ $$ = cb_int (COB_OPEN_INPUT); }
| OUTPUT			{ $$ = cb_int (COB_OPEN_OUTPUT); }
| I_O				{ $$ = cb_int (COB_OPEN_I_O); }
| EXTEND			{ $$ = cb_int (COB_OPEN_EXTEND); }
;

open_sharing:
  /* empty */			{ $$ = NULL; }
| SHARING _with sharing_option	{ $$ = $3; }
;

open_option:
| _with NO REWIND		{ /* ignored */ }
| _with LOCK			{ PENDING ("OPEN ... WITH LOCK"); }
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM			{ BEGIN_STATEMENT ("PERFORM"); }
  perform_body
;

perform_body:
  perform_procedure perform_option
  {
    cb_emit_perform ($2, $1);
  }
| perform_option
  {
    perform_stack = cb_cons ($1, perform_stack);
  }
  statement_list end_perform
  {
    perform_stack = CB_CHAIN (perform_stack);
    cb_emit_perform ($1, $3);
  }
| perform_option END_PERFORM
  {
    cb_emit_perform ($1, NULL);
  }
;

end_perform:
  /* empty */			{ terminator_error (); }
| END_PERFORM
;

perform_procedure:
  procedure_name
  {
    CB_REFERENCE ($1)->length = cb_true; /* return from $1 */
    $$ = cb_build_pair ($1, $1);
  }
| procedure_name THRU procedure_name
  {
    CB_REFERENCE ($3)->length = cb_true; /* return from $3 */
    $$ = cb_build_pair ($1, $3);
  }
;

perform_option:
  /* empty */
  {
    $$ = cb_build_perform_once (NULL);
  }
| x TIMES
  {
    $$ = cb_build_perform_times ($1);
    current_program->loop_counter++;
  }
| perform_test UNTIL condition
  {
    cb_tree varying = cb_list (cb_build_perform_varying (0, 0, 0, $3));
    $$ = cb_build_perform_until ($1, varying);
  }
| perform_test VARYING perform_varying_list
  {
    $$ = cb_build_perform_until ($1, $3);
  }
;

perform_test:
  /* empty */			{ $$ = CB_BEFORE; }
| _with TEST before_or_after	{ $$ = $3; }
;

perform_varying_list:
  perform_varying		{ $$ = cb_list ($1); }
| perform_varying_list AFTER
  perform_varying		{ $$ = cb_list_add ($1, $3); }
;

perform_varying:
  identifier FROM x BY x UNTIL condition
  {
    $$ = cb_build_perform_varying ($1, $3, $5, $7);
  }
;


/*
 * READ statements
 */

read_statement:
  READ				{ BEGIN_STATEMENT ("READ"); }
  file_name flag_next _record read_into with_lock read_key read_handler
  end_read
  {
    cb_emit_read ($3, $4, $6, $8, $7);
  }
;

read_into:
  /* empty */			{ $$ = NULL; }
| INTO x			{ $$ = $2; }
;

with_lock:
  /* empty */			{ $$ = NULL; }
| _with LOCK
	{
		$$ = cb_int1;
		PENDING ("READ ... WITH LOCK");
	}
| _with NO LOCK
	{
		$$ = cb_int2;
		PENDING ("READ ... WITH NO LOCK");
	}
;

read_key:
  /* empty */			{ $$ = NULL; }
| KEY _is x			{ $$ = $3; }
;

read_handler:
| at_end
| invalid_key
;

end_read:
  /* empty */			{ terminator_warning (); }
| END_READ
;


/*
 * RELEASE statement
 */

release_statement:
  RELEASE			{ BEGIN_STATEMENT ("RELEASE"); }
  record_name write_from
  {
    cb_emit_write ($3, $4, cb_int0);
  }
;


/*
 * RETURN statement
 */

return_statement:
  RETURN			{ BEGIN_STATEMENT ("RETURN"); }
  file_name _record read_into at_end
  end_return
  {
    cb_emit_return ($3, $5);
  }
;

end_return:
  /* empty */			{ terminator_warning (); }
| END_RETURN
;


/*
 * REWRITE statement
 */

rewrite_statement:
  REWRITE			{ BEGIN_STATEMENT ("REWRITE"); }
  record_name write_from opt_invalid_key
  end_rewrite
  {
    cb_emit_rewrite ($3, $4);
  }
;

end_rewrite:
  /* empty */			{ terminator_warning (); }
| END_REWRITE
;


/*
 * SEARCH statement
 */

search_statement:
  SEARCH			{ BEGIN_STATEMENT ("SEARCH"); }
  search_body
  end_search
;

search_body:
  table_name search_varying search_at_end search_whens
  {
    cb_emit_search ($1, $2, $3, $4);
  }
| ALL table_name search_at_end WHEN expr statement_list
  {
    cb_emit_search_all ($2, $3, $5, $6);
  }
;

search_varying:
  /* empty */			{ $$ = NULL; }
| VARYING x			{ $$ = $2; }
;

search_at_end:
  /* empty */			{ $$ = NULL; }
| _at END statement_list	{ $$ = $3; }
;

search_whens:
  search_when			{ $$ = $1; }
| search_when search_whens	{ $$ = $1; CB_IF ($1)->stmt2 = $2; }
;

search_when:
  WHEN condition statement_list	{ $$ = cb_build_if ($2, $3, 0); }
;

end_search:
  /* empty */			{ terminator_warning (); }
| END_SEARCH
;


/*
 * SET statement
 */

set_statement:
  SET				{ BEGIN_STATEMENT ("SET"); }
  set_body
;

set_body:
  set_environment
| set_to
| set_up_down
| set_to_on_off_sequence
| set_to_true_false_sequence
;

/* SET ENVIRONMENT ... TO ... */

set_environment:
  ENVIRONMENT simple_value TO simple_value
  {
	cb_emit_setenv ($2, $4);
  }
;

/* SET name ... TO expr */

set_to:
  target_x_list TO x
  {
    cb_emit_set_to ($1, $3);
  }
;

/* SET name ... UP/DOWN BY expr */

set_up_down:
  target_x_list up_or_down BY x
  {
    cb_emit_set_up_down ($1, $2, $4);
  }
;

up_or_down:
  UP				{ $$ = cb_int0; }
| DOWN				{ $$ = cb_int1; }
;

/* SET mnemonic-name-1 ... TO ON/OFF */

set_to_on_off_sequence:
  set_to_on_off
| set_to_on_off_sequence set_to_on_off
;

set_to_on_off:
  mnemonic_name_list TO on_or_off
  {
    cb_emit_set_on_off ($1, $3);
  }
;

/* SET condition-name-1 ... TO TRUE/FALSE */

set_to_true_false_sequence:
  set_to_true_false
| set_to_true_false_sequence set_to_true_false
;

set_to_true_false:
  target_x_list TO TOK_TRUE
  {
    cb_emit_set_true ($1);
  }
| target_x_list TO TOK_FALSE
  {
    cb_emit_set_false ($1);
  }
;


/*
 * SORT statement
 */

sort_statement:
  SORT				{ BEGIN_STATEMENT ("SORT"); }
  sort_body
;

sort_body:
  qualified_word sort_key_list sort_duplicates sort_collating
  {
    cb_emit_sort_init ($1, $2, $3, $4);
    $$ = $1; /* used in sort_input/sort_output */
  }
  sort_input sort_output
  {
    cb_emit_sort_finish ($1);
  }
;

sort_key_list:
  /* empty */			{ $$ = NULL; }
| sort_key_list
  _on ascending_or_descending _key opt_key_list
  {
    cb_tree l;
    if ($5 == NULL)
      $5 = cb_list (NULL);
    for (l = $5; l; l = CB_CHAIN (l))
      CB_PURPOSE (l) = $3;
    $$ = cb_list_append ($1, $5);
  }
;

opt_key_list:
  /* empty */			{ $$ = NULL; }
| opt_key_list qualified_word	{ $$ = cb_list_add ($1, $2); }
;

sort_duplicates:
| _with DUPLICATES _in _order		{ /* nothing */ }
;

sort_collating:
  /* empty */				{ $$ = cb_null; }
| _collating SEQUENCE _is reference	{ $$ = cb_ref ($4); }
;

sort_input:
| USING file_name_list
  {
    cb_emit_sort_using ($0, $2);
  }
| INPUT PROCEDURE _is perform_procedure
  {
    cb_emit_sort_input ($0, $4);
  }
;

sort_output:
| GIVING file_name_list
  {
    cb_emit_sort_giving ($-1, $2);
  }
| OUTPUT PROCEDURE _is perform_procedure
  {
    cb_emit_sort_output ($-1, $4);
  }
;


/*
 * START statement
 */

start_statement:
  START				{ BEGIN_STATEMENT ("START"); }
  file_name			{ $$ = cb_int (COB_EQ); }
  start_key opt_invalid_key
  end_start
  {
	if (CB_FILE_P (cb_ref ($3))) {
		if (CB_FILE (cb_ref ($3))->organization != COB_ORG_INDEXED &&
		     CB_FILE (cb_ref ($3))->organization != COB_ORG_RELATIVE) {
			cb_error ("START not allowed on SEQUENTIAL files");
			$$ = cb_error_node;
		} else {
			cb_emit_start ($3, $4, $5);
		}
	} else {
		cb_error_x ($3, _("'%s' not file name"), CB_NAME ($3));
		$$ = cb_error_node;
	}
  }
;

start_key:
  /* empty */			{ $$ = NULL; }
| KEY _is start_op x		{ $0 = $3; $$ = $4; }
;

start_op:
  flag_not eq		{ $$ = cb_int (($1 == cb_int1) ? COB_NE : COB_EQ); }
| flag_not gt		{ $$ = cb_int (($1 == cb_int1) ? COB_LE : COB_GT); }
| flag_not lt		{ $$ = cb_int (($1 == cb_int1) ? COB_GE : COB_LT); }
| flag_not ge		{ $$ = cb_int (($1 == cb_int1) ? COB_LT : COB_GE); }
| flag_not le		{ $$ = cb_int (($1 == cb_int1) ? COB_GT : COB_LE); }
;

end_start:
  /* empty */			{ terminator_warning (); }
| END_START
;


/*
 * STOP statement
 */

stop_statement:
  STOP RUN		{ BEGIN_STATEMENT ("STOP"); }
  stop_returning
  {
    cb_emit_stop_run ($4);
  }
| STOP literal
  {
    cb_verify (cb_stop_literal_statement, "STOP literal");
  }
;

stop_returning:
  /* empty */		{ $$ = cb_return_code; }
| RETURNING x		{ $$ = $2; }
| GIVING x		{ $$ = $2; }
;


/*
 * STRING statement
 */

string_statement:
  STRING			{ BEGIN_STATEMENT ("STRING"); }
  string_item_list INTO identifier opt_with_pointer on_overflow
  end_string
  {
    cb_emit_string ($3, $5, $6);
  }
;

string_item_list:
  string_item			{ $$ = cb_list ($1); }
| string_item_list string_item	{ $$ = cb_list_add ($1, $2); }
;

string_item:
  x				{ $$ = $1; }
| DELIMITED _by SIZE		{ $$ = cb_build_pair (cb_int0, 0); }
| DELIMITED _by x		{ $$ = cb_build_pair ($3, 0); }
;

opt_with_pointer:
  /* empty */			{ $$ = cb_int0; }
| _with POINTER x		{ $$ = $3; }
;

end_string:
  /* empty */			{ terminator_warning (); }
| END_STRING
;


/*
 * SUBTRACT statement
 */

subtract_statement:
  SUBTRACT			{ BEGIN_STATEMENT ("SUBTRACT"); }
  subtract_body
  end_subtract
;

subtract_body:
  x_list FROM arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($3, '-', cb_build_binary_list ($1, '+'));
  }
| x_list FROM x GIVING arithmetic_x_list on_size_error
  {
    cb_emit_arithmetic ($5, 0, cb_build_binary_list (cb_cons ($3, $1), '-'));
  }
| CORRESPONDING x FROM x flag_rounded on_size_error
  {
    cb_emit_corresponding (cb_build_sub, $4, $2, $5);
  }
;

end_subtract:
  /* empty */			{ terminator_warning (); }
| END_SUBTRACT
;


/*
 * UNLOCK statement
 */

unlock_statement:
  UNLOCK			{ BEGIN_STATEMENT ("UNLOCK"); }
  file_name opt_record		{ PENDING ("UNLOCK"); }
;

opt_record:
  /* empty */
| RECORD
| RECORDS
;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING			{ BEGIN_STATEMENT ("UNSTRING"); }
  identifier unstring_delimited unstring_into
  opt_with_pointer unstring_tallying on_overflow
  end_unstring
  {
    cb_emit_unstring ($3, $4, $5, $6, $7);
  }
;

unstring_delimited:
  /* empty */			{ $$ = NULL; }
| DELIMITED _by
  unstring_delimited_list	{ $$ = $3; }
;

unstring_delimited_list:
  unstring_delimited_item	{ $$ = cb_list ($1); }
| unstring_delimited_list OR
  unstring_delimited_item	{ $$ = cb_list_add ($1, $3); }
;

unstring_delimited_item:
  flag_all simple_value
  {
    $$ = cb_build_unstring_delimited ($1, $2);
  }
;

unstring_into:
  INTO unstring_into_item	{ $$ = cb_list ($2); }
| unstring_into
  unstring_into_item		{ $$ = cb_list_add ($1, $2); }
;

unstring_into_item:
  identifier unstring_into_delimiter unstring_into_count
  {
    $$ = cb_build_unstring_into ($1, $2, $3);
  }
;

unstring_into_delimiter:
  /* empty */			{ $$ = NULL; }
| DELIMITER _in identifier	{ $$ = $3; }
;

unstring_into_count:
  /* empty */			{ $$ = NULL; }
| COUNT _in identifier		{ $$ = $3; }
;

unstring_tallying:
  /* empty */			{ $$ = NULL; }
| TALLYING _in identifier	{ $$ = $3; }
;

end_unstring:
  /* empty */			{ terminator_warning (); }
| END_UNSTRING
;


/*
 * USE statement
 */

use_statement:
  use_exception
| use_debugging
;

use_exception:
  USE flag_global AFTER _standard exception_or_error PROCEDURE
  _on use_exception_target
  {
    current_section->need_begin = 1;
    current_section->need_return = 1;
    CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
  }
;

use_exception_target:
  file_name_list
  {
    cb_tree l;
    for (l = $1; l; l = CB_CHAIN (l))
      if (CB_VALUE (l) != cb_error_node)
	CB_FILE (cb_ref (CB_VALUE (l)))->handler = current_section;
  }
| INPUT	 { current_program->file_handler[COB_OPEN_INPUT]  = current_section; }
| OUTPUT { current_program->file_handler[COB_OPEN_OUTPUT] = current_section; }
| I_O	 { current_program->file_handler[COB_OPEN_I_O]    = current_section; }
| EXTEND { current_program->file_handler[COB_OPEN_EXTEND] = current_section; }
;

_standard: | STANDARD ;
exception_or_error: EXCEPTION | ERROR ;

use_debugging:
  USE _for DEBUGGING _on use_debugging_target
  {
    PENDING ("USE FOR DEBUGGING");
  }
;

use_debugging_target:
  procedure_name
| ALL PROCEDURES
;


/*
 * WRITE statement
 */

write_statement:
  WRITE				{ BEGIN_STATEMENT ("WRITE"); }
  record_name write_from write_option write_handler
  end_write
  {
    cb_emit_write ($3, $4, $5);
  }
;

write_from:
  /* empty */			{ $$ = NULL; }
| FROM x			{ $$ = $2; }
;

write_option:
  /* empty */
  {
    $$ = cb_int0;
  }
| before_or_after _advancing x _line_or_lines
  {
    $$ = cb_build_write_advancing_lines ($1, $3);
  }
| before_or_after _advancing mnemonic_name
  {
    $$ = cb_build_write_advancing_mnemonic ($1, $3);
  }
| before_or_after _advancing PAGE
  {
    $$ = cb_build_write_advancing_page ($1);
  }
;

before_or_after:
  BEFORE			{ $$ = CB_BEFORE; }
| AFTER				{ $$ = CB_AFTER; }
;

write_handler:
| at_eop
| invalid_key
;

end_write:
  /* empty */			{ terminator_warning (); }
| END_WRITE
;


/*******************
 * Status handlers
 *******************/

/*
 * ON EXCEPTION
 */

on_accp_exception:
  opt_on_exception
  opt_not_on_exception
  {
    current_statement->handler_id = COB_EC_IMP_ACCEPT;
  }
;

on_disp_exception:
  opt_on_exception
  opt_not_on_exception
  {
    current_statement->handler_id = COB_EC_IMP_DISPLAY;
  }
;

opt_on_exception:
| EXCEPTION statement_list		{ current_statement->handler1 = $2; }
;

opt_not_on_exception:
| NOT_EXCEPTION statement_list		{ current_statement->handler2 = $2; }
;

/*
 * ON SIZE ERROR
 */

on_size_error:
  opt_on_size_error
  opt_not_on_size_error
  {
    current_statement->handler_id = COB_EC_SIZE;
  }
;

opt_on_size_error:
| SIZE_ERROR statement_list		{ current_statement->handler1 = $2; }
;

opt_not_on_size_error:
| NOT_SIZE_ERROR statement_list		{ current_statement->handler2 = $2; }
;


/*
 * ON OVERFLOW
 */

on_overflow:
  opt_on_overflow
  opt_not_on_overflow
  {
    current_statement->handler_id = COB_EC_OVERFLOW;
  }
;

opt_on_overflow:
| OVERFLOW statement_list		{ current_statement->handler1 = $2; }
;

opt_not_on_overflow:
| NOT_OVERFLOW statement_list		{ current_statement->handler2 = $2; }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
    current_statement->handler_id = COB_EC_I_O_AT_END;
    current_statement->handler1 = $1;
  }
| not_at_end_sentence
  {
    current_statement->handler_id = COB_EC_I_O_AT_END;
    current_statement->handler2 = $1;
  }
| at_end_sentence not_at_end_sentence
  {
    current_statement->handler_id = COB_EC_I_O_AT_END;
    current_statement->handler1 = $1;
    current_statement->handler2 = $2;
  }
;

at_end_sentence:
  END statement_list		{ $$ = $2; }
| AT END statement_list		{ $$ = $3; }
;

not_at_end_sentence:
  NOT _at END statement_list	{ $$ = $4; }
;


/*
 * AT EOP
 */

at_eop:
  at_eop_sentence
  {
    current_statement->handler_id = COB_EC_I_O_EOP;
    current_statement->handler1 = $1;
  }
| not_at_eop_sentence
  {
    current_statement->handler_id = COB_EC_I_O_EOP;
    current_statement->handler2 = $1;
  }
| at_eop_sentence not_at_eop_sentence
  {
    current_statement->handler_id = COB_EC_I_O_EOP;
    current_statement->handler1 = $1;
    current_statement->handler2 = $2;
  }
;

at_eop_sentence:
  EOP statement_list		{ $$ = $2; }
;

not_at_eop_sentence:
  NOT_EOP statement_list	{ $$ = $2; }
;


/*
 * INVALID KEY
 */

opt_invalid_key:
| invalid_key
;

invalid_key:
  invalid_key_sentence
  {
    current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
    current_statement->handler1 = $1;
  }
| not_invalid_key_sentence
  {
    current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
    current_statement->handler2 = $1;
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    current_statement->handler_id = COB_EC_I_O_INVALID_KEY;
    current_statement->handler1 = $1;
    current_statement->handler2 = $2;
  }
;

invalid_key_sentence:
  INVALID_KEY statement_list		{ $$ = $2; }
;

not_invalid_key_sentence:
  NOT_INVALID_KEY statement_list	{ $$ = $2; }
;


/*****************************************************************************
 * Common Constructs
 *****************************************************************************/


/*******************
 * Expressions
 *******************/

condition:
  expr
  {
    $$ = cb_build_cond ($1);
  }
;

expr:
  partial_expr
  {
    $$ = cb_build_expr ($1);
  }
;

partial_expr:
  {
    current_expr = NULL;
  }
  expr_tokens
  {
    $$ = cb_list_reverse (current_expr);
  }
;

expr_tokens:
  expr_token
| expr_tokens IS
| expr_tokens expr_token
;

expr_token:
  x				{ push_expr ('x', $1); }
/* parenthesis */
| '('				{ push_expr ('(', 0); }
| ')'				{ push_expr (')', 0); }
/* arithmetic operators */
| '+'				{ push_expr ('+', 0); }
| '-'				{ push_expr ('-', 0); }
| '*'				{ push_expr ('*', 0); }
| '/'				{ push_expr ('/', 0); }
| '^'				{ push_expr ('^', 0); }
/* conditional operators */
| eq				{ push_expr ('=', 0); }
| gt				{ push_expr ('>', 0); }
| lt				{ push_expr ('<', 0); }
| GE				{ push_expr (']', 0); }
| LE				{ push_expr ('[', 0); }
| NE				{ push_expr ('~', 0); }
/* logical operators */
| NOT				{ push_expr ('!', 0); }
| AND				{ push_expr ('&', 0); }
| OR				{ push_expr ('|', 0); }
/* class condition */
| NUMERIC			{ push_expr ('9', 0); }
| ALPHABETIC			{ push_expr ('A', 0); }
| ALPHABETIC_LOWER		{ push_expr ('L', 0); }
| ALPHABETIC_UPPER		{ push_expr ('U', 0); }
/* sign condition */
  /* ZERO is defined in 'x' */
| POSITIVE			{ push_expr ('P', 0); }
| NEGATIVE			{ push_expr ('N', 0); }
;

eq: '=' | EQUAL _to ;
gt: '>' | GREATER _than ;
lt: '<' | LESS _than ;
ge: GE | GREATER _than OR EQUAL _to ;
le: LE | LESS _than OR EQUAL _to ;

/* Arithmetic expression */

e_list:
  e				{ $$ = cb_list ($1); }
| e_list e_sep e		{ $$ = cb_list_add ($1, $3); }
;

e_sep:
| COMMA_DELIM
;

e:
  x				{ $$ = $1; }
| '(' e ')'			{ $$ = $2; }
| '+' e  %prec UNARY_SIGN	{ $$ = $2; }
| '-' e  %prec UNARY_SIGN	{ $$ = cb_build_binary_op (cb_zero, '-', $2); }
| e '+' e			{ $$ = cb_build_binary_op ($1, '+', $3); }
| e '-' e			{ $$ = cb_build_binary_op ($1, '-', $3); }
| e '*' e			{ $$ = cb_build_binary_op ($1, '*', $3); }
| e '/' e			{ $$ = cb_build_binary_op ($1, '/', $3); }
| e '^' e			{ $$ = cb_build_binary_op ($1, '^', $3); }
;


/*******************
 * Names
 *******************/

/* LINAGE-COUNTER */

linage_counter:
  LINAGE_COUNTER
  {
	if (current_linage > 1) {
		cb_error ("LINAGE-COUNTER must be qualified here");
		$$ = cb_error_node;
	} else if (current_linage == 0) {
		cb_error ("Invalid LINAGE-COUNTER usage");
		$$ = cb_error_node;
	} else {
		$$ = linage_file->linage_ctr;
	}
  }
|  LINAGE_COUNTER in_of WORD
  {
	if (CB_FILE_P (cb_ref ($3))) {
		$$ = CB_FILE (cb_ref ($3))->linage_ctr;
	} else {
		cb_error_x ($3, _("'%s' not file name"), CB_NAME ($3));
		$$ = cb_error_node;
	}
  }
;


/* Data name */

arithmetic_x_list:
  arithmetic_x			{ $$ = $1; }
| arithmetic_x_list
  arithmetic_x			{ $$ = cb_list_append ($1, $2); }
;

arithmetic_x:
  x flag_rounded		{ $$ = cb_build_pair ($2, $1); }
;

/* Record name */

record_name:
  x
;

/* Table name */

table_name:
  qualified_word
  {
    cb_tree x = cb_ref ($1);
    if (!CB_FIELD_P (x))
      $$ = cb_error_node;
    else if (!CB_FIELD (x)->index_list)
      {
	cb_error_x ($1, _("'%s' not indexed"), cb_name ($1));
	cb_error_x (x, _("'%s' defined here"), cb_name (x));
	$$ = cb_error_node;
      }
    else
      $$ = $1;
  }
;

/* File name */

file_name_list:
  file_name			{ $$ = cb_list ($1); }
| file_name_list file_name	{ $$ = cb_list_add ($1, $2); }
;

file_name:
  qualified_word
  {
    if (CB_FILE_P (cb_ref ($1)))
      $$ = $1;
    else
      {
	cb_error_x ($1, _("'%s' not file name"), CB_NAME ($1));
	$$ = cb_error_node;
      }
  }
;

/* Mnemonic name */

mnemonic_name_list:
  mnemonic_name			{ $$ = cb_list ($1); }
| mnemonic_name_list
  mnemonic_name			{ $$ = cb_list_add ($1, $2); }
;

mnemonic_name:
  MNEMONIC_NAME			{ $$ = $1; }
;

/* Procedure name */

procedure_name_list:
  /* empty */			{ $$ = NULL; }
| procedure_name_list 
  procedure_name		{ $$ = cb_list_add ($1, $2); }
;

procedure_name:
  label
  {
    $$ = $1;
    CB_REFERENCE ($$)->offset = CB_TREE (current_section);
    current_program->label_list = cb_cons ($$, current_program->label_list);
  }
;

label:
  qualified_word
| integer_label
| integer_label in_of integer_label
;

integer_label:
  LITERAL
  {
    $$ = cb_build_reference ((char *)(CB_LITERAL ($1)->data));
    $$->source_file = $1->source_file;
    $$->source_line = $1->source_line;
  }
;

/* Reference */

reference_list:
  reference			{ $$ = cb_list ($1); }
| reference_list reference	{ $$ = cb_list_add ($1, $2); }
;

reference:
  qualified_word
  {
    $$ = $1;
    current_program->reference_list = cb_cons ($$, current_program->reference_list);
  }
;

opt_reference:
  /* empty */			{ $$ = NULL; }
| reference			{ $$ = $1; }
;

reference_or_literal:
  reference
| literal
;

/* Undefined word */

undefined_word:
  WORD
  {
    $$ = $1;
    if (CB_REFERENCE ($$)->word->count > 0)
      {
	redefinition_error ($$);
	$$ = cb_error_node;
      }
  }
;


/*******************
 * Primitive elements
 *******************/

/*
 * Primitive value
 */

target_x_list:
  target_x			{ $$ = cb_list ($1); }
| target_x_list target_x	{ $$ = cb_list_add ($1, $2); }
;

target_x:
  identifier
| ADDRESS _of prog_or_entry alnum_or_id		{ $$ = cb_build_ppointer ($4); }
| ADDRESS _of identifier_1			{ $$ = cb_build_address ($3); }
| linage_counter
;

x_list:
  x				{ $$ = cb_list ($1); }
| x_list x			{ $$ = cb_list_add ($1, $2); }
;

x:
  identifier
| LENGTH _of identifier_1			{ $$ = cb_build_length ($3); }
| LENGTH _of literal				{ $$ = cb_build_length ($3); }
| ADDRESS _of prog_or_entry alnum_or_id		{ $$ = cb_build_ppointer ($4); }
| ADDRESS _of identifier_1			{ $$ = cb_build_address ($3); }
| literal
| function
| linage_counter
;

prog_or_entry:
  PROGRAM
| ENTRY
;

alnum_or_id:
  identifier_1		{ $$ = $1; }
| alnum_literal		{ $$ = $1; }
| TOK_NULL		{ $$ = cb_null; }
;

simple_value:
  identifier
| basic_literal
;

/*
numeric_value:
  identifier
| integer
;
*/

id_or_lit:
  identifier
| LITERAL
;

/*
 * Identifier
 */

identifier:
  identifier_1			{ $$ = cb_build_identifier ($1); }
;

identifier_1:
  qualified_word		{ $$ = $1; }
| qualified_word subref		{ $$ = $1; }
| qualified_word refmod		{ $$ = $1; }
| qualified_word subref refmod	{ $$ = $1; }
;

qualified_word:
  WORD				{ $$ = $1; }
| WORD in_of qualified_word	{ $$ = $1; CB_REFERENCE ($1)->chain = $3; }
;

subref:
  '(' e_list ')'
  {
    $$ = $0;
    CB_REFERENCE ($0)->subs = cb_list_reverse ($2);
  }
;

refmod:
  '(' e ':' ')'
  {
    CB_REFERENCE ($0)->offset = $2;
  }
| '(' e ':' e ')'
  {
    CB_REFERENCE ($0)->offset = $2;
    CB_REFERENCE ($0)->length = $4;
  }
;

/*
 * Literal
 */

integer:
  LITERAL
;

literal:
  basic_literal			{ $$ = $1; }
| ALL basic_literal
  {
    $$ = $2;
    if (CB_LITERAL_P ($2))
      CB_LITERAL ($2)->all = 1;
  }
;

basic_literal:
  alnum_literal			{ $$ = $1; }
| SPACE				{ $$ = cb_space; }
| ZERO				{ $$ = cb_zero; }
| QUOTE				{ $$ = cb_quote; }
| HIGH_VALUE			{ $$ = cb_high; }
| LOW_VALUE			{ $$ = cb_low; }
| TOK_NULL			{ $$ = cb_null; }
;

alnum_literal:
  LITERAL			{ $$ = $1; }
| alnum_literal '&' LITERAL	{ $$ = cb_concat_literals ($1, $3); }
;

/*
 * Function
 */

function:
  FUNCTION_NAME func_args
  {
    $$ = cb_build_intrinsic ($1, $2);
  }
;

func_args:
  /* empty */			{ $$ = NULL; }
| '(' list_func_args ')'	{ $$ = $2;   }
;

list_func_args:
  /* empty */			{ $$ = NULL; }
| e_list			{ $$ = $1;   }
;


/*******************
 * Common rules
 *******************/

/*
 * Common flags
 */

flag_all:
  /* empty */			{ $$ = cb_int0; }
| ALL				{ $$ = cb_int1; }
;

flag_duplicates:
  /* empty */			{ $$ = cb_int0; }
| _with DUPLICATES		{ $$ = cb_int1; }
;

flag_global:
  /* empty */			{ $$ = cb_int0; }
| GLOBAL			{ $$ = cb_int1; }
;

flag_initialized:
  /* empty */			{ $$ = NULL; }
| INITIALIZED			{ $$ = cb_int1; }
;

flag_next:
  /* empty */			{ $$ = cb_int0; }
| NEXT				{ $$ = cb_int1; }
| PREVIOUS			{ $$ = cb_int2; }
;

flag_not:
  /* empty */			{ $$ = cb_int0; }
| NOT				{ $$ = cb_int1; }
;

flag_optional:
  /* empty */			{ $$ = cb_int0; }
| OPTIONAL			{ $$ = cb_int1; }
;

flag_rounded:
  /* empty */			{ $$ = cb_int0; }
| ROUNDED			{ $$ = cb_int1; }
;

flag_separate:
  /* empty */			{ $$ = cb_int0; }
| SEPARATE _character		{ $$ = cb_int1; }
;

/*
 * Prepositions
 */

in_of: IN | OF ;
is_are: IS | ARE ;
records: RECORD _is | RECORDS _are ;

_advancing:	| ADVANCING ;
_are:		| ARE ;
_area:		| AREA ;
_at:		| AT ;
_by:		| BY ;
_character:	| CHARACTER ;
_characters:	| CHARACTERS ;
_collating:	| COLLATING ;
_contains:	| CONTAINS ;
_data:		| DATA ;
_disk:		| DISK ;
_file:		| TOK_FILE ;
_for:		| FOR ;
_from:		| FROM ;
_in:		| IN ;
_is:		| IS ;
_is_are:	| IS | ARE ;
_key:		| KEY ;
_line_or_lines:	| LINE | LINES ;
_lines:		| LINES ;
_mode:		| MODE ;
_multiple:	| MULTIPLE ;
_number:	| NUMBER ;
_of:		| OF ;
_on:		| ON ;
_order:		| ORDER ;
_other:		| OTHER ;
_program:	| PROGRAM ;
_record:	| RECORD ;
_right:		| RIGHT ;
_set:		| SET ;
_sign:		| SIGN ;
_sign_is:	| SIGN _is ;
_size:		| SIZE ;
_status:	| STATUS ;
_tape:		| TAPE ;
_than:		| THAN ;
_then:		| THEN ;
_times:		| TIMES ;
_to:		| TO ;
/* _upon:		| UPON ; */
_when:		| WHEN ;
_with:		| WITH ;


%%

static void
emit_entry (const char *name, const int encode, cb_tree using_list)
{
	cb_tree	l, label;
	int	parmnum;
	char	buff[256];

	sprintf (buff, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = (unsigned char *)(cb_encode_program_id (name));
	} else {
		CB_LABEL (label)->name = (unsigned char *)name;
	}
	CB_LABEL (label)->need_begin = 1;
	emit_statement (label);

	parmnum = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		cb_tree x = CB_VALUE (l);
		if (x != cb_error_node && cb_ref (x) != cb_error_node) {
			struct cb_field *f = CB_FIELD (cb_ref (x));
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), cb_name (x));
			}
			if (!current_program->is_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), cb_name (x));
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' can not be BASED/EXTERNAL"), cb_name (x));
				}
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), cb_name (x));
				}
				f->flag_chained = 1;
				f->param_num = parmnum;
				parmnum++;
			}
			if (f->redefines) {
				cb_error_x (x, _("'%s' REDEFINES field not allowed here"), cb_name (x));
			}
		}
	}
	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		cb_tree x = CB_VALUE (l);

		if (strcmp ((char *)name, (char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (x, _("ENTRY '%s' duplicated"), name);
		}
	}

	entry_number++;

	current_program->entry_list = cb_list_append (current_program->entry_list,
							cb_build_pair (label, using_list));
}

static void
terminator_warning (void)
{
	if (cb_warn_terminator && current_statement->need_terminator) {
		cb_warning_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			current_statement->name, current_statement->name);
	}
}

static void
terminator_error (void)
{
	cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by END-%s"),
			current_statement->name, current_statement->name);
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return '"';
	} else if (x == cb_null) {
		return 0;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}
