/*
 * Copyright (C) 2001-2003 Keisuke Nishida
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

%expect 83

%defines
%verbose

%{
#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libcob.h>

#include "cobc.h"
#include "error.h"
#include "codegen.h"
#include "reserved.h"

#define yyerror		cb_error
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#define PENDING(x)	cb_warning (_("`%s' not implemented"), x)

#define VALIDATE(v,x,msg,cond)			\
  {						\
    if ((x) == cb_error_node)			\
      v = cb_error_node;			\
    else if (!(cond))				\
      {						\
	cb_error_x (x, msg, cb_name (x));	\
	v = cb_error_node;			\
      }						\
    else					\
      v = x;					\
  }

#define VALIDATE_2(v,x,flag,msg,cond)		\
  {						\
    if ((x) == cb_error_node)			\
      v = cb_error_node;			\
    else if (!(cond))				\
      {						\
	cb_error_x (x, msg, cb_name (x));	\
	v = cb_error_node;			\
      }						\
    else					\
      v = cb_build_parameter_1 (flag, x);	\
  }

#define cb_ref(x)	(CB_REFERENCE (x)->value)

#define push(x)	\
  current_program->exec_list = cons (x, current_program->exec_list)

#define push_funcall_0(f)	   push (cb_build_funcall_0 (f))
#define push_funcall_1(f,a)	   push (cb_build_funcall_1 (f, a))
#define push_funcall_2(f,a,b)	   push (cb_build_funcall_2 (f, a, b))
#define push_funcall_3(f,a,b,c)	   push (cb_build_funcall_3 (f, a, b, c))
#define push_funcall_4(f,a,b,c,d)  push (cb_build_funcall_4 (f, a, b, c, d))

#define make_handler(v,a,b)	cb_build_funcall_4 ("@handler", (void *) v, a, b, 0)

#define push_file_handler(file,handler)			\
  if (handler || CB_EXCEPTION_ENABLE (COB_EC_I_O))	\
    push (build_file_handler (file, handler));

#define push_sequence_with_handler(x,h)			\
  {							\
    cb_tree __x = (x);					\
    cb_tree __h = (h);					\
    if (__h) CB_SEQUENCE (__x)->save_status = 1;	\
    push (__x);						\
    if (__h) push (__h);				\
  }

#define push_entry(name,using_list)				\
  {								\
    cb_tree label = cb_build_label (make_reference (name), NULL);	\
    CB_LABEL (label)->need_begin = 1;				\
    push (label);						\
								\
    current_program->entry_list =				\
      list_add (current_program->entry_list,			\
		cb_build_pair (label, CB_TREE (using_list)));	\
  }

#define BEGIN_STATEMENT(name)			\
  current_statement = cb_build_statement (name);	\
  push (current_statement)

static struct cb_statement *current_statement = NULL;

static struct cb_field *current_field;
static struct cb_file *current_file;
static enum cb_storage current_storage;

static int current_call_mode;
static const char *current_inspect_func;
static cb_tree current_inspect_data;

static int last_operator;
static cb_tree last_lefthand;

static cb_tree build_file_handler (cb_tree file, cb_tree handler);

static void terminator_warning (void);
%}

%union {
  int ival;
  char *sval;
  cb_tree tree;
  struct cb_list *list;
  struct cb_picture *pict;
}

%token <sval>  FUNCTION_NAME
%token <pict> PICTURE
%token <tree> WORD LITERAL CLASS_NAME MNEMONIC_NAME

%token ACCEPT ADD CALL CANCEL CLOSE COMPUTE DELETE DISPLAY DIVIDE ENTRY
%token EVALUATE IF INITIALIZE INSPECT MERGE MOVE MULTIPLY OPEN PERFORM
%token READ RELEASE RETURN REWRITE SEARCH SET SORT START STRING
%token SUBTRACT UNSTRING WRITE WORKING_STORAGE ZERO PACKED_DECIMAL RECURSIVE
%token LINAGE FOOTING TOP BOTTOM SHARING ONLY RECORDING LOCAL_STORAGE
%token ACCESS ADVANCING AFTER ALL ALPHABET ALPHABETIC ALPHABETIC_LOWER AS
%token ALPHABETIC_UPPER ALPHANUMERIC ALPHANUMERIC_EDITED ALSO ALTER ALTERNATE
%token AND ANY ARE AREA ASCENDING ASSIGN AT AUTO BACKGROUND_COLOR BEFORE BELL
%token BINARY BLANK BLINK BLOCK BY CHARACTER CHARACTERS CLASS CODE_SET
%token COLLATING COLUMN COMMA COMMAND_LINE COMMON CONFIGURATION CONTAINS
%token CONTENT CONTINUE CONVERTING CORRESPONDING COUNT CRT CURRENCY CURSOR
%token DATA DATE DAY DAY_OF_WEEK DEBUGGING DECIMAL_POINT DECLARATIVES
%token DELIMITED DELIMITER DEPENDING DESCENDING DIVISION DOWN DUPLICATES
%token DYNAMIC ELSE END END_ACCEPT END_ADD END_CALL END_COMPUTE END_DELETE
%token END_DISPLAY END_DIVIDE END_EVALUATE END_IF END_MULTIPLY END_PERFORM
%token END_READ END_RETURN END_REWRITE END_SEARCH END_START END_STRING
%token END_SUBTRACT END_UNSTRING END_WRITE ENVIRONMENT ENVIRONMENT_VARIABLE
%token EOL EOS EOP EQUAL ERASE ERROR EXCEPTION EXIT EXTEND EXTERNAL FD GOBACK
%token FILE_CONTROL FILLER FIRST FOR FOREGROUND_COLOR FROM FULL GE GIVING
%token GLOBAL GO GREATER HIGHLIGHT HIGH_VALUE IDENTIFICATION IN INDEX INDEXED
%token INPUT INPUT_OUTPUT INTO INVALID IS I_O I_O_CONTROL JUSTIFIED KEY LABEL
%token LE LEADING LEFT LENGTH LESS LINE LINES LINKAGE LOCK LOWLIGHT LOW_VALUE
%token MEMORY MINUS MODE MULTIPLE NATIONAL NATIONAL_EDITED NATIVE NEGATIVE
%token NEXT NO NOT NUMBER NUMERIC NUMERIC_EDITED OBJECT_COMPUTER OCCURS OF OFF
%token OMITTED ON OPTIONAL OR ORDER ORGANIZATION OTHER OUTPUT OVERFLOW PADDING
%token PAGE PLUS POINTER POSITION POSITIVE PROCEDURE PROCEED PROGRAM
%token PROGRAM_ID QUOTE RANDOM RECORD RECORDS REDEFINES REEL REFERENCE
%token RELATIVE REMAINDER REMOVAL RENAMES REPLACING REQUIRED RESERVE RETURNING
%token REVERSE_VIDEO REWIND RIGHT ROUNDED RUN SAME SCREEN SD SECTION SECURE
%token SELECT SENTENCE SEPARATE SEQUENCE SEQUENTIAL SIGN SIZE SIZE SORT_MERGE
%token SOURCE_COMPUTER SPACE SPECIAL_NAMES STANDARD STANDARD_1 STANDARD_2
%token STATUS STOP SYMBOLIC SYNCHRONIZED TALLYING TAPE TEST THAN THEN THRU
%token TIME TIMES TO TOK_FILE TOK_INITIAL TOK_TRUE TOK_FALSE TRAILING
%token UNDERLINE UNIT UNTIL UP UPON USAGE USE USING VALUE VARYING WHEN WITH

%type <ival> flag_all flag_duplicates flag_optional flag_global
%type <ival> flag_not flag_next flag_rounded flag_separate
%type <ival> integer display_upon screen_plus_minus level_number
%type <ival> before_or_after perform_test replacing_option
%type <ival> ascending_or_descending opt_from_integer opt_to_integer
%type <list> occurs_key_list data_name_list value_list opt_value_list
%type <list> numeric_value_list inspect_before_after_list
%type <list> reference_list mnemonic_name_list file_name_list using_phrase
%type <list> expr_item_list numeric_name_list numeric_edited_name_list
%type <list> procedure_name_list
%type <tree> at_line_column column_number condition expr expr_1
%type <tree> expr_item record_description_list label line_number literal
%type <tree> field_name integer_label reference_or_literal basic_literal
%type <tree> integer_value numeric_value numeric_expr alphanumeric_value
%type <tree> on_or_off opt_screen_description_list as_literal
%type <tree> opt_with_pointer perform_option perform_procedure
%type <tree> reference screen_description screen_description_list at_eop
%type <tree> section_name statement_list non_all_value value write_from
%type <tree> on_size_error on_overflow at_end opt_invalid_key invalid_key
%type <tree> group_name record_name numeric_name numeric_edited_name
%type <tree> integer_name data_name file_name table_name mnemonic_name
%type <tree> label_name undefined_word program_name
%type <tree> identifier qualified_word subscript function


%%
/*****************************************************************************
 * COBOL Compilation Unit
 *****************************************************************************/

start:
  program_definition
  {
    if (errorcount > 0)
      YYABORT;
    if (!cb_flag_parse_only)
      codegen (current_program);
  }
;

program_definition:
  identification_division
  program_id_paragraph
  environment_division
  data_division
  {
    /* resolve all references so far */
    struct cb_list *l;
    for (l = list_reverse (current_program->reference_list); l; l = l->next)
      resolve_data_name (l->item);
  }
  procedure_division
  end_program
  {
    struct cb_list *l;
    for (l = list_reverse (current_program->label_list); l; l = l->next)
      resolve_label (l->item);
    current_program->file_list = list_reverse (current_program->file_list);
    current_program->exec_list = list_reverse (current_program->exec_list);
  }
;
end_program:
| END PROGRAM program_name '.'
;

identification_division:
| IDENTIFICATION DIVISION '.'
;


/*
 * PROGRAM-ID paragraph
 */

program_id_paragraph:
  PROGRAM_ID '.'
  {
    current_program = cb_build_program ();
    cb_build_index (make_reference ("RETURN-CODE"));
  }
  program_name as_literal program_type '.'
  {
    if ($<tree>5)
      current_program->program_id = CB_LITERAL ($<tree>5)->data;
    else
      {
	int converted = 0;
	char *s, *name = strdup (CB_NAME ($4));
	for (s = name; *s; s++)
	  if (*s == '-')
	    {
	      converted = 1;
	      *s = '_';
	    }
	if (converted)
	  cb_warning_x ($4, _("PROGRAM-ID is converted to `%s'"), name);
	current_program->program_id = name;
      }
  }
;
program_name:
  WORD
;
program_type:
| _is COMMON _program		{ current_program->flag_common = 1; }
| _is TOK_INITIAL _program	{ current_program->flag_initial = 1; }
| _is RECURSIVE _program	{ current_program->flag_recursive = 1; }
;


/*****************************************************************************
 * Environment division
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION '.'
  configuration_section
  input_output_section
;


/*******************
 * Conficuration section
 *******************/

configuration_section:
| CONFIGURATION SECTION '.'
  source_computer_paragraph
  object_computer_paragraph
  special_names_paragraph
;


/*
 * SOURCE-COMPUTER paragraph
 */

source_computer_paragraph:
| SOURCE_COMPUTER '.' source_computer_entry
;
source_computer_entry:
| '.'
| computer_name '.'
| computer_name with_debugging_mode '.'
| with_debugging_mode '.'
;
with_debugging_mode:
  _with DEBUGGING MODE		{ cb_obsolete_2002 ("DEBUGGING MODE"); }
;

computer_name:
  WORD { }
;


/*
 * OBJECT-COMPUTER paragraph
 */

object_computer_paragraph:
| OBJECT_COMPUTER '.' computer_name object_computer_options '.'
;
object_computer_options:
| object_computer_options object_computer_option
;
object_computer_option:
  _program collating_sequence
| MEMORY SIZE _is integer CHARACTERS	{ cb_obsolete_85 ("MEMORY SIZE"); }
;

/* COLLATING SEQUENCE */

collating_sequence:
  _collating SEQUENCE _is alphabet_name
  {
    PENDING ("COLLATING SEQUENCE");
  }
;
alphabet_name:
  WORD { }
;


/*
 * SPECIAL-NAMES paragraph
 */

special_names_paragraph:
| SPECIAL_NAMES '.' opt_special_names
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
;


/* Mnemonic name clause */

mnemonic_name_clause:
  WORD
  {
    $<tree>$ = lookup_system_name (CB_NAME ($1));
    if ($<tree>$ == cb_error_node)
      cb_error_x ($1, _("unknown system-name `%s'"), CB_NAME ($1));
  }
  special_name_mnemonic_define
  special_name_mnemonic_on_off
;
special_name_mnemonic_define:
| IS undefined_word
  {
    associate ($2, $<tree>0);
  }
;
special_name_mnemonic_on_off:
| special_name_mnemonic_on_off
  on_or_off _status _is undefined_word
  {
    cb_tree x = $<tree>-1;
    if (x != cb_error_node)
      {
	struct cb_system_name *s = CB_SYSTEM_NAME (x);
	if (s->category == CB_SWITCH_NAME)
	  {
	    cb_tree x = make_field ($5);
	    CB_FIELD (x)->level = 88;
	    CB_FIELD (x)->parent = CB_FIELD (cb_switch[s->token]);
	    CB_FIELD (x)->values = list ($2);
	  }
	else
	  cb_error_x ($<tree>-2, _("switch-name is expected `%s'"),
		      CB_NAME ($<tree>-2));
      }
  }
;
on_or_off:
  ON				{ $$ = cb_true; }
| OFF				{ $$ = cb_false; }
;


/* Alphabet name clause */

alphabet_name_clause:
  ALPHABET WORD _is alphabet_group
  {
    PENDING ("ALPHABET");
  }
;
alphabet_group:
  STANDARD_1
| STANDARD_2
| NATIVE
| WORD { }
| alphabet_literal_list
;
alphabet_literal_list:
  alphabet_literal
| alphabet_literal_list alphabet_literal
;
alphabet_literal:
  literal alphabet_literal_option { }
;
alphabet_literal_option:
| THRU literal
| also_literal_list
;
also_literal_list:
  ALSO literal
| also_literal_list ALSO literal
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
    current_program->proposition_list =
      list_add (current_program->proposition_list,
		cb_build_proposition ($2, $<list>4));
  }
;
class_item_list:
  class_item			{ $<list>$ = list ($<tree>1); }
| class_item_list class_item	{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
class_item:
  literal			{ $<tree>$ = $1; }
| literal THRU literal
  {
    if (CB_LITERAL ($1)->data[0] < CB_LITERAL ($3)->data[0])
      $<tree>$ = cb_build_pair ($1, $3);
    else
      $<tree>$ = cb_build_pair ($3, $1);
  }
;


/* CURRENCY SIGN clause */

currency_sign_clause:
  CURRENCY _sign _is LITERAL
  {
    unsigned char *s = CB_LITERAL ($4)->data;
    if (CB_LITERAL ($4)->size != 1)
      cb_error_x ($4, _("invalid currency sign `%s'"), s);
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
  CURSOR _is reference		{ PENDING ("CURSOR"); }
;


/* CRT STATUS clause */

crt_status_clause:
  CRT STATUS _is reference	{ PENDING ("CRT STATUS"); }
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
    current_file->optional = $2;

    /* register the file */
    current_program->file_list =
      cons (current_file, current_program->file_list);
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
  ASSIGN _to reference_or_literal { current_file->assign = $3; }
;


/* ACCESS MODE clause */

access_mode_clause:
  ACCESS _mode _is access_mode	{ current_file->access_mode = $<ival>4; }
;
access_mode:
  SEQUENTIAL			{ $<ival>$ = COB_ACCESS_SEQUENTIAL; }
| DYNAMIC			{ $<ival>$ = COB_ACCESS_DYNAMIC; }
| RANDOM			{ $<ival>$ = COB_ACCESS_RANDOM; }
;


/* ALTERNATIVE RECORD KEY clause */

alternative_record_key_clause:
  ALTERNATE RECORD _key _is reference flag_duplicates
  {
    struct cb_alt_key *p = malloc (sizeof (struct cb_alt_key));
    p->key = $5;
    p->duplicates = $6;
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
  collating_sequence
;


/* FILE STATUS clause */

file_status_clause:
  _file STATUS _is reference	{ current_file->file_status = $4; }
;


/* LOCK MODE clause */

lock_mode_clause:
  LOCK _mode _is		{ PENDING ("LOCK MODE"); }
;


/* ORGANIZATION clause */

organization_clause:
  ORGANIZATION _is organization	{ current_file->organization = $<ival>3; }
| organization			{ current_file->organization = $<ival>1; }
;
organization:
  INDEXED			{ $<ival>$ = COB_ORG_INDEXED; }
| SEQUENTIAL			{ $<ival>$ = COB_ORG_SEQUENTIAL; }
| RELATIVE			{ $<ival>$ = COB_ORG_RELATIVE; }
| LINE SEQUENTIAL		{ $<ival>$ = COB_ORG_LINE_SEQUENTIAL; }
;


/* PADDING CHARACTER clause */

padding_character_clause:
  PADDING _character _is reference_or_literal
  {
    cb_obsolete_2002 ("PADDING CHARACTER");
  }
;


/* RECORD DELIMITER clause */

record_delimiter_clause:
  RECORD DELIMITER _is STANDARD_1	{ /* ignored */ }
;


/* RECORD KEY clause */

record_key_clause:
  RECORD _key _is reference	{ current_file->key = $4; }
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
  SHARING _with sharing_option	{ PENDING ("SHARING"); }
;
sharing_option:
  ALL _other
| NO _other
| READ ONLY
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
    switch ($<ival>2)
      {
      case 0:
	/* SAME AREA */
	break;
      case 1:
	/* SAME RECORD */
	break;
      case 2:
	/* SAME sort-MERGE */
	break;
      }
  }
;
same_option:
  /* empty */			{ $<ival>$ = 0; }
| RECORD			{ $<ival>$ = 1; }
| SORT				{ $<ival>$ = 2; }
| SORT_MERGE			{ $<ival>$ = 2; }
;

/* MULTIPLE FILE TAPE clause */

multiple_file_tape_clause:
  MULTIPLE _file _tape		{ cb_obsolete_85 ("MULTIPLE FILE TAPE"); }
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
  file_descriptions
;
file_descriptions:
| file_descriptions file_description
;
file_description:
  file_description_entry
  record_description_list
  {
    finalize_file (current_file, CB_FIELD ($2));
  }
;


/*
 * File description entry
 */

file_description_entry:
  file_type file_name
  {
    if ($2 == cb_error_node)
      YYERROR;

    current_file = CB_FILE (cb_ref ($2));
    if ($<ival>1 != 0)
      current_file->organization = $<ival>1;
  }
  file_description_clause_sequence '.'
;
file_type:
  FD				{ $<ival>$ = 0; }
| SD				{ $<ival>$ = COB_ORG_SORT; }
;
file_description_clause_sequence:
| file_description_clause_sequence file_description_clause
;
file_description_clause:
  _is EXTERNAL			{ PENDING ("EXTERNAL"); }
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
  {
    /* ignored */
  }
;


/* RECORD clause */

record_clause:
  RECORD _contains integer _characters
  {
    current_file->record_max = $3;
  }
| RECORD _contains integer _to integer _characters
  {
    current_file->record_min = $3;
    current_file->record_max = $5;
  }
| RECORD _is VARYING _in _size opt_from_integer opt_to_integer _characters
  record_depending
  {
    current_file->record_min = $6;
    current_file->record_max = $7;
  }
;
record_depending:
| DEPENDING _on reference
  {
    current_file->record_depending = $3;
  }
;
opt_from_integer:
  /* empty */			{ $$ = 0; }
| _from integer			{ $$ = $2; }
;
opt_to_integer:
  /* empty */			{ $$ = 0; }
| TO integer			{ $$ = $2; }
;


/* LABEL RECORDS clause */

label_records_clause:
  LABEL records label_option	{ cb_obsolete_85 ("LABEL RECORDS"); }
;
label_option:
  STANDARD
| OMITTED
;


/* VALUE OF clause */

value_of_clause:
  WORD _is WORD			{ cb_obsolete_85 ("VALUE OF"); }
;


/* DATA RECORDS clause */

data_records_clause:
  DATA records reference_list	{ cb_obsolete_85 ("DATA RECORDS"); }
;


/* LINAGE clause */

linage_clause:
  LINAGE _is integer_value _lines
  linage_footing linage_top linage_bottom
  {
    cb_build_index (make_reference ("LINAGE-COUNTER"));

    cb_error ("LINAGE not implemented");
  }
;
linage_footing:
  _with FOOTING _at integer_value
;

linage_top:
  _lines _at TOP integer_value
;

linage_bottom:
  _lines _at BOTTOM integer_value
;


/* RECORDING MODE clause */

recording_mode_clause:
  RECORDING _mode _is WORD	{ /* ignore */ }
;


/* CODE-SET clause */

code_set_clause:
  CODE_SET _is WORD		{ PENDING ("CODE-SET"); }
;


/*******************
 * WORKING-STRAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION '.'	{ current_storage = CB_STORAGE_WORKING; }
  record_description_list
  {
    if ($5)
      current_program->working_storage = CB_FIELD ($5);
  }
;
record_description_list:
  /* empty */			{ $$ = NULL; }
| record_description_list_1	{ $$ = $<tree>1; }
;
record_description_list_1:
  {
    current_field = NULL;
  }
  record_description_list_2
  {
    struct cb_field *p;
    for (p = CB_FIELD ($<tree>2); p; p = p->sister)
      validate_field (p);
    $<tree>$ = $<tree>2;
  }
;
record_description_list_2:
  data_description		{ $<tree>$ = $<tree>1; }
| record_description_list_2
  data_description		{ $<tree>$ = $<tree>1; }
;
data_description:
  level_number field_name
  {
    current_field = build_field ($1, $2, current_field, current_storage);
    if (current_field == NULL)
      YYERROR;
  }
  field_options '.'
  {
    $<tree>$ = CB_TREE (current_field);
  }
;
level_number:
  WORD
  {
    int level = 0;
    const char *p = CB_REFERENCE ($1)->word->name;
    for (; *p; p++)
      level = level * 10 + (*p - '0');
    if ((01 <= level && level <= 49)
	|| (level == 66 || level == 77 || level == 88))
      $$ = level;
    else
      {
	cb_error_x ($1, _("invalid level number `%s'"), cb_name ($1));
	YYERROR;
      }
  }
;
field_name:
  /* empty */			{ $$ = make_filler (); }
| FILLER			{ $$ = make_filler (); }
| WORD				{ $$ = $1; }
;
field_options:
| field_options field_option
;
field_option:
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
| value_clause
| renames_clause
;


/* REDEFINES */

redefines_clause:
  REDEFINES qualified_word
  {
    current_field->redefines = validate_redefines (current_field, $2);
    if (current_field->redefines == NULL)
      YYERROR;
  }
;


/* EXTERNAL */

external_clause:
  _is EXTERNAL			{ current_field->flag_external = 1; }
;


/* GLOBAL */

global_clause:
  _is GLOBAL			{ PENDING ("GLOBAL"); }
;


/* PICTURE */

picture_clause:
  PICTURE			{ current_field->pic = $1; }
;


/* USAGE */

usage_clause:
  usage
| USAGE _is usage
;
usage:
  BINARY			{ current_field->usage = CB_USAGE_BINARY; }
| DISPLAY			{ current_field->usage = CB_USAGE_DISPLAY; }
| INDEX				{ current_field->usage = CB_USAGE_INDEX; }
| PACKED_DECIMAL		{ current_field->usage = CB_USAGE_PACKED; }
;


/* SIGN */

sign_clause:
  _sign_is LEADING flag_separate
  {
    current_field->flag_sign_separate = $3;
    current_field->flag_sign_leading  = 1;
  }
| _sign_is TRAILING flag_separate
  {
    current_field->flag_sign_separate = $3;
    current_field->flag_sign_leading  = 0;
  }
;
flag_separate:
  /* empty */			{ $$ = 0; }
| SEPARATE _character		{ $$ = 1; }
;


/* OCCURS */

occurs_clause:
  OCCURS integer _times
  occurs_keys occurs_indexed
  {
    current_field->occurs_min = 1;
    current_field->occurs_max = $2;
    current_field->indexes++;
    current_field->flag_occurs = 1;
  }
| OCCURS integer TO integer _times DEPENDING _on reference
  occurs_keys occurs_indexed
  {
    current_field->occurs_min = $2;
    current_field->occurs_max = $4;
    current_field->occurs_depending = $8;
    current_field->indexes++;
    current_field->flag_occurs = 1;
  }
;

occurs_keys:
  occurs_key_list
  {
    if ($1)
      {
	int i, nkeys = list_length ($1);
	struct cb_key *keys = malloc (sizeof (struct cb_key) * nkeys);
	struct cb_list *l = $1;
	for (i = 0; i < nkeys; i++)
	  {
	    struct cb_parameter *p = l->item;
	    keys[i].dir = p->type;
	    keys[i].key = p->x;
	    l = l->next;
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
    struct cb_list *l;
    for (l = $5; l; l = l->next)
      l->item = cb_build_parameter_1 ($2, l->item);
    $$ = list_append ($1, $5);
  }
;
ascending_or_descending:
  ASCENDING			{ $$ = COB_ASCENDING; }
| DESCENDING			{ $$ = COB_DESCENDING; }
;

occurs_indexed:
| INDEXED _by occurs_index_list
  {
    current_field->index_list = $<list>3;
  }
;
occurs_index_list:
  occurs_index			{ $<list>$ = list ($<tree>1); }
| occurs_index_list
  occurs_index			{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
occurs_index:
  undefined_word
  {
    $<tree>$ = cb_build_index ($1);
    current_program->index_list =
      list_add (current_program->index_list, $<tree>$);
  }
;

_times: | TIMES ;


/* JUSTIFIED RIGHT */

justified_clause:
  JUSTIFIED _right		{ current_field->flag_justified = 1; }
;
_right: | RIGHT ;


/* SYNCHRONIZED */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ current_field->flag_synchronized = 1; }
;
left_or_right:
| LEFT
| RIGHT
;


/* BLANK */

blank_clause:
  BLANK _when ZERO		{ current_field->flag_blank_zero = 1; }
;


/* VALUE */

value_clause:
  VALUE _is_are value_item_list	{ current_field->values = $<list>3; }
;
value_item_list:
  value_item			{ $<list>$ = list ($<tree>1); }
| value_item_list value_item	{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
value_item:
  literal			{ $<tree>$ = $1; }
| literal THRU literal		{ $<tree>$ = cb_build_pair ($1, $3); }
;


/* RENAMES */

renames_clause:
  RENAMES qualified_word
  {
    if (resolve_data_name ($2) != cb_error_node)
      {
	current_field->redefines = CB_FIELD (cb_ref ($2));
	current_field->pic = current_field->redefines->pic;
      }
  }
| RENAMES qualified_word THRU qualified_word
  {
    if (resolve_data_name ($2) != cb_error_node
	&& resolve_data_name ($4) != cb_error_node)
      {
	current_field->redefines = CB_FIELD (cb_ref ($2));
	current_field->rename_thru = CB_FIELD (cb_ref ($4));
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
    struct cb_field *p;
    for (p = CB_FIELD ($6); p; p = p->sister)
      validate_field (p);
    current_program->screen_storage = CB_FIELD ($6);
    current_program->flag_screen = 1;
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
  level_number field_name
  {
    current_field = build_field ($1, $2, current_field, current_storage);
    if (current_field == NULL)
      YYERROR;

    current_field->screen_flag |= COB_SCREEN_FG_NONE;
    current_field->screen_flag |= COB_SCREEN_BG_NONE;
    if (current_field->parent)
      current_field->screen_flag |= current_field->parent->screen_flag;
  }
  screen_options '.'
  {
    if (current_field->pic == NULL)
      current_field->pic = cb_parse_picture ("X(0)");
    if (!current_field->screen_line)
      {
	current_field->screen_line = cb_int1;
	current_field->screen_flag |= COB_SCREEN_LINE_CONST;
      }
    if (!current_field->screen_column)
      {
	current_field->screen_column = cb_int1;
	current_field->screen_flag |= COB_SCREEN_COLUMN_CONST;
      }
    $$ = CB_TREE (current_field);
  }
;
screen_options:
| screen_options screen_option
;
screen_option:
  BLANK LINE	{ current_field->screen_flag |= COB_SCREEN_BLANK_LINE; }
| BLANK SCREEN	{ current_field->screen_flag |= COB_SCREEN_BLANK_SCREEN; }
| BELL		{ current_field->screen_flag |= COB_SCREEN_BELL; }
| BLINK		{ current_field->screen_flag |= COB_SCREEN_BLINK; }
| ERASE EOL	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOL; }
| ERASE EOS	{ current_field->screen_flag |= COB_SCREEN_ERASE_EOS; }
| HIGHLIGHT	{ current_field->screen_flag |= COB_SCREEN_HIGHLIGHT; }
| LOWLIGHT	{ current_field->screen_flag |= COB_SCREEN_LOWLIGHT; }
| REVERSE_VIDEO	{ current_field->screen_flag |= COB_SCREEN_REVERSE; }
| UNDERLINE	{ current_field->screen_flag |= COB_SCREEN_UNDERLINE; }
| AUTO		{ current_field->screen_flag |= COB_SCREEN_AUTO; }
| SECURE	{ current_field->screen_flag |= COB_SCREEN_SECURE; }
| REQUIRED	{ current_field->screen_flag |= COB_SCREEN_REQUIRED; }
| FULL		{ current_field->screen_flag |= COB_SCREEN_FULL; }
| LINE _number _is screen_plus_minus integer_value
  {
    current_field->screen_line = $5;
    if (CB_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| COLUMN _number _is screen_plus_minus integer_value
  {
    current_field->screen_column = $5;
    if (CB_LITERAL_P ($5))
      current_field->screen_flag |= COB_SCREEN_LINE_CONST;
  }
| FOREGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_FG_MASK;
    switch ($3)
      {
      case 0: current_field->screen_flag |= COB_SCREEN_FG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_FG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_FG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_FG_SKYBLUE; break;
      case 4: current_field->screen_flag |= COB_SCREEN_FG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_FG_PURPLE; break;
      case 6: current_field->screen_flag |= COB_SCREEN_FG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_FG_WHITE; break;
      default:
	cb_error (_("invalid color `%d'"), $3);
      }
  }
| BACKGROUND_COLOR _is integer
  {
    current_field->screen_flag &= ~COB_SCREEN_BG_MASK;
    switch ($3)
      {
      case 0: current_field->screen_flag |= COB_SCREEN_BG_BLACK; break;
      case 1: current_field->screen_flag |= COB_SCREEN_BG_BLUE; break;
      case 2: current_field->screen_flag |= COB_SCREEN_BG_GREEN; break;
      case 3: current_field->screen_flag |= COB_SCREEN_BG_SKYBLUE; break;
      case 4: current_field->screen_flag |= COB_SCREEN_BG_RED; break;
      case 5: current_field->screen_flag |= COB_SCREEN_BG_PURPLE; break;
      case 6: current_field->screen_flag |= COB_SCREEN_BG_YELLOW; break;
      case 7: current_field->screen_flag |= COB_SCREEN_BG_WHITE; break;
      default:
	cb_error (_("invalid color `%d'"), $3);
      }
  }
| usage_clause
| blank_clause
| justified_clause
| sign_clause
| value_clause
| picture_clause
| USING data_name
  {
    current_field->screen_from = CB_FIELD ($2);
    current_field->screen_to = CB_FIELD ($2);
  }
| FROM data_name
  {
    current_field->screen_from = CB_FIELD ($2);
  }
| TO data_name
  {
    current_field->screen_to = CB_FIELD ($2);
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
| PROCEDURE DIVISION using_phrase '.'
  {
    current_section = NULL;
    current_paragraph = NULL;
  }
  procedure_declaratives
  {
    push_entry (current_program->program_id, $3); /* main entry point */
  }
  procedure_list
  {
    if (current_paragraph)
      push (cb_build_perform_exit (current_paragraph));
    if (current_section)
      push (cb_build_perform_exit (current_section));
  }
;
using_phrase:
  /* empty */			{ $$ = NULL; }
| USING data_name_list
  {
    struct cb_list *l;
    for (l = $2; l; l = l->next)
      if (l->item != cb_error_node)
	{
	  struct cb_field *f = CB_FIELD (cb_ref (l->item));
	  if (f->level != 01 && f->level != 77)
	    cb_error_x (l->item, _("`%s' not level 01 or 77"), f->name);
	  l->item = cb_ref (l->item);
	}
    $$ = $2;
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
| error '.'
| '.'
;


/*******************
 * Section/Paragraph
 *******************/

section_header:
  section_name SECTION '.'
  {
    /* Exit the last section */
    if (current_paragraph)
      push (cb_build_perform_exit (current_paragraph));
    if (current_section)
      push (cb_build_perform_exit (current_section));

    /* Begin a new section */
    current_section = CB_LABEL (cb_build_label ($1, NULL));
    current_paragraph = NULL;
    push (current_section);
  }
;

paragraph_header:
  section_name '.'
  {
    /* Exit the last paragraph */
    if (current_paragraph)
      push (cb_build_perform_exit (current_paragraph));

    /* Begin a new paragraph */
    current_paragraph = CB_LABEL (cb_build_label ($1, current_section));
    if (current_section)
      current_section->children =
	cons (current_paragraph, current_section->children);
    push (current_paragraph);
  }
;

invalid_statement:
  section_name
  {
    cb_error_x ($1, _("unknown statement `%s'"), CB_NAME ($1));
    YYERROR;
  }
;

section_name:
  WORD
  {
    struct cb_word *w = CB_REFERENCE ($1)->word;
    if (w->count > 0)
      {
	cb_tree item = w->items->item;
	if (/* used as a non-label name */
	    !CB_LABEL_P (item)
	    /* used as a section name */
	    || CB_LABEL (item)->section == NULL
	    /* used as the same paragraph name in the same section */
	    || CB_LABEL (item)->section == current_section)
	  {
	    redefinition_error ($1);
	    YYERROR;
	  }
      }
    $$ = $1;
  }
;


/*******************
 * Statements
 *******************/

statement_list:
  {
    $<list>$ = current_program->exec_list;
    current_program->exec_list = NULL;
  }
  {
    $<tree>$ = CB_TREE (current_statement);
    current_statement->need_terminator = 1;
    current_statement = NULL;
  }
  statements
  {
    $$ = make_sequence (list_reverse (current_program->exec_list));
    current_program->exec_list = $<list>1;
    current_statement = CB_STATEMENT ($<tree>2);
  }
;
statements:
  statement
| statements statement
;
statement:
  accept_statement
| add_statement
| alter_statement
| call_statement
| cancel_statement
| close_statement
| compute_statement
| delete_statement
| display_statement
| divide_statement
| entry_statement
| evaluate_statement
| exit_statement
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
| unstring_statement
| use_statement
| write_statement
| CONTINUE
| NEXT SENTENCE			{ cb_archaic ("NEXT SENTENCE"); }
;


/*
 * ACCEPT statement
 */

accept_statement:
  ACCEPT			{ BEGIN_STATEMENT ("ACCEPT"); }
  accept_body end_accept
;
accept_body:
  data_name at_line_column
  {
    if (current_program->flag_screen)
      {
	if (CB_FIELD ($1)->storage == CB_STORAGE_SCREEN)
	  {
	    cb_tree line = CB_PARAMETER ($2)->x;
	    cb_tree column = CB_PARAMETER ($2)->y;
	    push_funcall_3 ("cob_screen_accept", $1, line, column);
	  }
	else
	  cb_error_x ($1, "`%s' not defined in SCREEN SECTION", cb_name ($1));
      }
    else
      {
	push_funcall_2 ("cob_accept", $1, cb_build_integer (COB_SYSIN));
      }
  }
| data_name FROM DATE
  {
    push_funcall_1 ("cob_accept_date", $1);
  }
| data_name FROM DAY
  {
    push_funcall_1 ("cob_accept_day", $1);
  }
| data_name FROM DAY_OF_WEEK
  {
    push_funcall_1 ("cob_accept_day_of_week", $1);
  }
| data_name FROM TIME
  {
    push_funcall_1 ("cob_accept_time", $1);
  }
| data_name FROM COMMAND_LINE
  {
    push_funcall_1 ("cob_accept_command_line", $1);
  }
| data_name FROM ENVIRONMENT_VARIABLE value
  {
    push_funcall_2 ("cob_accept_environment", $1, $4);
  }
| data_name FROM mnemonic_name
  {
    switch (CB_SYSTEM_NAME (cb_ref ($3))->token)
      {
      case CB_CONSOLE:
      case CB_SYSIN:
	push_funcall_2 ("cob_accept", $1, cb_build_integer (COB_SYSIN));
	break;
      default:
	cb_error_x ($3, _("invalid input stream `%s'"), cb_name ($3));
	break;
      }
  }
;
end_accept:
| END_ACCEPT
;

at_line_column:
  /* empty */			{ $$ = cb_build_pair (cb_int1, cb_int1); }
| _at line_number column_number { $$ = cb_build_pair ($2, $3); }
| _at column_number line_number { $$ = cb_build_pair ($3, $2); }
;
line_number:
  LINE _number integer_value	{ $$ = cb_build_cast_integer ($3); }
;
column_number:
  COLUMN _number integer_value	{ $$ = cb_build_cast_integer ($3); }
;


/*
 * ADD statement
 */

add_statement:
  ADD				{ BEGIN_STATEMENT ("ADD"); }
  add_body on_size_error
  end_add
  {
    push_sequence_with_handler ($<tree>3, $4);
  }
;
add_body:
  numeric_value_list TO numeric_name_list
  {
    $<tree>$ = cb_build_assign ($3, '+', cb_build_connective_op ($1, '+'));
  }
| numeric_value_list add_to GIVING numeric_edited_name_list
  {
    $<tree>$ = cb_build_assign ($4, 0, cb_build_connective_op ($1, '+'));
  }
| CORRESPONDING group_name _to group_name flag_rounded
  {
    $<tree>$ = cb_build_corr (cb_build_add, $4, $2, $5);
  }
;
add_to:
| TO numeric_value		{ list_add ($<list>0, $2); }
;
end_add:
  /* empty */			{ terminator_warning (); }
| END_ADD
;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options		{ cb_obsolete_85 ("ALTER"); }
;
alter_options:
| alter_options
  label TO _proceed_to label
;
_proceed_to: | PROCEED TO ;


/*
 * CALL statement
 */

call_statement:
  CALL	 			{ BEGIN_STATEMENT ("CALL"); }
  alphanumeric_value call_using call_returning
  call_on_exception call_not_on_exception
  end_call
  {
    cb_tree x = $<tree>6 ? $<tree>6 : cb_build_funcall_0 ("cob_call_error");
    push_funcall_4 ("@call", $3, $<list>4, x, $<tree>7);
    if ($<tree>5)
      push (cb_build_move (cb_return_code, $<tree>5));
  }
;
call_using:
  /* empty */		{ $<list>$ = NULL; }
| USING			{ current_call_mode = CB_CALL_BY_REFERENCE; }
  call_param_list	{ $<list>$ = $<list>3; }
;
call_param_list:
  call_param		{ $<list>$ = list ($<tree>1); }
| call_param_list
  call_param		{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
call_param:
  value			{ $<tree>$ = cb_build_parameter_1 (current_call_mode, $1);}
| _by call_mode value	{ $<tree>$ = cb_build_parameter_1 (current_call_mode, $3);}
;
call_mode:
  REFERENCE		{ current_call_mode = CB_CALL_BY_REFERENCE; }
| CONTENT		{ current_call_mode = CB_CALL_BY_CONTENT; }
| CONTENT LENGTH _of	{ current_call_mode = CB_CALL_BY_LENGTH; }
| VALUE			{ current_call_mode = CB_CALL_BY_VALUE; }
;
call_returning:
  /* empty */		{ $<tree>$ = NULL; }
| RETURNING data_name	{ $<tree>$ = $2; }
;
call_on_exception:
  /* empty */		{ $<tree>$ = NULL; }
| _on OVERFLOW
  statement_list	{ $<tree>$ = $3; }
| _on EXCEPTION
  statement_list	{ $<tree>$ = $3; }
;
call_not_on_exception:
  /* empty */		{ $<tree>$ = NULL; }
| NOT _on EXCEPTION
  statement_list	{ $<tree>$ = $4; }
;
end_call:
  /* empty */		{ terminator_warning (); }
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
| cancel_list alphanumeric_value
  {
    push_funcall_1 ("cob_cancel", $2);
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
| close_list file_name close_option
  {
    if ($2 != cb_error_node)
      {
	cb_tree file = cb_ref ($2);
	push_funcall_2 ("cob_close", file, cb_build_integer ($<ival>3));
	push_file_handler (file, NULL);
      }
  }
;
close_option:
  /* empty */			{ $<ival>$ = COB_CLOSE_NORMAL; }
| reel_or_unit			{ $<ival>$ = COB_CLOSE_UNIT; }
| reel_or_unit _for REMOVAL	{ $<ival>$ = COB_CLOSE_UNIT_REMOVAL; }
| _with NO REWIND		{ $<ival>$ = COB_CLOSE_NO_REWIND; }
| _with LOCK			{ $<ival>$ = COB_CLOSE_LOCK; }
;
reel_or_unit: REEL | UNIT ;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE			{ BEGIN_STATEMENT ("COMPUTE"); }
  compute_body on_size_error
  end_compute
  {
    push_sequence_with_handler ($<tree>3, $4);
  }
;
compute_body:
  numeric_edited_name_list '=' numeric_expr
  {
    $<tree>$ = cb_build_assign ($1, 0, $3);
  }
;
end_compute:
  /* empty */			{ terminator_warning (); }
| END_COMPUTE
;


/*
 * DELETE statement
 */

delete_statement:
  DELETE			{ BEGIN_STATEMENT ("DELETE"); }
  file_name _record opt_invalid_key
  end_delete
  {
    cb_tree file = cb_ref ($3);
    push_funcall_1 ("cob_delete", file);
    push_file_handler (file, $5);
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
  opt_value_list display_upon at_line_column
  {
    struct cb_list *l;
    if (current_program->flag_screen)
      {
	for (l = $3; l; l = l->next)
	  if (CB_FIELD (l->item)->storage == CB_STORAGE_SCREEN)
	    {
	      cb_tree line = CB_PARAMETER ($5)->x;
	      cb_tree column = CB_PARAMETER ($5)->y;
	      push_funcall_3 ("cob_screen_display", l->item, line, column);
	    }
	  else
	    cb_error_x (l->item, "`%s' not defined in SCREEN SECTION",
			 cb_name (l->item));
      }
    else
      {
	cb_tree fd = cb_build_integer ($4);
	for (l = $3; l; l = l->next)
	  push_funcall_2 ("cob_display", l->item, fd);
      }
  }
  display_with_no_advancing
  end_display
  ;
display_upon:
  /* empty */			{ $$ = COB_SYSOUT; }
| _upon mnemonic_name
  {
    switch (CB_SYSTEM_NAME (cb_ref ($2))->token)
      {
      case CB_CONSOLE: $$ = COB_SYSOUT; break;
      case CB_SYSOUT:  $$ = COB_SYSOUT; break;
      case CB_SYSERR:  $$ = COB_SYSERR; break;
      default:
	cb_error_x ($2, _("invalid UPON item"));
	$$ = COB_SYSOUT;
	break;
      }
  }
| UPON WORD
  {
    cb_warning_x ($2, _("`%s' undefined in SPECIAL-NAMES"), CB_NAME ($2));
    $$ = COB_SYSOUT;
  }
;
display_with_no_advancing:
  /* empty */
  {
    if (!current_program->flag_screen)
      push_funcall_1 ("cob_newline", cb_build_integer ($<ival>-2));
  }
| _with NO ADVANCING { /* nothing */ }
;
end_display:
| END_DISPLAY
;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE			{ BEGIN_STATEMENT ("DIVIDE"); }
  divide_body on_size_error
  end_divide
  {
    push_sequence_with_handler ($<tree>3, $4);
  }
;
divide_body:
  numeric_value INTO numeric_name_list
  {
    $<tree>$ = cb_build_assign ($3, '/', $1);
  }
| numeric_value INTO numeric_value GIVING numeric_edited_name_list
  {
    $<tree>$ = cb_build_assign ($5, 0, cb_build_binary_op ($3, '/', $1));
  }
| numeric_value BY numeric_value GIVING numeric_edited_name_list
  {
    $<tree>$ = cb_build_assign ($5, 0, cb_build_binary_op ($1, '/', $3));
  }
| numeric_value INTO numeric_value GIVING numeric_edited_name
  REMAINDER numeric_edited_name
  {
    $<tree>$ = cb_build_divide ($3, $1, $5, $7);
  }
| numeric_value BY numeric_value GIVING numeric_edited_name
  REMAINDER numeric_edited_name
  {
    $<tree>$ = cb_build_divide ($1, $3, $5, $7);
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
  literal using_phrase
  {
    push_entry (CB_LITERAL ($3)->data, $4);
  }
;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE			{ BEGIN_STATEMENT ("EVALUATE"); }
  evaluate_subject_list evaluate_case_list
  end_evaluate
  {
    push (cb_build_evaluate ($<list>3, $<list>4));
  }
;

evaluate_subject_list:
  evaluate_subject		{ $<list>$ = list ($<tree>1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $<list>$ = list_add ($<list>1, $<tree>3); }
;
evaluate_subject:
  expr				{ $<tree>$ = $1; }
| TOK_TRUE			{ $<tree>$ = cb_true; }
| TOK_FALSE			{ $<tree>$ = cb_false; }
;

evaluate_case_list:
  /* empty */			{ $<list>$ = NULL; }
| evaluate_case_list
  evaluate_case			{ $<list>$ = list_add ($<list>1, $<list>2); }
;
evaluate_case:
  evaluate_when_list
  statement_list		{ $<list>$ = cons ($2, $<list>1); }
| WHEN OTHER
  statement_list		{ $<list>$ = cons ($3, NULL); }
;
evaluate_when_list:
  WHEN evaluate_object_list	{ $<list>$ = list ($<list>2); }
| evaluate_when_list
  WHEN evaluate_object_list	{ $<list>$ = list_add ($<list>1, $<list>3); }
;
evaluate_object_list:
  evaluate_object		{ $<list>$ = list ($<tree>1); }
| evaluate_object_list ALSO
  evaluate_object		{ $<list>$ = list_add ($<list>1, $<tree>3); }
;
evaluate_object:
  flag_not expr			{ $<tree>$ = cb_build_parameter ($1, $2, 0); }
| flag_not expr THRU expr	{ $<tree>$ = cb_build_parameter ($1, $2, $4); }
| ANY				{ $<tree>$ = cb_any; }
| TOK_TRUE			{ $<tree>$ = cb_true; }
| TOK_FALSE			{ $<tree>$ = cb_false; }
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
| PROGRAM			{ push_funcall_0 ("@exit-program"); }
;


/*
 * GO TO statement
 */

goto_statement:
  GO _to			{ BEGIN_STATEMENT ("GO TO"); }
  procedure_name_list goto_depending
  {
    if ($<tree>5)
      {
	/* GO TO procedure-name ... DEPENDING ON identifier */
	push_funcall_2 ("@goto-depending", $4, $<tree>5);
      }
    else
      {
	/* GO TO procedure-name */
	if ($4 == NULL)
	  cb_obsolete_85 ("GO TO without procedure-name");
	else if ($4->next)
	  cb_error (_("GO TO with multiple procesure-name"));
	else
	  push_funcall_1 ("@goto", $4->item);
      }
  }
;
goto_depending:
  /* empty */			{ $<tree>$ = NULL; }
| DEPENDING _on integer_name	{ $<tree>$ = $3; }
;


/*
 * GOBACK statement
 */

goback_statement:
  GOBACK			{ BEGIN_STATEMENT ("GOBACK"); }
  {
    push_funcall_0 ("@exit-program");
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
    push (cb_build_if ($3, $5, $<tree>6));
  }
| IF error END_IF
;
if_else_sentence:
  /* empty */			{ $<tree>$ = NULL; }
| ELSE statement_list		{ $<tree>$ = $2; }
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
  data_name_list initialize_replacing
  {
    struct cb_list *l;
    for (l = $3; l; l = l->next)
      push_funcall_2 ("@initialize", l->item, $<list>4);
  }
;
initialize_replacing:
  /* empty */			      { $<list>$ = NULL; }
| REPLACING initialize_replacing_list { $<list>$ = $<list>2; }
;
initialize_replacing_list:
  /* empty */			      { $<list>$ = NULL; }
| initialize_replacing_list
  replacing_option _data BY value
  {
    $<list>$ = list_add ($<list>1, cb_build_parameter_1 ($2, $5));
  }
;
replacing_option:
  ALPHABETIC			{ $$ = CB_CATEGORY_ALPHABETIC; }
| ALPHANUMERIC			{ $$ = CB_CATEGORY_ALPHANUMERIC; }
| NUMERIC			{ $$ = CB_CATEGORY_NUMERIC; }
| ALPHANUMERIC_EDITED		{ $$ = CB_CATEGORY_ALPHANUMERIC_EDITED; }
| NUMERIC_EDITED		{ $$ = CB_CATEGORY_NUMERIC_EDITED; }
| NATIONAL			{ $$ = CB_CATEGORY_NATIONAL; }
| NATIONAL_EDITED		{ $$ = CB_CATEGORY_NATIONAL_EDITED; }
;
_data: | DATA ;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT			{ BEGIN_STATEMENT ("INSPECT"); }
  data_name inspect_list
;
inspect_list:
| inspect_list inspect_item
;
inspect_item:
  inspect_tallying
  {
    struct cb_list *l = $<list>1;
    l = cons (cb_build_funcall_2 ("cob_inspect_init", $<tree>-1, cb_int0), l);
    l = list_add (l, cb_build_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
| inspect_replacing
  {
    struct cb_list *l = $<list>1;
    l = cons (cb_build_funcall_2 ("cob_inspect_init", $<tree>-1, cb_int1), l);
    l = list_add (l, cb_build_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
| inspect_converting
  {
    struct cb_list *l = $<list>1;
    l = cons (cb_build_funcall_2 ("cob_inspect_init", $<tree>-1, cb_int0), l);
    l = list_add (l, cb_build_funcall_0 ("cob_inspect_finish"));
    push (make_sequence (l));
  }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING
  {
    current_inspect_func = NULL;
    current_inspect_data = NULL;
  }
  tallying_list
  {
    $<list>$ = $<list>3;
  }
;
tallying_list:
  tallying_item			{ $<list>$ = $<list>1; }
| tallying_list tallying_item	{ $<list>$ = list_append ($<list>1, $<list>2); }
;
tallying_item:
  non_all_value FOR
  {
    current_inspect_data = $1;
    $<list>$ = NULL;
  }
| CHARACTERS inspect_before_after_list
  {
    if (current_inspect_data == NULL)
      cb_error (_("data name expected before CHARACTERS"));
    current_inspect_func = NULL;
    $<list>$ = list_add ($2, cb_build_funcall_1 ("cob_inspect_characters", current_inspect_data));
  }
| ALL
  {
    if (current_inspect_data == NULL)
      cb_error (_("data name expected before ALL"));
    current_inspect_func = "cob_inspect_all";
    $<list>$ = NULL;
  }
| LEADING
  {
    if (current_inspect_data == NULL)
      cb_error (_("data name expected before LEADING"));
    current_inspect_func = "cob_inspect_leading";
    $<list>$ = NULL;
  }
| non_all_value inspect_before_after_list
  {
    if (current_inspect_func == NULL)
      cb_error_x ($1, _("ALL or LEADING expected before `%s'"), cb_name ($1));
    $<list>$ = list_add ($2, cb_build_funcall_2 (current_inspect_func, current_inspect_data, $1));
  }
;

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_item
  {
    $<list>$ = $<list>2;
  }
| inspect_replacing replacing_item
  {
    $<list>$ = list_append ($<list>1, $<list>2);
  }
;
replacing_item:
  CHARACTERS BY value inspect_before_after_list
  {
    $<list>$ = list_add ($4, cb_build_funcall_1 ("cob_inspect_characters", $3));
  }
| ALL value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, cb_build_funcall_2 ("cob_inspect_all", $4, $2));
  }
| LEADING value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, cb_build_funcall_2 ("cob_inspect_leading", $4, $2));
  }
| FIRST value BY value inspect_before_after_list
  {
    $<list>$ = list_add ($5, cb_build_funcall_2 ("cob_inspect_first", $4, $2));
  }
;

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING value TO value inspect_before_after_list
  {
    $<list>$ = list_add ($5, cb_build_funcall_2 ("cob_inspect_converting", $2, $4));
  }
;

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* empty */
  {
    $$ = list (cb_build_funcall_0 ("cob_inspect_start"));
  }
| inspect_before_after_list before_or_after _initial value
  {
    if ($2 == CB_BEFORE)
      $$ = list_add ($1, cb_build_funcall_1 ("cob_inspect_before", $4));
    else
      $$ = list_add ($1, cb_build_funcall_1 ("cob_inspect_after", $4));
  }
;
_initial: | TOK_INITIAL ;


/*
 * MERGE statement
 */

merge_statement:
  MERGE				{ BEGIN_STATEMENT ("MERGE"); }
  file_name sort_key_list sort_collating
  {
    push_funcall_2 ("@sort-init", $3, $<list>4);
    $<tree>$ = $3; /* used in sort_input, sort_output */
  }
  sort_input sort_output
;


/*
 * MOVE statement
 */

move_statement:
  MOVE				{ BEGIN_STATEMENT ("MOVE"); }
  move_body
;
move_body:
  value TO data_name_list
  {
    struct cb_list *l;
    for (l = $3; l; l = l->next)
      push (cb_build_move ($1, l->item));
  }
| CORRESPONDING group_name TO group_name
  {
    if ($2 != cb_error_node && $4 != cb_error_node)
      push (cb_build_corr (cb_build_move, $2, $4, -1));
  }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY			{ BEGIN_STATEMENT ("MULTIPLY"); }
  multiply_body on_size_error
  end_multiply
  {
    push_sequence_with_handler ($<tree>3, $4);
  }
;
multiply_body:
  numeric_value BY numeric_name_list
  {
    $<tree>$ = cb_build_assign ($3, '*', $1);
  }
| numeric_value BY numeric_value GIVING numeric_edited_name_list
  {
    $<tree>$ = cb_build_assign ($5, 0, cb_build_binary_op ($1, '*', $3));
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
| open_list open_mode file_name_list
  {
    struct cb_list *l;
    for (l = $3; l; l = l->next)
      {
	cb_tree file = cb_ref (l->item);
	push_funcall_2 ("cob_open", file, cb_build_integer ($<ival>2));
	push_file_handler (file, NULL);
      }
  }
;
open_mode:
  INPUT				{ $<ival>$ = COB_OPEN_INPUT; }
| OUTPUT			{ $<ival>$ = COB_OPEN_OUTPUT; }
| I_O				{ $<ival>$ = COB_OPEN_I_O; }
| EXTEND			{ $<ival>$ = COB_OPEN_EXTEND; }
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
    CB_PERFORM ($2)->body = $1;
    push ($2);
  }
| perform_option statement_list end_perform
  {
    CB_PERFORM ($1)->body = $2;
    push ($1);
  }
;
end_perform:
  /* empty */
  {
    cb_error_x ($<tree>-2, _("%s statement not terminated by END-%s"),
	       "PERFORM", "PERFORM");
  }
| END_PERFORM
;

perform_procedure:
  label
  {
    CB_REFERENCE ($1)->length = cb_true; /* return from $1 */
    $$ = cb_build_pair ($1, $1);
  }
| label THRU label
  {
    CB_REFERENCE ($3)->length = cb_true; /* return from $3 */
    $$ = cb_build_pair ($1, $3);
  }
;

perform_option:
  /* empty */
  {
    $$ = cb_build_perform (CB_PERFORM_ONCE);
  }
| integer_value TIMES
  {
    $$ = cb_build_perform (CB_PERFORM_TIMES);
    CB_PERFORM ($$)->data = $1;
    current_program->loop_counter++;
  }
| perform_test UNTIL condition
  {
    $$ = cb_build_perform (CB_PERFORM_UNTIL);
    CB_PERFORM ($$)->test = $1;
    cb_add_perform_varying (CB_PERFORM ($$), 0, 0, 0, $3);
  }
| perform_test VARYING
  {
    $<tree>$ = cb_build_perform (CB_PERFORM_UNTIL);
    CB_PERFORM ($<tree>$)->test = $1;
  }
  perform_varying_list
  {
    $$ = $<tree>3;
  }
;
perform_test:
  /* empty */			{ $$ = CB_BEFORE; }
| _with TEST before_or_after	{ $$ = $3; }
;
perform_varying_list:
  perform_varying
| perform_varying_list AFTER	{ $<tree>$ = $<tree>0; }
  perform_varying
;
perform_varying:
  data_name FROM value BY value UNTIL condition
  {
    cb_tree step = cb_build_add ($1, $5, 0);
    cb_add_perform_varying (CB_PERFORM ($<tree>0), $1, $3, step, $7);
  }
;


/*
 * READ statements
 */

read_statement:
  READ				{ BEGIN_STATEMENT ("READ"); }
  file_name flag_next _record read_into read_key read_handler
  end_read
  {
    cb_tree file = cb_ref ($3);
    cb_tree key = $<tree>7;
    if ($4 || CB_FILE (file)->access_mode == COB_ACCESS_SEQUENTIAL)
      {
	/* READ NEXT */
	if (key)
	  cb_warning (_("KEY ignored with sequential READ"));
	push_funcall_2 ("cob_read", file, cb_int0);
      }
    else
      {
	/* READ */
	push_funcall_2 ("cob_read", file, key ? key : CB_FILE (file)->key);
      }
    if ($<tree>6)
      push (cb_build_move (CB_TREE (CB_FILE (file)->record), $<tree>6));
    push_file_handler (file, $<tree>8);
  }
;
read_into:
  /* empty */			{ $<tree>$ = NULL; }
| INTO data_name		{ $<tree>$ = $2; }
;
read_key:
  /* empty */			{ $<tree>$ = NULL; }
| KEY _is data_name		{ $<tree>$ = $3; }
;
read_handler:
  /* empty */			{ $<tree>$ = NULL; }
| at_end			{ $<tree>$ = $1; }
| invalid_key			{ $<tree>$ = $1; }
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
    cb_tree file = CB_TREE (CB_FIELD (cb_ref ($3))->file);
    if ($4)
      push (cb_build_move ($4, $3));
    push_funcall_2 ("cob_write", file, $3);
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
    cb_tree file = cb_ref ($3);
    push_funcall_2 ("cob_read", file, cb_int0);
    if ($<tree>5)
      push (cb_build_move (CB_TREE (CB_FILE (file)->record), $<tree>5));
    push_file_handler (file, $6);
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
    cb_tree file = CB_TREE (CB_FIELD (cb_ref ($3))->file);
    if ($4)
      push (cb_build_move ($4, $3));
    push_funcall_2 ("cob_rewrite", file, $3);
    push_file_handler (file, $5);
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
    push_funcall_4 ("@search", $1, $<tree>2, $<tree>3, $<tree>4);
  }
| ALL table_name search_at_end search_all_when
  {
    push_funcall_3 ("@search-all", $2, $<tree>3, $<tree>4);
  }
;
search_varying:
  /* empty */			{ $<tree>$ = NULL; }
| VARYING data_name		{ $<tree>$ = $2; }
;
search_at_end:
  /* empty */			{ $<tree>$ = NULL; }
| _at END statement_list	{ $<tree>$ = $3; }
;
search_whens:
  search_when			{ $<tree>$ = $<tree>1; }
| search_when search_whens
  {
    $<tree>$ = $<tree>1;
    CB_IF ($<tree>1)->stmt2 = $<tree>2;
  }
;
search_when:
  WHEN condition statement_list	{ $<tree>$ = cb_build_if ($2, $3, 0); }
;
search_all_when:
  WHEN expr statement_list
  {
    $<tree>$ = cb_build_if (cb_build_search_all ($<tree>-1, $2), $3, 0);
  }
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
  set_to
| set_up_down
| set_to_on_off_sequence
| set_to_true_false_sequence
;

/* SET name ... TO expr */

set_to:
  data_name_list TO value
  {
    struct cb_list *l;
    for (l = $1; l; l = l->next)
      push (cb_build_move ($3, l->item));
  }
;

/* SET name ... UP/DOWN BY expr */

set_up_down:
  data_name_list up_or_down BY numeric_value
  {
    struct cb_list *l;
    for (l = $1; l; l = l->next)
      if ($<ival>2 == 0)
	push (cb_build_add (l->item, $4, 0));
      else
	push (cb_build_sub (l->item, $4, 0));
  }
;
up_or_down:
  UP				{ $<ival>$ = 0; }
| DOWN				{ $<ival>$ = 1; }
;

/* SET mnemonic-name-1 ... TO ON/OFF */

set_to_on_off_sequence:
  set_to_on_off
| set_to_on_off_sequence set_to_on_off
;
set_to_on_off:
  mnemonic_name_list TO on_or_off
  {
    struct cb_list *l;
    for (l = $1; l; l = l->next)
      {
	struct cb_system_name *s = CB_SYSTEM_NAME (cb_ref (l->item));
	push (cb_build_move ($3, cb_switch[s->token]));
      }
  }
;

/* SET condition-name-1 ... TO TRUE/FALSE */

set_to_true_false_sequence:
  set_to_true_false
| set_to_true_false_sequence set_to_true_false
;
set_to_true_false:
  data_name_list TO TOK_TRUE
  {
    struct cb_list *l;
    for (l = $1; l; l = l->next)
      {
	struct cb_field *f = cb_field (l->item);
	cb_tree value = f->values->item;
	set_value (l->item, CB_TREE (f->parent));
	if (CB_PARAMETER_P (value))
	  push (cb_build_move (CB_PARAMETER (value)->x, l->item));
	else
	  push (cb_build_move (value, l->item));
      }
  }
;


/*
 * SORT statement
 */

sort_statement:
  SORT				{ BEGIN_STATEMENT ("SORT"); }
  file_name sort_key_list sort_duplicates sort_collating
  {
    push_funcall_2 ("@sort-init", $3, $<list>4);
    $<tree>$ = $3; /* used in sort_input, sort_output */
  }
  sort_input sort_output
;
sort_key_list:
  /* empty */			{ $<list>$ = NULL; }
| sort_key_list
  _on ascending_or_descending _key data_name_list
  {
    struct cb_list *l;
    for (l = $5; l; l = l->next)
      l->item = cb_build_parameter_1 ($3, l->item);
    $<list>$ = list_append ($<list>1, $5);
  }
;
sort_duplicates:
| _with DUPLICATES _in _order	{ PENDING ("DUPLICATES"); }
;
sort_collating:
| collating_sequence		{ PENDING ("COLLATING SEQUENCE"); }
;

sort_input:
  USING file_name_list
  {
    struct cb_list *l;
    push_funcall_2 ("cob_open", $<tree>0, cb_build_integer (COB_OPEN_OUTPUT));
    for (l = $2; l; l = l->next)
      push_funcall_2 ("cob_sort_using", $<tree>0, l->item);
    push_funcall_2 ("cob_close", $<tree>0, cb_build_integer (COB_CLOSE_NORMAL));
  }
| INPUT PROCEDURE _is perform_procedure
  {
    push_funcall_2 ("cob_open", $<tree>0, cb_build_integer (COB_OPEN_OUTPUT));
    push (cb_build_perform_once ($4));
    push_funcall_2 ("cob_close", $<tree>0, cb_build_integer (COB_CLOSE_NORMAL));
  }
;

sort_output:
  GIVING file_name_list
  {
    struct cb_list *l;
    for (l = $2; l; l = l->next)
      {
	push_funcall_2 ("cob_open", $<tree>-1, cb_build_integer (COB_OPEN_INPUT));
	push_funcall_2 ("cob_sort_giving", $<tree>-1, l->item);
	push_funcall_2 ("cob_close", $<tree>-1, cb_build_integer (COB_CLOSE_NORMAL));
      }
  }
| OUTPUT PROCEDURE _is perform_procedure
  {
    push_funcall_2 ("cob_open", $<tree>-1, cb_build_integer (COB_OPEN_INPUT));
    push (cb_build_perform_once ($4));
    push_funcall_2 ("cob_close", $<tree>-1, cb_build_integer (COB_CLOSE_NORMAL));
  }
;


/*
 * START statement
 */

start_statement:
  START				{ BEGIN_STATEMENT ("START"); }
  file_name			{ $<ival>$ = COB_EQ; }
  start_key opt_invalid_key
  end_start
  {
    cb_tree file = cb_ref ($3);
    if ($<tree>5 == NULL)
      $<tree>5 = CB_FILE (file)->key;
    push_funcall_3 ("cob_start", file, cb_build_integer ($<ival>4), $<tree>5);
    push_file_handler (file, $6);
  }
;
start_key:
  /* empty */			{ $<tree>$ = NULL; }
| KEY _is start_op data_name	{ $<ival>0 = $<ival>3; $<tree>$ = $4; }
;
start_op:
  flag_not equal		{ $<ival>$ = $1 ? COB_NE : COB_EQ; }
| flag_not greater		{ $<ival>$ = $1 ? COB_LE : COB_GT; }
| flag_not less			{ $<ival>$ = $1 ? COB_GE : COB_LT; }
| flag_not greater_or_equal	{ $<ival>$ = $1 ? COB_LT : COB_GE; }
| flag_not less_or_equal	{ $<ival>$ = $1 ? COB_GT : COB_LE; }
;
end_start:
  /* empty */			{ terminator_warning (); }
| END_START
;


/*
 * STOP statement
 */

stop_statement:
  STOP RUN			{ BEGIN_STATEMENT ("STOP"); }
  {
    push_funcall_0 ("cob_stop_run");
  }
| STOP LITERAL
  {
    cb_obsolete_85 ("STOP literal");
  }
;


/*
 * STRING statement
 */

string_statement:
  STRING			{ BEGIN_STATEMENT ("STRING"); }
  string_item_list INTO data_name opt_with_pointer on_overflow
  end_string
  {
    struct cb_list *seq;
    struct cb_list *start = $<list>3;

    seq = list (cb_build_funcall_2 ("cob_string_init", $5, $6));

    while (start)
      {
	struct cb_list *l, *end;
	cb_tree dlm;

	/* find DELIMITED item */
	for (end = start; end; end = end->next)
	  if (CB_PARAMETER_P (end->item))
	    break;

	/* cob_string_delimited */
	dlm = end ? CB_PARAMETER (end->item)->x : cb_int0;
	list_add (seq, cb_build_funcall_1 ("cob_string_delimited", dlm));

	/* cob_string_append */
	for (l = start; l != end; l = l->next)
	  list_add (seq, cb_build_funcall_1 ("cob_string_append", l->item));

	start = end ? end->next : NULL;
      }

    list_add (seq, cb_build_funcall_0 ("cob_string_finish"));
    push_sequence_with_handler (make_sequence (seq), $7);
  }
;
string_item_list:
  string_item			{ $<list>$ = list ($<tree>1); }
| string_item_list string_item	{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
string_item:
  value				{ $<tree>$ = $1; }
| DELIMITED _by SIZE		{ $<tree>$ = cb_build_parameter_1 (0, cb_int0); }
| DELIMITED _by value		{ $<tree>$ = cb_build_parameter_1 (0, $3); }
;
opt_with_pointer:
  /* empty */			{ $$ = cb_int0; }
| _with POINTER data_name	{ $$ = $3; }
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
  subtract_body on_size_error
  end_subtract
  {
    push_sequence_with_handler ($<tree>3, $4);
  }
;
subtract_body:
  numeric_value_list FROM numeric_name_list
  {
    $<tree>$ = cb_build_assign ($3, '-', cb_build_connective_op ($1, '+'));
  }
| numeric_value_list FROM numeric_value GIVING numeric_edited_name_list
  {
    $<tree>$ = cb_build_assign ($5, 0, cb_build_connective_op (cons ($3, $1), '-'));
  }
| CORRESPONDING group_name FROM group_name flag_rounded
  {
    $<tree>$ = cb_build_corr (cb_build_sub, $4, $2, $5);
  }
;
end_subtract:
  /* empty */			{ terminator_warning (); }
| END_SUBTRACT
;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING			{ BEGIN_STATEMENT ("UNSTRING"); }
  data_name unstring_delimited INTO unstring_into
  opt_with_pointer unstring_tallying on_overflow
  end_unstring
  {
    struct cb_list *l = $<list>4;
    l = cons (cb_build_funcall_2 ("cob_unstring_init", $3, $7), l);
    l = list_append (l, $<list>6);
    if ($<tree>8)
      l = list_add (l, cb_build_funcall_1 ("cob_unstring_tallying", $<tree>8));
    l = list_add (l, cb_build_funcall_0 ("cob_unstring_finish"));
    push_sequence_with_handler (make_sequence (l), $9);
  }
;

unstring_delimited:
  /* empty */			{ $<list>$ = NULL; }
| DELIMITED _by
  unstring_delimited_list	{ $<list>$ = $<list>3; }
;
unstring_delimited_list:
  unstring_delimited_item	{ $<list>$ = list ($<tree>1); }
| unstring_delimited_list OR
  unstring_delimited_item	{ $<list>$ = list_add ($<list>1, $<tree>3); }
;
unstring_delimited_item:
  flag_all value
  {
    cb_tree flag = $1 ? cb_int1 : cb_int0;
    $<tree>$ = cb_build_funcall_2 ("cob_unstring_delimited", $2, flag);
  }
;

unstring_into:
  unstring_into_item		{ $<list>$ = list ($<tree>1); }
| unstring_into
  unstring_into_item		{ $<list>$ = list_add ($<list>1, $<tree>2); }
;
unstring_into_item:
  data_name unstring_delimiter unstring_count
  {
    $<tree>$ = cb_build_funcall_3 ("cob_unstring_into", $1, $<tree>2, $<tree>3);
  }
;
unstring_delimiter:
  /* empty */			{ $<tree>$ = cb_int0; }
| DELIMITER _in data_name	{ $<tree>$ = $3; }
;
unstring_count:
  /* empty */			{ $<tree>$ = cb_int0; }
| COUNT _in data_name		{ $<tree>$ = $3; }
;

unstring_tallying:
  /* empty */			{ $<tree>$ = NULL; }
| TALLYING _in data_name	{ $<tree>$ = $3; }
;

end_unstring:
  /* empty */			{ terminator_warning (); }
| END_UNSTRING
;

/*
 * USE statement
 */

use_statement:
  USE flag_global AFTER _standard exception_or_error PROCEDURE _on use_target
  {
    current_section->need_begin = 1;
    current_section->need_return = 1;
    CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
  }
;
use_target:
  file_name_list
  {
    struct cb_list *l;
    for (l = $1; l; l = l->next)
      CB_FILE (cb_ref (l->item))->handler = current_section;
  }
| INPUT	 { current_program->file_handler[COB_OPEN_INPUT]  = current_section; }
| OUTPUT { current_program->file_handler[COB_OPEN_OUTPUT] = current_section; }
| I_O	 { current_program->file_handler[COB_OPEN_I_O]    = current_section; }
| EXTEND { current_program->file_handler[COB_OPEN_EXTEND] = current_section; }
;
_standard: | STANDARD ;
exception_or_error: EXCEPTION | ERROR ;


/*
 * WRITE statement
 */

write_statement:
  WRITE				{ BEGIN_STATEMENT ("WRITE"); }
  record_name write_from write_option write_exception
  end_write
  {
    struct cb_field *f = CB_FIELD (cb_ref ($3));
    struct cb_parameter *p = $<tree>5 ? CB_PARAMETER ($<tree>5) : 0;
    cb_tree file = CB_TREE (f->file);

    /* AFTER ADVANCING */
    if (p && p->type == CB_AFTER)
      {
	if (p->x)
	  push_funcall_2 ("cob_write_lines", file, p->x);
	else
	  push_funcall_1 ("cob_write_page", file);
      }

    /* WRITE */
    if ($4)
      push (cb_build_move ($4, $3));
    push_funcall_2 ("cob_write", file, $3);
    push_file_handler (file, $<tree>6);

    /* BEFORE ADVANCING */
    if (p && p->type == CB_BEFORE)
      {
	if (p->x)
	  push_funcall_2 ("cob_write_lines", file, p->x);
	else
	  push_funcall_1 ("cob_write_page", file);
      }
  }
;
write_from:
  /* empty */			{ $$ = NULL; }
| FROM value			{ $$ = $2; }
;
write_option:
  /* empty */
  {
    $<tree>$ = NULL;
  }
| before_or_after _advancing integer_value _line_or_lines
  {
    $<tree>$ = cb_build_parameter_1 ($1, cb_build_cast_integer ($3));
  }
| before_or_after _advancing PAGE
  {
    $<tree>$ = cb_build_parameter_1 ($1, 0);
  }
;
before_or_after:
  BEFORE			{ $$ = CB_BEFORE; }
| AFTER				{ $$ = CB_AFTER; }
;
write_exception:
  /* empty */			{ $<tree>$ = NULL; }
| at_eop			{ $<tree>$ = $1; }
| invalid_key			{ $<tree>$ = $1; }
;
end_write:
  /* empty */			{ terminator_warning (); }
| END_WRITE
;


/*******************
 * Status handlers
 *******************/

/*
 * ON SIZE ERROR
 */

on_size_error:
  opt_on_size_error
  opt_not_on_size_error
  {
    if ($<tree>1 || $<tree>2)
      $$ = make_handler (COB_EC_SIZE, $<tree>1, $<tree>2);
    else
      $$ = NULL;
  }
;
opt_on_size_error:
  /* empty */				{ $<tree>$ = NULL; }
| _on SIZE ERROR statement_list		{ $<tree>$ = $4; }
;
opt_not_on_size_error:
  /* empty */				{ $<tree>$ = NULL; }
| NOT _on SIZE ERROR statement_list	{ $<tree>$ = $5; }
;


/*
 * ON OVERFLOW
 */

on_overflow:
  opt_on_overflow
  opt_not_on_overflow
  {
    if ($<tree>1 || $<tree>2)
      $$ = make_handler (COB_EC_OVERFLOW, $<tree>1, $<tree>2);
    else
      $$ = NULL;
  }
;
opt_on_overflow:
  /* empty */				{ $<tree>$ = NULL; }
| _on OVERFLOW statement_list		{ $<tree>$ = $3; }
;
opt_not_on_overflow:
  /* empty */				{ $<tree>$ = NULL; }
| NOT _on OVERFLOW statement_list	{ $<tree>$ = $4; }
;


/*
 * AT END
 */

at_end:
  at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, $<tree>1, 0);
  }
| not_at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, 0, $<tree>1);
  }
| at_end_sentence not_at_end_sentence
  {
    $$ = make_handler (COB_EC_I_O_AT_END, $<tree>1, $<tree>2);
  }
;
at_end_sentence:
  END statement_list		{ $<tree>$ = $2; }
| AT END statement_list		{ $<tree>$ = $3; }
;
not_at_end_sentence:
  NOT _at END statement_list	{ $<tree>$ = $4; }
;


/*
 * AT EOP
 */

at_eop:
  at_eop_sentence
  {
    $$ = make_handler (COB_EC_I_O_EOP, $<tree>1, 0);
  }
| not_at_eop_sentence
  {
    $$ = make_handler (COB_EC_I_O_EOP, 0, $<tree>1);
  }
| at_eop_sentence not_at_eop_sentence
  {
    $$ = make_handler (COB_EC_I_O_EOP, $<tree>1, $<tree>2);
  }
;
at_eop_sentence:
  _at EOP statement_list		{ $<tree>$ = $3; }
;
not_at_eop_sentence:
  NOT _at EOP statement_list		{ $<tree>$ = $4; }
;


/*
 * INVALID KEY
 */

opt_invalid_key:
  /* empty */			{ $$ = NULL; }
| invalid_key
;
invalid_key:
  invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, $<tree>1, 0);
  }
| not_invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, 0, $<tree>1);
  }
| invalid_key_sentence
  not_invalid_key_sentence
  {
    $$ = make_handler (COB_EC_I_O_INVALID_KEY, $<tree>1, $<tree>2);
  }
;
invalid_key_sentence:
  INVALID _key statement_list		{ $<tree>$ = $3; }
;
not_invalid_key_sentence:
  NOT INVALID _key statement_list	{ $<tree>$ = $4; }
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

numeric_expr:
  expr
  {
    if (CB_TREE_CLASS ($1) != CB_CLASS_NUMERIC)
      {
	cb_error_x ($1, _("invalid expression `%s'"), cb_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

expr:
  {
    last_operator = 0;
    last_lefthand = NULL;
  }
  expr_1			{ $$ = $2; }
;
expr_1:
  expr_item_list
  {
    int i;
    const char *class_func = NULL;
    struct cb_list *l;
    struct stack_item {
      int prio;
      int token;
      cb_tree value;
    } stack[list_length ($1)];

    static int reduce (int prio)
      {
	while (i >= 2 && stack[i-2].token != VALUE && stack[i-2].prio <= prio)
	  {
	    int token = stack[i-2].token;
	    if (stack[i-1].token != VALUE
		&& stack[i-1].token != '&'
		&& stack[i-1].token != '|')
	      return -1;
	    switch (token)
	      {
	      case '+': case '-': case '*': case '/': case '^':
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  cb_build_binary_op (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      case '!':
		if (CB_TREE_CLASS (stack[i-1].value) != CB_CLASS_BOOLEAN)
		  stack[i-1].value =
		    cb_build_binary_op (last_lefthand, last_operator, stack[i-1].value);
		stack[i-2].token = VALUE;
		stack[i-2].value = cb_build_negation (stack[i-1].value);
		i -= 1;
		break;
	      case '&':
	      case '|':
		if (i < 3 || stack[i-3].token != VALUE)
		  return -1;
		if (CB_TREE_CLASS (stack[i-1].value) != CB_CLASS_BOOLEAN)
		  stack[i-1].value =
		    cb_build_binary_op (last_lefthand, last_operator, stack[i-1].value);
		if (cb_warn_parentheses
		    && token == '|'
		    && ((CB_BINARY_OP_P (stack[i-3].value)
			 && CB_BINARY_OP (stack[i-3].value)->op == '&')
			|| (CB_BINARY_OP_P (stack[i-1].value)
			    && CB_BINARY_OP (stack[i-1].value)->op == '&')))
		  cb_warning (_("suggest parentheses around AND within OR"));
		stack[i-3].token = VALUE;
		stack[i-3].value =
		  cb_build_binary_op (stack[i-3].value, token, stack[i-1].value);
		i -= 2;
		break;
	      default:
		if (stack[i-3].token == '&' || stack[i-3].token == '|')
		  {
		    last_operator = token;
		    stack[i-2].token = VALUE;
		    stack[i-2].value =
		      cb_build_binary_op (last_lefthand, token, stack[i-1].value);
		    i -= 1;
		  }
		else
		  {
		    last_lefthand = stack[i-3].value;
		    last_operator = token;
		    stack[i-3].token = VALUE;
		    stack[i-3].value =
		      cb_build_binary_op (last_lefthand, token, stack[i-1].value);
		    i -= 2;
		  }
		break;
	      }
	  }

	/* handle special case "cmp OR x AND" */
	if (i >= 2
	    && prio == 7
	    && stack[i-2].token == '|'
	    && CB_TREE_CLASS (stack[i-1].value) != CB_CLASS_BOOLEAN)
	  {
	    stack[i-1].token = VALUE;
	    stack[i-1].value =
	      cb_build_binary_op (last_lefthand, last_operator, stack[i-1].value);
	  }
	return 0;
      }

    static int shift (int prio, int token, cb_tree value)
      {
	if (prio > 0)
	  if (reduce (prio) == -1)
	    return -1;
	stack[i].prio  = prio;
	stack[i].token = token;
	stack[i].value = value;
	i++;
	return 0;
      }

    i = 0;
    for (l = $1; l; l = l->next)
      {
#define SHIFT(prio,token,value) \
        if (shift (prio, token, value) == -1) goto error
#define look_ahead(l) \
        ((l && CB_INTEGER_P (l->item)) ? CB_INTEGER (l->item)->val : 0)

	int token = 0;
	cb_tree x = l->item;
	switch (CB_TREE_TAG (x))
	  {
	  case CB_TAG_PROPOSITION:
	    class_func = CB_PROPOSITION (x)->cname;
	    goto unary_cond;
	  case CB_TAG_INTEGER:
	    {
	      token = CB_INTEGER (x)->val;
	      switch (token)
		{
		  /* arithmetic operator */
		case '^':
		  SHIFT (2, token, 0);
		  break;
		case '*':
		case '/':
		  SHIFT (3, token, 0);
		  break;
		case '-':
		  if (i == 0 || stack[i-1].token != VALUE)
		    {
		      /* unary negative */
		      l->next->item =
			cb_build_binary_op (cb_zero, '-', l->next->item);
		      break;
		    }
		  /* fall through */
		case '+':
		  SHIFT (4, token, 0);
		  break;

		  /* conditional operator */
		case '=':
		  SHIFT (5, '=', 0);
		  break;
		case '<':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, '[', 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, '<', 0);
		  break;
		case '>':
		  if (look_ahead (l->next) == OR)
		    {
		      if (look_ahead (l->next->next) != '=')
			goto error;
		      SHIFT (5, ']', 0);
		      l = l->next->next;
		    }
		  else
		    SHIFT (5, '>', 0);
		  break;
		case LE:
		  SHIFT (5, '[', 0);
		  break;
		case GE:
		  SHIFT (5, ']', 0);
		  break;

		  /* class condition */
		case NUMERIC:
		  class_func = "cob_is_numeric";
		  goto unary_cond;
		case ALPHABETIC:
		  class_func = "cob_is_alpha";
		  goto unary_cond;
		case ALPHABETIC_LOWER:
		  class_func = "cob_is_lower";
		  goto unary_cond;
		case ALPHABETIC_UPPER:
		  class_func = "cob_is_upper";
		  goto unary_cond;

		  /* sign condition */
		case POSITIVE:
		case NEGATIVE:
		  goto unary_cond;

		unary_cond:
		  {
		    int not_flag = 0;
		    if (i > 0 && stack[i-1].token == '!')
		      {
			not_flag = 1;
			i--;
		      }
		    reduce (5);
		    if (i > 0 && stack[i-1].token == VALUE)
		      {
			int op;
			switch (token)
			  {
			  case ZERO:
			  case POSITIVE:
			  case NEGATIVE:
			    op = ((token == POSITIVE) ? '>' :
				  (token == NEGATIVE) ? '<' : '=');
			    stack[i-1].value =
			      cb_build_binary_op (stack[i-1].value, op, cb_zero);
			    break;
			  default:
			    stack[i-1].value =
			      cb_build_funcall_1 (class_func, stack[i-1].value);
			    break;
			  }
			if (not_flag)
			  stack[i-1].value = cb_build_negation (stack[i-1].value);
			break;
		      }
		    goto error;
		  }

		  /* logical operator */
		case NOT:
		  switch (look_ahead (l->next))
		    {
		    case '=': SHIFT (5, '~', 0); l = l->next; break;
		    case '<': SHIFT (5, ']', 0); l = l->next; break;
		    case '>': SHIFT (5, '[', 0); l = l->next; break;
		    case LE:  SHIFT (5, '>', 0); l = l->next; break;
		    case GE:  SHIFT (5, '<', 0); l = l->next; break;
		    default:  SHIFT (6, '!', 0); break;
		    }
		  break;
		case AND: SHIFT (7, '&', 0); break;
		case OR:  SHIFT (8, '|', 0); break;
		}
	      break;
	    }
	  default:
	    if (x == cb_zero)
	      if (stack[i-1].token == VALUE
		  || stack[i-1].token == '!')
	      {
		token = ZERO;
		goto unary_cond;
	      }
	    SHIFT (0, VALUE, x);
	  }
      }
    reduce (9); /* reduce all */

    /*
     * At end
     */
    if (i != 1)
      {
      error:
	cb_error_x ($1->item, _("invalid expression `%s'"),
		   cb_name ($1->item));
	YYERROR;
      }

    $$ = stack[0].value;
  }
;

expr_item_list:
  expr_item			{ $$ = list ($1); }
| expr_item_list IS		{ $$ = $1; }
| expr_item_list expr_item	{ $$ = list_add ($1, $2); }
;
expr_item:
  value				{ $$ = $1; }
| '(' expr_1 ')'		{ $$ = cb_build_parenthesize ($2); }
/* arithmetic operator */
| '+'				{ $$ = cb_build_integer ('+'); }
| '-'				{ $$ = cb_build_integer ('-'); }
| '*'				{ $$ = cb_build_integer ('*'); }
| '/'				{ $$ = cb_build_integer ('/'); }
| '^'				{ $$ = cb_build_integer ('^'); }
/* conditional operator */
| equal				{ $$ = cb_build_integer ('='); }
| greater			{ $$ = cb_build_integer ('>'); }
| less				{ $$ = cb_build_integer ('<'); }
| GE				{ $$ = cb_build_integer (GE); }
| LE				{ $$ = cb_build_integer (LE); }
/* class condition */
| NUMERIC			{ $$ = cb_build_integer (NUMERIC); }
| ALPHABETIC			{ $$ = cb_build_integer (ALPHABETIC); }
| ALPHABETIC_LOWER		{ $$ = cb_build_integer (ALPHABETIC_LOWER); }
| ALPHABETIC_UPPER		{ $$ = cb_build_integer (ALPHABETIC_UPPER); }
| CLASS_NAME			{ $$ = resolve_class_name ($1); }
/* sign condition */
  /* ZERO is defined in `value' */
| POSITIVE			{ $$ = cb_build_integer (POSITIVE); }
| NEGATIVE			{ $$ = cb_build_integer (NEGATIVE); }
/* logical operator */
| NOT				{ $$ = cb_build_integer (NOT); }
| AND				{ $$ = cb_build_integer (AND); }
| OR				{ $$ = cb_build_integer (OR); }
;

equal: '=' | EQUAL _to ;
greater: '>' | GREATER _than ;
less: '<' | LESS _than ;
greater_or_equal: GE | GREATER _than OR EQUAL _to ;
less_or_equal: LE | LESS _than OR EQUAL _to ;


/*******************
 * Names
 *******************/

/* Group name */

group_name:
  data_name
  {
    VALIDATE ($$, $1, _("group identifier is expected `%s'"),
	      (cb_field ($1)->children
	       && CB_REFERENCE ($1)->offset == NULL));
  }
;

/* Record name */

record_name:
  data_name
  {
    VALIDATE ($$, $1, _("record name is expected `%s'"),
	      cb_field ($1)->file != NULL);
  }
;

/* Numeric name (with ROUNDED) */

numeric_name_list:
  numeric_name			{ $$ = list ($1); }
| numeric_name_list
  numeric_name			{ $$ = list_add ($1, $2); }
;
numeric_name:
  data_name flag_rounded
  {
    VALIDATE_2 ($$, $1, $2, _("numeric identifier is expected `%s'"),
		(CB_TREE_CLASS ($1) == CB_CLASS_NUMERIC));
  }
;

/* Numeric-edited name (with ROUNDED) */

numeric_edited_name_list:
  numeric_edited_name		{ $$ = list ($1); }
| numeric_edited_name_list
  numeric_edited_name		{ $$ = list_add ($1, $2); }
;
numeric_edited_name:
  data_name flag_rounded
  {
    VALIDATE_2 ($$, $1, $2, _("numeric or numeric-edited identifier is expected `%s'"),
		CB_TREE_CATEGORY ($1) == CB_CATEGORY_NUMERIC
		|| CB_TREE_CATEGORY ($1) == CB_CATEGORY_NUMERIC_EDITED);
  }
;

/* Integer name */

integer_name:
  data_name
  {
    VALIDATE ($$, $1, _("integer identifier is expected `%s'"),
	      CB_TREE_CLASS ($1) == CB_CLASS_NUMERIC
	      && cb_field ($1)->pic->expt >= 0);
  }
;

/* Data name */

data_name_list:
  data_name			{ $$ = list ($1); }
| data_name_list data_name	{ $$ = list_add ($1, $2); }
;
data_name:
  value
  {
    VALIDATE ($$, $1, _("identifier is expected `%s'"),
	      (CB_REFERENCE_P ($1)
	       && CB_FIELD_P (CB_REFERENCE ($1)->value)));
  }
;

/* Table name */

table_name:
  qualified_word
  {
    if (resolve_data_name ($1) == cb_error_node)
      YYERROR;
    else
      {
	cb_tree x = cb_ref ($1);
	if (!CB_FIELD (x)->index_list)
	  {
	    cb_error_x ($1, _("`%s' not indexed"), cb_name ($1));
	    cb_error_x (x, _("`%s' defined here"), cb_name (x));
	    YYERROR;
	  }
      }
    $$ = $1;
  }
;

/* File name */

file_name_list:
  file_name			{ $$ = list ($1); }
| file_name_list file_name	{ $$ = list_add ($1, $2); }
;
file_name:
  qualified_word
  {
    resolve_file_name ($1);
    $$ = $1;
  }
;

/* Mnemonic name */

mnemonic_name_list:
  mnemonic_name			{ $$ = list ($1); }
| mnemonic_name_list
  mnemonic_name			{ $$ = list_add ($1, $2); }
;
mnemonic_name:
  MNEMONIC_NAME			{ $$ = resolve_mnemonic_name ($1); }
;

/* Label name */

procedure_name_list:
  /* empty */			{ $$ = NULL; }
| procedure_name_list 
  label				{ $$ = list_add ($1, $2); }
;
label:
  label_name
  {
    $$ = $1;
    CB_REFERENCE ($$)->offset = CB_TREE (current_section);
    current_program->label_list = cons ($$, current_program->label_list);
  }
;
label_name:
  qualified_word
| integer_label
| integer_label in_of integer_label
;
integer_label:
  LITERAL
  {
    $$ = make_reference (CB_LITERAL ($1)->data);
    $$->source_file = $1->source_file;
    $$->source_line = $1->source_line;
  }
;

/* Reference */

reference_list:
  reference			{ $$ = list ($1); }
| reference_list reference	{ $$ = list_add ($1, $2); }
;
reference:
  qualified_word
  {
    $$ = $1;
    current_program->reference_list = cons ($$, current_program->reference_list);
  }
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
 * Values
 *******************/

/* Alphanumeric value */

alphanumeric_value:
  value
  {
    VALIDATE ($$, $1, _("alphanumeric value is expected `%s'"),
	      CB_TREE_CLASS ($1) == CB_CLASS_ALPHABETIC
	      || CB_TREE_CLASS ($1) == CB_CLASS_ALPHANUMERIC);
  }
;

/* Numeric value */

numeric_value_list:
  numeric_value			{ $$ = list ($1); }
| numeric_value_list
  numeric_value			{ $$ = list_add ($1, $2); }
;
numeric_value:
  value
  {
    VALIDATE ($$, $1, _("numeric value is expected `%s'"),
	      CB_TREE_CLASS ($1) == CB_CLASS_NUMERIC);
  }
;

/* Integer value */

integer:
  LITERAL
  {
    $$ = cb_literal_to_int (CB_LITERAL ($1));
  }
;

integer_value:
  value
  {
    if (CB_TREE_CLASS ($1) != CB_CLASS_NUMERIC)
      goto invalid;

    switch (CB_TREE_TAG ($1))
      {
      case CB_TAG_CONST:
	{
	  if ($1 != cb_zero)
	    goto invalid;
	  break;
	}
      case CB_TAG_LITERAL:
	{
	  struct cb_literal *l = CB_LITERAL ($1);
	  if (l->sign < 0 || l->expt < 0)
	    goto invalid;
	  break;
	}
      case CB_TAG_REFERENCE:
	{
	  struct cb_field *f = CB_FIELD (cb_ref ($1));
	  if (f->pic->expt < 0)
	    goto invalid;
	  break;
	}
      default:
      invalid:
	cb_error_x ($1, _("`%s' must be an integer value"), cb_name ($1));
	YYERROR;
      }
    $$ = $1;
  }
;

as_literal:
  /* empty */			{ $$ = NULL; }
| AS LITERAL			{ $$ = $2; }
;

/*******************
 * Primitive elements
 *******************/

/*
 * Primitive value
 */

opt_value_list:
  /* empty */			{ $$ = NULL; }
| value_list			{ $$ = $1; }
;
value_list:
  value				{ $$ = list ($1); }
| value_list value		{ $$ = list_add ($1, $2); }
;
value:
  identifier
| literal
| function
;

non_all_value:
  identifier
| basic_literal
;

/*
 * Identifier
 */

identifier:
  identifier_1
  {
    $$ = resolve_data_name ($<tree>1);
    if ($$ != cb_error_node)
      validate_identifier ($$);
  }
;
identifier_1:
  qualified_word		{ $<tree>$ = $1; }
| qualified_word subref		{ $<tree>$ = $1; }
| qualified_word refmod		{ $<tree>$ = $1; }
| qualified_word subref refmod	{ $<tree>$ = $1; }
;
qualified_word:
  WORD				{ $$ = $1; }
| WORD in_of qualified_word
  {
    $$ = $1;
    CB_REFERENCE ($1)->next = CB_REFERENCE ($3);
  }
;
subref:
  '(' subscript_list ')'
  {
    CB_REFERENCE ($<tree>0)->subs = $<list>2;
    $<tree>$ = $<tree>0;
  }
;
refmod:
  '(' subscript ':' ')'
  {
    CB_REFERENCE ($<tree>0)->offset = $2;
  }
| '(' subscript ':' subscript ')'
  {
    CB_REFERENCE ($<tree>0)->offset = $2;
    CB_REFERENCE ($<tree>0)->length = $4;
  }
;
subscript_list:
  subscript			{ $<list>$ = list ($1); }
| subscript_list subscript	{ $<list>$ = list_add ($<list>1, $2); }
;
subscript:
  integer_value			{ $$ = $1; }
| subscript '+' integer_value	{ $$ = cb_build_binary_op ($1, '+', $3); }
| subscript '-' integer_value	{ $$ = cb_build_binary_op ($1, '-', $3); }
;

/*
 * Literal
 */

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
  LITERAL			{ $$ = $1; }
| SPACE				{ $$ = cb_space; }
| ZERO				{ $$ = cb_zero; }
| QUOTE				{ $$ = cb_quote; }
| HIGH_VALUE			{ $$ = cb_high; }
| LOW_VALUE			{ $$ = cb_low; }
;

/*
 * Function
 */

function:
  FUNCTION_NAME '(' opt_value_list ')'
  {
    PENDING ("FUNCTION");
    YYABORT;
  }
;


/*******************
 * Common rules
 *******************/

/*
 * Common flags
 */

flag_all:
  /* empty */			{ $$ = 0; }
| ALL				{ $$ = 1; }
;
flag_duplicates:
  /* empty */			{ $$ = 0; }
| _with DUPLICATES		{ $$ = 1; }
;
flag_not:
  /* empty */			{ $$ = 0; }
| NOT				{ $$ = 1; }
;
flag_next:
  /* empty */			{ $$ = 0; }
| NEXT				{ $$ = 1; }
;
flag_global:
  /* empty */			{ $$ = 0; }
| GLOBAL			{ $$ = 1; }
;
flag_optional:
  /* empty */			{ $$ = 0; }
| OPTIONAL			{ $$ = 1; }
;
flag_rounded:
  /* empty */			{ $$ = 0; }
| ROUNDED			{ $$ = 1; }
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
_file:		| TOK_FILE ;
_for:		| FOR ;
_from:		| FROM ;
_in:		| IN ;
_is:		| IS ;
_is_are:	| IS | ARE ;
_key:		| KEY ;
_lines:		| LINES ;
_line_or_lines:	| LINE | LINES ;
_mode:		| MODE ;
_number:	| NUMBER ;
_of:		| OF ;
_on:		| ON ;
_order:		| ORDER ;
_other:		| OTHER ;
_program:	| PROGRAM ;
_record:	| RECORD ;
_sign:		| SIGN ;
_sign_is:	| SIGN _is ;
_size:		| SIZE ;
_status:	| STATUS ;
_tape:		| TAPE ;
_than:		| THAN ;
_then:		| THEN ;
_to:		| TO ;
_upon:		| UPON ;
_when:		| WHEN ;
_with:		| WITH ;
_records_or_characters: | RECORDS | CHARACTERS ;


%%

static cb_tree
build_file_handler (cb_tree file, cb_tree handler)
{
  if (handler == NULL)
    handler = make_handler (0, 0, 0);
  CB_FUNCALL (handler)->argv[3] = CB_FILE (file)->handler;
  return handler;
}


static void
terminator_warning (void)
{
  if (cb_warn_implicit_terminator && current_statement->need_terminator)
    cb_warning_x (CB_TREE (current_statement),
		  _("%s statement not terminated by END-%s"),
		  current_statement->name, current_statement->name);
}
