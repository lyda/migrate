/*
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

%expect 468

%{
#define yydebug		cob_trace_parser
#define YYDEBUG		COB_DEBUG
#define YYERROR_VERBOSE 1

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "codegen.h"
#include "_libcob.h"

#define OBSOLETE(x)	yywarn ("keyword `%s' is obsolete", x)

#define ASSERT_VALID_EXPR(x)			\
  do {						\
    if (!is_valid_expr (x))			\
      yyerror ("invalid expr");			\
  } while (0);

static unsigned long lbend, lbstart;
static unsigned int perform_after_sw;

static cob_tree current_section, current_paragraph;
static int current_call_mode;
static cob_tree inspect_name;

static int warning_count = 0;
static int error_count = 0;

static cob_tree make_opt_cond (cob_tree last, int type, cob_tree this);
%}

%union {
  cob_tree tree;
  cob_tree_list list;
  int ival;
  char *str;
  struct cob_picture *pic;
  struct inspect_item *insi;
  struct call_parameter *para;
  struct perf_info *pfval;
  struct perform_info *pfvals;
  struct math_var *mval;      /* math variables container list */
}

%left  '+', '-'
%left  '*', '/'
%left  '^'
%left  OR
%left  AND
%right NOT
%right OF

%token <str>  ID_TOK
%token <pic>  PICTURE_TOK
%token <tree> INTEGER_LITERAL,NUMERIC_LITERAL,NONNUMERIC_LITERAL,SYMBOL_TOK
%token <tree> SPECIAL_TOK,CLASS_TOK,VARIABLE,VARCOND,SUBSCVAR

%token EQUAL,GREATER,LESS,GE,LE,COMMAND_LINE,ENVIRONMENT_VARIABLE,ALPHABET
%token DATE,DAY,DAY_OF_WEEK,TIME,INKEY,READ,WRITE,OBJECT_COMPUTER,INPUT_OUTPUT
%token TO,FOR,IS,ARE,THRU,THAN,NO,CANCEL,ASCENDING,DESCENDING,ZEROS,PORT
%token SOURCE_COMPUTER,BEFORE,AFTER,SCREEN,REVERSE_VIDEO,NUMBER,PLUS,MINUS
%token FOREGROUND_COLOR,BACKGROUND_COLOR,UNDERLINE,HIGHLIGHT,LOWLIGHT,SEPARATE
%token RIGHT,AUTO,REQUIRED,FULL,JUSTIFIED,BLINK,SECURE,BELL,COLUMN,SYNCHRONIZED
%token TOK_INITIAL,FIRST,ALL,LEADING,OF,IN,BY,STRING,UNSTRING,DEBUGGING
%token START,DELETE,PROGRAM,GLOBAL,EXTERNAL,SIZE,DELIMITED,COLLATING,SEQUENCE
%token GIVING,ERASE,INSPECT,TALLYING,REPLACING,ON,OFF,POINTER,OVERFLOW,NATIVE
%token DELIMITER,COUNT,LEFT,TRAILING,CHARACTER,FILLER,OCCURS,TIMES,CLASS
%token ADD,SUBTRACT,MULTIPLY,DIVIDE,ROUNDED,REMAINDER,ERROR,SIZE,INDEX
%token FD,SD,REDEFINES,PICTURE,FILEN,USAGE,BLANK,SIGN,VALUE,MOVE,LABEL
%token PROGRAM_ID,DIVISION,CONFIGURATION,SPECIAL_NAMES,MEMORY,ALTER
%token FILE_CONTROL,I_O_CONTROL,FROM,UPDATE,SAME,AREA,EXCEPTION,UNTIL
%token WORKING_STORAGE,LINKAGE,DECIMAL_POINT,COMMA,DUPLICATES,WITH,EXIT
%token RECORD,OMITTED,STANDARD,STANDARD_1,STANDARD_2,RECORDS,BLOCK,VARYING
%token CONTAINS,CHARACTERS,COMPUTE,GO,STOP,RUN,ACCEPT,PERFORM,RENAMES
%token IF,ELSE,SENTENCE,LINE,PAGE,OPEN,CLOSE,REWRITE,SECTION,SYMBOLIC
%token ADVANCING,INTO,AT,END,NEGATIVE,POSITIVE,SPACES,NOT
%token CALL,USING,INVALID,CONTENT,QUOTES,LOW_VALUES,HIGH_VALUES
%token SELECT,ASSIGN,DISPLAY,UPON,CONSOLE
%token ORGANIZATION,ACCESS,MODE,KEY,STATUS,ALTERNATE,SORT,SORT_MERGE
%token SEQUENTIAL,INDEXED,DYNAMIC,RANDOM,RELATIVE,RELEASE
%token SET,UP,DOWN,TRACE,READY,RESET,SEARCH,WHEN,TEST
%token END_ADD,END_CALL,END_COMPUTE,END_DELETE,END_DIVIDE,END_EVALUATE
%token END_IF,END_MULTIPLY,END_PERFORM,END_READ,END_REWRITE,END_SEARCH
%token END_START,END_STRING,END_SUBTRACT,END_UNSTRING,END_WRITE
%token THEN,EVALUATE,OTHER,ALSO,CONTINUE,CURRENCY,REFERENCE,INITIALIZE
%token NUMERIC,ALPHABETIC,ALPHABETIC_LOWER,ALPHABETIC_UPPER
%token RETURNING,TRUE,FALSE,ANY,SUBSCVAR,FUNCTION,OPTIONAL
%token REPORT,RD,CONTROL,LIMIT,FINAL,HEADING,FOOTING,LAST,DETAIL,SUM
%token POSITION,FILE_ID,DEPENDING,TYPE,SOURCE,CORRESPONDING,CONVERTING
%token INITIATE,GENERATE,TERMINATE,NOECHO,LPAR,BUILTIN_TOK
%token IDENTIFICATION,ENVIRONMENT,DATA,PROCEDURE
%token AUTHOR,DATE_WRITTEN,DATE_COMPILED,INSTALLATION,SECURITY
%token COMMON,RETURN,END_RETURN,PREVIOUS,NEXT,PACKED_DECIMAL
%token INPUT,I_O,OUTPUT,EXTEND,EOL,EOS,BINARY,FLOAT_SHORT,FLOAT_LONG
%token ALPHANUMERIC,ALPHANUMERIC_EDITED,NUMERIC_EDITED,NATIONAL,NATIONAL_EDITED

%type <ival> if_then,search_body,search_all_body,class
%type <ival> search_when,search_when_list,search_opt_at_end
%type <ival> integer,operator,before_after,set_mode
%type <ival> opt_on_exception_or_overflow,opt_on_exception_sentence
%type <ival> opt_with_no_advancing
%type <ival> flag_all,opt_with_duplicates,opt_with_test,opt_optional
%type <ival> flag_not
%type <ival> flag_rounded,opt_sign_separate
%type <ival> organization_options,access_options,open_mode,call_mode
%type <ival> opt_read_next,usage
%type <ival> procedure_using,sort_direction,write_options
%type <ival> opt_on_size_error_sentence,opt_on_overflow_sentence
%type <ival> on_xxx_statement_list
%type <ival> at_end_sentence,invalid_key_sentence
%type <list> label_list,subscript_list,number_list,variable_list
%type <list> conditional_variable_list
%type <list> evaluate_subject_list,evaluate_when_list,evaluate_object_list
%type <para> call_using,call_parameter,call_parameter_list
%type <mval> numeric_variable_list,numeric_edited_variable_list
%type <pfval> perform_after
%type <pfvals> opt_perform_after
%type <list> sort_file_list,display_list
%type <str> idstring
%type <tree> field_description,label,label_name,filename
%type <tree> file_description,redefines_var,function_call,subscript
%type <tree> name,gname,number,file,level1_variable,opt_def_name,def_name
%type <tree> opt_read_into,opt_write_from,field_name,expr,unsafe_expr
%type <tree> opt_unstring_count,opt_unstring_delimiter,opt_unstring_tallying
%type <tree> numeric_variable,group_variable,numeric_edited_variable
%type <tree> qualified_var,unqualified_var,evaluate_subject
%type <tree> evaluate_object,evaluate_object_1,assign_clause
%type <tree> call_returning,var_or_lit,opt_add_to
%type <tree> opt_perform_thru
%type <tree> opt_read_key,file_name,opt_with_pointer
%type <tree> variable,sort_range,name_or_literal
%type <tree> indexed_variable,search_opt_varying,opt_key_is
%type <tree> from_rec_varying,to_rec_varying
%type <tree> literal,gliteral,without_all_literal,all_literal,special_literal
%type <tree> nliteral,conditional_variable
%type <tree> condition_1,condition_2,comparative_condition,class_condition
%type <tree> search_all_when_conditional
%type <list> inspect_tallying,inspect_replacing,inspect_converting
%type <list> tallying_list,replacing_list,inspect_before_after_list
%type <insi> tallying_item,replacing_item,inspect_before_after
%type <list> string_list,string_delimited_list,string_name_list
%type <list> unstring_delimited,unstring_delimited_list,unstring_into
%type <list> unstring_delimited_item,unstring_into_item


%%
/*****************************************************************************
 * COBOL program sequence
 *****************************************************************************/

top:
  program_sequence		{ if (error_count) YYABORT; }
;
program_sequence:
  program
| program_sequence program
;
program:
  identification_division
  environment_division
  data_division
  procedure_division
  opt_end_program
;
opt_end_program:
| END PROGRAM idstring dot
;


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

identification_division:
  IDENTIFICATION DIVISION dot
  PROGRAM_ID '.' idstring opt_program_parameter dot
  identification_division_options
  {
    init_program ($6);
  }
;
opt_program_parameter:
| opt_is TOK_INITIAL opt_program { yywarn ("INITIAL is not supported yet"); }
| opt_is COMMON opt_program	 { yywarn ("COMMON is not supported yet"); }
;
identification_division_options:
| identification_division_options identification_division_option
;
identification_division_option:
  AUTHOR '.' comment		{ OBSOLETE ("AUTHOR"); }
| DATE_WRITTEN '.' comment	{ OBSOLETE ("DATE-WRITTEN"); }
| DATE_COMPILED '.' comment	{ OBSOLETE ("DATE-COMPILED"); }
| INSTALLATION '.' comment	{ OBSOLETE ("INSTALLATION"); }
| SECURITY '.' comment		{ OBSOLETE ("SECURITY"); }
;
comment: { start_condition = START_COMMENT; };


/*****************************************************************************
 * ENVIRONMENT DIVISION.
 *****************************************************************************/

environment_division:
| ENVIRONMENT DIVISION dot	{ curr_division = CDIV_ENVIR; }
  configuration_section
  input_output_section
;


/*******************
 * CONFICURATION SECTION
 *******************/

configuration_section:
| CONFIGURATION SECTION dot
  configuration_list
;
configuration_list:
| configuration_list configuration
;
configuration:
  source_computer
| object_computer
| special_names
;


/*
 * SOURCE COMPUTER
 */

source_computer:
  SOURCE_COMPUTER '.' idstring opt_with_debugging_mode dot
;
opt_with_debugging_mode:
| opt_with DEBUGGING MODE	{ yywarn ("DEBUGGING MODE is ignored"); }
;


/*
 * OBJECT COMPUTER
 */

object_computer:
  OBJECT_COMPUTER '.' idstring object_computer_options dot
;
object_computer_options:
| object_computer_options object_computer_option
;
object_computer_option:
  opt_program opt_collating SEQUENCE opt_is SYMBOL_TOK
| MEMORY SIZE opt_is integer CHARACTERS { OBSOLETE ("MEMORY SIZE"); }
;
opt_collating: | COLLATING ;


/*
 * SPECIAL-NAMES
 */

special_names:
  SPECIAL_NAMES '.' opt_special_names
;
opt_special_names:
| special_names dot
;
special_names:
  special_name
| special_names special_name
;
special_name:
  special_name_builtin
| special_name_alphabet
| special_name_symbolic
| special_name_class
| special_name_currency
| special_name_decimal_point
| special_name_console
;


/* Buildin name */

special_name_builtin:
  BUILTIN_TOK opt_is SYMBOL_TOK
| BUILTIN_TOK opt_is SYMBOL_TOK on_off_names { COB_FIELD_TYPE ($3) = '!'; }
| BUILTIN_TOK on_off_names
;
on_off_names:
  on_status_is_name
| on_status_is_name off_status_is_name
| off_status_is_name
| off_status_is_name on_status_is_name
;
on_status_is_name:
  ON opt_status opt_is SYMBOL_TOK	{ COB_FIELD_TYPE ($4) = '8'; }
;
off_status_is_name:
  OFF opt_status opt_is SYMBOL_TOK	{ COB_FIELD_TYPE ($4) = '8'; }
;


/* ALPHABET */

special_name_alphabet:
  ALPHABET SYMBOL_TOK opt_is alphabet_name
  {
    yywarn ("ALPHABET name is ignored");
  }
;
alphabet_name:
  STANDARD_1
| STANDARD_2
| NATIVE
| SYMBOL_TOK { }
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
  ALSO without_all_literal
| also_literal_list ALSO without_all_literal
;

/* SYMBOLIC CHARACTER */

special_name_symbolic:
  SYMBOLIC opt_characters symbolic_characters_list
  {
    yywarn ("SYMBOLIC CHARACTERS is ignored");
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
  SYMBOL_TOK { }
| char_list SYMBOL_TOK { }
;
integer_list:
  integer { }
| integer_list integer { }
;
is_are: IS | ARE ;


/* CLASS */

special_name_class:
  CLASS SYMBOL_TOK opt_is special_name_class_options
  {
    COB_FIELD_TYPE ($2) = '@';
    yywarn ("CLASS name is ignored");
  }
;
special_name_class_options:
  special_name_class_option
| special_name_class_options special_name_class_option
;
special_name_class_option:
  NONNUMERIC_LITERAL opt_through_literal { }
;
opt_through_literal:
| THRU NONNUMERIC_LITERAL
;


/* CURRENCY */

special_name_currency:
  CURRENCY opt_sign opt_is NONNUMERIC_LITERAL
  {
    currency_symbol = COB_FIELD_NAME ($4)[0];
  }
;


/* DECIMAL_POINT */

special_name_decimal_point:
  DECIMAL_POINT opt_is COMMA	{ decimal_comma = 1; }
;


/* CONSOLE */

special_name_console:
  CONSOLE opt_is CONSOLE	{ yywarn ("CONSOLE name is ignored"); }
;


/*******************
 * INPUT-OUTPUT SECTION
 *******************/

input_output_section:
| INPUT_OUTPUT SECTION dot
  file_control
  i_o_control
;


/*
 * FILE-CONTROL
 */

file_control:
| FILE_CONTROL dot select_statement_list
;
select_statement_list:
| select_statement_list select_statement
;
select_statement:
  SELECT opt_optional def_name ASSIGN opt_to
  {
    COB_FIELD_TYPE ($3)='F';   /* mark as file variable */
    curr_file=$3;
    $3->pic=0;  /* suppose not indexed yet */
    $3->defined=1;
    $3->parent=NULL; /* assume no STATUS yet
			this is "file status" var in files */
    $3->organization = 2;
    $3->access_mode = 1;
    $3->times=-1;
    /*$3->filenamevar=$6;*/ /* this is the variable w/filename */
    $3->alternate=NULL; /* reset alternate key list */
    $3->flags.optional=$2; /* according to keyword */
  }
  assign_clause
  {
    $3->filenamevar=$7; /* this is the variable w/filename */
  }
  select_clauses '.'
  {
    if ((curr_file->organization==ORG_INDEXED) && !(curr_file->ix_desc)) {
      yyerror("indexed file must have a record key");
    }
  }
;
assign_clause:
  PORT { yywarn ("assign target is ignored"); $$=NULL; }
| filename { $$=$1; }
| PORT filename { $$=$2; }
| EXTERNAL filename
  {
    curr_file->access_mode = curr_file->access_mode + 5;
    $$=$2;
  }
| error  { yyerror("Invalid ASSIGN clause in SELECT statement"); }
;
select_clauses:
| select_clauses select_clause
;
select_clause:
  ORGANIZATION opt_is organization_options { curr_file->organization=$3; }
| ACCESS opt_mode opt_is access_options
  {
    if (curr_file->access_mode < 5)
      curr_file->access_mode=$4;
    else
      curr_file->access_mode = $4 + 5;
  }
| FILEN STATUS opt_is SYMBOL_TOK { curr_file->parent=$4; }
| RECORD KEY opt_is SYMBOL_TOK { curr_file->ix_desc=$4; }
| RELATIVE KEY opt_is SYMBOL_TOK { curr_file->ix_desc=$4; }
| ALTERNATE RECORD KEY opt_is SYMBOL_TOK opt_with_duplicates
  { add_alternate_key($5,$6); }
| error         { yyerror("invalid clause in select"); }
;
opt_with_duplicates:
  /* nothing */			{ $$ = 0; }
| WITH DUPLICATES		{ $$ = 1; }
;
opt_optional:
  /* nothing */			{ $$ = 0; }
| OPTIONAL			{ $$ = 1; }
;
organization_options:
  INDEXED			{ $$ = 1; }
| SEQUENTIAL			{ $$ = 2; }
| RELATIVE			{ $$ = 3; }
| LINE SEQUENTIAL		{ $$ = 4; }
;
access_options:
  SEQUENTIAL			{ $$ = 1; }
| DYNAMIC			{ $$ = 2; }
| RANDOM			{ $$ = 3; }
;


/*
 * I-O-CONTROL
 */

i_o_control:
| I_O_CONTROL dot same_statement_list
;
same_statement_list:
| same_statement_list same_statement
;
same_statement:
  SAME i_o_control_param opt_area opt_for filename_list '.'
  {
    yywarn ("I-O-CONTROL is not supported yet");
  }
;
i_o_control_param:
  RECORD
| SORT
| SORT_MERGE
;
filename_list:
  variable { }
| filename_list variable { }
;


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

data_division:
| DATA DIVISION dot { curr_division = CDIV_DATA; }
  file_section
  working_storage_section
  linkage_section
  report_section
  {
    data_trail();
  }
;


/*******************
 * FILE SECTION
 *******************/

file_section:
| FILEN SECTION dot	{ curr_field=NULL; }
  fd_list		{ close_fields(); }
;
fd_list:
| fd_list
  FD file_name file_attrib '.'
  {
    curr_field=NULL;
    if ($3->filenamevar == NULL)
      yyerror("External file name not defined for file %s",
	      COB_FIELD_NAME ($3));
  }
  file_description
  {
    close_fields();
    alloc_file_entry($3);
    gen_fdesc($3,$7);
  }
| fd_list
  SD file_name sort_attrib '.'
  {
    $3->organization=2;
    curr_field=NULL;
  }
  file_description
  {
    close_fields();
    alloc_file_entry($3);
    gen_fdesc($3,$7);
  }
;
file_name:
  { curr_division = CDIV_INITIAL; }
  file
  { curr_division = CDIV_DATA; $$ = $2; }
;
file_description:
  field_description       { $$=$1; }
| file_description field_description
  {
    if (($2 != NULL) && ($2->level == 1))
      {
	/* multiple 01 records (for a file descriptor) */
	$2->redefines=$1;
	$$=$2;
      }
    else
      $$=$1;
  }
;
file_attrib:
| file_attrib REPORT opt_is SYMBOL_TOK { yyerror ("REPORT is not supported"); }
| file_attrib opt_is GLOBAL     { COB_FIELD_TYPE ($<tree>0) = 'J'; }
| file_attrib opt_is EXTERNAL   { COB_FIELD_TYPE ($<tree>0) = 'K'; }
| file_attrib LABEL rec_or_recs opt_is_are std_or_omitt
| file_attrib BLOCK opt_contains integer opt_to_integer chars_or_recs
| file_attrib DATA rec_or_recs  opt_is_are var_strings
| file_attrib VALUE OF FILE_ID opt_is filename
  {
    if ($<tree>-1->filenamevar != NULL) {
      yyerror("Re-defining file name defined in SELECT statement");
    }
    else {
      $<tree>-1->filenamevar = $<tree>6;
    }
  }
| file_attrib RECORD opt_contains integer opt_characters
  {
    yywarn ("RECORD CONTAINS is ignored");
  }
| file_attrib RECORD opt_is VARYING opt_in_size
  from_rec_varying to_rec_varying opt_characters
  DEPENDING opt_on SYMBOL_TOK
  {
    set_rec_varying_info ($<tree>-1, $6, $7, $11);
  }
;
var_strings:
  SYMBOL_TOK { }
| var_strings SYMBOL_TOK { }
;
opt_to_integer:
| TO integer
;
from_rec_varying:
  /* nothing */ { $$ = NULL; }
| FROM nliteral { $$ = $2; }
;
to_rec_varying:
  /* nothing */ { $$ = NULL; }
| TO nliteral   { $$ = $2; }
;
sort_attrib:
| sort_attrib DATA rec_or_recs  opt_is_are var_strings
| sort_attrib RECORD opt_is VARYING opt_in_size
  from_rec_varying to_rec_varying opt_characters
  DEPENDING opt_on SYMBOL_TOK
  {
    set_rec_varying_info( $<tree>-1,$6,$7,$11 );
  }
;
rec_or_recs: RECORD | RECORDS ;
std_or_omitt: STANDARD | OMITTED ;
opt_contains: | CONTAINS ;
opt_characters: | CHARACTERS ;
chars_or_recs: CHARACTERS | RECORDS ;


/*******************
 * WORKING-STRAGE SECTION
 *******************/

working_storage_section:
| WORKING_STORAGE SECTION dot	{ curr_field = NULL; }
  field_description_list	{ close_fields (); }
;
field_description_list:
| field_description_list field_description
;
field_description:
  integer field_name		{ define_field ($1, $2); }
  field_options dot
  {
    update_field ();
    $$ = $2;
  }
;
field_name:
  /* nothing */			{ $$ = make_filler (); }
| FILLER			{ $$ = make_filler (); }
| SYMBOL_TOK
  {
    if ($1->defined)
      yyerror ("variable already defined: %s", COB_FIELD_NAME ($1));
    $1->defined=1;
    $$ = $1;
  }
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
  REDEFINES			{ curr_division = CDIV_INITIAL; }
  redefines_var
  {
    curr_division = CDIV_DATA;
    curr_field->redefines = lookup_for_redefines ($3);
  }
redefines_var:
  VARIABLE			{ $$ = $1; }
| SUBSCVAR			{ $$ = $1; }
;


/* EXTERNAL */

external_clause:
  opt_is EXTERNAL		{ save_named_sect (curr_field); }
;


/* GLOBAL */

global_clause:
  opt_is GLOBAL			{ yywarn ("GLOBAL is not supported"); }
;


/* PICTURE */

picture_clause:
  PICTURE { start_condition = START_PICTURE; } PICTURE_TOK
  {
    COB_FIELD_TYPE (curr_field) = $3->type;
    curr_field->picstr   = $3->str;
    curr_field->len      = $3->size;
    curr_field->decimals = $3->decimals;
  }
;


/* USAGE */

usage_clause:
  opt_usage opt_is usage
;
usage:
  DISPLAY
  {
    /* do nothing */
  }
| BINARY /* or COMP, COMP-5 */
  {
    int len = curr_field->len;
    COB_FIELD_TYPE (curr_field) = 'B';
    curr_field->len = (len <= 2) ? 1 : (len <= 4) ? 2 : (len <= 9) ? 4 : 8;
  }
| INDEX
  {
    COB_FIELD_TYPE (curr_field) = 'B';
    curr_field->len      = 4;
    curr_field->decimals = 0;
    curr_field->picstr   = "9\x04";
  }
| FLOAT_SHORT /* or COMP-1 */
  {
    COB_FIELD_TYPE (curr_field) = 'U';
    curr_field->len      =  4;
    curr_field->decimals =  7;
    curr_field->sign     =  1;
    curr_field->picstr   = "S\x01\x39\x07\x56\x01\x39\x07";
  }
| FLOAT_LONG /* or COMP-2 */
  {
    COB_FIELD_TYPE (curr_field) = 'U';
    curr_field->len      =  8;
    curr_field->decimals = 15;
    curr_field->sign     =  1;
    curr_field->picstr   = "S\x01\x39\x0f\x56\x01\x39\x0f";
  }
| PACKED_DECIMAL /* or COMP-3 */
  {
    COB_FIELD_TYPE (curr_field) = 'C';
  }
;
opt_usage: | USAGE ;


/* SIGN */

sign_clause:
  opt_sign_is LEADING opt_sign_separate
  {
    curr_field->flags.separate_sign = $3;
    curr_field->flags.leading_sign  = 1;
  }
| opt_sign_is TRAILING opt_sign_separate
  {
    curr_field->flags.separate_sign = $3;
    curr_field->flags.leading_sign  = 0;
  }
;
opt_sign_is:
| SIGN opt_is
;
opt_sign_separate:
  /* nothing */			{ $$ = 0; }
| SEPARATE opt_character	{ $$ = 1; }
;
opt_character: | CHARACTER ;


/* OCCURS */

occurs_clause:
  OCCURS integer opt_times { curr_field->times = $2; }
  opt_indexed_by
| OCCURS integer TO integer opt_times DEPENDING opt_on
  { curr_division = CDIV_INITIAL; }
  gname
  {
    curr_division = CDIV_DATA;
    curr_field->times = $4;
    curr_field->occurs = malloc (sizeof (struct occurs));
    curr_field->occurs->min = $2;
    curr_field->occurs->max = $4;
    curr_field->occurs->depend = $9;
  }
  opt_indexed_by
;
opt_indexed_by:
| opt_key_is INDEXED opt_by index_name_list { }
;
opt_key_is:
  /* nothing */				{ $$ = NULL; }
| ASCENDING opt_key opt_is SYMBOL_TOK	{ $4->level = -1; $$ = $4; }
| DESCENDING opt_key opt_is SYMBOL_TOK	{ $4->level = -2; $$ = $4; }
;
index_name_list:
  def_name { define_implicit_field ($1, $<tree>-2); }
| index_name_list
  def_name { define_implicit_field ($2, $<tree>-2); }
;
opt_times: | TIMES ;


/* JUSTIFIED */

justified_clause:
  JUSTIFIED opt_right		{ curr_field->flags.just_r = 1; }
;
opt_right: | RIGHT ;


/* SYNCHRONIZED */

synchronized_clause:
  SYNCHRONIZED left_or_right	{ curr_field->flags.sync = 1; }
;
left_or_right:
| LEFT
| RIGHT
;


/* BLANK */

blank_clause:
  BLANK opt_when ZEROS		{ curr_field->flags.blank=1; }
;


/* VALUE */

value_clause:
  VALUE opt_is_are value_list
;
value_list:
  value_item
| value_list value_item
;
value_item:
  gliteral			{ set_variable_values($1,$1); }
| gliteral THRU gliteral	{ set_variable_values($1,$3); }
;


/* RENAMES */

renames_clause:
  RENAMES			{ curr_division = CDIV_INITIAL; }
  variable opt_renames_thru	{ curr_division = CDIV_DATA; }
  {
    yywarn ("RENAMES is not supported yet");
  }
;
opt_renames_thru:
| THRU variable
;


/*******************
 * LINKAGE SECTION
 *******************/

linkage_section:
| LINKAGE SECTION dot		{ at_linkage=1; curr_field=NULL; }
  field_description_list	{ close_fields(); at_linkage=0; }
;


/*******************
 * REPORT SECTION
 *******************/

report_section:
| REPORT SECTION dot rd_statement_list
;
rd_statement_list:
| rd_statement_list rd_statement
;
rd_statement:
  RD SYMBOL_TOK { COB_FIELD_TYPE ($2) = 'W'; curr_division = CDIV_INITIAL; }
  report_controls { curr_division = CDIV_DATA; }
  report_description
;
report_controls:
| report_controls CONTROL opt_is_are opt_final report_break_list
| report_controls PAGE opt_limit_is integer opt_line
| report_controls
        HEADING opt_is integer
        opt_first_detail opt_last_detail
        opt_footing '.'
;
report_break_list:
| report_break_list name { $2->defined=1; }
;
report_description:
  report_item
| report_description report_item
;
report_item:
  integer opt_def_name
  {
    define_field ($1, $2);
  }
  report_clauses '.'
  {
    update_report_field ($2);
    curr_division = CDIV_DATA;
  }
;
report_clauses:
| report_clauses TYPE opt_is report_type
        report_position { curr_division = CDIV_INITIAL; }
        opt_report_name
| report_clauses LINE opt_number opt_line_rel integer
| report_clauses opt_report_column
        opt_picture_clause { curr_division = CDIV_INITIAL; }
        report_value
;
opt_report_name:
  name { }
| FINAL { }
| /* NOTHING */
;
report_value:
| VALUE opt_is gname
| SOURCE opt_is gname
| SUM opt_of name
;
opt_report_column:
  COLUMN opt_number integer
| /* nothing */
;
opt_number:
| NUMBER
;
opt_line_rel:
| '+'
| PLUS
;
report_position:
  HEADING
| FOOTING
| /* nothing */
;
report_type:
  PAGE
| CONTROL
| DETAIL
;
opt_picture_clause: | picture_clause ;
opt_limit_is: | LIMIT opt_is ;
opt_footing: | FOOTING opt_is integer ;
opt_last_detail: | LAST DETAIL opt_is integer ;
opt_first_detail: | FIRST DETAIL opt_is integer ;


/*****************************************************************************
 * PROCEDURE DIVISION
 *****************************************************************************/

procedure_division:
| PROCEDURE DIVISION		{ curr_division = CDIV_INITIAL; }
  procedure_using dot
  {
    current_section = NULL;
    current_paragraph = NULL;
    proc_header ($4);
    in_procedure = 1;
  }
  procedure_list
  {
    /* close procedure_list sections & paragraphs */
    if (current_paragraph)
      gen_end_label (current_paragraph);
    if (current_section)
      gen_end_label (current_section);
    resolve_labels ();
    proc_trail ($4);
    in_procedure = 0;
  }
;

/* USING clause */

procedure_using:
  /* nothing */			{ $$ = 0; }
| USING using_vars		{ $$ = 1; }
;
using_vars:
  gname				{ gen_save_using ($1); }
| using_vars gname		{ gen_save_using ($2); }
;

/* Procedures */

procedure_list:
| procedure_list procedure
;
procedure:
  procedure_section dot
| procedure_paragraph dot
| statement_list dot
| error '.'
| '.'
;


/*******************
 * Section/Paragraph
 *******************/

procedure_section:
  label_name SECTION
  {
    cob_tree label = $1;
    if (label->defined)
      {
	label = install (COB_FIELD_NAME (label), SYTB_LAB, 2);
      }
    label->defined = 1;
    COB_FIELD_TYPE (label) = 'S';

    /* End the last section */
    if (current_paragraph)
      gen_end_label (current_paragraph);
    if (current_section)
      gen_end_label (current_section);

    /* Begin a new section */
    current_section = label;
    current_paragraph = NULL;
    gen_begin_label (current_section);
  }
;

procedure_paragraph:
  label_name
  {
    cob_tree lab = $1;
    if (lab->defined)
      {
	lab = lookup_label (lab, current_section);
	if (!lab)
	  lab = install (COB_FIELD_NAME ($1), SYTB_LAB, 2);
      }
    lab->defined = 1;
    lab->parent = current_section;
    COB_FIELD_TYPE (lab) = 'P';

    /* End the last paragraph */
    if (current_paragraph)
      gen_end_label (current_paragraph);

    /* Begin a new paragraph */
    current_paragraph = lab;
    gen_begin_label (current_paragraph);
  }
;


/*
 * Label
 */

label:
  label_name
  {
    cob_tree lab = $1;
    if (lab->defined == 0)
      {
	lab->defined = 2;
	lab->parent = current_section;
      }
    else if ((lab = lookup_label (lab, current_section)) == NULL)
      {
	lab = install (COB_FIELD_NAME ($1), SYTB_LAB, 2);
	lab->defined = 2;
	lab->parent = current_section;
      }
    $$ = lab;
  }
| label_name in_of label_name
  {
    cob_tree lab = $1;
    if (lab->defined == 0)
      {
	lab->defined = 2;
	lab->parent = $3;
      }
    else if ((lab = lookup_label (lab, $3)) == NULL)
      {
	lab = install (COB_FIELD_NAME ($1), SYTB_LAB, 2);
	lab->defined = 2;
	lab->parent = $3;
      }
    $$ = lab;
  }
;
label_name:
  INTEGER_LITERAL		{ $$ = install_label (COB_FIELD_NAME ($1)); }
| SYMBOL_TOK		{ $$ = $1; }
;
in_of: IN | OF ;


/*******************
 * Statements
 *******************/

statement_list:
  statement
| statement_list statement
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
| evaluate_statement
| exit_statement
| goto_statement
| if_statement
| initialize_statement
| inspect_statement
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
| stoprun_statement
| string_statement
| subtract_statement
| unstring_statement
| write_statement
| INITIATE name     { yyerror ("INITIATE is not implemented yet"); }
| GENERATE name     { yyerror ("GENERATE is not implemented yet"); }
| TERMINATE name    { yyerror ("TERMINATE is not implemented yet"); }
| READY TRACE       { yyerror ("TRACE is not implemented yet"); }
| RESET TRACE       { yyerror ("TRACE is not implemented yet"); }
;

conditional_statement_list:
  conditional_statement
| conditional_statement_list conditional_statement
;
conditional_statement:
  statement
| CONTINUE
| NEXT SENTENCE
;


/*
 * ACCEPT statement
 */

accept_statement:
  ACCEPT name			{ asm_call_1 ("cob_accept", $2); }
| ACCEPT name FROM DATE		{ asm_call_1 ("cob_accept_date", $2); }
| ACCEPT name FROM DAY		{ asm_call_1 ("cob_accept_day", $2); }
| ACCEPT name FROM DAY_OF_WEEK	{ asm_call_1 ("cob_accept_day_of_week", $2); }
| ACCEPT name FROM TIME		{ asm_call_1 ("cob_accept_time", $2); }
| ACCEPT name FROM COMMAND_LINE	{ gen_accept_from_cmdline($2); }
| ACCEPT name FROM ENVIRONMENT_VARIABLE NONNUMERIC_LITERAL
  {
    save_literal ($5, 'X');
    gen_accept_env_var ($2, $5);
  }
| ACCEPT name FROM VARIABLE	{ yywarn ("not supported"); }
;


/*
 * ADD statement
 */

add_statement:
  ADD add_body opt_on_size_error end_add
;
add_body:
  number_list TO numeric_variable_list
  {
    gen_add_to ($1, $3);
  }
| number_list opt_add_to GIVING numeric_edited_variable_list
  {
    gen_add_giving ($2 ? list_add ($1, $2) : $1, $4);
  }
| CORRESPONDING group_variable opt_to group_variable flag_rounded
  {
    gen_corresponding (gen_add, $2, $4, $5);
  }
;
opt_add_to:
  /* nothing */			{ $$ = NULL; }
| TO gname			{ $$ = $2; }
;
end_add: | END_ADD ;


/*
 * ALTER statement
 */

alter_statement:
  ALTER alter_options		{ yywarn ("ALTER statement is obsolete"); }
;
alter_options:
  alter_option
| alter_options alter_option
;
alter_option:
  label TO idstring { }
| label TO idstring TO label { }
;


/*
 * CALL statement
 */

call_statement:
  CALL gname			{ current_call_mode = CALL_BY_REFERENCE; }
  call_using call_returning
  {
    gen_call ($2, $4);
    gen_store_fnres ($5);
  }
  opt_on_exception_or_overflow
  opt_end_call
;
call_using:
  /* nothing */			{ $$ = NULL; }
| USING call_parameter_list	{ $$ = $2; }
;
call_parameter_list:
  call_parameter		{ $$ = $1; }
| call_parameter_list
  call_parameter		{ $2->next = $1; $$ = $2; }
;
call_parameter:
  gname
  {
    $$ = make_parameter ($1, current_call_mode);
  }
| BY call_mode gname
  {
    current_call_mode = $2;
    $$ = make_parameter ($3, current_call_mode);
  }
;
call_mode:
  REFERENCE			{ $$ = CALL_BY_REFERENCE; }
| CONTENT			{ $$ = CALL_BY_CONTENT; }
;
call_returning:
  /* nothing */			{ $$ = NULL; }
| RETURNING variable		{ $$ = $2; }
| GIVING variable		{ $$ = $2; }
;

opt_on_exception_or_overflow:
  opt_on_exception_sentence
  opt_not_on_overflow_sentence	{ if ($1) gen_dstlabel ($1); }
;
opt_on_exception_sentence:
  /* nothing */
  {
    int lbl = gen_status_branch (COB_STATUS_OVERFLOW, 0);
    gen_call_error ($<tree>-4);
    $$ = gen_passlabel ();
    gen_dstlabel (lbl);
  }
| opt_on exception_or_overflow
  { $<ival>$ = gen_status_branch (COB_STATUS_OVERFLOW, 0); }
  on_xxx_statement_list		{ $$ = $4; }
;
exception_or_overflow:
  EXCEPTION
| OVERFLOW
;

opt_end_call: | END_CALL ;


/*
 * CANCEL statement
 */

cancel_statement:
  CANCEL gname { gen_cancel ($2); }
;


/*
 * CLOSE statement
 */

close_statement:
  CLOSE close_files
;
close_files:
  file				{ gen_close ($1); }
| close_files file		{ gen_close ($2); }
;


/*
 * COMPUTE statement
 */

compute_statement:
  COMPUTE compute_body opt_on_size_error opt_end_compute
;
compute_body:
  numeric_edited_variable_list '=' expr	{ gen_compute ($1, $3); }
;
opt_end_compute: | END_COMPUTE ;


/*
 * DELETE statement
 */

delete_statement:
  DELETE name opt_record
  {
    gen_delete($2);
  }
  opt_invalid_key
  opt_end_delete
;
opt_end_delete: | END_DELETE ;


/*
 * DISPLAY statement
 */

display_statement:
  DISPLAY display_list opt_upon display_upon opt_with_no_advancing
  {
    cob_tree_list l;
    for (l = $2; l; l = l->next)
      asm_call_1 ("cob_display", l->item);
    if ($5 == 0)
      asm_call ("cob_newline");
  }
  ;
display_list:
  /* nothing */			{ $$ = NULL; }
| display_list gname		{ $$ = list_add ($1, $2); }
;
display_upon:
| CONSOLE
| VARIABLE			{ yywarn ("not supported"); }
;
opt_with_no_advancing:
  /* nothing */			{ $$ = 0; }
| opt_with NO ADVANCING		{ $$ = 1; }
;


/*
 * DIVIDE statement
 */

divide_statement:
  DIVIDE divide_body opt_on_size_error opt_end_divide
;
divide_body:
  number INTO numeric_variable_list
  {
    gen_divide_into ($1, $3);
  }
| number INTO number GIVING numeric_edited_variable_list
  {
    gen_divide_giving ($1, $3, $5);
  }
| number BY number GIVING numeric_edited_variable_list
  {
    gen_divide_giving ($3, $1, $5);
  }
| number INTO number GIVING numeric_edited_variable flag_rounded REMAINDER numeric_edited_variable
  {
    gen_divide_giving_remainder ($1, $3, $5, $8, $6);
  }
| number BY number GIVING numeric_edited_variable flag_rounded REMAINDER numeric_edited_variable
  {
    gen_divide_giving_remainder ($3, $1, $5, $8, $6);
  }
;
opt_end_divide: | END_DIVIDE ;


/*
 * EVALUATE statement
 */

evaluate_statement:
  EVALUATE evaluate_subject_list
  {
    $<ival>$ = loc_label++; /* end of EVALUATE */
  }
  evaluate_body_list
  opt_end_evaluate
  {
    gen_dstlabel ($<ival>3); /* end of EVALUATE */
  }
;

evaluate_subject_list:
  evaluate_subject		{ $$ = make_list ($1); }
| evaluate_subject_list ALSO
  evaluate_subject		{ $$ = list_add ($1, $3); }
;
evaluate_subject:
  unsafe_expr opt_is		{ $$ = $1; }
| condition_1			{ $$ = $1; }
| TRUE				{ $$ = cob_true; }
| FALSE				{ $$ = cob_false; }
;

evaluate_body_list:
| evaluate_body_list evaluate_body
;
evaluate_body:
  evaluate_when_list
  {
    $<ival>$ = loc_label++; /* before next WHEN */
    gen_evaluate_when ($<list>-2, $1, $<ival>$);
  }
  conditional_statement_list
  {
    gen_jmplabel ($<ival>-1); /* end of EVALUATE */
    gen_dstlabel ($<ival>2); /* before next WHEN */
  }
| WHEN OTHER
  conditional_statement_list
  {
    gen_jmplabel ($<ival>-1); /* end of EVALUATE */
  }
;
evaluate_when_list:
  WHEN evaluate_object_list	{ $$ = make_list ((cob_tree) $2); }
| evaluate_when_list
  WHEN evaluate_object_list	{ $$ = list_add ($1, (cob_tree) $3); }
;
evaluate_object_list:
  evaluate_object		{ $$ = make_list ($1); }
| evaluate_object_list ALSO
  evaluate_object		{ $$ = list_add ($1, $3); }
;
evaluate_object:
  flag_not evaluate_object_1
  {
    if ($1)
      {
	if ($2 == cob_any || $2 == cob_true || $2 == cob_false)
	  {
	    yyerror ("cannot use NOT with TRUE, FALSE, or ANY");
	    $$ = cob_any;
	  }
	else
	  {
	    /* NOTE: $2 is not necessarily a condition, but
	     * use COND_NOT to store it here.  BE CAREFUL. */
	    $$ = make_unary_cond ($2, COND_NOT);
	  }
      }
    else
      $$ = $2;
  }
;
evaluate_object_1:
  ANY				{ $$ = cob_any; }
| TRUE				{ $$ = cob_true; }
| FALSE				{ $$ = cob_false; }
| condition_1			{ $$ = $1; }
| unsafe_expr			{ $$ = $1; }
| unsafe_expr THRU unsafe_expr	{ $$ = make_range ($1, $3); }
;
opt_end_evaluate: | END_EVALUATE ;


/*
 * EXIT statement
 */

exit_statement:
  EXIT
  {
    if (current_paragraph)
      gen_exit (current_paragraph);
    else if (current_section)
      gen_exit (current_section);
    else
      yywarn ("EXIT statement here has no effect");
  }
| EXIT PROGRAM			{ gen_exit_program (); }
;


/*
 * GO TO statement
 */

goto_statement:
  GO opt_to label_list
  {
    if ($3->next)
      yyerror ("too many labels with GO TO");
    gen_goto ($3, NULL);
  }
| GO opt_to label_list DEPENDING opt_on variable { gen_goto ($3, $6); }
| GO opt_to { yywarn ("GO TO without label is obsolete"); }
;
label_list:
  label				{ $$ = make_list ($1); }
| label_list label		{ $$ = list_add ($1, $2); }
;


/*
 * IF statement
 */

if_statement:
  if_then { gen_dstlabel($1); } opt_end_if
| if_then ELSE {
    $<ival>$=gen_passlabel();
    gen_dstlabel($1);
  }
  conditional_statement_list {
    gen_dstlabel($<ival>3);
  }
  opt_end_if
| IF error END_IF
;
if_then:
  IF condition { $<ival>$ = gen_testif(); }
  opt_then conditional_statement_list { $$ = $<ival>3; }
;
opt_end_if: | END_IF ;


/*
 * INITILIZE statement
 */

initialize_statement:
  INITIALIZE initialize_variables opt_initialize_replacing
;
initialize_variables:
  gname				{ gen_initialize ($1); }
| initialize_variables gname	{ gen_initialize ($2); }
;
opt_initialize_replacing:
| REPLACING initialize_replacing_list
  {
    yywarn ("INITIALIZE REPLACING is ignored");
  }
;
initialize_replacing_list:
  initialize_replacing
| initialize_replacing_list initialize_replacing
;
initialize_replacing:
  replacing_option opt_data BY name_or_literal
;
replacing_option:
  ALPHABETIC
| ALPHANUMERIC
| NUMERIC
| ALPHANUMERIC_EDITED
| NUMERIC_EDITED
| NATIONAL
| NATIONAL_EDITED
;
opt_data: | DATA ;


/*
 * INSPECT statement
 */

inspect_statement:
  INSPECT name inspect_tallying   { gen_inspect_tallying ($2, $3); }
| INSPECT name inspect_replacing  { gen_inspect_replacing ($2, $3); }
| INSPECT name inspect_converting { gen_inspect_converting ($2, $3); }
| INSPECT name inspect_tallying inspect_replacing
  {
    gen_inspect_tallying ($2, $3);
    gen_inspect_replacing ($2, $4);
  }
;

/* INSPECT TALLYING */

inspect_tallying:
  TALLYING tallying_list	{ $$ = $2; }
;
tallying_list:
  name FOR			{ inspect_name = $1; }
  tallying_item			{ $$ = cons ($4, NULL); }
| tallying_list name FOR	{ inspect_name = $2; }
  tallying_item			{ $$ = cons ($5, $1); }
| tallying_list tallying_item	{ $$ = cons ($2, $1); }
;
tallying_item:
  CHARACTERS inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_CHARACTERS, inspect_name, 0, $2);
  }
| ALL name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_ALL, inspect_name, $2, $3);
  }
| LEADING name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_LEADING, inspect_name, $2, $3);
  }

/* INSPECT REPLACING */

inspect_replacing:
  REPLACING replacing_list	{ $$ = $2; }
;
replacing_list:
  replacing_item		{ $$ = cons ($1, NULL); }
| replacing_list replacing_item	{ $$ = cons ($2, $1); }
;
replacing_item:
  CHARACTERS BY name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_CHARACTERS, NULL, $3, $4);
  }
| ALL name_or_literal BY name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_ALL, $4, $2, $5);
  }
| LEADING name_or_literal BY name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_LEADING, $4, $2, $5);
  }
| FIRST name_or_literal BY name_or_literal inspect_before_after_list
  {
    $$ = make_inspect_item (INSPECT_FIRST, $4, $2, $5);
  }

/* INSPECT CONVERTING */

inspect_converting:
  CONVERTING name_or_literal TO name_or_literal inspect_before_after_list
  {
    $$ = make_list (make_inspect_item (INSPECT_CONVERTING, $2, $4, $5));
  }

/* INSPECT BEFORE/AFTER */

inspect_before_after_list:
  /* nothing */					 { $$ = NULL; }
| inspect_before_after_list inspect_before_after { $$ = list_add ($1, $2); }
;
inspect_before_after:
  BEFORE opt_initial name_or_literal
  {
    $$ = make_inspect_item (INSPECT_BEFORE, $3, 0, 0);
  }
| AFTER opt_initial name_or_literal
  {
    $$ = make_inspect_item (INSPECT_AFTER, $3, 0, 0);
  }
;
opt_initial: | TOK_INITIAL ;


/*
 * MOVE statement
 */

move_statement:
  MOVE gname TO move_vars
| MOVE CORRESPONDING group_variable TO group_variable
  {
    gen_corresponding (gen_move, $3, $5, 0);
  }
;
move_vars:
  gname				{ gen_move ($<tree>-1, $1); }
| move_vars gname		{ gen_move ($<tree>-1, $2); }
;


/*
 * MULTIPLY statement
 */

multiply_statement:
  MULTIPLY multiply_body opt_on_size_error opt_end_multiply
;
multiply_body:
  number BY numeric_variable_list
  {
    gen_multiply_by ($1, $3);
  }
| number BY number GIVING numeric_edited_variable_list
  {
    gen_multiply_giving ($1, $3, $5);
  }
;
opt_end_multiply: | END_MULTIPLY ;


/*
 * OPEN statement
 */

open_statement:
  OPEN open_options
;
open_options:
  open_mode open_file_list { }
| open_options open_mode open_file_list
;
open_file_list:
  file				{ gen_open ($<ival>0, $1); }
| open_file_list file		{ gen_open ($<ival>0, $2); }
;
open_mode:
  INPUT  { $$=1; }
| I_O    { $$=2; }
| OUTPUT { $$=3; }
| EXTEND { $$=4; }
| error  { yyerror ("invalid OPEN mode"); }
;


/*
 * PERFORM statement
 */

perform_statement:
  PERFORM perform_options
  ;
perform_options:
  conditional_statement_list END_PERFORM
| gname TIMES
  {
    gen_push_int($1);
    $<ival>$=gen_marklabel();
    gen_perform_test_counter($<ival>$);
  }
  conditional_statement_list
  {
    gen_perform_times($<ival>3);
  }
  END_PERFORM
| opt_with_test UNTIL
  {
    if ($1 == 2)
      lbstart = gen_passlabel();
    $<ival>$ = gen_marklabel();
  }
  condition
  {
    $<ival>$=gen_orstart();
    if ($1 == 2)
      {
	lbend=gen_passlabel();
	gen_dstlabel(lbstart);
      }
  }
  conditional_statement_list
  {
    if ($1 == 2)
      {
	gen_jmplabel($<ival>3);
	gen_dstlabel(lbend);
	gen_jmplabel(lbstart);
	gen_dstlabel($<ival>5);
      }
    else
      {
	gen_jmplabel($<ival>3);
	gen_dstlabel($<ival>5);
      }
  }
  END_PERFORM
| opt_with_test VARYING name FROM gname opt_by gname UNTIL
  {
    gen_move($5,$3);
    /* BEFORE=1 AFTER=2 */
    if ($1 == 2)
      lbstart=gen_passlabel();
    $<ival>$=gen_marklabel();
  }
  condition
  {
    $<ival>$=gen_orstart();
    /* BEFORE=1 AFTER=2 */
    if ($1 == 2) {
      gen_add($7,$3,0);
      gen_dstlabel(lbstart);
    }
  }
  opt_perform_after
  conditional_statement_list
  {
    int i;
    struct perf_info *rf;
    /*struct perform_info *rpi;*/
    char *vn;

    /* Check for duplicate varaibles in VARYING/AFTER */
    if ($12 != NULL) {
      if ((vn = check_perform_variables($3, $12)) != NULL) {
	yyerror("Duplicate variable '%s' in VARYING/AFTER clause", vn);
      }
    }

    if ($1 == 2) {
      if ($12 != NULL) {
	for (i=3; i>=0; i--) {
	  rf = $12->pf[i];
	  if (rf != NULL) {
	    gen_jmplabel(rf->ljmp);
	    gen_dstlabel(rf->lend);
	  }
	}
      }
      gen_jmplabel($<ival>9);
      gen_dstlabel($<ival>11);
    }
    else {
      if ($12 != NULL) {
	for (i=3; i>=0; i--) {
	  rf = $12->pf[i];
	  if (rf != NULL) {
	    gen_add(rf->pname1, rf->pname2, 0);
	    gen_jmplabel(rf->ljmp);
	    gen_dstlabel(rf->lend);
	  }
	}
      }
      gen_add($7,$3,0);
      gen_jmplabel($<ival>9);
      gen_dstlabel($<ival>11);
    }
  }
  END_PERFORM
| label opt_perform_thru
  {
    gen_perform_thru($1,$2);
  }
| label opt_perform_thru opt_with_test UNTIL
  {
    $<ival>$=gen_marklabel();
    /* BEFORE=1 AFTER=2 */
    if ($3 == 2) {
      gen_perform_thru($1,$2);
    }
  }
  condition
  {
    unsigned long lbl;
    lbl=gen_orstart();
    /* BEFORE=1 AFTER=2 */
    if ($3 == 1) {
      gen_perform_thru($1,$2);
    }
    gen_jmplabel($<ival>5);
    gen_dstlabel(lbl);
  }
| label opt_perform_thru gname TIMES
  {
    unsigned long lbl;
    gen_push_int($3);
    lbl = gen_marklabel();
    gen_perform_test_counter(lbl);
    gen_perform_thru($1,$2);
    gen_perform_times(lbl);
  }
| label opt_perform_thru opt_with_test VARYING name
  FROM gname opt_by gname UNTIL
  {
    gen_move($7,$5);
    if ($3 == 2)
      lbstart=gen_passlabel();
    $<ival>$ = gen_marklabel();
  }
  condition
  {
    $<ival>$ = gen_orstart();
    /* BEFORE=1 AFTER=2 */
    if ($3 == 2) {
      gen_add($9,$5, 0);
      gen_dstlabel(lbstart);
    }
  }
  opt_perform_after
  {
    int i;
    struct perf_info *rf;
    char *vn = NULL;

    /* Check for duplicate varaibles in VARYING/AFTER */
    if ($14 != NULL) {
      if ((vn = check_perform_variables($5, $14)) != NULL) {
	yyerror("Duplicate variable '%s' in VARYING/AFTER clause", vn);
      }
    }
    gen_perform_thru($1,$2);
    /* BEFORE=1 AFTER=2 */
    if ($3 == 2) {
      if ($14 != NULL) {
	for (i=3; i>=0; i--) {
	  rf = $14->pf[i];
	  if (rf != NULL) {
	    gen_jmplabel(rf->ljmp);
	    gen_dstlabel(rf->lend);
	  }
	}
      }
      gen_jmplabel($<ival>11);
      gen_dstlabel($<ival>13);
    }
    else {
      if ($14 != NULL) {
	for (i=3; i>=0; i--) {
	  rf = $14->pf[i];
	  if (rf != NULL) {
	    gen_add(rf->pname1, rf->pname2, 0);
	    gen_jmplabel(rf->ljmp);
	    gen_dstlabel(rf->lend);
	  }
	}
      }
      gen_add($9,$5,0);
      gen_jmplabel($<ival>11);
      gen_dstlabel($<ival>13);
    }
  }
;
opt_perform_thru:
  /* nothing */			{ $$ = NULL; }
| THRU label			{ $$ = $2;}
;
opt_with_test:
  {
    $<ival>$=1;
    perform_after_sw=1;
  }
| opt_with TEST before_after
  {
    $$=$3;
    perform_after_sw=$3;
  }
;
opt_perform_after:
  /* nothing */			{ $$ = NULL; }
| perform_after
  {
    $<pfvals>$=create_perform_info();
    $<pfvals>$->pf[0] = $1;
    $$=$<pfvals>$;
  }
| perform_after perform_after
  {
    $<pfvals>$=create_perform_info();
    $<pfvals>$->pf[0] = $1;
    $<pfvals>$->pf[1] = $2;
    $$=$<pfvals>$;
  }
| perform_after perform_after perform_after
  {
    $<pfvals>$=create_perform_info();
    $<pfvals>$->pf[0] = $1;
    $<pfvals>$->pf[1] = $2;
    $<pfvals>$->pf[2] = $3;
    $$=$<pfvals>$;
  }
| perform_after perform_after perform_after perform_after
  {
    $<pfvals>$=create_perform_info();
    $<pfvals>$->pf[0] = $1;
    $<pfvals>$->pf[1] = $2;
    $<pfvals>$->pf[2] = $3;
    $<pfvals>$->pf[3] = $4;
    $$=$<pfvals>$;
  }
;
perform_after:
  AFTER name FROM gname BY gname UNTIL
  {
    gen_move ($4, $2);
    /* BEFORE=1 AFTER=2 */
    if (perform_after_sw == 2) {
      lbstart=gen_passlabel();
    }
    $<ival>$ = gen_marklabel();
  }
  condition
  {
    unsigned long lbl;
    lbl=gen_orstart();
    /* BEFORE=1 AFTER=2 */
    if (perform_after_sw == 2) {
      gen_add ($6, $2, 0);
      gen_dstlabel(lbstart);
    }
    $$ = create_perf_info($6, $2, $<ival>8, lbl);
  }
;
before_after:
  BEFORE			{ $$ = 1; }
| AFTER				{ $$ = 2; }
;


/*
 * READ statements
 */

read_statement:
  READ read_body opt_end_read
;
read_body:
  file opt_read_next opt_record opt_read_into opt_read_key
  {
    gen_reads ($1, $4, $5, $2);
  }
  read_option
;
opt_read_next:
  /* nothing */			{ $$ = 0; }
| NEXT				{ $$ = 1; }
| PREVIOUS			{ $$ = 2; }
;
opt_read_into:
  /* nothing */			{ $$ = NULL; }
| INTO name			{ $$ = $2; }
;
opt_read_key:
  /* nothing */			{ $$ = NULL; }
| KEY opt_is name		{ $$ = $3; }
;
read_option:
| at_end
| invalid_key
;
opt_end_read: | END_READ ;


/*
 * RELEASE statement
 */

release_statement:
  RELEASE level1_variable opt_write_from
  {
    gen_release ($2, $3);
  }
;


/*
 * RETURN statements
 */

return_statement:
  RETURN return_body opt_end_return
;
return_body:
  name opt_record opt_read_into
  {
    if ($1->organization != ORG_SEQUENTIAL)
      gen_read_next ($1, $3, 1);
    else
      gen_return ($1, $3);
  }
  opt_at_end
;
opt_end_return: | END_RETURN ;


/*
 * REWRITE statement
 */

rewrite_statement:
  REWRITE level1_variable opt_write_from
  {
    gen_rewrite ($2, $3);
  }
  opt_invalid_key
  opt_end_rewrite
;
opt_end_rewrite: | END_REWRITE ;


/*
 * SEARCH statement
 */

search_statement:
  SEARCH search_body opt_end_search
| SEARCH ALL search_all_body opt_end_search
;
search_body:
  indexed_variable
  {
    $<ival>$=loc_label++; /* determine END label name */
    gen_marklabel();
  }
  search_opt_varying
  {
    $<ival>$=loc_label++; /* determine search loop start label */
    gen_jmplabel($<ival>$); /* generate GOTO search loop start  */
  }
  search_opt_at_end
  {
    gen_jmplabel($<ival>2); /* generate GOTO END  */
    gen_dstlabel($<ival>4); /* generate search loop start label */
    $$ = $<ival>2;
  }
  search_when_list
  {
    /* increment loop index, check for end */
    gen_SearchLoopCheck($5, $3, $1);

    gen_jmplabel($<ival>4); /* generate goto search loop start label */
    gen_dstlabel($<ival>2); /* generate END label */
  }
;
search_all_body:
  indexed_variable
  {
     lbend=loc_label++; /* determine END label name */
     gen_marklabel();

     lbstart=loc_label++; /* determine search_all loop start label */

     /* Initilize and store search table index boundaries */
     Initialize_SearchAll_Boundaries ($1);

     gen_jmplabel(lbstart); /* generate GOTO search_all loop start  */
  }
  search_opt_at_end
  {
     gen_jmplabel(lbend); /* generate GOTO END  */
     gen_dstlabel(lbstart); /* generate search loop start label */
  }
  search_all_when_list
  {
     /* adjust loop index, check for end */
     gen_SearchAllLoopCheck($3, $1, curr_field, lbstart, lbend);
  }
;
search_opt_varying:
  /* nothing */			{ $$ = NULL; }
| VARYING variable		{ $$ = $2; }
;
search_opt_at_end:
  /* nothing */
  {
    $<ival>$=loc_label++; /* determine ATEND label name */
    gen_dstlabel($<ival>$); /* determine ATEND label name */
  }
| opt_at END
  {
    $<ival>$=loc_label++; /* determine ATEND label name */
    gen_dstlabel($<ival>$); /* determine ATEND label name */
  }
  statement_list
  {
    $<ival>$=$<ival>3;
  }
;
search_when_list:
  search_when			{ $$=$1; }
| search_when_list search_when	{ $$=$1; }
;
search_when:
  WHEN condition		{ $<ival>$=gen_testif(); }
  conditional_statement_list
  {
     $$ = $<ival>0;
     gen_jmplabel($$); /* generate GOTO END  */
     gen_dstlabel($<ival>3);
  }
;
search_all_when_list:
  search_all_when
| search_all_when_list search_all_when
;
search_all_when:
  WHEN { curr_field = NULL; }
  search_all_when_conditional
  {
    gen_condition ($3);
    $<ival>$=gen_testif();
  }
  conditional_statement_list
  {
     gen_jmplabel(lbend); /* generate GOTO END  */
     gen_dstlabel($<ival>4);
  }
;
search_all_when_conditional:
  variable opt_is equal_to var_or_lit
  {
    if (curr_field == NULL)
      curr_field = $1;
    $$ = make_cond ($1, COND_EQ, $4);
  }
| search_all_when_conditional AND search_all_when_conditional
  {
    $$ = make_cond ($1, COND_AND, $3);
  }
;
var_or_lit: variable | literal ;
equal_to: EQUAL opt_to | '=' opt_to ;
opt_end_search: | END_SEARCH ;


/*
 * SET statement
 */

set_statement:
  SET variable_list set_mode number	{ gen_set ($2, $3, $4); }
| SET conditional_variable_list TO TRUE	{ gen_set_true ($2); }
| SET set_on_off_list
  {
    yywarn ("SET ON/OFF is not supported");
  }
;
set_mode:
  TO			{ $$ = SET_TO; }
| UP BY			{ $$ = SET_UP; }
| DOWN BY		{ $$ = SET_DOWN; }
;
set_on_off_list:
  set_on_off
| set_on_off_list set_on_off
;
set_on_off:
  set_name_list TO ON
| set_name_list TO OFF
;
set_name_list:
  SPECIAL_TOK { }
| set_name_list SPECIAL_TOK { }
;


/*
 * SORT statement
 */

sort_statement:
  SORT name sort_keys		{ gen_sort ($2); }
  sort_input
  sort_output
;
sort_keys:
| sort_keys sort_direction KEY name
  {
    $4->direction = $2;
    $4->sort_data = $<tree>0->sort_data;
    $<tree>0->sort_data = $4;
  }
;
sort_direction:
  ASCENDING			{ $$ = 1; }
| DESCENDING			{ $$ = 2; }
;
sort_input:
  INPUT PROCEDURE opt_is sort_range
| USING sort_file_list
  {
    gen_sort_using ($<tree>-2, $2);
  }
;
sort_output:
  OUTPUT PROCEDURE opt_is sort_range
| GIVING sort_file_list
  {
    gen_sort_giving ($<tree>-3, $2);
  }
;
sort_file_list:
  file				{ $$ = make_list ($1); }
| sort_file_list file		{ $$ = list_add ($1, $2); }
;
sort_range:
  label opt_perform_thru
  {
    gen_perform_thru($1,$2);
    $$ = ($2 == NULL) ? $1 : $2;
  }
;


/*
 * START statement
 */

start_statement:
  START start_body opt_invalid_key opt_end_start
;
start_body:
  name				{ gen_start($1,0,NULL); }
| name KEY opt_is operator name	{ gen_start($1,$4,$5); }
;
opt_end_start: | END_START ;


/*
 * STOP RUN statement
 */

stoprun_statement:
  STOP RUN			{ gen_stoprun (); }
| STOP NONNUMERIC_LITERAL	{ yywarn ("STOP \"name\" is obsolete"); }
;


/*
 * STRING statement
 */

string_statement:
  STRING string_list INTO name opt_with_pointer
  {
    if ($5)
      $2 = cons (make_string_item (STRING_WITH_POINTER, $5), $2);
    gen_string ($4, $2);
  }
  opt_on_overflow
  opt_end_string
;
string_list:
  string_delimited_list			{ $$ = $1; }
| string_list string_delimited_list	{ $$ = list_append ($1, $2); }
;
string_delimited_list:
  string_name_list
  {
    $$ = $1;
  }
| string_name_list DELIMITED opt_by gname
  {
    $$ = cons (make_string_item (STRING_DELIMITED_NAME, $4), $1);
  }
| string_name_list DELIMITED opt_by SIZE
  {
    $$ = cons (make_string_item (STRING_DELIMITED_SIZE, 0), $1);
  }
;
string_name_list:
  gname
  {
    $$ = make_list (make_string_item (STRING_CONCATENATE, $1));
  }
| string_name_list gname
  {
    $$ = list_add ($1, make_string_item (STRING_CONCATENATE, $2));
  }
;
opt_with_pointer:
  /* nothing */			{ $$ = NULL; }
| opt_with POINTER name		{ $$ = $3; }
;
opt_end_string: | END_STRING ;


/*
 * SUBTRACT statement
 */

subtract_statement:
  SUBTRACT subtract_body opt_on_size_error opt_end_subtract
;
subtract_body:
  number_list FROM numeric_variable_list
  {
    gen_subtract_from ($1, $3);
  }
| number_list FROM number GIVING numeric_edited_variable_list
  {
    gen_subtract_giving ($1, $3, $5);
  }
| CORRESPONDING group_variable FROM group_variable flag_rounded
  {
    gen_corresponding (gen_sub, $2, $4, $5);
  }
;
opt_end_subtract: | END_SUBTRACT ;


/*
 * UNSTRING statement
 */

unstring_statement:
  UNSTRING name unstring_delimited
  INTO unstring_into opt_with_pointer opt_unstring_tallying
  {
    if ($6)
      $3 = cons (make_string_item (UNSTRING_WITH_POINTER, $6), $3);
    if ($7)
      $5 = list_add ($5, make_string_item (UNSTRING_TALLYING, $7));
    gen_unstring ($2, list_append ($3, $5));
  }
  opt_on_overflow
  opt_end_unstring
;

unstring_delimited:
  /* nothing */			{ $$ = NULL; }
| DELIMITED opt_by
  unstring_delimited_list	{ $$ = $3; }
;
unstring_delimited_list:
  unstring_delimited_item	{ $$ = $1; }
| unstring_delimited_list OR 
  unstring_delimited_item	{ $$ = list_append ($1, $3); }
;
unstring_delimited_item:
  flag_all gname
  {
    int type = $1 ? UNSTRING_DELIMITED_ALL : UNSTRING_DELIMITED_BY;
    $$ = make_list (make_string_item (type, $2));
  }
;

unstring_into:
  unstring_into_item		{ $$ = $1; }
| unstring_into
  unstring_into_item		{ $$ = list_append ($1, $2); }
;
unstring_into_item:
  name opt_unstring_delimiter opt_unstring_count
  {
    $$ = make_list (make_string_item (UNSTRING_INTO, $1));
    if ($2)
      $$ = list_add ($$, make_string_item (UNSTRING_DELIMITER, $2));
    if ($3)
      $$ = list_add ($$, make_string_item (UNSTRING_COUNT, $3));
  }
;
opt_unstring_delimiter:
  /* nothing */			{ $$ = NULL; }
| DELIMITER opt_in name		{ $$ = $3; }
;
opt_unstring_count:
  /* nothing */			{ $$ = NULL; }
| COUNT opt_in name		{ $$ = $3; }
;

opt_unstring_tallying:
  /* nothing */			{ $$ = NULL; }
| TALLYING opt_in name		{ $$ = $3; }
;
opt_end_unstring: | END_UNSTRING ;


/*
 * WRITE statement
 */

write_statement:
  WRITE level1_variable opt_write_from write_options
  {
    gen_write ($2, $4, $3);
  }
  opt_invalid_key
  opt_end_write
;
opt_write_from:
  /* nothing */			{ $$ = NULL; }
| FROM gname			{ $$ = $2; }
;
write_options:
  /* nothing */			{ $$ = 0; }
| before_after opt_advancing gname opt_line { gen_loadvar($3); $$ = $1; }
| before_after opt_advancing PAGE { $$ = -$1; }
;
opt_advancing: | ADVANCING ;
opt_end_write: | END_WRITE ;


/*******************
 * Status handlers
 *******************/

on_xxx_statement_list:
  statement_list
  {
    $$ = gen_passlabel ();
    gen_dstlabel ($<ival>0);
  }
;
not_on_xxx_statement_list:
  statement_list
  {
    gen_dstlabel ($<ival>0);
  }
;


/*
 * ON SIZE ERROR
 */

opt_on_size_error:
  opt_on_size_error_sentence
  opt_not_on_size_error_sentence { if ($1) gen_dstlabel ($1); }
;
opt_on_size_error_sentence:
  /* nothing */			{ $$ = 0; }
| opt_on SIZE ERROR
  { $<ival>$ = gen_status_branch (COB_STATUS_OVERFLOW, 0); }
  on_xxx_statement_list		{ $$ = $5; }
;
opt_not_on_size_error_sentence:
| NOT opt_on SIZE ERROR
  { $<ival>$ = gen_status_branch (COB_STATUS_OVERFLOW, 1); }
  not_on_xxx_statement_list
;


/*
 * ON OVERFLOW
 */

opt_on_overflow:
  opt_on_overflow_sentence
  opt_not_on_overflow_sentence	{ if ($1) gen_dstlabel ($1); }
;
opt_on_overflow_sentence:
  /* nothing */			{ $$ = 0; }
| opt_on OVERFLOW
  { $<ival>$ = gen_status_branch (COB_STATUS_OVERFLOW, 0); }
  on_xxx_statement_list		{ $$ = $4; }
;
opt_not_on_overflow_sentence:
| NOT opt_on OVERFLOW
  { $<ival>$ = gen_status_branch (COB_STATUS_OVERFLOW, 1); }
  not_on_xxx_statement_list
;


/*
 * AT END
 */

opt_at_end:
| at_end
;
at_end:
  at_end_sentence		{ gen_dstlabel ($1); }
| not_at_end_sentence
| at_end_sentence
  not_at_end_sentence		{ gen_dstlabel ($1); }
;
at_end_sentence:
  END
  { $<ival>$ = gen_status_branch (COB_STATUS_SUCCESS, 1); }
  on_xxx_statement_list		{ $$ = $3; }
| AT END
  { $<ival>$ = gen_status_branch (COB_STATUS_SUCCESS, 1); }
  on_xxx_statement_list		{ $$ = $4; }
;
not_at_end_sentence:
  NOT opt_at END
  { $<ival>$ = gen_status_branch (COB_STATUS_SUCCESS, 0); }
  not_on_xxx_statement_list
;


/*
 * INVALID KEY
 */

opt_invalid_key:
| invalid_key
;
invalid_key:
  invalid_key_sentence		{ gen_dstlabel ($1); }
| not_invalid_key_sentence
| invalid_key_sentence
  not_invalid_key_sentence	{ gen_dstlabel ($1); }
;
invalid_key_sentence:
  INVALID opt_key
  { $<ival>$ = gen_status_branch (23, 0); }
  on_xxx_statement_list		{ $$ = $4; }
;
not_invalid_key_sentence:
  NOT INVALID opt_key
  { $<ival>$ = gen_status_branch (0, 0); }
  not_on_xxx_statement_list
;


/*******************
 * Condition
 *******************/

condition:
  condition_1
  {
    gen_condition ($1);
  }
;
condition_1:
  VARCOND			{ $$ = make_unary_cond ($1, COND_VAR); }
| comparative_condition		{ $$ = $1; }
| class_condition		{ $$ = $1; }
| '(' condition_1 ')'		{ $$ = $2; }
| NOT condition_1		{ $$ = make_unary_cond ($2, COND_NOT); }
| condition_1 AND condition_2	{ $$ = make_cond ($1, COND_AND, $3); }
| condition_1 OR condition_2	{ $$ = make_cond ($1, COND_OR, $3); }
;
condition_2:
  condition_1			{ $$ = $1; }
| unsafe_expr opt_is		{ $$ = make_opt_cond ($<tree>-1, -1, $1); }
| NOT unsafe_expr opt_is	{ $$ = make_opt_cond ($<tree>-1, -1, $2); }
| operator unsafe_expr		{ $$ = make_opt_cond ($<tree>-1, $1, $2); }
;


/*
 * Comparative condition
 */

comparative_condition:
  unsafe_expr opt_is operator unsafe_expr
  {
    if (EXPR_P ($1) || EXPR_P ($4))
      {
	ASSERT_VALID_EXPR ($1);
	ASSERT_VALID_EXPR ($4);
      }
    $$ = make_cond ($1, $3, $4);
  }
;
operator:
  flag_not equal		{ $$ = $1 ? COND_NE : COND_EQ; }
| flag_not greater		{ $$ = $1 ? COND_LE : COND_GT; }
| flag_not less			{ $$ = $1 ? COND_GE : COND_LT; }
| flag_not greater_or_equal	{ $$ = $1 ? COND_LT : COND_GE; }
| flag_not less_or_equal	{ $$ = $1 ? COND_GT : COND_LE; }
;
equal: '=' | EQUAL opt_to ;
greater: '>' | GREATER opt_than ;
less: '<' | LESS opt_than ;
greater_or_equal: GE | GREATER opt_than OR EQUAL opt_to ;
less_or_equal: LE | LESS opt_than OR EQUAL opt_to ;


/*
 * Class condition
 */

class_condition:
  unsafe_expr opt_is flag_not class
  {
    if (EXPR_P ($1))
      yyerror ("expression is always NUMERIC");

    /* TODO: do more static class check here */

    $$ = make_unary_cond ($1, $4);
    if ($3)
      $$ = make_unary_cond ($$, COND_NOT);
  }
;
class:
  NUMERIC			{ $$ = COND_NUMERIC; }
| ALPHABETIC			{ $$ = COND_ALPHABETIC; }
| ALPHABETIC_LOWER		{ $$ = COND_LOWER; }
| ALPHABETIC_UPPER		{ $$ = COND_UPPER; }
| POSITIVE			{ $$ = COND_POSITIVE; }
| NEGATIVE			{ $$ = COND_NEGATIVE; }
| ZEROS				{ $$ = COND_ZERO; }
| CLASS_TOK			{ $$ = COND_ZERO; yywarn ("not supported"); }
;


/*******************
 * Expression
 *******************/

expr:
  unsafe_expr
  {
    ASSERT_VALID_EXPR ($1);
    $$ = $1;
  }
;
unsafe_expr:
  gname				{ $$ = $1; }
| '(' unsafe_expr ')'		{ $$ = $2; }
| unsafe_expr '+' unsafe_expr	{ $$ = make_expr ($1, '+', $3); }
| unsafe_expr '-' unsafe_expr	{ $$ = make_expr ($1, '-', $3); }
| unsafe_expr '*' unsafe_expr	{ $$ = make_expr ($1, '*', $3); }
| unsafe_expr '/' unsafe_expr	{ $$ = make_expr ($1, '/', $3); }
| unsafe_expr '^' unsafe_expr	{ $$ = make_expr ($1, '^', $3); }
;


/*****************************************************************************
 * Basic rules
 *****************************************************************************/

/*******************
 * Special variables
 *******************/


/* Level 1 variable */

level1_variable:
  name
  {
    if ($1->level != 1)
      yyerror ("variable `%s' must be level 01", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;


/* Numeric variable */

numeric_variable_list:
  numeric_variable flag_rounded	{ $$ = create_mathvar_info (NULL, $1, $2); }
| numeric_variable_list
  numeric_variable flag_rounded	{ $$ = create_mathvar_info ($1, $2, $3); }
;
numeric_variable:
  name
  {
    if (!is_numeric ($1))
      yyerror ("variable `%s' must be numeric", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;


/* Numeric edited variable */

numeric_edited_variable_list:
  numeric_edited_variable flag_rounded { $$ = create_mathvar_info (NULL, $1, $2); }
| numeric_edited_variable_list
  numeric_edited_variable flag_rounded { $$ = create_mathvar_info ($1, $2, $3); }
;
numeric_edited_variable:
  name
  {
    if (!is_editable ($1))
      yyerror ("variable `%s' must be numeric edited", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;


/* Group variable */

group_variable:
  variable
  {
    if (SUBREF_P ($1))
      $1 = SUBREF_SYM ($1);
    if (!SYMBOL_P ($1) || COB_FIELD_TYPE ($1) != 'G')
      yyerror ("variable `%s' must be group", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;


/* Filename */

file:
  VARIABLE
  {
    if (COB_FIELD_TYPE ($1) != 'F')
      yyerror ("file name is expected: %s", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;


/*******************
 * Special values
 *******************/


/* Number */

number_list:
  number			{ $$ = make_list ($1); }
| number_list number		{ $$ = list_add ($1, $2); }
;
number:
  gname
  {
    if (!is_numeric ($1))
      yyerror ("numeric value is expected: %s", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;

integer:
  INTEGER_LITERAL
  {
    char *s = COB_FIELD_NAME ($1);
    $$ = 0;
    while (*s)
      $$ = $$ * 10 + *s++ - '0';
  }
;


idstring:
  { start_condition = START_ID; } ID_TOK { $$ = $2; }
;


gname:
  name
| gliteral
| function_call
;
function_call:
  FUNCTION idstring '(' function_args ')'
  {
    yyerror ("function call is not supported yet");
    YYABORT;
  }
;
function_args:
  gname { }
| function_args gname
;
name_or_literal:
  name
| gliteral
;


/*
 * Literals
 */

gliteral:
  without_all_literal
| all_literal
;
without_all_literal:
  literal
| special_literal
;
all_literal:
  ALL literal			{ LITERAL ($2)->all=1; $$=$2; }
| ALL special_literal		{ $$ = $2; }
;
special_literal:
  SPACES			{ $$ = spe_lit_SP; }
| ZEROS				{ $$ = spe_lit_ZE; }
| QUOTES			{ $$ = spe_lit_QU; }
| HIGH_VALUES			{ $$ = spe_lit_HV; }
| LOW_VALUES			{ $$ = spe_lit_LV; }
;
literal:
  nliteral			{ $$ = $1; }
| NONNUMERIC_LITERAL		{ $$ = save_literal ($1, 'X'); }
;
nliteral:
  INTEGER_LITERAL		{ $$ = save_literal ($1, '9'); }
| NUMERIC_LITERAL		{ $$ = save_literal ($1, '9'); }
;



indexed_variable:
  SUBSCVAR
  {
    if ($1->times == 1)
      yyerror ("variable `%s' not OCCURS", COB_FIELD_NAME ($1));
    if (determine_table_index_name ($1) == NULL)
      yyerror ("variable `%s' must be indexed", COB_FIELD_NAME ($1));
    $$ = $1;
  }
;
opt_def_name:
  /* nothing */			{ $$ = make_filler(); }
| def_name			{ $$ = $1; }
;
def_name:
  FILLER			{ $$ = make_filler(); }
| SYMBOL_TOK
  {
    if ($1->defined)
      yyerror("variable redefined, %s", COB_FIELD_NAME ($1));
    $1->defined = 1;
    $$ = $1;
  }
;
filename:
  SYMBOL_TOK
| literal
;
conditional_variable_list:
  conditional_variable		{ $$ = make_list ($1); }
| conditional_variable_list
  conditional_variable		{ $$ = list_add ($1, $2); }
;
conditional_variable:
  VARCOND			{ $$ = $1; }
;
variable_list:
  variable			{ $$ = make_list ($1); }
| variable_list variable	{ $$ = list_add ($1, $2); }
;
name:
  variable
| variable '(' subscript ':' ')'	   { $$ = make_substring ($1, $3, 0); }
| variable '(' subscript ':' subscript ')' { $$ = make_substring ($1, $3, $5); }
;
variable:
  qualified_var
  {
    $$ = $1;
    if (need_subscripts) {
      yyerror("variable `%s' must be subscripted or indexed",
	      COB_FIELD_NAME ($1));
      need_subscripts=0;
    }
  }
| qualified_var LPAR subscript_list ')'
  {
    int required = count_subscripted ($1);
    int given = list_length ($3);
    if (given != required)
      {
	if (required == 1)
	  yyerror ("variable `%s' requires one subscript",
		   COB_FIELD_NAME ($1));
	else
	  yyerror ("variable `%s' requires %d subscripts",
		   COB_FIELD_NAME ($1), required);
      }
    $$ = make_subref ($1, $3);
  }
subscript_list:
  subscript			{ $$ = cons ($1, NULL); }
| subscript_list subscript	{ $$ = cons ($2, $1); }
;
subscript:
  gname				{ $$ = $1; }
| subscript '+' gname		{ $$ = make_expr ($1, '+', $3); }
| subscript '-' gname		{ $$ = make_expr ($1, '-', $3); }
;
qualified_var:
  unqualified_var		{ $$ = $1; }
| qualified_var in_of unqualified_var { $$ = lookup_variable($1,$3); }
;
unqualified_var:
  VARIABLE			{ $$ = $1; }
| SUBSCVAR			{ $$ = $1; need_subscripts = 1; }
;

dot:
  '.'
| error
| /* nothing */
  {
    int save_lineno = cob_orig_lineno;
    cob_orig_lineno = last_lineno;
    yywarn ("`.' is expected after `%s'", last_text);
    cob_orig_lineno = save_lineno;
  }
;


/*
 * Common flags
 */

flag_all:
  /* nothing */			{ $$ = 0; }
| ALL				{ $$ = 1; }
;
flag_not:
  /* nothing */			{ $$ = 0; }
| NOT				{ $$ = 1; }
;
flag_rounded:
  /* nothing */			{ $$ = 0; }
| ROUNDED			{ $$ = 1; }
;


/*
 * Common optional words
 */

opt_area: | AREA ;
opt_at: | AT ;
opt_by: | BY ;
opt_final: | FINAL ;
opt_for: | FOR ;
opt_in: | IN ;
opt_in_size: | IN SIZE ;
opt_is: | IS ;
opt_is_are: | IS | ARE ;
opt_key: | KEY ;
opt_line: | LINE ;
opt_mode: | MODE ;
opt_of: | OF ;
opt_on: | ON ;
opt_program: | PROGRAM ;
opt_record: | RECORD ;
opt_sign: | SIGN ;
opt_status: | STATUS ;
opt_than: | THAN ;
opt_then: | THEN ;
opt_to: | TO ;
opt_upon: | UPON ;
opt_when: | WHEN ;
opt_with: | WITH ;


%%

static cob_tree
make_opt_cond (cob_tree last, int type, cob_tree this)
{
 again:
  if (COND_TYPE (last) == COND_NOT)
    {
      COND_LEFT (last) = make_opt_cond (COND_LEFT (last), type, this);
      return last;
    }

  if (COND_IS_UNARY (last))
    {
      yyerror ("broken condition");
      return last; /* error recovery */
    }

  if (COND_TYPE (last) == COND_AND || COND_TYPE (last) == COND_OR)
    {
      last = COND_LEFT (last);
      goto again;
    }

  if (type == -1)
    type = COND_TYPE (last);
  return make_cond (COND_LEFT (last), type, this);
}

void
yywarn (char *fmt, ...)
{
  va_list argptr;

  /* Print warning line */
  fprintf (stderr, "%s:%d: warning: ", cob_orig_filename, cob_orig_lineno);

  /* Print error body */
  va_start (argptr, fmt);
  vfprintf (stderr, fmt, argptr);
  fputs ("\n", stderr);
  va_end (argptr);

  /* Count warning */
  warning_count++;
}

void
yyerror (char *fmt, ...)
{
  va_list argptr;

  /* Print error line */
  fprintf (stderr, "%s:%d: ", cob_orig_filename, cob_orig_lineno);

  /* Print error body */
  va_start (argptr, fmt);
  vfprintf (stderr, fmt, argptr);
  fputs ("\n", stderr);
  va_end (argptr);

  /* Count error */
  error_count++;
}
