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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "cobc.h"
#include "codegen.h"
#include "_libcob.h"

#define MAXNAMEBUF 	300
#define START_STACK_ADJUST 20
#define SYMBUF_SIZE 128

#define decimal_char() (decimal_comma ? ',' : '.')

int screen_io_enable = 0;
int scr_line, scr_column;
int decimal_comma = 0;
char currency_symbol = '$';
cob_tree curr_paragr = NULL, curr_section = NULL;
cob_tree curr_field;
int curr_call_mode = 0;
int at_procedure = 0;
int at_linkage = 0;
int loc_label = 1;
unsigned char picture[4096];
int piccnt;
int substring_slots = 0;

cob_tree spe_lit_ZE = NULL;
cob_tree spe_lit_SP = NULL;
cob_tree spe_lit_LV = NULL;
cob_tree spe_lit_HV = NULL;
cob_tree spe_lit_QU = NULL;

static int pgm_segment = -1;
static unsigned stack_offset = 0;	/* offset for variables on the stack */
static unsigned global_offset = 4; /* offset for global variables (DATA) */
static unsigned literal_offset = 0;
static unsigned data_offset = 0;
static unsigned linkage_offset = 0;
static unsigned using_offset = 8;
/* tmpvar_offset: for storage of temporary variables, 
with space reclaimed after the current instruction*/
static unsigned tmpvar_offset = 0;
static unsigned tmpvar_max = 0;

static struct list *files_list = NULL;
static struct list *disp_list = NULL;
static struct list *fields_list = NULL;
static struct list *last_field = NULL;
static struct index_to_table_list *index2table = NULL;
static struct named_sect *named_sect_list = NULL;
static int next_available_sec_no = SEC_FIRST_NAMED;
static int curr_sec_no = SEC_DATA;

static int screen_label = 0;
static int stackframe_cnt = 0;
static char program_id[120] = "main";
static char *pgm_label = "main";

static int need_desc_length_cleanup = 0;
static char name_buf[MAXNAMEBUF];
static char init_ctype;		// hold homogenous type
static int init_val;		// hold homogenous value
static unsigned curr_01_location;	// hold current root father when set_field_location


/*
**	Symbol table management routines
*/

#define HASHLEN 100
static cob_tree vartab[HASHLEN] = { NULL };
static cob_tree labtab[HASHLEN] = { NULL };
static cob_tree littab[HASHLEN] = { NULL };

static int
hash (const char *s)
{
  int val = 0;
  for (; *s; s++)
    val += toupper (*s);
  return (val % HASHLEN);
}

static char *
upcase (char *s, char *buf)
{
  char *t;
  int n = SYMBUF_SIZE - 1;
  t = buf;
  while (*s && n--)
    *t++ = toupper (*s++);
  if (n <= 0)
    yyerror ("Too large symbol");
  *t = 0;
  return buf;
}

static char *
chg_underline (char *str)
{
  char *p = str;
  while ((p = strchr (p, '-')) != NULL)
    *p = '_';
  return str;
}

cob_tree 
lookup (char *s, int tab)
{
  char sbuf[SYMBUF_SIZE];
  if (tab == SYTB_LIT)
    {				/* literals tab */
      cob_tree x;
      for (x = littab[hash (s)]; x; x = COB_FIELD_NEXT (x))
	if (strcmp (s, COB_FIELD_NAME (x)) == 0)
	  return x;
      return NULL;
    }
  else
    {
      cob_tree x;
      s = upcase (s, sbuf);
      if (tab == SYTB_VAR)
	x = vartab[hash (s)];
      else
	x = labtab[hash (s)];
      for (; x; x = COB_FIELD_NEXT (x))
	if (strcmp (s, COB_FIELD_NAME (x)) == 0)
	  return x;
      return NULL;
    }
}

cob_tree 
lookup_symbol (char *s)
{
  return lookup (s, SYTB_VAR);
}

cob_tree 
install (char *name, int tab, int cloning)
{
  char sbuf[SYMBUF_SIZE];
  cob_tree as;
  int val;

  name = upcase (name, sbuf);
  if ((as = lookup (name, tab)) == NULL)
    {
      as = make_symbol (strdup (name));
      val = hash (COB_FIELD_NAME (as));
      if (tab == SYTB_VAR)
	{
	  COB_FIELD_NEXT (as) = vartab[val];
	  vartab[val] = as;
	}
      else
	{
	  COB_FIELD_NEXT (as) = labtab[val];
	  labtab[val] = as;
	}
    }
  else if ((cloning && (as->defined == 1)) || (cloning == 2))
    {
      /* install clone (cloning==2 -> force) */
      cob_tree cl = make_symbol (COB_FIELD_NAME (as));
      cl->clone = as->clone;
      as->clone = cl;
      as = cl;
    }
  return (as);
}

cob_tree 
install_label (char *name)
{
  return install (name, SYTB_LAB, 0);
}

cob_tree
install_literal (const char *name)
{
  cob_tree p = make_literal (strdup (name));
  int val = hash (name);
  COB_FIELD_NEXT (p) = littab[val];
  littab[val] = p;
  return p;
}

cob_tree 
lookup_label (cob_tree sy, cob_tree parent)
{
  while (sy->clone && (sy->parent != parent))
    sy = sy->clone;
  if (sy->parent == parent)
    return sy;
  else
    return NULL;
}

static void
output (char *fmt, ...)
{
  va_list argptr;
  va_start (argptr, fmt);
  vfprintf (o_src, fmt, argptr);
  va_end (argptr);
}


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

static void
clear_symtab ()
{
  cob_tree sy, sy1;
  int i;
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy1 = vartab[i]; sy1 != NULL;)
	{
	  for (sy = sy1->clone; sy;)
	    if (sy)
	      sy = sy->clone;
	  sy1 = COB_FIELD_NEXT (sy1);
	}
      vartab[i] = NULL;
    }
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy1 = labtab[i]; sy1 != NULL;)
	{
	  for (sy = sy1->clone; sy;)
	    if (sy)
	      sy = sy->clone;
	  sy1 = COB_FIELD_NEXT (sy1);
	}
      labtab[i] = NULL;
    }
}

static void
clear_offsets ()
{
  stack_offset = 0;
  global_offset = 4;
  literal_offset = 0;
  data_offset = 0;
  linkage_offset = 0;
  using_offset = 8;
  substring_slots = 0;
  fields_list = NULL;
  files_list = NULL;
  /* clear all current paragraphs/sections and fields */
  curr_paragr = NULL;
  curr_section = NULL;
  curr_field = NULL;
  /* free tmpvar storage */
  tmpvar_offset = 0;
  tmpvar_max = 0;
}

static cob_tree
save_special_literal (char *val, char picc, char *nick)
{
  cob_tree lit = install_literal (nick);
  struct lit *p = LITERAL (lit);
  if (COB_FIELD_TYPE (lit))
    return lit;		/* already saved */
  COB_FIELD_TYPE (p) = picc;
  p->nick = val;
  p->all = 1;
  save_field_in_list (lit);
  p->location = literal_offset;
  p->sec_no = SEC_CONST;
  literal_offset += 2;		/* we have only 1-char special literals */
  p->descriptor = literal_offset;
  literal_offset += 14;
  return lit;
}

static void
define_special_fields ()
{
  cob_tree sy, tmp;

  spe_lit_SP = save_special_literal (" ", 'X', "%SPACES%");
  spe_lit_LV = save_special_literal ("\0", 'X', "%LOW-VALUES%");
  spe_lit_HV = save_special_literal ("\xff", 'X', "%HIGH-VALUES%");
  spe_lit_ZE = save_special_literal ("0", '9', "%ZEROS%");
  spe_lit_QU = save_special_literal ("\"", 'X', "%QUOTES%");

  sy = install (SVAR_RCODE, SYTB_VAR, 0);
  COB_FIELD_TYPE (sy) = 'B';	/* assume numeric "usage is comp" item */
  sy->len = 4;
  sy->decimals = 0;
  sy->level = 1;
  sy->sec_no = SEC_DATA;
  sy->times = 1;
  sy->flags.value = 1;
  picture[0] = '9';
  picture[1] = 5;
  picture[2] = 0;

  tmp = curr_field;
  curr_field = sy;
  curr_field->value = spe_lit_ZE;
  curr_field->value2 = spe_lit_ZE;
  update_field ();
  close_fields ();
  curr_field = tmp;
}

void
init_program (const char *id)
{
  pgm_segment++;
  clear_symtab ();
  clear_offsets ();

  if (!pgm_segment)
    output ("\t.file\t\"%s\"\n", cob_source_filename);
  strcpy (program_id, id);

  define_special_fields ();
}


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

/*******************
 * WORKING-STRAGE SECTION
 *******************/

cob_tree 
lookup_variable (cob_tree sy, cob_tree parent)
{
  if (SUBREF_P (parent))
    parent = SUBREF_SYM (parent);

  for (;;)
    {
      cob_tree p;
      for (p = sy; p != NULL; p = p->parent)
	if (p->parent == parent)
	  return sy;

      if (sy->clone == NULL)
	return sy;
      sy = sy->clone;
    }
}

cob_tree 
lookup_for_redefines (cob_tree sy)
{
  if (curr_field->parent == NULL)
    return lookup_symbol (COB_FIELD_NAME (sy));
  else
    return lookup_variable (sy, curr_field->parent);
}

void
save_named_sect (cob_tree sy)
{
  struct named_sect *nsp = malloc (sizeof (struct named_sect));
  nsp->sec_no = next_available_sec_no++;
  nsp->os_name = chg_underline (strdup (COB_FIELD_NAME (sy)));
  nsp->next = named_sect_list;
  named_sect_list = nsp;
  curr_sec_no = nsp->sec_no;	// Uncomment to activate
  sy->sec_no = curr_sec_no;
}


/*
 * Local functions
 */

static void asm_call_1 (const char *name, cob_tree sy1);
static void value_to_eax (cob_tree sy);
static void adjust_desc_length (cob_tree sy);

static void
push_immed (int i)
{
  stackframe_cnt += 4;
  output ("\tpushl\t$%d\n", i);
}

static void
push_eax ()
{
  stackframe_cnt += 4;
  output ("\tpushl\t%%eax\n");
}

static int
symlen (cob_tree x)
{
  if (COB_FIELD_TYPE (x) == 'C')
    return x->len / 2 + 1;
  else if (LITERAL_P (x))
    return LITERAL (x)->len;
  return x->len;
}

static const char *
varsize_movl (cob_tree sy)
{
  switch (symlen (sy))
    {
    case 1:  return "movsbl";
    case 2:  return "movswl";
    default: return "movl";
    }
}

static char *
sec_name (int sec_no)
{
  struct named_sect *nsp;

  for (nsp = named_sect_list; nsp != NULL;)
    {
      if (nsp->sec_no == sec_no)
	return nsp->os_name;
      nsp = nsp->next;
    }
  return (char *) nsp;
}

static char memref_buf[20];
static char *
memref (cob_tree sy)
{
  if (sy->sec_no < SEC_FIRST_NAMED)
    {
      switch (sy->sec_no)
	{
	case SEC_CONST:
	  sprintf (memref_buf, "$c_base%d+%d", pgm_segment, sy->location);
	  break;
	case SEC_DATA:
	  sprintf (memref_buf, "$w_base%d+%d", pgm_segment, sy->location);
	  break;
	case SEC_STACK:
	  sprintf (memref_buf, "-%d(%%ebp)", sy->location);
	  break;
	default:
	  // Make sure we have an error at assembly stage
	  sprintf (memref_buf, "$ww_base%d+%d #sec:%d", pgm_segment,
		   sy->location, sy->sec_no);
	}
    }
  else
    sprintf (memref_buf, "$%s+%d", sec_name (sy->sec_no), sy->location);

  return memref_buf;
}

static char *
memrefat (cob_tree sy)
{
  switch (sy->sec_no)
    {
    case SEC_CONST:
      sprintf (memref_buf, "c_base%d+%d", pgm_segment, sy->location);
      break;
    case SEC_DATA:
      sprintf (memref_buf, "w_base%d+%d", pgm_segment, sy->location);
      break;
    case SEC_STACK:
      sprintf (memref_buf, "-%d(%%ebp)", sy->location);
      break;
    default:
      // Make sure we have an error at assembly stage
      sprintf (memref_buf, "ww_base%d+%d #sec:%d", pgm_segment,
	       sy->location, sy->sec_no);
    }
  return memref_buf;
}

static char *
memrefd (cob_tree sy)
{
  sprintf (memref_buf, "$c_base%d+%d", pgm_segment, sy->descriptor);
  return memref_buf;
}

/* load in cpureg ("eax","ebx"...) location for normal 
	(file/working-storage) or linkage variable */
static void
load_location (cob_tree sy, char *reg)
{
  unsigned offset;
  if (SYMBOL_P (sy) && sy->linkage_flg)
    {
      cob_tree tmp = sy;
      while (tmp->linkage_flg == 1)
	tmp = tmp->parent;
      offset = sy->location - tmp->location;
      output ("\tmovl\t%d(%%ebp), %%%s\n", tmp->linkage_flg, reg);
      if (offset)
	output ("\taddl\t$%d, %%%s\n", offset, reg);
    }
  else if (sy->sec_no == SEC_STACK)
    output ("\tleal\t%s, %%%s\n", memref (sy), reg);
  else
    output ("\tmovl\t%s, %%%s\n", memref (sy), reg);
}

static void
gen_subscripted (cob_tree ref)
{
  cob_tree sy = SUBREF_SYM (ref);
  cob_tree_list ls;

#ifdef COB_DEBUG
  output ("# gen_subscripted\n");
#endif
  output ("\tpushl\t$0\n");
  for (ls = SUBREF_SUBS (ref); ls; ls = ls->next)
    {
      cob_tree x = ls->tree;
      value_to_eax (x);
      output ("\tdecl\t%%eax\n");	/* subscript start at 1 */

      /* find the first parent var that needs subscripting */
      while (sy->times == 1)
	sy = sy->parent;
      if (sy->len > 1)
	{
	  output ("\tmovl\t$%d, %%edx\n", symlen (sy));
	  output ("\timull\t%%edx\n");
	}
      output ("\taddl\t%%eax,0(%%esp)\n");
      sy = sy->parent;
    }
  output ("\tpopl\t%%eax\n");	/* return offset in %eax */
}

static void
loadloc_to_eax (cob_tree sy_p)
{
  unsigned offset;
  cob_tree sy = sy_p, var;

  if (SUBSTRING_P (sy))
    sy = SUBSTRING_VAR (sy);
  if (SUBREF_P (sy))
    {
      gen_subscripted (sy);
      var = SUBREF_SYM (sy);
      if (var->linkage_flg)
	{
	  cob_tree tmp = var;
	  while (tmp->linkage_flg == 1)
	    tmp = tmp->parent;
	  offset = var->location - tmp->location;
	  output ("\t%s\t%d(%%ebp), %%ebx\n",
		  varsize_movl (tmp), tmp->linkage_flg);
	  if (offset)
	    output ("\taddl\t$%d, %%ebx\n", offset);
	  output ("\taddl\t%%ebx, %%eax\n");
	}
      else
	{
	  if (var->sec_no == SEC_STACK)
	    output ("\tleal\t%s, %%ebx\n", memref (var));
	  else if (var->sec_no == SEC_DATA)
	    output ("\tleal\tw_base%d+%d, %%ebx\n",
		     pgm_segment, var->location);
	  else if (var->sec_no == SEC_CONST)
	    output ("\tleal\tc_base%d+%d, %%ebx\n",
		     pgm_segment, var->location);
	  output ("\taddl\t%%ebx,%%eax\n");
	}
    }
  else
    {
      load_location (sy, "eax");
    }
  //      At that stage, the address is ready in %eax; do we need
  //      to correct it because of substring's?
  if (SUBSTRING_P (sy_p))
    {
      // should avoid all that if literal 1
      output ("\tmovl\t%%eax, %%ebx\n");
      value_to_eax (SUBSTRING_OFFSET (sy_p));
      output ("\tdecl\t%%eax\n");
      output ("\taddl\t%%ebx, %%eax\n");
    }
}

static void
gen_loadloc (cob_tree sy)
{
  loadloc_to_eax (sy);
  push_eax ();
}

static cob_tree 
get_variable_item (cob_tree sy)
{
  cob_tree son, item;
  if (!SYMBOL_P (sy))
    return NULL;
  if (sy->occurs != NULL)
    return sy;
  for (son = sy->son; son != NULL; son = son->brother)
    if ((item = get_variable_item (son)))
      return item;
  return NULL;
}

static void
gen_temp_storage (int size)
{
  stackframe_cnt += 4;
  output ("\tpushl\t$tv_base%d+%d\n", pgm_segment, tmpvar_offset);
  tmpvar_offset += size;
  if (tmpvar_offset > tmpvar_max)
    tmpvar_max = tmpvar_offset;
}

static void
gen_loaddesc1 (cob_tree sy, int variable_length)
{
  cob_tree var;
  if (SUBREF_P (sy) || SUBSTRING_P (sy))
    {
      var = SUBREF_SYM (sy);
      if (SUBREF_P (var))
	var = SUBREF_SYM (var);
    }
  else
    {
      var = sy;
    }
  if (SUBSTRING_P (sy))
    {
      struct substring *rflp = SUBSTRING (sy);
      cob_tree len = rflp->len;
      if (len == NULL)
	{
	  output ("#  corrected length EOV\n");
	  value_to_eax (rflp->off);
	  output ("\tnegl\t%%eax\n");
	  output ("\taddl\t$%d, %%eax\n", symlen (var));
	  output ("\tincl\t%%eax\n");
	  output ("\tmovl\t%%eax, rf_base%d+%d\n",
		   pgm_segment, rflp->slot * 8);
	}
      else
	{
	  output ("#  corrected length %s\n", COB_FIELD_NAME (len));
	  if (LITERAL_P (len))
	    output ("\tmovl\t$%s, rf_base%d+%d\n",
		     COB_FIELD_NAME (len), pgm_segment, rflp->slot * 8);
	  else
	    {
	      value_to_eax (len);
	      output ("\tmovl\t%%eax, rf_base%d+%d\n",
		       pgm_segment, rflp->slot * 8);
	    }
	}
      output ("\tmovl\t$'%c', rf_base%d+%d\n", 'G',
	       pgm_segment, rflp->slot * 8 + 4);
      output ("\tmovl\t$rf_base%d+%d, %%eax\n",
	       pgm_segment, rflp->slot * 8);
    }
  else
    {
      /* adjust its length if there is a variable size item inside */
      if (variable_length && get_variable_item (sy) != NULL)
	{
	  adjust_desc_length (sy);
	}
      else
	{
#ifdef COB_DEBUG
	  output ("\tmovl\t%s, %%eax\t# descriptor of [%s]\n",
		   memrefd (var), COB_FIELD_NAME (var));
#else
	  output ("\tmovl\t%s, %%eax\n", memrefd (var));
#endif
	}
    }
  push_eax ();
}

static void
gen_loaddesc (cob_tree sy)
{
  gen_loaddesc1 (sy, 1);
}

void
gen_loadvar (cob_tree sy)
{
  if (sy == NULL)
    push_immed (0);
  else
    {
      gen_loadloc (sy);
      gen_loaddesc (sy);
    }
}

static void
value_to_eax (cob_tree x)
{
  char *s;

#ifdef COB_DEBUG
  output ("# value_to_eax: ");
  print_tree (x, o_src);
  fputs ("\n", o_src);
#endif

  if (x == NULL)
    {
      output ("\txorl\t%%eax,%%eax\n");
      return;
    }

  /* Subscript */
  if (EXPR_P (x))
    {
      value_to_eax (EXPR_LEFT (x));
      output ("\tpushl\t%%eax\n");
      do {
	cob_tree r = EXPR_RIGHT (x);
	value_to_eax (EXPR_P (r) ? EXPR_LEFT (r) : r);
	output ("\t%s\t%%eax,0(%%esp)\n",
		(EXPR_OP (x) == '+') ? "addl" : "subl");
	x = r;
      } while (EXPR_P (x));
      output ("\tpopl\t%%eax\n");
      return;
    }

  /* Literal */
  if (!SYMBOL_P (x))
    {
      /* if it's an integer, compute it now, not at runtime! */
      long long value = 0;
      long value2;
      /* integer's name is just it's value in ascii */
      s = COB_FIELD_NAME (x);
      while (*s)
	value = value * 10 + *s++ - '0';
      output ("#bef val\n");
      output ("\tmovl\t$%d,%%eax\n", (int) value);
      if ((value2 = value >> 32) != 0)
	output ("\tmovl\t$%d,%%edx\n", (int) value2);
      return;
    }

  /* Binary variable */
  if (COB_FIELD_TYPE (x) == 'B' || COB_FIELD_TYPE (x) == 'U')
    {
      /* load binary (comp) value directly */
      /* %eax doesn't hold greater than 4 bytes binary types
         so we use %edx to get the most significant part */
      if (symlen (x) > 4)
	{
	  output ("\tmovl\t%s+4, %%edx\n", memrefat (x));
	  output ("\tmovl\t%s, %%eax\n", memrefat (x));
	}
      else
	{
	  output ("\t%s\t%s, %%eax\n", varsize_movl (x), memrefat (x));
	}
      return;
    }

  /* Other variable */
  {
    int stack_save = stackframe_cnt;
    stackframe_cnt = 0;
    asm_call_1 ("get_index", x);
    stackframe_cnt = stack_save;
  }
}

static void
cleanup_rt_stack ()
{
  /* generate stack cleanup only if there is something to clean */
  if (stackframe_cnt == 1)
    output ("\tpopl\t%%ecx\n");
  else if (stackframe_cnt)
    output ("\taddl\t$%d, %%esp\n", stackframe_cnt);
  stackframe_cnt = 0;
  if (need_desc_length_cleanup)
    {
      tmpvar_offset = 0;	/* reuse this storage area */
      need_desc_length_cleanup = 0;
    }
}

static void
asm_call (const char *name)
{
  output ("\tcall\t%s\n", name);
  cleanup_rt_stack ();
}

static void
asm_call_1 (const char *name, cob_tree s1)
{
  gen_loadvar (s1);
  asm_call (name);
}

static void
asm_call_2 (const char *name, cob_tree s1, cob_tree s2)
{
  gen_loadvar (s2);
  gen_loadvar (s1);
  asm_call (name);
}

static void
asm_call_3 (const char *name, cob_tree s1, cob_tree s2, cob_tree s3)
{
  gen_loadvar (s3);
  gen_loadvar (s2);
  gen_loadvar (s1);
  asm_call (name);
}

static void
push_index (cob_tree sy)
{
  asm_call_1 ("get_index", sy);
  push_eax ();
}

static void
adjust_desc_length (cob_tree sy)
{
  int stack_save = stackframe_cnt;
  cob_tree item;
  stackframe_cnt = 0;
  item = get_variable_item (sy);
  //push_immed(0);
  gen_temp_storage (sizeof (struct fld_desc));
  gen_loaddesc1 (item, 0);
  gen_loaddesc1 (sy, 0);
  push_immed (item->occurs->max);
  push_immed (item->occurs->min);
  gen_loadvar (item->occurs->depend);
  asm_call ("cob_adjust_length");
  stackframe_cnt = stack_save;
  need_desc_length_cleanup = 1;
}


/*
 * Expression
 */

int
push_expr (cob_tree sy)
{
  if (!is_valid_expr (sy))
    {
      puts ("type error");
      abort ();
    }

  if (EXPR_P (sy))
    {
      push_expr (EXPR_LEFT (sy));
      push_expr (EXPR_RIGHT (sy));
      switch (EXPR_OP (sy))
	{
	case '+': asm_call ("cob_add"); break;
	case '-': asm_call ("cob_sub"); break;
	case '*': asm_call ("cob_mul"); break;
	case '/': asm_call ("cob_div"); break;
	case '^': asm_call ("cob_pow"); break;
	}
      return 1;
    }

  asm_call_1 ("cob_push_decimal", sy);
  return 1;
}

void
assign_expr (cob_tree sy, int rnd)
{
  push_immed (rnd);
  gen_loadvar (sy);
  asm_call ("cob_set");
}


/*
 * Condition
 */

static void
gen_not (void)
{
  int i = loc_label++;
  int j = loc_label++;
  output ("\tjz\t.L%d\n", i);
  output ("\txorl\t%%eax,%%eax\n");
  output ("\tjmp\t.L%d\n", j);
  output (".L%d:\tincl\t%%eax\n", i);
  output ("\t.align 16\n");
  output (".L%d:\n", j);
}

int
gen_orstart (void)
{
  int i = loc_label++;
  output ("\tjz\t.L%d\n", i);
  return i;
}

static void
gen_condvar (cob_tree sy)
{
  struct vrange *vr;
  cob_tree sy1 = sy;
  if (SUBREF_P (sy))
    sy1 = SUBREF_SYM (sy);
  push_immed (0);
  gen_loadvar (sy1->value2);
  gen_loadvar (sy1->value);
  vr = sy1->substring_redef.vr;
  while (vr)
    {
      gen_loadvar (vr->value2);
      gen_loadvar (vr->value);
      vr = vr->next;
    }
  if (SUBREF_P (sy))
    gen_loadvar (make_subref (sy1->parent, SUBREF_SUBS (sy)));
  else
    gen_loadvar (sy->parent);
  asm_call ("check_condition");
  output ("\tand\t%%eax,%%eax\n");
}

static void
gen_compare (cob_tree s1, int op, cob_tree s2)
{
  if (EXPR_P (s1) || EXPR_P (s2))
    {
      push_expr (s2);
      push_expr (s1);
      asm_call ("cob_cmp");
    }
  else
    {
      asm_call_2 ("cob_compare", s1, s2);
    }

  switch (op)
    {
    case COND_EQ:
      output ("\tand\t%%eax,%%eax\n");	/* equal */
      break;
    case COND_LT:
      output ("\tinc\t%%eax\n");	/* less */
      break;
    case COND_LE:
      output ("\tdec\t%%eax\n");	/* less or equal */
      gen_not ();
      break;
    case COND_GT:
      output ("\tdec\t%%eax\n");	/* greater */
      break;
    case COND_GE:
      output ("\tinc\t%%eax\n");	/* greater or equal */
      gen_not ();
      break;
    case COND_NE:
      output ("\tand\t%%eax,%%eax\n");	/* not equal */
      gen_not ();
      break;
    }
}

void
gen_condition (cob_tree cond)
{
  cob_tree l = COND_LEFT (cond);
  cob_tree r = COND_RIGHT (cond);
  enum cond_type type = COND_TYPE (cond);

  switch (type)
    {
    case COND_NUMERIC:
      asm_call_1 ("cob_is_numeric", l);
      output ("\tdecl\t%%eax\n");
      break;
    case COND_ALPHABETIC:
      asm_call_1 ("cob_is_alphabetic", l);
      output ("\tdecl\t%%eax\n");
      break;
    case COND_LOWER:
      asm_call_1 ("cob_is_lower", l);
      output ("\tdecl\t%%eax\n");
      break;
    case COND_UPPER:
      asm_call_1 ("cob_is_upper", l);
      output ("\tdecl\t%%eax\n");
      break;
    case COND_POSITIVE:
      gen_compare (l, COND_GT, spe_lit_ZE);
      break;
    case COND_NEGATIVE:
      gen_compare (l, COND_LT, spe_lit_ZE);
      break;
    case COND_ZERO:
      gen_compare (l, COND_EQ, spe_lit_ZE);
      break;
    case COND_NOT:
      gen_condition (l);
      gen_not ();
      return;
    case COND_AND:
    case COND_OR:
      {
	int lab = loc_label++;
	gen_condition (l);
	if (type == COND_AND)
	  output ("\tjnz\t.L%d\n", lab);
	else
	  output ("\tjz\t.L%d\n", lab);
	gen_condition (r);
	output (".L%d:\n", lab);
      }
      return;
    case COND_VAR:
      gen_condvar (l);
      return;
    default:
      gen_compare (l, type, r);
      return;
    }
  output ("\tand\t%%eax,%%eax\n");
}


/*
 *	Code Generating Routines
 */

static void
emit_lit (char *s, int len)
{
  int bcnt = 0;
  while (len--)
    if (!(bcnt++ % 8))
      {
	if (bcnt > 1)
	  putc ('\n', o_src);
	output ("\t.byte\t%d", *s++);
      }
    else
      {
	output (",%d", *s++);
      }
}

void
stabs_line ()
{
  static int label = 1;
  static char *last_filename = NULL;

  if (last_filename != cob_orig_filename)
    output (".stabs\t\"%s\",132,0,0,.LM%d\n", cob_orig_filename, label);
  output (".stabn\t68,0,%d,.LM%d-Ltext_%s\n", cob_orig_lineno, label, pgm_label);
  output (".LM%d:\n", label++);

  last_filename = cob_orig_filename;
}

void
data_trail (void)
{
  if (substring_slots > 0)
    output ("rf_base%d:\t.space\t%d\n", pgm_segment, substring_slots * 8);
}

int
adjust_linkage_vars (int start_offset)
{
  cob_tree sy, sy1;
  int i;
  int offset = start_offset;

  for (i = 0; i < HASHLEN; i++)
    for (sy1 = vartab[i]; sy1; sy1 = COB_FIELD_NEXT (sy1))
      for (sy = sy1; sy; sy = sy->clone)
	if (sy->parent == NULL && sy->linkage_flg == 1)
	  {
	    sy->linkage_flg = -offset;
	    offset += 4;
	  }

  return offset;
}

void
gen_store_fnres (cob_tree sy)
{
  if (sy == NULL)
    return;
  switch (COB_FIELD_TYPE (sy))
    {
    case 'B':
      switch (symlen (sy))
	{
	case 4:
	  output ("\tmovl\t%%eax, %s\n", memrefat (sy));
	  break;
	case 2:
	  output ("\tmov\t%%ax, %s\n", memrefat (sy));
	  break;
	};
      break;
    default:
      break;
    };
}


static int
get_nb_fields (cob_tree sy, int times, int sw_val)
{
  int nb_fields = 0;
  int type = COB_FIELD_TYPE (sy);

  if (type == 'G')
    {
      cob_tree tmp;
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	{
	  int tmpnf = tmp->times * get_nb_fields (tmp, times, sw_val);
	  if (tmp->redefines == NULL)
	    nb_fields += tmpnf;
	}
    }
  else
    {
      // A type C is considered for the moment as
      // non homogenous
      nb_fields = 1;
      if (type == 'C')
	init_ctype = '&';
      else if (type == '9' && sy->picstr[0] == 'S')
	init_ctype = '&';
      else if (init_ctype == ' ')
	init_ctype = type;
      else if (init_ctype != type)
	init_ctype = '&';
      if (sw_val == 1)
	{
	  int val = ((sy->value == NULL) ? 0 :
		     (sy->value == spe_lit_ZE) ? 2 :
		     (sy->value == spe_lit_SP) ? 3 :
		     (sy->value == spe_lit_LV) ? 4 :
		     (sy->value == spe_lit_HV) ? 5 : 1);
	  if (init_val == -1)
	    init_val = val;
	  if (type == init_ctype && val != init_val)
	    init_ctype = '&';
	}
    }
  return nb_fields * times;
}

static cob_tree 
get_init_symbol (char type)
{
  switch (type)
    {
    case '9': case 'C':
      return spe_lit_ZE;
    case 'B':
      return spe_lit_LV;
    default:
      return spe_lit_SP;
    }
}

static unsigned int
initialize_values_1 (cob_tree sy, unsigned int loc)
{
  int i;

  if (COB_FIELD_TYPE (sy) == 'G')
    {
      cob_tree p;
      for (i = 0; i < sy->times; i++)
	for (p = sy->son; p != NULL; p = p->brother)
	  loc = initialize_values_1 (p, loc);
    }
  else
    {
      if (!sy->flags.in_redefinition)
	for (i = 0; i < sy->times; i++)
	  {
	    if (sy->value != NULL)
	      {
		unsigned saved_loc = sy->location;
		sy->location = loc;
		gen_move (sy->value, sy);
		sy->location = saved_loc;
	      }
	    loc += symlen (sy);
	  }
    }
  return loc;
}

static void
initialize_values (void)
{
  cob_tree sy, sy1;
  int i;
  int nb_fields;

  for (i = 0; i < HASHLEN; i++)
    for (sy1 = vartab[i]; sy1; sy1 = COB_FIELD_NEXT (sy1))
      for (sy = sy1; sy != NULL; sy = sy->clone)
	{
	  int type = COB_FIELD_TYPE (sy);
	  if (type == 'F' || type == 'R' || type == 'K'
	      || type == 'J' || type == '8')
	    continue;
	  if (sy->level != 1 && sy->level != 77)
	    continue;
	  // for the time being spec_value will be true if any value encountered;
	  // later on it will contain only non standard values
	  if (!sy->flags.value)
	    continue;
	  if (sy->level == 77 || (sy->level == 1 && sy->son == NULL))
	    {
	      if (sy->value)
		gen_move (sy->value, sy);
	      continue;
	    }
	  init_ctype = ' ';
	  init_val = -1;
	  nb_fields = get_nb_fields (sy, sy->times, 1);
	  if (init_ctype != '&' && init_val != 1)
	    gen_move (get_init_symbol (init_ctype), sy);
	  else
	    initialize_values_1 (sy, sy->location);
	}
}

static void
dump_working ()
{

  cob_tree v, sy;
  struct list *list;
  int fld_len;
  int stabs_type = '3';
  int cur_sec_no = SEC_DATA;

  output ("w_base%d:\n", pgm_segment);
  for (list = fields_list; list != NULL; list = list->next)
    {
      v = list->var;
      sy = v;
      if (!SYMBOL_P (v))
	continue;
      if (v->sec_no == SEC_STACK)
	continue;
      if (COB_FIELD_TYPE (v) == 'F' || COB_FIELD_TYPE (v) == 'R')
	continue;
      fld_len = set_field_length (v, 1);
      if (v->sec_no != cur_sec_no && v->sec_no >= SEC_FIRST_NAMED)
	{			// switch of sections
	  if (v->sec_no >= SEC_FIRST_NAMED)
	    output ("\t.comm\t%s,%d,4\n",
		     sec_name (v->sec_no), fld_len);
	  else
	    output (".text\n");
	  cur_sec_no = v->sec_no;
	}
#ifdef COB_DEBUG
      output ("# FIELD %s, Data Loc: %d(hex: %x) %c\n",
	       COB_FIELD_NAME (v), v->location, v->location,
	       COB_FIELD_TYPE (v));
#endif
      if (cob_stabs_flag)
	switch (COB_FIELD_TYPE (sy))
	  {
	  case 'B':
	    switch (symlen (sy))
	      {
	      case 1:
		stabs_type = '6';
		break;
	      case 2:
		stabs_type = '5';
		break;
	      case 4:
		stabs_type = '3';
		break;
	      case 8:
		stabs_type = '7';
		break;
	      }
	    output (".stabs\t\"%s:S%c\",38,0,0,w_base%d+%d\n",
		     COB_FIELD_NAME (sy), stabs_type, pgm_segment,
		     sy->location);
	    break;

	  case 'C':
	    output (".stabs\t\"%s:S(1,%d)=ar3;1;%d;4\",38,0,0,w_base%d+%d\n",
		     COB_FIELD_NAME (sy), sy->len, sy->len, pgm_segment, 0);
	    break;

	  default:
	    output (".stabs\t\"%s:S(1,%d)=ar3;1;%d;2\",38,0,0,w_base%d+%d\n",
		     COB_FIELD_NAME (sy), sy->len, sy->len, pgm_segment,
		     sy->location);
	    break;
	  }

      if (v->parent)
	continue;
      if (fld_len)
	{			/* don't alloc dummy (zero storage) symbols */
	  output ("\t.space\t%d\n", fld_len);
	}
      if (fld_len == 0)
	yyerror ("Invalid picture in %s", COB_FIELD_NAME (v));
    }
  /* output tmpvar storage */
  if (tmpvar_max > 0)
    {
      output ("tv_base%d:\n", pgm_segment);
      output ("\t.space\t%d\n", tmpvar_max);
    }
}

void
proc_header (int using)
{
  cob_tree sy, sy1;
  int i;
  int stabs_type = '3';
  chg_underline (program_id);
  if (using || cob_module_flag)
    {
      pgm_label = program_id;
    }
  if (!pgm_segment)
    {
      if (cob_stabs_flag)
	{
	  output (".stabs\t\"%s\",100,0,0,Ltext_%s\n",
		   cob_source_filename, pgm_label);
	  output (".stabs\t\"%s:F1\",36,0,0,%s\n",
		   pgm_label, pgm_label);
	  output (".stabs\t\"display:t2=r2;0;255;\",128,0,0,0\n");
	  output (".stabs\t\"comp:t3=r3;-2147483648;2147483647;\",128,0,0,0\n");
	  output (".stabs\t\"comp3:t4=r3;0;255;\",128,0,0,0\n");
	  output (".stabs\t\"compw:t5=r5;-32768;32767;\",128,0,0,0\n");
	  output (".stabs\t\"compb:t6=r6;-128;127;\",128,0,0,0\n");
	  /* compll (comp with 8 bytes size) is wrong. Use a dump instead */
	  output (".stabs\t\"compll:t7=r(0,1);0;01777777777777777777777\",128,0,0,0\n");
	}
      output ("\t.version\t\"01.01\"\n");
      output ("cobc_compiled.:\n");
    }

  output (".text\n");
  output ("Ltext_%s:\n", pgm_label);
  if (!pgm_segment)
    {
      if (cob_stabs_flag)
	output (".stabs\t\":t1\",128,0,0,0\n");
      output ("\t.align 16\n");
    }
  output (".globl %s\n", pgm_label);
  output ("\t.type\t%s,@function\n", pgm_label);
  output ("%s:\n", pgm_label);
  output ("\tpushl\t%%ebp\n\tmovl\t%%esp, %%ebp\n");
  if (stack_offset & 1)
    stack_offset++;

  /*  
     Extra 16 bytes holds search all temporary data 
     EOT switch, min, max boundaries and saved ebx.
     Note: extra 4 bytes is to remove memory corruption problem
     found in test20c.cob, probably due to boundary alignment problem. 
   */
  stack_offset += START_STACK_ADJUST;
  /* add space for linkage section variables that are
     not arguments of the calling program */
  stack_offset += adjust_linkage_vars (START_STACK_ADJUST);

  output ("\tsubl\t$%u, %%esp\n", stack_offset);
  output ("\tmovl\t%%ebx, -%d(%%ebp)\n", stack_offset - 16);
  output (".Linit_%s:\n", pgm_label);
  output ("\tmovl\t$s_base%d+0, %%eax\n", pgm_segment);
  output ("\tcmpl\t$0, 0(%%eax)\n");
  output ("\tjne\t.Linite_%s\n", pgm_label);
  output ("\tmovl\t$1, 0(%%eax)\n");

  /********** initialize all VALUES of fields **********/
  initialize_values ();

  /********** dump stabs for local variables **********/
  if (cob_stabs_flag)
    for (i = 0; i < HASHLEN; i++)
      for (sy1 = vartab[i]; sy1; sy1 = COB_FIELD_NEXT (sy1))
	for (sy = sy1; sy; sy = sy->clone)
	  if (sy->sec_no == SEC_STACK)
	    switch (COB_FIELD_TYPE (sy))
	      {
	      case '8': case 'F': case 'K': case 'J':
		/* do nothing */
		break;
	      case 'B':
		switch (symlen (sy))
		  {
		  case 1: stabs_type = '6'; break;
		  case 2: stabs_type = '5'; break;
		  case 4: stabs_type = '3'; break;
		  case 8: stabs_type = '7'; break;
		  }
		output (".stabs\t\"%s:%c\",128,0,0,-%d\n",
			 COB_FIELD_NAME (sy), stabs_type, sy->location);
		break;
	      case 'C':
		output (".stabs\t\"%s:(1,%d)=ar3;1;%d;4\",128,0,0,-%d\n",
			COB_FIELD_NAME (sy), sy->len, sy->len, sy->location);
		break;
	      default:
		output (".stabs\t\"%s:(1,%d)=ar3;1;%d;2\",128,0,0,-%d\n",
			COB_FIELD_NAME (sy), sy->len, sy->len, sy->location);
	      }

  output (".Linite_%s:\n", pgm_label);
  output ("\tleal\t%s, %%eax\n", pgm_label);
  output ("\tpushl\t%%eax\n");
  output ("\tleal\t.Lend_pgm_%s, %%eax\n", pgm_label);
  output ("\tpushl\t%%eax\n");
  stack_offset += 8;		// length of the 2 pushes above
  if (!decimal_comma)
    {
      output ("\txorl\t%%eax,%%eax\n");
      output ("\tmovl\t%%eax,decimal_comma\n");
    }
  if (currency_symbol != '$')
    output ("\tmovb\t$%d,cCurrencySymbol\n", currency_symbol);
  at_procedure++;
}

static void
dump_alternate_keys (cob_tree r, struct alternate_list *alt)
{
  cob_tree key;
  for (; alt; alt = alt->next)
    {
      key = alt->key;
      output ("# alternate key %s\n", COB_FIELD_NAME (key));
      output ("\t.word\t%d\n\t.long\tc_base%d+%d\n\t.word\t%d\n\t.long\t0\n",
	      key->location - r->location, pgm_segment,
	      key->descriptor, alt->duplicates);
    }
  output ("# end of alternate keys\n.word\t-1\n");
}

static void
dump_fdesc ()
{
  cob_tree f;
  cob_tree r;
  struct list *list /*,*visited */ ;
  unsigned char fflags;

  output ("s_base%d:\t.long\t0\n", pgm_segment);
  for (list = files_list; list != NULL; list = list->next)
    {
      f = list->var;
      r = f->recordsym;
#ifdef COB_DEBUG
      output ("# FILE DESCRIPTOR, File: %s, Record: %s, Data Loc: %d(hex: %x), opt: %x\n",
	      COB_FIELD_NAME (f), COB_FIELD_NAME (r),
	      f->location, f->location, f->flags.optional);
#endif
      if (f->filenamevar == NULL)
	{
	  yyerror ("No file name assigned to %s.\n", COB_FIELD_NAME (f));
	  continue;
	}
      if (COB_FIELD_TYPE (f) == 'K')
	{
	  output ("\t.extern\t_%s:far\n", COB_FIELD_NAME (f));
	  continue;
	}
      if (COB_FIELD_TYPE (f) == 'J')
	{
	  output ("\tpublic\t_%s\n", COB_FIELD_NAME (f));
	  output ("_%s\tlabel\tbyte\n", COB_FIELD_NAME (f));
	}
      fflags = f->flags.optional;
      output ("\t.byte\t%u\n", RTL_FILE_VERSION);
      output ("\t.long\tc_base%d+%u\n",
	       pgm_segment, f->filenamevar->descriptor);
      output ("\t.long\t%d\n", r->len);
      output ("\t.byte\t%d,%d\n", f->organization, f->access_mode);
      output ("\t.long\t0\n");	/* open_mode */
      output ("\t.long\t0\n");	/* struct DBT (libdb) */
      output ("\t.long\t0\n");	/* start_record */
      output ("\t.byte\t%x\n", fflags);	/* flags */
      if (f->organization == 1)
	{			/* indexed file */
	  if (f->ix_desc)
	    {
	      output ("\t.word\t%d\n\t.long\tc_base%d+%d\n",
		       f->ix_desc->location - r->location,
		       pgm_segment, f->ix_desc->descriptor);
	    }
	  else
	    {
	      /* no key field was given for this file */
	      output ("\t.word\t0\n\t.long\t0\n");
	    }
	  output ("\t.long\t0\n");	/* struct altkey_desc *key_in_use */
	  dump_alternate_keys (r, (struct alternate_list *) f->alternate);
	}
    }
}

void
proc_trail (int using)
{
  int i;
  struct list *list;
  cob_tree sy;
  /*char s[9]; */
  char *pgm_label = "main";
  char flag;

  if (using || cob_module_flag)
    {
      pgm_label = program_id;
    }
  output (".Lend_pgm_%s:\n", pgm_label);

  //      Screen section io cleanup (curses library).
  if (screen_io_enable != 0)
    asm_call ("do_scrio_finish");

  asm_call ("cob_exit");

//      Program return code is stored in register %eax
//      Note:
//        The variable RETURN-CODE is a extention to the 
//        standard, since ANSI COBOL 85 does not support it.

  if ((sy = lookup_symbol (SVAR_RCODE)) == NULL)
    {
      output ("\tmovl\t$0, %%eax\n");
    }
  else
    {
      if (sy->sec_no == SEC_STACK)
	output ("\tleal\t-%d(%%ebp), %%edx\n", sy->location);
      else
	output ("\tleal\tw_base%d+%d, %%edx\n",
		 pgm_segment, sy->location);
      output ("\tmovl\t(%%edx), %%eax\n");
    }

  output ("\tjmp\t.LSend_%s\n", pgm_label);
  output ("\t.align 16\n");
  output (".LSend_%s:\n", pgm_label);

  output ("\tmovl\t-%d(%%ebp), %%ebx\n", stack_offset - 8 - 16);
  output ("\tmov\t%%ebp,%%esp\n");
  output ("\tpopl\t%%ebp\n");
  output ("\tret\n");

  /********** generate .Lfe statement   ************/
  output (".Lfe1_%s:\n", pgm_label);
  output ("\t.size\t%s,.Lfe1_%s-%s\n",
	   pgm_label, pgm_label, pgm_label);


  /********** generate data for literals & fields ************/
  output (".data\n\t.align 4\n");

  /* generate static working storage */
  dump_working ();

  /* predefined data for special literals */
  output ("v_base%d:\nc_base%d:\n", pgm_segment, pgm_segment);

  /**************** generate data for fields *****************/
  for (list = fields_list; list != NULL; list = list->next)
    {
      if (COB_FIELD_TYPE (list->var) == 'F')
	{			/* sort files */
	  char sl[21];		/* para inverter a lista */
	  char *s;
	  s = sl;
	  *s++ = 0;		/* final da lista invertida */
	  sy = list->var;
#ifdef COB_DEBUG
	  output ("# File: %s, Data loc: v_base+%d, Desc: c_base%d+%d\n",
		  COB_FIELD_NAME (sy), sy->location, pgm_segment,
		  sy->descriptor);
#endif
	  sy = sy->sort_data;
	  while (sy != NULL)
	    {
	      *s++ = (unsigned char) sy->direction;
	      *s++ = (unsigned char) sy->len;
	      sy = (sy->sort_data);
	    }
	  s--;
	  while (*s)
	    {
	      output ("\t.byte\t%u,%u\n", *s--, *s--);
	    }
	  output ("\t.byte\t0\n");
	}
      else if (!SYMBOL_P (list->var))
	{
	  /***** it is a literal *****/
	  int len, tmplen;
	  struct lit *v = LITERAL (list->var);
	  len = v->nick ? 1 : v->len;
#ifdef COB_DEBUG
	  output ("# Literal: %s, Data loc: c_base%d+%d, Desc: c_base+%d\n",
		  COB_FIELD_NAME (v), pgm_segment, v->location, v->descriptor);

#endif
	  if (!v->decimals)
	    {			/* print literal string, w/special chars */
	      int i;
	      char *s;
	      if (v->nick)
		{
		  s = v->nick;
		  i = 1;
		}
	      else
		{
		  s = COB_FIELD_NAME (v);
		  i = v->len;
		}
	      emit_lit (s, i);
	      if (i)
		output (",0\n");
	      else
		output ("\t.byte\t0\n");	/* null string? */
	    }
	  else
	    {
	      char *s;
	      s = COB_FIELD_NAME (v);
	      output ("\t.byte\t");
	      while (*s && (*s != decimal_char ()))
		output ("%d,", *s++);
	      s++;
	      while (*s)
		output ("%d,", *s++);
	      output ("0\n");
	    }
	  output ("\t.long\t%d\n", (v->decimals) ? len - 1 : len);
	  output ("\t.byte\t'%c',%d,%d\n",
		   COB_FIELD_TYPE (v), v->decimals, v->all);
	  output ("\t.long\tc_base%d+%d\n", pgm_segment, v->descriptor + 11);

	  if (v->decimals)
	    {
	      if (COB_FIELD_NAME (v)[v->len - 1] > '9')	/* signed too? */
		output ("\t.byte\t'S',1,'9',%d,'V',1,'9',%d,0\n",
			 len - v->decimals - 1, v->decimals);
	      else
		output ("\t.byte\t'9',%d,'V',1,'9',%d,0\n",
			 len - v->decimals - 1, v->decimals);
	    }
	  else if ((COB_FIELD_TYPE (v) == '9')
		   && (COB_FIELD_NAME (v)[v->len - 1] > '9'))
	    {
	      /* this is a signed literal, so reflect into its picture too */
	      output ("\t.byte\t'S',1,'9',%d,0\n", len);
	    }
	  else
	    {
	      tmplen = len;
	      while (tmplen > 255)
		{
		  output ("\t.byte\t\'%c\',%d\n",
			   COB_FIELD_TYPE (v), 255);
		  tmplen -= 255;
		}
	      output ("\t.byte\t\'%c\',%d,0\n",
		       COB_FIELD_TYPE (v), tmplen);

	    }
	}
      else
	{
	/********* it is a normal field ****************/
	  sy = list->var;
#ifdef COB_DEBUG
	  output ("# Field: %s, Mem loc: %s, Desc: c_base%d+%d\n",
		   COB_FIELD_NAME (sy), memref (sy), pgm_segment,
		   sy->descriptor);
#endif
	  if (sy->redefines != NULL)
	    sy->location = sy->redefines->location;

	  output ("\t.long\t%d\n", sy->len);

	  flag = sy->flags.just_r ? 2 : 0;
	  flag |= (sy->flags.separate_sign ? 4 : 0);
	  flag |= (sy->flags.leading_sign ? 8 : 0);
	  output ("\t.byte\t'%c',%d,%d\n",
		   COB_FIELD_TYPE (sy), sy->decimals, flag);
	  if (COB_FIELD_TYPE (sy) != 'G')
	    {
	      output ("\t.long\tc_base%d+%d\n", pgm_segment, sy->pic);
	      for (i = 0; i < strlen (sy->picstr); i += 2)
		output ("\t.byte\t\'%c\',%d\n",
			 *(sy->picstr + i),
			 *((unsigned char *) sy->picstr + i + 1));
	      output ("\t.byte\t0\n");
	    }
	}
    }
  /* generate data for files */
  dump_fdesc ();
  data_trail ();
  output ("\n\t.ident\t\"%s %s\"\n", COB_PACKAGE, COB_VERSION);
}

void
save_field_in_list (cob_tree sy)
{
  struct list *list;
  if (fields_list == NULL)
    {
      list = (struct list *) malloc (sizeof (struct list));
      last_field = fields_list = list;
      list->next = NULL;
      list->var = sy;
    }
  else
    {
      list = (struct list *) malloc (sizeof (struct list));
      list->var = sy;
      list->next = NULL;
      last_field->next = list;
      last_field = list;
    }
}

cob_tree
save_literal (cob_tree x, int type)
{
  char *s;
  char *dp;
  int piclen;
  struct lit *v = LITERAL (x);
  s = COB_FIELD_NAME (v);
  piclen = 3;			/* assume 'X'-only literal */
  if ((type == '9') && (s[v->len - 1] > '9'))
    {
      piclen += 2;		/* we need space for the sign picture char */
    }
  if (type != 'X' && (dp = strchr (s, decimal_char ())) != NULL)
    {
      piclen += 4;		/* reserve space for 'V' and decimal part */
      v->decimals = v->len - (int) (dp - s) - 1;
    }
  else
    v->decimals = 0;
  if (type == 'X' && v->len > 255)
    piclen += (v->len / 255) * 2;
  COB_FIELD_TYPE (v) = type;
	/****** save literal in fields list for later *******/
  save_field_in_list (x);
	/******** save address of const string ************/
  v->location = literal_offset;
  v->sec_no = SEC_CONST;
  if (v->decimals)
    literal_offset += v->len;
  /* it's already one chr larger (decimal point) */
  else
    literal_offset += v->len + 1;
	/******** save address of field descriptor ********/
  v->descriptor = literal_offset;
  literal_offset += 11 + piclen;
  return x;
}

void
put_disp_list (cob_tree sy)
{
  struct list *list, *tmp;
  list = (struct list *) malloc (sizeof (struct list));
  list->var = sy;
  list->next = NULL;
  if (disp_list == NULL)
    disp_list = list;
  else
    {
      tmp = disp_list;
      while (tmp->next != NULL)
	tmp = tmp->next;
      tmp->next = list;
    }
}

void
add_alternate_key (cob_tree sy, int duplicates)
{
  cob_tree f = curr_file;
  struct alternate_list *alt, *new;
  alt = (struct alternate_list *) f->alternate;
  new = malloc (sizeof (struct alternate_list));
  new->next = alt;
  new->key = sy;
  new->duplicates = duplicates;
  f->alternate = COB_TREE (new);
}


/*
 * Status handling
 */

static void
gen_init_status (void)
{
  output ("\tmovl\t$%d, cob_status\n", COB_STATUS_SUCCESS);
}

static void
gen_save_status (cob_tree f)
{
  if (f->parent)
    {
      gen_loadloc (f->parent);
      asm_call ("cob_save_status");
    }
}

int
gen_status_branch (int status, int flag)
{
  int lbl = loc_label++;

  output ("\tcmpl\t$%d, cob_status\n", status);
  if (flag)
    output ("\tje\t.L%d\n", lbl);
  else
    output ("\tjne\t.L%d\n", lbl);

  return lbl;
}


struct scr_info *
alloc_scr_info ()
{
  struct scr_info *new;
  new = malloc (sizeof (struct scr_info));
  new->attr = 0;
  new->line = 1;
  new->column = 1;
  new->foreground = 0;
  new->background = 7;
  new->from = NULL;
  new->to = NULL;
  return new;
}

struct inspect_before_after *
alloc_inspect_before_after (struct inspect_before_after *ba,
			    int before_after, cob_tree var)
{
  if (ba == NULL)
    {
      ba = malloc (sizeof (struct inspect_before_after));
      ba->before = ba->after = NULL;
    }
  if (before_after == 1)
    {				/* before given */
      if (ba->before)
	yyerror ("only one BEFORE phrase can be given");
      else
	ba->before = var;
    }
  else if (before_after == 2)
    {				/* after given */
      if (ba->after)
	yyerror ("only one AFTER phrase can be given");
      else
	ba->after = var;
    }
  return ba;
}

struct converting_struct *
alloc_converting_struct (cob_tree fromvar, cob_tree tovar,
			 struct inspect_before_after *ba)
{
  struct converting_struct *new;
  new = malloc (sizeof (struct converting_struct));
  new->fromvar = fromvar;
  new->tovar = tovar;
  new->before_after = ba;
  return new;
}

struct tallying_list *
alloc_tallying_list (struct tallying_list *tl, cob_tree count,
		     struct tallying_for_list *tfl)
{
  struct tallying_list *new;
  new = malloc (sizeof (struct tallying_list));
  new->next = tl;
  new->tflist = tfl;
  new->count = count;
  return new;
}

struct tallying_for_list *
alloc_tallying_for_list (struct tallying_for_list *tfl, int options,
			 cob_tree forvar, struct inspect_before_after *ba)
{
  struct tallying_for_list *new;
  new = malloc (sizeof (struct tallying_for_list));
  new->next = tfl;
  new->options = options;
  new->forvar = forvar;
  new->before_after = ba;
  return new;
}

struct replacing_list *
alloc_replacing_list (struct replacing_list *rl, int options,
		      struct replacing_by_list *rbl, cob_tree byvar,
		      struct inspect_before_after *ba)
{

  struct replacing_list *new;
  new = malloc (sizeof (struct replacing_list));
  new->next = rl;
  new->options = options;
  new->replbylist = rbl;
  new->byvar = byvar;
  new->before_after = ba;
  return new;
}

struct replacing_by_list *
alloc_replacing_by_list (struct replacing_by_list *rbl,
			 cob_tree replvar, cob_tree byvar,
			 struct inspect_before_after *ba)
{
  struct replacing_by_list *new;
  new = malloc (sizeof (struct replacing_by_list));
  new->next = rbl;
  new->replvar = replvar;
  new->byvar = byvar;
  new->before_after = ba;
  return new;
}


struct unstring_delimited *
alloc_unstring_delimited (int all, cob_tree var)
{
  struct unstring_delimited *ud;
  ud = malloc (sizeof (struct unstring_delimited));
  ud->next = NULL;
  ud->var = var;
  ud->all = all;
  return ud;
}

struct unstring_destinations *
alloc_unstring_dest (cob_tree var, cob_tree delim, cob_tree count)
{
  struct unstring_destinations *ud;
  ud = malloc (sizeof (struct unstring_destinations));
  ud->next = NULL;
  ud->var = var;
  ud->delim = delim;
  ud->count = count;
  return ud;
}

struct string_from *
alloc_string_from (cob_tree var, cob_tree delim)
{
  struct string_from *sf;
  sf = malloc (sizeof (struct string_from));
  sf->next = NULL;
  sf->var = var;
  sf->delim = delim;
  return sf;
}

void
gen_unstring (cob_tree var, struct unstring_delimited *delim,
	      struct unstring_destinations *dest, cob_tree ptr,
	      cob_tree tally)
{
  output ("# UNSTRING %s\n", COB_FIELD_NAME (var));
  push_immed (0);
  for (; dest; dest = dest->next)
    {
      gen_loadvar (dest->count);
      gen_loadvar (dest->delim);
      gen_loadvar (dest->var);
    }
  push_immed (0);
  for (; delim; delim = delim->next)
    {
      push_immed (delim->all);	/* push "all" flag */
      gen_loadvar (delim->var);
    }
  asm_call_3 ("cob_unstring", var, ptr, tally);
}

void
gen_string (struct string_from *sf, cob_tree sy, cob_tree ptr)
{
  output ("# STRING into %s\n", COB_FIELD_NAME (sy));
  push_immed (0);	/* mark the end of variables */
  for (; sf; sf = sf->next)
    {
      gen_loadvar (sf->delim);
      gen_loadvar (sf->var);
    }
  asm_call_2 ("cob_string", sy, ptr);
}

void
gen_display_screen (cob_tree sy, int main)
{
  cob_tree tmp;
  if (main)
    output ("# Screen Section: %s\n", COB_FIELD_NAME (sy));
  if (sy->son == NULL)
    {
      output ("# Screen Field: %s\n", COB_FIELD_NAME (sy));
      gen_loadvar (sy->scr->to);
      gen_loadvar (sy->scr->from);
      gen_loadvar (sy);
      push_immed (sy->scr->background);
      push_immed (sy->scr->foreground);
      push_immed (sy->scr->column);
      push_immed (sy->scr->line);
      push_immed (sy->scr->attr);
      asm_call ("cob_scr_process");
    }
  else
    {
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	gen_display_screen (tmp, 0);
    }
  if (main)
    {
      asm_call ("cob_display_screen");
      if (disp_list->next)
	yyerror ("we do not handle more than one screen");
      disp_list = disp_list->next;
    }
}

void
gen_display (int dupon, int nl)
{
  /*int len; */
  int dspflags;
  int first = 1;
  cob_tree sy;

  if (disp_list)
    {
      /* separate screen displays from display of regular variables */
      sy = disp_list->var;
      if (disp_list && !LITERAL_P (sy))
	if (!SUBSTRING_P (sy) && !SUBREF_P (sy) && sy->scr)
	  {
	    gen_display_screen (disp_list->var, 1);
	    return;
	  }
      /* continue w/a regular variable display */
      if (nl & 2)
	if (screen_io_enable == 0)
	  {
	    push_immed (dupon);
	    asm_call ("cob_display_erase");
	  }
    }
  while (disp_list)
    {
      sy = disp_list->var;

      if (screen_io_enable == 0)
	{
	  push_immed (dupon);
	  gen_loadvar (sy);
	  asm_call ("cob_display");
	}
      else
	{
	  dspflags = nl;
	  if (first)
	    first = 0;
	  else
	    dspflags &= ~2;	/* avoid erasing from now on */
	  if (disp_list->next != NULL)
	    dspflags |= 1;	/* allow newline only at the last item */
	  push_immed (dspflags);
	  gen_loadvar (sy);
	  asm_call ("display_curses");
	}
      disp_list = disp_list->next;
    }
  if (!(nl & 1))
    if (screen_io_enable == 0)
      {
	push_immed (dupon);
	asm_call ("cob_newline");
      }
}

void
gen_gotoxy_expr ()
{
  stackframe_cnt += 16;		/* eliminate the coords expressions */
  asm_call ("cob_goxy_expr");
}

void
gen_accept (cob_tree sy, int echo, int main)
{
  cob_tree tmp;
  if (sy->scr)
    {				/* screen or screen-item accept */
      if (main)
	output ("# Screen Section: %s\n", COB_FIELD_NAME (sy));
      if (sy->son == NULL)
	{
	  output ("# Screen Field: %s\n", COB_FIELD_NAME (sy));
	  gen_loadvar (sy->scr->to);
	  gen_loadvar (sy->scr->from);
	  gen_loadvar (sy);
	  push_immed (sy->scr->background);
	  push_immed (sy->scr->foreground);
	  push_immed (sy->scr->column);
	  push_immed (sy->scr->line);
	  push_immed (sy->scr->attr);
	  asm_call ("cob_scr_process");
	}
      else
	{
	  for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	    {
	      gen_accept (tmp, echo, 0);
	    }
	}
      if (main)
	asm_call ("cob_accept_screen");
    }
  else
    {
      push_immed (echo);
      output ("\tmovl\t$c_base%d+%u, %%eax\n", pgm_segment, sy->descriptor);
      push_eax ();
      gen_loadloc (sy);
      if (screen_io_enable == 0)
	asm_call ("accept_std");
      else
	asm_call ("accept_curses");
    }
}

void
gen_accept_from_time (cob_tree sy)
{
  gen_loadloc (sy);
  asm_call ("accept_time");
}

void
gen_accept_from_date (cob_tree sy)
{
  gen_loadloc (sy);
  asm_call ("accept_date");
}

void
gen_accept_from_day (cob_tree sy)
{
  gen_loadloc (sy);
  asm_call ("accept_day");
}

void
gen_accept_from_day_of_week (cob_tree sy)
{
  gen_loadloc (sy);
  asm_call ("accept_day_of_week");
}

void
gen_accept_from_inkey (cob_tree sy)
{
  gen_loadloc (sy);
  asm_call ("accept_inkey");
}

void
gen_accept_from_cmdline (cob_tree sy)
{

  cob_tree sy1;

  gen_loadvar (sy);
  output ("\tmovl\t12(%%ebp), %%eax\n");
  push_eax ();
  output ("\tmovl\t8(%%ebp), %%eax\n");
  push_eax ();
  asm_call ("accept_cmd_line");

//      Set RETURN-CODE with the value returned by 
//      the "accept_cmd_line" function, which is stored 
//      in register %eax

  if ((sy1 = lookup_symbol (SVAR_RCODE)) != NULL)
    {
      if (sy1->sec_no == SEC_STACK)
	{
	  output ("\tleal\t-%d(%%ebp), %%edx\n", sy1->location);
	}
      else
	{
	  output ("\tleal\tw_base%d+%d, %%edx\n",
		   pgm_segment, sy1->location);
	}
      output ("\tmovl\t%%eax, (%%edx)\n");
    }
}

void
gen_accept_env_var (cob_tree sy, cob_tree v)
{
  cob_tree sy2;

  gen_loadloc (v);
  asm_call_1 ("accept_env_var", sy);

//      Set RETURN-CODE with the value returned by 
//      the "accept_cmd_line" function, which is stored 
//      in register %eax

  if ((sy2 = lookup_symbol (SVAR_RCODE)) != NULL)
    {
      if (sy2->sec_no == SEC_STACK)
	output ("\tleal\t-%d(%%ebp), %%edx\n", sy2->location);
      else
	output ("\tleal\tw_base%d+%d, %%edx\n", pgm_segment, sy2->location);
      output ("\tmovl\t%%eax, (%%edx)\n");
    }
}

/******** structure allocation for perform info(s) ***********/

struct perf_info *
create_perf_info (cob_tree sy1, cob_tree sy2, unsigned long lj,
		  unsigned long le)
{
  struct perf_info *rf;
  rf = malloc (sizeof (struct perf_info));
  rf->pname1 = sy1;
  rf->pname2 = sy2;
  rf->ljmp = lj;
  rf->lend = le;
  return rf;
}

struct perform_info *
create_perform_info (void)
{
  struct perform_info *rf;
  rf = malloc (sizeof (struct perform_info));
  rf->pf[0] = NULL;
  rf->pf[1] = NULL;
  rf->pf[2] = NULL;
  rf->pf[3] = NULL;
  return rf;
}

char *
check_perform_variables (cob_tree sy1, struct perform_info *pi1)
{

  int i, j, k;

  j = 0;
  for (i = 0; i < 4; i++)
    if (pi1->pf[i] != NULL)
      j++;

  for (i = 0; i < j; i++)
    if (strcmp (COB_FIELD_NAME (sy1),
		COB_FIELD_NAME (pi1->pf[i]->pname2)) == 0)
      return COB_FIELD_NAME (sy1);

  for (i = 0; i < j; i++)
    for (k = i + 1; k < j; k++)
      if (strcmp (COB_FIELD_NAME (pi1->pf[i]->pname2),
		  COB_FIELD_NAME (pi1->pf[k]->pname2)) == 0)
	return COB_FIELD_NAME (pi1->pf[i]->pname2);

  return NULL;
}

/******** structure allocation for math verbs variables ***********/

struct math_var *
create_mathvar_info (struct math_var *mv, cob_tree sy, unsigned int opt)
{

  struct math_var *rf, *tmp1;

  rf = malloc (sizeof (struct math_var));
  rf->sname = sy;
  rf->rounded = opt;
  rf->next = NULL;

  if (mv == NULL)
    return rf;
  else
    {
      tmp1 = mv;
      while (tmp1->next != NULL)
	tmp1 = tmp1->next;
      tmp1->next = rf;
      return mv;
    }
}


/*
 * COMPUTE statement
 */

void
gen_compute (struct math_var *vl1, cob_tree sy1)
{
  gen_init_status ();
  for (; vl1; vl1 = vl1->next)
    {
      push_expr (sy1);
      assign_expr (vl1->sname, vl1->rounded);
    }
}


/*
 * ADD statement
 */

void
gen_add (cob_tree n1, cob_tree n2, int rnd)
{
  push_expr (n2);
  push_expr (n1);
  asm_call ("cob_add");
  assign_expr (n2, rnd);
}

void
gen_add_to (cob_tree_list nums, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      cob_tree_list l;
      push_expr (list->sname);
      for (l = nums; l; l = l->next)
	{
	  push_expr (l->tree);
	  asm_call ("cob_add");
	}
      assign_expr (list->sname, list->rounded);
    }
}

void
gen_add_giving (cob_tree_list nums, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      cob_tree_list l = nums;
      push_expr (l->tree);
      for (l = l->next; l; l = l->next)
	{
	  push_expr (l->tree);
	  asm_call ("cob_add");
	}
      assign_expr (list->sname, list->rounded);
    }
}


/*
 * SUBTRACT statement
 */

void
gen_sub (cob_tree n1, cob_tree n2, int rnd)
{
  push_expr (n2);
  push_expr (n1);
  asm_call ("cob_sub");
  assign_expr (n2, rnd);
}

void
gen_subtract_from (cob_tree_list subtrahend_list, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      cob_tree_list l;
      push_expr (list->sname);
      for (l = subtrahend_list; l; l = l->next)
	{
	  push_expr (l->tree);
	  asm_call ("cob_sub");
	}
      assign_expr (list->sname, list->rounded);
    }
}

void
gen_subtract_giving (cob_tree_list subtrahend_list, cob_tree minuend,
		     struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      cob_tree_list l;
      push_expr (minuend);
      for (l = subtrahend_list; l; l = l->next)
	{
	  push_expr (l->tree);
	  asm_call ("cob_sub");
	}
      assign_expr (list->sname, list->rounded);
    }
}


/*
 * MULTIPLY statement
 */

void
gen_multiply_by (cob_tree multiplicand, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      push_expr (multiplicand);
      push_expr (list->sname);
      asm_call ("cob_mul");
      assign_expr (list->sname, list->rounded);
    }
}

void
gen_multiply_giving (cob_tree multiplicand, cob_tree multiplier,
		     struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      push_expr (multiplicand);
      push_expr (multiplier);
      asm_call ("cob_mul");
      assign_expr (list->sname, list->rounded);
    }
}


/*
 * DIVIDE statement
 */

void
gen_divide_into (cob_tree divisor, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      push_expr (list->sname);
      push_expr (divisor);
      asm_call ("cob_div");
      assign_expr (list->sname, list->rounded);
    }
}

void
gen_divide_giving (cob_tree divisor, cob_tree dividend, struct math_var *list)
{
  gen_init_status ();
  for (; list; list = list->next)
    {
      push_expr (dividend);
      push_expr (divisor);
      asm_call ("cob_div");
      assign_expr (list->sname, list->rounded);
    }
}

void
gen_divide_giving_remainder (cob_tree divisor, cob_tree dividend,
			     cob_tree quotient, cob_tree remainder, int rnd)
{
  gen_init_status ();
  push_expr (dividend);
  push_expr (divisor);
  push_immed (rnd);
  asm_call_2 ("cob_divide_remainder", quotient, remainder);
}


/*
 * INSPECT statement
 */

void
gen_inspect (cob_tree var, void *list, int operation)
{
  /*struct inspect_before_after *ba,*ba1; */
  struct tallying_list *tl;
  struct tallying_for_list *tfl;
  struct replacing_list *rl;
  struct replacing_by_list *rbl;
  struct converting_struct *cv;

  if (!operation)
    {
      if (!list)
	return;
      push_immed (0);
      for (tl = (struct tallying_list *) list; tl; tl = tl->next)
	{
	  push_immed (0);
	  for (tfl = tl->tflist; tfl; tfl = tfl->next)
	    {
	      gen_loadvar (tfl->before_after->after);
	      gen_loadvar (tfl->before_after->before);
	      if (tfl->options != INSPECT_CHARACTERS)
		gen_loadvar (tfl->forvar);
	      push_immed (tfl->options);
	    }
	  gen_loadvar (tl->count);
	}
      asm_call_1 ("cob_inspect_tallying", var);
    }
  else if (operation == 1)
    {
      if (!list)
	return;
      push_immed (0);
      for (rl = (struct replacing_list *) list; rl; rl = rl->next)
	{
	  if (rl->options == INSPECT_CHARACTERS)
	    {
	      gen_loadvar (rl->before_after->after);
	      gen_loadvar (rl->before_after->before);
	      gen_loadvar (rl->byvar);
	      push_immed (rl->options);
	    }
	  else
	    {
	      
	      for (rbl = rl->replbylist; rbl; rbl = rbl->next)
		{
		  gen_loadvar (rbl->before_after->after);
		  gen_loadvar (rbl->before_after->before);
		  gen_loadvar (rbl->byvar);
		  gen_loadvar (rbl->replvar);
		  push_immed (rl->options);
		}
	    }
	}
      asm_call_1 ("cob_inspect_replacing", var);
    }
  else
    {
      cv = (struct converting_struct *) list;
      gen_loadvar (cv->before_after->after);
      gen_loadvar (cv->before_after->before);
      gen_loadvar (cv->tovar);
      gen_loadvar (cv->fromvar);
      gen_loadvar (var);
      asm_call ("cob_inspect_converting");
    }
}


/*
 * MOVE statement
 */

static void
gen_move_1 (cob_tree src)
{
  if (src == spe_lit_ZE)
    asm_call ("cob_move_zero");
  else if (src == spe_lit_SP)
    asm_call ("cob_move_space");
  else
    asm_call_1 ("cob_move", src);
}

void
gen_move (cob_tree src, cob_tree dst)
{
  gen_loadvar (dst);
  gen_move_1 (src);
}

/* The following functions will be activated when we change from
   defining the outermost group to define each elementary item. */
void
gen_move_corresponding (cob_tree sy1, cob_tree sy2)
{
  cob_tree t1, t2;
  if (!(SYMBOL_P (sy1) && SYMBOL_P (sy2)))
    {
      yyerror ("sorry we don't handle this case yet!");
      return;
    }

  for (t1 = sy1->son; t1 != NULL; t1 = t1->brother)
    if (!t1->redefines && t1->times == 1)
      for (t2 = sy2->son; t2 != NULL; t2 = t2->brother)
	if (!t2->redefines && t2->times == 1)
	  if (strcmp (COB_FIELD_NAME (t1), COB_FIELD_NAME (t2)) == 0)
	    {
	      if (COB_FIELD_TYPE (t1) != 'G' || COB_FIELD_TYPE (t2) != 'G')
		gen_move (t1, t2);
	      else
		gen_move_corresponding (t1, t2);
	    }
}


/*
 * INITIALIZE statement
 */

static void
gen_initialize_1 (cob_tree sy)
{
  if (!sy->flags.in_redefinition)
    {
      if (COB_FIELD_TYPE (sy) == 'G')
	{
	  int lab = 0;
	  cob_tree p;
	  if (sy->times != 1)
	    {
	      lab = loc_label++;
	      output ("\tpopl\t%%eax\n");
	      output ("\tpushl\t%%ebx\n");
	      output ("\tpushl\t%%eax\n");
	      output ("\tmovl\t$%d, %%ebx\n", sy->times);
	      output (".L%d:\n", lab);
	    }
	  for (p = sy->son; p; p = p->brother)
	    gen_initialize_1 (p);
	  if (sy->times != 1)
	    {
	      output ("\tdecl\t%%ebx\n");
	      output ("\tjnz\t.L%d\n", lab);
	      output ("\tpopl\t%%eax\n");
	      output ("\tpopl\t%%ebx\n");
	      output ("\tpushl\t%%eax\n");
	    }
	}
      else
	{
	  int i;
	  for (i = 0; i < sy->times; i++)
	    {
	      gen_loaddesc (sy);
	      gen_move_1 (get_init_symbol (COB_FIELD_TYPE (sy)));
	      output ("\taddl\t$%d, 0(%%esp)\n", symlen (sy));
	    }
	}
    }
}

void
gen_initialize (cob_tree sy)
{
  cob_tree sy1 = sy;
  if (SUBREF_P (sy))
    sy1 = SUBREF_SYM (sy);

  init_ctype = ' ';
  get_nb_fields (sy1, sy1->times, 0);
  if (init_ctype != '&')
    gen_move (get_init_symbol (init_ctype), sy);
  else
    {
      loadloc_to_eax (sy);
      output ("\tpushl\t%%eax\n");
      gen_initialize_1 (sy1);
      output ("\tpopl\t%%eax\n");
    }
}


/*
 * SET statement
 */

void
gen_set (cob_tree_list l, int mode, cob_tree v)
{
  for (; l; l = l->next)
    {
      switch (mode)
	{
	case SET_TO: gen_move (v, l->tree); break;
	case SET_UP: gen_add (v, l->tree, 0); break;
	case SET_DOWN: gen_sub (v, l->tree, 0); break;
	}
    }
}

void
gen_set_true (cob_tree_list l)
{
  for (; l; l = l->next)
    {
      cob_tree x = l->tree;
      if (SUBREF_P (x))
	x = SUBREF_SYM (x);

      if ((x->substring_redef.vr != NULL) || (x->value2 != x->value))
	yyerror ("conditional is not unique");
      else
	{
	  if (SUBREF_P (l->tree))
	    gen_move (x->value, make_subref (x->parent, SUBREF_SUBS (l->tree)));
	  else
	    gen_move (x->value, x->parent);
	}
    }
}


/*
 * EVALUATE statement
 */

void
push_boolean (int flag)
{
  push_immed (flag ? 0 : 1);
  asm_call ("cob_push_boolean");
}

void
push_condition (void)
{
  push_eax ();
  asm_call ("cob_push_boolean");
}

void
push_field (cob_tree x)
{
  asm_call_1 ("cob_push_field", x);
}

int
gen_evaluate_start ()
{
  int i = loc_label++;
  output ("# EVALUATE statement\n");
  asm_call ("cob_stack_clear");
  return i;
}

int
push_selection_subject_copy (int level, struct selsubject *ssbj,
			     int stkadd, int objtype)
{
  struct selsubject *p;

  /* find the target subject */
  while (level--)
    ssbj = ssbj->next;

  /* calculate the subject address */
  for (p = ssbj->next; p; p = p->next)
    stkadd++;

  /* push expressions to the stack */
  push_immed (stkadd);
  asm_call ("cob_push_copy");
  return 0;
}

int
selection_subject_type (int level, struct selsubject *ssbj)
{
  while (level--)
    ssbj = ssbj->next;
  return ssbj->type;
}

void
gen_when_check (int level, struct selsubject *ssbj, int type, int endcase)
{
  int real_type;

#ifdef COB_DEBUG
  output ("# WHEN check: level=%d, subject->type=%d, object type=%d\n",
	  level, ssbj->type, type);
#endif

  if (type == SOBJ_ANY)
    return;

  /* check if compatible subject/object found */
  real_type = type & SOBJ_TYPE_MASK;
  switch (selection_subject_type (level, ssbj))
    {
    case SSUBJ_STR:
      if (real_type != SOBJ_STR
	  && real_type != SOBJ_RANGE
	  && real_type != SOBJ_ZERO)
	yyerror ("incompatible selection object");
      break;
    case SSUBJ_EXPR:
      if (real_type == SOBJ_STR)
	{
	  yywarn ("expression expected");
	  type = SOBJ_EXPR | (type & 1);
	}
      break;
    case SSUBJ_BOOLEAN:
      if (real_type != SOBJ_BOOLEAN)
	yyerror ("incompatible selection object");
      break;
    }

  /* perform the actual tests */
  switch (real_type)
    {
    case SOBJ_ZERO:
      push_selection_subject_copy (level, ssbj, 0, type);
      asm_call ("cob_is_zero");
      break;
    case SOBJ_STR:
    case SOBJ_EXPR:
    case SOBJ_BOOLEAN:
      push_selection_subject_copy (level, ssbj, 1, type);
      asm_call ("cob_is_equal");
      break;
    case SOBJ_RANGE:
      push_selection_subject_copy (level, ssbj, 2, type);
      asm_call ("cob_in_range");
      break;
    }
  output ("\tand\t%%eax,%%eax\n");
  if (type & 1)
    output ("\tjnz\t.L%d\n", endcase);
  else
    output ("\tjz\t.L%d\n", endcase);
}

void
gen_bypass_when_case (int bypass)
{
  if (bypass)
    output (".L%d:\n", bypass);
}

int
gen_end_when (int n, int endcase, int sentence)
{
  int lab;
  if (sentence)
    {
      output ("\tjmp\t.L%d\t# end WHEN\n", n);
      lab = 0;
    }
  else
    {
      lab = loc_label++;
      output ("\tjmp\t.L%d\t# bypass WHEN test\n", lab);
    }
  output (".L%d:\n", endcase);
  return lab;
}


/*
 * GO TO statement
 */

void
gen_goto (cob_tree_list l, cob_tree x)
{
  if (x == NULL)
    {
      cob_tree sy = l->tree;
      output ("\tjmp\t.LB_%s\n", label_name (sy));
      if (l->next)
	yyerror ("GOTO only allows one target");
    }
  else
    {
      cob_tree_list tmp;
      cob_tree sy = x;
      gen_loadloc (sy);
      output ("\tmovl $c_base%d+%u, %%eax\n", pgm_segment, sy->descriptor);
      push_eax ();
      asm_call ("get_index");	/* this will return %eax with var's value */
      for (tmp = l; tmp != NULL; tmp = tmp->next)
	{
	  output ("\tdecl\t%%eax\n");
	  output ("\tjz\t.LB_%s\n", label_name (tmp->tree));
	}
    }
}


int
gen_check_zero ()
{
  int i = loc_label++;
  output ("\tand\t%%eax,%%eax\n");
  output ("\tjz\t.L%d\n", i);
  return i;
}

int
gen_testif (void)
{
  int i = loc_label++;
  int j = loc_label++;
  output ("\tjz\t.L%d\n", j);
  output ("\tjmp\t.L%d\n", i);
  output ("\t.align 16\n");
  output (".L%d:\n", j);
  return i;
}

void
gen_dstlabel (int lbl)
{
  output (".L%d:\n", lbl);
}

int
gen_passlabel (void)
{
  int i = loc_label++;
  output ("\tjmp\t.L%d\n", i);
  return i;
}

int
gen_marklabel (void)
{
  int i = loc_label++;
  output (".L%d:\n", i);
  return i;
}

void
gen_jmplabel (int lbl)
{
  output ("\tjmp\t.L%d\n", lbl);
}

void
gen_push_int (cob_tree sy)
{
#ifdef COB_DEBUG
  output ("# gen_put_int\n");
#endif
  asm_call_1 ("get_index", sy);
  /* this must be done without calling push_eax */
  output ("\tpushl\t%%eax\n");
}

void
gen_cancel (cob_tree sy)
{
  asm_call_1 ("cob_cancel", sy);
}


/*
 * PERFORM statement
 */

void
gen_perform_test_counter (int lbl)
{
  output ("\tcmpl\t$0,0(%%esp)\n");
  output ("\tjle\t.L%dE\n", lbl);
}

void
gen_perform_times (int lbl)
{
  output ("\tdecl\t0(%%esp)\n");
  output ("\tjnz\t.L%d\n", lbl);
  output (".L%dE:\tpopl\t%%ecx\n", lbl);
}

void
gen_perform_thru (cob_tree s1, cob_tree s2)
{
  if (s2 == NULL)
    s2 = s1;
  output ("\tleal\t.L%d, %%eax\n", loc_label);
  output ("\tpushl\t%%eax\n");
  output ("\tleal\t.LB_%s, %%eax\n", label_name (s1));
  output ("\tpushl\t%%eax\n");
  output ("\tleal\t.LE_%s, %%eax\n", label_name (s2));
  output ("\tpushl\t%%eax\n");
  output ("\tjmp\t.LB_%s\n", label_name (s1));

  output ("\t.align 16\n");
  output (".L%d:\n", loc_label++);
}

void
gen_perform (cob_tree sy)
{
  gen_perform_thru (sy, sy);
}


/*
 * SEARCH statement
 */

/* increment loop index, check for end */
void
gen_SearchLoopCheck (unsigned long lbl5, cob_tree syidx, cob_tree sytbl)
{
  cob_tree x;
  char tblmax[21];
  /*int len, i; */

  strcpy (tblmax, "1");
  x = install_literal (tblmax);
  save_literal (x, '9');

  gen_add (x, syidx, 0);

  sprintf (tblmax, "%d", sytbl->times);
  x = install_literal (tblmax);
  save_literal (x, '9');

  gen_compare (syidx, COND_GT, x);
  output ("\tjz\t.L%ld\n", lbl5);
}

void
gen_SearchAllLoopCheck (unsigned long lbl3, cob_tree syidx,
			cob_tree sytbl, cob_tree syvar,
			unsigned long lstart, unsigned long lend)
{

  cob_tree sy1;
  struct index_to_table_list *it1, *it2;
  unsigned long l1, l2, l3, l4, l5, l6;

  l1 = loc_label++;
  l2 = loc_label++;
  l3 = loc_label++;
  l4 = loc_label++;
  l5 = loc_label++;
  l6 = loc_label++;

  it1 = index2table;
  it2 = NULL;
  while (it1 != NULL)
    {
      if (strcmp (it1->tablename, COB_FIELD_NAME (sytbl)) == 0)
	{
	  it2 = it1;
	  it1 = NULL;
	}
      else
	{
	  it1 = it1->next;
	}
    }

  if (it2 == NULL)
    return;

  if ((it2->seq != '1') && (it2->seq != '2'))
    return;

  sy1 = make_subref (sytbl, cons (syidx, NULL));

  /* table sort sequence: '0' = none, '1' = ASCENDING, '2' = DESCENDING */

  /*    if ((bu - bl) > 1) */
  output ("\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  output ("\tsubl\t-%d(%%ebp), %%eax\n", stack_offset - 8);

  output ("\tcmpl $1, %%eax\n");
  output ("\tjle .L%ld\n", l1);

  output ("\t.align 16\n");

//    if (itbl1 > in) { /* '2' = DESCENDING */
  if (it2->seq == '2')
    {
      gen_compare (sy1, COND_GT, syvar);
      output ("\tjnz\t.L%ld\n", l2);
    }
  else
    {
      gen_compare (sy1, COND_LT, syvar);
      output ("\tjnz\t.L%ld\n", l2);
    }
  output ("\t.align 16\n");

//    bl  = idx + 1;
  output ("\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  output ("\taddl $1, %%eax\n");
  output ("\tmovl\t%%eax, -%d(%%ebp)\n", stack_offset - 8);

  gen_jmplabel (l3);
  output ("\t.align 16\n");

//    else {
  gen_dstlabel (l2);

//    bu  = idx - 1;
  output ("\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  output ("\tsubl $1, %%eax\n");
  output ("\tmovl\t%%eax, -%d(%%ebp)\n", stack_offset - 12);

  gen_dstlabel (l3);

//    idx = ((bu - bl)/2 + bl);
  output ("\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  output ("\tsubl\t-%d(%%ebp), %%eax\n", stack_offset - 8);
  output ("\tmovl\t%%eax, %%edx\n");
  output ("\tsarl\t$31, %%edx\n");
  output ("\tmovl\t%%edx, %%ecx\n");
  output ("\tsarl\t$31, %%ecx\n");
  output ("\tleal\t(%%ecx,%%eax), %%edx\n");
  output ("\tmovl\t%%edx, %%eax\n");
  output ("\tsarl\t$1, %%eax\n");
  output ("\taddl\t-%d(%%ebp), %%eax\n", stack_offset - 8);
  output ("\tmovl\t%%eax, -%d(%%ebp)\n", syidx->location);

  gen_jmplabel (l6);
  output ("\t.align 16\n");

//    else { /* l1 */
  gen_dstlabel (l1);

  if (it2->seq == '2')
    {
//       if (itbl1 > in) {
      gen_compare (sy1, COND_GT, syvar);
      output ("\tjnz\t.L%ld\n", l4);
    }
  else
    {
//       if (itbl1 < in) {
      gen_compare (sy1, COND_LT, syvar);
      output ("\tjnz\t.L%ld\n", l4);
    }
  output ("\t.align 16\n");


//    if (bu > idx) {
  output ("\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  output ("\tcmpl\t%%eax, -%d(%%ebp)\n", stack_offset - 12);

  output ("\tjle\t.L%ld\n", l5);
  output ("\t.align 16\n");


//    idx = bu;
  output ("\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  output ("\tmovl\t%%eax, -%d(%%ebp)\n", syidx->location);

  gen_jmplabel (l6);
  output ("\t.align 16\n");

//    else {
  gen_dstlabel (l5);


//    r++;
  output ("\taddl\t$1, -%d(%%ebp)\n", stack_offset - 4);

  gen_jmplabel (l6);
  output ("\t.align 16\n");

//       }
//    }
//    else {
  gen_dstlabel (l4);

//    r++;
  output ("\taddl\t$1, -%d(%%ebp)\n", stack_offset - 4);

  gen_dstlabel (l6);

  output ("\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 4);
  output ("\tcmpl $1, %%eax\n");
  output ("\tjz\t.L%ld\n", lbl3);


  gen_jmplabel (lstart);
  output ("\t.align 16\n");
  gen_dstlabel (lend);
}

void
define_implicit_field (cob_tree sy, cob_tree sykey, int idxlen)
{
  cob_tree tmp = NULL;
  struct index_to_table_list *i2t;

  COB_FIELD_TYPE (sy) = 'B';	/* assume numeric "usage is comp" item */
  sy->len = 4;
  sy->decimals = 0;	/* suppose no decimals yet */
  sy->level = 1;
  sy->redefines = NULL;
  sy->linkage_flg = 0;	/* should not go in the linkage section, never! */
  sy->sec_no = SEC_STACK;
  sy->times = 1;
  sy->son = sy->brother = NULL;
  sy->flags.is_pointer = 0;
  sy->flags.blank = 0;
  picture[0] = '9';
  picture[1] = (char) 8;
  picture[2] = 0;
  tmp = curr_field;
  curr_field = sy;
  update_field ();
  close_fields ();
  curr_field = tmp;

  i2t = malloc (sizeof (struct index_to_table_list));
  i2t->idxname = strdup (COB_FIELD_NAME (sy));
  i2t->tablename = strdup (COB_FIELD_NAME (curr_field));
  i2t->seq = '0';	/* no sort sequence is yet defined for the table */
  i2t->keyname = NULL;
  if (sykey != NULL)
    {
      if (sykey->level == -1)
	i2t->seq = '1';
      if (sykey->level == -2)
	i2t->seq = '2';
      i2t->keyname = strdup (COB_FIELD_NAME (sykey));
    }
  i2t->next = index2table;
  index2table = i2t;
}

void
Initialize_SearchAll_Boundaries (cob_tree sy, cob_tree syidx)
{
  int i;
  cob_tree x;
  char tblmax[21];
  struct index_to_table_list *i2t1, *i2t2;

  i = sy->times / 2;

  sprintf (tblmax, "%d", i);
  x = install_literal (tblmax);
  save_literal (x, '9');
  gen_move (x, syidx);

  output ("\tmovl\t$0, %%eax\n");
  output ("\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 4);
  output ("\tmovl\t$1, %%eax\n");
  output ("\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 8);
  output ("\tmovl\t$%d, %%eax\n", sy->times);
  output ("\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 12);

  i2t2 = NULL;
  i2t1 = index2table;
  while (i2t1 != NULL)
    {
      if (strcmp (i2t1->tablename, COB_FIELD_NAME (sy)) == 0)
	{
	  if (i2t1->seq != '0')
	    i2t2 = i2t1;
	  i2t1 = NULL;
	}
      else
	{
	  i2t1 = i2t1->next;
	}
    }

  if (i2t2 == NULL)
    yyerror ("Undefined sort order and key for table ");
}

cob_tree 
determine_table_index_name (cob_tree sy)
{
  cob_tree rsy = NULL;
  struct index_to_table_list *i2t;

  i2t = index2table;
  while (i2t != NULL)
    {
      if (strcmp (i2t->tablename, COB_FIELD_NAME (sy)) == 0)
	{
	  rsy = lookup_symbol (i2t->idxname);
	  i2t = NULL;
	}
      else
	{
	  i2t = i2t->next;
	}
    }

  return rsy;
}

void
define_field (int level, cob_tree sy)
{
  cob_tree tmp;
  cob_tree tmp1 = NULL;

  if (level == 88)
    {
      COB_FIELD_TYPE (sy) = '8';
      sy->defined = 1;
      sy->len = 0;
      sy->decimals = 0;
      sy->level = level;
      sy->linkage_flg = at_linkage;
      sy->sec_no = 0;
      sy->times = 1;
      sy->flags.just_r = 0;
      sy->flags.separate_sign = 0;
      sy->flags.leading_sign = 0;
      sy->son = sy->brother = NULL;
      if (curr_field->level == 88)
	{
	  curr_field->brother = sy;
	  sy->parent = curr_field->parent;
	}
      else
	sy->parent = curr_field;
      curr_field = sy;
      return;
    }
  if (level == 1 || level == 77)
    curr_sec_no = SEC_DATA;
  COB_FIELD_TYPE (sy) = '9';		/* assume numeric (elementary) item */
  sy->len = 0;
  sy->decimals = -1;		/* suppose no decimals yet */
  sy->level = level;
  sy->redefines = NULL;
  sy->linkage_flg = at_linkage;
  sy->sec_no = (at_linkage ? SEC_ARGS : curr_sec_no);
  sy->times = 1;
  sy->flags.just_r = 0;
  sy->flags.separate_sign = 0;
  sy->flags.leading_sign = 0;
  sy->son = sy->brother = NULL;
  tmp = curr_field;
  if (tmp && ((level == 1) || (level == 77)))
    close_fields ();
  if (!tmp && (level > 1) && (level < 49))
    {
      yyerror ("data field hierarchy broken");
    }
  if (level != 77)
    {
      while (tmp != NULL && tmp->level > level)
	{
	  tmp1 = tmp;
	  tmp = tmp->parent;
	}
      if (tmp == NULL)
	sy->parent = NULL;
      else if (tmp->level < level)
	{
	  sy->parent = tmp;
	  if (tmp->son == NULL)
	    tmp->son = sy;
	  else
	    {
	      tmp1->brother = sy;
	      sy->parent = tmp;
	      sy->level = tmp1->level;
	    }
	}
      else
	{
	  tmp->brother = sy;
	  sy->parent = tmp->parent;
	}
    }
  curr_field = sy;
}

struct selsubject *
save_sel_subject (struct selsubject *ssubj, int type)
{
  struct selsubject *tmp = malloc (sizeof (struct selsubject));
  struct selsubject *tmp1;
  tmp->type = type;
  tmp->next = NULL;
  if (ssubj != NULL)
    {
      tmp1 = ssubj;
      while (tmp1->next)
	tmp1 = tmp1->next;
      tmp1->next = tmp;
      return ssubj;
    }
  return tmp;
}

void
release_sel_subject (int label, struct selsubject *ssbj)
{
  asm_call ("cob_stack_clear");
  output (".L%d:\t# EVALUATE end\n", label);
}

int
check_fields (cob_tree sy)
{
  cob_tree tmp;
  int len;

  if (sy->son != NULL)
    {
      len = 0;
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	{
	  check_fields (tmp);
	}
    }
  if (COB_FIELD_TYPE (sy) == '9' && sy->len > 31)
    yyerror ("Elementary numeric item %s > 31 digits", COB_FIELD_NAME (sy));
  return 0;
}

int
set_field_value_sw (cob_tree sy, int times)
{
  cob_tree tmp;
  unsigned int res;
  struct
  {
    unsigned int v:1, sv:1, tmpv:1, tmpsv:1;
  }
  f;

  f.v = 0;
  f.sv = 0;
  if (sy->son != NULL)
    {
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	{
	  res = set_field_value_sw (tmp, times);
	  f.v = f.v || res;
	}
      sy->flags.value = f.v;
      sy->flags.spec_value = f.v;
    }
  f.v = f.v || sy->value != NULL;
  f.sv = f.v;
  //fprintf(stderr,"set_field_value_sw: %s -> %d,%d\n",sy->name,f.v,f.sv);
//      return f.v*2 + f.sv;
  return f.v;
}

int
set_field_length (cob_tree sy, int times)
{
  cob_tree tmp;
  int len, tmplen;
  if (sy->son != NULL)
    {
      len = 0;
      COB_FIELD_TYPE (sy) = 'G';
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	{
	  tmplen = tmp->times * set_field_length (tmp, times);
	  if (tmp->redefines == NULL)
	    len += tmplen;
	}
      sy->len = len;
    }
  len = symlen (sy) + sy->slack;
  //fprintf(stderr,"set_field_length: %s -> %d\n",sy->name,len*times);
  return len * times;
}

unsigned
field_alignment (cob_tree sy, unsigned location)
{
  unsigned slack_bytes = 0, mod_loc;

  if (sy->flags.sync == 0)
    return 0;
  switch (COB_FIELD_TYPE (sy))
    {
    case 'B':
      mod_loc = (location - curr_01_location) % symlen (sy);
      slack_bytes = (mod_loc == 0 ? 0 : symlen (sy) - mod_loc);
      break;
    }
  return slack_bytes;
}

void
set_field_location (cob_tree sy, unsigned location)
{
  cob_tree tmp;

  if (sy->level == 1)
    curr_01_location = location;
  /********* allocate field descriptor *************/
  sy->descriptor = literal_offset;
  literal_offset += (COB_FIELD_TYPE (sy) == 'G' ? 7 : 11);
  /********* generate picture for field ************/
  if (COB_FIELD_TYPE (sy) != 'G')
    {
      sy->pic = literal_offset;
      literal_offset += (strlen (sy->picstr) + 1);
    }
  else
    sy->decimals = sy->pic = 0;
  save_field_in_list (sy);
  if (sy->redefines != NULL)
    {
      location = sy->redefines->location;
      sy->slack = 0;
      sy->flags.in_redefinition = 1;
    }
  else
    {
      sy->slack = field_alignment (sy, location);
      location += sy->slack;
      sy->flags.in_redefinition = 0;
    }
  if (sy->parent != NULL && sy->parent->flags.in_redefinition)
    sy->flags.in_redefinition = 1;
  sy->location = location;
  for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
    {
      set_field_location (tmp, location);
      if (tmp->redefines == NULL)
	{
	  if (tmp->sec_no == SEC_STACK)
	    {
	      location -= (symlen (tmp) + tmp->slack) * tmp->times;
	      /* negative for it's at the stack */
	    }
	  else
	    {
	      location += (symlen (tmp) + tmp->slack) * tmp->times;
	    }
	}
    }
}

void
scr_set_column (struct scr_info *si, int val, int plus_minus)
{
  switch (plus_minus)
    {
    case -1:
      scr_column -= val;
      break;
    case 1:
      scr_column += val;
      break;
    case 0:
      scr_column = val;
      break;
    }
  si->column = scr_column;
}

void
scr_set_line (struct scr_info *si, int val, int plus_minus)
{
  switch (plus_minus)
    {
    case -1:
      scr_line -= val;
      break;
    case 1:
      scr_line += val;
      break;
    case 0:
      scr_line = val;
      break;
    }
  si->line = scr_line;
}

/*************** report section ******************/

void
update_report_field (cob_tree sy)
{
  update_field ();
  COB_FIELD_TYPE (sy) = 'Q';
}

void
update_screen_field (cob_tree sy, struct scr_info *si)
{
  cob_tree tmp;
  update_field ();
  COB_FIELD_TYPE (sy) = 'D';
  sy->scr = si;
  si->label = screen_label++;
  /* if picture is empty (implicit filler), and there is a
     value declared, create its picture from value literal. */
  if (*(sy->picstr) == 0 && sy->value != NULL)
    {
      tmp = sy->value;
      sy->len = strlen (COB_FIELD_NAME (tmp));
      sy->picstr = malloc (3);
      sy->picstr[0] = 'X';
      sy->picstr[1] = sy->len;
      sy->picstr[2] = 0;
    }
}

static int
pic_digits (cob_tree sy)
{
  char *p = NULL;
  int len = 0;
  if (sy == NULL)
    return 0;
  if (!SYMBOL_P (sy))
    {
      const char *name = COB_FIELD_NAME (sy);
      len = strlen (name);
      if (strchr (name, decimal_char ()))
	len--;
      if (strchr (name, '+'))
	len--;
      if (strchr (name, '-'))
	len--;
      return len;
    }
  else
    {
      p = sy->picstr;
      while (*p)
	{
	  if (*p++ == '9')
	    {
	      len += *p++;
	    }
	  else
	    p++;
	}
    }
  return len;
}

static int
query_comp_len (cob_tree sy)
{
  int plen;
  if ((plen = pic_digits (sy)) <= 2)
    return 1;
  if (plen <= 4)
    return 2;
  if (plen <= 9)
    return 4;
  return 8;
}

void
update_field (void)
{
  if (curr_field->level != 88)
    if (COB_FIELD_TYPE (curr_field) != 'G')
      curr_field->picstr = strdup (picture);

  if (COB_FIELD_TYPE (curr_field) != 'B' && COB_FIELD_TYPE (curr_field) != 'U')
    {
      curr_field->len = piccnt;
      if (curr_field->flags.separate_sign)
	curr_field->len++;
    }
  /* update COMP field length (but not BINARY-<something>) */
  if (curr_field->len == 0 && COB_FIELD_TYPE (curr_field) == 'B')
    curr_field->len = query_comp_len (curr_field);
}

void
close_fields (void)
{
  cob_tree sy;
  int saved_length;
  int ns_offset = 0;

  if (curr_field == NULL)
    return;

  /********** locate level 01 field   **************/
  for (sy = curr_field; sy->parent != NULL; sy = sy->parent);
  if (sy->level != 1 && sy->level != 77)
    yyerror ("field not subordinate to any other: %s", COB_FIELD_NAME (sy));

  /********** propagate value flags  *************/
  sy->flags.spec_value = set_field_value_sw (sy, 1);
  /********** update length of fields  *************/
  if (sy->linkage_flg)
    {
      linkage_offset += (set_field_length (sy, 1) * sy->times);
      set_field_location (sy, linkage_offset);
    }
  else if (sy->sec_no == SEC_DATA)
    {
      saved_length = (set_field_length (sy, 1) * sy->times);
      set_field_location (sy, data_offset);
      data_offset += saved_length;
    }
  else if (sy->sec_no >= SEC_FIRST_NAMED)
    {
      saved_length = (set_field_length (sy, 1) * sy->times);
      set_field_location (sy, ns_offset);
      ns_offset += saved_length;
    }
  else
    {
      stack_offset += (set_field_length (sy, 1) * sy->times);
      set_field_location (sy, stack_offset);
    }
  check_fields (sy);
  curr_field = NULL;
}

void
resolve_labels ()
{
  cob_tree sy, sy1, sy2;
  int i, def;
  output ("# resolving paragraphs/sections labels\n");
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy = labtab[i]; sy; sy = COB_FIELD_NEXT (sy))
	{
	  if (COB_FIELD_TYPE (sy) == 'f')
	    continue;
	  sy1 = sy;
	  while (sy1)
	    {
	      if (sy1->defined == 2)
		{
		  def = 0;
		  sy2 = sy;
		  while (sy2)
		    {
		      if (sy2->defined == 1 &&
			  sy2->parent != NULL &&
			  !strcmp (COB_FIELD_NAME (sy1->parent),
				   COB_FIELD_NAME (sy2->parent)))
			{
			  def++;
			  break;
			}
		      sy2 = sy2->clone;
		    }
		  if (!def)
		    {
		      sy2 = sy;
		      while (sy2)
			{
			  if (sy2->defined == 1 && sy2->parent != sy1->parent)
			    {
			      output (".LB_%s = ", label_name (sy1));
			      output (".LB_%s\n", label_name (sy2));
			      output (".LE_%s = ", label_name (sy1));
			      output (".LE_%s\n", label_name (sy2));
			      break;
			    }
			  sy2 = sy2->clone;
			}
		    }
		}
	      sy1 = sy1->clone;
	    }
	}
    }
}

void
open_section (cob_tree sect)
{
  COB_FIELD_TYPE (sect) = 'S';
  output (".LB_%s:\n", label_name (sect));
  curr_section = sect;
}

void
close_section (void)
{
  close_paragr ();
  if (curr_section)
    {
      output (".LE_%s:\n", label_name (curr_section));
      gen_exit ();
    }
}

char *
label_name (cob_tree lab)
{
  if (lab->parent)
    sprintf (name_buf, "%s__%s_%d",
	     COB_FIELD_NAME (lab), COB_FIELD_NAME (lab->parent), pgm_segment);
  else
    sprintf (name_buf, "%s_%d", COB_FIELD_NAME (lab), pgm_segment);
  return chg_underline (name_buf);
}

char *
var_name (cob_tree sy)
{
  int n;
  n = MAXNAMEBUF;
  strcpy (name_buf, "");
  while (n > strlen (COB_FIELD_NAME (sy)) + 4)
    {
      if (n < MAXNAMEBUF)
	strcat (name_buf, " OF ");
      strcat (name_buf, COB_FIELD_NAME (sy));
      n -= strlen (COB_FIELD_NAME (sy)) + 4;
      if ((lookup_symbol (COB_FIELD_NAME (sy))->clone == NULL)
	  || (sy->parent == NULL))
	break;
      sy = sy->parent;
    }
  return name_buf;
}

void
close_paragr (void)
{
  if (curr_paragr)
    {
      output (".LE_%s:\n", label_name (curr_paragr));
      gen_exit ();
      curr_paragr = NULL;
    }
}

void
open_paragr (cob_tree paragr)
{
  COB_FIELD_TYPE (paragr) = 'P';
  curr_paragr = paragr;
  output (".LB_%s:\n", label_name (paragr));
}

void
gen_stoprun (void)
{
  output ("\tleal\t.Lend_pgm_%s, %%eax\n", pgm_label);
  output ("\tpushl\t%%eax\n");
  output ("\tjmp\t.Lend_pgm_%s\n", pgm_label);
}

void
gen_exit (void)
{
  int l1 = loc_label++;
  int l2 = loc_label++;
#ifdef COB_DEBUG
  output ("# EXIT\n");
#endif
  if (curr_paragr != NULL)
    output ("\tleal\t.LE_%s, %%eax\n", label_name (curr_paragr));
  else
    output ("\tleal\t.LE_%s, %%eax\n", label_name (curr_section));
  output ("\tcmpl\t4(%%esp), %%eax\n");
  output ("\tjb\t\t.L%d\n", l1);
  output ("\tcmpl\t0(%%esp), %%eax\n");
  output ("\tjb\t\t.L%d\n", l2);
  output (".L%d:\n", l1);
  output ("\taddl\t$8,%%esp\n");
  output ("\tret\n");
  output (".L%d:\n", l2);
}

void
gen_exit_program (void)
{
#ifdef COB_DEBUG
  output ("# EXIT PROGRAM\n");
#endif
  output ("\tmovl\t-%d(%%ebp), %%ebx\n", stack_offset - 8 - 16);
  output ("\tmov\t%%ebp,%%esp\n");
  output ("\tpop\t%%ebp\n");
  output ("\tret\n");
}

/* save variable values, including 88-var range/values list */
void
set_variable_values (cob_tree v1, cob_tree v2)
{
  struct vrange *new;
  if (curr_field->value == NULL)
    {
      curr_field->substring_redef.vr = NULL;
      curr_field->value = v1;
      curr_field->value2 = v2;
      curr_field->flags.value = 1;
      curr_field->flags.spec_value = 1;
    }
  else
    {
      new = malloc (sizeof (struct vrange));
      new->value = v1;
      new->value2 = v2;
      // spec_value is not used for 88
      new->next = curr_field->substring_redef.vr;
      curr_field->substring_redef.vr = new;
    }
}

static void
gen_save_filevar (cob_tree f, cob_tree buf)
{
  if (buf != NULL)
    {
      gen_loadloc (buf);
    }
  else
    {
      output ("\tmovl\t%s, %%eax\n", memref (f->recordsym));
      push_eax ();
    }
  if (COB_FIELD_TYPE (f) == 'K')
    output ("\tmovl\t$_%s, %%eax\n", COB_FIELD_NAME (f));
  else
    output ("\tmovl\t$s_base%d+%u, %%eax\n", pgm_segment, f->location);
  push_eax ();
}

void
gen_save_filedesc (cob_tree f)
{
  if (COB_FIELD_TYPE (f) == 'K')
    output ("\tmovl\t$_%s, %%eax\n", COB_FIELD_NAME (f));
  else
    output ("\tmovl\t$s_base%d+%u, %%eax\n", pgm_segment, f->location);
  push_eax ();
}

static void
gen_save_sort_fields (cob_tree f, cob_tree buf)
{
  cob_tree datafld;
  if (f == NULL)
    return;
  datafld = f->sort_data;
  while (datafld != NULL)
    {
      gen_loadloc (datafld);
      datafld = (datafld->sort_data);
    }
  output ("\tmovl\t$c_base%d+%u, %%eax\n", pgm_segment, f->descriptor);
  push_eax ();
  gen_save_filevar (f, buf);
  /* returns number of stack levels used in storing fields */
}

void
alloc_file_entry (cob_tree f)
{
  f->record = stack_offset;
}

/*
** define a file, but don't generate code yet.
** (will be done later at dump_fdesc())
*/
void
gen_fdesc (cob_tree f, cob_tree r)
{
  int len;
  struct list *list, *templist;
  struct alternate_list *alt;
  list = (struct list *) malloc (sizeof (struct list));

  if (files_list == NULL)
    {
      files_list = list;
    }
  else
    {
      templist = files_list;
      while (templist->next != NULL)
	{
	  templist = templist->next;
	}
      templist->next = list;
    }
  list->var = f;
  list->next = NULL;

  f->recordsym = r;
  while (r != NULL)
    {
      r->ix_desc = f;
      r = r->redefines;
    }
  len = sizeof (struct file_desc) - 10;	/* suppose without indexes */
  if (f->organization == 1)
    {				/* indexed file */
      len += 10 + 2;
      /*  10 -> remaining of struct file_desc (only for indexed files)
         2 --> size of terminating "word -1| */
      /* now count each alternate description size */
      alt = (struct alternate_list *) f->alternate;
      while (alt)
	{
	  len += sizeof (struct altkey_desc);
	  alt = alt->next;
	}
    }
  f->fdesc = global_offset;
  global_offset += len;
}

/****** sort statement related functions *******/
struct sortfile_node *
alloc_sortfile_node (cob_tree sy)
{
  struct sortfile_node *sn;
  if (COB_FIELD_TYPE (sy) != 'F')
    {
      yyerror ("only files can be found here");
      return NULL;
    }
  sn = malloc (sizeof (struct sortfile_node));
  sn->next = NULL;
  sn->sy = sy;
  return sn;
}

cob_tree 
create_status_register (char *name)
{
  cob_tree sy;
  char pic[] = { '9', 2, 0 };
  sy = install (name, SYTB_VAR, 0);
  if (COB_FIELD_TYPE (sy))
    return sy;		/* it already exists */
  COB_FIELD_TYPE (sy) = '9';
  sy->picstr = malloc (strlen (pic) + 1);
  strcpy (sy->picstr, pic);
  sy->times = 1;
  sy->len = 2;
  sy->son = sy->brother = NULL;
  sy->linkage_flg = 0;
  sy->sec_no = SEC_DATA;
  sy->location = data_offset;
  data_offset += 2;
  sy->descriptor = literal_offset;
  literal_offset += 11;
  sy->pic = literal_offset;
  literal_offset += strlen (pic) + 1;
  save_field_in_list (sy);
  return sy;
}

void
gen_sort_using (cob_tree f, struct sortfile_node *sn)
{
  cob_tree vstatus = create_status_register ("SORT-RETURN");
  gen_save_sort_fields (f, NULL);
  push_immed (0);
  while (sn)
    {
      gen_loadloc (sn->sy->filenamevar);
      gen_save_filedesc (sn->sy);
      sn = sn->next;
    }
  asm_call ("cob_sort_using");
  /* save status returned by operation */
  push_eax ();
  gen_loadloc (vstatus);
  asm_call ("cob_save_status");
}

void
gen_sort_giving (cob_tree f, struct sortfile_node *sn)
{
  cob_tree vstatus = create_status_register ("SORT-RETURN");
  gen_save_sort_fields (f, NULL);
  push_immed (0);
  while (sn)
    {
      gen_loadloc (sn->sy->filenamevar);
      gen_save_filedesc (sn->sy);
      sn = sn->next;
    }
  asm_call ("cob_sort_giving");
  /* save status returned by operation */
  push_eax ();
  gen_loadloc (vstatus);
  asm_call ("cob_save_status");
}

void
gen_sort (cob_tree f)
{
  gen_loadloc (f->filenamevar);
  gen_save_filevar (f, NULL);
  asm_call ("sort_open");
  gen_save_status (f);
  gen_close_sort (f);
}

void
gen_open (int mode, cob_tree f)
{
  push_immed (mode);
  gen_loadloc (f->filenamevar);
  gen_save_filevar (f, NULL);
  asm_call ("cob_open");
  gen_save_status (f);
}

void
gen_close_sort (cob_tree f)
{
  cob_tree sortf;
	/********** allocate memory for SORT descriptor ***********/
  save_field_in_list (f);
  f->descriptor = literal_offset;
  sortf = (f->sort_data);
  while (sortf != NULL)
    {
      literal_offset += 2;
      sortf = (sortf->sort_data);
    }
  literal_offset++;
}

void
gen_close (cob_tree f)
{
  gen_save_filevar (f, NULL);
  asm_call ("cob_close");
  gen_save_status (f);
}

void
gen_return (cob_tree f, cob_tree buf)
{
  gen_save_filevar (f, buf);
  asm_call ("sort_return");
  gen_save_status (f);
}

void
gen_read (cob_tree f, cob_tree buf, cob_tree key)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  if (f->organization == ORG_INDEXED)
    gen_loadvar (key);
  /* pass the desc/address of reclen, if VARYING ... */
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    push_immed (0);
  gen_save_filevar (f, buf);
  asm_call ("cob_read");
  gen_save_status (f);
}

void
gen_read_next (cob_tree f, cob_tree buf, int next_prev)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    push_immed (0);
  gen_save_filevar (f, buf);
  if (next_prev == 1)
    asm_call ("cob_read_next");
  else
    asm_call ("cob_read_prev");
  gen_save_status (f);
}

int
gen_reads (cob_tree f, cob_tree buf, cob_tree key, int next_prev)
{
  if (next_prev > 0
      && (f->organization == ORG_INDEXED
	  || f->organization == ORG_RELATIVE)
      && (f->access_mode == ACC_DYNAMIC
	  || f->access_mode == ACC_SEQUENTIAL))
    gen_read_next (f, buf, next_prev);
  else
    gen_read (f, buf, key);
  return 0;
}

void
gen_release (cob_tree r, cob_tree buf)
{
  if (buf != NULL)
    gen_move (buf, r);
  gen_save_sort_fields (r->ix_desc, buf);
  asm_call ("sort_release");
  gen_save_status (r->ix_desc);
}

static void
gen_check_varying (cob_tree f)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (rv != NULL)
    {
      gen_loadvar (rv->reclen);
      gen_loadvar (rv->lmax);
      gen_loadvar (rv->lmin);
      gen_save_filedesc (f);
      asm_call ("cob_check_varying");
    }
}

void
gen_write (cob_tree r, int opt, cob_tree buf)
{
  cob_tree f = r->ix_desc;
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  gen_check_varying (f);
  if (opt)
    {
      if (rv != NULL)
	gen_loadvar (rv->reclen);
      else
	push_immed (0);
      push_immed (opt);
      gen_save_filevar (f, buf);
      if (buf == NULL)
	asm_call ("cob_write_adv");
      else
	{
	  gen_move (buf, r);
	  asm_call ("cob_write_adv");
	}
    }
  else
    {
      if (f->organization == ORG_RELATIVE)
	push_index (f->ix_desc);
      if (rv != NULL)
	gen_loadvar (rv->reclen);
      else
	push_immed (0);
      gen_save_filevar (f, buf);
      asm_call ("cob_write");
    }
  gen_save_status (f);
}

void
gen_rewrite (cob_tree r, cob_tree buf)
{
  cob_tree f = r->ix_desc;
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    push_immed (0);
  gen_save_filevar (f, buf);
  asm_call ("cob_rewrite");
  gen_save_status (f);
}

void
gen_start (cob_tree f, int cond, cob_tree key)
{
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  else
    gen_loadvar (key);
  push_immed (cond);
  gen_save_filevar (f, NULL);
  asm_call ("cob_start");
  gen_save_status (f);
}

void
gen_delete (cob_tree f)
{
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  gen_save_filevar (f, NULL);
  asm_call ("cob_delete");
  gen_save_status (f);
}

void
set_rec_varying_info (cob_tree f, cob_tree lmin, cob_tree lmax,
		      cob_tree reclen)
{
  struct rec_varying *rv = malloc (sizeof (struct rec_varying));
  f->rec_varying = (char *) rv;
  rv->lmin = lmin;
  rv->lmax = lmax;
  rv->reclen = reclen;
}

void
gen_save_using (cob_tree sy)
{
  sy->linkage_flg = using_offset;
  using_offset += 4;
}

int
gen_call (cob_tree v, struct call_parameter *parameter_list,
	  int exceplabel, int notexceplabel)
{
  struct call_parameter *l;
  int len, totlen = 0;
  int saved_stack_offset = stack_offset;
  int stack_save;
  int endlabel;

  /******** prepare all parameters which are passed by content ********/
  for (l = parameter_list; l != NULL; l = l->next)
    if (l->mode == CALL_BY_CONTENT && !LITERAL_P (l->var))
      {
	len = symlen (l->var);	// should we round to 4?
	totlen += len;
	l->sec_no = SEC_STACK;
	l->location = stack_offset + len;
	stack_offset += len;
	output ("\tsubl\t$%d, %%esp\n", len);
	push_immed (len);		// length
	gen_loadloc (l->var);	// src address
	output ("\tleal\t-%d(%%ebp), %%eax\n", l->location);
	push_eax ();		// dest address ie on stack
	asm_call ("memcpy");
      }

  /******** get the parameters from the parameter list ********/
  for (l = parameter_list; l != NULL; l = l->next)
    {
      switch (l->mode)
	{
	case CALL_BY_REFERENCE:
	  gen_loadloc (l->var);
	  break;
	case CALL_BY_CONTENT:
	  output ("\tleal\t-%d(%%ebp), %%eax\n", l->location);
	  push_eax ();
	  break;
	}
    }
  if (cob_link_style == LINK_STATIC && LITERAL_P (v))
    {
      /* static call */
      asm_call (COB_FIELD_NAME (v));
      endlabel = 0;
    }
  else
    {
      /* dynamic call */
      stack_save = stackframe_cnt;
      stackframe_cnt = 0;
      asm_call_1 ("cob_dyncall_resolve", v);
      stackframe_cnt = stack_save;
      output ("\tand\t%%eax,%%eax\n");
      output ("\tjz\t.L%d\n", exceplabel);
      output ("\tcall\t*%%eax\n");
      cleanup_rt_stack ();
      endlabel = loc_label++;
      output ("\tjmp\t.L%d\n", notexceplabel);
    }
  if (totlen != 0)
    output ("\taddl\t$%d, %%esp\n", totlen);
  stack_offset = saved_stack_offset;
  return endlabel;
}

int
begin_on_except ()
{
  int i = loc_label++;
  output (".L%d:\t# begin_on_except\n", i);
  return i;
}

void
check_call_except (int excep, int notexcep, int exceplabel,
		   int notexceplabel, int endlabel)
{
  /* generate code only if was "call <identifier>" */
  if (endlabel != 0)
    {
      output (".L%d:\t# exceplabel\n", exceplabel);
      if (excep)
	output ("\tjmp\t.L%d\n", excep);
      /* if no exception phrase was given */
      if (excep == 0)
	{
	  output ("\tcall\tcob_dyncall_error\n");
	  output ("\tjmp\t.L%d\n", endlabel);
	}
      output (".L%d:\t# notexceplabel\n", notexceplabel);
      if (notexcep)
	output ("\tjmp\t.L%d\n", notexcep);
      output (".L%d:\t# endlabel\n", endlabel);
    }
}
