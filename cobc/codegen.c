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

#include "cobc.h"
#include "codegen.h"
#include "_libcob.h"

#define MAXNAMEBUF 	300
#define START_STACK_ADJUST 20
#define SYMBUF_SIZE 128

#define decimal_char() (decimal_comma ? ',' : '.')

int pgm_segment = -1;
int screen_io_enable = 0;
int scr_line, scr_column;
int decimal_comma = 0;
char currency_symbol = '$';
char sch_convert_buf[512];

extern struct lextab literal;
extern struct sym *curr_file;

struct sym *curr_paragr = NULL, *curr_section = NULL;
struct sym *curr_field;
short curr_call_mode = 0;
unsigned stack_offset = 0;	/* offset for variables on the stack */
unsigned stack_plus = 0;
unsigned global_offset = 4;	/* offset for global variables (DATA) */
unsigned literal_offset = 0;
unsigned data_offset = 0;
unsigned linkage_offset = 0;
unsigned using_offset = 8;
/* tmpvar_offset: for storage of temporary variables, 
with space reclaimed after the current instruction*/
unsigned tmpvar_offset = 0;
unsigned tmpvar_max = 0;

unsigned last_lineno = 0;
short at_procedure = 0;
short refmod_slots = 0;

struct lit *spe_lit_ZE = NULL;
struct lit *spe_lit_SP = NULL;
struct lit *spe_lit_LV = NULL;
struct lit *spe_lit_HV = NULL;
struct lit *spe_lit_QU = NULL;

struct list *expr_list = NULL;
struct list *files_list = NULL;
struct list *disp_list = NULL;
struct parm_list *parameter_list = NULL;
struct list *fields_list = NULL;
struct list *last_field = NULL;
struct index_to_table_list *index2table = NULL;
struct named_sect *named_sect_list = NULL;
int next_available_sec_no = SEC_FIRST_NAMED;
int curr_sec_no = SEC_DATA;

int screen_label = 0;
int para_label = 0;
int block_label = 0;
int line_label = 0;
int paragr_num = 1;
int loc_label = 1;
unsigned char picture[4096];
int picix, piccnt, sign, v_flag, n_flag;
int active[37];
int at_linkage = 0;
int stackframe_cnt = 0;
char program_id[120] = "main";
char *pgm_label = "main";
struct list *report_list = NULL;

static int need_desc_length_cleanup = 0;
static char name_buf[MAXNAMEBUF];
static char init_ctype;		// hold homogenous type
static short init_val;		// hold homogenous value
static unsigned curr_01_location;	// hold current root father when set_field_location


/*
**	Symbol table management routines
*/

#define HASHLEN 100
static struct sym *vartab[HASHLEN] = { NULL };
static struct sym *labtab[HASHLEN] = { NULL };
static struct lit *littab[HASHLEN] = { NULL };

static int
hash (char *s)
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

struct sym *
lookup (char *s, int tab)
{
  char sbuf[SYMBUF_SIZE];
  if (tab == SYTB_LIT)
    {				/* literals tab */
      struct lit *as;
      for (as = littab[hash (s)]; as != NULL; as = as->next)
	if (strcmp (s, as->name) == 0)
	  return ((struct sym *) as);
      return (NULL);
    }
  else
    {
      struct sym *as;
      s = upcase (s, sbuf);
      if (tab == SYTB_VAR)
	as = vartab[hash (s)];
      else
	as = labtab[hash (s)];
      for (; as != NULL; as = as->next)
	if (strcmp (s, as->name) == 0)
	  return (as);
      return (NULL);
    }
}

struct sym *
lookup_symbol (char *s)
{
  return lookup (s, SYTB_VAR);
}

struct sym *
install (char *name, int tab, int cloning)
{
  char sbuf[SYMBUF_SIZE];
  struct sym *clone;
  struct sym *as;
  int val;

  name = upcase (name, sbuf);
  if ((as = lookup (name, tab)) == NULL)
    {
      as = make_symbol (strdup (name));
      val = hash (as->name);
      if (tab == SYTB_VAR)
	{
	  as->next = vartab[val];
	  vartab[val] = as;
	}
      else
	{
	  as->next = labtab[val];
	  labtab[val] = as;
	}
    }
  else if ((cloning && (as->defined == 1)) || (cloning == 2))
    {
      /* install clone (cloning==2 -> force) */
      clone = make_symbol (as->name);
      clone->clone = as->clone;
      as->clone = clone;
      as = clone;
    }
  return (as);
}

struct sym *
install_label (char *name)
{
  return install (name, SYTB_LAB, 0);
}

struct lit *
install_literal (const char *name)
{
  int val;
  struct lit *al = malloc (sizeof (struct lit));
  al->name = strdup (name);
  al->type = 0;
  al->all = 0;
  al->litflag = 1;
  al->nick = NULL;
  al->len = strlen (name);
  val = hash (al->name);
  al->next = littab[val];
  littab[val] = al;
  return al;
}

struct sym *
lookup_label (struct sym *sy, struct sym *parent)
{
  while (sy->clone && (sy->parent != parent))
    sy = sy->clone;
  if (sy->parent == parent)
    return sy;
  else
    return NULL;
}


/*****************************************************************************
 * IDENTIFICATION DIVISION.
 *****************************************************************************/

static void
clear_symtab ()
{
  struct sym *sy, *sy1, *tmp;
  int i;
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy1 = vartab[i]; sy1 != NULL;)
	{
	  for (sy = sy1->clone; sy;)
	    {
	      if (sy)
		{
		  tmp = sy;
		  sy = sy->clone;
		  free (tmp);
		}
	    }
	  tmp = sy1;
	  sy1 = sy1->next;
	  free (tmp);
	}
      vartab[i] = NULL;
    }
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy1 = labtab[i]; sy1 != NULL;)
	{
	  for (sy = sy1->clone; sy;)
	    {
	      if (sy)
		{
		  tmp = sy;
		  sy = sy->clone;
		  free (tmp);
		}
	    }
	  tmp = sy1;
	  sy1 = sy1->next;
	  free (tmp);
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
  refmod_slots = 0;
  free_list (fields_list);
  fields_list = NULL;
  free_list (files_list);
  files_list = NULL;
  /* clear all current paragraphs/sections and fields */
  curr_paragr = NULL;
  curr_section = NULL;
  curr_field = NULL;
  /* free tmpvar storage */
  tmpvar_offset = 0;
  tmpvar_max = 0;
}

static void
define_special_fields ()
{
  struct sym *sy, *tmp;

  spe_lit_SP = save_special_literal (' ', 'X', "%SPACES%");
  spe_lit_LV = save_special_literal ('\0', 'X', "%LOW-VALUES%");
  spe_lit_HV = save_special_literal ('\xff', 'X', "%HIGH-VALUES%");
  spe_lit_ZE = save_special_literal ('0', '9', "%ZEROS%");
  spe_lit_QU = save_special_literal ('"', 'X', "%QUOTES%");
  spe_lit_SP->all = 1;
  spe_lit_LV->all = 1;
  spe_lit_HV->all = 1;
  spe_lit_ZE->all = 1;
  spe_lit_QU->all = 1;

  sy = install (SVAR_RCODE, SYTB_VAR, 0);
  sy->type = 'B';		/* assume numeric "usage is comp" item */
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
    fprintf (o_src, "\t.file\t\"%s\"\n", cob_source_filename);
  strcpy (program_id, id);

  define_special_fields ();
}


/*****************************************************************************
 * DATA DIVISION.
 *****************************************************************************/

/*******************
 * WORKING-STRAGE SECTION
 *******************/

struct sym *
lookup_variable (struct sym *sy, struct sym *parent)
{
  if (SUBREF_P (parent))
    parent = SUBREF_SYM (parent);

  for (;;)
    {
      struct sym *p;
      for (p = sy; p != NULL; p = p->parent)
	if (p->parent == parent)
	  return sy;

      if (sy->clone == NULL)
	return sy;
      sy = sy->clone;
    }
}

struct sym *
lookup_for_redefines (struct sym *sy)
{
  if (curr_field->parent == NULL)
    return lookup_symbol (sy->name);
  else
    return lookup_variable (sy, curr_field->parent);
}

void
save_named_sect (struct sym *sy)
{
  struct named_sect *nsp = malloc (sizeof (struct named_sect));
  nsp->sec_no = next_available_sec_no++;
  nsp->os_name = chg_underline (strdup (sy->name));
  nsp->next = named_sect_list;
  named_sect_list = nsp;
  curr_sec_no = nsp->sec_no;	// Uncomment to activate
  sy->sec_no = curr_sec_no;
}


/*** we need this because the literal string is already stored ***/
char
sign_to_char (int digit)
{
  if (!digit)
    return '}';
  if (digit == 0x80)
    return '{';
  if (digit > 0)
    return 'A' + (char) (digit - 1);
  digit = -digit;
  return 'J' + (char) (digit - 1);
}

struct lit *
invert_literal_sign (struct lit *sy)
{
  char *s;
  s = sy->name;
  s += strlen (s) - 1;
  *s = sign_to_char (-(*s - 0x30));
  return sy;
}

/* convert control characters to don't corrupt the assembly output */
char *
sch_convert (char *s)
{
  int n = 0;
  char *d = sch_convert_buf;
  while (*s && n++ < 45)
    {
      if (*s >= ' ' && *s < '\x7f')
	*d++ = *s++;
      else
	*d++ = (*s++ & 0x0f) + ' ';
    }
  if (n >= 45)
    sprintf (sch_convert_buf + 40, "...");
  else
    *d = 0;
  return sch_convert_buf;
}

int
is_variable (struct sym *sy)
{
  if (SYMBOL_P (sy))
    switch (sy->type)
      {
      case '8':		/* 88 field */
      case '9':		/* numeric */
      case 'A':		/* alpha */
      case 'B':		/* binary (comp/computational) */
      case 'C':		/* compacted (comp-3/comptational-3) */
      case 'D':		/* screen data */
      case 'E':		/* edited */
      case 'G':		/* group */
      case 'U':		/* float(comp-1 4 bytes) / double(comp-2 8 bytes) */
      case 'X':		/* alphanum */
	return 1;
      }

  return 0;
}

int
is_subscripted (struct sym *sy)
{
  for (; sy; sy = sy->parent)
    if (sy->times > 1)
      return 1;
  return 0;
}


/*
 * Local functions
 */

static void asm_call_1 (const char *name, struct sym *sy1);
static void value_to_eax (struct sym *sy);

static void
push_immed (int i)
{
  stackframe_cnt += 4;
  fprintf (o_src, "\tpushl\t$%d\n", i);
}

static void
push_eax ()
{
  stackframe_cnt += 4;
  fprintf (o_src, "\tpushl\t%%eax\n");
}

static void
push_edx ()
{
  stackframe_cnt += 4;
  fprintf (o_src, "\tpushl\t%%edx\n");
}

static void
push_ebx ()
{
  stackframe_cnt += 4;
  fprintf (o_src, "\tpushl\t%%ebx\n");
}

static void
push_at_eax (struct sym *sy)
{
#ifdef COB_DEBUG
  fprintf (stderr, "push_at_eax:\n");
#endif
  stackframe_cnt += 4;
  if (sy->type == 'B' || sy->type == 'U')
    {
      if (symlen (sy) == 8)
	{
	  fprintf (o_src, "\tmovl\t4(%%eax), %%edx\n");
	  fprintf (o_src, "\tmovl\t0(%%eax), %%eax\n");
	  fprintf (o_src, "\tpushl\t%%edx\n");
	  stackframe_cnt += 4;
	}
      else if (symlen (sy) >= 4)
	fprintf (o_src, "\tmovl\t0(%%eax), %%eax\n");
      else
	fprintf (o_src, "\tmovs%cl\t0(%%eax), %%eax\n", varsize_ch (sy));
    }
  else
    fprintf (o_src, "\tmovl\t0(%%eax), %%eax\n");
  fprintf (o_src, "\tpushl\t%%eax\n");
}

static char *
sec_name (short sec_no)
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
memref (struct sym *sy)
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
memrefat (struct sym *sy)
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
    default:
      // Make sure we have an error at assembly stage
      sprintf (memref_buf, "ww_base%d+%d #sec:%d", pgm_segment,
	       sy->location, sy->sec_no);
    }
  return memref_buf;
}

static char *
memrefd (struct sym *sy)
{
  sprintf (memref_buf, "$c_base%d+%d", pgm_segment, sy->descriptor);
  return memref_buf;
}

/* load address for normal (file/working-storage) or linkage variable */
static void
load_address (struct sym *sy)
{
  unsigned offset;
  if (SYMBOL_P (sy) && sy->linkage_flg)
    {
      struct sym *tmp = sy;
      while (tmp->linkage_flg == 1)
	tmp = tmp->parent;
      offset = sy->location - tmp->location;
      fprintf (o_src, "\tmovl\t%d(%%ebp), %%eax\n", tmp->linkage_flg);
      if (offset)
	fprintf (o_src, "\taddl\t$%d, %%eax\n", offset);
    }
  else if (sy->sec_no == SEC_STACK)
    fprintf (o_src, "\tleal\t%s, %%eax\n", memref (sy));
  else if (sy->sec_no == SEC_DATA)
    fprintf (o_src, "\tleal\tw_base%d+%d, %%eax\n",
	     pgm_segment, sy->location);
  else if (sy->sec_no == SEC_CONST)
    fprintf (o_src, "\tleal\tc_base%d+%d, %%eax\n",
	     pgm_segment, sy->location);
}

/* load in cpureg ("eax","ebx"...) location for normal 
	(file/working-storage) or linkage variable */
static void
load_location (struct sym *sy, char *reg)
{
  unsigned offset;
  if (SYMBOL_P (sy) && sy->linkage_flg)
    {
      struct sym *tmp = sy;
      while (tmp->linkage_flg == 1)
	tmp = tmp->parent;
      offset = sy->location - tmp->location;
      fprintf (o_src, "\tmovl\t%d(%%ebp), %%%s\n", tmp->linkage_flg, reg);
      if (offset)
	fprintf (o_src, "\taddl\t$%d, %%%s\n", offset, reg);
    }
  else if (sy->sec_no == SEC_STACK)
    fprintf (o_src, "\tleal\t%s, %%%s\n", memref (sy), reg);
  else
    fprintf (o_src, "\tmovl\t%s, %%%s\n", memref (sy), reg);
}

static void
loadloc_to_eax (struct sym *sy_p)
{
  unsigned offset;
  struct sym *sy = sy_p, *var, *tmp;

#ifdef COB_DEBUG
  fprintf (o_src, "#gen_loadloc litflg %d\n", sy->litflag);
#endif
  if (REFMOD_P (sy))
    sy = ((struct refmod *) sy)->sym;	// temp bypass
  if (SUBREF_P (sy))
    {
      gen_subscripted ((struct subref *) sy);
      var = SUBREF_SYM (sy);
      if (var->linkage_flg)
	{
	  tmp = var;
	  while (tmp->linkage_flg == 1)
	    tmp = tmp->parent;
	  offset = var->location - tmp->location;
	  fprintf (o_src, "\tmovl %d(%%ebp), %%ebx\n", tmp->linkage_flg);
	  if (offset)
	    fprintf (o_src, "\taddl\t$%d, %%ebx\n", offset);
	  fprintf (o_src, "\taddl\t%%ebx, %%eax\n");
	}
      else
	{
	  if (var->sec_no == SEC_STACK)
	    fprintf (o_src, "\tleal\t%s, %%ebx\n", memref (var));
	  else if (var->sec_no == SEC_DATA)
	    fprintf (o_src, "\tleal\tw_base%d+%d, %%ebx\n",
		     pgm_segment, var->location);
	  else if (var->sec_no == SEC_CONST)
	    fprintf (o_src, "\tleal\tc_base%d+%d, %%ebx\n",
		     pgm_segment, var->location);
	  fprintf (o_src, "\taddl\t%%ebx,%%eax\n");
	}
    }
  else
    {
      load_location (sy, "eax");
    }
//      At that stage, the address is ready in %eax; do we need
//      to correct it because of RefMod's?
  if (REFMOD_P (sy_p))
    {				// should avoid all that if literal 1
      struct refmod *rfp = (struct refmod *) sy_p;
      fprintf (o_src, "\tmovl\t%%eax, %%ebx\n");
      value_to_eax (rfp->off);
      fprintf (o_src, "\tdecl\t%%eax\n");
      fprintf (o_src, "\taddl\t%%ebx, %%eax\n");
    }
}

static void
gen_loadloc (struct sym *sy)
{
  loadloc_to_eax (sy);
  push_eax ();
}

static void
gen_loaddesc1 (struct sym *sy, int variable_length)
{
  struct sym *var;
  if (SUBREF_P (sy) || REFMOD_P (sy))
    {
      var = SUBREF_SYM (sy);
      if (SUBREF_P (var))
	var = SUBREF_SYM (var);
    }
  else
    {
      var = sy;
    }
  if (REFMOD_P (sy))
    {
      struct refmod *rflp = (struct refmod *) sy;
      struct sym *syl = rflp->len;
      if (syl == NULL)
	{
	  fprintf (o_src, "#  corrected length EOV\n");
	  value_to_eax (rflp->off);
	  fprintf (o_src, "\tnegl\t%%eax\n");
	  fprintf (o_src, "\taddl\t$%d, %%eax\n", symlen (var));
	  fprintf (o_src, "\tincl\t%%eax\n");
	  fprintf (o_src, "\tmovl\t%%eax, rf_base%d+%d\n",
		   pgm_segment, rflp->slot * 8);
	}
      else
	{
	  fprintf (o_src, "#  corrected length %s\n", syl->name);
	  if (LITERAL_P (syl))
	    fprintf (o_src, "\tmovl\t$%s, rf_base%d+%d\n",
		     syl->name, pgm_segment, rflp->slot * 8);
	  else
	    {
	      value_to_eax (syl);
	      fprintf (o_src, "\tmovl\t%%eax, rf_base%d+%d\n",
		       pgm_segment, rflp->slot * 8);
	    }
	}
      fprintf (o_src, "\tmovl\t$'%c', rf_base%d+%d\n", 'G',
	       pgm_segment, rflp->slot * 8 + 4);
      fprintf (o_src, "\tmovl\t$rf_base%d+%d, %%eax\n",
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
	  fprintf (o_src, "\tmovl\t%s, %%eax\t# descriptor of [%s]\n",
		   memrefd (var), sch_convert (var->name));
#else
	  fprintf (o_src, "\tmovl\t%s, %%eax\n", memrefd (var));
#endif
	}
    }
  push_eax ();
}

static void
gen_loaddesc (struct sym *sy)
{
  gen_loaddesc1 (sy, 1);
}

void
gen_loadvar (struct sym *sy)
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
value_to_eax (struct sym *sy)
{
  long long value;
  long value2;
  int stack_save;
  char *s;
#ifdef COB_DEBUG
  if (sy)
    fprintf (o_src, "# value_to_eax %s\n", sy->name);
#endif
  if (sy == NULL)
    {
      fprintf (o_src, "\txorl\t%%eax,%%eax\n");
      return;
    }
  if (!SYMBOL_P (sy))
    {
      /* if it's an integer, compute it now, not at runtime! */
      value = 0;
      s = sy->name;		/* integer's name is just it's value in ascii */
      while (*s)
	value = value * 10 + *s++ - '0';
      fprintf (o_src, "#bef val\n");
      fprintf (o_src, "\tmovl\t$%d,%%eax\n", (int) value);
      if ((value2 = value >> 32) != 0)
	fprintf (o_src, "\tmovl\t$%d,%%edx\n", (int) value2);
    }
  else if (sy->type == 'B' || sy->type == 'U')
    {
      /* load binary (comp) value directly */
      /* %eax doesn't hold greater than 4 bytes binary types
         so we use %edx to get the most significant part */
      if (symlen (sy) > 4)
	{
	  fprintf (o_src, "\tmovl\t%s+4, %%edx\n", memref (sy));
	  fprintf (o_src, "\tmovl\t%s, %%eax\n", memref (sy));
	}
      else
	{
	  if (symlen (sy) >= 4)
	    {
	      switch (sy->sec_no)
		{
		case SEC_CONST:
		  fprintf (o_src, "\tmovl\tc_base%d+%d, %%eax\n",
			   pgm_segment, sy->location);
		  break;
		case SEC_DATA:
		  fprintf (o_src, "\tmovl\tw_base%d+%d, %%eax\n",
			   pgm_segment, sy->location);
		  break;
		case SEC_STACK:
		  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n",
			   sy->location);
		  break;
		}
	    }
	  else
	    {
	      switch (sy->sec_no)
		{
		case SEC_CONST:
		  fprintf (o_src, "\tmovs%cl\tc_base%d+%d, %%eax\n",
			   varsize_ch (sy), pgm_segment, sy->location);
		  break;
		case SEC_DATA:
		  fprintf (o_src, "\tmovs%cl\tw_base%d+%d, %%eax\n",
			   varsize_ch (sy), pgm_segment, sy->location);
		  break;
		case SEC_STACK:
		  fprintf (o_src, "\tmovs%cl\t-%d(%%ebp), %%eax\n",
			   varsize_ch (sy), sy->location);
		  break;
		}
	    }
	}
    }
  else
    {
      stack_save = stackframe_cnt;
      stackframe_cnt = 0;
      asm_call_1 ("get_index", sy);
      stackframe_cnt = stack_save;
    }
}

/* store variable pointer in eax to sy.
   sy must be a pointer or a linkage section 01/77 variable */
static void
set_ptr (struct sym *sy)
{
  if (SYMBOL_P (sy) && sy->linkage_flg)
    {
      if (sy->linkage_flg == 1)
	{
	  yyerror ("only level 01 or 77 linkage vars may be set");
	  return;
	}
      fprintf (o_src, "\tmovl\t%%eax,%d(%%ebp)\n", sy->linkage_flg);
      return;
    }
  else
    {
      if (SYMBOL_P (sy))
	{
	  load_location (sy, "ebx");
	  fprintf (o_src, "\tmovl\t%%eax,0(%%ebx)\n");
	}
      else
	{
	  fprintf (o_src, "\tpushl\t%%eax\t# saving ptr value\n");
	  loadloc_to_eax (sy);
	  fprintf (o_src, "\tmovl\t%%eax,%%ebx\n");
	  fprintf (o_src, "\tpopl\t%%eax\n");
	  fprintf (o_src, "\tmovl\t%%eax,0(%%ebx)\n");
	}
    }
}

static void
cleanup_rt_stack ()
{
  /* generate stack cleanup only if there is something to clean */
  if (stackframe_cnt == 1)
    fprintf (o_src, "\tpopl\t%%ecx\n");
  else if (stackframe_cnt)
    fprintf (o_src, "\taddl\t$%d, %%esp\n", stackframe_cnt);
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
  fprintf (o_src, "\tcall\t%s\n", name);
  cleanup_rt_stack ();
}

static void
asm_call_1 (const char *name, struct sym *s1)
{
  gen_loadvar (s1);
  asm_call (name);
}

static void
asm_call_2 (const char *name, struct sym *s1, struct sym *s2)
{
  gen_loadvar (s2);
  gen_loadvar (s1);
  asm_call (name);
}

static void
asm_call_3 (const char *name, struct sym *s1, struct sym *s2, struct sym *s3)
{
  gen_loadvar (s3);
  gen_loadvar (s2);
  gen_loadvar (s1);
  asm_call (name);
}

static void
push_index (struct sym *sy)
{
  asm_call_1 ("get_index", sy);
  push_eax ();
}


/*
 *	Code Generating Routines
 */

void
emit_lit (char *s, int len)
{
  int bcnt = 0;
  while (len--)
    if (!(bcnt++ % 8))
      {
	if (bcnt > 1)
	  putc ('\n', o_src);
	fprintf (o_src, "\t.byte\t%d", *s++);
      }
    else
      {
	fprintf (o_src, ",%d", *s++);
      }
}

void
emit_lit_fill (int c, int len)
{
  int bcnt = 0;
  while (len--)
    if (!(bcnt++ % 8))
      {
	if (bcnt > 1)
	  putc ('\n', o_src);
	fprintf (o_src, "\t.byte\t%d", c);
      }
    else
      {
	fprintf (o_src, ",%d", c);
      }
}

void
gen_init_value (struct lit *sy, int var_len)
{
  int bcnt = 0;
  int len, start_len;
  char *s;
  char pad;

  if (sy->nick)
    {
      s = sy->nick;
      len = 1;
      pad = sy->nick[0];
    }
  else
    {
      s = sy->name;
      len = sy->len;
      pad = ' ';
    }
  if (len > var_len)
    len = var_len;
  start_len = len;
  while (len)
    {
      if (!(bcnt++ % 8))
	{
	  if (bcnt > 1)
	    putc ('\n', o_src);
	  fprintf (o_src, "\t.byte\t%d", *s++);
	}
      else
	{
	  fprintf (o_src, ",%d", *s++);
	}
      len--;
    }
  putc ('\n', o_src);
  if (start_len < var_len)
    {
      len = var_len - start_len;
      bcnt = 0;
      while (len)
	{
	  if (!(bcnt++ % 8))
	    {
	      if (bcnt > 1)
		putc ('\n', o_src);
	      fprintf (o_src, "\t.byte\t%d", pad);
	    }
	  else
	    {
	      fprintf (o_src, ",%d", pad);
	    }
	  len--;
	}
      putc ('\n', o_src);
    }
}

void
stabs_line ()
{
  static int last_orig_lineno = 0;
  static char *last_orig_filename = NULL;

  if (!cob_stabs_flag)
    return;

  if (last_orig_lineno == cob_orig_lineno
      && last_orig_filename == cob_orig_filename)
    return;

  if (last_orig_filename != cob_orig_filename)
    fprintf (o_src, ".stabs\t\"%s\",132,0,0,.LS%d\n",
	     cob_orig_filename, line_label);
  fprintf (o_src, ".stabn\t68,0,%d,.LS%d-Ltext_%s\n",
	   cob_orig_lineno, line_label, pgm_label);
  fprintf (o_src, ".LS%d:\n", line_label++);

  last_orig_lineno = cob_orig_lineno;
  last_orig_filename = cob_orig_filename;
}

void
data_trail (void)
{
  if (refmod_slots > 0)
    fprintf (o_src, "rf_base%d:\t.space\t%d\n", pgm_segment, refmod_slots * 8);
}

int
adjust_linkage_vars (int start_offset)
{
  struct sym *sy, *sy1;
  int i;
  int offset = start_offset;

  for (i = 0; i < HASHLEN; i++)
    for (sy1 = vartab[i]; sy1 != NULL; sy1 = sy1->next)
      for (sy = sy1; sy; sy = sy->clone)
	if (sy->parent == NULL && sy->linkage_flg == 1)
	  {
	    sy->linkage_flg = -offset;
	    offset += 4;
	  }

  return offset;
}


static int
get_nb_fields (struct sym *sy, int times, int sw_val)
{
  struct sym *tmp;
  int nb_fields = 0;
  char ftype = sy->type;

  if (ftype == 'G')
    {
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
      if (ftype == 'C')
	init_ctype = '&';
      else if (ftype == '9' && sy->picstr[0] == 'S')
	init_ctype = '&';
      else if (init_ctype == ' ')
	init_ctype = ftype;
      else if (init_ctype != ftype)
	init_ctype = '&';
      if (sw_val == 1)
	{
	  short val = ((sy->value == NULL) ? 0 :
		       (sy->value == spe_lit_ZE) ? 2 :
		       (sy->value == spe_lit_SP) ? 3 :
		       (sy->value == spe_lit_LV) ? 4 :
		       (sy->value == spe_lit_HV) ? 5 : 1);
	  if (init_val == -1)
	    init_val = val;
	  if (ftype == init_ctype && val != init_val)
	    init_ctype = '&';
	}
    }
  return nb_fields * times;
}

static struct sym *
get_init_symbol (char type)
{
  switch (type)
    {
    case '9': case 'C':
      return (struct sym *) spe_lit_ZE;
    case 'B':
      return (struct sym *) spe_lit_LV;
    default:
      return (struct sym *) spe_lit_SP;
    }
}

static unsigned int
initialize_values_1 (struct sym *sy, unsigned int loc)
{
  int i;

  if (sy->type == 'G')
    {
      struct sym *p;
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
		gen_move ((struct sym *) sy->value, sy);
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
  struct sym *sy, *sy1, *v;
  int i;
  char typ;
  int nb_fields;

  for (i = 0; i < HASHLEN; i++)
    for (sy1 = vartab[i]; sy1 != NULL; sy1 = sy1->next)
      for (sy = sy1; sy != NULL; sy = sy->clone)
	if (sy->type != 'F' && sy->type != '8' &&
	    sy->type != 'K' && sy->type != 'J')
	  {
	    v = sy;
	    typ = v->type;
	    if (typ == 'F' || typ == 'R' || typ == 'K' || typ == 'J'
		|| typ == '8')
	      continue;
	    if (v->level != 1 && v->level != 77)
	      continue;
	    // for the time being spec_value will be true if any value encountered;
	    // later on it will contain only non standard values
	    if (!v->flags.value)
	      continue;
	    if (v->level == 77 || (v->level == 1 && v->son == NULL))
	      {
		if (v->value)
		  gen_move ((struct sym *) v->value, v);
		continue;
	      }
	    init_ctype = ' ';
	    init_val = -1;
	    nb_fields = get_nb_fields (v, v->times, 1);
	    if (init_ctype != '&' && init_val != 1)
	      gen_move (get_init_symbol (init_ctype), v);
	    else
	      initialize_values_1 (v, v->location);
	  }
}

void
proc_header (int using)
{
  struct sym *sy, *sy1;
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
	  fprintf (o_src, ".stabs\t\"%s\",100,0,0,Ltext_%s\n",
		   cob_source_filename, pgm_label);
	  fprintf (o_src, ".stabs\t\"%s:F1\",36,0,0,%s\n",
		   pgm_label, pgm_label);
	  fprintf (o_src, ".stabs\t\"display:t2=r2;0;255;\",128,0,0,0\n");
	  fprintf (o_src, ".stabs\t\"comp:t3=r3;-2147483648;2147483647;\",128,0,0,0\n");
	  fprintf (o_src, ".stabs\t\"comp3:t4=r3;0;255;\",128,0,0,0\n");
	  fprintf (o_src, ".stabs\t\"compw:t5=r5;-32768;32767;\",128,0,0,0\n");
	  fprintf (o_src, ".stabs\t\"compb:t6=r6;-128;127;\",128,0,0,0\n");
	  /* compll (comp with 8 bytes size) is wrong. Use a dump instead */
	  fprintf (o_src, ".stabs\t\"compll:t7=r(0,1);0;01777777777777777777777\",128,0,0,0\n");
	}
      fprintf (o_src, "\t.version\t\"01.01\"\n");
      fprintf (o_src, "cobc_compiled.:\n");
    }

  fprintf (o_src, ".text\n");
  fprintf (o_src, "Ltext_%s:\n", pgm_label);
  if (!pgm_segment)
    {
      if (cob_stabs_flag)
	fprintf (o_src, ".stabs\t\":t1\",128,0,0,0\n");
      fprintf (o_src, "\t.align 16\n");
    }
  fprintf (o_src, ".globl %s\n", pgm_label);
  fprintf (o_src, "\t.type\t%s,@function\n", pgm_label);
  fprintf (o_src, "%s:\n", pgm_label);
  fprintf (o_src, "\tpushl\t%%ebp\n\tmovl\t%%esp, %%ebp\n");
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

  fprintf (o_src, "\tsubl\t$%u, %%esp\n", stack_offset);
  fprintf (o_src, "\tmovl\t%%ebx, -%d(%%ebp)\n", stack_offset - 16);
  fprintf (o_src, ".Linit_%s:\n", pgm_label);
  fprintf (o_src, "\tmovl\t$s_base%d+0, %%eax\n", pgm_segment);
  fprintf (o_src, "\tcmpl\t$0, 0(%%eax)\n");
  fprintf (o_src, "\tjne\t.Linite_%s\n", pgm_label);
  fprintf (o_src, "\tmovl\t$1, 0(%%eax)\n");

  /********** initialize all VALUES of fields **********/
  initialize_values ();

  /********** dump stabs for local variables **********/
  if (cob_stabs_flag)
    for (i = 0; i < HASHLEN; i++)
      for (sy1 = vartab[i]; sy1 != NULL; sy1 = sy1->next)
	for (sy = sy1; sy != NULL; sy = sy->clone)
	  if (sy->sec_no == SEC_STACK)
	    switch (sy->type)
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
		fprintf (o_src, ".stabs\t\"%s:%c\",128,0,0,-%d\n",
			 sy->name, stabs_type, sy->location);
		break;
	      case 'C':
		fprintf (o_src,
			 ".stabs\t\"%s:(1,%d)=ar3;1;%d;4\",128,0,0,-%d\n",
			 sy->name, sy->len, sy->len, sy->location);
		break;
	      default:
		fprintf (o_src,
			 ".stabs\t\"%s:(1,%d)=ar3;1;%d;2\",128,0,0,-%d\n",
			 sy->name, sy->len, sy->len, sy->location);
	      }

  fprintf (o_src, ".Linite_%s:\n", pgm_label);
  if (cob_stabs_flag)
    {
      fprintf (o_src, ".stabn\t192,0,0,.LS%d-Ltext_%s\n",
	       line_label, pgm_label);
      fprintf (o_src, ".stabn\t224,0,0,.LSend_%s-Ltext_%s\n",
	       pgm_label, pgm_label);

    }

  fprintf (o_src, "\tleal\t%s, %%eax\n", pgm_label);
  fprintf (o_src, "\tpushl\t%%eax\n");
  fprintf (o_src, "\tleal\t.Lend_pgm_%s, %%eax\n", pgm_label);
  fprintf (o_src, "\tpushl\t%%eax\n");
  stack_offset += 8;		// length of the 2 pushes above
  if (!decimal_comma)
    {
      fprintf (o_src, "\txorl\t%%eax,%%eax\n");
      fprintf (o_src, "\tmovl\t%%eax,decimal_comma\n");
    }
  if (currency_symbol != '$')
    fprintf (o_src, "\tmovb\t$%d,cCurrencySymbol\n", currency_symbol);
  at_procedure++;
}

void
proc_trail (int using)
{
  int i;
  struct lit *v;
  struct list *list;
  struct sym *sy;
  /*char s[9]; */
  char *pgm_label = "main";
  char flag;

  if (using || cob_module_flag)
    {
      pgm_label = program_id;
    }
  fprintf (o_src, ".Lend_pgm_%s:\n", pgm_label);

  //      Screen section io cleanup (curses library).
  if (screen_io_enable != 0)
    asm_call ("do_scrio_finish");

  asm_call ("stop_run");

//      Program return code is stored in register %eax
//      Note:
//        The variable RETURN-CODE is a extention to the 
//        standard, since ANSI COBOL 85 does not support it.

  if ((sy = lookup_symbol (SVAR_RCODE)) == NULL)
    {
      fprintf (o_src, "\tmovl\t$0, %%eax\n");
    }
  else
    {
      if (sy->sec_no == SEC_STACK)
	fprintf (o_src, "\tleal\t-%d(%%ebp), %%edx\n", sy->location);
      else
	fprintf (o_src, "\tleal\tw_base%d+%d, %%edx\n",
		 pgm_segment, sy->location);
      fprintf (o_src, "\tmovl\t(%%edx), %%eax\n");
    }

  fprintf (o_src, "\tjmp\t.LSend_%s\n", pgm_label);
  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".LSend_%s:\n", pgm_label);

  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%ebx\n", stack_offset - 8 - 16);
  fprintf (o_src, "\tmov\t%%ebp,%%esp\n");
  fprintf (o_src, "\tpopl\t%%ebp\n");
  fprintf (o_src, "\tret\n");

  /********** generate .Lfe statement   ************/
  fprintf (o_src, ".Lfe1_%s:\n", pgm_label);
  fprintf (o_src, "\t.size\t%s,.Lfe1_%s-%s\n",
	   pgm_label, pgm_label, pgm_label);


  /********** generate data for literals & fields ************/
  fprintf (o_src, ".data\n\t.align 4\n");

  /* generate static working storage */
  dump_working ();

  /* predefined data for special literals */
  fprintf (o_src, "v_base%d:\nc_base%d:\n", pgm_segment, pgm_segment);

  /**************** generate data for fields *****************/
  for (list = fields_list; list != NULL; list = list->next)
    {
      if (((struct sym *) list->var)->type == 'F')
	{			/* sort files */
	  char sl[21];		/* para inverter a lista */
	  char *s;
	  s = sl;
	  *s++ = 0;		/* final da lista invertida */
	  sy = (struct sym *) list->var;
#ifdef COB_DEBUG
	  fprintf (o_src,
		   "# File: %s, Data loc: v_base+%d, Desc: c_base%d+%d\n",
		   sy->name, sy->location, pgm_segment, sy->descriptor);
#endif
	  sy = (struct sym *) sy->sort_data;
	  while (sy != NULL)
	    {
	      *s++ = (unsigned char) sy->direction;
	      *s++ = (unsigned char) sy->len;
	      sy = (struct sym *) (sy->sort_data);
	    }
	  s--;
	  while (*s)
	    {
	      fprintf (o_src, "\t.byte\t%u,%u\n", *s--, *s--);
	    }
	  fprintf (o_src, "\t.byte\t0\n");
	}
      else if (!SYMBOL_P (list->var))
	{
	  /***** it is a literal *****/
	  int len, tmplen;
	  v = (struct lit *) list->var;
	  //len = v->nick ? 1 : strlen(v->name);
	  len = v->nick ? 1 : v->len;
#ifdef COB_DEBUG
	  fprintf (o_src,
		   "# Literal: %s, Data loc: c_base%d+%d, Desc: c_base+%d\n",
		   sch_convert (v->name), pgm_segment, v->location,
		   v->descriptor);

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
		  s = v->name;
		  i = v->len;
		}
	      emit_lit (s, i);
	      if (i)
		fprintf (o_src, ",0\n");
	      else
		fprintf (o_src, "\t.byte\t0\n");	/* null string? */
	    }
	  else
	    {
	      char *s;
	      s = v->name;
	      fprintf (o_src, "\t.byte\t");
	      while (*s && (*s != decimal_char ()))
		fprintf (o_src, "%d,", *s++);
	      s++;
	      while (*s)
		fprintf (o_src, "%d,", *s++);
	      fprintf (o_src, "0\n");
	    }
	  fprintf (o_src, "\t.long\t%d\n", (v->decimals) ? len - 1 : len);
	  fprintf (o_src, "\t.byte\t'%c',%d,%d\n", v->type, v->decimals, v->all);
	  fprintf (o_src, "\t.long\tc_base%d+%d", pgm_segment, v->descriptor + 11);	/* pointer to the picture */
#ifdef COB_DEBUG
	  fprintf (o_src, "\t# c_base%d+%x(hex)",
		   pgm_segment, v->descriptor + 11);
#endif
	  fprintf (o_src, "\n");

	  if (v->decimals)
	    {
	      if (v->name[v->len - 1] > '9')	/* signed too? */
		fprintf (o_src, "\t.byte\t'S',1,'9',%d,'V',1,'9',%d,0\n",
			 len - v->decimals - 1, v->decimals);
	      else
		fprintf (o_src, "\t.byte\t'9',%d,'V',1,'9',%d,0\n",
			 len - v->decimals - 1, v->decimals);
	    }
	  else if ((v->type == '9') && (v->name[v->len - 1] > '9'))
	    {
	      /* this is a signed literal, so reflect into its picture too */
	      fprintf (o_src, "\t.byte\t'S',1,'9',%d,0\n", len);
	    }
	  else
	    {
	      tmplen = len;
	      while (tmplen > 255)
		{
		  fprintf (o_src, "\t.byte\t\'%c\',%d\n", v->type, 255);
		  tmplen -= 255;
		}
	      fprintf (o_src, "\t.byte\t\'%c\',%d,0\n", v->type, tmplen);

	    }
	}
      else			/*if ( ((struct sym *)list->var)->type!='D' ) */
	{
	/********* it is a normal field ****************/
	  sy = (struct sym *) list->var;
#ifdef COB_DEBUG
	  fprintf (o_src, "# Field: %s, Mem loc: %s, Desc: c_base%d+%d\n",
		   sy->name, memref (sy), pgm_segment, sy->descriptor);
#endif
	  if (sy->redefines != NULL)
	    sy->location = sy->redefines->location;

	  fprintf (o_src, "\t.long\t%d\n", sy->len);

	  flag = sy->flags.just_r ? 2 : 0;
	  flag |= (sy->flags.separate_sign ? 4 : 0);
	  flag |= (sy->flags.leading_sign ? 8 : 0);
	  fprintf (o_src, "\t.byte\t'%c',%d,%d\n",
		   sy->type, sy->decimals, flag);
	  if (sy->type != 'G')
	    {
#ifdef COB_DEBUG
	      fprintf (o_src, "\t.long\tc_base%d+%d\t# c_base%d+%x(hex)\n",
		       pgm_segment, sy->pic, pgm_segment, sy->pic);
#else
	      fprintf (o_src, "\t.long\tc_base%d+%d\n", pgm_segment, sy->pic);
#endif
	      for (i = 0; i < strlen (sy->picstr); i += 2)
		fprintf (o_src, "\t.byte\t\'%c\',%d\n",
			 *(sy->picstr + i),
			 *((unsigned char *) sy->picstr + i + 1));
	      fprintf (o_src, "\t.byte\t0\n");
	    }
	}
    }
/* generate data for files */
  dump_fdesc ();
  /* dump_scr_data(); */
  data_trail ();
  fprintf (o_src, "\n\t.ident\t\"%s %s\"\n", COB_PACKAGE, COB_VERSION);
}

/* 
** dump all static working storage
*/
void
dump_working ()
{

  struct sym *v, *sy;
  struct list *list;
  int fld_len;
  int stabs_type = '3';
  short cur_sec_no = SEC_DATA;

  fprintf (o_src, "w_base%d:\n", pgm_segment);
  for (list = fields_list; list != NULL; list = list->next)
    {
      v = (struct sym *) list->var;
      sy = v;
      if (!SYMBOL_P (v))
	continue;
      if (v->sec_no == SEC_STACK)
	continue;
      if (v->type == 'F' || v->type == 'R')
	continue;
      fld_len = set_field_length (v, 1);
      if (v->sec_no != cur_sec_no && v->sec_no >= SEC_FIRST_NAMED)
	{			// switch of sections
	  if (v->sec_no >= SEC_FIRST_NAMED)
	    fprintf (o_src, "\t.comm\t%s,%d,4\n",
		     sec_name (v->sec_no), fld_len);
	  else
	    fprintf (o_src, ".text\n");
	  cur_sec_no = v->sec_no;
	}
#ifdef COB_DEBUG
      fprintf (o_src, "# FIELD %s, Data Loc: %d(hex: %x) %c\n",
	       v->name, v->location, v->location, v->type);
#endif
      if (cob_stabs_flag)
	{
	  if (sy->type == 'B')
	    {
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
	      fprintf (o_src, ".stabs\t\"%s:S%c\",38,0,0,w_base%d+%d\n",
		       sy->name, stabs_type, pgm_segment, sy->location);
	    }
	  else if (sy->type == 'C')
	    fprintf (o_src,
		     ".stabs\t\"%s:S(1,%d)=ar3;1;%d;4\",38,0,0,w_base%d+%d\n",
		     sy->name, sy->len, sy->len, pgm_segment, 0);
	  else
	    fprintf (o_src,
		     ".stabs\t\"%s:S(1,%d)=ar3;1;%d;2\",38,0,0,w_base%d+%d\n",
		     sy->name, sy->len, sy->len, pgm_segment, sy->location);
	}

      if (v->parent)
	continue;
      if (fld_len)
	{			/* don't alloc dummy (zero storage) symbols */
	  fprintf(o_src,"\t.space\t%d\n",fld_len);
	}
      if (fld_len == 0)
	yyerror ("Invalid picture in %s", v->name);
    }
  /* output tmpvar storage */
  if (tmpvar_max > 0)
    {
      fprintf (o_src, "tv_base%d:\n", pgm_segment);
      fprintf (o_src, "\t.space\t%d\n", tmpvar_max);
    }
}

void
save_field_in_list (struct sym *sy)
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

void
save_literal (struct lit *v, int type)
{
  char *s;
  char *dp;
  int piclen;
  //if (v->type) return; /* already saved */
  s = v->name;
  piclen = 3;			/* assume 'X'-only literal */
  if ((type == '9') && (*(v->name + v->len - 1) > '9'))
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
    {
      piclen += (v->len / 255) * 2;
    }
  v->type = type;
	/****** save literal in fields list for later *******/
  save_field_in_list ((struct sym *) v);
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
}

struct lit *
save_special_literal (char val, char picc, char *nick)
{
  struct lit *v;
  v = install_literal (nick);
  if (v->type)
    return NULL;		/* already saved */
  v->decimals = 0;
  v->type = picc;
  v->nick = (char *) malloc (2);
  v->nick[0] = val;
  v->nick[1] = 0;
  v->all = 0;
  save_field_in_list ((struct sym *) v);
  v->location = literal_offset;
  v->sec_no = SEC_CONST;
  literal_offset += 2;		/* we have only 1-char special literals */
  v->descriptor = literal_offset;
  literal_offset += 14;
  return v;
}

void
put_disp_list (struct sym *sy)
{
  struct list *list, *tmp;
//fprintf(o_src,"# put_disp_list: %s\n",sy->name);
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

int
pic_digits (struct sym *sy)
{
  char *p = NULL;
  int len = 0;
  if (sy == NULL)
    return 0;
  if (!SYMBOL_P (sy))
    {
      len = strlen (sy->name);
      if (strchr (sy->name, decimal_char ()))
	len--;
      if (strchr (sy->name, '+'))
	len--;
      if (strchr (sy->name, '-'))
	len--;
      //printf("pic_digits: %s -> %d\n",sy->name,len);
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
  //printf("pic_digits: %s -> %d\n",sy->name,len);
  return len;
}

int
query_comp_len (struct sym *sy)
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

int
symlen (struct sym *sy)
{
  /*int plen; */
  if (sy->type == 'C')
    return sy->len / 2 + 1;
  else if (LITERAL_P (sy))
    return ((struct lit *) sy)->len;
  return sy->len;
}

int
varsize_ch (struct sym *sy)
{
  switch (symlen (sy))
    {
    case 1:  return 'b';
    case 2:  return 'w';
    default: return 'l';
    }
}

void
add_alternate_key (struct sym *sy, int duplicates)
{
  struct sym *f = curr_file;
  struct alternate_list *alt, *new;
  alt = (struct alternate_list *) f->alternate;
  new = malloc (sizeof (struct alternate_list));
  new->next = alt;
  new->key = sy;
  new->duplicates = duplicates;
  f->alternate = (struct sym *) new;
}

struct list *
insert_list (struct list *l, void *item)
{
  struct list *tmp;
  if (l == NULL)
    {
      l = malloc (sizeof (struct list));
      l->var = item;
      l->next = NULL;
    }
  else
    {
      for (tmp = l; tmp->next != NULL; tmp = tmp->next);
      tmp->next = malloc (sizeof (struct list));
      tmp->next->var = item;
      tmp->next->next = NULL;
    }
  return l;
}

void
free_list (struct list *l)
{
  struct list *tmp;
  while (l != NULL)
    {
      tmp = l->next;
      free (l);
      l = tmp;
    }
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
			    int before_after, struct sym *var)
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
alloc_converting_struct (struct sym *fromvar, struct sym *tovar,
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
alloc_tallying_list (struct tallying_list *tl, struct sym *count,
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
			 struct sym *forvar, struct inspect_before_after *ba)
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
		      struct replacing_by_list *rbl, struct sym *byvar,
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
			 struct sym *replvar, struct sym *byvar,
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
alloc_unstring_delimited (short int all, struct sym *var)
{
  struct unstring_delimited *ud;
  ud = malloc (sizeof (struct unstring_delimited));
  ud->next = NULL;
  ud->var = var;
  ud->all = all;
  return ud;
}

struct unstring_destinations *
alloc_unstring_dest (struct sym *var, struct sym *delim, struct sym *count)
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
alloc_string_from (struct sym *var, struct sym *delim)
{
  struct string_from *sf;
  sf = malloc (sizeof (struct string_from));
  sf->next = NULL;
  sf->var = var;
  sf->delim = delim;
  return sf;
}

void
gen_unstring (struct sym *var, struct unstring_delimited *delim,
	      struct unstring_destinations *dest, struct sym *ptr,
	      struct sym *tally)
{

  struct unstring_destinations *dest1;
  struct unstring_delimited *delim1;

  fprintf (o_src, "# UNSTRING %s\n", var->name);
  gen_loadvar ((struct sym *) NULL);	/* mark the end of destinations */
  while (dest)
    {
      gen_loadvar (dest->count);
      gen_loadvar (dest->delim);
      gen_loadvar (dest->var);
      dest1 = dest;
      dest = dest->next;
      free (dest1);
    }
  gen_loadvar ((struct sym *) NULL);	/* mark the end of delimiters */
  while (delim)
    {
      push_immed (delim->all);	/* push "all" flag */
      gen_loadvar (delim->var);
      delim1 = delim;
      delim = delim->next;
      free (delim1);
    }
  asm_call_3 ("cob_unstring", var, ptr, tally);
}

void
gen_stringcmd (struct string_from *sf, struct sym *sy, struct sym *ptr)
{
  struct string_from *sf1;
  fprintf (o_src, "# STRING into %s\n", sy->name);
  gen_loadvar ((struct sym *) NULL);	/* mark the end of variables */
  while (sf)
    {
      gen_loadvar (sf->delim);
      gen_loadvar (sf->var);
      sf1 = sf;
      sf = sf->next;
      free (sf1);
    }
  asm_call_2 ("cob_stringcmd", sy, ptr);
}

void
gen_display_screen (struct sym *sy, int main)
{
  struct sym *tmp;
  struct list *tmpl;
  if (main)
    {
      fprintf (o_src, "#                      Screen Section: %s\n",
	       sy->name);}
  if (sy->son == NULL)
    {
      fprintf (o_src, "#                      Screen Field: %s\n", sy->name);
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
      tmpl = disp_list;
      disp_list = disp_list->next;
      free (tmpl);
    }
}

void
gen_display (int dupon, int nl)
{
  struct list *tmp;
  /*int len; */
  int dspflags;
  int first = 1;
  struct sym *sy;

#ifdef COB_DEBUG
  fprintf (o_src, "### DISPLAY\n");
#endif

  if (disp_list)
    {
      /* separate screen displays from display of regular variables */
      sy = (struct sym *) disp_list->var;
      if (disp_list && !LITERAL_P (sy))
	if (!REFMOD_P (sy) && !SUBREF_P (sy) && sy->scr)
	  {
	    gen_display_screen (disp_list->var, 1);
	    return;
	  }
      /* continue w/a regular variable display */
      if (nl & 2)
	if (screen_io_enable == 0)
	  {
	    push_immed (dupon);
	    asm_call ("display_erase");
	  }
    }
  while (disp_list)
    {
      sy = disp_list->var;

      if (screen_io_enable == 0)
	{
	  push_immed (dupon);
	  gen_loadvar (sy);
	  asm_call ("display");
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
      tmp = disp_list;
      disp_list = disp_list->next;
      free (tmp);
    }
  if (!(nl & 1))
    if (screen_io_enable == 0)
      {
	push_immed (dupon);
	asm_call ("newline");
      }
}

void
gen_gotoxy_expr ()
{
  stackframe_cnt += 16;		/* eliminate the coords expressions */
  asm_call ("cob_goxy_expr");
}

void
gen_accept (struct sym *sy, int echo, int main)
{
  struct sym *tmp;
  if (sy->scr)
    {				/* screen or screen-item accept */
      if (main)
	{
	  fprintf (o_src, "#                      Screen Section: %s\n",
		   sy->name);
	}
      if (sy->son == NULL)
	{
	  fprintf (o_src, "#                      Screen Field: %s\n",
		   sy->name);
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
      fprintf (o_src, "\tmovl\t$c_base%d+%u, %%eax\n",
	       pgm_segment, sy->descriptor);
      push_eax ();
      gen_loadloc (sy);
      if (screen_io_enable == 0)
	asm_call ("accept_std");
      else
	asm_call ("accept_curses");
    }
}

void
gen_accept_from_time (struct sym *sy)
{
  gen_loadloc (sy);
  asm_call ("accept_time");
}

void
gen_accept_from_date (struct sym *sy)
{
  gen_loadloc (sy);
  asm_call ("accept_date");
}

void
gen_accept_from_day (struct sym *sy)
{
  gen_loadloc (sy);
  asm_call ("accept_day");
}

void
gen_accept_from_day_of_week (struct sym *sy)
{
  gen_loadloc (sy);
  asm_call ("accept_day_of_week");
}

void
gen_accept_from_inkey (struct sym *sy)
{
  gen_loadloc (sy);
  asm_call ("accept_inkey");
}

void
gen_accept_from_cmdline (struct sym *sy)
{

  struct sym *sy1;

  gen_loadvar (sy);
  fprintf (o_src, "\tmovl\t12(%%ebp), %%eax\n");
  push_eax ();
  fprintf (o_src, "\tmovl\t8(%%ebp), %%eax\n");
  push_eax ();
  asm_call ("accept_cmd_line");

//      Set RETURN-CODE with the value returned by 
//      the "accept_cmd_line" function, which is stored 
//      in register %eax

  if ((sy1 = lookup_symbol (SVAR_RCODE)) != NULL)
    {
      if (sy1->sec_no == SEC_STACK)
	{
	  fprintf (o_src, "\tleal\t-%d(%%ebp), %%edx\n", sy1->location);
	}
      else
	{
	  fprintf (o_src, "\tleal\tw_base%d+%d, %%edx\n",
		   pgm_segment, sy1->location);
	}
      fprintf (o_src, "\tmovl\t%%eax, (%%edx)\n");
    }
}

void
gen_accept_env_var (struct sym *sy, struct lit *v)
{

  struct sym *sy2;

  gen_loadloc ((struct sym *) v);
  asm_call_1 ("accept_env_var", sy);

//      Set RETURN-CODE with the value returned by 
//      the "accept_cmd_line" function, which is stored 
//      in register %eax

  if ((sy2 = lookup_symbol (SVAR_RCODE)) != NULL)
    {
      if (sy2->sec_no == SEC_STACK)
	fprintf (o_src, "\tleal\t-%d(%%ebp), %%edx\n", sy2->location);
      else
	fprintf (o_src, "\tleal\tw_base%d+%d, %%edx\n", pgm_segment, sy2->location);
      fprintf (o_src, "\tmovl\t%%eax, (%%edx)\n");
    }
}

/******** structure allocation for perform info(s) ***********/

struct perf_info *
create_perf_info (struct sym *sy1, struct sym *sy2, unsigned long lj,
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
check_perform_variables (struct sym *sy1, struct perform_info *pi1)
{

  int i, j, k;

  j = 0;
  for (i = 0; i < 4; i++)
    {
      if (pi1->pf[i] != NULL)
	{
	  j++;
#ifdef COB_DEBUG
	  if (cob_trace_codegen)
	    fprintf (stderr,
		     "trace: check_perform_variables: var(%d:%d) '%s'\n", 
		     i, j, pi1->pf[i]->pname2->name); 
#endif
	}
    }

  for (i = 0; i < j; i++)
    {
#ifdef COB_DEBUG
      if (cob_trace_codegen)
	fprintf (stderr,
		 "debug trace: check_perform_variables: var1='%s' var2(%d)='%s'\n", 
		 sy1->name, i, pi1->pf[i]->pname2->name); 
#endif
      if (strcmp (sy1->name, pi1->pf[i]->pname2->name) == 0)
	{
	  return sy1->name;
	}
    }

  for (i = 0; i < j; i++)
    {
      for (k = i + 1; k < j; k++)
	{
#ifdef COB_DEBUG
	  if (cob_trace_codegen)
	    fprintf (stderr,
		     "trace: check_perform_variables: var1(%d)='%s' var2(%d)='%s'\n", 
		     i, pi1->pf[i]->pname2->name, k, pi1->pf[k]->pname2->name);
#endif
	  if (strcmp (pi1->pf[i]->pname2->name, pi1->pf[k]->pname2->name) == 0)
	    return pi1->pf[i]->pname2->name;
	}
    }

  return NULL;
}

/******** structure allocation for math verbs variables ***********/

struct math_var *
create_mathvar_info (struct math_var *mv, struct sym *sy, unsigned int opt)
{

  struct math_var *rf, *tmp1, *tmp2;

  rf = malloc (sizeof (struct math_var));
  rf->sname = sy;
  rf->rounded = opt;
  rf->next = NULL;

#ifdef COB_DEBUG
  if (cob_trace_codegen)
    {
      fprintf (stderr,
	       "trace : create_mathvar_info 0: sy->name=%s;\n", sy->name);
      fprintf (stderr,
	       "trace : create_mathvar_info 1: rf->sname->name=%s;\n",
	       rf->sname->name);
    }
#endif

  if (mv == NULL)
    {
      tmp2 = rf;
      tmp1 = rf;
    }
  else
    {
      tmp1 = mv;
      tmp2 = mv;
      while (tmp1->next != NULL)
	{
#ifdef COB_DEBUG
	  if (cob_trace_codegen)
	    fprintf (stderr,
		     "trace : create_mathvar_info 2: tmp1->sname->name=%s;\n", 
		     tmp1->sname->name); 
#endif
	  tmp1 = tmp1->next;
	}
      tmp1->next = rf;
    }

#ifdef COB_DEBUG
  if (cob_trace_codegen)
    {
      fprintf (stderr,
	       "trace : create_mathvar_info 3: tmp1->sname->name=%s;\n", 
	       tmp1->sname->name); 
      
      tmp1 = tmp2; 
      while (tmp1 != NULL)
	{
	  fprintf(stderr,
		  "trace : create_mathvar_info 4: tmp1->sname->name=%s;\n", 
		  tmp1->sname->name); 
	  tmp1 = tmp1->next;
	}
    }
#endif

  return tmp2;
}

struct math_ose *
math_on_size_error0 (void)
{
  struct math_ose *v;
  v = malloc (sizeof (struct math_ose));
  v->ose = 0;			/* type of option */
  v->lbl1 = 0;			/* call label name 1 - on_size */
  v->lbl2 = 0;			/* call label name 2 - not_on_size */
  v->lbl4 = loc_label++;	/* determine bypass label name */
  gen_jmplabel (v->lbl4);	/* generate bypass jump */
  return v;
}

struct math_ose *
math_on_size_error1 (struct math_ose *v)
{
  v->ose = loc_label++;
  fprintf (o_src, ".L%d:\n", (int) v->ose);
  return v;
}

void
math_on_size_error2 (void)
{
  fprintf (o_src, "\tret\n");
}

void
math_on_size_error3 (struct math_ose *v)
{

  unsigned long lbl1, lbl2;
  lbl1 = loc_label++;

  switch (v->ose)
    {
    case 1:
      fprintf (o_src, "\tcmpl\t$0, cob_size_error_flag\n");
      fprintf (o_src, "\tje\t.L%ld\n", lbl1);
      fprintf (o_src, "\tleal\t.L%ld, %%eax\n", lbl1);
      fprintf (o_src, "\tpushl\t%%eax\n");
      gen_jmplabel (v->lbl1);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", lbl1);
      break;

    case 2:
      fprintf (o_src, "\tcmpl\t$0, cob_size_error_flag\n");
      fprintf (o_src, "\tjne\t.L%ld\n", lbl1);
      fprintf (o_src, "\tleal\t.L%ld, %%eax\n", lbl1);
      fprintf (o_src, "\tpushl\t%%eax\n");
      gen_jmplabel (v->lbl2);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", lbl1);
      break;

    default:
      lbl2 = loc_label++;
      fprintf (o_src, "\tcmpl\t$0, cob_size_error_flag\n");
      fprintf (o_src, "\tje\t.L%ld\n", lbl1);
      fprintf (o_src, "\tleal\t.L%ld, %%eax\n", lbl2);
      fprintf (o_src, "\tpushl\t%%eax\n");
      gen_jmplabel (v->lbl1);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", lbl1);
      fprintf (o_src, "\tleal\t.L%ld, %%eax\n", lbl2);
      fprintf (o_src, "\tpushl\t%%eax\n");
      gen_jmplabel (v->lbl2);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", lbl2);
      break;
    }
}

struct math_ose *
math_on_size_error4 (struct math_ose *v, unsigned long ty)
{

  /* ose=ty;     type of option */
  /* lbl1        call label name 1 - on_size */
  /* lbl2        call label name 2 - not_on_size */

  switch (ty)
    {
    case 1:
      v->lbl1 = v->ose;
      v->ose = ty;
      break;

    case 2:
      v->lbl2 = v->ose;
      v->ose = ty;
      break;

    case 3:
      v->lbl2 = v->ose;
      v->ose = ty;
      break;

    default:
      break;
    }

  return v;
}

/******** generic structure allocation and code genertion ***********/
struct ginfo *
ginfo_container0 (void)
{
  struct ginfo *v;
  v = malloc (sizeof (struct ginfo));
  v->sel = 0;			/* type of option */
  v->lbl1 = 0;			/* call label name 1 - true */
  v->lbl2 = 0;			/* call label name 2 - not true */
  v->lbl3 = loc_label++;	/* End of statement label name */
  v->lbl4 = 0;			/* not used */
  v->lbl5 = loc_label++;	/* determine test  bypass label name */
  gen_jmplabel (v->lbl5);	/* generate test bypass jump */
  return v;
}

struct ginfo *
ginfo_container1 (struct ginfo *v)
{
  v->sel = loc_label++;
  fprintf (o_src, ".L%d:\n", (int) v->sel);
  return v;
}

void
ginfo_container2 (struct ginfo *v, unsigned long ty)
{

  switch (ty)
    {
    case 1:
      v->lbl1 = v->sel;
      v->sel = 0;
      /*v->lbl3=loc_label++;    return 1 label name */
      /* gen_jmplabel(v->lbl3);  generate return 1 label jump */
      break;

    case 2:
      v->lbl2 = v->sel;
      v->sel = 0;
      /*v->lbl4=loc_label++;    return 2 label name */
      /*gen_jmplabel(v->lbl4); generate return 2 label jump */
      break;
    }
  gen_jmplabel (v->lbl3);	/* generate end label jump */
}

struct ginfo *
ginfo_container3 (struct ginfo *v, unsigned long ty)
{

  /* sel=ty;     type of option */
  /* lbl1        call label name 1 - true  */
  /* lbl2        call label name 2 - not true */

  switch (ty)
    {
    case 1:
      v->sel = ty;
      break;

    case 2:
      v->sel = ty;
      break;

    case 3:
      v->sel = ty;
      break;

    default:
      break;
    }

  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".L%d:\n", (int) v->lbl5);

  return v;
}

void
ginfo_container4 (struct ginfo *v)
{

  switch (v->sel)
    {
    case 1:
      fprintf (o_src, "\tcmpl\t$0, %%eax\n");
      fprintf (o_src, "\tjne\t.L%ld\n", v->lbl1);
      /* gen_jmplabel(v->lbl1); */
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", v->lbl3);
      break;

    case 2:
      fprintf (o_src, "\tcmpl\t$0, %%eax\n");
      fprintf (o_src, "\tje\t.L%ld\n", v->lbl2);
      /* gen_jmplabel(v->lbl2);  */
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", v->lbl3);
      break;

    default:
      fprintf (o_src, "\tcmpl\t$0, %%eax\n");
      fprintf (o_src, "\tje\t.L%ld\n", v->lbl2);
      gen_jmplabel (v->lbl1);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%ld:\n", v->lbl3);
      /* gen_jmplabel(v->lbl2);  */
      /* fprintf(o_src,"\t.align 16\n"); */
      /* fprintf(o_src,".L%ld:\n",v->lbl4); */
      break;
    }

}

struct invalid_key_element *
gen_before_invalid_key ()
{
  struct invalid_key_element *p =
    malloc (sizeof (struct invalid_key_element));
  p->lbl1 = loc_label++;
  p->lbl2 = loc_label++;
  p->lbl3 = loc_label++;
  gen_jmplabel (p->lbl1);
  gen_dstlabel (p->lbl2);
  return p;
}

struct invalid_key_element *
gen_after_invalid_key (struct invalid_key_element *p)
{
  gen_jmplabel (p->lbl3);
  return p;
}

struct invalid_keys *
gen_invalid_keys (struct invalid_key_element *p1,
		  struct invalid_key_element *p2)
{
  struct invalid_keys *p = malloc (sizeof (struct invalid_keys));
  p->invalid_key = p1;
  p->not_invalid_key = p2;
  if (p1)
    gen_dstlabel (p1->lbl1);
  if (p2)
    gen_dstlabel (p2->lbl1);
  return p;
}

void
gen_test_invalid_keys (struct invalid_keys *p)
{
  if (p->invalid_key)
    {
      int lbl = loc_label++;
#if COB_DEBUG
      fprintf (o_src, "# Test for INVALID KEY\n");
#endif
      fprintf (o_src, "\tcmp\t$23, %%eax\n");
      fprintf (o_src, "\tjz\t.L%d\n", lbl);
      gen_jmplabel (p->invalid_key->lbl2);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%d:\n", lbl);
    }

  if (p->not_invalid_key)
    {
      int lbl = loc_label++;
#if COB_DEBUG
      fprintf (o_src, "# Test for NOT INVALID KEY\n");
#endif
      fprintf (o_src, "\tcmp\t$0, %%eax\n");
      fprintf (o_src, "\tjz\t.L%d\n", lbl);
      gen_jmplabel (p->not_invalid_key->lbl2);
      fprintf (o_src, "\t.align 16\n");
      fprintf (o_src, ".L%d:\n", lbl);
    }

  if (p->invalid_key)
    gen_dstlabel (p->invalid_key->lbl3);
  if (p->not_invalid_key)
    gen_dstlabel (p->not_invalid_key->lbl3);

  if (p->invalid_key)
    free (p->invalid_key);
  if (p->not_invalid_key)
    free (p->not_invalid_key);
  free (p);
}

/******** functions to generate math verbs ***********/

struct sym *
create_expr (char op, struct sym *left, struct sym *right)
{
  struct expr *left_expr = (struct expr *) left;
  struct expr *right_expr = (struct expr *) right;
  struct expr *e = malloc (sizeof (struct expr));
  struct list *list = malloc (sizeof (struct list));
  e->litflag = 5;
  e->op = op;
  e->left = left_expr;
  e->right = right_expr;
  expr_list = list;
  list->next = NULL;
  list->var = e;
  return (struct sym *) e;
}

void
free_expr (struct expr *e)
{
  if (e && EXPR_P (e))
    {
      free_expr (EXPR_LEFT (e));
      free_expr (EXPR_RIGHT (e));
      free (e);
    }
}

void
free_expr_list ()
{
  struct list *list;
  struct expr *e;
  for (list = expr_list; list != NULL; list = list->next)
    {
      e = (struct expr *) list->var;
      free_expr (e);
    }
  expr_list = NULL;
}

void
gen_add (struct sym *sy1, struct sym *sy2, int rnd)
{
  push_expr (sy2);
  push_expr (sy1);
  asm_call ("cob_add");
  assign_expr (sy2, rnd);
}

void
gen_subtract (struct sym *sy1, struct sym *sy2, int rnd)
{
  push_expr (sy2);
  push_expr (sy1);
  asm_call ("cob_sub");
  assign_expr (sy2, rnd);
}

void
gen_multiply (struct sym *sy1, struct sym *sy2, struct sym *sy3, int rnd)
{
  push_expr (sy2);
  push_expr (sy1);
  asm_call ("cob_mul");
  assign_expr (sy3, rnd);
}

void
gen_divide (struct sym *sy1, struct sym *sy2,
	    struct sym *sy3, struct sym *sy4, int rnd)
{
  push_expr (sy2);
  push_expr (sy1);
  asm_call ("cob_div");
  assign_expr (sy3, rnd);

  if (sy4)
    {
      push_expr (sy3);
      push_expr (sy2);
      asm_call ("cob_mul");
      push_expr (sy1);
      asm_call ("cob_sub");
      assign_expr (sy4, rnd);
    }
}

void
gen_compute (struct math_var *vl1, struct sym *sy1, struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl1; vl1 = vl1->next)
    {
      push_expr (sy1);
      assign_expr (vl1->sname, vl1->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_add1 (struct math_var *vl1, struct math_var *vl2, struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl2; vl2 = vl2->next)
    {
      struct math_var *vl;

      push_expr (vl2->sname);
      for (vl = vl1; vl != NULL; vl = vl->next)
	{
	  push_expr (vl->sname);
	  asm_call ("cob_add");
	}
      assign_expr (vl2->sname, vl2->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_add2 (struct math_var *vl1, struct math_var *vl2,
	  struct sym *sy1, struct math_ose *ose)
{
  if (ose != NULL)
    gen_dstlabel (ose->lbl4);

  for (; vl2; vl2 = vl2->next)
    {
      struct math_var *vl = vl1;

      if (sy1)
	push_expr (sy1);
      else
	{
	  push_expr (vl1->sname);
	  vl = vl->next;
	  if (vl == NULL)
	    yyerror ("At least 2 variables literals required in ADD statement");
	}
      for (; vl; vl = vl->next)
	{
	  push_expr (vl->sname);
	  asm_call ("cob_add");
	}
      assign_expr (vl2->sname, vl2->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_subtract1 (struct math_var *vl1, struct math_var *vl2,
	       struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl2; vl2 = vl2->next)
    {
      struct math_var *vl;

      push_expr (vl2->sname);
      for (vl = vl1; vl; vl = vl->next)
	{
	  push_expr (vl->sname);
	  asm_call ("cob_sub");
	}
      assign_expr (vl2->sname, vl2->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_subtract2 (struct math_var *vl1, struct math_var *vl2, struct sym *sy1,
	       struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl2; vl2 = vl2->next)
    {
      struct math_var *vl;

      push_expr (sy1);
      for (vl = vl1; vl; vl = vl->next)
	{
	  push_expr (vl->sname);
	  asm_call ("cob_sub");
	}
      assign_expr (vl2->sname, vl2->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_multiply1 (struct math_var *vl1, struct sym *sy1, struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl1; vl1 = vl1->next)
    {
      push_expr (sy1);
      push_expr (vl1->sname);
      asm_call ("cob_mul");
      assign_expr (vl1->sname, vl1->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_multiply2 (struct math_var *vl1, struct sym *sy1, struct sym *sy2,
	       struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl1; vl1 = vl1->next)
    {
      push_expr (sy1);
      push_expr (sy2);
      asm_call ("cob_mul");
      assign_expr (vl1->sname, vl1->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_divide1 (struct math_var *vl1, struct sym *sy1, struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl1; vl1 = vl1->next)
    {
      push_expr (vl1->sname);
      push_expr (sy1);
      asm_call ("cob_div");
      assign_expr (vl1->sname, vl1->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

void
gen_divide2 (struct math_var *vl1, struct sym *sy1, struct sym *sy2,
	     struct math_ose *ose)
{
  if (ose)
    gen_dstlabel (ose->lbl4);

  for (; vl1; vl1 = vl1->next)
    {
      push_expr (sy1);
      push_expr (sy2);
      asm_call ("cob_div");
      assign_expr (vl1->sname, vl1->rounded);

      if (ose)
	math_on_size_error3 (ose);
    }
}

/******** functions for refmoded var manipulation ***********/
static int
check_refmods (struct sym *var)
{
  struct refmod *ref = (struct refmod *) var;
  struct sym *sy = ref->sym;

  if (SUBREF_P (sy))
    sy = SUBREF_SYM (sy);

  return (sy == NULL) ? 1 : 0;
}

struct refmod *
create_refmoded_var (struct sym *sy, struct sym *syoff, struct sym *sylen)
{
  struct refmod *ref;
  ref = malloc (sizeof (struct refmod));
  ref->litflag = 4;
  ref->sym = sy;
  ref->off = syoff;
  ref->len = sylen;
  ref->slot = refmod_slots++;
  check_refmods ((struct sym *) ref);
  return ref;
}

void
gen_subscripted (struct subref *subs)
{
  struct subref *ref;
  struct sym *sy;
  int outer_pushed, eax_in_use;
  ref = subs->next;		/* here start the subscripts */
  sy = subs->sym;		/* here our array */
  fprintf (o_src, "# gen_subscripted\n");
  outer_pushed = 0;
  eax_in_use = 0;
  while (ref)
    {
      if (((struct sym *) (ref->sym))->type == 'B' && symlen (ref->sym) > 4)
	yyerror ("warning: we don't handle this large subscript");
      if (eax_in_use && !outer_pushed)
	{
	  /* accumulate offsets here */
	  fprintf (o_src, "\tpushl\t%%eax\t# outer_pushed\n");
	  outer_pushed = 1;
	}
      eax_in_use = 1;
      value_to_eax (ref->sym);
      fprintf (o_src, "\tpushl\t%%eax\n");
      while (ref->litflag != ',')
	{
	  ref = ref->next;
	  if (symlen (ref->sym) > 4)
	    yyerror ("warning: we don't handle this large subscript");
	  value_to_eax (ref->sym);
	  if (ref->litflag == '+')
	    fprintf (o_src, "\taddl\t%%eax,0(%%esp)\n");
	  else
	    fprintf (o_src, "\tsubl\t%%eax,0(%%esp)\n");
	}
      /* find the first parent var that needs subscripting */
      while (sy && sy->times == 1)
	sy = sy->parent;
      fprintf (o_src, "\tpopl\t%%eax\n");
      fprintf (o_src, "\tdecl\t%%eax\n");	/* subscript start at 1 */
      if (sy->len != 1)
	{
	  fprintf (o_src, "\tmovl\t$%d, %%edx\n", symlen (sy));
	  fprintf (o_src, "\timull\t%%edx\n");
	}
      if (outer_pushed)
	{
	  fprintf (o_src, "\taddl\t%%eax,0(%%esp)\n");
	}
      if (sy)
	sy = sy->parent;
      ref = ref->next;
    }
  if (outer_pushed)
    fprintf (o_src, "\tpopl\t%%eax\n");	/* return offset in %eax */
}

struct sym *
get_variable_item (struct sym *sy)
{
  struct sym *son, *item;
  if (!SYMBOL_P (sy))
    return NULL;
  if (sy->occurs != NULL)
    return sy;
  for (son = sy->son; son != NULL; son = son->brother)
    {
      if ((item = get_variable_item (son)))
	return item;
    }
  return NULL;
}

void
gen_temp_storage (int size)
{
  stackframe_cnt += 4;
  fprintf (o_src, "\tpushl\t$tv_base%d+%d\n", pgm_segment, tmpvar_offset);
  tmpvar_offset += size;
  if (tmpvar_offset > tmpvar_max)
    {
      tmpvar_max = tmpvar_offset;
    }
}

void
adjust_desc_length (struct sym *sy)
{
  int stack_save = stackframe_cnt;
  struct sym *item;
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

static void
gen_pushval (struct sym *sy)
{
  unsigned offset;
  struct sym *var;

#ifdef COB_DEBUG
  fprintf (o_src, "#gen_pushval\n");
#endif
  if (SUBREF_P (sy))
    {
      gen_subscripted ((struct subref *) sy);
      var = SUBREF_SYM (sy);
      if (var->linkage_flg)
	{
	  struct sym *tmp = var;
	  while (tmp->linkage_flg == 1)
	    tmp = tmp->parent;
	  offset = var->location - tmp->location;
	  if (symlen (var) >= 4)
	    fprintf (o_src, "\tmovl %d(%%ebp), %%ebx\n", tmp->linkage_flg);
	  else
	    fprintf (o_src, "\tmovs%cl %d(%%ebp), %%ebx\n",
		     varsize_ch (var), tmp->linkage_flg);
	  if (offset)
	    fprintf (o_src, "\taddl\t$%d, %%ebx\n", offset);
	  fprintf (o_src, "\taddl\t%%eax, %%ebx\n");
	  push_ebx ();
	}
      else
	{
	  fprintf (o_src, "\tleal\t-%d(%%ebp), %%ebx\n", var->location);
	  fprintf (o_src, "\taddl\t%%ebx,%%eax\n");
	  push_at_eax (var);
	}
    }
  else if (SYMBOL_P (sy))
    {
      load_address (sy);
      push_at_eax (sy);
    }
  else
    {
      value_to_eax(sy);
    }
}

void
gen_store_fnres (struct sym *sy)
{
  if (sy == NULL)
    return;
  switch (sy->type)
    {
    case 'B':
      switch (symlen (sy))
	{
	case 4:
	  fprintf (o_src, "\tmovl\t%%eax, %s\n", memrefat (sy));
	  break;
	case 2:
	  fprintf (o_src, "\tmov\t%%ax, %s\n", memrefat (sy));
	  break;
	};
      break;
    default:
      break;
    };
}

int
is_numeric_sy (struct sym *sy)
{
  char type;
  if (SUBREF_P (sy))
    sy = SUBREF_SYM (sy);

  type = sy->type;
  if ((type == '9') || (type == 'B') || (type == 'C') || (type == 'U'))
    return 1;
  return 0;
}

void
gen_class_check (struct sym *sy, int class)
{
  int invert = 0;
  class &= ~(COND_UNARY | COND_CLASS);
  if (class & 4)
    {				/* was it inverted (NOT) ? */
      class ^= 7;
      invert++;
    }
  if (class == CLASS_NUMERIC)
    {
      if (sy)			/* don't save already pushed variable */
	gen_loadvar (sy);
      else
	stackframe_cnt += 8;
      asm_call ("cob_check_numeric");
      fprintf (o_src, "\tand\t%%eax,%%eax\n");
    }
  else
    {
      /* from now on, only alphabetic tests are allowed */
      switch (class)
	{
	case CLASS_ALPHABETIC:
	  asm_call_1 ("cob_check_alphabetic", sy);
	  break;
	case CLASS_ALPHABETIC_UPPER:
	  asm_call_1 ("cob_check_upper", sy);
	  break;
	case CLASS_ALPHABETIC_LOWER:
	  asm_call_1 ("cob_check_lower", sy);
	  break;
	default:
	  yyerror ("unknown class condition");
	  break;
	}
      fprintf (o_src, "\tand\t%%eax,%%eax\n");
    }
  if (invert)
    gen_not ();
}

void
gen_inspect (struct sym *var, void *list, int operation)
{
  /*struct inspect_before_after *ba,*ba1; */
  struct tallying_list *tl, *tl1;
  struct tallying_for_list *tfl, *tfl1;
  struct replacing_list *rl, *rl1;
  struct replacing_by_list *rbl, *rbl1;
  struct converting_struct *cv;

  if (!operation)
    {
      if (!list)
	return;
      fprintf (o_src, "# INSPECT TALLYING %s\n", var->name);
      gen_loadvar ((struct sym *) NULL);
      tl = (struct tallying_list *) list;
      while (tl)
	{
	  tfl = tl->tflist;
	  push_immed (0);
	  while (tfl)
	    {
	      gen_loadvar (tfl->before_after->after);
	      gen_loadvar (tfl->before_after->before);
	      if (tfl->options != INSPECT_CHARACTERS)
		{
		  gen_loadvar (tfl->forvar);
		}
	      push_immed (tfl->options);
	      free (tfl->before_after);
	      tfl1 = tfl;
	      tfl = tfl->next;
	      free (tfl1);
	    }
	  gen_loadvar (tl->count);
	  tl1 = tl;
	  tl = tl->next;
	  free (tl1);
	}
      asm_call_1 ("cob_inspect_tallying", var);
    }
  else if (operation == 1)
    {
      if (!list)
	return;
      fprintf (o_src, "# INSPECT REPLACING %s\n", var->name);
      rl = (struct replacing_list *) list;
      push_immed (0);
      while (rl)
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
	      rbl = rl->replbylist;
	      while (rbl)
		{
		  gen_loadvar (rbl->before_after->after);
		  gen_loadvar (rbl->before_after->before);
		  gen_loadvar (rbl->byvar);
		  gen_loadvar (rbl->replvar);
		  free (rbl->before_after);
		  rbl1 = rbl;
		  rbl = rbl->next;
		  free (rbl1);
		  push_immed (rl->options);
		}
	    }
	  rl1 = rl;
	  rl = rl->next;
	  free (rl1);
	}
      asm_call_1 ("cob_inspect_replacing", var);
    }
  else
    {
      fprintf (o_src, "# INSPECT CONVERTING %s\n", var->name);
      cv = (struct converting_struct *) list;
      gen_loadvar (cv->before_after->after);
      gen_loadvar (cv->before_after->before);
      gen_loadvar (cv->tovar);
      gen_loadvar (cv->fromvar);
      gen_loadvar (var);
      asm_call ("cob_inspect_converting");
    }
}

static void
gen_move_1 (struct sym *src)
{
  if (src == (struct sym *) spe_lit_ZE)
    asm_call ("cob_move_zero");
  else if (src == (struct sym *) spe_lit_SP)
    asm_call ("cob_move_space");
  else
    asm_call_1 ("cob_move", src);
}

void
gen_move (struct sym *src, struct sym *dst)
{
#ifdef COB_DEBUG
  {
    struct sym *esys = src, *esyd = dst;
    if (REFMOD_P (esys))
      esys = ((struct refmod *) esys)->sym;
    if (REFMOD_P (esyd))
      esyd = ((struct refmod *) esyd)->sym;
    fprintf (o_src, "### MOVE %s TO ", sch_convert (esys->name));
    fprintf (o_src, "%s\n", sch_convert (esyd->name));
  }
#endif

  gen_loadvar (dst);
  gen_move_1 (src);
}

/* The following functions will be activated when we change from
   defining the outermost group to define each elementary item. */
void
gen_movecorr (struct sym *sy1, struct sym *sy2)
{
  struct sym *t1, *t2;
  if (!(SYMBOL_P (sy1) && SYMBOL_P (sy2)))
    {
      yyerror ("sorry we don't handle this case yet!");
      return;
    }
#ifdef COB_DEBUG
  fprintf (o_src, "# MOVE CORR %s --> %s\n", sy1->name, sy2->name);
#endif

  for (t1 = sy1->son; t1 != NULL; t1 = t1->brother)
    if (!t1->redefines && t1->times == 1)
      for (t2 = sy2->son; t2 != NULL; t2 = t2->brother)
	if (!t2->redefines && t2->times == 1)
	  if (strcmp (t1->name, t2->name) == 0)
	    {
	      if ((t1->type != 'G') || (t2->type != 'G'))
		gen_move (t1, t2);
	      else
		gen_movecorr (t1, t2);
	    }
}

static void
gen_initialize_1 (struct sym *sy)
{
  if (!sy->flags.in_redefinition)
    {
      if (sy->type == 'G')
	{
	  int lab = 0;
	  struct sym *p;
	  if (sy->times != 1)
	    {
	      lab = loc_label++;
	      fprintf (o_src, "\tpopl\t%%eax\n");
	      fprintf (o_src, "\tpushl\t%%ebx\n");
	      fprintf (o_src, "\tpushl\t%%eax\n");
	      fprintf (o_src, "\tmovl\t$%d, %%ebx\n", sy->times);
	      fprintf (o_src, ".L%d:\n", lab);
	    }
	  for (p = sy->son; p; p = p->brother)
	    gen_initialize_1 (p);
	  if (sy->times != 1)
	    {
	      fprintf (o_src, "\tdecl\t%%ebx\n");
	      fprintf (o_src, "\tjnz\t.L%d\n", lab);
	      fprintf (o_src, "\tpopl\t%%eax\n");
	      fprintf (o_src, "\tpopl\t%%ebx\n");
	      fprintf (o_src, "\tpushl\t%%eax\n");
	    }
	}
      else
	{
	  int i;
	  for (i = 0; i < sy->times; i++)
	    {
	      gen_loaddesc (sy);
	      gen_move_1 (get_init_symbol (sy->type));
	      fprintf (o_src, "\taddl\t$%d, 0(%%esp)\n", symlen (sy));
	    }
	}
    }
}

void
gen_initialize (struct sym *sy)
{
#ifdef COB_DEBUG
  fprintf (o_src, "# INITIALIZE %s, type %c\n", sy->name, sy->type);
#endif
  init_ctype = ' ';
  get_nb_fields (sy, sy->times, 0);
  if (init_ctype != '&')
    gen_move (get_init_symbol (init_ctype), sy);
  else
    {
      loadloc_to_eax (sy);
      fprintf (o_src, "\tpushl\t%%eax\n");
      gen_initialize_1 (sy);
      fprintf (o_src, "\tpopl\t%%eax\n");
    }
}

void
gen_addcorr (struct sym *sy1, struct sym *sy2, int rnd)
{
  struct sym *t1, *t2;
  if (!(SYMBOL_P (sy1) && SYMBOL_P (sy2)))
    {
      yyerror ("sorry we don't handle this case yet!");
      return;
    }
#ifdef COB_DEBUG
  fprintf (o_src, "# ADD CORR %s --> %s\n", sy1->name, sy2->name);
#endif
  for (t1 = sy1->son; t1 != NULL; t1 = t1->brother)
    if (!t1->redefines && t1->times == 1)
      for (t2 = sy2->son; t2 != NULL; t2 = t2->brother)
	if (!t2->redefines && t2->times == 1)
	  if (strcmp (t1->name, t2->name) == 0)
	    {
	      if ((t1->type != 'G') && (t2->type != 'G'))
		gen_add (t1, t2, rnd);
	      else
		gen_addcorr (t1, t2, rnd);
	    }
}

void
gen_subtractcorr (struct sym *sy1, struct sym *sy2, int rnd)
{
  struct sym *t1, *t2;
  if (!(SYMBOL_P (sy1) && SYMBOL_P (sy2)))
    {
      yyerror ("sorry we don't handle this case yet!");
      return;
    }
#ifdef COB_DEBUG
  fprintf (o_src, "# ADD CORR %s --> %s\n", sy1->name, sy2->name);
#endif
  
  for (t1 = sy1->son; t1 != NULL; t1 = t1->brother)
    if (!t1->redefines && t1->times == 1)
      for (t2 = sy2->son; t2 != NULL; t2 = t2->brother)
	if (!t2->redefines && t2->times == 1)
	  if (strcmp (t1->name, t2->name) == 0)
	    {
	      if ((t1->type != 'G') && (t2->type != 'G'))
		gen_subtract (t1, t2, rnd);
	      else
		gen_subtractcorr (t1, t2, rnd);
	    }
}

void
gen_set (struct sym *idx, int which, struct sym *var,
	 int adrof_idx, int adrof_var)
{
  struct sym *sy = idx;
  if (REFMOD_P (idx))
    sy = ((struct refmod *) idx)->sym;
  else if (SUBREF_P (idx))
    sy = SUBREF_SYM (idx);

  if (sy->type == '8')
    {				/* conditional? */
      if ((sy->refmod_redef.vr != NULL) || (sy->value2 != sy->value))
	{
	  yyerror ("conditional is not unique");
	  return;
	}
      if (SUBREF_P (idx))
	{
	  struct subref *ref = make_subref (sy->parent, SUBREF_NEXT (idx));
	  gen_move ((struct sym *) sy->value, (struct sym *) ref);
	  free (ref);
	}
      else
	{
	  gen_move ((struct sym *) sy->value, sy->parent);
	}
      return;
    }
  if (sy->flags.is_pointer || adrof_idx)
    {				/* pointer? */
#ifdef COB_DEBUG
      fprintf (o_src, "# set %s to %s\n", idx ? idx->name : "(null)",
	       var ? var->name : "(null)");
      fprintf (o_src, "# adrof_idx: %d, adrof_var: %d\n",
	       adrof_idx, adrof_var);
#endif
      if (which != SET_TO)
	{
	  yyerror ("only SET TO work with pointers");
	  return;
	}
      if (adrof_idx && !(idx->linkage_flg))
	{
	  yyerror ("only linkage variables may be set to a new address");
	  return;
	}
      if (adrof_var)
	{
	  loadloc_to_eax (var);
	  set_ptr (idx);
	}
      else
	{
	  if (var == NULL)
	    {
	      fprintf (o_src, "\txorl\t%%eax,%%eax\n");
	    }
	  else
	    {
	      load_location (var, "ebx");
	      fprintf (o_src, "\tmovl\t0(%%ebx),%%eax\n");
	    }
	  set_ptr (idx);
	}
      return;
    }
	/******** it is not a pointer, so must be an index ********/
  if (idx->type != 'B')
    {
      yyerror ("only usage comp variables can be used as indices");
      return;
    }
  fprintf (o_src, "# SET %s \n", idx->name);
  /* first get the second operand */
  if (symlen (idx) > 4)
    yyerror ("warning: we don't allow this large index variable");
  value_to_eax (var);
  switch (which)
    {
    case SET_TO:		/* just move this value */
      fprintf (o_src, "\tmov%c\t%%eax, -%d(%%ebp)\n",
	       varsize_ch (idx), idx->location);
      break;
    case SET_UP_BY:		/* we need to add this value to the index */
      fprintf (o_src, "\tadd%c\t%%eax, -%d(%%ebp)\n",
	       varsize_ch (idx), idx->location);
      break;
    case SET_DOWN_BY:
      fprintf (o_src, "\tsub%c\t%%eax, -%d(%%ebp)\n",
	       varsize_ch (idx), idx->location);
      break;
    default:
      yyerror ("SET option unavailable");
    }
}

/******* short-circuit conditional evaluators ********/

void
push_boolean (int flag)
{
  push_immed (flag ? 0 : 1);
  asm_call ("cob_push_boolean");
}

void
push_condition ()
{
  push_eax ();
  asm_call ("cob_push_boolean");
}

void
push_field (struct sym *sy)
{
  asm_call_1 ("cob_push_field", sy);
}

int
gen_evaluate_start ()
{
  int i = loc_label++;
  fprintf (o_src, "# EVALUATE statement\n");
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

  fprintf (o_src,
	   "# WHEN check: level=%d, subject->type=%d, object type=%d\n",
	   level, ssbj->type, type);

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
  fprintf (o_src, "\tand\t%%eax,%%eax\n");
  if (type & 1)
    fprintf (o_src, "\tjnz\t.L%d\n", endcase);
  else
    fprintf (o_src, "\tjz\t.L%d\n", endcase);
}

void
gen_bypass_when_case (int bypass)
{
  if (bypass)
    fprintf (o_src, ".L%d:\n", bypass);
}

int
gen_end_when (int n, int endcase, int sentence)
{
  int lab;
  if (sentence)
    {
      fprintf (o_src, "\tjmp\t.L%d\t# end WHEN\n", n);
      lab = 0;
    }
  else
    {
      lab = loc_label++;
      fprintf (o_src, "\tjmp\t.L%d\t# bypass WHEN test\n", lab);
    }
  fprintf (o_src, ".L%d:\n", endcase);
  return lab;
}

void
gen_goto_depending (struct list *l, struct sym *sy)
{
  struct list *tmp;
  gen_loadloc (sy);
  fprintf (o_src, "\tmovl $c_base%d+%u, %%eax\n", pgm_segment, sy->descriptor);
  push_eax ();
  asm_call ("get_index");	/* this will return %eax with var's value */
  for (tmp = l; tmp != NULL; tmp = tmp->next)
    {
      fprintf (o_src, "\tdecl\t%%eax\n");
      fprintf (o_src, "\tjz\t.LB_%s\n", label_name ((struct sym *) tmp->var));
    }
  free_list (l);
}

void
gen_goto (struct list *l)
{
  struct sym *sy = (struct sym *) l->var;
  fprintf (o_src, "\tjmp\t.LB_%s\n", label_name (sy));
  if (l->next)
    {
      yyerror ("GOTO only allows one target");
    }
  free_list (l);
}

int
gen_check_zero ()
{
  int i = loc_label++;
  fprintf (o_src, "\tand\t%%eax,%%eax\n");
  fprintf (o_src, "\tjz\t.L%d\n", i);
  stabs_line ();
  return i;
}

int
gen_at_end (int status)
{
  int i, j;
  i = loc_label++;
  j = loc_label++;

  fprintf (o_src, "\tcmp\t$%d, %%eax\n", status);
  fprintf (o_src, "\tjz\t.L%d\n", j);
  fprintf (o_src, "\tjmp\t.L%d\n", i);

//      fprintf(o_src,"L%d:\n",j);
  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".L%d:\n", j);

  stabs_line ();
  return i;
}

int
gen_testif (void)
{
  int i, j;
  i = loc_label++;
  j = loc_label++;
  fprintf (o_src, "\tjz\t.L%d\n", j);
  fprintf (o_src, "\tjmp\t.L%d\n", i);
  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".L%d:\n", j);
  stabs_line ();
  return i;
}

void
gen_not (void)
{
  int i, j;
  i = loc_label++;
  j = loc_label++;

  fprintf (o_src, "\tjz\t.L%d\n", i);
  fprintf (o_src, "\txorl\t%%eax,%%eax\n");
  fprintf (o_src, "\tjmp\t.L%d\n", j);
  fprintf (o_src, ".L%d:\tincl\t%%eax\n", i);
  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".L%d:\n", j);

  stabs_line ();
}

int
gen_andstart (void)
{
  int i = loc_label++;
  fprintf (o_src, "\tjnz\t.L%d\n", i);
  return i;
}

int
gen_orstart (void)
{
  int i = loc_label++;
  fprintf (o_src, "\tjz\t.L%d\n", i);
  return i;
}

void
gen_dstlabel (int lbl)
{
  fprintf (o_src, ".L%d:\n", lbl);
  stabs_line ();
}

int
gen_passlabel (void)
{
  int i = loc_label++;
  fprintf (o_src, "\tjmp\t.L%d\n", i);
  return i;
}

int
gen_marklabel (void)
{
  int i = loc_label++;
  fprintf (o_src, ".L%d:\n", i);
  stabs_line ();
  return i;
}

void
gen_jmplabel (int lbl)
{
  fprintf (o_src, "\tjmp\t.L%d\n", lbl);
}

void
gen_push_int (struct sym *sy)
{
  gen_loadloc (sy);
  fprintf (o_src, "\tmovl $c_base%d+%u, %%eax\n", pgm_segment, sy->descriptor);
  push_eax ();
  asm_call ("get_index");
  /* this must be done without calling push_eax */
  fprintf (o_src, "\tpushl\t%%eax\n");
}

void gen_cancel (struct sym *sy)
{
  asm_call_1 ("cob_cancel", sy);
}

void
gen_perform_test_counter (int lbl)
{
  fprintf (o_src, "\tcmpl\t$0,0(%%esp)\n");
  fprintf (o_src, "\tjle\t.L%dE\n", lbl);
}

void
gen_perform_times (int lbl)
{
  fprintf (o_src, "\tdecl\t0(%%esp)\n");
  fprintf (o_src, "\tjnz\t.L%d\n", lbl);
  fprintf (o_src, ".L%dE:\tpopl\t%%ecx\n", lbl);
}

void
gen_perform_thru (struct sym *s1, struct sym *s2)
{
  if (s2 == NULL)
    s2 = s1;
  fprintf (o_src, "\tleal\t.L%d, %%eax\n", loc_label);
  fprintf (o_src, "\tpushl\t%%eax\n");
  fprintf (o_src, "\tleal\t.LB_%s, %%eax\n", label_name (s1));
  fprintf (o_src, "\tpushl\t%%eax\n");
  fprintf (o_src, "\tleal\t.LE_%s, %%eax\n", label_name (s2));
  fprintf (o_src, "\tpushl\t%%eax\n");
  fprintf (o_src, "\tjmp\t.LB_%s\n", label_name (s1));

  fprintf (o_src, "\t.align 16\n");
  fprintf (o_src, ".L%d:\n", loc_label++);
  //stabs_line(); 
}

void
gen_perform (struct sym *sy)
{
  gen_perform_thru (sy, sy);
}

int
save_pic_char (char c, int n)
{
  int c1 = toupper (c);
  switch (c1)
    {
    case 'A':
      piccnt += n;
      if (curr_field->type != 'X' && curr_field->type != 'E')
	curr_field->type = 'A';
      break;
    case 'N':
      piccnt += n * 2;
      if (curr_field->type == '9')
	curr_field->type = 'X';
      break;
    case 'X':
      piccnt += n;
      if (curr_field->type == '9')
	curr_field->type = 'X';
      break;
    case 'Z':
      curr_field->type = 'E';
    case '9':
      piccnt += n;
      if (v_flag)
	curr_field->decimals += n;
      n_flag = 1;
      break;
    case 'V':
      if (v_flag)
	{
	  yyerror ("too many `V's in picture");
	  return 0;
	}
      v_flag = 1;
      break;
    case 'P':
      if (!n_flag)
	v_flag = 1;		/* implicit V just before the first P */
      if (v_flag)
	curr_field->decimals += n;
      else
	curr_field->decimals -= n;
      break;
    case 'S':
      sign = 1;
      break;
    case '.':
    case ',':
    case '0':
    case 'B':
    case '/':
    case '+':
    case '-':
    case '*':
    case 'C':
    case 'R':
    case 'D':
      piccnt += n;
      curr_field->type = 'E';
      break;
    default:
      if (c == currency_symbol)
	{
	  piccnt += n;
	  curr_field->type = 'E';
	  break;
	}

      /* error */
      yyerror ("invalid char in picture: `%c'", c);
      return 0;
    }

  if (picture[picix] == 0 || picture[picix] != c1)
    {
      if (picture[picix] != 0)
	picix += 2;
      picture[picix] = c1;
      picture[picix + 1] = 0;
    }

  n += picture[picix + 1];
  while (n > 255)
    {
      picture[picix + 1] = 255;
      picture[picix + 2] = c1;
      picix += 2;
      n -= 255;
    }
  picture[picix + 1] = n;
  return 1;
}

/* increment loop index, check for end */
void
gen_SearchLoopCheck (unsigned long lbl5, struct sym *syidx, struct sym *sytbl)
{

  /*struct sym *sy1, *sy2; */
  struct lit *v;
  char tblmax[21];
  /*int len, i; */

  strcpy (tblmax, "1");
  v = install_literal (tblmax);
  save_literal (v, '9');

  gen_add ((struct sym *) v, syidx, 0);

  sprintf (tblmax, "%d", sytbl->times);
  v = install_literal (tblmax);
  save_literal (v, '9');

  gen_compare (syidx, GREATER, (struct sym *) v);
  fprintf (o_src, "\tjz\t.L%ld\n", lbl5);

  stabs_line ();
}

void
gen_SearchAllLoopCheck (unsigned long lbl3, struct sym *syidx,
			struct sym *sytbl, struct sym *syvar,
			unsigned long lstart, unsigned long lend)
{

  struct sym *sy1;
  struct subref *vr1;
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
      if (strcmp (it1->tablename, sytbl->name) == 0)
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

  vr1 = create_subscript (syidx);
  sy1 = (struct sym *) make_subref (sytbl, vr1);

  /* table sort sequence: '0' = none, '1' = ASCENDING, '2' = DESCENDING */

  /*    if ((bu - bl) > 1) */
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  fprintf (o_src, "\tsubl\t-%d(%%ebp), %%eax\n", stack_offset - 8);

  fprintf (o_src, "\tcmpl $1, %%eax\n");
  fprintf (o_src, "\tjle .L%ld\n", l1);

  fprintf (o_src, "\t.align 16\n");

//    if (itbl1 > in) { /* '2' = DESCENDING */
  if (it2->seq == '2')
    {
      gen_compare (sy1, GREATER, syvar);
      fprintf (o_src, "\tjnz\t.L%ld\n", l2);
    }
  else
    {
      gen_compare (sy1, LESS, syvar);
      fprintf (o_src, "\tjnz\t.L%ld\n", l2);
    }
  fprintf (o_src, "\t.align 16\n");

//    bl  = idx + 1;
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  fprintf (o_src, "\taddl $1, %%eax\n");
  fprintf (o_src, "\tmovl\t%%eax, -%d(%%ebp)\n", stack_offset - 8);

  gen_jmplabel (l3);
  fprintf (o_src, "\t.align 16\n");

//    else {
  gen_dstlabel (l2);

//    bu  = idx - 1;
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  fprintf (o_src, "\tsubl $1, %%eax\n");
  fprintf (o_src, "\tmovl\t%%eax, -%d(%%ebp)\n", stack_offset - 12);

  gen_dstlabel (l3);

//    idx = ((bu - bl)/2 + bl);
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  fprintf (o_src, "\tsubl\t-%d(%%ebp), %%eax\n", stack_offset - 8);
  fprintf (o_src, "\tmovl\t%%eax, %%edx\n");
  fprintf (o_src, "\tsarl\t$31, %%edx\n");
  fprintf (o_src, "\tmovl\t%%edx, %%ecx\n");
  fprintf (o_src, "\tsarl\t$31, %%ecx\n");
  fprintf (o_src, "\tleal\t(%%ecx,%%eax), %%edx\n");
  fprintf (o_src, "\tmovl\t%%edx, %%eax\n");
  fprintf (o_src, "\tsarl\t$1, %%eax\n");
  fprintf (o_src, "\taddl\t-%d(%%ebp), %%eax\n", stack_offset - 8);
  fprintf (o_src, "\tmovl\t%%eax, -%d(%%ebp)\n", syidx->location);

  gen_jmplabel (l6);
  fprintf (o_src, "\t.align 16\n");

//    else { /* l1 */
  gen_dstlabel (l1);

  if (it2->seq == '2')
    {
//       if (itbl1 > in) {
      gen_compare (sy1, GREATER, syvar);
      fprintf (o_src, "\tjnz\t.L%ld\n", l4);
    }
  else
    {
//       if (itbl1 < in) {
      gen_compare (sy1, LESS, syvar);
      fprintf (o_src, "\tjnz\t.L%ld\n", l4);
    }
  fprintf (o_src, "\t.align 16\n");


//    if (bu > idx) {
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", syidx->location);
  fprintf (o_src, "\tcmpl\t%%eax, -%d(%%ebp)\n", stack_offset - 12);

  fprintf (o_src, "\tjle\t.L%ld\n", l5);
  fprintf (o_src, "\t.align 16\n");


//    idx = bu;
  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 12);
  fprintf (o_src, "\tmovl %%eax, -%d(%%ebp)\n", syidx->location);

  gen_jmplabel (l6);
  fprintf (o_src, "\t.align 16\n");

//    else {
  gen_dstlabel (l5);


//    r++;
  fprintf (o_src, "\taddl\t$1, -%d(%%ebp)\n", stack_offset - 4);

  gen_jmplabel (l6);
  fprintf (o_src, "\t.align 16\n");

//       }
//    }
//    else {
  gen_dstlabel (l4);

//    r++;
  fprintf (o_src, "\taddl\t$1, -%d(%%ebp)\n", stack_offset - 4);

  gen_dstlabel (l6);

  fprintf (o_src, "\tmovl\t-%d(%%ebp), %%eax\n", stack_offset - 4);
  fprintf (o_src, "\tcmpl $1, %%eax\n");
  fprintf (o_src, "\tjz\t.L%ld\n", lbl3);


  gen_jmplabel (lstart);
  fprintf (o_src, "\t.align 16\n");
  gen_dstlabel (lend);

  stabs_line ();
}

void
define_implicit_field (struct sym *sy, struct sym *sykey, int idxlen)
{
  struct sym *tmp = NULL;
  struct index_to_table_list *i2t;

  sy->len = 4;
  sy->decimals = 0;	/* suppose no decimals yet */
  sy->level = 1;
  sy->type = 'B';	/* assume numeric "usage is comp" item */
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
  i2t->idxname = strdup (sy->name);
  i2t->tablename = strdup (curr_field->name);
  i2t->seq = '0';	/* no sort sequence is yet defined for the table */
  i2t->keyname = NULL;
  if (sykey != NULL)
    {
      if (sykey->level == -1)
	i2t->seq = '1';
      if (sykey->level == -2)
	i2t->seq = '2';
      i2t->keyname = strdup (sykey->name);
    }
  i2t->next = index2table;
  index2table = i2t;
}

void
Initialize_SearchAll_Boundaries (struct sym *sy, struct sym *syidx)
{
  int i;
  struct lit *v;
  char tblmax[21];
  struct index_to_table_list *i2t1, *i2t2;

  i = sy->times / 2;

  sprintf (tblmax, "%d", i);
  v = install_literal (tblmax);
#ifdef COB_DEBUG
  if (v->type)
    {				/* not already saved */
      fprintf (stderr,
	       "Initialize_SearchAll_Boundaries: literal is saved: %s\n",
	       tblmax);
    }
  else
    {
      fprintf (stderr,
	       "Initialize_SearchAll_Boundaries: literal not saved: %s\n",
	       tblmax);
    }
#endif

  save_literal (v, '9');
  gen_move ((struct sym *) v, syidx);

  fprintf (o_src, "\tmovl\t$0, %%eax\n");
  fprintf (o_src, "\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 4);

  fprintf (o_src, "\tmovl\t$1, %%eax\n");
  fprintf (o_src, "\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 8);

  fprintf (o_src, "\tmovl\t$%d, %%eax\n", sy->times);
  fprintf (o_src, "\tmovl\t%%eax,-%d(%%ebp)\n", stack_offset - 12);

  i2t2 = NULL;
  i2t1 = index2table;
  while (i2t1 != NULL)
    {
      if (strcmp (i2t1->tablename, sy->name) == 0)
	{
	  if (i2t1->seq != '0')
	    {
	      i2t2 = i2t1;
	    }
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

struct sym *
determine_table_index_name (struct sym *sy)
{
  struct sym *rsy = NULL;
  struct index_to_table_list *i2t;

  i2t = index2table;
  while (i2t != NULL)
    {
      if (strcmp (i2t->tablename, sy->name) == 0)
	{
	  rsy = lookup_symbol (i2t->idxname);
	  i2t = NULL;
	}
      else
	{
	  i2t = i2t->next;
	}
    }

#ifdef COB_DEBUG
  if (cob_trace_codegen)
    {
      if (rsy == NULL)
	{
	  fprintf (stderr,
		   "trace (determine_table_index_name): table name '%s' index name '(NULL)'\n",
		   sy->name);
	}
      else
	{
	  fprintf (stderr,
		   "trace (determine_table_index_name): table name '%s' index name '%s'\n",
		   sy->name, rsy->name);
	}
    }
#endif
  return rsy;
}

void
define_field (int level, struct sym *sy)
{
  struct sym *tmp;
  struct sym *tmp1 = NULL;

  if (level == 88)
    {
      sy->type = '8';
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
  sy->len = 0;
  sy->decimals = -1;		/* suppose no decimals yet */
  sy->level = level;
  sy->type = '9';		/* assume numeric (elementary) item */
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
  fprintf (o_src, ".L%d:\t# EVALUATE end\n", label);
}

int
check_fields (struct sym *sy)
{
  struct sym *tmp;
  int len;

  if (sy->son != NULL)
    {
      len = 0;
      for (tmp = sy->son; tmp != NULL; tmp = tmp->brother)
	{
	  check_fields (tmp);
	}
    }
  if (sy->type == '9' && sy->len > 31)
    yyerror ("Elementary numeric item %s > 31 digits", sy->name);
  return 0;
}

int
set_field_value_sw (struct sym *sy, int times)
{
  struct sym *tmp;
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
set_field_length (struct sym *sy, int times)
{
  struct sym *tmp;
  int len, tmplen;
  if (sy->son != NULL)
    {
      len = 0;
      sy->type = 'G';
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
field_alignment (struct sym *sy, unsigned location)
{
  unsigned slack_bytes = 0, mod_loc;

  if (sy->flags.sync == 0)
    return 0;
  switch (sy->type)
    {
    case 'B':
      mod_loc = (location - curr_01_location) % symlen (sy);
      slack_bytes = (mod_loc == 0 ? 0 : symlen (sy) - mod_loc);
      break;
    }
#ifdef COB_DEBUG
  //fprintf(o_src,"#fa: %d, %d, %d, %d, %d\n", curr_01_location, location, symlen(sy), mod_loc, slack_bytes);
#endif
  // slack_bytes = 0;
  return slack_bytes;
}

void
set_field_location (struct sym *sy, unsigned location)
{
  struct sym *tmp;

  //fprintf(stderr,"set_field_location: %s -> %d\n",sy->name,location);
  if (sy->level == 1)
    curr_01_location = location;
	/********* allocate field descriptor *************/
  sy->descriptor = literal_offset;
  literal_offset += (sy->type == 'G' ? 7 : 11);
	/********* generate picture for field ************/
  if (sy->type != 'G')
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
save_report (struct sym *rep, struct sym *file)
{
  struct rd *rd = (struct rd *) rep;
  struct list *item = malloc (sizeof (struct list));
  item->var = rd;
  item->next = report_list;
  report_list = item;
  rd->file = file;
  rd->type = 'W';
  rd->controls = rd->items = NULL;
  rd->page_limit = 66;
  rd->heading = 1;
  rd->footing = 66;
  rd->first_detail = rd->last_detail = 1;
}

void
update_report_field (struct sym *sy)
{
  update_field ();
  sy->type = 'Q';
}

void
update_screen_field (struct sym *sy, struct scr_info *si)
{
  struct sym *tmp;
  update_field ();
  sy->type = 'D';
  sy->scr = si;
  si->label = screen_label++;
  /* if picture is empty (implicit filler), and there is a
     value declared, create its picture from value literal. */
  if (*(sy->picstr) == 0 && sy->value != NULL)
    {
      tmp = (struct sym *) sy->value;
      sy->len = strlen (tmp->name);
      sy->picstr = malloc (3);
      sy->picstr[0] = 'X';
      sy->picstr[1] = sy->len;
      sy->picstr[2] = 0;
    }
}

void
update_field (void)
{
  if (curr_field->level != 88)
    if (curr_field->type != 'G')
      curr_field->picstr = strdup (picture);

  if ((curr_field->type != 'B') && (curr_field->type != 'U'))
    {
      curr_field->len = piccnt;
      if (curr_field->flags.separate_sign)
	curr_field->len++;
    }
  /* update COMP field length (but not BINARY-<something>) */
  if (curr_field->len == 0 && curr_field->type == 'B')
    {
      curr_field->len = query_comp_len (curr_field);
    }
}

void
close_fields (void)
{
  struct sym *sy;
  int saved_length;
  int ns_offset = 0;

  if (curr_field == NULL)
    return;

  /********** locate level 01 field   **************/
  for (sy = curr_field; sy->parent != NULL; sy = sy->parent);
  if (sy->level != 1 && sy->level != 77)
    {
      yyerror ("field not subordinate to any other: %s", sy->name);
    }
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
  struct sym *sy, *sy1, *sy2;
  int i, def;
  fprintf (o_src, "# resolving paragraphs/sections labels\n");
  for (i = 0; i < HASHLEN; i++)
    {
      for (sy = labtab[i]; sy != NULL; sy = sy->next)
	{
	  if (sy->type == 'f')
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
			  !strcmp (sy1->parent->name, sy2->parent->name))
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
			      fprintf (o_src, ".LB_%s = ", label_name (sy1));
			      fprintf (o_src, ".LB_%s\n", label_name (sy2));
			      fprintf (o_src, ".LE_%s = ", label_name (sy1));
			      fprintf (o_src, ".LE_%s\n", label_name (sy2));
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
open_section (struct sym *sect)
{
  sect->type = 'S';
  fprintf (o_src, ".LB_%s:\n", label_name (sect));
  curr_section = sect;
}

void
close_section (void)
{
  close_paragr ();
  if (curr_section)
    {
      fprintf (o_src, ".LE_%s:\n", label_name (curr_section));
      gen_exit (0);
    }
}

char *
label_name (struct sym *lab)
{
  if (lab->parent)
    sprintf (name_buf, "%s__%s_%d", lab->name, lab->parent->name, pgm_segment);
  else
    sprintf (name_buf, "%s_%d", lab->name, pgm_segment);
  return chg_underline (name_buf);
}

char *
var_name (struct sym *sy)
{
  int n;
  n = MAXNAMEBUF;
  strcpy (name_buf, "");
  while (n > strlen (sy->name) + 4)
    {
      if (n < MAXNAMEBUF)
	strcat (name_buf, " OF ");
      strcat (name_buf, sy->name);
      n -= strlen (sy->name) + 4;
      if ((lookup_symbol (sy->name)->clone == NULL) || (sy->parent == NULL))
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
      fprintf (o_src, ".LE_%s:\n", label_name (curr_paragr));
      gen_exit (0);
      curr_paragr = NULL;
    }
}

void
open_paragr (struct sym *paragr)
{
  paragr->type = 'P';
  curr_paragr = paragr;
  fprintf (o_src, ".LB_%s:\n", label_name (paragr));
}

void
gen_stoprun (void)
{
  fprintf (o_src, "\tleal\t.Lend_pgm_%s, %%eax\n", pgm_label);
  fprintf (o_src, "\tpushl\t%%eax\n");
  fprintf (o_src, "\tjmp\t.Lend_pgm_%s\n", pgm_label);
}

void
gen_exit (int code)
{
  int l1, l2;
  if (code)
    {
      fprintf (o_src, "\tmovl\t-%d(%%ebp), %%ebx\n", stack_offset - 8 - 16);
      fprintf (o_src, "\tmov\t%%ebp,%%esp\n");
      fprintf (o_src, "\tpop\t%%ebp\n");
      fprintf (o_src, "\tret\n");
    }
  else
    {
      l1 = loc_label++;
      l2 = loc_label++;
      if (curr_paragr != NULL)
	{
	  fprintf (o_src, "\tleal\t.LE_%s, %%eax\n",
		   label_name (curr_paragr));
	}
      else
	{
	  fprintf (o_src, "\tleal\t.LE_%s, %%eax\n",
		   label_name (curr_section));
	}
      fprintf (o_src, "\tcmpl\t4(%%esp), %%eax\n");
      fprintf (o_src, "\tjb\t\t.L%d\n", l1);
      fprintf (o_src, "\tcmpl\t0(%%esp), %%eax\n");
      fprintf (o_src, "\tjb\t\t.L%d\n", l2);
      fprintf (o_src, ".L%d:\n", l1);
      fprintf (o_src, "\taddl\t$8,%%esp\n");
      fprintf (o_src, "\tret\n");
      fprintf (o_src, ".L%d:\n", l2);
    }
}

/* save variable values, including 88-var range/values list */
void
set_variable_values (struct lit *v1, struct lit *v2)
{
  struct vrange *new;
  if (curr_field->value == NULL)
    {
      curr_field->refmod_redef.vr = NULL;
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
      new->next = curr_field->refmod_redef.vr;
      curr_field->refmod_redef.vr = new;
    }
}

void
gen_condition (struct sym *sy)
{
  struct vrange *vr;
  struct sym *sy1 = sy;
  if (SUBREF_P (sy))
    sy1 = SUBREF_SYM (sy);
  gen_loadvar ((struct sym *) NULL);
  gen_loadvar ((struct sym *) sy1->value2);
  gen_loadvar ((struct sym *) sy1->value);
  vr = sy1->refmod_redef.vr;
  while (vr)
    {
      gen_loadvar ((struct sym *) vr->value2);
      gen_loadvar ((struct sym *) vr->value);
      vr = vr->next;
    }
  if (SUBREF_P (sy))
    {
      /* alloc a tmp node for condition parent 
         so gen_loadvar will be happy */
      struct subref *ref = make_subref (sy1->parent, SUBREF_NEXT (sy));
      gen_loadvar ((struct sym *) ref);
      free (ref);
    }
  else
    {
      gen_loadvar (sy->parent);
    }
  asm_call ("check_condition");
  fprintf (o_src, "\tand\t%%eax,%%eax\n");
}

/* compare for already stacked expressions */
void
gen_compare_exp (int value)
{
  asm_call ("cob_cmp");
  switch (value)
    {
    case 0:
      fprintf (o_src, "\txor\t%%eax,%%eax\n\tinc\t%%eax\n");	/* false */
      break;
    case 1:
      fprintf (o_src, "\tand\t%%eax,%%eax\n");	/* equal */
      break;
    case 2:
      fprintf (o_src, "\tinc\t%%eax\n");	/* less */
      break;
    case 3:
      fprintf (o_src, "\tdec\t%%eax\n");	/* less or equal */
      gen_not ();
      break;
    case 4:
      fprintf (o_src, "\tdec\t%%eax\n");	/* greater */
      break;
    case 5:
      fprintf (o_src, "\tinc\t%%eax\n");	/* greater or equal */
      gen_not ();
      break;
    case 6:
      fprintf (o_src, "\tand\t%%eax,%%eax\n");	/* not equal */
      gen_not ();
      break;
    case 7:
      fprintf (o_src, "\txor\t%%eax,%%eax\n");	/* true */
      break;
    }
}

void
gen_compare (struct sym *s1, int value, struct sym *s2)
{
  /* if any of sy1 or sy2 is an expression, we must 
     compare full expressions */
  if (EXPR_P (s1) || EXPR_P (s2))
    {
      push_expr (s2);
      push_expr (s1);
      gen_compare_exp (value);
    }
  else
    {
      asm_call_2 ("compare", s1, s2);
      switch (value)
	{
	case 0:
	  fprintf (o_src, "\txor\t%%eax,%%eax\n");	/* false */
	  fprintf (o_src, "\tinc\t%%eax\n");
	  break;
	case 1:
	  fprintf (o_src, "\tand\t%%eax,%%eax\n");	/* equal */
	  break;
	case 2:
	  fprintf (o_src, "\tinc\t%%eax\n");	/* less */
	  break;
	case 3:
	  fprintf (o_src, "\tdec\t%%eax\n");	/* less or equal */
	  gen_not ();
	  break;
	case 4:
	  fprintf (o_src, "\tdec\t%%eax\n");	/* greater */
	  break;
	case 5:
	  fprintf (o_src, "\tinc\t%%eax\n");	/* greater or equal */
	  gen_not ();
	  break;
	case 6:
	  fprintf (o_src, "\tand\t%%eax,%%eax\n");	/* not equal */
	  gen_not ();
	  break;
	case 7:
	  fprintf (o_src, "\txor\t%%eax,%%eax\n");	/* true */
	  break;
	}
    }
}

void
assign_expr (struct sym *sy, int rnd)
{
  push_immed (rnd);
  gen_loadvar (sy);
  asm_call ("cob_set");
}

int
push_expr (struct sym *sy)
{
  if (EXPR_P (sy))
    {
      push_expr ((struct sym *) EXPR_LEFT (sy));
      push_expr ((struct sym *) EXPR_RIGHT (sy));
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

  if (!is_numeric_sy (sy))
    return 0;

#ifdef COB_DEBUG
  fprintf (o_src, "# push_expr: %s\n", sy->name);
#endif
  asm_call_1 ("cob_push_decimal", sy);
  return 1;
}

static void
gen_save_filevar (struct sym *f, struct sym *buf)
{
  if (buf != NULL)
    {
      gen_loadloc (buf);
    }
  else
    {
#ifdef COB_DEBUG
      fprintf (o_src, "# File '%s' Record Description Stack Location\n",
	       f->name);
#endif

      fprintf (o_src, "\tmovl\t%s, %%eax\n", memref (f->recordsym));
      push_eax ();
    }
  if (f->type == 'K')
    fprintf (o_src, "\tmovl\t$_%s, %%eax\n", f->name);
  else
#ifdef COB_DEBUG
    fprintf (o_src, "# File name '%s', Record name '%s'\n", f->name,
	     f->recordsym->name);
#endif
  fprintf (o_src, "\tmovl\t$s_base%d+%u, %%eax\n", pgm_segment, f->location);
  push_eax ();
}

void
gen_save_filedesc (struct sym *f)
{
  if (f->type == 'K')
    fprintf (o_src, "\tmovl\t$_%s, %%eax\n", f->name);
  else
#ifdef COB_DEBUG
    fprintf (o_src, "# File name '%s', Record name '%s'\n", f->name,
	     f->recordsym->name);
#endif
  fprintf (o_src, "\tmovl\t$s_base%d+%u, %%eax\n", pgm_segment, f->location);
  push_eax ();
}

static void
gen_save_sort_fields (struct sym *f, struct sym *buf)
{
  struct sym *datafld;
  if (f == NULL)
    return;
  datafld = (struct sym *) f->sort_data;
  while (datafld != NULL)
    {
      gen_loadloc (datafld);
      datafld = (struct sym *) (datafld->sort_data);
    }
  fprintf (o_src, "\tmovl\t$c_base%d+%u, %%eax\n", pgm_segment,
	   f->descriptor);
  push_eax ();
  gen_save_filevar (f, buf);
  /* returns number of stack levels used in storing fields */
}

void
alloc_file_entry (struct sym *f)
{
  f->record = stack_offset;
#ifdef COB_DEBUG
//      fprintf(o_src,"# Alloc space for file ENTRY, Stack Addr: %d\n",
//                      stack_offset);
  fprintf (o_src, "# Allocate space for file '%s' Stack Addr: %d\n",
	   f->name, stack_offset);
#endif
}

void
dump_alternate_keys (struct sym *r, struct alternate_list *alt)
{
  struct alternate_list *tmp;
  struct sym *key;
  while (alt)
    {
      key = alt->key;
      fprintf (o_src, "# alternate key %s\n", key->name);
      fprintf (o_src,
	       "\t.word\t%d\n\t.long\tc_base%d+%d\n\t.word\t%d\n\t.long\t0\n",
	       key->location - r->location, pgm_segment,
	       key->descriptor, alt->duplicates);
      tmp = alt;
      alt = alt->next;
      free (tmp);
    }
  fprintf (o_src, "# end of alternate keys\n.word\t-1\n");
}

/* 
** dump all file descriptors in file_list
*/
void
dump_fdesc ()
{

  struct sym *f;
  struct sym *r;
  struct list *list /*,*visited */ ;
  unsigned char fflags;

  fprintf (o_src, "s_base%d:\t.long\t0\n", pgm_segment);
  for (list = files_list; list != NULL; list = list->next)
    {
      f = (struct sym *) list->var;
      r = f->recordsym;
#ifdef COB_DEBUG
      fprintf (o_src,
	       "# FILE DESCRIPTOR, File: %s, Record: %s, Data Loc: %d(hex: %x), opt: %x\n",
	       f->name, r->name, f->location, f->location, f->flags.optional);
#endif
      if (f->filenamevar == NULL)
	{
	  yyerror ("No file name assigned to %s.\n", f->name);
	  continue;
	}
      if (f->type == 'K')
	{
	  fprintf (o_src, "\t.extern\t_%s:far\n", f->name);
	  continue;
	}
      if (f->type == 'J')
	{
	  fprintf (o_src, "\tpublic\t_%s\n", f->name);
	  fprintf (o_src, "_%s\tlabel\tbyte\n", f->name);
	}
      fflags = f->flags.optional;
      fprintf (o_src, "\t.byte\t%u\n", RTL_FILE_VERSION);
      fprintf (o_src, "\t.long\tc_base%d+%u\n",
	       pgm_segment, f->filenamevar->descriptor);
      fprintf (o_src, "\t.long\t%d\n", r->len);
      fprintf (o_src, "\t.byte\t%d,%d\n", f->organization, f->access_mode);
      fprintf (o_src, "\t.long\t0\n");	/* open_mode */
      fprintf (o_src, "\t.long\t0\n");	/* struct DBT (libdb) */
      fprintf (o_src, "\t.long\t0\n");	/* start_record */
      fprintf (o_src, "\t.byte\t%x\n", fflags);	/* flags */
      if (f->organization == 1)
	{			/* indexed file */
	  if (f->ix_desc)
	    {
	      fprintf (o_src, "\t.word\t%d\n\t.long\tc_base%d+%d\n",
		       f->ix_desc->location - r->location,
		       pgm_segment, f->ix_desc->descriptor);
	    }
	  else
	    {
	      /* no key field was given for this file */
	      fprintf (o_src, "\t.word\t0\n\t.long\t0\n");
	    }
	  fprintf (o_src, "\t.long\t0\n");	/* struct altkey_desc *key_in_use */
	  dump_alternate_keys (r, (struct alternate_list *) f->alternate);
	}
    }
}

/*
** define a file, but don't generate code yet.
** (will be done later at dump_fdesc())
*/
void
gen_fdesc (struct sym *f, struct sym *r)
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
//      f->location = file_offset;
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
//      file_offset += len;
}

void
gen_status (struct sym *f)
{
  if (f->parent)
    {
      push_eax ();
      gen_loadloc (f->parent);
      asm_call ("cob_save_status");
    }
}

/****** sort statement related functions *******/
struct sortfile_node *
alloc_sortfile_node (struct sym *sy)
{
  struct sortfile_node *sn;
  if (sy->type != 'F')
    {
      yyerror ("only files can be found here");
      return NULL;
    }
  sn = malloc (sizeof (struct sortfile_node));
  sn->next = NULL;
  sn->sy = sy;
  return sn;
}

struct sym *
create_status_register (char *name)
{
  struct sym *sy;
  char pic[] = { '9', 2, 0 };
  sy = install (name, SYTB_VAR, 0);
  if (sy->type)
    return NULL;		/* it already exists */
  sy->type = '9';
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
gen_sort_using (struct sym *f, struct sortfile_node *sn)
{
  struct sym *vstatus = create_status_register ("SORT-RETURN");
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
gen_sort_giving (struct sym *f, struct sortfile_node *sn)
{
  struct sym *vstatus = create_status_register ("SORT-RETURN");
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
gen_sort (struct sym *f)
{
  gen_loadloc (f->filenamevar);
  gen_save_filevar (f, NULL);
  asm_call ("sort_open");
  gen_status (f);
  gen_close_sort (f);
}

void
gen_open (int mode, struct sym *f)
{
  push_immed (mode);
  gen_loadloc (f->filenamevar);
  gen_save_filevar (f, NULL);
  asm_call ("cob_open");
  gen_status (f);
}

void
gen_close_sort (struct sym *f)
{
  struct sym *sortf;
	/********** allocate memory for SORT descriptor ***********/
  save_field_in_list (f);
  f->descriptor = literal_offset;
  sortf = (struct sym *) (f->sort_data);
  while (sortf != NULL)
    {
      literal_offset += 2;
      sortf = (struct sym *) (sortf->sort_data);
    }
  literal_offset++;
}

void
gen_close (struct sym *f)
{
  gen_save_filevar (f, NULL);
  asm_call ("cob_close");
  gen_status (f);
}

void
gen_return (struct sym *f, struct sym *buf)
{
  gen_save_filevar (f, buf);
  asm_call ("sort_return");
  gen_status (f);
}

int
gen_reads (struct sym *f, struct sym *buf, struct sym *key, int next_prev,
	   int sel)
{
// NOTE: 
// While this is functional, it requires to be updated to trap more syntax errors

  if (f->type != 'F')
    {
      yyerror ("invalid variable \'%s\', file name expected", f->name);
      return 1;
    }

  if ((sel > -1) && (sel < 4))
    {
      if (next_prev > 0
	  && (f->organization == ORG_INDEXED
	      || f->organization == ORG_RELATIVE)
	  && (f->access_mode == ACC_DYNAMIC
	      || f->access_mode == ACC_SEQUENTIAL))
	gen_read_next (f, buf, next_prev);
      else
	gen_read (f, buf, key);
    }
  else
    {
      if (f->organization != ORG_SEQUENTIAL)
	gen_read_next (f, buf, next_prev);
      else
	gen_return (f, buf);
    }
  return 0;
}

void
gen_read (struct sym *f, struct sym *buf, struct sym *key)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  if (f->organization == ORG_INDEXED)
    {
      gen_loadvar (key);
    }
  /* pass the desc/address of reclen, if VARYING ... */
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    gen_loadvar (NULL);
  gen_save_filevar (f, buf);
  asm_call ("cob_read");
  gen_status (f);
}

void
gen_read_next (struct sym *f, struct sym *buf, int next_prev)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    gen_loadvar (NULL);
  gen_save_filevar (f, buf);
  if (next_prev == 1)
    asm_call ("cob_read_next");
  else
    asm_call ("cob_read_prev");
  gen_status (f);
}

void
gen_release (struct sym *r, struct sym *buf)
{
  struct sym *f;
  f = r->ix_desc;
  if (buf != NULL)
    gen_move (buf, r);
  gen_save_sort_fields (f, buf);
  asm_call ("sort_release");
  gen_status (f);
}

void
gen_write (struct sym *r, int opt, struct sym *buf)
{
  struct sym *f = r->ix_desc;
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  gen_check_varying (f);
  if (opt)
    {
      if (rv != NULL)
	gen_loadvar (rv->reclen);
      else
	gen_loadvar (NULL);
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
	gen_loadvar (NULL);
      gen_save_filevar (f, buf);
      asm_call ("cob_write");
    }
  gen_status (f);
}

void
gen_rewrite (struct sym *r, struct sym *buf)
{
  struct sym *f = r->ix_desc;
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  if (rv != NULL)
    gen_loadvar (rv->reclen);
  else
    gen_loadvar (NULL);
  gen_save_filevar (f, buf);
  asm_call ("cob_rewrite");
  gen_status (f);
}

void
gen_start (struct sym *f, int cond, struct sym *key)
{
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  else
    gen_loadvar (key);
  push_immed (cond);
  gen_save_filevar (f, NULL);
  asm_call ("cob_start");
  gen_status (f);
}

void
gen_delete (struct sym *f)
{
  gen_check_varying (f);
  if (f->organization == ORG_RELATIVE)
    push_index (f->ix_desc);
  gen_save_filevar (f, NULL);
  asm_call ("cob_delete");
  gen_status (f);
}

void
set_rec_varying_info (struct sym *f, struct lit *lmin,
		      struct lit *lmax, struct sym *reclen)
{
  struct rec_varying *rv = malloc (sizeof (struct rec_varying));
  f->rec_varying = (char *) rv;
  rv->lmin = lmin;
  rv->lmax = lmax;
  rv->reclen = reclen;
}

void
gen_check_varying (struct sym *f)
{
  struct rec_varying *rv = (struct rec_varying *) f->rec_varying;
  if (rv != NULL)
    {
      gen_loadvar (rv->reclen);
      gen_loadvar ((struct sym *) rv->lmax);
      gen_loadvar ((struct sym *) rv->lmin);
      gen_save_filedesc (f);
      asm_call ("cob_check_varying");
    }
}

void
gen_push_using (struct sym *sy)
{
  struct parm_list *list;
  list = (struct parm_list *) malloc (sizeof (struct parm_list));
  list->var = (void *) sy;
  list->next = parameter_list;
  list->location = 0;
  list->sec_no = 0;
  parameter_list = list;
}

void
gen_save_using (struct sym *sy)
{
  sy->linkage_flg = using_offset;
  using_offset += 4;
}


unsigned long int
gen_call (struct lit *v, int stack_size, int exceplabel, int notexceplabel)
{
  struct parm_list *list, *tmp;
  struct sym *cp;
  struct lit *lp;
  int len, totlen = 0;
  int saved_stack_offset = stack_offset;
  int stack_save;
  int endlabel;

	/******** prepare all parameters which are passed by content ********/
  for (list = parameter_list; list != NULL; list = list->next)
    {
      cp = (struct sym *) list->var;
      if (!LITERAL_P (cp))
	{
	  if (cp->call_mode == CM_CONT)
	    {
	      len = symlen (cp);	// should we round to 4?
	      totlen += len;
	      list->sec_no = SEC_STACK;
	      list->location = stack_offset + len;
	      stack_offset += len;
	      fprintf (o_src, "\tsubl\t$%d, %%esp\n", len);
	      push_immed (len);	// length
	      gen_loadloc (cp);	// src address
	      fprintf (o_src, "\tleal\t-%d(%%ebp), %%eax\n", list->location);
	      push_eax ();	// dest address ie on stack
	      asm_call ("memcpy");
	    }
	}
    }
	/******** get the parameters from the parameter list ********/
  for (list = parameter_list; list != NULL;)
    {
      cp = (struct sym *) list->var;
      if (LITERAL_P (cp))
	{
	  lp = (struct lit *) cp;
#ifdef COB_DEBUG
	  fprintf (o_src, "#call %s by %d\n", lp->name, lp->call_mode);
#endif
	  if (lp->call_mode == CM_REF)
	    gen_loadloc ((struct sym *) list->var);
	  else if (lp->call_mode == CM_VAL)
	    {
	      value_to_eax (cp);
	      if (symlen (cp) > 4)
		push_edx ();
	      push_eax ();
	    }
	  else
	    abort ();
	}
      else
	{
#ifdef COB_DEBUG
	  fprintf (o_src, "### CALL %s BY %d\n", cp->name, cp->call_mode);
#endif
	  switch (cp->call_mode)
	    {
	    case CM_REF:
	      gen_loadloc ((struct sym *) list->var);
	      break;
	    case CM_VAL:
	      gen_pushval ((struct sym *) list->var);
	      break;
	    case CM_CONT:
	      fprintf (o_src, "\tleal\t-%d(%%ebp), %%eax\n", list->location);
	      push_eax ();
	      break;
	    }
	}
      tmp = list;
      list = list->next;
      free (tmp);
    }
  parameter_list = NULL;
  if (cob_link_style == LINK_STATIC && LITERAL_P (v))
    {
      /* static call */
      asm_call (v->name);
      endlabel = 0;
    }
  else
    {
      /* dynamic call */
      stack_save = stackframe_cnt;
      stackframe_cnt = 0;
      asm_call_1 ("cob_dyncall_resolve", (struct sym *) v);
      stackframe_cnt = stack_save;
      fprintf (o_src, "\tand\t%%eax,%%eax\n");
      fprintf (o_src, "\tjz\t.L%d\n", exceplabel);
      fprintf (o_src, "\tcall\t*%%eax\n");
      cleanup_rt_stack ();
      endlabel = loc_label++;
      fprintf (o_src, "\tjmp\t.L%d\n", notexceplabel);
    }
  if (totlen != 0)
    fprintf (o_src, "\taddl\t$%d, %%esp\n", totlen);
  stack_offset = saved_stack_offset;
  return endlabel;
}

int
begin_on_except ()
{
  int lab = loc_label++;
  fprintf (o_src, ".L%d:\t# begin_on_except\n", lab);
  stabs_line ();
  return lab;
}

void
check_call_except (int excep, int notexcep, int exceplabel,
		   int notexceplabel, int endlabel)
{
  /* generate code only if was "call <identifier>" */
  if (endlabel != 0)
    {
      fprintf (o_src, ".L%d:\t# exceplabel\n", exceplabel);
      if (excep)
	fprintf (o_src, "\tjmp\t.L%d\n", excep);
      /* if no exception phrase was given */
      if (excep == 0)
	{
	  fprintf (o_src, "\tcall\tcob_dyncall_error\n");
	  fprintf (o_src, "\tjmp\t.L%d\n", endlabel);
	}
      fprintf (o_src, ".L%d:\t# notexceplabel\n", notexceplabel);
      if (notexcep)
	fprintf (o_src, "\tjmp\t.L%d\n", notexcep);
      fprintf (o_src, ".L%d:\t# endlabel\n", endlabel);
    }
}


void
mark_actives (int first, int last)
{
  int i;
  if (last < first)
    last = first;
  if (first < 0 || first > 36)
    first = 0;
  if (last < 0 || last > 36)
    last = 0;
  for (i = first; i <= last; i++)
    active[i] = 1;
}

int
sort_exref_compare (const void *z1, const void *z2)
{
  char *str1, ss1[256], ss2[256];
  struct sym **zz1, **zz2;
  int r;

  zz1 = (struct sym **) z1;
  zz2 = (struct sym **) z2;
  str1 = var_name (*zz1);
  strcpy (ss1, str1);
  str1 = var_name (*zz2);
  strcpy (ss2, str1);
  r = strcmp (ss1, ss2);
  return r;
}
