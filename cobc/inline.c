/*
 * Copyright (C) 2002 Keisuke Nishida
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


/*
 * GO TO
 */

static void
output_goto (cobc_tree label)
{
  const char *name = COBC_LABEL_NAME (label)->cname;
  output_line ("goto lb_%s;", name);
}

static void
output_goto_depending (cobc_tree labels, cobc_tree index)
{
  int i = 1;
  struct cobc_list *l = (struct cobc_list *) labels;
  output_prefix ();
  output ("switch (");
  output_index (index);
  output (")\n");
  output_indent ("  {", 4);
  for (; l; l = l->next)
    {
      output_indent_level -= 2;
      output_line ("case %d:", i++);
      output_indent_level += 2;
      output_goto (l->item);
    }
  output_indent ("  }", -4);
}


/*
 * MOVE
 */

static void
output_advance_move (struct cob_field f, cobc_tree dst)
{
  struct cobc_field *p = COBC_FIELD (dst);
  struct cob_field_desc dst_desc;
  unsigned char dst_data[p->size];
  struct cob_field dst_fld = {&dst_desc, dst_data};

  dst_desc.size = p->size;
  if (p->children)
    {
      dst_desc.type = 'G';
    }
  else
    {
      dst_desc.type = get_type (p);
      dst_desc.digits = p->pic->digits;
      dst_desc.decimals = p->pic->decimals;
      dst_desc.have_sign = p->pic->have_sign;
      dst_desc.separate_sign = p->f.sign_separate;
      dst_desc.leading_sign = p->f.sign_leading;
      dst_desc.blank_zero = p->f.blank_zero;
      dst_desc.just_r = p->f.justified;
      dst_desc.pic = p->pic->str;
    }

  cob_move (f, dst_fld);
  output_memcpy (dst, dst_data, p->size);
}

static void
output_move_num (cobc_tree x, int high)
{
  switch (COBC_FIELD (x)->usage)
    {
    case USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0', COBC_FIELD (x)->size);
      break;
    case USAGE_BINARY:
    case USAGE_INDEX:
      output_native_assign (x, high ? -1 : 0);
      break;
    case USAGE_PACKED:
      printf ("%s: not implemented\n", __FUNCTION__);
      abort ();
    }
}

static void
output_move_all (cobc_tree x, char c)
{
  struct cobc_field *p = COBC_FIELD (x);
  struct cob_field_desc desc = {p->size, 'X'};
  unsigned char data[p->size];
  struct cob_field fld = {&desc, data};
  memset (data, c, p->size);
  output_advance_move (fld, x);
}

static void
output_move_zero (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC:
      output_move_num (x, 0);
      break;
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, '0', COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, '0');
      break;
    }
}

static void
output_move_space (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC:
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, ' ', COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, ' ');
      break;
    }
}

static void
output_move_high (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC:
      output_move_num (x, 1);
      break;
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, 255, COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, '\xff');
      break;
    }
}

static void
output_move_low (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC:
      output_move_num (x, 0);
      break;
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, 0, COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, '\0');
      break;
    }
}

static void
output_move_quote (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC:
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, '\"', COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, '\"');
      break;
    }
}

static void
output_move_all_literal (cobc_tree src, cobc_tree dst)
{
  int i;
  struct cobc_literal *src_p = COBC_LITERAL (src);
  unsigned char *src_data = src_p->str;
  int src_len = strlen (src_data);
  struct cobc_field *dst_p = COBC_FIELD (dst);
  unsigned char dst_data[dst_p->size];
  for (i = 0; i < dst_p->size; i++)
    dst_data[i] = src_data[i % src_len];
  output_memcpy (dst, dst_data, dst_p->size);
}

static void
output_move_literal (cobc_tree src, cobc_tree dst)
{
  if (COBC_FIELD (dst)->usage == USAGE_BINARY
      || COBC_FIELD (dst)->usage == USAGE_INDEX)
    {
      struct cobc_literal *p = COBC_LITERAL (src);
      long long val = literal_to_int (p);
      int decs = COBC_FIELD (dst)->pic->decimals;
      if (decs > p->decimals)
	val *= cob_exp10[decs - p->decimals];
      else if (decs < p->decimals)
	val /= cob_exp10[p->decimals - decs];
      output_native_assign (dst, val);
    }
  else if (!COBC_REFMOD_P (dst))
    {
      char src_pic[5];
      struct cobc_literal *l = COBC_LITERAL (src);
      struct cob_field_desc src_desc =
	{l->size, COBC_TREE_CLASS (l), l->size, l->decimals};
      unsigned char src_data[l->size + 1];
      struct cob_field src_fld = {&src_desc, src_data};
      strcpy (src_data, l->str);
      src_desc.pic = src_pic;
      src_pic[0] = COBC_TREE_CLASS (l);
      src_pic[1] = l->size;
      src_pic[2] = 0;
      if (l->sign)
	{
	  src_desc.have_sign = 1;
	  put_sign (src_fld, (l->sign < 0) ? 1 : 0);
	}
      output_advance_move (src_fld, dst);
    }
  else
    {
      output_call_2 ("cob_move", src, dst);
    }
}

static void
output_move (cobc_tree src, cobc_tree dst)
{
  if (src == cobc_zero)
    output_move_zero (dst);
  else if (src == cobc_space)
    output_move_space (dst);
  else if (src == cobc_high)
    output_move_high (dst);
  else if (src == cobc_low)
    output_move_low (dst);
  else if (src == cobc_quote)
    output_move_quote (dst);
  else if (COBC_LITERAL_P (src))
    {
      if (COBC_LITERAL (src)->all)
	output_move_all_literal (src, dst);
      else
	output_move_literal (src, dst);
    }
  else
    {
      struct cobc_field *srcp = COBC_FIELD (src);
      struct cobc_field *dstp = COBC_FIELD (dst);
      if (dstp->usage == USAGE_INDEX)
	{
	  output_prefix ();
	  output_index (dst);
	  output (" = ");
	  output_index (src);
	  output (";\n");
	}
      else if (srcp->usage == USAGE_INDEX)
	{
	  output_call_2 ("cob_set_int", dst, make_index (src));
	}
      else
	{
	  output_call_2 ("cob_move", src, dst);
	}
    }
}


/*
 * INITIALIZE
 */

static void output_initialize_internal (struct cobc_field *p);

static int
field_uniform_class (struct cobc_field *p)
{
  if (!p->children)
    {
      switch (p->category)
	{
	case COB_NUMERIC:
	  switch (p->usage)
	    {
	    case USAGE_DISPLAY:
	      return COB_NUMERIC;
	    case USAGE_BINARY:
	    case USAGE_INDEX:
	      return COB_BINARY;
	    default:
	      return COB_VOID;
	    }
	case COB_ALPHABETIC:
	case COB_ALPHANUMERIC:
	  return COB_ALPHANUMERIC;
	default:
	  return COB_VOID;
	}
    }
  else
    {
      int class = field_uniform_class (p->children);
      for (p = p->children->sister; p; p = p->sister)
	if (!p->redefines)
	  if (class != field_uniform_class (p))
	    return COB_VOID;
      return class;
    }
}

static void
output_initialize_uniform (cobc_tree x, int class, int size)
{
  int is_table = COBC_FIELD_P (x) && COBC_FIELD (x)->f.have_occurs;
  if (is_table)
    {
      output_indent ("{", 2);
      output_line ("int i%d = 1;", COBC_FIELD (x)->indexes);
    }
  switch (class)
    {
    case COB_BINARY:
      output_memset (x, 0, size);
      break;
    case COB_NUMERIC:
      output_memset (x, '0', size);
      break;
    case COB_ALPHANUMERIC:
      output_memset (x, ' ', size);
      break;
    }
  if (is_table)
    {
      output_indent ("}", -2);
    }
}

static void
output_initialize_compound (cobc_tree x)
{
  switch (COBC_FIELD (x)->category)
    {
    case COB_NUMERIC_EDITED:
      output_move_zero (x);
      break;
    case COB_ALPHANUMERIC_EDITED:
    case COB_NATIONAL_EDITED:
      output_move_space (x);
      break;
    default:
      output_recursive (output_initialize_internal, x);
      break;
    }
}

static void
output_initialize_internal (struct cobc_field *p)
{
  int last_class = COB_VOID;
  struct cobc_field *c;
  struct cobc_field *first_field = NULL;

  /* initialize all children, combining uniform sequence into one */
  for (c = p->children; c; c = c->sister)
    {
      /* check if this child is uniform */
      int class = field_uniform_class (c);
      if (class == COB_VOID || class != last_class)
	{
	  /* if not, or if this child is in a different category,
	     initialize the last uniform sequence */
	  if (first_field && last_class != COB_ALPHANUMERIC)
	    output_initialize_uniform (COBC_TREE (first_field), last_class,
				       c->offset - first_field->offset);
	  /* if not uniform, initialize the children */
	  if (class == COB_VOID)
	    output_initialize_compound (COBC_TREE (c));
	  last_class = class;
	  first_field = (class != COB_VOID) ? c : NULL;
	}
    }
  /* initialize the final uniform sequence */
  if (first_field && last_class != COB_ALPHANUMERIC)
    output_initialize_uniform (COBC_TREE (first_field), last_class,
			       p->offset + p->size - first_field->offset);
}

static void
output_initialize (cobc_tree x)
{
  int class = field_uniform_class (COBC_FIELD (x));
  if (class != COB_VOID)
    {
      /* if field is uniform (i.e., all children are the same category),
	 initialize it at once */
      output_initialize_uniform (x, class, COBC_FIELD (x)->size);
    }
  else
    {
      /* otherwise, fill the field by spaces first */
      if (COBC_FIELD (x)->children)
	output_initialize_uniform (x, COB_ALPHANUMERIC, COBC_FIELD (x)->size);
      /* then initialize the children recursively */
      output_initialize_compound (x);
    }
}

static struct cobc_list *initialize_replacing_list;

static void
output_initialize_replacing_internal (struct cobc_field *p)
{
  if (!p->children)
    {
      struct cobc_list *l;
      for (l = initialize_replacing_list; l; l = l->next)
	{
	  struct cobc_pair *pair = l->item;
	  int category = (int) pair->x;
	  cobc_tree text = pair->y;
	  if (category == p->category)
	    {
	      output_move (text, COBC_TREE (p));
	      break;
	    }
	}
      return;
    }

  for (p = p->children; p; p = p->sister)
    output_recursive (output_initialize_replacing_internal, COBC_TREE (p));
}

static void
output_initialize_replacing (cobc_tree x, struct cobc_list *l)
{
  initialize_replacing_list = l;
  output_recursive (output_initialize_replacing_internal, x);
}


/*
 * STRING
 */

static void
output_string (cobc_tree x, cobc_tree list)
{
  struct cobc_list *l = (struct cobc_list *) list;
  output_prefix ();
  output ("cob_string (");
  output_tree (x);
  for (; l; l = l->next)
    {
      struct string_item *p = l->item;
      output (", %d", p->type);
      if (p->sy)
	{
	  output (", ");
	  output_tree (p->sy);
	}
    }
  output (", 0);\n");
}


/*
 * UNSTRING
 */

static void
output_unstring (cobc_tree x, cobc_tree list)
{
  struct cobc_list *l = (struct cobc_list *) list;
  output_prefix ();
  output ("cob_unstring (");
  output_tree (x);
  for (; l; l = l->next)
    {
      struct string_item *p = l->item;
      output (", %d", p->type);
      if (p->sy)
	{
	  output (", ");
	  output_tree (p->sy);
	}
    }
  output (", 0);\n");
}


/*
 * INSPECT
 */

static void
output_inspect (char *name, cobc_tree var, struct cobc_list *list)
{
  output_prefix ();
  output ("%s (", name);
  output_tree (var);
  for (; list; list = list->next)
    {
      struct cobc_list *l;
      struct inspect_item *p = list->item;
      /* parameters */
      output (", %d", p->type);
      if (p->sy1) { output (", "); output_tree (p->sy1); }
      if (p->sy2) { output (", "); output_tree (p->sy2); }
      /* BEFORE/AFTER */
      for (l = p->list; l; l = l->next)
	{
	  struct inspect_item *p = l->item;
	  output (", %d, ", p->type);
	  output_tree (p->sy1);
	}
      output (", 0");
    }
  output (", 0);\n");
}

static void
output_inspect_tallying (cobc_tree var, cobc_tree list)
{
  output_inspect ("cob_inspect_tallying", var, (struct cobc_list *) list);
}

static void
output_inspect_replacing (cobc_tree var, cobc_tree list)
{
  output_inspect ("cob_inspect_replacing", var, (struct cobc_list *) list);
}

static void
output_inspect_converting (cobc_tree var, cobc_tree list)
{
  output_inspect ("cob_inspect_converting", var, (struct cobc_list *) list);
}


/*
 * DISPLAY
 */

static void
output_display (cobc_tree x)
{
  if (COBC_LITERAL_P (x))
    {
      struct cobc_literal *p = COBC_LITERAL (x);
      output_prefix ();
      output ("fputs (");
      if (COBC_TREE_CLASS (x) == COB_NUMERIC)
	{
	  /* numeric literal */
	  int i;
	  output ("\"");
	  if (p->sign)
	    output ((p->sign < 0) ? "-" : "+");
	  for (i = 0; i < p->size - p->decimals; i++)
	    output ("%c", p->str[i]);
	  if (i < p->size)
	    {
	      output (".");
	      for (; i < p->size; i++)
		output ("%c", p->str[i]);
	    }
	  output ("\"");
	}
      else
	{
	  /* non-numeric literal */
	  output_quoted_string (p->str, p->size);
	}
      output (", stdout);\n");
    }
  else
    {
      output_call_1 ("cob_display", x);
    }
}


/*
 * SEARCH
 */

static void
output_search (cobc_tree table, cobc_tree var, cobc_tree whens)
{
  struct cobc_field *p = COBC_FIELD (table);
  cobc_tree idx = COBC_TREE (p->index_list->item);
  output_prefix ();
  output ("while (");
  output_index (idx);
  output (" <= ");
  output_index (make_integer (p->occurs));
  output (")\n");
  output_indent ("  {", 4);
  output_tree (whens);
  output_line ("else");
  output_indent ("  {", 4);
  output_prefix ();
  output_index (idx);
  output ("++;\n");
  if (var && var != idx)
    output_tree (make_op_assign (var, '+', cobc_int1));
  output_line ("continue;");
  output_indent ("  }", -4);
  output_line ("break;");
  output_indent ("  }", -4);
}

static void
output_search_at_end (cobc_tree table, cobc_tree sentence)
{
  struct cobc_field *p = COBC_FIELD (table);
  cobc_tree idx = COBC_TREE (p->index_list->item);
  output_prefix ();
  output ("if (");
  output_index (idx);
  output (" > ");
  output_index (make_integer (p->occurs));
  output (")\n");
  output_tree (sentence);
}


/*
 * CALL
 */

static void
output_call_statement (cobc_tree name, struct cobc_list *args, cobc_tree ret)
{
  int static_link = 0;
  struct cobc_list *l;

  if (cobc_link_style == LINK_STATIC && COBC_LITERAL_P (name))
    static_link = 1;

  /* local variables */
  output_indent ("{", 2);
  output_line ("int ret = 0;");
  if (!static_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = args; l; l = l->next)
    {
      struct call_item *item = l->item;
      if (item->mode == COBC_CALL_BY_CONTENT)
	{
	  output_prefix ();
	  output ("char c_%s_data[", COBC_FIELD (item->var)->cname);
	  output_length (item->var);
	  output ("];\n");
	}
    }
  for (l = args; l; l = l->next)
    {
      struct call_item *item = l->item;
      if (item->mode == COBC_CALL_BY_CONTENT)
	{
	  output_prefix ();
	  output ("memcpy (c_%s_data, ", COBC_FIELD (item->var)->cname);
	  output_location (item->var);
	  output (", ");
	  output_length (item->var);
	  output (");\n");
	}
    }

  /* function name */
  output_prefix ();
  if (static_link)
    {
      /* static link */
      output ("ret = %s", COBC_LITERAL (name)->str);
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      output_func_1 ("cob_call_resolve", name);
      output (";\n");
      output_line ("if (func)");
      output_prefix ();
      output ("  ret = func");
    }

  /* arguments */
  output (" (");
  for (l = args; l; l = l->next)
    {
      struct call_item *item = l->item;
      cobc_tree x = item->var;
      switch (item->mode)
	{
	case COBC_CALL_BY_REFERENCE:
	  output_location (x);
	  break;
	case COBC_CALL_BY_CONTENT:
	  output ("c_%s_data", COBC_FIELD (x)->cname);
	  break;
	case COBC_CALL_BY_VALUE:
	  switch (COBC_TREE_TAG (x))
	    {
	    case cobc_tag_literal:
	      if (COBC_TREE_CLASS (x) == COB_NUMERIC)
		output ("%lld", literal_to_int (COBC_LITERAL (x)));
	      else
		output ("%d", COBC_LITERAL (x)->str[0]);
	      break;
	    default:
	      switch (COBC_FIELD (x)->usage)
		{
		case USAGE_BINARY:
		case USAGE_INDEX:
		  output_index (x);
		  break;
		default:
		  output ("*");
		  output_location (x);
		  break;
		}
	      break;
	    }
	  break;
	}
      if (l->next)
	output (", ");
    }
  output (");\n");
  output_line ("init_environment ();");

  /* return value */
  if (ret)
    {
      output_prefix ();
      output ("cob_set_int (");
      output_tree (ret);
      output (", ret);\n");
    }

  output_indent ("}", -2);
}
