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
  unsigned char dst_data[p->size + 1];
  struct cob_field dst_fld = {&dst_desc, dst_data};

  dst_desc.len = p->size;
  if (p->children)
    {
      dst_desc.type = 'G';
    }
  else
    {
      dst_desc.type = get_type (p);
      dst_desc.decimals = p->pic->decimals;
      dst_desc.just_r = p->f.justified;
      dst_desc.have_sign = p->pic->have_sign;
      dst_desc.separate_sign = p->f.sign_separate;
      dst_desc.leading_sign = p->f.sign_leading;
      dst_desc.blank_zero = p->f.blank_zero;
      dst_desc.pic = p->pic->str;
    }

  cob_move (f, dst_fld);
  dst_data[p->size] = 0;
  output_memcpy (dst, dst_data);
}

static void
output_move_num (cobc_tree x, int high)
{
  switch (COBC_FIELD (x)->usage)
    {
    case USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0');
      break;
    case USAGE_BINARY:
    case USAGE_INDEX:
      output_native_assign (x, high ? -1 : 0);
      break;
    case USAGE_PACKED:
      puts ("not implemented");
      abort ();
    }
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
      output_memset (x, '0');
      break;
    default:
      output_advance_move (COB_ZERO, x);
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
      output_memset (x, ' ');
      break;
    default:
      output_advance_move (COB_SPACE, x);
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
      output_memset (x, 255);
      break;
    default:
      output_advance_move (COB_HIGH, x);
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
      output_memset (x, 0);
      break;
    default:
      output_advance_move (COB_LOW, x);
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
      output_memset (x, '\"');
      break;
    default:
      output_advance_move (COB_QUOTE, x);
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
  unsigned char dst_data[dst_p->size + 1];
  for (i = 0; i < dst_p->size; i++)
    dst_data[i] = src_data[i % src_len];
  dst_data[i] = 0;
  output_memcpy (dst, dst_data);
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
  else if (COBC_FIELD_P (dst))
    {
      char src_pic[5];
      struct cobc_literal *l = COBC_LITERAL (src);
      struct cob_field_desc src_desc =
	{l->size, COBC_TREE_CLASS (l), l->decimals};
      struct cob_field src_fld = {&src_desc, l->str};
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
 * SET
 */

void
output_set_true (cobc_tree x)
{
  cobc_tree parent = COBC_TREE (COBC_FIELD (x)->parent);
  if (COBC_SUBREF_P (x))
    parent = make_subref (parent, COBC_SUBREF (x)->subs);
  output_move (COBC_TREE (COBC_FIELD (x)->value), parent);
}


/*
 * INITIALIZE
 */

static int
field_uniform_class (struct cobc_field *p)
{
  if (!p->children)
    {
      if (COBC_TREE_CLASS (p) == COB_NUMERIC)
	return COB_NUMERIC;
      else
	return COB_ALPHANUMERIC;
    }
  else
    {
      int class = field_uniform_class (p->children);
      for (p = p->children->sister; p; p = p->sister)
	if (class != field_uniform_class (p))
	  return COB_VOID;
      return class;
    }
}

static void
output_initialize_internal (struct cobc_field *p)
{
  switch (field_uniform_class (p))
    {
    case COB_ALPHANUMERIC:
      break;
    case COB_NUMERIC:
      output_move_zero (COBC_TREE (p));
      break;
    default:
      {
	struct cobc_field *c;
	for (c = p->children; c; c = c->sister)
	  output_recursive (output_initialize_internal, COBC_TREE (c));
      }
    }
}

static void
output_initialize (cobc_tree x)
{
  output_memset (x, ' ');
  output_recursive (output_initialize_internal, x);
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
	  output_quoted_string (p->str);
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
    output_call_2 ("cob_add_int", var, cobc_int1);
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
  struct call_item *item;

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
      item = l->item;
      if (item->mode == COBC_CALL_BY_CONTENT)
	{
	  output_line ("char c_%s_data[%d];",
		       COBC_FIELD (item->var)->cname,
		       COBC_FIELD (item->var)->size);
	}
    }
  for (l = args; l; l = l->next)
    {
      item = l->item;
      if (item->mode == COBC_CALL_BY_CONTENT)
	{
	  output_prefix ();
	  output ("memcpy (c_%s_data, ", COBC_FIELD (item->var)->cname);
	  output_location (item->var);
	  output (", %d);\n", COBC_FIELD (item->var)->size);
	}
    }

  /* function name */
  if (static_link)
    {
      /* static link */
      output_prefix ();
      output ("ret = %s", COBC_LITERAL (name)->str);
    }
  else
    {
      /* dynamic link */
      output_prefix ();
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
      item = l->item;
      switch (item->mode)
	{
	case COBC_CALL_BY_REFERENCE:
	  output_location (item->var);
	  break;
	case COBC_CALL_BY_CONTENT:
	  output ("c_%s_data", COBC_FIELD (item->var)->cname);
	  break;
	case COBC_CALL_BY_VALUE:
	  output_location (item->var);
	  output ("[0]");
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
      output (", cob_status);\n");
    }

  output_indent ("}", -2);
}
