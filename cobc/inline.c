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
 * File error handler
 */

void
output_file_handler (struct cobc_file_name *f, int type,
		     cobc_tree st1, cobc_tree st2)
{
  if (st1)
    {
      output_line ("if (%s_desc.file_status[0] == '%d')", f->cname, type);
      output_tree (st1);
      output_line ("else");
    }
  output_line ("if (%s_desc.file_status[0] != '0')", f->cname);
  output_indent_level += 2;
  output_perform_call (f->handler, f->handler);
  output_indent_level -= 2;
  if (st2)
    {
      output_line ("else");
      output_tree (st2);
    }
}


/*
 * GO TO
 */

void
output_goto (struct cobc_label *p)
{
  output_line ("goto lb_%s;", p->cname);
}

void
output_goto_depending (struct cobc_list *labels, cobc_tree index)
{
  int i = 1;
  struct cobc_list *l;
  output_prefix ();
  output ("switch (");
  output_index (index);
  output (")\n");
  output_indent ("  {", 4);
  for (l = labels; l; l = l->next)
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

#define CATEGORY(x) \
  (COBC_FIELD (x)->pic ? COBC_FIELD (x)->pic->category : COB_ALPHANUMERIC)

static void
output_advance_move (struct cob_field f, cobc_tree dst)
{
  struct cobc_field *p = COBC_FIELD (dst);
  struct cob_field_desc dst_desc;
  unsigned char dst_data[p->size];
  struct cob_field dst_fld = {p->size, dst_data, &dst_desc};

  if (p->children || p->rename_thru
      || (p->level == 66 && p->redefines->children))
    {
      dst_desc.type = COB_GROUP;
      dst_desc.justified = 0;
    }
  else
    {
      dst_desc.type = get_type (p);
      dst_desc.digits = p->pic->digits;
      dst_desc.decimals = p->pic->decimals;
      dst_desc.have_sign = p->pic->have_sign;
      dst_desc.sign_separate = p->f.sign_separate;
      dst_desc.sign_leading = p->f.sign_leading;
      dst_desc.blank_zero = p->f.blank_zero;
      dst_desc.justified = p->f.justified;
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
    case COBC_USAGE_DISPLAY:
      output_memset (x, high ? '9' : '0', COBC_FIELD (x)->size);
      break;
    case COBC_USAGE_BINARY:
    case COBC_USAGE_INDEX:
      output_native_assign (x, high ? -1 : 0);
      break;
    }
}

static void
output_move_all (cobc_tree x, char c)
{
  struct cobc_field *p = COBC_FIELD (x);
  struct cob_field_desc desc = {'X'};
  unsigned char data[p->size];
  struct cob_field fld = {p->size, data, &desc};
  memset (data, c, p->size);
  output_advance_move (fld, x);
}

static void
output_move_space (cobc_tree x)
{
  switch (CATEGORY (x))
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
output_move_zero (cobc_tree x)
{
  switch (CATEGORY (x))
    {
    case COB_NUMERIC:
      if (COBC_FIELD (x)->f.blank_zero)
	output_move_space (x);
      else
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
output_move_high (cobc_tree x)
{
  switch (CATEGORY (x))
    {
    case COB_NUMERIC:
      output_move_num (x, 9);
      break;
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, 255, COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, 255);
      break;
    }
}

static void
output_move_low (cobc_tree x)
{
  switch (CATEGORY (x))
    {
    case COB_NUMERIC:
      output_move_num (x, 0);
      break;
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, 0, COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, 0);
      break;
    }
}

static void
output_move_quote (cobc_tree x)
{
  switch (CATEGORY (x))
    {
    case COB_NUMERIC:
    case COB_ALPHABETIC:
    case COB_ALPHANUMERIC:
      output_memset (x, '"', COBC_FIELD (x)->size);
      break;
    default:
      output_move_all (x, '"');
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
  if (COBC_FIELD (dst)->usage == COBC_USAGE_BINARY
      || COBC_FIELD (dst)->usage == COBC_USAGE_INDEX)
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
	{COBC_TREE_CLASS (l), l->size, l->decimals};
      unsigned char src_data[l->size + 1];
      struct cob_field src_fld = {l->size, src_data, &src_desc};
      strcpy (src_data, l->str);
      src_desc.pic = src_pic;
      src_pic[0] = COBC_TREE_CLASS (l);
      src_pic[1] = l->size;
      src_pic[2] = 0;
      if (l->sign)
	{
	  src_desc.have_sign = 1;
	  cob_put_sign (src_fld, l->sign);
	}
      output_advance_move (src_fld, dst);
    }
  else
    {
      output_call_2 ("cob_move", src, dst);
    }
}

static void
output_move_index (cobc_tree src, cobc_tree dst)
{
  output_prefix ();
  output_index (dst);
  output (" = ");
  output_index (src);
  output (";\n");
}

void
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
  else if (src == cobc_return_code)
    output_call_2 ("cob_set_int", dst, src);
  else if (src == cobc_true || src == cobc_false)
    output_move_index (src, dst);
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
      if (dstp->usage == COBC_USAGE_INDEX)
	output_move_index (src, dst);
      else if (srcp->usage == COBC_USAGE_INDEX)
	output_call_2 ("cob_set_int", dst, make_index (src));
      else
	output_call_2 ("cob_move", src, dst);
    }
}


/*
 * INITIALIZE
 */

static void output_initialize_internal (struct cobc_field *p);

static int
field_uniform_class (struct cobc_field *p)
{
  if (p->children)
    {
      int class = field_uniform_class (p->children);
      for (p = p->children->sister; p; p = p->sister)
	if (!p->redefines)
	  if (class != field_uniform_class (p))
	    return COB_VOID;
      return class;
    }
  else
    {
      switch (p->pic->category)
	{
	case COB_NUMERIC:
	  switch (p->usage)
	    {
	    case COBC_USAGE_DISPLAY:
	      return COB_NUMERIC;
	    case COBC_USAGE_BINARY:
	    case COBC_USAGE_INDEX:
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
  switch (CATEGORY (x))
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

void
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
      output_initialize_uniform (x, COB_ALPHANUMERIC, COBC_FIELD (x)->size);
      /* then initialize the children recursively */
      output_initialize_compound (x);
    }
}

static struct cobc_list *initialize_replacing_list;

static void
output_initialize_replacing_internal (struct cobc_field *p)
{
  if (p->children)
    {
      for (p = p->children; p; p = p->sister)
	output_recursive (output_initialize_replacing_internal, COBC_TREE (p));
    }
  else
    {
      struct cobc_list *l;
      for (l = initialize_replacing_list; l; l = l->next)
	{
	  struct cobc_pair *pair = l->item;
	  int category = (int) pair->x;
	  cobc_tree text = pair->y;
	  if (category == p->pic->category)
	    {
	      output_move (text, COBC_TREE (p));
	      break;
	    }
	}
    }
}

void
output_initialize_replacing (cobc_tree x, struct cobc_list *l)
{
  initialize_replacing_list = l;
  output_recursive (output_initialize_replacing_internal, x);
}


/*
 * DISPLAY
 */

void
output_display (cobc_tree x, cobc_tree fd)
{
  if (COBC_LITERAL_P (x))
    {
      struct cobc_literal *p = COBC_LITERAL (x);
      output_prefix ();
      output ("cob_puts (");
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
      output (", %d);\n", COBC_INTEGER (fd)->val);
    }
  else if (COBC_FIELD_P (x) && COBC_FIELD (x)->usage == COBC_USAGE_INDEX)
    {
      output_call_2 ("cob_puti", make_index (x), fd);
    }
  else
    {
      output_call_2 ("cob_display", x, fd);
    }
}


/*
 * SEARCH
 */

static void
output_occurs (struct cobc_field *p)
{
  if (p->occurs_depending)
    output_index (p->occurs_depending);
  else
    output_index (make_integer (p->occurs));
}

void
output_search (cobc_tree table, cobc_tree var,
	     cobc_tree sentence, cobc_tree whens)
{
  struct cobc_list *l;
  struct cobc_field *p = COBC_FIELD (table);
  cobc_tree idx = NULL;

  /* determine the index to use */
  for (l = p->index_list; l; l = l->next)
    if (l->item == var)
      idx = var;
  if (!idx)
    idx = COBC_TREE (p->index_list->item);

  /* start loop */
  output_line ("while (1)");
  output_indent ("  {", 4);

  /* end test */
  output_prefix ();
  output ("if (");
  output_index (idx);
  output (" > ");
  output_occurs (p);
  output (")\n");
  output_indent ("  {", 4);
  if (sentence)
    output_tree (sentence);
  output_line ("break;");
  output_indent ("  }", -4);

  /* WHEN test */
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
search_set_keys (struct cobc_field *p, cobc_tree x)
{
  switch (COBC_COND (x)->type)
    {
    case COBC_COND_AND:
      search_set_keys (p, COBC_COND (x)->left);
      search_set_keys (p, COBC_COND (x)->right);
      break;
    case COBC_COND_EQ:
      {
	int i;
	cobc_tree ref = COBC_COND (x)->left;
	cobc_tree val = COBC_COND (x)->right;
	for (i = 0; i < p->nkeys; i++)
	  if (COBC_FIELD (ref) == COBC_FIELD (p->keys[i].key))
	    {
	      p->keys[i].ref = ref;
	      p->keys[i].val = val;
	      break;
	    }
	break;
      }
    default:
      /* cannot happen */
      break;
    }
}

void
output_search_all (cobc_tree table, cobc_tree sentence, cobc_tree when)
{
  int i;
  struct cobc_field *p = COBC_FIELD (table);
  struct cobc_if *ifp = COBC_IF (when);
  cobc_tree idx = COBC_TREE (p->index_list->item);

  /* set keys */
  for (i = 0; i < p->nkeys; i++)
    {
      p->keys[i].ref = 0;
      p->keys[i].val = 0;
    }
  search_set_keys (p, ifp->test);

  /* header */
  output_indent ("{", 2);
  output_line ("int head = %d - 1;", p->occurs_min);
  output_prefix ();
  output ("int tail = ");
  output_occurs (p);
  output (" + 1;\n");
  for (i = 0; i < p->nkeys; i++)
    if (p->keys[i].ref)
      output_line ("int cmp%d;", i);

  /* start loop */
  output_line ("while (1)");
  output_indent ("  {", 4);

  /* end test */
  output_line ("if (head >= tail - 1)");
  output_indent ("  {", 4);
  if (sentence)
    output_tree (sentence);
  output_line ("break;");
  output_indent ("  }", -4);

  /* next index */
  output_prefix ();
  output_index (idx);
  output (" = (head + tail) / 2;\n");

  /* WHEN test */
  for (i = 0; i < p->nkeys; i++)
    if (p->keys[i].ref)
      {
	output_prefix ();
	output ("cmp%d = ", i);
	output_compare (0, p->keys[i].ref, p->keys[i].val);
	output (";\n");
      }
  for (i = 0; i < p->nkeys; i++)
    if (p->keys[i].ref)
      {
	int flag = (p->keys[i].dir == COB_ASCENDING);
	output_line ("if (cmp%d < 0)", i);
	output_prefix ();
	output ("  %s = ", flag ? "head" : "tail");
	output_index (idx);
	output (";\n");
	output_line ("else if (cmp%d > 0)", i);
	output_prefix ();
	output ("  %s = ", flag ? "tail" : "head");
	output_index (idx);
	output (";\n");
	output_line ("else");
      }
  output_indent ("  {", 4);
  output_tree (ifp->stmt1);
  output_line ("break;");
  output_indent ("  }", -4);
  output_indent ("  }", -4);
  output_indent ("}", -2);
}


/*
 * CALL
 */

void
output_call_statement (cobc_tree name, struct cobc_list *args,
		       cobc_tree st1, cobc_tree st2)
{
  int n;
  int dynamic_link = 1;
  struct cobc_list *l;

  if (cobc_flags.static_call && COBC_LITERAL_P (name))
    dynamic_link = 0;

  /* local variables */
  output_indent ("{", 2);
  if (dynamic_link)
    output_line ("int (*func)();");

  /* setup arguments */
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      switch (p->type)
	{
	case COBC_CALL_BY_CONTENT:
	  output_prefix ();
	  output ("char content_%d[", n);
	  output_length (p->x);
	  output ("];\n");
	  break;
	case COBC_CALL_BY_LENGTH:
	  output_prefix ();
	  output ("int length_%d = ", n);
	  output_length (p->x);
	  output (";\n");
	}
    }
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      switch (p->type)
	{
	case COBC_CALL_BY_CONTENT:
	  output_prefix ();
	  output ("memcpy (content_%d, ", n);
	  output_location (p->x);
	  output (", ");
	  output_length (p->x);
	  output (");\n");
	}
    }

  /* function name */
  output_prefix ();
  if (!dynamic_link)
    {
      /* static link */
      output ("cob_return_code = %s", COBC_LITERAL (name)->str);
    }
  else
    {
      /* dynamic link */
      output ("func = ");
      if (COBC_LITERAL_P (name))
	output ("cob_resolve (\"%s\")", COBC_LITERAL (name)->str);
      else
	output_func_1 ("cob_call_resolve", name);
      output (";\n");
      output_line ("if (func == NULL)");
      output_indent_level += 2;
      if (st1)
	output_tree (st1);
      else
	output_call_0 ("cob_call_error");
      output_indent_level -= 2;
      output_line ("else");
      output_indent ("  {", 4);
      output_prefix ();
      output ("cob_return_code = func");
    }

  /* arguments */
  output (" (");
  for (l = args, n = 1; l; l = l->next, n++)
    {
      struct cobc_parameter *p = l->item;
      cobc_tree x = p->x;
      switch (p->type)
	{
	case COBC_CALL_BY_REFERENCE:
	  output_location (x);
	  break;
	case COBC_CALL_BY_CONTENT:
	  output ("content_%d", n);
	  break;
	case COBC_CALL_BY_LENGTH:
	  output ("&length_%d", n);
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
		case COBC_USAGE_BINARY:
		case COBC_USAGE_INDEX:
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
  if (st2)
    output_tree (st2);
  if (dynamic_link)
    output_indent ("  }", -4);
  output_indent ("}", -2);
}
