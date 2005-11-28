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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <libcob.h>

#include "cobc.h"
#include "tree.h"

struct expr_node {
	/* The token of this node.
	 *  'x'                          - values (cb_tree)
	 *  '+', '-', '*', '/', '^'      - arithmetic operators
	 *  '=', '~', '<', '>', '[', ']' - relational operators
	 *  '!', '&', '|'                - logical operators
	 *  '(', ')'                     - parentheses
	 */
	int		token;
	/* The value itself if this node is a value */
	cb_tree		value;
};

static int		expr_op;			/* last operator */
static cb_tree		expr_lh;			/* last left hand */

static int		expr_index;			/* stack index */
static int		expr_stack_size;		/* stack max size */
static struct expr_node *expr_stack;			/* expr node stack */

static char		expr_prio[128];

#define TOKEN(offset)	(expr_stack[expr_index + offset].token)
#define VALUE(offset)	(expr_stack[expr_index + offset].value)

#define cb_emit(x) \
	current_statement->body = cb_list_add (current_statement->body, x)
#define cb_emit_list(l) \
	current_statement->body = cb_list_append (current_statement->body, l)

#define VALIDATE(x) \
	if (x == cb_error_node) return;
#define VALIDATE_LIST(l)			\
{							\
	cb_tree _l;					\
	for (_l = l; _l; _l = CB_CHAIN (_l))		\
		if (CB_VALUE (_l) == cb_error_node)	\
return;							\
}

cb_tree
cb_check_group_name (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	if (CB_REFERENCE_P (x)
	    && CB_FIELD_P (cb_ref (x))
	    && CB_FIELD (cb_ref (x))->children != NULL && CB_REFERENCE (x)->offset == NULL)
		return x;

	cb_error_x (x, _("'%s' is not group name"), cb_name (x));
	return cb_error_node;
}

cb_tree
cb_check_numeric_name (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	if (CB_REFERENCE_P (x)
	    && CB_FIELD_P (cb_ref (x))
	    && CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC)
		return x;

	cb_error_x (x, _("'%s' is not numeric name"), cb_name (x));
	return cb_error_node;
}

cb_tree
cb_check_numeric_edited_name (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	if (CB_REFERENCE_P (x)
	    && CB_FIELD_P (cb_ref (x))
	    && (CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
		|| CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC_EDITED))
		return x;

	cb_error_x (x, _("'%s' is not numeric or numeric-edited name"), cb_name (x));
	return cb_error_node;
}

cb_tree
cb_check_numeric_value (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	if (CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC)
		return x;

	cb_error_x (x, _("'%s' is not numeric value"), cb_name (x));
	return cb_error_node;
}

cb_tree
cb_check_integer_value (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	if (CB_TREE_CATEGORY (x) != CB_CATEGORY_NUMERIC)
		goto invalid;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
	{
		if (x != cb_zero)
			goto invalid;
		return x;
	}
	case CB_TAG_LITERAL:
	{
		struct cb_literal *l = CB_LITERAL (x);

		if (l->sign < 0 || l->scale > 0)
			goto invalid;
		return x;
	}
	case CB_TAG_REFERENCE:
	{
		struct cb_field *f = CB_FIELD (cb_ref (x));

		if (f->pic->scale > 0)
			goto invalid;
		return x;
	}
	case CB_TAG_BINARY_OP:
	{
		/* TODO: need to check */
		return x;
	}
	default:
	      invalid:
		cb_error_x (x, _("'%s' is not integer value"), cb_name (x));
		return cb_error_node;
	}
}

void
cb_build_registers (void)
{
	time_t	t;
	char	buff[24];

	/* RETURN-CODE */
	cb_return_code = cb_build_index (cb_build_reference ("RETURN-CODE"));
	CB_FIELD (cb_ref (cb_return_code))->values = cb_list (cb_zero);

	/* NUMBER-OF-CALL-PARAMETERS */
	cb_call_params = cb_build_index (cb_build_reference ("NUMBER-OF-CALL-PARAMETERS"));
	CB_FIELD (cb_ref (cb_call_params))->values = cb_list (cb_zero);

	/* TALLY */
	/* 01 TALLY GLOBAL PICTURE 9(9) USAGE COMP-5 VALUE ZERO. */
	/* As TALLY only has relevance together with EXAMINE which
	   is not standard and not supported, comment this out
	   {
	   cb_tree x = cb_build_field (cb_build_reference ("TALLY"));

	   CB_FIELD (x)->pic = CB_PICTURE (cb_build_picture ("9(9)"));
	   CB_FIELD (x)->usage = CB_USAGE_COMP_5;
	   CB_FIELD (x)->values = cb_list (cb_zero);
	   cb_validate_field (CB_FIELD (x));
	   current_program->working_storage =
	   cb_field_add (current_program->working_storage, CB_FIELD (x));
	   }
	 */

	t = time (NULL);

	/* WHEN-COMPILED */
	memset (buff, 0, sizeof (buff));
	strftime (buff, 17, "%m/%d/%y%H.%M.%S", localtime (&t));
	cb_build_constant (cb_build_reference ("WHEN-COMPILED"),
			   cb_build_alphanumeric_literal ((ucharptr)buff, 16));

	/* FUNCTION WHEN-COMPILED */
	memset (buff, 0, sizeof (buff));
	strftime (buff, 22, "%Y%m%d%H%M%S00%z", localtime (&t));
	cb_intr_whencomp = cb_build_alphanumeric_literal ((ucharptr)buff, 21);

	/* FUNCTION PI */
	memset (buff, 0, sizeof (buff));
	strcpy (buff, "314159265358979323");
	cb_intr_pi = cb_build_numeric_literal (0, (ucharptr)buff, 17);

	/* FUNCTION E */
	memset (buff, 0, sizeof (buff));
	strcpy (buff, "271828182845904523");
	cb_intr_e = cb_build_numeric_literal (0, (ucharptr)buff, 17);

}

char *
cb_encode_program_id (const char *name)
{
	char		buff[FILENAME_MAX];
	char		*p = buff;
	const char	*s = name;

	/* encode the initial digit */
	if (isdigit (*s))
		p += sprintf (p, "$%02X", *s++);
	/* encode invalid letters */
	for (; *s; s++)
		if (isalnum (*s) || *s == '_')
			*p++ = *s;
		else
			p += sprintf (p, "$%02X", *s);
	*p = 0;
	return strdup (buff);
}

const char *
cb_build_program_id (cb_tree name, cb_tree alt_name)
{
	if (alt_name)
		return (char *)CB_LITERAL (alt_name)->data;
	else if (CB_LITERAL_P (name))
		return cb_encode_program_id ((char *)CB_LITERAL (name)->data);
	else
		return cb_encode_program_id (CB_NAME (name));
}

void
cb_define_switch_name (cb_tree name, cb_tree sname, cb_tree flag, cb_tree ref)
{
	VALIDATE (name);
	VALIDATE (sname);

	if (CB_SYSTEM_NAME (sname)->category != CB_SWITCH_NAME) {
		cb_error_x (ref, _("switch-name is expected '%s'"), CB_NAME (ref));
	} else {
		cb_tree switch_id = cb_int (CB_SYSTEM_NAME (sname)->token);
		cb_tree value = cb_build_funcall_1 ("cob_get_switch", switch_id);

		if (flag == cb_int0)
			value = cb_build_negation (value);
		cb_build_constant (name, value);
	}
}

cb_tree
cb_build_section_name (cb_tree name)
{
	if (name == cb_error_node)
		return cb_error_node;

	if (CB_REFERENCE (name)->word->count > 0) {
		cb_tree x = CB_VALUE (CB_REFERENCE (name)->word->items);

		/* used as a non-label name or used as a section name or
		   used as the same paragraph name in the same section */
		if (!CB_LABEL_P (x) || CB_LABEL (x)->section == NULL
		    || CB_LABEL (x)->section == current_section) {
			redefinition_error (name);
			return cb_error_node;
		}
	}

	return name;
}

cb_tree
cb_build_assignment_name (cb_tree name)
{
	if (name == cb_error_node)
		return cb_error_node;

	switch (CB_TREE_TAG (name)) {
	case CB_TAG_LITERAL:
		return name;

	case CB_TAG_REFERENCE:
		switch (cb_assign_clause) {
		case CB_ASSIGN_COBOL2002:
			/* TODO */
			return cb_error_node;

		case CB_ASSIGN_MF:
			current_program->reference_list =
			    cb_list_add (current_program->reference_list, name);
			return name;

		case CB_ASSIGN_IBM:
		{
			const char *s = CB_REFERENCE (name)->word->name;
			const char *p;

			/* check organization */
			if (strncmp (s, "S-", 2) == 0 || strncmp (s, "AS-", 3) == 0)
				goto org;
			/* skip the device label if exists */
			if ((p = strchr (s, '-')) != NULL)
				s = p + 1;
			/* check organization again */
			if (strncmp (s, "S-", 2) == 0 || strncmp (s, "AS-", 3) == 0) {
			      org:
				/* skip it for now */
				s = strchr (s, '-') + 1;
			}
			/* convert the name into literal */
			return cb_build_alphanumeric_literal ((ucharptr)s, strlen (s));
		}
		}

	default:
		ABORT ();
	}
}

cb_tree
cb_build_index (cb_tree x)
{
	if (CB_REFERENCE (x)->word->count == 0) {
		struct cb_field *f = CB_FIELD (cb_build_field (x));

		f->usage = CB_USAGE_INDEX;
		cb_validate_field (f);

		current_program->working_storage = cb_field_add (current_program->working_storage, f);

		return x;
	}

	if (!CB_INDEX_P (x)) {
		redefinition_error (x);
		return cb_error_node;
	}

	return x;
}

cb_tree
cb_build_identifier (cb_tree x)
{
	struct cb_reference	*r;
	struct cb_field		*f;
	const char		*name;
	cb_tree			v;

	if (x == cb_error_node)
		return cb_error_node;

	r = CB_REFERENCE (x);
	name = r->word->name;

	/* resolve reference */
	v = cb_ref (x);
	if (v == cb_error_node)
		return cb_error_node;

	/* check if it is a data name */
	if (!CB_FIELD_P (v)) {
		if (r->subs) {
			cb_error_x (x, _("'%s' cannot be subscripted"), name);
			return cb_error_node;
		}
		if (r->offset) {
			cb_error_x (x, _("'%s' cannot be reference modified"), name);
			return cb_error_node;
		}
		return x;
	}
	f = CB_FIELD (v);

	/* check the number of subscripts */
	if (!r->all && cb_list_length (r->subs) != f->indexes)
		switch (f->indexes) {
		case 0:
			cb_error_x (x, _("'%s' cannot be subscripted"), name);
			return cb_error_node;
		case 1:
			cb_error_x (x, _("'%s' requires 1 subscript"), name);
			return cb_error_node;
		default:
			cb_error_x (x, _("'%s' requires %d subscripts"), name, f->indexes);
			return cb_error_node;
		}

	/* subscript check */
	if (!r->all && r->subs) {
		struct cb_field *p;
		cb_tree l = r->subs;

		for (p = f; p; p = p->parent) {
			if (p->flag_occurs) {
				cb_tree sub = cb_check_integer_value (CB_VALUE (l));

				l = CB_CHAIN (l);

				if (sub == cb_error_node)
					continue;

				/* compile-time check */
				if (CB_LITERAL_P (sub)) {
					int n = cb_get_int (sub);

					if (n < p->occurs_min || n > p->occurs_max)
						cb_error_x (x, _("subscript of '%s' out of bounds: %d"),
							    name, n);
				}

				/* run-time check */
				if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_SUBSCRIPT)) {
					if (p->occurs_depending) {
						int n = p->occurs_max;

						if (CB_LITERAL_P (sub))
							n = cb_get_int (sub);
						if (p->occurs_min <= n && n <= p->occurs_max) {
							cb_tree e1, e2;

							e1 = cb_build_funcall_4 ("cob_check_odo",
								 cb_build_cast_integer(p->occurs_depending),
								 cb_int (p->occurs_min),
								 cb_int (p->occurs_max),
								 cb_build_string0
								 ((ucharptr)(cb_field (p->occurs_depending)->name)));
							e2 = cb_build_funcall_4 ("cob_check_subscript",
								 cb_build_cast_integer(sub),
								 cb_int (p->occurs_min),
								 cb_build_cast_integer(p->occurs_depending),
								 cb_build_string0 ((ucharptr)p->name));
							r->check = cb_list_add (r->check, e1);
							r->check = cb_list_add (r->check, e2);
						}
					} else {
						if (!CB_LITERAL_P (sub)) {
							cb_tree e1;

							e1 = cb_build_funcall_4 ("cob_check_subscript",
									 cb_build_cast_integer
									 (sub), cb_int1,
									 cb_int (p->occurs_max),
									 cb_build_string0 ((ucharptr)p->name));
							r->check = cb_list_add (r->check, e1);
						}
					}
				}
			}
		}
	}

	/* reference modification check */
	if (r->offset) {
		/* compile-time check */
		if (CB_LITERAL_P (r->offset)) {
			int offset = cb_get_int (r->offset);

			if (offset < 1 || offset > f->size)
				cb_error_x (x, _("offset of '%s' out of bounds: %d"), name, offset);
			else if (r->length && CB_LITERAL_P (r->length)) {
				int length = cb_get_int (r->length);

				if (length < 1 || length > f->size - offset + 1)
					cb_error_x (x, _("length of '%s' out of bounds: %d"),
						    name, length);
			}
		}

		/* run-time check */
		if (CB_EXCEPTION_ENABLE (COB_EC_BOUND_REF_MOD)) {
			if (!CB_LITERAL_P (r->offset)
			    || (r->length && !CB_LITERAL_P (r->length))) {
				cb_tree e1;

				e1 = cb_build_funcall_4 ("cob_check_ref_mod",
							 cb_build_cast_integer (r->offset),
							 r->length ? cb_build_cast_integer (r->length) :
							 cb_int1, cb_int (f->size),
							 cb_build_string0 ((ucharptr)f->name));
				r->check = cb_list_add (r->check, e1);
			}
		}
	}

	if (f->storage == CB_STORAGE_CONSTANT)
		return CB_VALUE (f->values);

	return x;
}

static cb_tree
cb_build_length_1 (cb_tree x)
{
	struct cb_field *f = CB_FIELD (cb_ref (x));

	if (cb_field_variable_size (f) == NULL) {
		/* constant size */
		return cb_int (cb_field_size (x));
	} else {
		/* variable size */
		cb_tree e = NULL;

		for (f = f->children; f; f = f->sister) {
			cb_tree size = cb_build_length_1 (cb_build_field_reference (f, x));
			if (f->occurs_depending)
				size = cb_build_binary_op (size, '*', f->occurs_depending);
			else if (f->occurs_max > 1)
				size = cb_build_binary_op (size, '*', cb_int (f->occurs_max));
			e = e ? cb_build_binary_op (e, '+', size) : size;
		}
		return e;
	}
}

cb_tree
cb_build_length (cb_tree x)
{
	cb_tree		temp;
	struct cb_field *f;
	char		buff[64];

	if (x == cb_error_node)
		return cb_error_node;
	if (CB_REFERENCE_P (x) && cb_ref (x) == cb_error_node)
		return cb_error_node;

	memset (buff, 0, sizeof (buff));
	if (CB_LITERAL_P (x)) {
		struct cb_literal *l = CB_LITERAL (x);

		sprintf (buff, "%d", (int)l->size);
		return cb_build_numeric_literal (0, (ucharptr)buff, 0);
	}
	if (CB_FIELD_P (x) || CB_REFERENCE_P (x)) {
		f = CB_FIELD (cb_ref (x));
		if (cb_field_variable_size (f) == NULL) {
			sprintf (buff, "%d", cb_field_size (x));
			return cb_build_numeric_literal (0, (ucharptr)buff, 0);
		}
	}
	temp = cb_build_index (cb_build_filler ());
	CB_FIELD (cb_ref (temp))->usage = CB_USAGE_LENGTH;
	CB_FIELD (cb_ref (temp))->count++;
	cb_emit (cb_build_assign (temp, cb_build_length_1 (x)));
	return temp;
}

cb_tree
cb_build_address (cb_tree x)
{
	if (x == cb_error_node || cb_ref (x) == cb_error_node)
		return cb_error_node;

	return cb_build_cast_address (x);
}

/* validate program */

void
cb_validate_program_environment (struct cb_program *prog)
{
	/* resolve the program collating sequence */
	if (prog->collating_sequence) {
		cb_tree x = cb_ref (prog->collating_sequence);

		if (x == cb_error_node) {
			prog->collating_sequence = NULL;
			return;
		}

		if (!CB_ALPHABET_NAME_P (x)) {
			cb_error_x (prog->collating_sequence, _("'%s' not alphabet name"),
				    cb_name (prog->collating_sequence));
			prog->collating_sequence = NULL;
			return;
		}

		if (CB_ALPHABET_NAME (x)->type == CB_ALPHABET_CUSTOM) {
			x = CB_VALUE (CB_ALPHABET_NAME (x)->custom_list);
			if (CB_PAIR_P (x) && CB_PAIR_X (x))
				cb_low = CB_PAIR_X (x);
			else if (CB_LIST_P (x))
				cb_low = CB_VALUE (x);
			else
				cb_low = cb_build_alphanumeric_literal (CB_LITERAL (x)->data, 1);
		}
	}
}

void
cb_validate_program_data (struct cb_program *prog)
{
	cb_tree l;

	/* build undeclared assignment name now */
	if (cb_assign_clause == CB_ASSIGN_MF)
		for (l = current_program->file_list; l; l = CB_CHAIN (l)) {
			cb_tree assign = CB_FILE (CB_VALUE (l))->assign;
			if (CB_REFERENCE_P (assign) && CB_REFERENCE (assign)->word->count == 0) {
				cb_tree x = cb_build_implicit_field (assign, FILENAME_MAX);
				struct cb_field *p = current_program->working_storage;

				CB_FIELD (x)->count++;
				if (p) {
					while (p->sister)
						p = p->sister;
					p->sister = CB_FIELD (x);
				} else
					current_program->working_storage = CB_FIELD (x);
			}
		}

	/* resolve all references so far */
	for (l = cb_list_reverse (prog->reference_list); l; l = CB_CHAIN (l))
		cb_ref (CB_VALUE (l));
}

void
cb_validate_program_body (struct cb_program *prog)
{
	/* resolve all labels */
	cb_tree l;

	for (l = cb_list_reverse (prog->label_list); l; l = CB_CHAIN (l)) {
		cb_tree x = CB_VALUE (l);
		cb_tree v = cb_ref (x);
		if (CB_LABEL_P (v)) {
			CB_LABEL (v)->need_begin = 1;
			if (CB_REFERENCE (x)->length)
				CB_LABEL (v)->need_return = 1;
		} else if (v != cb_error_node)
			cb_error_x (x, _("'%s' not procedure name"), cb_name (x));
	}

	prog->file_list = cb_list_reverse (prog->file_list);
	prog->exec_list = cb_list_reverse (prog->exec_list);
}

/*
 * Expressions
 */

void
cb_expr_init (void)
{
	static int initialized = 0;

	if (initialized == 0) {
		/* init priority talble */
		expr_prio['x'] = 0;
		expr_prio['^'] = 1;
		expr_prio['*'] = 2;
		expr_prio['/'] = 2;
		expr_prio['+'] = 3;
		expr_prio['-'] = 3;
		expr_prio['='] = 4;
		expr_prio['~'] = 4;
		expr_prio['<'] = 4;
		expr_prio['>'] = 4;
		expr_prio['['] = 4;
		expr_prio[']'] = 4;
		expr_prio['!'] = 5;
		expr_prio['&'] = 6;
		expr_prio['|'] = 7;
		expr_prio[')'] = 8;
		expr_prio['('] = 9;
		expr_prio[0] = 10;
		/* init stack */
		expr_stack_size = 8;
		expr_stack = cob_malloc (sizeof (struct expr_node) * expr_stack_size);
		expr_stack[0].token = 0;	/* dummy */
		expr_stack[1].token = 0;	/* dummy */
		expr_stack[2].token = 0;	/* dummy */
		initialized = 1;
	}

	expr_op = 0;
	expr_lh = NULL;
	expr_index = 3;
}

static int
expr_reduce (int token)
{
	/* Example:
	 * index: -3  -2  -1   0
	 * token: 'x' '*' 'x' '+' ...
	 */

	while (expr_prio[TOKEN (-2)] <= expr_prio[token]) {
		/* Reduce the expression depending on the last operator */
		int op = TOKEN (-2);

		switch (op) {
		case 'x':
			return 0;

		case '+':
		case '-':
		case '*':
		case '/':
		case '^':
			/* Arithmetic operators: 'x' op 'x' */
			if (TOKEN (-1) != 'x' || TOKEN (-3) != 'x')
				return -1;
			TOKEN (-3) = 'x';
			VALUE (-3) = cb_build_binary_op (VALUE (-3), op, VALUE (-1));
			expr_index -= 2;
			break;

		case '!':
			/* Negation: '!' 'x' */
			if (TOKEN (-1) != 'x')
				return -1;
			/* 'x' '=' 'x' '|' '!' 'x' */
			if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
				VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
			TOKEN (-2) = 'x';
			VALUE (-2) = cb_build_negation (VALUE (-1));
			expr_index -= 1;
			break;

		case '&':
		case '|':
			/* Logical AND/OR: 'x' op 'x' */
			if (TOKEN (-1) != 'x' || TOKEN (-3) != 'x')
				return -1;
			/* 'x' '=' 'x' '|' 'x' */
			if (expr_lh) {
				if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
					VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
				if (CB_TREE_CLASS (VALUE (-3)) != CB_CLASS_BOOLEAN)
					VALUE (-3) = cb_build_binary_op (expr_lh, expr_op, VALUE (-3));
			}
			/* warning for complex expressions without explicit parentheses
			   (i.e., "a OR b AND c" or "a AND b OR c") */
			if (cb_warn_parentheses && op == '|')
				if ((CB_BINARY_OP_P (VALUE (-3))
				     && CB_BINARY_OP (VALUE (-3))->op == '&')
				    || (CB_BINARY_OP_P (VALUE (-1))
					&& CB_BINARY_OP (VALUE (-1))->op == '&'))
					cb_warning (_("suggest parentheses around AND within OR"));
			TOKEN (-3) = 'x';
			VALUE (-3) = cb_build_binary_op (VALUE (-3), op, VALUE (-1));
			expr_index -= 2;
			break;

		case '(':
		case ')':
			return 0;

		default:
			/* Relational operators */
			if (TOKEN (-1) != 'x')
				return -1;
			switch (TOKEN (-3)) {
			case 'x':
				/* Simple condition: 'x' op 'x' */
				if (VALUE (-3) == cb_error_node || VALUE (-1) == cb_error_node)
					VALUE (-3) = cb_error_node;
				else {
					expr_lh = VALUE (-3);
					expr_op = op;
					TOKEN (-3) = 'x';
					if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
						VALUE (-3) = cb_build_binary_op (expr_lh, op, VALUE (-1));
					else
						VALUE (-3) = VALUE (-1);
				}
				expr_index -= 2;
				break;
			case '&':
			case '|':
				/* Complex condition: 'x' '=' 'x' '|' op 'x' */
				if (VALUE (-1) == cb_error_node)
					VALUE (-2) = cb_error_node;
				else {
					expr_op = op;
					TOKEN (-2) = 'x';
					if (CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN)
						VALUE (-2) = cb_build_binary_op (expr_lh, op, VALUE (-1));
					else
						VALUE (-2) = VALUE (-1);
				}
				expr_index -= 1;
				break;
			default:
				return -1;
			}
			break;
		}
	}

	/* handle special case "op OR x AND" */
	if (token == '&' && TOKEN (-2) == '|' && CB_TREE_CLASS (VALUE (-1)) != CB_CLASS_BOOLEAN) {
		TOKEN (-1) = 'x';
		VALUE (-1) = cb_build_binary_op (expr_lh, expr_op, VALUE (-1));
	}

	return 0;
}

void
cb_expr_shift (int token, cb_tree value)
{
	switch (token) {
	case 'x':
		/* sign ZERO condition */
		if (value == cb_zero)
			if (TOKEN (-1) == 'x' || TOKEN (-1) == '!') {
				cb_expr_shift_sign ('=');
				return;
			}

		/* class condition */
		if (CB_REFERENCE_P (value)
		    && CB_CLASS_NAME_P (cb_ref (value))) {
			cb_expr_shift_class (CB_CLASS_NAME (cb_ref (value))->cname);
			return;
		}

		/* unary sign */
		if ((TOKEN (-1) == '+' || TOKEN (-1) == '-') && TOKEN (-2) != 'x') {
			if (TOKEN (-1) == '-')
				value = cb_build_binary_op (cb_zero, '-', value);
			expr_index -= 1;
		}
		break;

	case '(':
		/* 'x' op '(' --> '(' 'x' op */
		switch (TOKEN (-1)) {
		case '=':
		case '~':
		case '<':
		case '>':
		case '[':
		case ']':
			expr_op = TOKEN (-1);
			if (TOKEN (-2) == 'x')
				expr_lh = VALUE (-2);
		}
		break;

	case ')':
		/* enclose by parentheses */
		expr_reduce (token);
		if (TOKEN (-2) == '(') {
			value = cb_build_parenthesis (VALUE (-1));
			expr_index -= 2;
			cb_expr_shift ('x', value);
			return;
		}
		break;

	default:
		/* '<' '|' '=' --> '[' */
		/* '>' '|' '=' --> ']' */
		if (token == '=' && TOKEN (-1) == '|' && (TOKEN (-2) == '<' || TOKEN (-2) == '>')) {
			token = (TOKEN (-2) == '<') ? '[' : ']';
			expr_index -= 2;
		}

		/* '!' '=' --> '~', etc. */
		if (TOKEN (-1) == '!')
			switch (token) {
			case '=':
				token = '~';
				expr_index--;
				break;
			case '~':
				token = '=';
				expr_index--;
				break;
			case '<':
				token = ']';
				expr_index--;
				break;
			case '>':
				token = '[';
				expr_index--;
				break;
			case '[':
				token = '>';
				expr_index--;
				break;
			case ']':
				token = '<';
				expr_index--;
				break;
			}
		break;
	}

	/* reduce */
	expr_reduce (token);

	/* allocate sufficient stack memory */
	if (expr_index >= expr_stack_size) {
		expr_stack_size *= 2;
		expr_stack = realloc (expr_stack, sizeof (struct expr_node) * expr_stack_size);
		if (!expr_stack) {
			fprintf (stderr, "Memory realloc failed - Aborting\n");
			fflush (stderr);
			(void)longjmp (cob_jmpbuf, 1);
		}
	}

	/* put on the stack */
	TOKEN (0) = token;
	VALUE (0) = value;
	expr_index++;
}

void
cb_expr_shift_class (const char *name)
{
	int have_not = 0;

	if (TOKEN (-1) == '!') {
		have_not = 1;
		expr_index--;
	}
	expr_reduce ('=');
	if (TOKEN (-1) == 'x') {
		VALUE (-1) = cb_build_funcall_1 (name, VALUE (-1));
		if (have_not)
			VALUE (-1) = cb_build_negation (VALUE (-1));
	}
}

void
cb_expr_shift_sign (char op)
{
	int have_not = 0;

	if (TOKEN (-1) == '!') {
		have_not = 1;
		expr_index--;
	}
	expr_reduce ('=');
	if (TOKEN (-1) == 'x') {
		VALUE (-1) = cb_build_binary_op (VALUE (-1), op, cb_zero);
		if (have_not)
			VALUE (-1) = cb_build_negation (VALUE (-1));
	}
}

static void
expr_expand (cb_tree * x)
{
      start:
	/* remove parenthesis */
	if (CB_BINARY_OP_P (*x)) {
		struct cb_binary_op *p = CB_BINARY_OP (*x);

		if (p->op == '@') {
			*x = p->x;
			goto start;
		}
		expr_expand (&p->x);
		if (p->y)
			expr_expand (&p->y);
	}
}

cb_tree
cb_expr_finish (void)
{
	expr_reduce (0);	/* reduce all */

	if (expr_index != 4) {
		cb_error (_("invalid expression"));
		return cb_error_node;
	}

	expr_expand (&expr_stack[3].value);
	return expr_stack[3].value;
}

/*
 * Numerical operation
 */

static cb_tree decimal_stack = NULL;

#define dpush(x) decimal_stack = cb_cons (x, decimal_stack)

static cb_tree
build_store_option (cb_tree x, cb_tree round)
{
	int opt = 0;

	if (round == cb_int1)
		opt |= COB_STORE_ROUND;

	switch (CB_FIELD (cb_ref (x))->usage) {
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
		if (current_statement->handler1)
			opt |= COB_STORE_KEEP_ON_OVERFLOW;
		break;
	default:
		if (!cb_binary_truncate) {
			if (current_statement->handler1) {
				opt |= COB_STORE_KEEP_ON_OVERFLOW;
			}
			break;
		}

		if (current_statement->handler_id)
			opt |= COB_STORE_KEEP_ON_OVERFLOW;
		else if (cb_binary_truncate)
			opt |= COB_STORE_TRUNC_ON_OVERFLOW;
		break;
	}

	return cb_int (opt);
}

static cb_tree
decimal_alloc (void)
{
	cb_tree x = cb_build_decimal (current_program->decimal_index++);

	if (current_program->decimal_index > current_program->decimal_index_max)
		current_program->decimal_index_max = current_program->decimal_index;
	return x;
}

static void
decimal_free (void)
{
	current_program->decimal_index--;
}

static void
decimal_compute (char op, cb_tree x, cb_tree y)
{
	const char *func;

	switch (op) {
	case '+':
		func = "cob_decimal_add";
		break;
	case '-':
		func = "cob_decimal_sub";
		break;
	case '*':
		func = "cob_decimal_mul";
		break;
	case '/':
		func = "cob_decimal_div";
		break;
	case '^':
		func = "cob_decimal_pow";
		break;
	default:
		ABORT ();
	}
	dpush (cb_build_funcall_2 (func, x, y));
}

static void
decimal_expand (cb_tree d, cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
	{
		if (x == cb_zero)
			dpush (cb_build_funcall_2 ("cob_decimal_set_int", d, cb_int0));
		else
			ABORT ();
		break;
	}
	case CB_TAG_LITERAL:
	{
		/* set d, N */
		struct cb_literal *l = CB_LITERAL (x);

		if (l->size < 10 && l->scale == 0)
			dpush (cb_build_funcall_2 ("cob_decimal_set_int", d, cb_build_cast_integer (x)));
		else
			dpush (cb_build_funcall_2 ("cob_decimal_set_field", d, x));
		break;
	}
	case CB_TAG_REFERENCE:
	{
		/* set d, X */
		struct cb_field *f = cb_field (x);

		/* check numeric */
		if (CB_EXCEPTION_ENABLE (COB_EC_DATA_INCOMPATIBLE))
			if (f->usage == CB_USAGE_DISPLAY)
				dpush (cb_build_funcall_2 ("cob_check_numeric",
							   x, cb_build_string0 ((ucharptr)(f->name))));

		if (cb_fits_int (x))
			dpush (cb_build_funcall_2 ("cob_decimal_set_int", d, cb_build_cast_integer (x)));
		else
			dpush (cb_build_funcall_2 ("cob_decimal_set_field", d, x));
		break;
	}
	case CB_TAG_BINARY_OP:
	{
		/* set d, X
		 * set t, Y
		 * OP d, t */
		struct cb_binary_op *p = CB_BINARY_OP (x);
		cb_tree t = decimal_alloc ();

		decimal_expand (d, p->x);
		decimal_expand (t, p->y);
		decimal_compute (p->op, d, t);
		decimal_free ();
		break;
	}
/* RXW */
	case CB_TAG_INTRINSIC:
		dpush (cb_build_funcall_2 ("cob_decimal_set_field", d, x));
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		fflush (stderr);
		ABORT ();
	}
}

static void
decimal_assign (cb_tree x, cb_tree d, cb_tree round)
{
	dpush (cb_build_funcall_3 ("cob_decimal_get_field", d, x, build_store_option (x, round)));
}

static cb_tree
build_decimal_assign (cb_tree vars, char op, cb_tree val)
{
	cb_tree l;
	cb_tree s1 = NULL;
	cb_tree d = decimal_alloc ();

	/* set d, VAL */
	decimal_expand (d, val);

	if (op == 0) {
		for (l = vars; l; l = CB_CHAIN (l)) {
			/* set VAR, d */
			decimal_assign (CB_VALUE (l), d, CB_PURPOSE (l));
			s1 = cb_list_add (s1, cb_list_reverse (decimal_stack));
			decimal_stack = NULL;
		}
	} else {
		cb_tree t = decimal_alloc ();

		for (l = vars; l; l = CB_CHAIN (l)) {
			/* set t, VAR
			 * OP t, d
			 * set VAR, t
			 */
			decimal_expand (t, CB_VALUE (l));
			decimal_compute (op, t, d);
			decimal_assign (CB_VALUE (l), t, CB_PURPOSE (l));
			s1 = cb_list_add (s1, cb_list_reverse (decimal_stack));
			decimal_stack = NULL;
		}
		decimal_free ();
	}

	decimal_free ();
	return s1;
}

void
cb_emit_arithmetic (cb_tree vars, char op, cb_tree val)
{
	cb_tree l;

	val = cb_check_numeric_value (val);
	if (op)
		cb_list_map (cb_check_numeric_name, vars);
	else
		cb_list_map (cb_check_numeric_edited_name, vars);

	VALIDATE (val);
	VALIDATE_LIST (vars);

	if (!CB_BINARY_OP_P (val))
		if (op == '+' || op == '-') {
			for (l = vars; l; l = CB_CHAIN (l)) {
				if (op == '+')
					CB_VALUE (l) = cb_build_add (CB_VALUE (l), val, CB_PURPOSE (l));
				else
					CB_VALUE (l) = cb_build_sub (CB_VALUE (l), val, CB_PURPOSE (l));
			}
			cb_emit_list (vars);
			return;
		}

	cb_emit (build_decimal_assign (vars, op, val));
}

/*
 * Condition
 */

static cb_tree
build_cond_88 (cb_tree x)
{
	struct cb_field *f = cb_field (x);
	cb_tree l;
	cb_tree c1 = NULL;

	/* refer to parent's data storage */
	x = cb_build_field_reference (f->parent, x);
	f->parent->count++;

	/* build condition */
	for (l = f->values; l; l = CB_CHAIN (l)) {
		cb_tree t = CB_VALUE (l);
		cb_tree c2;

		if (CB_PAIR_P (t)) {
			/* VALUE THRU VALUE */
			c2 = cb_build_binary_op (cb_build_binary_op (CB_PAIR_X (t), '[', x),
						 '&', cb_build_binary_op (x, '[', CB_PAIR_Y (t)));
		} else {
			/* VALUE */
			c2 = cb_build_binary_op (x, '=', t);
		}
		if (c1 == NULL)
			c1 = c2;
		else
			c1 = cb_build_binary_op (c1, '|', c2);
	}
	return c1;
}

cb_tree
cb_build_cond (cb_tree x)
{
	int size1, size2;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
	case CB_TAG_FUNCALL:
		return x;
	case CB_TAG_REFERENCE:
	{
		struct cb_field *f;

		if (!CB_FIELD_P (cb_ref (x)))
			return cb_build_cond (cb_ref (x));

		f = cb_field (x);

		/* level 88 condition */
		if (f->level == 88) {
			/* We need to build a 88 condition at every occurrence
			   instead of once at the beginning because a 88 item
			   may be subscripted (i.e., it is not a constant tree). */
			return cb_build_cond (build_cond_88 (x));
		}

		ABORT ();
	}
	case CB_TAG_BINARY_OP:
	{
		struct cb_binary_op *p = CB_BINARY_OP (x);

		switch (p->op) {
		case '!':
			return cb_build_negation (cb_build_cond (p->x));
		case '&':
		case '|':
			return cb_build_binary_op (cb_build_cond (p->x), p->op, cb_build_cond (p->y));
		default:
			if (CB_INDEX_P (p->x) || CB_INDEX_P (p->y)
			    || CB_TREE_CLASS (p->x) == CB_CLASS_POINTER
			    || CB_TREE_CLASS (p->y) == CB_CLASS_POINTER) {
				x = cb_build_binary_op (p->x, '-', p->y);
			} else if (CB_BINARY_OP_P (p->x) || CB_BINARY_OP_P (p->y)) {
				/* decimal comparison */
				cb_tree d1 = decimal_alloc ();
				cb_tree d2 = decimal_alloc ();

				decimal_expand (d1, p->x);
				decimal_expand (d2, p->y);
				dpush (cb_build_funcall_2 ("cob_decimal_cmp", d1, d2));
				decimal_free ();
				decimal_free ();
				x = cb_list_reverse (decimal_stack);
				decimal_stack = NULL;
			} else {
				if (CB_LITERAL_P (p->y)) {
					if (CB_TREE_CLASS (p->x) == CB_CLASS_NUMERIC
					    && CB_TREE_CLASS (p->y) == CB_CLASS_NUMERIC
					    && cb_fits_int (p->y)) {
						x = cb_build_funcall_2 ("cob_cmp_int",
							p->x,
							cb_build_cast_integer (p->y));
						break;
					}
				}

				/* field comparison */
				if ((CB_REFERENCE_P (p->x) || CB_FIELD_P (p->x))
				   && (CB_TREE_CATEGORY (p->x) == CB_CATEGORY_ALPHANUMERIC ||
				       CB_TREE_CATEGORY (p->x) == CB_CATEGORY_ALPHABETIC)
				   && (cb_field_size (p->x) == 1)
				   && (!current_program->alphabet_name_list)
				   && (p->y == cb_space || p->y == cb_low ||
				       p->y == cb_high || p->y == cb_zero) ) {
					x = cb_build_funcall_2 ("$G", p->x, p->y);
					return cb_build_binary_op (x, p->op, p->y);
				}
				if ((CB_LITERAL_P (p->x) || CB_FIELD_P (p->x) ||
				     CB_REFERENCE_P (p->x)) &&
				    (CB_LITERAL_P (p->y) || CB_FIELD_P (p->y) ||
				     CB_REFERENCE_P (p->y))) {
					size1 = cb_field_size (p->x);
					size2 = cb_field_size (p->y);
				} else {
					size1 = 0;
					size2 = 0;
				}
				if (!current_program->alphabet_name_list && size1 == 1 && size2 == 1 &&
				    (CB_TREE_CATEGORY (p->x) == CB_CATEGORY_ALPHANUMERIC ||
				     CB_TREE_CATEGORY (p->x) == CB_CATEGORY_ALPHABETIC) &&
				    (CB_TREE_CATEGORY (p->y) == CB_CATEGORY_ALPHANUMERIC ||
				     CB_TREE_CATEGORY (p->y) == CB_CATEGORY_ALPHABETIC)) {
					x = cb_build_funcall_2 ("$G", p->x, p->y);
				} else {
					x = cb_build_funcall_2 ("cob_cmp", p->x, p->y);
				}
			}
		}
		return cb_build_binary_op (x, p->op, p->y);
	}
	default:
		ABORT ();
	}
/* NOT REACHED */
	return x;
}

/*
 * ADD/SUBTRACT CORRESPONDING
 */

cb_tree
cb_build_add (cb_tree v, cb_tree n, cb_tree round)
{
	cb_tree opt;

	if (CB_INDEX_P (v) || CB_TREE_CLASS (v) == CB_CLASS_POINTER)
		return cb_build_move (cb_build_binary_op (v, '+', n), v);

	opt = build_store_option (v, round);
	if (opt == cb_int0 && cb_fits_int (n))
		return cb_build_funcall_2 ("cob_add_int", v, cb_build_cast_integer (n));
	return cb_build_funcall_3 ("cob_add", v, n, opt);
}

cb_tree
cb_build_sub (cb_tree v, cb_tree n, cb_tree round)
{
	cb_tree opt;

	if (CB_INDEX_P (v) || CB_TREE_CLASS (v) == CB_CLASS_POINTER)
		return cb_build_move (cb_build_binary_op (v, '-', n), v);

	opt = build_store_option (v, round);
	if (opt == cb_int0 && cb_fits_int (n))
		return cb_build_funcall_2 ("cob_sub_int", v, cb_build_cast_integer (n));
	return cb_build_funcall_3 ("cob_sub", v, n, opt);
}

static void
emit_corresponding (cb_tree (*func) (), cb_tree x1, cb_tree x2, cb_tree opt)
{
	struct cb_field *f1, *f2;

	for (f1 = cb_field (x1)->children; f1; f1 = f1->sister)
		if (!f1->redefines && !f1->flag_occurs)
			for (f2 = cb_field (x2)->children; f2; f2 = f2->sister)
				if (!f2->redefines && !f2->flag_occurs)
					if (strcmp (f1->name, f2->name) == 0) {
						cb_tree t1 = cb_build_field_reference (f1, x1);
						cb_tree t2 = cb_build_field_reference (f2, x2);

						if (f1->children && f2->children)
							emit_corresponding (func, t1, t2, opt);
						else
							cb_emit (func (t1, t2, opt));
					}
}

void
cb_emit_corresponding (cb_tree (*func) (), cb_tree x1, cb_tree x2, cb_tree opt)
{
	x1 = cb_check_group_name (x1);
	x2 = cb_check_group_name (x2);

	VALIDATE (x1);
	VALIDATE (x2);

	emit_corresponding (func, x1, x2, opt);
}

/*
 * ACCEPT statement
 */

void
cb_emit_accept (cb_tree var, cb_tree pos)
{
	if (current_program->flag_screen) {
		if (CB_FIELD (var)->storage == CB_STORAGE_SCREEN) {
			cb_tree line = CB_PAIR_X (pos);
			cb_tree column = CB_PAIR_Y (pos);
			cb_emit (cb_build_funcall_3 ("cob_screen_accept", var, line, column));
		} else
			cb_error_x (var, "'%s' not defined in SCREEN SECTION", cb_name (var));
	} else {
		cb_emit (cb_build_funcall_1 ("cob_accept", var));
	}
}

void
cb_emit_accept_date (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_date", var));
}

void
cb_emit_accept_date_yyyymmdd (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_date_yyyymmdd", var));
}

void
cb_emit_accept_day (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_day", var));
}

void
cb_emit_accept_day_yyyyddd (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_day_yyyyddd", var));
}

void
cb_emit_accept_day_of_week (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_day_of_week", var));
}

void
cb_emit_accept_time (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_time", var));
}

void
cb_emit_accept_command_line (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_command_line", var));
}

void
cb_emit_accept_environment (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_environment", var));
}

void
cb_emit_accept_arg_number (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_arg_number", var));
}

void
cb_emit_accept_arg_value (cb_tree var)
{
	cb_emit (cb_build_funcall_1 ("cob_accept_arg_value", var));
}

void
cb_emit_accept_mnemonic (cb_tree var, cb_tree mnemonic)
{
	switch (CB_SYSTEM_NAME (cb_ref (mnemonic))->token) {
	case CB_DEVICE_CONSOLE:
	case CB_DEVICE_SYSIN:
		cb_emit (cb_build_funcall_1 ("cob_accept", var));
		break;
	default:
		cb_error_x (mnemonic, _("invalid input stream '%s'"), cb_name (mnemonic));
		break;
	}
}

void
cb_emit_accept_name (cb_tree var, cb_tree name)
{
	if (CB_REFERENCE (name)->word->count == 0) {
		cb_tree sys = lookup_system_name (CB_NAME (name));

		if (sys != cb_error_node) {
			switch (CB_SYSTEM_NAME (sys)->token) {
			case CB_DEVICE_CONSOLE:
			case CB_DEVICE_SYSIN:
				cb_warning_x (name, _("'%s' undefined in SPECIAL-NAMES"), CB_NAME (name));
				cb_emit (cb_build_funcall_1 ("cob_accept", var));
				return;
			default:
				break;
			}
		}
	}

	cb_error_x (name, _("'%s' undefined in SPECIAL-NAMES"), CB_NAME (name));
}

/*
 * CALL statement
 */

void
cb_emit_call (cb_tree prog, cb_tree using, cb_tree returning,
	      cb_tree on_exception, cb_tree not_on_exception)
{
	cb_emit (cb_build_call (prog, using, on_exception, not_on_exception));
	if (returning)
		cb_emit (cb_build_move (cb_return_code, returning));
}

/*
 * CANCEL statement
 */

void
cb_emit_cancel (cb_tree prog)
{
	if (cb_flag_static_call == 2) {
		return;
	}
	cb_emit (cb_build_funcall_1 ("cob_cancel", prog));
}

/*
 * CLOSE statement
 */

void
cb_emit_close (cb_tree file, cb_tree opt)
{
	if ( file != cb_error_node ) {
		file = cb_ref (file);
		current_statement->file = file;
		cb_emit (cb_build_funcall_2 ("cob_close", file, opt));
	}
}

/*
 * DELETE statement
 */

void
cb_emit_delete (cb_tree file)
{
	if ( file != cb_error_node ) {
		file = cb_ref (file);
		current_statement->file = file;
		cb_emit (cb_build_funcall_1 ("cob_delete", file));
	}
}

/*
 * DISPLAY statement
 */

void
cb_emit_display (cb_tree values, cb_tree upon, cb_tree no_adv, cb_tree pos)
{
	cb_tree l;

	if (upon == cb_true) {
		/* DISPLAY x UPON ENVIRONMENT-NAME */
		if (cb_list_length (values) != 1) {
			cb_error (_("wrong number of data items"));
			return;
		}
		cb_emit (cb_build_funcall_1 ("cob_display_environment", CB_VALUE (values)));
	} else if (upon == cb_int3) {
		/* DISPLAY x UPON ENVIRONMENT-VALUE */
		if (cb_list_length (values) != 1) {
			cb_error (_("wrong number of data items"));
			return;
		}
		cb_emit (cb_build_funcall_1 ("cob_display_env_value", CB_VALUE (values)));
	} else if (upon == cb_int4) {
		/* DISPLAY x UPON ARGUMENT-NUMBER */
		if (cb_list_length (values) != 1) {
			cb_error (_("wrong number of data items"));
			return;
		}
		cb_emit (cb_build_funcall_1 ("cob_display_arg_number", CB_VALUE (values)));
	} else if (current_program->flag_screen) {
		/* screen mode */
		for (l = values; l; l = CB_CHAIN (l)) {
			cb_tree x = CB_VALUE (l);

			if (CB_FIELD_P (cb_ref (x))
			    && CB_FIELD (cb_ref (x))->storage == CB_STORAGE_SCREEN) {
				cb_tree line = CB_PAIR_X (pos);
				cb_tree column = CB_PAIR_Y (pos);
				cb_emit (cb_build_funcall_3 ("cob_screen_display", x, line, column));
			} else {
				cb_error_x (x, "'%s' not defined in SCREEN SECTION", cb_name (x));
			}
		}
	} else {
		/* DISPLAY x ... [UPON device-name] */

		/* Possible implementation using varargs */
/* Start comment out
	cb_tree	p, outorerr, newline;

	if ( upon == cb_int1 ) {
		outorerr = cb_int0;
	} else {
		outorerr = cb_int1;
	}
	if ( no_adv == cb_int0 ) {
		newline = cb_int1;
	} else {
		newline = cb_int0;
	}
	p = cb_build_funcall_3 ("cob_new_display", outorerr, newline, values);
	CB_FUNCALL(p)->varcnt = cb_list_length (values);
	cb_emit(p);
End comment out */
		/* End possible implementation */

		/* Original multiple call implementation */
		int is_stdout = (upon == cb_int1);
		const char *display = is_stdout ? "cob_display" : "cob_display_error";
		const char *newline = is_stdout ? "cob_newline" : "cob_newline_error";

		for (l = values; l; l = CB_CHAIN (l)) {
			cb_emit (cb_build_funcall_1 (display, CB_VALUE (l)));
			if (CB_FIELD_P (l)) {
				CB_FIELD (l)->count++;
			}
		}
		if (no_adv == cb_int0)
			cb_emit (cb_build_funcall_0 (newline));
		/* End Original multiple call implementation */
	}
}

cb_tree
cb_build_display_upon (cb_tree x)
{
	if (x == cb_error_node)
		return cb_error_node;

	switch (CB_SYSTEM_NAME (cb_ref (x))->token) {
	case CB_DEVICE_CONSOLE:
	case CB_DEVICE_SYSOUT:
		return cb_int1;
	case CB_DEVICE_SYSERR:
		return cb_int2;
	default:
		cb_error_x (x, _("invalid output stream"));
		return cb_error_node;
	}
}

cb_tree
cb_build_display_upon_direct (cb_tree x)
{
	const char *name = CB_NAME (x);

	if (CB_REFERENCE (x)->word->count == 0) {
		cb_tree sys = lookup_system_name (CB_NAME (x));

		if (sys != cb_error_node) {
			switch (CB_SYSTEM_NAME (sys)->token) {
			case CB_DEVICE_CONSOLE:
			case CB_DEVICE_SYSOUT:
				cb_warning_x (x, _("'%s' undefined in SPECIAL-NAMES"), name);
				return cb_int1;
			case CB_DEVICE_SYSERR:
				cb_warning_x (x, _("'%s' undefined in SPECIAL-NAMES"), name);
				return cb_int2;
			default:
				break;
			}
		}
	}

	cb_error_x (x, _("'%s' undefined in SPECIAL-NAMES"), name);
	return cb_error_node;
}

/*
 * DIVIDE statement
 */

void
cb_emit_divide (cb_tree dividend, cb_tree divisor, cb_tree quotient, cb_tree remainder)
{
	CB_VALUE (quotient) = cb_check_numeric_edited_name (CB_VALUE (quotient));
	CB_VALUE (remainder) = cb_check_numeric_edited_name (CB_VALUE (remainder));

	VALIDATE (CB_VALUE (quotient));
	VALIDATE (CB_VALUE (remainder));

	cb_emit (cb_build_funcall_4 ("cob_div_quotient", dividend, divisor,
				     CB_VALUE (quotient),
				     build_store_option (CB_VALUE (quotient), CB_PURPOSE (quotient))));
	cb_emit (cb_build_funcall_2 ("cob_div_remainder", CB_VALUE (remainder),
				     build_store_option (CB_VALUE (remainder), cb_int0)));
}

/*
 * EVALUATE statement
 */

static cb_tree
evaluate_test (cb_tree s, cb_tree o)
{
	int flag;
	cb_tree x, y;

	/* ANY is always true */
	if (o == cb_any)
		return cb_true;

	/* object TRUE or FALSE */
	if (o == cb_true)
		return s;
	if (o == cb_false)
		return cb_build_negation (s);

	flag = CB_PURPOSE_INT (o);
	x = CB_PAIR_X (CB_VALUE (o));
	y = CB_PAIR_Y (CB_VALUE (o));

	/* subject TRUE or FALSE */
	if (s == cb_true)
		return flag ? cb_build_negation (x) : x;
	if (s == cb_false)
		return flag ? x : cb_build_negation (x);

	/* x THRU y */
	if (y) {
		cb_tree t = cb_build_binary_op (cb_build_binary_op (x, '[', s),
						'&',
						cb_build_binary_op (s, '[', y));

		return flag ? cb_build_negation (t) : t;
	}

	/* regular comparison */
	if (flag)
		return cb_build_binary_op (s, '~', x);
	else
		return cb_build_binary_op (s, '=', x);
}

static cb_tree
build_evaluate (cb_tree subject_list, cb_tree case_list)
{
	cb_tree stmt;
	cb_tree c1 = NULL;
	cb_tree subjs, whens, objs;

	if (case_list == NULL)
		return NULL;

	whens = CB_VALUE (case_list);
	stmt = CB_VALUE (whens);
	whens = CB_CHAIN (whens);

	/* for each WHEN sequence */
	for (; whens; whens = CB_CHAIN (whens)) {
		cb_tree c2 = NULL;

		/* single WHEN test */
		for (subjs = subject_list, objs = CB_VALUE (whens);
		     subjs && objs; subjs = CB_CHAIN (subjs), objs = CB_CHAIN (objs)) {
			cb_tree c3 = evaluate_test (CB_VALUE (subjs), CB_VALUE (objs));

			if (c2 == NULL)
				c2 = c3;
			else
				c2 = cb_build_binary_op (c2, '&', c3);
		}
		if (subjs || objs)
			cb_error (_("wrong number of WHEN parameters"));
		/* connect multiple WHEN's */
		if (c1 == NULL)
			c1 = c2;
		else
			c1 = cb_build_binary_op (c1, '|', c2);
	}

	if (c1 == NULL)
		return stmt;
	else
		return cb_build_if (cb_build_cond (c1), stmt,
				    build_evaluate (subject_list, CB_CHAIN (case_list)));
}

void
cb_emit_evaluate (cb_tree subject_list, cb_tree case_list)
{
	cb_emit (build_evaluate (subject_list, case_list));
}

/*
 * GO TO statement
 */

void
cb_emit_goto (cb_tree target, cb_tree depending)
{
	if (depending) {
		/* GO TO procedure-name ... DEPENDING ON identifier */
		cb_emit (cb_build_goto (target, depending));
	} else {
		/* GO TO procedure-name */
		if (target == NULL)
			cb_verify (cb_goto_statement_without_name, "GO TO without procedure-name");
		else if (CB_CHAIN (target))
			cb_error (_("GO TO with multiple procedure-names"));
		else
			cb_emit (cb_build_goto (CB_VALUE (target), 0));
	}
}

void
cb_emit_exit (void)
{
	cb_emit (cb_build_goto (0, 0));
}

/*
 * IF statement
 */

void
cb_emit_if (cb_tree cond, cb_tree stmt1, cb_tree stmt2)
{
	cb_emit (cb_build_if (cond, stmt1, stmt2));
}

/*
 * INITIALIZE statement
 */

void
cb_emit_initialize (cb_tree vars, cb_tree fillinit, cb_tree value, cb_tree replacing, cb_tree def)
{
	cb_tree l;
	int fill_init = 1;

	if (value == NULL && replacing == NULL)
		def = cb_true;
	if (fillinit == cb_true)
		fill_init = 0;
	for (l = vars; l; l = CB_CHAIN (l))
		cb_emit (cb_build_initialize (CB_VALUE (l), value, replacing, def, fill_init));
}

/*
 * INSPECT statement
 */

static const char *inspect_func;
static cb_tree inspect_data;

void
cb_emit_inspect (cb_tree var, cb_tree body, cb_tree replacing)
{
	cb_emit (cb_build_funcall_2 ("cob_inspect_init", var, replacing));
	cb_emit_list (body);
	cb_emit (cb_build_funcall_0 ("cob_inspect_finish"));
}

void
cb_init_tarrying (void)
{
	inspect_func = NULL;
	inspect_data = NULL;
}

cb_tree
cb_build_tarrying_data (cb_tree x)
{
	inspect_data = x;
	return NULL;
}

cb_tree
cb_build_tarrying_characters (cb_tree l)
{
	if (inspect_data == NULL)
		cb_error (_("data name expected before CHARACTERS"));
	inspect_func = NULL;
	return cb_list_add (l, cb_build_funcall_1 ("cob_inspect_characters", inspect_data));
}

cb_tree
cb_build_tarrying_all (void)
{
	if (inspect_data == NULL)
		cb_error (_("data name expected before ALL"));
	inspect_func = "cob_inspect_all";
	return NULL;
}

cb_tree
cb_build_tarrying_leading (void)
{
	if (inspect_data == NULL)
		cb_error (_("data name expected before LEADING"));
	inspect_func = "cob_inspect_leading";
	return NULL;
}

cb_tree
cb_build_tarrying_value (cb_tree x, cb_tree l)
{
	if (inspect_func == NULL)
		cb_error_x (x, _("ALL or LEADING expected before '%s'"), cb_name (x));
	return cb_list_add (l, cb_build_funcall_2 (inspect_func, inspect_data, x));
}

cb_tree
cb_build_replacing_characters (cb_tree x, cb_tree l)
{
	return cb_list_add (l, cb_build_funcall_1 ("cob_inspect_characters", x));
}

cb_tree
cb_build_replacing_all (cb_tree x, cb_tree y, cb_tree l)
{
	return cb_list_add (l, cb_build_funcall_2 ("cob_inspect_all", y, x));
}

cb_tree
cb_build_replacing_leading (cb_tree x, cb_tree y, cb_tree l)
{
	return cb_list_add (l, cb_build_funcall_2 ("cob_inspect_leading", y, x));
}

cb_tree
cb_build_replacing_first (cb_tree x, cb_tree y, cb_tree l)
{
	return cb_list_add (l, cb_build_funcall_2 ("cob_inspect_first", y, x));
}

cb_tree
cb_build_converting (cb_tree x, cb_tree y, cb_tree l)
{
	return cb_list_add (l, cb_build_funcall_2 ("cob_inspect_converting", x, y));
}

cb_tree
cb_build_inspect_region_start (void)
{
	return cb_list (cb_build_funcall_0 ("cob_inspect_start"));
}

cb_tree
cb_build_inspect_region (cb_tree l, cb_tree pos, cb_tree x)
{
	if (pos == CB_BEFORE)
		return cb_list_add (l, cb_build_funcall_1 ("cob_inspect_before", x));
	else
		return cb_list_add (l, cb_build_funcall_1 ("cob_inspect_after", x));
}

/*
 * MOVE statement
 */

static void
warning_destination (cb_tree x)
{
	struct cb_reference *r = CB_REFERENCE (x);
	struct cb_field *f = CB_FIELD (r->value);
	cb_tree loc = CB_TREE (f);

	if (r->offset)
		return;

	if (f->pic)
		cb_warning_x (loc, _("'%s' defined here as PIC %s"), f->name, f->pic->orig);
	else
		cb_warning_x (loc, _("'%s' defined here as a group of length %d"), f->name, f->size);
}

static int
move_error (cb_tree src, cb_tree dst, int value_flag, int flag, const char *msg)
{
	cb_tree loc = src->source_line ? src : dst;

	if (value_flag) {
		/* VALUE clause */
		cb_warning_x (loc, msg);
	} else {
		/* MOVE statement */
		if (flag) {
			cb_warning_x (loc, msg);
			warning_destination (dst);
		}
	}

	return 0;
}

int
validate_move (cb_tree src, cb_tree dst, int is_value)
{
	struct cb_field *f;
	cb_tree loc = src->source_line ? src : dst;

	if (CB_TREE_CLASS (dst) == CB_CLASS_POINTER) {
		if (CB_TREE_CLASS (src) == CB_CLASS_POINTER)
			return 0;
		else
			goto invalid;
	}

	f = cb_field (dst);
	switch (CB_TREE_TAG (src)) {
	case CB_TAG_CONST:
	{
		if (src == cb_space) {
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
			    || (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC_EDITED && !is_value))
				goto invalid;
		} else if (src == cb_zero) {
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC)
				goto invalid;
		}
		break;
	}
	case CB_TAG_LITERAL:
	{
		struct cb_literal *l = CB_LITERAL (src);

		/* TODO: ALL literal */

		if (CB_TREE_CLASS (src) == CB_CLASS_NUMERIC) {
			/* Numeric literal */
			int i;
			int most_significant = -999;
			int least_significant = 999;

			/* compute the most significant figure place */
			for (i = 0; i < l->size; i++)
				if (l->data[i] != '0')
					break;
			if (i != l->size)
				most_significant = l->size - l->scale - i - 1;

			/* compute the least significant figure place */
			for (i = 0; i < l->size; i++)
				if (l->data[l->size - i - 1] != '0')
					break;
			if (i != l->size)
				least_significant = -l->scale + i;

			/* value check */
			switch (CB_TREE_CATEGORY (dst)) {
			case CB_CATEGORY_ALPHANUMERIC:
			case CB_CATEGORY_ALPHANUMERIC_EDITED:
			{
				if (is_value)
					goto expect_alphanumeric;

				if (l->scale == 0)
					goto expect_alphanumeric;
				else
					goto invalid;
			}
			case CB_CATEGORY_NUMERIC:
			{
				if (f->pic->scale < 0) {
					/* check for PIC 9(n)P(m) */
					if (least_significant < -f->pic->scale)
						goto value_mismatch;
				} else if (f->pic->scale > f->pic->size) {
					/* check for PIC P(n)9(m) */
					if (most_significant >= f->pic->size - f->pic->scale)
						goto value_mismatch;
				}
				break;
			}
			case CB_CATEGORY_NUMERIC_EDITED:
			{
				if (is_value)
					goto expect_alphanumeric;

				/* TODO */
				break;
			}
			default:
				if (is_value)
					goto expect_alphanumeric;
				goto invalid;
			}

			/* sign check */
			if (l->sign != 0 && !f->pic->have_sign) {
				if (is_value) {
					cb_error_x (loc, _("data item not signed"));
					return -1;
				}
				if (cb_warn_constant) {
					cb_warning_x (loc, _("ignoring negative sign"));
				}
			}

			/* size check */
			if (least_significant < -f->pic->scale)
				goto size_overflow;
			if (most_significant >= f->pic->digits - (f->pic->scale > 0 ? f->pic->scale : 0))
				goto size_overflow;
		} else {
			/* Alphanumeric literal */

			/* value check */
			switch (CB_TREE_CATEGORY (dst)) {
			case CB_CATEGORY_ALPHABETIC:
			{
				int i;

				for (i = 0; i < l->size; i++)
					if (!isalpha (l->data[i]) && !isspace (l->data[i]))
						goto value_mismatch;
				break;
			}
			case CB_CATEGORY_NUMERIC:
				goto expect_numeric;
			case CB_CATEGORY_NUMERIC_EDITED:
				if (!is_value)
					goto expect_numeric;

				/* TODO: validate the value */
				break;
			default:
				break;
			}

			/* size check */
			{
				int size = cb_field_size (dst);

				if (size >= 0 && l->size > size)
					goto size_overflow;
			}
		}
		break;
	}
	case CB_TAG_FIELD:
	case CB_TAG_REFERENCE:
	{
		/* non-elementary move */
		if (cb_field (src)->children || cb_field (dst)->children)
			break;

		/* elementary move */
		switch (CB_TREE_CATEGORY (src)) {
		case CB_CATEGORY_ALPHANUMERIC:
			break;
		case CB_CATEGORY_ALPHABETIC:
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
			switch (CB_TREE_CATEGORY (dst)) {
			case CB_CATEGORY_NUMERIC:
			case CB_CATEGORY_NUMERIC_EDITED:
				goto invalid;
			default:
				break;
			}
			break;
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
			switch (CB_TREE_CATEGORY (dst)) {
			case CB_CATEGORY_ALPHABETIC:
				goto invalid;
			case CB_CATEGORY_ALPHANUMERIC:
			case CB_CATEGORY_ALPHANUMERIC_EDITED:
				if (CB_TREE_CATEGORY (src) == CB_CATEGORY_NUMERIC
				    && cb_field (src)->pic->scale > 0) {
					if (cb_move_noninteger_to_alphanumeric == CB_ERROR)
						goto invalid;
					cb_warning_x (loc, _("move non-integer to alphanumeric"));
					break;
				}
			default:
				break;
			}
			break;
		default:
			ABORT ();
		}
		break;
	}
	case CB_TAG_INTEGER:
	case CB_TAG_BINARY_OP:
	case CB_TAG_INTRINSIC:
		/* TODO: check this */
		break;
	default:
		fprintf (stderr, "Invalid tree tag %d\n", CB_TREE_TAG (src));
		fflush (stdout);
		ABORT ();
	}
	return 0;

      invalid:
	if (is_value)
		cb_error_x (loc, _("invalid VALUE clause"));
	else
		cb_error_x (loc, _("invalid MOVE statement"));
	return -1;

      expect_numeric:
	return move_error (src, dst, is_value, cb_warn_strict_typing, _("numeric value is expected"));

      expect_alphanumeric:
	return move_error (src, dst, is_value, cb_warn_strict_typing,
			   _("alphanumeric value is expected"));

      value_mismatch:
	return move_error (src, dst, is_value, cb_warn_constant,
			   _("value does not fit the picture string"));

      size_overflow:
	return move_error (src, dst, is_value, cb_warn_constant, _("value size exceeds data size"));
}

static cb_tree
cb_build_memset (cb_tree x, char c)
{
	int size = cb_field_size (x);

	if (size == 1)
		return cb_build_funcall_2 ("$E", x, cb_int (c));
	else
		return cb_build_funcall_3 ("own_memset",
					   cb_build_cast_address (x),
					   cb_int (c), cb_build_cast_length (x));
}

static cb_tree
cb_build_move_copy (cb_tree src, cb_tree dst)
{
	int size = cb_field_size (dst);

	if (size == 1)
		return cb_build_funcall_2 ("$F", dst, src);
	else
		return cb_build_funcall_3 ("own_memcpy",
					   cb_build_cast_address (dst),
					   cb_build_cast_address (src), cb_build_cast_length (dst));
}

static cb_tree
cb_build_move_call (cb_tree src, cb_tree dst)
{
	return cb_build_funcall_2 ("cob_move", src, dst);
}

static cb_tree
cb_build_move_num_zero (cb_tree x)
{
	switch (cb_field (x)->usage) {
	case CB_USAGE_BINARY:
	case CB_USAGE_COMP_5:
	case CB_USAGE_COMP_X:
		return cb_build_assign (x, cb_int (0));
	case CB_USAGE_DISPLAY:
		return cb_build_memset (x, '0');
	case CB_USAGE_PACKED:
		return cb_build_memset (x, 0x00);
	default:
		ABORT ();
	}
/* NOT REACHED */
	return x;
}

static cb_tree
cb_build_move_space (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return cb_build_memset (x, ' ');
	default:
		return cb_build_move_call (cb_space, x);
	}
}

static cb_tree
cb_build_move_zero (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
		if (cb_field (x)->flag_blank_zero)
			return cb_build_move_space (x);
		else
			return cb_build_move_num_zero (x);
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return cb_build_memset (x, '0');
	default:
		return cb_build_move_call (cb_zero, x);
	}
}

static cb_tree
cb_build_move_high (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return cb_build_memset (x, 255);
	default:
		return cb_build_move_call (cb_high, x);
	}
}

static cb_tree
cb_build_move_low (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return cb_build_memset (x, 0);
	default:
		return cb_build_move_call (cb_low, x);
	}
}

static cb_tree
cb_build_move_quote (cb_tree x)
{
	switch (CB_TREE_CATEGORY (x)) {
	case CB_CATEGORY_NUMERIC:
	case CB_CATEGORY_ALPHABETIC:
	case CB_CATEGORY_ALPHANUMERIC:
		return cb_build_memset (x, '"');
	default:
		return cb_build_move_call (cb_quote, x);
	}
}

static cb_tree
cb_build_move_literal (cb_tree src, cb_tree dst)
{
	struct cb_literal *l = CB_LITERAL (src);
	struct cb_field *f = cb_field (dst);
	enum cb_category cat = CB_TREE_CATEGORY (dst);

	if (l->all) {
		int i;
		unsigned char *buff = cob_malloc (f->size);

		for (i = 0; i < f->size; i++)
			buff[i] = l->data[i % l->size];
		return cb_build_funcall_3 ("own_memcpy",
					   cb_build_cast_address (dst),
					   cb_build_string (buff, f->size), cb_build_cast_length (dst));
	} else if ((cat == CB_CATEGORY_NUMERIC
		    && f->usage == CB_USAGE_DISPLAY
		    && f->pic->scale == l->scale && !f->flag_sign_leading && !f->flag_sign_separate)
		   || ((cat == CB_CATEGORY_ALPHABETIC || cat == CB_CATEGORY_ALPHANUMERIC)
		       && f->size < l->size + 16 && !cb_field_variable_size (f))) {
		unsigned char *buff = cob_malloc (f->size);
		int diff = f->size - l->size;

		if (cat == CB_CATEGORY_NUMERIC) {
			if (diff <= 0) {
				memcpy (buff, l->data - diff, f->size);
			} else {
				memset (buff, '0', diff);
				memcpy (buff + diff, l->data, l->size);
			}
			if (l->sign < 0) {
				unsigned char *p = &buff[f->size - 1];

				switch (cb_display_sign) {
				case COB_DISPLAY_SIGN_ASCII:
					*p += 0x40;
					break;
				case COB_DISPLAY_SIGN_ASCII10:
					*p += 0x10;
					break;
				default:
					ABORT ();
				}
			}
		} else {
			if (f->flag_justified) {
				if (diff <= 0) {
					memcpy (buff, l->data - diff, f->size);
				} else {
					memset (buff, ' ', diff);
					memcpy (buff + diff, l->data, l->size);
				}
			} else {
				if (diff <= 0) {
					memcpy (buff, l->data, f->size);
				} else {
					memcpy (buff, l->data, l->size);
					memset (buff + l->size, ' ', diff);
				}
			}
		}
		return cb_build_funcall_3 ("own_memcpy",
					   cb_build_cast_address (dst),
					   cb_build_string (buff, f->size), cb_build_cast_length (dst));
	} else if (cb_fits_int (src) &&
		   (f->usage == CB_USAGE_BINARY || f->usage == CB_USAGE_COMP_5 ||
		    f->usage == CB_USAGE_COMP_X)) {
		int val = cb_get_int (src);
		int n = f->pic->scale - l->scale;

		for (; n > 0; n--)
			val *= 10;
		for (; n < 0; n++)
			val /= 10;
		return cb_build_assign (dst, cb_int (val));
	} else {
		return cb_build_move_call (src, dst);
	}
}

static cb_tree
cb_build_move_field (cb_tree src, cb_tree dst)
{
	int src_size = cb_field_size (src);
	int dst_size = cb_field_size (dst);
	struct cb_field *src_f = cb_field (src);
	struct cb_field *dst_f = cb_field (dst);

	if ((src_size > 0 && dst_size > 0 && src_size >= dst_size)
	    && (!cb_field_variable_size (src_f) && !cb_field_variable_size (dst_f)))
		switch (CB_TREE_CATEGORY (src)) {
		case CB_CATEGORY_ALPHABETIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHABETIC
			    || CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
				if (dst_f->flag_justified == 0)
					return cb_build_move_copy (src, dst);
			break;
		case CB_CATEGORY_ALPHANUMERIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC)
				if (dst_f->flag_justified == 0)
					return cb_build_move_copy (src, dst);
			break;
		case CB_CATEGORY_NUMERIC:
			if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_NUMERIC
			    && src_f->usage == dst_f->usage
			    && src_f->pic->size == dst_f->pic->size
			    && src_f->pic->digits == dst_f->pic->digits
			    && src_f->pic->scale == dst_f->pic->scale
			    && src_f->pic->have_sign == dst_f->pic->have_sign
			    && src_f->flag_binary_swap == dst_f->flag_binary_swap
			    && src_f->flag_sign_leading == dst_f->flag_sign_leading
			    && src_f->flag_sign_separate == dst_f->flag_sign_separate)
				return cb_build_move_copy (src, dst);
			else if (CB_TREE_CATEGORY (dst) == CB_CATEGORY_ALPHANUMERIC
				 && src_f->pic->have_sign == 0)
				return cb_build_move_copy (src, dst);
			break;
		default:
			break;
		}

	return cb_build_move_call (src, dst);
}

cb_tree
cb_build_move (cb_tree src, cb_tree dst)
{
	struct cb_field *f;

	if (src == cb_error_node || dst == cb_error_node)
		return cb_error_node;

	if (CB_TREE_CLASS (src) == CB_CLASS_POINTER && CB_REFERENCE_P (src)
	    && cb_field (src)->level > 50)
		cb_error_x (src, _("invalid MOVE statement"));
	if (CB_TREE_CLASS (dst) == CB_CLASS_POINTER && CB_REFERENCE_P (dst)
	    && cb_field (dst)->level > 50)
		cb_error_x (dst, _("invalid MOVE statement"));

	validate_move (src, dst, 0);

	if (CB_REFERENCE_P (src))
		CB_REFERENCE (src)->type = CB_SENDING_OPERAND;
	if (CB_REFERENCE_P (dst))
		CB_REFERENCE (dst)->type = CB_RECEIVING_OPERAND;

	if (CB_TREE_CLASS (dst) == CB_CLASS_POINTER)
		return cb_build_assign (dst, src);

	if (CB_INDEX_P (dst))
		return cb_build_assign (dst, src);

	if (CB_INDEX_P (src))
		return cb_build_funcall_2 ("cob_set_int", dst, cb_build_cast_integer (src));

	if (CB_INTRINSIC_P (src) || CB_INTRINSIC_P (dst))
		return cb_build_move_call (src, dst);

	f = cb_field (dst);

	/* no optimization for binary swap and packed decimal for now */
	if (f->flag_binary_swap
	    || f->usage == CB_USAGE_PACKED
	    || f->usage == CB_USAGE_FLOAT
	    || f->usage == CB_USAGE_DOUBLE
	    || ((f->usage == CB_USAGE_BINARY ||
		 f->usage == CB_USAGE_COMP_5 || f->usage == CB_USAGE_COMP_X)
		&& (f->size == 3 || f->size == 5 || f->size == 6 || f->size == 7)))
		return cb_build_move_call (src, dst);

	/* output optimal code */
	if (src == cb_zero)
		return cb_build_move_zero (dst);
	else if (src == cb_space)
		return cb_build_move_space (dst);
	else if (src == cb_high)
		return cb_build_move_high (dst);
	else if (src == cb_low)
		return cb_build_move_low (dst);
	else if (src == cb_quote)
		return cb_build_move_quote (dst);
	else if (CB_LITERAL_P (src))
		return cb_build_move_literal (src, dst);

	return cb_build_move_field (src, dst);
}

void
cb_emit_move (cb_tree src, cb_tree dsts)
{
	cb_tree l;

	VALIDATE (src);
	VALIDATE_LIST (dsts);

	for (l = dsts; l; l = CB_CHAIN (l))
		cb_emit (cb_build_move (src, CB_VALUE (l)));
}

/*
 * OPEN statement
 */

void
cb_emit_open (cb_tree file, cb_tree mode, cb_tree sharing)
{
	if (file != cb_error_node) {
		file = cb_ref (file);
		current_statement->file = file;

		if (sharing == NULL)
			sharing = CB_FILE (file)->sharing ? CB_FILE (file)->sharing : cb_int0;

		if (sharing == cb_int0	/* READ ONLY */
		    && CB_INTEGER (mode)->val != COB_OPEN_INPUT)
			sharing = cb_int1;

		cb_emit (cb_build_funcall_3 ("cob_open", file, mode, sharing));
	}
}

/*
 * PERFORM statement
 */

void
cb_emit_perform (cb_tree perform, cb_tree body)
{
	CB_PERFORM (perform)->body = body;
	cb_emit (perform);
}

cb_tree
cb_build_perform_once (cb_tree body)
{
	cb_tree x = cb_build_perform (CB_PERFORM_ONCE);

	CB_PERFORM (x)->body = body;
	return x;
}

cb_tree
cb_build_perform_times (cb_tree times)
{
	cb_tree x;

	if (cb_check_integer_value (times) == cb_error_node)
		return cb_error_node;

	x = cb_build_perform (CB_PERFORM_TIMES);
	CB_PERFORM (x)->data = times;
	return x;
}

cb_tree
cb_build_perform_until (cb_tree condition, cb_tree varying)
{
	cb_tree x = cb_build_perform (CB_PERFORM_UNTIL);

	CB_PERFORM (x)->test = condition;
	CB_PERFORM (x)->varying = varying;
	return x;
}

cb_tree
cb_build_perform_exit (struct cb_label * label)
{
	cb_tree x = cb_build_perform (CB_PERFORM_EXIT);

	CB_PERFORM (x)->data = CB_TREE (label);
	return x;
}

/*
 * READ statement
 */

void
cb_emit_read (cb_tree ref, cb_tree next, cb_tree into, cb_tree key)
{
	if (ref != cb_error_node) {
		cb_tree file = cb_ref (ref);
		cb_tree rec = cb_build_field_reference (CB_FILE (file)->record, ref);

		if (next == cb_int1 || CB_FILE (file)->access_mode == COB_ACCESS_SEQUENTIAL) {
			/* READ NEXT */
			if (key)
				cb_warning (_("KEY ignored with sequential READ"));
			cb_emit (cb_build_funcall_2 ("cob_read", file, cb_int0));
		} else {
			/* READ */
			cb_emit (cb_build_funcall_2 ("cob_read", file, key ? key : CB_FILE (file)->key));
		}
		if (into)
			cb_emit (cb_build_move (rec, into));
		current_statement->file = file;
	}
}

/*
 * REWRITE statement
 */

void
cb_emit_rewrite (cb_tree record, cb_tree from)
{
	if (record != cb_error_node) {
		cb_tree file = CB_TREE (CB_FIELD (cb_ref (record))->file);

		current_statement->file = file;
		if (from)
			cb_emit (cb_build_move (from, record));
		cb_emit (cb_build_funcall_2 ("cob_rewrite", file, record));
	}
}

/*
 * RETURN statement
 */

void
cb_emit_return (cb_tree ref, cb_tree into)
{
	cb_tree file = cb_ref (ref);
	cb_tree rec = cb_build_field_reference (CB_FILE (file)->record, ref);

	cb_emit (cb_build_funcall_2 ("cob_read", file, cb_int0));
	if (into)
		cb_emit (cb_build_move (rec, into));
	current_statement->file = file;
}

/*
 * SEARCH statement
 */

static void
search_set_keys (struct cb_field *f, cb_tree x)
{
	struct cb_binary_op *p;

	if (CB_REFERENCE_P (x))
		x = build_cond_88 (x);

	p = CB_BINARY_OP (x);
	switch (p->op) {
	case '&':
		search_set_keys (f, p->x);
		search_set_keys (f, p->y);
		break;
	case '=':
	{
		int i;

		for (i = 0; i < f->nkeys; i++)
			if (cb_field (p->x) == cb_field (f->keys[i].key)) {
				f->keys[i].ref = p->x;
				f->keys[i].val = p->y;
				break;
			}
		if (i == f->nkeys)
			cb_error_x (x, _("undeclared key '%s'"), cb_field (p->x)->name);
		break;
	}
	default:
		cb_error_x (x, _("invalid SEARCH ALL condition"));
		break;
	}
}

static cb_tree
cb_build_search_all (cb_tree table, cb_tree cond)
{
	int i;
	struct cb_field *f = cb_field (table);
	cb_tree c1 = NULL;

	/* set keys */
	for (i = 0; i < f->nkeys; i++)
		f->keys[i].ref = 0;
	search_set_keys (f, cond);

	/* build condition */
	for (i = 0; i < f->nkeys; i++)
		if (f->keys[i].ref) {
			cb_tree c2;

			if (f->keys[i].dir == COB_ASCENDING)
				c2 = cb_build_binary_op (f->keys[i].ref, '=', f->keys[i].val);
			else
				c2 = cb_build_binary_op (f->keys[i].val, '=', f->keys[i].ref);
			if (c1 == NULL)
				c1 = c2;
			else
				c1 = cb_build_binary_op (c1, '&', c2);
		}

	return cb_build_cond (c1);
}

void
cb_emit_search (cb_tree table, cb_tree varying, cb_tree at_end, cb_tree whens)
{
	cb_emit (cb_build_search (0, table, varying, at_end, whens));
}

void
cb_emit_search_all (cb_tree table, cb_tree at_end, cb_tree when, cb_tree stmts)
{
	cb_emit (cb_build_search (1, table, NULL, at_end,
				  cb_build_if (cb_build_search_all (table, when), stmts, NULL)));
}

/*
 * SET statement
 */

void
cb_emit_set_to (cb_tree vars, cb_tree x)
{
	cb_tree l;
#if 0
	enum cb_class class = CB_CLASS_UNKNOWN;
#endif

	VALIDATE (x);
	VALIDATE_LIST (vars);

#if 0
	/* determine the class of targets */
	for (l = vars; l; l = CB_CHAIN (l))
		if (CB_TREE_CLASS (CB_VALUE (l)) != CB_CLASS_UNKNOWN) {
			if (class == CB_CLASS_UNKNOWN)
				class = CB_TREE_CLASS (CB_VALUE (l));
			else if (class != CB_TREE_CLASS (CB_VALUE (l)))
				break;
		}
	if (l || (class != CB_CLASS_INDEX && class != CB_CLASS_POINTER)) {
		cb_error_x (CB_TREE (current_statement),
			    _("the targets of SET must be either indexes or pointers"));
		return;
	}
#endif

	/* validate the targets */
	for (l = vars; l; l = CB_CHAIN (l)) {
		cb_tree v = CB_VALUE (l);

		if (CB_CAST_P (v)) {
			struct cb_cast *p = CB_CAST (v);

			if (p->type == CB_CAST_ADDRESS
			    && CB_FIELD (cb_ref (p->val))->storage != CB_STORAGE_LINKAGE) {
				cb_error_x (p->val, _("the address of '%s' cannot be changed"),
					    cb_name (p->val));
				CB_VALUE (l) = cb_error_node;
			}
		}
	}
	VALIDATE_LIST (vars);

	for (l = vars; l; l = CB_CHAIN (l))
		cb_emit (cb_build_move (x, CB_VALUE (l)));
}

void
cb_emit_set_up_down (cb_tree l, cb_tree flag, cb_tree x)
{
	for (; l; l = CB_CHAIN (l))
		if (flag == cb_int0)
			cb_emit (cb_build_add (CB_VALUE (l), x, cb_int0));
		else
			cb_emit (cb_build_sub (CB_VALUE (l), x, cb_int0));
}

void
cb_emit_set_on_off (cb_tree l, cb_tree flag)
{
	for (; l; l = CB_CHAIN (l)) {
		struct cb_system_name *s = CB_SYSTEM_NAME (cb_ref (CB_VALUE (l)));
		cb_emit (cb_build_funcall_2 ("cob_set_switch", cb_int (s->token), flag));
	}
}

void
cb_emit_set_true (cb_tree l)
{
	for (; l; l = CB_CHAIN (l)) {
		cb_tree x = CB_VALUE (l);
		struct cb_field *f = cb_field (x);
		cb_tree ref = cb_build_field_reference (f->parent, x);
		cb_tree val = CB_VALUE (f->values);

		if (CB_PAIR_P (val))
			val = CB_PAIR_X (val);
		cb_emit (cb_build_move (val, ref));
	}
}

/*
 * SORT statement
 */

void
cb_emit_sort_init (cb_tree name, cb_tree keys, cb_tree dup, cb_tree col)
{
	cb_tree l;

	for (l = keys; l; l = CB_CHAIN (l)) {
		if (CB_VALUE (l) == NULL)
			CB_VALUE (l) = name;
		cb_ref (CB_VALUE (l));
	}

	if (CB_FILE_P (cb_ref (name))) {
#ifdef	WITH_DB
		cb_emit (cb_build_funcall_3 ("cob_sort_init", cb_ref (name),
					     cb_int (cb_list_length (keys)), col));
		for (l = keys; l; l = CB_CHAIN (l))
			cb_emit (cb_build_funcall_3 ("cob_sort_init_key", cb_ref (name),
						     CB_PURPOSE (l), CB_VALUE (l)));
#else
		cb_error_x (name, "SORT invalid - DB not configured");
#endif
	} else {
		struct cb_field *f = CB_FIELD (cb_ref (name));

		if (keys == NULL)
			cb_error_x (name, "table sort without keys not implemented yet");
		cb_emit (cb_build_funcall_1 ("cob_table_sort_init", cb_int (cb_list_length (keys))));
		for (l = keys; l; l = CB_CHAIN (l))
			cb_emit (cb_build_funcall_2 ("cob_table_sort_init_key",
						     CB_PURPOSE (l), CB_VALUE (l)));
		cb_emit (cb_build_funcall_2 ("cob_table_sort", name,
					     (f->occurs_depending
					      ? cb_build_cast_integer (f->occurs_depending)
					      : cb_int (f->occurs_max))));
	}
}

void
cb_emit_sort_using (cb_tree file, cb_tree l)
{
	cb_emit (cb_build_funcall_3 ("cob_open", cb_ref (file), cb_int (COB_OPEN_OUTPUT), cb_int0));
	for (; l; l = CB_CHAIN (l))
		cb_emit (cb_build_funcall_2 ("cob_sort_using", cb_ref (file), cb_ref (CB_VALUE (l))));
	cb_emit (cb_build_funcall_2 ("cob_close", cb_ref (file), cb_int (COB_CLOSE_NORMAL)));
}

void
cb_emit_sort_input (cb_tree file, cb_tree proc)
{
	cb_emit (cb_build_funcall_3 ("cob_open", cb_ref (file), cb_int (COB_OPEN_OUTPUT), cb_int0));
	cb_emit (cb_build_perform_once (proc));
	cb_emit (cb_build_funcall_2 ("cob_close", cb_ref (file), cb_int (COB_CLOSE_NORMAL)));
}

void
cb_emit_sort_giving (cb_tree file, cb_tree l)
{
	for (; l; l = CB_CHAIN (l)) {
		cb_emit (cb_build_funcall_3 ("cob_open", cb_ref (file),
					     cb_int (COB_OPEN_INPUT), cb_int0));
		cb_emit (cb_build_funcall_2 ("cob_sort_giving", cb_ref (file), cb_ref (CB_VALUE (l))));
		cb_emit (cb_build_funcall_2 ("cob_close", cb_ref (file), cb_int (COB_CLOSE_NORMAL)));
	}
}

void
cb_emit_sort_output (cb_tree file, cb_tree proc)
{
	cb_emit (cb_build_funcall_3 ("cob_open", cb_ref (file), cb_int (COB_OPEN_INPUT), cb_int0));
	cb_emit (cb_build_perform_once (proc));
	cb_emit (cb_build_funcall_2 ("cob_close", cb_ref (file), cb_int (COB_CLOSE_NORMAL)));
}

void
cb_emit_sort_finish (cb_tree file)
{
	if (CB_FILE_P (cb_ref (file)))
		cb_emit (cb_build_funcall_1 ("cob_sort_finish", cb_ref (file)));
}

/*
 * START statement
 */

void
cb_emit_start (cb_tree file, cb_tree op, cb_tree key)
{
	if ( file != cb_error_node ) {
		current_statement->file = cb_ref (file);
		cb_emit (cb_build_funcall_3 ("cob_start", cb_ref (file), op,
					     key ? key : CB_FILE (cb_ref (file))->key));
	}
}

/*
 * STOP statement
 */

void
cb_emit_stop_run (cb_tree x)
{
	cb_emit (cb_build_funcall_1 ("cob_stop_run", cb_build_cast_integer (x)));
}

/*
 * STRING statement
 */

void
cb_emit_string (cb_tree items, cb_tree into, cb_tree pointer)
{
	cb_tree start = items;

	cb_emit (cb_build_funcall_2 ("cob_string_init", into, pointer));
	while (start) {
		cb_tree l, end;
		cb_tree dlm;

		/* find DELIMITED item */
		for (end = start; end; end = CB_CHAIN (end))
			if (CB_PAIR_P (CB_VALUE (end)))
				break;

		/* cob_string_delimited */
		dlm = end ? CB_PAIR_X (CB_VALUE (end)) : cb_int0;
		cb_emit (cb_build_funcall_1 ("cob_string_delimited", dlm));

		/* cob_string_append */
		for (l = start; l != end; l = CB_CHAIN (l))
			cb_emit (cb_build_funcall_1 ("cob_string_append", CB_VALUE (l)));

		start = end ? CB_CHAIN (end) : NULL;
	}
	cb_emit (cb_build_funcall_0 ("cob_string_finish"));
}

/*
 * UNSTRING statement
 */

void
cb_emit_unstring (cb_tree name, cb_tree delimited, cb_tree into, cb_tree pointer, cb_tree tallying)
{
	cb_emit (cb_build_funcall_2 ("cob_unstring_init", name, pointer));
	cb_emit_list (delimited);
	cb_emit_list (into);
	if (tallying)
		cb_emit (cb_build_funcall_1 ("cob_unstring_tallying", tallying));
	cb_emit (cb_build_funcall_0 ("cob_unstring_finish"));
}

cb_tree
cb_build_unstring_delimited (cb_tree all, cb_tree value)
{
	return cb_build_funcall_2 ("cob_unstring_delimited", value, all);
}

cb_tree
cb_build_unstring_into (cb_tree name, cb_tree delimiter, cb_tree count)
{
	if (delimiter == NULL)
		delimiter = cb_int0;

	if (count == NULL)
		count = cb_int0;

	return cb_build_funcall_3 ("cob_unstring_into", name, delimiter, count);
}

/*
 * WRITE statement
 */

void
cb_emit_write (cb_tree record, cb_tree from, cb_tree opt)
{
	struct cb_field *f = CB_FIELD (cb_ref (record));
	cb_tree file = CB_TREE (f->file);

	current_statement->file = file;
	if (from)
		cb_emit (cb_build_move (from, record));
	cb_emit (cb_build_funcall_3 ("cob_write", file, record, opt));
}

cb_tree
cb_build_write_advancing_lines (cb_tree pos, cb_tree lines)
{
	int opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;
	cb_tree e = cb_build_binary_op (cb_int (opt | COB_WRITE_LINES), '+', lines);

	return cb_build_cast_integer (e);
}

cb_tree
cb_build_write_advancing_mnemonic (cb_tree pos, cb_tree mnemonic)
{
	switch (CB_SYSTEM_NAME (cb_ref (mnemonic))->token) {
	case CB_FEATURE_FORMFEED:
	{
		int opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;

		return cb_int (opt | COB_WRITE_PAGE);
	}
	default:
	{
		cb_error_x (mnemonic, _("invalid mnemonic name"));
		return cb_error_node;
	}
	}
}

cb_tree
cb_build_write_advancing_page (cb_tree pos)
{
	int opt = (pos == CB_BEFORE) ? COB_WRITE_BEFORE : COB_WRITE_AFTER;

	return cb_int (opt | COB_WRITE_PAGE);
}
