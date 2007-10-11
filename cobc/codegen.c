/*
 * Copyright (C) 2002-2007 Keisuke Nishida
 * Copyright (C) 2007 Roger While
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
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include <libcob.h>

#include "cobc.h"
#include "tree.h"
#include "call.def"

#define	COB_USE_SETJMP	0
#define NUM_I_COUNTERS	8

#define INITIALIZE_NONE		0
#define INITIALIZE_ONE		1
#define INITIALIZE_DEFAULT	2
#define INITIALIZE_COMPOUND	3
#define INITIALIZE_EXTERNAL	4

#ifndef __GNUC__
static int inside_check = 0;
static int inside_stack[64];
#endif
static int param_id = 0;
static int stack_id = 0;
static int num_cob_fields = 0;
static int num_cob_genned = 0;
static int loop_counter = 0;
static int progid = 0;
static int last_line = 0;
static int needs_exit_prog = 0;
static int need_double = 0;
static int gen_ebcdic = 0;
static int gen_ebcdic_ascii = 0;
static int gen_full_ebcdic = 0;
static int gen_native = 0;
static int gen_custom = 0;
static int field_iteration = 0;

static int i_counters[NUM_I_COUNTERS];

static int output_indent_level = 0;
static FILE *output_target;
static const char *excp_current_program_id = NULL;
static const char *excp_current_section = NULL;
static const char *excp_current_paragraph = NULL;

static struct label_list {
	struct label_list	*next;
	int			id;
	int			call_num;
} *label_cache = NULL;

static struct attr_list {
	struct attr_list	*next;
	int			id;
	int			type;
	int			digits;
	int			scale;
	int			flags;
	int			lenstr;
	unsigned char		*pic;
} *attr_cache = NULL;

static struct literal_list {
	struct literal_list	*next;
	int			id;
	struct cb_literal	*literal;
	cb_tree			x;
} *literal_cache = NULL;

static struct field_list {
	struct field_list	*next;
	struct cb_field		*f;
	cb_tree			x;
	int			nulldata;
} *field_cache = NULL;

static struct call_list {
	struct call_list	*next;
	const char		*callname;
	const char		*callorig;
} *call_cache = NULL;

struct system_table {
	const char		*syst_name;
	const char		*syst_call;
};

static const struct system_table	system_tab[] = {
#undef	COB_SYSTEM_GEN
#define	COB_SYSTEM_GEN(x, y, z)	{ x, #z },
#include "libcob/system.def"
	{ NULL, NULL }
};

/* Globals */
int entry_number = 0;
int has_external = 0;

#ifdef __GNUC__
static void output (const char *fmt, ...)
    __attribute__ ((__format__ (__printf__, 1, 2)));
static void output_line (const char *fmt, ...)
    __attribute__ ((__format__ (__printf__, 1, 2)));
static void output_storage (const char *fmt, ...)
    __attribute__ ((__format__ (__printf__, 1, 2)));
#else
static void output (const char *fmt, ...);
static void output_line (const char *fmt, ...);
static void output_storage (const char *fmt, ...);
#endif

static void output_stmt (cb_tree x);
static void output_integer (cb_tree x);
static void output_index (cb_tree x);
static void output_func_1 (const char *name, cb_tree x);
static void output_param (cb_tree x, int id);

static void
lookup_call (const char *p, const char *q)
{
	struct call_list *clp;

	for (clp = call_cache; clp; clp = clp->next) {
		if (strcmp (p, clp->callname) == 0) {
			return;
		}
	}
	clp = cobc_malloc (sizeof (struct call_list));
	clp->callname = p;
	clp->callorig = q;
	clp->next = call_cache;
	call_cache = clp;
}

static struct attr_list *
attr_list_reverse (struct attr_list *p)
{
	struct attr_list	*next;
	struct attr_list	*last = NULL;

	for (; p; p = next) {
		next = p->next;
		p->next = last;
		last = p;
	}
	return last;
}

static struct literal_list *
literal_list_reverse (struct literal_list *p)
{
	struct literal_list	*next;
	struct literal_list	*last = NULL;

	for (; p; p = next) {
		next = p->next;
		p->next = last;
		last = p;
	}
	return last;
}

/*
 * Output routines
 */

static void
output (const char *fmt, ...)
{
	if (output_target) {
		va_list ap;
		va_start (ap, fmt);
		vfprintf (output_target, fmt, ap);
		va_end (ap);
	}
}

static void
output_newline (void)
{
	if (output_target) {
		fputs ("\n", output_target);
	}
}

static void
output_prefix (void)
{
	int i;
	if (output_target) {
		for (i = 0; i < output_indent_level; i++) {
			fputc (' ', output_target);
		}
	}
}

static void
output_line (const char *fmt, ...)
{
	if (output_target) {
		va_list ap;
		va_start (ap, fmt);
		output_prefix ();
		vfprintf (output_target, fmt, ap);
		fputc ('\n', output_target);
		va_end (ap);
	}
}

static void
output_indent (const char *str)
{
	const char	*p;
	int		level = 2;

	for (p = str; *p == ' '; p++) {
		level++;
	}

	if (*p == '}' && strcmp (str, "})") != 0) {
		output_indent_level -= level;
	}

	output_line (str);

	if (*p == '{' && strcmp (str, ")}") != 0) {
		output_indent_level += level;
	}
}

static void
output_string (const unsigned char *s, int size)
{
	int i;
	int c;
	int printable = 1;

	for (i = 0; i < size; i++) {
		if (!isprint (s[i])) {
			printable = 0;
		}
	}

	output ("\"");
	for (i = 0; i < size; i++) {
		c = s[i];
		if (printable) {
			if (c == '\"' || c == '\\') {
				output ("\\%c", c);
			} else {
				output ("%c", c);
			}
		} else {
			output ("\\%03o", c);
		}
	}
	output ("\"");
}

static void
output_storage (const char *fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	vfprintf (cb_storage_file, fmt, ap);
	va_end (ap);
}

/*
 * Field
 */

static void
output_base (struct cb_field *f)
{
	struct cb_field	*f01 = cb_field_founder (f);
	struct cb_field *p;
	struct cb_field *v;
	char		*nmp;
	char		name[CB_MAX_CNAME];

	if (f->flag_item_78) {
		return;
	}

	if (f01->redefines) {
		f01 = f01->redefines;
	}

	/* base name */
	if (f01->flag_external) {
		strcpy (name, f01->name);
		for (nmp = name; *nmp; nmp++) {
			if (*nmp == '-') {
				*nmp = '_';
			}
		}
	} else {
/* RXW - Needs more thought - pity
		if (!f01->storage_id) {
			f01->storage_id = cb_storage_id++;
		}
		sprintf (name, "%d", f01->storage_id);
*/
		sprintf (name, "%d", f01->id);
	}

	if (!f01->flag_base) {
		if (!f01->flag_external) {
			if (!f01->flag_local) {
				output_storage ("static ");
			}
#ifdef __GNUC__
			output_storage ("unsigned char %s%s[%d] __attribute__((aligned));",
#else
			output_storage ("unsigned char %s%s[%d];",
#endif
					CB_PREFIX_BASE, name, f01->memory_size);
			output_storage ("\t/* %s */\n", f01->name);
		}
		f01->flag_base = 1;
	}
	output ("%s%s", CB_PREFIX_BASE, name);

	if (cb_field_variable_address (f)) {
		for (p = f->parent; p; f = f->parent, p = f->parent) {
			for (p = p->children; p != f; p = p->sister) {
				v = cb_field_variable_size (p);
				if (v) {
					output (" + %d + ", v->offset - p->offset);
					if (v->size != 1) {
						output ("%d * ", v->size);
					}
					output_integer (v->occurs_depending);
				} else {
					output (" + %d", p->size);
				}
			}
		}
	} else if (f->offset > 0) {
		output (" + %d", f->offset);
	}
}

static void
output_data (cb_tree x)
{
	struct cb_literal	*l;
	struct cb_reference	*r;
	struct cb_field		*f;
	cb_tree			lsub;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL:
		l = CB_LITERAL (x);
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			output ("(unsigned char *)\"%s%s\"", l->data,
				(l->sign < 0) ? "-" : (l->sign > 0) ? "+" : "");
		} else {
			output ("(unsigned char *)");
			output_string (l->data, l->size);
		}
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE (x);
		f = CB_FIELD (r->value);

		/* base address */
		output_base (f);

		/* subscripts */
		if (r->subs) {
			lsub = r->subs;
			for (; f; f = f->parent) {
				if (f->flag_occurs) {
					output (" + ");
					if (f->size != 1) {
						output ("%d * ", f->size);
					}
					output_index (CB_VALUE (lsub));
					lsub = CB_CHAIN (lsub);
				}
			}
		}

		/* offset */
		if (r->offset) {
			output (" + ");
			output_index (r->offset);
		}
		break;
	case CB_TAG_CAST:
		output ("&");
		output_param (x, 0);
		break;
	case CB_TAG_INTRINSIC:
		output ("module.cob_procedure_parameters[%d]->data", field_iteration);
		break;
	case CB_TAG_CONST:
		if (x == cb_null) {
			output ("NULL");
			return;
		}
		/* Fall through */
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		fflush (stderr);
		ABORT ();
	}
}

static void
output_size (cb_tree x)
{
	struct cb_literal	*l;
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*p;
	struct cb_field		*q;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL:
		l = CB_LITERAL (x);
		output ("%d", (int)(l->size + ((l->sign != 0) ? 1 : 0)));
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE (x);
		f = CB_FIELD (r->value);
		if (r->length) {
			output_integer (r->length);
		} else if (r->offset) {
			output ("%d - ", f->size);
			output_index (r->offset);
		} else {
			p = cb_field_variable_size (f);
			q = f;

again:
			if (p && (r->type == CB_SENDING_OPERAND
			    || !cb_field_subordinate (cb_field (p->occurs_depending), q))) {
				if (p->offset - q->offset > 0) {
					output ("%d + ", p->offset - q->offset);
				}
				if (p->size != 1) {
					output ("%d * ", p->size);
				}
				output_integer (p->occurs_depending);
				q = p;
			} else {
				output ("%d", q->size);
			}

			for (; q != f; q = q->parent) {
				if (q->sister && !q->sister->redefines) {
					q = q->sister;
					p = q->occurs_depending ? q : cb_field_variable_size (q);
					output (" + ");
					goto again;
				}
			}
		}
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}
}

static int
lookup_attr (int type, int digits, int scale, int flags, unsigned char *pic, int lenstr)
{
	struct attr_list *l;

	/* search attribute cache */
	for (l = attr_cache; l; l = l->next) {
		if (type == l->type
		    && digits == l->digits
		    && scale == l->scale && flags == l->flags
		    && ((pic == l->pic) || (pic && l->pic && lenstr == l->lenstr
		    && memcmp ((char *)pic, (char *)(l->pic), (size_t)lenstr) == 0))) {
			return l->id;
		}
	}

	/* output new attribute */

	/* cache it */
	l = cobc_malloc (sizeof (struct attr_list));
	l->id = cb_attr_id;
	l->type = type;
	l->digits = digits;
	l->scale = scale;
	l->flags = flags;
	l->pic = pic;
	l->lenstr = lenstr;
	l->next = attr_cache;
	attr_cache = l;

	return cb_attr_id++;
}

static void
output_attr (cb_tree x)
{
	int			id;
	int			type;
	int			flags;
	struct cb_literal	*l;
	struct cb_reference	*r;
	struct cb_field		*f;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_LITERAL:
		l = CB_LITERAL (x);
		if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
			flags = 0;
			if (l->sign != 0) {
				flags = COB_FLAG_HAVE_SIGN | COB_FLAG_SIGN_SEPARATE;
			}
			id = lookup_attr (COB_TYPE_NUMERIC_DISPLAY,
					  l->size, l->scale, flags, NULL, 0);
		} else {
			if (l->all) {
				id = lookup_attr (COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, NULL, 0);
			} else {
				id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
			}
		}
		break;
	case CB_TAG_REFERENCE:
		type = cb_tree_type (x);
		r = CB_REFERENCE (x);
		f = CB_FIELD (r->value);
		flags = 0;
		if (r->offset) {
			id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		} else {
			switch (type) {
			case COB_TYPE_GROUP:
			case COB_TYPE_ALPHANUMERIC:
				if (f->flag_justified) {
					id = lookup_attr (type, 0, 0, COB_FLAG_JUSTIFIED, NULL, 0);
				} else {
					id = lookup_attr (type, 0, 0, 0, NULL, 0);
				}
				break;
			default:
				if (f->pic->have_sign) {
					flags |= COB_FLAG_HAVE_SIGN;
					if (f->flag_sign_separate) {
						flags |= COB_FLAG_SIGN_SEPARATE;
					}
					if (f->flag_sign_leading) {
						flags |= COB_FLAG_SIGN_LEADING;
					}
				}
				if (f->flag_blank_zero) {
					flags |= COB_FLAG_BLANK_ZERO;
				}
				if (f->flag_justified) {
					flags |= COB_FLAG_JUSTIFIED;
				}
				if (f->flag_binary_swap) {
					flags |= COB_FLAG_BINARY_SWAP;
				}
				if (f->flag_real_binary) {
					flags |= COB_FLAG_REAL_BINARY;
				}

				id = lookup_attr (type, f->pic->digits, f->pic->scale,
						  flags, (ucharptr) f->pic->str, f->pic->lenstr);
				break;
			}
		}
		break;
	case CB_TAG_ALPHABET_NAME:
		id = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}

	output ("&%s%d", CB_PREFIX_ATTR, id);
}

static void
output_field (cb_tree x)
{
	output ("{");
	output_size (x);
	output (", ");
	output_data (x);
	output (", ");
	output_attr (x);
	output ("}");
}

/*
 * Literal
 */

static int
lookup_literal (cb_tree x)
{

	struct cb_literal	*literal = CB_LITERAL (x);
	struct literal_list	*l;
	FILE			*savetarget;

	/* search literal cache */
	for (l = literal_cache; l; l = l->next) {
		if (CB_TREE_CLASS (literal) == CB_TREE_CLASS (l->literal)
		    && literal->size == l->literal->size
		    && literal->all == l->literal->all
		    && literal->sign == l->literal->sign
		    && literal->scale == l->literal->scale
		    && memcmp (literal->data, l->literal->data, literal->size) == 0) {
			return l->id;
		}
	}

	/* output new literal */
	savetarget = output_target;
	output_target = NULL;
	output_field (x);

	output_target = savetarget;

	/* cache it */
	l = cobc_malloc (sizeof (struct literal_list));
	l->id = cb_literal_id;
	l->literal = literal;
	l->x = x;
	l->next = literal_cache;
	literal_cache = l;

	return cb_literal_id++;
}

/*
 * Integer
 */

static void
output_integer (cb_tree x)
{
	struct cb_binary_op	*p;
	struct cb_cast		*cp;
	struct cb_field		*f;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_zero) {
			output ("0");
		} else if (x == cb_null) {
			output ("(unsigned char *)NULL");
		} else {
			output ("%s", CB_CONST (x)->val);
		}
		break;
	case CB_TAG_INTEGER:
		output ("%d", CB_INTEGER (x)->val);
		break;
	case CB_TAG_LITERAL:
		output ("%d", cb_get_int (x));
		break;
	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP (x);
		if (p->op == '^') {
			output ("(int) pow (");
			output_integer (p->x);
			output (", ");
			output_integer (p->y);
			output (")");
		} else {
			output ("(");
			if (need_double) {
				output ("(double)");
			}
			output_integer (p->x);
			output (" %c ", p->op);
			if (need_double) {
				output ("(double)");
			}
			output_integer (p->y);
			output (")");
		}
		break;
	case CB_TAG_CAST:
		cp = CB_CAST (x);

		switch (cp->type) {
		case CB_CAST_ADDRESS:
			output ("(");
			output_data (cp->val);
			output (")");
			break;
		case CB_CAST_PROGRAM_POINTER:
			output_func_1 ("cob_call_resolve", x);
			break;
		default:
			fprintf (stderr, "Unexpected cast type %d\n", cp->type);
			ABORT ();
		}
		break;
	case CB_TAG_REFERENCE:
		f = cb_field (x);
		switch (f->usage) {
		case CB_USAGE_INDEX:
		case CB_USAGE_LENGTH:
			output ("(*(int *) (");
			output_data (x);
			output ("))");
			return;

		case CB_USAGE_POINTER:
#ifdef	COB_NON_ALIGNED
			output ("(cob_get_pointer (");
			output_data (x);
			output ("))");
#else
			output ("(*(unsigned char **) (");
			output_data (x);
			output ("))");
#endif
			return;

		case CB_USAGE_PROGRAM_POINTER:
#ifdef	COB_NON_ALIGNED
			output ("(cob_get_prog_pointer (");
			output_data (x);
			output ("))");
#else
			output ("(*(void **) (");
			output_data (x);
			output ("))");
#endif
			return;

		case CB_USAGE_DISPLAY:
			if (f->pic->scale >= 0
			    && f->size - f->pic->scale > 0
			    && f->size - f->pic->scale <= 9
			    && f->pic->have_sign == 0) {
				output ("cob_get_numdisp (");
				output_data (x);
				output (", %d)", f->size - f->pic->scale);
				return;
			}
			break;

		case CB_USAGE_PACKED:
			if (f->pic->scale == 0 && f->pic->digits < 10) {
				output_func_1 ("cob_get_packed_int", x);
				return;
			}
			break;

		case CB_USAGE_BINARY:
		case CB_USAGE_COMP_5:
		case CB_USAGE_COMP_X:
			if (f->size == 1) {
				output ("(*(");
				if (!f->pic->have_sign) {
					output ("unsigned ");
				}
				output ("char *) (");
				output_data (x);
				output ("))");
				return;
			}
#ifdef	COB_NON_ALIGNED
			if (f->indexes == 0 && (
#ifdef	COB_SHORT_BORK
				(f->size == 2 && (f->offset % 4 == 0)) ||
#else
				(f->size == 2 && (f->offset % 2 == 0)) ||
#endif
				(f->size == 4 && (f->offset % 4 == 0)) ||
				(f->size == 8 && (f->offset % 8 == 0)))) {
#else
			if (f->size == 2 || f->size == 4 || f->size == 8) {
#endif
				if (f->flag_binary_swap) {
					output ("((");
					if (!f->pic->have_sign) {
						output ("unsigned ");
					}
					switch (f->size) {
					case 2:
						output ("short)COB_BSWAP_16(");
						break;
					case 4:
						output ("int)COB_BSWAP_32(");
						break;
					case 8:
						output ("long long)COB_BSWAP_64(");
						break;
					}
					output ("*(");
					switch (f->size) {
					case 2:
						output ("short *)(");
						break;
					case 4:
						output ("int *)(");
						break;
					case 8:
						output ("long long *)(");
						break;
					}
					output_data (x);
					output (")))");
					return;
				} else {
					output ("(*(");
					if (!f->pic->have_sign) {
						output ("unsigned ");
					}
					switch (f->size) {
					case 2:
						output ("short *)(");
						break;
					case 4:
						output ("int *)(");
						break;
					case 8:
						output ("long long *)(");
						break;
					}
					output_data (x);
					output ("))");
					return;
				}
			}
			break;

		default:
			break;
		}

		output_func_1 ("cob_get_int", x);
		break;
	case CB_TAG_INTRINSIC:
		output ("cob_get_int (");
		output_param (x, -1);
		output (")");
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}
}

static void
output_index (cb_tree x)
{
	switch (CB_TREE_TAG (x)) {
	case CB_TAG_INTEGER:
		output ("%d", CB_INTEGER (x)->val - 1);
		break;
	case CB_TAG_LITERAL:
		output ("%d", cb_get_int (x) - 1);
		break;
	default:
		output ("(");
		output_integer (x);
		output (" - 1)");
		break;
	}
}

/*
 * Parameter
 */

static void
output_param (cb_tree x, int id)
{
	struct cb_reference	*r;
	struct cb_field		*f;
	struct cb_field		*pechk;
	struct cb_cast		*cp;
	struct cb_binary_op	*bp;
	struct field_list	*fl;
	FILE			*savetarget;
	struct cb_intrinsic	*ip;
	struct cb_alphabet_name	*abp;
	struct cb_alphabet_name	*rbp;
	cb_tree			l;
	int			n;
	int			extrefs;
	int			sav_stack_id;
	char			fname[12];

	param_id = id;

	if (x == NULL) {
		output ("0");
		return;
	}

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		output ("%s", CB_CONST (x)->val);
		break;
	case CB_TAG_INTEGER:
		output_integer (x);
		break;
	case CB_TAG_STRING:
		output_string (CB_STRING (x)->data, CB_STRING (x)->size);
		break;
	case CB_TAG_LOCALE_NAME:
		output_param (CB_LOCALE_NAME(x)->list, id);
		break;
	case CB_TAG_ALPHABET_NAME:
		abp = CB_ALPHABET_NAME (x);
		switch (abp->type) {
		case CB_ALPHABET_STANDARD_1:
		case CB_ALPHABET_STANDARD_2:
#ifdef	COB_EBCDIC_MACHINE
			gen_ebcdic_ascii = 1;
			output ("cob_ebcdic_ascii");
			break;
#endif
		case CB_ALPHABET_NATIVE:
			gen_native = 1;
			output ("NULL");
			break;
		case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
			gen_native = 1;
			output ("NULL");
#else
			gen_ebcdic = 1;
			output ("cob_a2e");
#endif
			break;
		case CB_ALPHABET_CUSTOM:
			gen_custom = 1;
			output ("%s%s", CB_PREFIX_SEQUENCE, abp->cname);
			break;
		}
		break;
	case CB_TAG_CAST:
		cp = CB_CAST (x);
		switch (cp->type) {
		case CB_CAST_INTEGER:
			output_integer (cp->val);
			break;
		case CB_CAST_ADDRESS:
			output_data (cp->val);
			break;
		case CB_CAST_ADDR_OF_ADDR:
			output ("&");
			output_data (cp->val);
			break;
		case CB_CAST_LENGTH:
			output_size (cp->val);
			break;
		case CB_CAST_PROGRAM_POINTER:
			output_param (cp->val, id);
			break;
		}
		break;
	case CB_TAG_DECIMAL:
		output ("&d%d", CB_DECIMAL (x)->id);
		break;
	case CB_TAG_FILE:
		output ("%s%s", CB_PREFIX_FILE, CB_FILE (x)->cname);
		break;
	case CB_TAG_LITERAL:
		output ("&%s%d", CB_PREFIX_CONST, lookup_literal (x));
		break;
	case CB_TAG_FIELD:
		/* TODO: remove me */
		output_param (cb_build_field_reference (CB_FIELD (x), 0), id);
		break;
	case CB_TAG_REFERENCE:
		r = CB_REFERENCE (x);
		extrefs = 0;
		if (r->check) {
#ifdef __GNUC__
			output_indent (" ({");
#else
			inside_stack[inside_check] = 0;
			++inside_check;
			output (" (\n");
#endif
			for (l = r->check; l; l = CB_CHAIN (l)) {
				sav_stack_id = stack_id;
				output_stmt (CB_VALUE (l));
				stack_id = sav_stack_id;
			}
		}

		if (CB_FILE_P (r->value)) {
			output ("%s%s", CB_PREFIX_FILE, CB_FILE (r->value)->cname);
			if (r->check) {
#ifdef __GNUC__
				output ("; })");
#else
				--inside_check;
				output (" )");
#endif
			}
			break;
		}
		if (CB_ALPHABET_NAME_P (r->value)) {
			rbp = CB_ALPHABET_NAME (r->value);
			switch (rbp->type) {
			case CB_ALPHABET_STANDARD_1:
			case CB_ALPHABET_STANDARD_2:
#ifdef	COB_EBCDIC_MACHINE
				gen_ebcdic_ascii = 1;
				output ("&f_ebcdic_ascii");
				break;
#endif
			case CB_ALPHABET_NATIVE:
				gen_native = 1;
				output ("&f_native");
				break;
			case CB_ALPHABET_EBCDIC:
#ifdef	COB_EBCDIC_MACHINE
				gen_native = 1;
				output ("&f_native");
#else
				gen_full_ebcdic = 1;
				output ("&f_ebcdic");
#endif
				break;
			case CB_ALPHABET_CUSTOM:
				gen_custom = 1;
				output ("&f_%s", rbp->cname);
				break;
			}
			if (r->check) {
#ifdef __GNUC__
				output ("; })");
#else
				--inside_check;
				output (" )");
#endif
			}
			break;
		}
		f = CB_FIELD (r->value);
		if (f->redefines && f->redefines->flag_external) {
			extrefs = 1;
			f->flag_item_external = 1;
			f->flag_external = 1;
		}
		if (f->redefines && f->redefines->flag_item_based) {
			f->flag_local = 1;
		}
		for (pechk = f->parent; pechk; pechk = pechk->parent) {
			if (pechk->flag_external) {
				extrefs = 1;
				f->flag_item_external = 1;
				break;
			}
			if (pechk->redefines && pechk->redefines->flag_external) {
				extrefs = 1;
				f->flag_item_external = 1;
				f->flag_external = 1;
				break;
			}
			if (pechk->flag_item_based) {
				f->flag_local = 1;
				break;
			}
			if (pechk->redefines && pechk->redefines->flag_item_based) {
				f->flag_local = 1;
				break;
			}
		}
		if (f->flag_external) {
			f->flag_item_external = 1;
		}
		if (!r->subs && !r->offset && f->count > 0
/* RXW possible tweak here, although unlikely
		if (!r->offset && f->count > 0
		    && !f->flag_external && !extrefs
*/
		    && !cb_field_variable_size (f) && !cb_field_variable_address (f)) {
			if (!f->flag_field) {
				savetarget = output_target;
				output_target = NULL;
				output_field (x);

				fl = cobc_malloc (sizeof (struct field_list));
				fl->x = x;
				fl->f = f;
				fl->nulldata = (r->subs != NULL);
				fl->next = field_cache;
				field_cache = fl;

				f->flag_field = 1;
				output_target = savetarget;
			}
			if (f->flag_local) {
				output ("(%s%d.data = ", CB_PREFIX_FIELD, f->id);
				output_data (x);
				output (", &%s%d)", CB_PREFIX_FIELD, f->id);
			} else {
				if (f->storage == CB_STORAGE_SCREEN && id != -1) {
					output ("&s_%d", f->id);
				} else {
					output ("&%s%d", CB_PREFIX_FIELD, f->id);
				}
			}
		} else {
			if (stack_id >= num_cob_fields) {
				num_cob_fields = stack_id + 1;
			}
			sprintf (fname, "f%d", stack_id++);
#ifndef __GNUC__
			if (inside_check != 0) {
				if (inside_stack[inside_check-1] != 0) {
					inside_stack[inside_check-1] = 0;
					output (",\n");
				}
			}
#endif
			output ("(%s.size = ", fname);
			output_size (x);
			output (", %s.data = ", fname);
			output_data (x);
			output (", %s.attr = ", fname);
			output_attr (x);
			output (", &%s)", fname);
		}

		if (r->check) {
#ifdef __GNUC__
			output ("; })");
#else
			--inside_check;
			output (" )");
#endif
		}
		break;
	case CB_TAG_BINARY_OP:
		bp = CB_BINARY_OP (x);
		output ("cob_intr_binop (");
		output_param (bp->x, id);
		output (", ");
		output ("%d", bp->op);
		output (", ");
		output_param (bp->y, id);
		output (")");
		break;
	case CB_TAG_INTRINSIC:
		n = 0;
		ip = CB_INTRINSIC (x);
		output ("%s (", ip->intr_tab->intr_routine);
		if (ip->intr_field) {
			if (ip->intr_field == cb_int0) {
				output ("NULL");
			} else if (ip->intr_field == cb_int1) {
				for (l = ip->args; l; l = CB_CHAIN (l)) {
					n++;
				}
				output ("%d", n);
			} else {
				output_param (ip->intr_field, id);
			}
			if (ip->args) {
				output (", ");
			}
		}
		for (l = ip->args; l; l = CB_CHAIN (l)) {
			output_param (CB_VALUE (l), id);
			id++;
			param_id++;
			if (CB_CHAIN (l)) {
				output (", ");
			}
		}
		output (")");
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}
}

/*
 * Function call
 */

static void
output_funcall (cb_tree x)
{
	struct cb_funcall	*p = CB_FUNCALL (x);
	cb_tree			l;
	int			i;

	if (p->name[0] == '$') {
/*
	struct cb_field *f = cb_field (p->argv[0]);
*/
		switch (p->name[1]) {
		case 'E':
			/* Set of one character */
			output ("*(");
			output_data (p->argv[0]);
			output (") = ");
			output_param (p->argv[1], 1);
			break;
		case 'F':
			/* Move of one character */
			output ("*(");
			output_data (p->argv[0]);
			output (") = *(");
			output_data (p->argv[1]);
			output (")");
			break;
		case 'G':
			/* Test of one character */
			output ("(int)(*(");
			output_data (p->argv[0]);
			if (p->argv[1] == cb_space) {
				output (") - ' ')");
			} else if (p->argv[1] == cb_zero) {
				output (") - '0')");
			} else if (p->argv[1] == cb_low) {
				output ("))");
			} else if (p->argv[1] == cb_high) {
				output (") - 255)");
			} else if (CB_LITERAL_P (p->argv[1])) {
				output (") - %d)", *(CB_LITERAL (p->argv[1])->data));
			} else {
				output (") - *(");
				output_data (p->argv[1]);
				output ("))");
			}
			break;
		default:
			ABORT ();
		}
		return;
	}
	output ("%s (", p->name);
	for (i = 0; i < p->argc; i++) {
		if (p->varcnt && i + 1 == p->argc) {
			output ("%d, ", p->varcnt);
			for (l = p->argv[i]; l; l = CB_CHAIN (l)) {
				output_param (CB_VALUE (l), i);
				i++;
				if (CB_CHAIN (l)) {
					output (", ");
				}
			}
		} else {
			output_param (p->argv[i], i);
			if (i + 1 < p->argc) {
				output (", ");
			}
		}
	}
	output (")");
}

static void
output_func_1 (const char *name, cb_tree x)
{
	output ("%s (", name);
	output_param (x, param_id);
	output (")");
}

/*
 * Condition
 */

static void
output_cond (cb_tree x, int save_flag)
{
	struct cb_binary_op	*p;

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_CONST:
		if (x == cb_true) {
			output ("1");
		} else if (x == cb_false) {
			output ("0");
		} else {
			ABORT ();
		}
		break;
	case CB_TAG_BINARY_OP:
		p = CB_BINARY_OP (x);
		switch (p->op) {
		case '!':
			output ("!");
			output_cond (p->x, save_flag);
			break;

		case '&':
		case '|':
			output ("(");
			output_cond (p->x, save_flag);
			output (p->op == '&' ? " && " : " || ");
			output_cond (p->y, save_flag);
			output (")");
			break;

		case '=':
		case '<':
		case '[':
		case '>':
		case ']':
		case '~':
			output ("((int)");
			output_cond (p->x, save_flag);
			switch (p->op) {
			case '=':
				output (" == 0");
				break;
			case '<':
				output (" <  0");
				break;
			case '[':
				output (" <= 0");
				break;
			case '>':
				output (" >  0");
				break;
			case ']':
				output (" >= 0");
				break;
			case '~':
				output (" != 0");
				break;
			}
			output (")");
			break;

		default:
			output_integer (x);
			break;
		}
		break;
	case CB_TAG_FUNCALL:
		if (save_flag) {
			output ("(ret = ");
		}
		output_funcall (x);
		if (save_flag) {
			output (")");
		}
		break;
	case CB_TAG_LIST:
		if (save_flag) {
			output ("(ret = ");
		}
#ifdef __GNUC__
		output_indent ("({");
#else
		inside_stack[inside_check] = 0;
		++inside_check;
		output ("(\n");
#endif
		for (; x; x = CB_CHAIN (x)) {
			output_stmt (CB_VALUE (x));
		}
#ifdef __GNUC__
		output_indent ("})");
#else
		--inside_check;
		output (")");
#endif
		if (save_flag) {
			output (")");
		}
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}
}

/*
 * MOVE
 */

static void
output_move (cb_tree src, cb_tree dst)
{
	output_stmt (cb_build_move (src, dst));
}

/*
 * INITIALIZE
 */

static int
initialize_type (struct cb_initialize *p, struct cb_field *f, int topfield)
{
	cb_tree		l;
	int		type;

	if (f->flag_item_78) {
		return INITIALIZE_NONE;
	}

	if (f->flag_chained) {
		return INITIALIZE_ONE;
	}

	if (f->flag_external) {
		return INITIALIZE_EXTERNAL;
	}

	if (f->redefines && (!topfield || !p->flag_statement)) {
		return INITIALIZE_NONE;
	}

	if (p->val && f->values) {
		return INITIALIZE_ONE;
	}

	if (p->flag_statement && !f->children) {
		if (strlen (f->name) > 4 && f->name[4] == '$') {
			return INITIALIZE_NONE;
		}
	}

	if (f->children) {
		type = initialize_type (p, f->children, 0);
		if (type == INITIALIZE_ONE) {
			return INITIALIZE_COMPOUND;
		}
		for (f = f->children->sister; f; f = f->sister) {
			if (type != initialize_type (p, f, 0)) {
				return INITIALIZE_COMPOUND;
			}
		}
		return type;
	} else {
		for (l = p->rep; l; l = CB_CHAIN (l)) {
			if ((int)CB_PURPOSE_INT (l) == (int)CB_TREE_CATEGORY (f)) {
				return INITIALIZE_ONE;
			}
		}
	}

	if (p->def) {
		if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
			return INITIALIZE_ONE;
		}
		switch (CB_TREE_CATEGORY (f)) {
		case CB_CATEGORY_NUMERIC_EDITED:
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
			return INITIALIZE_ONE;
		default:
			if (cb_tree_type (CB_TREE (f)) == COB_TYPE_NUMERIC_PACKED) {
				return INITIALIZE_ONE;
			} else {
				return INITIALIZE_DEFAULT;
			}
		}
	}

	return INITIALIZE_NONE;
}

static int
initialize_uniform_char (struct cb_field *f)
{
	int	c;

	if (f->children) {
		c = initialize_uniform_char (f->children);
		for (f = f->children->sister; f; f = f->sister) {
			if (!f->redefines) {
				if (c != initialize_uniform_char (f)) {
					return -1;
				}
			}
		}
		return c;
	} else {
		switch (cb_tree_type (CB_TREE (f))) {
		case COB_TYPE_NUMERIC_BINARY:
			return 0;
		case COB_TYPE_NUMERIC_DISPLAY:
			return '0';
		case COB_TYPE_ALPHANUMERIC:
			return ' ';
		default:
			return -1;
		}
	}
}

static void
output_initialize_fp (cb_tree x, struct cb_field *f)
{
	output_prefix ();
	if (f->usage == CB_USAGE_FLOAT) {
		output ("{float temp = 0.0;");
	} else {
		output ("{double temp = 0.0;");
	}
	output (" memcpy (");
	output_data (x);
	output (", (char *)&temp, sizeof(temp));}\n");
}

static void
output_initialize_external (cb_tree x, struct cb_field *f)
{
	unsigned char	*p;
	char		name[CB_MAX_CNAME];

	output_prefix ();
	output_data (x);
	if (f->ename) {
		output (" = cob_external_addr (\"%s\", %d);\n", f->ename, f->size);
	} else {
		strcpy (name, f->name);
		for (p = (unsigned char *)name; *p; p++) {
			if (islower (*p)) {
				*p = toupper (*p);
			}
		}
		output (" = cob_external_addr (\"%s\", %d);\n", name, f->size);
	}
}

static void
output_initialize_uniform (cb_tree x, int c, int size)
{
	output_prefix ();
	if (size == 1) {
		output ("*(unsigned char *)(");
		output_data (x);
		output (") = %d;\n", c);
	} else {
		output ("memset (");
		output_data (x);
		output (", %d, %d);\n", c, size);
	}
}

static void
output_initialize_one (struct cb_initialize *p, cb_tree x)
{
	struct cb_field		*f = cb_field (x);
	cb_tree			value;
	cb_tree			lrp;
	struct cb_literal	*l;
	int			i;
	int			n;
	int			buffchar;

	static char		*buff = NULL;
	static int		lastsize = 0;

	/* CHAINING */
	if (f->flag_chained) {
		output_prefix ();
		output ("cob_chain_setup (");
		output_data (x);
		output (", %d, %d);\n", f->param_num, f->size);
		return;
	}
	/* initialize by value */
	if (p->val && f->values) {
		value = CB_VALUE (f->values);
		if (value == cb_space) {
			/* SPACE */
			/* FIXME: This is to avoid an error when a numeric-edited item
			   has VALUE SPACE because cob_build_move doubly checks the value.
			   We should instead check the value only once.  */
			output_prefix ();
			if (f->size == 1) {
				output ("*(unsigned char *)(");
				output_data (x);
				output (") = ' ';\n");
			} else {
				output ("memset (");
				output_data (x);
				output (", ' ', %d);\n", f->size);
			}
		} else if (CB_CONST_P (value)
			   || CB_TREE_CLASS (value) == CB_CLASS_NUMERIC
			   || CB_LITERAL (value)->all) {
			/* figurative literal, numeric literal, or ALL literal */
			output_move (value, x);
		} else {
			/* alphanumeric literal */
			/* We do not use output_move here because
			   we do not want to have the value be edited. */
			l = CB_LITERAL (value);
			if (!buff) {
				if (f->size <= COB_SMALL_BUFF) {
					buff = cobc_malloc (COB_SMALL_BUFF);
					lastsize = COB_SMALL_BUFF;
				} else {
					buff = cobc_malloc (f->size);
					lastsize = f->size;
				}
			} else {
				if (f->size > lastsize) {
					free (buff);
					buff = cobc_malloc (f->size);
					lastsize = f->size;
				}
			}
			l = CB_LITERAL (value);
			if ((int)l->size >= (int)f->size) {
				memcpy (buff, l->data, (size_t)f->size);
			} else {
				memcpy (buff, l->data, l->size);
				memset (buff + l->size, ' ', f->size - l->size);
			}
			output_prefix ();
			if (f->size == 1) {
				output ("*(unsigned char *) (");
				output_data (x);
				output (") = %d;\n", *(unsigned char *)buff);
			} else {
				buffchar = *buff;
				for (i = 0; i < f->size; i++) {
					if (*(buff + i) != buffchar) {
						break;
					}
				}
				if (i == f->size) {
					output ("memset (");
					output_data (x);
					output (", %d, %d);\n", buffchar, f->size);
				} else {
					if (f->size >= 8) {
						buffchar = *(buff + f->size - 1);
						n = 0;
						for (i = f->size - 1; i >= 0; i--, n++) {
							if (*(buff + i) != buffchar) {
								break;
							}
						}
						if (n > 2) {
							output ("memcpy (");
							output_data (x);
							output (", ");
							output_string ((ucharptr) buff,
								       f->size - n);
							output (", %d);\n", f->size - n);
							output_prefix ();
							output ("memset (");
							output_data (x);
							output (" + %d, %d, %d);\n",
								f->size - n, buffchar, n);
							return;
						}
					}
					output ("memcpy (");
					output_data (x);
					output (", ");
					output_string ((ucharptr) buff, f->size);
					output (", %d);\n", f->size);
				}
			}
		}
		return;
	}

	/* initialize replacing */
	if (!f->children) {
		for (lrp = p->rep; lrp; lrp = CB_CHAIN (lrp)) {
			if ((int)CB_PURPOSE_INT (lrp) == (int)CB_TREE_CATEGORY (x)) {
				output_move (CB_VALUE (lrp), x);
				return;
			}
		}
	}

	/* initialize by default */
	if (p->def) {
		if (f->usage == CB_USAGE_FLOAT || f->usage == CB_USAGE_DOUBLE) {
			output_initialize_fp (x, f);
			return;
		}
		switch (CB_TREE_CATEGORY (x)) {
		case CB_CATEGORY_NUMERIC:
		case CB_CATEGORY_NUMERIC_EDITED:
			output_move (cb_zero, x);
			break;
		case CB_CATEGORY_ALPHANUMERIC_EDITED:
		case CB_CATEGORY_NATIONAL_EDITED:
			output_move (cb_space, x);
			break;
		default:
			fprintf (stderr, "Unexpected tree category %d\n", CB_TREE_CATEGORY (x));
			ABORT ();
		}
	}
}

static void
output_initialize_compound (struct cb_initialize *p, cb_tree x)
{
	struct cb_field	*ff = cb_field (x);
	struct cb_field	*f;
	struct cb_field	*last_field;
	cb_tree		c;
	int		type;
	int		last_char;
	int		i;
	size_t		size;

	for (f = ff->children; f; f = f->sister) {
		type = initialize_type (p, f, 0);
		c = cb_build_field_reference (f, x);

		switch (type) {
		case INITIALIZE_NONE:
			break;
		case INITIALIZE_DEFAULT:
		{
			last_field = f;
			last_char = initialize_uniform_char (f);

			if (last_char != -1) {
				if (f->flag_occurs) {
					CB_REFERENCE (c)->subs =
					    cb_cons (cb_int1, CB_REFERENCE (c)->subs);
				}

				for (; f->sister; f = f->sister) {
					if (!f->sister->redefines) {
						if (initialize_type (p, f->sister, 0) != INITIALIZE_DEFAULT
						    || initialize_uniform_char (f->sister) != last_char) {
							break;
						}
					}
				}

				if (f->sister) {
					size = f->sister->offset - last_field->offset;
				} else {
					size = ff->offset + ff->size - last_field->offset;
				}

				output_initialize_uniform (c, last_char, size);
				break;
			}
			/* fall through */
		}
		default:
			if (f->flag_occurs) {
				/* begin occurs loop */
				i = f->indexes;
				i_counters[i] = 1;
				output_line ("for (i%d = 1; i%d <= %d; i%d++)",
					     i, i, f->occurs_max, i);
				output_indent ("  {");
				CB_REFERENCE (c)->subs =
				    cb_cons (cb_i[i], CB_REFERENCE (c)->subs);
			}

			if (type == INITIALIZE_ONE) {
				output_initialize_one (p, c);
			} else {
				output_initialize_compound (p, c);
			}

			if (f->flag_occurs) {
				/* close loop */
				CB_REFERENCE (c)->subs = CB_CHAIN (CB_REFERENCE (c)->subs);
				output_indent ("  }");
			}
		}
	}
}

static void
output_initialize (struct cb_initialize *p)
{
	struct cb_field *f = cb_field (p->var);
	int		c;

	switch (initialize_type (p, f, 1)) {
	case INITIALIZE_NONE:
		break;
	case INITIALIZE_ONE:
		output_initialize_one (p, p->var);
		break;
	case INITIALIZE_EXTERNAL:
		output_initialize_external (p->var, f);
		break;
	case INITIALIZE_DEFAULT:
		c = initialize_uniform_char (f);
		if (c != -1) {
			output_initialize_uniform (p->var, c, f->size);
		} else {
			output_initialize_compound (p, p->var);
		}
		break;
	case INITIALIZE_COMPOUND:
		output_initialize_compound (p, p->var);
		break;
	}
}

/*
 * SEARCH
 */

static void
output_occurs (struct cb_field *p)
{
	if (p->occurs_depending) {
		output_integer (p->occurs_depending);
	} else {
		output ("%d", p->occurs_max);
	}
}

static void
output_search_whens (cb_tree table, cb_tree var, cb_tree stmt, cb_tree whens)
{
	cb_tree		l;
	struct cb_field *p = cb_field (table);
	cb_tree		idx = NULL;

	/* determine the index to use */
	if (var) {
		for (l = p->index_list; l; l = CB_CHAIN (l)) {
			if (cb_ref (CB_VALUE (l)) == cb_ref (var)) {
				idx = var;
			}
		}
	}
	if (!idx) {
		idx = CB_VALUE (p->index_list);
	}

	/* start loop */
	output_line ("while (1)");
	output_indent ("  {");

	/* end test */
	output_prefix ();
	output ("if (");
	output_integer (idx);
	output (" > ");
	output_occurs (p);
	output (")\n");
	output_indent ("  {");
	if (stmt) {
		output_stmt (stmt);
	}
	output_line ("break;");
	output_indent ("  }");

	/* WHEN test */
	output_stmt (whens);
	output_line ("else");
	output_indent ("  {");
	output_prefix ();
	output_integer (idx);
	output ("++;\n");
	if (var && var != idx) {
		output_move (idx, var);
	}
	output_line ("continue;");
	output_indent ("  }");
	output_line ("break;");
	output_indent ("  }");
}

static void
output_search_all (cb_tree table, cb_tree stmt, cb_tree cond, cb_tree when)
{
	struct cb_field *p = cb_field (table);
	cb_tree		idx = CB_VALUE (p->index_list);

	/* header */
	output_indent ("{");
	output_line ("int ret;");
	output_line ("int head = %d - 1;", p->occurs_min);
	output_prefix ();
	output ("int tail = ");
	output_occurs (p);
	output (" + 1;\n");

	/* start loop */
	output_line ("while (1)");
	output_indent ("  {");

	/* end test */
	output_line ("if (head >= tail - 1)");
	output_indent ("  {");
	if (stmt) {
		output_stmt (stmt);
	}
	output_line ("break;");
	output_indent ("  }");

	/* next index */
	output_prefix ();
	output_integer (idx);
	output (" = (head + tail) / 2;\n");

	/* WHEN test */
	output ("if (");
	output_cond (cond, 1);
	output (")\n");
	output_indent_level += 2;
	output_stmt (when);
	output_indent_level -= 2;
	output_line ("else");
	output_indent ("  {");
	output_line ("if (ret < 0)");
	output_prefix ();
	output ("  head = ");
	output_integer (idx);
	output (";\n");
	output_line ("else");
	output_prefix ();
	output ("  tail = ");
	output_integer (idx);
	output (";\n");
	output_line ("continue;");
	output_indent ("  }");
	output_line ("break;");
	output_indent ("  }");
	output_indent ("}");
}

static void
output_search (struct cb_search *p)
{
	if (p->flag_all) {
		output_search_all (p->table, p->end_stmt,
				   CB_IF (p->whens)->test, CB_IF (p->whens)->stmt1);
	} else {
		output_search_whens (p->table, p->var, p->end_stmt, p->whens);
	}
}

/*
 * CALL
 */

static void
output_call (struct cb_call *p)
{
	cb_tree			x;
	cb_tree			l;
	struct cb_literal	*lp;
	char			*callp;
	char			*system_call = NULL;
	struct system_table	*psyst;
	int			dynamic_link = 1;
	size_t			n;
	size_t			parmnum;

	/* User defined entry points */
	if (CB_LITERAL_P (p->name)) {
		lp = CB_LITERAL (p->name);
/* Prepare MF funnies
#ifndef	__MVS__
		if (lp->size == 1 &&
		    (*(lp->data) == 0x91 || *(lp->data) == 0xA7
		     || *(lp->data) == 0xAF || *(lp->data) == 0xB0
		     || *(lp->data) == 0xE5 || *(lp->data) == 0xF4
		     || *(lp->data) == 0xF5)) {
			dynamic_link = 0;
			system_call = 1;
		}
#endif
*/
		if (p->is_system) {
			psyst = (struct system_table *)&system_tab[0];
			for (; psyst->syst_name; psyst++) {
				if (!strcmp((char *)lp->data, (char *)psyst->syst_name)) {
					system_call = (char *)psyst->syst_call;
					dynamic_link = 0;
					break;
				}
			}
		} else {
			for (n = 0; n < CALLTABSIZE && calltab[n]; n++) {
				if (strcmp ((char *)(lp->data), calltab[n]) == 0) {
					dynamic_link = 0;
					break;
				}
			}
		}
	}

	if ((cb_flag_static_call == 1) && CB_LITERAL_P (p->name)) {
		dynamic_link = 0;
	}

	/* local variables */
	output_indent ("{");
	if (dynamic_link) {
		output_line ("int (*func)();");
	}

	if (CB_REFERENCE_P (p->name)
	    && CB_FIELD_P (CB_REFERENCE (p->name)->value)
	    && CB_FIELD (CB_REFERENCE (p->name)->value)->usage == CB_USAGE_PROGRAM_POINTER) {
		dynamic_link = 0;
	}

	/* setup arguments */
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_CONTENT:
			output_prefix ();
			output ("unsigned char content_%d[", (int)n);
			if (CB_NUMERIC_LITERAL_P (x) || CB_BINARY_OP_P (x) || x == cb_null) {
				output ("4");
			} else {
				output_size (x);
			}
			output ("];\n");
			break;
		}
	}
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_CONTENT:
			output_prefix ();
			if (CB_NUMERIC_LITERAL_P (x) || x == cb_null
			    || (CB_TREE_CATEGORY (x) == CB_CATEGORY_NUMERIC
				&& cb_field (x)->usage == CB_USAGE_LENGTH)) {
				output ("*(int *)content_%d = ", (int)n);
				output_integer (x);
				output (";\n");
			} else {
				output ("memcpy (content_%d, ", (int)n);
				output_data (x);
				output (", ");
				output_size (x);
				output (");\n");
			}
		}
	}

	/* function name */
	n = 0;
	for (l = p->args; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		field_iteration = n;
		output_prefix ();
		output ("module.cob_procedure_parameters[%d] = ", (int)n);
		switch (CB_TREE_TAG (x)) {
		case CB_TAG_LITERAL:
		case CB_TAG_FIELD:
		case CB_TAG_INTRINSIC:
			output_param (x, -1);
			break;
		case CB_TAG_REFERENCE:
			switch (CB_TREE_TAG (CB_REFERENCE(x)->value)) {
			case CB_TAG_LITERAL:
			case CB_TAG_FIELD:
			case CB_TAG_INTRINSIC:
				output_param (x, -1);
				break;
			default:
				output ("NULL");
				break;
			}
			break;
		default:
			output ("NULL");
			break;
		}
		output (";\n");
	}
	parmnum = n;
	output_prefix ();
	output ("cob_call_params = %d;\n", (int)n);
	output_prefix ();
	if (!dynamic_link) {
		if (CB_REFERENCE_P (p->name) && CB_FIELD_P (CB_REFERENCE (p->name)->value) &&
		    CB_FIELD (CB_REFERENCE (p->name)->value)->usage ==
		    CB_USAGE_PROGRAM_POINTER) {
			output ("func = ");
			output_integer (p->name);
			output (";\n");
			output_prefix ();
			output_integer (cb_return_code);
			output (" = func");
		} else {
			/* static link */
			output_integer (cb_return_code);
			if (system_call) {
				output (" = %s", system_call);
			} else {
				output (" = %s",
					cb_encode_program_id ((char *)(CB_LITERAL (p->name)->data)));
			}
		}
	} else {
		/* dynamic link */
		output ("func = ");
		if (!p->stmt1) {
			if (CB_LITERAL_P (p->name)) {
				if (cb_flag_static_call == 2) {
					callp = cb_encode_program_id ((char *)(CB_LITERAL (p->name)-> data));
					lookup_call (callp, (char *)(CB_LITERAL (p->name)->data));
					output ("call_%s", callp);
				} else {
					output ("cob_resolve_1 ((const char *)\"%s\")", CB_LITERAL (p->name)->data);
				}
			} else {
				output_funcall (cb_build_funcall_1 ("cob_call_resolve_1", p->name));
			}
			output (";\n");
		} else {
			if (CB_LITERAL_P (p->name)) {
				output ("cob_resolve ((const char *)\"%s\")", CB_LITERAL (p->name)->data);
			} else {
				output_funcall (cb_build_funcall_1 ("cob_call_resolve", p->name));
			}
			output (";\n");
			output_line ("if (unlikely(func == NULL))");
			output_indent_level += 2;
			output_stmt (p->stmt1);
			output_indent_level -= 2;
			output_line ("else");
			output_indent ("  {");
		}
		output_prefix ();
		output_integer (cb_return_code);
		output (" = func");
	}

	/* arguments */
	output (" (");
	for (l = p->args, n = 1; l; l = CB_CHAIN (l), n++) {
		x = CB_VALUE (l);
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_REFERENCE:
			if (CB_REFERENCE_P (x) && CB_FILE_P (cb_ref (x))) {
				output_param (cb_ref (x), -1);
			} else {
				output_data (x);
			}
			break;
		case CB_CALL_BY_CONTENT:
			output ("content_%d", (int)n);
			break;
		case CB_CALL_BY_VALUE:
			switch (CB_TREE_TAG (x)) {
			case CB_TAG_LITERAL:
				if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
					output ("%d", cb_get_int (x));
				} else {
					output ("%d", CB_LITERAL (x)->data[0]);
				}
				break;
			default:
				switch (cb_field (x)->usage) {
				case CB_USAGE_BINARY:
				case CB_USAGE_COMP_5:
				case CB_USAGE_COMP_X:
				case CB_USAGE_INDEX:
					output_integer (x);
					break;
				case CB_USAGE_LENGTH:
					output_integer (x);
					break;
				default:
					output ("*(");
					output_data (x);
					output (")");
					break;
				}
				break;
			}
			break;
		}
		if (CB_CHAIN (l)) {
			output (", ");
		}
	}
	if (cb_sticky_linkage && parmnum < 16 && !system_call) {
		for (n = parmnum; n < 16; n++) {
			if (n != 0) {
				output (", ");
			}
			output ("NULL");
		}
	}
	output (");\n");
	if (p->returning) {
		output_stmt (cb_build_move (cb_return_code, p->returning));
	}
	if (p->stmt2) {
		output_stmt (p->stmt2);
	}
	if (dynamic_link && p->stmt1) {
		output_indent ("  }");
	}
	output_indent ("}");
}

/*
 * GO TO
 */

static void
output_goto_1 (cb_tree x)
{
	output_line ("goto %s%d;", CB_PREFIX_LABEL, CB_LABEL (cb_ref (x))->id);
}

static void
output_goto (struct cb_goto *p)
{
	cb_tree l;
	int	i = 1;

	if (p->depending) {
		output_prefix ();
		output ("switch (");
		output_param (cb_build_cast_integer (p->depending), 0);
		output (")\n");
		output_indent ("  {");
		for (l = p->target; l; l = CB_CHAIN (l)) {
			output_indent_level -= 2;
			output_line ("case %d:", i++);
			output_indent_level += 2;
			output_goto_1 (CB_VALUE (l));
		}
		output_indent ("  }");
	} else if (p->target == NULL) {
		needs_exit_prog = 1;
		if (cb_implicit_init) {
			output_line ("goto exit_program;");
		} else {
			output_line ("if (module.next)");
			output_line ("  goto exit_program;");
		}
	} else if (p->target == cb_int1) {
		needs_exit_prog = 1;
		output_line ("goto exit_program;");
	} else {
		output_goto_1 (p->target);
	}
}

/*
 * PERFORM
 */

static void
output_perform_call (struct cb_label *lb, struct cb_label *le)
{
#ifndef	__GNUC__
	struct label_list *l;
#endif

	output_line ("/* PERFORM %s THRU %s */", lb->name, le->name);
	if (!cb_stack_check) {
		output_line ("frame_index++;");
	} else {
		output_line ("if (unlikely(++frame_index >= COB_STACK_SIZE))");
		output_line ("    cob_fatal_error (COB_FERROR_STACK);");
	}
	output_line ("frame_stack[frame_index].perform_through = %d;", le->id);
#ifndef	__GNUC__
	l = cobc_malloc (sizeof (struct label_list));
	l->next = label_cache;
	l->id = cb_id;
	if (label_cache == NULL) {
		l->call_num = 0;
	} else {
		l->call_num = label_cache->call_num + 1;
	}
	label_cache = l;
	output_line ("frame_stack[frame_index].return_address = %d;", l->call_num);
	output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
	output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
#elif	COB_USE_SETJMP
	output_line ("if (setjmp (frame_stack[frame_index].return_address) == 0)");
	output_line ("  goto %s%d;", CB_PREFIX_LABEL, lb->id);
#else
	output_line ("frame_stack[frame_index].return_address = &&%s%d;",
		     CB_PREFIX_LABEL, cb_id);
	output_line ("goto %s%d;", CB_PREFIX_LABEL, lb->id);
	output_line ("%s%d:", CB_PREFIX_LABEL, cb_id);
#endif
	cb_id++;
	output_line ("frame_index--;");
}

static void
output_perform_exit (struct cb_label *l)
{
	output_line ("if (frame_stack[frame_index].perform_through == %d)", l->id);
#ifndef	__GNUC__
	output_line ("  goto PSWITCH;");
#elif	COB_USE_SETJMP
	output_line ("  longjmp (frame_stack[frame_index].return_address, 1);");
#else
	output_line ("  goto *frame_stack[frame_index].return_address;");
#endif
	if (cb_perform_osvs) {
		output_line
		    ("for (temp_index = frame_index - 1; temp_index >= 0; temp_index--) {");
		output_line ("  if (frame_stack[temp_index].perform_through == %d) {", l->id);
		output_line ("    frame_index = temp_index;");
#ifndef	__GNUC__
		output_line ("    goto PSWITCH;");
#elif	COB_USE_SETJMP
		output_line ("    longjmp (frame_stack[frame_index].return_address, 1);");
#else
		output_line ("    goto *frame_stack[frame_index].return_address;");
#endif
		output_line ("  }");
		output_line ("}");
	}
}

static void
output_perform_once (struct cb_perform *p)
{
	if (p->body && CB_PAIR_P (p->body)) {
		output_perform_call (CB_LABEL (cb_ref (CB_PAIR_X (p->body))),
				     CB_LABEL (cb_ref (CB_PAIR_Y (p->body))));
	} else {
		output_stmt (p->body);
	}
	if (p->cycle_label) {
		output_stmt (cb_ref (p->cycle_label));
	}
}

static void
output_perform_until (struct cb_perform *p, cb_tree l)
{
	struct cb_perform_varying	*v;
	cb_tree				next;

	if (l == NULL) {
		/* perform body at the end */
		output_perform_once (p);
		return;
	}

	v = CB_PERFORM_VARYING (CB_VALUE (l));
	next = CB_CHAIN (l);

	output_line ("while (1)");
	output_indent ("  {");

	if (next && CB_PERFORM_VARYING (CB_VALUE (next))->name) {
		output_move (CB_PERFORM_VARYING (CB_VALUE (next))->from,
			     CB_PERFORM_VARYING (CB_VALUE (next))->name);
	}

	if (p->test == CB_AFTER) {
		output_perform_until (p, next);
	}

	output_prefix ();
	output ("if (");
	output_cond (v->until, 0);
	output (")\n");
	output_line ("  break;");

	if (p->test == CB_BEFORE) {
		output_perform_until (p, next);
	}

	if (v->step) {
		output_stmt (v->step);
	}

	output_indent ("  }");
}

static void
output_perform (struct cb_perform *p)
{
	struct cb_perform_varying *v;

	switch (p->type) {
	case CB_PERFORM_EXIT:
		if (CB_LABEL (p->data)->need_return) {
			output_perform_exit (CB_LABEL (p->data));
		}
		break;
	case CB_PERFORM_ONCE:
		output_perform_once (p);
		break;
	case CB_PERFORM_TIMES:
		output_prefix ();
		output ("for (n%d = ", loop_counter);
		output_param (cb_build_cast_integer (p->data), 0);
		output ("; n%d > 0; n%d--)\n", loop_counter, loop_counter);
		loop_counter++;
		output_indent ("  {");
		output_perform_once (p);
		output_indent ("  }");
		break;
	case CB_PERFORM_UNTIL:
		v = CB_PERFORM_VARYING (CB_VALUE (p->varying));
		if (v->name) {
			output_move (v->from, v->name);
		}
		output_perform_until (p, p->varying);
		break;
	}
	if (p->exit_label) {
		output_stmt (cb_ref (p->exit_label));
	}
}

/*
 * Output statement
 */

static void
output_stmt (cb_tree x)
{
	struct cb_statement	*p;
	struct cb_label		*lp;
	struct cb_assign	*ap;
	struct cb_if		*ip;
#ifdef	COB_NON_ALIGNED
	struct cb_cast		*cp;
#endif
	int			code;

	stack_id = 0;
	if (x == NULL) {
		output_line (";");
		return;
	}
#ifndef __GNUC__
	if (inside_check != 0) {
		if (inside_stack[inside_check - 1] != 0) {
			inside_stack[inside_check -1] = 0;
			output (",\n");
		}
	}
#endif

	switch (CB_TREE_TAG (x)) {
	case CB_TAG_STATEMENT:
		p = CB_STATEMENT (x);
		/* output source location as a comment */
		if (p->name) {
			output_line ("/* %s:%d: %s */",
				     x->source_file, x->source_line, p->name);
		}
		/* output source location as a code */
		if (x->source_file && last_line != x->source_line) {
			if (cb_flag_line_directive) {
				output ("#line %d \"%s\"\n", x->source_line, x->source_file);
			}
			if (cb_flag_source_location) {
				output_line ("cob_current_program_id = \"%s\";", excp_current_program_id);
				output_line ("cob_source_file = \"%s\";", x->source_file);
				output_line ("cob_source_line = %d;", x->source_line);
				if (excp_current_section) {
					output_line ("cob_current_section = \"%s\";", excp_current_section);
				} else {
					output_line ("cob_current_section = NULL;");
				}
				if (excp_current_paragraph) {
					output_line ("cob_current_paragraph = \"%s\";", excp_current_paragraph);
				} else {
					output_line ("cob_current_paragraph = NULL;");
				}
				if (p->name) {
					output_line ("cob_source_statement = \"%s\";", p->name);
				}
			}
			last_line = x->source_line;
		}

		if (p->handler1 || p->handler2 || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O))) {

			output_line ("cob_exception_code = 0;");
/* RXW - Dirty must be some other way */
			if (p->handler_id == COB_EC_I_O_EOP) {
				output_line ("cob_check_eop = 1;");
			}
		}

		if (p->body) {
			output_stmt (p->body);
		}

		if (p->handler1 || p->handler2 || (p->file && CB_EXCEPTION_ENABLE (COB_EC_I_O))) {
			code = CB_EXCEPTION_CODE (p->handler_id);
			if (p->handler1) {
				if ((code & 0x00ff) == 0) {
					output_line ("if (unlikely((cob_exception_code & 0xff00) == 0x%04x))",
					     code);
				} else {
					output_line ("if (unlikely(cob_exception_code == 0x%04x))", code);
				}
				output_indent ("  {");
				output_stmt (p->handler1);
				output_indent ("  }");
				if (p->handler2 || p->file) {
					output_line ("else");
				}
			}
			if (p->file) {
				output_line ("if (unlikely(cob_exception_code != 0))");
				output_indent ("  {");
				output_perform_call (CB_FILE (p->file)->handler,
						     CB_FILE (p->file)->handler);
				output_indent ("  }");
				if (p->handler2 || p->handler3) {
					output_line ("else");
				}
			}
			if (p->handler2) {
				if (p->handler1 == 0 && p->file == 0) {
					output_line ("if (!cob_exception_code)");
				}
				output_indent ("  {");
				if (p->handler3) {
					output_stmt (p->handler3);
				}
				output_stmt (p->handler2);
				output_indent ("  }");
			} else if (p->handler3) {
				output_stmt (p->handler3);
			}
		}
		break;
	case CB_TAG_LABEL:
		lp = CB_LABEL (x);
		output_newline ();
		if (lp->is_section) {
			if (strcmp ((char *)(lp->name) , "MAIN SECTION")) {
				output_line ("/* %s SECTION: */", lp->name);
			} else {
				output_line ("/* %s: */", lp->name);
			}
			excp_current_section = (const char *)lp->name;
			excp_current_paragraph = NULL;
		} else {
			output_line ("/* %s: */", lp->name);
			excp_current_paragraph = (const char *)lp->name;
		}
		if (lp->need_begin) {
			output_line ("%s%d:;", CB_PREFIX_LABEL, lp->id);
		}
		if (cb_flag_trace) {
			if (lp->is_section) {
				if (strcmp ((char *)(lp->name) , "MAIN SECTION")) {
					output_line ("fputs (\"%s:%s SECTION\\n\", stderr);", excp_current_program_id, lp->orig_name);
				} else {
					output_line ("fputs (\"%s:%s\\n\", stderr);", excp_current_program_id, lp->orig_name);
				}
			} else if (lp->is_entry) {
				output_line ("fputs (\"%s:ENTRY %s\\n\", stderr);", excp_current_program_id, lp->orig_name);
			} else {
				output_line ("fputs (\"%s:%s\\n\", stderr);", excp_current_program_id, lp->orig_name);
			}
			output_line ("fflush (stderr);");
		}
		break;
	case CB_TAG_FUNCALL:
		output_prefix ();
		output_funcall (x);
#ifdef __GNUC__
		output (";\n");
#else
		if (inside_check == 0) {
			output (";\n");
		} else {
			inside_stack[inside_check -1] = 1;
		}
#endif
		break;
	case CB_TAG_ASSIGN:
		ap = CB_ASSIGN (x);
#ifdef	COB_NON_ALIGNED
		/* Nonaligned */
		if (CB_TREE_CLASS (ap->var) == CB_CLASS_POINTER
		    || CB_TREE_CLASS (ap->val) == CB_CLASS_POINTER) {
			/* pointer assignment */
			output_indent ("{");
			output_line ("void *temp_ptr;");

			/* temp_ptr = source address; */
			output_prefix ();
			if (ap->val == cb_null || ap->val == cb_zero) {
				/* MOVE NULL ... */
				output ("temp_ptr = 0;\n");
			} else if (CB_TREE_TAG (ap->val) == CB_TAG_CAST) {
				/* MOVE ADDRESS OF val ... */
				cp = CB_CAST (ap->val);
				output ("temp_ptr = ");
				switch (cp->type) {
				case CB_CAST_ADDRESS:
					output_data (cp->val);
					break;
				case CB_CAST_PROGRAM_POINTER:
					output_func_1 ("cob_call_resolve", ap->val);
					break;
				default:
					fprintf (stderr, "Unexpected cast type %d\n", cp->type);
					ABORT ();
				}
				output (";\n");
			} else {
				/* MOVE val ... */
				output ("memcpy(&temp_ptr, ");
				output_data (ap->val);
				output (", sizeof(temp_ptr));\n");
			}

			/* destination address = temp_ptr; */
			output_prefix ();
			if (CB_TREE_TAG (ap->var) == CB_TAG_CAST) {
				/* SET ADDRESS OF var ... */
				cp = CB_CAST (ap->var);
				if (cp->type != CB_CAST_ADDRESS) {
					fprintf (stderr, "Unexpected tree type %d\n", cp->type);
					ABORT ();
				}
				output_data (cp->val);
				output (" = temp_ptr;\n");
			} else {
				/* MOVE ... TO var */
				output ("memcpy(");
				output_data (ap->var);
				output (", &temp_ptr, sizeof(temp_ptr));\n");
			}

			output_indent ("}");
		} else {
			/* numeric assignment */
			output_prefix ();
			output_integer (ap->var);
			output (" = ");
			output_integer (ap->val);
#ifdef __GNUC__
			output (";\n");
#else
			if (inside_check == 0) {
				output (";\n");
			} else {
				inside_stack[inside_check -1] = 1;
			}
#endif
		}
#else	/* Nonaligned */
		output_prefix ();
		output_integer (ap->var);
		output (" = ");
		output_integer (ap->val);
#ifdef __GNUC__
		output (";\n");
#else
		if (inside_check == 0) {
			output (";\n");
		} else {
			inside_stack[inside_check -1] = 1;
		}
#endif
#endif	/* Nonaligned */
		break;
	case CB_TAG_INITIALIZE:
		output_initialize (CB_INITIALIZE (x));
		break;
	case CB_TAG_SEARCH:
		output_search (CB_SEARCH (x));
		break;
	case CB_TAG_CALL:
		output_call (CB_CALL (x));
		break;
	case CB_TAG_GOTO:
		output_goto (CB_GOTO (x));
		break;
	case CB_TAG_IF:
		ip = CB_IF (x);
		output_prefix ();
		output ("if (");
		output_cond (ip->test, 0);
		output (")\n");
		if (ip->stmt1) {
			output_indent_level += 2;
			output_stmt (ip->stmt1);
			output_indent_level -= 2;
		} else {
			output_line ("  /* nothing */;");
		}
		if (ip->stmt2) {
			output_line ("else");
			output_indent_level += 2;
			output_stmt (ip->stmt2);
			output_indent_level -= 2;
		}
		break;
	case CB_TAG_PERFORM:
		output_perform (CB_PERFORM (x));
		break;
	case CB_TAG_CONTINUE:
		output_prefix ();
		output (";\n");
		break;
	case CB_TAG_LIST:
		output_indent ("{");
		for (; x; x = CB_CHAIN (x)) {
			output_stmt (CB_VALUE (x));
		}
		output_indent ("}");
		break;
	default:
		fprintf (stderr, "Unexpected tree tag %d\n", CB_TREE_TAG (x));
		ABORT ();
	}
}

/*
 * File definition
 */

static void
output_file_allocation (struct cb_file *f)
{

	/* output RELATIVE/RECORD KEY's */
	if (f->organization == COB_ORG_RELATIVE || f->organization == COB_ORG_INDEXED) {
		output ("  static cob_file_key *%s%s = NULL;\n", CB_PREFIX_KEYS, f->cname);
	}
	output ("  static cob_file *%s%s = NULL;\n", CB_PREFIX_FILE, f->cname);
	output ("  static char %s%s_status[4];\n", CB_PREFIX_FILE, f->cname);
}

static void
output_file_initialization (struct cb_file *f)
{
	int			nkeys = 1;
	struct cb_alt_key	*l;

	if (f->external) {
		output_line ("%s%s = (cob_file *)cob_external_addr (\"%s\", sizeof(cob_file));",
			     CB_PREFIX_FILE, f->cname, f->cname);
		output_line ("if (cob_initial_external)");
		output_indent ("{");
		if (f->linage) {
			output_line ("%s%s->linptr = cob_malloc (sizeof(struct linage_struct));", CB_PREFIX_FILE, f->cname);
		}
	} else {
		output_line ("if (!%s%s)", CB_PREFIX_FILE, f->cname);
		output_indent ("{");
		output_line ("%s%s = cob_malloc (sizeof(cob_file));", CB_PREFIX_FILE, f->cname);
		if (f->linage) {
			output_line ("%s%s->linptr = cob_malloc (sizeof(struct linage_struct));", CB_PREFIX_FILE, f->cname);
		}
		output_indent ("}");
	}
	/* output RELATIVE/RECORD KEY's */
	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED) {
		for (l = f->alt_key_list; l; l = l->next) {
			nkeys++;
		}
		output_line ("if (!%s%s)", CB_PREFIX_KEYS, f->cname);
		output_indent ("{");
		output_line ("%s%s = cob_malloc (sizeof (cob_file_key) * %d);",
			     CB_PREFIX_KEYS, f->cname, nkeys);
		output_indent ("}");
		nkeys = 1;
		output_prefix ();
		output ("%s%s->field = ", CB_PREFIX_KEYS, f->cname);
		output_param (f->key, -1);
		output (";\n");
		output_prefix ();
		output ("%s%s->flag = 0;\n", CB_PREFIX_KEYS, f->cname);
		output_prefix ();
		if (f->key) {
			output ("%s%s->offset = %d;\n", CB_PREFIX_KEYS, f->cname,
				cb_field(f->key)->offset);
		} else {
			output ("%s%s->offset = 0;\n", CB_PREFIX_KEYS, f->cname);
		}
		for (l = f->alt_key_list; l; l = l->next) {
			output_prefix ();
			output ("(%s%s + %d)->field = ", CB_PREFIX_KEYS, f->cname,
				nkeys);
			output_param (l->key, -1);
			output (";\n");
			output_prefix ();
			output ("(%s%s + %d)->flag = %d;\n", CB_PREFIX_KEYS, f->cname,
				nkeys, l->duplicates);
			output_prefix ();
			output ("(%s%s + %d)->offset = %d;\n", CB_PREFIX_KEYS, f->cname,
				nkeys, cb_field(l->key)->offset);
			nkeys++;
		}
	}

	output_line ("%s%s->select_name = (const char *)\"%s\";", CB_PREFIX_FILE, f->cname, f->name);
	if (f->external && !f->file_status) {
		output_line ("%s%s->file_status = cob_external_addr (\"%s%s_status\", 4);",
			     CB_PREFIX_FILE, f->cname, CB_PREFIX_FILE, f->cname);
	} else {
		output_line ("%s%s->file_status = %s%s_status;", CB_PREFIX_FILE, f->cname,
			     CB_PREFIX_FILE, f->cname);
		output_line ("memset (%s%s_status, '0', 2);", CB_PREFIX_FILE, f->cname);
	}
	output_prefix ();
	output ("%s%s->assign = ", CB_PREFIX_FILE, f->cname);
	if (f->special) {
		output ("NULL");
	} else {
		output_param (f->assign, -1);
	}
	output (";\n");
	output_prefix ();
	output ("%s%s->record = ", CB_PREFIX_FILE, f->cname);
	output_param (CB_TREE (f->record), -1);
	output (";\n");
	output_prefix ();
	output ("%s%s->record_size = ", CB_PREFIX_FILE, f->cname);
	if (f->record_depending) {
		output_param (f->record_depending, -1);
	} else {
		output ("NULL");
	}
	output (";\n");
	output_line ("%s%s->record_min = %d;", CB_PREFIX_FILE, f->cname, f->record_min);
	output_line ("%s%s->record_max = %d;", CB_PREFIX_FILE, f->cname, f->record_max);
	if (f->organization == COB_ORG_RELATIVE
	 || f->organization == COB_ORG_INDEXED) {
		output_line ("%s%s->nkeys = %d;", CB_PREFIX_FILE, f->cname, nkeys);
		output_line ("%s%s->keys = %s%s;", CB_PREFIX_FILE, f->cname, CB_PREFIX_KEYS,
			     f->cname);
	} else {
		output_line ("%s%s->nkeys = 0;", CB_PREFIX_FILE, f->cname);
		output_line ("%s%s->keys = NULL;", CB_PREFIX_FILE, f->cname);
	}
	output_line ("%s%s->file = NULL;", CB_PREFIX_FILE, f->cname);

	if (f->linage) {
		output_prefix ();
		output ("%s%s->linptr->linage = ", CB_PREFIX_FILE, f->cname);
		output_param (f->linage, -1);
		output (";\n");
		output_prefix ();
		output ("%s%s->linptr->linage_ctr = ", CB_PREFIX_FILE, f->cname);
		output_param (f->linage_ctr, -1);
		output (";\n");
		if (f->latfoot) {
			output_prefix ();
			output ("%s%s->linptr->latfoot = ", CB_PREFIX_FILE, f->cname);
			output_param (f->latfoot, -1);
			output (";\n");
		} else {
			output_line ("%s%s->linptr->latfoot = NULL;", CB_PREFIX_FILE, f->cname);
		}
		if (f->lattop) {
			output_prefix ();
			output ("%s%s->linptr->lattop = ", CB_PREFIX_FILE, f->cname);
			output_param (f->lattop, -1);
			output (";\n");
		} else {
			output_line ("%s%s->linptr->lattop = NULL;", CB_PREFIX_FILE, f->cname);
		}
		if (f->latbot) {
			output_prefix ();
			output ("%s%s->linptr->latbot = ", CB_PREFIX_FILE, f->cname);
			output_param (f->latbot, -1);
			output (";\n");
		} else {
			output_line ("%s%s->linptr->latbot = NULL;", CB_PREFIX_FILE, f->cname);
		}
		output_line ("%s%s->linptr->lin_lines = 0;", CB_PREFIX_FILE, f->cname);
		output_line ("%s%s->linptr->lin_foot = 0;", CB_PREFIX_FILE, f->cname);
		output_line ("%s%s->linptr->lin_top = 0;", CB_PREFIX_FILE, f->cname);
		output_line ("%s%s->linptr->lin_bot = 0;", CB_PREFIX_FILE, f->cname);
	}

	output_line ("%s%s->organization = %d;", CB_PREFIX_FILE, f->cname, f->organization);
	output_line ("%s%s->access_mode = %d;", CB_PREFIX_FILE, f->cname, f->access_mode);
	output_line ("%s%s->lock_mode = %d;", CB_PREFIX_FILE, f->cname, f->lock_mode);
	output_line ("%s%s->open_mode = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_optional = %d;", CB_PREFIX_FILE, f->cname, f->optional);
	output_line ("%s%s->last_open_mode = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->special = %d;", CB_PREFIX_FILE, f->cname, f->special);
	output_line ("%s%s->flag_nonexistent = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_end_of_file = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_begin_of_file = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_first_read = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_read_done = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_select_features = %d;", CB_PREFIX_FILE, f->cname,
		     ((f->file_status ? COB_SELECT_FILE_STATUS : 0) |
		     (f->external_assign ? COB_SELECT_EXTERNAL : 0)));
	output_line ("%s%s->flag_needs_nl = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->flag_needs_top = 0;", CB_PREFIX_FILE, f->cname);
	output_line ("%s%s->file_version = %d;", CB_PREFIX_FILE, f->cname, COB_FILE_VERSION);
	if (f->external) {
		output_indent ("}");
	}
}

/*
 * Screen definition
 */

static void
output_screen_definition (struct cb_field *p)
{
	int type;

	if (p->sister) {
		output_screen_definition (p->sister);
	}
	if (p->children) {
		output_screen_definition (p->children);
	}

	type = (p->children ? COB_SCREEN_TYPE_GROUP :
		p->values ? COB_SCREEN_TYPE_VALUE :
		(p->size > 0) ? COB_SCREEN_TYPE_FIELD : COB_SCREEN_TYPE_ATTRIBUTE);
	output ("cob_screen s_%d = {%d, ", p->id, type);

	if (type == COB_SCREEN_TYPE_GROUP) {
		output ("&s_%d, ", p->children->id);
	} else {
		output ("NULL, ");
	}
	if (type == COB_SCREEN_TYPE_FIELD) {
		p->count++;
		output_param (cb_build_field_reference (p, NULL), -1);
		output (", ");
	} else {
		output ("NULL, ");
	}
	if (type == COB_SCREEN_TYPE_VALUE) {
		output_param (CB_VALUE(p->values), p->id);
		output (", ");
	} else {
		output ("NULL, ");
	}

	if (p->sister) {
		output ("&s_%d, ", p->sister->id);
	} else {
		output ("NULL, ");
	}
	if (p->screen_from) {
		if (CB_FIELD_P(p->screen_from) || CB_REFERENCE_P(p->screen_from)) {
			cb_field (p->screen_from)->count++;
			output_param (cb_build_field_reference (cb_field (p->screen_from),
					NULL), cb_field (p->screen_from)->id);
		} else {
			output_param (p->screen_from, 0);
		}
		output (", ");
	} else {
		output ("NULL, ");
	}
	if (p->screen_to) {
		cb_field (p->screen_to)->count++;
		output_param (cb_build_field_reference (cb_field(p->screen_to), NULL),
				cb_field (p->screen_to)->id);
		output (", ");
	} else {
		output ("NULL, ");
	}
	output_param (p->screen_line, 0);
	output (", ");
	output_param (p->screen_column, 0);
	output (", %ld};\n", p->screen_flag);
}

/*
 * Alphabet-name
 */

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
		return cb_get_int (x) - 1;
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
output_alphabet_name_definition (struct cb_alphabet_name *p)
{
	cb_tree		l;
	cb_tree		ls;
	cb_tree		x;
	unsigned char	*data;
	size_t		i;
	size_t		n = 0;
	size_t		size;
	size_t		upper;
	size_t		lower;
	int		table[256];

	/* reset by -1 */
	for (i = 0; i < 256; i++) {
		table[i] = -1;
	}

	for (l = p->custom_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_PAIR_P (x)) {
			/* X THRU Y */
			lower = literal_value (CB_PAIR_X (x));
			upper = literal_value (CB_PAIR_Y (x));
			for (i = lower; i <= upper; i++) {
				table[i] = n++;
			}
		} else if (CB_LIST_P (x)) {
			/* X ALSO Y ... */
			for (ls = x; ls; ls = CB_CHAIN (ls)) {
				table[literal_value (CB_VALUE (ls))] = n;
			}
			n++;
		} else {
			/* literal */
			if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
				table[literal_value (x)] = n++;
			} else {
				size = CB_LITERAL (x)->size;
				data = CB_LITERAL (x)->data;
				for (i = 0; i < size; i++) {
					table[data[i]] = n++;
				}
			}
		}
	}

	/* fill the rest of characters */
	for (i = 0; i < 256; i++) {
		if (table[i] == -1) {
			table[i] = n++;
		}
	}

	/* output the table */
	output ("static const unsigned char %s%s[256] = {\n", CB_PREFIX_SEQUENCE, p->cname);
	for (i = 0; i < 256; i++) {
		output (" %d,", table[i]);
		if (i % 16 == 15) {
			output_newline ();
		}
	}
	output ("};\n\n");
}

/*
 * Class definition
 */

static void
output_class_name_definition (struct cb_class_name *p)
{
	cb_tree		l;
	cb_tree		x;
	unsigned char	*data;
	size_t		i;
	size_t		size;
	int		lower;
	int		upper;

	output_line ("static int");
	output_line ("%s (cob_field *f)", p->cname);
	output_indent ("{");
	output_line ("int i;");
	output_line ("for (i = 0; i < f->size; i++)");
	output_prefix ();
	output ("  if (!(    ");
	for (l = p->list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_PAIR_P (x)) {
			lower = literal_value (CB_PAIR_X (x));
			upper = literal_value (CB_PAIR_Y (x));
			if (!lower) {
				output ("f->data[i] <= %d", upper);
			} else {
				output ("(%d <= f->data[i] && f->data[i] <= %d)", lower, upper);
			}
		} else {
			if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
				output ("f->data[i] == %d", literal_value(x));
			} else {
				size = CB_LITERAL (x)->size;
				data = CB_LITERAL (x)->data;
				for (i = 0; i < size; i++) {
					output ("f->data[i] == %d", data[i]);
					if (i + 1 < size) {
						output (" || ");
					}
				}
			}
		}
		if (CB_CHAIN (l)) {
			output ("\n");
			output_prefix ();
			output ("         || ");
		}
	}
	output (" ))\n");
	output_line ("    return 0;");
	output_line ("return 1;");
	output_indent ("}");
	output_newline ();
}

static void
output_initial_values (struct cb_field *p)
{
	cb_tree x;
	cb_tree def = cb_auto_initialize ? cb_true : NULL;

	for (; p; p = p->sister) {
		x = cb_build_field_reference (p, 0);
		if (p->flag_item_based) {
			continue;
		}
		output_stmt (cb_build_initialize (x, cb_true, NULL, def, 0));
	}
}

static void
output_internal_function (struct cb_program *prog, cb_tree parameter_list)
{
	int			i;
	int			parmnum = 0;
	cb_tree			l;
	struct cb_field		*f;
	struct call_list	*clp;
	struct field_list	*k;
	struct cb_file		*fl;
	char			*p;
#ifndef	__GNUC__
	struct label_list	*pl;
#endif
	char			name[CB_MAX_CNAME];

	/* program function */
	output ("static int\n%s_ (const int entry", prog->program_id);
	if (!prog->is_chained) {
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			output (", unsigned char *%s%d",
				CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id);
			parmnum++;
		}
	}
	output (")\n");
	output_indent ("{");

	/* local variables */
	output_newline ();
	output ("#include \"%s\"  /* local variables */\n", cb_storage_file_name);
	output_newline ();

	output_line ("static int initialized = 0;");
	if (prog->decimal_index_max) {
		for (i = 0; i < prog->decimal_index_max; i++) {
			output_line ("static cob_decimal d%d;", i);
		}
	}

	output_prefix ();
	output ("static cob_field *cob_user_parameters[COB_MAX_FIELD_PARAMS];\n");
	output_prefix ();
	output ("static cob_module module = { NULL, ");
	if (prog->collating_sequence) {
		output_param (cb_ref (prog->collating_sequence), -1);
	} else {
		output ("NULL");
	}
	output (", ");
	if (prog->crt_status) {
		output_param (cb_ref (prog->crt_status), -1);
	} else {
		output ("NULL");
	}
	output (", ");
	if (prog->cursor_pos) {
		output_param (cb_ref (prog->cursor_pos), -1);
	} else {
		output ("NULL");
	}
	output (", cob_user_parameters");

	/* Note spare byte at end */
	output (", %d, '%c', '%c', '%c', %d, %d, %d, 0};\n",
		cb_display_sign, prog->decimal_point, prog->currency_symbol,
		prog->numeric_separator, cb_filename_mapping, cb_binary_truncate,
		cb_pretty_display);
	output_newline ();

	/* External items */
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->flag_external) {
			strcpy (name, f->name);
			for (p = name; *p; p++) {
				if (*p == '-') {
					*p = '_';
				}
			}
			output ("  static unsigned char *%s%s = NULL;", CB_PREFIX_BASE, name);
			output ("  /* %s */\n", f->name);
		}
	}
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		f = CB_FILE (CB_VALUE (l))->record;
		if (f->flag_external) {
			strcpy (name, f->name);
			for (p = name; *p; p++) {
				if (*p == '-') {
					*p = '_';
				}
			}
			output ("  static unsigned char *%s%s = NULL;", CB_PREFIX_BASE, name);
			output ("  /* %s */\n", f->name);
		}
	}
	output_newline ();
	if (cb_sticky_linkage && parmnum) {
		output_line ("/* Sticky linkage save pointers */");
		for (i = 0; i < parmnum; i++) {
			output ("  static unsigned char *cob_parm_%d = NULL;\n", i);
		}
		output_newline ();
	}

	/* files */
	if (prog->file_list) {
		output ("  /* Files */\n\n");
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			output_file_allocation (CB_FILE (CB_VALUE (l)));
		}
		output_newline ();
	}

	if (prog->loop_counter) {
		for (i = 0; i < prog->loop_counter; i++) {
			output_line ("int n%d;", i);
		}
	}

	output_newline ();

	/* BASED working-storage */
	i = 0;
	for (f = prog->working_storage; f; f = f->sister) {
		if (f->flag_item_based) {
			if (!i) {
				i = 1;
				output_line("/* BASED WORKING-STORAGE SECTION */");
			}
			output_line ("static unsigned char *%s%d = NULL; /* %s */",
				CB_PREFIX_BASE, f->id, f->name);
		}
	}
	if (i) {
		output ("\n");
	}

	/* BASED local-storage */
	i = 0;
	for (f = prog->local_storage; f; f = f->sister) {
		if (f->flag_item_based) {
			if (!i) {
				i = 1;
				output_line("/* BASED LOCAL-STORAGE */");
			}
			output_line ("unsigned char *%s%d = NULL; /* %s */",
				CB_PREFIX_BASE, f->id, f->name);
		}
	}
	if (i) {
		output ("\n");
	}

	/* linkage section */
	if (prog->linkage_storage) {
		output_line ("/* LINKAGE SECTION */");
	}
	for (f = prog->linkage_storage; f; f = f->sister) {
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			if (f == cb_field (CB_VALUE (l))) {
				break;
			}
		}
		if (l == NULL) {
			if (cb_sticky_linkage || cb_flag_static_linkage) {
				output_line ("static unsigned char *%s%d = NULL;  /* %s */",
					     CB_PREFIX_BASE, f->id, f->name);
			} else {
				output_line ("unsigned char *%s%d = NULL;  /* %s */",
					     CB_PREFIX_BASE, f->id, f->name);
			}
		}
	}
	output_newline ();

	/* screens */
	if (prog->screen_storage) {
		output ("/* Screens */\n\n");
/*
		for (f = prog->screen_storage; f; f = f->sister) {
			output_screen_definition (f);
		}
*/
		output_screen_definition (prog->screen_storage);
		output_newline ();
	}

	output_line ("/* perform frame stack */");
	if (cb_perform_osvs) {
		output_line ("int temp_index;");
	}
	output_line ("int frame_index;");
#ifndef	__GNUC__
	output_line ("struct frame { int perform_through; int return_address; } "
		     "frame_stack[%d];", COB_STACK_SIZE);
#elif	COB_USE_SETJMP
	output_line ("struct frame { int perform_through; jmp_buf return_address; } "
		     "frame_stack[%d];", COB_STACK_SIZE);
#else
	output_line ("struct frame {");
	output_line ("	int perform_through;");
	output_line ("	void *return_address;");
	output_line ("} frame_stack[%d];", COB_STACK_SIZE);
#endif
	output_newline ();

	output_line ("/* Start of function code */");
#if defined (__GNUC__) && (__GNUC__ >= 3)
	if (cb_flag_static_call == 2) {
		output_line ("auto void init_%s (void);", prog->program_id);
	}
	if (has_external) {
		output_line ("auto void init_external (void);");
	}
#endif
	output_newline ();
	output_line ("if (unlikely(entry < 0)) {");
	output_line ("	if (!initialized) {");
	output_line ("		return 0;");
	output_line ("	}");
	for (l = prog->file_list; l; l = CB_CHAIN (l)) {
		fl = CB_FILE (CB_VALUE (l));
		if (fl->organization != COB_ORG_SORT) {
			output_line ("	cob_close (%s%s, 0, NULL);",
					CB_PREFIX_FILE, fl->cname);
		}
	}
	if (prog->decimal_index_max) {
		for (i = 0; i < prog->decimal_index_max; i++) {
			output_line ("	mpz_clear (d%d.value);", i);
			output_line ("	d%d.scale = 0;", i);
		}
	}
	output_line ("	initialized = 0;");
	output_line ("	return 0;");
	output_line ("}");
	output_newline ();
	if (cb_sticky_linkage && parmnum) {
		output_line ("if (cob_call_params < %d) {", parmnum);
		output_line ("  switch (cob_call_params) {");
		for (i = 0, l = parameter_list; l; l = CB_CHAIN (l), i++) {
			output_line ("  case %d:", i);
			output_line ("   if (cob_parm_%d != NULL)", i);
			output_line ("       %s%d = cob_parm_%d;",
				     CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id, i);
		}
		output_line ("  }");
		output_line ("}");
		for (i = 0, l = parameter_list; l; l = CB_CHAIN (l), i++) {
			output_line ("if (%s%d != NULL)",
				     CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id);
			output_line ("  cob_parm_%d = %s%d;", i,
				     CB_PREFIX_BASE, cb_field (CB_VALUE (l))->id);
		}
		output_newline ();
	}
	output_line ("module.next = cob_current_module;");
	output_line ("cob_current_module = &module;");
	output_newline ();

	/* initialization */
	output_line ("if (unlikely(initialized == 0))");
	output_indent ("  {");
	/* output_stmt (cb_build_assign (cb_return_code, cb_int0)); */
	output_line ("if (!cob_initialized) {");
	if (cb_implicit_init) {
		output_line ("  cob_init (0,NULL);");
	} else {
		output_line ("  cob_fatal_error (COB_FERROR_INITIALIZED);");
	}
	output_line ("}");
	output_line
	    ("cob_check_version (COB_SOURCE_FILE, COB_PACKAGE_VERSION, COB_PATCH_LEVEL);");
	if (!prog->gen_main) {
		if (cb_implicit_init) {
			output_line ("cob_set_cancel ((const char *)\"%s\", (void *)%s, (void *)%s_);",
				prog->orig_source_name, prog->program_id,
				prog->program_id);
		} else {
			output_line ("if (module.next)");
			output_line ("  cob_set_cancel ((const char *)\"%s\", (void *)%s, (void *)%s_);",
				prog->orig_source_name, prog->program_id,
				prog->program_id);
		}
	}
	if (prog->flag_screen) {
		output_line ("cob_screen_init ();");
	}
	if (cb_flag_static_call == 2) {
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("init_%s ();", prog->program_id);
#else
		output_line ("goto L_init_%s;", prog->program_id);
		output_line ("LRET_init_%s: ;", prog->program_id);
#endif
	}
	if (prog->decimal_index_max) {
		output_line ("/* initialize decimal numbers */");
		for (i = 0; i < prog->decimal_index_max; i++) {
			output_line ("cob_decimal_init (&d%d);", i);
		}
		output_newline ();
	}
	if (!prog->flag_initial) {
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			f = CB_FILE (CB_VALUE (l))->record;
			if (f->flag_external) {
				strcpy (name, f->name);
				for (p = name; *p; p++) {
					if (*p == '-') {
						*p = '_';
					}
				}
				output_line ("%s%s = cob_external_addr (\"%s\", %d);",
					     CB_PREFIX_BASE, name, name,
					     CB_FILE (CB_VALUE (l))->record_max);
			}
		}
		output_initial_values (prog->working_storage);
		if (has_external) {
#if defined (__GNUC__) && (__GNUC__ >= 3)
			output_line ("init_external ();");
#else
			output_line ("goto L_init_external;");
			output_line ("LRET_init_external: ;");
#endif
		}
		output_newline ();
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			output_file_initialization (CB_FILE (CB_VALUE (l)));
		}
		output_newline ();
	}

	output_line ("initialized = 1;");
	if (prog->is_chained) {
		output ("    } else {\n");
		output_line ("  cob_fatal_error (COB_FERROR_CHAINING);");
		output_indent ("  }");
	} else {
		output_indent ("  }");
	}
	if (prog->flag_initial) {
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			f = CB_FILE (CB_VALUE (l))->record;
			if (f->flag_external) {
				strcpy (name, f->name);
				for (p = name; *p; p++) {
					if (*p == '-') {
						*p = '_';
					}
				}
				output_line ("%s%s = cob_external_addr (\"%s\", %d);",
					     CB_PREFIX_BASE, name, name,
					     CB_FILE (CB_VALUE (l))->record_max);
			}
		}
		output_initial_values (prog->working_storage);
		if (has_external) {
#if defined (__GNUC__) && (__GNUC__ >= 3)
			output_line ("init_external ();");
#else
			output_line ("goto L_init_external;");
			output_line ("LRET_init_external: ;");
#endif
		}
		output_newline ();
		for (l = prog->file_list; l; l = CB_CHAIN (l)) {
			output_file_initialization (CB_FILE (CB_VALUE (l)));
		}
		output_newline ();
	}
	output_initial_values (prog->local_storage);
	output_newline ();

	output_line ("/* initialize frame stack */");
	output_line ("frame_index = 0;");
	output_line ("frame_stack[0].perform_through = -1;");
	output_newline ();

	output_line ("/* initialize number of call params */");
	output ("  ");
	output_integer (cb_call_params);
	output_line (" = cob_call_params;");
	output_line ("cob_save_call_params = cob_call_params;");
	output_newline ();

	/* entry dispatch */
	if (entry_number > 1) {
		output_line ("switch (entry)");
		output_line ("  {");
		for (i = 0, l = prog->entry_list; l; l = CB_CHAIN (l)) {
			output_line ("  case %d:", i++);
			output_line ("    goto %s%d;",
				     CB_PREFIX_LABEL, CB_LABEL (CB_PURPOSE (l))->id);
		}
		output_line ("  }");
		output_newline ();
	} else {
		l = prog->entry_list;
		output_line ("goto %s%d;", CB_PREFIX_LABEL, CB_LABEL (CB_PURPOSE (l))->id);
		output_newline ();
	}

	/* PROCEDURE DIVISION */
	output_line ("/* PROCEDURE DIVISION */");
	for (l = prog->exec_list; l; l = CB_CHAIN (l)) {
		output_stmt (CB_VALUE (l));
	}
	output_newline ();

	if (needs_exit_prog) {
		output_line ("exit_program:");
	}
	output_line ("cob_current_module = cob_current_module->next;");
	output_prefix ();
	output ("return ");
	output_integer (cb_return_code);
	output (";\n\n");

	/* error handlers */
	if (prog->file_list) {
		output_line ("/* error handlers */");
		output_stmt (cb_standard_error_handler);
		output_line ("switch (cob_error_file->last_open_mode)");
		output_indent ("{");
		for (i = COB_OPEN_INPUT; i <= COB_OPEN_EXTEND; i++) {
			if (prog->file_handler[i]) {
				output_line ("case %d:", i);
				output_indent ("{");
				output_perform_call (prog->file_handler[i],
						     prog->file_handler[i]);
				output_line ("break;");
				output_indent ("}");
			}
		}
		output_line ("default:");
		output_indent ("{");
		output_line ("if (!(cob_error_file->flag_select_features & COB_SELECT_FILE_STATUS)) {");
		output_line ("	cob_default_error_handle ();");
		output_line ("	cob_stop_run (1);");
		output_line ("}");
		output_line ("break;");
		output_indent ("}");
		output_indent ("}");
		output_perform_exit (CB_LABEL (cb_standard_error_handler));
		output_line ("cob_fatal_error (COB_FERROR_CODEGEN);");
		output_newline ();
	}
#ifndef	__GNUC__
	output_line ("PSWITCH:");
	if (label_cache) {
		output_line (" switch (frame_stack[frame_index].return_address) {");
		for (pl = label_cache; pl; pl = pl->next) {
			output_line (" case %d:", pl->call_num);
			output_line ("   goto %s%d;", CB_PREFIX_LABEL, pl->id);
		}
		output_line (" }");
	}
	output_line (" cob_fatal_error (COB_FERROR_CODEGEN);");
	output_newline ();
#endif

	if (cb_flag_static_call == 2) {
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("void init_%s (void)", prog->program_id);
		output_line ("{");
#else
		output_line ("L_init_%s: ;", prog->program_id);
#endif
		for (clp = call_cache; clp; clp = clp->next) {
			output_line ("	call_%s = cob_resolve ((const char *)\"%s\");",
				     clp->callname, clp->callorig);
		}
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("}");
#else
		output_line ("goto LRET_init_%s;", prog->program_id);
#endif
	}

	if (has_external) {
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("void init_external (void)");
		output_line ("{");
#else
		output_line ("L_init_external:");
#endif
		for (k = field_cache; k; k = k->next) {
			if (k->f->flag_item_external) {
				output_prefix ();
				output ("%s%d.data = ", CB_PREFIX_FIELD, k->f->id);
				output_data (k->x);
				output (";\n");
			}
		}
#if defined (__GNUC__) && (__GNUC__ >= 3)
		output_line ("}");
#else
		output_line ("goto LRET_init_external;");
#endif
	}

	output_indent ("}");
	output_newline ();
}

static void
output_entry_function (struct cb_program *prog,
		       cb_tree entry, cb_tree parameter_list, const int gencode)
{

	const unsigned char	*entry_name = CB_LABEL (CB_PURPOSE (entry))->name;
	cb_tree			using_list = CB_VALUE (entry);
	cb_tree			l, l1, l2;
	struct cb_field		*f;
	int			parmnum;

#ifdef	_MSC_VER
	if (!gencode) {
		output ("__declspec(dllexport) ");
	}
#endif
	output ("int");
	if (gencode) {
		output ("\n");
	} else {
		output (" ");
	}
	output ("%s (", entry_name);
	if (prog->is_chained) {
		using_list = NULL;
		parameter_list = NULL;
	}
	if (!gencode && !using_list) {
		output ("void);\n");
		return;
	}
	for (l = using_list; l; l = CB_CHAIN (l)) {
		f = cb_field (CB_VALUE (l));
		switch (CB_PURPOSE_INT (l)) {
		case CB_CALL_BY_REFERENCE:
		case CB_CALL_BY_CONTENT:
			if (gencode) {
				output ("unsigned char *%s%d", CB_PREFIX_BASE, f->id);
			} else {
				output ("unsigned char *");
			}
			break;
		case CB_CALL_BY_VALUE:
			if (CB_TREE_CLASS (CB_VALUE (l)) == CB_CLASS_NUMERIC) {
				if (gencode) {
					output ("int i_%d", f->id);
				} else {
					output ("int");
				}
			} else {
				if (gencode) {
					output ("unsigned char i_%d", f->id);
				} else {
					output ("unsigned char");
				}
			}
			break;
		}
		if (CB_CHAIN (l)) {
			output (", ");
		}
	}
	if (gencode) {
		output (")\n");
	} else {
		output (");\n");
		return;
	}
	output ("{\n");
	for (l = using_list; l; l = CB_CHAIN (l)) {
		if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT) {
			f = cb_field (CB_VALUE (l));
			if (cb_sticky_linkage) {
				output ("  static unsigned char copy_%d[%d];  /* %s */\n",
					f->id, f->size, f->name);
			} else {
				output ("  unsigned char copy_%d[%d];  /* %s */\n",
					f->id, f->size, f->name);
			}
		}
	}
	parmnum = 1;
	for (l = using_list; l; l = CB_CHAIN (l), parmnum++) {
		if (CB_PURPOSE_INT (l) == CB_CALL_BY_CONTENT) {
			f = cb_field (CB_VALUE (l));
			output ("  if (cob_call_params >= %d)\n", parmnum);
			output ("      memcpy (copy_%d, %s%d, %d);\n",
				f->id, CB_PREFIX_BASE, f->id, f->size);
		}
	}

	output ("  return %s_ (%d", prog->program_id, progid++);
	for (l1 = parameter_list; l1; l1 = CB_CHAIN (l1)) {
		for (l2 = using_list; l2; l2 = CB_CHAIN (l2)) {
			if (strcasecmp (cb_field (CB_VALUE (l1))->name,
					cb_field (CB_VALUE (l2))->name) == 0) {
				f = cb_field (CB_VALUE (l2));
				switch (CB_PURPOSE_INT (l2)) {
				case CB_CALL_BY_REFERENCE:
					output (", %s%d", CB_PREFIX_BASE, f->id);
					break;
				case CB_CALL_BY_CONTENT:
					output (", copy_%d", f->id);
					break;
				case CB_CALL_BY_VALUE:
					output (", (unsigned char *) &i_%d", f->id);
					break;
				}
				break;
			}
		}
		if (l2 == NULL) {
			output (", NULL");
		}
	}
	output (");\n");
	output ("}\n\n");
}

static void
output_main_function (struct cb_program *prog)
{
	output_line ("int");
	output_line ("main (int argc, char **argv)");
	output_indent ("{");
	output_line ("cob_init (argc, argv);");
	output_line ("cob_stop_run (%s ());", prog->program_id);
	output_indent ("}\n");
}

void
codegen (struct cb_program *prog, int nested)
{
	int			i;
	cb_tree			l;
	cb_tree			parameter_list;
	struct attr_list	*j;
	struct literal_list	*m;
	struct field_list	*k;
	struct call_list	*clp;
	unsigned char		*s;
	struct cb_program	*cp;
	cb_tree			using_list;
	cb_tree			l1;
	cb_tree			l2;

	param_id = 0;
	stack_id = 0;
	num_cob_fields = 0;
	progid = 0;
	loop_counter = 0;
	output_indent_level = 0;
	last_line = 0;
	needs_exit_prog = 0;
	gen_ebcdic = 0;
	gen_ebcdic_ascii = 0;
	gen_full_ebcdic = 0;
	gen_native = 0;
	gen_custom = 0;
	field_cache = NULL;
	attr_cache = NULL;
	literal_cache = NULL;
	call_cache = NULL;
	label_cache = NULL;
	parameter_list = NULL;
	excp_current_program_id = prog->orig_source_name;
	excp_current_section = NULL;
	excp_current_paragraph = NULL;
	memset ((char *)i_counters, 0, sizeof (i_counters));

	output_target = yyout;

	if (!nested) {
		num_cob_genned = 0;
		output ("/* Generated from %s by cobc version %s patch level %d */\n\n",
			cb_source_file, PACKAGE_VERSION, PATCH_LEVEL);
		output ("#define  __USE_STRING_INLINES 1\n");
#ifdef	_XOPEN_SOURCE_EXTENDED
		output ("#ifndef	_XOPEN_SOURCE_EXTENDED\n");
		output ("#define  _XOPEN_SOURCE_EXTENDED 1\n");
		output ("#endif\n");
#endif
		output ("#include <stdio.h>\n");
		output ("#include <stdlib.h>\n");
		output ("#include <string.h>\n");
		output ("#include <math.h>\n");
#if	COB_USE_SETJMP
		output ("#include <setjmp.h>\n");
#endif
#ifdef	WORDS_BIGENDIAN
		output ("#define WORDS_BIGENDIAN 1\n");
#endif
/* Hmm, autoconf redefines inline and we do not have config.h here
#ifdef	COB_HAS_INLINE
		output ("#define COB_HAS_INLINE 1\n");
#endif
*/
		if (optimize_flag) {
			output ("#define COB_LOCAL_INLINE\n");
			if (optimize_flag > 1) {
				output ("#define SUPER_OPTIMIZE\n");
			}
		}
		output ("#include <libcob.h>\n\n");

		output ("#define COB_SOURCE_FILE		\"%s\"\n", cb_source_file);
		output ("#define COB_PACKAGE_VERSION	\"%s\"\n", PACKAGE_VERSION);
		output ("#define COB_PATCH_LEVEL		%d\n\n", PATCH_LEVEL);

		output_storage ("/* Generated from %s by cobc version %s patch level %d */\n\n",
				cb_source_file, PACKAGE_VERSION, PATCH_LEVEL);
		for (cp = prog; cp; cp = cp->next_program) {
			if (cp->gen_decset) {
/* Hmm, autoconf redefines inline and we do not have config.h here
				output("static COB_INLINE void\n");
*/
				output("static void\n");
				output("cob_decimal_set_int (cob_decimal *d, const int n)\n");
				output("{\n");
				output("	mpz_set_si (d->value, n);\n");
				output("	d->scale = 0;\n");
				output("}\n\n");
				break;
			}
		}
		for (cp = prog; cp; cp = cp->next_program) {
			if (cp->gen_ptrmanip) {
				output("static void\n");
				output("cob_pointer_manip (cob_field *f1, cob_field *f2, size_t addsub)\n");
				output("{\n");
				output("	unsigned char	*tmptr;\n");
				output("	memcpy (&tmptr, f1->data, sizeof(void *));\n");
				output("	if (addsub) {\n");
				output("		tmptr -= cob_get_int (f2);\n");
				output("	} else {\n");
				output("		tmptr += cob_get_int (f2);\n");
				output("	}\n");
				output("	memcpy (f1->data, &tmptr, sizeof(void *));\n");
				output("}\n\n");
				break;
			}
		}
	}

	/* class-names */
	if (prog->class_name_list) {
		output ("/* class-names */\n");
	}
	for (l = prog->class_name_list; l; l = CB_CHAIN (l)) {
		output_class_name_definition (CB_CLASS_NAME (CB_VALUE (l)));
	}

	/* build parameter list */
	for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
		using_list = CB_VALUE (l);
		for (l1 = using_list; l1; l1 = CB_CHAIN (l1)) {
			for (l2 = parameter_list; l2; l2 = CB_CHAIN (l2)) {
				if (strcasecmp (cb_field (CB_VALUE (l1))->name,
						cb_field (CB_VALUE (l2))->name) == 0) {
					break;
				}
			}
			if (l2 == NULL) {
				parameter_list = cb_list_add (parameter_list, CB_VALUE (l1));
			}
		}
	}

	/* prototype */
	output ("/* function prototypes */\n");
	output ("static int %s_ (const int", prog->program_id);
	if (!prog->is_chained) {
		for (l = parameter_list; l; l = CB_CHAIN (l)) {
			output (", unsigned char *");
		}
	}
	output (");\n\n");

	/* main function */
	if (prog->gen_main) {
		output ("int %s (void);\n\n", prog->program_id);
		output_main_function (prog);
	} else {
		for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
			output_entry_function (prog, l, parameter_list, 0);
		}
		output ("\n\n");
	}

	/* functions */
	output ("/* functions */\n\n");
	for (l = prog->entry_list; l; l = CB_CHAIN (l)) {
		output_entry_function (prog, l, parameter_list, 1);
	}
	output ("/* end functions */\n\n");

	output_internal_function (prog, parameter_list);
	output ("/* end function stuff */\n\n");

	if (gen_native || gen_full_ebcdic || gen_ebcdic_ascii || prog->alphabet_name_list) {
		(void)lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	}
	output_target = cb_storage_file;
	if (attr_cache) {
		output_storage ("\n/* attributes */\n");
		attr_cache = attr_list_reverse (attr_cache);
	} else {
		output_storage ("\n");
	}
	for (j = attr_cache; j; j = j->next) {
		output_storage ("static cob_field_attr %s%d\t= ", CB_PREFIX_ATTR, j->id);
		output_storage ("{%d, %d, %d, %d, ", j->type, j->digits, j->scale, j->flags);
		if (j->pic) {
			output_storage ("\"");
			for (s = j->pic; *s; s += 5) {
				output_storage ("%c\\%03o\\%03o\\%03o\\%03o",
					s[0], s[1], s[2], s[3], s[4]);
			}
			output_storage ("\"");
		} else {
			output_storage ("NULL");
		}
		output_storage ("};\n");
	}

	i = 0;
	for (k = field_cache; k; k = k->next) {
		if (!k->f->flag_local && !k->f->flag_item_external) {
			if (!i) {
				i = 1;
				output_storage ("\n/* fields */\n");
			}
			output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, k->f->id);
			output_field (k->x);
			output (";\t/* %s */\n", k->f->name);
		}
	}

	i = 0;
	for (k = field_cache; k; k = k->next) {
		if (k->f->flag_local || k->f->flag_item_external) {
			if (!i) {
				i = 1;
				output_storage ("\n/* local/external fields */\n");
			}
			output ("static cob_field %s%d\t= ", CB_PREFIX_FIELD, k->f->id);
			output ("{");
			output_size (k->x);
			output (", NULL, ");
			output_attr (k->x);
			output ("}");
			output (";\t/* %s */\n", k->f->name);
		}
	}

	if (literal_cache) {
		output_storage ("\n/* constants */\n");
		literal_cache = literal_list_reverse (literal_cache);
	} else {
		output_storage ("\n");
	}
	for (m = literal_cache; m; m = m->next) {
		output ("static cob_field %s%d\t= ", CB_PREFIX_CONST, m->id);
		output_field (m->x);
		output (";\n");
	}

	if (call_cache) {
		output_storage ("\n/* call parameters */\n");
	}
	for (clp = call_cache; clp; clp = clp->next) {
		output ("static int (*call_%s)();\n", clp->callname);
	}

	output_storage ("\n");
	for (i = 0; i < NUM_I_COUNTERS; i++) {
		if (i_counters[i]) {
			output ("int i%d;\n", i);
		}
	}

	if (num_cob_fields) {
		if (num_cob_fields > num_cob_genned) {
			output_storage ("\n/* cob fields */\n");
			for (i = num_cob_genned; i < num_cob_fields; i++) {
				output ("cob_field f%d;\n", i);
			}
			output_storage ("\n");
		}
		num_cob_genned = num_cob_fields;
	}
	i = lookup_attr (COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL, 0);
	if (gen_ebcdic) {
		output_storage ("/* EBCDIC translate table */\n");
		output ("static const unsigned char\tcob_a2e[256] = {\n");
		if (alt_ebcdic) {
			output ("\t0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,\n");
			output ("\t0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,\n");
			output ("\t0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,\n");
			output ("\t0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,\n");
			output ("\t0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,\n");
			output ("\t0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,\n");
			output ("\t0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,\n");
			output ("\t0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,\n");
			output ("\t0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,\n");
			output ("\t0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,\n");
			output ("\t0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,\n");
			output ("\t0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,\n");
			output ("\t0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,\n");
			output ("\t0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,\n");
			output ("\t0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,\n");
			output ("\t0xA7, 0xA8, 0xA9, 0xC0, 0x6A, 0xD0, 0xA1, 0x07,\n");
			output ("\t0x68, 0xDC, 0x51, 0x42, 0x43, 0x44, 0x47, 0x48,\n");
			output ("\t0x52, 0x53, 0x54, 0x57, 0x56, 0x58, 0x63, 0x67,\n");
			output ("\t0x71, 0x9C, 0x9E, 0xCB, 0xCC, 0xCD, 0xDB, 0xDD,\n");
			output ("\t0xDF, 0xEC, 0xFC, 0xB0, 0xB1, 0xB2, 0x3E, 0xB4,\n");
			output ("\t0x45, 0x55, 0xCE, 0xDE, 0x49, 0x69, 0x9A, 0x9B,\n");
			output ("\t0xAB, 0x9F, 0xBA, 0xB8, 0xB7, 0xAA, 0x8A, 0x8B,\n");
			output ("\t0xB6, 0xB5, 0x62, 0x4F, 0x64, 0x65, 0x66, 0x20,\n");
			output ("\t0x21, 0x22, 0x70, 0x23, 0x72, 0x73, 0x74, 0xBE,\n");
			output ("\t0x76, 0x77, 0x78, 0x80, 0x24, 0x15, 0x8C, 0x8D,\n");
			output ("\t0x8E, 0x41, 0x06, 0x17, 0x28, 0x29, 0x9D, 0x2A,\n");
			output ("\t0x2B, 0x2C, 0x09, 0x0A, 0xAC, 0x4A, 0xAE, 0xAF,\n");
			output ("\t0x1B, 0x30, 0x31, 0xFA, 0x1A, 0x33, 0x34, 0x35,\n");
			output ("\t0x36, 0x59, 0x08, 0x38, 0xBC, 0x39, 0xA0, 0xBF,\n");
			output ("\t0xCA, 0x3A, 0xFE, 0x3B, 0x04, 0xCF, 0xDA, 0x14,\n");
			output ("\t0xE1, 0x8F, 0x46, 0x75, 0xFD, 0xEB, 0xEE, 0xED,\n");
			output ("\t0x90, 0xEF, 0xB3, 0xFB, 0xB9, 0xEA, 0xBB, 0xFF\n");
		} else {
			/* MF */
			output ("\t0x00, 0x01, 0x02, 0x03, 0x1D, 0x19, 0x1A, 0x1B,\n");
			output ("\t0x0F, 0x04, 0x16, 0x06, 0x07, 0x08, 0x09, 0x0A,\n");
			output ("\t0x0B, 0x0C, 0x0D, 0x0E, 0x1E, 0x1F, 0x1C, 0x17,\n");
			output ("\t0x10, 0x11, 0x20, 0x18, 0x12, 0x13, 0x14, 0x15,\n");
			output ("\t0x21, 0x27, 0x3A, 0x36, 0x28, 0x30, 0x26, 0x38,\n");
			output ("\t0x24, 0x2A, 0x29, 0x25, 0x2F, 0x2C, 0x22, 0x2D,\n");
			output ("\t0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A,\n");
			output ("\t0x7B, 0x7C, 0x35, 0x2B, 0x23, 0x39, 0x32, 0x33,\n");
			output ("\t0x37, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D,\n");
			output ("\t0x5E, 0x5F, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66,\n");
			output ("\t0x67, 0x68, 0x69, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,\n");
			output ("\t0x70, 0x71, 0x72, 0x7D, 0x6A, 0x7E, 0x7F, 0x31,\n");
			output ("\t0x34, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41,\n");
			output ("\t0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,\n");
			output ("\t0x4A, 0x4B, 0x4C, 0x4E, 0x4F, 0x50, 0x51, 0x52,\n");
			output ("\t0x53, 0x54, 0x55, 0x56, 0x2E, 0x60, 0x4D, 0x05,\n");
			output ("\t0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,\n");
			output ("\t0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,\n");
			output ("\t0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,\n");
			output ("\t0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,\n");
			output ("\t0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,\n");
			output ("\t0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,\n");
			output ("\t0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,\n");
			output ("\t0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,\n");
			output ("\t0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,\n");
			output ("\t0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,\n");
			output ("\t0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,\n");
			output ("\t0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,\n");
			output ("\t0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,\n");
			output ("\t0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,\n");
			output ("\t0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,\n");
			output ("\t0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF\n");
		}
		output ("};\n");
		output_storage ("\n");
	}
	if (gen_full_ebcdic) {
		output ("static const unsigned char\tcob_ebcdic[256] = {\n");
		output ("\t0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,\n");
		output ("\t0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,\n");
		output ("\t0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,\n");
		output ("\t0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,\n");
		output ("\t0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,\n");
		output ("\t0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,\n");
		output ("\t0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,\n");
		output ("\t0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,\n");
		output ("\t0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,\n");
		output ("\t0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,\n");
		output ("\t0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,\n");
		output ("\t0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,\n");
		output ("\t0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,\n");
		output ("\t0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,\n");
		output ("\t0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,\n");
		output ("\t0xA7, 0xA8, 0xA9, 0xC0, 0x6A, 0xD0, 0xA1, 0x07,\n");
		output ("\t0x68, 0xDC, 0x51, 0x42, 0x43, 0x44, 0x47, 0x48,\n");
		output ("\t0x52, 0x53, 0x54, 0x57, 0x56, 0x58, 0x63, 0x67,\n");
		output ("\t0x71, 0x9C, 0x9E, 0xCB, 0xCC, 0xCD, 0xDB, 0xDD,\n");
		output ("\t0xDF, 0xEC, 0xFC, 0xB0, 0xB1, 0xB2, 0x3E, 0xB4,\n");
		output ("\t0x45, 0x55, 0xCE, 0xDE, 0x49, 0x69, 0x9A, 0x9B,\n");
		output ("\t0xAB, 0x9F, 0xBA, 0xB8, 0xB7, 0xAA, 0x8A, 0x8B,\n");
		output ("\t0xB6, 0xB5, 0x62, 0x4F, 0x64, 0x65, 0x66, 0x20,\n");
		output ("\t0x21, 0x22, 0x70, 0x23, 0x72, 0x73, 0x74, 0xBE,\n");
		output ("\t0x76, 0x77, 0x78, 0x80, 0x24, 0x15, 0x8C, 0x8D,\n");
		output ("\t0x8E, 0x41, 0x06, 0x17, 0x28, 0x29, 0x9D, 0x2A,\n");
		output ("\t0x2B, 0x2C, 0x09, 0x0A, 0xAC, 0x4A, 0xAE, 0xAF,\n");
		output ("\t0x1B, 0x30, 0x31, 0xFA, 0x1A, 0x33, 0x34, 0x35,\n");
		output ("\t0x36, 0x59, 0x08, 0x38, 0xBC, 0x39, 0xA0, 0xBF,\n");
		output ("\t0xCA, 0x3A, 0xFE, 0x3B, 0x04, 0xCF, 0xDA, 0x14,\n");
		output ("\t0xE1, 0x8F, 0x46, 0x75, 0xFD, 0xEB, 0xEE, 0xED,\n");
		output ("\t0x90, 0xEF, 0xB3, 0xFB, 0xB9, 0xEA, 0xBB, 0xFF\n");
		output ("};\n");
		output
		    ("static cob_field f_ebcdic = { 256, (unsigned char *)cob_ebcdic, &%s%d };\n",
		     CB_PREFIX_ATTR, i);
		output_storage ("\n");
	}
	if (gen_ebcdic_ascii) {
		output ("static const unsigned char\tcob_ebcdic_ascii[256] = {\n");
		output ("\t0x00, 0x01, 0x02, 0x03, 0xEC, 0x09, 0xCA, 0x7F,\n");
		output ("\t0xE2, 0xD2, 0xD3, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,\n");
		output ("\t0x10, 0x11, 0x12, 0x13, 0xEF, 0xC5, 0x08, 0xCB,\n");
		output ("\t0x18, 0x19, 0xDC, 0xD8, 0x1C, 0x1D, 0x1E, 0x1F,\n");
		output ("\t0xB7, 0xB8, 0xB9, 0xBB, 0xC4, 0x0A, 0x17, 0x1B,\n");
		output ("\t0xCC, 0xCD, 0xCF, 0xD0, 0xD1, 0x05, 0x06, 0x07,\n");
		output ("\t0xD9, 0xDA, 0x16, 0xDD, 0xDE, 0xDF, 0xE0, 0x04,\n");
		output ("\t0xE3, 0xE5, 0xE9, 0xEB, 0x14, 0x15, 0x9E, 0x1A,\n");
		output ("\t0x20, 0xC9, 0x83, 0x84, 0x85, 0xA0, 0xF2, 0x86,\n");
		output ("\t0x87, 0xA4, 0xD5, 0x2E, 0x3C, 0x28, 0x2B, 0xB3,\n");
		output ("\t0x26, 0x82, 0x88, 0x89, 0x8A, 0xA1, 0x8C, 0x8B,\n");
		output ("\t0x8D, 0xE1, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E,\n");
		output ("\t0x2D, 0x2F, 0xB2, 0x8E, 0xB4, 0xB5, 0xB6, 0x8F,\n");
		output ("\t0x80, 0xA5, 0x7C, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,\n");
		output ("\t0xBA, 0x90, 0xBC, 0xBD, 0xBE, 0xF3, 0xC0, 0xC1,\n");
		output ("\t0xC2, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,\n");
		output ("\t0xC3, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,\n");
		output ("\t0x68, 0x69, 0xAE, 0xAF, 0xC6, 0xC7, 0xC8, 0xF1,\n");
		output ("\t0xF8, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,\n");
		output ("\t0x71, 0x72, 0xA6, 0xA7, 0x91, 0xCE, 0x92, 0xA9,\n");
		output ("\t0xE6, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,\n");
		output ("\t0x79, 0x7A, 0xAD, 0xA8, 0xD4, 0x5B, 0xD6, 0xD7,\n");
		output ("\t0x9B, 0x9C, 0x9D, 0xFA, 0x9F, 0xB1, 0xB0, 0xAC,\n");
		output ("\t0xAB, 0xFC, 0xAA, 0xFE, 0xE4, 0x5D, 0xBF, 0xE7,\n");
		output ("\t0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,\n");
		output ("\t0x48, 0x49, 0xE8, 0x93, 0x94, 0x95, 0xA2, 0xED,\n");
		output ("\t0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,\n");
		output ("\t0x51, 0x52, 0xEE, 0x96, 0x81, 0x97, 0xA3, 0x98,\n");
		output ("\t0x5C, 0xF0, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,\n");
		output ("\t0x59, 0x5A, 0xFD, 0xF5, 0x99, 0xF7, 0xF6, 0xF9,\n");
		output ("\t0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,\n");
		output ("\t0x38, 0x39, 0xDB, 0xFB, 0x9A, 0xF4, 0xEA, 0xFF\n");
		output ("};\n");
		output
		    ("static cob_field f_ebcdic_ascii = { 256, (unsigned char *)cob_ebcdic_ascii, &%s%d };\n",
		     CB_PREFIX_ATTR, i);
		output_storage ("\n");
	}

	/* alphabet-names */
	if (prog->alphabet_name_list) {
		output_storage ("\n/* alphabet names */\n");
	}
	for (l = prog->alphabet_name_list; l; l = CB_CHAIN (l)) {
		output_alphabet_name_definition (CB_ALPHABET_NAME (CB_VALUE (l)));
		output ("static cob_field f_%s = { 256, (unsigned char *)%s%s, &%s%d };\n",
			CB_ALPHABET_NAME (CB_VALUE (l))->cname,
			CB_PREFIX_SEQUENCE, CB_ALPHABET_NAME (CB_VALUE (l))->cname,
			CB_PREFIX_ATTR, i);
		output_storage ("\n");
	}
	if (gen_native) {
		output ("static const unsigned char\tcob_native[256] = {\n");
		output ("\t0, 1, 2, 3, 4, 5, 6, 7,\n");
		output ("\t8, 9, 10, 11, 12, 13, 14, 15,\n");
		output ("\t16, 17, 18, 19, 20, 21, 22, 23,\n");
		output ("\t24, 25, 26, 27, 28, 29, 30, 31,\n");
		output ("\t32, 33, 34, 35, 36, 37, 38, 39,\n");
		output ("\t40, 41, 42, 43, 44, 45, 46, 47,\n");
		output ("\t48, 49, 50, 51, 52, 53, 54, 55,\n");
		output ("\t56, 57, 58, 59, 60, 61, 62, 63,\n");
		output ("\t64, 65, 66, 67, 68, 69, 70, 71,\n");
		output ("\t72, 73, 74, 75, 76, 77, 78, 79,\n");
		output ("\t80, 81, 82, 83, 84, 85, 86, 87,\n");
		output ("\t88, 89, 90, 91, 92, 93, 94, 95,\n");
		output ("\t96, 97, 98, 99, 100, 101, 102, 103,\n");
		output ("\t104, 105, 106, 107, 108, 109, 110, 111,\n");
		output ("\t112, 113, 114, 115, 116, 117, 118, 119,\n");
		output ("\t120, 121, 122, 123, 124, 125, 126, 127,\n");
		output ("\t128, 129, 130, 131, 132, 133, 134, 135,\n");
		output ("\t136, 137, 138, 139, 140, 141, 142, 143,\n");
		output ("\t144, 145, 146, 147, 148, 149, 150, 151,\n");
		output ("\t152, 153, 154, 155, 156, 157, 158, 159,\n");
		output ("\t160, 161, 162, 163, 164, 165, 166, 167,\n");
		output ("\t168, 169, 170, 171, 172, 173, 174, 175,\n");
		output ("\t176, 177, 178, 179, 180, 181, 182, 183,\n");
		output ("\t184, 185, 186, 187, 188, 189, 190, 191,\n");
		output ("\t192, 193, 194, 195, 196, 197, 198, 199,\n");
		output ("\t200, 201, 202, 203, 204, 205, 206, 207,\n");
		output ("\t208, 209, 210, 211, 212, 213, 214, 215,\n");
		output ("\t216, 217, 218, 219, 220, 221, 222, 223,\n");
		output ("\t224, 225, 226, 227, 228, 229, 230, 231,\n");
		output ("\t232, 233, 234, 235, 236, 237, 238, 239,\n");
		output ("\t240, 241, 242, 243, 244, 245, 246, 247,\n");
		output ("\t248, 249, 250, 251, 252, 253, 254, 255\n");
		output ("};\n");
		output
		    ("static cob_field f_native = { 256, (unsigned char *)cob_native, &%s%d };\n",
		     CB_PREFIX_ATTR, i);
		output_storage ("\n");
	}

	output_storage ("/* ---------------------------------------------- */\n");
	output_target = yyout;
	if (prog->next_program) {
		codegen (prog->next_program, 1);
	}

}
