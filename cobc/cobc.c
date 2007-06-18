/*
 * Copyright (C) 2001-2007 Keisuke Nishida
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
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#ifdef	HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#include <sys/stat.h>
#ifdef  HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef _WIN32
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#endif

#ifdef	HAVE_KPATHSEA_GETOPT_H
#include <kpathsea/getopt.h>
#else
#ifdef	HAVE_GETOPT_H
#include <getopt.h>
#else
#include "lib/getopt.h"
#endif
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

#include <libcob.h>

#include "cobc.h"
#include "tree.h"

/* Compile level */
enum cb_compile_level {
	CB_LEVEL_PREPROCESS = 1,
	CB_LEVEL_TRANSLATE,
	CB_LEVEL_COMPILE,
	CB_LEVEL_ASSEMBLE,
	CB_LEVEL_MODULE,
	CB_LEVEL_LIBRARY,
	CB_LEVEL_EXECUTABLE
};

/*
 * Global variables
 */

int cb_source_format = CB_FORMAT_FIXED;

struct cb_exception cb_exception_table[] = {
	{0, NULL, 0},		/* CB_EC_ZERO */
#undef COB_EXCEPTION
#define COB_EXCEPTION(code,tag,name,critical) {0x##code, name, 0},
#include <libcob/exception.def>
	{0, NULL, 0}		/* CB_EC_MAX */
};

#undef CB_FLAG
#define CB_FLAG(var,name,doc) int var = 0;
#include "flag.def"

#undef CB_WARNDEF
#define CB_WARNDEF(sig,var,name,wall,doc) int var = 0;
#include "warning.def"

int			cb_id = 1;
int			cb_attr_id = 1;
int			cb_literal_id = 1;
int			cb_field_id = 1;
int			cb_storage_id = 1;
int			cb_flag_main = 0;

int			errorcount = 0;
int			warningcount = 0;
int			alt_ebcdic = 0;
int			optimize_flag = 0;

char			*cb_source_file = NULL;
char			*source_name;
char			*demangle_name;
int			cb_source_line = 0;

FILE			*cb_storage_file;
char			*cb_storage_file_name;

FILE			*cb_listing_file = NULL;
FILE			*cb_depend_file = NULL;
char			*cb_depend_target = NULL;
struct cb_text_list	*cb_depend_list = NULL;
struct cb_text_list	*cb_include_list = NULL;
struct cb_text_list	*cb_extension_list = NULL;

struct cb_program	*current_program = NULL;
struct cb_statement	*current_statement = NULL;
struct cb_label		*current_section = NULL;
struct cb_label		*current_paragraph = NULL;

jmp_buf			cob_jmpbuf;

/*
 * Local variables
 */

static enum cb_compile_level cb_compile_level = 0;

static int		wants_nonfinal = 0;
static int		cb_flag_module = 0;
static int		cb_flag_library = 0;
static int		save_temps = 0;
static int		verbose_output = 0;
static int		cob_iteration = 0;
#ifndef _WIN32
static pid_t		cob_process_id = 0;
#endif

static int		strip_output = 0;
static int		gflag_set = 0;

static char		*program_name;
static char		*output_name;

static char		*cob_tmpdir;			/* /tmp */

static struct filename	*file_list;

#if defined (__GNUC__) && (__GNUC__ >= 3)
static const char fcopts[] = " -finline-functions -fno-gcse -freorder-blocks ";
#else
static const char fcopts[] = " ";
#endif

static char short_options[] = "hVvECScbmxOgwo:t:I:L:l:D:";

static struct option long_options[] = {
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, 'V'},
/* getopt_long_only has a problem with eg. -xv - remove
	{"verbose", no_argument, NULL, 'v'},
*/
	{"list-reserved", no_argument, NULL, 'R'},
	{"save-temps", no_argument, &save_temps, 1},
	{"std", required_argument, NULL, '$'},
	{"conf", required_argument, NULL, '&'},
	{"debug", no_argument, NULL, 'd'},
	{"ext", required_argument, NULL, 'e'},
	{"free", no_argument, &cb_source_format, CB_FORMAT_FREE},
	{"fixed", no_argument, &cb_source_format, CB_FORMAT_FIXED},
	{"static", no_argument, &cb_flag_static_call, 1},
	{"dynamic", no_argument, &cb_flag_static_call, 0},
	{"dynopt", no_argument, &cb_flag_static_call, 2},
	{"O2", no_argument, NULL, '2'},
	{"Os", no_argument, NULL, 's'},
	{"MT", required_argument, NULL, '%'},
	{"MF", required_argument, NULL, '@'},
#undef CB_FLAG
#define CB_FLAG(var,name,doc)			\
	{"f"name, no_argument, &var, 1},	\
	{"fno-"name, no_argument, &var, 0},
#include "flag.def"
	{"Wall", no_argument, NULL, 'W'},
#undef CB_WARNDEF
#define CB_WARNDEF(sig,var,name,wall,doc)		\
	{"W"name, no_argument, &var, 1},	\
	{"Wno-"name, no_argument, &var, 0},
#include "warning.def"
	{NULL, 0, NULL, 0}
};

static char		cob_cc[COB_MEDIUM_BUFF];		/* gcc */
static char		cob_cflags[COB_MEDIUM_BUFF];		/* -I... */
static char		cob_libs[COB_MEDIUM_BUFF];		/* -L... -lcob */
static char		cob_ldflags[COB_MEDIUM_BUFF];
static char		cob_define_flags[COB_MEDIUM_BUFF];	/* -D... */
char			cob_config_dir[COB_MEDIUM_BUFF];

/* cobc functions */

/*
 * Global functions
 */

void
cob_tree_cast_error (cb_tree x, const char *filen, int linenum, int tagnum)
{
	fprintf (stderr, "%s:%d: Invalid type cast from '%s'\n",
		filen, linenum, x ? cb_name (x) : "null");
	fprintf (stderr, "Tag 1 %d Tag 2 %d\n", x ? CB_TREE_TAG(x) : 0,
		tagnum);
	(void)longjmp (cob_jmpbuf, 1);
}

void *
cobc_malloc (const size_t size)
{
	void *mptr;

	mptr = malloc (size);
	if (!mptr) {
		fprintf (stderr, "Cannot acquire %d bytes of memory - Aborting\n", (int)size);
		fflush (stderr);
		(void)longjmp (cob_jmpbuf, 1);
	}
	memset (mptr, 0, size);
	return mptr;
}

struct cb_text_list *
cb_text_list_add (struct cb_text_list *list, const char *text)
{
	struct cb_text_list *p = cobc_malloc (sizeof (struct cb_text_list));
	struct cb_text_list *l;

	p->text = strdup (text);
	p->next = NULL;
	if (!list) {
		return p;
	} else {
		for (l = list; l->next; l = l->next) { ; }
		l->next = p;
		return list;
	}
}

/*
 * Local functions
 */

static void
init_var (char *var, const char *env, const char *def)
{
	char *p = getenv (env);

	if (p) {
		strcpy (var, p);
	} else {
		strcpy (var, def);
	}
}

static void
cob_clean_up (int status)
{
	struct filename	*fn;

	if (!save_temps) {
		for (fn = file_list; fn; fn = fn->next) {
			if (fn->need_preprocess
			    && (status == 1 || cb_compile_level > CB_LEVEL_PREPROCESS)) {
				if (fn->preprocess) {
					unlink (fn->preprocess);
				}
			}
			if (fn->need_translate
			    && (status == 1 || cb_compile_level > CB_LEVEL_TRANSLATE)) {
				if (fn->translate) {
					unlink (fn->translate);
				}
				if (fn->trstorage) {
					unlink (fn->trstorage);
				}
			}
			if (fn->need_assemble
			    && (status == 1 || cb_compile_level > CB_LEVEL_ASSEMBLE)) {
				if (fn->object) {
					unlink (fn->object);
				}
			}
		}
	}
}

static void
terminate (const char *str)
{
	fprintf (stderr, "%s: ", program_name);
	perror (str);
	cob_clean_up (1);
	exit (1);
}

/*
 * Command line
 */

static void
print_version (void)
{
	printf ("cobc (%s) %s.%d\n", PACKAGE_NAME, PACKAGE_VERSION, PATCH_LEVEL);
	puts ("Copyright (C) 2001-2007 Keisuke Nishida");
	puts ("Copyright (C) 2007 Roger While");
}

static void
print_usage (void)
{
	printf ("Usage: %s [options] file...\n\n", program_name);
	puts (_("Options:\n"
		"  --help                Display this message\n"
		"  --version, -V         Display compiler version\n"
		"  -v                    Display the programs invoked by the compiler\n"
		"  -x                    Build an executable program\n"
		"  -m                    Build a dynamically loadable module (default)\n"
		"  -std=<dialect>        Compile for a specific dialect :\n"
		"                          cobol2002   Cobol 2002\n"
		"                          cobol85     Cobol 85\n"
		"                          ibm         IBM Compatible\n"
		"                          mvs         MVS Compatible\n"
		"                          bs2000      BS2000 Compatible\n"
		"                          mf          Micro Focus Compatible\n"
		"                          v023        Open Cobol V23 Compatible (deprecated)\n"
		"                          default     When not specified\n"
		"                        See config/default.conf and config/*.conf\n"
		"  -free                 Use free source format\n"
		"  -fixed                Use fixed source format (default)\n"
		"  -O, -O2, -Os          Enable optimization\n"
		"  -g                    Produce debugging information in the output\n"
		"  -debug                Enable all run-time error checking\n"
		"  -o <file>             Place the output into <file>\n"
		"  -b                    Combine all input files into a single\n"
		"                        dynamically loadable module\n"
		"  -E                    Preprocess only; do not compile, assemble or link\n"
		"  -C                    Translation only; convert COBOL to C\n"
		"  -S                    Compile only; output assembly file\n"
		"  -c                    Compile and assemble, but do not link\n"
		"  -t <file>             Generate and place a program listing into <file>\n"
		"  -I <directory>        Add <directory> to copybook search path\n"
		"  -L <directory>        Add <directory> to library search path\n"
		"  -l <lib>              Link the library <lib>\n"
		"  -D <define>           Pass <define> to the C compiler\n"
		"  --list-reserved       Display all reserved words\n"
		"  -save-temps           Do not delete intermediate files\n"
		"  -MT <target>          Set target file used in dependency list\n"
		"  -MF <file>            Place dependency list into <file>\n"
		"  -ext <extension>      Add default file extension\n"
		"\n" "  -Wall                 Enable all warnings"));
#undef CB_WARNDEF
#define CB_WARNDEF(sig,var,name,wall,doc)		\
	printf ("  -W%-19s %s\n", name, gettext (doc));
#include "warning.def"
	puts ("");
#undef CB_FLAG
#define CB_FLAG(var,name,doc)			\
	printf ("  -f%-19s %s\n", name, gettext (doc));
#include "flag.def"
	puts ("");
}

static void
options_error ()
{
	fprintf (stderr, "Only one of options 'E', 'S', 'C' 'c' may be specified\n");
	exit (1);
}

static int
process_command_line (int argc, char *argv[])
{
	int			c, idx;
	enum cob_exception_id	i;
	char			ext[COB_SMALL_BUFF];

	/* default extension list */
	cb_extension_list = cb_text_list_add (cb_extension_list, "");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".CBL");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".COB");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".CPY");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cbl");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cob");
	cb_extension_list = cb_text_list_add (cb_extension_list, ".cpy");

	/* Enable default I/O exceptions */
	CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;

	while ((c = getopt_long_only (argc, argv, short_options, long_options, &idx)) >= 0) {
		switch (c) {
		case 0:
			break;
		case '?':
			break;
		case 'h':
			print_usage ();
			exit (0);
		case 'V':
			print_version ();
			exit (0);
		case 'R':
			cb_list_reserved ();
			exit (0);

		case 'E':
			if (wants_nonfinal) {
				options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_PREPROCESS;
			break;
		case 'C':
			if (wants_nonfinal) {
				options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_TRANSLATE;
			break;
		case 'S':
			if (wants_nonfinal) {
				options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_COMPILE;
			break;
		case 'c':
			if (wants_nonfinal) {
				options_error ();
			}
			wants_nonfinal = 1;
			cb_compile_level = CB_LEVEL_ASSEMBLE;
			break;
		case 'b':
			if (cb_flag_main || cb_flag_module) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_library = 1;
			break;
		case 'm':
			if (cb_flag_main || cb_flag_library) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_module = 1;
			break;
		case 'x':
			if (cb_flag_module || cb_flag_library) {
				fprintf (stderr, "Only one of options 'm', 'x', 'b' may be specified\n");
				exit (1);
			}
			cb_flag_main = 1;
			break;
		case 'v':
			verbose_output = 1;
			break;
		case 'o':
			output_name = strdup (optarg);
			break;

		case 'O':
			strcat (cob_cflags, " -O");
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 1;
			break;

		case '2':	/* -O2 */
			strip_output = 1;
#if defined(__hpux) && !defined(__GNUC__)
			strcat (cob_cflags, " -O");
#else
			strcat (cob_cflags, " -O2");
#endif
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 2;
			break;

		case 's':	/* -Os */
			strip_output = 1;
#if defined(__hpux) && !defined(__GNUC__)
			strcat (cob_cflags, " -O");
#else
			strcat (cob_cflags, " -Os");
#endif
			strcat (cob_cflags, fcopts);
			strcat (cob_cflags, COB_EXTRA_FLAGS);
			optimize_flag = 2;
			break;

		case 'g':
			cb_flag_line_directive = 1;
			gflag_set = 1;
#ifndef _MSC_VER
			strcat (cob_cflags, " -g");
#endif
			break;

		case '$':	/* -std */
			if (cb_load_std (optarg) != 0) {
				fprintf (stderr, _("Invalid option -std=%s\n"), optarg);
				exit (1);
			}
			break;

		case '&':	/* -conf */
			if (cb_load_conf (optarg, 1) != 0) {
				exit (1);
			}
			break;

		case 'd':	/* -debug */
			/* Turn on all exception conditions */
			for (i = 1; i < COB_EC_MAX; i++) {
				CB_EXCEPTION_ENABLE (i) = 1;
			}
			cb_flag_source_location = 1;
			break;

		case 't':
			cb_listing_file = fopen (optarg, "w");
			if (!cb_listing_file) {
				perror (optarg);
			}
			break;

		case 'D':	/* -D */
#ifdef _MSC_VER
			strcat (cob_define_flags, "/D \"");
			strcat (cob_define_flags, optarg);
			strcat (cob_define_flags, "\" ");
#else
			strcat (cob_define_flags, "-D");
			strcat (cob_define_flags, optarg);
			strcat (cob_define_flags, " ");
#endif
			break;

		case '%':	/* -MT */
			cb_depend_target = strdup (optarg);
			break;

		case '@':	/* -MF */
			cb_depend_file = fopen (optarg, "w");
			if (!cb_depend_file) {
				perror (optarg);
			}
			break;

		case 'I':
			cb_include_list = cb_text_list_add (cb_include_list, optarg);
			break;

		case 'L':
			strcat (cob_libs, " -L");
			strcat (cob_libs, optarg);
			break;

		case 'l':
			strcat (cob_libs, " -l");
			strcat (cob_libs, optarg);
			break;

		case 'e':
			sprintf (ext, ".%s", optarg);
			cb_extension_list = cb_text_list_add (cb_extension_list, ext);
			break;

		case 'w':
#undef CB_WARNDEF
#define CB_WARNDEF(sig,var,name,wall,doc)		\
          var = 0;
#include "warning.def"
			break;

		case 'W':
#undef CB_WARNDEF
#define CB_WARNDEF(sig,var,name,wall,doc)		\
          if (wall) var = 1;
#include "warning.def"
			break;

		default:
			ABORT ();
		}
	}

	if (cb_config_name == NULL) {
		if (cb_load_std ("default") != 0) {
			fprintf (stderr, "error: failed to load the initial config file\n");
			exit (1);
		}
	}

#ifdef	__GNUC__
	strcat (cob_cflags, " -fsigned-char");
#ifdef	HAVE_PSIGN_OPT
	strcat (cob_cflags, " -Wno-pointer-sign");
#endif
#endif

	if (gflag_set) {
		strip_output = 0;
		optimize_flag = 0;
	}
	if (cb_flag_source_location) {
		optimize_flag = 0;
	}
#if defined (__GNUC__) && (__GNUC__ >= 3)
	if (strip_output) {
		strcat (cob_cflags, " -fomit-frame-pointer");
	}
#endif

	return optind;
}

static void
file_basename (const char *filename, char *buff)
{
	size_t		len;
	const char	*startp, *endp;

	/* Remove directory name */
	startp = strrchr (filename, '/');
	if (startp) {
		startp++;
	} else {
		startp = filename;
	}

	/* Remove extension */
	endp = strrchr (filename, '.');
	if (endp > startp) {
		len = endp - startp;
	} else {
		len = strlen (startp);
	}

	/* Copy base name */
	strncpy (buff, startp, len);
	buff[len] = '\0';
}

static const char *
file_extension (const char *filename)
{
	const char *p = strrchr (filename, '.');

	if (p) {
		return p + 1;
	} else {
		return "";
	}
}

static char *
temp_name (const char *ext)
{
	char	buff[COB_MEDIUM_BUFF];
#ifdef _WIN32
	char	temp[MAX_PATH];

	GetTempPath (MAX_PATH, temp);
	GetTempFileName (temp, "cob", 0, buff);
	DeleteFile (buff);
	strcpy (buff + strlen (buff) - 4, ext);	/* replace ".tmp" by EXT */
#else
	sprintf (buff, "%s/cob%d_%d%s", cob_tmpdir, cob_process_id, cob_iteration, ext);
#endif
	return strdup (buff);
}

static struct filename *
process_filename (const char *filename)
{
	const char	*extension;
	struct filename	*fn;
	struct filename	*ffn;
	struct stat	st;
	char		basename[COB_SMALL_BUFF];

	if (stat (filename, &st) != 0) {
		terminate (filename);
	}
	fn = cobc_malloc (sizeof (struct filename));
	fn->need_preprocess = 1;
	fn->need_translate = 1;
	fn->need_assemble = 1;
	fn->next = NULL;

	if (!file_list) {
		file_list = fn;
	} else {
		for (ffn = file_list; ffn->next; ffn = ffn->next)
			;
		ffn->next = fn;
	}

	file_basename (filename, basename);
	fn->demangle_source =  cb_encode_program_id (basename);
	extension = file_extension (filename);

	/* Check input file type */
	if (strcmp (extension, "i") == 0) {
		/* already preprocessed */
		fn->need_preprocess = 0;
	} else if (strcmp (extension, "c") == 0 || strcmp (extension, "s") == 0) {
		/* already compiled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
#ifdef _MSC_VER
	} else if (strcmp (extension, "obj") == 0 ||
		   strcmp (extension, "lib") == 0) {
#else
	} else if (strcmp (extension, "o") == 0) {
#endif
		/* already assembled */
		fn->need_preprocess = 0;
		fn->need_translate = 0;
		fn->need_assemble = 0;
	}

	/* Set source filename */
	fn->source = cobc_malloc (strlen (filename) + 3);
	strcpy (fn->source, filename);

	/* Set preprocess filename */
	if (!fn->need_preprocess) {
		fn->preprocess = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_PREPROCESS) {
		fn->preprocess = strdup (output_name);
	} else if (save_temps) {
		fn->preprocess = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->preprocess, "%s.i", basename);
	} else {
		fn->preprocess = temp_name (".cob");
	}

	/* Set translate filename */
	if (!fn->need_translate) {
		fn->translate = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = strdup (output_name);
	} else if (save_temps || cb_compile_level == CB_LEVEL_TRANSLATE) {
		fn->translate = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->translate, "%s.c", basename);
	} else {
		fn->translate = temp_name (".c");
	}

	/* Set object filename */
	if (!fn->need_assemble) {
		fn->object = strdup (fn->source);
	} else if (output_name && cb_compile_level == CB_LEVEL_ASSEMBLE) {
		fn->object = strdup (output_name);
	} else if (save_temps || cb_compile_level == CB_LEVEL_ASSEMBLE) {
		fn->object = cobc_malloc (strlen (basename) + 5);
#ifdef	_MSC_VER
		sprintf (fn->object, "%s.obj", basename);
#else
		sprintf (fn->object, "%s.o", basename);
#endif
	} else {
#ifdef	_MSC_VER
		fn->object = cobc_malloc (strlen (basename) + 5);
		sprintf (fn->object, "%s.obj", basename);
#else
		fn->object = temp_name (".o");
#endif
	}

	cob_iteration++;

	return fn;
}

static int
process (const char *cmd)
{
	char	*p;
	char	*buffptr;
	int	clen;
	char	buff[COB_MEDIUM_BUFF];

	if (strchr (cmd, '$') == NULL) {
		if (verbose_output) {
			fprintf (stderr, "%s\n", (char *)cmd);
		}
		return system (cmd);
	}
	clen = strlen (cmd) + 32;
	if (clen > COB_MEDIUM_BUFF) {
		buffptr = cobc_malloc (clen);
	} else {
		buffptr = buff;
	}
	p = buffptr;
	/* quote '$' */
	for (; *cmd; cmd++) {
		if (*cmd == '$') {
			p += sprintf (p, "\\$");
		} else {
			*p++ = *cmd;
		}
	}
	*p = 0;

	if (verbose_output) {
		fprintf (stderr, "%s\n", buffptr);
	}
	clen = system (buffptr);
	if (buffptr != buff) {
		free (buffptr);
	}
	return clen;
}

static int
preprocess (struct filename *fn)
{
	struct cb_text_list	*l;
	char			line[COB_MEDIUM_BUFF];

	errorcount = 0;

	if (output_name || cb_compile_level > CB_LEVEL_PREPROCESS) {
		ppout = fopen (fn->preprocess, "w");
		if (!ppout) {
			terminate (fn->preprocess);
		}
	} else {
		ppout = stdout;
	}

	if (ppopen (fn->source, NULL) != 0) {
		if (ppout != stdout) {
			fclose (ppout);
			if (fn->preprocess) {
				unlink (fn->preprocess);
			}
		}
		exit (1);
	}

	if (verbose_output) {
		fprintf (stderr, "preprocessing %s into %s\n", fn->source, fn->preprocess);
	}

	ppparse ();

	if (ppout != stdout) {
		fclose (ppout);
		if (cb_listing_file) {
			ppout = fopen (fn->preprocess, "r");
			if (ppout) {
				memset (line, 0, sizeof (line));
				while (fgets (line, COB_MEDIUM_BUFF, ppout) != NULL) {
					if (cb_source_format != CB_FORMAT_FIXED) {
						fprintf (cb_listing_file,
							"%s", line);
					} else {
						if (line[0] == '\n') {
							fprintf (cb_listing_file,
								"%s", line);
						} else if (line[0] == ' ' &&
							   line[1] == '\n') {
							fprintf (cb_listing_file,
								"\n");
						} else if (line[0] == ' ') {
							fprintf (cb_listing_file,
								"          %s", line);
						} else if (line[0] == 0 ||
							   line[0] == '#') {
							fprintf (cb_listing_file,
								"%s", line);
						} else {
							fprintf (cb_listing_file,
								"       %s", line);
						}
					}
					memset (line, 0, sizeof (line));
				}
				fclose (ppout);
			}
		}
	}
	fclose (ppin);

	if (errorcount > 0) {
		return -1;
	}

	/* Output dependency list */
	if (cb_depend_file) {
		if (!cb_depend_target) {
			fputs (_("-MT must be given to specify target file\n"), stderr);
			exit (1);
		}
		fprintf (cb_depend_file, "%s: \\\n", cb_depend_target);
		for (l = cb_depend_list; l; l = l->next) {
			fprintf (cb_depend_file, " %s%s\n", l->text, l->next ? " \\" : "");
		}
		for (l = cb_depend_list; l; l = l->next) {
			fprintf (cb_depend_file, "%s:\n", l->text);
		}
		fclose (cb_depend_file);
	}

	return 0;
}

static struct cb_program *
program_list_reverse (struct cb_program *p)
{
	struct cb_program	*next;
	struct cb_program	*last = NULL;

	for (; p; p = next) {
		next = p->next_program;
		p->next_program = last;
		last = p;
	}
	return last;
}

static int
process_translate (struct filename *fn)
{
	int			ret;
	struct cb_program	*p;

	/* initialize */
	cb_source_file = NULL;
	cb_source_line = 0;
	cb_init_constants ();
	cb_init_reserved ();

	/* open the input file */
	yyin = fopen (fn->preprocess, "r");
	if (!yyin) {
		terminate (fn->preprocess);
	}

	/* parse */
	if (verbose_output) {
		fprintf (stderr, "translating %s into %s\n", fn->preprocess, fn->translate);
	}
	ret = yyparse ();

	fclose (yyin);
	if (ret) {
		return ret;
	}

	if (cb_flag_syntax_only || current_program->entry_list == NULL) {
		return 0;
	}

	/* open the output file */
	yyout = fopen (fn->translate, "w");
	if (!yyout) {
		terminate (fn->translate);
	}

	/* open the storage file */
	cb_storage_file_name = cobc_malloc (strlen (fn->translate) + 3);
	sprintf (cb_storage_file_name, "%s.h", fn->translate);
	fn->trstorage = cb_storage_file_name;
	cb_storage_file = fopen (cb_storage_file_name, "w");
	if (!cb_storage_file) {
		terminate (cb_storage_file_name);
	}

	/* translate to C */
	p = program_list_reverse (current_program);
	codegen (p, 0);

	/* close the files */
	fclose (cb_storage_file);
	fclose (yyout);
	return 0;
}

static int
process_compile (struct filename *fn)
{
	char buff[COB_MEDIUM_BUFF];
	char name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
	} else {
		file_basename (fn->source, name);
#ifndef _MSC_VER
		strcat (name, ".s");
#endif
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /c %s %s /Od /MDd /Zi /FR /c /Fa%s /Fo%s %s" :
		"%s /c %s %s /MD /c /Fa%s /Fo%s %s",
			cob_cc, cob_cflags, cob_define_flags, name, name, fn->translate);
#else
	sprintf (buff, "%s -S -o %s %s %s %s", cob_cc, name, cob_cflags, cob_define_flags,
		fn->translate);
#endif
	return process (buff);
}

static int
process_assemble (struct filename *fn)
{
	char buff[COB_MEDIUM_BUFF];

#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /c %s %s /Od /MDd /Zi /FR /Fo%s %s" :
		"%s /c %s %s /MD /Fo%s %s",
			cob_cc, cob_cflags, cob_define_flags, fn->object, fn->translate);
#else
	if (cb_compile_level == CB_LEVEL_MODULE ||
	    cb_compile_level == CB_LEVEL_LIBRARY) {
		sprintf (buff, "%s -c %s %s %s -o %s %s",
			 cob_cc, cob_cflags, cob_define_flags, COB_PIC_FLAGS, fn->object,
			 fn->translate);
	} else {
		sprintf (buff, "%s -c %s %s -o %s %s",
			 cob_cc, cob_cflags, cob_define_flags, fn->object, fn->translate);
	}
#endif
	return process (buff);
}

static int
process_module_direct (struct filename *fn)
{
	int	ret;
	char	buff[COB_MEDIUM_BUFF];
	char	name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
#ifndef _MSC_VER
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (fn->source, name);
#ifndef _MSC_VER
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
#endif
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s %s %s /Od /MDd /LDd /Zi /FR /Fe%s /Fo%s %s %s %s" :
		"%s %s %s /MD /LD /Fe%s /Fo%s %s %s %s",
			cob_cc, cob_cflags, cob_define_flags, name, name, cob_ldflags,
			fn->translate, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff, "mt /manifest %s.dll.manifest /outputresource:%s.dll;#2", name, name);
		ret = process (buff);
	}
#endif
#else	/* _MSC_VER */
	sprintf (buff, "%s %s %s %s %s %s %s -o %s %s %s",
		 cob_cc, cob_cflags, cob_define_flags, COB_SHARED_OPT, cob_ldflags,
		 COB_PIC_FLAGS, COB_EXPORT_DYN, name, fn->translate, cob_libs);
	ret = process (buff);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

static int
process_module (struct filename *fn)
{
	int	ret;
	char	buff[COB_MEDIUM_BUFF];
	char	name[COB_MEDIUM_BUFF];

	if (output_name) {
		strcpy (name, output_name);
#ifndef _MSC_VER
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (fn->source, name);
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /LDd /Zi /FR /Fe%s %s %s %s" :
		"%s /MD /LD /Fe%s %s %s %s",
			cob_cc, name, cob_ldflags, fn->object, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff, "mt /manifest %s.dll.manifest /outputresource:%s.dll;#2", name, name);
		ret = process (buff);
	}
#endif
#else	/* _MSC_VER */
	sprintf (buff, "%s %s %s %s %s -o %s %s %s",
		 cob_cc, COB_SHARED_OPT, cob_ldflags, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, fn->object, cob_libs);
	ret = process (buff);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

static int
process_library (struct filename *l)
{
	int		ret;
	int		bufflen;
	char		*buffptr;
	char		*objsptr;
	struct filename	*f;
	char		buff[COB_MEDIUM_BUFF];
	char		name[COB_MEDIUM_BUFF];
	char		objs[COB_MEDIUM_BUFF] = "\0";

	bufflen = 0;
	for (f = l; f; f = f->next) {
		bufflen += strlen (f->object) + 2;
	}
	if (bufflen >= COB_MEDIUM_BUFF) {
		objsptr = cobc_malloc (bufflen);
	} else {
		objsptr = objs;
	}
	for (f = l; f; f = f->next) {
		strcat (objsptr, f->object);
		strcat (objsptr, " ");
	}

	if (output_name) {
		strcpy (name, output_name);
#ifndef _MSC_VER
		if (strchr (output_name, '.') == NULL) {
			strcat (name, ".");
			strcat (name, COB_MODULE_EXT);
		}
#endif
	} else {
		file_basename (l->source, name);
#ifndef _MSC_VER
		strcat (name, ".");
		strcat (name, COB_MODULE_EXT);
#endif
	}

	bufflen = strlen (cob_cc) + strlen (cob_ldflags)
			+ strlen (COB_EXPORT_DYN) + strlen (COB_SHARED_OPT)
			+ strlen (name) + strlen (objsptr) + strlen (cob_libs)
			+ strlen (COB_PIC_FLAGS) + 16;
	if (bufflen >= COB_MEDIUM_BUFF) {
		buffptr = cobc_malloc (bufflen);
	} else {
		buffptr = buff;
	}

#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /LDd /Zi /FR /Fe%s %s %s %s" :
		"%s /MD /LD /Fe%s %s %s %s",
			cob_cc, name, cob_ldflags, objsptr, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff, "mt /manifest %s.dll.manifest /outputresource:%s.dll;#2", name, name);
		ret = process (buff);
	}
#endif
#else	/* _MSC_VER */
	sprintf (buffptr, "%s %s %s %s %s -o %s %s %s",
		 cob_cc, COB_SHARED_OPT, cob_ldflags, COB_PIC_FLAGS,
		 COB_EXPORT_DYN, name, objsptr, cob_libs);
	ret = process (buffptr);
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s", COB_STRIP_CMD, name);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

static int
process_link (struct filename *l)
{
	int		ret;
	int		bufflen;
	char		*buffptr;
	char		*objsptr;
	struct filename	*f;
	char		buff[COB_MEDIUM_BUFF];
	char		name[COB_MEDIUM_BUFF];
	char		objs[COB_MEDIUM_BUFF] = "\0";

	bufflen = 0;
	for (f = l; f; f = f->next) {
		bufflen += strlen (f->object) + 2;
	}
	if (bufflen >= COB_MEDIUM_BUFF) {
		objsptr = cobc_malloc (bufflen);
	} else {
		objsptr = objs;
	}
	for (f = l; f; f = f->next) {
		strcat (objsptr, f->object);
		strcat (objsptr, " ");
	}

	if (output_name) {
		strcpy (name, output_name);
	} else {
		file_basename (l->source, name);
	}

	bufflen = strlen (cob_cc) + strlen (cob_ldflags) + strlen (COB_EXPORT_DYN)
			+ strlen (name) + strlen (objsptr) + strlen (cob_libs)
			+ 16;
	if (bufflen >= COB_MEDIUM_BUFF) {
		buffptr = cobc_malloc (bufflen);
	} else {
		buffptr = buff;
	}
#ifdef _MSC_VER
	sprintf (buff, gflag_set ? 
		"%s /Od /MDd /Zi /FR /Fe%s %s %s %s" :
		"%s /MD /Fe%s %s %s %s",
			cob_cc, name, cob_ldflags, objsptr, cob_libs);
	ret = process (buff);
#if _MSC_VER >= 1400
	/* Embedding manifest */
	if (ret == 0) {
		sprintf (buff, "mt /manifest %s.exe.manifest /outputresource:%s.exe;#2", name, name);
		ret = process (buff);
	}
#endif
#else	/* _MSC_VER */
	sprintf (buffptr, "%s %s %s -o %s %s %s",
		 cob_cc, cob_ldflags, COB_EXPORT_DYN, name, objsptr, cob_libs);

	ret = process (buffptr);
#ifdef	__hpux
	if (ret == 0) {
		sprintf (buff, "chatr -s +s enable %s%s 1>/dev/null 2>&1", name, COB_EXEEXT);
		process (buff);
	}
#endif
#ifdef	COB_STRIP_CMD
	if (strip_output && ret == 0) {
		sprintf (buff, "%s %s%s", COB_STRIP_CMD, name, COB_EXEEXT);
		ret = process (buff);
	}
#endif
#endif	/* _MSC_VER */
	return ret;
}

int
main (int argc, char *argv[])
{
	int			i;
	int			iparams = 0;
	enum cb_compile_level	local_level = 0;
	int			status = 1;
	struct filename		*fn;
	char			*p;

#ifdef	ENABLE_NLS
	setlocale (LC_ALL, "");
	bindtextdomain (PACKAGE, LOCALEDIR);
	textdomain (PACKAGE);
#endif

#ifndef _WIN32
	cob_process_id = getpid();
#endif

	/* Initialize the global variables */

	/* Initialize program_name */
	program_name = strrchr (argv[0], '/');
	if (program_name) {
		program_name++;
	} else {
		program_name = argv[0];
	}

	output_name = NULL;

	if ((p = getenv ("TMPDIR")) != NULL) {
		cob_tmpdir = p;
	} else if ((p = getenv ("TMP")) != NULL) {
		cob_tmpdir = p;
	} else {
		cob_tmpdir = (char *)"/tmp";
	}
	init_var (cob_cc, "COB_CC", COB_CC);
#if defined (__GNUC__) && (__GNUC__ >= 3)
	strcat (cob_cc, " -pipe");
#endif
	init_var (cob_cflags, "COB_CFLAGS", COB_CFLAGS);
	init_var (cob_libs, "COB_LIBS", COB_LIBS);
	init_var (cob_ldflags, "COB_LDFLAGS", COB_LDFLAGS);
	init_var (cob_config_dir, "COB_CONFIG_DIR", COB_CONFIG_DIR);
	memset (cob_define_flags, 0, sizeof (cob_define_flags));

	p = getenv ("COB_LDADD");
	if (p) {
		strcat (cob_libs, " ");
		strcat (cob_libs, p);
	}
	p = getenv ("COB_EBCDIC");
	if (p && (*p == 'F' || *p == 'f')) {
		alt_ebcdic = 1;
	}

	/* Process command line arguments */
	i = process_command_line (argc, argv);

	/* Check the filename */
	if (i == argc) {
		fprintf (stderr, "%s: No input files\n", program_name);
		exit (1);
	}

	file_list = NULL;

	if (setjmp (cob_jmpbuf) != 0) {
		fprintf (stderr, "Aborting compile of %s at line %d\n", cb_source_file,
			 cb_source_line);
		fflush (stderr);
		if (yyout) {
			fflush (yyout);
		}
		if (cb_storage_file) {
			fflush (cb_storage_file);
		}
		status = 1;
		cob_clean_up (status);
		return status;
	}

	/* Defaults are set here */
	if (!cb_flag_syntax_only) {
		if (!wants_nonfinal) {
			if (cb_flag_main) {
				cb_compile_level = CB_LEVEL_EXECUTABLE;
			}
			if (cb_flag_module) {
				cb_compile_level = CB_LEVEL_MODULE;
			}
			if (cb_flag_library) {
				cb_compile_level = CB_LEVEL_LIBRARY;
			}
		}
		if (cb_compile_level == 0 && !wants_nonfinal) {
			cb_compile_level = CB_LEVEL_MODULE;
			cb_flag_module = 1;
		}
		if (wants_nonfinal && cb_compile_level != CB_LEVEL_PREPROCESS &&
		    !cb_flag_main && !cb_flag_module && !cb_flag_library) {
			cb_flag_module = 1;
		}
	} else {
			cb_compile_level = CB_LEVEL_TRANSLATE;
	}

	if (output_name && cb_compile_level < CB_LEVEL_LIBRARY &&
	    (argc - i) > 1) {
		fprintf (stderr, "%s: -o option invalid in this combination\n", program_name);
		exit (1);
	}

	while (i < argc) {
		fn = process_filename (argv[i++]);
		/* Preprocess */
		if (cb_compile_level >= CB_LEVEL_PREPROCESS && fn->need_preprocess) {
			if (preprocess (fn) != 0) {
				cob_clean_up (status);
				return status;
			}
		}
	}
	for (fn = file_list; fn; fn = fn->next) {
		cb_id = 1;
		cb_attr_id = 1;
		cb_literal_id = 1;
		cb_field_id = 1;
		cb_storage_id = 1;
		iparams++;
		demangle_name = fn->demangle_source;
		if (iparams > 1 && cb_compile_level == CB_LEVEL_EXECUTABLE &&
		    !cb_flag_syntax_only) {
			local_level = cb_compile_level;
			cb_flag_main = 0;
			cb_compile_level = CB_LEVEL_ASSEMBLE;
		}
		/* Translate */
		if (cb_compile_level >= CB_LEVEL_TRANSLATE && fn->need_translate) {
			if (process_translate (fn) != 0) {
				cob_clean_up (status);
				return status;
			}
		}
		if (cb_flag_syntax_only) {
			continue;
		}

		/* Compile */
		if (cb_compile_level == CB_LEVEL_COMPILE) {
			if (process_compile (fn) != 0) {
				cob_clean_up (status);
				return status;
			}
		}

		/* Build module */
		if (cb_compile_level == CB_LEVEL_MODULE && fn->need_assemble) {
			if (process_module_direct (fn) != 0) {
				cob_clean_up (status);
				return status;
			}
		} else {
			/* Assemble */
			if (cb_compile_level >= CB_LEVEL_ASSEMBLE && fn->need_assemble) {
				if (process_assemble (fn) != 0) {
					cob_clean_up (status);
					return status;
				}
			}

			/* Build module */
			if (cb_compile_level == CB_LEVEL_MODULE) {
				if (process_module (fn) != 0) {
					cob_clean_up (status);
					return status;
				}
			}
		}
	}

	if (!cb_flag_syntax_only) {
		/* Link */
		if (local_level == CB_LEVEL_EXECUTABLE) {
			cb_compile_level = CB_LEVEL_EXECUTABLE;
			cb_flag_main = 1;
		}
		if (cb_compile_level == CB_LEVEL_LIBRARY) {
			if (process_library (file_list) != 0) {
				cob_clean_up (status);
				return status;
			}
		} else if (cb_compile_level == CB_LEVEL_EXECUTABLE) {
			if (process_link (file_list) != 0) {
				cob_clean_up (status);
				return status;
			}
		}
	}

	/* We have successfully completed */
	status = 0;
	cob_clean_up (status);

	return status;
}
