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

#include "config.h"
#include "defaults.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <libcob.h>

#if !(defined __CYGWIN__ || defined __MINGW32__)
#include <unistd.h>
#endif

#ifdef __MINGW32__
#include <windows.h>		/* for GetTempPath, GetTempFileName */
#endif /* __MINGW32__ */

#include "cobc.h"
#include "tree.h"
#include "lib/getopt.h"


/*
 * Global variables
 */

enum cb_compile_level cb_compile_level = CB_LEVEL_EXECUTABLE;
enum cb_source_format cb_source_format = CB_FORMAT_FIXED;

struct cb_exception cb_exception_table[] = {
  {0, 0, 0},		/* CB_EC_ZERO */
#undef COB_EXCEPTION
#define COB_EXCEPTION(code,tag,name,critical) {0x##code, name, 0},
#include <libcob/exception.def>
  {0, 0, 0}		/* CB_EC_MAX */
};

#undef CB_FLAG
#define CB_FLAG(var,name,doc) int var = 0;
#include "flag.def"

#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc) int var = 0;
#include "warning.def"

int errorcount;
int warningcount;

char *cb_source_file = NULL;
int cb_source_line = 0;

FILE *cb_storage_file;
char *cb_storage_file_name;

FILE *cb_depend_file = NULL;
char *cb_depend_target = NULL;
struct cb_name_list *cb_depend_list = NULL;
struct cb_name_list *cb_include_list = NULL;
struct cb_name_list *cb_extension_list = NULL;

struct cb_program *current_program = NULL;
struct cb_label *current_section = NULL, *current_paragraph = NULL;


/*
 * Local variables
 */

static int save_temps = 0;
static int verbose_output = 0;

static char *program_name;
static char *output_name;

static char tmpdir[FILENAME_MAX];		/* /tmp */
static char cob_cc[FILENAME_MAX];		/* gcc */
static char cob_cflags[FILENAME_MAX];		/* -I... */
static char cob_libs[FILENAME_MAX];		/* -L... -lcob */
static char cob_ldflags[FILENAME_MAX];
char cob_config_dir[FILENAME_MAX];

static struct filename {
  int need_preprocess;
  int need_translate;
  int need_assemble;
  char source[FILENAME_MAX];			/* foo.cob */
  char preprocess[FILENAME_MAX];		/* foo.i */
  char translate[FILENAME_MAX];			/* foo.c */
  char object[FILENAME_MAX];			/* foo.o */
  struct filename *next;
} *file_list;


struct cb_name_list *
cb_name_list_add (struct cb_name_list *list, const char *name)
{
  struct cb_name_list *p = malloc (sizeof (struct cb_name_list));
  p->name = strdup (name);
  p->next = NULL;
  if (!list)
    return p;
  else
    {
      struct cb_name_list *l;
      for (l = list; l->next; l = l->next);
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
  if (p)
    strcpy (var, p);
  else
    strcpy (var, def);
}

static void
init_environment (int argc, char *argv[])
{
  char *p;

  /* Initialize program_name */
  program_name = strrchr (argv[0], '/');
  if (program_name)
    program_name++;
  else
    program_name = argv[0];

  output_name = NULL;

  init_var (tmpdir,      "TMPDIR",     "/tmp");
  init_var (cob_cc,      "COB_CC",     COB_CC);
  init_var (cob_cflags,  "COB_CFLAGS", COB_CFLAGS);
  init_var (cob_libs,    "COB_LIBS",   COB_LIBS);
  init_var (cob_ldflags, "COB_LDFLAGS", "");
  init_var (cob_config_dir, "COB_CONFIG_DIR", COB_CONFIG_DIR);

  p = getenv ("COB_LDADD");
  if (p)
    {
      strcat (cob_libs, " ");
      strcat (cob_libs, p);
    }
}

static void
terminate (const char *str)
{
  fprintf (stderr, "%s: ", program_name);
  perror (str);
  exit (1);
}


/*
 * Command line
 */

static char short_options[] = "hVvECScmOgwo:I:L:l:";

static struct option long_options[] = {
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'V'},
  {"verbose", no_argument, 0, 'v'},
  {"list-reserved", no_argument, 0, 'R'},
  {"save-temps", no_argument, &save_temps, 1},
  {"std", required_argument, 0, '$'},
  {"conf", required_argument, 0, '&'},
  {"debug", no_argument, 0, 'd'},
  {"ext", required_argument, 0, 'e'},
  {"free", no_argument, (int *) &cb_source_format, CB_FORMAT_FREE},
  {"fixed", no_argument, (int *) &cb_source_format, CB_FORMAT_FIXED},
  {"static", no_argument, &cb_flag_static_call, 1},
  {"dynamic", no_argument, &cb_flag_static_call, 0},
  {"O2", no_argument, 0, '2'},
  {"MT", required_argument, 0, '%'},
  {"MF", required_argument, 0, '@'},
#undef CB_FLAG
#define CB_FLAG(var,name,doc)			\
  {"f"name, no_argument, &var, 1},		\
  {"fno-"name, no_argument, &var, 0},
#include "flag.def"
  {"Wall", no_argument, 0, 'W'},
#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc)		\
  {"W"name, no_argument, &var, 1},		\
  {"Wno-"name, no_argument, &var, 0},
#include "warning.def"
  {0, 0, 0, 0}
};

static void
print_version (void)
{
  puts ("cobc (" PACKAGE_NAME ") " PACKAGE_VERSION);
  puts ("Copyright (C) 2001-2003 Keisuke Nishida");
}

static void
print_usage (void)
{
  printf ("Usage: %s [options] file...\n\n", program_name);
  puts (_("Options:\n"
"  --help                Display this message\n"
"  --version             Display compiler version\n"
"  --verbose, -v         Display the programs invoked by the compiler\n"
"  --list-reserved       Display all reserved words\n"
"  -save-temps           Do not delete intermediate files\n"
"  -E                    Preprocess only; do not compile, assemble or link\n"
"  -C                    Translation only; convert COBOL to C\n"
"  -S                    Compile only; output assembly file\n"
"  -c                    Compile and assemble, but do not link\n"
"  -m                    Build a dynamic-linking module\n"
"  -O, -O2               Enable optimization\n"
"  -g                    Produce debugging information in the output\n"
"  -debug                Enable all run-time error checking\n"
"  -o <file>             Place the output into <file>\n"
"  -I <directory>        Add <directory> to copybook search path\n"
"  -L <directory>        Add <directory> to library search path\n"
"  -l <lib>              Search for library <lib>\n"
"  -MT <target>          Set target file used in dependency list\n"
"  -MF <file>            Place dependency list into <file>\n"
"  -free                 Use free source format\n"
"  -fixed                Use fixed source format\n"
"  -ext=<extension>      Add file extension\n"
"\n"
"  -Wall                 Enable all warnings"));
#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc)		\
  printf ("  -W%-19s %s\n", name, gettext (doc));
#include "warning.def"
  puts ("");
#undef CB_FLAG
#define CB_FLAG(var,name,doc)			\
  printf ("  -f%-19s %s\n", name, gettext (doc));
#include "flag.def"
  puts ("");
}

static int
process_command_line (int argc, char *argv[])
{
  int c, idx;

  /* default extension list */
  cb_extension_list = cb_name_list_add (cb_extension_list, "");
  cb_extension_list = cb_name_list_add (cb_extension_list, ".CBL");
  cb_extension_list = cb_name_list_add (cb_extension_list, ".COB");
  cb_extension_list = cb_name_list_add (cb_extension_list, ".cbl");
  cb_extension_list = cb_name_list_add (cb_extension_list, ".cob");

  while ((c = getopt_long_only (argc, argv, short_options,
				long_options, &idx)) >= 0)
    {
      switch (c)
	{
	case 0: break;
	case '?': break;
	case 'h': print_usage (); exit (0);
	case 'V': print_version (); exit (0);
	case 'R': cb_list_reserved (); exit (0);

	case 'E': cb_compile_level = CB_LEVEL_PREPROCESS; break;
	case 'C': cb_compile_level = CB_LEVEL_TRANSLATE; break;
	case 'S': cb_compile_level = CB_LEVEL_COMPILE; break;
	case 'c': cb_compile_level = CB_LEVEL_ASSEMBLE; break;
	case 'm': cb_compile_level = CB_LEVEL_MODULE; break;
	case 'v': verbose_output = 1; break;
	case 'o': output_name = strdup (optarg); break;

	case 'O':
	  cb_flag_runtime_inlining = 1;
	  strcat (cob_cflags, " -O");
	  break;

	case '2': /* -O2 */
	  cb_flag_runtime_inlining = 1;
	  strcat (cob_cflags, " -O2");
	  break;

	case 'g':
	  cb_flag_line_directive = 1;
	  strcat (cob_cflags, " -g");
	  break;

	case '$': /* -std */
	  if (cb_load_std (optarg) != 0)
	    {
	      fprintf (stderr, _("Invalid option -std=%s\n"), optarg);
	      exit (1);
	    }
	  break;

	case '&': /* -conf */
	  if (cb_load_conf (optarg, 1) != 0)
	    exit (1);
	  break;

	case 'd': /* -debug */
	  {
	    /* Turn on all exception conditions */
	    enum cob_exception_id i;
	    for (i = 1; i < COB_EC_MAX; i++)
	      CB_EXCEPTION_ENABLE (i) = 1;
	    cb_flag_source_location = 1;
	    break;
	  }

	case '%': /* -MT */
	  cb_depend_target = strdup (optarg);
	  break;

	case '@': /* -MF */
	  cb_depend_file = fopen (optarg, "w");
	  if (!cb_depend_file)
	    perror (optarg);
	  break;

	case 'I':
	  cb_include_list = cb_name_list_add (cb_include_list, optarg);
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
	  {
	    char ext[strlen (optarg) + 2];
	    sprintf (ext, ".%s", optarg);
	    cb_extension_list = cb_name_list_add (cb_extension_list, ext);
	    break;
	  }

	case 'w':
#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc)		\
          var = 0;
#include "warning.def"
	  break;

	case 'W':
#undef CB_WARNING
#define CB_WARNING(sig,var,name,doc)		\
          var = 1;
#include "warning.def"
	  break;

	default:
	  ABORT ();
	}
    }

  if (cb_config_name == NULL)
    if (cb_load_std ("default") != 0)
      {
	fprintf (stderr, "error: failed to load the initial config file\n");
	exit (1);
      }

  if (cb_compile_level == CB_LEVEL_EXECUTABLE)
    cb_flag_main = 1;

  return optind;
}

static void
file_basename (const char *filename, char *buff)
{
  int len;
  const char *startp, *endp;

  /* Remove directory name */
  startp = strrchr (filename, '/');
  if (startp)
    startp++;
  else
    startp = filename;

  /* Remove extension */
  endp = strrchr (filename, '.');
  if (endp > startp)
    len = endp - startp;
  else
    len = strlen (startp);

  /* Copy base name */
  strncpy (buff, startp, len);
  buff[len] = '\0';
}

static const char *
file_extension (const char *filename)
{
  const char *p = strrchr (filename, '.');
  if (p)
    return p + 1;
  else
    return "";
}

static void
temp_name (char *buff, const char *ext)
{
#ifdef __MINGW32__
  char temp[MAX_PATH];
  GetTempPath (MAX_PATH, temp);
  GetTempFileName (temp, "cob", 0, buff);
  DeleteFile(buff);
  strcpy (buff + strlen (buff) - 4, ext); /* replace ".tmp" by EXT */
#else /* not __MINGW32__ */
  sprintf (buff, "%s/cobXXXXXX", tmpdir);
  close (mkstemp (buff));
  unlink (buff);
  strcat (buff, ext);
#endif /* not __MINGW32__ */
}

static struct filename *
process_filename (const char *filename)
{
  char basename[FILENAME_MAX];
  const char *extension;
  struct filename *fn = malloc (sizeof (struct filename));
  fn->need_preprocess = 1;
  fn->need_translate  = 1;
  fn->need_assemble   = 1;
  fn->next = NULL;

  file_basename (filename, basename);
  extension = file_extension (filename);

  /* Check input file type */
  if (strcmp (extension, "i") == 0)
    {
      /* already preprocessed */
      fn->need_preprocess = 0;
    }
  else if (strcmp (extension, "c") == 0 || strcmp (extension, "s") == 0)
    {
      /* already compiled */
      fn->need_preprocess = 0;
      fn->need_translate  = 0;
    }
  else if (strcmp (extension, "o") == 0)
    {
      /* already assembled */
      fn->need_preprocess = 0;
      fn->need_translate  = 0;
      fn->need_assemble   = 0;
    }

  /* Set source filename */
  strcpy (fn->source, filename);

  /* Set preprocess filename */
  if (!fn->need_preprocess)
    strcpy (fn->preprocess, fn->source);
  else if (output_name && cb_compile_level == CB_LEVEL_PREPROCESS)
    strcpy (fn->preprocess, output_name);
  else if (save_temps)
    sprintf (fn->preprocess, "%s.i", basename);
  else
    temp_name (fn->preprocess, ".cob");

  /* Set translate filename */
  if (!fn->need_translate)
    strcpy (fn->translate, fn->source);
  else if (output_name && cb_compile_level == CB_LEVEL_TRANSLATE)
    strcpy (fn->translate, output_name);
  else if (save_temps || cb_compile_level == CB_LEVEL_TRANSLATE)
    sprintf (fn->translate, "%s.c", basename);
  else
    temp_name (fn->translate, ".c");

  /* Set object filename */
  if (!fn->need_assemble)
    strcpy (fn->object, fn->source);
  else if (output_name && cb_compile_level == CB_LEVEL_ASSEMBLE)
    strcpy (fn->object, output_name);
  else if (save_temps || cb_compile_level == CB_LEVEL_ASSEMBLE)
    sprintf (fn->object, "%s.o", basename);
  else
    temp_name (fn->object, ".o");

  return fn;
}

static int
process (const char *cmd)
{
  if (verbose_output)
    fprintf (stderr, "%s\n", cmd);
  return system (cmd);
}

static int
preprocess (struct filename *fn)
{
  errorcount = 0;

  ppout = stdout;
  if (output_name || cb_compile_level > CB_LEVEL_PREPROCESS)
    {
      ppout = fopen (fn->preprocess, "w");
      if (!ppout)
	terminate (fn->preprocess);
    }

  if (ppopen (fn->source, NULL) != 0)
    exit (1);

  if (verbose_output)
    fprintf (stderr, "preprocessing %s into %s\n",
	     fn->source, fn->preprocess);
  ppparse ();

  fclose (ppout);
  fclose (ppin);

  if (errorcount > 0)
    return -1;

  /* Output dependency list */
  if (cb_depend_file)
    {
      struct cb_name_list *l;
      if (!cb_depend_target)
	{
	  fputs (_("-MT must be given to specify target file\n"), stderr);
	  exit (1);
	}
      fprintf (cb_depend_file, "%s: \\\n", cb_depend_target);
      for (l = cb_depend_list; l; l = l->next)
	fprintf (cb_depend_file, " %s%s\n", l->name, l->next ? " \\" : "");
      for (l = cb_depend_list; l; l = l->next)
	fprintf (cb_depend_file, "%s:\n", l->name);
      fclose (cb_depend_file);
    }

  return 0;
}

static int
process_translate (struct filename *fn)
{
  int ret;

  /* initialize */
  cb_source_file = NULL;
  cb_source_line = 0;
  cb_init_constants ();
  cb_init_reserved ();

  /* open the input file */
  yyin = fopen (fn->preprocess, "r");
  if (!yyin)
    terminate (fn->preprocess);

  /* parse */
  if (verbose_output)
    fprintf (stderr, "translating %s into %s\n",
	     fn->preprocess, fn->translate);
  ret = yyparse ();

  fclose (yyin);
  if (ret)
    return ret;

  if (cb_flag_syntax_only)
    return 0;

  /* open the output file */
  yyout = fopen (fn->translate, "w");
  if (!yyout)
    terminate (fn->translate);

  /* open the storage file */
  cb_storage_file_name = malloc (strlen (fn->translate) + 3);
  sprintf (cb_storage_file_name, "%s.h", fn->translate);
  cb_storage_file = fopen (cb_storage_file_name, "w");
  if (!cb_storage_file)
    terminate (cb_storage_file_name);

  /* translate to C */
  codegen (current_program);

  /* close the files */
  fclose (cb_storage_file);
  fclose (yyout);
  return 0;
}

static int
process_compile (struct filename *fn)
{
  char buff[FILENAME_MAX];
  char name[FILENAME_MAX];
  if (output_name)
    strcpy (name, output_name);
  else
    {
      file_basename (fn->source, name);
      strcat (name, ".s");
    }
  sprintf (buff, "%s -S -o %s %s %s",
	   cob_cc, name, cob_cflags, fn->translate);
  return process (buff);
}

static int
process_assemble (struct filename *fn)
{
  char buff[FILENAME_MAX];
  sprintf (buff, "%s -c -o %s %s %s",
	   cob_cc, fn->object, cob_cflags, fn->translate);
  return process (buff);
}

static int
process_module (struct filename *fn)
{
  char buff[FILENAME_MAX];
  char name[FILENAME_MAX];
  if (output_name)
    strcpy (name, output_name);
  else
    {
      file_basename (fn->source, name);
      strcat (name, ".");
      strcat (name, COB_MODULE_EXT);
    }
  sprintf (buff, "%s -shared %s -o %s %s %s",
	   cob_cc, cob_ldflags, name, fn->object, cob_libs);
  return process (buff);
}

static int
process_link (struct filename *l)
{
  char buff[FILENAME_MAX], objs[FILENAME_MAX] = "";
  char name[FILENAME_MAX];

  for (; l; l = l->next)
    {
      strcat (objs, l->object);
      strcat (objs, " ");
      if (!l->next)
	file_basename (l->source, name);
    }
  if (output_name)
    strcpy (name, output_name);

  sprintf (buff, "%s -rdynamic %s -o %s %s %s",
	   cob_cc, cob_ldflags, name, objs, cob_libs);
  return process (buff);
}

int
main (int argc, char *argv[])
{
  int i;
  int status = 1;

#if ENABLE_NLS
  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);
#endif

  /* Initialize the global variables */
  init_environment (argc, argv);

  /* Process command line arguments */
  i = process_command_line (argc, argv);

  /* Check the filename */
  if (i == argc)
    {
      fprintf (stderr, "%s: No input files\n", program_name);
      exit (1);
    }

  file_list = NULL;
  while (i < argc)
    {
      struct filename *fn = process_filename (argv[i++]);
      fn->next = file_list;
      file_list = fn;

      /* Preprocess */
      if (cb_compile_level >= CB_LEVEL_PREPROCESS && fn->need_preprocess)
	if (preprocess (fn) != 0)
	  goto cleanup;

      /* Translate */
      if (cb_compile_level >= CB_LEVEL_TRANSLATE && fn->need_translate)
	if (process_translate (fn) != 0)
	  goto cleanup;
      if (cb_flag_syntax_only)
	continue;

      /* Compile */
      if (cb_compile_level == CB_LEVEL_COMPILE)
	if (process_compile (fn) != 0)
	  goto cleanup;

      /* Assemble */
      if (cb_compile_level >= CB_LEVEL_ASSEMBLE && fn->need_assemble)
	if (process_assemble (fn) != 0)
	  goto cleanup;

      /* Build module */
      if (cb_compile_level == CB_LEVEL_MODULE)
	if (process_module (fn) != 0)
	  goto cleanup;
    }

  /* Link */
  if (!cb_flag_syntax_only && cb_compile_level == CB_LEVEL_EXECUTABLE)
    if (process_link (file_list) > 0)
      goto cleanup;

  /* We have successfully completed */
  status = 0;

  /* Remove unnecessary files */
 cleanup:
  if (!save_temps)
    {
      struct filename *fn;
      for (fn = file_list; fn; fn = fn->next)
	{
	  if (fn->need_preprocess
	      && (status == 1 || cb_compile_level > CB_LEVEL_PREPROCESS))
	    remove (fn->preprocess);
	  if (fn->need_translate
	      && (status == 1 || cb_compile_level > CB_LEVEL_TRANSLATE))
	    {
	      remove (fn->translate);
	      remove (cb_storage_file_name);
	    }
	  if (fn->need_assemble
	      && (status == 1 || cb_compile_level > CB_LEVEL_ASSEMBLE))
	    remove (fn->object);
	}
    }

  return status;
}
