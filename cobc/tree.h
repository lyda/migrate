/*
 * Literals
 */

struct lit
{
  char litflag;			/* 1 for literals */
  struct lit *next;		/* next in literals list */
  char *name;			/* name (value) of literal */
  char type;
  int decimals;
  unsigned location;		/* data area for literal @lit+n */
  unsigned descriptor;		/* descriptor @lit+n */
  short sec_no;			/* asm section number */
  unsigned char all;
  char *nick;
  int access_mode;		/* ie call_mode */
  int len;			/* length of (possibly including NULLs) string */
};

#define LITERAL(x)		((struct lit *) (x))
#define LITERAL_P(x)		(LITERAL (x)->litflag == 1)


/*
 * Symbols
 */

#define organization decimals
#define fdesc location
#define direction access_mode
#define call_mode access_mode
#define sort_data value2
#define record pic
#define recordsym redefines
#define filenamevar son
#define alternate brother
#define rec_varying picstr

struct sym
{
  char litflag;			/* 1 for literals, 2 for variables */
  struct sym *next;		/* pointer to next symbol with same hash */
  char *name;			/* symbol (variable) name */
  char type;			/* label or elementary item or group item 
				   9,A,X,B,C=elem; 
				   G=group;
				   F=file; 
				   R=record; 
				   S=screen */
  int decimals;			/* decimal places 
				   or organization (files ) */
  unsigned location;		/* offset of variable in stack area */
  /* or offset file descriptor in data area */
  unsigned descriptor;		/* field descriptor offset in data seg */
  /* or index field descriptor (files) */
  short sec_no;			/* asm section number */
  struct sym *clone;		/* NULL if this symbol is unique
				   otherwise, it must be further qualified */
  int times;			/* occurs times */
  char *picstr;			/* pointer to picture string saved or 
				   pointer to rec_varying (files) */
  struct sym *parent;		/* pointer to parent node (level)
				   pointer to STATUS var (files) */
  struct sym *son;		/* used in field hierarchy
				   for files this is the assign 
				   variable (filename) */
  struct sym *brother;		/* field variable at the same level or
				   alternate key list pointer (for indexed files) */
  struct sym *ix_desc;		/* key variable (in file descriptor)
				   pointer to fdesc (in record) */
  struct scr_info *scr;		/*  screen info in screen items */
  struct report_info *ri;	/*  report info in report items */
  struct lit *value;		/* pointer to literal with initial value */
  struct lit *value2;		/* pointer to first/next key (sort files) */
  union
  {
    struct vrange *vr;		/* pointer to next range of values (88 var) */
    struct refmod *rfm;		/* offset and length of refmod */
  }
  refmod_redef;
  int level;			/* level of field 
				   or ASSIGN TO DISK/PRINTER (files) */
  int access_mode;		/* access mode (files) */
  /* or direction (sort files data ) or call_mode for variables */
  int sign;			/* signal type (0=no signal,1=trailing) */
  int len;			/* length of item */
  char defined;			/* first time defined? */
  unsigned pic;			/* picture offset in data segment */
  int linkage_flg;		/* or record offset in stack (files) */
  struct sym *redefines;	/* points to a redefined field 
				   or record symbol (files) */
  struct
  {
    unsigned int just_r:1;
    unsigned int is_pointer:1;
    unsigned int blank:1;
    unsigned int spec_value:1;
    unsigned int value:1;
    unsigned int sync:1;
    unsigned int in_redefinition:1;
    unsigned int separate_sign:1;
    unsigned int leading_sign:1;
    unsigned int optional:1;
    unsigned int reserved:6;
  }
  flags;
  unsigned char slack;		/* slack bytes inserted */
  struct occurs *occurs;	/* for DEPENDING ON or null if fixed table */
};

#define SYMBOL(x)		((struct sym *) (x))
#define SYMBOL_P(x)		(SYMBOL (x)->litflag == 0)

extern struct sym *make_symbol (char *name);
extern struct sym *make_filler (void);


/*
 * expression nodes
 */
struct expr
{
  char litflag;			/* 5 for expr */
  char op;
  struct expr *left;
  struct expr *right;
};

#define EXPR(x)		((struct expr *) (x))
#define EXPR_P(x)	(EXPR (x)->litflag == 5)
#define EXPR_OP(x)	(EXPR (x)->op)
#define EXPR_LEFT(x)	(EXPR (x)->left)
#define EXPR_RIGHT(x)	(EXPR (x)->right)

/*
 * Storage for subscripted variable references.
 * First node is the variable, other are subscripts as 
 * variable/literals with operations.
 * For instance: VAR ( SUB1 - 5, SUB2 + 3 ) is represented as
 * 5 nodes: (sy,op) = (VAR,0) (SUB1,'-') (5,',') (SUB2,'+') (3,',')
 * where the numbers are (struct lit *) pointers and the variables
 * are (struct sym *) pointers.
 */
struct subref
{
  char litflag;			/* ',' = end of subscript, 
				   '+','-' = subscript arith */
  struct subref *next;		/* link to next in list or NULL */
  void *sym;			/* variable/literal at this node */
};

#define SUBREF(x)	((struct subref *) (x))
#define SUBREF_P(x)	(SUBREF (x)->litflag == 2)
#define SUBREF_NEXT(x)	(SUBREF (x)->next)
#define SUBREF_SYM(x)	(SUBREF (x)->sym)



/* Node for refmod's */
struct refmod
{
  char litflag;			/* 4 = refmod */
  struct sym *off;		/* offset from normal start address */
  struct sym *sym;		/* pointer to original var: must be at the same relative offset as sym in subref */
  struct sym *len;		/* corrected length */
  short slot;			/* slot in the data section */
};

#define REFMOD(x)	((struct refmod *) (x))
#define REFMOD_P(x)	(REFMOD (x)->litflag == 4)


/*
 * compile-time list for value ranges of 88-level variables.
 * the first range is stored at the "struct sym", with sym->vr
 * being a pointer to the remaining "struct vrange" nodes.
 */
struct vrange
{
  struct vrange *next;		/* pointer to next range of values (88 var) */
  struct lit *value;		/* pointer to literal with initial value */
  struct lit *value2;		/* pointer to first/next key (sort files) */
};

/*
 * Node for external data (named sections).
 */
struct named_sect
{
  struct named_sect *next;	/* pointer to next named section */
  short sec_no;			/* key: section id */
  char *os_name;		/* name of 01 or 77 data as known by OS */
};

/* this (struct rd) is aliased with (struct sym), so tail data is garbage! */
struct rd
{
  char litflag;
  struct rd *next;
  char *name;
  char type;			/* 'W' for report (RD) */
  struct sym *file;		/* file for writing this report */
  struct list *controls;	/* list of controls (listing breaks) */
  struct list *items;		/* list of all report items */
  int page_limit;
  int heading;
  int footing;
  int first_detail;
  int last_detail;
};

/* additional information for report items */
struct report_info
{
  int line;
  int line_offset;		/* PLUS <offset> given */
  int column;
  int value_source;		/* SUM, SOURCE (from a variable), literal */
  /* the actual source symbol is in (struct sym *)->value */
};

/* varying record range and actual size */
struct rec_varying
{
  struct lit *lmin;
  struct lit *lmax;
  struct sym *reclen;
};

/* selection subject set (evaluate statement) */
struct selsubject
{
  struct selsubject *next;
  int type;
};

/* sort file list for using/giving clauses*/
struct sortfile_node
{
  struct sortfile_node *next;
  struct sym *sy;
};

/* information required by the 'perform ... varying ... after' statements */
struct perf_info
{
  struct sym *pname1;		/* symbol name */
  struct sym *pname2;		/* symbol name */
  unsigned long ljmp;		/* jump label  */
  unsigned long lend;		/* end  label  */
};

struct perform_info
{
  struct perf_info *pf[4];
};

/* information required by the math verbs statements */
struct math_var
{
  struct sym *sname;		/* symbol name */
  unsigned int rounded;		/* rounded option: 0=false, 1=true */
  struct math_var *next;
};

/* information required by the math ON SIZE ERROR statement */
struct math_ose
{
  unsigned long ose;		/* 1=on_size, 2=not_on_size, 3=both */
  unsigned long lbl1;		/* call label name 1 - on_size */
  unsigned long lbl2;		/* call label name 2 - not_on_size */
  unsigned long lbl4;		/* bypass label name  */
};

/* generic information container used by the [NOT] AT END cluases */
struct ginfo
{
  unsigned long sel;		/* 1=true, 2=not true, 3=both */
  unsigned long lbl1;		/* call label name 1 - true */
  unsigned long lbl2;		/* call label name 2 - not true */
  unsigned long lbl3;		/* retrun 1 label name  */
  unsigned long lbl4;		/* retrun 2 label name  */
  unsigned long lbl5;		/* test bypass label name  */
};

/* information required by [NOT] INVALID KEY clauses */
struct invalid_key_element
{
  unsigned long lbl1;		/* skip label */
  unsigned long lbl2;		/* start label */
  unsigned long lbl3;		/* finish label */
};

struct invalid_keys
{
  struct invalid_key_element *invalid_key;
  struct invalid_key_element *not_invalid_key;
};

/******* supplemental information for screen items **********/
/* this is linked at the sym->index (aliased scrinfo) */
struct scr_info
{
  int attr;
  int line;
  int column;
  short int foreground;
  short int background;
  struct sym *from;
  struct sym *to;
  int label;
};

struct converting_struct
{
  struct sym *fromvar;
  struct sym *tovar;
  struct inspect_before_after *before_after;
};

struct tallying_list
{
  struct tallying_list *next;
  struct tallying_for_list *tflist;
  struct sym *count;
};

struct tallying_for_list
{
  struct tallying_for_list *next;
  int options;
  struct sym *forvar;
  struct inspect_before_after *before_after;
};

struct replacing_list
{
  struct replacing_list *next;
  int options;
  struct sym *byvar;
  struct replacing_by_list *replbylist;
  struct inspect_before_after *before_after;
};

struct replacing_by_list
{
  struct replacing_by_list *next;
  struct sym *replvar;
  struct sym *byvar;
  struct inspect_before_after *before_after;
};

struct inspect_before_after
{
  struct sym *before;
  struct sym *after;
};


struct alternate_list
{
  struct alternate_list *next;
  struct sym *key;
  int duplicates;
};

struct unstring_delimited
{
  struct unstring_delimited *next;
  short int all;
  struct sym *var;
};

struct unstring_destinations
{
  struct unstring_destinations *next;
  struct sym *var;
  struct sym *delim;
  struct sym *count;
};

struct string_from
{
  struct string_from *next;
  struct sym *var;
  struct sym *delim;
};

struct list
{
  struct list *next;
  void *var;
};

struct parm_list
{
  struct parm_list *next;
  void *var;
  unsigned location;
  short sec_no;
};

struct coord_pair
{
  int lin;
  int col;
};

struct index_to_table_list
{
  struct index_to_table_list *next;
  char *idxname;
  char *tablename;
  char *keyname;
  char seq;		/* '0' = none, '1' = ASCENDING, '2' = DESCENDING */
};

struct condition
{
  struct sym *sy;		/* implied first operand */
  int oper;			/* operator */
};

/* OCCURS ... DEPENDING ON info */
struct occurs
{
  struct sym *depend;
  int min, max;
};


/*
 * Pair
 */

struct pair {
  void *car;
  struct pair *cdr;
};

#define CAR(x)		((x)->car)
#define CDR(x)		((x)->cdr)

extern struct pair *cons (void *car, struct pair *cdr);
