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
  /* or record offset in stack (files) */
  int occurs_flg;
  int linkage_flg;
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
