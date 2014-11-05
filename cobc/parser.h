/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    ACCEPT = 258,
    ACCESS = 259,
    ADD = 260,
    ADDRESS = 261,
    ADVANCING = 262,
    AFTER = 263,
    ALL = 264,
    ALLOCATE = 265,
    ALPHABET = 266,
    ALPHABETIC = 267,
    ALPHABETIC_LOWER = 268,
    ALPHABETIC_UPPER = 269,
    ALPHANUMERIC = 270,
    ALPHANUMERIC_EDITED = 271,
    ALSO = 272,
    ALTER = 273,
    ALTERNATE = 274,
    AND = 275,
    ANY = 276,
    ARE = 277,
    AREA = 278,
    ARGUMENT_NUMBER = 279,
    ARGUMENT_VALUE = 280,
    AS = 281,
    ASCENDING = 282,
    ASCII = 283,
    ASSIGN = 284,
    AT = 285,
    ATTRIBUTE = 286,
    AUTO = 287,
    AUTOMATIC = 288,
    AWAY_FROM_ZERO = 289,
    BACKGROUND_COLOR = 290,
    BASED = 291,
    BEFORE = 292,
    BELL = 293,
    BINARY = 294,
    BINARY_C_LONG = 295,
    BINARY_CHAR = 296,
    BINARY_DOUBLE = 297,
    BINARY_LONG = 298,
    BINARY_SHORT = 299,
    BLANK = 300,
    BLINK = 301,
    BLOCK = 302,
    BOTTOM = 303,
    BY = 304,
    BYTE_LENGTH = 305,
    CALL = 306,
    CANCEL = 307,
    CAPACITY = 308,
    CF = 309,
    CH = 310,
    CHAINING = 311,
    CHARACTER = 312,
    CHARACTERS = 313,
    CLASS = 314,
    CLASSIFICATION = 315,
    CLOSE = 316,
    CODE = 317,
    CODE_SET = 318,
    COLLATING = 319,
    COL = 320,
    COLS = 321,
    COLUMN = 322,
    COLUMNS = 323,
    COMMA = 324,
    COMMAND_LINE = 325,
    COMMA_DELIM = 326,
    COMMIT = 327,
    COMMON = 328,
    COMP = 329,
    COMPUTE = 330,
    COMP_1 = 331,
    COMP_2 = 332,
    COMP_3 = 333,
    COMP_4 = 334,
    COMP_5 = 335,
    COMP_6 = 336,
    COMP_X = 337,
    CONCATENATE_FUNC = 338,
    CONDITION = 339,
    CONFIGURATION = 340,
    CONSTANT = 341,
    CONTAINS = 342,
    CONTENT = 343,
    CONTINUE = 344,
    CONTROL = 345,
    CONTROLS = 346,
    CONVERSION = 347,
    CONVERTING = 348,
    COPY = 349,
    CORRESPONDING = 350,
    COUNT = 351,
    CRT = 352,
    CRT_UNDER = 353,
    CURRENCY = 354,
    CURRENT_DATE_FUNC = 355,
    CURSOR = 356,
    CYCLE = 357,
    DATA = 358,
    DATE = 359,
    DAY = 360,
    DAY_OF_WEEK = 361,
    DE = 362,
    DEBUGGING = 363,
    DECIMAL_POINT = 364,
    DECLARATIVES = 365,
    DEFAULT = 366,
    DELETE = 367,
    DELIMITED = 368,
    DELIMITER = 369,
    DEPENDING = 370,
    DESCENDING = 371,
    DETAIL = 372,
    DISC = 373,
    DISK = 374,
    DISPLAY = 375,
    DISPLAY_OF_FUNC = 376,
    DIVIDE = 377,
    DIVISION = 378,
    DOWN = 379,
    DUPLICATES = 380,
    DYNAMIC = 381,
    EBCDIC = 382,
    EC = 383,
    ELSE = 384,
    END = 385,
    END_ACCEPT = 386,
    END_ADD = 387,
    END_CALL = 388,
    END_COMPUTE = 389,
    END_DELETE = 390,
    END_DISPLAY = 391,
    END_DIVIDE = 392,
    END_EVALUATE = 393,
    END_FUNCTION = 394,
    END_IF = 395,
    END_MULTIPLY = 396,
    END_PERFORM = 397,
    END_PROGRAM = 398,
    END_READ = 399,
    END_RETURN = 400,
    END_REWRITE = 401,
    END_SEARCH = 402,
    END_START = 403,
    END_STRING = 404,
    END_SUBTRACT = 405,
    END_UNSTRING = 406,
    END_WRITE = 407,
    ENTRY = 408,
    ENVIRONMENT = 409,
    ENVIRONMENT_NAME = 410,
    ENVIRONMENT_VALUE = 411,
    EOL = 412,
    EOP = 413,
    EOS = 414,
    EQUAL = 415,
    ERASE = 416,
    ERROR = 417,
    ESCAPE = 418,
    EVALUATE = 419,
    EVENT_STATUS = 420,
    EXCEPTION = 421,
    EXCEPTION_CONDITION = 422,
    EXCLUSIVE = 423,
    EXIT = 424,
    EXPONENTIATION = 425,
    EXTEND = 426,
    EXTERNAL = 427,
    FD = 428,
    FILE_CONTROL = 429,
    FILE_ID = 430,
    FILLER = 431,
    FINAL = 432,
    FIRST = 433,
    FLOAT_BINARY_128 = 434,
    FLOAT_BINARY_32 = 435,
    FLOAT_BINARY_64 = 436,
    FLOAT_DECIMAL_16 = 437,
    FLOAT_DECIMAL_34 = 438,
    FLOAT_DECIMAL_7 = 439,
    FLOAT_EXTENDED = 440,
    FLOAT_LONG = 441,
    FLOAT_SHORT = 442,
    FOOTING = 443,
    FOR = 444,
    FOREGROUND_COLOR = 445,
    FOREVER = 446,
    FORMATTED_DATE_FUNC = 447,
    FORMATTED_DATETIME_FUNC = 448,
    FORMATTED_TIME_FUNC = 449,
    FREE = 450,
    FROM = 451,
    FROM_CRT = 452,
    FULL = 453,
    FUNCTION = 454,
    FUNCTION_ID = 455,
    FUNCTION_NAME = 456,
    GENERATE = 457,
    GIVING = 458,
    GLOBAL = 459,
    GO = 460,
    GOBACK = 461,
    GREATER = 462,
    GREATER_OR_EQUAL = 463,
    GROUP = 464,
    HEADING = 465,
    HIGHLIGHT = 466,
    HIGH_VALUE = 467,
    ID = 468,
    IDENTIFICATION = 469,
    IF = 470,
    IGNORE = 471,
    IGNORING = 472,
    IN = 473,
    INDEX = 474,
    INDEXED = 475,
    INDICATE = 476,
    INITIALIZE = 477,
    INITIALIZED = 478,
    INITIATE = 479,
    INPUT = 480,
    INPUT_OUTPUT = 481,
    INSPECT = 482,
    INTO = 483,
    INTRINSIC = 484,
    INVALID = 485,
    INVALID_KEY = 486,
    IS = 487,
    I_O = 488,
    I_O_CONTROL = 489,
    JUSTIFIED = 490,
    KEPT = 491,
    KEY = 492,
    KEYBOARD = 493,
    LABEL = 494,
    LAST = 495,
    LEADING = 496,
    LEFT = 497,
    LEFTLINE = 498,
    LENGTH = 499,
    LENGTH_OF = 500,
    LESS = 501,
    LESS_OR_EQUAL = 502,
    LIMIT = 503,
    LIMITS = 504,
    LINAGE = 505,
    LINAGE_COUNTER = 506,
    LINE = 507,
    LINE_COUNTER = 508,
    LINES = 509,
    LINKAGE = 510,
    LITERAL = 511,
    LOCALE = 512,
    LOCALE_DATE_FUNC = 513,
    LOCALE_TIME_FUNC = 514,
    LOCALE_TIME_FROM_FUNC = 515,
    LOCAL_STORAGE = 516,
    LOCK = 517,
    LOWER = 518,
    LOWER_CASE_FUNC = 519,
    LOWLIGHT = 520,
    LOW_VALUE = 521,
    MANUAL = 522,
    MEMORY = 523,
    MERGE = 524,
    MINUS = 525,
    MNEMONIC_NAME = 526,
    MODE = 527,
    MOVE = 528,
    MULTIPLE = 529,
    MULTIPLY = 530,
    NAME = 531,
    NATIONAL = 532,
    NATIONAL_EDITED = 533,
    NATIONAL_OF_FUNC = 534,
    NATIVE = 535,
    NEAREST_AWAY_FROM_ZERO = 536,
    NEAREST_EVEN = 537,
    NEAREST_TOWARD_ZERO = 538,
    NEGATIVE = 539,
    NEXT = 540,
    NEXT_PAGE = 541,
    NO = 542,
    NO_ECHO = 543,
    NORMAL = 544,
    NOT = 545,
    NOT_END = 546,
    NOT_EOP = 547,
    NOT_EQUAL = 548,
    NOT_EXCEPTION = 549,
    NOT_INVALID_KEY = 550,
    NOT_OVERFLOW = 551,
    NOT_SIZE_ERROR = 552,
    NO_ADVANCING = 553,
    NUMBER = 554,
    NUMBERS = 555,
    NUMERIC = 556,
    NUMERIC_EDITED = 557,
    NUMVALC_FUNC = 558,
    OBJECT_COMPUTER = 559,
    OCCURS = 560,
    OF = 561,
    OFF = 562,
    OMITTED = 563,
    ON = 564,
    ONLY = 565,
    OPEN = 566,
    OPTIONAL = 567,
    OR = 568,
    ORDER = 569,
    ORGANIZATION = 570,
    OTHER = 571,
    OUTPUT = 572,
    OVERLINE = 573,
    PACKED_DECIMAL = 574,
    PADDING = 575,
    PAGE = 576,
    PAGE_COUNTER = 577,
    PARAGRAPH = 578,
    PERFORM = 579,
    PH = 580,
    PF = 581,
    PICTURE = 582,
    PICTURE_SYMBOL = 583,
    PLUS = 584,
    POINTER = 585,
    POSITION = 586,
    POSITIVE = 587,
    PRESENT = 588,
    PREVIOUS = 589,
    PRINTER = 590,
    PRINTING = 591,
    PROCEDURE = 592,
    PROCEDURES = 593,
    PROCEED = 594,
    PROGRAM = 595,
    PROGRAM_ID = 596,
    PROGRAM_NAME = 597,
    PROGRAM_POINTER = 598,
    PROHIBITED = 599,
    PROMPT = 600,
    QUOTE = 601,
    RANDOM = 602,
    RD = 603,
    READ = 604,
    READY_TRACE = 605,
    RECORD = 606,
    RECORDING = 607,
    RECORDS = 608,
    RECURSIVE = 609,
    REDEFINES = 610,
    REEL = 611,
    REFERENCE = 612,
    REFERENCES = 613,
    RELATIVE = 614,
    RELEASE = 615,
    REMAINDER = 616,
    REMOVAL = 617,
    RENAMES = 618,
    REPLACE = 619,
    REPLACING = 620,
    REPORT = 621,
    REPORTING = 622,
    REPORTS = 623,
    REPOSITORY = 624,
    REPO_FUNCTION = 625,
    REQUIRED = 626,
    RESERVE = 627,
    RESET = 628,
    RESET_TRACE = 629,
    RETURN = 630,
    RETURNING = 631,
    REVERSE_FUNC = 632,
    REVERSE_VIDEO = 633,
    REVERSED = 634,
    REWIND = 635,
    REWRITE = 636,
    RF = 637,
    RH = 638,
    RIGHT = 639,
    ROLLBACK = 640,
    ROUNDED = 641,
    RUN = 642,
    SAME = 643,
    SCREEN = 644,
    SCREEN_CONTROL = 645,
    SCROLL = 646,
    SD = 647,
    SEARCH = 648,
    SECTION = 649,
    SECURE = 650,
    SEGMENT_LIMIT = 651,
    SELECT = 652,
    SEMI_COLON = 653,
    SENTENCE = 654,
    SEPARATE = 655,
    SEQUENCE = 656,
    SEQUENTIAL = 657,
    SET = 658,
    SHARING = 659,
    SIGN = 660,
    SIGNED = 661,
    SIGNED_INT = 662,
    SIGNED_LONG = 663,
    SIGNED_SHORT = 664,
    SIZE = 665,
    SIZE_ERROR = 666,
    SORT = 667,
    SORT_MERGE = 668,
    SOURCE = 669,
    SOURCE_COMPUTER = 670,
    SPACE = 671,
    SPECIAL_NAMES = 672,
    STANDARD = 673,
    STANDARD_1 = 674,
    STANDARD_2 = 675,
    START = 676,
    STATIC = 677,
    STATUS = 678,
    STDCALL = 679,
    STEP = 680,
    STOP = 681,
    STRING = 682,
    SUBSTITUTE_FUNC = 683,
    SUBSTITUTE_CASE_FUNC = 684,
    SUBTRACT = 685,
    SUM = 686,
    SUPPRESS = 687,
    SYMBOLIC = 688,
    SYNCHRONIZED = 689,
    SYSTEM_DEFAULT = 690,
    TAB = 691,
    TALLYING = 692,
    TAPE = 693,
    TERMINATE = 694,
    TEST = 695,
    THAN = 696,
    THEN = 697,
    THRU = 698,
    TIME = 699,
    TIMEOUT = 700,
    TIMES = 701,
    TO = 702,
    TOK_AMPER = 703,
    TOK_CLOSE_PAREN = 704,
    TOK_COLON = 705,
    TOK_DIV = 706,
    TOK_DOT = 707,
    TOK_EQUAL = 708,
    TOK_FALSE = 709,
    TOK_FILE = 710,
    TOK_GREATER = 711,
    TOK_INITIAL = 712,
    TOK_LESS = 713,
    TOK_MINUS = 714,
    TOK_MUL = 715,
    TOK_NULL = 716,
    TOK_OVERFLOW = 717,
    TOK_OPEN_PAREN = 718,
    TOK_PLUS = 719,
    TOK_TRUE = 720,
    TOP = 721,
    TOWARD_GREATER = 722,
    TOWARD_LESSER = 723,
    TRAILING = 724,
    TRANSFORM = 725,
    TRIM_FUNC = 726,
    TRUNCATION = 727,
    TYPE = 728,
    UNDERLINE = 729,
    UNIT = 730,
    UNLOCK = 731,
    UNSIGNED = 732,
    UNSIGNED_INT = 733,
    UNSIGNED_LONG = 734,
    UNSIGNED_SHORT = 735,
    UNSTRING = 736,
    UNTIL = 737,
    UP = 738,
    UPDATE = 739,
    UPON = 740,
    UPON_ARGUMENT_NUMBER = 741,
    UPON_COMMAND_LINE = 742,
    UPON_ENVIRONMENT_NAME = 743,
    UPON_ENVIRONMENT_VALUE = 744,
    UPPER = 745,
    UPPER_CASE_FUNC = 746,
    USAGE = 747,
    USE = 748,
    USER = 749,
    USER_DEFAULT = 750,
    USER_FUNCTION_NAME = 751,
    USER_REPO_FUNCTION = 752,
    USING = 753,
    VALUE = 754,
    VARYING = 755,
    WAIT = 756,
    WHEN = 757,
    WHEN_COMPILED_FUNC = 758,
    WITH = 759,
    WORD = 760,
    WORDS = 761,
    WORKING_STORAGE = 762,
    WRITE = 763,
    YYYYDDD = 764,
    YYYYMMDD = 765,
    ZERO = 766,
    SHIFT_PREFER = 767
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */
