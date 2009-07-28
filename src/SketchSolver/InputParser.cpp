/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_dbl = 258,
     T_int = 259,
     T_bool = 260,
     T_ident = 261,
     T_OutIdent = 262,
     T_NativeCode = 263,
     T_string = 264,
     T_true = 265,
     T_false = 266,
     T_vartype = 267,
     T_twoS = 268,
     T_ppls = 269,
     T_mmns = 270,
     T_eq = 271,
     T_neq = 272,
     T_and = 273,
     T_or = 274,
     T_For = 275,
     T_ge = 276,
     T_le = 277,
     T_Table = 278,
     T_Pipeline = 279,
     T_SplitJoin = 280,
     T_Filter = 281,
     T_Native = 282,
     T_NativeMethod = 283,
     T_Work = 284,
     T_Sketches = 285,
     T_OutRate = 286,
     T_new = 287,
     T_InRate = 288,
     T_add = 289,
     T_Init = 290,
     T_setSplitter = 291,
     T_setJoiner = 292,
     T_assert = 293,
     T_eof = 294
   };
#endif
/* Tokens.  */
#define T_dbl 258
#define T_int 259
#define T_bool 260
#define T_ident 261
#define T_OutIdent 262
#define T_NativeCode 263
#define T_string 264
#define T_true 265
#define T_false 266
#define T_vartype 267
#define T_twoS 268
#define T_ppls 269
#define T_mmns 270
#define T_eq 271
#define T_neq 272
#define T_and 273
#define T_or 274
#define T_For 275
#define T_ge 276
#define T_le 277
#define T_Table 278
#define T_Pipeline 279
#define T_SplitJoin 280
#define T_Filter 281
#define T_Native 282
#define T_NativeMethod 283
#define T_Work 284
#define T_Sketches 285
#define T_OutRate 286
#define T_new 287
#define T_InRate 288
#define T_add 289
#define T_Init 290
#define T_setSplitter 291
#define T_setJoiner 292
#define T_assert 293
#define T_eof 294




/* Copy the first part of user declarations.  */
#line 1 "InputParser/InputParser.yy"


using namespace std;

BooleanDAG* currentBD;
stack<string> namestack;
vartype Gvartype;

string *comparisson (string *p1, string *p2, bool_node::Type atype)
{
    Assert (p1 || p2, "Can't have both comparisson's children NULL");
   
    string s1 = currentBD->new_name();
    bool_node *an = newBoolNode(atype);
    an->name = s1;
    currentBD->new_node((p1 ? *p1 : ""), 
			(p2 ? *p2 : ""), an); 
    if (p1)
	delete p1;
    if (p2)
	delete p2;
    return new string(s1); 
}




/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 29 "InputParser/InputParser.yy"
{
	int intConst;
	bool boolConst;
	std::string* strConst;
	double doubleConst;		
	std::list<int>* iList;
	list<bool_node*>* nList;
	list<string*>* sList;
	vartype variableType;
}
/* Line 187 of yacc.c.  */
#line 212 "InputParser.cpp"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 225 "InputParser.cpp"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   251

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  64
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  92
/* YYNRULES -- Number of states.  */
#define YYNSTATES  223

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   294

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    50,     2,     2,    59,    43,    60,     2,
      56,    58,    41,    40,    53,    63,    57,    42,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    47,    54,
      44,    55,    45,    46,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    51,     2,    52,    62,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    48,    61,    49,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     7,    10,    11,    18,    20,    21,
      24,    26,    27,    31,    33,    36,    39,    43,    44,    51,
      58,    60,    64,    65,    68,    70,    75,    80,    92,    99,
     100,   108,   109,   118,   119,   130,   131,   134,   136,   141,
     153,   155,   160,   164,   170,   175,   180,   182,   186,   190,
     194,   198,   202,   206,   210,   217,   221,   225,   229,   233,
     237,   241,   245,   249,   253,   257,   263,   264,   267,   269,
     272,   274,   285,   288,   291,   295,   297,   301,   306,   312,
     314,   318,   322,   324,   328,   332,   336,   340,   342,   345,
     347,   349,   351
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      65,     0,    -1,    66,    39,    -1,    -1,    67,    66,    -1,
      -1,    69,     6,    68,    48,    70,    49,    -1,    26,    -1,
      -1,    79,    70,    -1,     6,    -1,    -1,     6,    72,    71,
      -1,     6,    -1,     6,    73,    -1,    12,     6,    -1,    50,
      12,     6,    -1,    -1,    12,    75,    51,    90,    52,    71,
      -1,    50,    12,    51,    90,    52,    73,    -1,    74,    -1,
      74,    53,    76,    -1,    -1,    77,    78,    -1,    54,    -1,
      33,    55,     4,    54,    -1,    31,    55,     4,    54,    -1,
       6,    56,     6,    57,     6,    53,     6,    57,     6,    58,
      54,    -1,    35,    56,    58,    48,    77,    49,    -1,    -1,
      29,    56,    58,    48,    80,    83,    49,    -1,    -1,     6,
      81,    56,    76,    58,    48,    83,    49,    -1,    -1,     6,
      30,     6,    82,    56,    76,    58,    48,    83,    49,    -1,
      -1,    83,    84,    -1,    54,    -1,     6,    55,    86,    54,
      -1,    59,    88,    13,    87,    59,    51,    86,    52,    55,
      86,    54,    -1,    85,    -1,     7,    55,    86,    54,    -1,
      38,    86,    54,    -1,    38,    86,    47,     9,    54,    -1,
      33,    55,     4,    54,    -1,    31,    55,     4,    54,    -1,
      89,    -1,    89,    60,    89,    -1,    89,    18,    89,    -1,
      89,    61,    89,    -1,    89,    19,    89,    -1,    89,    62,
      89,    -1,    89,    17,    89,    -1,    89,    16,    89,    -1,
      59,    87,    59,    51,    86,    52,    -1,    13,    87,    13,
      -1,    89,    40,    89,    -1,    89,    42,    89,    -1,    89,
      43,    89,    -1,    89,    41,    89,    -1,    89,    63,    89,
      -1,    89,    45,    89,    -1,    89,    44,    89,    -1,    89,
      21,    89,    -1,    89,    22,    89,    -1,    86,    46,    86,
      47,    86,    -1,    -1,    89,    87,    -1,     6,    -1,     6,
      88,    -1,    93,    -1,     6,    51,    12,    52,    56,    87,
      58,    56,    86,    58,    -1,    63,    89,    -1,    50,    89,
      -1,    56,    86,    58,    -1,    94,    -1,    44,    94,    45,
      -1,    44,    94,    93,    45,    -1,    44,    94,    93,    41,
      45,    -1,    91,    -1,    90,    40,    91,    -1,    90,    63,
      91,    -1,    92,    -1,    56,    91,    58,    -1,    91,    41,
      91,    -1,    91,    42,    91,    -1,    91,    43,    91,    -1,
      93,    -1,    63,    93,    -1,     4,    -1,    10,    -1,    11,
      -1,     6,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   109,   109,   112,   113,   116,   116,   121,   125,   126,
     129,   140,   140,   151,   152,   158,   167,   176,   176,   180,
     183,   184,   187,   188,   191,   192,   193,   194,   196,   198,
     197,   209,   207,   222,   220,   239,   240,   243,   244,   249,
     281,   282,   287,   297,   311,   312,   315,   316,   323,   330,
     337,   344,   351,   364,   373,   392,   408,   420,   431,   442,
     452,   468,   471,   474,   477,   480,   497,   498,   509,   513,
     518,   522,   566,   575,   584,   587,   600,   611,   619,   625,
     626,   627,   629,   630,   631,   632,   634,   638,   639,   642,
     643,   644,   646
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "T_dbl", "T_int", "T_bool", "T_ident",
  "T_OutIdent", "T_NativeCode", "T_string", "T_true", "T_false",
  "T_vartype", "T_twoS", "T_ppls", "T_mmns", "T_eq", "T_neq", "T_and",
  "T_or", "T_For", "T_ge", "T_le", "T_Table", "T_Pipeline", "T_SplitJoin",
  "T_Filter", "T_Native", "T_NativeMethod", "T_Work", "T_Sketches",
  "T_OutRate", "T_new", "T_InRate", "T_add", "T_Init", "T_setSplitter",
  "T_setJoiner", "T_assert", "T_eof", "'+'", "'*'", "'/'", "'%'", "'<'",
  "'>'", "'?'", "':'", "'{'", "'}'", "'!'", "'['", "']'", "','", "';'",
  "'='", "'('", "'.'", "')'", "'$'", "'&'", "'|'", "'^'", "'-'", "$accept",
  "Program", "FilterList", "Filter", "@1", "FilterType", "MethodList",
  "InList", "@2", "OutList", "ParamDecl", "@3", "ParamList", "InitBody",
  "InitStatement", "Method", "@4", "@5", "@6", "WorkBody", "WorkStatement",
  "RateSet", "Expression", "varList", "IdentList", "Term", "ConstantExpr",
  "ConstantTerm", "NegConstant", "Constant", "Ident", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
      43,    42,    47,    37,    60,    62,    63,    58,   123,   125,
      33,    91,    93,    44,    59,    61,    40,    46,    41,    36,
      38,   124,    94,    45
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    64,    65,    66,    66,    68,    67,    69,    70,    70,
      71,    72,    71,    73,    73,    74,    74,    75,    74,    74,
      76,    76,    77,    77,    78,    78,    78,    78,    79,    80,
      79,    81,    79,    82,    79,    83,    83,    84,    84,    84,
      84,    84,    84,    84,    85,    85,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    87,    87,    88,    88,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    90,
      90,    90,    91,    91,    91,    91,    91,    92,    92,    93,
      93,    93,    94
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     0,     2,     0,     6,     1,     0,     2,
       1,     0,     3,     1,     2,     2,     3,     0,     6,     6,
       1,     3,     0,     2,     1,     4,     4,    11,     6,     0,
       7,     0,     8,     0,    10,     0,     2,     1,     4,    11,
       1,     4,     3,     5,     4,     4,     1,     3,     3,     3,
       3,     3,     3,     3,     6,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     5,     0,     2,     1,     2,
       1,    10,     2,     2,     3,     1,     3,     4,     5,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     2,     1,
       1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     7,     0,     0,     3,     0,     1,     2,     4,     5,
       0,     8,    31,     0,     0,     0,     8,     0,     0,     0,
       0,     6,     9,    33,     0,     0,     0,     0,    17,     0,
      20,     0,    29,    22,     0,    15,     0,     0,     0,     0,
      35,     0,     0,     0,    16,     0,    21,    35,     0,     0,
       0,     0,    28,    24,    23,     0,    89,    90,    91,     0,
       0,     0,    79,    82,    87,     0,     0,     0,     0,     0,
       0,     0,    30,    37,     0,    36,    40,     0,     0,     0,
      35,     0,    88,     0,     0,     0,     0,     0,     0,     0,
      32,     0,     0,     0,     0,    92,    66,     0,     0,     0,
      66,     0,     0,    46,    70,    75,    68,     0,     0,     0,
       0,     0,    83,    80,    10,    18,    81,    84,    85,    86,
      13,    19,     0,     0,     0,     0,     0,     0,    66,    92,
       0,    73,     0,     0,    72,     0,     0,    42,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,    66,     0,    26,    25,    34,
       0,    14,    38,    41,    45,    44,     0,    55,    67,    76,
       0,    74,     0,     0,     0,    53,    52,    48,    50,    63,
      64,    56,    59,    57,    58,    62,    61,    47,    49,    51,
      60,     0,     0,    12,     0,     0,    77,     0,     0,    43,
       0,     0,    66,    78,     0,    65,     0,     0,     0,    54,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      27,    71,    39
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,     4,    10,     5,    15,   115,   160,   121,
      30,    36,    31,    41,    54,    16,    40,    18,    27,    48,
      75,    76,   102,   127,   107,   103,    61,    62,    63,   104,
     105
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -92
static const yytype_int16 yypact[] =
{
      -8,   -92,     6,   -18,    -8,    82,   -92,   -92,   -92,   -92,
     -17,    85,    48,    39,    42,    53,    85,   127,    79,    95,
     120,   -92,   -92,   -92,   -10,   131,   133,   100,   177,   176,
     137,   135,   -92,   -92,   -10,   -92,   143,    19,   -10,   147,
     -92,    68,   138,    13,   -92,    13,   -92,   -92,    78,   141,
     146,   148,   -92,   -92,   -92,   150,   -92,   -92,   -92,    13,
     166,    40,   132,   -92,   -92,   119,    90,   149,   151,   152,
     153,    23,   -92,   -92,   193,   -92,   -92,   194,   198,   201,
     -92,   -28,   -92,    13,   203,    13,    13,    13,    13,   204,
     -92,    23,    23,   207,   208,   162,    31,   209,    31,    23,
      31,    31,    72,   124,   -92,   -92,   193,   205,   157,   163,
     165,    98,   -92,   132,   210,   -92,   132,   108,   178,   -92,
     204,   -92,    54,    84,   168,   169,   212,   213,    31,   -92,
     144,   -92,   -42,   161,   -92,    23,   216,   -92,    31,    31,
      31,    31,    31,    31,    31,    31,    31,    31,    31,    31,
      31,    31,    31,    31,   -92,    31,   221,   -92,   -92,   -92,
     203,   -92,   -92,   -92,   -92,   -92,   179,   -92,   -92,   -92,
      27,   -92,   181,   145,   174,   -92,   -92,   -92,   -92,   -92,
     -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,
     -92,   170,   180,   -92,   182,   185,   -92,    23,    23,   -92,
     183,   229,    31,   -92,   -24,   -92,    23,   184,   186,   -92,
     -20,   230,   187,   190,   188,    23,    23,   195,    25,   126,
     -92,   -92,   -92
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -92,   -92,   233,   -92,   -92,   -92,   223,    80,   -92,   122,
     -92,   -92,    55,   -92,   -92,   -92,   -92,   -92,   -92,    30,
     -92,   -92,   -91,   -90,   142,   -89,   202,    75,   -92,   -40,
     154
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -12
static const yytype_int16 yytable[] =
{
     122,   123,    28,    64,   135,    64,     6,   128,   132,   131,
     133,   128,   134,    86,    87,    88,   171,    56,     1,    64,
      82,     7,   135,    57,    58,    44,   135,    56,   209,    95,
     112,    11,   213,    57,    58,    56,    96,    95,   168,   128,
      29,    57,    58,    64,   173,    64,    64,    64,    64,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,   190,   191,   128,    97,   195,    59,
      45,   135,   196,    98,    49,    97,    60,    66,    17,    99,
      83,    98,   100,   221,    67,    68,   101,    99,     9,    42,
     170,    12,    84,    46,   101,    19,    67,    68,    20,    50,
     135,    51,    21,    85,    67,    68,   204,   205,   162,    69,
     111,    70,   208,   128,    13,   210,    71,    52,   135,   136,
      14,    69,    53,    70,   218,   219,   137,    72,    71,    69,
     135,    70,    73,    23,    81,    24,    71,    74,   163,    90,
     138,   139,   140,   141,    73,   142,   143,   159,    56,    74,
      87,    88,    73,    25,    57,    58,    34,    74,   113,    83,
     116,   117,   118,   119,   144,   145,   146,   147,   148,   149,
      56,    89,   135,    86,    87,    88,    57,    58,    26,    32,
     222,    33,    85,    35,   150,   151,   152,   153,    37,   169,
      38,   135,   198,    39,    43,    47,    55,    77,    80,   106,
     108,    78,   109,    79,    91,   110,    92,    93,    94,   114,
     120,   124,   125,   126,   156,   129,   -11,   157,   155,   158,
     172,    88,   164,   165,   166,   174,   167,   192,   199,   200,
     203,   194,   197,   201,   206,   207,   214,     8,   202,    22,
     193,   211,   161,   215,   212,   216,   217,    65,   154,   220,
       0,   130
};

static const yytype_int16 yycheck[] =
{
      91,    92,    12,    43,    46,    45,     0,    96,    99,    98,
     100,   100,   101,    41,    42,    43,    58,     4,    26,    59,
      60,    39,    46,    10,    11,     6,    46,     4,    52,     6,
      58,    48,    52,    10,    11,     4,    13,     6,   128,   128,
      50,    10,    11,    83,   135,    85,    86,    87,    88,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   155,   155,    44,    41,    56,
      51,    46,    45,    50,     6,    44,    63,    47,    30,    56,
      40,    50,    59,    58,     6,     7,    63,    56,     6,    34,
     130,     6,    52,    38,    63,    56,     6,     7,    56,    31,
      46,    33,    49,    63,     6,     7,   197,   198,    54,    31,
      80,    33,   202,   202,    29,   206,    38,    49,    46,    47,
      35,    31,    54,    33,   215,   216,    54,    49,    38,    31,
      46,    33,    54,     6,    59,    56,    38,    59,    54,    49,
      16,    17,    18,    19,    54,    21,    22,    49,     4,    59,
      42,    43,    54,    58,    10,    11,    56,    59,    83,    40,
      85,    86,    87,    88,    40,    41,    42,    43,    44,    45,
       4,    52,    46,    41,    42,    43,    10,    11,    58,    48,
      54,    48,    63,     6,    60,    61,    62,    63,    12,    45,
      53,    46,    47,    58,    51,    48,    58,    56,    48,     6,
       6,    55,     4,    55,    55,     4,    55,    55,    55,     6,
       6,     4,     4,    51,    57,     6,     6,    54,    13,    54,
      59,    43,    54,    54,    12,     9,    13,     6,    54,    59,
      45,    52,    51,    53,    51,     6,     6,     4,    56,    16,
     160,    57,   120,    56,    58,    55,    58,    45,   106,    54,
      -1,    97
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    26,    65,    66,    67,    69,     0,    39,    66,     6,
      68,    48,     6,    29,    35,    70,    79,    30,    81,    56,
      56,    49,    70,     6,    56,    58,    58,    82,    12,    50,
      74,    76,    48,    48,    56,     6,    75,    12,    53,    58,
      80,    77,    76,    51,     6,    51,    76,    48,    83,     6,
      31,    33,    49,    54,    78,    58,     4,    10,    11,    56,
      63,    90,    91,    92,    93,    90,    83,     6,     7,    31,
      33,    38,    49,    54,    59,    84,    85,    56,    55,    55,
      48,    91,    93,    40,    52,    63,    41,    42,    43,    52,
      49,    55,    55,    55,    55,     6,    13,    44,    50,    56,
      59,    63,    86,    89,    93,    94,     6,    88,     6,     4,
       4,    83,    58,    91,     6,    71,    91,    91,    91,    91,
       6,    73,    86,    86,     4,     4,    51,    87,    89,     6,
      94,    89,    86,    87,    89,    46,    47,    54,    16,    17,
      18,    19,    21,    22,    40,    41,    42,    43,    44,    45,
      60,    61,    62,    63,    88,    13,    57,    54,    54,    49,
      72,    73,    54,    54,    54,    54,    12,    13,    87,    45,
      93,    58,    59,    86,     9,    89,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    89,
      89,    87,     6,    71,    52,    41,    45,    51,    47,    54,
      59,    53,    56,    45,    86,    86,    51,     6,    87,    52,
      86,    57,    58,    52,     6,    56,    55,    58,    86,    86,
      54,    58,    54
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 109 "InputParser/InputParser.yy"
    {  (yyval.intConst)=0; return 0;}
    break;

  case 3:
#line 112 "InputParser/InputParser.yy"
    { }
    break;

  case 4:
#line 113 "InputParser/InputParser.yy"
    { (yyval.intConst) = 0; }
    break;

  case 5:
#line 116 "InputParser/InputParser.yy"
    {namestack.push(*(yyvsp[(2) - (2)].strConst)); }
    break;

  case 6:
#line 116 "InputParser/InputParser.yy"
    { 
				namestack.pop();
			}
    break;

  case 7:
#line 121 "InputParser/InputParser.yy"
    { }
    break;

  case 8:
#line 125 "InputParser/InputParser.yy"
    {}
    break;

  case 9:
#line 126 "InputParser/InputParser.yy"
    {}
    break;

  case 10:
#line 129 "InputParser/InputParser.yy"
    {  

    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(1) - (1)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(1) - (1)].strConst)); 
	}	

}
    break;

  case 11:
#line 140 "InputParser/InputParser.yy"
    {
	
    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(1) - (1)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(1) - (1)].strConst)); 
	}	
}
    break;

  case 13:
#line 151 "InputParser/InputParser.yy"
    { 	 currentBD->create_outputs(-1, *(yyvsp[(1) - (1)].strConst)); }
    break;

  case 14:
#line 152 "InputParser/InputParser.yy"
    {
	
	currentBD->create_outputs(-1, *(yyvsp[(1) - (2)].strConst));
}
    break;

  case 15:
#line 158 "InputParser/InputParser.yy"
    {  
	if( (yyvsp[(1) - (2)].variableType) == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(2) - (2)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(2) - (2)].strConst)); 
	}	
}
    break;

  case 16:
#line 167 "InputParser/InputParser.yy"
    {
 	 if( (yyvsp[(2) - (3)].variableType) == INT){

		 currentBD->create_outputs(NINPUTS, *(yyvsp[(3) - (3)].strConst));
 	 }else{

	 	 currentBD->create_outputs(-1, *(yyvsp[(3) - (3)].strConst)); 
 	 }
 }
    break;

  case 17:
#line 176 "InputParser/InputParser.yy"
    {
Gvartype = (yyvsp[(1) - (1)].variableType);

 }
    break;

  case 22:
#line 187 "InputParser/InputParser.yy"
    { /* Empty */ }
    break;

  case 23:
#line 188 "InputParser/InputParser.yy"
    { /* */ }
    break;

  case 24:
#line 191 "InputParser/InputParser.yy"
    { /* */ }
    break;

  case 29:
#line 198 "InputParser/InputParser.yy"
    {
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		functionMap["work"] = currentBD;
	}
    break;

  case 30:
#line 205 "InputParser/InputParser.yy"
    { }
    break;

  case 31:
#line 209 "InputParser/InputParser.yy"
    {
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		cout<<"CREATING "<<*(yyvsp[(1) - (1)].strConst)<<endl;
		functionMap[*(yyvsp[(1) - (1)].strConst)] = currentBD;
}
    break;

  case 33:
#line 222 "InputParser/InputParser.yy"
    {
		if( currentBD != NULL){
			//currentBD->print(cout);
		}
		currentBD = new BooleanDAG();
		cout<<*(yyvsp[(1) - (3)].strConst)<<" SKETCHES "<<*(yyvsp[(3) - (3)].strConst)<<endl;
		sketchMap[*(yyvsp[(1) - (3)].strConst)] = currentBD;
		sketches[currentBD] = *(yyvsp[(3) - (3)].strConst);
		delete (yyvsp[(3) - (3)].strConst);
		delete (yyvsp[(1) - (3)].strConst);
}
    break;

  case 35:
#line 239 "InputParser/InputParser.yy"
    { /* Empty */ }
    break;

  case 36:
#line 240 "InputParser/InputParser.yy"
    { /* */ }
    break;

  case 37:
#line 243 "InputParser/InputParser.yy"
    {  (yyval.intConst)=0;  /* */ }
    break;

  case 38:
#line 244 "InputParser/InputParser.yy"
    {
	currentBD->alias( *(yyvsp[(1) - (4)].strConst), *(yyvsp[(3) - (4)].strConst));
	delete (yyvsp[(3) - (4)].strConst);
	delete (yyvsp[(1) - (4)].strConst);
}
    break;

  case 39:
#line 249 "InputParser/InputParser.yy"
    {

	list<string*>* childs = (yyvsp[(2) - (11)].sList);
	list<string*>::reverse_iterator it = childs->rbegin();
	
	list<bool_node*>* oldchilds = (yyvsp[(4) - (11)].nList);
	list<bool_node*>::reverse_iterator oldit = oldchilds->rbegin();
	
	bool_node* rhs;
	rhs = currentBD->get_node(*(yyvsp[(10) - (11)].strConst));
	int bigN = childs->size();
	Assert( bigN == oldchilds->size(), "This can't happen");	

	for(int i=0; i<bigN; ++i, ++it, ++oldit){
		string s1 = currentBD->new_name();
		ARRASS_node* an = dynamic_cast<ARRASS_node*>(newArithNode(arith_node::ARRASS));
		an->multi_mother.reserve(2);
		an->multi_mother.push_back(*oldit);			
		an->multi_mother.push_back(rhs);
		an->name = s1;
		Assert( rhs != NULL, "AAARRRGH This shouldn't happen !!");
		Assert((yyvsp[(7) - (11)].strConst) != NULL, "1: THIS CAN'T HAPPEN!!");
		currentBD->new_node(*(yyvsp[(7) - (11)].strConst),  "",  an);
		currentBD->alias( *(*it), s1);
		an->quant = i;
		delete *it;
	}
	delete childs;
	delete oldchilds;
	delete (yyvsp[(7) - (11)].strConst);
	delete (yyvsp[(10) - (11)].strConst);
}
    break;

  case 40:
#line 281 "InputParser/InputParser.yy"
    {}
    break;

  case 41:
#line 282 "InputParser/InputParser.yy"
    {
	currentBD->new_node(*(yyvsp[(3) - (4)].strConst),  "",  bool_node::DST, *(yyvsp[(1) - (4)].strConst));
	delete (yyvsp[(3) - (4)].strConst);
	delete (yyvsp[(1) - (4)].strConst);
}
    break;

  case 42:
#line 287 "InputParser/InputParser.yy"
    {
  if ((yyvsp[(2) - (3)].strConst)) {
    /* Asserting an expression, construct assert node. */
//    cout << "Generating assertion node..." << endl;
    string s = currentBD->new_name ();
    currentBD->new_node (*(yyvsp[(2) - (3)].strConst), "", bool_node::ASSERT, s);
//    cout << "Assertion node created, name=" << s << endl;
    delete (yyvsp[(2) - (3)].strConst);
  }
}
    break;

  case 43:
#line 297 "InputParser/InputParser.yy"
    {
  if ((yyvsp[(2) - (5)].strConst)) {
    /* Asserting an expression, construct assert node. */
//    cout << "Generating assertion node..." << endl;
    string s = currentBD->new_name ();
    bool_node* bn = currentBD->new_node (*(yyvsp[(2) - (5)].strConst), "", bool_node::ASSERT, s);
    dynamic_cast<ASSERT_node*>(bn)->setMsg(*(yyvsp[(4) - (5)].strConst));
//    cout << "Assertion node created, name=" << s << endl;
    delete (yyvsp[(2) - (5)].strConst);
    delete (yyvsp[(4) - (5)].strConst);
  }
}
    break;

  case 44:
#line 311 "InputParser/InputParser.yy"
    { currentBD->create_inputs((yyvsp[(3) - (4)].intConst)); }
    break;

  case 45:
#line 312 "InputParser/InputParser.yy"
    { currentBD->create_outputs((yyvsp[(3) - (4)].intConst)); }
    break;

  case 46:
#line 315 "InputParser/InputParser.yy"
    { (yyval.strConst) = (yyvsp[(1) - (1)].strConst); }
    break;

  case 47:
#line 316 "InputParser/InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::AND, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);	  					  
}
    break;

  case 48:
#line 323 "InputParser/InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::AND, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);	  					  
}
    break;

  case 49:
#line 330 "InputParser/InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::OR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);		  					  
}
    break;

  case 50:
#line 337 "InputParser/InputParser.yy"
    { 	
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::OR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
}
    break;

  case 51:
#line 344 "InputParser/InputParser.yy"
    {	
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::XOR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
}
    break;

  case 52:
#line 351 "InputParser/InputParser.yy"
    { 
	bool_node* lChild=NULL;
	bool_node* rChild=NULL;
	bool_node* bn = currentBD->get_node(*(yyvsp[(1) - (3)].strConst));
	lChild = dynamic_cast<bool_node*>(bn);
	bn = currentBD->get_node(*(yyvsp[(3) - (3)].strConst));
	rChild = dynamic_cast<bool_node*>(bn);			
	string* tmp = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::EQ);
    string s = currentBD->new_name ();
    currentBD->new_node (*tmp, "", bool_node::NOT, s);
    delete tmp;
    (yyval.strConst) = new string(s);
}
    break;

  case 53:
#line 364 "InputParser/InputParser.yy"
    { 	
	bool_node* lChild=NULL;
	bool_node* rChild=NULL;
	bool_node* bn = currentBD->get_node(*(yyvsp[(1) - (3)].strConst));
	lChild = dynamic_cast<bool_node*>(bn);
	bn = currentBD->get_node(*(yyvsp[(3) - (3)].strConst));
	rChild = dynamic_cast<bool_node*>(bn);			
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::EQ);
}
    break;

  case 54:
#line 373 "InputParser/InputParser.yy"
    {
	int pushval = 0;
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ARRACC);
	list<bool_node*>* childs = (yyvsp[(2) - (6)].nList);
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}		
	Assert((yyvsp[(5) - (6)].strConst) != NULL, "2: THIS CAN'T HAPPEN!!");
	an->name = s1;
	currentBD->new_node(*(yyvsp[(5) - (6)].strConst), "",  an); 
	(yyval.strConst) = new string(s1);
	delete childs;
	delete (yyvsp[(5) - (6)].strConst);
}
    break;

  case 55:
#line 392 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ACTRL);
	list<bool_node*>* childs = (yyvsp[(2) - (3)].nList);
	list<bool_node*>::reverse_iterator it = childs->rbegin();
	int bigN = childs->size();
	an->multi_mother.reserve(bigN);
	for(int i=0; i<bigN; ++i, ++it){
		an->multi_mother.push_back(*it);
	}	
	an->name = s1;
	currentBD->new_node("", "", an); 
	(yyval.strConst) = new string(s1);  
	delete childs;
}
    break;

  case 56:
#line 408 "InputParser/InputParser.yy"
    {
	currentBD->moveNNb();
	string s1 = currentBD->new_name();
	bool_node* an = newBoolNode(bool_node::PLUS);
	Assert((yyvsp[(1) - (3)].strConst) != NULL && (yyvsp[(3) - (3)].strConst) != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), an); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1); 
}
    break;

  case 57:
#line 420 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	bool_node* an = newBoolNode(bool_node::DIV);
	Assert((yyvsp[(1) - (3)].strConst) != NULL && (yyvsp[(3) - (3)].strConst) != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst), *(yyvsp[(3) - (3)].strConst), an); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1);
}
    break;

  case 58:
#line 431 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	bool_node* an = newBoolNode(bool_node::MOD);
	Assert((yyvsp[(1) - (3)].strConst) != NULL && (yyvsp[(3) - (3)].strConst) != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst), *(yyvsp[(3) - (3)].strConst), an); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1);
}
    break;

  case 59:
#line 442 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	bool_node* an = newBoolNode(bool_node::TIMES);
	Assert((yyvsp[(1) - (3)].strConst) != NULL && (yyvsp[(3) - (3)].strConst) != NULL, "THIS CAN't Happen, const * const should have been taken care of by frontend.");
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst), *(yyvsp[(3) - (3)].strConst), an); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1);
}
    break;

  case 60:
#line 452 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	bool_node* an = newBoolNode(bool_node::PLUS);	
	string neg1 = currentBD->new_name();
	bool_node* negn = newBoolNode(bool_node::NEG);	
	negn->name = neg1;
	currentBD->new_node(*(yyvsp[(3) - (3)].strConst), "", negn);
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst), neg1, an); 
	
	
	Assert((yyvsp[(1) - (3)].strConst) != NULL && (yyvsp[(3) - (3)].strConst) != NULL, "THIS CAN't Happen");	
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1);
}
    break;

  case 61:
#line 468 "InputParser/InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::GT);
}
    break;

  case 62:
#line 471 "InputParser/InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::LT);
}
    break;

  case 63:
#line 474 "InputParser/InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::GE);
}
    break;

  case 64:
#line 477 "InputParser/InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::LE);
}
    break;

  case 65:
#line 480 "InputParser/InputParser.yy"
    {
	string s1 = currentBD->new_name();
	arith_node* an = newArithNode(arith_node::ARRACC);
	bool_node* yesChild = currentBD->get_node(*(yyvsp[(3) - (5)].strConst));
	bool_node* noChild = currentBD->get_node(*(yyvsp[(5) - (5)].strConst));
	an->multi_mother.push_back( noChild );
	an->multi_mother.push_back( yesChild );
	(yyval.strConst) = new string(s1);
	an->name = s1;
	currentBD->new_node(*(yyvsp[(1) - (5)].strConst), "", an); 
	if( (yyvsp[(1) - (5)].strConst) != NULL && (yyvsp[(1) - (5)].strConst) != (yyval.strConst)){ delete (yyvsp[(1) - (5)].strConst); }
	if( (yyvsp[(3) - (5)].strConst) != NULL && (yyvsp[(3) - (5)].strConst) != (yyval.strConst)){ delete (yyvsp[(3) - (5)].strConst); }
	if( (yyvsp[(5) - (5)].strConst) != NULL && (yyvsp[(5) - (5)].strConst) != (yyval.strConst)){ delete (yyvsp[(5) - (5)].strConst); }		  					  
}
    break;

  case 66:
#line 497 "InputParser/InputParser.yy"
    { /* Empty */  	(yyval.nList) = new list<bool_node*>();	}
    break;

  case 67:
#line 498 "InputParser/InputParser.yy"
    {
//The signs are already in the stack by default. All I have to do is not remove them.
	if((yyvsp[(1) - (2)].strConst) != NULL){
		(yyvsp[(2) - (2)].nList)->push_back( currentBD->get_node(*(yyvsp[(1) - (2)].strConst)) );
		delete (yyvsp[(1) - (2)].strConst);
	}else{
		(yyvsp[(2) - (2)].nList)->push_back( NULL );
	}
	(yyval.nList) = (yyvsp[(2) - (2)].nList);
}
    break;

  case 68:
#line 509 "InputParser/InputParser.yy"
    {
	(yyval.sList) = new list<string*>();	
	(yyval.sList)->push_back( (yyvsp[(1) - (1)].strConst));
}
    break;

  case 69:
#line 513 "InputParser/InputParser.yy"
    {
	(yyval.sList) = (yyvsp[(2) - (2)].sList);
	(yyval.sList)->push_back( (yyvsp[(1) - (2)].strConst));
}
    break;

  case 70:
#line 518 "InputParser/InputParser.yy"
    {
				 (yyval.strConst) = new string(currentBD->create_const((yyvsp[(1) - (1)].intConst)));
				 }
    break;

  case 71:
#line 522 "InputParser/InputParser.yy"
    {
	
	list<bool_node*>* params = (yyvsp[(6) - (10)].nList);
	if(params->size() == 0){
		if( (yyvsp[(3) - (10)].variableType) == INT){

			currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(1) - (10)].strConst)); 
		}else{

			currentBD->create_inputs(-1, *(yyvsp[(1) - (10)].strConst));
		}
		(yyval.strConst) = (yyvsp[(1) - (10)].strConst);
	}else{	
		string& fname = *(yyvsp[(1) - (10)].strConst);
		list<bool_node*>::reverse_iterator parit = params->rbegin();
		UFUN_node* ufun = new UFUN_node(fname);
		for( ; parit != params->rend(); ++parit){
			ufun->multi_mother.push_back((*parit));
		}
		
		if( (yyvsp[(3) - (10)].variableType) == INT){
			ufun->set_nbits( 2 /*NINPUTS*/  );
		}else{
	
			ufun->set_nbits( 1  );
		}
		
				
		string s1;
		{
			stringstream str;
			str<<fname<<"_"<<currentBD->size();
			s1 = str.str();
		}
		ufun->name = s1;
		currentBD->new_node(currentBD->get_node(*(yyvsp[(9) - (10)].strConst)), NULL, ufun, s1);
		
		(yyval.strConst) = new string(s1);
		delete (yyvsp[(1) - (10)].strConst);
	}
	delete (yyvsp[(6) - (10)].nList);
	delete (yyvsp[(9) - (10)].strConst);
}
    break;

  case 72:
#line 566 "InputParser/InputParser.yy"
    {
	string neg1 = currentBD->new_name();
	bool_node* negn = newBoolNode(bool_node::NEG);
	negn->name = neg1;
	currentBD->new_node(*(yyvsp[(2) - (2)].strConst), "", negn);
	Assert((yyvsp[(2) - (2)].strConst) != NULL, "THIS CAN't Happen");	
	delete (yyvsp[(2) - (2)].strConst);
	(yyval.strConst) = new string(neg1);
}
    break;

  case 73:
#line 575 "InputParser/InputParser.yy"
    { 
    /* Check the Boolean coefficient of the term, being either 0 (false) or 1 (true). */
    /* Generate an alternating NOT node, push a unit (true) coefficient. */
    string s = currentBD->new_name ();
    currentBD->new_node (*(yyvsp[(2) - (2)].strConst), "", bool_node::NOT, s);
    (yyval.strConst) = new string (s);
    delete (yyvsp[(2) - (2)].strConst);
}
    break;

  case 74:
#line 584 "InputParser/InputParser.yy"
    { 
						(yyval.strConst) = (yyvsp[(2) - (3)].strConst); 
						}
    break;

  case 75:
#line 587 "InputParser/InputParser.yy"
    { 
			if( !currentBD->has_alias(*(yyvsp[(1) - (1)].strConst)) ){ 
				(yyval.strConst) = (yyvsp[(1) - (1)].strConst);
			}else{ 
				string alias(currentBD->get_alias(*(yyvsp[(1) - (1)].strConst))); 
				if(alias == ""){
					(yyval.strConst) = NULL;
				}else{
					(yyval.strConst) = new string( alias ); 
				}  
				delete (yyvsp[(1) - (1)].strConst);
			} 
		}
    break;

  case 76:
#line 600 "InputParser/InputParser.yy"
    {
	currentBD->create_controls(-1, *(yyvsp[(2) - (3)].strConst));
	if( !currentBD->has_alias(*(yyvsp[(2) - (3)].strConst)) ){ 
		(yyval.strConst) = (yyvsp[(2) - (3)].strConst);
	}else{ 
		Assert( false, "THIS SHOULD NEVER HAPPEN !!!!!!!!!!!!!!!!");
		string alias(currentBD->get_alias(*(yyvsp[(2) - (3)].strConst))); 
		(yyval.strConst) = new string( alias);
		delete (yyvsp[(2) - (3)].strConst);
	} 
}
    break;

  case 77:
#line 611 "InputParser/InputParser.yy"
    {
	int nctrls = (yyvsp[(3) - (4)].intConst);
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	int N=currentBD->create_controls(nctrls, *(yyvsp[(2) - (4)].strConst));
	(yyval.strConst) = (yyvsp[(2) - (4)].strConst);
}
    break;

  case 78:
#line 619 "InputParser/InputParser.yy"
    {
	int N=currentBD->create_controls((yyvsp[(3) - (5)].intConst), *(yyvsp[(2) - (5)].strConst));
	(yyval.strConst) = (yyvsp[(2) - (5)].strConst);

}
    break;

  case 79:
#line 625 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 80:
#line 626 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) + (yyvsp[(3) - (3)].intConst); }
    break;

  case 81:
#line 627 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) - (yyvsp[(3) - (3)].intConst); }
    break;

  case 82:
#line 629 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 83:
#line 630 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(2) - (3)].intConst); }
    break;

  case 84:
#line 631 "InputParser/InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) * (yyvsp[(3) - (3)].intConst); }
    break;

  case 85:
#line 632 "InputParser/InputParser.yy"
    { Assert( (yyvsp[(3) - (3)].intConst) != 0, "You are attempting to divide by zero !!");
							      (yyval.intConst) = (yyvsp[(1) - (3)].intConst) / (yyvsp[(3) - (3)].intConst); }
    break;

  case 86:
#line 634 "InputParser/InputParser.yy"
    { Assert( (yyvsp[(3) - (3)].intConst) != 0, "You are attempting to mod by zero !!");
							      (yyval.intConst) = (yyvsp[(1) - (3)].intConst) % (yyvsp[(3) - (3)].intConst); }
    break;

  case 87:
#line 638 "InputParser/InputParser.yy"
    {  (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 88:
#line 639 "InputParser/InputParser.yy"
    {  (yyval.intConst) = -(yyvsp[(2) - (2)].intConst); }
    break;

  case 89:
#line 642 "InputParser/InputParser.yy"
    {  (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 90:
#line 643 "InputParser/InputParser.yy"
    { (yyval.intConst) = 1; }
    break;

  case 91:
#line 644 "InputParser/InputParser.yy"
    { (yyval.intConst) = 0; }
    break;

  case 92:
#line 646 "InputParser/InputParser.yy"
    { (yyval.strConst)=(yyvsp[(1) - (1)].strConst); }
    break;


/* Line 1267 of yacc.c.  */
#line 2409 "InputParser.cpp"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 648 "InputParser/InputParser.yy"



void Inityyparse(){

	 	
}

void yyerror(char* c){
	Assert(false, c); 
}


int isatty(int i){



return 1;
}

