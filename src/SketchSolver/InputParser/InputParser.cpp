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
#define YYPURE 1

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
     T_Native = 278,
     T_NativeMethod = 279,
     T_Sketches = 280,
     T_new = 281,
     T_add = 282,
     T_Init = 283,
     T_def = 284,
     T_assert = 285,
     T_eof = 286
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
#define T_Native 278
#define T_NativeMethod 279
#define T_Sketches 280
#define T_new 281
#define T_add 282
#define T_Init 283
#define T_def 284
#define T_assert 285
#define T_eof 286




/* Copy the first part of user declarations.  */
#line 1 "InputParser.yy"


using namespace std;

BooleanDAGCreator* currentBD;
stack<string> namestack;
vartype Gvartype;

string *comparisson (string *p1, string *p2, bool_node::Type atype)
{
    Assert (p1 || p2, "Can't have both comparisson's children NULL");
   
    string s1 = currentBD->new_name();
    
    currentBD->new_node((p1 ? *p1 : ""), 
			(p2 ? *p2 : ""), atype, s1); 
    if (p1)
	delete p1;
    if (p2)
	delete p2;
    return new string(s1); 
}





#define YYLEX_PARAM yyscanner
#define YYPARSE_PARAM yyscanner
#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);



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
#line 37 "InputParser.yy"
{
	int intConst;
	bool boolConst;
	std::string* strConst;
	double doubleConst;		
	std::list<int>* iList;
	list<bool_node*>* nList;
	list<string*>* sList;
	vartype variableType;
	BooleanDAG* bdag;
}
/* Line 187 of yacc.c.  */
#line 204 "InputParser.cpp"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 217 "InputParser.cpp"

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
#define YYFINAL  11
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   200

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  26
/* YYNRULES -- Number of rules.  */
#define YYNRULES  81
/* YYNRULES -- Number of states.  */
#define YYNSTATES  181

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   286

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    40,     2,     2,    50,    35,    51,     2,
      44,    45,    33,    32,    43,    54,     2,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    39,    48,
      36,    49,    37,    38,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    41,     2,    42,    53,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    46,    52,    47,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     7,    10,    13,    15,    16,    20,
      22,    25,    28,    32,    33,    40,    47,    49,    53,    54,
      64,    68,    69,    74,    80,    81,    84,    87,    88,    91,
      93,    98,   110,   115,   119,   125,   127,   131,   135,   139,
     143,   147,   151,   155,   162,   166,   170,   174,   178,   182,
     186,   190,   194,   198,   202,   208,   209,   212,   214,   217,
     219,   230,   233,   236,   240,   242,   246,   251,   257,   259,
     263,   267,   269,   273,   277,   281,   285,   287,   290,   292,
     294,   296
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      56,     0,    -1,    57,    31,    -1,    -1,    64,    57,    -1,
      67,    57,    -1,     6,    -1,    -1,     6,    59,    58,    -1,
       6,    -1,     6,    60,    -1,    12,     6,    -1,    40,    12,
       6,    -1,    -1,    12,    62,    41,    76,    42,    58,    -1,
      40,    12,    41,    76,    42,    60,    -1,    61,    -1,    61,
      43,    63,    -1,    -1,    29,     6,    65,    44,    63,    45,
      46,    70,    47,    -1,     6,    25,     6,    -1,    -1,    30,
      68,    66,    48,    -1,     6,    44,    69,    45,    48,    -1,
      -1,     6,    69,    -1,     9,    69,    -1,    -1,    70,    71,
      -1,    48,    -1,     6,    49,    72,    48,    -1,    50,    74,
      13,    73,    50,    41,    72,    42,    49,    72,    48,    -1,
       7,    49,    72,    48,    -1,    30,    72,    48,    -1,    30,
      72,    39,     9,    48,    -1,    75,    -1,    75,    51,    75,
      -1,    75,    18,    75,    -1,    75,    52,    75,    -1,    75,
      19,    75,    -1,    75,    53,    75,    -1,    75,    17,    75,
      -1,    75,    16,    75,    -1,    50,    73,    50,    41,    72,
      42,    -1,    13,    73,    13,    -1,    75,    32,    75,    -1,
      75,    34,    75,    -1,    75,    35,    75,    -1,    75,    33,
      75,    -1,    75,    54,    75,    -1,    75,    37,    75,    -1,
      75,    36,    75,    -1,    75,    21,    75,    -1,    75,    22,
      75,    -1,    72,    38,    72,    39,    72,    -1,    -1,    75,
      73,    -1,     6,    -1,     6,    74,    -1,    79,    -1,     6,
      41,    12,    42,    44,    73,    45,    44,    72,    45,    -1,
      54,    75,    -1,    40,    75,    -1,    44,    72,    45,    -1,
      80,    -1,    36,    80,    37,    -1,    36,    80,    79,    37,
      -1,    36,    80,    79,    33,    37,    -1,    77,    -1,    76,
      32,    77,    -1,    76,    54,    77,    -1,    78,    -1,    44,
      77,    45,    -1,    77,    33,    77,    -1,    77,    34,    77,
      -1,    77,    35,    77,    -1,    79,    -1,    54,    79,    -1,
       4,    -1,    10,    -1,    11,    -1,     6,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   114,   114,   117,   118,   119,   122,   133,   133,   144,
     145,   151,   160,   169,   169,   173,   176,   177,   181,   180,
     195,   200,   200,   210,   220,   223,   227,   233,   234,   237,
     238,   243,   275,   280,   290,   305,   306,   313,   320,   327,
     334,   341,   348,   351,   370,   386,   394,   402,   410,   417,
     426,   429,   432,   435,   438,   455,   456,   467,   471,   476,
     480,   521,   527,   536,   539,   549,   554,   562,   568,   569,
     570,   572,   573,   574,   575,   577,   581,   582,   585,   586,
     587,   589
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
  "T_or", "T_For", "T_ge", "T_le", "T_Native", "T_NativeMethod",
  "T_Sketches", "T_new", "T_add", "T_Init", "T_def", "T_assert", "T_eof",
  "'+'", "'*'", "'/'", "'%'", "'<'", "'>'", "'?'", "':'", "'!'", "'['",
  "']'", "','", "'('", "')'", "'{'", "'}'", "';'", "'='", "'$'", "'&'",
  "'|'", "'^'", "'-'", "$accept", "Program", "MethodList", "InList", "@1",
  "OutList", "ParamDecl", "@2", "ParamList", "Method", "@3",
  "AssertionExpr", "HLAssertion", "@4", "TokenList", "WorkBody",
  "WorkStatement", "Expression", "varList", "IdentList", "Term",
  "ConstantExpr", "ConstantTerm", "NegConstant", "Constant", "Ident", 0
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
     285,   286,    43,    42,    47,    37,    60,    62,    63,    58,
      33,    91,    93,    44,    40,    41,   123,   125,    59,    61,
      36,    38,   124,    94,    45
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    55,    56,    57,    57,    57,    58,    59,    58,    60,
      60,    61,    61,    62,    61,    61,    63,    63,    65,    64,
      66,    68,    67,    67,    69,    69,    69,    70,    70,    71,
      71,    71,    71,    71,    71,    72,    72,    72,    72,    72,
      72,    72,    72,    72,    72,    72,    72,    72,    72,    72,
      72,    72,    72,    72,    72,    73,    73,    74,    74,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    76,    76,
      76,    77,    77,    77,    77,    77,    78,    78,    79,    79,
      79,    80
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     0,     2,     2,     1,     0,     3,     1,
       2,     2,     3,     0,     6,     6,     1,     3,     0,     9,
       3,     0,     4,     5,     0,     2,     2,     0,     2,     1,
       4,    11,     4,     3,     5,     1,     3,     3,     3,     3,
       3,     3,     3,     6,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     5,     0,     2,     1,     2,     1,
      10,     2,     2,     3,     1,     3,     4,     5,     1,     3,
       3,     1,     3,     3,     3,     3,     1,     2,     1,     1,
       1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,     0,    21,     0,     0,     3,     3,    24,    18,
       0,     1,     2,     4,     5,    24,    24,     0,     0,     0,
       0,    25,    26,     0,     0,     0,    22,    23,    13,     0,
      16,     0,    20,    11,     0,     0,     0,     0,     0,    12,
       0,    17,    27,    78,    79,    80,     0,     0,     0,    68,
      71,    76,     0,     0,     0,    77,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    19,    29,     0,    28,
      72,    69,     6,    14,    70,    73,    74,    75,     9,    15,
       0,     0,    81,    55,     0,     0,     0,    55,     0,     0,
      35,    59,    64,    57,     0,     0,    10,     0,     0,     0,
       0,    55,    81,     0,    62,     0,     0,    61,     0,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,    55,     8,
      30,    32,     0,    44,    56,    65,     0,    63,     0,     0,
       0,    42,    41,    37,    39,    52,    53,    45,    48,    46,
      47,    51,    50,    36,    38,    40,    49,     0,     0,     0,
      66,     0,     0,    34,     0,    55,    67,     0,    54,     0,
       0,    43,     0,     0,     0,     0,     0,     0,     0,    60,
      31
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,     5,    73,    95,    79,    30,    34,    31,     6,
      18,    20,     7,    10,    17,    53,    69,    89,   100,    94,
      90,    48,    49,    50,    91,    92
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -81
static const yytype_int16 yypact[] =
{
      -4,   -30,    23,   -81,    91,    69,    -4,    -4,   140,   -81,
     106,   -81,   -81,   -81,   -81,   140,   140,    81,    70,   108,
      83,   -81,   -81,    93,    -8,   157,   -81,   -81,   158,   154,
     122,   123,   -81,   -81,   126,    14,    -8,   124,     8,   -81,
       8,   -81,   -81,   -81,   -81,   -81,     8,    88,    24,   117,
     -81,   -81,    48,    58,   -18,   -81,     8,   163,     8,     8,
       8,     8,   165,   125,   127,    57,   -81,   -81,   166,   -81,
     -81,   117,   167,   -81,   117,   113,   142,   -81,   165,   -81,
      57,    57,   134,    73,   172,    73,    57,    73,    73,    37,
     102,   -81,   -81,   166,   168,   163,   -81,   -35,   -27,   170,
     171,    73,   -81,    20,   -81,    65,   129,   -81,    57,   174,
     -81,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,   -81,    73,   -81,
     -81,   -81,   138,   -81,   -81,   -81,    21,   -81,   144,   119,
     139,   -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,   -81,
     -81,   -81,   -81,   -81,   -81,   -81,   -81,   136,   145,   151,
     -81,    57,    57,   -81,   149,    73,   -81,    87,   -81,    57,
     147,   -81,    90,   150,   148,    57,    57,    77,    56,   -81,
     -81
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -81,   -81,   153,    96,   -81,   115,   -81,   -81,   159,   -81,
     -81,   -81,   -81,   -81,   146,   -81,   -81,   -80,   -79,   103,
     -78,   160,    84,   -81,    13,   114
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -8
static const yytype_int16 yytable[] =
{
      97,    98,     1,   108,    28,   101,   105,   104,   106,   101,
     107,   108,    43,   130,     8,    59,    60,    61,    44,    45,
      39,   131,   134,   101,    43,     2,     3,    70,   139,     9,
      44,    45,    29,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     101,    51,    46,    51,   159,    40,    56,   135,   160,    51,
      55,    43,    47,    82,    63,    64,    57,    44,    45,    51,
      83,    51,    51,    51,    51,   108,   109,    43,    58,    82,
      56,   167,   168,    44,    45,   110,   170,   101,    65,   172,
      62,    11,    43,    84,   108,   177,   178,    85,    44,    45,
      12,    86,    58,   108,   180,    66,    67,    87,    68,    84,
     137,    88,    19,    85,    24,   108,   136,    86,   111,   112,
     113,   114,   179,   115,   116,   108,    23,    88,   108,   171,
      54,    26,   174,    25,   117,   118,   119,   120,   121,   122,
      71,    27,    74,    75,    76,    77,    15,    60,    61,    16,
      59,    60,    61,   123,   124,   125,   126,   108,   162,    13,
      14,    21,    22,    32,    33,    36,    35,    38,    37,    72,
      42,    78,    93,    -7,    80,    99,    81,    61,   102,   138,
     158,   128,   132,   140,   133,   161,   164,   163,   166,   165,
     169,   129,   173,    96,   175,    41,   127,   176,   103,     0,
      52
};

static const yytype_int16 yycheck[] =
{
      80,    81,     6,    38,    12,    83,    86,    85,    87,    87,
      88,    38,     4,    48,    44,    33,    34,    35,    10,    11,
       6,    48,   101,   101,     4,    29,    30,    45,   108,     6,
      10,    11,    40,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   128,
     128,    38,    44,    40,    33,    41,    32,    37,    37,    46,
      47,     4,    54,     6,     6,     7,    42,    10,    11,    56,
      13,    58,    59,    60,    61,    38,    39,     4,    54,     6,
      32,   161,   162,    10,    11,    48,   165,   165,    30,   169,
      42,     0,     4,    36,    38,   175,   176,    40,    10,    11,
      31,    44,    54,    38,    48,    47,    48,    50,    50,    36,
      45,    54,     6,    40,    44,    38,   103,    44,    16,    17,
      18,    19,    45,    21,    22,    38,    45,    54,    38,    42,
      46,    48,    42,    25,    32,    33,    34,    35,    36,    37,
      56,    48,    58,    59,    60,    61,     6,    34,    35,     9,
      33,    34,    35,    51,    52,    53,    54,    38,    39,     6,
       7,    15,    16,     6,     6,    43,    12,    41,    45,     6,
      46,     6,     6,     6,    49,    41,    49,    35,     6,    50,
      42,    13,    12,     9,    13,    41,    50,    48,    37,    44,
      41,    95,    45,    78,    44,    36,    93,    49,    84,    -1,
      40
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,    29,    30,    56,    57,    64,    67,    44,     6,
      68,     0,    31,    57,    57,     6,     9,    69,    65,     6,
      66,    69,    69,    45,    44,    25,    48,    48,    12,    40,
      61,    63,     6,     6,    62,    12,    43,    45,    41,     6,
      41,    63,    46,     4,    10,    11,    44,    54,    76,    77,
      78,    79,    76,    70,    77,    79,    32,    42,    54,    33,
      34,    35,    42,     6,     7,    30,    47,    48,    50,    71,
      45,    77,     6,    58,    77,    77,    77,    77,     6,    60,
      49,    49,     6,    13,    36,    40,    44,    50,    54,    72,
      75,    79,    80,     6,    74,    59,    60,    72,    72,    41,
      73,    75,     6,    80,    75,    72,    73,    75,    38,    39,
      48,    16,    17,    18,    19,    21,    22,    32,    33,    34,
      35,    36,    37,    51,    52,    53,    54,    74,    13,    58,
      48,    48,    12,    13,    73,    37,    79,    45,    50,    72,
       9,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    73,    42,    33,
      37,    41,    39,    48,    50,    44,    37,    72,    72,    41,
      73,    42,    72,    45,    42,    44,    49,    72,    72,    45,
      48
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
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
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
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;

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
#line 114 "InputParser.yy"
    {  (yyval.intConst)=0; return 0;}
    break;

  case 3:
#line 117 "InputParser.yy"
    {}
    break;

  case 4:
#line 118 "InputParser.yy"
    {}
    break;

  case 5:
#line 119 "InputParser.yy"
    {}
    break;

  case 6:
#line 122 "InputParser.yy"
    {  

    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(1) - (1)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(1) - (1)].strConst)); 
	}	

}
    break;

  case 7:
#line 133 "InputParser.yy"
    {
	
    if( Gvartype == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(1) - (1)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(1) - (1)].strConst)); 
	}	
}
    break;

  case 9:
#line 144 "InputParser.yy"
    { 	 currentBD->create_outputs(-1, *(yyvsp[(1) - (1)].strConst)); }
    break;

  case 10:
#line 145 "InputParser.yy"
    {
	
	currentBD->create_outputs(-1, *(yyvsp[(1) - (2)].strConst));
}
    break;

  case 11:
#line 151 "InputParser.yy"
    {  
	if( (yyvsp[(1) - (2)].variableType) == INT){

		currentBD->create_inputs( 2 /*NINPUTS*/ , *(yyvsp[(2) - (2)].strConst)); 
	}else{

		currentBD->create_inputs(-1, *(yyvsp[(2) - (2)].strConst)); 
	}	
}
    break;

  case 12:
#line 160 "InputParser.yy"
    {
 	 if( (yyvsp[(2) - (3)].variableType) == INT){

		 currentBD->create_outputs(NINPUTS, *(yyvsp[(3) - (3)].strConst));
 	 }else{

	 	 currentBD->create_outputs(-1, *(yyvsp[(3) - (3)].strConst)); 
 	 }
 }
    break;

  case 13:
#line 169 "InputParser.yy"
    {
Gvartype = (yyvsp[(1) - (1)].variableType);

 }
    break;

  case 18:
#line 181 "InputParser.yy"
    {		modelBuilding.restart ();
		if(currentBD!= NULL){
			delete currentBD;
		}
		currentBD = envt->newFunction(*(yyvsp[(2) - (2)].strConst));
		delete (yyvsp[(2) - (2)].strConst);
}
    break;

  case 19:
#line 188 "InputParser.yy"
    { 
	currentBD->finalize();
	modelBuilding.stop();
}
    break;

  case 20:
#line 196 "InputParser.yy"
    {
	(yyval.bdag) = envt->prepareMiter(envt->getCopy(*(yyvsp[(3) - (3)].strConst)),  envt->getCopy(*(yyvsp[(1) - (3)].strConst)));
}
    break;

  case 21:
#line 200 "InputParser.yy"
    {solution.start();}
    break;

  case 22:
#line 201 "InputParser.yy"
    {
	int tt = envt->assertDAG((yyvsp[(3) - (4)].bdag), std::cout);
	envt->printControls("");
	solution.stop();
	cout<<"COMPLETED"<<endl;
	if(tt != 0){
		return tt;
	}
}
    break;

  case 23:
#line 211 "InputParser.yy"
    {
	int tt = envt->runCommand(*(yyvsp[(1) - (5)].strConst), *(yyvsp[(3) - (5)].sList));
	delete (yyvsp[(1) - (5)].strConst);
	delete (yyvsp[(3) - (5)].sList);
	if(tt >= 0){
		return tt;
	}
}
    break;

  case 24:
#line 220 "InputParser.yy"
    {
	(yyval.sList) = new list<string*>();	
}
    break;

  case 25:
#line 223 "InputParser.yy"
    {
	(yyval.sList) = (yyvsp[(2) - (2)].sList);
	(yyval.sList)->push_back( (yyvsp[(1) - (2)].strConst));
}
    break;

  case 26:
#line 227 "InputParser.yy"
    {
	(yyval.sList) = (yyvsp[(2) - (2)].sList);
	(yyval.sList)->push_back( (yyvsp[(1) - (2)].strConst));
}
    break;

  case 27:
#line 233 "InputParser.yy"
    { /* Empty */ }
    break;

  case 28:
#line 234 "InputParser.yy"
    { /* */ }
    break;

  case 29:
#line 237 "InputParser.yy"
    {  (yyval.intConst)=0;  /* */ }
    break;

  case 30:
#line 238 "InputParser.yy"
    {
	currentBD->alias( *(yyvsp[(1) - (4)].strConst), *(yyvsp[(3) - (4)].strConst));
	delete (yyvsp[(3) - (4)].strConst);
	delete (yyvsp[(1) - (4)].strConst);
}
    break;

  case 31:
#line 243 "InputParser.yy"
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
		string s1( currentBD->new_name() );
		ARRASS_node* an = dynamic_cast<ARRASS_node*>(newArithNode(arith_node::ARRASS));
		an->multi_mother.reserve(2);
		an->multi_mother.push_back(*oldit);			
		an->multi_mother.push_back(rhs);
		an->name = s1;
		Assert( rhs != NULL, "AAARRRGH This shouldn't happen !!");
		Assert((yyvsp[(7) - (11)].strConst) != NULL, "1: THIS CAN'T HAPPEN!!");
		an->quant = i;
		currentBD->new_node(*(yyvsp[(7) - (11)].strConst),  "",  an);
		currentBD->alias( *(*it), s1);
		delete *it;
	}
	delete childs;
	delete oldchilds;
	delete (yyvsp[(7) - (11)].strConst);
	delete (yyvsp[(10) - (11)].strConst);
}
    break;

  case 32:
#line 275 "InputParser.yy"
    {
	currentBD->create_outputs(NINPUTS, currentBD->get_node(*(yyvsp[(3) - (4)].strConst)), *(yyvsp[(1) - (4)].strConst));
	delete (yyvsp[(3) - (4)].strConst);
	delete (yyvsp[(1) - (4)].strConst);
}
    break;

  case 33:
#line 280 "InputParser.yy"
    {
  if ((yyvsp[(2) - (3)].strConst)) {
    /* Asserting an expression, construct assert node. */

    string s = currentBD->new_name ();
    currentBD->new_node (*(yyvsp[(2) - (3)].strConst), "", bool_node::ASSERT, s);

    delete (yyvsp[(2) - (3)].strConst);
  }
}
    break;

  case 34:
#line 290 "InputParser.yy"
    {
  if ((yyvsp[(2) - (5)].strConst)) {
    /* Asserting an expression, construct assert node. */

    ASSERT_node* bn = dynamic_cast<ASSERT_node*>(newBoolNode(bool_node::ASSERT));
    bn->setMsg(*(yyvsp[(4) - (5)].strConst));
    currentBD->new_node (*(yyvsp[(2) - (5)].strConst), "", bn);
    

    delete (yyvsp[(2) - (5)].strConst);
    delete (yyvsp[(4) - (5)].strConst);
  }
}
    break;

  case 35:
#line 305 "InputParser.yy"
    { (yyval.strConst) = (yyvsp[(1) - (1)].strConst); }
    break;

  case 36:
#line 306 "InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::AND, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);	  					  
}
    break;

  case 37:
#line 313 "InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::AND, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);	  					  
}
    break;

  case 38:
#line 320 "InputParser.yy"
    {
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::OR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);		  					  
}
    break;

  case 39:
#line 327 "InputParser.yy"
    { 	
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::OR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
}
    break;

  case 40:
#line 334 "InputParser.yy"
    {	
	string s = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::XOR, s);
	(yyval.strConst) = new string(s);
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
}
    break;

  case 41:
#line 341 "InputParser.yy"
    {	
	string* tmp = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::EQ);
    string s = currentBD->new_name ();
    currentBD->new_node (*tmp, "", bool_node::NOT, s);
    delete tmp;
    (yyval.strConst) = new string(s);
}
    break;

  case 42:
#line 348 "InputParser.yy"
    { 			
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::EQ);
}
    break;

  case 43:
#line 351 "InputParser.yy"
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

  case 44:
#line 370 "InputParser.yy"
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

  case 45:
#line 386 "InputParser.yy"
    {
	string s1 = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::PLUS, s1); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1); 
}
    break;

  case 46:
#line 394 "InputParser.yy"
    {
	string s1 = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::DIV, s1); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1); 
}
    break;

  case 47:
#line 402 "InputParser.yy"
    {
	string s1 = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::MOD, s1); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1); 
}
    break;

  case 48:
#line 410 "InputParser.yy"
    {
	string s1 = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst),  *(yyvsp[(3) - (3)].strConst), bool_node::TIMES, s1); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1); 
}
    break;

  case 49:
#line 417 "InputParser.yy"
    {
	string neg1 = currentBD->new_name();
	string s1 = currentBD->new_name();
	currentBD->new_node(*(yyvsp[(3) - (3)].strConst), "", bool_node::NEG, neg1);
	currentBD->new_node(*(yyvsp[(1) - (3)].strConst), neg1, bool_node::PLUS, s1); 
	delete (yyvsp[(1) - (3)].strConst);
	delete (yyvsp[(3) - (3)].strConst);
	(yyval.strConst) = new string(s1);
}
    break;

  case 50:
#line 426 "InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::GT);
}
    break;

  case 51:
#line 429 "InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::LT);
}
    break;

  case 52:
#line 432 "InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::GE);
}
    break;

  case 53:
#line 435 "InputParser.yy"
    {
	(yyval.strConst) = comparisson((yyvsp[(1) - (3)].strConst), (yyvsp[(3) - (3)].strConst), bool_node::LE);
}
    break;

  case 54:
#line 438 "InputParser.yy"
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
	delete (yyvsp[(1) - (5)].strConst); 
	delete (yyvsp[(3) - (5)].strConst); 
	delete (yyvsp[(5) - (5)].strConst); 		  					  
}
    break;

  case 55:
#line 455 "InputParser.yy"
    { /* Empty */  	(yyval.nList) = new list<bool_node*>();	}
    break;

  case 56:
#line 456 "InputParser.yy"
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

  case 57:
#line 467 "InputParser.yy"
    {
	(yyval.sList) = new list<string*>();	
	(yyval.sList)->push_back( (yyvsp[(1) - (1)].strConst));
}
    break;

  case 58:
#line 471 "InputParser.yy"
    {
	(yyval.sList) = (yyvsp[(2) - (2)].sList);
	(yyval.sList)->push_back( (yyvsp[(1) - (2)].strConst));
}
    break;

  case 59:
#line 476 "InputParser.yy"
    {
	(yyval.strConst) = new string(currentBD->create_const((yyvsp[(1) - (1)].intConst)));
}
    break;

  case 60:
#line 480 "InputParser.yy"
    {
	
	list<bool_node*>* params = (yyvsp[(6) - (10)].nList);
	if(false && params->size() == 0){
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
			s1 = currentBD->new_name(fname);
		}
		ufun->name = s1;
		currentBD->new_node(*(yyvsp[(9) - (10)].strConst), "", ufun);
		
		(yyval.strConst) = new string(s1);
		delete (yyvsp[(1) - (10)].strConst);
	}
	delete (yyvsp[(6) - (10)].nList);
	delete (yyvsp[(9) - (10)].strConst);
}
    break;

  case 61:
#line 521 "InputParser.yy"
    {
	string neg1 = currentBD->new_name();	
	currentBD->new_node(*(yyvsp[(2) - (2)].strConst), "", bool_node::NEG, neg1);	
	delete (yyvsp[(2) - (2)].strConst);
	(yyval.strConst) = new string(neg1);
}
    break;

  case 62:
#line 527 "InputParser.yy"
    { 
    /* Check the Boolean coefficient of the term, being either 0 (false) or 1 (true). */
    /* Generate an alternating NOT node, push a unit (true) coefficient. */
    string s = currentBD->new_name ();
    currentBD->new_node (*(yyvsp[(2) - (2)].strConst), "", bool_node::NOT, s);
    (yyval.strConst) = new string (s);
    delete (yyvsp[(2) - (2)].strConst);
}
    break;

  case 63:
#line 536 "InputParser.yy"
    { 
						(yyval.strConst) = (yyvsp[(2) - (3)].strConst); 
						}
    break;

  case 64:
#line 539 "InputParser.yy"
    { 
			if( !currentBD->has_alias(*(yyvsp[(1) - (1)].strConst)) ){ 
				(yyval.strConst) = (yyvsp[(1) - (1)].strConst);
			}else{ 
				string alias(currentBD->get_alias(*(yyvsp[(1) - (1)].strConst))); 
				Assert( alias != "", "You need to have an alias for "<<*(yyvsp[(1) - (1)].strConst));				
				(yyval.strConst) = new string( alias ); 				  
				delete (yyvsp[(1) - (1)].strConst);
			} 
		}
    break;

  case 65:
#line 549 "InputParser.yy"
    {
	currentBD->create_controls(-1, *(yyvsp[(2) - (3)].strConst));
	Assert( !currentBD->has_alias(*(yyvsp[(2) - (3)].strConst)), "THIS SHOULD NEVER HAPPEN !!!!!!!!!!!!!!!!");	
	(yyval.strConst) = (yyvsp[(2) - (3)].strConst);	
}
    break;

  case 66:
#line 554 "InputParser.yy"
    {
	int nctrls = (yyvsp[(3) - (4)].intConst);
	if(overrideNCtrls){
		nctrls = NCTRLS;
	}
	currentBD->create_controls(nctrls, *(yyvsp[(2) - (4)].strConst));
	(yyval.strConst) = (yyvsp[(2) - (4)].strConst);
}
    break;

  case 67:
#line 562 "InputParser.yy"
    {
	currentBD->create_controls((yyvsp[(3) - (5)].intConst), *(yyvsp[(2) - (5)].strConst));
	(yyval.strConst) = (yyvsp[(2) - (5)].strConst);

}
    break;

  case 68:
#line 568 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 69:
#line 569 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) + (yyvsp[(3) - (3)].intConst); }
    break;

  case 70:
#line 570 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) - (yyvsp[(3) - (3)].intConst); }
    break;

  case 71:
#line 572 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 72:
#line 573 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(2) - (3)].intConst); }
    break;

  case 73:
#line 574 "InputParser.yy"
    { (yyval.intConst) = (yyvsp[(1) - (3)].intConst) * (yyvsp[(3) - (3)].intConst); }
    break;

  case 74:
#line 575 "InputParser.yy"
    { Assert( (yyvsp[(3) - (3)].intConst) != 0, "You are attempting to divide by zero !!");
							      (yyval.intConst) = (yyvsp[(1) - (3)].intConst) / (yyvsp[(3) - (3)].intConst); }
    break;

  case 75:
#line 577 "InputParser.yy"
    { Assert( (yyvsp[(3) - (3)].intConst) != 0, "You are attempting to mod by zero !!");
							      (yyval.intConst) = (yyvsp[(1) - (3)].intConst) % (yyvsp[(3) - (3)].intConst); }
    break;

  case 76:
#line 581 "InputParser.yy"
    {  (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 77:
#line 582 "InputParser.yy"
    {  (yyval.intConst) = -(yyvsp[(2) - (2)].intConst); }
    break;

  case 78:
#line 585 "InputParser.yy"
    {  (yyval.intConst) = (yyvsp[(1) - (1)].intConst); }
    break;

  case 79:
#line 586 "InputParser.yy"
    { (yyval.intConst) = 1; }
    break;

  case 80:
#line 587 "InputParser.yy"
    { (yyval.intConst) = 0; }
    break;

  case 81:
#line 589 "InputParser.yy"
    { (yyval.strConst)=(yyvsp[(1) - (1)].strConst); }
    break;


/* Line 1267 of yacc.c.  */
#line 2301 "InputParser.cpp"
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


#line 591 "InputParser.yy"



void Inityyparse(){

	 	
}

void yyerror(char* c){
	Assert(false, c); 
}


int isatty(int i){



return 1;
}

