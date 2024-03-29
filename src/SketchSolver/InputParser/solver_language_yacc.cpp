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
     identifier_y = 258,
     solver_token = 259,
     while_token = 260,
     for_token = 261,
     if_token = 262,
     else_token = 263,
     return_token = 264,
     lambda_token = 265,
     op_eq = 266,
     op_geq = 267,
     op_plus_plus = 268,
     var_val_rule = 269
   };
#endif
/* Tokens.  */
#define identifier_y 258
#define solver_token 259
#define while_token 260
#define for_token 261
#define if_token 262
#define else_token 263
#define return_token 264
#define lambda_token 265
#define op_eq 266
#define op_geq 267
#define op_plus_plus 268
#define var_val_rule 269




/* Copy the first part of user declarations.  */
#line 1 "solver_language_parser.y"


#include "SolverLanguageYaccHeader.h"

int yylex_init (yyscan_t* scanner);

#include "solver_language_yacc.h"


#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

extern void yyset_in  ( FILE * _in_str , yyscan_t yyscanner );



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
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
#line 22 "solver_language_parser.y"
{
	SL::Var* var;
	SL::VarVal* var_val;
	SL::FunctionCall* func_call;
	SL::Identifier* identifier_;
	SL::Params* params;
	SL::Param* param;
	SL::CodeBlock* code_block;
	SL::UnitLine* unit_line;
	SL::Assignment* assignment;
	SL::Method* method;
	SL::Methods* methods;
	SL::BinaryExpression* bool_expr;
	SL::BinaryOp my_operator;
	SL::SLType* my_type;
	SL::TypeParams* type_params;
	SL::Expression* expression;
	SL::LambdaExpression* lambda_expr;}
/* Line 193 of yacc.c.  */
#line 159 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 172 "y.tab.c"

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
# if defined YYENABLE_NLS && YYENABLE_NLS
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
#define YYFINAL  10
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   256

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  32
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  24
/* YYNRULES -- Number of rules.  */
#define YYNRULES  66
/* YYNRULES -- Number of states.  */
#define YYNSTATES  152

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   269

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      22,    23,    20,    18,    29,    19,    28,    21,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    31,    15,
      16,    30,    17,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    24,     2,    25,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    26,     2,    27,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     6,     8,    12,    15,    17,    19,    21,
      23,    25,    27,    29,    31,    33,    35,    37,    39,    41,
      45,    47,    58,    62,    64,    67,    69,    70,    75,    83,
      92,   104,   108,   115,   120,   125,   129,   133,   138,   140,
     144,   146,   151,   154,   159,   164,   168,   172,   175,   177,
     179,   180,   182,   186,   192,   193,   195,   199,   202,   204,
     205,   207,   211,   220,   228,   230,   233
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      55,     0,    -1,    39,    15,    -1,    41,    -1,    39,    15,
      33,    -1,    41,    33,    -1,    16,    -1,    17,    -1,    11,
      -1,    12,    -1,    18,    -1,    19,    -1,    20,    -1,    21,
      -1,     3,    -1,    42,    -1,    38,    -1,    14,    -1,    37,
      -1,    22,    35,    23,    -1,    43,    -1,    10,    24,    52,
      25,    22,    52,    23,    26,    33,    27,    -1,    35,    34,
      35,    -1,    46,    -1,     9,    35,    -1,    35,    -1,    -1,
       8,    26,    33,    27,    -1,     5,    22,    35,    23,    26,
      33,    27,    -1,     7,    22,    35,    23,    26,    33,    27,
      40,    -1,     6,    22,    39,    15,    35,    15,    39,    23,
      26,    33,    27,    -1,    26,    33,    27,    -1,    35,    28,
       3,    22,    48,    23,    -1,     3,    22,    48,    23,    -1,
      35,    24,    48,    25,    -1,    24,    48,    25,    -1,    26,
      50,    27,    -1,    45,    22,    48,    23,    -1,    45,    -1,
      45,    29,    44,    -1,     3,    -1,     3,    16,    44,    17,
      -1,    45,     3,    -1,    45,     3,    30,    35,    -1,    45,
       3,    30,    36,    -1,     3,    30,    36,    -1,     3,    30,
      35,    -1,     3,    13,    -1,    35,    -1,    36,    -1,    -1,
      47,    -1,    47,    29,    48,    -1,    22,     3,    31,     3,
      23,    -1,    -1,    49,    -1,    49,    29,    50,    -1,    45,
       3,    -1,     3,    -1,    -1,    51,    -1,    51,    29,    52,
      -1,    45,     3,    22,    52,    23,    26,    33,    27,    -1,
       3,    22,    52,    23,    26,    33,    27,    -1,    53,    -1,
      53,    54,    -1,    54,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    81,    81,    82,    83,    84,    87,    88,    89,    90,
      91,    92,    93,    94,   105,   106,   107,   108,   110,   111,
     114,   117,   120,   122,   123,   124,   127,   128,   131,   132,
     133,   135,   137,   138,   139,   140,   145,   154,   156,   156,
     158,   159,   162,   163,   164,   165,   166,   167,   176,   176,
     178,   178,   179,   181,   198,   199,   200,   203,   204,   206,
     206,   207,   209,   211,   214,   214,   216
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "identifier_y", "solver_token",
  "while_token", "for_token", "if_token", "else_token", "return_token",
  "lambda_token", "op_eq", "op_geq", "op_plus_plus", "var_val_rule", "';'",
  "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'('", "')'", "'['", "']'",
  "'{'", "'}'", "'.'", "','", "'='", "':'", "$accept", "code_block",
  "binary_op", "expression", "constuctor_call_expression",
  "lambda_expression", "binary_expression", "unit_line", "maybe_else",
  "macro_unit", "function_call", "constructor_call", "type_params",
  "type_rule", "assignment", "param", "params", "key_col_val",
  "key_col_vals", "parameter_declaration", "signature_params", "method",
  "methods", "root", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,    59,    60,    62,    43,    45,
      42,    47,    40,    41,    91,    93,   123,   125,    46,    44,
      61,    58
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    32,    33,    33,    33,    33,    34,    34,    34,    34,
      34,    34,    34,    34,    35,    35,    35,    35,    35,    35,
      36,    37,    38,    39,    39,    39,    40,    40,    41,    41,
      41,    41,    42,    42,    42,    42,    42,    43,    44,    44,
      45,    45,    46,    46,    46,    46,    46,    46,    47,    47,
      48,    48,    48,    49,    50,    50,    50,    51,    51,    52,
      52,    52,    53,    53,    54,    54,    55
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     3,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,    10,     3,     1,     2,     1,     0,     4,     7,     8,
      11,     3,     6,     4,     4,     3,     3,     4,     1,     3,
       1,     4,     2,     4,     4,     3,     3,     2,     1,     1,
       0,     1,     3,     5,     0,     1,     3,     2,     1,     0,
       1,     3,     8,     7,     1,     2,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    40,     0,    64,    66,     0,     0,    59,     0,    65,
       1,    40,     0,    38,    58,     0,    60,     0,    59,    41,
       0,    57,    59,     0,     0,    39,    61,     0,     0,    14,
       0,     0,     0,     0,     0,    17,     0,    50,    54,     0,
      25,    18,    16,     0,     3,    15,     0,    23,     0,    47,
      50,     0,     0,     0,     0,    14,    54,    24,    59,     0,
      14,    48,    49,    20,     0,    51,     0,     0,     0,    55,
       0,    63,     8,     9,     6,     7,    10,    11,    12,    13,
      50,     0,     0,     2,     5,    42,     0,     0,    46,    45,
       0,     0,     0,     0,     0,    19,    50,    50,    35,    14,
      31,    54,    36,     0,     0,    22,     4,     0,    62,    33,
       0,     0,     0,     0,     0,     0,    52,     0,    56,    34,
      50,    43,    44,     0,     0,     0,    59,    37,     0,     0,
       0,     0,     0,     0,    53,    32,    28,     0,    26,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,    21,
      30,    27
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    39,    82,    40,    62,    41,    42,    43,   142,    44,
      45,    63,    12,    46,    47,    65,    66,    69,    70,    16,
      17,     3,     4,     5
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -50
static const yytype_int16 yypact[] =
{
       6,    -3,     7,     6,   -50,    11,     9,    27,    -8,   -50,
     -50,    15,    16,     5,    12,    41,    18,    33,    27,   -50,
       9,   -50,    27,    35,    36,   -50,   -50,   107,    37,    13,
      44,    49,    51,   134,    50,   -50,   134,   147,   129,    48,
     228,   -50,   -50,    61,   107,   -50,    74,   -50,   107,   -50,
     147,   147,   134,    43,   134,    56,    57,   228,    27,   172,
      38,   228,   -50,   -50,    59,    54,    62,   156,    63,    60,
      64,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     147,    83,   134,   107,   -50,    58,    65,    70,   228,   -50,
     186,    79,   200,    95,    75,   -50,   147,   147,   -50,    14,
     -50,    57,   -50,    76,    81,   228,   -50,   147,   -50,   -50,
      80,   134,    85,    84,    96,    82,   -50,   116,   -50,   -50,
     147,   228,   -50,   107,   214,   107,    27,   -50,   102,   104,
     101,    43,   103,   117,   -50,   -50,   -50,   118,   137,   120,
     121,   123,   -50,   107,   107,   107,   115,   125,   127,   -50,
     -50,   -50
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -50,   -21,   -50,   -12,   -43,   -50,   -50,   -49,   -50,   -50,
     -50,   -50,   142,     0,   -50,   -50,   -48,   -50,    66,   -50,
     -17,   -50,   160,   -50
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -41
static const yytype_int16 yytable[] =
{
       2,    24,    87,     2,    91,    26,    13,    15,    89,     1,
       8,    10,    11,     6,    18,   -40,   -40,    68,    15,     7,
      13,    57,    15,    84,    59,    61,    49,    86,     6,     6,
      14,     6,   103,    19,    20,    50,    50,    64,    61,    88,
      90,    94,    92,    51,    21,   117,    29,    22,   115,   116,
      64,    64,    33,    34,     6,    59,    23,    35,    15,    28,
      50,    27,   106,    48,   122,    36,    52,    37,    61,    56,
     105,    53,   129,    54,    58,    71,    83,    85,    50,    93,
      64,    96,   137,    97,    61,    61,   104,    98,   107,   101,
     100,   102,   108,   109,   111,   121,    64,    64,   113,   124,
     114,   119,   130,   120,   132,   127,   123,    64,    61,   133,
      29,   125,    30,    31,    32,   117,    33,    34,   126,   128,
      64,    35,   146,   147,   148,   134,    15,   135,   136,    36,
     138,    37,    29,    38,    30,    31,    32,    55,    33,    34,
     139,   140,   149,    35,    34,   141,   143,   144,    35,   145,
      60,    67,   150,    37,   151,    38,    36,    34,    37,    99,
      56,    35,    25,     9,     0,     0,    34,   118,     0,    36,
      35,    37,     0,    56,     0,     0,     0,     0,    36,     0,
      37,     0,    56,    72,    73,     0,     0,     0,    74,    75,
      76,    77,    78,    79,     0,    95,    80,    72,    73,     0,
      81,     0,    74,    75,    76,    77,    78,    79,     0,   110,
      80,    72,    73,     0,    81,     0,    74,    75,    76,    77,
      78,    79,     0,   112,    80,    72,    73,     0,    81,   131,
      74,    75,    76,    77,    78,    79,     0,     0,    80,    72,
      73,     0,    81,     0,    74,    75,    76,    77,    78,    79,
       0,     0,    80,     0,     0,     0,    81
};

static const yytype_int16 yycheck[] =
{
       0,    18,    50,     3,    53,    22,     6,     7,    51,     3,
       3,     0,     3,    16,    22,     3,     3,    38,    18,    22,
      20,    33,    22,    44,    36,    37,    13,    48,    16,    16,
       3,    16,    80,    17,    29,    22,    22,    37,    50,    51,
      52,    58,    54,    30,     3,    31,     3,    29,    96,    97,
      50,    51,     9,    10,    16,    67,    23,    14,    58,    23,
      22,    26,    83,    26,   107,    22,    22,    24,    80,    26,
      82,    22,   120,    22,    24,    27,    15,     3,    22,    22,
      80,    22,   131,    29,    96,    97,     3,    25,    30,    29,
      27,    27,    27,    23,    15,   107,    96,    97,     3,   111,
      25,    25,   123,    22,   125,    23,    26,   107,   120,   126,
       3,    26,     5,     6,     7,    31,     9,    10,    22,     3,
     120,    14,   143,   144,   145,    23,   126,    23,    27,    22,
      27,    24,     3,    26,     5,     6,     7,     3,     9,    10,
      23,    23,    27,    14,    10,     8,    26,    26,    14,    26,
       3,    22,    27,    24,    27,    26,    22,    10,    24,     3,
      26,    14,    20,     3,    -1,    -1,    10,   101,    -1,    22,
      14,    24,    -1,    26,    -1,    -1,    -1,    -1,    22,    -1,
      24,    -1,    26,    11,    12,    -1,    -1,    -1,    16,    17,
      18,    19,    20,    21,    -1,    23,    24,    11,    12,    -1,
      28,    -1,    16,    17,    18,    19,    20,    21,    -1,    23,
      24,    11,    12,    -1,    28,    -1,    16,    17,    18,    19,
      20,    21,    -1,    23,    24,    11,    12,    -1,    28,    15,
      16,    17,    18,    19,    20,    21,    -1,    -1,    24,    11,
      12,    -1,    28,    -1,    16,    17,    18,    19,    20,    21,
      -1,    -1,    24,    -1,    -1,    -1,    28
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    45,    53,    54,    55,    16,    22,     3,    54,
       0,     3,    44,    45,     3,    45,    51,    52,    22,    17,
      29,     3,    29,    23,    52,    44,    52,    26,    23,     3,
       5,     6,     7,     9,    10,    14,    22,    24,    26,    33,
      35,    37,    38,    39,    41,    42,    45,    46,    26,    13,
      22,    30,    22,    22,    22,     3,    26,    35,    24,    35,
       3,    35,    36,    43,    45,    47,    48,    22,    33,    49,
      50,    27,    11,    12,    16,    17,    18,    19,    20,    21,
      24,    28,    34,    15,    33,     3,    33,    48,    35,    36,
      35,    39,    35,    22,    52,    23,    22,    29,    25,     3,
      27,    29,    27,    48,     3,    35,    33,    30,    27,    23,
      23,    15,    23,     3,    25,    48,    48,    31,    50,    25,
      22,    35,    36,    26,    35,    26,    22,    23,     3,    48,
      33,    15,    33,    52,    23,    23,    27,    39,    27,    23,
      23,     8,    40,    26,    26,    26,    33,    33,    33,    27,
      27,    27
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
      yyerror (state, YY_("syntax error: cannot back up")); \
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
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
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
# define YYLEX yylex (&yylval, yyscanner)
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
		  Type, Value, state); \
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
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, yyscan_t yyscanner, HyperSketchState* state)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, state)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    yyscan_t yyscanner, HyperSketchState* state;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (state);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, yyscan_t yyscanner, HyperSketchState* state)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, state)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    yyscan_t yyscanner, HyperSketchState* state;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, state);
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
yy_reduce_print (YYSTYPE *yyvsp, int yyrule, yyscan_t yyscanner, HyperSketchState* state)
#else
static void
yy_reduce_print (yyvsp, yyrule, state)
    YYSTYPE *yyvsp;
    int yyrule;
    yyscan_t yyscanner, HyperSketchState* state;
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
		       		       , state);
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule, state); \
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, HyperSketchState* state)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, state)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    yyscan_t yyscanner, HyperSketchState* state;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (state);

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
int yyparse (yyscan_t yyscanner, HyperSketchState* state);
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
yyparse (yyscan_t yyscanner, HyperSketchState* state)
#else
int
yyparse (state)
    yyscan_t yyscanner, HyperSketchState* state;
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
#line 81 "solver_language_parser.y"
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[(1) - (2)].unit_line));}
    break;

  case 3:
#line 82 "solver_language_parser.y"
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[(1) - (1)].unit_line));}
    break;

  case 4:
#line 83 "solver_language_parser.y"
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[(1) - (3)].unit_line), (yyvsp[(3) - (3)].code_block));}
    break;

  case 5:
#line 84 "solver_language_parser.y"
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[(1) - (2)].unit_line), (yyvsp[(2) - (2)].code_block));}
    break;

  case 6:
#line 87 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_lt;}
    break;

  case 7:
#line 88 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_gt;}
    break;

  case 8:
#line 89 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_eq;}
    break;

  case 9:
#line 90 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_geq;}
    break;

  case 10:
#line 91 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_plus;}
    break;

  case 11:
#line 92 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_minus;}
    break;

  case 12:
#line 93 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_mult;}
    break;

  case 13:
#line 94 "solver_language_parser.y"
    {(yyval.my_operator) = SL::BinaryOp::_div;}
    break;

  case 14:
#line 105 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].identifier_));}
    break;

  case 15:
#line 106 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].func_call));}
    break;

  case 16:
#line 107 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].bool_expr));}
    break;

  case 17:
#line 108 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].var_val));}
    break;

  case 18:
#line 110 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].lambda_expr));}
    break;

  case 19:
#line 111 "solver_language_parser.y"
    {(yyval.expression) = (yyvsp[(2) - (3)].expression);}
    break;

  case 20:
#line 114 "solver_language_parser.y"
    {(yyval.expression) = new SL::Expression((yyvsp[(1) - (1)].func_call));}
    break;

  case 21:
#line 117 "solver_language_parser.y"
    {(yyval.lambda_expr) = new SL::LambdaExpression((yyvsp[(3) - (10)].params), (yyvsp[(6) - (10)].params), (yyvsp[(9) - (10)].code_block));}
    break;

  case 22:
#line 120 "solver_language_parser.y"
    {(yyval.bool_expr) = new SL::BinaryExpression((yyvsp[(2) - (3)].my_operator), (yyvsp[(1) - (3)].expression), (yyvsp[(3) - (3)].expression));}
    break;

  case 23:
#line 122 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[(1) - (1)].assignment));}
    break;

  case 24:
#line 123 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine(new SL::Return((yyvsp[(2) - (2)].expression)));}
    break;

  case 25:
#line 124 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[(1) - (1)].expression));}
    break;

  case 26:
#line 127 "solver_language_parser.y"
    {(yyval.code_block) = nullptr;}
    break;

  case 27:
#line 128 "solver_language_parser.y"
    {(yyval.code_block) = (yyvsp[(3) - (4)].code_block);}
    break;

  case 28:
#line 131 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine(new SL::While((yyvsp[(3) - (7)].expression), (yyvsp[(6) - (7)].code_block)));}
    break;

  case 29:
#line 132 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine(new SL::If((yyvsp[(3) - (8)].expression), (yyvsp[(6) - (8)].code_block), (yyvsp[(8) - (8)].code_block)));}
    break;

  case 30:
#line 134 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine(new SL::For((yyvsp[(3) - (11)].unit_line), (yyvsp[(5) - (11)].expression), (yyvsp[(7) - (11)].unit_line), (yyvsp[(10) - (11)].code_block)));}
    break;

  case 31:
#line 135 "solver_language_parser.y"
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[(2) - (3)].code_block));}
    break;

  case 32:
#line 137 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall((yyvsp[(1) - (6)].expression), (yyvsp[(3) - (6)].identifier_), (yyvsp[(5) - (6)].params));}
    break;

  case 33:
#line 138 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall((yyvsp[(1) - (4)].identifier_), (yyvsp[(3) - (4)].params));}
    break;

  case 34:
#line 139 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall((yyvsp[(1) - (4)].expression), new SL::Identifier("get"), (yyvsp[(3) - (4)].params));}
    break;

  case 35:
#line 140 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall(
               				new SL::SLType(new SL::Identifier("vector"),
               				new SL::TypeParams(
               					new SL::SLType(new SL::Identifier("any"))
               				)), (yyvsp[(2) - (3)].params));}
    break;

  case 36:
#line 145 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall(
               				new SL::SLType(new SL::Identifier("map"),
               				new SL::TypeParams(
               					new SL::SLType(new SL::Identifier("string")),
               					new SL::TypeParams(new SL::SLType(new SL::Identifier("any")))
               				)), (yyvsp[(2) - (3)].params));}
    break;

  case 37:
#line 154 "solver_language_parser.y"
    {(yyval.func_call) = new SL::FunctionCall((yyvsp[(1) - (4)].my_type), (yyvsp[(3) - (4)].params));}
    break;

  case 38:
#line 156 "solver_language_parser.y"
    {(yyval.type_params) = new SL::TypeParams((yyvsp[(1) - (1)].my_type));}
    break;

  case 39:
#line 156 "solver_language_parser.y"
    {(yyval.type_params) = new SL::TypeParams((yyvsp[(1) - (3)].my_type), (yyvsp[(3) - (3)].type_params));}
    break;

  case 40:
#line 158 "solver_language_parser.y"
    {(yyval.my_type) = new SL::SLType((yyvsp[(1) - (1)].identifier_));}
    break;

  case 41:
#line 159 "solver_language_parser.y"
    {(yyval.my_type) = new SL::SLType((yyvsp[(1) - (4)].identifier_), (yyvsp[(3) - (4)].type_params));}
    break;

  case 42:
#line 162 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[(1) - (2)].my_type), (yyvsp[(2) - (2)].identifier_)));}
    break;

  case 43:
#line 163 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[(1) - (4)].my_type), (yyvsp[(2) - (4)].identifier_)), (yyvsp[(4) - (4)].expression));}
    break;

  case 44:
#line 164 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[(1) - (4)].my_type), (yyvsp[(2) - (4)].identifier_)), (yyvsp[(4) - (4)].expression));}
    break;

  case 45:
#line 165 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment((yyvsp[(1) - (3)].identifier_), (yyvsp[(3) - (3)].expression));}
    break;

  case 46:
#line 166 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment((yyvsp[(1) - (3)].identifier_), (yyvsp[(3) - (3)].expression));}
    break;

  case 47:
#line 168 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(
		(yyvsp[(1) - (2)].identifier_),
		new SL::Expression(
			new SL::BinaryExpression(
				SL::BinaryOp::_plus,
				new SL::Expression(new SL::Identifier((yyvsp[(1) - (2)].identifier_))),
				new SL::Expression(new SL::VarVal((int)1)))));}
    break;

  case 48:
#line 176 "solver_language_parser.y"
    {(yyval.param) = new SL::Param((yyvsp[(1) - (1)].expression));}
    break;

  case 49:
#line 176 "solver_language_parser.y"
    {(yyval.param) = new SL::Param((yyvsp[(1) - (1)].expression));}
    break;

  case 50:
#line 178 "solver_language_parser.y"
    {(yyval.params) = new SL::Params();}
    break;

  case 51:
#line 178 "solver_language_parser.y"
    {(yyval.params) = new SL::Params((yyvsp[(1) - (1)].param));}
    break;

  case 52:
#line 179 "solver_language_parser.y"
    {(yyval.params) = new SL::Params((yyvsp[(1) - (3)].param), (yyvsp[(3) - (3)].params));}
    break;

  case 53:
#line 181 "solver_language_parser.y"
    {
             			SL::Params* params =
             				new SL::Params(
             					new SL::Param(new SL::Expression((yyvsp[(2) - (5)].identifier_))),
             					new SL::Params(new SL::Param(new SL::Expression((yyvsp[(4) - (5)].identifier_)))));
             			SL::Expression* almost_ret = new SL::Expression(
             				new SL::FunctionCall(
             					new SL::SLType(new SL::Identifier("pair"),
             					new SL::TypeParams(
             						new SL::SLType(new SL::Identifier("string")),
             						new SL::TypeParams(new SL::SLType(new SL::Identifier("any")))
             					)
             				), params)
				);
				(yyval.param) = new SL::Param(almost_ret);}
    break;

  case 54:
#line 198 "solver_language_parser.y"
    {(yyval.params) = new SL::Params();}
    break;

  case 55:
#line 199 "solver_language_parser.y"
    {(yyval.params) = new SL::Params((yyvsp[(1) - (1)].param));}
    break;

  case 56:
#line 200 "solver_language_parser.y"
    {(yyval.params) = new SL::Params((yyvsp[(1) - (3)].param), (yyvsp[(3) - (3)].params));}
    break;

  case 57:
#line 203 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[(1) - (2)].my_type), (yyvsp[(2) - (2)].identifier_)));}
    break;

  case 58:
#line 204 "solver_language_parser.y"
    {(yyval.assignment) = new SL::Assignment(new SL::Var(new SL::SLType(new SL::Identifier("any")), (yyvsp[(1) - (1)].identifier_)), nullptr);}
    break;

  case 59:
#line 206 "solver_language_parser.y"
    {(yyval.params) = new SL::Params();}
    break;

  case 60:
#line 206 "solver_language_parser.y"
    {(yyval.params) = new SL::Params(new SL::Param((yyvsp[(1) - (1)].assignment)));}
    break;

  case 61:
#line 207 "solver_language_parser.y"
    {(yyval.params) = new SL::Params(new SL::Param((yyvsp[(1) - (3)].assignment)), (yyvsp[(3) - (3)].params));}
    break;

  case 62:
#line 210 "solver_language_parser.y"
    {(yyval.method) = new SL::Method(new SL::Var((yyvsp[(1) - (8)].my_type), (yyvsp[(2) - (8)].identifier_)), (yyvsp[(4) - (8)].params), (yyvsp[(7) - (8)].code_block));}
    break;

  case 63:
#line 212 "solver_language_parser.y"
    {(yyval.method) = new SL::Method(new SL::Var(new SL::SLType(new SL::Identifier("any")), (yyvsp[(1) - (7)].identifier_)), (yyvsp[(3) - (7)].params), (yyvsp[(6) - (7)].code_block));}
    break;

  case 64:
#line 214 "solver_language_parser.y"
    {(yyval.methods) = new SL::Methods((yyvsp[(1) - (1)].method));}
    break;

  case 65:
#line 214 "solver_language_parser.y"
    {(yyval.methods) = new SL::Methods((yyvsp[(1) - (2)].method), (yyvsp[(2) - (2)].methods));}
    break;

  case 66:
#line 216 "solver_language_parser.y"
    {state->add_root((yyvsp[(1) - (1)].methods));}
    break;


/* Line 1267 of yacc.c.  */
#line 1870 "y.tab.c"
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
      yyerror (state, YY_("syntax error"));
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
	    yyerror (state, yymsg);
	  }
	else
	  {
	    yyerror (state, YY_("syntax error"));
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
		      yytoken, &yylval, state);
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
		  yystos[yystate], yyvsp, state);
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
  yyerror (state, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, state);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, state);
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


#line 218 "solver_language_parser.y"


void parse_solver_langauge_program(HyperSketchState* state, string hypersketch_file)
{
	yyscan_t scanner;
	yylex_init(&scanner);

	FILE* file_pointer = fopen(hypersketch_file.c_str(), "r");
	yyset_in(file_pointer, scanner);
	int rv = yyparse(scanner, state);
	free(scanner);
}
