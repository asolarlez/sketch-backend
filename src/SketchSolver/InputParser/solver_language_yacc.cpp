/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "solver_language_parser.y" /* yacc.c:339  */


//#include "SolverLanguageLexAndYaccHeader.h"
#include "SolverLanguageYaccHeader.h"

int yylex_init (yyscan_t* scanner);

#include "solver_language_yacc.h"


#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

extern void yyset_in  ( FILE * _in_str , yyscan_t yyscanner );


#line 83 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
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
    identifier = 258,
    solver_token = 259,
    while_token = 260,
    for_token = 261,
    if_token = 262,
    return_token = 263,
    op_eq = 264,
    op_plus_plus = 265,
    var_val_rule = 266
  };
#endif
/* Tokens.  */
#define identifier 258
#define solver_token 259
#define while_token 260
#define for_token 261
#define if_token 262
#define return_token 263
#define op_eq 264
#define op_plus_plus 265
#define var_val_rule 266

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 23 "solver_language_parser.y" /* yacc.c:355  */

	SL::Var* var;
	SL::VarVal* var_val;
	SL::FuncCall* func_call;
	SL::Name* name;
	SL::Params* params;
	SL::Param* param;
	SL::CodeBlock* code_block;
	SL::UnitLine* unit_line;
	SL::Assignment* assignment;
	SL::Method* method;
	SL::Methods* methods;
	SL::Predicate* predicate;
	SL::MyOperator my_operator;
	SL::SLType* my_type;
	SL::TypeParams* type_params;
	SL::Expression* expression;

#line 164 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (yyscan_t yyscanner, SolverProgramState* state);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 180 "y.tab.c" /* yacc.c:358  */

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
#else
typedef signed char yytype_int8;
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
# elif ! defined YYSIZE_T
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
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   120

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  24
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  19
/* YYNRULES -- Number of rules.  */
#define YYNRULES  46
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  107

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   266

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      17,    18,     2,     2,    22,     2,    21,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    12,
      13,    23,    14,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    15,     2,    16,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    19,     2,    20,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9,    10,    11
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    72,    72,    73,    74,    75,    77,    77,    77,    79,
      82,    83,    84,    85,    86,    89,    93,    94,    95,    98,
      99,   100,   102,   104,   105,   107,   107,   109,   110,   113,
     114,   115,   116,   126,   128,   128,   129,   132,   133,   135,
     135,   136,   138,   140,   143,   143,   145
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "identifier", "solver_token",
  "while_token", "for_token", "if_token", "return_token", "op_eq",
  "op_plus_plus", "var_val_rule", "';'", "'<'", "'>'", "'['", "']'", "'('",
  "')'", "'{'", "'}'", "'.'", "','", "'='", "$accept", "code_block",
  "comparison_op", "constant", "expression", "predicate", "unit_line",
  "macro_unit", "function_call", "type_params", "type_rule", "assignment",
  "param", "params", "parameter_declaration", "signature_params", "method",
  "methods", "root", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,    59,    60,    62,    91,    93,    40,    41,   123,
     125,    46,    44,    61
};
# endif

#define YYPACT_NINF -54

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-54)))

#define YYTABLE_NINF -28

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
       6,    10,     6,   -54,    25,    13,    28,   -54,   -54,    37,
      42,    36,    35,    46,    43,    11,    66,    51,    58,    42,
     -54,    37,   -54,    42,    61,    63,   -54,   -54,    56,    64,
      29,    67,    68,    70,    12,   -54,    73,    56,    71,   -54,
      69,   -54,    80,    56,   -54,     2,   -54,    56,   -54,   -54,
      37,   -54,    12,    90,    91,    26,    91,    65,    69,    78,
     -54,    76,   -54,    94,    56,   -54,    75,    12,    79,    69,
     -54,     3,    82,    89,    84,   -54,    86,   -54,    12,    69,
      83,    88,   -54,   -54,    85,    91,    92,    12,    69,    12,
     -54,    56,    95,    56,    96,   -54,    93,    26,    97,   -54,
     -54,    98,   -54,    99,    56,   100,   -54
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,    44,    46,     0,    27,     0,    45,     1,     0,
      39,     0,    27,     0,    25,    38,     0,    40,     0,    39,
      28,     0,    37,    39,     0,     0,    26,    41,     0,     0,
      10,     0,     0,     0,     0,    13,     0,     0,     0,    14,
      18,    12,     0,     3,    11,     0,    16,     0,     8,    32,
       0,     7,     0,     0,     0,     0,     0,    10,    17,     0,
       9,     0,    43,     0,     2,     5,    29,    34,     0,    31,
      15,     0,     0,     0,     0,    22,     0,     4,     0,    33,
      35,     0,    42,     6,     0,     0,     0,    34,    30,    34,
      24,     0,     0,     0,     0,    36,     0,     0,     0,    23,
      19,     0,    20,     0,     0,     0,    21
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -54,   -36,   -54,   -54,   -31,   -50,   -53,   -54,   -54,    87,
      -1,   -54,   -54,   -17,   -54,    31,   -54,   107,   -54
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    38,    53,    39,    40,    41,    42,    43,    44,    13,
      45,    46,    80,    81,    17,    18,     2,     3,     4
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
       6,    61,    73,    58,    72,    66,    74,    65,    14,    16,
       1,    68,    48,     5,   -27,    57,    83,    51,    16,    67,
      14,    69,    16,    35,     9,     8,     9,    36,    77,    30,
      10,    11,   -27,    59,    34,    92,    79,    35,    48,    49,
      12,    36,    50,    51,   101,    15,   -27,    88,     9,    14,
      25,    59,    52,    19,    27,    96,    79,    98,    79,    30,
      20,    31,    32,    33,    34,    21,    59,    35,   105,    22,
      94,    36,    95,    23,    48,    37,    24,    59,    50,    51,
      28,    29,   -27,    47,    54,    55,    59,    56,    59,    60,
      63,    62,    64,    70,    71,    67,    75,    76,    78,    82,
      84,    85,    86,    87,    91,    89,    90,    97,    26,     7,
       0,    93,     0,   100,    99,     0,   103,   102,   104,     0,
     106
};

static const yytype_int8 yycheck[] =
{
       1,    37,    55,    34,    54,     3,    56,    43,     9,    10,
       4,    47,     9,     3,     3,     3,    13,    14,    19,    17,
      21,    52,    23,    11,    13,     0,    13,    15,    64,     3,
      17,     3,     3,    34,     8,    85,    67,    11,     9,    10,
       3,    15,    13,    14,    97,     3,    17,    78,    13,    50,
      19,    52,    23,    17,    23,    91,    87,    93,    89,     3,
      14,     5,     6,     7,     8,    22,    67,    11,   104,     3,
      87,    15,    89,    22,     9,    19,    18,    78,    13,    14,
      19,    18,    17,    19,    17,    17,    87,    17,    89,    16,
      21,    20,    12,     3,     3,    17,    20,     3,    23,    20,
      18,    12,    18,    17,    19,    22,    18,    12,    21,     2,
      -1,    19,    -1,    20,    18,    -1,    18,    20,    19,    -1,
      20
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,    40,    41,    42,     3,    34,    41,     0,    13,
      17,     3,     3,    33,    34,     3,    34,    38,    39,    17,
      14,    22,     3,    22,    18,    39,    33,    39,    19,    18,
       3,     5,     6,     7,     8,    11,    15,    19,    25,    27,
      28,    29,    30,    31,    32,    34,    35,    19,     9,    10,
      13,    14,    23,    26,    17,    17,    17,     3,    28,    34,
      16,    25,    20,    21,    12,    25,     3,    17,    25,    28,
       3,     3,    29,    30,    29,    20,     3,    25,    23,    28,
      36,    37,    20,    13,    18,    12,    18,    17,    28,    22,
      18,    19,    29,    19,    37,    37,    25,    12,    25,    18,
      20,    30,    20,    18,    19,    25,    20
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    24,    25,    25,    25,    25,    26,    26,    26,    27,
      28,    28,    28,    28,    28,    29,    30,    30,    30,    31,
      31,    31,    31,    32,    32,    33,    33,    34,    34,    35,
      35,    35,    35,    36,    37,    37,    37,    38,    38,    39,
      39,    39,    40,    40,    41,    41,    42
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     3,     2,     1,     1,     1,     2,
       1,     1,     1,     1,     1,     3,     1,     2,     1,     7,
       7,    11,     3,     6,     4,     1,     3,     1,     4,     2,
       4,     3,     2,     1,     0,     1,     3,     2,     1,     0,
       1,     3,     9,     8,     1,     2,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (yyscanner, state, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, yyscanner, state); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, yyscan_t yyscanner, SolverProgramState* state)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yyscanner);
  YYUSE (state);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, yyscan_t yyscanner, SolverProgramState* state)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yyscanner, state);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, yyscan_t yyscanner, SolverProgramState* state)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , yyscanner, state);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, yyscanner, state); \
} while (0)

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
#ifndef YYINITDEPTH
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
static YYSIZE_T
yystrlen (const char *yystr)
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
static char *
yystpcpy (char *yydest, const char *yysrc)
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, yyscan_t yyscanner, SolverProgramState* state)
{
  YYUSE (yyvaluep);
  YYUSE (yyscanner);
  YYUSE (state);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t yyscanner, SolverProgramState* state)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, yyscanner);
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

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
     '$$ = $1'.

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
#line 72 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[-1].unit_line));}
#line 1330 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 73 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[0].unit_line));}
#line 1336 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 74 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[-2].unit_line), (yyvsp[0].code_block));}
#line 1342 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 75 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.code_block) = new SL::CodeBlock((yyvsp[-1].unit_line), (yyvsp[0].code_block));}
#line 1348 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 77 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.my_operator) = SL::MyOperator::lt;}
#line 1354 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 77 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.my_operator) = SL::MyOperator::gt;}
#line 1360 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 77 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.my_operator) = SL::MyOperator::eq;}
#line 1366 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 79 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.var_val) = new SL::VarVal(new SL::PolyVec(new SL::PolyType(new vector<SL::SLType*>(1, new SL::SLType(new SL::Name("any"))))));}
#line 1372 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 82 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.expression) = new SL::Expression((yyvsp[0].name));}
#line 1378 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 83 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.expression) = new SL::Expression((yyvsp[0].func_call));}
#line 1384 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 84 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.expression) = new SL::Expression((yyvsp[0].predicate));}
#line 1390 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 85 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.expression) = new SL::Expression((yyvsp[0].var_val));}
#line 1396 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 86 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.expression) = new SL::Expression((yyvsp[0].var_val));}
#line 1402 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 89 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.predicate) = new SL::Predicate((yyvsp[-1].my_operator), new SL::Expression((yyvsp[-2].name)), new SL::Expression((yyvsp[0].name)));}
#line 1408 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 93 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[0].assignment));}
#line 1414 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 94 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine(new SL::Return((yyvsp[0].expression)));}
#line 1420 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 95 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[0].expression));}
#line 1426 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 98 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine(new SL::While(new SL::Expression((yyvsp[-4].predicate)), (yyvsp[-1].code_block)));}
#line 1432 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 99 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine(new SL::If(new SL::Expression((yyvsp[-4].predicate)), (yyvsp[-1].code_block)));}
#line 1438 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 101 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine(new SL::For((yyvsp[-8].unit_line), new SL::Expression((yyvsp[-6].predicate)), (yyvsp[-4].unit_line), (yyvsp[-1].code_block)));}
#line 1444 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 102 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.unit_line) = new SL::UnitLine((yyvsp[-1].code_block));}
#line 1450 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 104 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.func_call) = new SL::FuncCall((yyvsp[-5].expression), (yyvsp[-3].name), (yyvsp[-1].params));}
#line 1456 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 105 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.func_call) = new SL::FuncCall((yyvsp[-3].my_type), (yyvsp[-1].params));}
#line 1462 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 107 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.type_params) = new SL::TypeParams((yyvsp[0].my_type));}
#line 1468 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 107 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.type_params) = new SL::TypeParams((yyvsp[-2].my_type), (yyvsp[0].type_params));}
#line 1474 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 109 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.my_type) = new SL::SLType((yyvsp[0].name));}
#line 1480 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 110 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.my_type) = new SL::SLType((yyvsp[-3].name), (yyvsp[-1].type_params));}
#line 1486 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 113 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[-1].my_type), (yyvsp[0].name)));}
#line 1492 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 114 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[-3].my_type), (yyvsp[-2].name)), (yyvsp[0].expression));}
#line 1498 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 115 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment((yyvsp[-2].name), (yyvsp[0].expression));}
#line 1504 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 117 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment(
		(yyvsp[-1].name),
		new SL::Expression(
			new SL::FuncCall(
				new SL::Name("plus"),
				new SL::Params(new SL::Param(
					new SL::Expression((yyvsp[-1].name))),
					new SL::Param(new SL::Expression(new SL::VarVal((int)1)))))));}
#line 1517 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 126 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.param) = new SL::Param((yyvsp[0].expression));}
#line 1523 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 128 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params();}
#line 1529 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 128 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params((yyvsp[0].param));}
#line 1535 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 129 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params((yyvsp[-2].param), (yyvsp[0].params));}
#line 1541 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 132 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment(new SL::Var((yyvsp[-1].my_type), (yyvsp[0].name)));}
#line 1547 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 133 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.assignment) = new SL::Assignment(new SL::Var(new SL::SLType(new SL::Name("any")), (yyvsp[0].name)), nullptr);}
#line 1553 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 135 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params();}
#line 1559 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 135 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params(new SL::Param((yyvsp[0].assignment)));}
#line 1565 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 136 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.params) = new SL::Params(new SL::Param((yyvsp[-2].assignment)), (yyvsp[0].params));}
#line 1571 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 139 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.method) = new SL::Method(new SL::Var((yyvsp[-7].my_type), (yyvsp[-6].name)), (yyvsp[-4].params), (yyvsp[-1].code_block));}
#line 1577 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 141 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.method) = new SL::Method(new SL::Var(new SL::SLType(new SL::Name("any")), (yyvsp[-6].name)), (yyvsp[-4].params), (yyvsp[-1].code_block));}
#line 1583 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 143 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.methods) = new SL::Methods((yyvsp[0].method));}
#line 1589 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 143 "solver_language_parser.y" /* yacc.c:1646  */
    {(yyval.methods) = new SL::Methods((yyvsp[-1].method), (yyvsp[0].methods));}
#line 1595 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 145 "solver_language_parser.y" /* yacc.c:1646  */
    {state->add_root((yyvsp[0].methods));}
#line 1601 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1605 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (yyscanner, state, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yyscanner, state, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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
                      yytoken, &yylval, yyscanner, state);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
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

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
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
                  yystos[yystate], yyvsp, yyscanner, state);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (yyscanner, state, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, yyscanner, state);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yyscanner, state);
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
  return yyresult;
}
#line 147 "solver_language_parser.y" /* yacc.c:1906  */


void parse_solver_langauge_program(SolverProgramState* state, string solver_program_file)
{
	void* scanner;
	yylex_init(&scanner);

	char solver_program_file_char[solver_program_file.size()];
	for(int i = 0;i<solver_program_file.size();i++)
	{
		solver_program_file_char[i] = solver_program_file[i];
	}

	FILE* file_pointer = fopen(solver_program_file_char, "r");
	yyset_in(file_pointer, scanner);
	int rv = yyparse(scanner, state);
}

