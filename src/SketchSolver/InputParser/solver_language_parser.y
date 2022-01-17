%{

//#include "SolverLanguageLexAndYaccHeader.h"
#include "SolverLanguageYaccHeader.h"

int yylex_init (yyscan_t* scanner);

#include "solver_language_yacc.h"


#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

extern void yyset_in  ( FILE * _in_str , yyscan_t yyscanner );

%}

%pure-parser
%parse-param {yyscan_t yyscanner} {SolverProgramState* state}
%lex-param {yyscan_t yyscanner}


%union{
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
	SL::Expression* expression;}

%start root
%type <methods> methods
%type <code_block> code_block
%type <unit_line> unit_line
%type <unit_line> macro_unit
%type <assignment> assignment
%type <assignment> parameter_declaration
%token <name> identifier
%token <name> solver_token
%token <name> while_token
%token <name> for_token
%token <name> if_token
%token <name> return_token
%token <name> op_eq
%token <name> op_plus_plus
%token <var_val> var_val_rule
%type <var_val> constant
%type <param> param
%type <func_call> function_call
%type <params> params
%type <params> signature_params
%type <method> method
%type <predicate> predicate
%type <my_operator> comparison_op
%type <my_type> type_rule
%type <type_params> type_params
%type <expression> expression

%%

code_block :
	unit_line ';' {$$ = new SL::CodeBlock($1);} |
   	macro_unit {$$ = new SL::CodeBlock($1);} |
	unit_line ';' code_block {$$ = new SL::CodeBlock($1, $3);} |
	macro_unit code_block {$$ = new SL::CodeBlock($1, $2);}

comparison_op: '<' {$$ = SL::MyOperator::lt;} | '>' {$$ = SL::MyOperator::gt;} | op_eq {$$ = SL::MyOperator::eq;}

constant: '[' ']' {$$ = new SL::VarVal(new SL::PolyVec(new SL::PolyType(new vector<SL::SLType*>(1, new SL::SLType(new SL::Name("any"))))));}

expression:
	identifier {$$ = new SL::Expression($1);} |
	function_call {$$ = new SL::Expression($1);} |
	predicate {$$ = new SL::Expression($1);} |
	var_val_rule {$$ = new SL::Expression($1);} |
	constant {$$ = new SL::Expression($1);}

predicate:
	identifier comparison_op identifier {$$ = new SL::Predicate($2, new SL::Expression($1), new SL::Expression($3));}
////	|
//	expression comparison_op expression {$$ = new SL::Predicate($2, $1, $3);}

unit_line: assignment {$$ = new SL::UnitLine($1);} |
	return_token expression {$$ = new SL::UnitLine(new SL::Return($2));} |
	expression {$$ = new SL::UnitLine($1);}

macro_unit:
	while_token '(' predicate ')' '{' code_block '}' {$$ = new SL::UnitLine(new SL::While(new SL::Expression($3), $6));} |
	if_token '(' predicate ')' '{' code_block '}' {$$ = new SL::UnitLine(new SL::If(new SL::Expression($3), $6));} |
	for_token '(' unit_line ';' predicate ';' unit_line ')' '{' code_block '}'
		{$$ = new SL::UnitLine(new SL::For($3, new SL::Expression($5), $7, $10));} |
	'{' code_block '}' {$$ = new SL::UnitLine($2);}

function_call : expression '.' identifier '(' params ')' {$$ = new SL::FuncCall($1, $3, $5);}
	     | type_rule '(' params ')' {$$ = new SL::FuncCall($1, $3);}

type_params : type_rule {$$ = new SL::TypeParams($1);} | type_rule ',' type_params {$$ = new SL::TypeParams($1, $3);}

type_rule : identifier {$$ = new SL::SLType($1);} |
		identifier '<' type_params '>' {$$ = new SL::SLType($1, $3);}

assignment:
	type_rule identifier {$$ = new SL::Assignment(new SL::Var($1, $2));}
	| type_rule identifier '=' expression {$$ = new SL::Assignment(new SL::Var($1, $2), $4);}
	| identifier '=' expression {$$ = new SL::Assignment($1, $3);}
	| identifier op_plus_plus
	{$$ = new SL::Assignment(
		$1,
		new SL::Expression(
			new SL::FuncCall(
				new SL::Name("plus"),
				new SL::Params(new SL::Param(
					new SL::Expression($1)),
					new SL::Param(new SL::Expression(new SL::VarVal((int)1)))))));}

param : expression {$$ = new SL::Param($1);}

params :  {$$ = new SL::Params();} | param {$$ = new SL::Params($1);}
	| param ',' params {$$ = new SL::Params($1, $3);}

parameter_declaration:
	type_rule identifier {$$ = new SL::Assignment(new SL::Var($1, $2));}
	| identifier {$$ = new SL::Assignment(new SL::Var(new SL::SLType(new SL::Name("any")), $1), nullptr);}

signature_params: {$$ = new SL::Params();} | parameter_declaration {$$ = new SL::Params(new SL::Param($1));} |
	      parameter_declaration ',' signature_params {$$ = new SL::Params(new SL::Param($1), $3);}

method : solver_token type_rule identifier '(' signature_params ')'
	  '{' code_block '}' {$$ = new SL::Method(new SL::Var($2, $3), $5, $8);}
	  | solver_token identifier '(' signature_params ')'
            	  '{' code_block '}' {$$ = new SL::Method(new SL::Var(new SL::SLType(new SL::Name("any")), $2), $4, $7);}

methods : method {$$ = new SL::Methods($1);} | method methods {$$ = new SL::Methods($1, $2);}

root: methods {state->add_root($1);}

%%

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

