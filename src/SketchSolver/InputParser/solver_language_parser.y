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
	SL::Operand* operand;
	SL::MyOperator my_operator;}

%start root
%type <methods> methods
%type <code_block> lines
%type <unit_line> unit
%type <assignment> assignment
%token <name> identifier
%token <name> solver_token
%token <name> while_token
%token <name> if_token
%token <name> return_token
%token <name> op_eq
%token <var_val> var_val_rule
%type <param> param
%type <assignment> declaration
%type <func_call> function_call
%type <params> params
%type <params> typed_params
%type <method> method
%type <predicate> predicate
%type <operand> operand
%type <my_operator> operator

%%


lines : unit ';' {$$ = new SL::CodeBlock($1);} | unit ';' lines {$$ = new SL::CodeBlock($1, $3);}

operand: identifier {$$ = new SL::Operand($1);}

operator: '<' {$$ = SL::MyOperator::lt;} | '>' {$$ = SL::MyOperator::gt;} | op_eq {$$ = SL::MyOperator::eq;}

predicate: operand operator operand {$$ = new SL::Predicate(new SL::CompositePredicate($2, $1, $3));}

unit: declaration {$$ = new SL::UnitLine($1);} | assignment {$$ = new SL::UnitLine($1);} |
	while_token '(' predicate ')' '{' lines '}' {$$ = new SL::UnitLine(new SL::While($3, $6));} |
	if_token '(' predicate ')' '{' lines '}' {$$ = new SL::UnitLine(new SL::If($3, $6));} |
	return_token identifier {$$ = new SL::UnitLine(new SL::Return($2));} | function_call {$$ = new SL::UnitLine($1);}
function_call : identifier '.' identifier '(' params ')' {$$ = new SL::FuncCall($1, $3, $5);}
	     | identifier '(' params ')' {$$ = new SL::FuncCall(new SL::Name("global"), $1, $3);}
declaration : identifier identifier {$$ = new SL::Assignment(new SL::Var($1, $2));}
		| identifier identifier '=' function_call {$$ = new SL::Assignment(new SL::Var($1, $2), $4);}
		| identifier identifier '=' var_val_rule {$$ = new SL::Assignment(new SL::Var($1, $2), $4);}
assignment : identifier '=' function_call {$$ =
//set_var_val($1, $3);
new SL::Assignment($1, $3);}
//		| declaration '=' function_call {$$ = set_var_val($1, $3);}
	|
		identifier '=' identifier {$$ = new SL::Assignment($1, $3);}
param : identifier {$$ = new SL::Param($1);} | var_val_rule {$$ = new SL::Param($1);}
params :  {$$ = new SL::Params();} | param {$$ = new SL::Params($1);}
	| param ',' params {$$ = new SL::Params($1, $3);}

typed_params: {$$ = new SL::Params();} | declaration {$$ = new SL::Params(new SL::Param($1));} |
	      declaration ',' typed_params {$$ = new SL::Params(new SL::Param($1), $3);}

method : solver_token identifier identifier '(' typed_params ')'
	  '{' lines '}' {$$ = new SL::Method(new SL::Var($2, $3), $5, $8);}

methods : method {$$ = new SL::Methods($1);} | method methods {$$ = new SL::Methods($1, $2);}

root: methods {state->add_root($1);}

%%

void run_solver_langauge_program(SolverProgramState* state, string solver_program_file)
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

int main(){
	run_solver_langauge_program(nullptr, "");
}
