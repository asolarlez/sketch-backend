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
	Var* var;
	VarVal* var_val;
	FuncCall* func_call;
	Name* name;
	Params* params;
	Param* param;
	CodeBlock* code_block;
	UnitLine* unit_line;
	Assignment* assignment;
	Method* method;
	Methods* methods;
	Predicate* predicate;
	Operand* operand;
	MyOperator my_operator;}

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


lines : unit ';' {$$ = new CodeBlock($1);} | unit ';' lines {$$ = new CodeBlock($1, $3);}

operand: identifier {$$ = new Operand($1);}

operator: '<' {$$ = MyOperator::lt;} | '>' {$$ = MyOperator::gt;} | op_eq {$$ = MyOperator::eq;}

predicate: operand operator operand {$$ = new Predicate(new CompositePredicate($2, $1, $3));}

unit: declaration {$$ = new UnitLine($1);} | assignment {$$ = new UnitLine($1);} |
	while_token '(' predicate ')' '{' lines '}' {$$ = new UnitLine(new While($3, $6));} |
	if_token '(' predicate ')' '{' lines '}' {$$ = new UnitLine(new If($3, $6));} |
	return_token identifier {$$ = new UnitLine(new Return($2));}
function_call : identifier '.' identifier '(' params ')' {$$ = new FuncCall($1, $3, $5);}
	     | identifier '(' params ')' {$$ = new FuncCall(new Name("global"), $1, $3);}
declaration : identifier identifier {$$ = new Assignment(new Var($1, $2));}
		| identifier identifier '=' function_call {$$ = new Assignment(new Var($1, $2), $4);}
		| identifier identifier '=' var_val_rule {$$ = new Assignment(new Var($1, $2), $4);}
assignment : identifier '=' function_call {$$ =
//set_var_val($1, $3);
new Assignment($1, $3);}
//		| declaration '=' function_call {$$ = set_var_val($1, $3);}
	|
		identifier '=' identifier {$$ = new Assignment($1, $3);}
param : identifier {$$ = new Param($1);} | var_val_rule {$$ = new Param($1);}
params :  {$$ = new Params();} | param {$$ = new Params($1);}
	| param ',' params {$$ = new Params($1, $3);}

typed_params: {$$ = new Params();} | declaration {$$ = new Params(new Param($1));} |
	      declaration ',' typed_params {$$ = new Params(new Param($1), $3);}

method : solver_token identifier identifier '(' typed_params ')'
	  '{' lines '}' {$$ = new Method(new Var($2, $3), $5, $8);}

methods : method {$$ = new Methods($1);} | method methods {$$ = new Methods($1, $2);}

root: methods {state->add_root($1);}

%%

//void SL_LY::set_var_val(Name* name, FuncCall* expr){assignments[name->get_name()] = expr;}
//void SL_LY::set_var_val(Var* var, FuncCall* expr){assignments[var->get_name()->get_name()] = expr;}

void run_solver_langauge_program(SolverProgramState* state)
{
	void* scanner;
	yylex_init(&scanner);

	FILE* file_pointer = fopen("solver_language_program.txt", "r");
	yyset_in(file_pointer, scanner);
	int rv = yyparse(scanner, state);
}

int main(){
	run_solver_langauge_program(nullptr);
}
