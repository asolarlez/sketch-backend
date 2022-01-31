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

%start root
%type <methods> methods
%type <code_block> code_block
%type <unit_line> unit_line
%type <unit_line> macro_unit
%type <assignment> assignment
%type <assignment> parameter_declaration
%token <identifier_> identifier
%token <identifier_> solver_token
%token <identifier_> while_token
%token <identifier_> for_token
%token <identifier_> if_token
%token <identifier_> return_token
%token <identifier_> lambda_token
%token <identifier_> op_eq
%token <identifier_> op_geq
%token <identifier_> op_plus_plus
%token <var_val> var_val_rule
%type <var_val> constant
%type <param> param
%type <func_call> function_call
%type <func_call> constructor_call
%type <params> params
%type <params> signature_params
%type <method> method
%type <bool_expr> binary_expression
%type <lambda_expr> lambda_expression
%type <my_operator> binary_op
%type <my_type> type_rule
%type <type_params> type_params
%type <expression> expression
%type <expression> constuctor_call_expression

%%

code_block :
	unit_line ';' {$$ = new SL::CodeBlock($1);} |
   	macro_unit {$$ = new SL::CodeBlock($1);} |
	unit_line ';' code_block {$$ = new SL::CodeBlock($1, $3);} |
	macro_unit code_block {$$ = new SL::CodeBlock($1, $2);}

binary_op:
	'<' {$$ = SL::BinaryOp::_lt;} |
	'>' {$$ = SL::BinaryOp::_gt;} |
	op_eq {$$ = SL::BinaryOp::_eq;} |
	op_geq {$$ = SL::BinaryOp::_geq;} |
	'+' {$$ = SL::BinaryOp::_plus;} |
	'-' {$$ = SL::BinaryOp::_minus;} |
	'*' {$$ = SL::BinaryOp::_mult;} |
	'/' {$$ = SL::BinaryOp::_div;}

constant: '[' ']' {$$ = new SL::VarVal(new SL::PolyVec(new SL::PolyType(new vector<SL::SLType*>(1, new SL::SLType(new SL::Identifier("any"))))));}

expression:
	identifier {$$ = new SL::Expression($1);} |
	function_call {$$ = new SL::Expression($1);} |
	binary_expression {$$ = new SL::Expression($1);} |
	var_val_rule {$$ = new SL::Expression($1);} |
	constant {$$ = new SL::Expression($1);} |
	lambda_expression {$$ = new SL::Expression($1);} |
	'(' expression ')' {$$ = $2;}

constuctor_call_expression:
	constructor_call {$$ = new SL::Expression($1);}

lambda_expression:
	lambda_token '[' signature_params ']' '(' signature_params ')' '{' code_block '}' {$$ = new SL::LambdaExpression($3, $6, $9);}

binary_expression:
	expression binary_op expression {$$ = new SL::BinaryExpression($2, $1, $3);}

unit_line: assignment {$$ = new SL::UnitLine($1);} |
	return_token expression {$$ = new SL::UnitLine(new SL::Return($2));} |
	expression {$$ = new SL::UnitLine($1);}

macro_unit:
	while_token '(' expression ')' '{' code_block '}' {$$ = new SL::UnitLine(new SL::While(new SL::Expression($3), $6));} |
	if_token '(' expression ')' '{' code_block '}' {$$ = new SL::UnitLine(new SL::If(new SL::Expression($3), $6));} |
	for_token '(' unit_line ';' expression ';' unit_line ')' '{' code_block '}'
		{$$ = new SL::UnitLine(new SL::For($3, new SL::Expression($5), $7, $10));} |
	'{' code_block '}' {$$ = new SL::UnitLine($2);}

function_call : expression '.' identifier '(' params ')' {$$ = new SL::FunctionCall($1, $3, $5);}
	     | identifier '(' params ')' {$$ = new SL::FunctionCall($1, $3);}

constructor_call:
	type_rule '(' params ')' {$$ = new SL::FunctionCall($1, $3);}

type_params : type_rule {$$ = new SL::TypeParams($1);} | type_rule ',' type_params {$$ = new SL::TypeParams($1, $3);}

type_rule : identifier {$$ = new SL::SLType($1);} |
		identifier '<' type_params '>' {$$ = new SL::SLType($1, $3);}

assignment:
	type_rule identifier {$$ = new SL::Assignment(new SL::Var($1, $2));}
	| type_rule identifier '=' expression {$$ = new SL::Assignment(new SL::Var($1, $2), $4);}
	| type_rule identifier '=' constuctor_call_expression {$$ = new SL::Assignment(new SL::Var($1, $2), $4);}
	| identifier '=' expression {$$ = new SL::Assignment($1, $3);}
	| identifier op_plus_plus
	{$$ = new SL::Assignment(
		$1,
		new SL::Expression(
			new SL::BinaryExpression(
				SL::BinaryOp::_plus,
				new SL::Expression($1),
				new SL::Expression(new SL::VarVal((int)1)))));}

param : expression {$$ = new SL::Param($1);} | constuctor_call_expression {$$ = new SL::Param($1);}

params :  {$$ = new SL::Params();} | param {$$ = new SL::Params($1);}
	| param ',' params {$$ = new SL::Params($1, $3);}

parameter_declaration:
	type_rule identifier {$$ = new SL::Assignment(new SL::Var($1, $2));}
	| identifier {$$ = new SL::Assignment(new SL::Var(new SL::SLType(new SL::Identifier("any")), $1), nullptr);}

signature_params: {$$ = new SL::Params();} | parameter_declaration {$$ = new SL::Params(new SL::Param($1));} |
	      parameter_declaration ',' signature_params {$$ = new SL::Params(new SL::Param($1), $3);}

method : solver_token type_rule identifier '(' signature_params ')'
	  '{' code_block '}' {$$ = new SL::Method(new SL::Var($2, $3), $5, $8);}
	  | solver_token identifier '(' signature_params ')'
            	  '{' code_block '}' {$$ = new SL::Method(new SL::Var(new SL::SLType(new SL::Identifier("any")), $2), $4, $7);}

methods : method {$$ = new SL::Methods($1);} | method methods {$$ = new SL::Methods($1, $2);}

root: methods {state->add_root($1);}

%%

void parse_solver_langauge_program(SolverProgramState* state, string solver_program_file)
{
	void* scanner = nullptr;
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

