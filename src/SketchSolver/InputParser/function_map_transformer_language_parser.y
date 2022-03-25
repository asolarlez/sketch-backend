%{

#include "FunctionMapTransformerLanguage.h"

int yylex_init (yyscan_t* scanner);

#include "function_map_transformer_language_yacc.h"

#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
extern int yylex (YYSTYPE* yylval, yyscan_t yyscanner);

extern void yyset_in  ( FILE * _in_str , yyscan_t yyscanner );

%}

%pure-parser
%parse-param {yyscan_t yyscanner} {FMTL::FunctionMapTransformerState* state}
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
	SL::BinaryExpression* bool_expr;
	SL::BinaryOp my_operator;
	SL::SLType* my_type;
	SL::TypeParams* type_params;
	SL::Expression* expression;
	SL::LambdaExpression* lambda_expr;}

%start root
%type <code_block> code_block
%type <unit_line> unit_line
%type <assignment> assignment
%token <identifier_> identifier
%token <identifier_> solver_token
%token <identifier_> while_token
%token <identifier_> for_token
%token <identifier_> if_token
%token <identifier_> else_token
%token <identifier_> return_token
%token <identifier_> lambda_token
%token <identifier_> op_eq
%token <identifier_> op_geq
%token <identifier_> op_plus_plus
%token <var_val> var_val_rule
%type <param> param
%type <param> key_col_val
%type <func_call> function_call
%type <params> params
%type <params> key_col_vals
%type <expression> expression

%%

root: code_block {state->add_root($1);}

code_block :
	unit_line ';' {$$ = new SL::CodeBlock($1);} |
	unit_line ';' code_block {$$ = new SL::CodeBlock($1, $3);}

expression:
	identifier {$$ = new SL::Expression($1);} |
	function_call {$$ = new SL::Expression($1);} |
	var_val_rule {$$ = new SL::Expression($1);}

unit_line: assignment {$$ = new SL::UnitLine($1);} |
	expression {$$ = new SL::UnitLine($1);}

function_call : expression '.' identifier '(' params ')' {$$ = new SL::FunctionCall($1, $3, $5);}
	     | '[' params ']' {$$ = new SL::FunctionCall(
               				new SL::SLType(new SL::Identifier("vector"),
               				new SL::TypeParams(
               					new SL::SLType(new SL::Identifier("any"))
               				)), $2);}
	     | '{' key_col_vals '}' {$$ = new SL::FunctionCall(
               				new SL::SLType(
               					new SL::Identifier("map"),
						new SL::TypeParams(
							new SL::SLType(new SL::Identifier("string")),
							new SL::TypeParams(new SL::SLType(new SL::Identifier("any")))
						)
					), $2);}

assignment: identifier '=' expression {$$ = new SL::Assignment($1, $3);}

param : expression {$$ = new SL::Param($1);}

params :  {$$ = new SL::Params();} | param {$$ = new SL::Params($1);}
	| param ',' params {$$ = new SL::Params($1, $3);}

key_col_val: '(' var_val_rule ':' identifier ')' {
             			SL::Params* params =
             				new SL::Params(
             					new SL::Param(new SL::Expression($2)),
             					new SL::Params(new SL::Param(new SL::Expression($4))));
				SL::Expression* almost_ret = new SL::Expression(
             				new SL::FunctionCall(
             					new SL::SLType(new SL::Identifier("pair"),
             					new SL::TypeParams(
             						new SL::SLType(new SL::Identifier("string")),
             						new SL::TypeParams(new SL::SLType(new SL::Identifier("any")))
             					)
             				), params)
				);
				$$ = new SL::Param(almost_ret);}

key_col_vals:
  	{$$ = new SL::Params();} |
	key_col_val {$$ = new SL::Params($1);} |
	key_col_val ',' key_col_vals {$$ = new SL::Params($1, $3);}

%%

void yyerror(yyscan_t scanner, FMTL::FunctionMapTransformerState* state, string s)
{
    cout << "ERROR " << s << endl;
}

void FMTL::parse_function_map_transformer_program(FMTL::FunctionMapTransformerState* state, string function_map_transformer_program_file)
{
	yyscan_t scanner;
	yylex_init(&scanner);

	char function_map_transformer_program_file_char[function_map_transformer_program_file.size()];
	for(int i = 0;i<function_map_transformer_program_file.size();i++)
	{
		function_map_transformer_program_file_char[i] = function_map_transformer_program_file[i];
	}

	FILE* file_pointer = fopen(function_map_transformer_program_file_char, "r");
	cout << "ENTERING LEXER" << endl;
	yyset_in(file_pointer, scanner);
	cout << "ENTERING PARSER" << endl;
	int rv = yyparse(scanner, state);
	free(scanner);
}

