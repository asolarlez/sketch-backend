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
	SL::FunctionCall* func_call;
	SL::Identifier* identifier_;
	SL::Params* params;
	SL::Param* param;
	SL::UnitLine* unit_line;
	SL::Assignment* assignment;
	SL::CodeBlock* code_block;
	SL::Expression* expression;
	}

%start root
%type <unit_line> unit_line
%type <assignment> assignment
%token <identifier_> identifier
%type <param> param
%type <func_call> function_call
%type <params> params
%type <code_block> code_block
%type <expression> expression

%%

root: code_block {state->add_root($1);}

code_block :
	unit_line ';' {$$ = new SL::CodeBlock($1);} |
	unit_line ';' code_block {$$ = new SL::CodeBlock($1, $3);}

unit_line: assignment {$$ = new SL::UnitLine($1);} |
//	return_token expression {$$ = new SL::UnitLine(new SL::Return($2));} |
	expression {$$ = new SL::UnitLine($1);}


expression:
	identifier {$$ = new SL::Expression($1);} |
	function_call {$$ = new SL::Expression($1);}

function_call :
	expression '.' identifier '(' params ')' {$$ = new SL::FunctionCall($1, $3, $5);} |
	'[' params ']' {$$ = new SL::FunctionCall(new SL::SLType(new SL::Identifier("vector"), new SL::TypeParams(new SL::SLType(new SL::Identifier("any")))), $2);} |
	'{' params '}' {$$ = new SL::FunctionCall(new SL::Identifier("Map"), $2);} |
	param ':' param {$$ = new SL::FunctionCall(new SL::Identifier("Pair"), new SL::Params($1, new SL::Params($3)));}

assignment:
 	identifier '=' expression {$$ = new SL::Assignment($1, $3);}

param : expression {$$ = new SL::Param($1);}

params :  {$$ = new SL::Params();} | param {$$ = new SL::Params($1);} | param ',' params {$$ = new SL::Params($1, $3);}


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
	yyset_in(file_pointer, scanner);
	int rv = yyparse(scanner, state);
	free(scanner);
}

