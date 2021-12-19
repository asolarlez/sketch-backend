%option reentrant
%{

#include "SolverLanguageLexAndYaccHeader.h"

#include "solver_language_yacc.h"

#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)
void yyerror(yyscan_t scanner, string s);

void print_rule(string description, string str);

%}


%option noyywrap

Digit        ([0-9])
Integer      ({Digit}+)
HexInteger   ("0x"{Integer})
Double       ({Integer}"."{Digit}*("E"[\+\-]{Digit}+)?)
String       ("\""[^\n\"]*"\"")
Identifier   ([a-zA-Z_\#][a-zA-Z_0-9]*)
Operator     ([\%\/\<\>\;\!\?\*\-\+\,\.\:\[\]\(\)\{\}\=\|\&\^\$])
WhiteSpace   ([ \t\n]*)
Comment      ("//"[^\n]*)

%%
Solver                      {print_rule("Solver", string(yytext));
                                yylval->name = new Name(string(yytext)); return solver_token;}
while                       {print_rule("while", string(yytext));
                                yylval->name = new Name(string(yytext)); return while_token;}
if                       {print_rule("if", string(yytext));
                                yylval->name = new Name(string(yytext)); return if_token;}
return                       {print_rule("return", string(yytext)); return return_token;}
==                          {print_rule("return", string(yytext)); return op_eq;}

{Identifier}                {print_rule("Identifier", string(yytext));
                                yylval->name = new Name(string(yytext)); return identifier;}
{String}                    {print_rule("String", string(yytext));
                                yylval->my_const = new Const(string(yytext));
                                return my_constant;}
{Integer}                   {print_rule("Integer", string(yytext));
                                yylval->my_const = new Const(atoi(yytext)); return my_constant;}
{Comment}                   {print_rule("Comment", string(yytext));}
{WhiteSpace}                {print_rule("WhiteSpace", string(yytext));}
{Operator}                  {print_rule("Operator", string(yytext));
                                assert(string(yytext).size() == 1); return yytext[0];}
.                           {print_rule("NotRecognized", string(yytext)); assert(false);}
%%

void print_rule(string description, string str)
{
    cout << "RULE " << description <<" TOKEN " << str << endl;
}

void yyerror(yyscan_t scanner, string s)
{
    cout << "ERROR " << s << endl;
}
