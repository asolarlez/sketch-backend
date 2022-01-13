%option reentrant
%{

#include "SolverLanguageLexAndYaccHeader.h"

#include "solver_language_yacc.h"

#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)

void print_rule(string description, string str);

%}


%option noyywrap

Digit        ([0-9])
Integer      ({Digit}+)
HexInteger   ("0x"{Integer})
Double       ({Integer}"."{Digit}*("E"[\+\-]{Digit}+)?)
String       ("\""[^\n\"]*"\"")
Identifier   ([a-zA-Z_][a-zA-Z_0-9]*)
Operator     ([\%\/\<\>\;\!\?\*\-\+\,\.\:\[\]\(\)\{\}\=\|\&\^\$])
WhiteSpace   ([ \t\n]*)
Comment      ("//"[^\n]*)

%x C_COMMENT

%%
Solver                      {print_rule("Solver", string(yytext));
                                yylval->name = new SL::Name(string(yytext)); return solver_token;}
while                       {print_rule("while", string(yytext));
                                yylval->name = new SL::Name(string(yytext)); return while_token;}
for                       {print_rule("for", string(yytext));
                                yylval->name = new SL::Name(string(yytext)); return for_token;}
if                       {print_rule("if", string(yytext));
                                yylval->name = new SL::Name(string(yytext)); return if_token;}
return                       {print_rule("return", string(yytext)); return return_token;}
==                          {print_rule("return", string(yytext)); return op_eq;}
\+\+                          {print_rule("++", string(yytext)); return op_plus_plus;}

{Identifier}                {print_rule("Identifier", string(yytext));
                                yylval->name = new SL::Name(string(yytext)); return identifier;}
{String}                    {print_rule("String", string(yytext));
                                string ret = string(yytext);
                                ret = ret.substr(1, ret.size()-2);
                                yylval->var_val = new SL::VarVal(ret);
                                return var_val_rule;}
{Integer}                   {print_rule("Integer", string(yytext));
                                yylval->var_val = new SL::VarVal(atoi(yytext)); return var_val_rule;}
{Comment}                   {print_rule("Single Line Comment", string(yytext));}
"/*"            {print_rule("Begin C_COMMENT", string(yytext)); BEGIN(C_COMMENT); }
<C_COMMENT>"*/" {print_rule("END C_COMMENT", string(yytext));  BEGIN(INITIAL); }
<C_COMMENT>.    {print_rule("READ C_COMMENT", string(yytext)); }
{WhiteSpace}                {print_rule("WhiteSpace", string(yytext));}
{Operator}                  {print_rule("Operator", string(yytext));
                                assert(string(yytext).size() == 1); return yytext[0];}
.                           {print_rule("NotRecognized", string(yytext)); assert(false);}
%%

void print_rule(string description, string str)
{
    cout << "RULE " << description <<" TOKEN " << str << endl;
}

void yyerror(yyscan_t scanner, SolverProgramState* state, string s)
{
    cout << "ERROR " << s << endl;
}
