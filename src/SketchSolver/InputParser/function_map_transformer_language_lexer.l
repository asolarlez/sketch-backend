%option reentrant
%{

#include "FunctionMapTransformerLanguage.h"

#include "function_map_transformer_language_yacc.h"

#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)

void print_rule(string description, string str);

%}


%option noyywrap

String       ("\""[^\n\"]*"\"")
Identifier   ([a-zA-Z_][a-zA-Z_0-9]*)
Operator     ([\%\/\<\>\;\!\?\*\-\+\,\.\:\[\]\(\)\{\}\=\|\&\^\$])
WhiteSpace   ([ \t\n]*)

%%
{Identifier}                {print_rule("Identifier", string(yytext));
                                yylval->identifier_ = new SL::Identifier(string(yytext));
                                return identifier;}
{String}                    {print_rule("String", string(yytext));
                                string ret = string(yytext);
                                ret = ret.substr(1, ret.size()-2);
                                yylval->var_val = new SL::VarVal(ret);
                                return var_val_rule;}
{WhiteSpace}                {print_rule("WhiteSpace", string(yytext));}
{Operator}                  {print_rule("Operator", string(yytext));
                                assert(string(yytext).size() == 1); return yytext[0];}
.                           {print_rule("NotRecognized", string(yytext)); assert(false);}
%%

void print_rule(string description, string str)
{
    cout << "RULE " << description <<" TOKEN " << str << endl;
}