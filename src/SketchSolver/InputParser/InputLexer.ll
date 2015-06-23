%option reentrant
%{

#include "InputParser.hpp"
#include <cstdlib>
#include "InputReader.h"

#define YY_DECL int yylex (YYSTYPE* yylval, yyscan_t yyscanner)

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

{Comment} { string tmp(yytext);
			if( tmp.find(":") < tmp.size() )
				context = string(yytext); 
			}

{WhiteSpace}	{ /* skip */ }

{Operator} {  return yytext[0]; }

{Double}   {
				yylval->doubleConst = atof(yytext);
				return T_dbl;
			}

("###NATIVE_CODE_BEGIN"[^\#]*"###NATIVE_CODE_END") { 				
				string tmp(yytext);
				tmp = tmp.substr(20, tmp.size() - ( 20 + 18 ) );
				yylval->strConst = new string(tmp);
				return T_NativeCode;
			}

{Integer}  {							
				yylval->intConst = atoi(yytext); 				
				return T_int;
			}
{HexInteger}  {	
				yylval->intConst = atoi(yytext); 
				return T_int;
			}

"null"  {							
				yylval->intConst = 0; 				
				return T_int;
			}
{String}   {								
				string tmp(yytext);
				tmp = tmp.substr(1, tmp.size() -2);
				yylval->strConst = new string(tmp);
				return T_string;
			}

"++"		{
				return T_ppls;
			}
"true"		{
				return T_true;
			}
"false"		{
				return T_false;
			}

"\$\$"		{
				return T_twoS;
			}
"{\$"		{
				return T_leftAC;
			}	
"\$}"		{
				return T_rightAC;
			}	
"{<"		{
				return T_leftTC;
			}	
">}"		{
				return T_rightTC;
			}	
"->"		{
				return T_arrow;
			}
"[|"    {
        return T_leftAR;
      }
"|]"    {
        return T_rightAR;
      }
"--"		{
				return T_mmns;
			}
"=="		{
				return T_eq;
			}
"!="		{
				return T_neq;
			}

"&&"		{
				return T_and;
			}

"||"		{
				return T_or;
			}


">="		{
				return T_ge;
			}

"<="		{
				return T_le;
			}

("OUTPUT_"{Integer}) { 	
						yylval->strConst = new string(yytext); 
						return T_OutIdent; }

"int"	{
				yylval->variableType = INT;
				return T_vartype;
		}
		
"bit"	{
				yylval->variableType = BIT;
				return T_vartype;
		}

"float"	{
		yylval->variableType = FLOAT;
		return T_vartype;
}

"int_arr"	{
				yylval->variableType = INT_ARR;
				return T_vartype;
		}
		
"bit_arr"	{
				yylval->variableType = BIT_ARR;
				return T_vartype;
		}

"float_arr" {
				yylval->variableType = FLOAT_ARR;
				return T_vartype;
		}


"for"		{
				
				return T_For;
			}	

"new"		{				
				return T_new;
			}

"assert" {
    return T_assert;
}

"assume" {
    return T_assume;
}

"hassert" {
    return T_hassert;
}


"NativeFilter"	{				
					return T_Native;
				}
"NATIVE_METHOD" { 				
					return T_NativeMethod;
				}

			
"SKETCHES"	{
				return T_Sketches;
			}

"EQUALS" {
        return T_equals;
      }

"replace" {
        return T_replace;
      }
						

"def"		{
				return T_def;
			}

"mdl_def"	{
				return T_mdldef;
			}

"MINVAR"	{
				return T_Min;
			}
"SPVAR" {
        return T_sp;
      }
"typedef" {
                return T_Typedef;
            }


{Identifier} {
	yylval->strConst = new string(yytext);
	return T_ident;
}

<<EOF>> {
			cout<<"End of File"<<endl;
			return T_eof;
		}

%%

void Inityylex(void){
	//printf("Initializing scanner\n");
}

