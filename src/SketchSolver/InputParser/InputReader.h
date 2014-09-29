#ifndef InptReader_h
#define InptReader_h

#include <cstdio>

namespace INp{

extern FILE *yyin;

void Inityylex(void);
void Inityyparse(void);

// int yylex(void);

extern int yylineno;
extern char yytext[];
extern int yy_flex_debug;

typedef enum{ INT, LONG, BIT, INT_ARR, BIT_ARR, FLOAT, FLOAT_ARR, TUPLE} vartype;

/*
 * This is from y.tab.c
 */
// int yyparse(void);
#define YY_NO_UNISTD_H 1

int yyparse (void *yyscanner);
int yylex_init (void** scanner);
int yylex_destroy (void* scanner);
void yyset_in (FILE *  in_str , void* yyscanner);

void yyerror(void* yyscanner, const char* );

}

#endif 
