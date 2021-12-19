rm solver_language_executable
rm solver_language_lex.cpp
rm solver_language_yacc.cpp
rm solver_language_yacc.h

lex solver_language_lexer.l && mv lex.yy.c solver_language_lex.cpp
yacc -d solver_language_parser.y && mv y.tab.c solver_language_yacc.cpp && mv y.tab.h solver_language_yacc.h
g++ solver_language_lex.cpp solver_language_yacc.cpp -o solver_language_executable
./solver_language_executable < solver_language_program.txt
