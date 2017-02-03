#!/usr/bin/bash

cd $1/src/SketchSolver
bison -y -dvty -oInputParser.cpp InputParser/InputParser.yy
flex -oInputLexer.cpp InputParser/InputLexer.ll
cd ../..

CPPS="";
for f in `find . -regex  ".*c[p|c]p?" | grep -v "InputLexer.cpp" | grep -v "InputParser.cpp"`;
do
CPPS=" "$f" "$CPPS;
done;
HDR=$(for f in `find . -name  "*.h" | grep -v -e ".hg\|bindings\|.vs\|tests"`; do echo -n "-I "; dirname $f; done | sort | uniq)

echo $CPPS

echo $HDR

g++ -std=c++11 $HDR -o ./cegis $CPPS -lrt -lpthread
