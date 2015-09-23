
#!/bin/sh
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 TEST_NUMBER PATH_TO_SKETCH_BINARY" >&2
  exit 1
fi
i=$1
SKETCH=$2
mkdir -p smt
mkdir -p tmp
mkdir -p dag
mkdir -p out

$SKETCH -V 100 --fe-keep-tmp --beopt:writeSMT ./smt/test$i.smt2 --debug-output-dag ./dag/test$i.dag ./sk/test$i.sk  &> ./out/test$i.out
cp ~/.sketch/tmp/test$i.sk/input0.tmp ./tmp/test$i.tmp 
cp ~/.sketch/tmp/test$i.sk/solution0-0 ./tmp/sol$i
z3 ./smt/test$i.smt2  &> ./smt/sol$i

UNSAT=`grep -l unsat ./smt/sol$i`
if [ -n "$UNSAT" ]; then
	echo -n "z3 was UNSAT: "
fi

cat ./smt/sol$i | sed 's/!/ /g;s/)//g' | awk 'BEGIN {T=0} { if(T==1){print $1; T=0}  if($1 == "(define-fun"){ printf $2"\t"; T=1 } }' | sort -k1 | sed 's/false/0/g;s/true/1/g' > ./smt/sol$i.parsed
cat ./tmp/sol$i | sort -k1 > ./tmp/sol$i.parsed

X=`diff ./tmp/sol$i.parsed ./smt/sol$i.parsed`

if [ -z "$X" ]; then
	Y=`wc -l ./tmp/sol$i.parsed | awk '{print $1}'`
	echo "Same CTRL values produced: $Y"

else
	echo "ERROR! CTRL Values are not same"
	echo "SMT SOL:"
	cat ./smt/sol$i.parsed
	echo "SK SOL:"
	cat ./tmp/sol$i.parsed
fi

