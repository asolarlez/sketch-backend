#include "BasicError.h"
#include "BooleanDAG.h"

#include <fstream>
#include <ctime>

using std::ofstream;

namespace INp{
extern  map<string, BooleanDAG*> functionMap;
}

string context;

int main(int argc, char** argv){
  int input_idx = 1;
  try{

    Assert( argc > 1, "The input file name must be passed as an argument");
  
    cout<<"Reading Streamit Program in File "<<argv[input_idx]<<endl;

    INp::yyin = fopen(argv[input_idx], "r");
    INp::Inityylex();
    INp::Inityyparse();

    try{
      if (INp::yyparse() != 0) {
		    cerr<<"\n*** Rejected\n";
		    exit(1);
      }
    }catch(BasicError& be){
      cerr<<"There was an error parsing the input"<<endl<<"Exiting compiler"<<endl;
      exit(1);
    }

  	context = " ";
    {
      string fname(argv[input_idx]);
      int x1 = fname.find_last_of("/");
      int x2 = fname.find_last_of("\\");
      int x3 = fname.find_last_of(".");
  
      x1 = x1>x2? x1: x2;
      x3 = x3 > 0? x3 : fname.size();
      ++x1;
      fname = fname.substr(x1, x3-x1);
      string msg = "There is no filter ";
      msg += fname;
      msg += " in file ";
      msg += argv[input_idx];

      Assert( INp::functionMap.find(fname) != INp::functionMap.end(),  msg );
    }
	return 0;
}

