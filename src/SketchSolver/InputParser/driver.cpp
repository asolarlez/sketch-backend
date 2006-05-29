#include "BasicError.h"
#include "BooleanDAG.h"
#include "InputReader.h"
#include "SolveFromInput.h"
#include "ABCSATSolver.h"
#include "ZchaffSATSolver.h"
#include "MiniSATSolver.h"


#include <fstream>
#include <ctime>

using std::ofstream;


namespace INp{
extern  map<string, BooleanDAG*> functionMap;
extern  map<string, BooleanDAG*> sketchMap;
extern  map<BooleanDAG*, string> sketches;
extern  int NCTRLS;
extern  bool overrideNCtrls;
}

string context;

class paramInterp{
	public:
	
	typedef enum {ABC, ABCLIGHT, ZCHAFF, MINI} SolverType;
  int input_idx;
  int seedsize;
  int seed;
  SolverType synthtype, veriftype;
  bool outputAIG; 
  
	paramInterp(int argc, char** argv){
		input_idx = 1;
		seedsize = 1;
		seed = -1;
		synthtype=MINI;
		veriftype=MINI;
		outputAIG =false;
		
		
	  for(int ii=0; ii<argc; ++ii){
	    if( string(argv[ii]) == "-seedsize" ){
	      Assert(ii<(argc-1), "-seedsize needs an extra parameter");
	      seedsize = atoi(argv[ii+1]);
	      input_idx = ii+2;      
	    }
	    if( string(argv[ii]) == "-outputAIG" ){
	    	outputAIG = true;
	      input_idx = ii+1;      
	    }
	    if( string(argv[ii]) == "-overrideCtrls" ){
	      Assert(ii<(argc-1), "-overrideCtrls needs an extra parameter");
	      INp::NCTRLS= atoi(argv[ii+1]);
		  INp::overrideNCtrls=true;
	      input_idx = ii+2;      
	    }  
	    if( string(argv[ii]) == "-seed" ){
	      Assert(ii<(argc-1), "-seed needs an extra parameter");
	      seed = atoi(argv[ii+1]);	  
	      input_idx = ii+2;
	    } 
	    
	    if( string(argv[ii]) == "-synth" ){
	      Assert(ii<(argc-1), "-synth needs an extra parameter");
	      string synth(argv[ii+1]);	  
		  if( synth == "ABC" ) synthtype = ABC;
		  if( synth == "ABCLIGHT" ) synthtype = ABCLIGHT;
  		  if( synth == "ZCHAFF" ) synthtype = ZCHAFF;	 
		  if( synth == "MINI" ) synthtype = MINI;
		  cout<<" synth = |"<<synth<<"|"<<endl;
	      input_idx = ii+2;
	    }
	    
	    if( string(argv[ii]) == "-verif" ){
	      Assert(ii<(argc-1), "-verif needs an extra parameter");
   	      string verif(argv[ii+1]);
		  if( verif == "ABC" ) veriftype = ABC;
		  if( verif == "ABCLIGHT" ) veriftype = ABCLIGHT;
		  if( verif == "ZCHAFF" ) veriftype = ZCHAFF;	      
  		  if( verif == "MINI" ) veriftype = MINI;	      
  		  cout<<" verif = |"<<verif<<"|"<<endl;
	      input_idx = ii+2;
	    }        
	  }		
	}	
	
	
};




int main(int argc, char** argv){
  ABCSolverStart();
  
  paramInterp params(argc, argv);
  
  try{

    Assert( argc > 1, "The input file name must be passed as an argument");
  
    cout<<"Reading Streamit Program in File "<<argv[params.input_idx]<<endl;

    INp::yyin = fopen(argv[params.input_idx], "r");
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
      string fname(argv[params.input_idx]);
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
      msg += argv[params.input_idx];
      cout<<"XXXXXXXXXXXXXXXXXXXXXXX"<<endl;
      //Assert( INp::functionMap.find(fname) != INp::functionMap.end(),  msg );
		ofstream out(argv[params.input_idx+1]);
      for(map<BooleanDAG*, string>::iterator it = INp::sketches.begin(); it != INp::sketches.end(); ++it){
      	cout<<"PROCESSING SKETCH "<<it->second<<endl;
      	Dout(INp::functionMap[it->second]->print(cout)); //spec
      	Dout(it->first->print(cout)); //sketch
      	
      	SATSolver* finder = NULL;
      	if( params.synthtype ==  paramInterp::ABC ){
      		finder = new ABCSATSolver("find", ABCSATSolver::FULL);
      		cout<<" FIND = ABC"<<endl;
      		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(finder)->setOutputAIG();	
     		}
      	}else if ( params.synthtype ==  paramInterp::ABCLIGHT ){
      		finder = new ABCSATSolver("find", ABCSATSolver::BASICSAT);
      		cout<<" FIND = ABC LIGHT"<<endl;
      		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(finder)->setOutputAIG();
     		}
      	}else if( params.synthtype ==  paramInterp::ZCHAFF){
      		finder = new ZchaffSATSolver("find");
     		cout<<" FIND = ZCHAFF"<<endl;
      	}else if( params.synthtype ==  paramInterp::MINI){
      		finder = new MiniSATSolver("find");
     		cout<<" FIND = MINI"<<endl;
      	}
      	
      	SATSolver* checker = NULL;
      	if( params.veriftype ==  paramInterp::ABC ){
      		checker = new ABCSATSolver("check", ABCSATSolver::FULL);
     		cout<<" CHECK = ABC"<<endl;
     		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(checker)->setOutputAIG();	
     		}
      	}else if ( params.veriftype ==  paramInterp::ABCLIGHT ){
      		checker = new ABCSATSolver("check", ABCSATSolver::BASICSAT);
     		cout<<" CHECK = ABC LIGHT"<<endl;
     		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(checker)->setOutputAIG();	
     		}
      	}else if( params.veriftype ==  paramInterp::ZCHAFF){
      		checker = new ZchaffSATSolver("check");
     		cout<<" CHECK = ZCHAFF"<<endl;
      	}else if( params.veriftype ==  paramInterp::MINI){
      		checker = new MiniSATSolver("check");
     		cout<<" CHECK = MINI"<<endl;
      	}
      	
      	SolveFromInput solver(INp::functionMap[it->second], it->first, *finder, *checker, params.seedsize);
      	if(params.seed >= 0){
      		cout<<"SOLVER RAND SEED = "<<params.seed<<endl;
      		solver.set_randseed(params.seed);
      	}
	  	solver.setup();
	  	int solveCode = 0;
	  	try{
		  	solveCode = solver.solve();
	  	}catch(SolverException* ex){
	  		cout<<"ERROR: "<<ex->code<<"  "<<ex->msg<<endl;
	        ABCSolverEnd();
	  		return ex->code + 2;
	  	}
	  	if( solveCode ){
			solver.output_control_map(out);
	  	}else{
		    ABCSolverEnd();
	  		return 1;	
	  	}
      }

    }
    ABCSolverEnd();
	return 0;
    }catch(BasicError& be){
      cerr<<"There was an error parsing the input"<<endl<<"Exiting compiler"<<endl;
      exit(1);
    }
}


