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
extern  int NINPUTS;
extern  bool overrideInputs;
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
  bool output2QBF; 
  int terminateafter;
  bool hasCpt;
  string cptfile;
  bool hasRestore;
  string restorefile;
  bool outputEuclid;
    bool doBvectArith;
  
	paramInterp(int argc, char** argv){
		input_idx = 1;
		seedsize = 1;
		seed = -1;
		synthtype=MINI;
		veriftype=MINI;
		outputAIG =false;
		output2QBF = false;
		outputEuclid = false;
		terminateafter = -1;
		hasCpt = false;
		hasRestore = false;
	doBvectArith = false;
		
	  for(int ii=0; ii<argc; ++ii){
	    if( string(argv[ii]) == "-seedsize" ){
	      Assert(ii<(argc-1), "-seedsize needs an extra parameter");
	      seedsize = atoi(argv[ii+1]);
	      input_idx = ii+2;      
	    }
	    
	    
	    if( string(argv[ii]) == "-output2QBF" ){
	    	outputAIG = true;
	    	output2QBF = true;
	    	veriftype = ABC;
	      input_idx = ii+1;      
	    }
	    
	    if( string(argv[ii]) == "-outputAIG" ){
	    	outputAIG = true;
	      input_idx = ii+1;      
	    }
	    if( string(argv[ii]) == "-outputEuclid" ){
	    	outputEuclid = true;
	      input_idx = ii+1;      
	    }
	    if( string(argv[ii]) == "-overrideCtrls" ){
	      Assert(ii<(argc-1), "-overrideCtrls needs an extra parameter");
	      INp::NCTRLS= atoi(argv[ii+1]);
		  INp::overrideNCtrls=true;
		  cout<<"Overriding controls with "<<INp::NCTRLS<<endl;
	      input_idx = ii+2;      
	    }
	    if( string(argv[ii]) == "-overrideInputs" ){
	      Assert(ii<(argc-1), "-overrideInputs needs an extra parameter");
	      INp::NINPUTS= atoi(argv[ii+1]);
		  INp::overrideInputs=true;
		  cout<<"Overriding inputs with "<<INp::NINPUTS<<endl;
	      input_idx = ii+2;      
	    }
	    if( string(argv[ii]) == "-checkpoint" ){
	      Assert(ii<(argc-1), "-checkpoint needs an extra parameter");
	      hasCpt = true;
		  cptfile = argv[ii+1]; 
	      input_idx = ii+2;      
	    }
	    if( string(argv[ii]) == "-restore" ){
	      Assert(ii<(argc-1), "-restore needs an extra parameter");
	      hasRestore = true;
		  restorefile = argv[ii+1];
	      input_idx = ii+2;
	    }
	    if( string(argv[ii]) == "-seed" ){
	      Assert(ii<(argc-1), "-seed needs an extra parameter");
	      seed = atoi(argv[ii+1]);	  
	      input_idx = ii+2;
	    } 
	    if( string(argv[ii]) == "-terminateafter" ){
	      Assert(ii<(argc-1), "-terminateafter needs an extra parameter");
	      terminateafter = atoi(argv[ii+1]);	  
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
	    if (string (argv[ii]) == "-bvectarith") {
		doBvectArith = true;
		input_idx = ii + 1;
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
  
    cout<<"Reading SKETCH Program in File "<<argv[params.input_idx]<<endl;

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
		ofstream out((argc>params.input_idx+1)?argv[params.input_idx+1]:"/dev/null");
      for(map<BooleanDAG*, string>::iterator it = INp::sketches.begin(); it != INp::sketches.end(); ++it){
      	string sketchName = it->second;
      	string findName = sketchName;
      	findName += "_find";
      	string checkName = sketchName;
      	checkName += "_check";
      	
      	
      	cout<<"PROCESSING SKETCH "<<it->second<<endl;
      	if( INp::functionMap.find(it->second)== INp::functionMap.end() ){
      		cout<<"There is no function named "<<it->second<<" make sure it is not a sketch. Sketches can't be specs. "<<endl;
      		ABCSolverEnd();
	  		return 1;	 
      	}
      	Dout(INp::functionMap[it->second]->print(cout)); //spec
      	Dout(it->first->print(cout)); //sketch
      	
      	SATSolver* finder = NULL;
      	bool FINDER = false;
      	bool CHECKER = true;
      	if( params.synthtype ==  paramInterp::ABC ){
      		finder = new ABCSATSolver(findName, ABCSATSolver::FULL, FINDER);
      		cout<<" FIND = ABC"<<endl;
      		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(finder)->setOutputAIG();	
     		}
      	}else if ( params.synthtype ==  paramInterp::ABCLIGHT ){
      		finder = new ABCSATSolver(findName, ABCSATSolver::BASICSAT, FINDER);
      		cout<<" FIND = ABC LIGHT"<<endl;
      		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(finder)->setOutputAIG();
     		}
      	}else if( params.synthtype ==  paramInterp::ZCHAFF){
      		finder = new ZchaffSATSolver(findName, FINDER);
     		cout<<" FIND = ZCHAFF"<<endl;
      	}else if( params.synthtype ==  paramInterp::MINI){
      		finder = new MiniSATSolver(findName, FINDER);
     		cout<<" FIND = MINI"<<endl;
      	}
      	
      	SATSolver* checker = NULL;
      	if( params.veriftype ==  paramInterp::ABC ){
      		checker = new ABCSATSolver(checkName, ABCSATSolver::FULL, CHECKER);
     		cout<<" CHECK = ABC"<<endl;
     		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(checker)->setOutputAIG();	
     		}
      	}else if ( params.veriftype ==  paramInterp::ABCLIGHT ){
      		checker = new ABCSATSolver(checkName, ABCSATSolver::BASICSAT, CHECKER);
     		cout<<" CHECK = ABC LIGHT"<<endl;
     		if( params.outputAIG){
     			dynamic_cast<ABCSATSolver*>(checker)->setOutputAIG();	
     		}
      	}else if( params.veriftype ==  paramInterp::ZCHAFF){
      		checker = new ZchaffSATSolver(checkName, CHECKER);
     		cout<<" CHECK = ZCHAFF"<<endl;
      	}else if( params.veriftype ==  paramInterp::MINI){
      		checker = new MiniSATSolver(checkName, CHECKER);
     		cout<<" CHECK = MINI"<<endl;
      	}
      	
      	SolveFromInput solver(INp::functionMap[it->second], it->first, *finder, *checker, params.seedsize);
      	if(params.outputEuclid){      		
      		ofstream fout("bench.ucl");
      		solver.outputEuclid(fout);
      	}
      	
      	if(params.output2QBF){
      		solver.setup2QBF();
      		string fname = sketchName;
      		fname += "_2qbf.blif";
      		cout<<" OUTPUTING 2QBF problem to file "<<fname<<endl;
      		dynamic_cast<ABCSATSolver*>(checker)->completeProblemSetup();
      		dynamic_cast<ABCSATSolver*>(checker)->outputToFile(fname);
      	}
      	
      	
      	if( params.terminateafter > 0 ){ solver.setIterLimit( params.terminateafter ); }
      	if( params.hasCpt ){ 
      		string fname = params.cptfile;
      		fname += "_";
      		fname += it->second;
      		solver.setCheckpoint(fname);
      		}
      	if(params.seed >= 0){
      		cout<<"SOLVER RAND SEED = "<<params.seed<<endl;
      		solver.set_randseed(params.seed);
      	}
	  	solver.setup();
	  	int solveCode = 0;
	  	try{
	  		if(!params.hasRestore){
			  	solveCode = solver.solve();
	  		}else{
	  			
	  			string fname = params.restorefile;
	      		fname += "_";
	      		fname += it->second;
	  			cout<<"restoring from "<<fname<<endl;
	  			ifstream input(fname.c_str());
	  			solveCode = solver.solveFromCheckpoint(input);
	  		}
	  	}catch(SolverException* ex){
	  		cout<<"ERROR "<<sketchName<<": "<<ex->code<<"  "<<ex->msg<<endl;
	        ABCSolverEnd();
	  		return ex->code + 2;
	  	}catch(BasicError& be){
		    cout<<"ERROR: "<<sketchName<<endl;
		    ABCSolverEnd();
	  		return 3;
    	}
	  	if( solveCode ){
			solver.output_control_map(out);
	  	}else{
	  		cout<<"** Outputing bad controls"<<endl;
	  		solver.output_control_map(out);
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


