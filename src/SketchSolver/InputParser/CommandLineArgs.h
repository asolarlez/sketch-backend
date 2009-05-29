#ifndef COMMANDLINEARGS_H
#define COMMANDLINEARGS_H

#include <stdlib.h> // atoi
#include "SATSolver.h"


namespace INp{
extern  int NCTRLS;
extern  bool overrideNCtrls;
extern  int NINPUTS;
extern  bool overrideInputs;
}

class CommandLineArgs{
	public:		
  int input_idx;
  int seedsize;
  int seed;
  SATSolver::SolverType synthtype, veriftype;
  bool outputAIG;
  bool output2QBF; 
  int terminateafter;
  bool hasCpt;
  string cptfile;
  bool hasRestore;
  string restorefile;
  bool outputEuclid;
  bool doBvectArith;
  bool printDiag;
  int inlineAmnt;
  bool mergeFunctions;
  bool hastimeout;
  int timeout;
  int verbosity;
  bool showInputs;
  bool showDAG;
  bool ufunSymmetry;
  string inputFname;
  string outputFname;
  bool alterARRACS;
  bool interactive;
  int olevel;

	CommandLineArgs(int argc, char** argv){
		input_idx = 1;
		seedsize = 1;
		seed = -1;
		synthtype=SATSolver::MINI;
		veriftype=SATSolver::MINI;
		outputAIG =false;
		output2QBF = false;
		outputEuclid = false;
		terminateafter = -1;
		hasCpt = false;
		hasRestore = false;
		printDiag = false;
	doBvectArith = false;
		inlineAmnt = 10;
		hastimeout = false;
		timeout = -1;
		mergeFunctions = false;
		verbosity = 0;
		showInputs = false;
		showDAG = false;
		ufunSymmetry = false;
		alterARRACS = false;
		interactive = false;
		olevel = 5;



	  for(int ii=0; ii<argc; ++ii){
	    if( string(argv[ii]) == "-seedsize" ){
	      Assert(ii<(argc-1), "-seedsize needs an extra parameter");
	      seedsize = atoi(argv[ii+1]);
	      input_idx = ii+2;      
	    }
		if( string(argv[ii]) == "-alterARRACS" ){
	    	alterARRACS = true;
	      input_idx = ii+1;      
	    }

		if( string(argv[ii]) == "-olevel" ){
	      Assert(ii<(argc-1), "-olevel needs an extra parameter");
	      olevel = atoi(argv[ii+1]);
	      input_idx = ii+2;      
	    }

	    
		 if( string(argv[ii]) == "-inlineamnt" ){
	      Assert(ii<(argc-1), "-inlineamnt needs an extra parameter");
	      inlineAmnt = atoi(argv[ii+1]);
	      input_idx = ii+2;      
	    }
	    
	    if( string(argv[ii]) == "-output2QBF" ){
	    	outputAIG = true;
	    	output2QBF = true;
	    	veriftype = SATSolver::ABC;
	      input_idx = ii+1;      
	    }
	    
	    if( string(argv[ii]) == "-outputAIG" ){
	    	outputAIG = true;
	      input_idx = ii+1;      
	    }
	    
		if( string(argv[ii]) == "-ufunSymmetry" ){
	    	ufunSymmetry = true;
			input_idx = ii+1;      
	    }

		if( string(argv[ii]) == "-showinputs" ){
	    	showInputs = true;
	      input_idx = ii+1;      
	    }
		
		if( string(argv[ii]) == "-interactive" ){
	    	interactive = true;
	      input_idx = ii+1;      
	    }
	    
	    if( string(argv[ii]) == "-printDiagnostics" ){
	    	printDiag = true;
	      input_idx = ii+1;      
	    }
	    
	    if( string(argv[ii]) == "-outputEuclid" ){
	    	outputEuclid = true;
	      input_idx = ii+1;      
	    }

		if( string(argv[ii]) == "-mergeFunctions" ){
	    	mergeFunctions = true;
	      input_idx = ii+1;      
	    }

		if( string(argv[ii]) == "-showDAG" ){
	    	showDAG = true;
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

		if( string(argv[ii]) == "-timeout" ){
	      Assert(ii<(argc-1), "-timeout needs an extra parameter");
	      timeout = atoi(argv[ii+1]);	  
		  hastimeout=true;
	      input_idx = ii+2;
	    } 


		if( string(argv[ii]) == "-verbosity" ){
	      Assert(ii<(argc-1), "-verbosity needs an extra parameter");
	      verbosity = atoi(argv[ii+1]);	  
		  hastimeout=true;
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
		  if( synth == "ABC" ) synthtype = SATSolver::ABC;
		  if( synth == "ABCLIGHT" ) synthtype = SATSolver::ABCLIGHT;
  		  if( synth == "ZCHAFF" ) synthtype = SATSolver::ZCHAFF;	 
		  if( synth == "MINI" ) synthtype = SATSolver::MINI;
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
		  if( verif == "ABC" ) veriftype = SATSolver::ABC;
		  if( verif == "ABCLIGHT" ) veriftype = SATSolver::ABCLIGHT;
		  if( verif == "ZCHAFF" ) veriftype = SATSolver::ZCHAFF;	      
  		  if( verif == "MINI" ) veriftype = SATSolver::MINI;	      
  		  cout<<" verif = |"<<verif<<"|"<<endl;
	      input_idx = ii+2;
	    }        
	  }
	  Assert( input_idx < argc, "No input file specified");
	  inputFname = argv[input_idx];
	  outputFname = (argc>input_idx+1)?argv[input_idx+1]:"/dev/null";
	  if(verbosity > 4){
			printDiag = true;
	  }
	}	
	
	
};




#endif

