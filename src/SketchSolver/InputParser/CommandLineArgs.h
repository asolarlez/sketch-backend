#ifndef COMMANDLINEARGS_H
#define COMMANDLINEARGS_H

#include <stdlib.h> // atoi
#include "SATSolver.h"
#include <vector>
#include <string>
#include "string.h"
#include <ctime>

using std::cout;
using std::endl;

namespace INp{
extern  int NCTRLS;
extern  bool overrideNCtrls;
extern  bool overrideInputs;
}


class CommandLineArgs;
extern CommandLineArgs* PARAMS;

const string VERSION_INFO = "vlarrays";

struct CommandLineArgs{
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
  bool showControls;
  bool showDAG;
  bool outputMRDAG;
  bool minvarHole;
  string mrdagfile;
  bool ufunSymmetry;
  string inputFname;
  string outputFname;
  bool alterARRACS;
  bool interactive;
  int olevel;
  bool assumebcheck;
  bool angelic_model;
  int NINPUTS;
  int NANGELICS;
  int angelic_arrsz;
  bool simulate;
  int simiters;
  int simstopsize;
  string simplifycex;
  bool setMemo;
  bool debug;
  bool superChecks;  
  int randBnd;
  bool lightVerif;
  bool outputSat;
  int boundedCount;
  bool randomassign;
  typedef enum {CALLSITE, CALLNAME} BoundMode;
  BoundMode boundmode;
	CommandLineArgs(vector<string> args) {
		char** argv = (char**)malloc(sizeof(char*) * args.size());
		for(int i = 0; i < args.size(); i++) {
			argv[i] = (char*)malloc(sizeof(char) * args[i].length());
			argv[i] = (char*)args[i].c_str();
		}
		int argc = args.size();
		initializeCommandLineArgs(argc, argv);
	}

	CommandLineArgs(int argc, char** argv){
		initializeCommandLineArgs(argc, argv);
	}	

	void initializeCommandLineArgs(int argc, char** argv) {
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
		inlineAmnt = 3;
		hastimeout = false;
		timeout = -1;
		mergeFunctions = false;
		verbosity = 0;
		showInputs = false;
		showControls = false;
		showDAG = false;
		outputMRDAG = false;
		ufunSymmetry = false;
		alterARRACS = false;
		interactive = false;
		assumebcheck = false;
		angelic_model = false;
		olevel = 6;
		NINPUTS = 5;
		NANGELICS = -1;
		angelic_arrsz = -1;
		simulate = true;
		simiters = 4;
		simstopsize = 2000;
		simplifycex = "RECSYM";
		setMemo = true;
		debug = false;
		superChecks = false;
		randBnd = -1;
		lightVerif = false;
		minvarHole = false;
		outputSat = false;
		boundmode = CALLNAME;
		boundedCount = 80;
		randomassign =false;
	  for(int ii=0; ii<argc; ++ii){
        if (string(argv[ii]) == "--print-version") {
            cout << "CEGIS version features: " << VERSION_INFO << endl;
            input_idx = ii+1;
			continue;
        }
		if( string(argv[ii]) == "-debug" ){	      
	      debug = true;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-randassign" ){	      
	      randomassign = true;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-outputSat" ){	      
	      outputSat = true;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-superChecks" ){	      
	      superChecks = true;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-lightverif" ){	      
	      lightVerif = true;
	      input_idx = ii+1;
		  continue;
	    }
	    if( string(argv[ii]) == "-nosim" ){	      
	      simulate = false;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-nomemo" ){	      
	      setMemo = false;
	      input_idx = ii+1;
		  continue;
	    }
		if( string(argv[ii]) == "-simiters" ){ 
	      Assert(ii<(argc-1), "-simiters needs an extra parameter");
	      simiters = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }

		if( string(argv[ii]) == "-boundedcount" ){
	      Assert(ii<(argc-1), "-boundedcount needs an extra parameter");
	      boundedCount = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }

		if( string(argv[ii]) == "-simstopsize" ){
	      Assert(ii<(argc-1), "-simstopsize needs an extra parameter");
	      simstopsize = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }

		if( string(argv[ii]) == "--bndwrand" ){	      
		  Assert(ii<(argc-1), "--bndwrand needs an extra parameter");
	      randBnd = atoi(argv[ii+1]);
		  cout<<"BND W RAND = "<<randBnd<<endl;
	      input_idx = ii+2;      
		  continue;
	    }

		if( string(argv[ii]) == "--boundmode" ){
	      Assert(ii<(argc-1), "-synth needs an extra parameter");
	      string bm  = argv[ii+1];	  
		  cout<<"boundmode = "<<bm<<endl;
		  Assert(bm == "CALLSITE" || bm == "CALLNAME" , 
			  "The argument to boundmode should be one of \n CALLSITE = bound by callsite \n CALLNAME = bound by call name ");
	      if(bm == "CALLSITE"){
			  boundmode = CALLSITE;
		  }else{
			  boundmode = CALLNAME;
		  }
		  input_idx = ii+2;
		  continue;
	    }

		if( string(argv[ii]) == "-simplifycex" ){
	      Assert(ii<(argc-1), "-synth needs an extra parameter");
	      simplifycex = argv[ii+1];	  
		  cout<<"simplifycex = "<<simplifycex<<endl;
		  Assert(simplifycex == "NOSIM" || simplifycex == "SIMSIM" || simplifycex=="RECSIM", 
			  "The argument to simplifycex should be one of \n NOSIM = no simplify \n SIMSIM = simple simplify \n RECSIM = recursive simplify ");
	      input_idx = ii+2;
		  continue;
	    }

	    if( string(argv[ii]) == "-seedsize" ){
	      Assert(ii<(argc-1), "-seedsize needs an extra parameter");
	      seedsize = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }
		if( string(argv[ii]) == "-alterARRACS" ){
	    	alterARRACS = true;
	      input_idx = ii+1;      
		  continue;
	    }
		if( string(argv[ii]) == "--assumebcheck" ){
	    	assumebcheck = true;
			cout << "assuming  bounds checks" << endl;
	      input_idx = ii+1;      
		  continue;
	    }
		if( string(argv[ii]) == "-angelic-model" ){
	    	angelic_model = true;
			cout << "assuming  angelic ufun value in the model" << endl;
	      input_idx = ii+1;      
		  continue;
	    }
		if( string(argv[ii]) == "--olevel" ){
	      Assert(ii<(argc-1), "--olevel needs an extra parameter");
	      olevel = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }

	    
		 if( string(argv[ii]) == "--bnd-inline-amnt" ){
	      Assert(ii<(argc-1), "--bnd-inline-amnt needs an extra parameter");
	      inlineAmnt = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }
	    
	    if( string(argv[ii]) == "-output2QBF" ){	    	
	    	output2QBF = true;
			input_idx = ii+1;
			continue;
	    }
	    
	    if( string(argv[ii]) == "-outputAIG" ){
	    	outputAIG = true;
	      input_idx = ii+1;      
		  continue;
	    }
	    
		if( string(argv[ii]) == "-ufunSymmetry" ){
	    	ufunSymmetry = true;
			input_idx = ii+1;      
			continue;
	    }

		if( string(argv[ii]) == "--print-cex" ){
	    	showInputs = true;
	      input_idx = ii+1;      
		  continue;
	    }
		if( string(argv[ii]) == "-showctrls" ){
	    	showControls = true;
	      input_idx = ii+1;      
		  continue;
	    }
		if( string(argv[ii]) == "-interactive" ){
	    	interactive = true;
	      input_idx = ii+1;      
		  continue;
	    }
	    
	    if( string(argv[ii]) == "-printDiagnostics" ){
	    	printDiag = true;
	      input_idx = ii+1;      
		  continue;
	    }
	    
	    if( string(argv[ii]) == "-outputEuclid" ){
	    	outputEuclid = true;
	      input_idx = ii+1;      
		  continue;
	    }

		if( string(argv[ii]) == "-mergeFunctions" ){
	    	mergeFunctions = true;
	      input_idx = ii+1;      
		  continue;
	    }

		if( string(argv[ii]) == "-showDAG" ){
	    	showDAG = true;
	      input_idx = ii+1;      
		  continue;
	    }

		if( string(argv[ii]) == "-writeDAG" ){
	    	outputMRDAG = true;
			mrdagfile = argv[ii+1];
	      input_idx = ii+2;      
		  continue;
	    }
		
		if( string(argv[ii]) == "--minvarHole" ){
	    	minvarHole = true;
	      input_idx = ii+1;      
		  continue;
	    }

	    if( string(argv[ii]) == "--bnd-cbits" ){
	      Assert(ii<(argc-1), "--bnd-cbits needs an extra parameter");
	      INp::NCTRLS= atoi(argv[ii+1]);
		  INp::overrideNCtrls=true;
		  cout<<"Overriding controls with "<<INp::NCTRLS<<endl;
	      input_idx = ii+2;      
		  continue;
	    }
	    if( string(argv[ii]) == "--bnd-inbits" ){
	      Assert(ii<(argc-1), "--bnd-inbits needs an extra parameter");
	      NINPUTS= atoi(argv[ii+1]);
		  INp::overrideInputs=true;
		  cout<<"Overriding inputs with "<<NINPUTS<<endl;
	      input_idx = ii+2;      
		  continue;
	    }
	    if( string(argv[ii]) == "--bnd-angelicbits" ){
	      Assert(ii<(argc-1), "--bnd-angelicbits needs an extra parameter");
	      NANGELICS = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }
	    if( string(argv[ii]) == "--bnd-angelic-arrsz" ){
	      Assert(ii<(argc-1), "--bnd-angelic-arrsz needs an extra parameter");
	      angelic_arrsz = atoi(argv[ii+1]);
	      input_idx = ii+2;      
		  continue;
	    }
	    if( string(argv[ii]) == "-checkpoint" ){
	      Assert(ii<(argc-1), "-checkpoint needs an extra parameter");
	      hasCpt = true;
		  cptfile = argv[ii+1]; 
	      input_idx = ii+2;      
		  continue;
	    }
	    if( string(argv[ii]) == "-restore" ){
	      Assert(ii<(argc-1), "-restore needs an extra parameter");
	      hasRestore = true;
		  restorefile = argv[ii+1];
	      input_idx = ii+2;
		  continue;
	    }

		if( string(argv[ii]) == "-timeout" ){
	      Assert(ii<(argc-1), "-timeout needs an extra parameter");
	      timeout = atoi(argv[ii+1]);	  
		  hastimeout=true;
	      input_idx = ii+2;
		  continue;
	    } 


		if( string(argv[ii]) == "--verbosity" ){
	      Assert(ii<(argc-1), "--verbosity needs an extra parameter");
	      verbosity = atoi(argv[ii+1]);	  
		  hastimeout=true;
	      input_idx = ii+2;
		  continue;
	    } 




	    if( string(argv[ii]) == "--seed" ){
	      Assert(ii<(argc-1), "--seed needs an extra parameter");
	      seed = atoi(argv[ii+1]);	  
	      input_idx = ii+2;
		  continue;
	    } 
	    if( string(argv[ii]) == "-terminateafter" ){
	      Assert(ii<(argc-1), "-terminateafter needs an extra parameter");
	      terminateafter = atoi(argv[ii+1]);	  
	      input_idx = ii+2;
		  continue;
	    } 
	    if ( (string(argv[ii]) == "-o") || (string(argv[ii]) == "--output") ) {
	      Assert(ii<(argc-1), "-o / --output needs an extra parameter");
          //string tmp(argv[ii+1]);
          outputFname = argv[ii+1];
	      input_idx = ii+2;
		  continue;
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
		  continue;
	    }
	    if (string (argv[ii]) == "-bvectarith") {
		doBvectArith = true;
		input_idx = ii + 1;
		continue;
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
		  continue;
	    }        
		if(argv[ii][0] == '-'){
			cout<<"Unknown flag "<<string(argv[ii])<<endl;
			input_idx = ii+1;
		}
	  }
	  if (NANGELICS<NINPUTS) { NANGELICS=NINPUTS; }
	  if (angelic_arrsz<=0) { angelic_arrsz=(1<<NANGELICS); }
	  Assert( input_idx < argc, "No input file specified");
	  inputFname = argv[input_idx];
	  // outputFname = (argc>input_idx+1)?argv[input_idx+1]:"/dev/null";
	  if(verbosity > 4){
			printDiag = true;
	  }
	  if(seed < 0){
			seed = time(NULL);
	  }
	  srand(seed);

	  if(verbosity>=0){
		cout<<"SOLVER RAND SEED = "<<seed<<endl;
	  }
	}
	
	void setPARAMS() {
		PARAMS = this;
	}
	
};



#endif

