#include "driver.h"
#include "DagOptimizeCommutAssoc.h"
#include "BackwardsAnalysis.h"
#include "InterpreterEnvironment.h"

string context;
Driver::Driver(CommandLineArgs& p_params):params(p_params){
	

}

int Driver::resolveSketches(){
	try{
		/*
		InterpreterEnvironment env(params);
		context = " ";
		{
		  string fname = procFname(params.inputFname);			  
		  string outname =  params.outputFname;
		  ofstream out(outname.c_str());
		
		  for(map<string, BooleanDAG*>::iterator it = INp::functionMap.begin(); it != INp::functionMap.end(); ++it){
			env.addFunction(it->first, it->second);
		  }

		  for(map<BooleanDAG*, string>::iterator it = INp::sketches.begin(); it != INp::sketches.end(); ++it){	      	
	      	
  			cout<<"PROCESSING SKETCH "<<it->second<<endl;
  			if( INp::functionMap.find(it->second)== INp::functionMap.end() ){
  				cout<<"There is no function named "<<it->second<<" make sure it is not a sketch. Sketches can't be specs. "<<endl;
  				ABCSolverEnd();
  				return 1;	 
  			}
  			Dout(INp::functionMap[it->second]->print(cout)); //spec
  			Dout(it->first->print(cout)); //sketch
	      	

			int rv = env.assertDAG(env.prepareMiter(INp::functionMap[it->second]->clone(), it->first->clone()), out);
			if(rv != 0){ return rv; }
			//outname += "C";
			//ofstream pout(outname.c_str());
			//env.printControls(pout);
		  }
		  env.printControls(out);

		}
		ABCSolverEnd();
		return 0;
		*/
	}catch(BasicError& be){
	  cerr<<"There was an error solving the problem. "<<endl<<"Exiting compiler"<<endl;
	  exit(1);
	}
	return 0;
}




void Driver::parseInput(){
	try{
		cout<<"Reading SKETCH Program in File "<<params.inputFname<<endl;


		INp::Inityylex();
		INp::Inityyparse();
		INp::envt = new InterpreterEnvironment(params);
		void* scanner;
		INp::yylex_init(&scanner);
		FILE* tmp = NULL;
		if(params.interactive){
			INp::yyset_in(stdin, scanner);			
		}else{
			tmp = fopen(params.inputFname.c_str(), "r");
			INp::yyset_in(tmp, scanner);
		}
		int rv = INp::yyparse(scanner);
		if (rv != 0) {
			INp::yylex_destroy(scanner);
			cerr<<"\n*** Rejected\n";
			exit(rv);
		}
		delete INp::envt;
	}catch(BasicError& be){
		  cerr<<"Ther sketch did not resolve "<<endl<<"Exiting compiler"<<endl;
		  exit(1);
	}
}

void PyDriver::parseInput(){
	try{
		cout<<"Reading SKETCH Program in File "<<params.inputFname<<endl;


		INp::Inityylex();
		INp::Inityyparse();
		INp::envt = new InterpreterEnvironment(params);
		void* scanner;
		INp::yylex_init(&scanner);
		FILE* tmp = NULL;
		if(params.interactive){
			INp::yyset_in(stdin, scanner);			
		}else{
			tmp = fopen(params.inputFname.c_str(), "r");
			INp::yyset_in(tmp, scanner);
		}
		if (INp::yyparse(scanner) != 0) {
			INp::yylex_destroy(scanner);
			cerr<<"\n*** Rejected\n";
			exit(1);
		}
	}catch(BasicError& be){
		  cerr<<"There was an error parsing the input"<<endl<<"Exiting compiler"<<endl;
		  exit(1);
	}
}




int Driver2::solveSketch(ostream& out, BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, SATSolver* finder, SATSolver* checker, string& name){

	BooleanDAG* skOri = sketch->clone();
	Assert(false, "OBSOLETE");
	BooleanDAG* miter = NULL; /* prepareMiter(spec, sketch, funMap, name);*/

	BooleanDAG* rest = NULL;
	for(map<string, BooleanDAG*>::iterator it = funMap.begin(); it != funMap.end(); ++it){
		int rpos = it->first.find("rest");
		if( rpos != -1){
			rest = it->second;
		}
	}

	SolverHelper f(*finder);
	SolverHelper ch(*checker);
  						   
	
  	
	if(params.outputEuclid){      		
		ofstream fout("bench.ucl");
		
	}
  	
//	if(params.output2QBF){
//		solver.setup2QBF(cout);
//		string fname = name;
//		fname += "_2qbf.blif";
//		cout<<" OUTPUTING 2QBF problem to file "<<fname<<endl;
//		dynamic_cast<ABCSATSolver*>(checker)->completeProblemSetup();
//		dynamic_cast<ABCSATSolver*>(checker)->outputToFile(fname);
//	}
  	
  	
	if( params.hasCpt ){ 
		string fname = params.cptfile;
		fname += "_";
		fname += name;
		
		}
	// solver.setup();
	int solveCode = 0;
	try{
		if(!params.hasRestore){
  			
		}else{	  			
			string fname = params.restorefile;
			fname += "_";
			fname += name;
			cout<<"restoring from "<<fname<<endl;
			ifstream input(fname.c_str());
			
		}
	}catch(SolverException* ex){
		cout<<"ERROR "<<name<<": "<<ex->code<<"  "<<ex->msg<<endl;
//		ABCSolverEnd();
		return ex->code + 2;
	}catch(BasicError& be){
		cout<<"ERROR: "<<name<<endl;
//		ABCSolverEnd();
		return 3;
	}
	if( solveCode ){
		
	}else{
		cout<<"** Outputing bad controls"<<endl;
		
//		ABCSolverEnd();
		return 1;	
	}
	return 0;
}

