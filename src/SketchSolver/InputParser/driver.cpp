#include "driver.h"
#include "SFIOutputSeq.h"
#include "DagOptimizeCommutAssoc.h"

string context;

Driver::Driver(CommandLineArgs& p_params):params(p_params){
	

}

int Driver::resolveSketches(){
	try{

		context = " ";
		{
		  string fname = procFname(params.inputFname);			  
		  string outname =  params.outputFname;
		  ofstream out(outname.c_str());

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
	      	
  			SATSolver* finder;
			finder = SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName);
			SATSolver* checker = NULL;      	
			checker = SATSolver::solverCreate(params.veriftype, SATSolver::CHECKER, checkName);

			if(typeid(*finder) == typeid(ABCSATSolver)){
				if( params.outputAIG){
 					dynamic_cast<ABCSATSolver*>(finder)->setOutputAIG();	
 				}
				if(params.hastimeout){
					dynamic_cast<ABCSATSolver*>(finder)->setTimeout(params.timeout);
				}
			}

			if(typeid(*checker) == typeid(ABCSATSolver)){
				if( params.outputAIG){
 					dynamic_cast<ABCSATSolver*>(checker)->setOutputAIG();	
 				}
				if(params.hastimeout){
					dynamic_cast<ABCSATSolver*>(checker)->setTimeout(params.timeout);
				}
			}

	      	int rv = solveSketch(out, INp::functionMap[it->second], it->first,INp::functionMap, finder, checker, sketchName);
			if(rv != 0){ return rv; }
		  }

		}
		ABCSolverEnd();
		return 0;
	}catch(BasicError& be){
	  cerr<<"There was an error solving the problem. "<<endl<<"Exiting compiler"<<endl;
	  exit(1);
	}

}


int Driver::solveSketch(ostream& out, BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, SATSolver* finder, SATSolver* checker, string& name){


	BooleanDAG* miter = prepareMiter(spec, sketch, funMap, name);


	SolveFromInput solver(out, miter, *finder, *checker, params.seedsize, INp::NINPUTS);
  						   
	if(params.printDiag){
		cout<<" Printing Diagnostics "<<endl;
		solver.activatePrintDiag();	
	}
  	
	if(params.outputEuclid){      		
		ofstream fout("bench.ucl");
		solver.outputEuclid(fout);
	}
  	
	if(params.output2QBF){
		solver.setup2QBF();
		string fname = name;
		fname += "_2qbf.blif";
		cout<<" OUTPUTING 2QBF problem to file "<<fname<<endl;
		dynamic_cast<ABCSATSolver*>(checker)->completeProblemSetup();
		dynamic_cast<ABCSATSolver*>(checker)->outputToFile(fname);
	}
  	
  	
	if( params.terminateafter > 0 ){ solver.setIterLimit( params.terminateafter ); }
	if( params.hasCpt ){ 
		string fname = params.cptfile;
		fname += "_";
		fname += name;
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
			fname += name;
			cout<<"restoring from "<<fname<<endl;
			ifstream input(fname.c_str());
			solveCode = solver.solveFromCheckpoint(input);
		}
	}catch(SolverException* ex){
		cout<<"ERROR "<<name<<": "<<ex->code<<"  "<<ex->msg<<endl;
		ABCSolverEnd();
		return ex->code + 2;
	}catch(BasicError& be){
		cout<<"ERROR: "<<name<<endl;
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
	return 0;
}


BooleanDAG* Driver::prepareMiter(BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, string& name ){
	if(params.verbosity > 2){
		cout<<"* before  EVERYTHING: SPEC nodes = "<<spec->size()<<"\t SKETCH nodes = "<<sketch->size()<<endl;
	}

	for(map<string, BooleanDAG*>::iterator it =  funMap.begin();
				it != funMap.end(); ++it){
		int sz1 = it->second->size(); 				
		DagOptim cse(*it->second);
		cse.process(*it->second);	
		int sz2 = it->second->size(); 		
		if(params.verbosity > 3){
		cout<<" optimizing "<<	it->first <<" went from size "<<sz1<<" to "<<sz2<<endl;
		}
	}
	{
		int sz1 = sketch->size();
		DagOptim cse(*sketch);
		cse.process(*sketch);
		int sz2 = sketch->size();
		if(params.verbosity > 3){
		cout<<" optimizing "<<	name <<" went from size "<<sz1<<" to "<<sz2<<endl;
		}
	}

	{
		Dout( cout<<"BEFORE Matching input names"<<endl );
		vector<bool_node*>& specIn = spec->getNodesByType(bool_node::SRC);
		vector<bool_node*>& sketchIn = sketch->getNodesByType(bool_node::SRC);
		Assert(specIn.size() <= sketchIn.size(), "The number of inputs in the spec and sketch must match");	
		for(int i=0; i<specIn.size(); ++i){
			Dout( cout<<"Matching inputs spec: "<<sketchIn[i]->name<<" with sketch: "<<specIn[i]->name<<endl );
			sketch->rename(sketchIn[i]->name, specIn[i]->name);
		}
	}

	{
		Dout( cout<<"BEFORE Matching output names"<<endl );
		vector<bool_node*>& specDST = spec->getNodesByType(bool_node::DST);
		vector<bool_node*>& sketchDST = sketch->getNodesByType(bool_node::DST);
		Assert(specDST.size() == sketchDST.size(), "The number of inputs in the spec and sketch must match");	
		for(int i=0; i<sketchDST.size(); ++i){
			sketch->rename(sketchDST[i]->name, specDST[i]->name);			
		}
	}

	{
		if(params.verbosity > 3){ cout<<" Inlining amount = "<<params.inlineAmnt<<endl; }
		{
			if(params.verbosity > 3){ cout<<" Inlining functions in the sketch."<<endl; }
			DagFunctionInliner cse(*sketch, funMap, params.inlineAmnt, params.mergeFunctions );	
			cse.process(*sketch);
		}
		{
			if(params.verbosity > 3){ cout<<" Inlining functions in the spec."<<endl; }
			DagFunctionInliner cse(*spec, funMap,  params.inlineAmnt, params.mergeFunctions  );	
			cse.process(*spec);
		}
		
	}

	{
		DagElimUFUN eufun;	
		eufun.process(*spec);
		eufun.stopProducingFuns();
		eufun.process(*sketch);
	}

	
	spec->makeMiter(*sketch);
	BooleanDAG* problem = spec;

	if(params.verbosity > 2){ cout<<"after Creating Miter: Problem nodes = "<<problem->size()<<endl; }
	
	{
		DagOptim cse(*problem);	
		//cse.alterARRACS();
		cse.process(*problem);
	}
	if(params.verbosity > 3){cout<<"* after OPTIM: Problem nodes = "<<problem->size()<<endl;	}

	{
		DagOptimizeCommutAssoc opt;
		//opt.process(*problem);
	}

	//problem->print(cout) ;

	// cout<<"* after CAoptim: Problem nodes = "<<problem->size()<<endl;

	{
		DagOptim cse(*problem);	
		//cse.alterARRACS();
		cse.process(*problem);
	}
	
	if(params.verbosity > 0){ cout<<"* Final Problem size: Problem nodes = "<<problem->size()<<endl;	}
	if(params.showDAG){ 
		problem->print(cout);
	}

	return problem;
}


void Driver::parseInput(){
	try{
		cout<<"Reading SKETCH Program in File "<<params.inputFname<<endl;

		INp::yyin = fopen(params.inputFname.c_str(), "r");
		INp::Inityylex();
		INp::Inityyparse();

		
		if (INp::yyparse() != 0) {
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

	BooleanDAG* miter = prepareMiter(spec, sketch, funMap, name);

	BooleanDAG* rest = NULL;
	for(map<string, BooleanDAG*>::iterator it = funMap.begin(); it != funMap.end(); ++it){
		int rpos = it->first.find("rest");
		if( rpos != -1){
			rest = it->second;
		}
	}


	SFIOutputSeq solver(out, miter, *finder, *checker, params.seedsize, INp::NINPUTS, rest, skOri);
  						   
	if(params.printDiag){
		solver.activatePrintDiag();	
	}
  	
	if(params.outputEuclid){      		
		ofstream fout("bench.ucl");
		solver.outputEuclid(fout);
	}
  	
	if(params.output2QBF){
		solver.setup2QBF();
		string fname = name;
		fname += "_2qbf.blif";
		cout<<" OUTPUTING 2QBF problem to file "<<fname<<endl;
		dynamic_cast<ABCSATSolver*>(checker)->completeProblemSetup();
		dynamic_cast<ABCSATSolver*>(checker)->outputToFile(fname);
	}
  	
  	
	if( params.terminateafter > 0 ){ solver.setIterLimit( params.terminateafter ); }
	if( params.hasCpt ){ 
		string fname = params.cptfile;
		fname += "_";
		fname += name;
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
			fname += name;
			cout<<"restoring from "<<fname<<endl;
			ifstream input(fname.c_str());
			solveCode = solver.solveFromCheckpoint(input);
		}
	}catch(SolverException* ex){
		cout<<"ERROR "<<name<<": "<<ex->code<<"  "<<ex->msg<<endl;
		ABCSolverEnd();
		return ex->code + 2;
	}catch(BasicError& be){
		cout<<"ERROR: "<<name<<endl;
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
	return 0;
}

