#include "InterpreterEnvironment.h"
#include "InputReader.h"
#include "CallGraphAnalysis.h"
#include "ComplexInliner.h"
#include "DagFunctionToAssertion.h"

InterpreterEnvironment::~InterpreterEnvironment(void)
{
}

int InterpreterEnvironment::runCommand(const string& cmd, list<string*>& parlist){
	if(cmd == "exit"){
		return 0;
	}
	if(cmd == "print"){
		if(parlist.size() > 0){
			printControls(*parlist.front());
			for(list<string*>::iterator it = parlist.begin(); it != parlist.end(); ++it){
				delete *it;
			}
		}else{
			printControls("");
		}
		return -1;
	}
	
	if(cmd == "import"){
					
		string& fname = 	*parlist.front();
		cout<<"Reading SKETCH Program in File "<<fname<<endl;
		
		
		void* scanner;
		INp::yylex_init(&scanner);
		INp::yyset_in(fopen(fname.c_str(), "r"), scanner);			
		int tmp = INp::yyparse(scanner);
		INp::yylex_destroy(scanner);
		cout<<"DONE INPUTING"<<endl;
		for(list<string*>::iterator it = parlist.begin(); it != parlist.end(); ++it){
			delete *it;
		}
		if(tmp != 0) return tmp;
		return -1;			
	}
	
	Assert(false, "NO SUCH COMMAND"<<cmd);
	return 1;
}

BooleanDAG* InterpreterEnvironment::prepareMiter(BooleanDAG* spec, BooleanDAG* sketch){
	if(params.verbosity > 2){
		cout<<"* before  EVERYTHING: SPEC nodes = "<<spec->size()<<"\t SKETCH nodes = "<<sketch->size()<<endl;
	}

	if(params.verbosity > 2){
		cout<<" INBITS = "<<INp::NINPUTS<<endl;
		cout<<" CBITS  = "<<INp::NCTRLS<<endl;
	}

	{
		Dout( cout<<"BEFORE Matching input names"<<endl );
		vector<bool_node*>& specIn = spec->getNodesByType(bool_node::SRC);
		vector<bool_node*>& sketchIn = sketch->getNodesByType(bool_node::SRC);

		int inints = 0;
		int inbits = 0;

		Assert(specIn.size() <= sketchIn.size(), "The number of inputs in the spec and sketch must match");	
		for(int i=0; i<specIn.size(); ++i){
			Dout( cout<<"Matching inputs spec: "<<sketchIn[i]->name<<" with sketch: "<<specIn[i]->name<<endl );
			sketch->rename(sketchIn[i]->name, specIn[i]->name);
			if(sketchIn[i]->getOtype() == bool_node::BOOL){
				inbits++;
			}else{
				inints++;
			}
		}

		if(params.verbosity > 2){
			cout<<" input_ints = "<<inints<<" \t input_bits = "<<inbits<<endl;
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

	
	
	//spec->repOK();
	//sketch->repOK();

	if(params.verbosity > 1){
		cout<<" optimization level = "<<params.olevel<<endl;
	}

	if(false){
		CallGraphAnalysis cga;
		cout<<"sketch:"<<endl;
		cga.process(*sketch, functionMap);
		cout<<"spec:"<<endl;
		cga.process(*spec, functionMap);
	}

	if(params.olevel >= 3){
		if(params.verbosity > 3){ cout<<" Inlining amount = "<<params.inlineAmnt<<endl; }
		{
			if(params.verbosity > 3){ cout<<" Inlining functions in the sketch."<<endl; }
			doInline(*sketch, functionMap, params.inlineAmnt);
			/*
			ComplexInliner cse(*sketch, functionMap, params.inlineAmnt, params.mergeFunctions );	
			cse.process(*sketch);
			*/
		}
		{
			if(params.verbosity > 3){ cout<<" Inlining functions in the spec."<<endl; }
			doInline(*spec, functionMap, params.inlineAmnt);
			/*
			ComplexInliner cse(*spec, functionMap,  params.inlineAmnt, params.mergeFunctions  );	
			cse.process(*spec);
			*/
		}
		
	}

	
	//spec->repOK();
	//sketch->repOK();
		

	{
		DagElimUFUN eufun;	
		eufun.process(*spec);
		if(params.ufunSymmetry){ eufun.stopProducingFuns(); }
		eufun.process(*sketch);
	}
	//At this point spec and sketch may be inconsistent, because some nodes in spec will have nodes in sketch as their children.
	spec->makeMiter(sketch);
	BooleanDAG* result = spec;
	


	if(params.verbosity > 2){ cout<<"after Creating Miter: Problem nodes = "<<result->size()<<endl; }
	


	return runOptims(result);
}


void InterpreterEnvironment::doInline(BooleanDAG& dag, map<string, BooleanDAG*> functionMap, int steps){	
	OneCallPerCSiteInliner fin;
	DagFunctionInliner dfi(dag, functionMap, &fin);	
	int oldSize = -1;
	for(int i=0; i<steps; ++i){
		do{
			dfi.process(dag);
			set<string>& dones = dfi.getFunsInlined();
			cout<<"inlined "<<dfi.nfuns()<<" new size ="<<dag.size()<<endl;			
			if(oldSize > 0){
				if(dag.size() > 10000 && dag.size() > oldSize * 10){
					i=steps;
					break;
				}				
			}
			oldSize = dag.size();
		}while(dfi.changed());
		// fin.ctt.printCtree(cout, dag);
		fin.clear();
	}
	{		
		DagFunctionToAssertion makeAssert(dag, functionMap);
		makeAssert.process(dag);
	}
}

int InterpreterEnvironment::assertDAG(BooleanDAG* dag, ostream& out){
	Assert(status==READY, "You can't do this if you are UNSAT");
	++assertionStep;
	BooleanDAG* problem;
	if(bgproblem == NULL){
		problem = dag;
		bgproblem = dag;
	}else{
		if(bgproblem->getNodesByType(bool_node::SRC).size() == 0){
			cout<<"No inputs"<<endl;
			problem = dag;
			bgproblem->andDag(dag->clone());			
			//bgproblem = runOptims(bgproblem);
		}else{
			bgproblem->andDag(dag);
			bgproblem = runOptims(bgproblem);
			problem = bgproblem;
		}
	}

	SATSolver* checker = SATSolver::solverCreate(params.veriftype, SATSolver::CHECKER, checkName());
	SolverHelper check(*checker);
	CEGISSolver solver(problem, *finder, check, params.seedsize, INp::NINPUTS);
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
		string fname = basename();
		fname += "_2qbf.blif";
		cout<<" OUTPUTING 2QBF problem to file "<<fname<<endl;
		dynamic_cast<ABCSATSolver*>(checker)->completeProblemSetup();
		dynamic_cast<ABCSATSolver*>(checker)->outputToFile(fname);
	}
  	
  	
	if( params.terminateafter > 0 ){ solver.setIterLimit( params.terminateafter ); }
	if( params.hasCpt ){ 
		string fname = params.cptfile;
		fname += "_";
		fname += basename();
		solver.setCheckpoint(fname);
		}
	if(params.seed >= 0){
		cout<<"SOLVER RAND SEED = "<<params.seed<<endl;
		solver.set_randseed(params.seed);
	}

	int solveCode = 0;
	try{
		if(!params.hasRestore){
			solveCode = solver.solve();
		}else{	  			
			string fname = params.restorefile;
			fname += "_";
			fname += basename();
			cout<<"restoring from "<<fname<<endl;
			ifstream input(fname.c_str());
			solveCode = solver.solveFromCheckpoint(input);
		}
		solver.get_control_map(currentControls);
	}catch(SolverException* ex){
		cout<<"ERROR "<<basename()<<": "<<ex->code<<"  "<<ex->msg<<endl;
		status=UNSAT;
		delete checker;
		if(problem != bgproblem){ problem->clear(); delete problem; }
		return ex->code + 2;
	}catch(BasicError& be){
		cout<<"ERROR: "<<basename()<<endl;
		status=UNSAT;
		delete checker;
		if(problem != bgproblem){ problem->clear(); delete problem; }
		return 3;
	}
	if( !solveCode ){
		status=UNSAT;
		delete checker;
		if(problem != bgproblem){ problem->clear(); delete problem; }
		return 1;	
	}
	delete checker;
	if(problem != bgproblem){ problem->clear(); delete problem; }
	return 0;

}

BooleanDAG* InterpreterEnvironment::runOptims(BooleanDAG* result){
	if(params.olevel >= 3){
		DagOptim cse(*result);	
		//cse.alterARRACS();
		cse.process(*result);
	}

	if(params.verbosity > 3){cout<<"* after OPTIM: Problem nodes = "<<result->size()<<endl;	}

	if(params.olevel >= 5){
		BackwardsAnalysis opt;
		//cout<<"BEFORE: "<<endl;
		//result->print(cout);
		opt.process(*result);
		// cout<<"AFTER: "<<endl;
		// result->print(cout);
	}

	if(params.olevel >= 8){
		DagOptimizeCommutAssoc opt;
		opt.process(*result);
	}

	//result->print(cout) ;

	// cout<<"* after CAoptim: Problem nodes = "<<result->size()<<endl;

	if(params.olevel >= 4){
		DagOptim cse(*result);	
		if(params.alterARRACS){ 
			cout<<" alterARRACS"<<endl;
			cse.alterARRACS(); 
		}
		cse.process(*result);
	}
	
	if(params.verbosity > 0){ cout<<"* Final Problem size: Problem nodes = "<<result->size()<<endl;	}
	if(params.showDAG){ 
		result->print(cout);
	}

	return result;
}
