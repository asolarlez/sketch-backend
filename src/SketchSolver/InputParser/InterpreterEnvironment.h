#pragma once

#include "CommandLineArgs.h"
#include "BooleanDAG.h"
#include "BooleanDAGCreator.h"
#include "SATSolver.h"

#include "DagFunctionInliner.h"
#include "DagElimUFUN.h"
#include "BackwardsAnalysis.h"
#include "DagOptimizeCommutAssoc.h"
#include "CEGISSolver.h"
#include "ABCSATSolver.h"


#include <sstream>

extern timerclass solution;
extern timerclass modelBuilding;

using namespace std;

class InterpreterEnvironment
{
	typedef enum {READY, UNSAT} STATUS;
	STATUS status;
	CommandLineArgs& params;
	map<string, BooleanDAG*> functionMap;
	map<string, int> currentControls;
	BooleanDAG * problem;
	SolverHelper* finder;

	int assertionStep;
	string sessionName;
	string findName(){
		stringstream s;
		s<<sessionName;
		s<<"_find";
		return s.str();
	}
	string checkName(){
		stringstream s;
		s<<sessionName;
		s<<"_check_"<<assertionStep;
		return s.str();
	}
	string basename(){
		stringstream s;
		s<<sessionName<<"_"<<assertionStep;
		return s.str();
	}

	string procFname(string& fname){
		int x1 = fname.find_last_of("/");
		int x2 = fname.find_last_of("\\");
		int x3 = fname.find_last_of(".");

		x1 = x1>x2? x1: x2;
		x3 = x3 > 0? x3 : fname.size();
		++x1;
		
		return fname.substr(x1, x3-x1);
	}

	BooleanDAG* runOptims(BooleanDAG* result){
		if(params.olevel >= 3){
			DagOptim cse(*result);	
			//cse.alterARRACS();
			cse.process(*result);
		}

		if(params.verbosity > 3){cout<<"* after OPTIM: Problem nodes = "<<result->size()<<endl;	}

		if(params.olevel >= 5){
			BackwardsAnalysis opt;
			opt.process(*result);
		}

		if(params.olevel >= 6){
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


public:
	InterpreterEnvironment(CommandLineArgs& p): problem(NULL), params(p), status(READY), assertionStep(0){
		finder = new SolverHelper(*SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName()));
		sessionName = procFname(params.inputFname);			  
	}
	
	void addFunction(const string& name, BooleanDAG* fun){
		functionMap[name] = fun;
	}	

	BooleanDAGCreator* newFunction(const string& name){
		BooleanDAG* tmp = new BooleanDAG();
		cout<<"CREATING "<<name<<endl;
		functionMap[name] = tmp;
		return new BooleanDAGCreator(tmp);		
	}


	void printControls(ostream& out){
		for(map<string, int>::iterator it = currentControls.begin(); it != currentControls.end(); ++it){
			out<<it->first<<"\t"<<it->second<<endl;
		}
	}


	int runCommand(const string& cmd, list<string*>& parlist){
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


	void printControls(const string& s){
		string tmp = s;
		if(tmp == ""){
			tmp = params.outputFname;
		}
		ofstream out(tmp.c_str());
		printControls(out);
	}

	BooleanDAG* prepareMiter(BooleanDAG* spec, BooleanDAG* sketch){
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

		if(params.olevel >= 3){
			if(params.verbosity > 3){ cout<<" Inlining amount = "<<params.inlineAmnt<<endl; }
			{
				if(params.verbosity > 3){ cout<<" Inlining functions in the sketch."<<endl; }
				DagFunctionInliner cse(*sketch, functionMap, params.inlineAmnt, params.mergeFunctions );	
				cse.process(*sketch);
			}
			{
				if(params.verbosity > 3){ cout<<" Inlining functions in the spec."<<endl; }
				DagFunctionInliner cse(*spec, functionMap,  params.inlineAmnt, params.mergeFunctions  );	
				cse.process(*spec);
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



	

	BooleanDAG* getCopy(const string& s){
		return functionMap[s]->clone();
	}

	/**
		This function takes ownership of dag. After this, 
		dag will be useless, and possibly deallocated.
	*/
	int assertDAG(BooleanDAG* dag, ostream& out){
		Assert(status==READY, "You can't do this if you are UNSAT");
		++assertionStep;
		if(problem == NULL){
			problem = dag;
		}else{
			problem->andDag(dag);
			problem = runOptims(problem);
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
			return ex->code + 2;
		}catch(BasicError& be){
			cout<<"ERROR: "<<basename()<<endl;
			status=UNSAT;
			delete checker;
			return 3;
		}
		if( !solveCode ){
			status=UNSAT;
			delete checker;
			return 1;	
		}
		delete checker;
		return 0;

	}

	virtual ~InterpreterEnvironment(void);
};
