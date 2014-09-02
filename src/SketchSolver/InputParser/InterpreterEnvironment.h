#pragma once

#include "CegisCApi.h"
#include "CommandLineArgs.h"
#include "BooleanDAG.h"
#include "BooleanDAGCreator.h"
#include "SATSolver.h"

#include "DagFunctionInliner.h"
#include "DagElimUFUN.h"
#include "BackwardsAnalysis.h"
#include "DagOptimizeCommutAssoc.h"
#include "CEGISSolver.h"


#include <sstream>

extern timerclass solution;
extern timerclass modelBuilding;
extern const int LEAVEALONE;

using namespace std;

class InterpreterEnvironment
{
	map<string, BooleanDAG*> functionMap;
	CommandLineArgs& params;
	SolverHelper* finder;
	SATSolver* _pfind;
	int assertionStep;
	string sessionName;
	map<string, int> hardcodedholes;

	/*Debug state: */
	vector<BooleanDAG*> history;
	vector<vector<Tvalue> > statehistory;

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

	BooleanDAG* runOptims(BooleanDAG* result);

public:
	typedef enum {READY, UNSAT} STATUS;
	STATUS status;
	map<string, int> currentControls;
	BooleanDAG * bgproblem;
	CEGISSolver* solver;
	InterpreterEnvironment(CommandLineArgs& p): bgproblem(NULL), params(p), status(READY), assertionStep(0){
		_pfind = SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName());
		if(p.outputSat){
			_pfind->outputSAT();
		}
		finder = new SolverHelper(*_pfind);
		finder->setMemo(p.setMemo && p.synthtype == SATSolver::MINI);
		sessionName = procFname(params.inputFname);		
		solver = new CEGISSolver(*finder, params);
	}
	
	void addFunction(const string& name, BooleanDAG* fun){
		functionMap[name] = fun;
	}	

	BooleanDAGCreator* newFunction(const string& name, bool isModel){
		BooleanDAG* tmp = new BooleanDAG(name, isModel);
       
		if(params.verbosity>5) cout<<"CREATING "<<name<<endl;
		if(functionMap.count(name)>0){
			delete functionMap[name];
		}
       
       
		functionMap[name] = tmp;
        
        
		return new BooleanDAGCreator(tmp);		
	}
	
	void printControls(ostream& out){
		for(map<string, int>::iterator it = hardcodedholes.begin(); it != hardcodedholes.end(); ++it){
			if(it->second != LEAVEALONE){
				out<<it->first<<"\t"<<it->second<<endl;
			}
		}

		for(map<string, int>::iterator it = currentControls.begin(); it != currentControls.end(); ++it){
			out<<it->first<<"\t"<<it->second<<endl;
		}
	}
	
	void printControls_wrapper() {
		ostream& out = std::cout;
		printControls(out);
	}

	void printControls_wrapper(const string& s) {
		printControls(s);
	}

	int runCommand(const string& cmd, list<string*>& parlist);


	void printControls(const string& s){
		string tmp = s;
		if(tmp == ""){
			tmp = params.outputFname;
		}
		ofstream out(tmp.c_str());
		printControls(out);
	}

	BooleanDAG* prepareMiter(BooleanDAG* spec, BooleanDAG* sketch);

	void doInline(BooleanDAG& dag, map<string, BooleanDAG*> functionMap, int i);

	BooleanDAG* getCopy(const string& s){
		return functionMap[s]->clone();
	}

	/**
		This function takes ownership of dag. After this, 
		dag will be useless, and possibly deallocated.
	*/
	int assertDAG(BooleanDAG* dag, ostream& out);
	int assertDAG_wrapper(BooleanDAG* dag);
	int assertDAG_wrapper(BooleanDAG* dag, const char* fileName);

	void set_function(const string& s, BooleanDAG* dag) {
		functionMap[s] = dag;
	}

	virtual ~InterpreterEnvironment(void);
};
