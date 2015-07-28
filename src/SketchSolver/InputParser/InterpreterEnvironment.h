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

using namespace std;

class InterpreterEnvironment
{
	map<string, BooleanDAG*> functionMap;
	CommandLineArgs& params;
	SolverHelper* finder;
	SATSolver* _pfind;
	int assertionStep;
	string sessionName;
	HoleHardcoder hardcoder;
	map<string, map<string, string> > replaceMap;
	int maxRndSize;
	int avgRndsSize;
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
		hardcoder.setSolver(finder);
		sessionName = procFname(params.inputFname);		
		solver = new CEGISSolver(*finder, hardcoder, params);
	}
	
	vector<pair<string, string> > spskpairs;

	void addspskpair(const string& spec, const string& sketch  ){
		spskpairs.push_back(make_pair(spec, sketch));
	}

	int doallpairs();


	void reset(){
		delete finder;
		delete _pfind;
		delete solver;
		_pfind = SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName());
		finder = new SolverHelper(*_pfind);
		hardcoder.reset();
		hardcoder.setSolver(finder);
		solver = new CEGISSolver(*finder, hardcoder, params);
		cout<<"ALLRESET"<<endl;
		status=READY;
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
		hardcoder.printControls(out);

		for(map<string, int>::iterator it = currentControls.begin(); it != currentControls.end(); ++it){
			if(!hardcoder.hasValue(it->first)){
				out<<it->first<<"\t"<<it->second<<endl;
			}
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

  /*
   * Replaces f1 inside f2 with functions in f3.
   */
  void registerFunctionReplace(string f1, string f2, string f3, int stCount) {
    map<string, map<string, string> >::iterator it = replaceMap.find(f1);
    if (it == replaceMap.end()) {
      map<string, string> fmap;
      fmap[f2] = f3;
      replaceMap[f1] = fmap;
    } else {
      map<string, string> fmap = it->second;
      Assert(fmap.find(f2) == fmap.end(), "Error: replacing the same functions in two different ways");
      fmap[f2] = f3;
    }
  }
  
	BooleanDAG* prepareMiter(BooleanDAG* spec, BooleanDAG* sketch);

	void doInline(BooleanDAG& dag, map<string, BooleanDAG*> functionMap, int i, map<string, map<string, string> > replaceMap);

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
    
    void replaceSrcWithTuple(BooleanDAG& dag);
	virtual ~InterpreterEnvironment(void);
};
