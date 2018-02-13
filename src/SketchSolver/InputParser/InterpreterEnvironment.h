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

class ClauseExchange{
	/*!
	File from which to read data from other cores.
	*/
	string infile;
	/*!
	File from which to write data to other cores. 
	In some setups, this will be the same as infile, and all cores
	read and write to the same file. With large numbers of cores, though, 
	alternative setups are possible, for example, with individual files
	used to communicate between two neighboring calls.
	*/
	string outfile;
	int failures;
	MiniSATSolver* msat;
	/*!
	Contains the IDs of literals that are known to be true. This is very valuable 
	information that will be exchanged.
	*/
	set<int> single;
	/*!
	Contains binary clauses known to the global solver. These are also valuable
	and should also be exchanged.
	*/
	set<pair<int, int> > dble;
	/*!
	These are binary clauses that should not be exchanged because everyone knows them already. 
	*/
	set<pair<int, int> > baseline;

	/*!
	Populates single and dble from the contents of the msat SAT solver.
	*/
	void analyzeLocal();
	/*!
	Read the content of infile into the solver.
	*/
	void readInfile();
	/*!
	Write the contents of single and dble to the outfile.
	*/
	void pushOutfile();
public:
	ClauseExchange(MiniSATSolver* ms, const string& inf, const string& outf);	
	void exchange();
	/*!
	Print all the information that the exchanger is ready to exchange with others.
	*/
	void printToExchange() {
		cout << "Single: ";
		for (auto sit = single.begin(); sit != single.end(); ++sit) {
			cout << ", " << (*sit);
		}
		cout << endl;
		cout << "Double: ";
		for (auto dit = dble.begin(); dit != dble.end(); ++dit) {
			cout << ", (" << dit->first << " " << dit->second << ")";
		}
		cout << endl;
		/*
		cout << "Baseline: ";
		for (auto dit = baseline.begin(); dit != baseline.end(); ++dit) {
			cout << ", (" << dit->first << " " << dit->second << ")";
		}
		cout << endl;
		*/
	}

};


struct spskpair {
	const string spec;
	const string sketch;
	const string file;

	spskpair(const string& _spec, const string& _sketch, const string& _file):
	spec(_spec),
	sketch(_sketch),
	file(_file)
	{

	}
	spskpair(const string& _spec, const string& _sketch) :
		spec(_spec),
		sketch(_sketch),
		file("")
	{

	}
};



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
	ClauseExchange* exchanger;
	FloatManager floats;
	map<string, BooleanDAG*> numericalAbsMap;

	string findName() {
		stringstream s;
		s << sessionName;
		s << "_find";
		return s.str();
	}
	string checkName() {
		stringstream s;
		s << sessionName;
		s << "_check_" << assertionStep;
		return s.str();
	}
	string basename() {
		stringstream s;
		s << sessionName << "_" << assertionStep;
		return s.str();
	}

	string procFname(string& fname) {
		int x1 = fname.find_last_of("/");
		int x2 = fname.find_last_of("\\");
		int x3 = fname.find_last_of(".");

		x1 = x1>x2 ? x1 : x2;
		x3 = x3 > 0 ? x3 : fname.size();
		++x1;

		return fname.substr(x1, x3 - x1);
	}

	BooleanDAG* runOptims(BooleanDAG* result);

public:
	typedef enum { READY, UNSAT } STATUS;
	STATUS status;
	bool hasGoodEnoughSolution;
	map<string, string> currentControls;
	BooleanDAG * bgproblem;
	CEGISSolver* solver;
	InterpreterEnvironment(CommandLineArgs& p): bgproblem(NULL), params(p), status(READY), assertionStep(0),floats(p.epsilon){
		_pfind = SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName());
		if (p.outputSat) {
			_pfind->outputSAT();
		}
		finder = new SolverHelper(*_pfind);
		finder->setMemo(p.setMemo && p.synthtype == SATSolver::MINI);
		hardcoder.setSolver(finder);
		sessionName = procFname(params.inputFname);
		solver = new CEGISSolver(*finder, hardcoder, params, floats);
		exchanger = NULL;
		hasGoodEnoughSolution = false;
	}

	vector<spskpair > spskpairs;

	void addspskpair(const string& spec, const string& sketch) {
		spskpairs.push_back(spskpair(spec, sketch));
	}


	void addspskpair(const string& spec, const string& sketch, const string& file) {
		spskpairs.push_back(spskpair(spec, sketch, file));
	}

	int doallpairs();
	void share();

	/*!
	Check if adaptive concretization is enabled.
	*/
	bool acEnabled() {
		return (params.ntimes > 1 || params.randomassign);
	}


	void resetCore() {
		delete finder;
		delete _pfind;
		delete solver;
		_pfind = SATSolver::solverCreate(params.synthtype, SATSolver::FINDER, findName());
		finder = new SolverHelper(*_pfind);
		finder->setNumericalAbsMap(numericalAbsMap);
		hardcoder.setSolver(finder);
		solver = new CEGISSolver(*finder, hardcoder, params, floats);
		cout << "ALLRESET" << endl;
		status = READY;
	}

	/*!
	Wipes out the finder SATSolver and SolverHelper as well as the CEGIS solver. 
	Resets the hardcoder and registers the new finder with it.
	*/
	void reset(){
		hardcoder.reset();	
		resetCore();
	}


	void resetMinimize() {
		hasGoodEnoughSolution = true;
		hardcoder.resetForMinimize(currentControls);
		resetCore();
	}




	void addFunction(const string& name, BooleanDAG* fun) {
		functionMap[name] = fun;
	}

	BooleanDAGCreator* newFunction(const string& name, bool isModel) {
		BooleanDAG* tmp = new BooleanDAG(name, isModel);

		if (params.verbosity>5) cout << "CREATING " << name << endl;
		if (functionMap.count(name)>0) {
			delete functionMap[name];
		}


		functionMap[name] = tmp;


		return new BooleanDAGCreator(tmp, floats);		
	}


	void recordSolution() {
		solver->get_control_map(currentControls);
		hardcoder.get_control_map(currentControls);
		cout << "VALUES ";
		for (auto it = currentControls.begin(); it != currentControls.end(); ++it) {
			cout << it->first << ": " << it->second << ", ";
		}
		cout << endl;
	}

	void printControls(ostream& out){		
		for(auto it = currentControls.begin(); it != currentControls.end(); ++it){
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


	void printControls(const string& s) {
		string tmp = s;
		if (tmp == "") {
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
	
  
	BooleanDAG* prepareMiter(BooleanDAG* spec, BooleanDAG* sketch, int inlineAmnt);

	void doInline(BooleanDAG& dag, map<string, BooleanDAG*>& functionMap, int i, map<string, map<string, string> > replaceMap);

	vector<vector<string> > holesToHardcode;

	void fixes(const string& holename);

	BooleanDAG* getCopy(const string& s) {
		return functionMap[s]->clone();
	}

	/**
	This function takes ownership of dag. After this,
	dag will be useless, and possibly deallocated.
	*/
	SATSolver::SATSolverResult assertDAG(BooleanDAG* dag, ostream& out, const string& file);
	int assertDAG_wrapper(BooleanDAG* dag);
	int assertDAG_wrapper(BooleanDAG* dag, const char* fileName);

	void set_function(const string& s, BooleanDAG* dag) {
		functionMap[s] = dag;
	}

  void replaceSrcWithTuple(BooleanDAG& dag);
  int inlineAmnt() { return params.inlineAmnt; }
  void abstractNumericalPart(BooleanDAG& dag);
	virtual ~InterpreterEnvironment(void);
};
