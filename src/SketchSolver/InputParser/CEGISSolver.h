#pragma once

#include "BooleanToCNF.h"
#include "FindCheckSolver.h"
#include "BooleanDAG.h"
#include "Tvalue.h"
#include "Checkpointer.h"
#include "VarStore.h"
#include "CommandLineArgs.h"
#include "SolverTypes.h"
#include "HoleHardcoder.h"
#include <stack>
#include <ctime>
#include "FloatSupport.h"

using namespace MSsolverNS;

class DagOptim;
class CEGISparams{
public:	
	int iterlimit;
	bool printDiag;
	int NINPUTS;
	int nseeds;	
	bool simulate;
	int simiters;
	int simstopsize;
	bool setMemo;
	typedef enum{ NOSIM /*no simplify*/, SIMSIM/*simple simplify*/, RECSIM/*recursive simplify*/} simtype;
	simtype simplifycex;
	bool superChecks;
	bool lightVerif;
	float sparseArray;	
	CEGISparams(CommandLineArgs& args):
		printDiag(false),
		nseeds(1),
		NINPUTS(3),
		iterlimit(-1),		
		simulate(true),
		simiters(3),		
		simplifycex(RECSIM),
		superChecks(false),
		setMemo(args.setMemo),
		lightVerif(args.lightVerif),
		sparseArray(args.sparseArray)
	{
		printDiag = args.printDiag;
		nseeds = args.seedsize;		
		NINPUTS = args.NINPUTS;
		if(args.terminateafter > 0){
			iterlimit = args.terminateafter;
		}		
		simulate = args.simulate;
		simiters = args.simiters;
		simstopsize = args.simstopsize;
		superChecks = args.superChecks;
		if(args.simplifycex == "NOSIM"){ simplifycex = NOSIM; }
		if(args.simplifycex == "SIMSIM"){ simplifycex = SIMSIM; }
		if(args.simplifycex == "RECSIM"){ simplifycex = RECSIM; }
	}

	void activatePrintDiag(){
		printDiag = true;
	}		
	void setIterLimit(int p_iterlimit){iterlimit = p_iterlimit; };

};


class CEGISFinder {
	vector<Tvalue> find_node_ids;
	vector<Tvalue> find_history;
	bool stoppedEarly;
	CEGISparams params;
	BooleanDAG* lastFproblem;
	FloatManager& floats;
	HoleHardcoder& hcoder;
	SolverHelper& dirFind;
	SATSolver& mngFind;
	
	void addInputsToTestSet(BooleanDAG* problem, VarStore& input);
public:
	CEGISFinder(FloatManager& _floats,
		HoleHardcoder& _hcoder,
		SolverHelper& _dirFind,
		SATSolver& _mngFind, CommandLineArgs& args) :floats(_floats), hcoder(_hcoder), dirFind(_dirFind), mngFind(_mngFind), params(args), lastFproblem(NULL) {

	}

	bool minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes);

	bool find(BooleanDAG* problem, VarStore& input, VarStore& controls, bool hasInputChanged);

	void declareControl(CTRL_node* cnode) {
		dirFind.declareControl(cnode);
	}

	void updateCtrlVarStore(VarStore& ctrlStore) {
		for (map<string, int>::const_iterator it = dirFind.arrsize_begin(); it != dirFind.arrsize_end(); ++it) {
			if (!ctrlStore.contains(it->first)) {
				ctrlStore.newVar(it->first, it->second, NULL);
			}
		}
	}

	void retractAssumptions() {
		dirFind.getMng().retractAssumptions();
	}
};



BooleanDAG* hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type, FloatManager& floats);
void printDiagnostics(SATSolver& mng, char c);


class CEGISSolver
{
	FloatManager& floats;
	HoleHardcoder& hcoder;
	int curProblem;
	vector<BooleanDAG*> problems;
	stack<BooleanDAG*> problemStack;
	map<int, vector<VarStore> > expensives;
	void pushProblem(BooleanDAG* p){		
		problemStack.push(p);
	}
	BooleanDAG* getProblem(){
		return problemStack.top();
	}
	void popProblem(){
		BooleanDAG* t = problemStack.top();
		problemStack.pop();
		t->clear();
		delete t;
	}
	int problemLevel(){
		return problemStack.size();
	}

	CEGISFinder& finder;
	

	
	VarStore inputStore;
	// vector<struct InputGen *> inputGens;

	CEGISparams params;
	
	
	Checkpointer cpt;

	map<int, string> files;
	vector<Tvalue> check_node_ids;
	map<string, int> last_input;	
protected:
	void declareControl(CTRL_node* cnode);
	void declareInput(const string& cname, int size, int arrSz, OutType* otype);
	bool solveCore();
	//bool solveOptimization();
	bool simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive);
	bool find(VarStore& input, VarStore& controls, bool hasInputChanged);
	

	bool check(VarStore& input, VarStore& controls);
	lbool baseCheck(VarStore& controls, VarStore& input);
	void setNewControls(VarStore& controls, SolverHelper& dirCheck);
	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);
	bool_node* nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse);

	void normalizeInputStore();
	void abstractProblem();
	
	void growInputs(BooleanDAG* dag, BooleanDAG* oridag, bool isTop);
public:
	
	VarStore ctrlStore;	

	CEGISSolver(CEGISFinder& finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats);
	~CEGISSolver(void);
	void addProblem(BooleanDAG* miter, const string& file);


	virtual bool solve();
	void print_control_map(ostream& out);
	

	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);
	
	
	

	void redeclareInputs(BooleanDAG* dag, bool firstTime=false);
	

	void get_control_map(map<string, string>& values);
	void outputEuclid(ostream& fout);
	void setup2QBF(ofstream& out);


    VarStore prevCtrlStore;
	VarStore prevInputStore;
	bool prevSolutionFound;
	void storePreviousSolution(VarStore prevInputStore1, VarStore prevCtrlStore1);
	void getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes);
	string minVarNodeName; 
	int minVarNodeSize;
};
