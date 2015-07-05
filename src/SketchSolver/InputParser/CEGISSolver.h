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
		lightVerif(args.lightVerif)
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


class CEGISSolver
{
	HoleHardcoder& hcoder;
	int curProblem;
	vector<BooleanDAG*> problems;
	stack<BooleanDAG*> problemStack;
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

	SolverHelper& dirFind;		
	SATSolver& mngFind;
	

	
	VarStore inputStore;
	// vector<struct InputGen *> inputGens;

	CEGISparams params;

	bool stoppedEarly;
	
	Checkpointer cpt;
	BooleanDAG* lastFproblem;
	
	vector<Tvalue> find_node_ids;
	vector<Tvalue> check_node_ids;
	map<string, int> last_input;	
protected:
	void declareControl(CTRL_node* cnode);
	void declareInput(const string& cname, int size, int arrSz);
	bool solveCore();
	bool simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive);
	bool find(VarStore& input, VarStore& controls, bool hasInputChanged);
	void addInputsToTestSet(VarStore& input);

	bool check(VarStore& input, VarStore& controls);
	lbool baseCheck(VarStore& controls, VarStore& input);
	void setNewControls(VarStore& controls, SolverHelper& dirCheck);
	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);
	bool_node* nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse);

	void normalizeInputStore();
	void abstractProblem();
	bool minimizeHoleValue(vector<string>& mhnames, vector<int>& mhsizes);
	void growInputs(BooleanDAG* dag, BooleanDAG* oridag, bool isTop);
public:
	vector<Tvalue> find_history;
	VarStore ctrlStore;
	BooleanDAG* hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type);

	CEGISSolver(SolverHelper& finder, HoleHardcoder& hc, CommandLineArgs& args);
	~CEGISSolver(void);
	void addProblem(BooleanDAG* miter);


	virtual bool solve();
	void print_control_map(ostream& out);
	

	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);
	


	void printDiagnostics(SATSolver& mng, char c);
	

	void redeclareInputs(BooleanDAG* dag);
	

	void get_control_map(map<string, int>& values);
	void outputEuclid(ostream& fout);
	void setup2QBF(ostream& out);


    VarStore prevCtrlStore;
	VarStore prevInputStore;
	bool prevSolutionFound;
	void storePreviousSolution(VarStore prevInputStore1, VarStore prevCtrlStore1);
	void getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes);
	string minVarNodeName; 
	int minVarNodeSize;
};
