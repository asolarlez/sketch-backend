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
#include "CEGISFinder.h"
#include "CEGISParams.h"

using namespace MSsolverNS;

class DagOptim;


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

	CEGISFinderSpec* finder;
	

	
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

	CEGISSolver(CEGISFinderSpec* finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats);
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
