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

class CEGISChecker
{

	HoleHardcoder& hcoder;
	map<int, vector<VarStore> > expensives;

	bool simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive);

	lbool baseCheck(VarStore& controls, VarStore& input);

	void setNewControls(VarStore& controls, SolverHelper& dirCheck);
	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);

	void growInputs(VarStore & inputStore, BooleanDAG* dag, BooleanDAG* oridag, bool isTop);


	void pushProblem(BooleanDAG* p){		
		problemStack.push(p);
	}
	int problemLevel(){
		return problemStack.size();
	}
	void popProblem(){
		BooleanDAG* t = problemStack.top();
		problemStack.pop();
		t->clear();
		delete t;
	}
	void clear_problemStack()
	{
		while(!problemStack.empty())
		{
			popProblem();
		}
	}


	void abstractProblem(VarStore & inputStore, VarStore& ctrlStore);

//--moved from protected;

	stack<BooleanDAG*> problemStack;

	map<int, string> files;

	int curProblem; 

	FloatManager& floats;
	CEGISparams params;

	vector<BooleanDAG*> problems;

	vector<Tvalue> check_node_ids;

	//ownwer is CEGISSolver.
	Checkpointer* cpt;
protected:

public:

	CEGISChecker(CommandLineArgs& args,  HoleHardcoder& hc, FloatManager& _floats, Checkpointer* _cpt): 
		params(args), floats(_floats), hcoder(hc), cpt(_cpt) 
		{}

	bool check(VarStore& input, VarStore& controls);

	void addProblem(BooleanDAG* problem, const string& file)
	{
		curProblem = problems.size();
		problems.push_back(problem);
		if (file != "") {
			files[curProblem] = file;
		}
	}

	bool problemStack_is_empty()
	{
		return problemStack.size() == 0;
	}

	BooleanDAG* getProblem(){
		return problemStack.top();
	}

	vector<Tvalue>& get_check_node_ids()
	{
		return check_node_ids;
	}

	//MODIFIES InputStore
	void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype);

	//MODIFIES InputStore
	void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime=false);
};

void printDiagnostics(SATSolver& mng, char c);
