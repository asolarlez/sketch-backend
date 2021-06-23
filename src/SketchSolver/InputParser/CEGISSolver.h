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
#include "CEGISChecker.h"

using namespace MSsolverNS;

class DagOptim;

class CEGISSolver
{

	CEGISFinderSpec* finder;
	CEGISChecker* checker;

	map<string, int> last_input;	

	Checkpointer cpt;
protected:

	FloatManager& floats;
	CEGISparams params;

	vector<BooleanDAG*> problems;

	//CEGISSolver owns inputStore, but CEGISChecker has declareInputs, redeclareInputs, and growInputs; which modify the inputStore. 
	VarStore inputStore;

	void declareControl(CTRL_node* cnode);
	bool solveCore();

	// bool check(VarStore& input, VarStore& controls)
	// {
	// 	return checker->check(input, controls);
	// }

	bool find(VarStore& input, VarStore& controls, bool hasInputChanged);
	
	bool_node* nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse);

	void normalizeInputStore();

//-- internal wrappers arround the checker methods
	bool check(VarStore& input, VarStore& controls)
	{
		return checker->check(input, controls);
	}

	bool problemStack_is_empty()
	{
		return checker->problemStack_is_empty();
	}

	BooleanDAG* getProblem(){
		return checker->getProblem();
	}

	vector<Tvalue>& get_check_node_ids()
	{
		return checker->get_check_node_ids();
	}

	//MODIFIES InputStore
	void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype)
	{
		return checker->declareInput(inputStore, cname, size, arrSz, otype);
	}

	//MODIFIES InputStore
	void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime=false)
	{
		return checker->redeclareInputs(inputStore, dag, firstTime);
	}

public:
	
	VarStore ctrlStore;	
	CEGISSolver(CEGISFinderSpec* _finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats):
	finder(_finder), 
	floats(_floats), 
	params(args),
	checker(new CEGISChecker(args, hc, _floats, &cpt))
	{
	//	cout << "miter:" << endl;
	//	miter->lprint(cout);
			
	}
	~CEGISSolver(void);
	void addProblem(BooleanDAG* miter, const string& file);


	virtual bool solve();
	void print_control_map(ostream& out);
	

	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);



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
