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
#include "CEGISParams.h"
#include "CEGISChecker.h"
#include "SkVal.h"
#include "CEGISFinder.h"

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

	void declareControl(CTRL_node* cnode);
	bool solveCore();

	// bool check(VarStore& input, VarStore& controls)
	// {
	// 	return checker->check(input, controls);
	// }

	bool_node* nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse);

	void normalizeInputStore();

//-- internal wrappers arround the checker methods
    BooleanDAG* getProblem()
    {
        return checker->getProblem();
    }

	bool problemStack_is_empty()
	{
		return checker->problemStack_is_empty();
	}

	vector<Tvalue>& get_check_node_ids()
	{
		return checker->get_check_node_ids();
	}

public:

	VarStore ctrlStore;
	CEGISSolver(CEGISFinderSpec* _finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats):
	finder(_finder), 
	floats(_floats), 
	params(args),
	checker(new CEGISChecker(args, hc, _floats))
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



    void get_control_map_as_map_str_skval(Assignment_SkVal *values);
	void get_control_map_as_map_str_str(map<string, string>& values);
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
