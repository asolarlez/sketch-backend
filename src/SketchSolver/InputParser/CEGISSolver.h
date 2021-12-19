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

class ElapsedTime
{
    double find_time;
    double check_time;
    double total_time;

public:
    ElapsedTime(double _find_time, double _check_time, double _total_time):
    find_time(_find_time), check_time(_check_time),total_time(_total_time) {}

    string to_string()
    {
        return "FIND TIME " + std::to_string(find_time) + " CHECK TIME " + std::to_string(check_time) + "; TOTAL TIME " + std::to_string(total_time);
    }
};

class CEGISSolver
{

	CEGISFinderSpec* finder;
	CEGISChecker* checker;

	map<string, int> last_input;

	HoleHardcoder& hc;

    ElapsedTime* last_elapsed_time = NULL;

protected:

	FloatManager& floats;
	CEGISparams params;

	vector<Harness*> problems;

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

    void clear()
    {

    }

	VarStore ctrlStore;
	CEGISSolver(CEGISFinderSpec* _finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats, HoleHardcoder& _hc):
	finder(_finder), 
	floats(_floats), 
	params(args),
	checker(new CEGISChecker(args, hc, _floats)),
	hc(_hc)
	{
	//	cout << "miter:" << endl;
	//	miter->lprint(cout);
			
	}
	~CEGISSolver(void);
	void addProblem(Harness *harness, File *file);


    ElapsedTime* get_last_elapsed_time()
    {
        assert(last_elapsed_time != NULL);
        return last_elapsed_time;
    }

	virtual bool solve();
	void print_control_map(ostream& out);
	

	bool solveFromCheckpoint(istream& in);



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
