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
#include "CEGISFinder.h"

using namespace MSsolverNS;

class DagOptim;

class ElapsedTime
{
    double find_time = -1;
    double check_time = -1;
    double total_time = -1;

public:
    ElapsedTime() = default;
    ElapsedTime(double _find_time, double _check_time, double _total_time):
    find_time(_find_time), check_time(_check_time),total_time(_total_time) {}

    string to_string() const {
        return "FIND TIME " + std::to_string(find_time) + " CHECK TIME " + std::to_string(check_time) + "; TOTAL TIME " + std::to_string(total_time);
    }
};

struct CEGISSolverResult
{
    bool success;
    HoleVarStore* final_ctrl_var_store;
    vector<HoleVarStore*> intermediate_solutions;
};

class CEGISSolver {

	CEGISFinderSpec* finder;
	CEGISChecker* checker;

	map<string, int> last_input;

	HoleHardcoder& hc;

    ElapsedTime last_elapsed_time;

protected:

	FloatManager& floats;
	CEGISParams params;

	vector<BooleanDagLightUtility*> problems;
    vector<const File*> files;

	void declareControl(CTRL_node* cnode);


//-- internal wrappers arround the checker methods
    const BooleanDAG* getProblemDAG()
    {
        return checker->getProblemDag();
    }

    BooleanDagLightUtility* getProblem()
    {
        return checker->getProblem();
    }

	bool problem_stack_is_empty()
	{
		return checker->problem_stack_is_empty();
	}

	vector<Tvalue>& get_check_node_ids()
	{
		return checker->get_check_node_ids();
	}

public:

    void clear()
    {
        finder->clear();
        checker->clear();
        delete checker;

        last_input.clear();
    }

	VarStore ctrl_store;
    map<string, string> current_hole_name_to_original_hole_name;

	CEGISSolver(CEGISFinderSpec* _finder, CommandLineArgs& args, FloatManager& _floats, HoleHardcoder& _hc):
	finder(_finder),
	floats(_floats),
	params(args),
	checker(new CEGISChecker(args, _hc, _floats)),
	hc(_hc) {}

	~CEGISSolver(void);
	void addProblem(BooleanDagLightUtility *harness, const File *file);

    const ElapsedTime& get_last_elapsed_time() {
        return last_elapsed_time;
    }

	virtual CEGISSolverResult solve(
            const long long _budget = numeric_limits<unsigned long long>::max(),
            const long long find_solve_max_timeout_in_microseconds = numeric_limits<unsigned long long>::max(),
            const File* validation_file = nullptr);

	void print_control_map(ostream& out);

	bool solveFromCheckpoint(istream& in);

	void get_control_map_as_map_str_str(map<string, string>& values);
//	void outputEuclid(ostream& fout);
	void setup2QBF(ofstream& out);


    VarStore prevCtrlStore;
	VarStore prevInputStore;
	bool prevSolutionFound;
	void storePreviousSolution(VarStore prevInputStore1, VarStore prevCtrlStore1);
	void getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes);
	string minVarNodeName; 
	int minVarNodeSize;
};
