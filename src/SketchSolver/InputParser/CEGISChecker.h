#pragma once

//#include "BooleanToCNF.h"
//#include "FindCheckSolver.h"
#include "BooleanDAG.h"
//#include "Tvalue.h"
//#include "Checkpointer.h"
//#include "VarStore.h"
#include "CommandLineArgs.h"
//#include "SolverTypes.h"
#include "HoleHardcoder.h"
#include <stack>
#include <ctime>
//#include "FloatSupport.h"
#include "CEGISParams.h"
#include "CounterexampleFinder.h"

using namespace MSsolverNS;

#include "BooleanDagLightUtility.h"


class CEGISChecker
{

	HoleHardcoder& hcoder;
	map<int, vector<VarStore> > expensives;

	bool simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive);

	lbool baseCheck(VarStore& controls, VarStore& input);

	void setNewControls(VarStore& controls, SolverHelper& dirCheck);
	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);

	void growInputs(VarStore & inputStore, BooleanDAG* dag, BooleanDAG* oridag, bool isTop);


	void pushProblem(BooleanDagLightUtility* p, bool assert_concretized = false){
        p->increment_shared_ptr();
        if(assert_concretized) {
            assert(p->get_dag()->getNodesByType(bool_node::CTRL).size() == 0);
        }
		problem_stack.push(p);
	}
	int problemLevel(){
		return (int) problem_stack.size();
	}
	void popProblem(){
        BooleanDagLightUtility* t = problem_stack.top();
		problem_stack.pop();
		t->clear();
	}


	void abstractProblem(VarStore & inputStore, VarStore& ctrlStore);

//--moved from protected;

	stack<BooleanDagLightUtility*> problem_stack;

	map<int, const File*> files;

	int curProblem; 

	FloatManager& floats;
	CEGISParams params;

	vector<BooleanDagLightUtility*> problems;

	vector<Tvalue> check_node_ids;

    BooleanDAG * check(VarStore& controls, VarStore& input);

    VarStore input_store;

public:

    void clear()
    {
        clear_problem_stack();
        assert(problem_stack.empty());

        files.clear();

        problems.clear();

        check_node_ids.clear();

    }


    void clear_problem_stack()
    {
        while(!problem_stack.empty())
        {
            popProblem();
        }
    }

    inline VarStore& get_input_store()
    {
        return input_store;
    }

    BooleanDAG* getProblemDag__non_const(){
        assert(!problem_stack.empty());
        return problem_stack.top()->get_dag__non_const();
    }

    const BooleanDAG* getProblemDag(){
        assert(!problem_stack.empty());
        return problem_stack.top()->get_dag();
    }

    BooleanDagLightUtility* getProblem(){
        assert(!problem_stack.empty());
        return problem_stack.top();
    }

    BooleanDagLightUtility* get_main_problem(){
        assert(!problems.empty());
        return problems[0];
    }

    BooleanDagLightUtility* getHarness()
    {
        assert(!problem_stack.empty());
        return problem_stack.top();
    }

	CEGISChecker(CommandLineArgs& args,  HoleHardcoder& hc, FloatManager& _floats):
		params(args), floats(_floats), hcoder(hc)
		{}

    BooleanDAG* check(VarStore& controls)
    {
        return check(controls, get_input_store());
    }


	void addProblem(BooleanDagLightUtility *harness, const File *file);

	bool problem_stack_is_empty()
	{
		return problem_stack.empty();
	}


	vector<Tvalue>& get_check_node_ids()
	{
		return check_node_ids;
	}


};
