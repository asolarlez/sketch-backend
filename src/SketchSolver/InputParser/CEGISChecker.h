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
		problemStack.push(p);
	}
	int problemLevel(){
		return (int) problemStack.size();
	}
	void popProblem(){
        BooleanDagLightUtility* t = problemStack.top();
		problemStack.pop();
		t->clear();
	}


	void abstractProblem(VarStore & inputStore, VarStore& ctrlStore);

//--moved from protected;

	stack<BooleanDagLightUtility*> problemStack;

	map<int, File*> files;

	int curProblem; 

	FloatManager& floats;
	CEGISParams params;

	vector<BooleanDagLightUtility*> problems;

	vector<Tvalue> check_node_ids;

    BooleanDAG* check(VarStore& controls, VarStore& input);

    VarStore input_store;

public:

    void clear()
    {
        clear_problemStack();
        assert(problemStack.empty());

        files.clear();

        problems.clear();

        check_node_ids.clear();

    }


    void clear_problemStack()
    {
        while(!problemStack.empty())
        {
            popProblem();
        }
    }

    inline VarStore& get_input_store()
    {
        return input_store;
    }

    BooleanDAG* getProblemDag(){
        assert(!problemStack.empty());
        return problemStack.top()->get_dag();
    }


    BooleanDagLightUtility* getProblem(){
        assert(!problemStack.empty());
        return problemStack.top();
    }

    BooleanDagLightUtility* getHarness()
    {
        assert(!problemStack.empty());
        return problemStack.top();
    }

	CEGISChecker(CommandLineArgs& args,  HoleHardcoder& hc, FloatManager& _floats):
		params(args), floats(_floats), hcoder(hc)
		{}

    BooleanDAG* check(VarStore& controls)
    {
        return check(controls, get_input_store());
    }


	void addProblem(BooleanDagLightUtility *harness, File *file);

	bool problemStack_is_empty()
	{
		return problemStack.empty();
	}


	vector<Tvalue>& get_check_node_ids()
	{
		return check_node_ids;
	}


};
