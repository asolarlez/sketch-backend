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
#include "CounterexampleFinder.h"
#include "BooleanDagUtility.h"

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


	void pushProblem(BooleanDagUtility* p){
        p->increment_shared_ptr();
		problemStack.push(p);
	}
	int problemLevel(){
		return (int) problemStack.size();
	}
	void popProblem(){
        BooleanDagUtility* t = problemStack.top();
		problemStack.pop();
		t->clear();
	}


	void abstractProblem(VarStore & inputStore, VarStore& ctrlStore);

//--moved from protected;

	stack<BooleanDagUtility*> problemStack;

	map<int, File*> files;

	int curProblem; 

	FloatManager& floats;
	CEGISparams params;

	vector<BooleanDagUtility*> problems;

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


    BooleanDagUtility* getProblem(){
        assert(!problemStack.empty());
        return problemStack.top();
    }

    BooleanDagUtility* getHarness()
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


	void addProblem(BooleanDagUtility *harness, File *file)
	{
		curProblem = (int) problems.size();
		problems.push_back(harness);
        if (file != nullptr) {
            files[curProblem] = file;
        }

        BooleanDagUtility* inlined_harness = harness->produce_inlined_dag();
        inlined_harness->increment_shared_ptr();
        redeclareInputsAndAngelics(get_input_store(), inlined_harness->get_dag());

        // IS THIS DEBUG CODE? YES
        Dout( cout << "problem->get_n_controls() = " << root_dag->get_n_controls() << "  " << root_dag << endl );
        {
            auto problemIn = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
            if(PARAMS->verbosity > 2){
                cout<<"  # OF CONTROLS:    "<< problemIn.size() <<endl;
            }
            int cints = 0;
            int cbits = 0;
            int cfloats = 0;
            for(int i=0; i<problemIn.size(); ++i){
                CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);
                if(ctrlnode->getOtype() == OutType::BOOL){
                    cbits++;
                } else if (ctrlnode->getOtype() == OutType::FLOAT) {
                    cfloats++;
                } else{
                    cints++;
                }
            }
            if(PARAMS->verbosity > 2){
                cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
            }
        }

        inlined_harness->clear();
    }


	bool problemStack_is_empty()
	{
		return problemStack.empty();
	}


	vector<Tvalue>& get_check_node_ids()
	{
		return check_node_ids;
	}


};
