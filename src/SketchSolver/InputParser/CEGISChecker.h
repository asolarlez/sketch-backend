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
		return (int) problemStack.size();
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

	map<int, File*> files;

	int curProblem; 

	FloatManager& floats;
	CEGISparams params;

	vector<BooleanDAG*> problems;

	vector<Tvalue> check_node_ids;

    BooleanDAG* check(VarStore& controls, VarStore& input);

    VarStore input_store;

public:

    inline VarStore& get_input_store()
    {
        return input_store;
    }

    BooleanDAG* getProblem(){
        return problemStack.top();
    }

	CEGISChecker(CommandLineArgs& args,  HoleHardcoder& hc, FloatManager& _floats):
		params(args), floats(_floats), hcoder(hc)
		{}

    BooleanDAG* check(VarStore& controls)
    {
        return check(controls, get_input_store());
    }


	void addProblem(BooleanDAG* problem, File* file)
	{
		curProblem = (int) problems.size();
		problems.push_back(problem);
        if (file != NULL) {
            files[curProblem] = file;
        }

        redeclareInputsAndAngelics(get_input_store(), problem);

        // IS THIS DEBUG CODE? YES
        Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
        {
            vector<bool_node*>& problemIn = problem->getNodesByType(bool_node::CTRL);
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
    }


	bool problemStack_is_empty()
	{
		return problemStack.size() == 0;
	}


	vector<Tvalue>& get_check_node_ids()
	{
		return check_node_ids;
	}


};
