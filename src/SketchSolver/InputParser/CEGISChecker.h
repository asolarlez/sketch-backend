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

    //MODIFIES InputStore
    static void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype);

    //MODIFIES InputStore
    static void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime=false);

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


	void addProblem(BooleanDAG* problem, const string& file)
	{
		curProblem = problems.size();
		problems.push_back(problem);

        {
            Dout( cout<<"BEFORE declaring input names"<<endl );
            redeclareInputs(get_input_store(), problem, true);
        }

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
                int nbits = ctrlnode->get_nbits();
                if(ctrlnode->getOtype() == OutType::BOOL){
                    cbits++;
                } else if (ctrlnode->getOtype() == OutType::FLOAT) {
                    cfloats++;
                } else{
                    cints++;
                }
                if (ctrlnode->spAngelic) {
                    declareInput(get_input_store(), problemIn[i]->get_name() + "_src", nbits, ctrlnode->getArrSz(), ctrlnode->getOtype());
                }
            }
            if(PARAMS->verbosity > 2){
                cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
            }
        }

        if (!file.empty()) {
            files[curProblem] = new File();
            vector<bool_node*>& inputs = problem->getNodesByType(bool_node::SRC);
            File::Result res = files[curProblem]->parseFile(file, floats, inputs, get_input_store());
            while (res == File::MOREBITS) {
                if (PARAMS->verbosity > 5) {
                    cout << "CONTROL: growing l=" << problemLevel() << " inputs to size " << (problem->getIntSize() + 1) << endl;
                }
                growInputs(get_input_store(), problem, problem, (problemLevel() - 1) == 1);
                res = files[curProblem]->parseFile(file, floats, inputs, get_input_store());
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
