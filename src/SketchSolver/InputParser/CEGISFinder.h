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
#include "REASSolver.h"
#include "IntToFloatRewriteDag.h"

using namespace MSsolverNS;

class CEGISFinderSpec
{
protected:
    FloatManager& floats;
public:
    explicit CEGISFinderSpec(FloatManager& _floats): floats(_floats) {}
	virtual bool find(BooleanDAG* problem, VarStore& controls, bool hasInputChanged)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
		return false;
	}

	virtual bool minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
		return false;
	}

	virtual void declareControl(CTRL_node* cnode)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
	}

	virtual void updateCtrlVarStore(VarStore& ctrlStore)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
	}

	virtual void retractAssumptions()
	{
		Assert(false, "CEGISFinderSpec is just an interface.")
	}

    FloatManager &get_floats() {
        return floats;
    }
};


class CEGISFinder: public CEGISFinderSpec  {
	vector<Tvalue> find_node_ids;
	vector<Tvalue> find_history;
	bool stoppedEarly;
	CEGISparams params;
	SolverHelper& dirFind;
	SATSolver& mngFind;
	
	void addInputsToTestSet(BooleanDAG* problem, VarStore& input);
    void addProblemToTestSet(BooleanDAG* problem);

public:

	CEGISFinder(FloatManager &_floats, SolverHelper &_dirFind, SATSolver &_mngFind, CommandLineArgs &args) :
		CEGISFinderSpec(_floats), dirFind(_dirFind), mngFind(_mngFind), params(args)
	{

	}

	bool find(BooleanDAG* problem, VarStore& controls, bool hasInputChanged) override;

	bool minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes) override;

	void declareControl(CTRL_node* cnode) override {
		dirFind.declareControl(cnode);
	}

	void updateCtrlVarStore(VarStore& ctrlStore) override {
		for (map<string, int>::const_iterator it = dirFind.arrsize_begin(); it != dirFind.arrsize_end(); ++it)  {
			if (!ctrlStore.contains(it->first)) {
				ctrlStore.newVar(it->first, it->second, dirFind.getOtype(it->first));
			}
		}
	}

	void retractAssumptions() override {
		dirFind.getMng().retractAssumptions();
	}
};


class CEGISFinderNumerical: public CEGISFinderSpec
{
	static const int  float_idx_size = 18;

    REASSolver* reasSolver = NULL;

    BooleanDAG* allInputsDag = NULL;

	SATSolver::SATSolverResult assertDAGNumerical(
		BooleanDAG* dag, map<string, string>& currentControls, map<string, int>& currentControlInts, map<string, float>& currentControlFloats) {

		IntToFloatRewriteDag rewriter = IntToFloatRewriteDag(*dag->clone(), floats);
		BooleanDAG* new_dag = rewriter.rewrite();

	    reasSolver->addProblem(new_dag);
	       	
	    int solveCode = 0;
	    try{
	        solveCode = reasSolver->solve();
	        currentControls = rewriter.extract_result_typed(reasSolver->get_result(), reasSolver->get_ctrls(),currentControlInts, currentControlFloats);
	        //reasSolver->get_control_map_as_map_str_str(currentControls);
	    }catch(SolverException* ex){
	        cout<<"ERROR "/*<<basename()*/<<": "<<ex->code<<"  "<<ex->msg<<endl;
			return SATSolver::UNSATISFIABLE; // ex->code + 2;
	    }catch(BasicError& be){
	        currentControls = rewriter.extract_result_typed(reasSolver->get_result(), reasSolver->get_ctrls(),currentControlInts, currentControlFloats);
	        cout<<"ERROR: "/*<<basename()*/<<endl;
	        return SATSolver::UNSATISFIABLE;
	    }
	    if( !solveCode ){			
	        return SATSolver::UNSATISFIABLE;
	    }
	    
	    return SATSolver::SATISFIABLE;
	}

	static int get_bit(int bitstring, int idx)
	{
		return (bitstring & (1<<idx) )!= 0;
	}


public:

	CEGISFinderNumerical(FloatManager& _floats, ostream& out): CEGISFinderSpec(_floats)
	{
		reasSolver = new REASSolver(floats);
	}


	bool find(BooleanDAG* newdag, VarStore& controls, bool hasInputChanged) override
	{
		if(hasInputChanged)
        {
			if(allInputsDag == NULL)
			{
				allInputsDag = newdag;
			}
			else
			{
				allInputsDag->andDag(newdag);
			}		
		}
		else
		{
			//claim: solution is already optimal.
			return false;
		}

		map<string, string> outputControls;
		map<string, int> outputControlInts;
		map<string, float> outputControlFloats;

		SATSolver::SATSolverResult result = 
			assertDAGNumerical(allInputsDag, outputControls, outputControlInts, outputControlFloats);
		
		if(result != SATSolver::SATISFIABLE)
		{
			return false;
		}

		updateCtrlVarStore(controls);

		long long sum = 0;
		//Save outputControls in (VarStore) controls
		for(VarStore::iterator it = controls.begin(); it !=controls.end(); ++it){
			const string& cname = it->getName();
			cout << "cname = " << cname << endl;
			OutType* out_type = allInputsDag->get_node(cname)->getOtype();
			cout << "out_type = " <<  out_type->str() << endl;
 			//int cnt = dirFind.getArrSize(cname);
			assert(allInputsDag->get_node(cname)->isArrType() == false); 
			if(out_type == OutType::INT || out_type == OutType::BOOL)
			{
				INTER_node* inter_node = (INTER_node*)allInputsDag->get_node(cname);
				int cnt = inter_node->get_nbits();
				Assert( cnt == it->size(), "find: SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
				int ctrl_val = outputControlInts[cname];
				cout << "ctrl_val = " << ctrl_val << endl;
				for(int i=0; i<cnt; ++i){
					//int val = mngFind.getVarVal(dirFind.getArr(cname, i));
					int val = get_bit(ctrl_val, i);
					it->setBit(i, (val==1) ? 1 : 0);			
				}
			}
			else
			{
				Assert(allInputsDag->get_node(cname)->getOtype() == OutType::FLOAT, "Otype is " + allInputsDag->get_node(cname)->getOtype()->str() + " but should be FLOAT.");
				int float_idx = floats.getIdx(outputControlFloats[cname]);
				sum = float_idx;
				int cnt = float_idx_size; 
				Assert((1<<cnt) > float_idx, "num bits for float idx is too small");
				Assert( cnt == it->size(), "find: SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
				cout << "float_idx = " << float_idx << " | ctrl_val = " << floats.getFloat(float_idx) << endl;
				for(int i=0; i<cnt; ++i){
					//int val = mngFind.getVarVal(dirFind.getArr(cname, i));
					int val = get_bit(float_idx, i);
					it->setBit(i, (val==1) ? 1 : 0);			
				}
			}
		}

		return true;
	}

	bool minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes) override
	{
		//do nothing
	}

	void declareControl(CTRL_node* cnode) override
	{
		//do nothing
	}

	void updateCtrlVarStore(VarStore& ctrlStore) override
	{
		if(allInputsDag == NULL)
		{
			return;
		}
		vector<bool_node*>& problemIn = allInputsDag->getNodesByType(bool_node::CTRL);
	    for(int i=0; i<problemIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);	
			if (ctrlnode->getOtype() == OutType::FLOAT) {
				string name = ctrlnode->get_name();
				if (!ctrlStore.contains(name)) {
					ctrlStore.newVar(name, float_idx_size, OutType::FLOAT);
				}
			}
		}
	}

	void retractAssumptions() override
	{
		//do nothing
	}
};

