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
	FloatManager &floats;
	BooleanDAG *allInputsDag = nullptr;

public:
	explicit CEGISFinderSpec(FloatManager &_floats) : floats(_floats) {}
	virtual SATSolverResult find(BooleanDAG *problem, VarStore &controls, bool hasInputChanged, unsigned long long max_finder_solve_timeout_in_microseconds)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
		return UNSPECIFIED;
	}

	virtual bool minimizeHoleValue(VarStore &ctrlStore, vector<string> &mhnames, vector<int> &mhsizes)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
		return false;
	}

	virtual void declareControl(CTRL_node *cnode)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
	}

	virtual void updateCtrlVarStore(VarStore &ctrlStore)
	{
		Assert(false, "CEGISFinderSpec is just an interface.");
	}

	virtual void retractAssumptions(){
		Assert(false, "CEGISFinderSpec is just an interface.")}

	FloatManager &get_floats()
	{
		return floats;
	}

	BooleanDAG *&get_all_inputs_dag()
	{
		return allInputsDag;
	}

	virtual void clear()
	{
		assert(false);
	}
};

class CEGISFinder : public CEGISFinderSpec
{
	vector<Tvalue> find_node_ids;
	vector<Tvalue> find_history;
	bool stoppedEarly;
	CEGISParams params;
	SolverHelper &dirFind;
	SATSolver &mngFind;

	void addInputsToTestSet(BooleanDAG *problem, VarStore &input);
	void addProblemToTestSet(BooleanDAG *problem);

public:
	CEGISFinder(FloatManager &_floats, SolverHelper &_dirFind, SATSolver &_mngFind, CommandLineArgs &args) : CEGISFinderSpec(_floats), dirFind(_dirFind), mngFind(_mngFind), params(args)
	{
	}

	void clear() override
	{
		find_node_ids.clear();
		find_history.clear();
		delete this;
	}

	SATSolverResult find(BooleanDAG *problem, VarStore &controls, bool hasInputChanged, unsigned long long max_finder_solve_timeout_in_microseconds) override;

	bool minimizeHoleValue(VarStore &ctrlStore, vector<string> &mhnames, vector<int> &mhsizes) override;

	void declareControl(CTRL_node *cnode) override
	{
		dirFind.declareControl(cnode);
	}

	void updateCtrlVarStore(VarStore &ctrlStore) override
	{
		for (auto it = dirFind.arrsize_begin(); it != dirFind.arrsize_end(); ++it)
		{
			AssertDebug(ctrlStore.contains(it->first), "It seems like ctrlStore should already contain all ctrls bc VarStore is a reference, probably initialized before calling this function; not sure why this is necessary. IF THIS ASSERT FAILS examine why are certain holes not declared ahead of time. ");
			ctrlStore.newVar(it->first, it->second, dirFind.getOtype(it->first), dirFind.get_type(it->first), dirFind.get_original_name(it->first), dirFind.get_source_dag_name(it->first));
		}
	}

	void retractAssumptions() override
	{
		dirFind.getMng().retractAssumptions();
	}
};


class CEGISFinderNumerical : public CEGISFinderSpec
{
	static const int float_idx_size = 18;

	REASSolver *reasSolver = NULL;

	void clear() override
	{
		delete reasSolver;
	}

	SATSolverResult assertDAGNumerical(
		BooleanDAG *dag, map<string, string> &currentControls, map<string, int> &currentControlInts, map<string, float> &currentControlFloats)
	{

		cout << "CEGISFinderNumerical::assertDAGNumerical" << endl;

		bool use_int_to_float_rewrite = true; // can be made into an input param. 
		BooleanDAG *new_dag = nullptr;
		IntToFloatRewriteDag *rewriter = nullptr;
		if (use_int_to_float_rewrite) {
			cout << "[assertDAGNumerical] Rewriting dag from int to float using IntToFloatRewriteDag" << endl;
			rewriter = new IntToFloatRewriteDag(*dag->clone(), floats);
			new_dag = rewriter->rewrite();
		}
		else {
			cout << "[assertDAGNumerical] Using DagOptim. NOT using IntToFloatRewriteDag" << endl;
			new_dag = dag->clone();
			DagOptim dagOptim = DagOptim(*new_dag, floats);
			dagOptim.process(*new_dag);
		}

		reasSolver->addProblem(new_dag);

		int solveCode = 0;
		try
		{
			solveCode = reasSolver->solve();
			if (use_int_to_float_rewrite) {
				assert(rewriter != nullptr);
				currentControls = rewriter->extract_result_typed(reasSolver->get_result(), reasSolver->get_ctrls(), currentControlInts, currentControlFloats);
			}
			else {
				reasSolver->get_control_map_and_floats(currentControls, currentControlFloats);
			}
		}
		catch (SolverException *ex)
		{
			cout << "ERROR " /*<<basename()*/ << ": " << ex->code << "  " << ex->msg << endl;
			return SAT_UNSATISFIABLE; // ex->code + 2;
		}
		catch (BasicError &be)
		{
			if (use_int_to_float_rewrite) {
				assert(rewriter != nullptr);
				currentControls = rewriter->extract_result_typed(reasSolver->get_result(), reasSolver->get_ctrls(), currentControlInts, currentControlFloats);
			}
			else {
				reasSolver->get_control_map_and_floats(currentControls, currentControlFloats);
			}
			cout << "ERROR: " /*<<basename()*/ << endl;
			return SAT_UNSATISFIABLE;
		}
		if (!solveCode)
		{
			return SAT_UNSATISFIABLE;
		}

		return SAT_SATISFIABLE;
	}

	static int get_bit(int bitstring, int idx)
	{
		return (bitstring & (1 << idx)) != 0;
	}

public:
	CEGISFinderNumerical(FloatManager &_floats, ostream &out) : CEGISFinderSpec(_floats)
	{
		reasSolver = new REASSolver(floats);
	}

	SATSolverResult find(BooleanDAG *problem, VarStore &controls, bool hasInputChanged, unsigned long long _unused_timeout = numeric_limits<unsigned long long>::max()) override
	{
		cout << "CEGISFinderNumerical::find: hasInputChanged: " << hasInputChanged << endl;
		if (hasInputChanged)
		{
			BooleanDAG *newdag = problem->clone();
			// Assert that all nodes in newdag and allInputsDag have unique ids
			std::set<int> uniqueIds;
			

			if (allInputsDag == nullptr)
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
			Assert(problem == nullptr, "We expect problem to be null.");
			// claim: solution is already optimal.
			assert(allInputsDag->get_failed_assert() == nullptr);
			AssertDebug(false, "THIS NEEDS TO BE CONFIRMED THAT IT'S WHAT WE WANT IF YOU CAN EVEN GET HERE.");
			return SAT_SATISFIABLE;
		}

		map<string, string> outputControls;
		map<string, int> outputControlInts;
		map<string, float> outputControlFloats;

		SATSolverResult result =
			assertDAGNumerical(allInputsDag, outputControls, outputControlInts, outputControlFloats);

		if (result != SAT_TIME_OUT && result != SAT_SATISFIABLE)
		{
			return result;
		}

		updateCtrlVarStore(controls);

		long long sum = 0;
		// Save outputControls in (VarStore) controls
		for (auto it = controls.begin(); it != controls.end(); ++it)
		{
			const string &cname = it->get_name();
			cout << "cname = " << cname << endl;
			auto node = allInputsDag->unchecked_get_node(cname);
			if (node == nullptr) {
				cout << "warning: node " << cname << " not found" << endl;
				continue;
			}
			OutType *out_type = node->getOtype();
			cout << "out_type = " << out_type->str() << endl;
			// int cnt = dirFind.getArrSize(cname);
			assert(node->isArrType() == false);
			if (out_type == OutType::INT || out_type == OutType::BOOL)
			{
				INTER_node *inter_node = (INTER_node *)node;
				int cnt = inter_node->get_nbits();
				AssertDebug(cnt == it->element_size(), "find: SIZE MISMATCH: " + std::to_string(cnt) + " != " + std::to_string(it->element_size()));
				int ctrl_val = outputControlInts[cname];
				cout << "ctrl_val = " << ctrl_val << endl;
				for (int i = 0; i < cnt; ++i)
				{
					// int val = mngFind.getVarVal(dirFind.getArr(cname, i));
					int val = get_bit(ctrl_val, i);
					it->set_bit(i, (val == 1) ? 1 : 0);
				}
			}
			else
			{
				Assert(node->getOtype() == OutType::FLOAT, "Otype is " + node->getOtype()->str() + " but should be FLOAT.");
				int float_idx = floats.getIdx(outputControlFloats[cname]);
				sum = float_idx;
				int cnt = float_idx_size;
				Assert((1 << cnt) > float_idx, "num bits for float idx is too small");
				AssertDebug(cnt == it->element_size(), "find: SIZE MISMATCH: " + std::to_string(cnt) + " != " + std::to_string(it->element_size()));
				cout << "float_idx = " << float_idx << " | ctrl_val = " << floats.getFloat(float_idx) << endl;
				for (int i = 0; i < cnt; ++i)
				{
					// int val = mngFind.getVarVal(dirFind.getArr(cname, i));
					int val = get_bit(float_idx, i);
					it->set_bit(i, (val == 1) ? 1 : 0);
				}
			}
		}

		return result;
	}

	bool minimizeHoleValue(VarStore &ctrlStore, vector<string> &mhnames, vector<int> &mhsizes) override
	{
		// do nothing
		AssertDebug(false, "TODO: return something meaningful here. Not sure what needs to be returned here.");
	}

	void declareControl(CTRL_node *cnode) override
	{
		// do nothing
	}

	void updateCtrlVarStore(VarStore &ctrlStore) override
	{
		if (allInputsDag == nullptr)
		{
			return;
		}
		auto problemIn = allInputsDag->getNodesByType(bool_node::CTRL);
		for (int i = 0; i < problemIn.size(); ++i)
		{
			CTRL_node *ctrlnode = dynamic_cast<CTRL_node *>(problemIn[i]);
			string name = ctrlnode->get_name();
			AssertDebug(ctrlStore.contains(name), "It seems like ctrlStore should already contain all ctrls bc VarStore is a reference, probably initialized before calling this function; not sure why this is necessary. IF THIS ASSERT FAILS examine why are certain holes not declared ahead of time. ");
			if (ctrlnode->getOtype() == OutType::FLOAT)
			{
				ctrlStore.newVar(name, float_idx_size, OutType::FLOAT, bool_node::CTRL, ctrlnode->get_original_name(), ctrlnode->get_source_dag_name());
			}
		}
	}

	void retractAssumptions() override
	{
		// do nothing
	}
};
