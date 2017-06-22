#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <utility>
#include <Sort.h>
#include <math.h>

using namespace std;

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"
#include "RangeDiff.h"
#include "GradientDescent.h"
#include "IntervalPropagator.h"
#include "SimpleEvaluator.h"
#include "GlobalEvaluator.h"



class NumericalSolver : public Synthesizer {
	BooleanDAG* dag; // DAG containing the numerical abstraction
	int ninputs;
	int ncontrols;
  vector<float> ctrlVals; // Ordered list of ctrl values found by the numerical solver
	map<string, int> ctrlMap; // maps ctrl names to indexes
	map<int, int> imap; // Maps inputs to actual nodes in the dag

  GradientDescent* gd;
	
	// Different kinds of evaluators
	SimpleEvaluator* eval;
	RangeDiff* evalR;
	GlobalEvaluator* evalG;
	
	gsl_vector* prevState; // keeps track of previous iteration best ctrl values to use it as the init state for the next iteration
	gsl_vector* t;// temp vector to store intermediate results
	
	float threshold = 1e-5; // accuracy for minimizing the error
	int MAX_TRIES = 9; // Number retries of GD algorithm for each iteration
	float cthresh = 0.01; // threshold for approximate conflict detection
	
	map<int, IntervalPropagator*> propMap; // maps each example instance to an inteval propagator
	
	// Parameters when trying to just minimize the error (instead of making error = 0)
	bool onlyOptimize = false;
	float minErrorSoFar;
	int counter = 0;
	
	// used for debugging
	int dcounter = 0;

public:
	NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	
	double evalLocal(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs);
	double simpleEval(const gsl_vector* state, const vector<vector<int>>& allInputs);
	double evalAll(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs);
	
	IntervalPropagator* createPropagator();
	bool doIntervalProp(int instance, int inputid, int val, int level);
	
	bool doGradientDescent(const vector<vector<int>>& allInputs, const vector<int>& conflictids, int instance, int inputid);
	
	void generateConflict(const vector<int>& conflictids);
	void collectSuggestions(vec<Lit>& suggestions, const vector<vector<int>>& allInputs, const vector<int>& conflictids);
	
	virtual bool synthesis(int instance, int inputid, int val, int level, vec<Lit>& suggestions);
	virtual void newInstance() { // Note: this method is called before adding the new instance to the input output matrix
		// Create a new IntervalPropagator for the instance
		//int idx = inout->getNumInstances();
		//propMap[idx] = createPropagator();
	}
	virtual void finalize() {}
	virtual void backtrack(int level) {
		for (auto it = propMap.begin(); it != propMap.end(); it++) {
			it->second->cancelUntil(level);
		}
	}
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params);
  virtual void print(ostream& out) {
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
      out << it->first << ":" << ctrlVals[it->second] << endl;
    }
  }
  virtual void getControls(map<string, string>& values) {
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
      stringstream str;
      str << ctrlVals[it->second];
      values[it->first] = str.str();
    }
  }
	
	bool_node* getNodeForInput(int inputid) {
		return (*dag)[imap[inputid]];
	}
	
	int getInputForNode(bool_node* node) {
		if (node->type == bool_node::CONST){
			return -1;
		} else if (node->type == bool_node::ASSERT) {
			return -1;
		} else if (node->type == bool_node::CTRL) {
			return -1;
		} else {
			for (int i = 0; i < ninputs; i++) {
				if (imap[i] == node->id) {
					return i;
				}
			}
			Assert(false, "Something is wrong output " + node->lprint());
		}
	}
	
	void collectAllInputs(vector<vector<int>>& allInputs, vector<int>& conflictids) {
		for (int i = 0; i < inout->getNumInstances(); ++i) {
			vector<int> inputs;
			for (int j = 0; j < ninputs; j++) {
				int val = inout->getVal(i, j);
				inputs.push_back(val);
			}
			allInputs.push_back(inputs);
			conflictids.push_back(i);
		}
		Assert(allInputs.size() == conflictids.size(), "This should not be possible");
	}
	
	void printInputs(vector<vector<int>>& allInputs) {
		for (int k = 0; k < allInputs.size(); k++) {
			cout << "Input: ";
			for (int i = 0; i < allInputs[k].size(); i++) {
				if (allInputs[0][i] == EMPTY) {
					cout << "2,";
				} else {
					cout << allInputs[0][i] << ",";
				}
			}
			cout << endl;
		}
	}

	// Debugging: prints out the data to generate graphs
	void genData2D(const vector<vector<int>>& allInputs) {
		gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		cout << "Counter " <<  dcounter << endl;
		ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/dist/g"+ to_string(dcounter++) +".txt");
		{
			double i = -15.0;
			double j = -15.0;
			while (i < 15.0) {
				j = -15.0;
				while (j < 15.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				double err = evalLocal(state, d, allInputs);
				cout << i << " " << j << " " << err << endl;
				file << err << ";";
					j+=0.1;
				}
				i += 0.1;
			}
		}
		file << endl;
		{
			double i = -15.0;
			double j = -15.0;
			while (i < 15.0) {
				cout << i << endl;
				j = -15.0;
				while(j < 15.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				double err = simpleEval(state, allInputs);
				file << err << ";";
					j+=0.1;
				}
				i += 0.1;
			}
			file << endl;
			file.close();
		}
	}
	
	void genData1D(const vector<vector<int>>& allInputs) {
		gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		cout << "Counter " <<  dcounter << endl;
		ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/dist/g"+ to_string(dcounter++) +".txt");
		{
			double i = -30.0;
			while (i < 30.0) {
				gsl_vector_set(state, 0, i);
				double err = evalLocal(state, d, allInputs);
				cout << i << " " << err << endl;
				file << err << ";";
				i += 0.01;
			}
		}
		file << endl;
		{
			double i = -30.0;
			while (i < 30.0) {
				gsl_vector_set(state, 0, i);
				double err = simpleEval(state, allInputs);
				cout << i << " " << err << endl;
				file << err << ";";
				i += 0.01;
			}
			file << endl;
			file.close();
		}
	}
};

class GDParameters {
	public:
	NumericalSolver* ns;
	vector<vector<int>> allInputs;
};

class GDEvaluator {
	static gsl_vector* curGrad;
	public:
	
	static void init(int size) {
		curGrad = gsl_vector_alloc(size);
	}
	
	static double f(const gsl_vector* x, void* params) {
		GDParameters* p = (GDParameters*) params;
		return p->ns->evalLocal(x, curGrad, p->allInputs);
	}
	
	static void df(const gsl_vector* x, void* params, gsl_vector* d) {
		// just use the curGrad - assuming that this function is called immediately after calling eval_f with the same arguments
		gsl_vector_memcpy(d, curGrad);
	}
	
	static void fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
		GDParameters* p = (GDParameters*) params;
		*f = p->ns->evalLocal(x, df, p->allInputs);
	}
	
	
};
