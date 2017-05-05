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

class NumericalSolver;

class Parameters {
public:
  NumericalSolver* ns;
  vector<vector<int>> allInputs;
};

class NumericalSolver : public Synthesizer {
public:
	BooleanDAG* dag;
  map<string, int> ctrlMap; // maps ctrl names to indexes
  vector<float> ctrlVals; // Ordered list of ctrl values found by the numerical solver
  int ninputs;
  int ncontrols;
  GradientDescent* gd;
	float threshold = 1e-5;
	int dcounter = 0;
	SimpleEvaluator* eval;
	RangeDiff* evalR;
	GlobalEvaluator* evalG;
	gsl_vector* prevState;
	int MAX_TRIES = 5;
	float cthresh = 0.01;
	map<int, int> imap; // Maps inputs to actual nodes in the dag
	map<int, IntervalPropagator*> propMap; // maps each example instance to an inteval propagator
	gsl_vector* t; // temp vector
  NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap) :Synthesizer(_fm), dag(_dag), imap(_imap) {
		//dag->lprint(cout);
		//cout << "Input mapping " << endl;
		//for (auto it = imap.begin(); it != imap.end(); it++) {
		//	cout << it->first << " -> " << (*dag)[it->second]->lprint() << endl;
		//}
    ninputs = imap.size();
    vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
			if (ctrls[i]->getOtype() == OutType::FLOAT) {
				ctrlMap[ctrls[i]->get_name()] = ctrlVals.size();
				ctrlVals.push_back(0.0);
			}
    }
		ncontrols = ctrlVals.size();
		if (ncontrols == 0) ncontrols = 1;
		gd = new GradientDescent(ncontrols);
		
		evalR = new RangeDiff(*dag, fm, ctrlMap);
		eval = new SimpleEvaluator(*dag, fm);
		evalG = new GlobalEvaluator(*dag, fm, ctrlMap);
		
		prevState = gsl_vector_alloc(ncontrols);
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(prevState, i, 0);
		}
		t = gsl_vector_alloc(ncontrols);
  }
	
	
	void genData(const vector<vector<int>>& allInputs) {
		gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		cout << "Counter " <<  dcounter << endl;
		ofstream file("/Users/Jeevu/projects/symdiff/scripts/parallelPark/graphs/t"+ to_string(dcounter++) +".txt");
		{
		double i = 0.0;
		double j = 0.0;
		while (i < 10.0) {
			//cout << i << endl;
			j = 0.0;
			while (j < 10.0) {
			gsl_vector_set(state, 0, i);
			gsl_vector_set(state, 1, j);
			double err = evalLocal(state, d, allInputs);
			file << err << ";";
			j+= 0.1;
			}
			i += 0.1;
		}
		}
		file << endl;
		{
		double i = 0.0;
		double j = 0.0;
		while (i < 10.0) {
			//cout << i << endl;
			j = 0.0;
			while (j < 10.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				double err = simpleEval(state, allInputs);
				file << err << ";";
				j+= 0.1;
			}
			i += 0.1;
		}
		file << endl;
		file.close();
		}
	}

	
  double evalLocal(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
    for (int i = 0; i < ncontrols; i++ ) {
      gsl_vector_set(d, i, 0);
    }
		double error = 0;
    for (int i = 0; i < allInputs.size(); i++) {
			VarStore ctrlStore;
			map<int, int> inputValues; // maps node id to value set by the sat solver
			// Collect all inputs (assumes inputs are not floats)
      for (int j = 0; j < allInputs[i].size(); j++) {
        if (allInputs[i][j] != EMPTY) {
					inputValues[imap[j]] = allInputs[i][j];
        }
      }
      // Collect all controls (assumes controls are floats)
			for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
        ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
      }
			
      // Run automatic differentiation on ranges
      error += evalR->run(ctrlStore, inputValues, d);
      //evalR->print();
		}
		//cout << "State: " << gsl_vector_get(state, 0) << " " << gsl_vector_get(state, 1) << " " << gsl_vector_get(state, 2) << endl;
		//cout << "Error: " << error << endl;
		//cout << "Grad: " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << " " << gsl_vector_get(d, 2) << endl;
    return error;
  }

	double simpleEval(const gsl_vector* state, const vector<vector<int>>& allInputs) {
		double error = 0;
		for (int i = 0; i < allInputs.size(); i++) {
			VarStore ctrlStore;
			map<int, int> inputValues; // maps node id to value set by the sat solver
			// Collect all inputs (assumes inputs are not floats)
			for (int j = 0; j < allInputs[i].size(); j++) {
				if (allInputs[i][j] != EMPTY) {
					inputValues[imap[j]] = allInputs[i][j];
				}
			}
			// Collect all controls (assumes controls are floats)
			for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
				ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
			}
			
			// Run automatic differentiation on ranges
			error += eval->run1(ctrlStore, inputValues);
		}
		return error;
	}

	double evalAll(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
		for (int i = 0; i < ncontrols; i++ ) {
			gsl_vector_set(d, i, 0);
		}
		double error = 0;
		for (int i = 0; i < allInputs.size(); i++) {
			VarStore ctrlStore;
			map<int, int> inputValues; // maps node id to value set by the sat solver
			// Collect all inputs (assumes inputs are not floats)
			for (int j = 0; j < allInputs[i].size(); j++) {
				if (allInputs[i][j] != EMPTY) {
					inputValues[imap[j]] = allInputs[i][j];
				}
			}
			// Collect all controls (assumes controls are floats)
			for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
				ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
			}
			
			// Run automatic differentiation on ranges
			error += evalG->run(ctrlStore, inputValues, d, state);
			//evalR->print();
		}
		//cout << "State: " << gsl_vector_get(state, 0) << " " << gsl_vector_get(state, 1) << " " << gsl_vector_get(state, 2) << endl;
		//cout << "Error: " << error << endl;
		//cout << "Grad: " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << " " << gsl_vector_get(d, 2) << endl;
		return error;
	}

  static double eval_f(const gsl_vector* x, void* params) {}
  
  static void eval_df(const gsl_vector* x, void* params, gsl_vector* d) {}
  
  static void eval_fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
    Parameters* p = (Parameters*) params;
    *f = p->ns->evalLocal(x, df, p->allInputs);
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
	
	bool doIntervalProp(int instance, int inputid, int val, int level) {
		IntervalPropagator* iprop = propMap[instance];
		bool_node* node = getNodeForInput(inputid);
		float fval = (node->getOtype() == OutType::FLOAT) ? fm.getFloat(val) : (float) val;
#if IP_DEBUG
		cout << "IP: " << node->lprint() << " " << fval << endl;
#endif
		bool success = iprop->setInterval(*node, fval, fval, level);
		if (!success) {
			vector<bool_node*>& conflictNodes = iprop->conflictNodes;
#if IP_DEBUG
			cout << "IP conflict nodes" << endl;
			cout << conflictNodes.size() << endl;
#endif
			for (int i = 0 ; i < conflictNodes.size(); i++) {
				int iid = getInputForNode(conflictNodes[i]);
				if (iid >= 0) {
#if IP_DEBUG
					cout << conflictNodes[i]->lprint() << " "  << iid << endl;
#endif
					conflict.push(inout->valueid(instance, iid));
				}
			}
			cout << "***** " << "CONFLICT (IP)" << " ******" << endl;
			return false;
		}
		return true;
		
		// TODO: we can intersect the intervals for the ctrl nodes for all instances of interval propagators to detect further conflicts
		// TODO: use the intervals for ctrl nodes during the gradient descent
	}
	
	bool doGradientDescent(const vector<vector<int>>& allInputs, const vector<int>& conflictids, int instance, int inputid) {
		gd->init(this, allInputs, prevState);
		double minError = gd->optimize();
		gsl_vector* curState = gd->getResults();
		int numtries = 0;
		while (minError > threshold  && numtries < MAX_TRIES) {
			cout << "Retry attempt: " << (numtries+1) << endl;
			// redo gradient descent with random initial point
			for (int i = 0; i < ncontrols; i++) {
				float r = -10.0 + (rand()%200)/10.0; // random number between 0 to 10.0  TODO: don't hardcode
				gsl_vector_set(t, i, r);
				cout << r << "; ";
			}
			cout << endl;
			
			gd->init(this, allInputs, t);
			minError = gd->optimize();
			curState = gd->getResults();
			numtries++;
		}
		
		//evalR->print();
		if (minError <= threshold) {
			prevState = curState;
			// Update the controls
			for (int i = 0; i < ncontrols; i++) {
				ctrlVals[i] = gsl_vector_get(curState, i);
			}
			return true;
		} else {
			cout << "******************* Found a conflict *******************" << endl;
			generateConflict(conflictids);
			return false;
		}
	}
	
	void generateConflict(const vector<int>& conflictids) {
		for(auto conflictId: conflictids) {
			for (int j = 0; j < ninputs; j++) {
				if (inout->getVal(conflictId, j) != EMPTY) {
					conflict.push(inout->valueid(conflictId, j));
				}
			}
		}
	}
	
	void collectSuggestions(vec<Lit>& suggestions, const vector<vector<int>>& allInputs, const vector<int>& conflictids) {
		gsl_vector* d = gsl_vector_alloc(ncontrols);
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(d, i, 0.0);
		}
		for (int i = 0; i < allInputs.size(); i++) {
			VarStore ctrlStore;
			// Collect all controls (assumes controls are floats)
			for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
				ctrlStore.setVarVal(it->first, fm.getIdx(ctrlVals[it->second]));
			}
			vector<tuple<float, int, int>> s = eval->run(ctrlStore, imap);
			sort(s.begin(), s.end());
			reverse(s.begin(), s.end());
			for (int k = 0; k < s.size(); k++) {
				int idx = get<1>(s[k]);
				if (allInputs[i][idx] == EMPTY) {
					//cout << "Suggesting " << idx << " " << get<2>(s[k]) <<  " " << get<0>(s[k]) << endl;
					suggestions.push(getLit(inout->valueid(conflictids[i], idx), get<2>(s[k])));
				}
			}
		}
	}
	
  virtual bool synthesis(int instance, int inputid, int val, int level, vec<Lit>& suggestions) {
    conflict.clear();
		suggestions.clear();
    vector<vector<int>> allInputs;
    vector<int> conflictids;
		/*vector<int> inputs;
		int arr[100] = {1,2,2,2,2,0,2,2,2,2,0,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0};
		for (int i = 0; i < ninputs; i++) {
			if (arr[i] == 2) {
				inputs.push_back(EMPTY);
			} else {
				inputs.push_back(arr[i]);
			}
		}
		allInputs.push_back(inputs);*/
		collectAllInputs(allInputs, conflictids);
		if (allInputs.size() == 0) return true;
		printInputs(allInputs);
		
		/*gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		gsl_vector_set(state, 0, 0);
		gsl_vector_set(state, 1, 0.5);
		cout << evalLocal(state, d, allInputs) << endl;*/
		//genData(allInputs);
		// First, do interval propagation to detect any conflicts
		if (!doIntervalProp(instance, inputid, val, level)) {
				return false;
		}
		// If no conflict is found, do gradient descent
		bool sat = doGradientDescent(allInputs, conflictids, instance, inputid);
		if (sat) {
			collectSuggestions(suggestions, allInputs, conflictids);
		}
		return sat;
	}
	
	IntervalPropagator* createPropagator() {
		IntervalPropagator* iprop = new IntervalPropagator(*dag);
		
		// Initialize constants, assertions and ctrl nodes
		for (int i = 0; i < dag->size(); i++) {
			bool_node* n = (*dag)[i];
			if (n->type == bool_node::CONST) {
				float fval;
				if (n->getOtype() == OutType::FLOAT) {
					fval = ((CONST_node*) n)->getFval();
				} else {
					fval = ((CONST_node*) n)->getVal();
				}
				Assert(iprop->setInterval(*n, fval, fval, 0), "Setting constant interval failed");
			}
			if(n->type == bool_node::ASSERT) {
				// set the range of mother to 1
				Assert(iprop->setInterval(*n, 1, 1, 0), "Setting assert failed");
			}
			if (n->type == bool_node::CTRL) {
				if (n->getOtype() == OutType::FLOAT) {
					Assert(iprop->setInterval(*n, -32.0, 32.0, 0), "Setting ctrl range failed"); // TODO: Don't hardcode the range here
				}
			}
		}
		iprop->processAllNodes();
		return iprop;
	}
	
	// Note: this method is called before adding the new instance to the input output matrix
  virtual void newInstance() {
		// Create a new IntervalPropagator for the instance
		int idx = inout->getNumInstances();
		propMap[idx] = createPropagator();
	}
  
  virtual void finalize() {}
	
	virtual void backtrack(int level) {
		for (auto it = propMap.begin(); it != propMap.end(); it++) {
			it->second->cancelUntil(level);
		}
	}
  
  virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
    // Add the appropriate expression from the dag after replacing inputs with params and ctrls with synthesized parameters
    BooleanDAG newdag = *(dag->clone());
    for (int i = 0; i < newdag.size(); i++) {
      if (newdag[i]->type == bool_node::CTRL) {
				// TODO: what to do with non float ctrls that are solved by the SAT solver??
        newdag.replace(i, dopt->getCnode(ctrlVals[ctrlMap[newdag[i]->get_name()]]));
      } else {
				if (newdag[i]->type != bool_node::DST && newdag[i]->type != bool_node::SRC) {
          bool_node* n = dopt->computeOptim(newdag[i]);
          if (n == newdag[i]) {
            dopt->addNode(n);
          }
          if (newdag[i] != n) {
            newdag.replace(i, n);
					}
					if (n->type == bool_node::ASSERT) {
						dopt->addAssert((ASSERT_node*)n);
					}
					
        }
      }
    }
		return dopt->getCnode(0);
  }
  
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
};
