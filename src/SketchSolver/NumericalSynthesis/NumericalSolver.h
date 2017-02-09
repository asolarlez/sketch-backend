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
#include "AutoDiff.h"
#include "RangeDiff.h"
#include "GradientDescent.h"
#include "IntervalPropagator.h"

class NumericalSolver;

class Parameters {
public:
  NumericalSolver* ns;
  vector<vector<int>> allInputs;
  vector<vector<int>> allOutputs;
};

class NumericalSolver : public Synthesizer {
  public: BooleanDAG* dag;
  map<string, int> ctrlMap; // maps ctrl names to indexes
  vector<float> ctrlVals; // Ordered list of ctrl values found by the numerical solver
  int ninputs;
  int noutputs;
  int ncontrols;
  GradientDescent* gd;
  map<string, BooleanDAG*> empty; // Empty function map for NodeEvaluator
  //map<string, int> inputs; // To check for repeated inputs
	gsl_vector* t;  // a temporary vector for storing intermediate results
	float threshold = 1e-10;
	int dcounter = 0;
	AutoDiff* eval;
	RangeDiff* evalR;
	
	map<int, IntervalPropagator*> propMap; // maps each example instance to an inteval propagator
	
  NumericalSolver(FloatManager& _fm, BooleanDAG* _dag) :Synthesizer(_fm), dag(_dag) {
    ninputs = dag->getNodesByType(bool_node::SRC).size();
    noutputs = (dynamic_cast<TUPLE_CREATE_node*>(dag->get_node("OUTPUT")->mother))->multi_mother.size();
    ncontrols = dag->getNodesByType(bool_node::CTRL).size();
    gd = new GradientDescent(ncontrols);
		t = gsl_vector_alloc(ncontrols);
    vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      ctrlMap[ctrls[i]->get_name()] = i;
      ctrlVals.push_back(0.0);
    }
		eval = new AutoDiff(empty, *dag, fm, ctrlMap);
		evalR = new RangeDiff(empty, *dag, fm, ctrlMap);
  }
	
	
	void genData(const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
		/*vector<vector<int>> allInputs;
		vector<vector<int>> allOutputs;
		// Fill in test inputs and outputs
		vector<int> inputs;
		for (int i = 0; i < ninputs; i++) {
			inputs.push_back(EMPTY);
		}
		allInputs.push_back(inputs);
		vector<int> outputs;
		for (int i = 0; i < noutputs; i++) {
			outputs.push_back(EMPTY);
		}
		outputs[noutputs-2] = 0;
		outputs[noutputs-1] = 0;
		allOutputs.push_back(outputs);*/
		
		gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		cout << dcounter << endl;
		ofstream file("/Users/Jeevu/projects/symdiff/scripts/test/t"+ to_string(dcounter++) +".txt");
		double i = 0.0;
		while (i < 15) {
			gsl_vector_set(state, 0, i);
			double err = evalWithGrad(state, d, allInputs, allOutputs);
			file << err << ";";
			i += 0.1;
		}
		file << endl;
		file.close();
	}
	
	void computeError(bool_node* node, gsl_vector* d, double& error) {
		float m1 = fm.getFloat(eval->getValue(node->mother));
		float m2 = fm.getFloat(eval->getValue(node->father));
		float diff = m1 - m2;
		if (diff == 0.0) diff = 0.1; // Min error
		error += pow(diff, 2);
		gsl_vector* d1 = eval->getGrad(node->mother);
		gsl_vector* d2 = eval->getGrad(node->father);
		gsl_vector_memcpy(t, d1);
		gsl_vector_sub(t, d2);
		gsl_vector_scale(t, 2*diff);
		gsl_vector_add(d, t);
	}
	
	void computeErrorRange(bool_node* node, float output, gsl_vector* d, double& error) {
		IntervalGrad* mrange = evalR->r(node->mother);
		IntervalGrad* frange = evalR->r(node->father);
		
		float m1 = mrange->getLow();
		float m2 = mrange->getHigh();
		float f1 = frange->getLow();
		float f2 = frange->getHigh();
		
		gsl_vector* md1 = mrange->getLGrad();
		gsl_vector* md2 = mrange->getHGrad();
		gsl_vector* fd1 = frange->getLGrad();
		gsl_vector* fd2 = frange->getHGrad();
		
		
		float v1 = 0.0;
		float v2 = 0.0;
		gsl_vector* d1;
		gsl_vector* d2;
		bool unsat = false;
		
		if (node->type == bool_node::EQ) {
			if (output == 1) {
				if (m2 < f1) {
					unsat = true;
					v1 = m2;
					v2 = f1;
					d1 = md2;
					d2 = fd1;
				}
				if (m1 > f2) {
					unsat = true;
					v1 = m1;
					v2 = f2;
					d1 = md1;
					d2 = fd2;
				}
			} else {
				if (m1 == m2 && m2 == f1 && f1 == f2) {
					unsat = true;
					v1 = m1;
					v2 = f1;
					d1 = md1;
					d2 = fd1;
				}
			}
		} else { // lt case
			if (output == 1) {
				if (m1 >= f2) {
					unsat = true;
					v1 = m1;
					v2 = f2;
					d1 = md1;
					d2 = fd2;
				}
			} else {
				if (m2 < f1) {
					unsat = true;
					v1 = m2;
					v2 = f1;
					d1 = md2;
					d2 = fd1;
				}
			}
		}
		
		if (unsat) {
			float diff = v1 - v2;
			if (diff == 0.0) { // TODO: check this
				error += 1.0; // min error
				for (int i = 0; i < ncontrols; i++ ) {
					gsl_vector_set(t, i, 1.0);
				}
				gsl_vector_add(d, t);
			}else {
				error += pow(diff, 2);
				gsl_vector_memcpy(t, d1);
				gsl_vector_sub(t, d2);
				gsl_vector_scale(t, 2*diff);
				gsl_vector_add(d, t);
			}
		}
	}
	
  double evalWithGrad(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
    for (int i = 0; i < ncontrols; i++ ) {
      gsl_vector_set(d, i, 0);
    }
    double error = 0;
		
    for (int i = 0; i < allInputs.size(); i++) {
      VarStore inputStore;
      // Collect all inputs (assumes inputs are not floats)
      vector<bool_node*> inputs = dag->getNodesByType(bool_node::SRC);
      for (int j = 0; j < allInputs[i].size(); j++) {
        if (allInputs[i][j] != EMPTY) {
          inputStore.setVarVal(inputs[j]->get_name(), allInputs[i][j]);
        }
      }
      // Collect all controls (assumes controls are floats)
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int j = 0; j < state->size; j++) {
        inputStore.setVarVal(ctrls[j]->get_name(), fm.getIdx(gsl_vector_get(state,j)));
      }
			
      if (false) { // Run auto differentiation on value - this requires all inputs and outputs to be non empty
        eval->run(inputStore);
				
        /*for (int k = 0; k < dag->size(); k++) {
            bool_node* n = (*dag)[k];
            cout << n->lprint() << endl;
            if (n->getOtype() == OutType::FLOAT) {
              cout << "Val: " << fm.getFloat(eval.getValue(n)) << endl;
            } else {
              cout << "Val: " << eval.getValue(n) << endl;
            }
            cout << "Grad: ";
            gsl_vector* g = eval.getGrad(n);
            for (int i = 0; i < g->size; i++) {
              cout <<  gsl_vector_get(g, i) << " ";
            }
            cout << endl;
        }*/
      

        bool_node* output = dag->get_node("OUTPUT")->mother;
        vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
        vector<int> outputs = eval->getTuple(eval->getValue(output));
        for (int j = 0; j < outputs.size(); j++) {
          if (allOutputs[i][j] != EMPTY && outputs[j] != allOutputs[i][j]) { // Assumes all outputs are of the form float1 op float2
						computeError(outputmothers[j], d, error);
          }
        }
				
				DllistNode* cur = dag->assertions.head;
				while(cur != NULL){
					bool_node* an = dynamic_cast<bool_node*>(cur);
					bool_node* sn = dynamic_cast<SRC_node*>(an->mother->mother);
					if(an->type == bool_node::ASSERT)	{
						string name = sn->get_name();
						if (inputStore.contains(name)) {
							int out = inputStore[name]; // this assumes that the src node is not a float type
							if (eval->getValue(an->mother->father) == out) {
								computeError(an->mother->father, d, error);
							}
						}
					}
					cur = cur->next;
				}
			}
			
      if (true) { // Run automatic differentiation on ranges
      evalR->run(inputStore);
      bool_node* output = dag->get_node("OUTPUT")->mother;
      vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
      //evalR->print();
      for (int j = 0; j < outputmothers.size(); j++) {
        if (allOutputs[i][j] != EMPTY) {
					computeErrorRange(outputmothers[j], allOutputs[i][j], d, error);
				}
      }
			DllistNode* cur = dag->assertions.head;
			while(cur != NULL){
				bool_node* an = dynamic_cast<bool_node*>(cur);
				bool_node* sn = dynamic_cast<SRC_node*>(an->mother->mother);
				if(an->type == bool_node::ASSERT)	{
					string name = sn->get_name();
					if (inputStore.contains(name)) {
						int out = inputStore[name]; // this assumes that the src node is not a float type
						computeErrorRange(an->mother->father, out, d, error);
					}
				}
				cur = cur->next;
			}
      }
    }
    //cout << "error: " << error << endl;
    //cout << "grad: ";
    for (int i = 0; i < d->size; i++) {
			float f = gsl_vector_get(d,i);
			if (!isfinite(f)) {
				gsl_vector_set(d, i, numeric_limits<float>::max()); //TODO: check this
			}
    }
    //cout << endl;
    return error;
  }


  static double eval_f(const gsl_vector* x, void* params) {}
  
  static void eval_df(const gsl_vector* x, void* params, gsl_vector* d) {}
  
  static void eval_fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
    Parameters* p = (Parameters*) params;
    *f = p->ns->evalWithGrad(x, df, p->allInputs, p->allOutputs);
  }
	
	bool_node* getNodeForInput(int inputid) {
		vector<bool_node*>& srcnodes = dag->getNodesByType(bool_node::SRC);
		bool_node* output = dag->get_node("OUTPUT")->mother;
		vector<bool_node*>& outputs = ((TUPLE_CREATE_node*)output)->multi_mother;
		if (inputid < ninputs) {
			return srcnodes[inputid];
		} else {
			int idx = inputid - ninputs;
			return outputs[idx];
		}
	}
	
	int getInputForNode(bool_node* node) {
		vector<bool_node*>& srcnodes = dag->getNodesByType(bool_node::SRC);
		bool_node* output = dag->get_node("OUTPUT")->mother;
		vector<bool_node*>& outputs = ((TUPLE_CREATE_node*)output)->multi_mother;
		if (node->type == bool_node::SRC) {
			auto it = find(srcnodes.begin(), srcnodes.end(), node);
			Assert(it != srcnodes.end(), "Something is wrong src");
			return it - srcnodes.begin();
		} else if (node->type == bool_node::CONST){
			return -1;
		} else if (node->type == bool_node::ASSERT) {
			return -1;
		} else {
			auto it = find(outputs.begin(), outputs.end(), node);
			Assert(it != outputs.end(), "Something is wrong output");
			return it - outputs.begin() + ninputs;
		}
	}
	
  virtual bool synthesis(int instance, int inputid, int val, int level) {
    conflict.clear();
    InputMatrix& im = *inout;
    
    
    vector<vector<int>> allInputs;
    vector<vector<int>> allOutputs;
    vector<int> conflictids;
    for (int i = 0; i < inout->getNumInstances(); ++i) {
      vector<int> inputs;
      vector<int> outputs;
      bool notset = false;
      for (int j = 0; j < ninputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          //break;
        }
        inputs.push_back(val);
      }
      //if (notset) continue;
      for (int j = ninputs; j < ninputs + noutputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          //break;
        }
        outputs.push_back(val);
      }
      //if (notset) continue;
      allInputs.push_back(inputs);
      allOutputs.push_back(outputs);
      conflictids.push_back(i);
    }
    
    Assert(allInputs.size() == allOutputs.size(), "This should not be possible");
    
    if (allInputs.size() == 0) return true;
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
    cout << "Output: ";
    for (int i = 0; i < allOutputs[k].size(); i++) {
			if (allOutputs[0][i] == EMPTY) {
				cout << "2,";
			} else {
				cout << allOutputs[0][i] << ",";
			}
    }
		cout << endl;
		}
		
		//genData(allInputs, allOutputs);
    //if (inputs.find(instr) != inputs.end()) {
    //  cout << "Repeated Input" << endl;
    //  inputs[instr]++;
    //} else {
    //  inputs[instr] = 1;
    //}
		
		// First perform interval propagation to detect any conflicts
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
			//cout << "IP CONFLICT" << endl;
			//cout << conflictNodes.size() << endl;
#endif
			for (int i = 0 ; i < conflictNodes.size(); i++) {
				int iid = getInputForNode(conflictNodes[i]);
 				if (iid >= 0) {
#if IP_DEBUG
					//cout << conflictNodes[i]->lprint() << " "  << iid << endl;
#endif
					conflict.push(im.valueid(instance, iid));
				}
			}
			cout << "***** " << "CONFLICT (IP)" << " ******" << endl;
			return false;
		}
		// TODO: we can intersect the intervals for the ctrl nodes for all instances of interval propagators to detect further conflicts
		// TODO: use the intervals for ctrl nodes during the gradient descent
		
		// If no conflict is found, do gradient descent on intervals
    gd->init(this, allInputs, allOutputs);
    double minError = gd->optimize();
    gsl_vector* curState = gd->getResults();
		
		//evalR->print();
    if (minError >= -threshold && minError <= threshold) {
      // Update the controls
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int i = 0; i < ctrls.size(); i++) {
        ctrlVals[ctrlMap[ctrls[i]->get_name()]] = gsl_vector_get(curState, i);
      }
      return true;
    } else {
      cout << "******************* Found a conflict *******************" << endl;
      generateConflict(conflictids);
     return false;
    }
  }
	
	void generateConflict(const vector<int>& conflictids) {
		InputMatrix& im = *inout;
		for(auto conflictId: conflictids) {
			for (int j = 0; j < ninputs + noutputs; j++) {
				if (im.getVal(conflictId, j) != EMPTY) {
					//cout << "pushing " << j << endl;
					conflict.push(im.valueid(conflictId, j));
				}
			}
		}
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
			// TODO: set any ranges for ctrl nodes
		}
		
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
		map<string, bool_node*> srcMap;
		vector<bool_node*>& srcNodes = dag->getNodesByType(bool_node::SRC);
		for (int i = 0; i < srcNodes.size(); i++) {
			srcMap[srcNodes[i]->get_name()] = params[i];
		}
    for (int i = 0; i < newdag.size(); i++) {
      if (newdag[i]->type == bool_node::SRC) {
        newdag.replace(i, srcMap[newdag[i]->get_name()]);
      } else if (newdag[i]->type == bool_node::CTRL) {
        newdag.replace(i, dopt->getCnode(ctrlVals[ctrlMap[newdag[i]->get_name()]]));
      } else {
        if (newdag[i]->type != bool_node::DST) {
          bool_node* n = dopt->computeOptim(newdag[i]);
          if (n == newdag[i]) {
            dopt->addNode(n);
          }
          if (newdag[i] != n) {
            newdag.replace(i, n);
          }
        }
      }
    }
    return newdag.get_node("OUTPUT")->mother;
  }
  
  virtual void print(ostream& out) {
    vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      out << ctrls[i]->get_name() << ":" << ctrlVals[ctrlMap[ctrls[i]->get_name()]] << endl;
    }
  }
  
  virtual void getControls(map<string, string>& values) {
    vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      stringstream str;
      str << ctrlVals[ctrlMap[ctrls[i]->get_name()]];
      values[ctrls[i]->get_name()] = str.str();
    }
  }
};
