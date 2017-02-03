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
  }
	
  double evalWithGrad(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
    for (int i = 0; i < ncontrols; i++ ) {
      gsl_vector_set(d, i, 0);
    }
    double error = 0;
    AutoDiff eval(empty, *dag, fm, ctrlMap);
    RangeDiff evalR(empty, *dag, fm, ctrlMap);
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
        eval.run(inputStore);
				
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
        vector<int> outputs = eval.getTuple(eval.getValue(output));
        for (int j = 0; j < outputs.size(); j++) {
          if (allOutputs[i][j] != EMPTY && outputs[j] != allOutputs[i][j]) { // Assumes all outputs are of the form float1 op float2
            float m1 = fm.getFloat(eval.getValue(outputmothers[j]->mother));
            float m2 = fm.getFloat(eval.getValue(outputmothers[j]->father));
            float diff = m1 - m2;
            if (diff == 0.0) diff = 0.1; // Min error
            error += pow(diff, 2);
            gsl_vector* d1 = eval.getGrad(outputmothers[j]->mother);
            gsl_vector* d2 = eval.getGrad(outputmothers[j]->father);
            gsl_vector_memcpy(t, d1);
            gsl_vector_sub(t, d2);
            gsl_vector_scale(t, 2*diff);
            gsl_vector_add(d, t);
          }
        }
      }
			
      if (true) { // Run automatic differentiation on ranges
      evalR.run(inputStore);
      bool_node* output = dag->get_node("OUTPUT")->mother;
      vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
      //evalR.print();
      for (int j = 0; j < outputmothers.size(); j++) {
        if (allOutputs[i][j] != EMPTY) {
          pair<int, int> mrange = evalR.r(outputmothers[j]->mother);
          pair<int, int> frange = evalR.r(outputmothers[j]->father);
          
          float m1 = fm.getFloat(mrange.first);
          float m2 = fm.getFloat(mrange.second);
          float f1 = fm.getFloat(frange.first);
          float f2 = fm.getFloat(frange.second);
					
          gsl_vector* md1 = evalR.getLGrad(outputmothers[j]->mother);
          gsl_vector* md2 = evalR.getHGrad(outputmothers[j]->mother);
          gsl_vector* fd1 = evalR.getLGrad(outputmothers[j]->father);
          gsl_vector* fd2 = evalR.getHGrad(outputmothers[j]->father);
          
          
          float v1 = 0.0;
          float v2 = 0.0;
          gsl_vector* d1;
          gsl_vector* d2;
          bool unsat = false;
          
          if (outputmothers[j]->type == bool_node::EQ) {
            if (allOutputs[i][j] == 1) {
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
            if (allOutputs[i][j] == 1) {
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
								gsl_vector_set(d, i, 1.0);
							}
						}else {
							error += pow(diff, 2);
							gsl_vector_memcpy(t, d1);
							gsl_vector_sub(t, d2);
							gsl_vector_scale(t, 2*diff);
							gsl_vector_add(d, t);
						}
          }
        }
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
	
  virtual bool synthesis() {
    conflict.clear();
    InputMatrix& im = *inout;
    
    
    vector<vector<int>> allInputs;
    vector<vector<int>> allOutputs;
    vector<int> conflictids;
    stringstream str;
    for (int i = 0; i < inout->getNumInstances(); ++i) {
      vector<int> inputs;
      vector<int> outputs;
      bool notset = false;
      for (int j = 0; j < ninputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          str << 2 << ",";
          //break;
        } else {
          str << val << ",";
        }
        inputs.push_back(val);
      }
      //if (notset) continue;
      for (int j = ninputs; j < ninputs + noutputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          str << 2 << ",";
          //break;
        } else {
          str << val << ",";
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
		cout << "Input: ";
    for (int i = 0; i < allInputs[0].size(); i++) {
			if (allInputs[0][i] == EMPTY) {
					cout << "2,";
			} else {
				cout << allInputs[0][i] << ",";
			}
    }
    cout << endl;
    cout << "Output: ";
    for (int i = 0; i < allOutputs[0].size(); i++) {
			if (allOutputs[0][i] == EMPTY) {
				cout << "2,";
			} else {
				cout << allOutputs[0][i] << ",";
			}
    }
    cout << endl;
    string instr = str.str();
    cout << instr << endl;
    //if (inputs.find(instr) != inputs.end()) {
    //  cout << "Repeated Input" << endl;
    //  inputs[instr]++;
    //} else {
    //  inputs[instr] = 1;
    //}
    gd->init(this, allInputs, allOutputs);
    double minError = gd->optimize();
    gsl_vector* curState = gd->getResults();
		
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

	
  virtual void newInstance() {}
  
  virtual void finalize() {}
  
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
