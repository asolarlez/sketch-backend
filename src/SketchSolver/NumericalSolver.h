#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <utility>
#include <Sort.h>
#include <math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_blas.h>
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

class NumericalSolver;

class Parameters {
public:
  NumericalSolver* ns;
  vector<vector<int>> allInputs;
  vector<vector<int>> allOutputs;
};


class SimulatedAnnealing {
  double T = 10;
  double coolingRate = 0.1;
  int NUM_STEPS = 100;
  double acceptanceProb(double e, double e1, double T) {
    if (e1 < e) {
      return 1.0;
    }
    return exp((e - e1)/T);
  }
  
  void getNeighboringState(const gsl_vector* curState, gsl_vector* newState) {
    for (int i = 0; i < curState->size; i++) {
      double c = 0;
      while(true) {
        c = gsl_vector_get(curState, i) - 5.0 + (rand() %100)/10.0; // Randomly permutate cur value with +/- 5 (TODO: magic numbers)
        if (c >= 0.0 && c <= 32.0) break;
      }
      gsl_vector_set(newState, i, c);
    }
  }
public:
  double optimize(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, const gsl_vector* curState);
};

class GradientDescent {
  int N; // Total number of components
  gsl_vector *x;  // The current state
  gsl_vector *ds; // The step size
  
  gsl_multimin_fdfminimizer* minidf;  // The GSL minimizer for when derivatives needed
  const gsl_multimin_fdfminimizer_type *Tdf ;
  gsl_multimin_function_fdf myfundf;
  double INIT_STEP_SIZE = 5.0;
  double TOLERANCE = 0.1;
  
public:
  GradientDescent(int N_p): N(N_p) {
    Tdf = gsl_multimin_fdfminimizer_steepest_descent;
    minidf = gsl_multimin_fdfminimizer_alloc(Tdf, N);
    myfundf.n = N;
  }
  
  gsl_vector* getResults() {
    return minidf->x;
  }
  void init(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs);
  double optimize();
};

class NumericalSolver : public Synthesizer {
  public: BooleanDAG* dag;
  map<string, int> ctrlMap;
  vector<float> ctrlVals;
  int ninputs;
  int noutputs;
  int ncontrols;
  GradientDescent* gd;
  map<string, BooleanDAG*> empty;
  map<string, int> inputs;
  
  NumericalSolver(FloatManager& _fm, BooleanDAG* _dag) :Synthesizer(_fm), dag(_dag) {
    ninputs = dag->getNodesByType(bool_node::SRC).size();
    noutputs = (dynamic_cast<TUPLE_CREATE_node*>(dag->get_node("OUTPUT")->mother))->multi_mother.size();
    ncontrols = dag->getNodesByType(bool_node::CTRL).size();
    gd = new GradientDescent(ncontrols);
    vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      ctrlMap[ctrls[i]->get_name()] = i;
      ctrlVals.push_back(0.0);
    }
  }
  
  double evalState(const gsl_vector* state, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
    double error = 0;
    NodeEvaluator eval(empty, *dag, fm);
    for (int i = 0; i < allInputs.size(); i++) {
      VarStore inputStore;
      // Store all inputs
      vector<bool_node*> inputs = dag->getNodesByType(bool_node::SRC);
      for (int j = 0; j < allInputs[i].size(); j++) {
        if (allInputs[i][j] != EMPTY) {
          inputStore.setVarVal(inputs[j]->get_name(), allInputs[i][j]);
        }
      }
      // Store all controls
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int j = 0; j < state->size; j++) {
        inputStore.setVarVal(ctrls[j]->get_name(), fm.getIdx(gsl_vector_get(state,j)));
      }
      
      eval.run(inputStore);
      /*for (int k = 0; k < dag->size(); k++) {
       bool_node* n = (*dag)[k];
       cout << n->lprint() << endl;
       if (n->type == bool_node::SRC || n->getOtype() == OutType::FLOAT) {
       cout << fm.getFloat(eval.getValue(n)) << endl;
       } else {
       cout << eval.getValue(n) << endl;
       }
      }*/
      bool_node* output = dag->get_node("OUTPUT")->mother;
      vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
      vector<int> outputs = eval.getTuple(eval.getValue(output));
      for (int j = 0; j < outputs.size(); j++) {
        //cout << outputs[j] << endl;
        if (allOutputs[i][j] != EMPTY && outputs[j] != allOutputs[i][j]) {
          float m1 = fm.getFloat(eval.getValue(outputmothers[j]->mother)); // TODO: this assumes all values are floats which is probably true, but still we need to check
          float m2 = fm.getFloat(eval.getValue(outputmothers[j]->father));
          float diff = m1 - m2;
          if (diff == 0.0) diff = 0.1; // Min error
          error += pow(diff, 2);
        }
      }
      
    }
    return error;
  }
  
  double evalStateWithGrad(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
    for (int i = 0; i < ncontrols; i++ ) {
      gsl_vector_set(d, i, 0);
    }
    gsl_vector* t = gsl_vector_alloc(ncontrols);
    double error = 0;
    AutoDiff eval(empty, *dag, fm, ctrlMap);
    RangeDiff evalR(empty, *dag, fm, ctrlMap);
    for (int i = 0; i < allInputs.size(); i++) {
      VarStore inputStore;
      // Store all inputs
      vector<bool_node*> inputs = dag->getNodesByType(bool_node::SRC);
      for (int j = 0; j < allInputs[i].size(); j++) {
        if (allInputs[i][j] != EMPTY) {
          inputStore.setVarVal(inputs[j]->get_name(), allInputs[i][j]);
        }
      }
      // Store all controls
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
         //cout << outputs[j] << endl;
          if (allOutputs[i][j] != EMPTY && outputs[j] != allOutputs[i][j]) {
            float m1 = fm.getFloat(eval.getValue(outputmothers[j]->mother)); // TODO: this assumes all values are floats which is probably true, but still we need to check
            float m2 = fm.getFloat(eval.getValue(outputmothers[j]->father));
            //cout << j << " " << m1 << " " << m2 << endl;
            float diff = m1 - m2;
            if (diff == 0.0) diff = 0.1; // Min error
            error += pow(diff, 2);
            gsl_vector* d1 = eval.getGrad(outputmothers[j]->mother);
            gsl_vector* d2 = eval.getGrad(outputmothers[j]->father);
            gsl_vector_memcpy(t, d1);
            gsl_vector_sub(t, d2);
            gsl_vector_scale(t, 2*diff);
            gsl_vector_add(d, t);
            //for (int i = 0; i < t->size; i++) {
            //  cout <<  gsl_vector_get(t, i) << " ";
            //}
            //cout << endl;
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
          //cout << f1 << " " << f2 << endl;
          
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
              if (m1 == m2 && m2 == f1 && f1 == f2) { // TODO: check this
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
            //cout << "expOut: " << allOutputs[i][j] << endl;
            //cout << outputmothers[j]->lprint() << endl;
            //cout << outputmothers[j]->mother->lprint() << endl;
            //cout << outputmothers[j]->father->lprint() << endl;
            //cout << "m: " << fm.getFloat(mrange.first) << " " << fm.getFloat(mrange.second) << endl;
            //cout << "f: " << fm.getFloat(frange.first) << " " << fm.getFloat(frange.first) << endl;

            float diff = v1 - v2;
            if (diff == 0.0) diff = 0.1; // Min error
            error += pow(diff, 2);
            gsl_vector_memcpy(t, d1);
            gsl_vector_sub(t, d2);
            gsl_vector_scale(t, 2*diff);
            gsl_vector_add(d, t);
          }
        }
      }

      //cout << gsl_vector_get(d, 0) << endl;
      
      }
    }
    //cout << "error: " << error << endl;
    //cout << "grad: ";
    //for (int i = 0; i < d->size; i++) {
    //  cout <<  gsl_vector_get(d, i) << " ";
    //}
    //cout << endl;
    return error;
  }


  static double eval_f(const gsl_vector* x, void* params) {
    Parameters* p = (Parameters*) params;
    return p->ns->evalState(x, p->allInputs, p->allOutputs);
    
  }
  
  static void eval_df(const gsl_vector* x, void* params, gsl_vector* d) {
    
  }
  
  static void eval_fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
    Parameters* p = (Parameters*) params;
    *f = p->ns->evalStateWithGrad(x, df, p->allInputs, p->allOutputs);
    //cout << "val: " << (*f) << endl;
    //eval_df(x, params, df);
  }
  
  /*void generateProfile(vector<double> state, int idx, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, string fname) {
    ofstream out(fname, ios_base::out);
    
    double i = 0.0;
    while (i < 32.0) {
      state[idx] = i;
      out << getEnergy(state, allInputs, allOutputs) << ";";
      i += 0.1;
    }
    out << endl;
  }*/
  
  void generateConflict(const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, const vector<int>& conflictids) {
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
  
  /*void analyzeOutput(bool_node* node) {
    vector<bool_node*> parents = node->parents();
    for (int i = 0; i < parents.size(); i++) {
      analyzeOutput(parents[i]);
    }
    cout << node->lprint() << endl;
  }
  void analyzeDAG() {
    bool_node* output = dag->get_node("OUTPUT")->mother;
    vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
    for (int i = 0; i < outputmothers.size(); i++) {
      cout << "Output " << i << endl;
      analyzeOutput(outputmothers[i]);
    }
  }*/
  
  virtual bool synthesis() {
    //analyzeDAG();
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
          str << 2;
          //break;
        } else {
          str << val;
        }
        inputs.push_back(val);
      }
      //if (notset) continue;
      for (int j = ninputs; j < ninputs + noutputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          str << 2;
          //break;
        } else {
          str << val;
        }
        outputs.push_back(val);
      }
      //if (notset) continue;
      allInputs.push_back(inputs);
      allOutputs.push_back(outputs);
      conflictids.push_back(i);
      for (int k = 0; k < inputs.size(); k++) {
        //cout << "Input" << k << ": " << inputs[k] << endl;
      }
      for (int k = 0; k < outputs.size(); k++) {
        //cout << "Output" << k << ": " << outputs[k] << endl;
      }
      //if (!notset) {
      //  cout << "Found a input output pair" << endl;
      //}
      
    }
    
    Assert(allInputs.size() == allOutputs.size(), "This should not be possible");
    
    if (allInputs.size() == 0) return true;
    /*if (incnt > 0 && outcnt > 0) {
      vector<double> state;
      state.push_back(18.1);
      //state.push_back(18.5);
      generateProfile(state, 0, allInputs, allOutputs, "/Users/Jeevu/projects/symdiff/scripts/profile0.txt");
      //generateProfile(state, 1, allInputs, allOutputs, "/Users/Jeevu/projects/symdiff/scripts/thermo1.txt");
     }*/
    // print a DAG representation of the optimization problem
    /*string dagfilename = "/Users/Jeevu/projects/symdiff/scripts/acc4.dag";
     string inpfilename = "/Users/Jeevu/projects/symdiff/scripts/acc4.inp";
     ofstream outdag(dagfilename, ios_base::out);
     dag->mrprint(outdag);
     ofstream outinp(inpfilename, ios_base::out);
     vector<bool_node*> inputs = dag->getNodesByType(bool_node::SRC);
     vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
     for (int i = 0; i < allInputs.size(); i++) {
     for (int k = 0; k < allInputs[i].size(); k++) {
     outinp << inputs[k]->get_name() << ":" << allInputs[i][k] << ";";
     }
     outinp << endl;
     for (int k = 0; k < allOutputs[i].size(); k++) {
     outinp << allOutputs[i][k] << ";";
     }
     outinp << endl;
     
     }*/
    cout << "Input: ";
    for (int i = 0; i < allInputs[0].size(); i++) {
      cout << allInputs[0][i] << " ";
    }
    cout << endl;
    cout << "Output: ";
    for (int i = 0; i < allOutputs[0].size(); i++) {
      cout << allOutputs[0][i] << " ";
    }
    cout << endl;
    string instr = str.str();
    cout << instr << endl;
    if (inputs.find(instr) != inputs.end()) {
      cout << "Repeated Input" << endl;
      inputs[instr]++;
    } else {
      inputs[instr] = 1;
    }
    gd->init(this, allInputs, allOutputs);
    double minError = gd->optimize();
    gsl_vector* curState = gd->getResults();
    /*gsl_vector* curState = gsl_vector_alloc(ncontrols);
    for (int i = 0; i < ncontrols; i++) {
      double c = (rand() % 320) / 10.0; // random float from 0 to 32 (TODO: magic number)
      gsl_vector_set(curState, i, c);
    }
    double minError = sa.optimize(this, allInputs, allOutputs, curState);*/
    //cout << "min error: " << minError << endl;
    if (minError >= -1e-10 && minError <= 1e-10) {
      // Update the controls
      //evalState(curState, allInputs, allOutputs);
      //cout << "Input: " << allInputs[0][0] << endl;
      //cout << "Outputs: " << allOutputs[0][0] << " " << allOutputs[0][1] << endl;
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int i = 0; i < ctrls.size(); i++) {
        ctrlVals[ctrlMap[ctrls[i]->get_name()]] = gsl_vector_get(curState, i);
      }
      return true;
    } else {
      cout << "******************* Found a conflict *******************" << endl;
      generateConflict(allInputs, allOutputs, conflictids);
     return false;
    }
  }
  virtual void newInstance() {
    
  }
  
  virtual void finalize() {
    
  }
  
  virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
    // Add the appropriate expression from the dag after replacing inputs with params and ctrls with synthesized parameters
    BooleanDAG newdag = *(dag->clone());
    int src_counter = 0;
    for (int i = 0; i < newdag.size(); i++) {
      if (newdag[i]->type == bool_node::SRC) {
        newdag.replace(i, params[src_counter++]); // TODO: is this correct?
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
    vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      out << ctrls[i]->get_name() << ":" << ctrlVals[ctrlMap[ctrls[i]->get_name()]] << endl;
    }
  }
  
  virtual void getControls(map<string, string>& values) {
    vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      stringstream str;
      str << ctrlVals[ctrlMap[ctrls[i]->get_name()]];
      values[ctrls[i]->get_name()] = str.str();
    }
  }
};


double SimulatedAnnealing::optimize(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, const gsl_vector* curState) {
  double curError = ns->evalState(curState, allInputs, allOutputs);
  
  for (int i = 0; i < NUM_STEPS; i++) {
    //cout << "state: " << curState[0] << " error: " << curError << endl;
    if (curError == 0.0) break;
    gsl_vector* nextState = gsl_vector_alloc(curState->size);
    getNeighboringState(curState, nextState);
    double nextError = ns->evalState(nextState, allInputs, allOutputs);
    //cout << "next state: " << nextState[0] << " error: " << nextError << endl;
    double prob = acceptanceProb(curError, nextError, T);
    double randflip = (rand()%10)/10.0;
    //cout << "prob: " << prob << " randflip: " << randflip << endl;
    if (prob >= randflip) {
      //cout << "Transitioning to next state" << endl;
      curState = nextState;
      curError = nextError;
    }
    T = T*(1 - coolingRate);
  }
  return curError;
}

void GradientDescent::init(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs) {
  x = gsl_vector_alloc(N);
  for (int i = 0; i < N; i++) {
    gsl_vector_set(x, i, 0);
  }
  myfundf.f = &ns->eval_f;
  myfundf.df = &ns->eval_df;
  myfundf.fdf = &ns->eval_fdf;
  Parameters* p = new Parameters();
  p->allInputs = allInputs;
  p->allOutputs = allOutputs;
  p->ns = ns;
  myfundf.params = p;
  gsl_multimin_fdfminimizer_set(minidf, &myfundf, x, INIT_STEP_SIZE, TOLERANCE);
}

double GradientDescent::optimize() {
  size_t iter = 0;
  int status;
  double size;
  double fval;
  const gsl_vector* grad;
  
  do
  {
    iter++;
    status = gsl_multimin_fdfminimizer_iterate (minidf);
    //cout << gsl_strerror(status) << endl;
    if (status)
      break;
    grad = gsl_multimin_fdfminimizer_gradient(minidf);
    size = gsl_blas_dasum(grad);
    status = gsl_multimin_test_gradient (grad, 1e-5);
    
    if (status == GSL_SUCCESS)
      printf ("Minimum found at:\n");
    // printOutput(iter);
    fval = minidf->f;
  }
  while (status == GSL_CONTINUE && iter < 4000 && fval > 1e-7);
  
  cout << "Ending search..." << endl;
  for (int i = 0; i < minidf->x->size; i++) {
    cout << "Parameter #" << i << ": " << gsl_vector_get (minidf->x, i) << "; " << endl;
  }
  cout << "Optimal value found: " << minidf->f << endl;
  return fval;
}