#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <utility>
#include "Sort.h"
#include <math.h>
using namespace std;

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"

class NumericalSolver;

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
	
	vector<double> getNeighboringState(const vector<double>& curState) {
		vector<double> newState;
		
		for (int i = 0; i < curState.size(); i++) {
			double c = 0;
			while(true) {
				c = curState[i] - 5.0 + (rand() %100)/10.0; // Randomly permutate cur value with +/- 5 (TODO: magic numbers)
				if (c >= 0.0 && c <= 32.0) break;
			}
			newState.push_back(c);
		}
		return newState;
	}
public:
	double optimize(NumericalSolver* ns, vector<vector<int>> allInputs, vector<vector<int>> allOutputs, vector<double>& curState);
};

class NumericalSolver : public Synthesizer {
public: BooleanDAG* dag;
	map<string, float> ctrlValMap;
	int ninputs;
	int noutputs;
	int ncontrols;
	SimulatedAnnealing sa;
	map<string, BooleanDAG*> empty;
	NumericalSolver(FloatManager& _fm, BooleanDAG* _dag) :Synthesizer(_fm), dag(_dag) {
		ninputs = dag->getNodesByType(bool_node::SRC).size();
		noutputs = (dynamic_cast<TUPLE_CREATE_node*>(dag->get_node("OUTPUT")->mother))->multi_mother.size();
		ncontrols = dag->getNodesByType(bool_node::CTRL).size();
		vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
		for (int i = 0; i < ctrls.size(); i++) {
			ctrlValMap[ctrls[i]->get_name()] = 0.0;
		}
	}
	
	double evalState(vector<double> state, vector<vector<int>> allInputs, vector<vector<int>> allOutputs) {
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
			for (int j = 0; j < state.size(); j++) {
				inputStore.setVarVal(ctrls[j]->get_name(), fm.getIdx(state[j]));
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
					error += pow((m1 - m2), 2);
				}
			}
			
		}
		return error;
	}
	
	
	/*void generateProfile(vector<double> state, int idx, vector<vector<int>> allInputs, vector<vector<int>> allOutputs, string fname) {
	 ofstream out(fname, ios_base::out);
	 
	 double i = 0.0;
	 while (i < 32.0) {
	 state[idx] = i;
	 out << getEnergy(state, allInputs, allOutputs) << ";";
	 i += 0.1;
	 }
	 out << endl;
  }*/
	
	void generateConflict(vector<vector<int>> allInputs, vector<vector<int>> allOutputs, vector<int> conflictids) {
		InputMatrix& im = *inout;
		for(auto conflictId: conflictids) {
			for (int j = 0; j < ninputs + noutputs; j++) {
				conflict.push(getLit(im.valueid(conflictId, j)));
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
	
	virtual bool synthesis(vec<Lit>& suggestions) {
		//analyzeDAG();
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
					break;
				}
				inputs.push_back(val);
			}
			if (notset) continue;
			for (int j = ninputs; j < ninputs + noutputs; j++) {
				int val = im.getVal(i, j);
				if (val == EMPTY) {
					notset = true;
					break;
				}
				outputs.push_back(val);
			}
			if (notset) continue;
			allInputs.push_back(inputs);
			allOutputs.push_back(outputs);
			conflictids.push_back(i);
			for (int k = 0; k < inputs.size(); k++) {
				//cout << "Input" << k << ": " << inputs[k] << endl;
			}
			for (int k = 0; k < outputs.size(); k++) {
				//cout << "Output" << k << ": " << outputs[k] << endl;
			}
			if (!notset) {
				cout << "Found a input output pair" << endl;
			}
			
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
		
		vector<double> curState;
		for (int i = 0; i < ncontrols; i++) {
			double c = (rand() % 320) / 10.0; // random float from 0 to 32 (TODO: magic number)
			curState.push_back(c);
		}
		double minError = sa.optimize(this, allInputs, allOutputs, curState);
		if (minError == 0.0) {
			// Update the controls
			vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
			for (int i = 0; i < ctrls.size(); i++) {
				ctrlValMap[ctrls[i]->get_name()] = curState[i];
			}
			return true;
		} else {
			cout << "Found a conflict" << endl;
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
				newdag.replace(i, dopt->getCnode(ctrlValMap[newdag[i]->get_name()]));
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
		out << "( ";
		vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
		for (int i = 0; i < ctrls.size(); i++) {
			out << ctrlValMap[ctrls[i]->get_name()] << ", ";
		}
		out << " )";
	}
};


double SimulatedAnnealing::optimize(NumericalSolver* ns, vector<vector<int>> allInputs, vector<vector<int>> allOutputs, vector<double>& curState) {
	double curError = ns->evalState(curState, allInputs, allOutputs);
	
	for (int i = 0; i < NUM_STEPS; i++) {
		//cout << "state: " << curState[0] << " error: " << curError << endl;
		if (curError == 0.0) break;
		vector<double> nextState = getNeighboringState(curState);
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
