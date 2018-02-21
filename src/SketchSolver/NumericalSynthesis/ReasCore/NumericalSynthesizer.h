#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>


#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "BasicSolver.h"
#include "InequalitySolver.h"
#include "BoolApproxSolver.h"
#include "IteApproxSolver.h"
#include "SmoothSatSolver.h"


class NumericalSynthesizer : public Synthesizer {
	BooleanDAG* dag;
	map<int, int> imap; // Map boolean inputs to actual nodes in the dag // TODO: this should be by reference
	map<string, double> ctrlVals; // maps ctrl names to values found by the numerical solver
	NumericalSolver* solver;
	
	int counter;
    timerclass timer;
	
	
public:
	NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap, Lit _softConflictLit);
    NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface, Lit _softConflictLit);
	
	virtual bool synthesis(int instance, int inputid, int val, int level, vec<Lit>& suggestions);
	virtual void newInstance() {}
	virtual void finalize() {}
	virtual void backtrack(int level) {}
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params);
    
	virtual void print(ostream& out) {
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			out << it->first << ":" << it->second << endl;
		}
	}
	virtual void getControls(map<string, string>& values) {
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			stringstream str;
			str << it->second;
			values[it->first] = str.str();
		}
	}
	
	void collectAllInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds);
	void printInputs(vector<vector<int>>& allInputs);
	void convertSuggestions(const vector<tuple<int, int, int>>& s, vec<Lit>& suggestions);
	void convertConflicts(const vector<pair<int, int>>& c);
		
	void debug();
    void checkInput();
	void genData(gsl_vector* state, int idx, SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, bool useSnopt = true);
    void genData2D(gsl_vector* state, int idx1, int idx2, SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, bool useSnopt = true );
    double getError(SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, gsl_vector* d, bool useSnopt = true);
    void analyze(SymbolicEvaluator* eval, gsl_vector* d, int idx, const set<int>& nodeids);
    set<int> getRelevantIds();
};

