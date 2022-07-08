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
#include "NumericalSolver.h"
#include "Interface.h"
#include "SmoothEvaluators.h"
#include "ActualEvaluators.h"
#include "SnoptWrapper.h"
#include "GradientDescentWrapper.h"
#include "MaxSolverWrapper.h"
#include "SimpleConflictGenerator.h"
#include "SimpleSuggestionGenerator.h"
#include "SmartSuggestionGenerator1.h"
#include "SuggestionGeneratorUsingMax.h"

#include "NumDebugger.h"
#include "BoolNodeSimplifier.h"

#include "BasicSampler.h"
#include "BoolBasedSampler.h"
#include <unordered_map>
class NumericalSynthesizer {
	BooleanDAG* dag;
	Interface* interf;
	map<string, double> ctrlVals; // maps ctrl names to values found by the numerical solver
	NumericalSolver* solver;
	vector<pair<int, int>> conflicts; // (nodeid, val) pairs
	timerclass timer;

	map<string, int> ctrls; // maps ctrl names to index in the state vector
	vector<vector<int>> dependentCtrls; // maps nodes to dependent ctrls
	vector<vector<int>> dependentInputs; // maps nodes to dependent input nodes

	NumDebugger* debugger;
	SuggestionGenerator* sg;

	gsl_vector* state;
	gsl_vector* prevState;

	bool searchWithOnlySmoothing();
	bool searchWithPredicates();
	bool searchWithBooleans();
	bool searchWithBoolBasedSampling();


	int CONFLICT_THRESHOLD = PARAMS->conflictCutoff;
	int NUM_SUGGESTIONS_THRESHOLD = 5; // TODO: check this

	BasicSampler* basicSampler;
	BoolBasedSampler* boolBasedSampler;
public:
	NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface);
	~NumericalSynthesizer() {
		gsl_vector_free(state);
		delete(debugger);
		delete(sg);
		delete(solver);
	}
	bool solve();
	bool search();
	bool concretize();
	bool search_concretize();
	bool simple_concretize();


	void print(ostream& out) {
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			out << it->first << ":" << it->second << endl;
		}
	}
	void getControls(map<string, string>& values) {
		for (auto it = ctrls.begin(); it != ctrls.end(); it++) {
			ctrlVals[it->first] = gsl_vector_get(state, it->second);
		}
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			stringstream str;
			str << it->second;
			values[it->first] = str.str(); // TODO: deal with boolean holes
		}
	}
    map<string, int>& get_ctrls()
    {
        return solver->get_ctrls();
    }
    gsl_vector* get_result()
    {
        return solver->get_result();
    }
};
