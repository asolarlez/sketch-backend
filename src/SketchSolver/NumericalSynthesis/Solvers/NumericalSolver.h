#pragma once 

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "FloatSupport.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "ConflictGenerator.h"
#include "SuggestionGenerator.h"
#include "SimpleEvaluator.h"
#include "CommandLineArgs.h"
#include "SymbolicEvaluator.h"
#include "NumDebugger.h"


using namespace std;

class NumericalSolver {
protected:
	BooleanDAG* dag;
	Interface* interf;
    
    int ncontrols;
    gsl_vector* state;
    
    bool previousSAT;
    bool fullSAT;
    int numConflictsAfterSAT;
    bool inputConflict;
    
    int CONFLICT_CUTOFF = PARAMS->conflictCutoff;

    
    map<string, int>& ctrls;
    SymbolicEvaluator* eval;
    OptimizationWrapper* opt;
    ConflictGenerator* cg;
    SuggestionGenerator* sg;
    
    SimpleEvaluator* seval;
    
    set<int> assertConstraints;
    int minimizeNode;
    
    const vector<vector<int>>& dependentInputs;
    const vector<vector<int>>& dependentCtrls;
    vector<int> inputsToAsserts; // TODO: this is pretty local hack

    map<int, string> inputStrings; // TODO: local info for debugging input node id to <iteration,ctrlid>
	
	NumDebugger* debugger;
	
public:
    NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt, ConflictGenerator* _cg, SuggestionGenerator* _sg, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls, NumDebugger* _debugger);
    ~NumericalSolver(void);
    
	// Called by the NumericalSolver
    bool checkSAT();
	bool ignoreConflict();
	vector<tuple<int, int, int>> collectSatSuggestions();
    vector<tuple<int, int, int>> collectUnsatSuggestions();
	void getConflicts(vector<pair<int, int>>& conflicts);
	void getControls(map<string, double>& ctrlVals);
    void setState(gsl_vector* state);
    
    // helper functions
    bool checkInputs();
    bool checkCurrentSol();
    bool checkFullSAT();
    bool initializeState(bool suppressPrint);
    void printControls();
    
    void printGraphCmd(string prefix);
    void printInput();

};
