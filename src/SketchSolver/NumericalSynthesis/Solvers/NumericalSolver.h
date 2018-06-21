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
    map<string, int>& ctrls;
    SymbolicEvaluator* eval;
    OptimizationWrapper* opt;
    
    SimpleEvaluator* seval;
    
    set<int> assertConstraints;
    int minimizeNode;
    
    const vector<vector<int>>& dependentInputs;
    const vector<vector<int>>& dependentCtrls;

    gsl_vector* result;
    LocalState* localState;
	
	NumDebugger* debugger;
	
public:
    NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls, NumDebugger* _debugger);
    ~NumericalSolver(void);
    
	// Called by the NumericalSolver
    bool checkSAT(gsl_vector* initState = NULL);
    
    // helper functions
    bool checkCurrentSol(gsl_vector* state);
    bool checkFullSAT(gsl_vector* state);
    bool initializeState();

    gsl_vector* getResult();
    LocalState* getLocalState();
    

};
