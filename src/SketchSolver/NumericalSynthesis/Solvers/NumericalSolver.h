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
#include "ActualEvaluators.h"
#include "CommandLineArgs.h"
#include "SmoothEvaluators.h"
#include "NumDebugger.h"


using namespace std;


class NumericalSolver {
protected:
	BooleanDAG* dag;
	Interface* interf;
    
    int ncontrols;
    map<string, int>& ctrls;
    SmoothEvaluators* smoothEval;
    ActualEvaluators* actualEval;
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
    NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SmoothEvaluators* _smoothEval, ActualEvaluators* _actualEval, OptimizationWrapper* _opt, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls, NumDebugger* _debugger);
    ~NumericalSolver(void);
    
	// Called by the NumericalSolver
    bool checkSAT(int level, gsl_vector* initState = NULL);
    
    bool checkFullSAT(gsl_vector* state);

    gsl_vector* getResult();
    LocalState* getLocalState();
    

};
