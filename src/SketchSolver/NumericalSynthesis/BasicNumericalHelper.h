#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <Sort.h>
#include <math.h>

#include "BooleanDAG.h"
#include "CommandLineArgs.h"
#include "AutoDiff.h"
#include "SimpleEvaluator.h"

#ifndef _NOGSL
#include "GradientDescentWrapper.h"
#else
#include "CustomSolver.h"
#endif


#ifndef _NOSNOPT
#include "SnoptWrapper.h"
#endif

#include "SymbolicEvaluator.h"
#include "NumericalSolverHelper.h"
#include "ConflictGenerator.h"
#include "SimpleConflictGenerator.h"


// Only handles the numerical part of the circuit. If the circuit contains any boolean structure, it will wait until the sat solver fixes all the boolean variables at the interface to the boolean structure.
class BasicNumericalHelper: public NumericalSolverHelper {
	set<int> boolNodes;
	OptimizationWrapper* opt;
	int ncontrols;
	gsl_vector* state;
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	SymbolicEvaluator* eval;
	SimpleEvaluator* seval;
	
	map<string, int> ctrlMap; // Maps float ctrl names to indices with grad vectors
	map<string, int> boolCtrlMap; // Maps bool ctrl names to indices with grad vectors
	
	AbstractConflictGenerator* cg;
	
public:
	BasicNumericalHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	~BasicNumericalHelper(void);
	virtual void setInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds);
	virtual bool checkInputs(int rowid, int colid);
	virtual bool checkSAT();
	virtual bool ignoreConflict();
	virtual vector<tuple<int, int, int>> collectSatSuggestions();
    virtual vector<tuple<int, int, int>> collectUnsatSuggestions();
	virtual vector<pair<int, int>> getConflicts(int rowid, int colid);
	virtual void getControls(map<string, double>& ctrls);
    virtual void setState(gsl_vector* state) { }
    virtual bool clearSoftLearnts() { return false; }
};
