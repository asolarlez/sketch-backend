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
#include "IteAutoDiff.h"
#include "SimpleEvaluator.h"
#include "GradientDescentWrapper.h"
#include "SnoptWrapper.h"
#include "SymbolicEvaluator.h"
#include "NumericalSolver.h"
#include "ConflictGenerator.h"
#include "SimpleConflictGenerator.h"


// Only handles the numerical part of the circuit. If the circuit contains any boolean structure, it will wait until the sat solver fixes all the boolean variables at the interface to the boolean structure.
class IteApproxSolver: public NumericalSolver {
	set<int> boolNodes;
	OptimizationWrapper* opt;
	int ncontrols;
	gsl_vector* state;
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	SymbolicEvaluator* eval;
	SimpleEvaluator* seval;
	
	map<string, int> ctrlMap; // Maps float ctrl names to indices with grad vectors
	map<int, int> extraCtrlMap;
    map<string, int> boolCtrlMap; // Maps bool ctrl names to indices with grad vectors
	
	AbstractConflictGenerator* cg;
	
public:
	IteApproxSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	~IteApproxSolver(void);
	virtual void setInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds);
	virtual bool checkInputs(int rowid, int colid);
	virtual bool checkSAT();
	virtual bool ignoreConflict();
	virtual vector<tuple<int, int, int>> collectSatSuggestions();
    virtual vector<tuple<int, int, int>> collectUnsatSuggestions();
	virtual vector<pair<int, int>> getConflicts(int rowid, int colid);
	virtual void getControls(map<string, double>& ctrls);
    virtual void setState(gsl_vector* state) { }
};
