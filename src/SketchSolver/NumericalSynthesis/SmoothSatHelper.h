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
#include "BoolAutoDiff.h"
#include "SimpleEvaluator.h"
#include "GradientDescentWrapper.h"
#include "SnoptWrapper.h"
#include "SymbolicEvaluator.h"
#include "NumericalSolverHelper.h"
#include "SimpleConflictGenerator.h"
#include "Util.h"

// Reasons about everything modulo boolean holes
class SmoothSatHelper: public NumericalSolverHelper {
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
	
    int CONFLICT_CUTOFF = 5;
    bool previousSAT;
    
public:
	SmoothSatHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	~SmoothSatHelper(void);
	virtual void setInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds);
	virtual bool checkInputs(int rowid, int colid);
	virtual bool checkSAT();
	virtual bool ignoreConflict();
	virtual vector<tuple<int, int, int>> collectSuggestions();
	virtual vector<pair<int, int>> getConflicts(int rowid, int colid);
	virtual void getControls(map<string, double>& ctrls);
	bool validObjective();
};
