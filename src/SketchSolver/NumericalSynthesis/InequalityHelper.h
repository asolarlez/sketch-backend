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
#include "NumericalSolverHelper.h"
#include "GradUtil.h"
#include "Util.h"


// Reasons about simple linear inequalities like x < c and an outlier counter
class InequalityHelper: public NumericalSolverHelper {
	vector<vector<int>> allInputs;
	vector<int> instanceIds;

	map<string, double> ctrlVals; // map from ctrl names to values synthesized
	map<string, vector<int>> ctrlToNodesMap; // map from ctrl names to list of related inequality nodes
	vector<int> counterPosNodes; // list of boolean nodes related to the outlier counter
	vector<int> counterNegNodes;
	double maxCount; // max outlier count
	vector<int> conflictNodes;
	map<string, double> ctrlToMinValues;
	map<string, double> ctrlToMaxValues;
	
	int tcount = 0;
	
	
public:
	InequalityHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
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
