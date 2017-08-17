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
	set<int> ignoredBoolNodes;
	vector<vector<int>> allInputs;
	vector<int> instanceIds;

	map<string, float> ctrlVals; // map from ctrl names to values synthesized
	map<string, vector<int>> ctrlToNodesMap; // map from ctrl names to list of related inequality nodes
	vector<int> counterPosNodes; // list of boolean nodes related to the outlier counter
	vector<int> counterNegNodes;
	float maxCount; // max outlier count
	vector<int> conflictNodes;
	
	
public:
	InequalityHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	virtual void setInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds);
	virtual bool checkInputs(int rowid, int colid);
	virtual bool checkSAT();
	virtual bool ignoreConflict();
	virtual vector<tuple<int, int, int>> collectSuggestions();
	virtual vector<pair<int, int>> getConflicts(int rowid, int colid);
	
	virtual float evalGD(const gsl_vector* state, gsl_vector* d);
	virtual void randomizeCtrls(gsl_vector* state);
	virtual void getControls(map<string, float>& ctrls);
	//virtual void autodiff(const gsl_vector* state, int rowid);
};
