#pragma once 

#include <gsl/gsl_vector.h>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "FloatSupport.h"
#include "BooleanDAG.h"

using namespace std;


// The helper class is collection of different methods/classes to handle different parts of the numerical solver
class NumericalSolverHelper {
protected:
	BooleanDAG* dag;
	map<int, int>& imap;
	FloatManager& fm;
	
	
	// class for picking the part of the numerical problem to handle
	// class to do symbolic evaluation
	// class for computing error function and gradients - I think this should be part of the optimization wrapper
	// class to perform optimization
	// class to generate suggestions
	// class to generate conflicts
	
public:
	NumericalSolverHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): fm(_fm), dag(_dag), imap(_imap){}
	
	// Called by the NumericalSolver
	virtual void setInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds) = 0;
	virtual bool checkInputs(int rowid, int colid) = 0;
	virtual bool checkSAT() = 0;
	virtual bool ignoreConflict() = 0;
	virtual vector<tuple<int, int, int>> collectSatSuggestions() = 0;
    virtual vector<tuple<int, int, int>> collectUnsatSuggestions() = 0;
	virtual vector<pair<int, int>> getConflicts(int rowid, int colid) = 0;
	virtual void getControls(map<string, double>& ctrls) = 0;
};
