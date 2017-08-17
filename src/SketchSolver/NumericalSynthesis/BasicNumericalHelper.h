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
#include "GradientDescentWrapper.h"
#include "SymbolicEvaluator.h"
#include "NumericalSolverHelper.h"
#include "ConflictGenerator.h"


class BasicNumericalHelper: public NumericalSolverHelper {
	set<int> ignoredBoolNodes;
	OptimizationWrapper* opt;
	int ncontrols;
	gsl_vector* state;
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	SymbolicEvaluator* eval;
	SimpleEvaluator* seval;
	
	map<string, int> ctrlMap; // Maps float ctrl names to indices with grad vectors
	map<string, int> boolCtrlMap; // Maps bool ctrl names to indices with grad vectors
	
	ConflictGenerator* cg;
	
public:
	BasicNumericalHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap);
	~BasicNumericalHelper(void);
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
