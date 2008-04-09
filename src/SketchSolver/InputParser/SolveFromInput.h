#ifndef SOLVEFROMINPUT_H_
#define SOLVEFROMINPUT_H_

#include "BooleanDAG.h"
#include "FindCheckSolver.h"
#include "Tvalue.h"



class SolveFromInput: public FindCheckSolver{
	BooleanDAG* problem;	
	int YES;
	int NINPUTS;
	bool firstTime;
	vector<Tvalue> node_ids;
	vector<Tvalue> f_node_ids;
	vector<bool> f_flags;
	ostream& out;
	map<bool_node*, int> node_values; // -1=false, 1=true, 0=unknown
	
	int* last_input;
	protected:
	virtual void setupCheck();
	
	
	virtual void addInputsToTestSet(vector<int>& input);
	virtual void setNewControls(vector<int>& controls);
	virtual void defineProblem(SATSolver& mng, SolverHelper& dir);
	virtual void declareInput(const string& inname, int size);
	protected:
	virtual bool check(vector<int>& controls, vector<int>& input);
	virtual BooleanDAG* hardCodeControls(BooleanDAG* dag, vector<int>& controls);
	public:
	SolveFromInput(ostream& out_p, BooleanDAG* miter, SATSolver& finder, SATSolver& checker, int p_nseeds=1, int NINPUTS_p=3);
	void output_control_map(ostream& out);
	void outputEuclid(ostream& fout);
	void setup2QBF();
	virtual ~SolveFromInput();
};









#endif /*SOLVEFROMINPUT_H_*/
