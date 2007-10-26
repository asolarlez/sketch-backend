#ifndef SOLVEFROMINPUT_H_
#define SOLVEFROMINPUT_H_

#include "BooleanDAG.h"
#include "FindCheckSolver.h"
#include "Tvalue.h"



class SolveFromInput: public FindCheckSolver{
	BooleanDAG* problem;	
	const string TIP_NAME;
	int YES;
	int NINPUTS;
	bool firstTime;
	vector<Tvalue> node_ids;
	vector<Tvalue> f_node_ids;
	vector<bool> f_flags;
	
	map<bool_node*, int> node_values; // -1=false, 1=true, 0=unknown
	
	int* last_input;
	protected:
	virtual void setupCheck();
	
	
	virtual void addInputsToTestSet(vector<int>& input);
	virtual void setNewControls(vector<int>& controls);
	virtual void defineProblem(SATSolver& mng, varDir& dir);
		
	protected:
	virtual bool check(vector<int>& controls, vector<int>& input);
	virtual BooleanDAG* hardCodeControls(vector<int>& controls);
	public:
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker,
	map<string, BooleanDAG*>& functionMap, int p_nseeds=1, int inlineAmnt = 20, int NINPUTS_p=3);
	void output_control_map(ostream& out);
	void outputEuclid(ostream& fout);
	void setup2QBF();
};










#endif /*SOLVEFROMINPUT_H_*/
