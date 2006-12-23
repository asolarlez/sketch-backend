#ifndef SOLVEFROMINPUT_H_
#define SOLVEFROMINPUT_H_

#include "BooleanDAG.h"
#include "FindCheckSolver.h"
#include "Tvalue.h"



class SolveFromInput: public FindCheckSolver{
	BooleanDAG* spec;
	BooleanDAG* sketch;
		
	int YES;
	
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
	
	
	virtual void defineSketch(SATSolver& mng, varDir& dir);
	virtual void defineSpec(SATSolver& mng, varDir& dir);
	virtual void translator(SATSolver& mng, varDir& dir, BooleanDAG* bdag, const string& outname);	
	
	public:	
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker, int NS_p=1);
	void output_control_map(ostream& out);
	void outputEuclid(ostream& fout);
};










#endif /*SOLVEFROMINPUT_H_*/
