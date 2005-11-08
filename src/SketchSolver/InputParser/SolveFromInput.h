#ifndef SOLVEFROMINPUT_H_
#define SOLVEFROMINPUT_H_

#include "BooleanDAG.h"
#include "FindCheckSolver.h"


class SolveFromInput: public FindCheckSolver{
	BooleanDAG* spec;
	BooleanDAG* sketch;
	const string CTRL;
	int N;
	int YES;
	int Nout;
	bool firstTime;
	map<bool_node*, int> node_ids;
	map<bool_node*, vector<int> > num_ranges;
	int* last_input;
	protected:
	virtual void setupCheck();
	virtual void addInputsToTestSet(int input[], int insize);
	virtual void defineSketch(SAT_Manager mng, varDir& dir);
	virtual void defineSpec(SAT_Manager mng, varDir& dir);
	virtual void translator(SAT_Manager mng, varDir& dir, BooleanDAG* bdag, const string& outname);
	virtual void processArithNode(SAT_Manager mng, varDir& dir, arith_node* anode, 	map<bool_node*, int>& node_ids,  map<bool_node*, vector<int> >& num_ranges);
	virtual bool checkParentsChanged(bool_node* node, bool more);
	public:
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, int NS_p=1);
	void output_control_map(ostream& out);
};










#endif /*SOLVEFROMINPUT_H_*/
