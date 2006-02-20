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
	map<bool_node*, int> node_values; // -1=false, 1=true, 0=unknown
	map<bool_node*, vector<int> > num_ranges;
	int* last_input;
	protected:
	virtual void setupCheck();
	virtual void addInputsToTestSet(int input[], int insize);
	virtual void defineSketch(SATSolver& mng, varDir& dir);
	virtual void defineSpec(SATSolver& mng, varDir& dir);
	virtual void translator(SATSolver& mng, varDir& dir, BooleanDAG* bdag, const string& outname);
	virtual void processArithNode(SATSolver& mng, varDir& dir, arith_node* anode, map<bool_node*, int>& node_ids, map<bool_node*, vector<int> >& num_ranges);
	virtual void doNonBoolArrAcc(SATSolver& mng, varDir& dir,arith_node* anode, map<bool_node*, int>& node_ids, map<bool_node*, vector<int> >& num_ranges);
	virtual bool checkParentsChanged(bool_node* node, bool more);	
	template<typename COMP>
	bool booleanPartialEval(bool_node* node);
	public:
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, int NS_p=1);
	void output_control_map(ostream& out);
};










#endif /*SOLVEFROMINPUT_H_*/
