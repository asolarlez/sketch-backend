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
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	void translator(SAT_Manager mng, varDir& dir, BooleanDAG* bdag, const string& outname);
	void processArithNode(SAT_Manager mng, varDir& dir, arith_node* anode, 	map<bool_node*, int>& node_ids,  map<bool_node*, vector<int> >& num_ranges);
	public:
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, int NS_p=1):CTRL("_C"){
		N = spec_p->get_n_inputs();
		Nout = spec_p->get_n_outputs();
		spec = spec_p;
		sketch = sketch_p;
      	sketch->cleanup();
      	spec->cleanup();
	    sketch->sort_graph();
	    spec->sort_graph();
	    Dout( cout<<"sketch->get_n_controls() = "<<sketch->get_n_controls()<<"  "<<sketch<<endl );
		declareControl(CTRL, sketch->get_n_controls());
		nseeds = NS_p;
		cout<<"Random seeds = "<<nseeds<<endl;
	}
	void output_control_map(ostream& out);
};










#endif /*SOLVEFROMINPUT_H_*/
