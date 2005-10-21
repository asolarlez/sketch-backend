#ifndef SOLVEFROMINPUT_H_
#define SOLVEFROMINPUT_H_

#include "BooleanDAG.h"
#include "FindCheckSolver.h"


class SolveFromInput: public FindCheckSolver{
	BooleanDAG* spec;
	BooleanDAG* sketch;
	int N;
	int Nout;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	void translator(BooleanDAG* bdag, const string& outname);
	public:
	SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, int NS_p=1){
		N = spec_p->get_n_inputs();
		Nout = spec_p->get_n_outputs();
		spec = spec_p;
		sketch = sketch_p;
      	sketch->cleanup();
      	spec->cleanup();
	    sketch->sort_graph();
	    spec->sort_graph();
		nseeds = NS_p;
	}
};










#endif /*SOLVEFROMINPUT_H_*/
