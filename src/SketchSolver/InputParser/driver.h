#ifndef DRIVER_H
#define DRIVER_H

#include "BasicError.h"
#include "BooleanDAG.h"
#include "InputReader.h"
#include "SolveFromInput.h"
#include "CommandLineArgs.h"
#include "ABCSATSolver.h"
#include "DAGOptim.h"
#include "DAGFunctionInliner.h"
#include "DagElimUFUN.h"


#include <fstream>
#include <ctime>

using std::ofstream;

namespace INp{
	extern  map<string, BooleanDAG*> functionMap;
	extern  map<string, BooleanDAG*> sketchMap;
	extern  map<BooleanDAG*, string> sketches;
}

class Driver{
protected:
	CommandLineArgs& params;
public:

	virtual void parseInput();


	string procFname(string& fname){
		int x1 = fname.find_last_of("/");
		int x2 = fname.find_last_of("\\");
		int x3 = fname.find_last_of(".");

		x1 = x1>x2? x1: x2;
		x3 = x3 > 0? x3 : fname.size();
		++x1;
		fname = fname.substr(x1, x3-x1);
		return fname;
	}


	virtual BooleanDAG* prepareMiter(BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, string& name );

	virtual int solveSketch(ostream& out, BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, SATSolver* finder, SATSolver* checker, string& name);

	virtual int resolveSketches();

	Driver(CommandLineArgs& p_params);

};


class Driver2: public Driver{
public:
	Driver2(CommandLineArgs& p_params):Driver(p_params){
	}

	virtual int solveSketch(ostream& out, BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, SATSolver* finder, SATSolver* checker, string& name);
};


#endif
