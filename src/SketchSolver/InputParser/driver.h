#ifndef DRIVER_H
#define DRIVER_H

#include "BasicError.h"
#include "BooleanDAG.h"
#include "CegisCApi.h"
#include "InputReader.h"
#include "SolveFromInput.h"
#include "CommandLineArgs.h"
#include "DagOptim.h"
#include "DagFunctionInliner.h"
#include "DagElimUFUN.h"
#include "InterpreterEnvironment.h"

#include <fstream>
#include <ctime>

using std::ofstream;

namespace INp{
	extern InterpreterEnvironment* envt;
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




	virtual int resolveSketches();

	Driver(CommandLineArgs& p_params);

};


class Driver2: public Driver{
public:
	Driver2(CommandLineArgs& p_params):Driver(p_params){
	}

	virtual int solveSketch(ostream& out, BooleanDAG* spec, BooleanDAG* sketch, map<string, BooleanDAG*>& funMap, SATSolver* finder, SATSolver* checker, string& name);
};

/*Driver used by the python implementation of sketch-wrapper*/
class PyDriver: public Driver{
public:
	PyDriver(CommandLineArgs& p_params):Driver(p_params){}
	
	virtual void parseInput();
};
#endif
