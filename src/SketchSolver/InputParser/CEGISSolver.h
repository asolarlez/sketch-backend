#pragma once

#include "BooleanToCNF.h"
#include "FindCheckSolver.h"
#include "BooleanDAG.h"
#include "Tvalue.h"
#include "Checkpointer.h"
#include "VarStore.h"
#include "CommandLineArgs.h"
#include <stack>
#include <ctime>


class CEGISparams{
public:
	int randseed;
	int iterlimit;
	bool printDiag;
	int NINPUTS;
	int nseeds;

	bool simulate;
	int simiters;

	typedef enum{ NOSIM /*no simplify*/, SIMSIM/*simple simplify*/, RECSIM/*recursive simplify*/} simtype;
	simtype simplifycex;
	bool superChecks;

	CEGISparams(CommandLineArgs& args):
		printDiag(false),
		nseeds(1),
		NINPUTS(3),
		iterlimit(-1),
		randseed(time(NULL)),
		simulate(true),
		simiters(3),
		simplifycex(RECSIM),
		superChecks(false)
	{
		printDiag = args.printDiag;
		nseeds = args.seedsize;		
		NINPUTS = args.NINPUTS;
		if(args.terminateafter > 0){
			iterlimit = args.terminateafter;
		}
		if(args.seed >= 0){
			randseed = args.seed;			
		}
		if(PARAMS->verbosity < 0) {Dout(cout<<"SOLVER RAND SEED = "<<randseed<<endl;)}
		else {cout<<"SOLVER RAND SEED = "<<randseed<<endl;}
		simulate = args.simulate;
		simiters = args.simiters;
		superChecks = args.superChecks;
		if(args.simplifycex == "NOSIM"){ simplifycex = NOSIM; }
		if(args.simplifycex == "SIMSIM"){ simplifycex = SIMSIM; }
		if(args.simplifycex == "RECSIM"){ simplifycex = RECSIM; }
	}

	void activatePrintDiag(){
		printDiag = true;
	}	
	void set_randseed(int seed){ randseed = seed; };
	void setIterLimit(int p_iterlimit){iterlimit = p_iterlimit; };

};


class CEGISSolver
{

	stack<BooleanDAG*> problemStack;
	void pushProblem(BooleanDAG* p){
		problemStack.push(p);
	}
	BooleanDAG* getProblem(){
		return problemStack.top();
	}
	void popProblem(){
		BooleanDAG* t = problemStack.top();
		problemStack.pop();
		t->clear();
		delete t;
	}
	int problemLevel(){
		return problemStack.size();
	}

	SolverHelper& dirFind;	
	SolverHelper& dirCheck;

	SATSolver& mngFind;
	SATSolver& mngCheck;

	
	VarStore inputStore;

	CEGISparams params;
	
	Checkpointer cpt;
	BooleanDAG* lastFproblem;
	
	vector<Tvalue> find_node_ids;
	vector<Tvalue> check_node_ids;
	map<string, int> last_input;
	bool firstTime;
protected:
	void declareControl(const string& cname, int size);
	void declareInput(const string& cname, int size);
	bool solveCore();
	bool simulate(VarStore& controls, VarStore& input);
	bool find(VarStore& input, VarStore& controls);
	void addInputsToTestSet(VarStore& input);

	bool check(VarStore& input, VarStore& controls);
	bool baseCheck(VarStore& controls, VarStore& input);
	void setNewControls(VarStore& controls);


	void defineProblem(SATSolver& mng, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids);

	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);
	

	void normalizeInputStore();
	void abstractProblem();
	void growInputs(BooleanDAG* dag, BooleanDAG* oridag);
public:
	vector<Tvalue> find_history;
	VarStore ctrlStore;
	BooleanDAG* hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type);

	CEGISSolver(BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, CommandLineArgs& args);
	~CEGISSolver(void);

	virtual bool solve();
	void print_control_map(ostream& out);

	virtual void setup();

	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);
	


	void printDiagnostics(SATSolver& mng, char c);
	void printDiagnostics();

	void redeclareInputs(BooleanDAG* dag);
	

	void get_control_map(map<string, int>& values);
	void outputEuclid(ostream& fout);
	void setup2QBF(ostream& out);

	void outputCheckVarmap(ostream& out){
		dirCheck.outputVarMap(out);	
	}
};
