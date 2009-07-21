#pragma once

#include "BooleanToCNF.h"
#include "FindCheckSolver.h"
#include "BooleanDAG.h"
#include "Tvalue.h"
#include "Checkpointer.h"
#include "VarStore.h"
#include <stack>

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

	VarStore ctrlStore;
	VarStore inputStore;

	int randseed;
	int iterlimit;

	bool printDiag;

	int NINPUTS;

	int nseeds;

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
	BooleanDAG* hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type);

	void normalizeInputStore();
	void abstractProblem();
	bool growInputs(BooleanDAG* dag, BooleanDAG* oridag);
public:
	CEGISSolver(BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, int p_nseeds=1, int NINPUTS_p=3);
	~CEGISSolver(void);

	virtual bool solve();
	void print_control_map(ostream& out);

	virtual void setup();

	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);
	


	void printDiagnostics(SATSolver& mng, char c);
	void printDiagnostics();



	void set_randseed(int seed){ randseed = seed; };
	void setIterLimit(int p_iterlimit){iterlimit = p_iterlimit; };

	void get_control_map(map<string, int>& values);
	void outputEuclid(ostream& fout);
	void setup2QBF();

	void activatePrintDiag(){
		printDiag = true;
	}
	
	void outputCheckVarmap(ostream& out){
		dirCheck.outputVarMap(out);	
	}
};
