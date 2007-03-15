#ifndef FINDCHECKSOLVER_H_
#define FINDCHECKSOLVER_H_

#include "BooleanToCNF.h"

class SolverException{
	public:
	int code;
	string msg;
	SolverException(int code_p, const string& msg_p){ msg = msg_p; code = code_p; };
};



class Checkpointer{
	ofstream cptfile;
	bool doCheckpoint;
	public:
	virtual void checkpoint(char nm, vector<int>& ar);
	virtual void setCheckpoint(const string& filename);
	virtual void resizeInput(const string& name, int newsz);
	Checkpointer(){
		doCheckpoint = false;
	}
};




class FindCheckSolver{
	private:
	vector<int> ctrl;
	vector<int> input;	
	
	SATSolver& mngFind;
	varDir dirFind;	
	
	SATSolver& mngCheck;
	varDir dirCheck;
	
	int randseed;
	int iterlimit;
		
	
	map<string, int> controlVars;
	map<string, int> controlStarts;
	map<string, int> inputVars;
	map<string, int> inputStarts;
	//vector<bitVector> inputs;

	protected:
	
	Checkpointer cpt;
	
	int nseeds;
	//Reserved variable names.
	
	const string OUT;
	const string SOUT;
	
	int Nout;
	
	
	typedef vector<int>::const_iterator ctrl_iterator;
	
	virtual ctrl_iterator begin()const{
		return ctrl.begin();	
	}
	
	virtual ctrl_iterator end()const{
		return ctrl.end();
	}
	
	virtual void defineProblem(SATSolver& mng, varDir& dir)=0;
				

	virtual void buildChecker();	
	virtual void setupCheck();
	virtual void setNewControls(vector<int>& controls);
	virtual bool check(vector<int>& controls, vector<int>& input);
		
	virtual void buildFinder();
	virtual void setupFind();
	virtual void addInputsToTestSet(vector<int>& input);
	virtual bool find(vector<int>&  input, vector<int>&  controls);
	
	virtual void printDiagnostics(SATSolver& mng, char c);
	virtual void printDiagnostics();
	
	
	virtual bool solveCore();
	
	
	public:
	void outputCheckVarmap(ostream& out){
		dirCheck.outputVarMap(out);	
	}
	bool solveFromCheckpoint(istream& in);
	virtual void setCheckpoint(const string& filename);
	
	FindCheckSolver(SATSolver& finder, SATSolver& checker);
	void setIterLimit(int p_iterlimit);
	virtual void declareControl(const string& cname, int size);
	virtual void declareInput(const string& inname, int size);	
	virtual int getInSize(const string& input);
	virtual int getCtrlSize(const string& ctrl);
	virtual int getInStart(const string& ctrl);
	virtual int getCtrlStart(const string& ctrl);
	virtual int getInSize();
	virtual int getCtrlSize();
	virtual bool solve();
	virtual void setup();
	void set_randseed(int seed){ randseed = seed; };
};

#endif /*FINDCHECKSOLVER_H_*/
