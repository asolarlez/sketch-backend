#ifndef HOLEHARDCODER_H_
#define HOLEHARDCODER_H_ 1

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "DagOptim.h"

class BadConcretization{


};



#include "MiniSATSolver.h"

class DepTracker{
	map<string, int> ctrlIdx;
	vector<set<int> > harnessPerHole; // holeId -> [harnesses]
	vector<set<int> > holesPerHarness; // harnesId -> {holes}
	vector<vector<Lit> > decisionsPerHarness;
	int curHarness;
public:

	void reset(){
		for(int i=0; i<holesPerHarness.size(); ++i){
			holesPerHarness[i].clear();
			decisionsPerHarness[i].clear();
		}
		for(int i=0; i<harnessPerHole.size(); ++i){
			harnessPerHole[i].clear();
		}
	}

	void genConflict(vec<Lit>& vl){
		genConflict(curHarness, vl);
	}
	void recordDecision(const guardedVal& gv){
		decisionsPerHarness[curHarness].push_back(lfromInt(-gv.guard));
	}
	void genConflict(int harnid, vec<Lit>& vl);
	void helper(int harnid, vector<char>& visited, set<int>& out);
	void declareControl(string const & ctrl){
		if(ctrlIdx.count(ctrl)==0){
			ctrlIdx[ctrl] = ctrlIdx.size();
			harnessPerHole.push_back(set<int>());
		}
	}
	void setCurHarness(int hid){
		curHarness = hid;
	}
	void setHarnesses(int nharnesses){
		holesPerHarness.resize(nharnesses);
		decisionsPerHarness.resize(nharnesses);		
	}


	void regHoleInHarness(string const & hname){
		int hole = ctrlIdx[hname];
		harnessPerHole[hole].insert(curHarness);
		holesPerHarness[curHarness].insert(hole);
	}

};


class HoleHardcoder{
	
	bool LEAVEALONE(int v){ return v < 0; }
	static const int REALLYLEAVEALONE = -8888888;
	map<string, int> randholes;			
	vector<bool> chkrbuf;
	SolverHelper* sat;
	SolverHelper* globalSat;
	vec<Lit> sofar;
	int fixValue(CTRL_node& node, int bound, int nbits);
	long long int totsize;
public:
	DepTracker dt;
	HoleHardcoder(){		
		totsize = 1;
		MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
		globalSat = new SolverHelper(*ms);
	}
	~HoleHardcoder(){		
		delete &globalSat->getMng();
		delete globalSat;
	}

	bool isDone(){
		return !globalSat->getMng().isOK();
	}

	long long int getTotsize(){
		return totsize;
	}
	void setHarnesses(int nharnesses){
		dt.setHarnesses(nharnesses);
	}
	int recordDecision(const guardedVal& gv);
	void declareControl(CTRL_node* node){
		globalSat->declareControl(node);
		dt.declareControl(node->get_name());
	}
	void reset(){
		cout<<"SUMMRY ";
		for(int i=0; i<sofar.size(); ++i){
			cout<<", "<<toInt(sofar[i]);
		}
		cout<<endl;
		globalSat->getMng().reset();

		dt.genConflict(sofar);
		cout<<"POST-SUMMRY ";
		for(int i=0; i<sofar.size(); ++i){
			cout<<", "<<toInt(sofar[i]);
		}
		cout<<endl;
		globalSat->getMng().addHelperClause(sofar);
		
		dt.reset();

		((MiniSATSolver&) globalSat->getMng()).dump();
		randholes.clear();
		sofar.clear();
		totsize=1;
	}
	void setSolver(SolverHelper* sh){
		sat = sh;
	}
	bool_node* checkRandHole(CTRL_node* node, DagOptim& opt);
	void afterInline();
	void printControls(ostream& out);
	bool hasValue(const string& s){
		map<string, int>::iterator it = randholes.find(s);
		if(it == randholes.end()){ return false; }
		return (!LEAVEALONE(it->second));
	}
};








#endif
