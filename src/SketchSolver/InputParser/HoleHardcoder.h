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
	void recordDecision(const Lit l){
		decisionsPerHarness[curHarness].push_back(l);
	}
	void genConflict(int harnid, vec<Lit>& vl);
	void helper(int harnid, vector<char>& visited, set<int>& out);
	void declareControl(string const & ctrl){
		if(ctrlIdx.count(ctrl)==0){
			int sz = ctrlIdx.size();
			ctrlIdx[ctrl] = sz;
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
	double totsize;


	/**
	When hardcoding a hole that was used by a previous harness, 
	you introduce additional constraints to (*sat). If you switch
	harnesses before those constraints are dismissed through calling 
	solve, you will missattribute those constraints to the wrong harness.
	*/
	bool pendingConstraints;
public:
	DepTracker dt;
	HoleHardcoder(){		
		totsize = 1.0;
		MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
		globalSat = new SolverHelper(*ms);
		pendingConstraints = false;
	}
	~HoleHardcoder(){		
		delete &globalSat->getMng();
		delete globalSat;
	}

	void addedConstraint(){
		pendingConstraints = true;
	}

	void dismissedPending(){
		pendingConstraints=false;
	}

	bool checkHarnessSwitch(int hid){
		if(pendingConstraints){
			int res = sat->getMng().solve();
			if(res != SATSolver::SATISFIABLE){
				return false;
			}
		}
		dt.setCurHarness(hid);
		pendingConstraints = false;
		return true;
	}

	void setCurHarness(int hid){
		dt.setCurHarness(hid);
		Assert(!pendingConstraints, "There can't be any pending unchecked constraints!");
	}

	bool isDone(){
		return !globalSat->getMng().isOK();
	}

	double getTotsize(){
		return totsize;
	}
	void setHarnesses(int nharnesses){
		dt.setHarnesses(nharnesses);
	}
	int recordDecision(const gvvec& options, int rv, int bnd, bool special);
	void declareControl(CTRL_node* node){
		globalSat->declareControl(node);
    string name = node->get_name();
		dt.declareControl(name);
	}

	void tryHarder(){
		cout<<"Got too big, trying harder to concretize before="<<randholes.size();
		for(map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ){
			if(it->second < 0){
				randholes.erase(it++);
			}else{
				++it;
			}
		}
		cout<<"  after ="<<randholes.size()<<endl;
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
		totsize=1.0;
		pendingConstraints = false;
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
