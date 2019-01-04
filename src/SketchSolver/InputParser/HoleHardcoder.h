#ifndef HOLEHARDCODER_H_
#define HOLEHARDCODER_H_ 1

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "MiniSATSolver.h"

/*
Excelption class used to signal that a concretization failed.
*/
class BadConcretization {
public:
	string msg;
	BadConcretization(const string& s) :msg(s) {}
	BadConcretization() {}
};

/*
This class tracks dependencies between holes and harnesses. 
*/
class DepTracker{
	/*Each hole has an index*/
	map<string, int> ctrlIdx;
	vector<set<int> > harnessPerHole; // holeId -> [harnesses]
	vector<set<int> > holesPerHarness; // harnesId -> {holes}

	/*In this variable, we record all the decisions that have been made during each harness.*/
	vector<vector<Lit> > decisionsPerHarness;
	int curHarness;
	

	/*
	out tracks all the literals that were set leading to a concretization failure. 
	If harnid failed, first, we will add all the holes that were concretized for harness harnid.
	But then it could also be that the problem was not the concretization of holes in harnid, but of holes somewhere else.
	For example, suppose you have a harness that says
	assert H_1 == H_2.
	Then you have another harness that says 
	assert H_2 > 5;

	If I concretize H_1 to 3, that could lead to an assertion failure in the second harness despite the fact that 
	the only hole (H_2) in that harness was not concretized. 
	Therefore, we must include as dependencies not just the holes in that harness, but holes in all other harnesses
	that transitively share holes with the currentt harness.

	*/
	void helper(int harnid, vector<char>& visited, set<int>& out);

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



class RandDegreeControl {	
public:
	vector<int> currentRandDegs;
	map<int, vector<double> > scores;
	RandDegreeControl(int randdegree, int nprocs) : currentRandDegs(2) {
		if (randdegree == 0 ) {
			int rr = rand() % nprocs;
			int t = 2;
			rr = min(10, rr / 2);
			t = t << rr;
			currentRandDegs[0] = 5*t;
			currentRandDegs[1] = 5;
		}	
	}

};


class HoleHardcoder{
	
	bool LEAVEALONE(int v){ return v < 0; }
	static const int REALLYLEAVEALONE = -8888888;
	/*!
	randholes keeps track of which holes have been considered for concretization.
	If it was decided that the hole would be concretized, then this stored the value to which it was concretized.
	If it was decided not to concretize, this records the score. When the hole is revisited, we may change our mind
	about not concretizing if the score changes significantly.
	*/
	map<string, int> randholes;	


	/*
	settledHoles keeps track of holes whose value is already known and therefore should be concretized to that value. The reason to keep them as a separate map is that
	whereas randholes represent just random guesses on the value of a hole, settledHoles represent holes whose value is a consequence of other decisions.
	*/
	map<string, int> settledHoles;

	set<string> minholes;
	bool hardcodeMinholes;


	vector<bool> chkrbuf;
	/*!
	This is the sat solver from the current cegis solver, which we use to tell us whether a candidate
	concretization is consistent with the constraints so far.
	*/
	SolverHelper* sat;
	/*!
	This is a global sat solver that keeps track of 
	all the concretization attempts by the global SAT solver so that
	you never try the same assignment twice.
	*/
	SolverHelper* globalSat;
	vec<Lit> sofar;
	double totsize;
	int randdegree;
	DepTracker dt;
	RandDegreeControl degreeControl;

	
	/*!
	When hardcoding a hole that was used by a previous harness, 
	you introduce additional constraints to (*sat). If you switch
	harnesses before those constraints are dismissed through calling 
	solve, you will missattribute those constraints to the wrong harness.
	*/
	bool pendingConstraints;

	double getAvg(vector<double>& vd){
		double rv = 0.0;
		if(vd.size()<1){ return 0.0; }
		for(int i=0; i<vd.size(); ++i){
			rv += vd[i];
		}
		return rv / vd.size();
	}


	int nextRandDegree(int cur) {
		unsigned int v = rand() % 800;
		unsigned x = v;
		int t = 1024;
		while (x > 0) {
			t = t / 2;
			x = x / 2;
		}
		if (t < 2) { t = 2; }
		if (v % 2 == 0) {
			return cur * t;
		}
		else {
			if (cur / t > 2) {
				return cur / t;
			}
			else {
				return cur * 2;
			}
		}
	}


	/*!
	Computes a numerical score that determines how strongly we want to concretize this hole.
	*/
	int computeHoleScore(CTRL_node* node);
	

	int recordDecision(const gvvec& options, int rv, int bnd, bool special);
	void addedConstraint() {
		pendingConstraints = true;
	}
public:


	void settleHole(const string& name, int value) {
		settledHoles[name] = value;
	}


	bool isSettled(const string& name, int& out) {
		auto it = settledHoles.find(name);
		if (it != settledHoles.end()) {
			out = it->second;
			return true;
		}
		return false;
	}

    int fixValue(CTRL_node& node, int bound, int nbits);

	void get_control_map(map<string, string>& values);
	
	/*
	This function updates the current set of rand degrees using an MCMC style search to try to 
	converge to the best rand degree.
	*/
	void adjust(){
		vector<int>& rd = degreeControl.currentRandDegs;
		int cur = rd[0];
		int next = nextRandDegree(rd[0]);
		if (next == cur) {
			return;
		}
		int scorenext = getAvg(degreeControl.scores[next]);
		int scorecur = getAvg(degreeControl.scores[cur]);
		cout << "cur=" << cur << " next=" << next << " scorecur=" << scorecur << " scorenext=" << scorenext << endl;
		if (scorenext < scorecur) {
			cout << "Switch because next is better"<<endl;
			rd[0] = next;
			rd[1] = cur;
		}else {
			float ratio = scorecur / (float)scorenext;
			if (rand() % 1000 < ratio * 1000) {
				cout << " Switch by luck"<<endl;
				rd[0] = next;
				rd[1] = cur;
			}else{
				cout << " Stay with what we had " <<rd[0]<<", "<<rd[1]<<endl;
			}
		}

	}

	void addScore(double score) {
		degreeControl.scores[randdegree].push_back(score);
	}

	void setRanddegree(int step){
		randdegree = degreeControl.currentRandDegs[step % 2];
	}
	int getRanddegree(){
		return randdegree;
	}
	HoleHardcoder():
		degreeControl(PARAMS->randdegree, PARAMS->nprocs),
		totsize(0.0),
		randdegree(PARAMS->randdegree)
	{
		
		MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
		globalSat = new SolverHelper(*ms);
		pendingConstraints = false;		
		hardcodeMinholes = false;
	}

	~HoleHardcoder(){		
		delete &globalSat->getMng();
		delete globalSat;
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

	void registerAllControls(map<string, BooleanDAG*>& functionMap) {
		for (auto it = functionMap.begin(); it != functionMap.end(); ++it) {
			BooleanDAG* bd = it->second;
			vector<bool_node*>& ctrl = bd->getNodesByType(bool_node::CTRL);
			for (int i = 0; i<ctrl.size(); ++i) {
				declareControl((CTRL_node*)ctrl[i]);
			}
		}
	}
	
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

	MiniSATSolver* getMiniSat(){
		return (MiniSATSolver*) &(globalSat->getMng());
	}

	
	
	/*!
	This should be called when minimization is on and concretization succeeds
	but we want to keep trying to see if a better solution is possible.
	*/
	void resetForMinimize(map<string, string>& currentControls) {
		resetCore();
		//Now, we need to add constraints that ensure that we never see 
		//a worse assignment to the minimization variables.
		cout << "Adding constraints to minimize" << endl;
		for (auto mhit = minholes.begin(); mhit != minholes.end(); ++mhit) {
			string& sval = currentControls[*mhit];
			int val = atoi(sval.c_str());
			Tvalue& glob = globalSat->getControl(*mhit);
			Assert(glob.isSparse(), "Control must be sparse");
			const gvvec& options = glob.num_ranges;
			for (int i = 0; i < options.size(); ++i) {
				const guardedVal& gv = options[i];
				if (gv.value > val) {
					cout << *mhit << ":" << -gv.guard << endl;
					globalSat->addAssertClause(-gv.guard);
				} else if (gv.value == val) {
					cout << *mhit << ":sofar:" << -gv.guard << endl;
					sofar.push(lfromInt(-gv.guard));
				}
			}
		}
		globalSat->getMng().addHelperClause(sofar);
		sofar.clear();
		hardcodeMinholes = true;
	}
	
	void resetCore() {
		globalSat->getMng().reset();
		cout << "POST-SUMMRY ";
		for (int i = 0; i<sofar.size(); ++i) {
			cout << ", " << toInt(sofar[i]);
		}
		cout << endl;
		globalSat->getMng().addHelperClause(sofar);

		dt.reset();

		((MiniSATSolver&)globalSat->getMng()).dump();
		randholes.clear();
		sofar.clear();
		totsize = 0.0;
		pendingConstraints = false;
	}
	
	void reset(){
		cout<<"SUMMRY ";
		for(int i=0; i<sofar.size(); ++i){
			cout<<", "<<toInt(sofar[i]);
		}
		cout<<endl;		
		dt.genConflict(sofar);
		resetCore();		
	}

	void setSolver(SolverHelper* sh){
		sat = sh;
	}

	/*!
	Attempt to concretize this hole based on how many children it has, among other things.
	*/
	bool_node* checkRandHole(CTRL_node* node, DagOptim& opt);
	void afterInline();
	void printControls(ostream& out);
	bool hasValue(const string& s){
		map<string, int>::iterator it = randholes.find(s);
		if(it == randholes.end()){ return false; }
		return (!LEAVEALONE(it->second));
	}
    int getValue(const string& s) {
        return randholes[s];
    }
};








#endif
