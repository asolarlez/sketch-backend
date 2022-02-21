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

//#define REMOVE_SkVal

#ifndef REMOVE_SkVal
class Assignment_SkVal;
#endif

class BadConcretization {
public:
	string msg;
	BadConcretization(const string& s) :msg(s) {}
	BadConcretization() {}
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

/*!

This class is used to manage hard-coding of hole values during random concretization. 
It keeps track of which concretizations we have tried so we never try the same concretization 
twice. It also has the ability of declaring a problem UNSAT if we have tried a set of 
concretizations that covers all solutions. 
A possible solution is "covered" by a given set of concretizations if there is 
a concretization in that sets a subset of the variables in the assignment.
*/


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

	//For SAT problems with multiple harnesses, when we concretize the assignment in 
	//a later harness, we have to check that the concretization is consistent with 
	//the constraints that were imposed by the previous harnesses. This variable
	//is the SAT solver from the CEGIS solver so far.
	//Additionally, if you do decide to concretize a variable, you need to tell this SAT solver,
	//so that the harnesses that saw those variables before concretization will knwo what values to 
	//assign them.
	SolverHelper* sat;
	/*!
	This is a global sat solver that keeps track of 
	all the concretization attempts by the global SAT solver so that
	you never try the same assignment twice.
	*/

	//This SAT solver keeps track of all the concretizations we have tried so far. 
	//The set of satisfying assignments corresponds to all assignments that are not known
	//to be bad, so when this problem becomes UNSAT, it means that the concretizations we 
	//have tried cover the set of all solutions.
	SolverHelper* globalSat;


	vec<Lit> sofar;
	double totsize;
	int randdegree;
	RandDegreeControl degreeControl;

	
	/*!
	When hardcoding a hole that was used by a previous harness, 
	you introduce additional constraints to (*sat). If you switch
	harnesses before those constraints are dismissed through calling 
	solve, you will missattribute those constraints to the wrong harness.
	*/
//	bool pendingConstraints;

	double getAvg(vector<double>& vd){
		double rv = 0.0;
		if(vd.size()<1){ return 0.0; }
		for(size_t i=0; i<vd.size(); ++i){
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
        set_pendingConstraints(true);
	}
public:

    SolverHelper* get_globalSat()
    {
        return globalSat;
    }

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
		double scorenext = getAvg(degreeControl.scores[next]);
		double scorecur = getAvg(degreeControl.scores[cur]);
		cout << "cur=" << cur << " next=" << next << " scorecur=" << scorecur << " scorenext=" << scorenext << endl;
		if (scorenext < scorecur) {
			cout << "Switch because next is better"<<endl;
			rd[0] = next;
			rd[1] = cur;
		}else {
			float ratio = scorecur / scorenext;
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
	HoleHardcoder() :
		degreeControl(PARAMS->randdegree, PARAMS->nprocs),
		totsize(0.0),
		randdegree(PARAMS->randdegree)
	{
		
		MiniSATSolver* ms = new MiniSATSolver("global", SATSolver::FINDER);
		globalSat = new SolverHelper(*ms);
        set_pendingConstraints(false);
		hardcodeMinholes = false;
	}

	~HoleHardcoder(){		
		delete &globalSat->getMng();
		delete globalSat;
	}

// REDO

private:
    bool pendingConstraints;
	DepTracker dt;
public:

    void setCurrentHarness(int hid){
//        if(pendingConstraints){
//            int res = sat->getMng().solve();
//            if(res != SAT_SATISFIABLE){
//                return false;
//            }
//        }
        get_dt().setCurHarness(hid);
       // pendingConstraints = false;
    }

    DepTracker &get_dt();

    bool get_pendingConstraints();

    void set_pendingConstraints(bool val);

    void dismissedPending();

	void setCurHarness(int hid){
		get_dt().setCurHarness(hid);
		Assert(!get_pendingConstraints(), "There can't be any pending unchecked constraints!");
	}

	bool isDone(){
		return !globalSat->getMng().isOK();
	}

	double getTotsize(){
		return totsize;
	}
	void setHarnesses(int nharnesses){
		get_dt().setHarnesses(nharnesses);
	}

	void registerAllControls(map<string, BooleanDAG*>& functionMap) {
		for (auto it = functionMap.begin(); it != functionMap.end(); ++it) {
			BooleanDAG* bd = it->second;
			auto ctrl = bd->getNodesByType(bool_node::CTRL);
			for (size_t i = 0; i<ctrl.size(); ++i) {
				declareControl((CTRL_node*)ctrl[i]);
			}
		}
	}
	
	void declareControl(CTRL_node* node){
		globalSat->declareControl(node);
    string name = node->get_name();
		get_dt().declareControl(name);
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
			for (size_t i = 0; i < options.size(); ++i) {
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
		for (size_t i = 0; i<sofar.size(); ++i) {
			cout << ", " << toInt(sofar[i]);
		}
		cout << endl;
		globalSat->getMng().addHelperClause(sofar);

		get_dt().reset();

		((MiniSATSolver&)globalSat->getMng()).dump();
		randholes.clear();
		sofar.clear();
		totsize = 0.0;
        set_pendingConstraints(false);
	}
	
	void reset(){
		cout<<"SUMMRY ";
		for(size_t i=0; i<sofar.size(); ++i){
			cout<<", "<<toInt(sofar[i]);
		}
		cout<<endl;		
		get_dt().genConflict(sofar);
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

    bool solvePendingConstraints();
};








#endif
