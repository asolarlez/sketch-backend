#pragma once

#include "BooleanToCNF.h"
#include "FindCheckSolver.h"
#include "BooleanDAG.h"
#include "Tvalue.h"
#include "Checkpointer.h"

template<typename T>
int intFromBV(T bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){		
		if( bv[start + i] > 0){
			nval += t;	
		}
		t = t*2;
	}
	return nval;
}


class VarStore{
private:


public:
	class objP{
	public:
		string name;
		vector<int> vals;
		objP(const string& nm, int size):name(nm),vals(size){	}
		objP(const objP& old):vals(old.vals), name(old.name){}
		objP& operator=(const objP& old){ vals=old.vals; name=old.name; return *this;}
		int size(){ return vals.size(); }
		int resize(int n){ vals.resize(n); return n; }
		void setBit(int i, int val){ vals[i] = val; }
		int getInt() const{
			return intFromBV(vals, 0, vals.size());
		}
		void printBit(ostream& out) const{
			for(int i=0; i<vals.size(); ++i){
				out<<vals[i];
			}
		}
		void makeRandom(){
			for(int i=0; i<vals.size(); ++i){
				vals[i] = (rand() & 0x1) > 0? -1 : 1;
			}
		}
	};

private:
	vector<objP> objs;
	map<string, int> index;
	int bitsize;
public:
	VarStore(){
		bitsize=0;
	}
	objP& getObj(const string& name){ 
		return objs[index[name]];
	}
	
	typedef vector<objP>::iterator iterator;
	iterator begin(){ return objs.begin(); }
	iterator end(){ return objs.end(); }
	void makeRandom(){ 
		for(int i=0; i<objs.size(); ++i){
			objs[i].makeRandom();
		}
	}
	void newVar(const string& name, int size){
		Assert(index.count(name)==0, "This variable already existed!!");
		objs.push_back(objP(name, size));
		index[name] = objs.size()-1;
		bitsize += size;
	}
	void resizeVar(const string& name, int size){
		objP& tmp = getObj(name);
		bitsize -= tmp.size();
		tmp.resize(size);
		bitsize += size;		
	}
	int getBitsize() const{ 
		return bitsize;
	}
	int getIntsize(){
		return objs.size();
	}
	void printBrief(ostream& out) const{
		for(int i=0; i<objs.size(); ++i){
			objs[i].printBit(out);
		}
	}
	bool contains(const string& name) const{
		return index.count(name)>0;
	}
	vector<int> serialize() const{
		vector<int> out;
		for(int t=0; t<objs.size(); ++t){
			const objP& tmp = objs[t];
			out.insert(out.end(), tmp.vals.begin(), tmp.vals.end());			
		}
		return out;
	}
	void setBit(int i, int val){
		bool found = false;
		for(int t=0; t<objs.size(); ++t){
			objP& tmp = objs[t];
			if(i < tmp.size()){
				tmp.setBit(i, val);
				found = true;
				break;
			}else{
				i = i-tmp.size();
			}
		}
		Assert(found, "This is a bug");
	}
	int operator[](const string& name) {
		return objs[index[name]].getInt();
	}
};


class CEGISSolver
{
	BooleanDAG* problem;	
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

	vector<Tvalue> node_ids;
	vector<Tvalue> f_node_ids;
	vector<bool> f_flags;
	map<string, int> last_input;
	bool firstTime;
protected:
	void declareControl(const string& cname, int size);
	void declareInput(const string& cname, int size);
	bool solveCore();

	bool find(VarStore& input, VarStore& controls);
	void addInputsToTestSet(VarStore& input);

	bool check(VarStore& input, VarStore& controls);
	bool baseCheck(VarStore& controls, VarStore& input);
	void setNewControls(VarStore& controls);


	void defineProblem(SATSolver& mng, SolverHelper& dir, map<bool_node*,  int>& node_values);

	
	int valueForINode(INTER_node* inode, VarStore& values, int& nbits);
	BooleanDAG* hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type);
public:
	CEGISSolver(BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, int p_nseeds=1, int NINPUTS_p=3);
	~CEGISSolver(void);

	virtual bool solve();
	

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
