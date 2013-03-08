#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>

using namespace std;

/*
struct saentry{
	int val; 
	int vers;
	int next;
};

class simarray{
	vector<int>* store;
	vector<int>* base;
	int deflt;
	int time;
public:

};
*/

const int UNSET = -22;
class cpvec{
	int bnd;
	cpvec* parent;
	//Entry 0 corresponds to the true value stored here.
	//Entry's 1 and 2 are cache locations to avoid having to traverse too much.
	int val[3];
	int idx[3];
	int flip;
public:
	int* vv;
	~cpvec(){
		if(vv != NULL){
			delete[] vv;
		}
	}

	void update(cpvec* pp, int ii, int v){
		Assert(vv==NULL, "qwejh;u88");
		parent = pp;
		if(parent==NULL){
			bnd = max(0, ii+1);
		}else{
			if(parent->idx[0] == UNSET && parent->vv == NULL && parent->parent != NULL){
				parent = parent->parent;
			}
			bnd = max(parent->bnd, ii+1);
		}		
		if(ii<0){
			idx[0] = UNSET;
		}else{
			idx[0] = ii;			
			val[0] = v;
		}
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}

	cpvec(cpvec* pp, int ii, int v):vv(NULL){		
		update(pp, ii, v);
	}
	cpvec(int sz):vv(new int[sz]){
		bnd = sz;		
		parent = NULL;
		idx[0] = UNSET;
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}
	cpvec(int sz, VarStore::objP* op):vv(new int[sz]){
		bnd = sz;	
		while(op != NULL){
			Assert(op->index < sz, "Out of bounds error in solver ;alkwebbn");
			vv[op->index] = op->getInt();
			op = op->next;
		}
		parent = NULL;
		idx[0] = UNSET;
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}
	int size() const{
		return bnd;
	}
	bool lget(int ii, int& rv){
		if(vv != NULL){ rv= vv[ii]; return true;}
		int b0 = idx[0]==ii;
		int b1 = idx[1]==ii;
		int b2 = idx[2]==ii;
		if(b0){ rv= val[0]; return true;}
		if(b1){ rv= val[1]; return true;}
		if(b2){ rv= val[2]; return true;}
		return false;
	}
	int get(int ii, int deflt){
		if(ii>=bnd){
			return deflt;
		}
		Assert(ii < bnd, "Out of bounds error in solver ;qek;kl");		
		int rv;
		cpvec* tt = this;
		while(tt != NULL && !tt->lget(ii, rv)){
			tt = tt->parent;
		}
		if(tt == NULL){ 
			return deflt; 
		}
		if(tt == this){ return rv; }
		//If the value was stored far away, we keep a copy closer.
		idx[1+flip] = ii;
		val[1+flip] = rv;
		flip = 1-flip;
		return rv;
	}
};

class NodeEvaluator :
	public NodeVisitor
{
	map<UFUN_node*, NodeEvaluator> recursives;
	map<string, BooleanDAG*>& functionMap;
	BooleanDAG& bdag;
	vector<int> values;
	vector<cpvec*> vecvalues;
	vector<bool> changes;
	vector<int> pendingChanges;
	vector<int> validResult;  // the values that does not violate Hassert
	VarStore* inputs;
	bool failedAssert;
	bool failedHAssert;
	bool trackChange;
	bool hasValidResult;
	int i(bool_node& bn){
		return values[bn.id];
	}

	bool b(bool_node& bn){
		return values[bn.id] == 1;
	}
	void setbn(bool_node& bn, int i){
		if(trackChange && hasValidResult){
			int id = bn.id;
			if (validResult[id] != i) {
				pendingChanges.push_back(id);
			}
			//changes[id] = changes[id] || (t!=i);
			values[id] = i;
		}else{
			values[bn.id] = i;
		}
	}

	void setbn(bool_node& bn, bool c){
		setbn(bn, c ? 1 : 0);
	}
public:
	NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p);
	~NodeEvaluator(void);
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ASSERT_node &node);		

	virtual void visit( ARR_R_node &node);
	virtual void visit( ARR_W_node &node);
	virtual void visit( ARR_CREATE_node &node);

	bool run(VarStore& inputs_p);
	void display(ostream& out);
	int scoreNodes();
	void trackChanges(){
		trackChange = true;
	}
	void printNodeValue(int i);

	int getValue(bool_node& bn){
		return i(bn);
	}
	int getValue(bool_node* bn){
		return i(*bn);
	}
	int getValidValue(bool_node* bn) {
		return validResult[bn->id];
	}
	bool hasValidValues() {
		return hasValidResult;
	}
};
