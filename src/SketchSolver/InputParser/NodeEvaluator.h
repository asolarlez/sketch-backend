#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>

#include <iostream>

using namespace std;

/*Started creating a class to represent tuples */
class cptuple{
	int bnd;
public:
    
	int *vv;
    ~cptuple(){
		if(vv != NULL){
			delete[] vv;
		}
	}
	cptuple(int n):vv(new int[n]){
		memset(vv, 0, n*sizeof(int));
        bnd = n;
    }
    int size() const{
		return bnd;
	}
   


};



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
		// NOTE xzL: set uninitialized value to be 0
		memset(vv, 0, sz*sizeof(int));
		bnd = sz;		
		parent = NULL;
		idx[0] = UNSET;
		idx[1] = UNSET;
		idx[2] = UNSET;
		flip = 0;
	}
	cpvec(int sz, VarStore::objP* op):vv(new int[sz]){
		// NOTE xzL: set uninitialized value to be 0
		memset(vv, 0, sz*sizeof(int));
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
		if(vv != NULL){ 
			if(ii>=bnd){ return false; }		
			rv= vv[ii]; 
			return true;
		}

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
	void print(ostream & os) {
		// recursively print the structure of a cpvec
		os << bnd << "[";
		for (int i=0; i<bnd; i++) {
			os << get(i, -1) << " ";
		}
		for (int i=0; i<3; i++) {
			os << idx[i] << "," << val[i] << " ";
		}
		if (parent) {
			parent->print(os);
		}
		os << "]";
	}
};

class NodeEvaluator :
	public NodeVisitor
{
protected:
	map<UFUN_node*, NodeEvaluator> recursives;
	map<string, BooleanDAG*>& functionMap;
	BooleanDAG& bdag;
	vector<int> values;
	vector<cpvec*> vecvalues;
    vector<cptuple*> tuplevalues;
	vector<bool> changes;
	VarStore* inputs;
	bool failedAssert;
	bool failedHAssert;
	bool trackChange;
	int i(bool_node& bn){
		return values[bn.id];
	}

	bool b(bool_node& bn){
		return values[bn.id] == 1;
	}
	void setbn(bool_node& bn, int i){
		if(trackChange){
			int id = bn.id;
			int& t = values[id];
			changes[id] = changes[id] || (t!=i);
			t = i;
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
    virtual void visit( TUPLE_CREATE_node &node);
    virtual void visit( TUPLE_R_node &node);

	bool run(VarStore& inputs_p);
	void display(ostream& out);
	// get unchanged node, but only starting from start
	int scoreNodes(int start = 0);
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
};
