#pragma once
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "NodeStore.h"
#include <map>

using namespace std;

class setComp{
public:
	bool operator()(const set<int>* a, const set<int>* b) const{
		return *a < *b;
	}
};


typedef int inputId;


class inputNode{
public:
	bool_node* node;
	set<int> interfs;
	map<int, int> distances;
};

inline int nodeDistance(const inputNode& n1, const inputNode& n2){

	set<int>::const_iterator it1 = n1.interfs.begin();
	set<int>::const_iterator it2 = n2.interfs.begin();
	int cnt = 0;
	while(true){
		if(it1 == n1.interfs.end() || it2 == n2.interfs.end() ){
				break;
		}
		if(*it1 == *it2){
			++cnt;
			++it1;
			++it2;
		}else{			
			if(*it1 < *it2){
				++it1;
			}else{
				++it2;
			}
		}
		
	}
	return cnt;
}


typedef map<set<int>*, int, setComp>::iterator setMapIter;
typedef map<int, set<int>*>::iterator invSetMapIter;
typedef vector<pair<int, set<int>* > >::iterator interfIter;

class DagOptimizeCommutAssoc;

class CAoptimizer{

	vector<pair<int, set<int>* > > interfaces; //positive
	BooleanDAG* dag;
	bool_node::Type bnType;
	DagOptimizeCommutAssoc* parent;
	map<set<int>*, int ,setComp> setMap; //maps from a set to a nodeID corresponding to that set.
	map<int, set<int>* > setMapInv;
	
	map<int, set<int> > interfToInput;
	map<inputId, inputNode > inputToInterf; //This map says, for each input node, which interface nodes it flows to.

	bool_node* addGeneral(bool_node& bn, bool isInternal);
	set<int>* getSet(bool_node* bn);
	bool_node* existingNode(set<int>* cset);

	void addInternal(bool_node& bn, set<int>* cset);
	void addInterface(bool_node& bn, set<int>* cset);

	bool_node* lnewNode(bool_node* mother, bool_node* father);
	bool_node* getNode(int id);
	bool_node*  mergeInputs(int input1, int input2, map<inputId, inputNode >& inputToInterf, map<int, set<int> >& interfToInput, map<pair<int, int>, int>& distances, bool mngDistances = false);

public:
	void setDag(BooleanDAG* p_dag){ dag = p_dag; }
	
	void setType(bool_node::Type t){ bnType = t; }

	void setParent(DagOptimizeCommutAssoc* p_parent){ parent = p_parent; }

	/*!
		If this function returns true, then the output node is the node that should replace bn in the dag.
	*/
	bool_node* checkNode(bool_node& bn);

	bool_node* replacement(bool_node* bn);

	void computeCommonSubs();
	pair<int, int> selectPairToMerge(map<pair<int, int>, int>& distances);

};



class DagOptimizeCommutAssoc :
	public NodeVisitor, public virtual NodeStore
{
public:
	typedef map<bool_node::Type, CAoptimizer>::iterator optiterator;
	map<bool_node::Type, CAoptimizer> optimizers;
	BooleanDAG* dag;
	bool_node* getNode(int id);
public:
	DagOptimizeCommutAssoc(void);
	virtual void process(BooleanDAG& bdag);

	virtual void visitACNode(bool_node& bn);
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	/*
	virtual void visit( XOR_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	*/

public:
	~DagOptimizeCommutAssoc(void);
};
