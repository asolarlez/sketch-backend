#ifndef DAGELIMUFUN_H_
#define DAGELIMUFUN_H_


#include "BooleanDAG.h"
#include "SATSolver.h"
#include <set>



using namespace std;



class t_node{
	public:
	bool_node* control;
	int nidx;	
	vector<t_node*> children;
	bool_node* node;
	
	
	t_node(bool_node* c): control(c),nidx(0), node(NULL){};		
	
	 
	
	void print(ostream& out){
		set<t_node*> ts;
		out<<" BEGIN ----------"<<endl;
		print(out, ts);
		out<<" END   ----------"<<endl;
	}
	
	string tostring(){
		string tmp = control->get_name();
		tmp += "_";
		if(nidx==0){
			tmp += "_NOT";	
		}			
		return tmp;
	}
	
	void print(ostream& out, set<t_node*>& ts){
		if( ts.find(this) == ts.end()){
			ts.insert(this);
			for(int i=0; i<children.size(); ++i){
				out<<tostring()<<" -> "<<children[i]->tostring()<<endl;
				children[i]->print(out, ts);	
			}			
		}
	}	
	
	
	bool_node* guard(vector<bool_node*>& store){
		if( nidx == 0){
			NOT_node* nn = new NOT_node();
			nn->mother = control;
			nn->addToParents();
			store.push_back(nn);
			return nn;
		}else{
			return control;	
		}
	}
	
	
	bool_node* childDisjunct(vector<bool_node*>& store){
		Assert( children.size() > 0 , "This function is being misused");
		bool_node* cur = children[0]->node;
		for(int j=1; j<children.size(); ++j){
			OR_node* on = new OR_node();
			on->mother = cur;
			on->father = children[j]->node;
			on->addToParents();
			cur = on;
			store.push_back(on);							
		}
		return cur;
	}
	
	bool_node* circuit(vector<bool_node*>& store){		
		if( children.size() == 0){
			node = guard(store);
		}else{
			bool_node* cur = childDisjunct(store);
			AND_node* anode = new AND_node();
			anode->mother = cur;
			anode->father = guard(store);
			anode->addToParents();
			store.push_back(anode);
			node = anode;
		}
		return node;
	}
	
	
};

class build_tnodes{

char buf[100];

map<string, t_node*> visited;

public:
vector<bool_node*> store;
virtual ~build_tnodes(){
	for(map<string, t_node*>::iterator it = visited.begin(); it!= visited.end(); ++it){
		delete it->second;	
	}
}


void tn_build(bool_node* bn, bool_node* parent, t_node* partn){
	if( typeid(*bn) == typeid(ARRACC_node)){
		ARRACC_node* an = dynamic_cast<ARRACC_node*>(bn);		
		for(int i=0; i<an->multi_mother.size(); ++i){
			if( an->multi_mother[i] == parent ){
				sprintf(buf, "%dI%d", an->mother, i);
				string tmp(buf);
				if(visited.find(tmp) != visited.end()){
					t_node* tn = visited[tmp];
					partn->children.push_back(tn);
				}else{				
					t_node* tn = new t_node(an->mother);
					tn->nidx = i;
					partn->children.push_back(tn);
					visited[tmp] = tn;					
					for(int j=0; j<an->children.size(); ++j){						
						tn_build(an->children[j], bn, tn);						
					}
					
					tn->circuit(store);
					
				}
			}
		}
		return;
	}	
	for(int j=0; j<bn->children.size(); ++j){						
		tn_build(bn->children[j], bn, partn);			
	}
}

};









class SFunInfo{
	public:
	BooleanDAG* fun;
	vector<bool_node* > actuals;
	bool_node* symval;	
	bool_node* outval;
	int step;	
	bool moreNewFuns;
	SFunInfo():
	fun(NULL),
	symval(NULL),
	outval(NULL),
	step(0),
	moreNewFuns(true)
	{
			 
	}
};



class DagElimUFUN : public NodeVisitor
{
	
	bool oneMoreFun;
	
	build_tnodes tnbuilder;
	
	map<int, BooleanDAG> comparators;	
	map<string, SFunInfo> functions;
	BooleanDAG& getComparator(int sz);
	
	vector<bool_node*> newnodes;
	bool_node* rvalue;
	int dagsize;
	vector<BooleanDAG> bdags;
public:
	DagElimUFUN();
	virtual ~DagElimUFUN();
	bool_node* produceNextSFunInfo( UFUN_node& node  );
	
	virtual void stopProducingFuns();
	
	virtual void visit( SRC_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( CONST_node& node );
	
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
		
	virtual void visit( NOT_node& node );
	
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	
	virtual void visit( GT_node& node );
	virtual void visit( GE_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( LE_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	
	virtual void visit( ASSERT_node &node);	
	virtual void visit( DST_node& node );
	
	virtual void process(BooleanDAG& bdag);
};

#endif /*DAGELIMUFUN_H_*/
