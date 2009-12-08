#ifndef EXTRACTEVALUATIONCONDITION_H_
#define EXTRACTEVALUATIONCONDITION_H_

#include "BooleanDAG.h"
#include "NodeStore.h"
#include <set>
#include <typeinfo>

using namespace std;


/**
 * 
 * See info for ExtractEvaluationCondition before reading this comment.
 * 
 * The boolean formula corresponding to the guard for a node 
 * is represented as a network of t-nodes.
 * Each t-node corresponds to a clause of the form 'control==nidx'.
 * In the rest of this comment I refer to this clause as tn.C(I) for a given t_node tn.
 * Additionally, there is a function P(I) associated with each t_node under the following definition:
 * 
 * tn.P(I) :=  tn.C(I) & ( \exists tn' in children tn'.P(I) )
 * 
 * In other words, tn.P(I) will be true if there is a path from n to the end of the dag 
 * where all the C(I) along that path are true.
 * 
 * for a given t_node, tn.P(I) is stored in the node field of the t_node, and it is a function of the 
 * tn.P(I) of its children.
 * 
 */


class t_node{
	public:
	bool_node* m;
	bool_node* f;


	/**
	 * tn.C(I) := control == nidx.
	 * 
	 */
	bool_node* control;
	int nidx;	
	vector<t_node*> children;
	
	/**
	 * node is a pointer to a bool_node corresponding to tn.P(I). 
	 * 
	 */
	bool_node* node;
	
	
	t_node(bool_node* c): control(c),nidx(0), node(NULL){};		
	
	 
	
	void print(ostream& out){
		set<t_node*> ts;
		out<<" BEGIN ----------"<<endl;
		print(out, ts);
		out<<" END   ----------"<<endl;
	}
	
	string tostring(){
		if(control == NULL) return "NULL";
		stringstream str;
		if(nidx == 0){
			str<<" ! ";
		}
		str<<control->get_name()<<":";
		str<<control->id;		
    	return str.str();
		/*
		string tmp = control->get_name();		
		tmp += "_";
		if(nidx==0){
			tmp += "_NOT";	
		}			
		return tmp; */
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
	
	
	bool_node* guard(NodeStore& store){
		if( nidx == 0){
			NOT_node* nn = new NOT_node();
			nn->name = control->name;
			nn->name += "FAFUFO";
			nn->mother = control;
			nn->addToParents();
			store.setTimestamp(nn);
			store.addNode(nn);
			return nn;
		}else{
			return control;	
		}
	}
	
	/**
	 * 
	 * childDisjunct returns a boolean node corresponding to the formula 
	 * 
	 * ( \exists tn' in children tn'.P(I) )
	 * 
	 * The store is a vector where we will push any new bool_nodes that get allocated by this function.
	 */
	bool_node* childDisjunct(NodeStore& store){
		Assert( children.size() > 0 , "This function is being misused");
		bool_node* cur = children[0]->node;
		
		for(int j=1; j<children.size(); ++j){
			OR_node* on = new OR_node();
			on->mother = cur;
			on->father = children[j]->node;
			Assert( on->mother != NULL, "Mother can't be null");
			Assert( on->father != NULL, "Father can't be null");
			on->addToParents();
			store.setTimestamp(on);
			cur = on;
			store.addNode(on);
		}
		return cur;
	}
	
	
	/**
	 * 
	 * circuit returns a boolean node corresponding to the formula 
	 * 
	 * tn.C(I) & ( \exists tn' in children tn'.P(I) )
	 * 
	 * The store is a vector where we will push any new bool_nodes that get allocated by this function.
	 */
	bool_node* circuit(NodeStore& store){		
		if( children.size() == 0){
			node = guard(store);
		}else{
			bool_node* cur = childDisjunct(store);
			AND_node* anode = new AND_node();
			anode->mother = cur;
			anode->father = guard(store);
			anode->addToParents();
			store.setTimestamp(anode);
			store.addNode(anode);
			node = anode;
		}
		return node;
	}

};


/**
 * This class creates a circuit P(I) from a node n 
 * such that P(I) is true iff the value of n can flow
 * to the output.  
 * 
 * 
 * In order to determine flow, it looks only at ARRACC nodes, 
 * so the circuit P(I) includes the branch conditions of all ARRACC nodes 
 * that are reachable from node n. Note that because it looks at ARRACC nodes, 
 * any transformation that replaces ARRACC nodes with, say, ANDs and ORs will make
 * the resulting P(I) overly conservative.  
 * 
 * To use this class, simply call t_build(n, parent, partn)
 * where n is the node you are interested in. 
 * parent should be NULL (it is used internally when n is called).
 * And partn is a fresh t_node.
 * 
 * Note: Need a cleaner interface.
 * 
 * 
 */


class ExtractEvaluationCondition{

char buf[100];

map<string, t_node*> visited;



public:

vector<t_node*> garbage;
map<bool_node* , t_node*> tvisited;

int ivisit;

virtual void reset(){
	for(map<string, t_node*>::iterator it = visited.begin(); it!= visited.end(); ++it){
		delete it->second;
	}
	for(vector<t_node*>::iterator it = garbage.begin(); it!= garbage.end(); ++it){
		delete *it;
	}
	visited.clear();
	garbage.clear();
	tvisited.clear();
		
}


virtual ~ExtractEvaluationCondition(){
	reset();
}



bool_node* get_exe_cond(bool_node* bn, NodeStore& store, bool print_flag = false){
	t_node* tn = new t_node(NULL);
	tn_build(bn, NULL, tn, store);
	bool_node* rv = NULL;
	if(tn->children.size() > 0){ 
		rv = tn->childDisjunct(store);
		if(print_flag){ tn->print(cout); } 
	}
	garbage.push_back(tn);
	return rv;
}


/*
 * Each ARRACC bn node that is a descendant of n will have associated with it 
 * a set of t_nodes, one for each input of ARRACC into which n can flow.
 * 
 * tn build builds the t_node graph recursively. Initially it is called with 
 * bn = n
 * partn = a fresh t_node tn.
 * parent = null.
 * 
 * After it terminates, the fresh t_node will now have tn.P(I) = P(I). In other
 * words, the P(I) of tn will correspond to the P(I) for n.
 * 
 *On recursive calls, the arguments have the following meaning:
 * parent = the node that lead us to bn.
 * partn is the tnode associated with parent.
 */
void tn_build(bool_node* bn, bool_node* parent, t_node* partn, NodeStore& store){
	++ivisit;
	if( typeid(*bn) == typeid(ARRACC_node) && parent != bn->mother  ){
		ARRACC_node* an = dynamic_cast<ARRACC_node*>(bn);	
		
		if( an->multi_mother.size() == 2 && an->mother->getOtype() == bool_node::BOOL ){
			int found = 0;	
			for(int i=0; i<an->multi_mother.size(); ++i){
				if( an->multi_mother[i] == parent ){
					++found;
					sprintf(buf, "%dI%d", an, i);
					string tmp(buf);
					if(visited.find(tmp) != visited.end()){
						t_node* tn = visited[tmp];
						partn->children.push_back(tn);
						Assert( tn->node != NULL, "This can't be happening hgfrkj"<<tn<<", "<<tn->control);
					}else{			
						t_node* tn = new t_node(an->mother);
						tn->nidx = i;
						partn->children.push_back(tn);
						visited[tmp] = tn;				
						for(child_iter child = an->children.begin(); child!=an->children.end(); ++child){						
							tn_build(*child, bn, tn, store);
						}
						
						tn->circuit(store);					
						Assert( tn->node != NULL, "This can't be happening pm;askd");
					}
				}
			}			
			Assert( found > 0, "This is very strange; this shouldn't happen.");
			return;
		}
	}
	
	if( tvisited[bn] == partn ){ 
		return;	
	}
	tvisited[bn] = partn;
	
	for(child_iter child = bn->children.begin(); child!=bn->children.end(); ++child){
		tn_build(*child, bn, partn, store);			
	}
}

};


#endif


