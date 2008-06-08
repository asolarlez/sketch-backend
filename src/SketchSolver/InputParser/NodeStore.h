#pragma once


#include "BooleanDAG.h"

class NodeStore
{
	int timestamp;
protected:
	int dagsize;
	vector<bool_node*> newnodes;
public:
	NodeStore(){
		timestamp = 0;
	}
	inline int getTimestamp(){
		return ++timestamp;
	}
	inline void setTimestamp(bool_node* bn){
		bn->layer = ++timestamp;
	}
	inline void setTimestampChildren(bool_node* bn){
		for(child_iter it = bn->children.begin(); it != bn->children.end(); ++it){
			(*it)->layer = ++timestamp;
		}
	}		

	inline void dagsizeSet(int i){
		dagsize= i;
	}
	inline int newNodesSize(){
		return newnodes.size();
	}
	virtual void addNode(bool_node* node);
};
