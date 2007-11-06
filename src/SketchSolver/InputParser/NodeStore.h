#pragma once


#include "BooleanDAG.h"

class NodeStore
{
	int timestamp;
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
	virtual void addNode(bool_node* node)= 0;
};
