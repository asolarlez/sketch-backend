#pragma once


#include "BooleanDAG.h"

class NodeStore
{

protected:
	int dagsize;
	vector<bool_node*> newnodes;
public:
	NodeStore(){
		
	}

	inline void dagsizeSet(int i){
		dagsize= i;
	}
	inline int newNodesSize(){
		return newnodes.size();
	}
	virtual void addNode(bool_node* node);
	void printNewNodes() {
		for (auto it = newnodes.begin(); it != newnodes.end(); ++it) {
			cout<<(*it)->lprint() << endl;
		}
	}
};
