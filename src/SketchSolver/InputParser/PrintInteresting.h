#pragma once
#include "NodeVisitor.h"

class PrintInteresting :
	public NodeVisitor
{
public:
	vector<bool> tovisit;
	PrintInteresting(void)
	{
	}

	~PrintInteresting(void)
	{
	}


	virtual void visitBool(bool_node& node ){

		for (auto it = node.p_begin(); it != node.p_end(); ++it) {
			if ((*it) != NULL) {
				tovisit[(*it)->id] = true;
			}
		}

		rvalue = &node;
	}

	virtual void print(BooleanDAG& bdag, int seed){
		for(int i=0; i<=seed; ++i){
			if(tovisit[i]){
				cout<<bdag[i]->lprint()<<endl;
			}
		}
	}
	void process(BooleanDAG& bdag, int seed){
		tovisit.clear();
		tovisit.resize(seed+1, 0);
		tovisit[seed] = true;
		for(int i=seed; i>=0; i--){
			if(tovisit[i]){
				bdag[i]->accept(*this);
			}
		}
		print(bdag, seed);
	}
};

