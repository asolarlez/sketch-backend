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

	
virtual void visitArith(arith_node& node ){
	if(node.mother != NULL){
		tovisit[node.mother->id] = true;
	}
	if(node.father != NULL){
		tovisit[node.father->id] = true;
	}
	for(int i=0; i<node.multi_mother.size(); ++i){
		tovisit[node.multi_mother[i]->id] = true;
	}
	rvalue = &node;
}

virtual void visitBool(bool_node& node ){
	if(node.mother != NULL){
		tovisit[node.mother->id] = true;
	}
	if(node.father != NULL){
		tovisit[node.father->id] = true;
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

