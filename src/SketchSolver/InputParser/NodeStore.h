#pragma once


#include "BooleanDAG.h"

class NodeStore
{
public:
	virtual void addNode(bool_node* node)= 0;
};
