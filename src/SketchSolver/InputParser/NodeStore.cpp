#include "NodeStore.h"

void NodeStore::addNode(bool_node* node){
	node->id = newnodes.size() + dagsize;
	Dout(cout<<" add "<<node->id<<"  "<<node->get_name()<<endl);
	newnodes.push_back(node);
}

