#include "NodeSlicer.h"

NodeSlicer::NodeSlicer(map<string, BooleanDAG*>& functionMap_p, VarStore& inputs_p, BooleanDAG& bdag_p):
functionMap(functionMap_p), inputs(inputs_p), ne(functionMap_p, bdag_p)
{
}

NodeSlicer::~NodeSlicer(void)
{
}


void NodeSlicer::visit( AND_node& node ){
	if(isMarked(node)){ return; }
	mark(node);	
	int tmother = ne.getValue(node.mother);
	int tfather = ne.getValue(node.father);
	if(tfather != 0){
		node.mother->accept(*this);
	}
	if(tmother != 0 && tfather == 0){
		node.father->accept(*this);
	}
}
void NodeSlicer::visit( OR_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	int tmother = ne.getValue(node.mother);
	int tfather = ne.getValue(node.father);
	if(tfather != 1){
		node.mother->accept(*this);
	}
	if(tmother != 1 || tfather == 1){
		node.father->accept(*this);
	}
}
void NodeSlicer::visit( XOR_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}
void NodeSlicer::visit( SRC_node& node ){
	if(isMarked(node)){ return; }
	influential[node.get_name()] = &node;
	mark(node);
}
void NodeSlicer::visit( DST_node& node ){
	Assert(false, "This should not be visited here");
}
void NodeSlicer::visit( NOT_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
}

void NodeSlicer::visit( CTRL_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
}
void NodeSlicer::visit( PLUS_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}
void NodeSlicer::visit( TIMES_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	int tmother = ne.getValue(node.mother);
	int tfather = ne.getValue(node.father);
	if(tfather != 0){
		node.mother->accept(*this);
	}
	if(tmother != 0){
		node.father->accept(*this);
	}
}
void NodeSlicer::visit( UFUN_node& node ){
	Assert(false, "NYI");
}
void NodeSlicer::visit( ARRACC_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	int idx = ne.getValue(node.mother);
	if(idx >=0 && idx < node.multi_mother.size()){
		node.multi_mother[idx]->accept(*this);
	}
}
void NodeSlicer::visit( DIV_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}
void NodeSlicer::visit( MOD_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}
void NodeSlicer::visit( NEG_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
}
void NodeSlicer::visit( CONST_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
}

void NodeSlicer::visit( LT_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}

void NodeSlicer::visit( EQ_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	node.father->accept(*this);
}

void NodeSlicer::visit( ARRASS_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	node.mother->accept(*this);
	if(ne.getValue(node.mother)== node.quant){
		node.multi_mother[1]->accept(*this);
	}else{
		node.multi_mother[0]->accept(*this);
	}
}

void NodeSlicer::visit( ACTRL_node& node ){
	if(isMarked(node)){ return; }
	mark(node);
	for(int i=0; i<node.multi_mother.size(); ++i){
		node.multi_mother[i]->accept(*this);
	}
}
void NodeSlicer::visit( ASSERT_node &node){
	mark(node);
	node.mother->accept(*this);
}	




void NodeSlicer::process(BooleanDAG& bdag){
	
	ne.process(bdag);
	marked.clear();
	marked.resize(bdag.size(), 0);
	vector<bool_node*>& asserts = bdag.getNodesByType(bool_node::ASSERT);
	int insize = bdag.getNodesByType(bool_node::SRC).size();
	bool found = false;
	for(BooleanDAG::iterator node_it = asserts.begin(); node_it != asserts.end(); ++node_it){
		if(ne.getValue(**node_it)==0){
			(*node_it)->accept(*this);
			found = true;
			break;
		}
	}
	Assert(slice.size()>0, "Couldn't find any unsatisfied assertion!!");
	cout<<" Slice fraction = "<< ((float)slice.size()/bdag.size())<<endl;
	cout<<" Slice interface fraction = "<< ((float)influential.size()/ insize)<<endl;
}
