#include "NodeVisitor.h"
#include "BooleanDAG.h"

NodeVisitor::NodeVisitor()
{
}

NodeVisitor::~NodeVisitor()
{
}



void NodeVisitor::process(BooleanDAG& bdag){
	int i=0;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
		//if(i>1000){ cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl; }
		(*node_it)->accept(*this);
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    	}
	}
}


void NodeVisitor::visitArith(arith_node& node ){
	rvalue = &node;
}

void NodeVisitor::visitBool(bool_node& node ){
	rvalue = &node;
}

void NodeVisitor::visit( AND_node& node ){
	visitBool(node);
}
void NodeVisitor::visit( OR_node& node ){
	visitBool(node);
}
void NodeVisitor::visit( XOR_node& node ){
	visitBool(node);	
}
void NodeVisitor::visit( SRC_node& node ){
	visitBool(node);	
}
void NodeVisitor::visit( DST_node& node ){
	visitBool(node);	
}
void NodeVisitor::visit( NOT_node& node ){
	visitBool(node);	
}
void NodeVisitor::visit( CTRL_node& node ){
	visitBool(node);	
}
void NodeVisitor::visit( PLUS_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( TIMES_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( UFUN_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( ARRACC_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( DIV_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( MOD_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( NEG_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( CONST_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( GT_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( GE_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( LT_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( LE_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( EQ_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( ARRASS_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( ACTRL_node& node ){
	visitArith(node);	
}
void NodeVisitor::visit( ASSERT_node &node){
	visitBool(node);	
}	

