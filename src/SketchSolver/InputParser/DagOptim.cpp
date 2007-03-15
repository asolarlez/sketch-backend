#include "DagOptim.h"
#include "SATSolver.h"

DagOptim::DagOptim(BooleanDAG& dag):cse(dag)
{
	rvalue = NULL;	
}

DagOptim::~DagOptim()
{
}
CONST_node* DagOptim::getCnode(int val){
	if( cnmap.find(val) == cnmap.end() ){
		CONST_node* cnode = new CONST_node(val);
		cnode->id = newnodes.size() + dagsize;
		newnodes.push_back(cnode);
		cnmap[val] = cnode;
		return cnode;
	}else{
		return cnmap[val];	
	}
}

CONST_node* DagOptim::getCnode(bool c){
	return getCnode( c ? 1 : 0 );
}


bool DagOptim::isNegOfEachOther(bool_node* n1, bool_node* n2){
	if( n1->type == bool_node::NOT){
		if( n1->mother == n2){
			return true;	
		}
	}
	
	if( n2->type == bool_node::NOT){
		if( n2->mother == n1){
			return true;
		}
	}
	return false;
}
bool DagOptim::isConst(bool_node* n1){
	if( n1->type == bool_node::ARITH ){
		arith_node* an = dynamic_cast<arith_node*>(n1);
		if( an->arith_type == arith_node::CONST ){
			return true;	
		}	
	}	
	return false;
}


bool DagOptim::getBval(bool_node* n1){
	return getIval(n1) != 0;
}	

int  DagOptim::getIval(bool_node* n1){
	Assert( isConst(n1), "This node is not a constant !!");
	CONST_node * cn = dynamic_cast<CONST_node*>(n1);
	return cn->getVal()	;
}


void DagOptim::visit( SRC_node& node ){
	Dout( cout<<" node "<<node.get_name()<<endl );
	rvalue = &node;
}

void DagOptim::visit( CTRL_node& node ){
	rvalue = &node;
}

void DagOptim::visit( CONST_node& node ){
	int val = node.getVal();
	if( cnmap.find(val) == cnmap.end() ){
		cnmap[val] = &node;
		rvalue = &node;
	}else{
		rvalue = cnmap[val];	
	}	
}

void DagOptim::visit( AND_node& node ){
	if( node.father == node.mother ){ // x & x == x
		rvalue = node.father;
		return;	
	}
	if( isNegOfEachOther(node.father, node.mother) ){ // x & !x == false
		rvalue = getCnode(0);
		return;	
	}
	if( isConst(node.father) ){
		if( !getBval(node.father) ){ // x & false == false;
			rvalue = getCnode(0);
			return;
		}		
		if( isConst(node.mother) ){ // const prop
			rvalue = getCnode ( getBval( node.father ) && getBval( node.mother ) );				
			return;
		}
	}
	if( isConst(node.mother) ){
		if( ! getBval(node.mother)){ // false & x == false;
			rvalue = getCnode(0);			
			return;
		}
	}
	rvalue = &node;
}

void DagOptim::visit( OR_node& node ){
	if( node.father == node.mother ){ // x | x == x
		rvalue = node.father;
		return;	
	}
	if( isNegOfEachOther(node.father, node.mother) ){ // x | !x == true
		rvalue = getCnode(1);
		return;	
	}
	if( isConst(node.father) ){
		if( getBval(node.father) ){ // x | true == true
			rvalue = getCnode(1);			
			return;
		}
		if( isConst(node.mother) ){ // const prop
			rvalue = getCnode ( getBval( node.father ) || getBval( node.mother ) );				
			return;
		}
	}
	if( isConst(node.mother) ){ // true | x == true
		if( getBval(node.mother)){
			rvalue = getCnode(1);			
			return;
		}
	}
	rvalue = &node;
}


void DagOptim::visit( XOR_node& node ){
	if( node.father == node.mother ){ // x ^ x == false
		rvalue = getCnode(0);
		return;	
	}
	if( isNegOfEachOther(node.father, node.mother) ){ // x ^ !x == true
		rvalue = getCnode(1);
		return;	
	}
	if( isConst(node.father) ){
		if( isConst(node.mother)){ // const prop
			rvalue  = getCnode( getBval( node.father ) != getBval( node.mother ) );
			return;
		}
	}
	
	rvalue = &node;
}

void DagOptim::visit( NOT_node& node ){
	if( isConst(node.mother) ){ // const prop
		rvalue = getCnode( ! getBval(node.mother) );			
		return;
	}
	if( node.mother->type == bool_node::NOT){// ! ! x == x;
		rvalue = node.mother->mother;
		return;
	}
	rvalue = &node;
}


void DagOptim::visit( PLUS_node& node ){
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) + getIval( node.father ) );
			return;
		}			
	}
	rvalue = &node;
}

void DagOptim::visit( TIMES_node& node ){
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) * getIval( node.father ) );
			return;
		}
		if(  getIval( node.mother ) == 0 ){
			rvalue  = getCnode( 0 );
			return;
		}
		if(  getIval( node.mother ) == 1 ){
			rvalue  = node.father;
			return;
		}	
	}
	if( isConst(node.father) ){
		if(  getIval( node.father ) == 0 ){
			rvalue  = getCnode( 0 );
			return;
		}
		if(  getIval( node.father ) == 1 ){
			rvalue  = node.mother;
			return;
		}
	}
	rvalue = &node;
}

void DagOptim::visit( DIV_node& node ){
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) / getIval( node.father ) );
			return;
		}			
	}
	rvalue = &node;
}
void DagOptim::visit( MOD_node& node ){
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) % getIval( node.father ) );
			return;
		}			
	}
	rvalue = &node;
}
void DagOptim::visit( NEG_node& node ){
	if( isConst(node.mother) ){	// const prop
		rvalue  = getCnode( -getIval( node.mother ) );
		return;
	}
	if( typeid(*(node.mother)) == typeid(node)  ){// - -x == x;
		rvalue = node.mother->mother;
		return;
	}
	rvalue = &node;
}
	
void DagOptim::visit( GT_node& node ){
	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) > getIval( node.father ) );
			return;
		}			
	}
	
	if( node.mother == node.father ){
		rvalue = getCnode(false);
		return;
	} 
	rvalue = &node;
}
void DagOptim::visit( GE_node& node ){
	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) >= getIval( node.father ) );
			return;
		}			
	}
	if( node.mother == node.father ){
		rvalue = getCnode(true);
		return;
	} 
	rvalue = &node;
}
void DagOptim::visit( LT_node& node ){
	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) < getIval( node.father ) );
			return;
		}			
	}
	if( node.mother == node.father ){
		rvalue = getCnode(false);
		return;
	} 
	rvalue = &node;
}

void DagOptim::visit( LE_node& node ){
	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) <= getIval( node.father ) );
			return;
		}			
	}
	if( node.mother == node.father ){
		rvalue = getCnode(true);
		return;
	} 
	rvalue = &node;
}
void DagOptim::visit( EQ_node& node ){
	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) == getIval( node.father ) );
			return;
		}			
	}
	if( node.mother == node.father ){
		rvalue = getCnode(true);
		return;
	} 
	rvalue = &node;
}

void DagOptim::visit( UFUN_node& node ){
	rvalue = &node;	
}

void DagOptim::visit( ARRACC_node& node ){
	if( isConst(node.mother) ){
		int val = getIval( node.mother );		
		if(val < node.multi_mother.size()){
			rvalue = node.multi_mother[val];
		}else{
			rvalue = getCnode(0);
		}
		return;
	}
	bool tmp = true;
	int i=0;
	if( i<node.multi_mother.size() ){
		bool_node* bn =  node.multi_mother[0];
		for(i=1; i<node.multi_mother.size(); ++i){
			if( bn != node.multi_mother[i] ){
				tmp = false;
			}
		}
		if( tmp ){
			rvalue = bn;
			return;	
		}
	}
	
	
	if( node.multi_mother.size()==2  ){
		if( typeid(*node.multi_mother[1]) == typeid(node)  ){
			ARRACC_node& mm1 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[1]);			
			if( node.multi_mother[0] == mm1.multi_mother[0] ){
				AND_node* an = new AND_node();
				an->name = node.name;
				an->name += "AND";
				an->mother = node.mother;
				an->father = mm1 .mother;
				an->addToParents();
				an->id = newnodes.size() + dagsize;
				newnodes.push_back(an);
				
				an->accept(cse);
				if(cse.cse_map.find(cse.ccode) != cse.cse_map.end()){
					an = dynamic_cast<AND_node*>(cse.cse_map[cse.ccode]);		
				}else{
					cse.cse_map[cse.ccode] = an;
				}
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[1] = mm1.multi_mother[1];
				node.addToParents();
			}
		}
	}
	
	rvalue = &node;
}
void DagOptim::visit( ARRASS_node& node ){	
	rvalue = &node;	
}
void DagOptim::visit( ACTRL_node& node ){
	rvalue = &node;
}
	
void DagOptim::visit( ASSERT_node &node){
	rvalue = &node;
}	
void DagOptim::visit( DST_node& node ){
	rvalue = &node;
}

void DagOptim::process(BooleanDAG& dag){
	dagsize = dag.size();	
	int k=0;
	for(int i=0; i<dag.size(); ++i ){
		// Get the code for this node. 
		dag[i]->accept(*this);
		bool_node* node = rvalue;
		
		node->accept(cse);
		
		// look it up in the cse map.		
		Dout(cout<<dag[i]->id<<"  "<<dag[i]->get_name()<<"("<< node->get_name() <<"): "<<cse.ccode<<endl) ;
		if( cse.cse_map.find(cse.ccode) != cse.cse_map.end() ){
			// if we do find it, then remove the node and replace it with its cse.			
			bool_node * csubexp = cse.cse_map[cse.ccode];
			Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<csubexp->get_name()<<endl );
			dag.replace(i, csubexp); 
		}else{
			// if we don't find it, just add it.
			cse.cse_map[cse.ccode] = node;
			if( dag[i] != node ){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				dag.replace(i, node);
			}
		}
	}
	dag.removeNullNodes();
	dag.addNewNodes(newnodes);	
	dag.sort_graph();
	dag.cleanup(false);
	dag.relabel();
	Dout(cout<<" end cse "<<endl);
}
