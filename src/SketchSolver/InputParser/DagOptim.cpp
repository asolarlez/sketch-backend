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









/**
 * return value: 
 * 	1 : always true.
 * -1 : always false.
 */
template<typename COMP>
int DagOptim::staticCompare(bool_node* n1, int C , bool reverse ){
	COMP comp;	
	
	if(  isConst(n1) ){
		bool cm = reverse? comp(C, getIval(n1)) : comp(getIval(n1), C);
		return cm ? 1 : -1;	
	} 	
	
	if(  typeid(*n1) == typeid(ARRACC_node) ){
		ARRACC_node& ar = *dynamic_cast<ARRACC_node*>(n1);
		int rv = -2;
		for(int i=0; i<ar.multi_mother.size(); ++i){
			int tmp = staticCompare<COMP>(ar.multi_mother[i], C, reverse);
			if(rv != -2 && tmp != rv){
				return 0;	
			}	
			rv = tmp;
		}
		if(rv == -2){ return 0; }
		return rv;		
	}
	return 0;
}




template<typename COMP, typename NTYPE>
bool DagOptim::compSymplification(NTYPE& node){

	if( typeid(*node.mother) == typeid(PLUS_node) &&  typeid(*node.father) == typeid(PLUS_node) ){
		bool_node* momo = node.mother->mother;
		bool_node* mofa = node.mother->father;
		
		bool_node* famo = node.father->mother;
		bool_node* fafa = node.father->father;
		if(momo == fafa){
			bool_node* tmp = fafa;
			fafa = famo;
			famo = tmp;	
		}
		
		if(mofa == famo){
			bool_node* tmp = momo;
			momo = mofa;
			mofa = tmp;
		}
		
		if(mofa == fafa){
			bool_node* tmp = fafa;
			fafa = famo;
			famo = tmp;	
			
			tmp = momo;
			momo = mofa;
			mofa = tmp;	
		}
		
		if(momo == famo){
			//cout<<" EQUALITY REPLACEMENT"<<endl;
			NTYPE* pnode = new NTYPE();
			pnode->mother = mofa;
			pnode->father = fafa;
			pnode->addToParents();
			pnode->id = newnodes.size() + dagsize;
			newnodes.push_back(pnode);
			rvalue  = pnode;
			visit(*pnode);
			return true;
		}
	}
		
	if( typeid(*node.mother) == typeid(PLUS_node) ){
		
		bool_node* momo = node.mother->mother;
		bool_node* mofa = node.mother->father;
		
		if(mofa == node.father){
			momo = 	node.mother->father;;
			mofa = node.mother->mother;
		}
		//At this point, if any of the two is equal to node.father, it will be momo
		
		if(momo == node.father){
			NTYPE* pnode = new NTYPE();
			pnode->mother = mofa;
			pnode->father = getCnode(0);
			pnode->addToParents();
			pnode->id = newnodes.size() + dagsize;
			newnodes.push_back(pnode);
			rvalue  = pnode;
			visit(*pnode);			
			return true;
		}			
	}
		
	if( typeid(*node.father) == typeid(PLUS_node) ){
		
		bool_node* momo = node.father->mother;
		bool_node* mofa = node.father->father;
		
		if(mofa == node.mother){
			momo = 	node.father->father;;
			mofa = node.father->mother;
		}
		//At this point, if any of the two is equal to node.father, it will be momo
		
		if(momo == node.mother){
			NTYPE* pnode = new NTYPE();
			pnode->mother = mofa;
			pnode->father = getCnode(0);
			pnode->addToParents();
			pnode->id = newnodes.size() + dagsize;
			newnodes.push_back(pnode);
			rvalue  = pnode;
			visit(*pnode);			
			return true;
		}			
	}
		
	if(isConst(node.mother)){
		int tmp = staticCompare<COMP>(node.father, getIval(node.mother), true);
		if(tmp == 1){
			rvalue  = getCnode(1);
			return 	true;
		}
		if(tmp == -1){
			rvalue  = getCnode(0);
			return true;	
		}
	}
	
	if(isConst(node.father)){
		int tmp = staticCompare<COMP>(node.mother, getIval(node.father), false);
		if(tmp == 1){
			rvalue  = getCnode(1);
			return 	true;
		}
		if(tmp == -1){
			rvalue  = getCnode(0);
			return true;	
		}
	}
		
	return false;
}




bool DagOptim::isNegOfEachOther(bool_node* n1, bool_node* n2){
	if( typeid(*n1) == typeid(NEG_node) ){
		if(  n1->mother == n2){
			return true;	
		}
	}
	
	if(  typeid(*n2) == typeid(NEG_node) ){
		if( n2->mother == n1){
			return true;
		}
	}
	return false;	
}



bool DagOptim::isNotOfEachOther(bool_node* n1, bool_node* n2){
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
	if( isNotOfEachOther(node.father, node.mother) ){ // x & !x == false
		rvalue = getCnode(0);
		return;	
	}
	if( isConst(node.father) ){
		if( isConst(node.mother) ){ // const prop
			rvalue = getCnode ( getBval( node.father ) && getBval( node.mother ) );				
			return;
		}
		if( !getBval(node.father) ){ // x & false == false;
			rvalue = getCnode(0);
			return;
		}else{
			rvalue = node.mother;
			return;	
		}		
	}
	if( isConst(node.mother) ){
		if( ! getBval(node.mother)){ // false & x == false;
			rvalue = getCnode(0);			
			return;
		}else{
			rvalue = node.father;
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
	if( isNotOfEachOther(node.father, node.mother) ){ // x | !x == true
		rvalue = getCnode(1);
		return;	
	}
	if( isConst(node.father) ){
		if( isConst(node.mother) ){ // const prop
			rvalue = getCnode ( getBval( node.father ) || getBval( node.mother ) );				
			return;
		}
		if( getBval(node.father) ){ // x | true == true
			rvalue = getCnode(1);			
			return;
		}else{
			rvalue = node.mother;
			return;				
		}		
	}
	if( isConst(node.mother) ){ // true | x == true
		if( getBval(node.mother)){
			rvalue = getCnode(1);			
			return;
		}else{
			rvalue = node.father;
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
	if( isNotOfEachOther(node.father, node.mother) ){ // x ^ !x == true
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
	
	bool_node* mother = node.mother;
	bool_node* father = node.father;
	int nc = 0;
	
	if(isConst(node.father)){
		mother = node.father;
		father = node.mother;
		nc++;
	}
	if(isConst(node.mother)){
		nc++;	
	}
	//At this point, if nc>0, mother is const, 
	
	if(nc > 0){
		if(nc > 1){
			rvalue  = getCnode( getIval( mother ) + getIval( father ) );
			return;
		}else{
			if(getIval( mother ) == 0){
				rvalue  = father;
				return;
			}
			if( typeid(*father) == typeid(node) ){
				PLUS_node& fathernode = *dynamic_cast<PLUS_node*>(father);
				if( isConst(fathernode.father) ){
					//cout<<" DID REPLACEMENT 1"<<endl;
					bool_node* nconst = getCnode(getIval( fathernode.father ) + getIval( mother ));
					PLUS_node* pnode = new PLUS_node();
					pnode->mother = nconst;
					Assert(!isConst(fathernode.mother), "This can't happen");
					pnode->father = fathernode.mother;
					pnode->addToParents();
					pnode->id = newnodes.size() + dagsize;
					newnodes.push_back(pnode);
					rvalue  = pnode;
					return;
				}
				if( isConst(fathernode.mother) ){
					//cout<<" DID REPLACEMENT 2"<<endl;
					bool_node* nconst = getCnode(getIval( fathernode.mother ) + getIval( mother ));
					PLUS_node* pnode = new PLUS_node();
					pnode->mother = nconst;
					Assert(!isConst(fathernode.father), "This can't happen");
					pnode->father = fathernode.father;
					pnode->addToParents();
					pnode->id = newnodes.size() + dagsize;
					newnodes.push_back(pnode);
					rvalue  = pnode;
					return;
				}
				
			}
			
		}	
	}
	
	rvalue = &node;
	
	
	if( isNegOfEachOther(node.mother, node.father) ){
		rvalue = getCnode(0);
		return;
	}
	
	if( typeid(*node.father) == typeid(node) ){
		PLUS_node& parent = *dynamic_cast<PLUS_node*>(node.father);
		bool_node* tt1 = parent.father;
		bool_node* tt2 = parent.mother;
		bool_node* tt3 = node.mother;
			
		if(isNegOfEachOther( tt1 , tt2)){
			rvalue = tt3;
			return;	
		}
		if(isNegOfEachOther( tt1 , tt3)){
			rvalue = tt2;
			return;	
		}
		if(isNegOfEachOther( tt2 , tt3)){
			rvalue = tt1;
			return;	
		}
	}
	
	
	if( typeid(*node.mother) == typeid(node) ){
		PLUS_node& parent = *dynamic_cast<PLUS_node*>(node.mother);
		bool_node* tt1 = parent.father;
		bool_node* tt2 = parent.mother;
		bool_node* tt3 = node.father;
			
		if(isNegOfEachOther( tt1 , tt2)){
			rvalue = tt3;
			return;	
		}
		if(isNegOfEachOther( tt1 , tt3)){
			rvalue = tt2;
			return;	
		}
		if(isNegOfEachOther( tt2 , tt3)){
			rvalue = tt1;
			return;	
		}
	}
	
	
	
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) + getIval( node.father ) );
			return;
		}	
		if(  getIval( node.mother ) == 0 ){
			rvalue  = node.father;
			return;
		}		
		if( typeid(*node.father) == typeid(node) ){
				PLUS_node& fathernode = *dynamic_cast<PLUS_node*>(node.father);
				if( isConst(fathernode.father) ){
					bool_node* nconst = getCnode(getIval( fathernode.father ) + getIval( node.mother ));
					PLUS_node* pnode = new PLUS_node();
					pnode->mother = nconst;
					Assert(!isConst(fathernode.mother), "This can't happen");
					pnode->father = fathernode.mother;
					pnode->addToParents();
					pnode->id = newnodes.size() + dagsize;
					newnodes.push_back(pnode);
				}
			
		}
	}
	if( isConst(node.father) ){
		if(  getIval( node.father ) == 0 ){
			rvalue  = node.mother;
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
	
	
	
	if( compSymplification<greater<int> , GT_node>(node) ){
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
	
	if( compSymplification<greater_equal<int> , GE_node>(node) ){
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
	
	if( compSymplification<less<int> , LT_node>(node) ){
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
	
	
	if( compSymplification<less_equal<int> , LE_node>(node) ){
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
		
	if( compSymplification<equal_to<int> , EQ_node>(node) ){
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
				if(cse.hasCSE(cse.ccode)){
					an = dynamic_cast<AND_node*>(cse[cse.ccode]);		
				}else{
					cse[cse.ccode] = an;
				}
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[1] = mm1.multi_mother[1];
				node.addToParents();
			}	
			
			
			/*
			if( node.multi_mother[0] == mm1.multi_mother[1] && mm1.children.size() < 2){
				AND_node* an = new AND_node();
				an->name = node.name;
				an->name += "AND";
				an->mother = node.mother;
				
								
				NOT_node* nt = new NOT_node();
				nt->name = node.name;
				nt->name += "NOT";
				nt->mother = mm1 .mother;
				nt->id = newnodes.size() + dagsize;
				newnodes.push_back(nt);
				
				
				an->father = nt;
				an->addToParents();
				an->id = newnodes.size() + dagsize;
				newnodes.push_back(an);
				
				
				an->accept(cse);
				if(cse.find(cse.ccode) != cse.end()){
					an = dynamic_cast<AND_node*>(cse[cse.ccode]);		
				}else{
					cse[cse.ccode] = an;
				}
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[1] = mm1.multi_mother[0];
				node.addToParents();
			}
			*/
			
					
			
		}
		
		if( typeid(*node.multi_mother[0]) == typeid(node)  ){
			ARRACC_node& mm1 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[0]);		
			if( node.multi_mother[1] == mm1.multi_mother[1] && mm1.children.size() < 2){
				OR_node* an = new OR_node();
				an->name = node.name;
				an->name += "OR";
				an->mother = node.mother;
				an->father = mm1 .mother;
				an->addToParents();
				an->id = newnodes.size() + dagsize;
				newnodes.push_back(an);
				
				an->accept(cse);
				if(cse.hasCSE(cse.ccode)){
					an = dynamic_cast<OR_node*>(cse[cse.ccode]);		
				}else{
					cse[cse.ccode] = an;
				}
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[0] = mm1.multi_mother[0];
				node.addToParents();
			}
			
			
			/*
			if( node.multi_mother[1] == mm1.multi_mother[0] && mm1.children.size() < 2){
				OR_node* an = new OR_node();
				an->name = node.name;
				an->name += "OR";
				an->mother = node.mother;
				
								
				NOT_node* nt = new NOT_node();
				nt->name = node.name;
				nt->name += "NOT";
				nt->mother = mm1 .mother;
				nt->id = newnodes.size() + dagsize;
				newnodes.push_back(nt);
				
				an->father = nt;
				an->addToParents();
				an->id = newnodes.size() + dagsize;
				newnodes.push_back(an);
					
				an->accept(cse);
				if(cse.find(cse.ccode) != cse.end()){
					an = dynamic_cast<OR_node*>(cse[cse.ccode]);		
				}else{
					cse[cse.ccode] = an;
				}
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[0] = mm1.multi_mother[1];
				node.addToParents();
			}
			*/
			
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
	timerclass everything("everything");
	timerclass opttimer("OPTIMIZATION");
	timerclass identify("identify");
	timerclass replace("replace");

	everything.start();
	opttimer.start();
	dagsize = dag.size();	
	int k=0;
	for(int i=0; i<dag.size(); ++i ){
		// Get the code for this node.
		identify.restart(); 
		dag[i]->accept(*this);
		bool_node* node = rvalue;		
		node->accept(cse);
		identify.stop();
		
		// look it up in the cse map.		
		Dout(cout<<dag[i]->id<<"  "<<dag[i]->get_name()<<"("<< node->get_name() <<"): "<<cse.ccode<<endl) ;
		if( cse.hasCSE(cse.ccode) ){
			// if we do find it, then remove the node and replace it with its cse.			
			bool_node * csubexp = cse[cse.ccode];
			Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<csubexp->get_name()<<endl );
			replace.restart();
			dag.replace(i, csubexp);
			replace.stop(); 
		}else{
			// if we don't find it, just add it.
			cse[cse.ccode] = node;
			if( dag[i] != node ){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				replace.restart();
				dag.replace(i, node);
				replace.stop();
			}
		}
	}
	opttimer.stop();
	
	
	dag.removeNullNodes();
	dag.addNewNodes(newnodes);
	dag.sort_graph();
	dag.cleanup(false);
	dag.relabel();
	everything.stop();
	everything.print();
	opttimer.print();
	identify.print();
	replace.print();

	Dout(cout<<" end cse "<<endl);
}
