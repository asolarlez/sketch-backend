#include "DagOptim.h"
#include "SATSolver.h"

DagOptim::DagOptim(BooleanDAG& dag):cse(dag)
{
	ALTER_ARRACS = false;
	rvalue = NULL;	
	initialize(dag);
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
		Dout(cout<<" add "<<cnode->id<<"  "<<cnode->get_name()<<endl);
		return cnode;
	}else{
		return cnmap[val];	
	}
}

CONST_node* DagOptim::getCnode(bool c){
	return getCnode( c ? 1 : 0 );
}



void DagOptim::addNode(bool_node* node){
	node->id = newnodes.size() + dagsize;
	Dout(cout<<" add "<<node->id<<"  "<<node->get_name()<<endl);
	newnodes.push_back(node);
}



timerclass statcomp("static compare");

/**
 * return value: 
 * 	1 : always true.
 * -1 : always false.
 */
template<typename COMP>
int DagOptim::staticCompare(bool_node* n1, int C , bool reverse ){
	COMP comp;
	if( anv.count(n1) != 0 ){
		return anv[n1].staticCompare<COMP>(C, reverse);
	}
	if(  isConst(n1) ){
		anv[n1].init(getIval(n1));
		bool cm = reverse? comp(C, getIval(n1)) : comp(getIval(n1), C);
		return cm ? 1 : -1;	
	}
	
	if(typeid(*n1) == typeid(NOT_node)){
		int rv = staticCompare<COMP>(n1->mother, C, reverse);
		anv[n1] = anv[n1->mother];
		return rv;
	}

	if(typeid(*n1) == typeid(SRC_node) || typeid(*n1) == typeid(CTRL_node)){
		INTER_node* inode = dynamic_cast<INTER_node*>(n1);
		if(inode->getOtype() == bool_node::BOOL){
			anv[n1].init(0);	
			anv[n1].insert(1);
			return anv[n1].staticCompare<COMP>(C, reverse);
		}
	}


	if(  typeid(*n1) == typeid(ARRACC_node) || typeid(*n1) == typeid(ARRASS_node)){
		arith_node& ar = *dynamic_cast<arith_node*>(n1);
		int rv = -2;
		AbstractNodeValue& nv = anv[n1];
		for(int i=0; i<ar.multi_mother.size(); ++i){
			bool_node* parent = ar.multi_mother[i];
			int tmp = 0;
			if(anv.count(parent)==0){
				tmp = staticCompare<COMP>(parent, C, reverse);
			}else{
				tmp = anv[parent].staticCompare<COMP>(C, reverse);
			}			
			if(tmp == 0 || (rv != -2 && tmp != rv)){				
				rv = 0;	
			}
			nv.insert( anv[parent] );
			if(rv != 0){
				rv = tmp;
			}
		}
		if( typeid(*n1) == typeid(ARRACC_node) && (n1->mother->getOtype() == bool_node::INT || ar.multi_mother.size() < 2)){			
			nv.insert(0);
			bool cm = reverse? comp(C, 0) : comp(0, C);
			int tmp = cm ? 1 : -1;	
			if(tmp == 0 || (rv != -2 && tmp != rv)){				
				rv = 0;	
			}
			if(rv != 0){
				rv = tmp;
			}
		}
		if(rv == -2){nv.makeTop(); return 0; }		
		return rv;		
	}
	anv[n1].makeTop();
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
			rvalue  = pnode;
			visit(*pnode);
			if(rvalue != pnode){
				pnode->dislodge();
				delete pnode;
				cout<<"=================== saved 3"<<endl;
			}else{
				addNode(pnode);	
			}
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
			rvalue  = pnode;
			visit(*pnode);		
			if(rvalue != pnode){
				pnode->dislodge();
				delete pnode;
				cout<<"=================== saved 3"<<endl;
			}else{
				addNode(pnode);	
			}
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
			pnode->father = mofa;
			pnode->mother = getCnode(0);
			pnode->addToParents();		
			rvalue  = pnode;
			visit(*pnode);
			if(rvalue != pnode){
				pnode->dislodge();
				delete pnode;
				cout<<"=================== saved 3"<<endl;
			}else{
				addNode(pnode);	
			}
			return true;
		}			
	}
		
	
	if(isConst(node.mother)){
		statcomp.restart();
		int tmp = staticCompare<COMP>(node.father, getIval(node.mother), true);
		statcomp.stop();
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
		statcomp.restart();
		int tmp = staticCompare<COMP>(node.mother, getIval(node.father), false);
		statcomp.stop();
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
					addNode(pnode);					
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
					addNode(pnode);	
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
					addNode(pnode);
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

	
	if( node.mother == node.father ){
		rvalue = getCnode(true);
		return;
	} 

	if( isConst(node.mother) ){
		if( isConst(node.father) ){
			rvalue  = getCnode( getIval( node.mother ) == getIval( node.father ) );
			return;
		}
		
		if( node.father->getOtype() == bool_node::BOOL ){
			int mvalue = getIval( node.mother );
			
			if(mvalue == 1){
				rvalue = node.father;
				return;	
			}
			
			if(mvalue == 0){
				NOT_node* nt = new NOT_node();
				nt->name = node.name;
				nt->name += "NOTa";
				nt->mother = node.father;
				nt->addToParents();
				addNode(nt);
				nt->accept(*this);
				return;	
			}
		}
	}
	
	if( isConst(node.father) ){
		if( node.mother->getOtype() == bool_node::BOOL ){
			int mvalue = getIval( node.father );
			
			if(mvalue == 1){
				rvalue = node.mother;
				return;	
			}
			
			if(mvalue == 0){
				NOT_node* nt = new NOT_node();
				nt->name = node.name;
				nt->name += "NOTb";
				nt->mother = node.mother;
				nt->addToParents();
				addNode(nt);
				nt->accept(*this);
				return;	
			}
		}
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
	
	
	
	if( node.multi_mother.size()==2 && node.mother->getOtype()== bool_node::BOOL ){

		/* 
		 * It may seem that the transformation inside the following if statement also applies to
		 * ARRACC with non-bool mother. But this is not the case, because if the mother is non-bool, 
		 * then the access could be out of bounds, and then, the result could be different to 
		 * multi_mother[0] even if all multimothers are the same. (REMEMBER: out of bounds ARRACC evaluates to zero).
		 * */
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




		if( isConst(node.multi_mother[0]) ){
			int val0 = getIval( node.multi_mother[0] );
			if( isConst(node.multi_mother[1]) ){
				int val1 = getIval( node.multi_mother[1] );	
				if( (val0 == 0 || val0 == 1) &&  (val1 == 0 || val1 == 1) ){
				//We know val0 and val1 are different.
					if(val0 == 0){
						rvalue = node.mother;
						return;
					}else{
						NOT_node* nt = new NOT_node();
						nt->name = node.name;
						nt->name += "NOTc";
						nt->mother = node.mother;
						nt->addToParents();
						addNode(nt);
						nt->accept(*this);
						return;
					}
				}
			}
			
			if(val0 == 0 && node.multi_mother[1]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();
				an->name = node.name;
				an->name += "AND";
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();

				addNode(an);

				an->accept(*this);
				return;
			}
		}

		if( isConst(node.multi_mother[1]) ){
			int val1 = getIval( node.multi_mother[1] );	
			if(val1 == 1 && node.multi_mother[0]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				OR_node* an = new OR_node();
				an->name = node.name;
				an->name += "OR";
				an->mother = node.mother;
				an->father = node.multi_mother[0];
				an->addToParents();
				addNode(an);
				an->accept(*this);
				return;
			}
		}
		
		if( node.mother == node.multi_mother[0] && node.multi_mother[1]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();
				an->name = node.name;
				an->name += "AND";
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();
				addNode(an);
				an->accept(*this);
				return;
		}
		
		
		
		
		
		if( typeid(*node.multi_mother[1]) == typeid(node)  ){			
			ARRACC_node& mm1 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[1]);
			if( node.multi_mother[0] == mm1.multi_mother[0] && node.mother->getOtype() == bool_node::BOOL && mm1 .mother->getOtype() == bool_node::BOOL ){
				AND_node* an = new AND_node();
				an->name = node.name;
				an->name += "AND";
				an->mother = node.mother;
				an->father = mm1 .mother;
				an->addToParents();
				addNode(an);

				an = dynamic_cast<AND_node*>( cse.computeCSE(an) );
				
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
				nt->name += "NOTc";
				nt->mother = mm1 .mother;
				nt->addToParents();
				addNode(nt);

				
				
				an->father = nt;
				an->addToParents();
				addNode(an);

				
				

				an = dynamic_cast<AND_node*>( cse.computeCSE(an) );
				
				
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
				addNode(an);

				an = dynamic_cast<OR_node*>( cse.computeCSE(an) );
				
				
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
				nt->name += "NOTd";
				nt->mother = mm1 .mother;
				addNode(nt);

				an->father = nt;
				an->addToParents();
				addNode(an);

					
				an = dynamic_cast<AND_node*>( cse.computeCSE(an) );
				
				
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
	if( isConst(node.mother) ){
		int val = getIval( node.mother );
		if(node.quant == val){
			rvalue = node.multi_mother[1];
		}else{
			rvalue = node.multi_mother[0];
		}
		return;
	}

	if(node.multi_mother[1] == node.multi_mother[0]){
		rvalue = node.multi_mother[0];
		return;
	}

	int sc = staticCompare<equal_to<int> >(node.mother, node.quant, true);
	if(sc == 1){
		rvalue = node.multi_mother[1];
		return;
	}
	if(sc == -1){
		rvalue = node.multi_mother[0];
		return;
	}

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


void DagOptim::initialize(BooleanDAG& dag){
	dag.removeNullNodes();
	dag.sort_graph();
	dag.cleanup(false);
	dag.relabel();
	dagsize = dag.size();	
	anv.clear();
	newnodes.clear();
	cnmap.clear();
	cse.clear();
}

bool_node* DagOptim::computeOptim(bool_node* node){
	node->accept(*this);
	node = rvalue;
	node = cse.computeCSE(node);
	return node;
}


void DagOptim::cleanup(BooleanDAG& dag){
	dag.removeNullNodes();
	dag.addNewNodes(newnodes);
	//dag.repOK();
	newnodes.clear();
	dag.sort_graph();
	dag.cleanup(false);
	dag.relabel();
	statcomp.print();
}

void DagOptim::process(BooleanDAG& dag){
	timerclass everything("everything");
	timerclass opttimer("OPTIMIZATION");
	timerclass identify("identify");
	timerclass replace("replace");	

	timerclass replacepar("dislodge");		

	everything.start();
	opttimer.start();
	
	int k=0;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				
		identify.restart(); 

		bool_node* node = computeOptim(dag[i]);

		identify.stop();

		if(dag[i] != node){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				replace.restart();
				dag.replace(i, node, replacepar);
				replace.stop();
		}
	}
	opttimer.stop();
	
	
	cleanup(dag);

	everything.stop();
	

	everything.print();
	//statcomp.print();
	//opttimer.print();
	//identify.print();
	//replace.print();
	//replacepar.print();
	Dout(cout<<" end cse "<<endl);
}
