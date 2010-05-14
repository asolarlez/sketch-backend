#include "DagOptim.h"
#include "SATSolver.h"
#include "CommandLineArgs.h"

DagOptim::DagOptim(BooleanDAG& dag):cse(dag)
{
	ALTER_ARRACS = false;
	rvalue = NULL;	
	initialize(dag);
}

DagOptim::~DagOptim()
{
}

extern CommandLineArgs* PARAMS;

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



/**
 * return value: 
 * 	1 : always true.
 * -1 : always false.
 */
template<typename COMP>
int DagOptim::staticCompare(bool_node* n1, int C , bool reverse ){
	COMP comp;
	{
		map<bool_node*, AbstractNodeValue>::iterator it = anv.find(n1);
		if( it != anv.end() ){
			if(it->second.timestamp == n1->globalId){		
				return it->second.staticCompare<COMP>(C, reverse);
			}else{
				anv.erase(it);
			}
		}
	}
	if(  isConst(n1) ){
		anv[n1].init(getIval(n1));
		anv[n1].timestamp = n1->globalId;	
		bool cm = reverse? comp(C, getIval(n1)) : comp(getIval(n1), C);
		return cm ? 1 : -1;	
	}
	
	if(n1->type == bool_node::NOT){
		int rv = staticCompare<COMP>(n1->mother, C, reverse);
		anv[n1] = anv[n1->mother];
		anv[n1].timestamp = n1->globalId;		
		return rv;
	}

	if(n1->getOtype() == bool_node::BOOL){
		AbstractNodeValue& nv = anv[n1];
		nv.init(0);	
		nv.insert(1);
		nv.timestamp = n1->globalId;
		return nv.staticCompare<COMP>(C, reverse);
	}

	if(n1->type == bool_node::SRC ||  n1->type == bool_node::CTRL){
		INTER_node* inode = dynamic_cast<INTER_node*>(n1);
		AbstractNodeValue& nv = anv[n1];
		if(inode->getOtype() == bool_node::BOOL){
			nv.init(0);	
			nv.insert(1);
			nv.timestamp = n1->globalId;
			return nv.staticCompare<COMP>(C, reverse);
		}else{
			nv.makeTop();
			nv.timestamp = n1->globalId;
			return 0;
		}
	}


	if( n1->type == bool_node::PLUS){
		AbstractNodeValue& nv = anv[n1];
		nv.timestamp = n1->globalId;
		{
			bool_node* parent = n1->mother;
			map<bool_node*, AbstractNodeValue>::iterator it = anv.find(parent);

			if(it == anv.end()){
				staticCompare<COMP>(parent, C, reverse);
			}else{
				if(it->second.timestamp != parent->globalId){
					anv.erase(it);
					staticCompare<COMP>(parent, C, reverse);
				}
			}

			nv.insert( anv[parent] );
		}
		{
			bool_node* parent = n1->father;
			map<bool_node*, AbstractNodeValue>::iterator it = anv.find(parent);
			if(it == anv.end()){
				staticCompare<COMP>(parent, C, reverse);
			}else{
				if(it->second.timestamp != parent->globalId){
					anv.erase(it);
					staticCompare<COMP>(parent, C, reverse);
				}
			}
			nv.add( anv[parent] );
		}
		return nv.staticCompare<COMP>(C, reverse);
	}





	if(  typeid(*n1) == typeid(ARRACC_node) || typeid(*n1) == typeid(ARRASS_node)){
		arith_node& ar = *dynamic_cast<arith_node*>(n1);
		int rv = -2;
		AbstractNodeValue& nv = anv[n1];
		nv.timestamp = n1->globalId;
		map<bool_node*, AbstractNodeValue>::iterator mot = anv.find(ar.mother);
		bool tata = false;
		if(mot == anv.end() || mot->second.timestamp != ar.mother->globalId){
			staticCompare<COMP>(ar.mother, C, reverse);
			tata = true;
		}
		AbstractNodeValue& mnv = tata? anv[ar.mother] : mot->second;
		if(mnv.isTop() || ar.arith_type == arith_node::ARRASS /*typeid(*n1) == typeid(ARRASS_node)*/){
			for(int i=0; i<ar.multi_mother.size(); ++i){
				bool_node* parent = ar.multi_mother[i];
				helper<COMP>(parent, C, reverse, rv, nv);
			}
			if((!PARAMS->assumebcheck) && ar.arith_type == arith_node::ARRACC  && (n1->mother->getOtype() == bool_node::INT || ar.multi_mother.size() < 2)){			
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
		}else{
			set<int>::const_iterator end = mnv.vset_end();
			int sz = ar.multi_mother.size();
			bool didZero = PARAMS->assumebcheck;
			for(set<int>::const_iterator it = mnv.vset_begin(); it != end; ++it){
				if((*it)>=0 && (*it)<sz){
					bool_node* parent = ar.multi_mother[(*it)];
					helper<COMP>(parent, C, reverse, rv, nv);
				}else{
					if(!didZero){
						nv.insert(0);
						bool cm = reverse? comp(C, 0) : comp(0, C);
						int tmp = cm ? 1 : -1;	
						if(tmp == 0 || (rv != -2 && tmp != rv)){				
							rv = 0;	
						}
						if(rv != 0){
							rv = tmp;
						}
						didZero = true;
					}
				}
			}
		}
		if(rv == -2){
			nv.makeTop(); 
			return 0; 
		}		
		return rv;		
	}
	anv[n1].makeTop();
	anv[n1].timestamp = n1->globalId;
	return 0;
}

template<typename COMP>
void DagOptim::helper(bool_node* parent, int C, int reverse, int& rv, AbstractNodeValue& nv){
				int tmp = 0;
				
				map<bool_node*, AbstractNodeValue>::iterator it = anv.find(parent);

				if(it == anv.end()){
					tmp = staticCompare<COMP>(parent, C, reverse);
					nv.insert( anv[parent] );
				}else{
					if(it->second.timestamp == parent->globalId){
						tmp = it->second.staticCompare<COMP>(C, reverse);
						nv.insert(it->second);
					}else{
						anv.erase(it);
						tmp = staticCompare<COMP>(parent, C, reverse);
						nv.insert( anv[parent] );
					}
				}

				if(tmp == 0 || (rv != -2 && tmp != rv)){				
					rv = 0;	
				}				
				if(rv != 0){
					rv = tmp;
				}
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
				//cout<<"=================== saved 3"<<endl;
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
				//cout<<"=================== saved 3"<<endl;
			}else{
				addNode(pnode);	
			}
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
bool DagOptim::isConst(const bool_node* n1){
	if( n1->type == bool_node::CONST ){
		return true;
	}	
	return false;
}


bool DagOptim::getBval(const bool_node* n1){
	return getIval(n1) != 0;
}	

int  DagOptim::getIval(const bool_node* n1){
	Assert( isConst(n1), "This node is not a constant !!");
	const CONST_node * cn = dynamic_cast<const CONST_node*>(n1);
	return cn->getVal()	;
}


void DagOptim::visit( SRC_node& node ){
	Dout( cout<<" node "<<node.get_name()<<endl );
	if(specialization.count(node.get_name())>0){
		rvalue = getCnode(specialization[node.get_name()]);
		return;
	}
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

	if(node.mother->type == bool_node::AND){
		if(isNotOfEachOther(node.father, node.mother->mother)){
			rvalue = getCnode(0);
			return;	
		}
		if(isNotOfEachOther(node.father, node.mother->father)){
			rvalue = getCnode(0);
			return;	
		}

	}

	if(node.father->type == bool_node::AND){
		if(isNotOfEachOther(node.mother, node.father->mother)){
			rvalue = getCnode(0);
			return;	
		}
		if(isNotOfEachOther(node.mother, node.father->father)){
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
					stillPrivate = pnode;
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
					stillPrivate = pnode;
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
			Assert(false, "Optim is useless");
			return;
		}	
		if(  getIval( node.mother ) == 0 ){
			rvalue  = node.father;
			Assert(false, "Optim is useless");
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
					Assert(false, "Optim is useless");
				}
			
		}
	}
	if( isConst(node.father) ){
		if(  getIval( node.father ) == 0 ){
			rvalue  = node.mother;
			Assert(false, "Optim is useless");
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
			int c = getIval( node.father );
			if(c != 0){
				rvalue  = getCnode( getIval( node.mother ) / c );
			}else{
				rvalue  = getCnode( 0 );
			}
			return;
		}			
	}
	if(node.mother->type == bool_node::MOD){
		bool_node* mpar = node.mother;
		if(mpar->father == node.father){
			rvalue = getCnode(0);
			return;
		}
	}
	rvalue = &node;
}
void DagOptim::visit( MOD_node& node ){
	if( isConst(node.mother) ){ // const prop
		if( isConst(node.father) ){
			int c = getIval( node.father );
			if(c != 0){
				rvalue  = getCnode( getIval( node.mother ) % c );
			}else{
				rvalue  = getCnode( 0 );
			}
			return;
		}			
	}
	if(node.mother->type == bool_node::MOD){
		bool_node* mpar = node.mother;
		if(mpar->father == node.father){
			rvalue = mpar;
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
		int mvalue = getIval( node.mother );
		if( node.father->getOtype() == bool_node::BOOL ){			
			
			if(mvalue == 1){
				rvalue = node.father;
				return;	
			}
			
			if(mvalue == 0){
				NOT_node* nt = new NOT_node();				
				nt->mother = node.father;
				nt->addToParents();
				addNode(nt);
				nt->accept(*this);
				return;	
			}
		}

		/*

		if(typeid(*node.father) == typeid(ARRACC_node)){
			ARRACC_node* nf = dynamic_cast<ARRACC_node*>(node.father);
			if(nf->mother->getOtype() == bool_node::BOOL){
				if(isConst(nf->multi_mother[0]) && isConst(nf->multi_mother[1])){
					int mm1 = getIval(nf->multi_mother[0]);
					int mm2 = getIval(nf->multi_mother[1]);
					if((mm1 == mvalue && mm2 != mvalue) || (mm2 == mvalue && mm1 != mvalue)){
						cout<<"Sweet"<<endl;
					}
				}
			}
		}

		*/

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
				nt->mother = node.mother;
				nt->addToParents();
				addNode(nt);
				nt->accept(*this);
				return;	
			}
		}
	}
	

	if(node.father->getOtype() == bool_node::BOOL && node.mother->getOtype() == bool_node::BOOL){
		XOR_node* x = new XOR_node();
		x->mother = node.mother;
		x->father = node.father;
		x->addToParents();
		addNode(x);
		NOT_node* n = new NOT_node();
		n->mother = x;
		n->addToParents();
		addNode(n);
		stillPrivate = n;
		rvalue = n;
		return;
	}

		
	if( compSymplification<equal_to<int> , EQ_node>(node) ){
		return;	
	}		
	
	rvalue = &node;
}

void DagOptim::visit( UFUN_node& node ){
	if(isConst(node.mother)){
		if(getIval( node.mother ) == 0){
			rvalue = getCnode(0);
			return;
		}
	}

	string tmp = node.get_ufname();
	tmp += ".";
	tmp += node.outname;
	tmp += "(";

	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
		char tmpbo[256];
		itoa(mmid, tmpbo, 10);
		tmp += tmpbo;
 		tmp += ",";
 	}

	tmp +=")";

	
	map<string, UFUN_node*>::iterator bro =  callMap.find(tmp);
	if(bro != callMap.end()){
		UFUN_node* brother = bro->second;

		if(brother->mother == node.mother){
			rvalue = brother;
			return;
		}

		brother->dislodge();
		brother->mother->remove_child(brother);
		bool_node* on = new OR_node();
		on->mother = brother->mother;
		on->father = node.mother;
		
		{
			bool_node* onPr = this->computeOptim(on);
			if(onPr == on){
				on->addToParents();
				addNode(on);
			}else{
				delete on;
			}
			on = onPr;
		}
		
		brother->mother = on;
		brother->resetId();
		brother->addToParents();
		rvalue = brother;
		return;
	}else{
		callMap[tmp] = &node;
		rvalue  = &node;
		return;
	}


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
	
	
	
	if( node.multi_mother.size() >0 ){
		bool_node* bn =  node.multi_mother[0];
		for(int i=1; i<node.multi_mother.size(); ++i){
			if( bn != node.multi_mother[i] ){
				tmp = false;
			}
		}
		if( tmp ){
			if( (node.multi_mother.size()==2 && node.mother->getOtype()== bool_node::BOOL) || PARAMS->assumebcheck ){
				/* 
				 * It may seem that the transformation inside the following if statement also applies to
				 * ARRACC with non-bool mother. But this is not the case, because if the mother is non-bool, 
				 * then the access could be out of bounds, and then, the result could be different to 
				 * multi_mother[0] even if all multimothers are the same. (REMEMBER: out of bounds ARRACC evaluates to zero).
				 * */
				rvalue = bn;
				return;	
			}else{
				if( isConst(bn) &&  getIval( bn ) == 0){
					rvalue = getCnode(0);
					return;
				}else{
					
					LT_node* nt = new LT_node();
					nt->mother = node.mother;
					nt->father = this->getCnode((int) node.multi_mother.size());
					nt->addToParents();
					addNode(nt);
					
					ARRACC_node* ar = new ARRACC_node();
					ar->mother = nt;
					ar->multi_mother.push_back(getCnode(0));
					ar->multi_mother.push_back(bn);						
					ar->addToParents();
					addNode(ar);

					GE_node* gt = new GE_node();
					gt->mother = node.mother;
					gt->father = this->getCnode(0);
					gt->addToParents();
					addNode(gt);
					
					ARRACC_node* gar = new ARRACC_node();
					gar->mother = gt;
					gar->multi_mother.push_back(getCnode(0));
					gar->multi_mother.push_back(ar);						
					gar->addToParents();
					addNode(gar);

					
					rvalue = gar;
					return;
				}
			}
		}
	}


	
	if( node.multi_mother.size()==2 && node.mother->getOtype()== bool_node::BOOL ){


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
						nt->mother = node.mother;
						nt->addToParents();
						addNode(nt);
						stillPrivate = nt;
						nt->accept(*this);
						return;
					}
				}
			}
			
			if(val0 == 0 && node.multi_mother[1]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();				
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();
				addNode(an);
				stillPrivate = an;
				an->accept(*this);
				return;
			}
		}

		if( isConst(node.multi_mother[1]) ){
			int val1 = getIval( node.multi_mother[1] );	
			if(val1 == 1 && node.multi_mother[0]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				OR_node* an = new OR_node();
				an->mother = node.mother;
				an->father = node.multi_mother[0];
				an->addToParents();
				addNode(an);
				stillPrivate = an;
				an->accept(*this);
				return;
			}
		}


		if( node.mother == node.multi_mother[0] && node.multi_mother[1]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();
				addNode(an);
				stillPrivate = an;
				an->accept(*this);
				return;
		}
		
		
		if( isNotOfEachOther(node.mother, node.multi_mother[0])&& node.multi_mother[1]->getOtype()==bool_node::BOOL && ALTER_ARRACS){
				OR_node* an = new OR_node();
				an->mother = node.multi_mother[0];
				an->father = node.multi_mother[1];
				an->addToParents();
				addNode(an);
				stillPrivate = an;
				an->accept(*this);
				return;
		}


		if(node.mother->type == bool_node::EQ || (node.mother->type == bool_node::NOT && node.mother->mother->type == bool_node::EQ) ){
				
				bool_node* eqmom = node.mother->type == bool_node::EQ ? node.mother  :node.mother->mother;

				if(isConst(eqmom->mother) || isConst(eqmom->father)){
					bool_node* mother = NULL;
					int C;
					if(isConst(eqmom->mother)){
						mother = eqmom->father;
						C = this->getIval(eqmom->mother);
					}else{
						mother = eqmom->mother;
						C = this->getIval(eqmom->father);
					}
					ARRASS_node* an = new ARRASS_node();
					an->mother = mother;
					int b = 0;
					if(node.mother->type == bool_node::NOT){
						b = 1;
					}else{
						b = 0;
					}
					an->multi_mother.push_back(node.multi_mother[b]);
					an->multi_mother.push_back(node.multi_mother[1-b]);
					an->addToParents();
					an->quant = C;
					addNode(an);
					stillPrivate = an;
					rvalue = an;
					return;
				}
		}
		
		
		
		if( typeid(*node.multi_mother[1]) == typeid(node)  ){			
			ARRACC_node& mm1 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[1]);
			if( node.multi_mother[0] == mm1.multi_mother[0] && node.mother->getOtype() == bool_node::BOOL && mm1 .mother->getOtype() == bool_node::BOOL ){
				AND_node* an = new AND_node();
				an->mother = node.mother;
				an->father = mm1 .mother;
				an->addToParents();
				addNode(an);

				an = dynamic_cast<AND_node*>( cse.computeCSE(an) );
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[1] = mm1.multi_mother[1];
				node.resetId();
				node.addToParents();				
				node.accept(*this);
				return;
			}	

			if(node.mother == mm1.mother){
				//cout<<" I just saved a bunch of nodes !! Wehee! "<<endl;
				node.dislodge();
				node.multi_mother[1] = mm1.multi_mother[1];
				node.resetId();
				node.addToParents();				
				rvalue = &node;
				return;
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
			ARRACC_node& mm0 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[0]);		
			if( node.multi_mother[1] == mm0.multi_mother[1] && mm0.children.size() < 2){
				OR_node* on = new OR_node();
				on->mother = node.mother;
				on->father = mm0 .mother;
				on->addToParents();
				addNode(on);

				// on = dynamic_cast<OR_node*>( cse.computeCSE(on) );
				
				bool_node* ton = this->computeOptim(on);

				node.dislodge();
				node.mother = ton;
				node.multi_mother[0] = mm0.multi_mother[0];
				node.resetId();
				node.addToParents();
				node.accept(*this);
				return;
			}
			
			
			if(node.mother == mm0.mother){
				//cout<<" I just saved a bunch of nodes !! Wehee! "<<endl;
				node.dislodge();
				node.multi_mother[0] = mm0.multi_mother[0];
				node.resetId();
				node.addToParents();
				rvalue = &node;
				return;
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
		
		
	}else{
		staticCompare<less<int> >(node.mother, node.multi_mother.size(), false);
		AbstractNodeValue& val = anv[node.mother];
		if(!val.isTop()){
			int h = val.getHigh();
			int l = val.getLow();
			/*Look at the range for node.mother. Then do the following optimizations:
			--If the range is entirely within node.multi_mother and all the entries in that range are equal to tmpnode, return tmpnode.
			--If the lower-bound of the range is >0, set all entries below l to zero.
			--If the upper-bound of the range is < node.multi_mother.size()-1, shave off all those extra entries.
			*/
			if(h < node.multi_mother.size()-1 || l > 0){
				ARRACC_node* ar = new ARRACC_node();
				ar->mother = node.mother;
				int size = (h+1) < node.multi_mother.size() ? (h+1) : node.multi_mother.size();
				ar->multi_mother.reserve(size);
				bool_node* tmpnode = NULL;
				bool tflag =  (h < node.multi_mother.size() && l >= 0) || PARAMS->assumebcheck;
				for(int i=0; i<size; ++i){
					if(i >= l && !(val.isList() && !val.contains(i))){
						bool_node* x = node.multi_mother[i];
						if(tmpnode ==NULL){
							tmpnode = x;
						}else{
							tflag = tflag && (tmpnode == x);
						}
						ar->multi_mother.push_back( x );
					}else{
						ar->multi_mother.push_back(getCnode(0));
					}
				}
				if(size == 0){
					rvalue = getCnode(0);
					return;
				}
				if(tflag){ 
					delete ar;
					if(tmpnode == NULL){
						rvalue = getCnode(0);
					}else{
						rvalue = tmpnode;
					}
					return;
				}else{
					ar->addToParents();					
					addNode(ar);
					if(ar->multi_mother.size() != node.multi_mother.size()){
						stillPrivate = ar;
						ar->accept(*this);
					}else{
						rvalue = ar;
					}
					return;
				}
				
			}
		}

	}
	
	rvalue = &node;
}

void checkArrass(vector<bool_node*>&  vv,  bool_node* mot, ARRASS_node& node, int lev){
	vv.push_back(node.multi_mother[1]);
	ARRASS_node* dc = dynamic_cast<ARRASS_node*>(node.multi_mother[0]);
	if(dc != NULL && dc->quant == lev + 1 && dc->mother == mot){
		checkArrass(vv ,mot, *dc, lev+1);
	}else{
		vv.push_back(node.multi_mother[0]);
	}
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

	if(false && node.quant == 0){ // EXPERIMENTAL OPTIM.
		vector<bool_node*>  vv;		
		checkArrass(vv, node.mother, node, 0);
		if(vv.size()>2){
			ARRACC_node* an = new ARRACC_node();
			an->mother = node.mother;
			an->multi_mother.swap(vv);
			an->addToParents();
			addNode(an);
			int sz = an->multi_mother.size();
			LT_node* ltn = new LT_node();
			ltn->mother = node.mother;
			ltn->father = this->getCnode(sz);
			ltn->addToParents();
			addNode(ltn);

			ARRACC_node* an2 = new ARRACC_node();
			an2->mother = ltn;
			an2->multi_mother.push_back(an->multi_mother[sz-1]);
			an2->multi_mother.push_back(an);
			an2->addToParents();
			addNode(an2);
			rvalue = an2;
			return;
		}
	}


	if(typeid(*node.mother) == typeid(PLUS_node) && (isConst(node.mother->mother) || isConst(node.mother->father)  )){
		bool_node* n1 = node.mother;		
		bool_node* nm;
		int C;
		if(isConst(n1->mother)){
			nm = n1->father;
			C = getIval(n1->mother);
		}else{
			nm = n1->mother;
			C = getIval(n1->father);
		}
		
		ARRASS_node* as = new ARRASS_node();
		as->mother = nm;
		as->quant = node.quant - C;
		as->multi_mother = node.multi_mother;
		as->addToParents();
		addNode(as);

		rvalue = as;
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
	if(isConst(node.mother)){
		int cv = this->getIval(node.mother);
		if(cv == 1){
			rvalue = this->getCnode(0);
			return ;
		}
	}
	rvalue = &node;
}	
void DagOptim::visit( DST_node& node ){
	rvalue = &node;
}

void DagOptim::initLight(BooleanDAG& dag){
	dagsize = dag.size();	
	newnodes.clear();
	cnmap.clear();
	cse.clear();
	callMap.clear();
}



void DagOptim::initialize(BooleanDAG& dag){
	/*
	dag.removeNullNodes();
	dag.sort_graph();
	dag.cleanup(false);
	dag.relabel();
	*/
	anv.clear();
	initLight(dag);
}

bool_node* DagOptim::computeCSE(bool_node* node){
	return cse.computeCSE(node);
}

bool_node* DagOptim::computeOptim(bool_node* node){	
	node->accept(*this);
	node = rvalue;
	bool_node* tmp = cse.computeCSE(node);	
	if(tmp != node){
		if(newnodes.size() > 0 && node == stillPrivate && stillPrivate == *( newnodes.rbegin() )){
			stillPrivate->dislodge();
			newnodes.pop_back();
			delete stillPrivate;
			stillPrivate = NULL;
			//cout<<"Saved a node"<<endl;
		}
	}
	return tmp;
}


void DagOptim::cleanup(BooleanDAG& dag){
	dag.removeNullNodes();
	dag.addNewNodes(newnodes);
	//dag.repOK();
	newnodes.clear();
	//dag.sort_graph();
	dag.cleanup();
	dag.relabel();
#ifdef _DEBUG
	dag.repOK();
#endif
}

void DagOptim::process(BooleanDAG& dag){
	timerclass everything("everything");


	everything.start();
	
	
	int k=0;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				

		if(dag[i] != NULL){
			bool_node* node = computeOptim(dag[i]);



			if(dag[i] != node){
					Dout(cout<<dag[i]->id<<" replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl) ;
					
					dag.replace(i, node);
					
			}
		}
	}
	
	
	
	cleanup(dag);

	everything.stop();
	

	Dout( everything.print(); )
	
	Dout(cout<<" end cse "<<endl);
}
