#include "DagOptim.h"
#include "SATSolver.h"
#include "CommandLineArgs.h"

DagOptim::DagOptim(BooleanDAG& dag):cse(dag), stillPrivate(NULL)
{
	ALTER_ARRACS = false;
	possibleCycles = false;
	rvalue = NULL;	
	initialize(dag);
	BOTTOM=-1;
	DONE=-2;
	INSTACK=0;
	isTopLevel = false;
	nccount = 0;
}

DagOptim::~DagOptim()
{
}

//extern CommandLineArgs* PARAMS;

CONST_node* DagOptim::getCnode(double c){
	long long int code = CONST_node::code(c);
	if(code == 0){ 
		return getCnode(0);
	}
	if( cnmap.find(code) == cnmap.end() ){
		CONST_node* cnode = new CONST_node(c);
		cnode->id = newnodes.size() + dagsize;		
		newnodes.push_back(cnode);
		cnmap[code] = cnode;
		Dout(cout<<" add "<<cnode->id<<"  "<<cnode->get_name()<<endl);
		return cnode;
	}else{
		return cnmap[code];	
	}

}


CONST_node* DagOptim::getCnode(int val){
	map<long long int, CONST_node*>::iterator fit = cnmap.find(val<<1);
	if( fit == cnmap.end() ){
		CONST_node* cnode = new CONST_node(val);
		cnode->id = newnodes.size() + dagsize;		
		newnodes.push_back(cnode);
		cnmap[val<<1] = cnode;
		Dout(cout<<" add "<<cnode->id<<"  "<<cnode->get_name()<<endl);
		return cnode;
	}else{
		return fit->second;
	}
}

CONST_node* DagOptim::getCnode(bool c){
	return getCnode( c ? 1 : 0 );
}

bool_node* DagOptim::optAdd(bool_node* bn){
	bn->accept(*this);
    bool_node* tmp = rvalue;
	if(bn == tmp){
		stillPrivate = bn;
		addNode(bn);
		return bn;
	}else{
		bn->dislodge();
		delete bn;
		return tmp;
	}
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

	if(n1->getOtype() == OutType::BOOL){
		AbstractNodeValue& nv = anv[n1];
		nv.init(0);	
		nv.insert(1);
		nv.timestamp = n1->globalId;
		return nv.staticCompare<COMP>(C, reverse);
	}

	if(n1->type == bool_node::SRC ||  n1->type == bool_node::CTRL){
		INTER_node* inode = dynamic_cast<INTER_node*>(n1);
		AbstractNodeValue& nv = anv[n1];
		if(inode->getOtype() == OutType::BOOL){
			nv.init(0);	
			nv.insert(1);
			nv.timestamp = n1->globalId;
			return nv.staticCompare<COMP>(C, reverse);
		}else{
			if(n1->type == bool_node::CTRL){
				int bnd = 1;
					for(int i=0; i<inode->get_nbits(); ++i){
						bnd = bnd*2;
					}
				nv.init(0, bnd);				
			}else{
				if(isTopLevel){
					int s = 1;
					int bnd = 1;
					for(int i=0; i<PARAMS->NINPUTS; ++i){
						bnd = bnd*2;
					}
					nv.init(0, bnd*4);				
				}else{
					nv.makeTop();
				}
			}
			nv.timestamp = n1->globalId;
			return 0;
		}
	}

	if( n1->type == bool_node::TIMES){

		if(isConst(n1->mother) || isConst(n1->father)){
			bool_node* cparent = n1->mother;
			bool_node* oparent = n1->father;
			if(!isConst(cparent)){
				cparent = oparent;
				oparent = n1->mother;
			}
			int cv = getIval(cparent);
				
			AbstractNodeValue& nv = anv[n1];
			nv.timestamp = n1->globalId;
			{
				bool_node* parent = oparent;
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
			nv.scale(cv);
			return nv.staticCompare<COMP>(C, reverse);
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
		if(mnv.isTop() || ar.type == bool_node::ARRASS /*typeid(*n1) == typeid(ARRASS_node)*/){
			for(int i=0; i<ar.multi_mother.size(); ++i){
				bool_node* parent = ar.multi_mother[i];
				helper<COMP>(parent, C, reverse, rv, nv);
			}
			if((!PARAMS->assumebcheck) && ar.type == bool_node::ARRACC  && (n1->mother->getOtype() == OutType::INT || ar.multi_mother.size() < 2)){			
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
			
			int sz = ar.multi_mother.size();
			bool didZero = PARAMS->assumebcheck;
			AbstractNodeValue::ANVIterator ait = mnv.getIter(sz);
			while( ait.next() ){
				bool_node* parent = ar.multi_mother[(*ait)];
				helper<COMP>(parent, C, reverse, rv, nv);
			}
			if(!PARAMS->assumebcheck && ait.dropped){
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
		//(a+b) < (a + c) ==> b < c
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
			rvalue  = optAdd(pnode);
			return true;
		}
	}
		
	if( typeid(*node.mother) == typeid(PLUS_node) ){
		//(a+b) < x
		bool_node* momo = node.mother->mother;
		bool_node* mofa = node.mother->father;
		
		if(mofa == node.father || (isConst(mofa) && (isConst(node.mother))) ){
			//The second check for (isConst(node.mother) was missing
			//This is important or else in cases like x + 1 < x 
			//there won't be any optimization 
			momo = 	node.mother->father;;
			mofa = node.mother->mother;
		}
		//At this point, if any of the two is equal to node.father, it will be momo
				

		if(momo == node.father || (isConst(node.father) && isConst(momo))){
			//(x+b) < x  or  (c2+b) < c1
			NTYPE* pnode = new NTYPE();
			pnode->mother = mofa;
			if(momo == node.father ){
				pnode->father = getCnode(0);
			}else{
				pnode->father = getCnode( getIval(node.father) - getIval(momo) );
				//cout<<"I JUST SAVED SOME CLAUSES A"<<endl;
			}			
			pnode->addToParents();					
			rvalue  = optAdd(pnode);			
			return true;
		}			
		

	}
		
	if( typeid(*node.father) == typeid(PLUS_node) ){
		
		bool_node* momo = node.father->mother;
		bool_node* mofa = node.father->father;
		
		if(mofa == node.mother || ( (isConst(mofa)) && (isConst(node.mother)) )  ){
			momo = 	node.father->father;;
			mofa = node.father->mother;
		}
		//At this point, if any of the two is equal to node.mother, it will be momo
		
		if(momo == node.mother|| ( (isConst(momo)) && (isConst(node.mother)) )){
			NTYPE* pnode = new NTYPE();
			pnode->father = mofa;
			if(momo==node.mother){
				pnode->mother = getCnode(0);
			}else{
				pnode->mother = getCnode( getIval(node.mother) - getIval(momo) );
				//cout<<"I JUST SAVED SOME CLAUSES B"<<endl;
			}
			pnode->addToParents();	
			rvalue  = optAdd(pnode);			
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
	long long int val = node.getCode();	
	if( cnmap.find(val) == cnmap.end() ){
		cnmap[val] = &node;
		rvalue = &node;
	}else{
		rvalue = cnmap[val];	
	}	
}

void DagOptim::visit( AND_node& node ){
	bool_node* nfather = node.father;
	bool_node* nmother = node.mother;

	if( nfather == nmother ){ // x & x == x
		rvalue = nfather;
		return;	
	}
	if( isNotOfEachOther(nfather, nmother) ){ // x & !x == false
		rvalue = getCnode(0);
		return;	
	}
	if( isConst(nfather) ){
		if( isConst(nmother) ){ // const prop
			rvalue = getCnode ( getBval( nfather ) && getBval( nmother ) );				
			return;
		}
		if( !getBval(nfather) ){ // x & false == false;
			rvalue = getCnode(0);
			return;
		}else{
			rvalue = nmother;
			return;	
		}		
	}
	if( isConst(nmother) ){
		if( ! getBval(nmother)){ // false & x == false;
			rvalue = getCnode(0);			
			return;
		}else{
			rvalue = nfather;
			return;	
		}
	}

	if(nmother->type == bool_node::AND){
		if(isNotOfEachOther(nfather, nmother->mother)){
			rvalue = getCnode(0);
			return;	
		}
		if(isNotOfEachOther(nfather, nmother->father)){
			rvalue = getCnode(0);
			return;	
		}

	}

	if(nfather->type == bool_node::AND){
		if(isNotOfEachOther(nmother, nfather->mother)){
			rvalue = getCnode(0);
			return;	
		}
		if(isNotOfEachOther(nmother, nfather->father)){
			rvalue = getCnode(0);
			return;	
		}
	}	
	
	if(nfather->type == bool_node::LT && nmother->type == bool_node::LT){		
		if(nfather->father == nmother->father){
			// (a<x)&(b<x) --> a<x when b<a
			if(isConst(nfather->mother) && isConst(nmother->mother)){
				if(this->getIval(nfather->mother) < this->getIval(nmother->mother)){
					rvalue = nmother;
				}else{
					rvalue = nfather;
				}
				return;
			}

			// (a+e<x) & (b+e<x) ---> a+e<x when b<a
			if(nfather->mother->type == bool_node::PLUS && nmother->mother->type == bool_node::PLUS){
				bool_node* nfm = nfather->mother;
				bool_node* nmm = nmother->mother;

				bool_node* nmmConst = nmm->mother;
				bool_node* nmmExp = nmm->father;
				if(isConst(nmmExp)){
					bool_node* tmp = nmmExp;
					nmmExp = nmmConst;
					nmmConst = tmp;
				}

				bool_node* nfmConst = nfm->mother;
				bool_node* nfmExp = nfm->father;
				if(isConst(nfmExp)){
					bool_node* tmp = nfmExp;
					nfmExp = nfmConst;
					nfmConst = tmp;
				}

				if(isConst(nfmConst) && isConst(nmmConst) && nfmExp== nmmExp){
					if(this->getIval(nfmConst) < this->getIval(nmmConst)){
						rvalue = nmother;
					}else{
						rvalue = nfather;
					}
					return;
				}
			}
			// (a+e<x) & (e<x) ---> a+e<x when 0<a
			if(nmother->mother->type == bool_node::PLUS){
				bool_node* nfm = nfather->mother;
				bool_node* nmm = nmother->mother;

				bool_node* nmmConst = nmm->mother;
				bool_node* nmmExp = nmm->father;
				if(isConst(nmmExp)){
					bool_node* tmp = nmmExp;
					nmmExp = nmmConst;
					nmmConst = tmp;
				}
				if(isConst(nmmConst) && nfm== nmmExp){
					if(0 < this->getIval(nmmConst)){
						rvalue = nmother;
					}else{
						rvalue = nfather;
					}
					return;
				}
			}

		}
		


		// (x<a)&(a<x) --> 0 
		if(nfather->father == nmother->mother && nmother->father == nfather->mother){
			rvalue = this->getCnode(0);
			return;
		}
	}

	if(nfather->type == bool_node::AND && nmother->type == bool_node::LT && nfather->mother->type == bool_node::LT ){
		// (a<x)&((b<x)& T)--> a<x & T when b<a
		bool_node* nfm = nfather->mother;
		if(nfm->father == nmother->father){
			if(isConst(nfm->mother) && isConst(nmother->mother)){
				if(this->getIval(nfm->mother) < this->getIval(nmother->mother)){
					AND_node* an = new AND_node();
					an->mother = nmother;
					an->father = nfather->father;
					an->addToParents();
					rvalue = optAdd(an);					
					return;
				}else{
					rvalue = nfather;
				}
				return;
			}
		}
		
	}


	rvalue = &node;
}

void DagOptim::visit( OR_node& node ){
	bool_node* nfather = node.father;
	bool_node* nmother = node.mother;
	if( nfather == nmother ){ // x | x == x
		rvalue = nfather;
		return;	
	}
	if( isNotOfEachOther(nfather, nmother) ){ // x | !x == true
		rvalue = getCnode(1);
		return;	
	}
	if( isConst(nfather) ){
		if( isConst(nmother) ){ // const prop
			rvalue = getCnode ( getBval( nfather ) || getBval( node.mother ) );				
			return;
		}
		if( getBval(nfather) ){ // x | true == true
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
			rvalue = nfather;
			return;	
		}
	}	
	if(nfather->type == bool_node::NOT && node.mother->type == bool_node::NOT){
		bool_node* an = new AND_node();
		an->mother = node.mother->mother;
		an->father = nfather->mother;
		an->addToParents();
		an = optAdd(an);
		NOT_node* nn = new NOT_node();
		nn->mother = an;
		nn->addToParents();
		rvalue = optAdd(nn);		
		return;
	}	
	if(nmother->type == bool_node::LT && nfather->type == bool_node::NOT && nfather->mother->type == bool_node::LT){
		bool_node* nfe = nfather->mother;
		if(nmother->father == nfe->father){
			if(isConst(nmother->mother)&& isConst(nfe->mother)){
				if(getIval(nmother->mother)<=getIval(nfe->mother)){
					rvalue = getCnode(1);
					return;
				}
			}
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
					rvalue  = optAdd(pnode);
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
					rvalue = optAdd(pnode);						
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
					bool_node* pnode = new PLUS_node();
					pnode->mother = nconst;
					Assert(!isConst(fathernode.mother), "This can't happen");
					pnode->father = fathernode.mother;
					pnode->addToParents();
					pnode = optAdd(pnode);
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

void DagOptim::visit( TUPLE_CREATE_node& node) {
  if (node.depth == -1) {
    int d = 0; bool first = true;
    for (int i = 0; i < node.multi_mother.size(); i++) {
      if (node.multi_mother[i]->getOtype()->isTuple) {
        int dd = node.multi_mother[i]->depth;
        if (first) {
          d = dd;
          first = false;
        }
        if (dd == -1) {
          d = -1;
          break;
        } else if (dd > d) d = dd;
      }
    }
    if (d == -1) node.depth = -1;
    else node.depth = d + 1;
  }
  rvalue = &node;
  return;
}

void DagOptim::visit( TUPLE_R_node& node){
    
    if(!(node.mother->getOtype()->isTuple)){
        rvalue = getCnode(0);
        return;
    }
  if (node.depth == -1) {
    int mdepth = node.mother->depth;
    if (mdepth == 0) { // mother is null
      rvalue = getCnode(0);
      return;
    } else if (mdepth > 0) {
      node.depth = mdepth - 1;
    }
  }
  
    if(node.mother->type == bool_node::TUPLE_CREATE){
        int idx = node.idx;
        TUPLE_CREATE_node* parent = dynamic_cast<TUPLE_CREATE_node*>(node.mother);
        if(idx >= parent->multi_mother.size()|| idx <0){
            //this should never happen
           rvalue = &node;
            return;
        }
        rvalue = parent->multi_mother[idx];
        return;
    }
    if(node.mother->type ==bool_node::ARRACC){
       int idx =node.idx;
       ARRACC_node* parent = dynamic_cast<ARRACC_node*>(node.mother);
        
        /*// multimother size = 2 and one node is null
        if(parent->multi_mother.size() ==2){
            if(!parent->multi_mother[0]->getOtype()->isTuple){
                //0 node is null and 1 node should output a tuple
                //set node's parent to multi_mother[1]
                node.dislodge();
                node.mother = parent->multi_mother[1];
				node.resetId();
				node.addToParents();
				node.accept(*this);
				return;
            }
            //symetric case
            else if(!parent->multi_mother[1]->getOtype()->isTuple){
                //1 node is null and 0 node should output a tuple
                //set node's parent to multi_mother[0]
                node.dislodge();
                node.mother = parent->multi_mother[0];
				node.resetId();
				node.addToParents();
				node.accept(*this);
				return;
            }
            //both parents are non-nulls
            else{
                parent->dislodge();
                node.dislodge();
                for(int i=0; i< parent->multi_mother.size(); i++){
                    TUPLE_R_node* tnode = new TUPLE_R_node();
                    tnode->idx = node.idx;
                    tnode->mother = parent->multi_mother[i];
                    tnode->addToParents();
                    tnode->accept(*this);
                    parent->multi_mother[i] = tnode;
                }
                parent->addToParents();
                
                rvalue = parent;
                return;
                
            }
        } */
        
        
       //If all nodes in arracc in tuples
       bool allTuple = true;
        for(int i=0; i< parent->multi_mother.size(); i++){
            if(parent->multi_mother[i]->type != bool_node::TUPLE_CREATE){
                allTuple = false;
                break;
            }
        }
        if(allTuple){
            if (!node.getOtype()->isArr || parent->mother->getOtype() == OutType::BOOL) {
                ARRACC_node* newParent = new ARRACC_node();
                newParent->mother = parent->mother;
                for(int i=0; i< parent->multi_mother.size(); i++){
                    TUPLE_CREATE_node* tuple_i = dynamic_cast<TUPLE_CREATE_node*>(parent->multi_mother[i]);
                    if(idx >= tuple_i->multi_mother.size()|| idx <0){
                        rvalue = &node;
                        return;
                    }
                    
                    newParent->multi_mother.push_back(tuple_i->multi_mother[idx]);
                }
                newParent->addToParents();
                node.dislodge();
                rvalue = optAdd(newParent);
                return;
              
            } else {
                int size = parent->multi_mother.size();
                bool_node* mother = parent->mother;
                bool_node* curr;
                
                
                for (int i = size - 1; i >= 0; i--) {
                    EQ_node* eq_node = new EQ_node();
                    eq_node->mother = mother;
                    eq_node->father = getCnode(i);
                    eq_node->addToParents();
                    
                    ARRACC_node* an = new ARRACC_node();
                    an->mother = optAdd(eq_node);
                    if (i == size - 1) {
                        an->multi_mother.push_back(getCnode(0));
                    } else {
                        an->multi_mother.push_back(curr);
                    }
                    TUPLE_CREATE_node* tuple_i = dynamic_cast<TUPLE_CREATE_node*>(parent->multi_mother[i]);
                    if(idx >= tuple_i->multi_mother.size()|| idx <0){
                        rvalue = &node;
                        return;
                    }
                    
                    an->multi_mother.push_back(tuple_i->multi_mother[idx]);
                    an->addToParents();
                    curr = optAdd(an);
                    
                }
                Assert(curr != NULL, "This is not possible");
                node.dislodge();
                rvalue = curr;
                return;
            }
        }
        
    }
  
    rvalue = &node;
}

void DagOptim::visit( ARR_R_node& node ){
	//mother = index
	//father = inputarr
	bool_node* nfather = node.father;
	bool_node* nmother = node.mother;

	if(nfather->type == bool_node::ARR_W){
		if(nfather->mother == nmother){
			/* X = A{i -> t}; y = X[i];  ===> y = t;
			*/
			rvalue = dynamic_cast<ARR_W_node*>(nfather)->multi_mother[1];
			return;
		}else{

			if(isConst(nfather->mother) && isConst(nmother) && getIval(nfather->mother)!=getIval(nmother)){
				//They must be different constants, otherwise we wouldn't be in this branch.
				/* X = A{i -> t}; y = X[j];  ===> y = A[j];
				*/
				//Experimental optimization. node.father and node.mother can't be equal because they are different type.
				//So we don't have to dislodge from both, only from father, since that's what we are changing.
				//Also, we only have to insert to the new father.
				//Also, there is no need to change the id of the node, because its semantics haven't changed.
				//node.dislodge();

				bool_node* newFather = ((ARR_W_node*)nfather)->multi_mother[0];

				node.father->remove_child(&node);				
				while(newFather->type == bool_node::ARR_W && isConst(newFather->mother) && getIval(newFather->mother)!=getIval(nmother)){
					newFather = ((ARR_W_node*)newFather)->multi_mother[0];				
				}
				


				node.father = newFather;
				// node.resetId();
				// node.addToParents();
				node.father->children.insert(&node);				
				node.accept(*this);
				return;
			}





			if(node.father->mother->type == bool_node::PLUS &&
				node.father->mother->mother == node.mother && 
				isConst(node.father->mother->father) &&
				getIval(node.father->mother->father) != 0
				){
				/*X = A{b+i -> t}; y = X[b]; y = A[b];*/
				node.dislodge();
				node.father = dynamic_cast<ARR_W_node*>(node.father)->multi_mother[0];
				node.resetId();
				node.addToParents();
				node.accept(*this);
				return;
			}


			if(node.mother->type == bool_node::PLUS){
				if(isConst(node.mother->father)){// not equal zero.
					int jval = getIval(node.mother->father);
					Assert(jval!= 0, "AEfalke");
					if(node.mother->mother == node.father->mother ||
						(node.father->mother->type == bool_node::PLUS &&
						node.father->mother->mother == node.mother->mother && 
						isConst(node.father->mother->father) &&
						getIval(node.father->mother->father) != jval)
						){
				/*
				X = A{b+i -> t}; y = X[b+j]; y = A[j];
				or
				X = A{b -> t}; y = X[b+j]; y = A[j];
				*/
							node.dislodge();
							node.father = dynamic_cast<ARR_W_node*>(node.father)->multi_mother[0];
							node.resetId();
							node.addToParents();
							node.accept(*this);
							return;
					}
				}
			}

			if(isConst(nfather->mother) && nmother->type == bool_node::ARRACC && isConst(((ARRACC_node*)nmother)->multi_mother[0]) && isConst(((ARRACC_node*)nmother)->multi_mother[1]) && ((ARRACC_node*)nmother)->multi_mother.size()==2 ){
				/* X = A{i -> t}; y = X[a?b:c];  ===> y = a?X[b]:X[c];
				*/
				ARRACC_node *amother = ((ARRACC_node*)nmother);
				ARRACC_node *an = new ARRACC_node();
				an->mother = nmother->mother;
				ARR_R_node* ar0 = new ARR_R_node();
				ar0->mother = amother->multi_mother[0];
				ar0->father = nfather;
				ar0->addToParents();


				ARR_R_node* ar1 = new ARR_R_node();
				ar1->mother = amother->multi_mother[1];
				ar1->father = nfather;
				ar1->addToParents();

				an->multi_mother.push_back( optAdd(ar0) );
				an->multi_mother.push_back( optAdd(ar1) );
				an->addToParents();
				rvalue = optAdd(an);				
				return;
			}



		}
	}

	if(isConst(nmother) && nfather->type == bool_node::ARR_CREATE){
		int idx = getIval(nmother);
		ARR_CREATE_node* acn = ((ARR_CREATE_node*)node.father);
		if(idx >= acn->multi_mother.size()){
			rvalue = getCnode(acn->dfltval);
			return;
		}
		rvalue = acn->multi_mother[idx];
		return;
	}

	if(isConst(node.father)){		
		rvalue = node.father;		
		return;
	}
	rvalue = &node;
}

void DagOptim::visit( ARR_W_node& node ){
	 /* multi-mother[0] = old-array;
        multi-mother[1] = new-value;*/
	if(node.multi_mother[0] == node.multi_mother[1]){
		rvalue = node.multi_mother[0];
		return;
	}


	if(isConst(node.multi_mother[0]) && isConst(node.mother) && isConst(node.multi_mother[1])){
		if(nccount < NCREATORS){
			bool_node* defval = node.multi_mother[0];		
			int idx = getIval(node.mother);
			if(idx == 0){
				tempcreators[nccount].first = node.globalId;
				tempcreators[nccount].second.dfltval = getIval(defval);
				tempcreators[nccount].second.multi_mother.push_back(node.multi_mother[1]);
				++nccount;
			}			
		}
	}

	if(node.multi_mother[0]->type == bool_node::ARR_W && isConst(node.mother) && isConst(node.multi_mother[1])){
		for(int i=0; i<nccount; ++i){
			if(tempcreators[i].first == node.multi_mother[0]->globalId){
				ARR_CREATE_node* old = &(tempcreators[i].second);
				int idx = getIval(node.mother);
				if(idx == old->multi_mother.size()){
					old->multi_mother.push_back(node.multi_mother[1]);
					tempcreators[i].first = node.globalId;
				}
				break;
			}
		}
	}


	/*
	X = A{i->t}; Y = X{i->q} ===> X=A{i->t}; Y = A{i->q}
	*/
	if(node.multi_mother[0]->type == bool_node::ARR_W){
		if(node.mother == node.multi_mother[0]->mother){
			ARR_W_node* X = dynamic_cast<ARR_W_node*>(node.multi_mother[0]);
			node.dislodge();
			node.multi_mother[0] = X->multi_mother[0];
			node.resetId();
			node.addToParents();
			node.accept(*this);
			return;
		}
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





void DagOptim::visit( EQ_node& node ){
    node.mother->getOtype();
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
		if( node.father->getOtype() == OutType::BOOL ){			
			
			if(mvalue == 1){
				rvalue = node.father;
				return;	
			}
			
			if(mvalue == 0){
				NOT_node* nt = new NOT_node();				
				nt->mother = node.father;
				nt->addToParents();
				rvalue = optAdd(nt);				
				return;	
			}
		}
        if(node.father->type == bool_node::TUPLE_CREATE){
            rvalue = getCnode(false);
			return;
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
        
        
		if( node.mother->getOtype() == OutType::BOOL ){
           
			int mvalue = getIval( node.father );
			
			if(mvalue == 1){
				rvalue = node.mother;
				return;	
			}
			
			if(mvalue == 0){
				NOT_node* nt = new NOT_node();
				nt->mother = node.mother;
				nt->addToParents();
				rvalue = optAdd(nt);				
				return;	
			}
            
            
		}
        if(node.mother->type == bool_node::TUPLE_CREATE){
            rvalue = getCnode(false);
			return;
        }
        
	}
	
	
	if(node.father->getOtype() == OutType::BOOL && node.mother->getOtype() == OutType::BOOL){
		bool_node* x = new XOR_node();
		x->mother = node.mother;
		x->father = node.father;
		x->addToParents();
		x = optAdd(x);
		bool_node* n = new NOT_node();
		n->mother = x;
		n->addToParents();
		n = optAdd(n);		
		rvalue = n;
		return;
	}

	
	if( compSymplification<equal_to<int> , EQ_node>(node) ){
		return;	
	}		
	
	rvalue = &node;
}


void DagOptim::checkAndPush(bool_node* bn, stack<bool_node*>& sd, set<bool_node*>& bnmap){
	if(bn != NULL && bnmap.count(bn)==0){
		bnmap.insert(bn);		
		sd.push(bn);
	}
}


/* Is dest reachable from src? 
* 
*/
bool DagOptim::checkPrecedence(bool_node* dest, bool_node* src){	
	stack<bool_node*> sd;
	set<bool_node*> bnmap;
	sd.push(dest);	
	int cnt = 0;
	while(!sd.empty()){
		++cnt;
		bool_node* bn = sd.top();
		sd.pop();
		if(bn == src){
			return true;
		}
		checkAndPush(bn->mother, sd, bnmap);
		checkAndPush(bn->father, sd, bnmap);
		if(bn->isArith()){
			arith_node* an = dynamic_cast<arith_node*>(bn);
			for(int i=0; i<an->multi_mother.size(); ++i){
				checkAndPush(an->multi_mother[i], sd, bnmap);
			}
		}
	}
	return false;
}






void DagOptim::visit( UFUN_node& node ){
    
    //cout<<"in ufun"<<endl;
    
	if(isConst(node.mother)){
		if(getIval( node.mother ) == 0){
			rvalue = getCnode(0);
			return;
		}
	}
	/*if(node.dependent()){
		if(isConst(node.multi_mother[0])){
			//The node on which I depend was eliminated, so I should go away as well.
			//not strictly necessary since the only way the mother could go away is if the 
			//path condition is false, in which case whatever I output here will not matter.
			rvalue = getCnode(0);
			return;
		}
		UFUN_node* mn = dynamic_cast<UFUN_node*>(node.multi_mother[0]);
		if(mn->fgid != node.fgid){
			node.fgid = mn->fgid;
		}
	}*/
   
   /* int fgid = node.fgid;
    if (fgid !=0 && !node.combined) {
        if (combinedFunCallMap.count(fgid) > 0) {
            UFUN_node* brother = combinedFunCallMap[fgid];
            brother->dislodge();
                brother->mother->remove_child(brother);
                bool_node* on = new OR_node();
                on->mother = brother->mother;
                on->father = node.mother;
                on->addToParents();
                on = optAdd(on);
                brother->mother = on;
                int size = brother->multi_mother.size();
                int size1 = node.multi_mother.size();
                Assert(size == size1, "Size of inputs of both nodes should be equal");
                
                for (int i = 0; i < size; i++) {
                    ARRACC_node* inputNode = new ARRACC_node();
                    inputNode->mother = node.mother;
                    inputNode->multi_mother.push_back(brother->multi_mother[i]);
                    inputNode->multi_mother.push_back(node.multi_mother[i]);
                    
                    inputNode->addToParents();
                    bool_node* optInputNode = optAdd(inputNode);
                    
                    brother->multi_mother[i] = optInputNode;
                }
                brother->resetId();
                brother->addToParents();
                brother->fgid = 0;
                rvalue = brother;
                return;
            
        } else {
            combinedFunCallMap[fgid] = &node;
            node.combined = true;
            node.fgid = 0;
            rvalue  = &node;
            return;
        }
    } */

    if (node.fgid != 0) {
        rvalue  = &node;
		return;
    }    

	string tmp = node.get_ufname();
	if(node.ignoreAsserts){
		tmp += "_IA";
	}
	tmp += ".";
	tmp += node.outname;
	tmp += "(";

	for(int i=0; i<node.multi_mother.size(); ++i){
 		int mmid = node.multi_mother[i] == NULL? -1: node.multi_mother[i]->id;
		char tmpbo[256];		
		// itoa(mmid, tmpbo, 10);
		sprintf(tmpbo,"%d", mmid);
		tmp += tmpbo;
 		tmp += ",";
 	}

	tmp +=")";

	
	map<string, UFUN_node*>::iterator bro =  callMap.find(tmp);
	
	if(bro != callMap.end()){
		UFUN_node* brother = bro->second;
		if(brother->mother == node.mother){			
			rvalue = brother;

			bool_node* tnode = cse.computeCSE(rvalue);
			if(tnode != rvalue){
				rvalue = tnode;				
			}
			return;
		}
		possibleCycles = true;				
		if(true /*|| !checkPrecedence(node.mother, brother)*/){
			
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
//			brother->resetId();
//			Shouldn't reset id. Global ID stays the same because meaning stays the same.			
			brother->addToParents();
			
            rvalue = brother;

			bool_node* tnode = cse.computeCSE(rvalue);
			if(tnode != rvalue){
				rvalue = tnode;				
			}

			return;
		}else{
			Assert(false, "Dead branch");
			child_iter end = node.children.end();
			for(child_iter it = node.children.begin(); 
												it !=end; ++it){
				
				(*it)->replace_parent(&node, brother);
			}
			node.children.clear();
			rvalue = &node;
			bool_node* tnode = cse.computeCSE(rvalue);
			if(tnode != rvalue){
				rvalue = tnode;				
			}
			return;
		}


		 
	}else{
		rvalue  = &node;
		bool_node* tnode = cse.computeCSE(rvalue);
		if(tnode != rvalue){
			rvalue = tnode;			
		}
		callMap[tmp] = dynamic_cast<UFUN_node*>(rvalue);
		return;
	}


	rvalue = &node;	
}

// m >= f <==> !(m < f);
bool_node*  DagOptim::addGE(bool_node* mother, bool_node* father){
	bool_node* lt = new LT_node();
	lt->mother = mother;
	lt->father = father;
	lt->addToParents();
	lt = optAdd(lt);
	NOT_node* nn = new NOT_node();
	nn->mother = lt;
	nn->addToParents();	
	return optAdd(nn);
}

// m > f <==> (f < m);
bool_node*  DagOptim::addGT(bool_node* mother, bool_node* father){
	LT_node* lt = new LT_node();
	lt->mother = father;
	lt->father = mother;
	lt->addToParents();	
	return optAdd(lt);
}

// m <= f <==> !(f < m);
bool_node*  DagOptim::addLE(bool_node* mother, bool_node* father){
	bool_node* lt = new LT_node();
	lt->mother = father;
	lt->father = mother;
	lt->addToParents();
	lt = optAdd(lt);
	NOT_node* nn = new NOT_node();
	nn->mother = lt;
	nn->addToParents();	
	return optAdd(nn);	
}


bool DagOptim::optimizeMMsize2(ARRACC_node& node){
	
    
		if( isConst(node.multi_mother[0]) ){
			int val0 = getIval( node.multi_mother[0] );
			if( isConst(node.multi_mother[1]) ){
                int val1 = getIval( node.multi_mother[1] );
				if( (val0 == 0 || val0 == 1) &&  (val1 == 0 || val1 == 1) ){
				//We know val0 and val1 are different.
					if(val0 == 0){
						rvalue = node.mother;
						return true;
					}else{
						NOT_node* nt = new NOT_node();
						nt->mother = node.mother;
						nt->addToParents();
						rvalue = optAdd(nt);												
						return true;
					}
				}
			}
			// if( isConst(node.multi_mother[0]) )
			if(val0 == 0 && node.multi_mother[1]->getOtype()==OutType::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();				
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();
				rvalue = optAdd(an);			
				return true;
			}

			if(val0==1 && node.multi_mother[1] == node.mother){
				rvalue = getCnode(1);
				return true;
			}

		}//if( isConst(node.multi_mother[0]) )
    
		if( isConst(node.multi_mother[1]) ){
            
			int val1 = getIval( node.multi_mother[1] );	
			if(val1 == 1 && node.multi_mother[0]->getOtype()==OutType::BOOL && ALTER_ARRACS){
				OR_node* an = new OR_node();
				an->mother = node.mother;
				an->father = node.multi_mother[0];
				an->addToParents();
				rvalue = optAdd(an);			
				return true;
			}
            
		}//if( isConst(node.multi_mother[1]) )


		if( node.mother == node.multi_mother[0] && node.multi_mother[1]->getOtype()==OutType::BOOL && ALTER_ARRACS){
				AND_node* an = new AND_node();
				an->mother = node.mother;
				an->father = node.multi_mother[1];
				an->addToParents();
				rvalue = optAdd(an);		
				return true;
		}
		
		
		if( isNotOfEachOther(node.mother, node.multi_mother[0])&& node.multi_mother[1]->getOtype()==OutType::BOOL && ALTER_ARRACS){
				OR_node* an = new OR_node();
				an->mother = node.multi_mother[0];
				an->father = node.multi_mother[1];
				an->addToParents();
				rvalue = optAdd(an);		
				return true;
		}

   
		if(node.mother->type == bool_node::EQ || (node.mother->type == bool_node::NOT && node.mother->mother->type == bool_node::EQ) ){
				
				bool_node* eqmom = node.mother->type == bool_node::EQ ? node.mother  :node.mother->mother;
           
				if(node.getOtype() == OutType::INT || node.getOtype() == OutType::BOOL){
                   
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
						rvalue = optAdd(an);
						return true;
					}
				}
           
		}
		
		
		
		if( node.multi_mother[1]->type == bool_node::ARRACC  ){
           
			ARRACC_node& mm1 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[1]);
			if( node.multi_mother[0] == mm1.multi_mother[0] && node.mother->getOtype() == OutType::BOOL && mm1 .mother->getOtype() == OutType::BOOL ){
				bool_node* an = new AND_node();
				an->mother = node.mother;
				an->father = mm1 .mother;
				an->addToParents();
				an = optAdd(an);

				an =  cse.computeCSE(an) ;
				
				node.dislodge();
				node.mother = an;
				node.multi_mother[1] = mm1.multi_mother[1];
				node.resetId();
				node.addToParents();				
				node.accept(*this);
				return true;
			}	

			if(node.mother == mm1.mother){
				//cout<<" I just saved a bunch of nodes !! Wehee! "<<endl;
				node.dislodge();
				node.multi_mother[1] = mm1.multi_mother[1];
				node.resetId();
				node.addToParents();				
				rvalue = &node;
				return true;
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
   
		
		if( node.multi_mother[0]->type == bool_node::ARRACC  ){
			ARRACC_node& mm0 = 	dynamic_cast<ARRACC_node&>(*node.multi_mother[0]);		
			if( node.multi_mother[1] == mm0.multi_mother[1] && mm0.mother->getOtype() == OutType::BOOL &&  mm0.children.size() < 2){
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
				return true;
			}
			
			
			if(node.mother == mm0.mother){
				//cout<<" I just saved a bunch of nodes !! Wehee! "<<endl;
				node.dislodge();
				node.multi_mother[0] = mm0.multi_mother[0];
				node.resetId();
				node.addToParents();
				rvalue = &node;
				return true;
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
    
		if(node.mother->type == bool_node::NOT){
			bool_node* newmom = node.mother->mother;
			bool_node* np0 = node.multi_mother[1];
			bool_node* np1 = node.multi_mother[0];
			node.dislodge();
			node.mother = newmom;
			node.multi_mother[0] = np0;
			node.multi_mother[1] = np1;
			node.resetId();
			node.addToParents();
			node.accept(*this);
			return true;
		}

		return false;
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
 if (node.depth == -1 ){
    if (node.getOtype()->isTuple) {
      if (node.multi_mother.size() > 0) {
        int d = node.multi_mother[0]->depth;
        if (d != -1) {
          for (int i = 1; i < node.multi_mother.size(); i++) {
            int dd = node.multi_mother[i]->depth;
            if (dd == -1) {
              d = -1;
              break;
            }
            else if (dd > d) d = dd;
          }
        }
        node.depth = d;
      }
      
      
    } else {
      node.depth = -1;
    }
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
			if( (node.multi_mother.size()==2 && node.mother->getOtype()== OutType::BOOL) || PARAMS->assumebcheck ){
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
					
					bool_node* nt = new LT_node();
					nt->mother = node.mother;
					nt->father = this->getCnode((int) node.multi_mother.size());
					nt->addToParents();
					nt = optAdd(nt);
					
					ARRACC_node* ar = new ARRACC_node();
					ar->mother = nt;
					ar->multi_mother.push_back(getCnode(0));
					ar->multi_mother.push_back(bn);						
					ar->addToParents();
					bool_node* arrr = optAdd(ar);

					bool_node* gt = addGE(node.mother, this->getCnode(0));
					
					ARRACC_node* gar = new ARRACC_node();
					gar->mother = gt;
					gar->multi_mother.push_back(getCnode(0));
					gar->multi_mother.push_back(arrr);						
					gar->addToParents();
					rvalue = optAdd(gar);					
					return;
				}
			}
		}
	}


	
	if( node.multi_mother.size()==2 && node.mother->getOtype()== OutType::BOOL ){
        if(this->optimizeMMsize2(node)){
			return;
		}
	}else{
		if(node.mother->type == bool_node::ARRACC){
			ARRACC_node* an = dynamic_cast<ARRACC_node*>(node.mother);
			if(an->mother->getOtype() == OutType::BOOL && an->multi_mother.size()==2 && an->multi_mother[0]->type == bool_node::CONST && an->multi_mother[1]->type == bool_node::CONST){
				int c0 = this->getIval(an->multi_mother[0]);
				int c1 = this->getIval(an->multi_mother[1]);
				ARRACC_node* ar = new ARRACC_node();
				ar->mother = an->mother;
				if(c0 < node.multi_mother.size()){
					ar->multi_mother.push_back(node.multi_mother[c0]);
				}else{
					ar->multi_mother.push_back(this->getCnode(0));
				}
				if(c1 < node.multi_mother.size()){
					ar->multi_mother.push_back(node.multi_mother[c1]);
				}else{
					ar->multi_mother.push_back(this->getCnode(0));
				}
				ar->addToParents();					
				rvalue = optAdd(ar);				
				return;
			}
		}



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
					delete ar;
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


	if(node.multi_mother.size()==2 && node.multi_mother[0]->type == bool_node::ARR_W){
		ARR_W_node* arrw = dynamic_cast<ARR_W_node*>(node.multi_mother[0]);
		/*!
			arrw 
			multi-mother[0] = old-array;
			multi-mother[1] = new-value;        
		*/
		if(arrw->multi_mother[0] == node.multi_mother[1]){
			/*
			X = W{i->t}   //arrw
			Y= [p]$X, W$  //node
			should be transformed into
			to = W[i]
			tn = [p]$t, to$
			Y = W{i->tn}
			*/
			bool_node* W = node.multi_mother[1];
			ARR_R_node* to = new ARR_R_node();
			to->mother = arrw->mother; // index;
			to->father = W; 			
			to->addToParents();			
			bool_node* too = optAdd(to);
				
			ARRACC_node* tn = new ARRACC_node();
			tn->mother = node.mother;
			tn->multi_mother.push_back(arrw->multi_mother[1]);
			tn->multi_mother.push_back(too);			
			tn->addToParents();			
			bool_node* tnn = optAdd(tn);

			ARR_W_node* Y = new ARR_W_node();
			Y->mother = arrw->mother;
			Y->multi_mother.push_back(W);
			Y->multi_mother.push_back(tnn);			
			Y->addToParents();
			rvalue = optAdd(Y);
			return;
		}

	}


	if(node.multi_mother.size()==2 && node.multi_mother[1]->type == bool_node::ARR_W){
        
		ARR_W_node* arrw = dynamic_cast<ARR_W_node*>(node.multi_mother[1]);
		/*!
			arrw 
			multi-mother[0] = old-array;
			multi-mother[1] = new-value;        
		*/
		if(arrw->multi_mother[0] == node.multi_mother[0]){
			/*
			X = W{i->t}   //arrw
			Y= [p]$W, X$  //node
			should be transformed into
			to = W[i]
			tn = [p]$to, t$
			Y = W{i->tn}
			*/
			bool_node* W = node.multi_mother[0];
			ARR_R_node* to = new ARR_R_node();
			to->mother = arrw->mother; // index;
			to->father = W; 			
			to->addToParents();			
			bool_node* too = optAdd(to);
				
			ARRACC_node* tn = new ARRACC_node();
			tn->mother = node.mother;
			tn->multi_mother.push_back(too);
			tn->multi_mother.push_back(arrw->multi_mother[1]);			
			tn->addToParents();			
			bool_node* tnn = optAdd(tn);

			ARR_W_node* Y = new ARR_W_node();
			Y->mother = arrw->mother;
			Y->multi_mother.push_back(W);
			Y->multi_mother.push_back(tnn);			
			Y->addToParents();
			rvalue = optAdd(Y);
			return;
		}

		if(node.multi_mother[0]->type == bool_node::ARR_W){
			ARR_W_node* arrt = dynamic_cast<ARR_W_node*>(node.multi_mother[0]);
			if(arrw->mother == arrt->mother && arrw->multi_mother[0] == arrt->multi_mother[0]){
				/*
				T = W{i->q}	  //arrt
				X = W{i->t}   //arrw
				Y= [p]$T, X$  //node
				should be transformed into
				tn = [p]$t, q$
				Y = W{i->tn}
				*/
				bool_node* W = arrw->multi_mother[0];
				ARRACC_node* tn = new ARRACC_node();
				tn->mother = node.mother;
				tn->multi_mother.push_back(arrt->multi_mother[1]);
				tn->multi_mother.push_back(arrw->multi_mother[1]);								
				tn->addToParents();				
				bool_node* tnn = optAdd(tn);

				ARR_W_node* Y = new ARR_W_node();
				Y->mother = arrw->mother;
				Y->multi_mother.push_back(W);
				Y->multi_mother.push_back(tnn);				
				Y->addToParents();
				rvalue = optAdd(Y);
				return;
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

	OutType* ot = node.getOtype();
    if (ot->isArr) {
        Assert(false, "This is bad");
    }
    if(node.quant == 0  &&  (ot == OutType::INT || ot == OutType::BOOL) ){ // This only works for non-array things, bt you don't need the test because you'll never have array ARRASS nodes.
        
		vector<bool_node*>  vv;		
		checkArrass(vv, node.mother, node, 0);
		if(vv.size()>3){
			ARRACC_node* an = new ARRACC_node();
			an->mother = node.mother;
			an->multi_mother.swap(vv);
			an->addToParents();
			addNode(an);
			int sz = an->multi_mother.size();
			bool_node* ltn = new LT_node();
			ltn->mother = node.mother;
			ltn->father = this->getCnode(sz);
			ltn->addToParents();
			ltn = optAdd(ltn);

			ARRACC_node* an2 = new ARRACC_node();
			an2->mother = ltn;
			an2->multi_mother.push_back(an->multi_mother[sz-1]);
			an2->multi_mother.push_back(an);
			an2->addToParents();			
			rvalue = optAdd(an2);
			return;
		}
	}


	if(node.mother->type == bool_node::PLUS && (isConst(node.mother->mother) || isConst(node.mother->father)  )){
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
		rvalue = optAdd(as);
		return;
	}
	
	if(node.multi_mother[0]->type == bool_node::EQ){
		bool_node* eqmm0 = node.multi_mother[0];
		bool dochange = false;
		if(eqmm0->mother == node.mother){
			if(eqmm0->father->type == bool_node::CONST){
				if(this->getIval(eqmm0->father)==node.quant){
					dochange = true;
		}	}	}
		if(eqmm0->father == node.mother){
			if(eqmm0->mother->type == bool_node::CONST){
				if(this->getIval(eqmm0->mother)==node.quant){
					dochange = true;
		}	}	}

		if(dochange){
			ARRASS_node* as = new ARRASS_node();
			as->mother = node.mother;
			as->quant = node.quant;
			as->multi_mother = node.multi_mother;
			as->multi_mother[0] = this->getCnode(0);
			as->addToParents();
			rvalue = optAdd(as);			
			return;
		}

	}
	
	if(isConst( node.multi_mother[0] ) && isConst( node.multi_mother[1] )){
       
		int m0 = getIval(node.multi_mother[0]);
		int m1 = getIval(node.multi_mother[1]);
        if(m0 == 0 && m1==1){
			EQ_node* en = new EQ_node();
			en->mother = node.mother;
			en->father = getCnode(node.quant);
			en->addToParents();
            rvalue = optAdd(en);
			
			return;
		}
		if(m0 == 1 && m1==0){
			EQ_node* en = new EQ_node();
			en->mother = node.mother;
			en->father = getCnode(node.quant);
			en->addToParents();
           
			rvalue = optAdd(en);			

			NOT_node* nn = new NOT_node();
			nn->mother = rvalue;
			nn->addToParents();
			rvalue = optAdd(nn);	
			return;
		}
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

char tbuf[40];
char* toString(TempTriple& tmp, int& len){
	int p=0;
	for(int i=0; i<3; i=i+2){
		if(tmp.f[i]){
			writeInt(tbuf, (tmp.bn[i]->globalId<<1), p);
		}else{
			writeInt(tbuf, (tmp.bn[i]->globalId<<1) | 1, p);
		}
		tbuf[p] = ','; ++p;
	}
	tbuf[p] = 0;
	len = p;
	return tbuf;
}
	
void DagOptim::visit( ASSERT_node &node){

	if(isConst(node.mother)){
		int cv = this->getIval(node.mother);
		if(cv == 1){
			rvalue = this->getCnode(0);
			return ;
		}
	}
	
	if(!node.isNormal()){
		//We should clear the callMap after an assume to prevent merging calls from before Assume with calls after Assume. 
		//otherwise this may reorder assumes and asserts and change the semantics of the program.
		callMap.clear();
		rvalue = &node;
		return;
	}

	if(node.mother->type == bool_node::OR){
		TempTriple* nt = triples.newObj();
		TempTriple& tmp = *nt;
		bool good = false;
		bool_node* ornode = node.mother;
		if(ornode->mother->type == bool_node::NOT){			
			tmp.add(0, ornode->mother->mother, true);
		}else{
			tmp.add(0, ornode->mother, false);
		}
		if(ornode->father->type == bool_node::NOT && ornode->father->mother->type == bool_node::AND){
			bool_node* andnode = ornode->father->mother;
			if(andnode->mother->type == bool_node::NOT){
				tmp.add(1, andnode->mother->mother, false);
			}else{
				tmp.add(1, andnode->mother, true);
			}
			if(andnode->father->type == bool_node::NOT){
				tmp.add(2, andnode->father->mother, false);
			}else{
				tmp.add(2, andnode->father, true);
			}
			good = true;
		}else if(ornode->father->type == bool_node::OR){
			bool_node* oonode = ornode->father;
			if(oonode->mother->type == bool_node::NOT){
				tmp.add(1, oonode->mother->mother, true);
			}else{
				tmp.add(1, oonode->mother, false);
			}
			if(oonode->father->type == bool_node::NOT){
				tmp.add(2, oonode->father->mother, true);
			}else{
				tmp.add(2, oonode->father, false);
			}
			good = true;
		}
		if(good){
			int len;
			char* tt = toString(tmp, len);
			TempTriple* ttrip;			 
			if(testAsserts.condAdd(tt, len, nt, ttrip)){
				TempTriple& ttri = *ttrip;
				if(ttri.bn[1] == tmp.bn[1] && ttri.f[1] != tmp.f[1]){
					if(!ttri.hasModified){
						bool_node* on = new OR_node();
						on ->mother = ornode->mother;
						if(ttri.f[2]){
							on->father = new NOT_node();
							on->father->mother = ttri.bn[2];
							on->father->addToParents();
							on->father = optAdd(on->father);
						}else{
							on->father = ttri.bn[2];
						}
						on->addToParents();
						on = optAdd(on);
						ttri.hasModified = true;
						ttri.main->dislodge();
						ttri.main->mother = on;
						ttri.main->addToParents();
						string msg = ttri.main->getMsg();
						msg += " or ";
						msg += node.getMsg();
						ttri.main->setMsg(msg);
						rvalue = ttri.main;
						return;
					}else{
						rvalue = getCnode(0);
						return;
					}
				}else{
					if(ttri.bn[1]->type == bool_node::LT && tmp.bn[1]->type == bool_node::LT){
						if(ttri.bn[1]->father == tmp.bn[1]->father){
							if( isConst(ttri.bn[1]->mother) && isConst(tmp.bn[1]->mother) ){
								if(ttri.f[1] && tmp.f[1] && getIval(ttri.bn[1]->mother) <= getIval(tmp.bn[1]->mother)){
								// assert u | !(a<x) | v
								// assert u | !(b<x) | v
								// if (a <= b) then the second assertion is unnecessary.
									rvalue = getCnode(0);
									return;
								}
							}
						}
					}
				}
			}else{
				tmp.main = &node;				
				//cout<<tmp.bn[0]->id<<", "<<", "<<tmp.bn[1]->id<<", "<<tmp.bn[2]->id<<endl;
			}
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
	testAsserts.clear();
   /* // Reset this so that the remaining nodes in the map can be optimized as before.
    for(map<int, UFUN_node*>::iterator it =combinedFunCallMap.begin();
        it != combinedFunCallMap.end(); ++it) {
        UFUN_node* node = it->second;
        node->combined = false;
    }*/
    combinedFunCallMap.clear();
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

bool_node* DagOptim::process( UFUN_node* node ){
    
     int fgid = node->fgid;
     if (fgid != 0) {
        if (combinedFunCallMap.count(fgid) > 0) {
             UFUN_node* brother = combinedFunCallMap[fgid];
             brother->dislodge();
             brother->mother->remove_child(brother);
             bool_node* on = new OR_node();
             on->mother = brother->mother;
             on->father = node->mother;
             on->addToParents();
             on = optAdd(on);
             brother->mother = on;
             int size = brother->multi_mother.size();
             int size1 = node->multi_mother.size();
             Assert(size == size1, "Size of inputs of both nodes should be equal");
             
             for (int i = 0; i < size; i++) {
                 if (brother->multi_mother[i] != node->multi_mother[i]) {
                     ARRACC_node* inputNode = new ARRACC_node();
                     inputNode->mother = node->mother;
                     inputNode->multi_mother.push_back(brother->multi_mother[i]);
                     inputNode->multi_mother.push_back(node->multi_mother[i]);
                 
                     inputNode->addToParents();
                     bool_node* optInputNode = optAdd(inputNode);
                 
                 
                     brother->multi_mother[i] = optInputNode;
                 }
             }
             brother->resetId();
             brother->addToParents();
             brother->fgid = 0;
             return brother;
         
         } else {
            /* node->dislodge();
             node->mother->remove_child(node);
             int size = node->multi_mother.size();
             for (int i = 0; i < size; i++) {
                 ARRACC_node* inputNode = new ARRACC_node();
                 inputNode->mother = node->mother;
                 inputNode->multi_mother.push_back(getCnode(0));
                 inputNode->multi_mother.push_back(node->multi_mother[i]);
                 inputNode->addToParents();
                 addNode(inputNode);
                 node->multi_mother[i] = inputNode;
             }
             node->resetId();
             node->addToParents();
             */
             combinedFunCallMap[fgid] = node;
             node->fgid = 0;
             
             return node;
         }
     }
    return node;
}

void DagOptim::combineFunCalls(BooleanDAG& dag) {
    combinedFunCallMap.clear();
    for(int i=0; i<dag.size() ; ++i ){
		if(dag[i] != NULL){
            if (dag[i]->type == bool_node::UFUN) {
                bool_node* node = process(dynamic_cast<UFUN_node*>(dag[i]));
                
                if(dag[i] != node){
                    //cout<<"Replacing "<<dag[i]->lprint()<<" with "<<node->lprint()<<endl;
                    Dout(cout<<"Replacing ["<<dag[i]->globalId<<"] "<<dag[i]->id<<" with ["<<node->globalId<<"] "<<node->id<<endl);
                    dag.replace(i, node);
                }else{
                    //cout<<"Kept      "<<dag[i]->lprint()<<endl;
                }
            }
		}
	}
    
	cleanup(dag);
    //dag.lprint(cout);
    
}

bool_node* DagOptim::computeCSE(bool_node* node){
	return cse.computeCSE(node);
}

bool_node* DagOptim::computeOptim(bool_node* node){
    node->accept(*this);
	node = rvalue;
	bool_node* tmp = node;
   if(node->type != bool_node::UFUN){
	   //if it is ufun, accept already called computeCSE.
		tmp = cse.computeCSE(node);
   }
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


bool DagOptim::checkTempcreators(BooleanDAG& dag, bool_node* node, int i){	
			bool_node* cur = node;
			int count = tempcreators[i].second.multi_mother.size();
			bool replace = true;
			cur = dynamic_cast<ARR_W_node*>(cur)->getOldArr();
			for(int t=1; t< count; ++t){
				if(cur->type == bool_node::ARR_W && cur->children.size() == 1){
					cur = dynamic_cast<ARR_W_node*>(cur)->getOldArr();
				}else{
					replace = false;
					break;
				}
			}
			if(replace){
				bool_node* acn = tempcreators[i].second.clone(false);
				acn->addToParents();
				int id = node->id;
				dag.replace(id, acn);
				dag[id] = acn;				
				return true;
			}
			return false;
}

void DagOptim::cleanup(BooleanDAG& dag){

	dag.removeNullNodes();
	dag.addNewNodes(newnodes);
	newnodes.clear();
	dag.relabel();

	if(nccount > 0){
		bool modified = false;
		int size = dag.size();
		for(int i=0; i<size; ++i){
			if(dag[i]->type == bool_node::ARR_W){
				for(int j=0; j<nccount; ++j){
					int id = tempcreators[j].first;
					if(dag[i] == NULL || dag[i]->globalId != id){ continue; }
					modified = modified || checkTempcreators(dag, dag[i], j);
				}
			}
		}		
		
		nccount = 0;
	}

	

	if(possibleCycles){ 
		findCycles(dag);
		possibleCycles = false;
	}

	dag.removeNullNodes();
	dag.addNewNodes(newnodes);
	//dag.repOK();
	newnodes.clear();
	//dag.sort_graph();
	//dag.print(cout);
	dag.cleanup();	
	// dag.relabel();
#ifdef _DEBUG
	dag.repOK();
#endif	
	// dag.lprint(cout);
}

/*The goal of this routine is to break any cycles 
that were introduced by merging UFUNs. For example, consider
the following code:
t = f(x);
if(t){
  y = f(x);
}
In this case, both calls to f can be merged into one. However, 
when we do that, the path condition for the first call will
get or-ed with the path condition for the second one, causing
a cycle in the DAG. 
However, that cycle is really a fake cycle. Consider two 
calls to a function:
t1=f(x)[p1] and t2=f(x)[p2]
both have the same input, but one of them has path condition p1
and one has path condition p2. 
Both calls are going to get merged into t1=f(x)[p1 or p2]
Now, suppose p2 is actually of 
the form p2=F(t1, q); I argue that I can replace p2 with 
p2'=F(0, q); and that it won't make any difference. If this is
true, that solves the problem because now the cycle will be broken.

The reason why the replacement works is as follows. Consider the cases:
First, in the case where p1 is true, then the value of p2 doesn't matter, 
so replacing p2 with p2' makes no difference. Now, consider what 
happens when p1 is false. One invariant in the DAG is that if
the path condition to a function is false, then the value of its output
has no effect on the computation, so replacing t1 with zero should
make no difference. So either way, the replacement is ok.

The reasoning above doesn't quite generalize to more complex
cycles. When the cycles get complicated, the solution is to have 
two versions of the function; one where we care only about its
output and ignore any asserts in it, and one where we only care 
about the asserts. The routine below finds the best places to break cycles.

	*/
void DagOptim::findCycles(BooleanDAG& dag){
	for(int i=0; i<dag.size() ; ++i ){
			// Get the code for this node.				
		if(dag[i]->type == bool_node::UFUN){
			UFUN_node& uf = *dynamic_cast<UFUN_node*>(dag[i]);
			//uidcount = max(uidcount, uf.fgid);
		}
	}
	map<int, UFUN_node*> dupNodes;
	stack<pair<bool_node*, childset::iterator> > bns;
	vector<bool_node*> acreates;

	for(int i=0; i<dag.size(); ++i){
		dag[i]->flag = BOTTOM;
		if(dag[i]->type == bool_node::ARR_CREATE){
			acreates.push_back(dag[i]);
		}
	}
	{
		vector<bool_node*>& ins = dag.getNodesByType(bool_node::SRC);
		for(int i=0; i<ins.size(); ++i){		
			cbPerNode(ins[i], bns, dupNodes); // do depth first search starting from the sources.
		}
	}
	{
		vector<bool_node*>& ins = dag.getNodesByType(bool_node::CTRL);
		for(int i=0; i<ins.size(); ++i){		
			cbPerNode(ins[i], bns, dupNodes); // do depth first search starting from the controls.
		}
	}

	for(map<long long int, CONST_node*>::iterator it = this->cnmap.begin(); it != this->cnmap.end(); ++it){
		cbPerNode(it->second, bns, dupNodes); // do DFS starting from constant nodes.
	}
	for(int i=0; i<acreates.size(); ++i){
		cbPerNode(acreates[i], bns, dupNodes); // do depth first search starting from the array creators.
	}
#ifdef _DEBUG
	for(int i=0; i<dag.size(); ++i){
		Assert(dag[i]->flag != BOTTOM, "This is a big mistake!");
	}
#endif
}

/*
The function below uses a stack to do depth first search to find cycles.
When it finds a cycle, it calls the breakCycle routine to break it and 
then restarts the DFS from the place where the cycle was broken.

All nodes are initialized to flag==BOTTOM. When a node is first visited by DFS, its flag is changed to INSTACK.
The node is then pushed into the stack, and the stack also keeps track of which of its children we are currently visiting
as part of DFS.

If as part of DFS we find a node that is currently in the stack, that means we found a cycle and we have to break it.
*/
void DagOptim::cbPerNode(bool_node* cur, stack<pair<bool_node*, childset::iterator> >& bns, map<int, UFUN_node*>& dupNodes){
	if(cur->flag ==BOTTOM){
			bool_node* n = cur;
			bns.push(make_pair(n, n->children.begin()));
			n->flag = INSTACK;
			while(!bns.empty()){	
				n = bns.top().first;
				childset::iterator& it = bns.top().second;
				if(!n->children.checkIter(it)){//if the node got modified, the childrens list will change
					it = n->children.begin(); // and the iterator will be corrupted so we have to reset it to begin.
				}
				bool esc = false;
				for(; it != n->children.end(); ++it){
					if((*it)->flag == INSTACK){
						// found cycle.
						breakCycle((*it), bns, dupNodes);
						bns.top().second = bns.top().first->children.begin();																		
						esc = true;
						break;
					}
					if((*it)->flag == BOTTOM){
						bns.push(make_pair((*it), (*it)->children.begin()));
						(*it)->flag = INSTACK;
						break;
					}
				}
				if(!esc && it == n->children.end()){
					n->flag = DONE;
					bns.pop();
				}
			}
		}
}


/*
When this function is called, s is the DFS stack and it looks like this:

a->b->c->d->e->f->g->h

where one of h's children is a node in s (say c), and that node c is equal to bn.

The map dupNodes stores nodes stores nodes for which a duplicate has been made.

One by one, we pop nodes from the stack and push them to a temporary list sp.
We can only break cycles in function nodes, and specifically cycles caused by the 
guard of a function node, so we are looking for a node in the stack that is a function
node. In this case, suppose f is a function node and e is f's guard.

At the end of the while loop, the state will look like this:

s = a->b
sp = f->g->h
tst = e -> d -> c

All the nodes that have been popped from s are now set to bottom.

*/


void DagOptim::breakCycle(bool_node* bn, stack<pair<bool_node*, childset::iterator> >& s, map<int, UFUN_node*>& dupNodes){	
	int BOTTOM=-1;
	list<pair<bool_node*, childset::iterator> > sp;
	stack<pair<bool_node*, childset::iterator> > tst;
	bool good = false;
	
	UFUN_node* luf = NULL;	
	while(s.top().first != bn){	
		s.top().first->flag = BOTTOM;
		Assert(!s.empty(), "njkflaiuy");
		luf = dynamic_cast<UFUN_node*>(s.top().first);
		sp.push_front( s.top() );		
		s.pop();
		if(luf != NULL){
			if(s.top().first == luf->mother /*&& (luf->mother->type == bool_node::OR || dupNodes.count(luf->globalId) )*/){
				//Found a point to break the cycle. 
				while(s.top().first != bn){											
					s.top().first->flag = BOTTOM;
					tst.push(s.top());
					s.pop();
					Assert(!s.empty(), "zmiyeoiaujn");
				}		
				tst.push(s.top());
				s.top().first->flag = BOTTOM;				
				s.pop();
				break;
			}
		}
	}
	/*
	Let's say the original stack was
	a->b->c->d->e->f->g->h
	At this point we have 
	s = a->b
	sp = f->g->h
	tst = e -> d->c

	All the nodes that have been popped from s are now set to bottom.

	*/


	if(s.size()>0 && s.top().first == bn){
		//This branch deals with the corner case where c and f are the same and tst is empty.
		Assert(tst.empty(), "Invariant");
		s.top().first->flag = BOTTOM;
		sp.push_front( s.top() );
		s.pop();
	}


	while(!tst.empty()){
		sp.push_back(tst.top());
		tst.pop();
	}
	/*
	After pushing tst back int sp, the state looks like this:
	s = a->b
	sp = f->g->h->c->d->e	
	At this point, s no longer matters. The important thing is that we have 
	a complete representation of the loop, with the top of sp corresponding to 
	the function where the loop will be broken.
	*/

	if(PARAMS->verbosity > 5){ cout<<"Found Cycle of size "<< sp.size()<<"; Breaking."<<endl; }
	// sp.front() is the function that is having trouble.
	{		
		bool_node* lastOr = sp.back().first;
		sp.pop_back();
		Assert(lastOr == sp.front().first->mother, "m;lqkey");
		/*
		bool_node* outer = NULL;
		bool_node* inner = NULL;
		if(lastOr->mother == sp.back().first){
			inner = lastOr->mother;
			outer = lastOr->father;
		}else{
			Assert(lastOr->father == sp.back().first, "Kokjne;");
			inner = lastOr->father;
			outer = lastOr->mother;
		}
		*/
		UFUN_node* oldNode = dynamic_cast<UFUN_node*>(sp.front().first);
		// cout<<"double: "<<oldNode->get_name()<<endl;
		UFUN_node* newNode = NULL;
		bool isRecycled = false;
		//int oldfgid = -1;
		if(dupNodes.count(oldNode->globalId)>0){
			newNode = dupNodes[oldNode->globalId];
			isRecycled = true;
		}else{
			newNode = dynamic_cast<UFUN_node*>(oldNode->clone(false));
			newNode->ignoreAsserts = true;
			newnodes.push_back(newNode);
			//oldNode->replace_parent(lastOr, inner);
			//lastOr->remove_child(oldNode);
			newNode->replace_parent(lastOr, this->getCnode(true));
			dupNodes[oldNode->globalId] = newNode;
			newNode->addToParents();
			//It's important that nodes don't have duplicate fgid's.
			//++uidcount;
			//oldNode->fgid = uidcount;
		}		
		sp.pop_front();		
		

		for(childset::iterator it = oldNode->children.begin(); it != oldNode->children.end(); ++it){
			(*it)->replace_parent(oldNode, newNode);
			/*if(!isRecycled){
				if((*it)->type==bool_node::UFUN){
					UFUN_node* un = dynamic_cast<UFUN_node*>(*it);
					if(un->fgid == newNode->fgid){
						//Assert(un->mother == lastOr, "I don't believe this!! What's going on here?");
						un->replace_parent(lastOr, this->getCnode(true));
						lastOr->remove_child(un);
						un->ignoreAsserts = true;
					}					
				}
			}*/	
		}
		oldNode->children.clear();

		/*
		//At this point the only potential problem is that oldNode may be out of place in the DLLlist, because it is in the position of the first
		//occcurence of the function, but other functions may have flowed to the last occurence of the function.
		//We have decided to fis this later during dag cleanup. 
		if(!isRecycled){
			Dllist* dl = oldNode->parent;
			if(dl->tail != oldNode){
				oldNode->remove();
				//The solution is simple; to avoid further problems, just move it to the end.
				dl->tail->addBefore(oldNode);
			}
		}
		*/

		/*
		for(list<pair<bool_node*, childset::iterator> >::iterator it = sp.begin(); it != sp.end(); ++it){
			if(oldNode->children.count(it->first)>0){
				it->first->replace_parent(oldNode, newNode);
				oldNode->remove_child(it->first);
			}
		}
		*/
		
		//oldNode->remove_child(sp.front().first);
		UFUN_node* puf = dynamic_cast<UFUN_node*>(sp.back().first);
		while(puf == NULL){
			sp.pop_back();
			if(sp.empty()){ break; }
			puf = dynamic_cast<UFUN_node*>(sp.back().first);
		}
		/*
		if(!isRecycled){
			oldNode->addBefore(newNode);
		}
		if(puf != NULL){
			oldNode->remove();
			puf->add(oldNode);
		}
		*/
	}
}


void DagOptim::process(BooleanDAG& dag){
	timerclass everything("everything");


	everything.start();
	
	
	int k=0;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				

		if(dag[i] != NULL){
            // cout<<"Orig = "<<dag[i]->lprint();
			bool_node* node = computeOptim(dag[i]);
			// cout<<"  becomes "<<node->lprint()<<endl;


			if(dag[i] != node){			
					Dout(cout<<"Replacing ["<<dag[i]->globalId<<"] "<<dag[i]->id<<" with ["<<node->globalId<<"] "<<node->id<<endl);
					dag.replace(i, node);					
			}else{
					//cout<<"Kept      "<<dag[i]->lprint()<<endl;
			}
		}
	}
    
	
	cleanup(dag);

	everything.stop();
	
	
	Dout( everything.print(); )
	
	Dout(cout<<" end cse "<<endl);
}
