#include "DagElimUFUN.h"


bool WITH_RESTRICTIONS ;













DagElimUFUN::DagElimUFUN()
{
	oneMoreFun = false;
}

DagElimUFUN::~DagElimUFUN()
{
}


void DagElimUFUN::stopProducingFuns(){
	oneMoreFun = true;
	Dout(cout<<" NO MORE NEW FUNCTIONS WILL BE PRODUCED "<<endl);	
}

BooleanDAG& DagElimUFUN::getComparator(int sz){
	Dout(cout<<" producing a comparator of size "<<sz<<endl );	    
	if( comparators.find(sz) != comparators.end()){
		BooleanDAG& argComp = comparators[sz];
		Dout( cout<<" comparator already there "<<endl );
		//Dout( argComp.print(cout) );
		return argComp;	
	}else{
		Dout( cout<<" comparator NOT already there "<<endl );		
		BooleanDAG& argComp = comparators[sz];
		bool_node* peq = NULL;
		int nargs = sz;
		for(int i=0; i<nargs; ++i){
			string ina;
			string inb;
			{
				stringstream str;
				str<<"ina_"<<i;
				ina = str.str();
				argComp.create_inputs(nargs, ina);
			}
			{
				stringstream str;
				str<<"inb_"<<i;
				inb = str.str();
				argComp.create_inputs(nargs, inb);
			}
			EQ_node* eq = new EQ_node();
			eq->name = argComp.new_name();
			argComp.new_node(ina, inb, eq);
			if(peq != NULL){
				peq = argComp.new_node(peq->name, eq->name , bool_node::AND,argComp.new_name() );
			}else{
				peq = eq;
			}
		}
		
		peq = argComp.new_node(peq->name, "" , bool_node::DST, "OUT");
		argComp.sort_graph();		
		argComp.relabel();
		// Dout( argComp.print(cout) );
		return argComp;
	}	
}


void DagElimUFUN::visit( SRC_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( CTRL_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( CONST_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( AND_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( OR_node& node ){
	rvalue = &node;
}


void DagElimUFUN::visit( XOR_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( NOT_node& node ){
	rvalue = &node;
}


void DagElimUFUN::visit( PLUS_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( TIMES_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( DIV_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( MOD_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( NEG_node& node ){
	rvalue = &node;
}
	
void DagElimUFUN::visit( GT_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( GE_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( LT_node& node ){
	rvalue = &node;
}

void DagElimUFUN::visit( LE_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( EQ_node& node ){
	rvalue = &node;
}


bool_node* DagElimUFUN::produceNextSFunInfo( UFUN_node& node  ){
	bool_node* rv = NULL;
	
	string& name = node.get_ufname();
	Dout( cout<<"Replacing call to function "<< node.get_ufname() <<endl );
	int nargs = node.multi_mother.size();
	if( functions.find(name) == functions.end() ){
		//This means this is the first time we see
		//this function, so the function just outputs its symvalue.
		//The symvalue has to be created first, though.
		Dout( cout<<" First time for the function"<<endl );
		stringstream str;
		str<< node.get_ufname() <<"_0";
		SRC_node* src =  new SRC_node( str.str() );
		src->set_nbits( node.get_nbits() );
		newnodes.push_back( src );
		rv = src;
		
		//Now, we have to build the first SFunInfo for the node.
				
		SFunInfo& sfi = functions[name];		
		sfi.step = 1;
		sfi.fun = new BooleanDAG();
		sfi.fun->new_node("", "" , src->type, "SVAR"  );
		
		sfi.symval = src;
		sfi.outval = rv;
		for(int i=0; i<nargs; ++i){
			stringstream str;
			str<<"PARAM_"<<i;
			bool_node* tmp = sfi.fun->new_node("", "", bool_node::SRC, str.str() );
			sfi.actuals.push_back(node.multi_mother[i]);	
		}
		sfi.fun->new_node("SVAR", "", bool_node::DST, "OUT"  );
		// sfi.fun->sort_graph();		
		// sfi.fun->relabel();
		// Dout(  sfi.fun->print(cout) );
	}else{
		BooleanDAG& comp = getComparator(node.multi_mother.size());
		
		Dout( cout<<" before clone "<<endl);
		//As a first step, we get a comparator for the parameters, and we replace the 
		//inb parameters with the actuals from the previous version of the function, encoded in the SFunInfo.		
		BooleanDAG* cclone = comp.clone();
		// Dout( cclone->print(cout) );
		Dout( cout<<" after clone "<<endl);		
		SFunInfo& sfi = functions[name];				
		for(int i=0; i< nargs ; ++i){
			stringstream str;
			str<<"inb_"<<i;
			bool_node* tt = cclone->get_node(str.str());
			Dout( cout<<" replacing "<<tt->get_name()<<"  with "<<sfi.actuals[i]->get_name()<<endl );
			Dout( cout<<" tt->children.size()="<<tt->children.size()<<endl );
			Assert( (*cclone)[tt->id] == tt , "Thid is an error. The id should be the position in the array. "<<
											  tt->get_name()<<":"<<tt->id<<" vs "<<(*cclone)[tt->id]->get_name()<<":"<<
											  (*cclone)[tt->id]->id<<endl);
			cclone->replace(tt->id, sfi.actuals[i]);
			//As we do this, we also update the actuals in sfi.
			sfi.actuals[i] = node.multi_mother[i];
		}
		// Dout( cclone->print(cout) );
		Dout( cout<<" after replacing inputs "<<endl);	
		bool_node* dst = cclone->get_node("OUT");
		Assert( dst != NULL && dst->type == bool_node::DST, "This can't happen!asdpoiuy");  
		
		//The output of node of the comparator is removed, and instead, the output of the
		//comparisson is fed into a mux that selects between the current symvalue and the previous symvalue.
		
		
		ARRACC_node* ch = new ARRACC_node();				
		ch->mother = dst->mother;
		
		stringstream str;
		str<< node.get_ufname() <<"_"<<sfi.step;
		string curParamName = str.str() ;
		SRC_node* src =  new SRC_node( curParamName);
		src->set_nbits( node.get_nbits() );
		sfi.step++;				
		ch->multi_mother.push_back(src);				
		ch->multi_mother.push_back(sfi.symval);
		ch->addToParents();

		Dout(cout<<" After addToParents "<<endl);
		
		cclone->addNewNode(src);
		
		Dout( cout<<" After raddNewNode src"<<endl  );
		
		cclone->addNewNode(ch);
		Dout( cout<<" After raddNewNode ch"<<endl  );
		Assert( (*cclone)[dst->id]==dst, "This is not correct. ");
		cclone->replace(dst->id, ch);	
		dst=NULL;
		Dout( cout<<" After replacement "<<endl  );
		// Dout( cclone->print(cout) );
		Dout( cout<<" After adding mux "<<endl  );
		//Now, sfi has a copy of the function from the last appearence, so we take this function, and we replace the 
		//params with the current arguments.	
		Assert(sfi.fun != NULL, "This is ridiculos, how could this ever happen!"<<endl);	
		
		
		sfi.fun->resetBackPointers();		
		Dout( cout<<" After rbp "<<endl  );
		//Dout( sfi.fun->print(cout) );
				
		for(int i=0; i<nargs; ++i){
			
			stringstream str1;
			str1<<"PARAM_"<<i;
			bool_node* inarg = sfi.fun->get_node( str1.str() );
			Assert(inarg != NULL, "This can't be happening!!!");
		
			
			stringstream str2;
			str2<<"ina_"<<i;
			bool_node* tt = cclone->get_node(str2.str());
			
			sfi.fun->replace(inarg->id, tt);
		}
		// Dout( sfi.fun->print(cout) );
		Dout(cout<<" After replacing PARAMs"<<endl);
		
		//The input SVAR is also replaced with the output of the MUX.
		{
			bool_node* svar = sfi.fun->get_node("SVAR" );
			Assert(svar != NULL, "This can't be happening!!!");
			sfi.fun->replace(svar->id,ch);
		}
		
		Dout( sfi.fun->print(cout) );
		Dout(cout<<" After replacing SVAR"<<endl);
				
		bool_node* outn = sfi.fun->get_node("OUT" );		
		rv = outn->mother;
		
		
		
		///////////////////////////////////
		/**
		 * This set of gates is completely redundant, and can be safely 
		 * left out. They are here because they should make propagation faster. 
		 * This is a hypothesis, and needs to be tested out.
		 */
		
		if(false){ //This code is incorrect. Needs to be fixed.
			EQ_node* symEq = new EQ_node();
			symEq->mother = src;
			symEq->father = sfi.symval; 
			symEq->addToParents();
			
			OR_node* or1 = new OR_node();
			or1->mother = symEq;
			or1->father = ch->mother;
			or1->addToParents();
			
			EQ_node* outEq = new EQ_node();
			outEq->mother = sfi.outval;
			outEq->father = rv;
			outEq->addToParents();
			
			NOT_node* notNode = new NOT_node();
			notNode->mother = outEq;
			notNode->addToParents();
			
			OR_node* or2 = new OR_node();
			or2->mother = notNode;
			or2->father = or1;
			or2->addToParents();
			
			ASSERT_node* asrtNode = new ASSERT_node();
			asrtNode->mother = or2;
			asrtNode->addToParents();
			asrtNode->makeHardAssert();
			
			cclone->addNewNode(symEq);	
			cclone->addNewNode(or1);
			cclone->addNewNode(outEq);
			cclone->addNewNode(notNode);
			cclone->addNewNode(or2);
			cclone->addNewNode(asrtNode);	
			cout<<"=========================== FOR FUN "<<node.get_name()<<endl;
			asrtNode->printSubDAG(cout);	
			cout<<"=========================== END FOR FUN "<<node.get_name()<<endl;
		}
		//////////////////////////////////
		
		
		
		sfi.outval = rv;
		//The previous symvalue is replaced with the current one.
		sfi.symval = src;
		
		
		
		
		for(int i=0; i<sfi.fun->size(); ++i){
			bool_node* n = (*sfi.fun)[i];
			if( n != NULL &&  n->type != bool_node::DST ){
				cclone->addNewNode(n);	
			}else{
				Assert( n==NULL || n == outn, "I thought this was going to be the only DST node");
				if( n!= NULL){
					n->dislodge();
					delete n;	
				}
			}
		}
		
		// Dout( cclone->print(cout) );
		Dout( cout<<" Almost fully integrated cclone. "<<endl  );
		delete sfi.fun;
		cclone->removeNullNodes();
		cclone->relabel();
		sfi.fun = cclone->clone();
		
		
		for(int i=0; i< nargs ; ++i){
			stringstream str;
			str<<"ina_"<<i;
			{
				bool_node* tt = cclone->get_node(str.str());
				cclone->replace(tt->id, node.multi_mother[i]);
			}
			
			stringstream parnm;
			parnm<<"PARAM_"<<i;
			bool_node* par =  new SRC_node( parnm.str() );	
			sfi.fun->new_node(NULL, NULL, par);		
			
			bool_node* npar = sfi.fun->get_node(str.str());
			sfi.fun->replace(npar->id, par);
		}
		
		// Dout( cclone->print(cout) );
		Dout( cout<<"fully integrated cclone. "<<endl  );
		
		{
			bool_node* tmpbn = sfi.fun->get_node(curParamName);
			Assert(tmpbn != NULL, "This is an abomination.");		
			bool_node* nsvar = sfi.fun->new_node("", "" , bool_node::SRC, "SVAR"  );
			Dout( cout<<" replacing "<<tmpbn->get_name()<<":ch="<< tmpbn->children.size()  <<" with "<< nsvar->get_name() <<endl);
			sfi.fun->replace(tmpbn->id, nsvar  );
		}
		
		DST_node* dstn = new DST_node();
		dstn->name = "OUT";
		sfi.fun->new_node((*sfi.fun)[rv->id] , NULL, dstn  );
		
				
		Dout( cout<<" ADDING "<<cclone->size()<<" NODES"<<endl );
		newnodes.insert(newnodes.end(), cclone->begin(), cclone->end());
		sfi.fun->removeNullNodes();
		sfi.fun->relabel();
		// Dout(  sfi.fun->print(cout) );
		Dout( cout<<" DONE DONE DONE"<<endl );
	}	
	return rv;
}


void DagElimUFUN::visit( UFUN_node& node ){
	string& name = node.get_ufname();
	if(( functions.find(name) == functions.end()) || functions[name].moreNewFuns ){	
		rvalue = produceNextSFunInfo( node  );
	}else{
		Dout( cout<<"Replacing call to function "<< node.get_ufname() <<endl );
		int nargs = node.multi_mother.size();
		SFunInfo& sfi = functions[name];
		BooleanDAG* cclone = sfi.fun->clone();
		for(int i=0; i<nargs; ++i){
			stringstream str1;
			str1<<"PARAM_"<<i;
			bool_node* inarg = sfi.fun->get_node( str1.str() );
			Assert(inarg != NULL, "This can't be happening!!!");
			cclone->replace(inarg->id, node.multi_mother[i]);
		}
		
		bool_node* src = NULL;
		if(true){
			src =  new CONST_node(0);			
		}else{
			stringstream str;
			str<< node.get_ufname() <<"_"<<sfi.step;
			string curParamName = str.str() ;		
			src =  new SRC_node( curParamName);
			dynamic_cast<SRC_node*>(src)->set_nbits( node.get_nbits() );
		}
		sfi.step++;		
		cclone->addNewNode(src);		
		Dout(cout<<" Adding parameter "<<src->get_name()<<endl);
		
		{
			bool_node* svar = cclone->get_node("SVAR" );
			Assert(svar != NULL, "This can't be happening!!!");
			Assert( (*cclone)[svar->id] == svar, "The indexing is incorrect!!"); 
			cclone->replace(svar->id,src);
		}
		
		
		// Dout( cclone->print(cout); );
		Dout( cout<<" FUNCTION CLONE TO BE ADDED "<<endl);
		
		bool_node* outn = cclone->get_node("OUT" );		
		rvalue = outn->mother;
		
		int oldsize = newnodes.size();
		for(int i=0; i<cclone->size(); ++i){
			bool_node* n = (*cclone)[i];
			if( n != NULL &&  n->type != bool_node::DST ){
				newnodes.push_back(n);	
			}else{
				Assert( n==NULL || n == outn, "I thought this was going to be the only DST node "<<n->get_name()<<" != "<<outn->get_name());
				if( n!= NULL){
					n->dislodge();
					delete n;	
				}
			}
		}
		
		
		
		t_node tn(src);
		tnbuilder.tn_build(src, NULL, &tn);
		
		t_node tn2(rvalue);
		tnbuilder.tn_build(&node, NULL, &tn2);
		
		
		bool_node* cur = NULL;
		
		if(tn.children.size() > 0){
			cur = tn.childDisjunct(newnodes);	
		}
		
		if(tn2.children.size() > 0){
			if(cur == NULL){
				cur = tn2.childDisjunct(newnodes);
			}else{
				AND_node* anode = new AND_node();
				anode->mother = cur;
				anode->father = tn2.childDisjunct(newnodes);
				anode->addToParents();
				newnodes.push_back(anode);
				cur = anode;					
			}
		}
		
		if( cur != NULL){
			NOT_node* nn = new NOT_node();
			nn->mother = cur;
			nn->addToParents();
			newnodes.push_back(nn);
			
			ASSERT_node* asn = new ASSERT_node();
			asn->mother = nn;
			asn->addToParents();
			newnodes.push_back(asn);
		}		
		
		//tn.print(cout);
		//tn2.print(cout);
		
		Dout( cout<<" ADDING "<<(newnodes.size()-oldsize)<<" NODES"<<endl );
	}	
	if(oneMoreFun){
		functions[name].moreNewFuns = false;
	}
}

void DagElimUFUN::visit( ARRACC_node& node ){
	rvalue = &node;
}
void DagElimUFUN::visit( ARRASS_node& node ){	
	rvalue = &node;	
}
void DagElimUFUN::visit( ACTRL_node& node ){
	rvalue = &node;
}
	
void DagElimUFUN::visit( ASSERT_node &node){
	rvalue = &node;
}
void DagElimUFUN::visit( DST_node& node ){
	rvalue = &node;
}



void DagElimUFUN::process(BooleanDAG& dag){
	dagsize = dag.size();	
	int k=0;
	// Dout( dag.print(cout) );	
	Dout( cout<<" BEFORE PROCESS "<<endl );
	for(int i=0; i<dag.size(); ++i ){
		// Get the code for this node. 
		dag[i]->accept(*this);
		bool_node* node = rvalue;
		if( dag[i] != node ){
			Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
			dag.replace(i, node);
		}
	}
	dag.addNewNodes(newnodes);
	dag.addNewNodes( tnbuilder.store );
	tnbuilder.store.clear();
	newnodes.clear();
	dag.removeNullNodes();
	Dout( cout<<" AFTER PROCESS "<<endl );
	//Dout( dag.print(cout) );	
	Dout(cout<<" end ElimFun "<<endl);
}


