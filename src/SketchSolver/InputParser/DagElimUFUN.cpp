#include "DagElimUFUN.h"

#include "BooleanDAGCreator.h"


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
			bool_node* inaNode;
			bool_node* inbNode;
			{
				stringstream str;
				str<<"ina_"<<i;
				ina = str.str();
				inaNode = argComp.create_inputs(nargs, ina);
			}
			{
				stringstream str;
				str<<"inb_"<<i;
				inb = str.str();
				inbNode = argComp.create_inputs(nargs, inb);
			}
			bool_node* eq = argComp.new_node(inaNode, inbNode, bool_node::EQ);			
			if(peq != NULL){
				peq = argComp.new_node(peq, eq , bool_node::AND);
			}else{
				peq = eq;
			}
		}

		peq = argComp.create_outputs(nargs, peq, "OUT");
		argComp.sort_graph();		
		argComp.relabel();
		// Dout( argComp.print(cout) );
		return argComp;
	}	
}

/**
SFIfun( PARAM, SVAL){ return SVAL if PARAM is different to the params of all previous iterations }.
*/


bool_node* DagElimUFUN::produceNextSFunInfo( UFUN_node& node  ){
	bool_node* rv = NULL;
	
	const string& name = node.get_ufname();
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
		bool_node* svar = sfi.fun->create_inputs(src->get_nbits(),  "SVAR");
		
		sfi.symval = src;
		sfi.outval = rv;
		for(int i=0; i<nargs; ++i){
			stringstream str;
			str<<"PARAM_"<<i;
			sfi.fun->create_inputs(src->get_nbits(),  str.str());
			sfi.actuals.push_back(node.multi_mother[i]);	
		}
		sfi.fun->create_outputs(src->get_nbits(), svar, "OUT");

	}else{
		BooleanDAG& comp = getComparator(node.multi_mother.size());
		
		Dout( cout<<" before clone "<<endl);
		//As a first step, we get a comparator for the parameters, and we replace the 
		//inb parameters with the actuals from the previous version of the function, encoded in the SFunInfo.		
		BooleanDAG* cclone = comp.clone();
		// Dout( cclone->print(cout) );
		Dout( cout<<" after clone "<<endl);		
		SFunInfo& sfi = functions[name];	
		//This loop replaces the inb parameters in the comparission with the last set of inputs.
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
		//At this point, cclone compares ina with the last set of inputs. If they are the same, ch (which is the last node),
		//will produce the previous symval. Otherwise, it will produce the current symval.

		
		sfi.fun->resetBackPointers();		
		Dout( cout<<" After rbp "<<endl  );
		//Dout( sfi.fun->print(cout) );
		//Now, we take sfi.fun and we replace the input parameters with ina from cclone.
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
			bool_node* par =  sfi.fun->create_inputs(node.get_nbits(), parnm.str());
			
			bool_node* npar = sfi.fun->get_node(str.str());
			sfi.fun->replace(npar->id, par);
		}
		
		// Dout( cclone->print(cout) );
		Dout( cout<<"fully integrated cclone. "<<endl  );
		
		{
			bool_node* tmpbn = sfi.fun->get_node(curParamName);
			Assert(tmpbn != NULL, "This is an abomination.");		
			bool_node* nsvar = sfi.fun->create_inputs(node.get_nbits(), "SVAR");
			Dout( cout<<" replacing "<<tmpbn->get_name()<<":ch="<< tmpbn->children.size()  <<" with "<< nsvar->get_name() <<endl);
			sfi.fun->replace(tmpbn->id, nsvar  );
		}
		
		bool_node* dstn = sfi.fun->create_outputs(node.get_nbits(), (*sfi.fun)[rv->id], "OUT");
				
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
	const string& name = node.get_ufname();
	if(( functions.find(name) == functions.end()) || functions[name].moreNewFuns ){	
		rvalue = produceNextSFunInfo( node  );
	}else{
		( cout<<"Replacing call to function "<< node.get_ufname() <<" : "<<node.id<<endl );
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
		
		tnbuilder.ivisit = 0;
		bool_node* tn1 = tnbuilder.get_exe_cond(src, *this, false);
		bool_node* tn2 = node.mother; // tnbuilder.get_exe_cond(&node, *this, false);
		
		bool_node* cur = NULL;
		
		cur = tn1;
		
		if(tn2 != NULL){
			if(cur == NULL){
				cur = tn2;
			}else{
				AND_node* anode = new AND_node();
				anode->mother = cur;
				anode->father = tn2;
				anode->addToParents();
				newnodes.push_back(anode);
				cur = anode;					
			}
		}
		
		if( cur != NULL){
			NOT_node* nn = new NOT_node();
			nn->name = cur->name;
			nn->name += "NOTfa";
			nn->mother = cur;
			nn->addToParents();
			newnodes.push_back(nn);
			
			ASSERT_node* asn = new ASSERT_node();
			asn->mother = nn;
			asn->addToParents();
			string msg = name;
			msg += ": UFUN is being misused";
			asn->setMsg(msg);
			newnodes.push_back(asn);
		}
		
		
		Dout( cout<<" ADDING "<<(newnodes.size()-oldsize)<<" NODES"<<endl );
	}	
	if(oneMoreFun){
		functions[name].moreNewFuns = false;
	}
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
	newnodes.clear();
	dag.removeNullNodes();
	Dout( cout<<" AFTER PROCESS "<<endl );
	//Dout( dag.print(cout) );	
	Dout(cout<<" end ElimFun "<<endl);
}


