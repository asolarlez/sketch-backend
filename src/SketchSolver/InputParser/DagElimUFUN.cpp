#include "DagElimUFUN.h"

#include "BooleanDAGCreator.h"
#include "CommandLineArgs.h"


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
				inaNode = argComp.create_inputs(nargs, OutType::INT, ina);
			}
			{
				stringstream str;
				str<<"inb_"<<i;
				inb = str.str();
				inbNode = argComp.create_inputs(nargs, OutType::INT, inb);
			}
			bool_node* eq = argComp.new_node(inaNode, inbNode, bool_node::EQ);
			if(peq != NULL){
				peq = argComp.new_node(peq, eq , bool_node::AND);
			}else{
				peq = eq;
			}
		}
        
		peq = argComp.create_outputs(nargs, peq, "OUT");
		argComp.registerOutputs();
		argComp.cleanup();
		argComp.relabel();
		// Dout( argComp.print(cout) );
		return argComp;
	}
}


SRC_node* DagElimUFUN::srcNode(UFUN_node& node, int i){
	stringstream str;
    str<< node.get_ufname() <<"_"<<node.outname<<"_"<<i;
    SRC_node* src =  new SRC_node( str.str() );
    // BUGFIX: this is not wide enough! UFUN_node.nbits is either 1 or 2, set by InputParser
    // TODO xzl: confirm this bug. Confirmed by cout, need to confirm with asolar
    //src->set_nbits( node.get_nbits() );
    //cout << "DagElimUFUN: " << node.lprint() << " nbits=" << node.get_nbits() << " isArr=" << node.isArr() << endl;
    // BUGFIX: nbits must be 1 if original UFUN out is boolean type
    int nbits = node.get_nbits();
    if (nbits > 1) { nbits = PARAMS->NANGELICS; }
    src->set_nbits(nbits);
    //if(node.getOtype() == bool_node::INT_ARR || node.getOtype() == bool_node::BOOL_ARR){
    if(node.isArr()) {
        // TODO xzl: is this fix correct?
        // will this be used with angelic CTRL? see Issue #5 and DagFunctionInliner
        //int sz = PARAMS->angelic_arrsz;
        int sz = 1 << PARAMS->NINPUTS;
        //int sz = 1;
        //for(int i=0; i<PARAMS->NINPUTS; ++i){
        //	sz = sz *2;
        //}
        src->setArr(sz);
    }
    
    src->setTuple(node.getTupleName(), true);
    return src;
}

/**
 SFIfun( PARAM, SVAL){ return SVAL if PARAM is different to the params of all previous iterations }.
 */


bool_node* DagElimUFUN::produceNextSFunInfo( UFUN_node& node  ){
	bool_node* rv = NULL;
	
	string name = node.get_ufname();
	name += node.outname;
	Dout( cout<<"Replacing call to function "<< node.get_ufname() <<endl );
	int nargs = node.multi_mother.size();
	vector<bool_node*>* nmm = &node.multi_mother;
	/*if(node.dependent()){
		nargs = mothercache[node.fgid].size();
		nmm = &(mothercache[node.fgid]);
	}else{
		mothercache[node.fgid] = node.multi_mother;
	}*/
	vector<bool_node*>& nmmother = *nmm;
	if( functions.find(name) == functions.end() ){
		//This means this is the first time we see
		//this function, so the function just outputs its symvalue.
		//The symvalue has to be created first, though.
		Dout( cout<<" First time for the function"<<endl );
        
        
        SRC_node* src = srcNode(node, 0);
        
		newnodes.push_back( src );
		rv = src;
        
		//Now, we have to build the first SFunInfo for the node.
		// This first SFunInfo will be equal to:
		// F(SVAR, PARAM_0, PARAM_1, ..., PARAM_N) := SVAR;
		SFunInfo& sfi = functions[name];
		sfi.step = 1;
		sfi.fun = new BooleanDAG("tmp");
		bool_node* svar = sfi.fun->create_inputs(1, node.getOtype(),  "SVAR");
        
		sfi.symval = src;
		sfi.outval = rv;
		for(int i=0; i<nargs; ++i){
			stringstream str;
			str<<"PARAM_"<<i;
			sfi.fun->create_inputs(1, node.getOtype(),  str.str());
			sfi.actuals.push_back(nmmother[i]);
		}
		sfi.fun->create_outputs(src->get_nbits(), svar, "OUT");
		bool_node* C0 = new CONST_node(0);
		sfi.fun->addNewNode(C0);
		sfi.fun->create_outputs(1, C0, "RES");
	}else{
		if(nmmother.size()==0){
			return functions[name].symval;
		}
        
		BooleanDAG& comp = getComparator(nmmother.size());
		//comp is now a comparator that will compare N inputs with N other inputs.
		// comp(ina_0, ..., ina_N, inb_0, ..., inb_N) := and_(0<=i<=N)(ina_i == inb_i);
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
			sfi.actuals[i] = nmmother[i];
		}
		// Dout( cclone->print(cout) );
		Dout( cout<<" after replacing inputs "<<endl);
		bool_node* dst = cclone->get_node("OUT");
		Assert( dst != NULL && dst->type == bool_node::DST, "This can't happen!asdpoiuy");
		
		//The output of node of the comparator is removed, and instead, the output of the
		//comparisson is fed into a mux that selects between the current symvalue and the previous symvalue.
		
		
		ARRACC_node* ch = new ARRACC_node();
		ch->mother = dst->mother;
		
        
		SRC_node* src = srcNode(node, sfi.step);
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
		
		bool_node* res = sfi.fun->get_node("RES" );
		OR_node* on = new OR_node();
		on->mother = res->mother;
		on->father = ch->mother;
		cclone->addNewNode(on);
		
        
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
				//Assert( n==NULL || n == outn, "I thought this was going to be the only DST node");
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
				cclone->replace(tt->id, nmmother[i]);
			}
			
			stringstream parnm;
			parnm<<"PARAM_"<<i;
			bool_node* par =  sfi.fun->create_inputs(1, node.getOtype(), parnm.str());
			
			bool_node* npar = sfi.fun->get_node(str.str());
			sfi.fun->replace(npar->id, par);
		}
		
		// Dout( cclone->print(cout) );
		Dout( cout<<"fully integrated cclone. "<<endl  );
		
		{
			bool_node* tmpbn = sfi.fun->get_node(src->name);
			Assert(tmpbn != NULL, "This is an abomination.");
			bool_node* nsvar = sfi.fun->create_inputs(1, node.getOtype(), "SVAR");
			Dout( cout<<" replacing "<<tmpbn->get_name()<<":ch="<< tmpbn->children.size()  <<" with "<< nsvar->get_name() <<endl);
			sfi.fun->replace(tmpbn->id, nsvar  );
		}
		
		sfi.fun->create_outputs(1, (*sfi.fun)[rv->id], "OUT");
		sfi.fun->create_outputs(1, (*sfi.fun)[on->id], "RES");
        
		Dout( cout<<" ADDING "<<cclone->size()<<" NODES"<<endl );
        
		newnodes.insert(newnodes.end(), cclone->begin(), cclone->end());
		sfi.fun->removeNullNodes();
		sfi.fun->relabel();
		cclone->disownNodes();
		delete cclone;
		
        
		Dout(  sfi.fun->print(cout) );
		Dout( cout<<" DONE DONE DONE"<<endl );
	}
	return rv;
}


void DagElimUFUN::visit( UFUN_node& node ){
	string name = node.get_ufname();
	name += node.outname;
	if(( functions.find(name) == functions.end()) || functions[name].moreNewFuns ){
		rvalue = produceNextSFunInfo( node  );
	}else{
		//This branch is only taken if the user passes the -ufunSymmetry flag. This flag means that uninterpreted functions in the
		//sketch are required to have the same parameters in the spec. This means that no new symbolic values need to be added
		//when evaluating uninterpreted functions in the sketch. (Except for the very first call, which needs a fresh symbolic value
		//to distinguish it from the others; hence the moreNewFuns flag.).
		Dout( cout<<"Replacing call to function "<< node.get_ufname() <<" : "<<node.id<<endl );
		int nargs = node.multi_mother.size();
		vector<bool_node*>* nmm = &node.multi_mother;
		/*if(node.dependent()){
			nargs = mothercache[node.fgid].size();
			nmm = &(mothercache[node.fgid]);
		}else{
			mothercache[node.fgid] = node.multi_mother;
		}*/
		vector<bool_node*>& nmmother = *nmm;
		if(nargs == 0){
			if(oneMoreFun){
				functions[name].moreNewFuns = false;
			}
			rvalue = functions[name].symval;
			return;
		}
        
		SFunInfo& sfi = functions[name];
		BooleanDAG* cclone = sfi.fun->clone();
		for(int i=0; i<nargs; ++i){
			stringstream str1;
			str1<<"PARAM_"<<i;
			bool_node* inarg = sfi.fun->get_node( str1.str() );
			Assert(inarg != NULL, "This can't be happening!!!");
			cclone->replace(inarg->id, nmmother[i]);
		}
		
		bool_node* src = NULL;
		if(true){
			src =  new CONST_node(0);
		}else{
			stringstream str;
			
			src = srcNode(node, sfi.step);
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
        
		bool_node* resn = cclone->get_node("RES" );
		bool_node* rrn = resn->mother;
        
		int oldsize = newnodes.size();
		for(int i=0; i<cclone->size(); ++i){
			bool_node* n = (*cclone)[i];
			if( n != NULL &&  n->type != bool_node::DST ){
				newnodes.push_back(n);
			}else{
				//Assert( n==NULL || n == outn, "I thought this was going to be the only DST node "<<n->get_name()<<" != "<<outn->get_name());
				if( n!= NULL){
					n->dislodge();
					delete n;
				}
			}
		}
		
        
        NOT_node* nn = new NOT_node();
        nn->mother = node.mother;
        nn->addToParents();
        newnodes.push_back(nn);
        
        OR_node* orode = new OR_node();
        orode->mother = nn;
        orode->father = rrn;
        orode->addToParents();
        newnodes.push_back(orode);
        
        ASSERT_node* asn = new ASSERT_node();
        asn->mother = orode;
        asn->addToParents();
        string msg = name;
        msg += ": UFUN is being misused";
        asn->setMsg(msg);
        newnodes.push_back(asn);
		
		
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
    
    
    
	/*
     You don't want to do cleanup here, because when you eliminate ufun from the spec,
     some nodes may no longer be used by the spec, but they will be used by the sketch, so you don't want to get rid of them yet.
     dag.cleanup(false);
     */
	Dout( cout<<" AFTER PROCESS "<<endl );
	Dout( dag.print(cout) );	
	Dout(cout<<" end ElimFun "<<endl);
}


