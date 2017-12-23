#include "NodeHardcoder.h"




bool_node* NodeHardcoder::nodeForINode(INTER_node* inode){
	int arrsz = -1;
  if (inode->type == bool_node::SRC) {
    SRC_node* src_ = dynamic_cast<SRC_node*>(inode);
    if (src_->isTuple) {
      Assert(false, "Not possible");
    }
  }
	if(inode->type== bool_node::SRC){	
		arrsz = dynamic_cast<SRC_node*>(inode)->arrSz;
	}
	if(arrsz>=0){
		VarStore::objP* val = &(values.getObj(inode->get_name()));
		int nbits = inode->get_nbits();
		ARR_CREATE_node* acn = new ARR_CREATE_node();
		while(val != NULL){
			bool_node* cnst;
			if(nbits==1){
				cnst= getCnode( val->getInt() ==1 );
			}else{
				cnst= getCnode( val->getInt() );
			}
			while(acn->multi_mother.size()< val->index){
				acn->multi_mother.push_back( getCnode(0) );
			}
			acn->multi_mother.push_back( cnst );
			val = val->next;
		}
		acn->addToParents();		
		if(showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< acn->lprint() <<endl; }
		return optAdd(acn);
	}else{
		int nbits = inode->get_nbits();		
		bool_node* onode;
		if(nbits==1){
			onode= getCnode( values[inode->get_name()]==1 );
		}else{
			onode= getCnode( values[inode->get_name()] );
		}
		if(showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< onode->lprint() <<endl; }
		return onode;
	}
}


NodeHardcoder::~NodeHardcoder(void)
{
}




void NodeHardcoder::visit( SRC_node& node ){
	if(type == bool_node::SRC){
		rvalue = nodeForINode(&node);
	}else{
		DagOptim::visit(node);
	}
}

void NodeHardcoder::visit( CTRL_node& node ){
	if(type == bool_node::CTRL){
		CTRL_node* cn =  &node;
		if(cn->get_Angelic()){
			if(cn->children.size() != 0){
				if (cn->spAngelic) {
					// replace it with a src node					
					SRC_node* src = dynamic_cast<SRC_node*>(bdag->create_inputs(cn->get_nbits(), OutType::INT, cn->get_name() + "_src", cn->getArrSz()));
					rvalue = src;
					return;				
				}

				Assert(cn->children.size() == 1, "NYI; hafdst");
				bool_node* bn = *(cn->children.begin());
				Assert(bn->type == bool_node::ARRACC || bn->type == bool_node::ARRASS, "NYI;aytut");
				arith_node* an = dynamic_cast<arith_node*>(bn);
				if(an->multi_mother[0]==cn){
					Assert(an->multi_mother[0]==cn, "NYI; cvbnm");
					rvalue =  an->multi_mother[1];
					return;
				}else{
					Assert(an->multi_mother[1]==cn, "NYI; weafhgdz");
					rvalue =  an->multi_mother[0];
					return;
				}			
			}else{
				rvalue = getCnode(0);
				return;
			}
		}
		rvalue = nodeForINode(&node);
	}else{
		DagOptim::visit(node);
	}
}


bool_node* NodeHardcoder::nodeForFun(UFUN_node* uf){
	Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(uf->getTupleName()));
	int size = tuple_type->actSize;
	TUPLE_CREATE_node* new_node = new TUPLE_CREATE_node();
	for (int j = 0; j < size ; j++) {
		stringstream sstr;
		sstr<<uf->get_ufname()<<"_"<<uf->get_uniquefid()<<"_"<<j;
		OutType* type = tuple_type->entries[j];
		Assert(!type->isTuple, "NYS");	
		if(type->isArr){
			VarStore::objP* val = &(values.getObj(sstr.str()));
			int nbits = val->size();
			ARR_CREATE_node* acn = new ARR_CREATE_node();
			while(val != NULL){
				bool_node* cnst;
				if(nbits==1){
					cnst= getCnode( val->getInt() ==1 );
				}else{
					cnst= getCnode( val->getInt() );
				}
				while(acn->multi_mother.size()< val->index){
					acn->multi_mother.push_back( getCnode(0) );
				}
				acn->multi_mother.push_back( cnst );
				val = val->next;
			}
			acn->addToParents();					
			new_node->multi_mother.push_back( optAdd(acn) );
		}else{
			int val = values[sstr.str()];
			new_node->multi_mother.push_back( getCnode(val) );
		}
	}
	new_node->addToParents();
	return optAdd(new_node);	
}


void NodeHardcoder::nodeFromSyn(UFUN_node& node) {
	auto it = values.synths.find(node.get_ufname());
	if (it == values.synths.end()) {
		DagOptim::visit(node);
	} else {
		SynthInSolver* sin = it->second;
		rvalue = sin->getExpression(this, node.multi_mother);
    if(rvalue->type == bool_node::TUPLE_CREATE) {
      return;
    }


		Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(node.getTupleName()));
		int size = tuple_type->actSize;
		TUPLE_CREATE_node* new_node = new TUPLE_CREATE_node();
		for (int j = 0; j < size; j++) {
			OutType* type = tuple_type->entries[j];
			Assert(!type->isTuple, "NYS");
			if (type->isArr) {
				Assert(false, "NYI");
			} else {				
				new_node->multi_mother.push_back(rvalue);
			}
		}
		new_node->addToParents();
		rvalue = optAdd(new_node);
		return;
	}
}


void NodeHardcoder::visit( UFUN_node& node ){
	if(type == bool_node::SRC){		

		if (floats.hasFun(node.get_ufname()) || node.get_ufname() == "_cast_int_float_math") {
			DagOptim::visit(node);
			return;
		}

		if (node.isSynNode()) {		
			DagOptim::visit(node);
			return;
		}



		UFUN_node* uf = &node;
		vector<pair<bool_node*, vector<bool_node*> > >& params = ufunparams[uf->get_ufname()];
		
		vector<bool_node*> pars = uf->multi_mother;
		
		bool_node* out = nodeForFun(uf);
		for(int ii=0; ii<params.size(); ii++){
			vector<bool_node*>& cpar = params[ii].second;						
			bool_node* cond = NULL;
			for(int jj=0; jj<pars.size(); ++jj){
				bool_node* eq = new EQ_node();
				
				eq->mother = cpar[jj];
				eq->father = pars[jj];
				eq->addToParents();
				eq = optAdd(eq);
				if(cond==NULL){
					cond = eq;
				}else{
					bool_node* an = new AND_node();
					an->mother = cond;
					an->father = eq;
					an->addToParents();
					an = optAdd(an);
					cond = an;
				}
			}
			ARRACC_node* chose = new ARRACC_node();
			chose->mother = cond;
			chose->multi_mother.push_back(out);
			chose->multi_mother.push_back(params[ii].first);
			chose->addToParents();			
			out = optAdd(chose);
		}
		out = cse.computeCSE(out);
		params.push_back(make_pair(out, pars));
		rvalue = out;		
	}else{
		if (node.isSynNode()) {
			nodeFromSyn(node);
			return;
		}
		DagOptim::visit(node);
	}
}

