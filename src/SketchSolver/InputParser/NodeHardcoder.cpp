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
	auto otype = inode->getOtype();
	if(arrsz>=0) {
        auto val = &(values.getObjConst(inode->get_name()));
        int nbits = inode->get_nbits();
		vector<bool_node*> multi_mother;

        if(val != nullptr) {
            assert(inode->isArrType());
        }

        val->populate_multi_mother_nodeForINode(multi_mother, this, nbits, floats);

		ARR_CREATE_node* acn = ARR_CREATE_node::create(multi_mother, getCnode(0));
		acn->addToParents();		
		if(showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< acn->lprint() <<endl; }
		return optAdd(acn);
	}else{
		bool_node* onode;
		if(!values.contains(inode->get_name()))
		{
		    //IS THIS OK?
		    onode = inode;
		}
		else
		{
            assert(!inode->isArrType());
//            assert(values.getObjConst(inode->get_name()).get_size() == inode->get_nbits());

		    if(inode->getOtype() == OutType::BOOL){
		        Assert(otype != OutType::FLOAT, "node should not be a FLOAT.");
		        onode= getCnode( values[inode->get_name()]==1 );
		    }else{
		        if (otype == OutType::FLOAT) {
		            onode = getCnode( floats.getFloat(values[inode->get_name()]) );
		        }
		        else {
		            onode = getCnode(values[inode->get_name()]);
		        }
		    }
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
	    if(values.contains(node.get_name()))
	    {
            if (!node.isArrType()) {
                int obj_const_size = values.getObjConst(node.get_name()).get_size();
                int node_get_nbits = node.get_nbits();
//                assert(obj_const_size == node_get_nbits);
            } else {
                int obj_const_size = values.getObjConst(node.get_name()).get_size();
                int node_nbits = node.get_nbits();
                int node_arrsz = node.getArrSz();
                assert(obj_const_size == node_nbits * node_arrsz);
            }
            rvalue = nodeForINode(&node);
	    }
	    else
	    {
	        DagOptim::visit(node);
	    }
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
				bool_node* an = bn;
				if(an->get_parent(1) ==cn){
					Assert(an->get_parent(1) == cn, "NYI; cvbnm");
					rvalue =  an->get_parent(2);
					return;
				}else{
					Assert(an->get_parent(2) == cn, "NYI; weafhgdz");
					rvalue =  an->get_parent(1);
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
	TUPLE_CREATE_node* new_node = TUPLE_CREATE_node::create(size);
	for (int j = 0; j < size ; j++) {
		stringstream sstr;
		sstr << uf->get_ufun_name() << "_" << uf->get_uniquefid() << "_" << j;
		OutType* type = tuple_type->entries[j];
		Assert(!type->isTuple, "NYS");	
		if(type->isArr){
			auto val = &(values.getObjConst(sstr.str()));
			int nbits = val->element_size();
			vector<bool_node*> multi_mother;

            val->populate_multi_mother_nodeForFun(multi_mother, this, nbits);

			ARR_CREATE_node* acn = ARR_CREATE_node::create(multi_mother, getCnode(0));
			
			acn->addToParents();					
			new_node->set_parent(j, optAdd(acn) );
		}else{
			int val = values[sstr.str()];
			new_node->set_parent(j, getCnode(val) );
		}
	}
	new_node->addToParents();
	return optAdd(new_node);	
}


void NodeHardcoder::nodeFromSyn(UFUN_node& node) {
	auto it = values.synths.find(node.get_ufun_name());
	if (it == values.synths.end()) {
		DagOptim::visit(node);
	} else {
		SynthInSolver* sin = it->second;
		rvalue = sin->getExpression(this, node.arg_begin(), node.arg_end());
		if (rvalue == NULL) return;
    if(rvalue->type == bool_node::TUPLE_CREATE) {
      return;
    }


		Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(node.getTupleName()));
		int size = tuple_type->actSize;
		TUPLE_CREATE_node* new_node = TUPLE_CREATE_node::create(size);
		for (int j = 0; j < size; j++) {
			OutType* type = tuple_type->entries[j];
			Assert(!type->isTuple, "NYS");
			if (type->isArr) {
				Assert(false, "NYI");
			} else {				
				new_node->set_parent(j, rvalue);
			}
		}
		new_node->addToParents();
		rvalue = optAdd(new_node);
		return;
	}
}


void NodeHardcoder::visit( UFUN_node& node ){
	if(type == bool_node::SRC){		

		if (floats.hasFun(node.get_ufun_name()) || node.get_ufun_name() == "_cast_int_float_math") {
			DagOptim::visit(node);
			return;
		}

		if (node.isSynNode()) {		
			DagOptim::visit(node);
			return;
		}



		UFUN_node* uf = &node;
		vector<pair<bool_node*, vector<bool_node*> > >& params = ufunparams[uf->get_ufun_name()];
		
		vector<bool_node*> pars( uf->arg_begin(), uf->arg_end());
		
		bool_node* out = nodeForFun(uf);
		for(int ii=0; ii<params.size(); ii++){
			vector<bool_node*>& cpar = params[ii].second;						
			bool_node* cond = NULL;
			for(int jj=0; jj<pars.size(); ++jj){
				bool_node* eq = EQ_node::create();
				
				eq->mother() = cpar[jj];
				eq->father() = pars[jj];
				eq->addToParents();
				eq = optAdd(eq);
				if(cond==NULL){
					cond = eq;
				}else{
					bool_node* an = AND_node::create();
					an->mother() = cond;
					an->father() = eq;
					an->addToParents();
					an = optAdd(an);
					cond = an;
				}
			}
			ARRACC_node* chose = ARRACC_node::create(cond, out, params[ii].first);
			
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

bool_node *NodeHardcoder::get_rvalue() {
	return rvalue;
}

BooleanDAG *hardCodeINodeNoClone(BooleanDAG *dag, const VarStore &values, const bool_node::Type type, FloatManager &floats) {
    int oldsize = dag->size();

    // if(PARAMS->verbosity > 2) {
    // char const * stype = (type == bool_node::CTRL? "Controls" : "Inputs");
    // cout<<" * Specializing problem for "<< stype <<endl;
    // cout<<" * Before specialization: nodes = "<<newdag->size()<<" " << stype << "= " <<  inodeList.size() <<endl;
    // }

    NodeHardcoder nhc(PARAMS->showInputs, *dag, values, type, floats);
    nhc.process(*dag);

    Dout(dag->print(cout) );
    DagOptim cse(*dag, floats);
    cse.process(*dag);
    dag->cleanup();
    if(false){
        BackwardsAnalysis ba;
        ba.process(*dag);
    }
    if(false){
        DagOptim cse(*dag, floats);
        cse.process(*dag);
    }
    if(PARAMS->verbosity > 3){ cout << " * After optims it became = " << dag->size() << " was " << oldsize << endl; }

    dag->set_failed_assert(cse.get_failedAssert());

//    cout << "FROM ORIGINAL INLINING: newdag" << endl;
//    newdag->lprint(cout);
//    cout << "//////////////" << endl;

    return dag;
}

BooleanDAG *hardCodeINode(BooleanDAG *dag, VarStore &values, bool_node::Type type, FloatManager &floats) {
    BooleanDAG* newdag = dag->clone();
    return hardCodeINodeNoClone(newdag, values, type, floats);
}

