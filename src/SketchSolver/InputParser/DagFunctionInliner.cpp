#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"
#include "CommandLineArgs.h"
#include <sstream>


//extern CommandLineArgs* PARAMS;

#ifndef  _MSC_VER
static const int MAX_NODES = 1000000;
#else
static const int MAX_NODES = 510000;
#endif

DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap,  map<string, map<string, string> > p_replaceMap, HoleHardcoder* p_hcoder,
	bool p_randomize, InlineControl* ict):
dag(p_dag), 
DagOptim(p_dag), 
ufunAll(" ufun all"),
functionMap(p_functionMap),
replaceMap(p_replaceMap),
ictrl(ict),
randomize(p_randomize),
hcoder(p_hcoder)
{
	alterARRACS();
}

DagFunctionInliner::~DagFunctionInliner()
{
}


/*
int space[100];
ARRACC_node tmparrac;
UFUN_node tmpufun("NULL");
ARRASS_node tmparrass;
ACTRL_node tmpactrl;
*/


int sizeForNode(bool_node* bn){

	if(typeid(*bn) == typeid(SRC_node)){
		return sizeof(SRC_node);
	}
	if(typeid(*bn) == typeid(CTRL_node)){
		return sizeof(CTRL_node);
	}	
	if(typeid(*bn) == typeid(DST_node)){
		return sizeof(CTRL_node);
	}	
	if(typeid(*bn) == typeid(CONST_node)){
		return sizeof(CONST_node);
	}
	if(typeid(*bn) == typeid(ASSERT_node)){
		return sizeof(CONST_node);
	}
	return sizeof(AND_node);
}

bool_node* DagFunctionInliner::optAndAddNoMap(bool_node* nnode){ //assumes node is unnattached
	bool_node* nnodep = this->computeOptim(nnode);						
	if(nnodep == nnode){
		nnode->addToParents();
		this->addNode(nnode);								
	}else{
		delete nnode;
	}
	return nnodep;
}


void DagFunctionInliner::optAndAdd(bool_node* n, vector<const bool_node*>& nmap){//Assumes node has already been attached
	int nodeId = n->id;
	bool_node* nnode = this->computeOptim(n);						
	if(nnode == n){
		this->addNode( n );							
		nmap[nodeId] = n;	
	}else{
		n->dislodge();
		delete n;
		nmap[nodeId] = nnode;
	}
}


void HoleHardcoder::afterInline(){
	for(map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ){
		if(LEAVEALONE(it->second)){
			randholes.erase(it++);
		}else{
			++it;
		}
	}
}


void HoleHardcoder::printControls(ostream& out){
	for(map<string, int>::iterator it = randholes.begin(); it != randholes.end(); ++it){
			if(!LEAVEALONE(it->second)){
				out<<it->first<<"\t"<<it->second<<endl;
			}
		}
}


	int HoleHardcoder::fixValue(const string& s, int bound, int nbits){
		int rv = rand() % bound;
		if(sat->checkVar(s)){
			int H__0_var_idx = sat->getVar(s);			
			Tvalue tv = H__0_var_idx;
			tv.setSize(nbits);
			tv.makeSparse(*sat);			
			vector<guardedVal>& gvs =tv.num_ranges;
			for(int i=0; i<gvs.size(); ++i){
				guardedVal& gv = gvs[i];
				if(gv.value == rv){
					int xx = sat->getMng().isValKnown(gv.guard);
					if(xx==0){
						//it's ok to set the value to this.						
						if(!sat->assertIfPossible(gv.guard)){
							// tried to set it but didn't work. we'll have to try something different.
							break;
						}
						randholes[s] = rv;
						return rv;
					}
					if(xx==1){
						//the hole is already equal to this.
						randholes[s] = rv;
						return rv;
					}else{
						cout<<" can't set to "<<rv<<endl;
						//the hole cannot be equal to this vale. 
						break;
					}
				}
			}
			//If I get here, it means that rv either didn't match with any of the guarded values, or matched
			//with a guarded value that was already equal to false, so we need to find another guarded value 
			//that does match.
			do{
				int t = rand() % gvs.size();
				guardedVal& g2 = gvs[t];
				Assert(sat->getMng().isOK(), "Could not set hole "<<s);
				int yy =  sat->getMng().isValKnown(g2.guard);
				if(yy==0){
					//it's ok to set the value to this, but let's check
					if(!sat->assertIfPossible(g2.guard)){
							// tried to set it but didn't work. we'll have to try something different.
							// cout<<"can't set to "<<g2.value<<endl;
							continue;
					}
					randholes[s] = g2.value;
					return g2.value;
				}
				if(yy==1){
					randholes[s] = g2.value;
					return g2.value;
				}
				// cout<<"can't set to "<<g2.value<<endl;
				//This cannot be an infinite loop because the guards for the guarded values cannot all be false.
			}while(true);				
		}else{
			randholes[s] = rv;
		}
		return rv;
	}

bool_node* HoleHardcoder::checkRandHole(CTRL_node* node, DagOptim& opt){
		map<string, int>::iterator it = randholes.find( node->get_name()  );
		int chsize = node->children.size();							
			int tchld = 0;
			int bchld = 0;
			for(childset::iterator it = node->children.begin(); it != node->children.end(); ++it){
				bool_node* chld = *it;				
				if(chld->type == bool_node::ARRACC){
					ARRACC_node* an = (ARRACC_node*) chld;
					bool allconst = true;
					for(vector<bool_node*>::iterator it = an->multi_mother.begin(); it != an->multi_mother.end(); ++it){
						if((*it)->type != bool_node::CONST){
							allconst = false;
							break;
						}
					}	
					if(allconst){ 
						// cout<<"     ALLCONSTS "<<an->children.size()<<endl;
						tchld += an->children.size(); 
					}else{
						tchld += 1;
					}
				}else if(chld->type==bool_node::NOT){
					bchld += chld->children.size();
				}else{
					if(chld->type == bool_node::AND || chld->type == bool_node::OR){
						bchld += 1;
					}else{
						tchld += 1;
					}					
				}
			}	
			tchld += bchld / 2;
		if( it != randholes.end() ){
			if(LEAVEALONE(it->second)){
				int oldchld = -it->second; //how many chlidren it had the first time around.
				if(tchld > oldchld + 10 ){ 
					// cout<<"I've seen this before, but I am going to try again "<< node->lprint() <<" nchildren ="<<tchld<<endl;
				}else{
					return node;
				}
			}else{
				return opt.getCnode(it->second);
			}
		}
		{							
			int baseline = PARAMS->randdegree;
			int odds = max(2, baseline/ (tchld>0?tchld:1)  );					
			chsize = tchld;
			if(chsize == 1){
				bool_node* bn = * node->children.begin();
				if(bn->type == bool_node::DST || bn->type == bool_node::TUPLE_CREATE || bn->type == bool_node::UFUN){
					// cout<<"postponing for later"<<endl;
					return node;
				}else{
					// cout<<"Single child is "<<bn->lprint()<<endl;
				}
			}			
			cout<<node->get_name()<<" odds = 1/"<<odds<<"  ("<<chsize<<", "<<tchld<<") ";
			if(rand() % odds == 0 || chsize > 1500){
				cout<<" try to replace"<<endl;
				int bound = 1;
				
				int nbits = node->get_nbits();
				for(int qq = 0; qq < nbits; ++qq){
					bound = bound*2;
				}
				bool ARRASSEQonly = true;
				chkrbuf.resize(bound);
				for(int i=0; i<bound; ++i){ chkrbuf[i] = false; }
				int obound = bound;
				int ul = -1;
				for(childset::iterator it = node->children.begin(); it != node->children.end(); ++it){
					// cout<<node->get_name()<<"  "<<(*it)->lprint()<<endl;
					bool_node* child = *it;
					if(child->type == bool_node::LT){
						if(child->mother == node && child->father->type == bool_node::CONST){
							ul = max(ul, opt.getIval(child->father));
						}else if(child->father == node && child->mother->type == bool_node::CONST){	

							if(dynamic_cast<CONST_node*>(child->mother)->getVal()==0){
								//nothing to do, but so far so good.
								// cout<<"so far so good"<<child->lprint()<<endl;
							}else if (child->children.size()==1 && (*child->children.begin())->type == bool_node::NOT){
								ul = max(ul, opt.getIval(child->mother)+1);
							}else{
								cout<<"    has a bad child"<<child->lprint()<<endl;
								randholes[node->get_name()] = REALLYLEAVEALONE;
								return node;
							}								
						}else{
							cout<<"    has a bad child"<<child->lprint()<<endl;
							randholes[node->get_name()] = REALLYLEAVEALONE;
							return node;
						}
						ARRASSEQonly = false;
					}else{
						if(child->type == bool_node::ARRACC && child->mother == node){
							bound = min(bound, (int) ((arith_node*)child)->multi_mother.size());
							ARRASSEQonly = false;
						}else{
							if(child->type == bool_node::ARRASS && child->mother == node){
								int quant = ((ARRASS_node*)child)->quant;
								chkrbuf[quant] = true;
								ul = max(ul, quant);
							}else{
							if(!(child->type == bool_node::EQ )){

								if(child->type == bool_node::AND || child->type == bool_node::OR || child->type == bool_node::NOT){
									// cout<<"so far so good"<<child->lprint()<<endl;
								}else{									
									cout<<"    has a bad child"<<child->lprint()<<endl;
									randholes[node->get_name()] = REALLYLEAVEALONE;
									return node;
								}
							}else{ //child->type == eq
								if(child->father->type == bool_node::CONST){
									chkrbuf[((CONST_node*)child->father)->getVal()] = true;
								}
								if(child->mother->type == bool_node::CONST){
									chkrbuf[((CONST_node*)child->mother)->getVal()] = true;
								}
							}
							}
						}
					}
				}
				if(ul > 0 && bound ==obound ){
					bound = min(bound, ul);
				}				
				int rv = fixValue(node->get_name(), bound, nbits);
				cout<<node->get_name()<<": replacing with value "<<rv<<" bnd= "<<bound<<endl;
				return opt.getCnode(rv);
			}else{
				cout<<" not replacing"<<endl;
				if(chsize == 0){ chsize = 1; }
				randholes[node->get_name()] = -chsize;
				return node;
			}
		}
	}


void DagFunctionInliner::visit(CTRL_node& node){
	
	
	if(node.get_Pcond()){
		rvalue = this->getCnode(true);
	}else{
		if(randomize){
			rvalue = hcoder->checkRandHole(&node, *this);
			return;
		}
		DagOptim::visit(node);
	}
}


bool checkParentsInMap(bool_node* node, vector<const bool_node*>& secondarynmap, vector<const bool_node*>& nmap){
	bool chk = false;
	bool_node* tt = node->mother;
	chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);
	tt = node->father;
	chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);
	arith_node* an = NULL;
	if(node->isArith()){
		an = dynamic_cast<arith_node*>(node);
		for(int i=0; i<an->multi_mother.size(); ++i){
			tt = an->multi_mother[i];
			chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);			
		}
	}
	if(chk){
		tt = node->mother;
		if(tt!= NULL && secondarynmap[tt->id] == NULL){
			secondarynmap[tt->id] = nmap[tt->id];
		}
		tt = node->father;
		if(tt!= NULL && secondarynmap[tt->id] == NULL){
			secondarynmap[tt->id] = nmap[tt->id];
		}
		if(an != NULL){
			for(int i=0; i<an->multi_mother.size(); ++i){
				tt = an->multi_mother[i];
				if(tt!= NULL && secondarynmap[tt->id] == NULL){
					secondarynmap[tt->id] = nmap[tt->id];
				}
			}
		}
	}
	return chk;
}


void DagFunctionInliner::visit( UFUN_node& node ){	
	Dllist tmpList;
	const string& name = node.get_ufname();
	map<int, int> oldToNew;

	if(ictrl != NULL && !ictrl->checkInline(node)){
		//mpcontroller[node.fgid]["__ALL"] = NULL;
		DagOptim::visit(node);			
		return;
	}	


	bool_node* condition = node.mother;
	if(isConst(condition) && !getBval(condition)){
		rvalue = getCnode(0);
		//mpcontroller[node.fgid]["__ALL"] = getCnode(0);
		return;
	}
  
  map<string, map<string, string> >::iterator it = replaceMap.find(name);
  if (it != replaceMap.end()) {
    if (ictrl != NULL && ictrl->isRecursive(node)) {
      cout << "Replacing ufun node " << name << endl;
      string oldOutputType = node.getTupleName();
      string newName = (it->second).begin()->second;
      node.modify_ufname(newName);
      node.outname = "_p_out_" + newName + "_ANONYMOUS"; // TODO: fix these magic strings
      string newNameCaps = newName;
      std::transform(newNameCaps.begin(), newNameCaps.end(), newNameCaps.begin(), ::toupper);
      string newOutputType = newNameCaps + "_ANONYMOUS";
      node.set_tupleName(newOutputType);
      int oriInpSize = node.multi_mother.size();
      Assert(functionMap.find(newName) != functionMap.end(), "Function " + newName + " is not found");
      BooleanDAG& repFun = *functionMap[newName];
      vector<bool_node*>& inputs  = repFun.getNodesByType(bool_node::SRC);
      int inpSize = inputs.size();
      vector<bool_node*> extraInputs;
      
      for (int i = 0; i < oriInpSize; i++) {
        bool_node* input = inputs[i];
        // Assuming that the first inputs will correspond to the inputs of the original function
        Assert(input->getOtype() == node.multi_mother[i]->getOtype(), "Input types mismatch");
      }
      
      for (int i = oriInpSize; i < inpSize; i++) {
          SRC_node* inp = dynamic_cast<SRC_node*>(inputs[i]);
          OutType* outputType = inp->getOtype();

          //create a angelic hole
          CTRL_node* qn = new CTRL_node();
          stringstream st;
          st<<inp->name;
          st<<"_";
          st<<node.get_callsite();
          
          qn->name = st.str();
          qn->set_Special_Angelic();
        
          if (outputType->isTuple) {
            Assert(!outputType->isArr, "NYI");
            qn->setTuple(((Tuple*)outputType)->name);
          }
      
          int nbits = 5;
          if (outputType == OutType::BOOL || outputType == OutType::BOOL_ARR) {
            nbits = 1;
          }
          if (outputType == OutType::INT || outputType == OutType::INT_ARR) {
            nbits = 5;
          }
        
          qn->set_nbits(nbits);
        
          if (outputType == OutType::INT_ARR || outputType == OutType::BOOL_ARR) {
            qn->setArr(PARAMS->angelic_arrsz);
          }
          addNode(qn);
          node.multi_mother.push_back(qn);
          extraInputs.push_back(qn);
        
      }
      if (inpSize > oriInpSize) {
        node.makeAssertsHard(); // TODO: this is wrong
      }
      TUPLE_CREATE_node* output = dynamic_cast<TUPLE_CREATE_node*>(computeOptim(&node));
  
      TUPLE_CREATE_node* tuple_node = new TUPLE_CREATE_node();
      int outSize = output->multi_mother.size();
    
      Tuple* oldOutputTuple = (Tuple*)OutType::getTuple(oldOutputType);
      Tuple* tup = (Tuple*) (oldOutputTuple->entries[oldOutputTuple->entries.size() - 1]);
      tuple_node->setName(tup->name);
      int size = tup->entries.size();
      tuple_node->multi_mother.push_back(getCnode(20)); // TODO: get rid of this magic number
      int stateSize = inpSize - oriInpSize;
      int actOutSize = outSize - stateSize;
      int actFields = size - stateSize - outSize ;
      //cout << stateSize << " " << actOutSize << " " << actFields << endl;
      for (int i = 1; i <  actFields; i++) {
        tuple_node->multi_mother.push_back(getCnode(0));
      }
      
      for (int i = 0; i < stateSize; i++) {
        tuple_node->multi_mother.push_back(extraInputs[i]);
      }
      
      
      for (int i = 0; i < outSize; i++) {
        tuple_node->multi_mother.push_back(output->multi_mother[i]);
      }
     
      tuple_node->addToParents();
      output->dislodge();
      //delete output;
      TUPLE_CREATE_node* new_output = new TUPLE_CREATE_node();
      new_output->setName(oldOutputType);
      for (int i = 1; i < oriInpSize; i++) {
        new_output->multi_mother.push_back(node.multi_mother[i]);
      }
      new_output->multi_mother.push_back(optAdd(tuple_node));
      new_output->addToParents();
      rvalue = optAdd(new_output);
      return;
    }
  }
  
  bool shouldReplaceSpecialNode = false;
  
  map<string, map<string, string > >::iterator it1 = replaceMap.begin();
  
  for(; it1 != replaceMap.end(); it1++) {
    map<string, string> fmap = it1->second;
    if (fmap.find(name) != fmap.end()) {
      shouldReplaceSpecialNode = true;
      break;
    }
  }
  
	ufunAll.restart();


	if( functionMap.find(name) != functionMap.end() ){
    //cout << "Inlining " << name << endl;
		/*if(mpcontroller.count(node.fgid) > 0){
			map<string,bool_node*>::iterator it = mpcontroller[node.fgid].find(node.outname);
			if(it != mpcontroller[node.fgid].end()){
				bool_node* rv = it->second;
				rvalue = rv;
			}else{
				bool_node* rv = mpcontroller[node.fgid]["__ALL"];
				if(rv == NULL){
					DagOptim::visit(node);	
				}else{
					rvalue = rv;
				}
			}			
			return;
		}*/

				
		BooleanDAG& oldFun = *functionMap[name];

		if(oldFun.isModel && node.children.size() == 0){
			rvalue = getCnode(false);
			return;
		}

		funsInlined.insert(name);
		if(ictrl != NULL){ ictrl->registerInline(node); }


		// cout<<" Inlined "<<node.get_ufname()<<"   mother = "<<node.mother->lprint()<<endl;
		/*
		if(!isConst(node.mother)){
			cout<<" Inlined "<<node.get_ufname()<<"   mother = "<<node.mother->lprint()<<endl;
			stack<bool_node*> bns;
			bns.push(node.mother);
			while(! bns.empty()){
				bool_node* c = bns.top(); 
				bns.pop();
				if(c->mother != NULL && !isConst(c->mother)){					
					bns.push(c->mother);
				}
				if(c->father != NULL && !isConst(c->father)){
					bns.push(c->father);
				}
				if(c->isArith() && c->type != bool_node::UFUN){
					vector<bool_node*>& bn = dynamic_cast<arith_node*>(c)->multi_mother;
					for(int i=0; i<bn.size(); ++i){
						if(!isConst(bn[i])){
							bns.push(bn[i]);
						}
					}
				}
				cout<<"          "<<c->lprint()<<endl;
			}
		}
		*/

		

		//oldFun->clone_nodes(clones);		
		vector<const bool_node*> nmap;
    TUPLE_CREATE_node* newOutputTup;
    bool_node* spCond;
		nmap.resize( oldFun.size() );		
		{
			vector<bool_node*>& inputs  = oldFun.getNodesByType(bool_node::SRC);
			// ADT, int, int ... (state)
			Assert( inputs.size() == node.multi_mother.size() , node.get_ufname()<<" argument missmatch: "<<inputs.size()<<" formal parameters, but only got "<<node.multi_mother.size()<<" actuals.\n"<<node.lprint());
      
      
      vector<bool_node*> inp_arg;
      
      if (shouldReplaceSpecialNode) {
        Assert(inputs.size() > 0, "Number of inputs should be atleast 1");
        bool_node* actual = node.multi_mother[0];
        
        Assert(actual->getOtype()->isTuple, "First input should be a tuple");
        int size = ((Tuple*) (actual->getOtype()))->entries.size();
        TUPLE_R_node* tr = new TUPLE_R_node();
        tr->idx = 0;
        tr->mother = actual;
        tr->addToParents();
        
        EQ_node* nullCheck = new EQ_node();
        nullCheck->mother = actual;
        nullCheck->father = getCnode(-1);
        nullCheck->addToParents();
        
        NOT_node* notNull = new NOT_node();
        notNull->mother = optAdd(nullCheck);
        notNull->addToParents();
        
        AND_node* cond = new AND_node();
        cond->mother = optAdd(notNull);
        
        EQ_node* eq = new EQ_node();
        eq->mother = optAdd(tr);
        eq->father = getCnode(20); // magic number
        eq->addToParents();
        cond->father = optAdd(eq);
        cond->addToParents();
        
        bool_node* optEq = optAdd(cond);
        spCond = optEq;
        
        vector<bool_node*> newOutputs;
        
        int state_size = inputs.size() - 1;
        Tuple* retType = (Tuple*) (node.getOtype());
        int outSize = retType->entries.size() - state_size;
        
        
        NOT_node* not_ = new NOT_node();
        AND_node* and_ = new AND_node();
        and_->mother = node.mother;
        and_->father = optEq;
        and_->addToParents();
        
        not_->mother = optAdd(and_);
        not_->addToParents();
        bool_node* optNot = optAdd(not_);
        
        for (int i = 0; i < state_size; i++) {
          bool_node* actSt = node.multi_mother[i+1];
          
          TUPLE_R_node* tr2 = new TUPLE_R_node();
          tr2->idx = size - outSize - state_size + i;
          tr2->mother = actual;
          tr2->addToParents();
          
          newOutputs.push_back(optAdd(tr2));
    
          OR_node* or_ = new OR_node();
          
          or_->mother = optNot;
          
          EQ_node* eq1 = new EQ_node();
          eq1->mother = actSt;
          TUPLE_R_node* tr3 = new TUPLE_R_node();
          tr3->idx = size - outSize - 2*state_size + i;
          tr3->mother = actual;
          tr3->addToParents();
          
          eq1->father = optAdd(tr3);
          eq1->addToParents();
          
          or_->father = optAdd(eq1);
          or_->addToParents();
          
          ASSERT_node* stateEq = new ASSERT_node();
          stateEq->setMsg("state equivalence");
          stateEq->mother = optAdd(or_);
          stateEq->makeHardAssert();
          stateEq->addToParents();
          this->addNode(stateEq);
          DllistNode* tt = getDllnode(stateEq);
          tmpList.append(tt);
          callMap.clear();
        }
        
        for (int i = 0; i < outSize; i++) {
          TUPLE_R_node* tr1 = new TUPLE_R_node();
          tr1->idx = size - outSize + i;
          tr1->mother = actual;
          tr1->addToParents();
          
          newOutputs.push_back(optAdd(tr1));
        }
        
        newOutputTup = new TUPLE_CREATE_node();
        newOutputTup->setName(retType->name);
        
        Assert(((Tuple*)newOutputTup->getOtype())->entries.size() == newOutputs.size(), "Something is wrong here");
        for (int i = 0; i < newOutputs.size(); i++) {
          newOutputTup->multi_mother.push_back(newOutputs[i]);
        }
        
        newOutputTup->addToParents();
        addNode(newOutputTup);
        
        
        node.mother->children.erase((bool_node*)&node);
        
        bool_node* new_mother = new AND_node();
        new_mother->mother = node.mother;
        new_mother->father = optNot;
        
        new_mother->addToParents();
        
        node.mother = optAdd(new_mother);
        node.mother->children.insert(&node);
        
      }
			
			for(int i=0; i<inputs.size(); ++i){
				
				bool_node* actual = node.multi_mother[i];
        
        nmap[inputs[i]->id] = actual;
				actual->children.erase((bool_node*)&node);
//				actual->children.insert(inputs[i]->children.begin(), inputs[i]->children.end());				
			}
		}
		//cout<<endl;
		{
			vector<bool_node*>& controls  = oldFun.getNodesByType(bool_node::CTRL);
			
			for(int i=0; i<controls.size(); ++i){	
				CTRL_node* ctrl = dynamic_cast<CTRL_node*>(controls[i]);
				if(ctrl->get_Pcond()){
					nmap[ctrl->id] = node.mother;
					continue;
				}
				if(randomize){
					bool_node* subst = hcoder->checkRandHole(ctrl, *this);
					if(subst != ctrl){
						nmap[ctrl->id] = subst;
						continue;
					}
				}

				bool_node* actual = dag.unchecked_get_node( ctrl->name );

				

				if(actual != NULL){
					nmap[controls[i]->id] = actual;
					actual->children.erase((bool_node*)&node);
//					actual->children.insert(controls[i]->children.begin(), controls[i]->children.end());
				}else{
					if(seenControls.count(ctrl->name)>0){						
						nmap[controls[i]->id] = seenControls[ctrl->name];
					}else{
						bool_node* tmp = controls[i]->clone(false);
						addNode( tmp );
						nmap[controls[i]->id] = tmp;
						seenControls[ctrl->name] = tmp;
					}
				}
			}
		}
		
		lnfuns++;
		
		// cout<<node.get_ufname()<<endl;

		/*
			The idea is that we have a wavefront moving through the graph as we add more nodes.
			n is going to be in the boundary of this wavefront. The parents of n will be good nodes,
			but their children will still have pointers to oldFun. 

		*/

		vector<const bool_node*> secondarynmap;
		secondarynmap.resize( oldFun.size(), NULL );
		map<const UFUN_node*, SRC_node*> ufToSrc;
		
		bool doubleCopies = false;

		
		bool_node* output = NULL;
		bool_node* assertCond = NULL;

		for(int i=0; i<oldFun.size(); ++i){
			
			bool_node::Type t = oldFun[i]->type;


			if(t == bool_node::SRC || t == bool_node::CTRL) continue;
			if(t == bool_node::CONST){
				CONST_node* n =  dynamic_cast<CONST_node*>(oldFun[i]);
				if(n->isFloat()){
					nmap[n->id] = getCnode(n->getFval());
				}else{
					nmap[n->id] = getCnode(n->getVal());
				}
				
				continue;
			}
			bool_node* n =  oldFun[i]->clone(false);
			//bool_node* of = oldFun[i];
			n->redirectParentPointers(oldFun, nmap, true, oldFun[i]);			

			bool_node* nprime = NULL;

			if(doubleCopies){				
				if(checkParentsInMap(oldFun[i], secondarynmap, nmap)){
					nprime = oldFun[i]->clone(false);					
					nprime->redirectParentPointers(oldFun, secondarynmap, true, oldFun[i]);
				}
			}

			int nodeId = n->id;
			if( n != NULL &&  n->type != bool_node::DST ){			
				if(n->type != bool_node::ASSERT){
					if(typeid(*n) != typeid(UFUN_node)){
						
						optAndAdd(n, nmap);
						if(nprime != NULL){
							optAndAdd(nprime, secondarynmap);
						}						

					}else{			
						UFUN_node* ufun = dynamic_cast<UFUN_node*>(n);						
						ufun->ignoreAsserts = ufun->ignoreAsserts || node.ignoreAsserts;
            
            if (node.hardAsserts()) {
              ufun->makeAssertsHard();
            }

						if(!ufun->ignoreAsserts){													
							DllistNode* tt = getDllnode(ufun);
							tmpList.append(tt);
						}

						{
							/*if(oldToNew.count(ufun->fgid)>0){
								ufun->fgid = oldToNew[ufun->fgid];
							}else{
								++uidcount;
								oldToNew[ufun->fgid] = uidcount;
								ufun->fgid = uidcount;
							}*/
						}

						bool_node * oldMother = ufun->mother;	
						if(!ufun->ignoreAsserts){	
							/*
							ufun->mother->remove_child( n );
							bool_node* andCond = new AND_node();
							andCond->mother = ufun->mother;
							andCond->father = condition;

							{
							
								bool_node* andcondPrime = this->computeOptim(andCond);
							
								if(andcondPrime == andCond){
								
									this->addNode(andCond);
									andCond->addToParents();
								}else{
									delete andCond;
								}
								andCond = andcondPrime;
							}
							ufun->mother = andCond;
							ufun->mother->children.insert(n);
							if(andCond != oldMother){
								for(vector<bool_node*>::iterator it = ufun->multi_mother.begin(); it != ufun->multi_mother.end(); ++it){
									if(*it == oldMother){
  										oldMother->children.insert( n );
										break;
									}
								  }
							}
							*/
						}else{
							if(!isConst(ufun->mother)){
								ufun->mother->remove_child(n);
								ufun->mother = getCnode(1);
								ufun->mother->children.insert(n);
								for(vector<bool_node*>::iterator it = ufun->multi_mother.begin(); it != ufun->multi_mother.end(); ++it){
									if(*it == oldMother){
  										oldMother->children.insert( n );
										break;
									}
								  }
							}
						}
						DagOptim::visit(*ufun);						
						const bool_node* nnode = rvalue;
						if( nnode->type == bool_node::UFUN ){
							//&& !(isConst(oldMother) && this->getIval(oldMother)==0)
							if(ictrl != NULL){ ictrl->registerCall(node, dynamic_cast<const UFUN_node*>(nnode)); }
						}
						if(nnode == n){
							//bool_node* tmp = n->clone();
							// cse.setCSE(tmp);
							// replaceDummy(n, tmp);
							
							this->addNode( n );
							nmap[nodeId] = n;							
							nnode = n;
						}else{
							n->dislodge();
							delete n;
							//n->removeFromParents(of);
							nmap[nodeId] = nnode;
						}
						if(nprime != NULL){
							nprime->dislodge();
							delete nprime;
						}
						if(oldFun.isModel && nnode->type == bool_node::UFUN ){
							const UFUN_node* ufn = dynamic_cast<const UFUN_node*>(nnode);
							//Add an SRC node to stand in place of this ufun.
							string nm = ufn->get_ufname();
							nm += "_";
							nm += ufn->outname;
							SRC_node* sn = new SRC_node(nm);
                            sn->setTuple(ufn->getTupleName());
							// BUGFIX: this is not wide enough! ufn->nbits is either 1 or 2, set by InputParser.cpp
							//sn->set_nbits( ufn->get_nbits() );
							//cout << "DagFunctionInliner: ufn=" << ufn->lprint() << " nbits=" << ufn->get_nbits() << " isArr=" << ufn->isArr() << endl;
							// BUGFIX: nbits must be 1 if original UFUN out is boolean type
							int nbits = ufn->get_nbits();
							if (nbits > 1) { nbits = PARAMS->NANGELICS; }
							sn->set_nbits(nbits);
							// BUGFIX: need to setArr if UFUN out is array. related to Issue #5.
							// TODO xzl: is this correct? is it necessary?
							if (ufn->isArr()) {
								//cout << "sn " << sn->lprint() << " ufn " << ufn->lprint() << endl;
								sn->setArr(PARAMS->angelic_arrsz);
							}
							doubleCopies = true;
							secondarynmap[nodeId] = sn;
							ufToSrc[ufn] = sn;
						}

					}
				}else{
					n->mother->remove_child( n );		
					if(nprime != NULL){
						nprime->mother->remove_child( nprime );
					}
					//Assertion.					


					if((isConst(n->mother) && getIval(n->mother) == 1)|| (!oldFun.isModel && node.ignoreAsserts ) ){ 					
						delete n;
						if(nprime != NULL){							
							delete nprime;
						}
						continue;
					}					

					if(!oldFun.isModel || nprime == NULL){	
						/*
						bool_node* nnode = new NOT_node();
						nnode->mother = condition;
						nnode = optAndAddNoMap(nnode);
					
					
						bool_node* cur = n->mother;
						bool_node* ornode = new OR_node();
						ornode->mother = cur;
						ornode->father = nnode;
						ornode = optAndAddNoMap(ornode);
						n->mother = ornode;
						*/
						
            if (node.hardAsserts()) {
              dynamic_cast<ASSERT_node*>(n)->makeHardAssert();
            }
					}else{
						bool_node* cur = n->mother;
						n->mother = nprime->mother;
						delete nprime;
						dynamic_cast<ASSERT_node*>(n)->makeHardAssert();
						if(assertCond == NULL){
							assertCond = cur;							
						}else{			
							bool_node* tt = new AND_node();
							tt->mother = assertCond;
							tt->father = cur;
							tt->addToParents();
							addNode(tt);							
							assertCond = tt;
						}
					}
					

					{							
						DllistNode* tt = getDllnode(n);
						tmpList.append(tt);
					}
					n->mother->children.insert(n);	
					
					this->addNode(n);
					if(!dynamic_cast<ASSERT_node*>(n)->isNormal()){						
						callMap.clear();						
					}
				}
			}else{
				if( n!= NULL){
					DST_node* dn = dynamic_cast<DST_node*>(n);
					bool_node* ttv = n->mother;
					if(oldFun.isModel){
						if(assertCond == NULL){
                       
                            TUPLE_CREATE_node* outTuple = dynamic_cast<TUPLE_CREATE_node*>(ttv);
                            TUPLE_R_node* tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->multi_mother[0]);
                            UFUN_node* un = dynamic_cast<UFUN_node*>(tupleRead->mother);
                            SRC_node* sc = ufToSrc[un];
                            sc->neighbor_replace(ttv);
                            sc->children.clear();
                               
                        
						}else{
                            Assert(ttv->type == bool_node::TUPLE_CREATE,"dfwer" );
                            TUPLE_CREATE_node* outTuple = dynamic_cast<TUPLE_CREATE_node*>(ttv);
                            Assert(outTuple->multi_mother[0]->type == bool_node::TUPLE_R,"rwtr");
                            int numOutputs = outTuple->multi_mother.size();
                            TUPLE_R_node* tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->multi_mother[0]);
                            UFUN_node* un = dynamic_cast<UFUN_node*>(tupleRead->mother);
                            
                            TUPLE_CREATE_node* newOutTuple = new TUPLE_CREATE_node();
                            newOutTuple->setName(outTuple->name);
                            
                            for ( int p = 0; p < numOutputs; p++) {
                                CTRL_node* qn = new CTRL_node();
                                stringstream st;
                                st<<dn->name;
                                st<<lnfuns;
                                st<<"_"<<p;
                                
                                qn->name = st.str();
                                qn->set_Angelic();
                            
                                ARRACC_node* mx = new ARRACC_node();
                                if(!PARAMS->angelic_model)
                                    mx->mother = assertCond;
                                else{
                                    //create new src node which should be valued to false in Synth phase
                                    SRC_node *st = new SRC_node("__rs_node");
                                    st->set_nbits(1);//just one bit input
                                    mx->mother = st;
                                    addNode(st);
                                    
                                }
                                Assert(outTuple->multi_mother[0]->type == bool_node::TUPLE_R,"rwtr");
                                tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->multi_mother[p]);
                                
                                mx->multi_mother.push_back(qn);
                                mx->multi_mother.push_back(tupleRead);
                                mx->addToParents();
                                addNode(qn);
                                addNode(mx);
                                
                                OutType* outputType = tupleRead->getOtype();
                                // BUGFIX: nbits must be 1 if original UFUN out is boolean type
                                int nbits = 0;
                                if (outputType == OutType::BOOL || outputType == OutType::BOOL_ARR) {
                                    nbits = 1;
                                }
                                if (outputType == OutType::INT || outputType == OutType::INT_ARR) {
                                    nbits = 2;
                                }
                                if (nbits > 1) { nbits = PARAMS->NANGELICS; }
                                qn->set_nbits(nbits);
                                
                                // BUGFIX xzl: fix Issue #5, when angelic CTRL is an array
                                if (outputType == OutType::INT_ARR || outputType == OutType::BOOL_ARR) {
                                    //cout << "qn " << qn->lprint() << " un " << un->lprint() << endl;
                                    qn->setArr(PARAMS->angelic_arrsz);
                                }
                                newOutTuple->multi_mother.push_back(mx);
                                
                            
                            }
                            SRC_node* sc = ufToSrc[un];
                            newOutTuple->addToParents();
                            addNode(newOutTuple);
                            sc->neighbor_replace(newOutTuple);
                            sc->children.clear();
                            ttv = newOutTuple;

                             
						}
																		
						nprime->dislodge();
						delete nprime;
					}
					//mpcontroller[node.fgid][dn->name] = ttv;
					if(dn->name == node.outname){
						output = ttv;
					}										
					n->dislodge();
					delete n;
				}
			}
		}

		for(map<const UFUN_node*, SRC_node*>::iterator it = ufToSrc.begin(); it != ufToSrc.end(); ++it){
			if(it->second->children.size() == 0){
				delete it->second;
			}else{
				Assert(false, "NYI: All the outputs of a function in a model should be outputed by the model.");
			}
		}

		node.add(&tmpList);
		node.remove();
    
    if (shouldReplaceSpecialNode) {
      ARRACC_node* an = new ARRACC_node();
      an->mother = spCond;
      an->multi_mother.push_back(output);
      an->multi_mother.push_back(newOutputTup);
      
      an->addToParents();
      
      rvalue = optAdd(an);
      
    } else {
      rvalue = output;
    }
		if(rvalue == NULL){
			rvalue = getCnode(0);
		}
		somethingChanged = true;
	}else{
		DagOptim::visit(node);
	}
	ufunAll.stop();	
}




/*
extern map<string, pair<int, int> > sizes;
*/

void DagFunctionInliner::process(BooleanDAG& dag){
	// cout<<" funmap has size " << functionMap.size() << endl;
	initLight(dag);
	funsInlined.clear();
	somethingChanged = false;
	lnfuns = 0;
	uidcount = 0;
	if(ictrl != NULL){
		DllistNode* lastDln = NULL;
		for(int i=0; i<dag.size() ; ++i ){

			if(isDllnode(dag[i])){
				lastDln = getDllnode(dag[i]);
			}

			// Get the code for this node.				
			if(dag[i]->type == bool_node::UFUN){
				UFUN_node& uf = *dynamic_cast<UFUN_node*>(dag[i]);
				//uidcount = max(uidcount, uf.fgid);
				/*
				When the inline controller checks a function and the function is 
				not inlined, the controller makes sure other function with the same path 
				condition are also not inlined. Therefore, it is good to call checkInline
				on all functions first, before we start inlining.
				*/
				ictrl->preCheckInline(uf);
				map<string, BooleanDAG*>::iterator it = functionMap.find(uf.get_ufname());
				if(it != functionMap.end()){
					if(it->second->isModel && uf.ignoreAsserts){
						if(lastDln != NULL){
							lastDln->add(&uf);
						}else{
							dag.assertions.append(&uf);
						}
					}
				}
			}
		}
	}
	
    //dag.lprint(cout);
	mpcontroller.clear();

	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
        //cout<<dag[i]->lprint()<<endl;
		bool_node* node = computeOptim(dag[i]);
       if(dag[i] != node){
                Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				dag.replace(i, node);
		}
	}

	// cout<<" added nodes = "<<newnodes.size()<<endl;
    seenControls.clear();
    cleanup(dag);
}


