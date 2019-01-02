#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"
#include "CommandLineArgs.h"
#include <sstream>
#include "NodeEvaluator.h"


//extern CommandLineArgs* PARAMS;

#ifndef  _MSC_VER
static const int MAX_NODES = 1000000;
#else
static const int MAX_NODES = 510000;
#endif

DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap,  map<string, map<string, string> > p_replaceMap, FloatManager& fm, HoleHardcoder* p_hcoder, const set<string>& p_pureFunctions,
	bool p_randomize, InlineControl* ict, bool p_onlySpRandomize, int p_spRandBias):
dag(p_dag), 
DagOptim(p_dag, fm), 
ufunAll(" ufun all"),
functionMap(p_functionMap),
replaceMap(p_replaceMap),
ictrl(ict),
randomize(p_randomize),
onlySpRandomize(p_onlySpRandomize),
spRandBias(p_spRandBias),
replaceDepth(0),
pureFunctions(p_pureFunctions),
hcoder(p_hcoder),
symbolicSolve(false)
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
		return sizeof(DST_node);
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



void DagFunctionInliner::visit(CTRL_node& node){
	
	
	if(node.get_Pcond()){
		rvalue = this->getCnode(true);
	}else{
		if(randomize){
		  if (node.is_sp_concretize()) {
			rvalue = hcoder->checkRandHole(&node, *this);
			return;
		  }
		  if (!onlySpRandomize) {
			rvalue = hcoder->checkRandHole(&node, *this);
			return;
		  }
		}
		else {
			int val;
			if (hcoder->isSettled(node.get_name(), val)) {
				rvalue = this->getCnode(val);
				return;
			}
		}		
		DagOptim::visit(node);
	}
}


bool checkParentsInMap(bool_node* node, vector<const bool_node*>& secondarynmap, vector<const bool_node*>& nmap){
	bool chk = false;
	bool_node* tt;
	for (auto it = node->p_begin(); it != node->p_end(); ++it) {
		tt = *it;
		chk = chk || (tt != NULL && secondarynmap[tt->id] != NULL);
	}

	if(chk){
		for (auto it = node->p_begin(); it != node->p_end(); ++it) {
			tt = *it;
			if (tt != NULL && secondarynmap[tt->id] == NULL) {
				secondarynmap[tt->id] = nmap[tt->id];
			}
		}
	}
	return chk;
}

bool_node* DagFunctionInliner::createTupleAngelicNode(string tuple_name, string node_name, int depth) {
  if (depth == 0) return getCnode(-1);
  
  Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
  TUPLE_CREATE_node* new_node = TUPLE_CREATE_node::create(tuple_type->entries.size());
  new_node->setName(tuple_name);
  int size = tuple_type->actSize;

  for (int j = 0; j < size ; j++) {
    stringstream str;
    str<<node_name<<"_"<<j;
    
    OutType* type = tuple_type->entries[j];
    
    if (type->isTuple) {
      new_node->set_parent(j, createTupleAngelicNode(((Tuple*)type)->name, str.str(), depth - 1));
    } else {
      
      CTRL_node* src =  CTRL_node::create();
      src->name = str.str();
      src->set_Special_Angelic();
      
      int nbits = 0;
      if (type == OutType::BOOL || type == OutType::BOOL_ARR) {
        nbits = 1;
      }
      if (type == OutType::INT || type == OutType::INT_ARR) {
        nbits = 5;
      }
      
      src->set_nbits(nbits);
      
      if(type == OutType::INT_ARR || type == OutType::BOOL_ARR) {
        src->setArr(PARAMS->angelic_arrsz);
      }
      addNode(src);
      
      new_node->set_parent(j, src);
    }
  }
  
  for (int i = size; i < tuple_type->entries.size(); i++) {
    new_node->set_parent(i, getCnode(-1));
  }
  new_node->addToParents();
  
  
  stringstream str;
  str<<node_name<<"__";
  
  CTRL_node* src = CTRL_node::create();
  src->name = str.str();
  src->set_nbits(1);
  src->set_Special_Angelic();
  addNode(src);
  
  ARRACC_node* ac = ARRACC_node::create(src, getCnode(-1), optAdd(new_node));
  
  ac->addToParents();
  return optAdd(ac);
}

bool_node* DagFunctionInliner::createEqNode(bool_node* left, bool_node* right, int depth) {
  if (depth == 0) {
    EQ_node* eq = EQ_node::create();
    eq->mother() = left;
    eq->father() = right;
    eq->addToParents();
    return optAdd(eq);
  }
  OutType* ltype = left->getOtype();
  OutType* rtype = right->getOtype();
  
  if (!ltype->isTuple && !rtype->isTuple) {
    EQ_node* eq = EQ_node::create();
    eq->mother() = left;
    eq->father() = right;
    eq->addToParents();
    return optAdd(eq);
  } else if (!ltype->isTuple) {
    EQ_node* eq = EQ_node::create();
    eq->mother() = right;
    eq->father() = getCnode(-1);
    eq->addToParents();
    return optAdd(eq);
  } else if (!rtype->isTuple) {
    EQ_node* eq = EQ_node::create();
    eq->mother() = left;
    eq->father() = getCnode(-1);
    eq->addToParents();
    return optAdd(eq);
  } else {
    int lentries = ((Tuple*)ltype)->actSize;
    int rentries = ((Tuple*)rtype)->actSize;
    Assert(lentries == rentries, "Wrong types");
    bool_node* cur=NULL;
    for (int i = 0; i < lentries; i++) {
      TUPLE_R_node* left_r = TUPLE_R_node::create();
      left_r->idx = i;
      left_r->mother() = left;
      left_r->addToParents();
      
      TUPLE_R_node* right_r = TUPLE_R_node::create();
      right_r->idx = i;
      right_r->mother() = right;
      right_r->addToParents();
      
      if (i == 0) {
        cur = createEqNode(optAdd(left_r), optAdd(right_r), depth - 1);
      } else {
        AND_node* an = AND_node::create();
        an->mother() = cur;
        an->father() = createEqNode(optAdd(left_r), optAdd(right_r), depth-1);
        an->addToParents();
        
        cur = optAdd(an);
      }
      
    }
    return cur;
  }
}

void DagFunctionInliner::visit( UFUN_node& node ){
	Dllist tmpList;
	const string& name = node.get_ufname();
	map<int, int> oldToNew;



	if(ictrl != NULL && !ictrl->checkInline(node)){		
		DagOptim::visit(node);
		return;
	}	


	bool_node* condition = node.mother();
	if(isConst(condition) && !getBval(condition)){
		rvalue = getCnode(0);		
		return;
	}
  
  map<string, map<string, string> >::iterator it = replaceMap.find(name);
  if (it != replaceMap.end()) {
    if (ictrl != NULL && ictrl->isRecursive(node)) {
      cout << "Replacing ufun node " << name << endl;
      string oldOutputType = node.getTupleName();
      
      int oriInpSize = node.nargs();
      
      
    
      Tuple* oldOutputTuple = (Tuple*)OutType::getTuple(oldOutputType);
      Tuple* tup = (Tuple*) (oldOutputTuple->entries[oldOutputTuple->entries.size() - 1]);
      
	  int size = tup->entries.size();
	  TUPLE_CREATE_node* tuple_node = TUPLE_CREATE_node::create(size);
	  
	  tuple_node->setName(tup->name);
      
      tuple_node->set_parent(0, getCnode(20)); // TODO: get rid of this magic number
      
      int actFields = size - oriInpSize;

      for (int i = 1; i <  actFields; i++) {
        tuple_node->set_parent(i, getCnode(0));
      }
      
      for (int i = 0; i < oriInpSize; i++) {
        if (i == 0) {
          Assert(isConst(node.arguments(0)) || node.arguments(0)->getOtype()->isTuple, "First node must be a tuple");
          int d = node.arguments(0)->depth;
          if (d == -1 || replaceDepth == -1) replaceDepth = -1;
          else if (d > replaceDepth) {
            replaceDepth = d;
          }
        }
        tuple_node->set_parent(actFields + i, node.arguments(i));
      }
      
      Assert(tuple_node->nparents() == size, "Dfqwq");
      
     
      tuple_node->addToParents();
      
      TUPLE_CREATE_node* new_output = TUPLE_CREATE_node::create(1);
      new_output->setName(oldOutputType);
      
      new_output->set_parent(0, optAdd(tuple_node));
      new_output->addToParents();
      rvalue = optAdd(new_output);
      return;
    } else {
      int d = node.arguments(0)->depth - 1;
      if (d == -1 || replaceDepth == -1) replaceDepth = -1;
      else if (d > replaceDepth) {
        replaceDepth = d;
      }
    }
  }
  
  bool shouldReplaceSpecialNode = false;
  string replaceFunName = "";
  if (node.replaceFun) {
  map<string, map<string, string > >::iterator it1 = replaceMap.begin();
  
  for(; it1 != replaceMap.end(); it1++) {
    map<string, string> fmap = it1->second;
    if (fmap.find(name) != fmap.end()) {
      shouldReplaceSpecialNode = true;
      replaceFunName = fmap[name];
      break;
    }
  }
  } 
  
	ufunAll.restart();


	if( functionMap.find(name) != functionMap.end() ){
    //cout << "Inlining " << name << endl;
				
		BooleanDAG& oldFun = *functionMap[name];

		if(oldFun.isModel && node.children.size() == 0){
			rvalue = getCnode(false);
			return;
		}

		funsInlined.insert(name);
		if(ictrl != NULL){ ictrl->registerInline(node); }


	

		

		//oldFun->clone_nodes(clones);
		vector<const bool_node*> nmap;
    bool_node* newOutput;
    bool_node* spCond;
		nmap.resize( oldFun.size() );
		{
			vector<bool_node*>& inputs  = oldFun.getNodesByType(bool_node::SRC);
			// ADT, int, int ... (state)
			Assert( inputs.size() == node.nargs() , node.get_ufname()<<" argument missmatch: "<<inputs.size()<<" formal parameters, but only got "<<node.nargs()<<" actuals.\n"<<node.lprint());

      vector<bool_node*> inp_arg;

      if (shouldReplaceSpecialNode) {
        Assert(inputs.size() > 0, "Number of inputs should be atleast 1");
        
        string newNameCaps = replaceFunName;
        for(int i = 0; i < newNameCaps.size(); i++) {
          newNameCaps.at(i) = toupper(newNameCaps.at(i));
        }
        string newOutputType = newNameCaps + "_ANONYMOUS";
        
		
		vector<bool_node*> newparams;
        bool_node* actual=NULL;
        for (int i = 0; i < inputs.size(); i++) {
			actual = node.arguments(i);
			if (actual->getOtype()->isTuple) break;
			newparams.push_back(actual);
        }
        if (isConst(actual)) {
			shouldReplaceSpecialNode = false;
			
        } else {
			//Assert(actual->getOtype()->isTuple, "First input should be a tuple");
			int size = ((Tuple*) (actual->getOtype()))->entries.size();
			int actSize = ((Tuple*) (actual->getOtype()))->actSize;
			TUPLE_R_node* tr = TUPLE_R_node::create();
			tr->idx = 0;
			tr->mother() = actual;
			tr->addToParents();
        
			EQ_node* nullCheck = EQ_node::create();
			nullCheck->mother() = actual;
			nullCheck->father() = getCnode(-1);
			nullCheck->addToParents();
        
			NOT_node* notNull = NOT_node::create();
			notNull->mother() = optAdd(nullCheck);
			notNull->addToParents();
        
			AND_node* cond = AND_node::create();
			cond->mother() = optAdd(notNull);
        
			EQ_node* eq = EQ_node::create();
			eq->mother() = optAdd(tr);
			eq->father() = getCnode(20); // magic number
			eq->addToParents();
			cond->father() = optAdd(eq);
			cond->addToParents();
        
			bool_node* optEq = optAdd(cond);
			spCond = optEq;
        
			NOT_node* not_ = NOT_node::create();
			AND_node* and_ = AND_node::create();
			and_->mother() = node.mother();
			and_->father() = optEq;
			and_->addToParents();
        
			bool_node* optCond = optAdd(and_);
			not_->mother() = optCond;
			not_->addToParents();
			bool_node* optNot = optAdd(not_);
        

			
        
			for (int i = actSize; i < size; i++) {
			  TUPLE_R_node* tr1 = TUPLE_R_node::create();
			  tr1->idx = i;
			  tr1->mother() = actual;
			  tr1->addToParents();
			  bool_node* optnode = optAdd(tr1);
			  if (i == actSize) {
				optnode->depth = replaceDepth;
			  }
			  newparams.push_back(optnode);
			}
        
			BooleanDAG& newFun = *functionMap[replaceFunName];
			vector<bool_node*>& new_inputs  = newFun.getNodesByType(bool_node::SRC);
			int curSize = newparams.size();
			for (int i = curSize; i < new_inputs.size(); i++) {
			  int j = i + actSize - size + 1;
			  Assert(j < inputs.size(), "dfae");
			  newparams.push_back(node.arguments(j));
			}
      
			UFUN_node* repFun = UFUN_node::create(replaceFunName, newparams);
			repFun->outname = "_p_out_" + replaceFunName + "_ANONYMOUS"; // TODO: fix these magic strings
			repFun->set_tupleName(newOutputType);
			repFun->replaceFun = false;
			repFun->mother() = optCond;

			if(ictrl != NULL){
			  int numRec = ictrl->numRecursive(node);
			  ictrl->registerCallWithLimit(node, repFun, ictrl->getLimit() - numRec);
			}
			repFun->addToParents();
			this->addNode(repFun);
			newOutput = repFun;
        
			node.mother()->children.erase((bool_node*)&node);
        
			bool_node* new_mother = AND_node::create();
			new_mother->mother() = node.mother();
			new_mother->father() = optNot;
        
			new_mother->addToParents();
        
			node.mother() = optAdd(new_mother);
			node.mother()->children.insert(&node);
        }
        
      }

		for(int i=0; i<inputs.size(); ++i){
			bool_node* actual;
			if (node.arguments(i)->getOtype()->isTuple && node.arguments(i)->depth == 0) {
				actual = getCnode(-1);
			} else {
				actual = node.arguments(i);
			}
        
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
					nmap[ctrl->id] = node.mother();
					continue;
				}
        if(randomize){
          if (ctrl->is_sp_concretize()) {
            bool_node* subst = hcoder->checkRandHole(ctrl, *this);
            if(subst != ctrl){
              nmap[ctrl->id] = subst;
              continue;
            }
          } else if (!onlySpRandomize) {
            bool_node* subst = hcoder->checkRandHole(ctrl, *this);
            if(subst != ctrl){
              nmap[ctrl->id] = subst;
              continue;
            }
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
						ufun->set_uniquefid();
						if (node.hardAsserts()) {
						  ufun->makeAssertsHard();
						}
						
						if (ufun->getOtype()->str() == "_GEN_Solver") {
							symbolicSolve = true;
						}


						if (!node.replaceFun) {
						  ufun->replaceFun = false;
						}

						if(!ufun->ignoreAsserts){
							DllistNode* tt = getDllnode(ufun);
							tmpList.append(tt);
						}

						bool_node * oldMother = ufun->mother();
						
						const bool_node* nnode;
						if (!ufun->ignoreAsserts &&  isConst(oldMother) && this->getBval(oldMother) && !oldFun.isModel && pureFunctions.count(node.get_ufname()) > 0) {
							//cout << "Pre inlining " << ufun->get_ufname() << endl;
							if (ictrl != NULL) { ictrl->registerCall(node, ufun); }							
							visit(*ufun);
							nnode = rvalue;
							if (ictrl != NULL) { ictrl->registerInline(node); }
						}
						else {
							DagOptim::visit(*ufun);
							nnode = rvalue;
							if (nnode->type == bool_node::UFUN) {
								//&& !(isConst(oldMother) && this->getIval(oldMother)==0)
								if (ictrl != NULL) { ictrl->registerCall(node, dynamic_cast<const UFUN_node*>(nnode)); }
							}
						}


						
						
						if(nnode == n){							
							
							this->addNode( n );
							nmap[nodeId] = n;
							nnode = n;
						}else{
							n->dislodge();
							delete n;							
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
							SRC_node* sn = SRC_node::create(nm);
                            sn->setTuple(ufn->getTupleName(), true);
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
					n->mother()->remove_child( n );
					if(nprime != NULL){
						nprime->mother()->remove_child( nprime );
					}
					//Assertion.


					if((isConst(n->mother()) && getIval(n->mother()) == 1)|| (!oldFun.isModel && node.ignoreAsserts ) ){
						delete n;
						if(nprime != NULL){
							delete nprime;
						}
						continue;
					}

					if(!oldFun.isModel || nprime == NULL){
						/*
						bool_node* nnode = new NOT_node();
						nnode->mother() = condition;
						nnode = optAndAddNoMap(nnode);
					
					
						bool_node* cur = n->mother();
						bool_node* ornode = new OR_node();
						ornode->mother() = cur;
						ornode->father() = nnode;
						ornode = optAndAddNoMap(ornode);
						n->mother() = ornode;
						*/

            if (node.hardAsserts()) {
              dynamic_cast<ASSERT_node*>(n)->makeHardAssert();
            }
					}else{
						bool_node* cur = n->mother();
						n->mother() = nprime->mother();
						delete nprime;
						dynamic_cast<ASSERT_node*>(n)->makeHardAssert();
						if(assertCond == NULL){
							assertCond = cur;
						}else{
							bool_node* tt = AND_node::create();
							tt->mother() = assertCond;
							tt->father() = cur;
							tt->addToParents();
							addNode(tt);
							assertCond = tt;
						}
					}
					

					{
						DllistNode* tt = getDllnode(n);
						tmpList.append(tt);
					}
					n->mother()->children.insert(n);
					
					this->addNode(n);
					if(!dynamic_cast<ASSERT_node*>(n)->isNormal()){
						callMap.clear();
					}
				}
			}else{
				if( n!= NULL){
					DST_node* dn = dynamic_cast<DST_node*>(n);
					bool_node* ttv = n->mother();
					if(oldFun.isModel){
						if(assertCond == NULL){
                       
                            TUPLE_CREATE_node* outTuple = dynamic_cast<TUPLE_CREATE_node*>(ttv);
                            TUPLE_R_node* tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->get_parent(0));
                            UFUN_node* un = dynamic_cast<UFUN_node*>(tupleRead->mother());
                            SRC_node* sc = ufToSrc[un];
                            sc->neighbor_replace(ttv);
                            sc->children.clear();
                               
                        
						}else{
                            Assert(ttv->type == bool_node::TUPLE_CREATE,"dfwer" );
                            TUPLE_CREATE_node* outTuple = dynamic_cast<TUPLE_CREATE_node*>(ttv);
                            Assert(outTuple->get_parent(0)->type == bool_node::TUPLE_R,"rwtr");
                            int numOutputs = outTuple->nparents();
                            TUPLE_R_node* tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->get_parent(0));
                            UFUN_node* un = dynamic_cast<UFUN_node*>(tupleRead->mother());
                            
                            TUPLE_CREATE_node* newOutTuple = TUPLE_CREATE_node::create(numOutputs);
                            newOutTuple->setName(outTuple->name);
                            
                            for ( int p = 0; p < numOutputs; p++) {
                                CTRL_node* qn = CTRL_node::create();
                                stringstream st;
                                st<<dn->name;
                                st<<lnfuns;
                                st<<"_"<<p;
                                
                                qn->name = st.str();
                                qn->set_Angelic();
                            
                                ARRACC_node* mx = ARRACC_node::create(2);
                                if(!PARAMS->angelic_model)
                                    mx->mother() = assertCond;
                                else{
                                    //create new src node which should be valued to false in Synth phase
                                    SRC_node *st = SRC_node::create("__rs_node");
                                    st->set_nbits(1);//just one bit input
                                    mx->mother() = st;
                                    addNode(st);
                                    
                                }
                                Assert(outTuple->get_parent(0)->type == bool_node::TUPLE_R,"rwtr");
                                tupleRead  = dynamic_cast<TUPLE_R_node*>(outTuple->get_parent(p));
                                
                                mx->arguments(0) = qn;
								mx->arguments(1) = tupleRead;
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
                                newOutTuple->set_parent(p, mx);
                                
                            
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
      ARRACC_node* an = ARRACC_node::create(spCond, output, newOutput);
      
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
	failedAssert = NULL;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
        //cout<<dag[i]->lprint()<<endl;
		bool_node* node = computeOptim(dag[i]);
       if(dag[i] != node){
                Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				dag.replace(i, node);
		}
	   if (failedAssert != NULL) {		   
		   for (++i; i < dag.size(); ++i) {
			   if (dag[i]->type == bool_node::ASSERT || dag[i]->type == bool_node::UFUN) {
				   dag.replace(i, getCnode(0));
			   }
		   }
		   if (failedAssert->isNormal()) {
			   cout << "Assertion Failure" << failedAssert->getMsg() << endl;
			   cleanup(dag);
			   throw BadConcretization(failedAssert->getMsg());
		   }
	   }
	}

	// cout<<" added nodes = "<<newnodes.size()<<endl;
    seenControls.clear();
    cleanup(dag);
	if (this->symbolicSolve) {
		dag.setUseSymbolic();
	}
}


