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
	bool p_randomize, InlineControl* ict, bool p_onlySpRandomize, int p_spRandBias):
dag(p_dag), 
DagOptim(p_dag), 
ufunAll(" ufun all"),
functionMap(p_functionMap),
replaceMap(p_replaceMap),
ictrl(ict),
randomize(p_randomize),
onlySpRandomize(p_onlySpRandomize),
spRandBias(p_spRandBias),
replaceDepth(0),
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


	int HoleHardcoder::fixValue(CTRL_node& node, int bound, int nbits){
    int rv;
    if (node.is_sp_concretize()) {
      Assert(node.max <= bound, "max should be less than bound");
      bound = node.max + 1;
      
      vector<string> parents = node.parents;
      for (int i = 0; i < parents.size(); i++) {
        map<string, int>::iterator it = randholes.find(parents[i]);
        Assert(it != randholes.end(), "Parent hole should have been seen before this node");
        if (it->second >= 0) {
          if ((it->second - i) < bound) {
            bound = max(1, it->second - i);
          }
        }
      }
     
      int nflips = 0;
      if (bound > 2) {
        nflips = 1;
      }
      //if (bound > 4) {
       // nflips = 2;
      //}
      rv = rand() % bound;
      for (int i = 0; i < nflips; i++) {
        int x = rand() % bound;
        if (x < rv) rv = x;
      }
      cout << "rv for special node " << rv << " bound " << bound << endl;
    } else {
      rv = rand() % bound;
    }
		const string& s = node.get_name();
		Tvalue& glob = globalSat->declareControl(&node);
		if(!glob.isSparse()){			
			glob.makeSparse(*globalSat);
		}
		Tvalue* loc = NULL;
		Tvalue tloc;
		if(sat->checkVar(s)){
			tloc = (sat->getControl(&node));
			if(!tloc.isSparse()){
				tloc.makeSparse(*sat);
			}
			loc = &tloc;
		}
		const gvvec& options = glob.num_ranges;		
		int sz = options.size();
    int bnd = sz;
    if (node.is_sp_concretize()) {
      if (sz > bound) {
        sz = bound;
        bnd = sz - rv;
      }
    }
    
		for(int i=0; i<bnd; ++i){
			const guardedVal& gv = options[(i + rv) % sz];
			int xx = globalSat->getMng().isValKnown(gv.guard);			
			if(xx==0){
				//global has no opinion. Should check local.
				if(loc!= NULL){					
					const gvvec& locopt = loc->num_ranges;
					const guardedVal& lgv = locopt[(i + rv) % sz];					
					Assert(lgv.value == gv.value, "Can't happen. c");
					int yy = sat->getMng().isValKnown(lgv.guard);
					if(yy==-1){
						//local had already decided that this value was bad. Move to the next.
						continue;
					}else{
						//local also has no opinion. 
						//record decision.
						if(!sat->assertIfPossible(lgv.guard)){
							// tried to set it but didn't work. we'll have to try something different.
							//this is the same as yy==-1
							continue;
						}
						addedConstraint();
						if(!globalSat->tryAssignment(gv.guard)){
							if(yy==1){
								throw BadConcretization();
							}else{
								continue;
							}
						}
						if(yy==1){
							return gv.value;
						}else{
							return recordDecision(options, (i + rv) % sz, sz, node.is_sp_concretize());
						}
					}
				}else{
					//There is no local, so we take this value.
					if(!globalSat->tryAssignment(gv.guard)){
						continue;
					}
					return recordDecision(options, (i+rv) % sz, sz, node.is_sp_concretize());
				}
			}
			if(xx==1){
				//global says it should be true. local better agree.
				//no need to record this decision because it was not really a decision.
				if(loc!= NULL){					
					const gvvec& locopt = loc->num_ranges;
					const guardedVal& lgv = locopt[(i + rv) % sz];
					Assert(lgv.value == gv.value, "Can't happen. d");
					int yy = sat->getMng().isValKnown(lgv.guard);
					if(yy==-1){
						//local had already decided that this value was bad. 
						throw BadConcretization();
					}else{
						//local has no opinion. return without recording
						if(!sat->assertIfPossible(lgv.guard)){
							// tried to set it but didn't work. we'll have to try something different.
							//this is the same as yy==-1
							throw BadConcretization();
						}
						addedConstraint();
						return (lgv.value);
					}					
				}else{
					//There is no local, so we take this value.
					return (gv.value);
				}
			}
			if(xx==-1){
				//global says it should be false. local better agree.
				//no need to record this decision because it was not really a decision.
				if(loc!= NULL){					
					const gvvec& locopt = loc->num_ranges;
					const guardedVal& lgv = locopt[(i + rv) % sz];
					Assert(lgv.value == gv.value, "Can't happen. b");
					int yy = sat->getMng().isValKnown(lgv.guard);
					if(yy==1){
						//local had also decided that is should be true.
						throw BadConcretization();
					}
					continue;
				}else{					
					continue;
				}
			}
		}
		//I wasn't able to concretize to anything. 
		throw BadConcretization();
	}

void DepTracker::helper(int harnid,  vector<char>& visited, set<int>& out){
	visited[harnid] = 1;
	vector<Lit>& lits = decisionsPerHarness[harnid];
	
	for(vector<Lit>::iterator llit = lits.begin(); llit < lits.end(); ++llit){	
		out.insert(toInt(*llit));
	}
	
	set<int>& holes = holesPerHarness[harnid];
	for(set<int>::iterator it = holes.begin(); it != holes.end(); ++it){
		set<int>& vi = harnessPerHole[*it];
		if(vi.size()>1){
			for(set<int>::iterator vvi = vi.begin(); vvi != vi.end(); ++vvi){
				if(visited[*vvi]==0){
					helper(*vvi, visited, out);
				}
			}
		}
	}
}

void DepTracker::genConflict(int harnid, vec<Lit>& out){	
	cout<<" charness = "<<harnid<<endl;
	out.clear();
	vector<char> visited(holesPerHarness.size(), 0);
	set<int> tout;
	helper(harnid, visited, tout);
	for(set<int>::iterator it = tout.begin(); it != tout.end(); ++it){
		out.push(toLit(*it));
	}
}


  int HoleHardcoder::recordDecision(const gvvec& options, int rv, int bnd, bool special){
    if (!special) {
      const guardedVal& gv = options[rv];
      Lit l =  lfromInt(-gv.guard);
      sofar.push(l);
      dt.recordDecision(l);
      return gv.value;
    } else {
      for (int i = rv + 1; i < bnd; i++) {
        const guardedVal& gv = options[i];
        Lit l = lfromInt(gv.guard);
        sofar.push(l);
        dt.recordDecision(l);
        
      }
      return options[rv].value;
    }
}

bool_node* HoleHardcoder::checkRandHole(CTRL_node* node, DagOptim& opt){
  int chsize = node->children.size();
  if (chsize == 0) return node;
		string name = node->get_name();
		dt.regHoleInHarness(name);

		map<string, int>::iterator it = randholes.find( name  );

		
  
			int tchld = 0;
			int bchld = 0;
			for(childset::iterator chit = node->children.begin(); chit != node->children.end(); ++chit){
				bool_node* chld = *chit;
				if(chld->type == bool_node::ARRACC){
					ARRACC_node* an = (ARRACC_node*) chld;
					bool allconst = true;
					for(vector<bool_node*>::iterator mit = an->multi_mother.begin(); mit != an->multi_mother.end(); ++mit){
						if((*mit)->type != bool_node::CONST){
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
			int odds;
      if (node->is_sp_concretize()) {
        odds = max(1, baseline / (tchld>0?tchld:1));
      } else {
        odds = max(2, baseline/ (tchld>0?tchld:1)  );
      }
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
			if(rand() % odds == 0 || (chsize > 1500 && totsize< 10000) || chsize > 5000 ){
				cout<<node->get_name()<<" odds = 1/"<<odds<<"  ("<<chsize<<", "<<tchld<<") "<<" try to replace"<<endl;
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
								/*
								if(child->type == bool_node::AND || child->type == bool_node::OR || child->type == bool_node::NOT || child->type == bool_node::UFUN){
									// cout<<"so far so good"<<child->lprint()<<endl;
								}else{									
									cout<<"    has a bad child"<<child->lprint()<<endl;
									randholes[node->get_name()] = REALLYLEAVEALONE;
									return node;
								}*/
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
				totsize*=bound;
				int rv = fixValue(*node, bound, nbits);		
				randholes[node->get_name()] = rv;
				cout<<node->get_name()<<": replacing with value "<<rv<<" bnd= "<<bound<<" totsize= "<<totsize<<endl;
				return opt.getCnode(rv);
			}else{
				if(PARAMS->verbosity>5){
					cout<<node->get_name()<<" odds = 1/"<<odds<<"  ("<<chsize<<", "<<tchld<<") "<<" not replacing"<<endl;
				}
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
      if (node.is_sp_concretize()) {
        rvalue = hcoder->checkRandHole(&node, *this);
        return;
      }
      if (!onlySpRandomize) {
        rvalue = hcoder->checkRandHole(&node, *this);
        return;
      }
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

bool_node* DagFunctionInliner::createTupleAngelicNode(string tuple_name, string node_name, int depth) {
  if (depth == 0) return getCnode(-1);
  
  Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
  TUPLE_CREATE_node* new_node = new TUPLE_CREATE_node();
  new_node->setName(tuple_name);
  int size = tuple_type->actSize;
  for (int j = 0; j < size ; j++) {
    stringstream str;
    str<<node_name<<"_"<<j;
    
    OutType* type = tuple_type->entries[j];
    
    if (type->isTuple) {
      new_node->multi_mother.push_back(createTupleAngelicNode(((Tuple*)type)->name, str.str(), depth - 1));
    } else {
      
      CTRL_node* src =  new CTRL_node();
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
      
      new_node->multi_mother.push_back(src);
    }
  }
  
  for (int i = size; i < tuple_type->entries.size(); i++) {
    new_node->multi_mother.push_back(getCnode(-1));
  }
  new_node->addToParents();
  
  ARRACC_node* ac = new ARRACC_node();
  stringstream str;
  str<<node_name<<"__";
  
  CTRL_node* src = new CTRL_node();
  src->name = str.str();
  src->set_nbits(1);
  src->set_Special_Angelic();
  addNode(src);
  
  ac->mother = src;
  ac->multi_mother.push_back(getCnode(-1));
  ac->multi_mother.push_back(optAdd(new_node));
  ac->addToParents();
  return optAdd(ac);
}

bool_node* DagFunctionInliner::createEqNode(bool_node* left, bool_node* right, int depth) {
  if (depth == 0) {
    EQ_node* eq = new EQ_node();
    eq->mother = left;
    eq->father = right;
    eq->addToParents();
    return optAdd(eq);
  }
  OutType* ltype = left->getOtype();
  OutType* rtype = right->getOtype();
  
  if (!ltype->isTuple && !rtype->isTuple) {
    EQ_node* eq = new EQ_node();
    eq->mother = left;
    eq->father = right;
    eq->addToParents();
    return optAdd(eq);
  } else if (!ltype->isTuple) {
    EQ_node* eq = new EQ_node();
    eq->mother = right;
    eq->father = getCnode(-1);
    eq->addToParents();
    return optAdd(eq);
  } else if (!rtype->isTuple) {
    EQ_node* eq = new EQ_node();
    eq->mother = left;
    eq->father = getCnode(-1);
    eq->addToParents();
    return optAdd(eq);
  } else {
    int lentries = ((Tuple*)ltype)->actSize;
    int rentries = ((Tuple*)rtype)->actSize;
    Assert(lentries == rentries, "Wrong types");
    bool_node* cur;
    for (int i = 0; i < lentries; i++) {
      TUPLE_R_node* left_r = new TUPLE_R_node();
      left_r->idx = i;
      left_r->mother = left;
      left_r->addToParents();
      
      TUPLE_R_node* right_r = new TUPLE_R_node();
      right_r->idx = i;
      right_r->mother = right;
      right_r->addToParents();
      
      if (i == 0) {
        cur = createEqNode(optAdd(left_r), optAdd(right_r), depth - 1);
      } else {
        AND_node* an = new AND_node();
        an->mother = cur;
        an->father = createEqNode(optAdd(left_r), optAdd(right_r), depth-1);
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
      
      int oriInpSize = node.multi_mother.size();
      
      TUPLE_CREATE_node* tuple_node = new TUPLE_CREATE_node();
    
      Tuple* oldOutputTuple = (Tuple*)OutType::getTuple(oldOutputType);
      Tuple* tup = (Tuple*) (oldOutputTuple->entries[oldOutputTuple->entries.size() - 1]);
      tuple_node->setName(tup->name);
      int size = tup->entries.size();
      tuple_node->multi_mother.push_back(getCnode(20)); // TODO: get rid of this magic number
      
      int actFields = size - oriInpSize;

      for (int i = 1; i <  actFields; i++) {
        tuple_node->multi_mother.push_back(getCnode(0));
      }
      
      for (int i = 0; i < oriInpSize; i++) {
        if (i == 0) {
          Assert(isConst(node.multi_mother[0]) || node.multi_mother[0]->getOtype()->isTuple, "First node must be a tuple");
          int d = node.multi_mother[0]->depth;
          if (d == -1 || replaceDepth == -1) replaceDepth = -1;
          else if (d > replaceDepth) {
            replaceDepth = d;
          }
        }
        tuple_node->multi_mother.push_back(node.multi_mother[i]);
      }
      
      Assert(tuple_node->multi_mother.size() == size, "Dfqwq");
      
     
      tuple_node->addToParents();
      
      TUPLE_CREATE_node* new_output = new TUPLE_CREATE_node();
      new_output->setName(oldOutputType);
      
      new_output->multi_mother.push_back(optAdd(tuple_node));
      new_output->addToParents();
      rvalue = optAdd(new_output);
      return;
    } else {
      int d = node.multi_mother[0]->depth - 1;
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
    bool_node* newOutput;
    bool_node* spCond;
		nmap.resize( oldFun.size() );		
		{
			vector<bool_node*>& inputs  = oldFun.getNodesByType(bool_node::SRC);
			// ADT, int, int ... (state)
			Assert( inputs.size() == node.multi_mother.size() , node.get_ufname()<<" argument missmatch: "<<inputs.size()<<" formal parameters, but only got "<<node.multi_mother.size()<<" actuals.\n"<<node.lprint());
      
      
      vector<bool_node*> inp_arg;
      
      if (shouldReplaceSpecialNode) {
        Assert(inputs.size() > 0, "Number of inputs should be atleast 1");
        UFUN_node* repFun = new UFUN_node(replaceFunName);
        repFun->outname = "_p_out_" + replaceFunName + "_ANONYMOUS"; // TODO: fix these magic strings
        string newNameCaps = replaceFunName;
        for(int i = 0; i < newNameCaps.size(); i++) {
          newNameCaps.at(i) = toupper(newNameCaps.at(i));
        }
        string newOutputType = newNameCaps + "_ANONYMOUS";
        repFun->set_tupleName(newOutputType);
        repFun->replaceFun = false;
        bool_node* actual;
        for (int i = 0; i < inputs.size(); i++) {
          actual = node.multi_mother[i];
          if (actual->getOtype()->isTuple) break;
          repFun->multi_mother.push_back(actual);
        }
        if (isConst(actual)) {
          shouldReplaceSpecialNode = false;
          delete repFun;
        } else {
        //Assert(actual->getOtype()->isTuple, "First input should be a tuple");
        int size = ((Tuple*) (actual->getOtype()))->entries.size();
        int actSize = ((Tuple*) (actual->getOtype()))->actSize;
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
        
        NOT_node* not_ = new NOT_node();
        AND_node* and_ = new AND_node();
        and_->mother = node.mother;
        and_->father = optEq;
        and_->addToParents();
        
        bool_node* optCond = optAdd(and_);
        not_->mother = optCond;
        not_->addToParents();
        bool_node* optNot = optAdd(not_);
        
        repFun->mother = optCond;
        
        for (int i = actSize; i < size; i++) {
          TUPLE_R_node* tr1 = new TUPLE_R_node();
          tr1->idx = i;
          tr1->mother = actual;
          tr1->addToParents();
          bool_node* optnode = optAdd(tr1);
          if (i == actSize) {
            optnode->depth = replaceDepth;
          }
          repFun->multi_mother.push_back(optnode);
        }
        
        BooleanDAG& newFun = *functionMap[replaceFunName];
        vector<bool_node*>& new_inputs  = newFun.getNodesByType(bool_node::SRC);
        int curSize = repFun->multi_mother.size();
        for (int i = curSize; i < new_inputs.size(); i++) {
          int j = i + actSize - size + 1;
          Assert(j < inputs.size(), "dfae");
          repFun->multi_mother.push_back(node.multi_mother[j]);
        }
      
        if(ictrl != NULL){
          int numRec = ictrl->numRecursive(node);
          ictrl->registerCallWithLimit(node, repFun, ictrl->getLimit() - numRec);
        }
        repFun->addToParents();
        this->addNode(repFun);
        newOutput = repFun;
        
        node.mother->children.erase((bool_node*)&node);
        
        bool_node* new_mother = new AND_node();
        new_mother->mother = node.mother;
        new_mother->father = optNot;
        
        new_mother->addToParents();
        
        node.mother = optAdd(new_mother);
        node.mother->children.insert(&node);
        }
        
      }
			
			for(int i=0; i<inputs.size(); ++i){
        bool_node* actual;
				if (node.multi_mother[i]->getOtype()->isTuple && node.multi_mother[i]->depth == 0) {
          actual = getCnode(-1);
        } else {
          actual = node.multi_mother[i];
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
					nmap[ctrl->id] = node.mother;
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
            
            if (node.hardAsserts()) {
              ufun->makeAssertsHard();
            }
            
            if (!node.replaceFun) {
              ufun->replaceFun = false;
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
      an->multi_mother.push_back(newOutput);
      
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


