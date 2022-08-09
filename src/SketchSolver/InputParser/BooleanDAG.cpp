// BooleanDAG.cpp: implementation of the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#include "BooleanDAG.h"
#include "BasicError.h"
#include "SATSolver.h"
#include "BooleanNodes.h"
#include <map>
#include <sstream>
#include <fstream>
#include <algorithm>
#include "DagLikeProgramInterpreter.h"

using namespace std;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

#ifdef SCHECKMEM
size_t prev_num_dags = 0;
set<BooleanDAG*> BooleanDAG::allocated;
long long BooleanDAG::global_boolean_dag_id = 0;
#endif

string BooleanDAG::create_suffix(bool modify_name, long long int dag_id)
{
    if(modify_name) {
//        if(dag_id == 698)
//        {
//            cout << "HERE" << endl;
//        }
        return "__id"+std::to_string(dag_id);
    }
    else {
        return "";
    }
}

string construct_name(const string& name_, const string& explicit_name, bool _is_clone, long long global_boolean_dag_id)
{
    if(explicit_name.empty())
    {
        return name_ + BooleanDAG::create_suffix(_is_clone, global_boolean_dag_id);
    }
    else
    {
        return explicit_name;
    }
}

BooleanDAG::BooleanDAG(const string& name_, bool isModel_, const string& explicit_name, bool _is_clone):
    name(construct_name(name_, explicit_name, _is_clone, global_boolean_dag_id)), isModel(isModel_), is_clone(_is_clone)
{  
  is_layered=false;
  is_sorted=false;
  n_inputs = 0;
  n_outputs = 0;
  n_controls = 0;
  offset = 0;
  ownsNodes = true;
  intSize = 2;
  useSymbolicSolver = false;
#ifdef SCHECKMEM
  allocated.insert(this);
  dag_id = global_boolean_dag_id++;
  if(false) {
    if(prev_num_dags != allocated.size()) {
      cout << "---------------------" << endl;
      cout << "NEW_DAG" << endl;
      cout << "DAG_ID " << dag_id << endl;
      cout << "DAG_NAME " << name << endl;
      cout << "NUM_DAGS " << allocated.size() << endl;
      cout << "NUM_NODES " << bool_node::get_allocated().size() << endl;
      cout << "---------------------" << endl;
      prev_num_dags = allocated.size();
    }
  }
#endif

//  if(dag_id == 370) {
//     cout << "HERE" << endl;
//  }

}


void BooleanDAG::growInputIntSizes(){
	{
		auto specIn = getNodesByType(bool_node::SRC);
		++intSize;
		for(int i=0; i<specIn.size(); ++i){	
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			// BUGFIX xzl: nbits might be strange for SRC_nodes turned from Angelic CTRLs
			if(nbits >= 2 && nbits < intSize){
				srcnode->set_nbits(nbits+1);
				// NOTE xzl: this might no longer be true for general case since we have Angelic CTRL turned into SRC_node. Hence we added a check in the "if" branch
				Assert(nbits + 1 == intSize, "This is very strange. An abomination indeed.");
			}
		}
	}
}


void BooleanDAG::sliceH(bool_node* n, BooleanDAG* bd){
	if(n->flag == 0){
		n->flag = 1;
		for (auto it = n->p_begin(); it != n->p_end(); ++it) {
			sliceH(*it, bd);
		}		
		bd->addNewNode(n);
	}
}



void BooleanDAG::clear(){
	if(ownsNodes){
	  for(int i=0; i < nodes.size(); ++i){
		  if (nodes[i] != nullptr) {
			  delete nodes[i];
			  nodes[i] = nullptr;
		  }
	  }
	}
  nodes.clear();
  named_nodes.clear();
  delete this;
}


BooleanDAG::~BooleanDAG()
{
#ifdef SCHECKMEM
	allocated.erase(this);
#endif
}

void BooleanDAG::relabel(){
  for(int i=0; i < nodes.size(); ++i){
  	if( nodes[i] != NULL ){  		
    	nodes[i]->id = i;
  	}
  }
}



void BooleanDAG::compute_layer_sizes(){
  int plyr = 0;
  int lsize = 0;
  layer_sizes.clear();
  for(int i=0; i< nodes.size(); ++i){
    if( nodes[i]->layer != plyr){
      layer_sizes.push_back(lsize);
      lsize = 0;
      plyr = nodes[i]->layer;
    }
    ++lsize;
  }
  layer_sizes.push_back(lsize);
}

void BooleanDAG::layer_graph(){
  Assert(is_sorted, "The nodes must be sorted before you can layer them.");
  
  //Because the nodes are topologically sorted, you can be sured that
  //your parents will be layered before you.
  int max_lyr=-1;
  {
    for(int i=0; i < nodes.size(); ++i){
      nodes[i]->set_layer(false);
      if(nodes[i]->layer > max_lyr){
        max_lyr = nodes[i]->layer;
      }
    }  
  } 
  //Now, we have to make sure the output nodes are in the last layer.
  //Now, unless something is wrong, the max layer should contain only outputs,
  //But there may be some outputs in other layers, so we want to have them all in
  //the max layer.
  Assert(nodes.size()>0, "There should be at least one node. Something here is not right.");  

  int minlyr = 1;
  if( nodes[0]->type == bool_node::DST ){
    minlyr = 0;
  }
  {
    for(int i=0; i < nodes.size(); ++i){
      Assert( nodes[i]->layer != max_lyr || nodes[i]->type == bool_node::DST || max_lyr == 0, "ERROR, you are computing some stuff that is never used!");
      if( nodes[i]->type == bool_node::DST )
        nodes[i]->layer = max_lyr>1 ? max_lyr : minlyr;
    }
  }
  sort(nodes.begin(), nodes.end(), comp_layer);
  //Now, we subdivide each layer into layers based on the types
  //of the nodes.
  int prvlayer=0;
  int ofst=0;
  map<bool_node::Type, int> lid;
  for(int i=0; i < nodes.size(); ++i){
    if(nodes[i]->layer == prvlayer){
      bool_node::Type ctp = nodes[i]->type;
      if( lid.find( ctp ) != lid.end() ){
        //it's there.
        nodes[i]->layer += (ofst + lid[ctp]);
      }else{
        //it's not there
        int tmp = lid.size();
        lid[nodes[i]->type] = tmp;  
        nodes[i]->layer += (ofst + tmp);
      }
    }else{
      Assert(lid.size() > 0, "BooleanDAG::layerGraph: This should not happen.");
      ofst += lid.size()-1;
      prvlayer = nodes[i]->layer;
      nodes[i]->layer += ofst;
      lid.clear();
      lid[nodes[i]->type] = 0;
    }
  }
  //And we sort back.
  sort(nodes.begin(), nodes.end(), comp_layer);   
  is_layered = true;
  
  //layering does not destroy the sorted property, because
  //even though now we have sorted by layer, this still guarantees that your parents come
  //before you.
}




//WARNING: This leaves nodesByType[bool_node::ASSERT] holding garbage pointers.
//They will get cleaned up by cleanup.
void BooleanDAG::replace(int original, bool_node* replacement){	
	int i = original;
	Assert( i < nodes.size() && i >= 0, "Out of bounds violation "<<i<<" >= "<<nodes.size()<<endl);
	Assert( replacement != NULL, "Why are you replacing with a null replacement");
	Assert( replacement->id != -22, "Why are you replacing with a corpse?");
	bool_node* onode = nodes[i];
	Assert( onode != NULL, "This can't happen");
	
	onode->neighbor_replace(replacement);
	
	
	//
	if(onode->isInter()){
		INTER_node* inonode = dynamic_cast<INTER_node*>(onode);
		map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
		if(it != named_nodes.end() && it->second==inonode){
			named_nodes.erase(it);
		}
	}
//    for (auto ctrl_it: this->getNodesByType(bool_node::CTRL)) {
//        assert(ctrl_it->type == bool_node::CTRL);
//    }
	
	//The assert list in nodesByType is left intentionally out of date, because it is rebuilt by cleanup later anyway.
	if( onode->type == bool_node::SRC || onode->type == bool_node::DST || onode->type == bool_node::CTRL ||  onode->type == bool_node::UFUN){
		vector<bool_node*>& bnv = nodesByType[onode->type];
		vector<bool_node*>::iterator end = bnv.end();
		for(vector<bool_node*>::iterator it = bnv.begin(); it < end; ++it){
			if(*it == onode){
				*it = NULL;
				// there are no duplicates, so once we find we can stop.
				break;
			}
		}			
	}
//    for (auto ctrl_it: this->getNodesByType(bool_node::CTRL)) {
//        assert(ctrl_it->type == bool_node::CTRL);
//    }
	
	nodes[i]->id = -22;
	delete nodes[i];
  	nodes[i] = NULL;
}


void BooleanDAG::removeNullNodes(){
	int nullnodes = 0;
	for(vector<bool_node*>::iterator it = nodes.begin(); it != nodes.end(); ++it){
		if(*it != NULL) nullnodes++;
	} 
	if( nullnodes < nodes.size()){
		vector<bool_node*> newnodes(nullnodes);
		nullnodes = 0;
		for(vector<bool_node*>::iterator it = nodes.begin(); it != nodes.end(); ++it){
			if(*it != NULL){
				newnodes[nullnodes] = *it;
				nullnodes++;
			}
		}
		swap(newnodes, nodes);
		Assert(nodes.size() == nullnodes, "If this fails, it means I don't know how to use STL");
		Dout( cout<<"Removing "<<newnodes.size() - nodes.size()<<" nodes"<<endl );
	}

	for(map<bool_node::Type, vector<bool_node*> >::iterator mapit = nodesByType.begin(); mapit != nodesByType.end(); ++mapit){
		vector<bool_node*>& bnv = mapit->second;
		int out=0;
		for(int i=0; i<bnv.size(); ++i){
			if(bnv[i] != NULL){
				bnv[out] = bnv[i];
				++out;
			}
		}
		bnv.resize(out);
	}
}
void BooleanDAG::remove(int i){
	bool_node* onode = nodes[i];	
	onode->dislodge();
	if(onode->isInter()){
		INTER_node* inonode = dynamic_cast<INTER_node*>(onode);	
		map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
		if(it != named_nodes.end() && it->second==inonode){
			named_nodes.erase(it);
		}
	}
	if( onode->type == bool_node::SRC || onode->type == bool_node::DST || onode->type == bool_node::CTRL || onode->type == bool_node::ASSERT || onode->type == bool_node::UFUN){
		vector<bool_node*>& bnv = nodesByType[onode->type];
		for(int t=0; t<bnv.size(); ){
			if( bnv[t] == onode ){
				bnv.erase( bnv.begin() + t);
				break;
			}else{
				++t;	
			}
		}	
	}
	nodes[i] = NULL;
	delete onode;
}
void BooleanDAG::shareparent_remove(int i){
	
  bool_node* onode = nodes[i];	
	
  Assert( onode->father() == onode->mother(), "This must be true, otherwise, the compiler is wrong");  
  Assert( onode->father() != NULL, "Can this happen? To me? Nah  ");

	//Removing from the father's children list. Note we are assuming father==mother.
	
  onode->father()->remove_child(onode);
  for(child_iter child = onode->children.begin(); child != onode->children.end(); ++child){  	
    if(  (*child)->father() == onode ){
      (*child)->father() = onode->father();
      onode->father()->children.insert( (*child) );      
    }
    if(  (*child)->mother() == onode ){
      (*child)->mother() = onode->father();
      onode->father()->children.insert( (*child) );
    }
  }
  
  	if(onode->isInter()){
		INTER_node* inonode = dynamic_cast<INTER_node*>(onode);	
		map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
		if(it != named_nodes.end() && it->second==inonode){
			named_nodes.erase(it);
		}
	}
  
  
  onode->id = -22;
  delete onode;
  nodes[i] = NULL;  
}

int checkOkForARRACC(BooleanDAG * dag, bool_node* bnode, int line /*=0*/) {
  if (bnode == NULL || bnode->type != bool_node::ARRACC) {
		return -1;
	}
	ARRACC_node * node = dynamic_cast<ARRACC_node*>(bnode);
	if ( (node->nargs()>2 || node->mother()->getOtype() != OutType::BOOL)) {
			for (int j=0; j<node->nargs(); ++j) {
				bool_node * m = node->arguments(j);
				if (m != NULL && m->isArrType()) {
					cout << "Error! line=" << line << endl;
				  cout << "ARRACC " << node->get_name() << endl;
					cout << " array elem " << m->get_name() << " in dag:" << endl;
					dag->lprint(cout);
					return j;
				}
			}
	}
	return -1;
}

void okForARRACC(BooleanDAG * dag, bool_node* bnode, int line /*=0*/) {
	int result = checkOkForARRACC(dag, bnode, line);
	if (result >= 0) {
		Assert(false, "ARRACC array elm");
	}
}

void okForARRACC(BooleanDAG * dag, vector<bool_node*> const & nodes, int line /*=0*/) {
	for(int i=0; i<nodes.size(); ++i){
		if (nodes[i] != NULL) {
			okForARRACC(dag, nodes[i], line);
		}
	}
}

void BooleanDAG::repOK(){
	return;
	cout<<"*** DOING REPOK ****"<<endl;

	map<bool_node::Type, set<bool_node*> > tsets;
	for(map<bool_node::Type, vector<bool_node*> >::iterator it = nodesByType.begin(); it != nodesByType.end(); ++it){
		bool_node::Type type = it->first;
		vector<bool_node*>& nv = it->second;
		for(int i=0; i<nv.size(); ++i){
			Assert(tsets[type].count(nv[i])==0, "You have a repeated node in the list");
			tsets[type].insert(nv[i]);
			Assert(nv[i]->type >= 0, "This is corrupted!!");
		}		
	}

	  for(map<string, INTER_node*>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it){
		  Assert( it->second != NULL, "Named node was null");
	  }

	//First, we check that the array doesn't contain any repeated nodes.
	map<bool_node*, int> nodeset;
	for(int i=0; i<nodes.size(); ++i){
		if(nodes[i] != NULL){
		Assert(nodeset.count(nodes[i]) == 0, "There is a repeated node!!! it's in pos "<<i<<" and "<<nodeset[nodes[i]] );		
		nodeset[nodes[i]] = i;
		}
		if(tsets.count(nodes[i]->type)>0){
			Assert( tsets[nodes[i]->type].count(nodes[i])==1 , "All the typed nodes should be accounted for in the nodesByType");
		}
	}
	DllistNode* cur = this->assertions.head;
	DllistNode* last=NULL;
	//while(cur != NULL && isUFUN(cur)){ last = cur; cur = cur->next; }	
	//Now, we have to check whether all the node's predecessors are in the nodeset.
	//We also check that each node is in the children of all its parents.
	for(int i=0; i<nodes.size(); ++i){	
		bool_node* n = nodes[i];
		if(n != NULL){
			if( isDllnode(n) ){
//                             cout<<"  "<<n->get_name()<<"  "<<dynamic_cast<bool_node*>(cur)->get_name()<<endl;
				//if(!n->isArith()){
					Assert(getDllnode(n) == cur, "You are skipping a node");
					UFUN_node* uf = dynamic_cast<UFUN_node*>(n);
					if(uf != NULL){
						Assert(!uf->ignoreAsserts, "If a function ignores asserts it should not be in Dll list");
					}
					
						last = cur; 
						cur = cur->next; 
					
			}
			UFUN_node* uf = dynamic_cast<UFUN_node*>(n);
			if(uf != NULL){
				if(uf->ignoreAsserts){
					CONST_node* cn = dynamic_cast<CONST_node*>(n->mother());
					// Assert(cn != NULL && cn->getVal()==1, "If a node ignores asserts, it should have a constant one condition");
				}
				
			}
			
			
				
			for (auto it = n->p_begin(); it != n->p_end(); ++it) {
				if((*it) != NULL){
					bool_node* par = (*it);
					Assert( nodeset.count(par)==1, "Multi-Mother is not in dag "<<n->get_name()<<"  "<<i << "  " << par->get_name());
					Assert( par->children.count(n) != 0, "My multimother has disowned me "<<n->get_name()<<"  "<<i);
				}
			}
			
			set<bool_node*> seen;
			for(child_iter child = n->children.begin(); child != n->children.end(); ++child){
				  Assert( nodeset.count(*child) == 1, "This child is outside the network "<<(*child)->get_name()<<"  "<<i);
				  Assert(seen.count(*child)==0, "The children set has repeat nodes !!!");
				  seen.insert(*child);
			}
		}
	}
	Assert(last == this->assertions.tail, "Missing nodes: last != assertions.tail" );
	
	okForARRACC(this, nodes, 0);
	cout<<"*** DONE REPOK ****"<<endl;
	return;
}


void BooleanDAG::cleanUnshared(){
  for(int i=0; i < nodes.size(); ++i){
  	if(nodes[i]->flag == 0){
		nodes[i]->dislodge();  		
	}
  }
  for(int i=0; i < nodes.size(); ++i){
	bool_node* onode = nodes[i];
	if(onode->flag == 0 ){ 
		if(onode->isInter()){
			INTER_node* inonode = dynamic_cast<INTER_node*>(onode);	
			map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
			if(it != named_nodes.end() && it->second==inonode){
				named_nodes.erase(it);
			}
		}
  		onode->id = -22;
  		delete onode;
		nodes[i] = NULL;
	}
  }

  removeNullNodes();  
}



//This routine removes nodes that do not flow to the output.
void BooleanDAG::cleanup(){
  //The first optimization is to remove nodes that don't contribute to the output.
  for(int i=0; i < nodes.size(); ++i){
    nodes[i]->flag = 0;
  }
  int idx=0;
  {
    // Need to give higher ids to black box numerical abstractions because these can sometimes create cycles in the dag
    // So first ignore them
    for (int i = 0; i < nodes.size(); i++) {
      bool_node* n = nodes[i];
      if (n->type == bool_node::UFUN && (dynamic_cast<UFUN_node*>(n))->getTupleName().find("_GEN_NUM_SYNTH") == 0) {
        n->flag = 1;
      }
    }
  }
  {
	  {
		  DllistNode* cur = assertions.head;
		  while(cur != NULL){
			  //if(typeid(*cur) == typeid(ASSERT_node) || typeid(*cur) == typeid(DST_node)){
			  bool_node* tbn = dynamic_cast<bool_node*>(cur);
			  //if(tbn->type != bool_node::ASSERT){ cout<<tbn->lprint()<<endl;}
				// TODO: temporary fix for eliminating unused custom synthesizer calls
				if (tbn->type != bool_node::UFUN || (dynamic_cast<UFUN_node*>(tbn))->getTupleName().find("_GEN_") != 0){
					idx = tbn->back_dfs(idx);
				}
				//  			  if(tbn->type != bool_node::ASSERT){ cout<<tbn->lprint()<<endl;}
			  //}
			  cur = cur->next;
		  }    
	  }
  }
  {
    // Now give higher ids to give higher ids to black box numerical abstractions
    for (int i = 0; i < nodes.size(); i++) {
      bool_node* n = nodes[i];
      if (n->type == bool_node::UFUN && (dynamic_cast<UFUN_node*>(n))->getTupleName().find("_GEN_NUM_SYNTH") == 0) {
        n->flag = 0;
        idx = n->back_dfs(idx);
      }
    }
  }
  
  for(int i=0; i < nodes.size(); ++i){
  	if(nodes[i]->flag == 0 && 
  		nodes[i]->type != bool_node::SRC){
		nodes[i]->dislodge();  		
	}else{
		if(nodes[i]->flag == 0){
			nodes[i]->id = idx;
			++idx;
		}
	}
  }
  vector<bool_node*> tmpv;
  tmpv.resize(idx);
  for(int i=0; i < nodes.size(); ++i){
	bool_node* onode = nodes[i];
  	if(onode->flag == 0 && 
  		onode->type != bool_node::SRC){
  		  	
		if(onode->isInter()){
			INTER_node* inonode = dynamic_cast<INTER_node*>(onode);	
			map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
			if(it != named_nodes.end() && it->second==inonode){
				named_nodes.erase(it);
			}
		}  		
		if(onode->type == bool_node::UFUN || onode->type == bool_node::CTRL){
			vector<bool_node*>& nvec = nodesByType[onode->type];
			vector<bool_node*>::iterator it = find(nvec.begin(), nvec.end(), onode);
			if(it != nvec.end()){
				nvec.erase(it);
			}
		}
  		onode->id = -22;
  		delete onode;		
  	}else{
		tmpv[onode->id] = onode;
	}
  }
  swap(tmpv, nodes);
  removeNullNodes();
  sort(nodes.begin(), nodes.end(), comp_id);
  vector<bool_node*>& nvec = nodesByType[bool_node::ASSERT];
  nvec.clear();
   DllistNode* cur = this->assertions.head;
  DllistNode* last=NULL;
  for(int i=0; i < nodes.size(); ++i){
	bool_node* onode = nodes[i];
	if( isDllnode(onode) ){
			if (onode->type == bool_node::ASSERT) {
				nvec.push_back(onode);
			}
			DllistNode* dn = getDllnode(onode);
			if(dn != cur){
				//dn is out of place in the list. we need to put it back in its place.
				//We are assuming, however, that the list does contain all the necessary nodes, just maybe not in the right order.
				//that assumption ensures that cur will never be null, because if it were, it would mean we were not expecting any more dllnodes.
				dn->remove();
				cur->addBefore(dn);
				cur = dn;// this restores the invariant that cur points onode, the currently visited node.				
			}
			last = cur; 
			cur = cur->next; 
	}
  }
}

/* Removes extraneous children that are not part of the dag */
void BooleanDAG::cleanup_children() {
  set<bool_node*> nodeset;
  for(int i=0; i<nodes.size(); ++i){
    if(nodes[i] != NULL){
      nodeset.insert(nodes[i]);
    }
  }
  for (int i = 0; i < nodes.size(); i++) {
    bool_node* n = nodes[i];
    for(child_iter child = n->children.begin(); child != n->children.end(); ++child) {
      if (nodeset.count(*child) == 0) {
        n->children.erase(*child);
      }
    }
  }
}


void BooleanDAG::change_father(const string& father, const string& son){
  try{
    Assert( named_nodes.find(father) != named_nodes.end(), "The father name does not exist: "<<father);  
  }catch(BasicError& be){
    if(father.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<father.substr(6)<<endl;
    }    
    throw be;
  }
  Assert( named_nodes.find(son) != named_nodes.end(), "The son name does not exist.");  
  Assert(named_nodes[son]->father() == NULL, "You should not call this function if you already have a father.");
  named_nodes[son]->father() = named_nodes[father];
  named_nodes[father]->children.insert( named_nodes[son] );
}

void BooleanDAG::change_mother(const string& mother, const string& son){
  try{
    Assert(named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist: "<<mother);
  }catch(BasicError& be){    
    if(mother.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<mother.substr(6)<<endl;
    }
    throw be;
  }
  Assert( named_nodes.find(son) != named_nodes.end(), "The son name does not exist.");  
  Assert(named_nodes[son]->mother() == NULL, "You should not call this function if you already have a father.");
  named_nodes[son]->mother() = named_nodes[mother];
  named_nodes[mother]->children.insert( named_nodes[son] );
}




void BooleanDAG::addNewNode(bool_node* node){
	Assert( node != NULL, "null node can't be added.");
	Assert( node->id != -22, "This node should not exist anymore");
	node->id = nodes.size() + offset;
	nodes.push_back(node);	
	if(node->isInter()){
		INTER_node* innode = dynamic_cast<INTER_node*>(node);	
		if(innode->name.size() > 0){
			named_nodes[innode->name] = innode;
		}
	}
		
	if( node->type == bool_node::SRC || node->type == bool_node::DST || node->type == bool_node::CTRL || node->type == bool_node::ASSERT || node->type == bool_node::UFUN){
		vector<bool_node*>& tmpv = nodesByType[node->type]; 
		tmpv.push_back(node);		
	}
	 
}

void BooleanDAG::addNewNodes(vector<bool_node*>& v){
	//Assume all the nodes in v are already part of the network, meaning all their parents and children are properly set.
	nodes.reserve(nodes.size() + v.size());
	for(int i=0; i<v.size(); ++i){
		if(v[i] != NULL){
			addNewNode(v[i]);
		}
	}
}


bool_node* BooleanDAG::unchecked_get_node(const string& name){
  bool_node* fth;
  if(name.size()==0){
    fth = NULL;
  }else{
	  if(named_nodes.find(name) != named_nodes.end()){
		fth = named_nodes[name];
	  }else{
		fth = NULL;
	  }
  }
  return fth;
}


bool_node* BooleanDAG::get_node(const string& name){
  bool_node* fth;
  //Assert(name.size()==0 || named_nodes.find(name) != named_nodes.end(), "name does not exist: "<<name);
  if(name.size()==0){
    fth = NULL;
  }else{
	if(named_nodes.find(name) != named_nodes.end()){
		fth = named_nodes[name];	
	}else{
		// cout<<"WARNING, DANGEROUS!! " << name << endl;
		fth = CONST_node::create(-333);
		nodes.push_back(fth);
	}
  }
  return fth;
}


bool_node* BooleanDAG::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){
                                	
  bool_node* tmp = newNode(t);
  tmp->father() = father;
  tmp->mother() = mother;  
  tmp->addToParents();
  Assert( tmp->id != -22, "This node should not exist anymore");
  tmp->id = nodes.size() + offset;
  nodes.push_back(tmp);  
  if(t == bool_node::SRC || t == bool_node::DST || t == bool_node::CTRL || t == bool_node::ASSERT || t == bool_node::UFUN){
  		nodesByType[t].push_back(tmp);
  }
  return tmp;
}

const vector<bool_node*> dummy_vec;

const vector<bool_node*>& BooleanDAG::getNodesByType(bool_node::Type t) const{
    if(nodesByType.find(t) != nodesByType.end()) {
        return nodesByType.at(t);
    }
    else
    {
        return dummy_vec;
    }
}

vector<CTRL_node*> BooleanDAG::get_CTRL_nodes() const {
    vector<CTRL_node*> ret;
    for(auto _it : getNodesByType(bool_node::CTRL))
    {
        ret.push_back((CTRL_node*)_it);
    }
    return ret;
}

vector<bool_node*>& BooleanDAG::getNodesByType_NonConst(bool_node::Type t){
    return nodesByType[t];
}


INTER_node* BooleanDAG::create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type){
	//Create interface nodes, either source, dest, or ctrl.
	
	if( named_nodes.find(gen_name) != named_nodes.end() ){
		return named_nodes[gen_name];
	}
	
  if(n < 0){
    INTER_node* tmp = dynamic_cast<INTER_node*>(newNode(type));
    nodesByType[type].push_back(tmp);
    tmp->id = nodes.size() + offset;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = OutType::BOOL;
    ++counter;
	return tmp;
  }else{
  	INTER_node* tmp = dynamic_cast<INTER_node*>(newNode(type));
  	nodesByType[type].push_back(tmp);
    dynamic_cast<INTER_node*>(tmp)->set_nbits(n);
    tmp->id = nodes.size() + offset;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = OutType::INT;
    counter += n;
	return tmp;
  }
}


INTER_node* BooleanDAG::create_inputs(int n, OutType* type, const string& gen_name, int arrSz, int tupDepth){
	SRC_node* src = dynamic_cast<SRC_node*>(create_inter(n, gen_name, n_inputs, bool_node::SRC));
	if(type == OutType::FLOAT || type == OutType::FLOAT_ARR || type->isTuple){
		src->otype = type;
	}
  if (type->isTuple) {
    src->setTuple(((Tuple*)type)->name);
    src->depth = tupDepth;
  }
	src->setArr(arrSz);
	return src;
}

INTER_node* BooleanDAG::create_controls(
	int n, const string& gen_name, bool toMinimize, bool angelic, bool spConcretize, int max, bool isFloat, bool isSpecial){
  INTER_node* tmp = create_inter(n, gen_name, n_controls, bool_node::CTRL);
  CTRL_node* cnode = dynamic_cast<CTRL_node*>(tmp);
  cnode->set_toMinimize(toMinimize);
  if (angelic) cnode->set_Special_Angelic();
  if (spConcretize) {
    cnode->special_concretize(max);
  }
  if (isFloat) {
    cnode->setFloat();
  }
	if (isSpecial) {
		cnode->setSpecial();
	}
  return tmp;
}

INTER_node* BooleanDAG::create_outputs(int n, bool_node* nodeToOutput, const string& gen_name){
  INTER_node* tmp = create_inter(n, gen_name, n_outputs, bool_node::DST);
  tmp->mother() = nodeToOutput;
  tmp->addToParents();
  return tmp;
}


INTER_node* BooleanDAG::create_outputs(int n, const string& gen_name){
  INTER_node* tmp = create_inter(n, gen_name, n_outputs, bool_node::DST);
  return tmp;
}


void BooleanDAG::printSlice(bool_node* node, ostream& out)const{    
	out<<"digraph G{"<<endl;
	set<const bool_node* > s;

	node->printSubDAG(out, s);
	

	out<<"}"<<endl;
}



void BooleanDAG::print(ostream& out)const{    
	out<<"digraph "<<this->name<<"{"<<endl;
  for(int i=0; i<nodes.size(); ++i){
  	if(nodes[i] != NULL){
  		nodes[i]->outDagEntry(out);
  	}    
  }

  out<<"}"<<endl;
}

void BooleanDAG::mrprint(ostream& out, bool print_only_nodes){

    if(!print_only_nodes) {
        out << "dag " << this->get_name() << " :" << endl;
        for (map<string, OutType *>::iterator itr = OutType::tupleMap.begin(); itr != OutType::tupleMap.end(); ++itr) {
            out << "TUPLE_DEF " << itr->first;
            vector<OutType *> entries = dynamic_cast<Tuple *>(itr->second)->entries;
            for (int i = 0; i < entries.size(); i++) {
                out << " " << entries[i]->str();
            }
            out << endl;
            out.flush();
        }
    }
    for(int i=0; i<nodes.size(); ++i) {
        if (nodes[i] != NULL) {
            out << nodes[i]->mrprint() << endl;
        }
    }
    out.flush();
}

string smt_op(bool_node::Type t){
	if(t == bool_node::AND){
		return "and";
	}
	else if(t == bool_node::OR){
		return "or";
	}
	else if(t == bool_node::XOR){
		return "xor";
	}
	else if(t == bool_node::ARRACC){
		return "ite";
	}
	else if(t == bool_node::EQ){
		return "=";
	}
	else if(t == bool_node::PLUS){
		return "+";
	}
	else if(t == bool_node::TIMES){
		return "*";
	}
	else if(t == bool_node::MOD){
		return "mod";
	}
	else if(t == bool_node::DIV){
		return "div";
	}
	else if(t == bool_node::LT){
		return "<";
	}
	else if(t == bool_node::NEG){
		return "-";
	}
	else if(t == bool_node::NOT){
		return "not";
	}
	else if(t == bool_node::ARR_R){
		return "select";
	}
	else if(t == bool_node::ARR_W){
		return "store";
	}
	else Assert(false, "Ivalid type for SMT!");
}
void getCTRLasserts(vector<bool_node*> &ctrls,string &asserted, string &exists, bool letstmts){
	for(int i=0;i<ctrls.size();i++){
		CTRL_node* cn = (CTRL_node*)(ctrls[i]);
		if(letstmts) exists= exists + " (" + cn->get_name() + " " + cn->getSMTOtype() + ") ";
		else {
			exists = exists + " (declare-const " + cn->get_name() + " " + cn->getSMTOtype() + ") \n";
		}
		if(cn->getOtype() == OutType::INT){
			int k = cn->get_nbits();
			if(asserted == ""){
				asserted = " (and (>= "+cn->get_name()+" 0) (< "+cn->get_name()+" "+ int2str(int(pow(2.0,1.0*k))) +" )) ";
			}
			else{
				asserted = " (and "+ asserted +" (and (>= "+cn->get_name()+" 0) (< "+cn->get_name()+" "+ int2str(int(pow(2.0,1.0*k))) +" ))) ";
			}
		}
	}
}

void getSRCpre(vector<bool_node*> &srcs, string &pre, string &forall, int &nbits){
	for(int i=0;i<srcs.size();i++){
		SRC_node* sn = (SRC_node*)(srcs[i]);
		forall = forall + "(" + sn->get_name() + " " + sn->getSMTOtype() + " )";
		if(sn->getOtype() == OutType::INT){
			int k = nbits;//sn->get_nbits();
			if(pre==""){
				pre= " (and (>= "+sn->get_name()+" 0) (< "+sn->get_name()+" "+ int2str(int(pow(2.0,1.0*k))) +" )) ";
			}
			else{
				pre= " (and " + ("(and (>= "+sn->get_name()+" 0) (< "+sn->get_name()+" "+ int2str(int(pow(2.0,1.0*k))) +" )) ") + pre + ") ";
			}
		}
	}
}


void getAssertStr(vector<bool_node*> &assert_nodes, string & assert_str, string &asserted, string &pre){
	//pre is for assumes!
	int assume_ctr = 0;
	int assert_ctr = 0;
	for(int i=0;i<assert_nodes.size();i++){
		ASSERT_node* an = (ASSERT_node*)(assert_nodes[i]);
		string cur_bool = " _n"+int2str(assert_nodes[i]->mother()->id)+" ";
		if(an->isAssume() || an->isHard()){
			assume_ctr++;
			if(pre=="") pre=cur_bool;
			else pre = "(and " + pre + cur_bool + ")";
		}
		else{
			assert_ctr++;
			assert_str += cur_bool;
		}
	}
	if(assert_ctr >= 2) assert_str = "(and " + assert_str + ") ";
	if(asserted=="") asserted = assert_str;
	else asserted = "(and "+asserted+" " + assert_str+")";
}

void opSMTend(ostream &out){
	out<<"\n(check-sat)\n(get-model)\n(exit)";
	out.flush();
	//cout<<"Done with Output SMT"<<endl;
}

void BooleanDAG::smtlinprint(ostream &out, int &nbits){
	string exists,forall; 
	string asserted = "";
	string pre = "";
	
	vector<bool_node*> ctrls = getNodesByType(bool_node::CTRL);
	getCTRLasserts(ctrls,asserted,exists,true);
	
	vector<bool_node*> srcs = getNodesByType(bool_node::SRC);
	getSRCpre(srcs, pre, forall, nbits);
	
	vector<bool_node*> assert_nodes = getNodesByType(bool_node::ASSERT);
	string assert_str = "";
	getAssertStr(assert_nodes, assert_str, asserted,pre);
	
	int parentheses = 1;
	out<<"(assert ";
	if(exists != "" && forall != ""){
		out<<"(exists ("<<exists<<") (forall ("<<forall<<") ";
		parentheses += 2;
	}
	else if(exists == ""){
		out<<"(forall ("<<forall<<") ";
		parentheses++;
	}
	else if(forall == ""){
		out<<"(exists ("<<exists<<") ";
		parentheses++;
	}
	else Assert(false,"Can't have both srcs and ctrls empty from the DAG");

	//output all asserts after lets
	for(int i=0; i<nodes.size(); ++i){
  		if(nodes[i] != NULL){
			if(nodes[i]->type != bool_node::ASSERT && nodes[i]->type != bool_node::DST && nodes[i]->type != bool_node::TUPLE_CREATE){
				out<<"(let ((_n"<<nodes[i]->id;
				out<<nodes[i]->smtletprint();
				parentheses++;
				out<<"))"<<endl;
			}
  		}    
	}
	if(pre != ""){
		out<<"(implies "<<pre<<" ";
		parentheses++;
	}
	if(asserted != "") out<<" "<<asserted<<" ";
	else out <<" true "<<endl;
	for(int i=0;i<parentheses;i++) out<<")";
	opSMTend(out);
}

void BooleanDAG::smt_exists_print(ostream &out){
	string asserted;
	string exists;
	vector<bool_node*> ctrls = getNodesByType(bool_node::CTRL);
	getCTRLasserts(ctrls,asserted,exists,false);
	out<<exists;
	vector<bool_node*> srcs = getNodesByType(bool_node::SRC);
	Assert(srcs.empty(), "Cannot have SRC nodes here");
	vector<bool_node*> assert_nodes = getNodesByType(bool_node::ASSERT);
	string assert_str;
	string pre;
	getAssertStr(assert_nodes, assert_str, asserted,pre);

	//output all asserts after declaring each node
	for(int i=0; i<nodes.size(); ++i){
  		if(nodes[i] != NULL){
			if(nodes[i]->type != bool_node::ASSERT && nodes[i]->type != bool_node::DST){
				out<<"(declare-const _n"<<nodes[i]->id<<" "<<nodes[i]->getSMTOtype()<<")"<<endl;
				out<<"(assert (= _n"<<nodes[i]->id<<" "<<nodes[i]->smtletprint()<<"))"<<endl;
			}
  		}    
	}
	out<<"(assert ";
	if(pre != "") out<<"(implies "<<pre<<" ";
	out<<asserted;
	if(pre!="") out <<")";
	out<<" )"<<endl;
	opSMTend(out);
}


void BooleanDAG::lprint(ostream& out){    
	out<<"dag "<< this->get_name() <<"{"<<endl;
  for(int i=0; i<nodes.size(); ++i){
  	if(nodes[i] != NULL){
  		out<<nodes[i]->lprint()<<endl;
  	}    
  }

  out<<"}"<<endl;
}

void BooleanDAG::print_wrapper()const{    
  ostream& out = std::cout;
  print(out);
}

void BooleanDAG::lprint_wrapper(){
  ostream& out = std::cout;    
  lprint(out);
}

void BooleanDAG::print_wrapper(const char* fileName)const{    
  std::ofstream out(fileName, ios_base::out);
  print(out);
}

void BooleanDAG::lprint_wrapper(const char* fileName){
  std::ofstream out(fileName, ios_base::out);    
  lprint(out);
}

void BooleanDAG::clearBackPointers(){
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		(*node_it)->children.clear();				
	}	
}

void BooleanDAG::resetBackPointers(){
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		if( *node_it != NULL){ 
			(*node_it)->children.clear();
		}
	}
	int i=0;
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it, ++i){
		if( *node_it != NULL){
			(*node_it)->addToParents();
		}
	}
}



void BooleanDAG::andDag(BooleanDAG* bdag){
	relabel();
	bdag->relabel();
	if(this->intSize != bdag->intSize){
		// give priority to intsize of new dag.
		intSize = bdag->intSize;
		auto sn = getNodesByType(bool_node::SRC);
		for(int i=0; i<sn.size(); ++i){
			INTER_node* inter = dynamic_cast<INTER_node*>((sn[i]));
			if(inter->get_nbits()>1 && inter->get_nbits() < bdag->intSize){
				// BUGFIX xzl: the inter might be a SRC_node turned from Angelic CTRL, and having a larger nbits than bdag
				Assert(inter->get_nbits() <= bdag->intSize, "inter " << inter->lprint() << " " << inter->get_nbits() << " > " << bdag->intSize);
				inter->set_nbits(bdag->intSize);
			}
		}
	}

	map<bool_node*, bool_node*> replacements;	
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		Assert( (*node_it) != NULL, "Can't make a miter when you have null nodes.");
		Assert((*node_it)->type != bool_node::DST, "This DAG should be a miter");
		Dout( cout<<" adding "<<(*node_it)->get_name()<<endl );		
			
		if( (*node_it)->type != bool_node::CTRL ){ 
			nodes.push_back( (*node_it) );
			if(isDllnode(*node_it)){
				DllistNode* dln = getDllnode(*node_it);
				dln->remove();
				assertions.append(dln);
			}
			(*node_it)->switchInputs(*this, replacements);
			if( (*node_it)->type == bool_node::ASSERT ||
				(*node_it)->type == bool_node::SRC || 
				(*node_it)->type == bool_node::UFUN 
				){
				nodesByType[(*node_it)->type].push_back((*node_it));
				if((*node_it)->type == bool_node::SRC){
					INTER_node* inter = dynamic_cast<INTER_node*>((*node_it));
					while(has_name(inter->name)){
						inter->name += "_b";
					}
					named_nodes[inter->name] = inter;
				}
			}
		}else{
			CTRL_node* cnode = dynamic_cast<CTRL_node*>((*node_it));
			if( !has_name(cnode->name) ){
				nodes.push_back( cnode );
				nodesByType[cnode->type].push_back(cnode);	
				named_nodes[cnode->name] = cnode;
			}else{
				replacements[(*node_it)] = get_node(cnode->name);
				delete *node_it;
			}
		}
	}
	removeNullNodes();
	cleanup();
	delete bdag;
	relabel();
}


void BooleanDAG::makeMiter(BooleanDAG* bdag){
	bool_node* tip = NULL;
	relabel();
	bdag->relabel();
	map<bool_node*, bool_node*> replacements;	

	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		Assert( (*node_it) != NULL, "Can't make a miter when you have null nodes.");
		(*node_it)->flag = 0;
		Dout( cout<<" adding "<<(*node_it)->get_name()<<endl );		
				
		if( (*node_it)->type != bool_node::SRC && (*node_it)->type != bool_node::DST){ 
			nodes.push_back( (*node_it) );
			(*node_it)->switchInputs(*this, replacements);
			if( (*node_it)->type == bool_node::CTRL ||  (*node_it)->type == bool_node::ASSERT || (*node_it)->type == bool_node::UFUN  ){
				nodesByType[(*node_it)->type].push_back((*node_it));
				if( (*node_it)->type == bool_node::CTRL ){
					INTER_node* inode = dynamic_cast<INTER_node*>(*node_it);
					named_nodes[inode->name] = inode;
				}				
				if( isDllnode(*node_it)  ){
					DllistNode* tt = getDllnode((*node_it));
					tt->remove();
					assertions.append( tt );
				}
			}
		} else if( (*node_it)->type == bool_node::SRC ){
			INTER_node* inode = dynamic_cast<INTER_node*>(*node_it);
			if( !has_name(inode->name) ){
				nodes.push_back( inode );
				nodesByType[inode->type].push_back(inode);	
				named_nodes[inode->name] = inode;
			}else{
				replacements[*node_it] = this->get_node(inode->name);
				delete (*node_it);
				continue;
			}
		}
				
		if( (*node_it)->type == bool_node::DST){
            INTER_node* inode = dynamic_cast<INTER_node*>(*node_it);
			//nodesByType[(*node_it)->type].push_back((*node_it));
			INTER_node* otherDst = named_nodes[inode->name];
			Assert(otherDst != NULL, "AAARGH: Node is not registered "<<(inode)->name<<endl);
            
            if (otherDst->mother()->type == bool_node::TUPLE_CREATE) {
                TUPLE_CREATE_node* inodeTuple = dynamic_cast<TUPLE_CREATE_node*>(inode->mother());
                TUPLE_CREATE_node* otherDstTuple = dynamic_cast<TUPLE_CREATE_node*>(otherDst->mother());
                int inodeCount = inodeTuple->nparents();
                int otherDstCount = otherDstTuple->nparents();
                Assert(inodeCount == otherDstCount, "Number of outputs should be the same" << (inode)->name<<endl);
                for (int i = 0; i < inodeCount; i++) {
                    EQ_node* eq = EQ_node::create();
                    eq->mother() = otherDstTuple->get_parent(i);
                    eq->father() = inodeTuple->get_parent(i);
                    
                    //eq->addToParents();
                    Dout(cout<<"           switching inputs "<<endl);
                    eq->switchInputs(*this, replacements);
                    Dout(cout<<"           replacing "<<otherDst->get_name()<<" with "<<eq->get_name()<<endl);
                                    string mm = "The spec and sketch can not be made to be equal. ";
                    mm += otherDst->name;
                    if (i==inodeCount -1) {
                        Assert( nodes[otherDst->id] == otherDst, "The replace won't work, because the id's are wrong");
                        replace( otherDst->id, eq);
                    }
                    
                    nodes.push_back( eq );

                    ASSERT_node* finalAssert = ASSERT_node::create();			
                    finalAssert->setMsg( mm );
                    finalAssert->mother() = eq;
                    finalAssert->addToParents();
                    nodes.push_back(finalAssert);

                    nodesByType[finalAssert->type].push_back(finalAssert);
                    assertions.append( getDllnode(finalAssert) );
                }
                if(replacements.count((*node_it)->mother())==0){
                    (*node_it)->dislodge();
                }
                delete (*node_it);
            } else {
                EQ_node* eq = EQ_node::create();
                eq->father() = otherDst->mother();
                eq->mother() = (*node_it)->mother();
                
                //eq->addToParents();
                
                Dout(cout<<"           switching inputs "<<endl);
                eq->switchInputs(*this, replacements);
                
                Dout(cout<<"           replacing "<<otherDst->get_name()<<" with "<<eq->get_name()<<endl);
                Assert( nodes[otherDst->id] == otherDst, "The replace won't work, because the id's are wrong");
                
                string mm = "The spec and sketch can not be made to be equal. ";
                mm += otherDst->name;
                
                replace( otherDst->id, eq);
                if(replacements.count((*node_it)->mother())==0){
                    (*node_it)->dislodge();
                }
                delete (*node_it);
                nodes.push_back( eq );
                
                ASSERT_node* finalAssert = ASSERT_node::create();			
                finalAssert->setMsg( mm );
                finalAssert->mother() = eq;
                finalAssert->addToParents();
                nodes.push_back(finalAssert);
                
                nodesByType[finalAssert->type].push_back(finalAssert);
                assertions.append( getDllnode(finalAssert) );
            }
            
		}
	}

	nodesByType[bool_node::DST].clear();
	this->useSymbolicSolver = this->useSymbolicSolver || bdag->useSymbolicSolver;
	removeNullNodes();
	cleanup();
	delete bdag;
	//sort_graph();
	relabel();
}




void BooleanDAG::rename(const string& oldname,  const string& newname){
	INTER_node* node = named_nodes[oldname];
	node->name = newname;
	named_nodes.erase(oldname);
	named_nodes[newname] = node;	
}



void BooleanDAG::clone_nodes(vector<bool_node*>& nstore, Dllist* dl){
	nstore.resize(nodes.size());

	Dout( cout<<" after relabel "<<endl );
	int nnodes = 0;
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		if( (*node_it) != NULL ){		
			AssertDebug( (*node_it)->id != -22 , "This node has already been deleted " + (*node_it)->get_name() );
            Assert( (*node_it)->id != -22 , "This node has already been deleted "<<	(*node_it)->get_name() );
            bool_node* bn = (*node_it)->clone(false);
            assert(bn->type == (*node_it)->type);
            assert(bn->id == (*node_it)->id);
			
			if( dl != NULL && isDllnode(bn) ){
				dl->append(getDllnode(bn));
			}

			Dout( cout<<" node "<<(*node_it)->get_name()<<" clones into "<<bn->get_name()<<endl );
			nstore[nnodes] = bn;
			assert(bn->id == nnodes);
            nnodes++;
		}else{
			Assert( false, "The graph you are cloning should not have any null nodes.");
		}
	}
	Dout( cout<<" after indiv clone "<<endl );
	//nstore.resize(nnodes);
	//BooleanDAG::iterator old_it = begin();
	for(BooleanDAG::iterator node_it = nstore.begin(); node_it != nstore.end(); ++node_it /*,++old_it*/){
		//(*node_it)->redirectPointers(*this, (vector<const bool_node*>&) nstore, (*old_it)->children);
		(*node_it)->redirectParentPointers(*this, (vector<const bool_node*>&) nstore, true, NULL);
	}
}



BooleanDAG* BooleanDAG::clone(const string& explict_name, const bool rename_holes, const map<string, string>* _hole_renaming_map){

    if(!rename_holes) {
        AssertDebug(_hole_renaming_map == nullptr, "IF YOU ARE NOT RENAME HOLES, YOU MUSTN'T PASS A _hole_renaming_map");
    }
    if(_hole_renaming_map != nullptr) {
        AssertDebug(rename_holes, "IF YOU ARE PASSING A _hole_renaming_map, YOU MUST rename_holes");
        set<string> all_hole_names;
        for (const auto& it: *_hole_renaming_map) {
            AssertDebug(all_hole_names.find(it.second) == all_hole_names.end(), "ALL NEW HOLE NAMES MUST BE UNIQUE.");
            all_hole_names.insert(it.second);
        }
    }

    for(map<bool_node::Type, vector<bool_node*> >::iterator it = nodesByType.begin();
        it != nodesByType.end(); ++it){
        for(int i=0; i<it->second.size(); ++i){
            assert(nodes[it->second[i]->id] == it->second[i]);
        }
    }
	Dout( cout<<" begin clone "<<endl );

	BooleanDAG* bdag = new BooleanDAG(name, isModel, explict_name, true);
	relabel();

    if(dag_id_from_the_user != -1)
    {
        bdag->set_dag_id_from_the_user(dag_id_from_the_user);
    }


    for(map<bool_node::Type, vector<bool_node*> >::iterator it = nodesByType.begin();
        it != nodesByType.end(); ++it){
        for(int i=0; i<it->second.size(); ++i){
            assert(nodes[it->second[i]->id] == it->second[i]);
        }
    }

	clone_nodes(bdag->nodes, &bdag->assertions);

	Dout( cout<<" after redirectPointers "<<endl );
	bdag->n_controls = n_controls;
	bdag->n_inputs = n_inputs;
	bdag->n_outputs = n_outputs;
	bdag->intSize = intSize;
	bdag->useSymbolicSolver = useSymbolicSolver;

    for(map<bool_node::Type, vector<bool_node*> >::iterator it = nodesByType.begin();
        it != nodesByType.end(); ++it){
        for(int i=0; i<it->second.size(); ++i){
            assert(nodes[it->second[i]->id] == it->second[i]);
        }
    }
	
	for(map<bool_node::Type, vector<bool_node*> >::iterator it = nodesByType.begin();
					it != nodesByType.end(); ++it){
		vector<bool_node*>& tmp = bdag->nodesByType[it->first];
		Assert( tmp.size() == 0, "This can't happen. This is an invariant.");
		for(int i=0; i<it->second.size(); ++i){
            AssertDebug( it->second[i]->id != -22 , "This node has already been deleted " + it->second[i]->get_name() + "\n");
			Assert( it->second[i]->id != -22 , "This node has already been deleted "<<it->second[i]->get_name()<<endl );
            assert(bdag->nodes[ it->second[i]->id ]->type == it->first);
            tmp.push_back( bdag->nodes[ it->second[i]->id ] );
		}							
	}

    map<string, string> prev_name_to_new_name;
    {

        auto ctrls = bdag->getNodesByType(bool_node::CTRL);

        map<string, string> hole_renaming_map;

        if(rename_holes) {

            if(_hole_renaming_map == nullptr) {
                //_hole_renaming_map not specified, so use default.
                for (auto &node: ctrls) {
                    CTRL_node *ctrl = (CTRL_node *) node;
                    string original_name = ctrl->get_original_name();
                    if(original_name != "#PC") {
                        assert(hole_renaming_map.find(original_name) == hole_renaming_map.end());
                        hole_renaming_map[original_name] = ctrl->get_name() + create_suffix(true, bdag->dag_id);
                    }
                }
            }
            else {
                //_hole_renaming_map specified, so use that.
                hole_renaming_map = *_hole_renaming_map;
            }
        }
        else {
            assert(_hole_renaming_map == nullptr);
        }

        for (auto &node: ctrls) {
            CTRL_node* ctrl = (CTRL_node*)node;
            assert(ctrl->type == bool_node::CTRL);
            string ctrl_name = ctrl->get_name();
            if (ctrl->get_Pcond()) {
                assert(ctrl_name == "#PC");
                ctrl->set_dag_name(bdag->get_name());
            } else {
                assert(ctrl_name != "#PC");
                if (ctrl_name.size() >= 3) {
                    assert(ctrl_name.substr(0, 3) != "#PC");
                }
                if (rename_holes) {
                    string prev_name = ctrl->get_name();
                    string original_name = ctrl->get_original_name();
                    string new_name = hole_renaming_map[original_name];

                    assert(hole_renaming_map.find(original_name) != hole_renaming_map.end());
                    if(_hole_renaming_map == nullptr) {
                        assert(new_name == prev_name + create_suffix(true, bdag->dag_id));
                    }

                    ctrl->save_dag_name_and_update_ctrl_name(
                            bdag->get_name(), new_name);
                    assert(new_name == ctrl->get_name());
                    assert(prev_name_to_new_name.find(prev_name) == prev_name_to_new_name.end());
                    prev_name_to_new_name[prev_name] = new_name;

                } else {
                    string prev_name = ctrl->get_name();
                    ctrl->save_dag_name_and_update_ctrl_name(
                            bdag->get_name(), prev_name);
                }
            }
        }
    }

    if(rename_holes) {
        for(auto it : bdag->getNodesByType(bool_node::CTRL)) {
            string actual_name = ((CTRL_node*)it)->get_name();
            if(actual_name != "#PC") {
                string var_name = ((CTRL_node *) it)->get_original_name();
                string sub_dag_name = ((CTRL_node *) it)->get_source_dag_name();

                assert(sub_dag_name == bdag->get_name());
            }
        }

        for (map<string, INTER_node *>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it) {
            AssertDebug(it->second->id != -22, "This node has already been deleted " + it->first + "\n");
            Assert(it->second->id != -22, "This node has already been deleted " << it->first << endl);
            AssertDebug(bdag->nodes.size() > it->second->id, " Bad node  " + it->first + "\n");
            Assert(bdag->nodes.size() > it->second->id, " Bad node  " << it->first << endl);
            if(prev_name_to_new_name.find(it->first) != prev_name_to_new_name.end())
            {
                assert(it->second->type == bool_node::CTRL);
                assert(bdag->named_nodes.find(prev_name_to_new_name[it->first]) == bdag->named_nodes.end());
                //changed name
                assert(prev_name_to_new_name[it->first] != it->first);
                bdag->named_nodes[prev_name_to_new_name[it->first]] = dynamic_cast<INTER_node *>(bdag->nodes[it->second->id]);
            }
            else {
                if(it->first != "#PC")
                assert(it->second->type != bool_node::CTRL);
                assert(bdag->named_nodes.find(it->first) == bdag->named_nodes.end());
                //name hasn't been changed
                bdag->named_nodes[it->first] = dynamic_cast<INTER_node *>(bdag->nodes[it->second->id]);
            }
        }
    }
    else {
        for (map<string, INTER_node *>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it) {
            AssertDebug(it->second->id != -22, "This node has already been deleted " + it->first + "\n");
            Assert(it->second->id != -22, "This node has already been deleted " << it->first << endl);
            AssertDebug(bdag->nodes.size() > it->second->id, " Bad node  " + it->first + "\n");
            Assert(bdag->nodes.size() > it->second->id, " Bad node  " << it->first << endl);
            bdag->named_nodes[it->first] = dynamic_cast<INTER_node *>(bdag->nodes[it->second->id]);
        }
    }

    assert(bdag->check_ctrl_node_source_dag_naming_invariant());

    return bdag;
}


void BooleanDAG::registerOutputs(){
	 auto vn = getNodesByType(bool_node::DST);
	 for(int i=0; i<vn.size(); ++i){
		assertions.append( getDllnode(vn[i]) );
	 }
}

void BooleanDAG::replace_label_with_another(const string replace_this, const string with_this) {
    auto ufun_nodes = getNodesByType(bool_node::UFUN);
    bool enter = false;
    for(auto ufun_node : ufun_nodes){
        assert(ufun_node->type == bool_node::UFUN);
        if(((UFUN_node *) ufun_node)->get_ufun_name() == replace_this) {
            ((UFUN_node*)ufun_node)->modify_ufname(with_this);
            enter = true;
        }
    }
    AssertDebug(enter, replace_this + " doesn't exist in " + name);
}

bool BooleanDAG::get_is_clone() {
    return is_clone;
}

vector<string> BooleanDAG::get_ufun_names() const {
    auto ufuns = getNodesByType(bool_node::UFUN);
    vector<string> ret;
    ret.reserve(ufuns.size());
    for(auto it : ufuns)
    {
        ret.push_back(((UFUN_node *) it)->get_ufun_name());
    }
    return ret;
}

map<string, string> BooleanDAG::get_hole_assignment_map() {
    map<string, string> hole_assignment_map;
    for(const auto& _it: getNodesByType(bool_node::CTRL))
    {
        const CTRL_node* ctrl_node = (CTRL_node*)_it;
        if(ctrl_node->get_name() != "#PC")
        {
            const string& var_name = ctrl_node->get_original_name();
            const string& val_name = ctrl_node->get_name();
            assert(hole_assignment_map.find(var_name) == hole_assignment_map.end());
            hole_assignment_map[var_name] = val_name;
        }
    }
    return hole_assignment_map;
}


#include "NodeEvaluator.h"
#include "File.h"
#include "BenchmarkScore.h"

string zeros(int n)
{
    string ret = "";
    for(int i = 0;i<n;i++) ret+= "0";
    return ret;
}

#include "FileForVecInterp.h"
#include "vectorized_interpreter_main.h"

vector<bool> evaluate_inputs_baseline(BooleanDAG& dag, const File* file, FloatManager& floats, int repeats = 0)
{
    for(int i = 0;i<repeats;i++) {
        evaluate_inputs_baseline(dag, file, floats, 0);
    }

    NodeEvaluator node_evaluator(dag, floats);

    vector<bool> ret(file->size());

    string size_str = std::to_string(dag.size());
    size_str = zeros(4-size_str.size()) + size_str;
    int ground_truth_score = 0;
    auto after_prep = chrono::steady_clock::now();
    for(int i = 0;i<file->size();i++) {
        VarStore* row_pointer = (VarStore*)file->at(i);
        bool fails = node_evaluator.run(*row_pointer, false, true);
        ret[i] = !fails;
        ground_truth_score += ret[i];
    }
//    timestamp(after_prep, "exec[baseline]_n"+size_str);
    timestamp(after_prep, "exec[baseline]");

    node_evaluator.reset_src_to_input_id();

    return ret;
}

vector<bool> evaluate_inputs_vectorized_interpreter_assert_correctness(BooleanDAG& dag, const File* file, FloatManager& floats, int repeats = 0)
{

    for(int i = 0;i<repeats;i++) {
        evaluate_inputs_vectorized_interpreter_assert_correctness(dag, file, floats, 0);
    }

    NodeEvaluator node_evaluator(dag, floats);

    vector<bool> ret(file->size());

    string size_str = std::to_string(dag.size());
    size_str = zeros(4-size_str.size()) + size_str;
    int ground_truth_score = 0;
    auto after_prep = chrono::steady_clock::now();
    for(int i = 0;i<file->size();i++) {
        VarStore* row_pointer = (VarStore*)file->at(i);
        bool fails = node_evaluator.run(*row_pointer, false, true);
        ret[i] = !fails;
        ground_truth_score += ret[i];
    }
    timestamp(after_prep, "exec[baseline]_n"+size_str);
    timestamp(after_prep, "exec[baseline]");

    //vectorized_interpreter part

    stringstream dagstream;

    dag.mrprint(dagstream, true);

    cout << "GROUND_TRUTH_CHECKSUM: " << ground_truth_score << endl;
//    cout << performance_summary_to_string(true) << endl;

    auto start_reading_exhausitve_inputs = std::chrono::steady_clock::now();

    string dag_string = dagstream.str();

//    cout << "dagstream: " << dag_string << endl;

    vector<VectorizedInterpreter::BaseType> batch_output =
            VectorizedInterpreter::main(dag_string, *file->get_file_from_vectorized_interpreter());

    int predicted_checksum = 0;
    for(int i = 0;i<batch_output.size();i++)
    {
        assert(VectorizedInterpreter::is_bit(batch_output[i]));
        predicted_checksum += batch_output[i];
    }

//    cout << "PREDICTED_CHECKSUM = " << predicted_checksum << endl;
//    cout << performance_summary_to_string(true) << endl;

    assert(ground_truth_score == predicted_checksum);

    if(repeats != 0) {
        assert(false);
    }

    node_evaluator.reset_src_to_input_id();

    for(int i = 0;i<batch_output.size();i++) {
        assert(ret[i] == batch_output[i]);
    }

    return ret;
}

vector<bool> evaluate_inputs_vectorized_interpreter(BooleanDAG& dag, const File* file, FloatManager& floats, int repeats = 0)
{
    for(int i = 0;i<repeats;i++) {
        evaluate_inputs_vectorized_interpreter(dag, file, floats, 0);
    }

    NodeEvaluator node_evaluator(dag, floats);

    vector<bool> ret(file->size());

    string size_str = std::to_string(dag.size());
    size_str = zeros(4-size_str.size()) + size_str;
    int ground_truth_score = 0;
    auto after_prep = chrono::steady_clock::now();
    for(int i = 0;i<file->size();i++) {
        VarStore* row_pointer = (VarStore*)file->at(i);
        bool fails = node_evaluator.run(*row_pointer, false, true);
        ret[i] = !fails;
        ground_truth_score += ret[i];
        break;
    }
    timestamp(after_prep, "prep_for_vecinterp");

    //vectorized_interpreter part

    stringstream dagstream;

    dag.mrprint(dagstream, true);

//    cout << "GROUND_TRUTH_CHECKSUM: " << ground_truth_score << endl;
//    cout << performance_summary_to_string(true) << endl;

    string dag_string = dagstream.str();

//    cout << "dagstream: " << dag_string << endl;

    vector<VectorizedInterpreter::BaseType> batch_output =
            VectorizedInterpreter::main(dag_string, *file->get_file_from_vectorized_interpreter());

//    int predicted_checksum = 0;
//    for(int i = 0;i<batch_output.size();i++)
//    {
//        assert(VectorizedInterpreter::is_bit(batch_output[i]));
//        predicted_checksum += batch_output[i];
//    }

//    cout << "PREDICTED_CHECKSUM = " << predicted_checksum << endl;
//    cout << performance_summary_to_string(true) << endl;

//    assert(ground_truth_score == predicted_checksum);

    if(repeats != 0) {
        assert(false);
    }

    node_evaluator.reset_src_to_input_id();

    for(int i = 0;i<batch_output.size();i++) {
        assert(VectorizedInterpreter::is_bit(batch_output[i]));
        ret[i] = batch_output[i];
    }

    return ret;
}


vector<bool> BooleanDAG::evaluate_inputs(const File* file, FloatManager& floats, int repeats) {
//    return evaluate_inputs_vectorized_interpreter(*this, file, floats, 0);
//    return evaluate_inputs_vectorized_interpreter_assert_correctness(*this, file, floats, 0);
    return evaluate_inputs_baseline(*this, file, floats, 0);
}

int get_passing_input_id__w__vectorized_interpreter(BooleanDAG& dag, const File* _file, FloatManager& floats)
{
    NodeEvaluator node_evaluator(dag, floats);

    string size_str = std::to_string(dag.size());
    size_str = zeros(4-size_str.size()) + size_str;

    auto after_prep = chrono::steady_clock::now();
    for(int i = 0;i<_file->size();i++) {
        VarStore* row_pointer = (VarStore*)_file->at(i);
        bool fails = node_evaluator.run(*row_pointer, false, true);
        break;
    }
    timestamp(after_prep, "prep_for_vecinterp_main_get_id");

    //vectorized_interpreter part

    stringstream dagstream;
    dag.mrprint(dagstream, true);
    string dag_string = dagstream.str();

    int rez = VectorizedInterpreter::main_get_passing_input_idx(
            dag_string, *_file->get_file_from_vectorized_interpreter());

    node_evaluator.reset_src_to_input_id();
    return rez;

//    auto file = _file->get_file_from_vectorized_interpreter();
//    assert(file->size() % 2 == 0);
//    const int stage_size = file->size()/2;
//    vector<const VectorizedInterpreter::FileForVecInterp*> slices =
//            file->get_slices(stage_size);
//
//    int sum = 0;
//    for(int slice_id = 0;slice_id<slices.size();slice_id++) {
//        vector<VectorizedInterpreter::BaseType> batch_output =
//                VectorizedInterpreter::main(dag_string, *slices[slice_id]);
//        for (int i = 0; i < batch_output.size(); i++) {
//            if (batch_output[i]) {
//                node_evaluator.reset_src_to_input_id();
//                return i+sum;
//            }
//        }
//        sum += slices[slice_id]->size();
//    }
//    node_evaluator.reset_src_to_input_id();
//    return -1;

}

int get_passing_input_id__from_native_evaluator(BooleanDAG& dag, const File* file, FloatManager& floats)
{
    NodeEvaluator node_evaluator(dag, floats);

    string size_str = std::to_string(dag.size());
    size_str = zeros(4-size_str.size()) + size_str;

    int ret = -1;

    auto after_prep = chrono::steady_clock::now();
    for(int i = 0;i<file->size();i++) {
        VarStore* row_pointer = (VarStore*)file->at(i);
        bool fails = node_evaluator.run(*row_pointer, false, true);

        if(!fails)
        {
            ret = i;
            break;
        }
    }
    timestamp(after_prep, "exec_n"+size_str+"_m"+std::to_string(ret));

    node_evaluator.reset_src_to_input_id();

    return ret;
}

int BooleanDAG::get_passing_input_id(const File* file, FloatManager& floats)
{
    int ret_predicted = get_passing_input_id__w__vectorized_interpreter(*this, file, floats);
//    int ret_ground_truth =  get_passing_input_id__from_native_evaluator(*this, file, floats);
//    assert(ret_ground_truth == ret_predicted);
    return ret_predicted;
//    return ret_ground_truth;
}


void BooleanDAG::set_dag_id_from_the_user(int _dag_id_from_the_user) {
    assert(dag_id_from_the_user == -1);
    dag_id_from_the_user = _dag_id_from_the_user;
}

int BooleanDAG::get_dag_id_from_the_user() {
    return dag_id_from_the_user;
}
