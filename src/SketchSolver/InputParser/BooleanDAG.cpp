// BooleanDAG.cpp: implementation of the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#include "BooleanDAG.h"
#include "BasicError.h"
#include "SATSolver.h"
#include "BooleanNodes.h"

#include <sstream>
#include <fstream>
#include <algorithm>


using namespace std;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

#ifdef SCHECKMEM
set<BooleanDAG*> BooleanDAG::allocated;
#endif 

BooleanDAG::BooleanDAG(const string& name_, bool isModel_):name(name_), isModel(isModel_)
{  
  is_layered=false;
  is_sorted=false;
  n_inputs = 0;
  n_outputs = 0;
  n_controls = 0;
  offset = 0;
  ownsNodes = true;
  intSize = 2;
#ifdef SCHECKMEM
  cout<<"Checking allocation"<<endl;
  allocated.insert(this);
#endif
}


void BooleanDAG::growInputIntSizes(){
	vector<bool_node*>& specIn = getNodesByType(bool_node::SRC);
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


void BooleanDAG::sliceH(bool_node* n, BooleanDAG* bd){
	if(n->flag == 0){
		n->flag = 1;
		if(n->mother != NULL){
			sliceH(n->mother, bd);
		}
		if(n->father != NULL){
			sliceH(n->father, bd);
		}
		if(n->isArith()){
			arith_node* an = dynamic_cast<arith_node*>(n);
			for(int i=0; i<an->multi_mother.size(); ++i){
				sliceH(an->multi_mother[i], bd);
			}
		}
		bd->addNewNode(n);
	}
}



void BooleanDAG::clear(){	
	if(ownsNodes){
	  for(int i=0; i < nodes.size(); ++i){
  		nodes[i]->id = -22;
  		delete nodes[i];
  		nodes[i] = NULL;
	  }
	}
  nodes.clear();
  named_nodes.clear();
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

	
	
	if( onode->type == bool_node::SRC || onode->type == bool_node::DST || onode->type == bool_node::CTRL || onode->type == bool_node::ASSERT){
		vector<bool_node*>& bnv = nodesByType[onode->type];
		vector<bool_node*>::iterator end = bnv.end();
		for(vector<bool_node*>::iterator it = bnv.begin(); it < end; ++it){
			if(*it == onode){
				it = bnv.erase(it);
				// there are no duplicates, so once we find we can stop.
				break;
			}
		}			
	}
	
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
	if( onode->type == bool_node::SRC || onode->type == bool_node::DST || onode->type == bool_node::CTRL || onode->type == bool_node::ASSERT){
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
	
  Assert( onode->father == onode->mother, "This must be true, otherwise, the compiler is wrong");  
  Assert( onode->father != NULL, "Can this happen? To me? Nah  ");

	//Removing from the father's children list. Note we are assuming father==mother.
	
  onode->father->remove_child(onode);
  for(child_iter child = onode->children.begin(); child != onode->children.end(); ++child){  	
    if(  (*child)->father == onode ){
      (*child)->father = onode->father;
      onode->father->children.insert( (*child) );      
    }
    if(  (*child)->mother == onode ){
      (*child)->mother = onode->father;
      onode->father->children.insert( (*child) );
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
	if ( (node->multi_mother.size()>2 || node->mother->getOtype() != OutType::BOOL)) {
			for (int j=0; j<node->multi_mother.size(); ++j) {
				bool_node * m = node->multi_mother[j];
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
					CONST_node* cn = dynamic_cast<CONST_node*>(n->mother);
					Assert(cn != NULL && cn->getVal()==1, "If a node ignores asserts, it should have a constant one condition");
				}
				/*if(uf->dependent()){
					Assert(uf->ignoreAsserts, "Dependent ufun nodes should ignore asserts");
					Assert(uf->multi_mother.size() == 1 && 
						uf->multi_mother[0]->type == bool_node::UFUN, "Multi-mother of a dependent node should be a ufun node.");
					UFUN_node* mf = dynamic_cast<UFUN_node*>(uf->multi_mother[0]);
					Assert(mf->fgid == uf->fgid, "Mother of dependent node should have the same fgid");
				}else{*/
					if(uf->multi_mother.size() == 1){
						UFUN_node* mf = dynamic_cast<UFUN_node*>(uf->multi_mother[0]);
						/*if(mf != NULL){
							Assert(mf->fgid != uf->fgid, "Looks like this should have been a dependent node.");
						}*/
					}
				//}
			}
			if(n->mother != NULL){
				bool_node* par = n->mother;
				Assert( nodeset.count(n->mother)==1, "Mother is not in dag "<<n->get_name()<<"  "<<i << "  " << n->mother->get_name());
				Assert( par->children.count(n) != 0, "My mother has disowned me "<<n->get_name()<<"  "<<i);
			}
			if(n->father != NULL){
				bool_node* par = n->father;
				Assert( nodeset.count(n->father)==1, "Father is not in dag "<<n->get_name()<<"  "<<i);
				Assert( par->children.count(n) != 0, "My father has disowned me "<<n->get_name()<<"  "<<i);
			}
			if(n->isArith()){
				arith_node* an = dynamic_cast<arith_node*>(n);
				for(int t=0; t<an->multi_mother.size(); ++t){
					if(an->multi_mother[t] != NULL){
						bool_node* par = an->multi_mother[t];
						Assert( nodeset.count(par)==1, "Multi-Mother is not in dag "<<n->get_name()<<"  "<<i << "  " << par->get_name());
						Assert( par->children.count(n) != 0, "My multimother has disowned me "<<n->get_name()<<"  "<<i);
					}
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
	  vector<bool_node*>& vn = nodesByType[bool_node::ASSERT];
	  {
		  DllistNode* cur = assertions.head;
		  while(cur != NULL){
			  //if(typeid(*cur) == typeid(ASSERT_node) || typeid(*cur) == typeid(DST_node)){
			  bool_node* tbn = dynamic_cast<bool_node*>(cur);
			  //if(tbn->type != bool_node::ASSERT){ cout<<tbn->lprint()<<endl;}
				  idx = tbn->back_dfs(idx);
				//  			  if(tbn->type != bool_node::ASSERT){ cout<<tbn->lprint()<<endl;}
			  //}
			  cur = cur->next;
		  }
		  sort(vn.begin(), vn.end(), comp_id);
	  }
  }
  
  
  for(int i=0; i < nodes.size(); ++i){
  	if(nodes[i]->flag == 0 && 
  		nodes[i]->type != bool_node::SRC && 
  		nodes[i]->type != bool_node::CTRL){
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
  		onode->type != bool_node::SRC && 
  		onode->type != bool_node::CTRL){
  		  	
		if(onode->isInter()){
			INTER_node* inonode = dynamic_cast<INTER_node*>(onode);	
			map<string, INTER_node*>::iterator it = named_nodes.find(inonode->name);
			if(it != named_nodes.end() && it->second==inonode){
				named_nodes.erase(it);
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
   DllistNode* cur = this->assertions.head;
  DllistNode* last=NULL;
  for(int i=0; i < nodes.size(); ++i){
	bool_node* onode = nodes[i];
	if( isDllnode(onode) ){
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
  Assert(named_nodes[son]->father == NULL, "You should not call this function if you already have a father.");
  named_nodes[son]->father = named_nodes[father];
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
  Assert(named_nodes[son]->mother == NULL, "You should not call this function if you already have a father.");
  named_nodes[son]->mother = named_nodes[mother];
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
		
	if( node->type == bool_node::SRC || node->type == bool_node::DST || node->type == bool_node::CTRL || node->type == bool_node::ASSERT){
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
		cout<<"WARNING, DANGEROUS!! " << name << endl;
		fth = new CONST_node(-333);
		nodes.push_back(fth);
	}
  }
  return fth;
}


bool_node* BooleanDAG::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){
                                	
  bool_node* tmp = newNode(t);
  tmp->father = father;
  tmp->mother = mother;  
  tmp->addToParents();
  Assert( tmp->id != -22, "This node should not exist anymore");
  tmp->id = nodes.size() + offset;
  nodes.push_back(tmp);  
  if(t == bool_node::SRC || t == bool_node::DST || t == bool_node::CTRL || t == bool_node::ASSERT){
  		nodesByType[t].push_back(tmp);
  }
  return tmp;
}


vector<bool_node*>& BooleanDAG::getNodesByType(bool_node::Type t){
	return 	nodesByType[t];
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

INTER_node* BooleanDAG::create_controls(int n, const string& gen_name, bool toMinimize, bool angelic, bool spConcretize, int max){
  INTER_node* tmp = create_inter(n, gen_name, n_controls, bool_node::CTRL);  
  dynamic_cast<CTRL_node*>(tmp)->set_toMinimize(toMinimize);
  if (angelic) dynamic_cast<CTRL_node*>(tmp)->set_Special_Angelic();
  if (spConcretize) {
    dynamic_cast<CTRL_node*>(tmp)->special_concretize(max);
  }
  return tmp;
}

INTER_node* BooleanDAG::create_outputs(int n, bool_node* nodeToOutput, const string& gen_name){
  INTER_node* tmp = create_inter(n, gen_name, n_outputs, bool_node::DST);
  tmp->mother = nodeToOutput;
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

void BooleanDAG::mrprint(ostream& out){
  
  out<<"dag "<< this->get_name()<<" :"<<endl;
    for(map<string,OutType*>::iterator itr = OutType::tupleMap.begin(); itr != OutType::tupleMap.end(); ++itr){
        out<<"TUPLE_DEF "<<itr->first;
        vector<OutType*> entries = dynamic_cast<Tuple*>(itr->second)->entries;
        for(int i=0;i< entries.size();i++){
            out<<" "<<entries[i]->str();
        }
        out<<endl;
        out.flush();
    }
  for(int i=0; i<nodes.size(); ++i){
  	if(nodes[i] != NULL){
  		out<<nodes[i]->mrprint()<<endl;
  	}    
  }
  out.flush();
}

void BooleanDAG::lprint(ostream& out){    
	out<<"dag"<< this->get_name() <<"{"<<endl;
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
		vector<bool_node*>& sn = getNodesByType(bool_node::SRC);
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
				(*node_it)->type == bool_node::SRC){
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
			if( (*node_it)->type == bool_node::CTRL ||  (*node_it)->type == bool_node::ASSERT ){
				nodesByType[(*node_it)->type].push_back((*node_it));
				if( (*node_it)->type == bool_node::CTRL ){
					INTER_node* inode = dynamic_cast<INTER_node*>(*node_it);
					named_nodes[inode->name] = inode;
				}
				if( (*node_it)->type == bool_node::ASSERT ){
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
            
            if (otherDst->mother->type == bool_node::TUPLE_CREATE) {
                TUPLE_CREATE_node* inodeTuple = dynamic_cast<TUPLE_CREATE_node*>(inode->mother);
                TUPLE_CREATE_node* otherDstTuple = dynamic_cast<TUPLE_CREATE_node*>(otherDst->mother);
                int inodeCount = inodeTuple->multi_mother.size();
                int otherDstCount = otherDstTuple->multi_mother.size();
                Assert(inodeCount == otherDstCount, "Number of outputs should be the same" << (inode)->name<<endl);
                for (int i = 0; i < inodeCount; i++) {
                    EQ_node* eq = new EQ_node();
                    eq->mother = otherDstTuple->multi_mother[i];
                    eq->father = inodeTuple->multi_mother[i];
                    
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

                    ASSERT_node* finalAssert = new ASSERT_node();			
                    finalAssert->setMsg( mm );
                    finalAssert->mother = eq;
                    finalAssert->addToParents();
                    nodes.push_back(finalAssert);

                    nodesByType[finalAssert->type].push_back(finalAssert);
                    assertions.append( getDllnode(finalAssert) );
                }
                if(replacements.count((*node_it)->mother)==0){
                    (*node_it)->dislodge();
                }
                delete (*node_it);
            } else {
                EQ_node* eq = new EQ_node();
                eq->father = otherDst->mother;
                eq->mother = (*node_it)->mother;
                
                //eq->addToParents();
                
                Dout(cout<<"           switching inputs "<<endl);
                eq->switchInputs(*this, replacements);
                
                Dout(cout<<"           replacing "<<otherDst->get_name()<<" with "<<eq->get_name()<<endl);
                Assert( nodes[otherDst->id] == otherDst, "The replace won't work, because the id's are wrong");
                
                string mm = "The spec and sketch can not be made to be equal. ";
                mm += otherDst->name;
                
                replace( otherDst->id, eq);
                if(replacements.count((*node_it)->mother)==0){
                    (*node_it)->dislodge();
                }
                delete (*node_it);
                nodes.push_back( eq );
                
                ASSERT_node* finalAssert = new ASSERT_node();			
                finalAssert->setMsg( mm );
                finalAssert->mother = eq;
                finalAssert->addToParents();
                nodes.push_back(finalAssert);
                
                nodesByType[finalAssert->type].push_back(finalAssert);
                assertions.append( getDllnode(finalAssert) );
            }
            
		}
	}

	nodesByType[bool_node::DST].clear();
	
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
			Assert( (*node_it)->id != -22 , "This node has already been deleted "<<	(*node_it)->get_name() );
			bool_node* bn = (*node_it)->clone(false);
			
			if( dl != NULL && isDllnode(bn) ){
				dl->append(getDllnode(bn));
			}

			Dout( cout<<" node "<<(*node_it)->get_name()<<" clones into "<<bn->get_name()<<endl );
			nstore[nnodes] = bn;
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



BooleanDAG* BooleanDAG::clone(){
	Dout( cout<<" begin clone "<<endl );
	BooleanDAG* bdag = new BooleanDAG(name, isModel);
	relabel();

	clone_nodes(bdag->nodes, &bdag->assertions);

	Dout( cout<<" after redirectPointers "<<endl );
	bdag->n_controls = n_controls;
	bdag->n_inputs = n_inputs;
	bdag->n_outputs = n_outputs;
	bdag->intSize = intSize;

	for(map<string, INTER_node*>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it){
		Assert( it->second->id != -22 , "This node has already been deleted "<<it->first<<endl );
		Assert( bdag->nodes.size() > it->second->id, " Bad node  "<<it->first<<endl );
		bdag->named_nodes[it->first] = dynamic_cast<INTER_node*>(bdag->nodes[it->second->id]);	
	}
	
	for(map<bool_node::Type, vector<bool_node*> >::iterator it =nodesByType.begin(); 
					it != nodesByType.end(); ++it){
		vector<bool_node*>& tmp = bdag->nodesByType[it->first];
		Assert( tmp.size() == 0, "This can't happen. This is an invariant.");
		for(int i=0; i<it->second.size(); ++i){
			Assert( it->second[i]->id != -22 , "This node has already been deleted "<<it->second[i]->get_name()<<endl );			
			tmp.push_back( bdag->nodes[ it->second[i]->id ] );	
		}							
	}
	return bdag;
}


void BooleanDAG::registerOutputs(){
	 vector<bool_node*>& vn = getNodesByType(bool_node::DST);
	 for(int i=0; i<vn.size(); ++i){
		assertions.append( getDllnode(vn[i]) );
	 }
}












