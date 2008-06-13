// BooleanDAG.cpp: implementation of the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#include "BooleanDAG.h"
#include "BasicError.h"
#include "SATSolver.h"

#include <sstream>
#include <algorithm>


using namespace std;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

BooleanDAG::BooleanDAG()
{
  has_passthrough=false;
  is_layered=false;
  is_sorted=false;
  n_inputs = 0;
  n_outputs = 0;
  n_controls = 0;
}


void BooleanDAG::clear(){	
  for(int i=0; i < nodes.size(); ++i){
  	nodes[i]->id = -22;
  	delete nodes[i];
  	nodes[i] = NULL;
  }
  nodes.clear();
  named_nodes.clear();
}


BooleanDAG::~BooleanDAG()
{
  
}

void BooleanDAG::relabel(){
  for(int i=0; i < nodes.size(); ++i){
  	if( nodes[i] != NULL ){
  		Assert( nodes[i]->id != -22, "This node should be dead (relabel) "<<nodes[i]->get_name());
    	nodes[i]->id = i;
  	}
  }
}



void BooleanDAG::sort_graph(){
  for(int i=0; i < nodes.size(); ++i){
    nodes[i]->flag = 0;
  }
  int idx=nodes.size()-1;
  {
    //for(int i=n_inputs-1; i >=0 ; --i){
    for(int i=0; i < nodes.size(); ++i){
      if( nodes[i]->type  == bool_node::SRC || nodes[i]->type  == bool_node::CTRL || (nodes[i]->father == NULL && nodes[i]->mother == NULL) ){
	      idx = nodes[i]->do_dfs(idx);
      }
    }
    //}
  }
  if( idx != -1){
  	bool check=true;
  	string msg = " ";
    for(int i=0; i < nodes.size(); ++i){
    	if(! (nodes[i]->flag == 1 || nodes[i]->type == bool_node::DST) ){
	   		msg += "There are some nodes in the graph that are isolated ";
	      	msg += nodes[i]->get_name();
	      	msg += "\n";
	      	check = false;
    	}
    }
    Assert(check, msg);
  }
  sort(nodes.begin(), nodes.end(), comp_id);
  is_sorted = true;
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
      nodes[i]->set_layer();
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
      Assert(lid.size() > 0, "This should not happen.");
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
	
	map<string, bool_node*>::iterator it = named_nodes.find(onode->name);
	if(it != named_nodes.end() && it->second==onode){
		named_nodes.erase(it);
	}
	
	
	
	if( onode->type == bool_node::SRC || onode->type == bool_node::DST || onode->type == bool_node::CTRL ){
		vector<bool_node*>& bnv = nodesByType[onode->type];
		for(int i=0; i<bnv.size(); ){
			if( bnv[i] == onode ){
				bnv.erase( bnv.begin() + i);
			}else{
				++i;	
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
  

	map<string, bool_node*>::iterator it = named_nodes.find(onode->name);
	if(it != named_nodes.end() && it->second==onode){
		named_nodes.erase(it);
	}
  
  
  onode->id = -22;
  delete onode;
  nodes[i] = NULL;  
}



void BooleanDAG::repOK(){

	cout<<"*** DOING REPOK ****"<<endl;
	//First, we check that the array doesn't contain any repeated nodes.
	map<bool_node*, int> nodeset;
	for(int i=0; i<nodes.size(); ++i){
		if(nodes[i] != NULL){
		Assert(nodeset.count(nodes[i]) == 0, "There is a repeated node!!! it's in pos "<<i<<" and "<<nodeset[nodes[i]] );		
		nodeset[nodes[i]] = i;
		}
	}

	//Now, we have to check whether all the node's predecessors are in the nodeset.
	//We also check that each node is in the children of all its parents.
	for(int i=0; i<nodes.size(); ++i){
		bool_node* n = nodes[i];
		if(n != NULL){
			if(n->mother != NULL){
				bool_node* par = n->mother;
				Assert( nodeset.count(n->mother)==1, "Mother is not in dag "<<n->get_name()<<"  "<<i);
				Assert( par->children.count(n) != 0, "My mother has disowned me "<<n->get_name()<<"  "<<i);
			}
			if(n->father != NULL){
				bool_node* par = n->father;
				Assert( nodeset.count(n->father)==1, "Father is not in dag "<<n->get_name()<<"  "<<i);
				Assert( par->children.count(n) != 0, "My father has disowned me "<<n->get_name()<<"  "<<i);
			}
			if(n->type == bool_node::ARITH){
				arith_node* an = dynamic_cast<arith_node*>(n);
				for(int t=0; t<an->multi_mother.size(); ++t){
					if(an->multi_mother[t] != NULL){
						bool_node* par = an->multi_mother[t];
						Assert( nodeset.count(par)==1, "Mother is not in dag "<<n->get_name()<<"  "<<i);
						Assert( par->children.count(n) != 0, "My multimother has disowned me "<<n->get_name()<<"  "<<i);
					}
				}
			}

			for(child_iter child = n->children.begin(); child != n->children.end(); ++child){
				Assert( nodeset.count(*child) == 1, "This child is outside the network "<<(*child)->get_name()<<"  "<<i);
			}

		}
	}
}






//This routine performs simple peephole optimizations
//on the boolean function.
void BooleanDAG::cleanup(bool moveNots){
	

  //The first optimization is to remove nodes that don't contribute to the output. 	  
  for(int i=0; i < nodes.size(); ++i){
    nodes[i]->flag = 0;
  }
  int idx=nodes.size()-1;
  {
    //for(int i=n_inputs-1; i >=0 ; --i){
    for(int i=0; i < nodes.size(); ++i){
      if( nodes[i]->type == bool_node::DST
          || nodes[i]->type == bool_node::ASSERT)
      {
	      idx = nodes[i]->back_dfs(idx);
      }
    }
    //}
  }
  for(int i=0; i < nodes.size(); ++i){
  	if(nodes[i]->flag == 0 && 
  		nodes[i]->type != bool_node::SRC && 
  		nodes[i]->type != bool_node::CTRL){
		nodes[i]->dislodge();  		
  	}
  }
  for(int i=0; i < nodes.size(); ++i){
	bool_node* onode = nodes[i];
  	if(onode->flag == 0 && 
  		onode->type != bool_node::SRC && 
  		onode->type != bool_node::CTRL){
  		  		
		map<string, bool_node*>::iterator it = named_nodes.find(onode->name);
		if(it != named_nodes.end() && it->second==onode){
			named_nodes.erase(it);
		}
  			
  		onode->id = -22;	
  		delete onode;
		nodes[i] = NULL;
  	}
  }
  
  removeNullNodes();  
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
	if(node->name.size() > 0){
		named_nodes[node->name] = node;
	}
		
	if( node->type == bool_node::SRC || node->type == bool_node::DST || node->type == bool_node::CTRL ){
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
		fth = new CONST_node(-333);
		nodes.push_back(fth);
	}
  }
  return fth;
}


bool_node* BooleanDAG::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){
                                	
  bool_node* tmp = newBoolNode(t);
  tmp->father = father;
  tmp->mother = mother;  
  tmp->addToParents();
  Assert( tmp->id != -22, "This node should not exist anymore");
  tmp->id = nodes.size() + offset;
  nodes.push_back(tmp);  
  if(t == bool_node::SRC || t == bool_node::DST || t == bool_node::CTRL ){
  		nodesByType[t].push_back(tmp);
  }
  return tmp;
}


vector<bool_node*>& BooleanDAG::getNodesByType(bool_node::Type t){
	return 	nodesByType[t];
}


bool_node* BooleanDAG::create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type){
	//Create interface nodes, either source, dest, or ctrl.
	
	if( named_nodes.find(gen_name) != named_nodes.end() ){
		return named_nodes[gen_name];
	}
	
  if(n < 0){
    bool_node* tmp = newBoolNode(type);
    nodesByType[type].push_back(tmp);
    tmp->ion_pos = counter;
    tmp->id = nodes.size() + offset;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = bool_node::BOOL;
    ++counter;
	return tmp;
  }else{
  	bool_node* tmp = newBoolNode(type);
  	nodesByType[type].push_back(tmp);
    dynamic_cast<INTER_node*>(tmp)->set_nbits(n);
    tmp->ion_pos = counter;
    tmp->id = nodes.size() + offset;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = bool_node::INT;
    counter += n;
	return tmp;
  }
}


bool_node* BooleanDAG::create_inputs(int n, const string& gen_name){
	return create_inter(n, gen_name, n_inputs, bool_node::SRC);
}

bool_node* BooleanDAG::create_controls(int n, const string& gen_name){
  return create_inter(n, gen_name, n_controls, bool_node::CTRL);  
}

bool_node* BooleanDAG::create_outputs(int n, bool_node* nodeToOutput, const string& gen_name){
  bool_node* tmp = create_inter(n, gen_name, n_outputs, bool_node::DST);
  tmp->mother = nodeToOutput;
  tmp->addToParents();
  return tmp;
}


bool_node* BooleanDAG::create_outputs(int n, const string& gen_name){
  bool_node* tmp = create_inter(n, gen_name, n_outputs, bool_node::DST);
  return tmp;
}






void BooleanDAG::print(ostream& out)const{    
  out<<"digraph G{"<<endl;
  for(int i=0; i<nodes.size(); ++i){
  	if(nodes[i] != NULL){
  		nodes[i]->outDagEntry(out);
  	}    
  }

  out<<"}"<<endl;
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



void BooleanDAG::makeMiter(BooleanDAG& bdag){
	bool_node* tip = NULL; 
	relabel();
	bdag.relabel();
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		Assert( (*node_it) != NULL, "Can't make a miter when you have null nodes.");
		Dout( cout<<" adding "<<(*node_it)->get_name()<<endl );		
				
		if( (*node_it)->type != bool_node::SRC && (*node_it)->type != bool_node::DST){ 
			nodes.push_back( (*node_it) );
			(*node_it)->switchInputs(*this);
		}
		
		if( (*node_it)->type == bool_node::SRC ){
			if( !has_name((*node_it)->name) ){
				nodes.push_back( (*node_it) );
				nodesByType[(*node_it)->type].push_back((*node_it));	
				named_nodes[(*node_it)->name] = (*node_it);
			}
		}
		
		if( (*node_it)->type == bool_node::CTRL){//Shouldn't there be a nodes.push_back here???
			nodesByType[(*node_it)->type].push_back((*node_it));
		}
		if( (*node_it)->type == bool_node::DST){
			nodesByType[(*node_it)->type].push_back((*node_it));
			bool_node* otherDst = named_nodes[(*node_it)->name];
			Assert(otherDst != NULL, "AAARGH: Node is not registered "<<(*node_it)->name<<endl);
			EQ_node* eq = new EQ_node();			
			eq->father = otherDst->mother;
			eq->mother = (*node_it)->mother;
			
			eq->addToParents();			
			
			Dout(cout<<"           switching inputs "<<endl);
			eq->switchInputs(*this);
			
			Dout(cout<<"           replacing "<<otherDst->get_name()<<" with "<<eq->get_name()<<endl);
			Assert( nodes[otherDst->id] == otherDst, "The replace won't work, because the id's are wrong");
			replace( otherDst->id, eq);	
			(*node_it)->dislodge();
			nodes.push_back( eq );	

			ASSERT_node* finalAssert = new ASSERT_node();
			finalAssert->setMsg("The spec and sketch can not be made to be equal.");
			finalAssert->mother = eq;
			finalAssert->addToParents();
			nodes.push_back(finalAssert);
		}
	}

	nodesByType[bool_node::DST].clear();
	
	removeNullNodes();
	sort_graph();
	relabel();
}




void BooleanDAG::rename(const string& oldname,  const string& newname){
	bool_node* node = named_nodes[oldname];
	node->name = newname;
	named_nodes.erase(oldname);
	named_nodes[newname] = node;	
}



void BooleanDAG::clone_nodes(vector<bool_node*>& nstore){
	nstore.resize(nodes.size());

	Dout( cout<<" after relabel "<<endl );
	int nnodes = 0;
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		if( (*node_it) != NULL ){		
			Assert( (*node_it)->id != -22 , "This node has already been deleted "<<	(*node_it)->get_name() );
			bool_node* bn = (*node_it)->clone();
			Dout( cout<<" node "<<(*node_it)->get_name()<<" clones into "<<bn->get_name()<<endl );
			nstore[nnodes] = bn;
			nnodes++;
		}else{
			Assert( false, "The graph you are cloning should not have any null nodes.");
		}
	}
	Dout( cout<<" after indiv clone "<<endl );
	nstore.resize(nnodes);
	for(BooleanDAG::iterator node_it = nstore.begin(); node_it != nstore.end(); ++node_it){		
		(*node_it)->redirectPointers(*this, (vector<const bool_node*>&) nstore);
	}
}



BooleanDAG* BooleanDAG::clone(){
	Dout( cout<<" begin clone "<<endl );
	BooleanDAG* bdag = new BooleanDAG();
	relabel();

	clone_nodes(bdag->nodes);

	Dout( cout<<" after redirectPointers "<<endl );
	bdag->n_controls = n_controls;
	bdag->n_inputs = n_inputs;
	bdag->n_outputs = n_outputs;
	for(map<string, bool_node*>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it){
		Assert( it->second->id != -22 , "This node has already been deleted "<<it->first<<endl );
		Assert( bdag->nodes.size() > it->second->id, " Bad node  "<<it->first<<endl );
		bdag->named_nodes[it->first] = bdag->nodes[it->second->id];	
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















