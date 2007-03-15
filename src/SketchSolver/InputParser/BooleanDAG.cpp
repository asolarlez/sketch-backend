// BooleanDAG.cpp: implementation of the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#include "BooleanDAG.h"
#include "BasicError.h"
#include "SATSolver.h"

#include <sstream>

using namespace std;

string bool_node::get_name(){
    stringstream str;
    if(name.size() > 0)
      str<<name<<"__"<<get_tname();
    else{      
      str<<"name_"<<abs(id)<<"_"<<this<<"__"<<get_tname();
      
    }    
    str<<"_"<<this;
    //str<<":"<<id;
    Assert( id != -22, "This is a corpse. It's living gargabe "<<str.str()<<" id ="<<id );
    return str.str();
  }


void bool_node::set_layer(){
  layer = 0;
  if(mother != NULL){
    layer = mother->layer + 1;
  }
  if(father != NULL && father->layer >= layer){
    layer = father->layer + 1;
  }
}

int bool_node::do_dfs(int idx){
	Assert( id != -22, "This node should be dead (dfs) "<<this->get_name());
  if( flag != 0)
    return idx;
  flag = 1;    
  for(int i=0; i< children.size(); ++i){  	
    idx = children[i]->do_dfs(idx);
  }
  id = idx;
  return idx-1;
}

int bool_node::back_dfs(int idx){
	Assert( id != -22, "This node should be dead (back_dfs)"<<this->get_name());
  if( flag != 0)
    return idx;
  flag = 1;    
  if(father != NULL){
  	 idx = father->back_dfs(idx);
  }
  if(mother != NULL){
  	 idx = mother->back_dfs(idx);
  }
  id = idx;
  return idx-1;
}


int arith_node::back_dfs(int idx){
	Assert( id != -22, "This node should be dead (arith:back_dfs) "<<this->get_name());
  if( flag != 0)
    return idx;
  flag = 1;    
  if(father != NULL){
  	 idx = father->back_dfs(idx);
  }
  if(mother != NULL){
  	 idx = mother->back_dfs(idx);
  }
  for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it != NULL){
	  	idx = (*it)->back_dfs(idx);
  	}
  }
  id = idx;
  return idx-1;
}



void bool_node::remove_child(bool_node* bn){
	vector<bool_node*>& tmpv = children;
	for(int k=0; k<tmpv.size(); ){
	  if( tmpv[k] == bn ){
	    tmpv.erase( tmpv.begin() + k );
	  }else{
	    ++k;
	  }
	}
}


void bool_node::replace_parent(const bool_node * oldpar, bool_node* newpar){
	Assert( oldpar != NULL, "oldpar can't be null");
	Assert( newpar != NULL, "oldpar can't be null");
	if(father == oldpar){
  		father = newpar;
  		//oldpar->remove_child(this);
  		//we assume the old parent is going to be distroyed, so we don't
  		//bother modifying it.
  		newpar->children.push_back( this );
  	}
  	if(mother == oldpar){
  		mother = newpar;
  		//oldpar->remove_child(this);
  		//we assume the old parent is going to be distroyed, so we don't
  		//bother modifying it.
  		newpar->children.push_back( this );
  	}
}


void arith_node::replace_parent(const bool_node * oldpar, bool_node* newpar){
	bool_node::replace_parent(oldpar, newpar);
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it == oldpar){
	  		*it = newpar;
	  		//oldpar->remove_child(this);
  			newpar->children.push_back( this );
	  	}
	}
}




void arith_node::printSubDAG(ostream& out){	
	int i=0;
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
	  	if(*it != NULL){
	  		(*it)->printSubDAG(out);  		
	  	}
	}
	bool_node::printSubDAG(out);
}


void bool_node::printSubDAG(ostream& out){
	if( father != NULL){
			father->printSubDAG(out);
	}	
	if(mother != NULL){
			mother->printSubDAG(out);
	}
	outDagEntry(out);	
}



void bool_node::outDagEntry(ostream& out){
	if( father != NULL){
        out<<" "<<father->get_name()<<" -> "<<get_name()<<" ; "<<endl;
    }
    if( mother != NULL){
          out<<" "<<mother->get_name()<<" -> "<<get_name()<<" ; "<<endl;
    }
    if(father == NULL && mother == NULL){
   // 	  Dout( out<<"// orphan node: "<<get_name()<<" ; "<<endl );
    }
    for(int i=0; i<children.size() ; ++i){
    //	Dout( out<<"// "<<get_name()<<" -> "<<children[i]->get_name()<<" ; "<<endl );
    }
}


void arith_node::outDagEntry(ostream& out){
	bool_node::outDagEntry(out);
	int i=0;
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
	  	if(*it != NULL){
	  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<" ; "<<endl;	  		
	  	}
	}
}


void bool_node::dislodge(){
  if(father != NULL){
  	 father->remove_child(this);
  }
  if(mother != NULL){
  	mother->remove_child(this);
  }
}

void arith_node::dislodge(){
	bool_node::dislodge();
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it != NULL){
	  		(*it)->remove_child(this);	
	  	}
	}
}



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
  new_names = 0;
  
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
	Assert( i < nodes.size(), "Out of bounds violation "<<i<<" >= "<<nodes.size()<<endl);
	Assert( replacement != NULL, "Why are you replacing with a null replacement");
	Assert( replacement->id != -22, "Why are you replacing with a corpse?");
	bool_node* onode = nodes[i];
	Assert( onode != NULL, "This can't happen");
	onode->dislodge();
	vector<bool_node*> tmpchild = onode->children;
	for(int k=0; k<onode->children.size(); ++k){		
		bool_node* cchild =  onode->children[k];
		cchild->replace_parent(onode, replacement);
	}
	//
	if(named_nodes.find(onode->name) != named_nodes.end() && named_nodes[onode->name]==onode){
		named_nodes.erase(onode->name);
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
  Assert( nodes[i]->father == nodes[i]->mother, "This must be true, otherwise, the compiler is wrong");  
  Assert( nodes[i]->father != NULL, "Can this happen? To me? Nah  ");

	//Removing from the father's children list. Note we are assuming father==mother.
	
	nodes[i]->father->remove_child(nodes[i]);
	
  for(int j=0; j<nodes[i]->children.size(); ++j){
    if(  nodes[i]->children[j]->father == nodes[i] ){
      nodes[i]->children[j]->father = nodes[i]->father;
      nodes[i]->father->children.push_back( nodes[i]->children[j] );      
    }
    if(  nodes[i]->children[j]->mother == nodes[i] ){
      nodes[i]->children[j]->mother = nodes[i]->father;
      nodes[i]->father->children.push_back( nodes[i]->children[j] );
    }
  }
  delete nodes[i];
  nodes[i] = NULL;  
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
  for(int i=0; i < nodes.size(); ){
  	if(nodes[i]->flag == 0 && 
  		nodes[i]->type != bool_node::SRC && 
  		nodes[i]->type != bool_node::CTRL){
  		delete nodes[i];
		nodes[i] = NULL;
		nodes.erase( nodes.begin() + i);
  	}else{
  		++i;
  	}
  }
  
  
  
  
  for(int i=0; i< nodes.size(); ){    

    //Now, this optimization will remove redundant 
    //ands and ors, taking advantage of the fact
    //that (a AND a) == a, and also (a OR a) == a
    if( nodes[i]->father == nodes[i]->mother ){
      switch(nodes[i]->type){
      case bool_node::AND:         
        {
          remove(i); ++i;              
        }
        break;
      case bool_node::OR:         
        {
          remove(i); ++i;      
        }
        break;
      default:
        ++i;
      }// switch(nodes[i]->type)
    }else{
      ++i;
    }//  if( nodes[i]->father == nodes[i]->mother )    
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
  named_nodes[father]->children.push_back( named_nodes[son] );
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
  named_nodes[mother]->children.push_back( named_nodes[son] );
}




void BooleanDAG::addNewNode(bool_node* node){
	Assert( node != NULL, "null node can't be added.");
	Assert( node->id != -22, "This node should not exist anymore");	
	node->id = nodes.size();
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
	for(int i=0; i<v.size(); ++i){
		if(v[i] != NULL){
			addNewNode(v[i]);
		}
	}
}


bool_node* BooleanDAG::get_node(const string& name){
  bool_node* fth;
  Assert(name.size()==0 || named_nodes.find(name) != named_nodes.end(), "name does not exist: "<<name);
  if(name.size()==0){
    fth = NULL;
  }else{
    fth = named_nodes[name];
  }
  return fth;
}


bool_node* BooleanDAG::new_node(const string& mother, 
                                const string& father, bool_node::Type t, const string& p_name){	
  bool_node* fth;
  bool_node* mth;
  try{
    Assert(father.size()==0 || named_nodes.find(father) != named_nodes.end(), "The father name does not exist: "<<father);
  }catch(BasicError& be){
    if(father.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<father.substr(6)<<endl;
    }    
    throw be;
  }
  try{
    Assert(mother.size()==0 ||named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist: "<< mother);
  }catch(BasicError& be){    
    if(mother.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<mother.substr(6)<<endl;
    }
    throw be;
  }
  if(father.size()==0){
    fth = NULL;
  }else{
    fth = named_nodes[father];
  }

  if(mother.size()==0){
    mth = NULL;
  }else{
    mth = named_nodes[mother];
  }

	string name = p_name;
  if(name.size() == 0){ name = new_name(); }
  map<string, bool_node*>::iterator it =  named_nodes.find(name);

  bool_node* tmp;
  if( it == named_nodes.end() ){
    tmp = new_node(mth, fth, t);    
    tmp->name = name;
    named_nodes[name] = tmp;
  }else{
    tmp = it->second;
    set_node(tmp, mth, fth, t);
  }
  return tmp;
}

bool_node* BooleanDAG::new_node(const string& mother, 
                                const string& father,  bool_node* thenode){
  bool_node* fth;
  bool_node* mth;
  try{
    Assert(father.size()==0 || named_nodes.find(father) != named_nodes.end(), "The father name does not exist: "<<father);
  }catch(BasicError& be){
    if(father.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<father.substr(6)<<endl;
    }    
    throw be;
  }
  try{
    Assert(mother.size()==0 ||named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist: "<<mother);
  }catch(BasicError& be){    
    if(mother.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<mother.substr(6)<<endl;
    }
    throw be;
  }
  if(father.size()==0){
    fth = NULL;
  }else{
    fth = named_nodes[father];
  }

  if(mother.size()==0){
    mth = NULL;
  }else{
    mth = named_nodes[mother];
  }

  string& name = thenode->name;
  
	bool_node* tmp;
	tmp = new_node(mth, fth, thenode);	
  
  return tmp;
}


bool_node* BooleanDAG::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){
                                	
  bool_node* tmp = newBoolNode(t);
  set_node(tmp, mother,  father,  t);
  Assert( tmp->id != -22, "This node should not exist anymore");
  tmp->id = nodes.size();
  nodes.push_back(tmp);  
  if(t == bool_node::SRC || t == bool_node::DST || t == bool_node::CTRL ){
  		nodesByType[t].push_back(tmp);
  }
  return tmp;
}


bool_node* BooleanDAG::new_node(bool_node* mother,  
                                bool_node* father,  bool_node* thenode){
                                	
  bool_node* tmp = thenode;
  bool_node::Type t = tmp->type;
  set_node(tmp, mother, father, t);
  Assert( tmp->id != -22, "This node should not exist anymore");
  tmp->id = nodes.size();
  nodes.push_back(tmp);
  if( tmp->name.size() > 0){
  	named_nodes[tmp->name] = tmp;	
  }
  if(t == bool_node::SRC || t == bool_node::DST || t == bool_node::CTRL ){
  		nodesByType[t].push_back(tmp);
  }
  return tmp;
}


bool_node* BooleanDAG::new_node(bool_node* mother,  
                      bool_node* father, bool_node* thenode, const string& name){

  named_nodes[name] = thenode;
  return new_node(mother, father, thenode);
                      	
}





bool_node* BooleanDAG::set_node(bool_node* tmp, bool_node* mother,  
                                bool_node* father, bool_node::Type t){

  tmp->father = father;
  tmp->mother = mother;  

  if( t != tmp->type){
    if(t == bool_node::SRC){
      tmp->ion_pos = n_inputs;
      ++n_inputs;
    }else{
      if(t == bool_node::DST){
        tmp->ion_pos = n_outputs;
        ++n_outputs;
      }
    }
  }


  tmp->addToParents();
    

  tmp->type = t;  
  return tmp;
}


vector<bool_node*>& BooleanDAG::getNodesByType(bool_node::Type t){
	return 	nodesByType[t];
}


string BooleanDAG::create_const(int n){
	CONST_node* c = new CONST_node(n);		
	nodes.push_back(c);
	string s = new_name();
	named_nodes[s] = c;
	c->name = s;
	return s;
}


void BooleanDAG::create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type){
	//Create interface nodes, either source, dest, or ctrl.
	
	if( named_nodes.find(gen_name) != named_nodes.end() ){
		//cout<< gen_name <<"This interface node already exists"<<endl;
		return;	
	}
	
  if(n < 0){
    bool_node* tmp = newBoolNode(type);
    nodesByType[type].push_back(tmp);
    tmp->ion_pos = counter;
    tmp->id = nodes.size();
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = bool_node::BOOL;
    ++counter;
  }else{
  	bool_node* tmp = newBoolNode(type);
  	nodesByType[type].push_back(tmp);
    dynamic_cast<INTER_node*>(tmp)->set_nbits(n);
    tmp->ion_pos = counter;
    tmp->id = nodes.size();
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    tmp->otype = bool_node::INT;
    counter += n;
  }
}


void BooleanDAG::create_inputs(int n, const string& gen_name){
	create_inter(n, gen_name, n_inputs, bool_node::SRC);
}

int BooleanDAG::create_controls(int n, const string& gen_name){
  create_inter(n, gen_name, n_controls, bool_node::CTRL);  
  return nodes.size();
}

void BooleanDAG::create_outputs(int n, const string& gen_name){
  create_inter(n, gen_name, n_outputs, bool_node::DST);
}






void arith_node::addToParents(){
  bool_node::addToParents();
  for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it != NULL){
	  	(*it)->children.push_back(this);
  	}
  }
}


void bool_node::addToParents(){
  if(father != NULL){
    father->children.push_back(this);
  }
  if(mother != NULL && father != mother){
    mother->children.push_back(this);
  }
}




void arith_node::switchInputs(BooleanDAG& bdag){	
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it != NULL){
	  		if(  (*it)->type == bool_node::SRC ){
	  			if( bdag.has_name((*it)->name)){
    				(*it) = bdag.get_node((*it)->name);
	  			}
	  		}
	  	}
  	}
  	bool_node::switchInputs(bdag);
}


void bool_node::switchInputs(BooleanDAG& bdag){
	if(father != NULL){
		if(  father->type == bool_node::SRC ){
    		if( bdag.has_name(father->name )){
    			father = bdag.get_node(father->name);
    		}
		}
  	}
	if(mother != NULL){
		if(  mother->type == bool_node::SRC ){
			if( bdag.has_name(mother->name )){
				mother = bdag.get_node(mother->name);
			}
		}
	}
	addToParents();
}



void arith_node::redirectPointers(BooleanDAG& oribdag, BooleanDAG& bdag){
  bool_node::redirectPointers(oribdag, bdag);
  for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it != NULL){
  		Assert( (*it)->id != -22, "This node should not exist anymore");
  		if( (*it)->id < oribdag.size() && (*it)== oribdag[(*it)->id]){
	  		(*it) = bdag[(*it)->id];
  		}
  	}
  }
}



void bool_node::redirectPointers(BooleanDAG& oribdag, BooleanDAG& bdag){
	if(father != NULL){
		Assert( father->id != -22, "This node should not exist anymore");
		if( father->id < oribdag.size() && father== oribdag[father->id]){
			//If the father was from the original bdag, then we switch to the father in the new bdag.
	    	father = bdag[father->id];
		}
  	}
	if(mother != NULL){
		Assert( mother->id != -22, "This node should not exist anymore");
		if( mother->id < oribdag.size() && mother== oribdag[mother->id]){
			mother = bdag[mother->id];
		}
	}
	for(int i=0; i<children.size(); ++i){
		Assert( children[i]->id != -22, "This node should not exist anymore");
		if( children[i]->id < oribdag.size() && children[i]== oribdag[children[i]->id]){
			children[i] = bdag[ children[i]->id ];
		}	
	}
}


void BooleanDAG::print(ostream& out){    
  out<<"digraph G{"<<endl;
  for(int i=0; i<nodes.size(); ++i){
  	if(nodes[i] != NULL){
  		nodes[i]->outDagEntry(out);
  	}    
  }

  out<<"}"<<endl;
}


  void BooleanDAG::alias(const string& ssource,  const string& starg){
  	map<string, bool_node*>::iterator it = named_nodes.find(ssource);
  	if( it != named_nodes.end() ){
		new_node(starg, "",  it->second->type, it->second->name);
	}else{
		if( has_alias(starg) ){
		   string p( get_alias(starg) );
	      aliasmap[ssource] =  p;
	    }else{
	      aliasmap[ssource] = starg;
	    }
  	}
  }





void NodeVisitor::process(BooleanDAG& bdag){
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		try{
		(*node_it)->accept(*this);
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    	}
	}
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
	
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		if( *node_it != NULL){
			(*node_it)->addToParents();
		}
	}
}



void BooleanDAG::makeMiter(BooleanDAG& bdag, const string& tip_name ){
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
			}
		}
		
		if( (*node_it)->type == bool_node::CTRL){
			nodesByType[(*node_it)->type].push_back((*node_it));
		}
		if( (*node_it)->type == bool_node::DST){
			nodesByType[(*node_it)->type].push_back((*node_it));
			bool_node* otherDst = named_nodes[(*node_it)->name];
			Assert(otherDst != NULL, "AAARGH: Node is not registered "<<(*node_it)->name<<endl);
			EQ_node* eq = new EQ_node();			
			eq->father = otherDst->mother;
			eq->mother = (*node_it)->mother;
			eq->name = new_name();
			named_nodes[eq->name] = eq;			
			Dout(cout<<"           adding to parents of eq="<<eq->get_name()<<endl);
			eq->addToParents();			
			
			Dout(cout<<"           switching inputs "<<endl);
			eq->switchInputs(*this);
			
			Dout(cout<<"           replacing "<<otherDst->get_name()<<" with "<<eq->get_name()<<endl);
			Assert( nodes[otherDst->id] == otherDst, "The replace won't work, because the id's are wrong");
			replace( otherDst->id, eq);	
			
			nodes.push_back( eq );		
			if( tip == NULL  ){
				tip = eq;				
			}else{
				tip = new_node(tip->name, eq->name, bool_node::AND, new_name() );
			}
		}
	}
	Assert(tip != NULL, "This is not possible!!!");
	create_outputs(1, tip_name);
	Dout(cout<<"      Creating tip, connecting with  "<<tip->get_name()<<endl);
	bool_node* outnode = new_node(tip->name, "", bool_node::DST, tip_name);
	nodesByType[bool_node::DST].clear();
	nodesByType[bool_node::DST].push_back(outnode);
	
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

BooleanDAG* BooleanDAG::clone(){
	Dout( cout<<" begin clone "<<endl );
	BooleanDAG* bdag = new BooleanDAG();
	relabel();
	Dout( cout<<" after relabel "<<endl );
	for(BooleanDAG::iterator node_it = begin(); node_it != end(); ++node_it){
		if( (*node_it) != NULL ){		
			Assert( (*node_it)->id != -22 , "This node has already been deleted "<<	(*node_it)->get_name() );
			bool_node* bn = (*node_it)->clone();
			Dout( cout<<" node "<<(*node_it)->get_name()<<" clones into "<<bn->get_name()<<endl );
			bdag->nodes.push_back( bn );
		}else{
			Assert( false, "The graph you are cloning should not have any null nodes.");
		}
	}	
	Dout( cout<<" after indiv clone "<<endl );
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){		
		(*node_it)->redirectPointers(*this, *bdag);	
	}
	Dout( cout<<" after redirectPointers "<<endl );
	bdag->n_controls = n_controls;
	bdag->n_inputs = n_inputs;
	bdag->n_outputs = n_outputs;
	for(map<string, bool_node*>::iterator it = named_nodes.begin(); it != named_nodes.end(); ++it){
		Assert( it->second->id != -22 , "This node has already been deleted "<<it->first<<endl );
		bdag->named_nodes[it->first] = bdag->nodes[it->second->id];	
	}
	bdag->new_names = new_names;
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















