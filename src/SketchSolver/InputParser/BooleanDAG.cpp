// BooleanDAG.cpp: implementation of the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#include "BooleanDAG.h"
#include "BasicError.h"

#include <sstream>

using namespace std;


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
  Assert(is_sorted, "You can only relabel when the graph is sorted");
  for(int i=0; i < nodes.size(); ++i){
    nodes[i]->id = i;
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
      if( nodes[i]->type  == bool_node::SRC || nodes[i]->type  == bool_node::CTRL){
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

void BooleanDAG::remove(int i){
  Assert( nodes[i]->father == nodes[i]->mother, "This must be true, otherwise, the compiler is wrong");
  Assert( nodes[i]->father_sgn == nodes[i]->mother_sgn, "This must be true, otherwise, the compiler is wrong");
  Assert( nodes[i]->father != NULL, "Can this happen? To me? Nah  ");

	//Removing from the father's children list. Note we are assuming father==mother.
  vector<bool_node*>& tmpv = nodes[i]->father->children;  
  for(int k=0; k<tmpv.size(); ){
    if( tmpv[k] == nodes[i] ){
      tmpv.erase( tmpv.begin() + k );
    }else{
      ++k;
    }
  }
  for(int j=0; j<nodes[i]->children.size(); ++j){
    if(  nodes[i]->children[j]->father == nodes[i] ){
      nodes[i]->children[j]->father = nodes[i]->father;
      nodes[i]->father->children.push_back( nodes[i]->children[j] );
      nodes[i]->children[j]->father_sgn = nodes[i]->children[j]->father_sgn == nodes[i]->father_sgn;
    }
    if(  nodes[i]->children[j]->mother == nodes[i] ){
      nodes[i]->children[j]->mother = nodes[i]->father;
      nodes[i]->father->children.push_back( nodes[i]->children[j] );
      nodes[i]->children[j]->mother_sgn = nodes[i]->children[j]->mother_sgn == nodes[i]->father_sgn;
    }
  }
  delete nodes[i];
  nodes[i] = NULL;
  nodes.erase( nodes.begin() + i);
}


//This routine performs simple peephole optimizations
//on the boolean function.
void BooleanDAG::cleanup(bool moveNots){  
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
    {//This first optimization has as it's purpose to
      //move the negations as far down the tree as we can. 
      //This will guarantee that they bundle up in the same layer,
      //and we end up with less xor matrices, which boils down to less operations.
      if( nodes[i]->type == bool_node::XOR && moveNots){
        //move nots from the father to the children.
        
        //But first, we will use the opportunity to remove duplicate entries from the children list.
        //Otherwise, these will cause problems later.
        for(int jj=0; jj< nodes[i]->children.size(); ++jj){
        	for(int k=jj+1; k< nodes[i]->children.size(); ){
        		if( nodes[i]->children[jj] == nodes[i]->children[k] ){
        			nodes[i]->children.erase( nodes[i]->children.begin() + k );
        		}else{
        			++k;	
        		}
        	}        	        
        }
        
        
        if( !nodes[i]->father_sgn ){
          for(int jj=0; jj< nodes[i]->children.size(); ++jj){
            if( nodes[i]->children[jj]->father == nodes[i] ){
              nodes[i]->children[jj]->father_sgn = ! nodes[i]->children[jj]->father_sgn;
            }
            if( nodes[i]->children[jj]->mother == nodes[i] ){
              nodes[i]->children[jj]->mother_sgn = ! nodes[i]->children[jj]->mother_sgn;
            }
          }
          nodes[i]->father_sgn = true;
        }//if( nodes[i]->father_sgn )


        //move nots from the mother to the children.
        if( !nodes[i]->mother_sgn ){
          nodes[i]->mother_sgn = true;
          for(int jj=0; jj< nodes[i]->children.size(); ++jj){          	
	            if( nodes[i]->children[jj]->father == nodes[i] ){
	              nodes[i]->children[jj]->father_sgn = ! nodes[i]->children[jj]->father_sgn;
	            }
	            if( nodes[i]->children[jj]->mother == nodes[i] ){
	              nodes[i]->children[jj]->mother_sgn = ! nodes[i]->children[jj]->mother_sgn;
	            }
          	
          }
        }// if( nodes[i]->mother_sgn )

      }// if( nodes[i]->type == bool_node::XOR )
    }

    //Now, this optimization will remove redundant 
    //ands and ors, taking advantage of the fact
    //that (a AND a) == a, and also (a OR a) == a
    if( nodes[i]->father == nodes[i]->mother ){
      switch(nodes[i]->type){
      case bool_node::AND:         
        if( nodes[i]->father_sgn == nodes[i]->mother_sgn ){
          remove(i);                  
        }else{
          ++i;
        }
        break;
      case bool_node::OR:         
        if( nodes[i]->father_sgn == nodes[i]->mother_sgn ){
          remove(i);           
        }else{
          ++i;
        }
        break;
      default:
        ++i;
      }
    }else{
      ++i;
    }    
  }
}


void BooleanDAG::add_passthrough(){
  Assert(is_layered, "The graph must have been layered already");
  map<bool_node*, bool_node*> parent_map;
  for(int i=n_inputs; i < nodes.size() ; ++i){
    bool_node& bn = *nodes[i];
    bn.father = ps_for_parent(bn.father, parent_map, bn.layer);
    bn.mother = ps_for_parent(bn.mother, parent_map, bn.layer);
    Assert(bn.mother == NULL || bn.mother->layer == bn.layer-1, "This is a bug");
    Assert(bn.father == NULL || bn.father->layer == bn.layer-1, "This is a bug");
    bn.children.clear();
  }
  { for(int i=0; i<n_inputs; ++i){
       bool_node& bn = *nodes[i];
       bn.children.clear();
    }
  }

  is_layered = false;
  is_sorted = false;
  //At these point, we have destroyed the layered property, since the passthrough nodes were added 
  //at the end. This also destroyed the sorted property, because those whose parents are
  //ps nodes will come before their parents. However, all nodes have their correct layer labels, 
  //so we can reestablish both the layered and the sorted property by simply sorting by layer.
  //Also, after this stage, all the children are gone, and should not be used.
  sort(nodes.begin(), nodes.end(), comp_layer);
  compute_layer_sizes();
  is_layered = true;
  is_sorted = true;
  has_passthrough = true;
}

bool_node* BooleanDAG::ps_for_parent(bool_node* parent, map<bool_node*, bool_node*>& parent_map, int my_layer){
  if(parent != NULL){
    if(my_layer != parent->layer + 1){
      //In this case, I need a pt node in all the layers
      //between parent->layer and my_layer.
      bool_node* pred = parent;
      if( parent_map.find(parent) != parent_map.end() ){
        //Someone already created a pt for my father, so I use that as my starting pt.
        pred = parent_map[parent];
      }
      for(int j=pred->layer+1; j < my_layer; ++j){
        pred = new_node(pred, true, NULL, true, bool_node::PT);
        pred->layer = j;
      }
      parent_map[parent] = pred;
      return pred;
    }
  }
  return parent;
}


void BooleanDAG::change_father(const string& father, const string& son){
  try{
    Assert( named_nodes.find(father) != named_nodes.end(), "The father name does not exist.", father);  
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
    Assert(named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist. ", mother);
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


bool_node* BooleanDAG::new_node(const string& mother, bool mother_sgn, 
                                const string& father, bool father_sgn, bool_node::Type t, const string& name){
  bool_node* fth;
  bool_node* mth;
  try{
    Assert(father.size()==0 || named_nodes.find(father) != named_nodes.end(), "The father name does not exist.", father);
  }catch(BasicError& be){
    if(father.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<father.substr(6)<<endl;
    }    
    throw be;
  }
  try{
    Assert(mother.size()==0 ||named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist. ", mother);
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

  map<string, bool_node*>::iterator it =  named_nodes.find(name);

  bool_node* tmp;
  if( it == named_nodes.end() ){
    tmp = new_node(mth, mother_sgn, fth, father_sgn, t);    
    tmp->name = name;
    named_nodes[name] = tmp;
  }else{
    tmp = it->second;
    set_node(tmp, mth, mother_sgn, fth, father_sgn, t);
  }
  return tmp;
}

bool_node* BooleanDAG::get_node(const string& name){
  bool_node* fth;
  Assert(name.size()==0 || named_nodes.find(name) != named_nodes.end(), "name does not exist.", name);
  if(name.size()==0){
    fth = NULL;
  }else{
    fth = named_nodes[name];
  }
  return fth;
}
bool_node* BooleanDAG::new_node(const string& mother, bool mother_sgn, 
                                const string& father, bool father_sgn, bool_node::Type t, const string& name, bool_node* thenode){
  bool_node* fth;
  bool_node* mth;
  try{
    Assert(father.size()==0 || named_nodes.find(father) != named_nodes.end(), "The father name does not exist.", father);
  }catch(BasicError& be){
    if(father.substr(0,5) == "INPUT"){
      cerr<<"It looks like you are trying peek beyond the "<<n_inputs<<" bits that you said you would"<<endl;
      cerr<<"You are trying to read bit "<<father.substr(6)<<endl;
    }    
    throw be;
  }
  try{
    Assert(mother.size()==0 ||named_nodes.find(mother) != named_nodes.end(), "The mother name does not exist. ", mother);
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

  map<string, bool_node*>::iterator it =  named_nodes.find(name);

  bool_node* tmp;
  if( it == named_nodes.end() ){
    tmp = new_node(mth, mother_sgn, fth, father_sgn, t, thenode);    
    tmp->name = name;
    named_nodes[name] = tmp;
  }else{
    tmp = it->second;
    set_node(tmp, mth, mother_sgn, fth, father_sgn, t);
  }
  return tmp;
}


void BooleanDAG::create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type){
	//Create interface nodes, either source, dest, or ctrl.
	
	if( named_nodes.find(gen_name) != named_nodes.end() ){
		//cout<< gen_name <<"This interface node already exists"<<endl;
		return;	
	}
	
  if(n < 0){
    bool_node* tmp = newBoolNode(type);
    tmp->ion_pos = counter;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
    ++counter;
  }else{
  	bool_node* tmp = newBoolNode(type);
    dynamic_cast<INTER_node*>(tmp)->set_nbits(n);
    tmp->ion_pos = counter;
    nodes.push_back(tmp);
    tmp->name = gen_name;
    named_nodes[gen_name] = tmp;
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


void BooleanDAG::print(ostream& out){    
  out<<"digraph G{"<<endl;
  for(int i=0; i<nodes.size(); ++i){
    if( nodes[i]->father != NULL){
        out<<" "<<nodes[i]->father->get_name()<<" -> "<<
          nodes[i]->get_name()<<" [label = \" "<<nodes[i]->father_sgn <<"  \"]; "<<endl;
    }
    if( nodes[i]->mother != NULL){
          out<<" "<<nodes[i]->mother->get_name()<<" -> "<<
          nodes[i]->get_name()<<" [label = \" "<<nodes[i]->mother_sgn <<"  \"]; "<<endl;
    }
    for(int j=0; j< nodes[i]->children.size(); ++j){
          out<<" "<<nodes[i]->get_name()<<" -> "<<
          nodes[i]->children[j]->get_name()<<"[ style = dotted]; "<<endl;
    }
  }

  out<<"}"<<endl;
}

bool_node* BooleanDAG::set_node(bool_node* tmp, bool_node* mother, bool mother_sgn, 
                                bool_node* father, bool father_sgn, bool_node::Type t){

  tmp->father = father;
  tmp->father_sgn = father_sgn;
  tmp->mother = mother;
  tmp->mother_sgn = mother_sgn;  

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

  if(father != NULL){
    father->children.push_back(tmp);
  }

  if(mother != NULL && father != mother){
    mother->children.push_back(tmp);
  }

  tmp->type = t;  
  return tmp;
}

  void BooleanDAG::alias(const string& ssource, int sgn, const string& starg){
  	map<string, bool_node*>::iterator it = named_nodes.find(ssource);
  	if( it != named_nodes.end() ){
		new_node(starg, sgn, "", true,  it->second->type, it->second->name);
	}else{
		if( has_alias(starg) ){
		    pair<string, int> p( get_alias(starg) );
	      aliasmap[ssource] = make_pair( p.first, sgn != p.second);
	    }else{
	      aliasmap[ssource] = make_pair(starg, sgn);
	    }
  	}
  }




bool_node* BooleanDAG::new_node(bool_node* mother, bool mother_sgn, 
                                bool_node* father, bool father_sgn, bool_node::Type t){
                                	
  bool_node* tmp = newBoolNode(t);
  set_node(tmp, mother, mother_sgn, father, father_sgn, t);
  nodes.push_back(tmp);
  return tmp;
}


bool_node* BooleanDAG::new_node(bool_node* mother, bool mother_sgn, 
                                bool_node* father, bool father_sgn, bool_node::Type t, bool_node* thenode){
  bool_node* tmp = thenode;
  set_node(tmp, mother, mother_sgn, father, father_sgn, t);
  nodes.push_back(tmp);
  return tmp;
}

void NodeVisitor::process(BooleanDAG& bdag){
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		(*node_it)->accept(*this);
	}
}
