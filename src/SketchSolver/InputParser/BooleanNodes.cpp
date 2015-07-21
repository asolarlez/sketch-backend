#include "BooleanNodes.h"
#include "BooleanDAG.h"

#include <sstream>
#include <algorithm>
#include <map>

extern const int UNINITIALIZED = 0;

int bool_node::NEXT_GLOBAL_ID = 0;
int UFUN_node::CALLSITES = 0;

OutType* OutType::BOTTOM = new Bottom();
OutType* OutType::BOOL = new Bool();
OutType* OutType::INT = new Int();
OutType* OutType::FLOAT = new Float();
OutType* OutType::BOOL_ARR = new Arr(BOOL);
OutType* OutType::INT_ARR = new Arr(INT);
OutType* OutType::FLOAT_ARR = new Arr(FLOAT);
vector<OutType*> OutType::store;
map<string, OutType*> OutType::tupleMap;
int i;

OutType* OutType::getTuple(const string& name){
    if(tupleMap.count(name)>0 ){
       
		return tupleMap[name];
    }
    
    Tuple* t = new Tuple();
    t->name = name;
    t->actSize = 0;
    tupleMap[name] = t;
    return t;
}

OutType* OutType::makeTuple(const string& name, vector<OutType*>& elems, int actFields){
    Tuple* t;
	if(tupleMap.count(name)>0 ){
		t =  dynamic_cast<Tuple*>(tupleMap[name]);
    }else{
        t = new Tuple();
        t->name = name;
        tupleMap[name] = t;
    }
    t->entries = elems;
    t->actSize = actFields == -1 ? elems.size() : actFields;
    
    return t;
    
}

OutType* OutType::makeTuple(vector<OutType*>& elems){
   for(map<string,OutType*>::iterator itr = tupleMap.begin(); itr != tupleMap.end(); ++itr){
        bool matched = true;
        vector<OutType*> entries = dynamic_cast<Tuple*>(itr->second)->entries;
        if(entries.size()!= elems.size()) continue;
        for(int i=0;i< entries.size();i++){
            if(joinOtype(entries[i], elems[i]) != entries[i]){
                matched = false;
                break;
            }
        }
        if(matched){
            return itr->second;
        }
    }
    
    cout<<"did not match anything"<<endl;
    

}


  /**
                          TUPLE_ARR
					     /	       \
					   /            \
					 /  FLOAT_ARR    \   
			       	 |   |    \       \   
				    INT_ARR    FLOAT  TUPLE
		            |     \   /       /
		     	BOOL_ARR   INT-------/    
			        \     /            
	   		   	      BOOL           
		      

  */
  OutType* OutType::joinOtype(OutType* t1, OutType* t2) {
    if(t1 == BOTTOM){ return t2; }
  	if(t2 == BOTTOM){ return t1; }
      
	if(t1->isTuple){
		if(t2->isArr){
			if(t2 == INT_ARR){
				return ((Tuple*)t1)->arr;
			}else{
				return t2;
			}
		}else{
			return t1;
		}
	}
    if(t2->isTuple){
        if(t1->isArr){
            if(t1==INT_ARR){
                return ((Tuple*)t2)->arr;
            }else{
                return t1;
            }
        }else{
            return t2;
        }
    }
	if(t1->isArr){
		if(((Arr*)t1)->atype->isTuple){
			return t1;
		}
	}
    if(t2->isArr){
        if(((Arr*)t2)->atype->isTuple){
            return t2;
        }
    }
  	if( t2 == t1 ){
        return t1; 
  	}else{ 		
		if(t1==FLOAT_ARR || t2 == FLOAT_ARR){
			return FLOAT_ARR;
		}
		if(t1==INT_ARR || t2==INT_ARR){
			if(t1==FLOAT || t2==FLOAT){
				return FLOAT_ARR;
			}else{
				return INT_ARR;
			}
		}		
		if(t1==FLOAT || t2 == FLOAT){
			if(t1==BOOL_ARR || t2==BOOL_ARR){
				return  FLOAT_ARR;
			}else{
				return FLOAT;
			}
		}
		if(t1==INT || t2==INT){			
			if(t1==BOOL_ARR || t2==BOOL_ARR){
				return INT_ARR;
			}else{
				return INT;
			}			
		}else{
			if(t1==BOOL_ARR || t2==BOOL_ARR){				
				return BOOL_ARR;				
			}
		}		
  		return BOOL; 
  	}
  }


#ifdef SCHECKMEM
set<bool_node*> bool_node::allocated;
#endif 


void bool_node::replace_child(bool_node* ori, bool_node* replacement){
	child_iter it = children.find(ori);
	if(it != children.end() ){
		children.erase(it);
		children.insert(replacement);
	}

}

bool_node::bool_node(Type t):globalId(NEXT_GLOBAL_ID++), mother(NULL), layer(0), father(NULL), flag(0), id(-1), otype(OutType::BOTTOM), type(t), depth(-1)
  {
      
	  layer = 0;
#ifdef SCHECKMEM
  allocated.insert(this);
#endif
  }  
  bool_node::bool_node(const bool_node& bn, bool copyChildren):globalId(NEXT_GLOBAL_ID++), mother(bn.mother), layer(bn.layer), 
  								 father(bn.father), 
  								 flag(bn.flag), id(bn.id),
								 otype(bn.otype), type(bn.type), depth(bn.depth)
  {
      
      if(copyChildren){ children = bn.children; }
#ifdef SCHECKMEM
  allocated.insert(this);
#endif
  }

  bool_node::~bool_node(){
#ifdef SCHECKMEM
	allocated.erase(this);
#endif
  }



void arith_node::replace_child_inParents(bool_node* ori, bool_node* replacement){
	bool_node::replace_child_inParents(ori, replacement);
	for(int i=0; i<multi_mother.size(); ++i){
		multi_mother[i]->replace_child(ori, replacement);
	}
}


void bool_node::replace_child_inParents(bool_node* ori, bool_node* replacement){
	if(mother != NULL){
		mother->replace_child(ori, replacement);
	}
	if(father != NULL){
		father->replace_child(ori, replacement);
	}
}


void bool_node::neighbor_replace(bool_node* replacement){
	bool_node* onode = this;
	onode->dislodge();
	child_iter end = onode->children.end();
	for(child_iter it = onode->children.begin(); 
										it !=end; ++it){
		
		(*it)->replace_parent(onode, replacement);
	}

}



void arith_node::addToParents(bool_node* only_thisone){
  for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it == only_thisone){
	  	only_thisone->children.insert(this);
		break;
  	}
  }
}



void arith_node::addToParents(){
  
    bool_node::addToParents();
    for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it != NULL){
		bool_node* tmp = (*it);
	  	tmp->children.insert(this);
  	}
  }
}


void bool_node::addToParents(){
    
    
  if(father != NULL){
      father->children.insert(this);
    }
  if(mother != NULL && father != mother){
      mother->children.insert(this);
  }
    
}




void arith_node::switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements){	
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it != NULL){
			if( replacements.count(*it) > 0){
				(*it) = replacements[*it];
			}
	  	}
  	}
  	bool_node::switchInputs(bdag, replacements);
}


void bool_node::switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements){
	if(father != NULL){
		if(replacements.count(father) > 0){
			father = replacements[father];
		}		
  	}
	if(mother != NULL){
		if(replacements.count(mother) > 0){
			mother = replacements[mother];
		}
	}
	addToParents();
}



void bool_node::redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert){
	if(father != NULL){
		Assert( father->id != -22, "This node should not exist anymore");
		if( oribdag.checkNodePosition(father) ){
			//If the father was from the original bdag, then we switch to the father in the new bdag.
	    	father = (bool_node*) bdag[father->id];
			if(setChildrn){
				/*
				child_iter it = father->children.find(childToInsert);
				if(it != father->children.end() ){
					father->children.erase(it);
				}
				*/
				father->children.insert(this); 
			}
		}
  	}
	if(mother != NULL){
		Assert( mother->id != -22, "This node should not exist anymore");
		if( oribdag.checkNodePosition(mother)){
			mother = (bool_node*) bdag[mother->id];
			if(setChildrn){ 
				/*
				child_iter it = mother->children.find(childToInsert);
				if(it != mother->children.end() ){
					mother->children.erase(it);
				}
				*/
				mother->children.insert(this); 
			}
		}
	}
}


void arith_node::redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert){
  bool_node::redirectParentPointers(oribdag, bdag, setChildrn, childToInsert);
  for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
  	if(*it != NULL){
  		if( oribdag.checkNodePosition(*it) ){
	  		(*it) = (bool_node*) bdag[(*it)->id];
			if(setChildrn){ 
				/*
				child_iter ttt = (*it)->children.find(childToInsert);
				if(ttt != (*it)->children.end() ){
					(*it)->children.erase(ttt);
				}
				*/
				(*it)->children.insert(this);
			}
  		}
  	}
  }
}



void bool_node::redirectPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, childset& tchild){
	redirectParentPointers(oribdag, bdag, false, NULL);
	
	for(child_iter child = tchild.begin(); 
		child != tchild.end(); 
		++child){
		bool_node* bn = *child;
		if( oribdag.checkNodePosition(bn) ){
			children.insert((bool_node*) bdag[ bn->id ] );
		}else{
			children.insert(bn);
		}
	}
}


string bool_node::get_name() const {
    stringstream str;
    {      
      str<<"name_"<<abs(id)<<"__"<<get_tname();
      
    }    
    //str<<"_"<<this;
    //str<<":"<<id;
    Assert( id != -22, "This is a corpse. It's living gargabe "<<str.str()<<" id ="<<id );
    return str.str();
  }



OutType* bool_node::getOtype() const{
	return OutType::BOOL;
}

OutType* arith_node::getOtype() const{
    
	return OutType::INT;
}

void bool_node::set_layer(bool isRecursive){
	if(!isRecursive){ layer = 0;}
  if(mother != NULL){
	  if(isRecursive && mother->layer < 0){ mother->set_layer(isRecursive); }
      layer = mother->layer + 1;
  }
  if(father != NULL){
	  if(isRecursive && father->layer < 0){
		father->set_layer(isRecursive); 
	  }
	if(father->layer >= layer){
		layer = father->layer + 1;
	}
  }
}


void arith_node::set_layer(bool isRecursive){
	bool_node::set_layer(isRecursive);

	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		if(*it != NULL){
			if(isRecursive && (*it)->layer < 0){
			(*it)->set_layer(isRecursive); 
			}
			if((*it)->layer >= layer){
			layer = (*it)->layer + 1;
			}
		}
	}	
}


int bool_node::do_dfs(int idx){
	Assert( id != -22, "This node should be dead (dfs) "<<this->get_name());
  if( flag != 0)
    return idx;
  flag = 1;    
  for(child_iter child = children.begin(); child != children.end(); ++child){  	
    idx = (*child)->do_dfs(idx);
	Assert(idx >= 0, "This shouldn't happen");
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
  return idx+1;
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
  return idx+1;
}



void bool_node::replace_parent(const bool_node * oldpar, bool_node* newpar){
	Assert( oldpar != NULL, "oldpar can't be null");
	Assert( newpar != NULL, "oldpar can't be null");
	if(father == oldpar){
  		father = newpar;
  		//oldpar->remove_child(this);
  		//we assume the old parent is going to be distroyed, so we don't
  		//bother modifying it.
		newpar->children.insert( this );
  	}
  	if(mother == oldpar){
  		mother = newpar;
  		//oldpar->remove_child(this);
  		//we assume the old parent is going to be distroyed, so we don't
  		//bother modifying it.
  		newpar->children.insert( this );
  	}
}


void arith_node::replace_parent(const bool_node * oldpar, bool_node* newpar){
	bool_node::replace_parent(oldpar, newpar);
	replace<vector<bool_node*>::iterator, bool_node*>(multi_mother.begin(), multi_mother.end(), ( bool_node*)oldpar, newpar);
	newpar->children.insert( this );	
}




void arith_node::printSubDAG(ostream& out, set<const bool_node* >& s)const{	
	int i=0;
	s.insert(this);
	for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
	  	if(*it != NULL && s.count(*it) == 0){
	  		(*it)->printSubDAG(out, s);  		
	  	}
	}
	bool_node::printSubDAG(out, s);
}


void bool_node::printSubDAG(ostream& out, set<const bool_node* >& s)const{
	s.insert(this);
	if( father != NULL && s.count(father) == 0 ){
			father->printSubDAG(out, s);
	}	
	if(mother != NULL && s.count(mother) == 0){
			mother->printSubDAG(out, s);
	}
	out<<lprint()<<endl;	
}


void bool_node::lprintSubDAG(ostream& out, set<const bool_node* >& s, int bnd)const{
	if (bnd <=0){ return; }
	s.insert(this);
	if( father != NULL && s.count(father) == 0 ){
			father->lprintSubDAG(out, s, bnd-1);
	}	
	if(mother != NULL && s.count(mother) == 0){
			mother->lprintSubDAG(out, s, bnd-1);
	}
	out<<lprint()<<endl;	
}




void arith_node::lprintSubDAG(ostream& out, set<const bool_node* >& s, int bnd)const{	
	if(bnd <=0){ return; }
	int i=0;
	s.insert(this);
	for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
	  	if(*it != NULL && s.count(*it) == 0){
	  		(*it)->lprintSubDAG(out, s, bnd-1);  		
	  	}
	}
	bool_node::lprintSubDAG(out, s, bnd);
}



void bool_node::outDagEntry(ostream& out) const{
	if( father != NULL){
        out<<" "<<father->get_name()<<" -> "<<get_name()<<"[label=\"f\"] ; "<<endl;
    }
    if( mother != NULL){
          out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[label=\"m\"] ; "<<endl;
    }
    if(father == NULL && mother == NULL){
    	   out<<"// orphan node: "<<get_name()<<" ; "<<endl ;
    }
	for(child_citer child = children.begin(); child != children.end(); ++child){  	
    //	Dout( out<<"// "<<get_name()<<" -> "<<children[i]->get_name()<<" ; "<<endl );
    }
}


void arith_node::outDagEntry(ostream& out) const{
	bool_node::outDagEntry(out);
	int i=0;
	for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
	  	if(*it != NULL){
	  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<" ; "<<endl;	  		
	  	}
	}
}

/*
void arith_node::removeFromParents(bool_node* bn){
	bool_node::removeFromParents(bn);
	bool_node* tmp = NULL;
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it != NULL && *it != tmp){
	  		(*it)->remove_child(bn);
			tmp = *it;
	  	}
	}
}
*/

/*
void bool_node::removeFromParents(bool_node* bn){
  if(father != NULL){
  	 father->remove_child(bn);
  }
  if(mother != NULL){
  	mother->remove_child(bn);
  }
}
*/

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
	bool_node* tmp = NULL;
	for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
	  	if(*it != NULL && *it != tmp){
	  		(*it)->remove_child(this);	
			tmp = *it;
	  	}
	}
}
