#include "BooleanNodes.h"
#include "BooleanDAG.h"

#include <sstream>
#include <algorithm>
#include <map>

extern const int UNINITIALIZED = 0;

int bool_node::NEXT_GLOBAL_ID = 0;
int UFUN_node::CALLSITES = 0;
int UFUN_node::FGID= 0;


OutType* OutType::BOTTOM = new Bottom();
OutType* OutType::BOOL = new Bool();
OutType* OutType::INT = new Int();
OutType* OutType::FLOAT = new Float();
OutType* OutType::BOOL_ARR = new Arr(BOOL);
OutType* OutType::INT_ARR = new Arr(INT);
OutType* OutType::FLOAT_ARR = new Arr(FLOAT);
vector<OutType*> OutType::store;
map<string, OutType*> OutType::tupleMap;


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
    return NULL;
	Assert(false,"Control shouldn't reach here");
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

bool_node::bool_node(Type t, size_t nparents):
	globalId(NEXT_GLOBAL_ID++), 
	numparents(nparents),
	layer(0), flag(0), id(-1), otype(OutType::BOTTOM), type(t), depth(-1)
  {
	parents = new bool_node*[nparents];
	memset(parents, 0, sizeof(bool_node*)*nparents);
	  layer = 0;
#ifdef SCHECKMEM
  allocated.insert(this);
#endif
  }  
bool_node::bool_node(const bool_node& bn, bool copyChildren) :globalId(NEXT_GLOBAL_ID++), layer(bn.layer),
numparents(bn.numparents),
flag(bn.flag), id(bn.id),
otype(bn.otype), type(bn.type), depth(bn.depth)
{
	parents = new bool_node*[numparents];
	for (int i = 0; i < numparents; ++i) {
		parents[i] = bn.parents[i];
	}
      if(copyChildren){ children = bn.children; }
#ifdef SCHECKMEM
  allocated.insert(this);
#endif
}

  bool_node::~bool_node(){
	  delete[] parents;
#ifdef SCHECKMEM
	allocated.erase(this);
#endif
  }



void bool_node::replace_child_inParents(bool_node* ori, bool_node* replacement){
	auto f = [=](bool_node* parent){
		parent->replace_child(ori, replacement);
	};
	forallparents(f);
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



void bool_node::addToParent(bool_node* only_thisone){
  for(auto it = p_begin(); it != p_end(); ++it){
  	if(*it == only_thisone){
	  	only_thisone->children.insert(this);
		break;
  	}
  }
}



void bool_node::addToParents(){
    
	for (auto it = p_begin(); it != p_end(); ++it) {
		if (*it != NULL) {
			bool_node* tmp = (*it);
			tmp->children.insert(this);
		}
	}
}



void bool_node::switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements){

	for (auto it = p_begin(); it != p_end(); ++it) {
		if (*it != NULL) {
			if (replacements.count(*it) > 0) {
				(*it) = replacements[*it];
			}
		}
	}

	addToParents();
}



void bool_node::redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert){
  for(auto it = p_begin(); it != p_end(); ++it){
  	if(*it != NULL){
  		if( oribdag.checkNodePosition(*it) ){
	  		(*it) = (bool_node*) bdag[(*it)->id];
			if(setChildrn){				
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


void bool_node::set_layer(bool isRecursive){
	if(!isRecursive){ layer = 0;}
	bool frst = true;
	for (auto it = p_begin(); it != p_end(); ++it) {
		if (*it != NULL) {
			if (isRecursive && (*it)->layer < 0) {
				(*it)->set_layer(isRecursive);
			}
			if (frst || (*it)->layer >= layer) {
				layer = (*it)->layer + 1;
			}
			frst = false;
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
  auto f = [&idx](bool_node* parent) {
	  if (parent != NULL) {
		  idx = parent->back_dfs(idx);
	  }
  };
  forallparents(f);
  id = idx;
  return idx+1;
}




void bool_node::replace_parent(const bool_node * oldpar, bool_node* newpar){
	Assert( oldpar != NULL, "oldpar can't be null");
	Assert( newpar != NULL, "oldpar can't be null");

	replace<parent_iter, bool_node*>(p_begin(), p_end(), (bool_node*)oldpar, newpar);
	newpar->children.insert(this);
}





void bool_node::printSubDAG(ostream& out, set<const bool_node* >& s)const{
	int i=0;
	s.insert(this);
	for(auto it = p_begin(); it != p_end(); ++it, ++i){
	  	if(*it != NULL && s.count(*it) == 0){
	  		(*it)->printSubDAG(out, s);  		
	  	}
	}
	out << lprint() << endl;
}



void bool_node::lprintSubDAG(ostream& out, set<const bool_node* >& s, int bnd)const{
	if (bnd <=0){ return; }
	s.insert(this);

	for (auto it = p_begin(); it != p_end(); ++it) {
		if (*it != NULL) {
			(*it)->lprintSubDAG(out, s, bnd - 1);
		}
	}

	out<<lprint()<<endl;	
}




void bool_node::outDagEntry(ostream& out) const{
	for (parent_iter it = p_begin(); it != p_end(); ++it) {
		if (*it != NULL) {
			out << " " << (*it)->get_name() << " -> " << get_name() << " ; " << endl;
		}
	}
}



void bool_node::dislodge(){
	bool_node* tmp = NULL;
	auto f = [&](bool_node* parent) {
		if (parent != NULL && parent != tmp) {
			parent->remove_child(this);
			tmp = parent;
		}
	};
	forallparents(f);
}

