#include "BooleanDAGCreator.h"
#include "CommandLineArgs.h"
#include "BackwardsAnalysis.h"
//extern CommandLineArgs* PARAMS;

BooleanDAGCreator::BooleanDAGCreator(BooleanDAG* p_dag):dag(p_dag),optim(*p_dag)
{
	//
	// optim.alterARRACS();
	new_names = 0;
	new_namesb = 0;
}

BooleanDAGCreator::~BooleanDAGCreator(void)
{
	// cout<<"nnsize = "<<named_nodes.size()<<endl;
}

 void BooleanDAGCreator::finalize(){
	dag->registerOutputs();
     optim.cleanup(*dag);
     optim.combineFunCalls(*dag);
        
	 if(PARAMS->verbosity>7){
		 cout<<"size = "<<dag->size()<<endl;		 
	 }
	 // BackwardsAnalysis ba;
	 // ba.process(*dag);
	 if(PARAMS->verbosity>7){
		 cout<<"after ba size = "<<dag->size()<<endl;		 
	 }	 
 }

bool_node* BooleanDAGCreator::get_node(const string& name){	
  bool_node* fth;
  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
  //Assert(name.size()==0 || named_nodes.find(name) != named_nodes.end(), "name does not exist: "<<name);
  if(name.size()==0){
    fth = NULL;
  }else{
	  if(!named_nodes.get(name.c_str(), name.size(), fth)){
		  cout<<"WARNING, DANGEROUS!!"<<endl;
		  fth = optim.getCnode(-333);
		  dag->setOffset(optim.newNodesSize());
	  }	
  }
  return fth;
}

void BooleanDAGCreator::getMotherFather(const string& mother, 
					 const string& father, /*OUT*/bool_node*& mth, /*OUT*/bool_node*& fth){
  
  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");    
  
  
  if(!named_nodes.get(father.c_str(), father.size(), fth)){
	  Assert(false, "You are using a variable which was never initialized: "<<father);
  }
  if(!named_nodes.get(mother.c_str(), mother.size(), mth)){
	  Assert(false, "You are using a variable which was never initialized: "<< mother);
  }
}
/*
 * Specialized for tuple_r
 */
bool_node* BooleanDAGCreator::new_node(bool_node* mother,
                                       int idx){
   
    bool_node* fth=NULL;
    bool_node* mth=mother;
    bool_node::Type t = bool_node::TUPLE_R;
    bool_node* trv = this->optim.quickcse(mth != NULL? mth->globalId: -1, fth != NULL ? fth->globalId : -1, t);
    if(trv != NULL){ return trv; }
    Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
    Assert(t != bool_node::SRC && t != bool_node::DST && t != bool_node::CTRL, "You can only use new_node to create internal nodes, you can not use it for either SRC, DST, or CTRL");
    bool_node* tmp;
    tmp = newNode(t);
    tmp->father = fth;
    tmp->mother = mth;
    dynamic_cast<TUPLE_R_node*>(tmp)->idx = idx;
    
   
    tmp = optimizeAndAdd(tmp);
   
    return tmp;
}

/* Creates a new node of type 't' with inputs 'mother' and 'father' as its
 * parents in the BooleanDAG. Note: Unary expressions are represented with
 * 'father' as NULL.
 */
bool_node* BooleanDAGCreator::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){	
  bool_node* fth=father;
  bool_node* mth=mother;
   

  bool_node* trv = this->optim.quickcse(mth != NULL? mth->globalId: -1, fth != NULL ? fth->globalId : -1, t);
   
  if(trv != NULL){ return trv; }

  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
  Assert(t != bool_node::SRC && t != bool_node::DST && t != bool_node::CTRL, "You can only use new_node to create internal nodes, you can not use it for either SRC, DST, or CTRL");


  if(t == bool_node::AND || t == bool_node::OR || t == bool_node::XOR || t == bool_node::NOT){
      
	 Assert( mth->getOtype() == OutType::BOOL  && (fth == NULL || fth->getOtype() == OutType::BOOL), "The parents of a boolean operator must be boolean !!!"<<"  mth="<<mth->get_name()<<"  fth="<<(fth!=NULL? fth->get_name():"NULL"));
  }
 

  bool_node* tmp;

//  tmp = dag->new_node(mth, fth, t);    ARMANDO: This is a big performance bug.

  tmp = newNode(t);
  tmp->father = fth;
  tmp->mother = mth;
   
  
  //tmp->name = name;
  if(t == bool_node::ASSERT || typeid(*tmp) == typeid(UFUN_node)){
	  this->dag->assertions.append( dynamic_cast<ASSERT_node*>(tmp) );
  }
 
  tmp = optimizeAndAdd(tmp);
  
  return tmp;
}


/* Do online optimization while the parser is reading the input file to create
 * the nodes for the BooleanDAG.
 */
bool_node* BooleanDAGCreator::optimizeAndAdd(bool_node* node){
	
   
	bool_node* tmp;
    Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	
    if(PARAMS->olevel == 0){
        
		tmp = node;
	}else{
       
		if(PARAMS->olevel == 1){
            
			tmp = optim.computeCSE(node);
		}else{
            tmp = optim.computeOptim(node);
           
        }
	}
   
	if(tmp == node){
		node->addToParents();
		optim.addNode(node);
	}else{
		node->dislodge(); //Node must be dislodged before being deleted, even if it was never explicitly atatched to its parents because the optim may have atatched to parents.
		delete node;		
	}
    
	dag->setOffset(optim.newNodesSize());
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	
    return tmp;
}




bool_node* BooleanDAGCreator::new_node(bool_node* mother, 
                                bool_node* father,  bool_node* thenode){
  bool_node* fth=father;
  bool_node* mth = mother;
  bool_node::Type t = thenode->type;
  
  Assert(t != bool_node::SRC && t != bool_node::DST && t != bool_node::CTRL, "You can only use new_node to create internal nodes, you can not use it for either SRC, DST, or CTRL");

  if(isDllnode(thenode)){
	  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	  this->dag->assertions.append( getDllnode(thenode) );
  }

  thenode->father = fth;
  thenode->mother = mth;  
  
  thenode = optimizeAndAdd(thenode);
  return thenode;
}



void BooleanDAGCreator::alias(const string& ssource,  bool_node* starg){
	bool_node* dst;
	if(named_nodes.condAdd(ssource.c_str(), ssource.size(), starg, dst)){
		Assert(dst->type==bool_node::DST, "Only destination nodes can be assigned after being defined "<<ssource);

		Assert(dst->mother == NULL, "The mother of the DST node had already been defined. DST="<<ssource);
		dst->mother = starg;
		dst->addToParents();		
	}  	
  }



bool_node* BooleanDAGCreator::create_const(int n){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	CONST_node* c = optim.getCnode(n);
	dag->setOffset(optim.newNodesSize());	
	return c;
}

bool_node* BooleanDAGCreator::create_const(double n){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	CONST_node* c = optim.getCnode(n);
	dag->setOffset(optim.newNodesSize());	
	return c;
}


INTER_node* BooleanDAGCreator::create_inputs(int n, OutType* type, const string& gen_name, int arrSz, int tupDepth){
	INTER_node* tmp = dag->create_inputs(n, type, gen_name, arrSz, tupDepth);
	bool_node* f;
	bool flag = named_nodes.condAdd(gen_name.c_str(), gen_name.size(), tmp, f);
	Assert(!flag, "Two inputs with the same name!");	
	optim.dagsizeSet(dag->size());
	return tmp;
}

INTER_node* BooleanDAGCreator::create_controls(int n, const string& gen_name, bool toMinimize, bool angelic, bool spConcretize, int max){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	INTER_node* tmp =  dag->create_controls(n, gen_name, toMinimize, angelic, spConcretize, max);
	if(gen_name == "#PC"){
		dynamic_cast<CTRL_node*>(tmp)->set_Pcond();
	}
	bool_node* f;
	bool flag = named_nodes.condAdd(gen_name.c_str(), gen_name.size(), tmp, f);	
	optim.dagsizeSet(dag->size());
	return tmp;
}

INTER_node* BooleanDAGCreator::create_outputs(int n, bool_node* nodeToOutput, const string& gen_name){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	INTER_node* tmp =  dag->create_outputs(n, nodeToOutput, gen_name);
	bool_node* f;
	bool flag = named_nodes.condAdd(gen_name.c_str(), gen_name.size(), tmp, f);
	Assert(!flag, "Two inputs with the same name!");	
	optim.dagsizeSet(dag->size());
	return tmp;
}

INTER_node* BooleanDAGCreator::create_outputs(int n, const string& gen_name){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	INTER_node* tmp =  dag->create_outputs(n, gen_name);
	bool_node* f;
	bool flag = named_nodes.condAdd(gen_name.c_str(), gen_name.size(), tmp, f);
	Assert(!flag, "Two inputs with the same name!");	
	optim.dagsizeSet(dag->size());
	return tmp;
}
