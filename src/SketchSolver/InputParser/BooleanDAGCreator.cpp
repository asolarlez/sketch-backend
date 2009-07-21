#include "BooleanDAGCreator.h"
#include "CommandLineArgs.h"

extern CommandLineArgs* PARAMS;

BooleanDAGCreator::BooleanDAGCreator(BooleanDAG* p_dag):dag(p_dag),optim(*p_dag)
{
	new_names = 0;
	new_namesb = 0;
}

BooleanDAGCreator::~BooleanDAGCreator(void)
{
	cout<<"nnsize = "<<named_nodes.size()<<endl;
}


bool_node* BooleanDAGCreator::get_node(const string& name){
  bool_node* fth;
  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
  //Assert(name.size()==0 || named_nodes.find(name) != named_nodes.end(), "name does not exist: "<<name);
  if(name.size()==0){
    fth = NULL;
  }else{
	if(named_nodes.find(name) != named_nodes.end()){
		fth = named_nodes[name];	
	}else{
		fth = optim.getCnode(-333);
		dag->setOffset(optim.newNodesSize());
	}
  }
  return fth;
}

void BooleanDAGCreator::getMotherFather(const string& mother, 
					 const string& father, /*OUT*/bool_node*& mth, /*OUT*/bool_node*& fth){
  Assert(father.size()==0 || named_nodes.find(father) != named_nodes.end(), "You are using a variable which was never initialized: "<<father);
  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");    
  Assert(mother.size()==0 ||named_nodes.find(mother) != named_nodes.end(), "You are using a variable which was never initialized: "<< mother);
  
  fth = get_node(father);

  mth = get_node(mother);  
}


bool_node* BooleanDAGCreator::new_node(bool_node* mother, 
                                bool_node* father, bool_node::Type t){	
  bool_node* fth=father;
  bool_node* mth=mother;
  Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
  Assert(t != bool_node::SRC && t != bool_node::DST && t != bool_node::CTRL, "You can only use new_node to create internal nodes, you can not use it for either SRC, DST, or CTRL");


  if(t == bool_node::AND || t == bool_node::OR || t == bool_node::XOR || t == bool_node::NOT){
	  Assert( mth->getOtype() == bool_node::BOOL && (fth == NULL || fth->getOtype() == bool_node::BOOL), "The parents of a boolean operator must be boolean !!!"<<"  mth="<<mth->get_name()<<"  fth="<<(fth!=NULL? fth->get_name():"NULL"));
  }



  bool_node* tmp;

  tmp = dag->new_node(mth, fth, t);    

  tmp = newBoolNode(t);
  tmp->father = fth;
  tmp->mother = mth;    
  //tmp->name = name;
  if(t == bool_node::ASSERT || typeid(*tmp) == typeid(UFUN_node)){
	  this->dag->assertions.append( dynamic_cast<ASSERT_node*>(tmp) );
  }

  tmp = optimizeAndAdd(tmp);
  
  return tmp;
}



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
  string s;    
  swap(thenode->name, s);
  thenode = optimizeAndAdd(thenode);
  if(s.size()>0){
	named_nodes[s] = thenode;
  }
  return thenode;
}



void BooleanDAGCreator::alias(const string& ssource,  bool_node* starg){
  	map<string, bool_node*>::iterator it = named_nodes.find(ssource);		
  	if( it != named_nodes.end() ){
		Assert(it->second->type==bool_node::DST, "Only destination nodes can be assigned after being defined "<<ssource);

		Assert(it->second->mother == NULL, "The mother of the DST node had already been defined. DST="<<ssource);
		it->second->mother = starg;
		it->second->addToParents();		
	}else{
		named_nodes[ssource] = starg;		
  	}
  }



bool_node* BooleanDAGCreator::create_const(int n){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	CONST_node* c = optim.getCnode(n);
	dag->setOffset(optim.newNodesSize());	
	return c;
}



bool_node* BooleanDAGCreator::create_inputs(int n, const string& gen_name){
	bool_node* tmp = dag->create_inputs(n, gen_name);
	named_nodes[tmp->name]= tmp;
	optim.dagsizeSet(dag->size());
	return tmp;
}

bool_node* BooleanDAGCreator::create_controls(int n, const string& gen_name){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	bool_node* tmp =  dag->create_controls(n, gen_name);
	named_nodes[tmp->name]= tmp;
	optim.dagsizeSet(dag->size());
	return tmp;
}

bool_node* BooleanDAGCreator::create_outputs(int n, bool_node* nodeToOutput, const string& gen_name){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	bool_node* tmp =  dag->create_outputs(n, nodeToOutput, gen_name);
	named_nodes[tmp->name]= tmp;
	optim.dagsizeSet(dag->size());
	return tmp;
}

bool_node* BooleanDAGCreator::create_outputs(int n, const string& gen_name){
	Assert(this->dag->assertions.tail == NULL || this->dag->assertions.tail->next == NULL, "this is bad");
	bool_node* tmp =  dag->create_outputs(n, gen_name);
	named_nodes[tmp->name]= tmp;
	optim.dagsizeSet(dag->size());
	return tmp;
}
