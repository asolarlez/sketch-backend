#include "DagFunctionInliner.h"
#include "timerclass.h"


DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap):
dag(p_dag),functionMap(p_functionMap) {
	somethingChanged = false;
}

DagFunctionInliner::~DagFunctionInliner()
{
}



DagFunctionToAssertion::DagFunctionToAssertion(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap):
dag(p_dag),functionMap(p_functionMap) {
	
}

DagFunctionToAssertion::~DagFunctionToAssertion()
{
}


void DagFunctionInliner::visit( UFUN_node& node ){	
	string& name = node.get_ufname();
	if( functionMap.find(name) != functionMap.end() ){
		cout<<" inlining "<<name<<endl;
		BooleanDAG* fun = functionMap[name]->clone();

		vector<bool_node*> inputs  = fun->getNodesByType(bool_node::SRC);
		
		Assert( inputs.size() == node.multi_mother.size() , "Argument missmatch: More formal than actual parameters");
		
		for(int i=0; i<inputs.size(); ++i){			
			bool_node* formal = inputs[i];			
			bool_node* actual = node.multi_mother[i];
			//cout<<" replacing formal : " << formal->get_name() << " with actual "<<actual->get_name()<<endl;
			Assert( (*fun)[formal->id] == formal, "ID is incorrect");
			fun->replace(formal->id, actual);
		}
		

		vector<bool_node*> controls  = fun->getNodesByType(bool_node::CTRL);
		
		for(int i=0; i<controls.size(); ++i){			
			bool_node* formal = controls[i];
			bool_node* actual = dag.get_node( formal->name );		
			Assert( actual != NULL, "Can't find node "<<formal->name<<endl);
			Assert( (*fun)[formal->id] == formal, "ID is incorrect");	
			fun->replace(formal->id, actual);
		}
		
		
		vector<bool_node*>& outputs  = fun->getNodesByType(bool_node::DST);
		
		Assert( outputs.size() == 1, "The outputs are of the wrong size "<< outputs.size()<<"  "<< name);
				
		rvalue = outputs[0]->mother;
		
		
		bool_node* tn = NULL;
				
		
		int hasBuiltTN = false;
		
		
		for(int i=0; i<fun->size(); ++i){
			bool_node* n = (*fun)[i];
			if( n != NULL &&  n->type != bool_node::DST ){			
				if(n->type != bool_node::ASSERT){
					newnodes.push_back(n);
				}else{
					if(!hasBuiltTN){	
						hasBuiltTN = true;					
						int szz1 = tnbuilder.store.size();
						timerclass timer("         tnbuild  ");
						timer.start();		
						tn = tnbuilder.get_exe_cond(&node, newnodes);						
						timer.stop();
						int szz2 = tnbuilder.store.size();
						cout<<" added "<<(szz2-szz1)<<" nodes"<<endl;		
						timer.print();	
					}
					
					bool_node* cur = n->mother;
					if(tn != NULL){						
						AND_node* anode = new AND_node();
						anode->mother = cur;
						anode->father = tn;
						anode->addToParents();
						newnodes.push_back(anode);
						cur = anode;				
					}
					ASSERT_node* asn = new ASSERT_node();
					asn->mother = cur;
					asn->addToParents();
					newnodes.push_back(asn);
					
					n->dislodge();
					n->id = -22;
					delete n;
				}
			}else{
				if( n!= NULL){
					n->dislodge();
					n->id = -22;
					delete n;
				}
			}
		}		
		
		somethingChanged = true;
	}else{
		rvalue = &node;
	}
}

void DagFunctionInliner::immInline(BooleanDAG& dag){
	
	int k=0;
	// Dout( dag.print(cout) );	
	newnodes.clear();
	// dag.print(cout) ;	
	for(int i=0; i<dag.size(); ++i ){
		// Get the code for this node. 
		dag[i]->accept(*this);
		bool_node* node = rvalue;
		if( dag[i] != node ){
			dag.replace(i, node);
		}
	}
	
	dag.addNewNodes(newnodes);
	//( dag.print(cout) );	
	dag.addNewNodes( tnbuilder.store );
	//( dag.print(cout) );
	tnbuilder.reset();
	newnodes.clear();	
	dag.removeNullNodes();
	
	{	
		( cout<<" after inline, before optim dag.size()=="<<dag.size()<<endl);
		DagOptim optim(dag);
		optim.process(dag);
	}
	Dout( cout<<" AFTER PROCESS "<<endl );
	Dout(cout<<" end ElimFun "<<endl);
}



void DagFunctionInliner::process(BooleanDAG& dag){
	cout<<" funmap has size " << functionMap.size() << endl;
	somethingChanged = true;
	{
		DagOptim optim(dag);
		optim.process(dag);
	}
	int inlin = 0;
	while(somethingChanged && dag.size() < 25000 && inlin < 5){
		somethingChanged = false;
		cout<<inlin<<": inside the loop dag.size()=="<<dag.size()<<endl;
		immInline(dag);	
		//if(inlin==0){( dag.print(cout) );}
		++inlin;
	}
	
	( cout<<" after all inlining dag.size()=="<<dag.size()<<endl);
	
	
	// dag.print(cout);
	{
		DagFunctionToAssertion makeAssert(dag, functionMap);
		makeAssert.process(dag);
	}
	
	cout<<" After everything: dag.size()=="<<dag.size()<<endl;	
}




void DagFunctionToAssertion::process(BooleanDAG& dag){
	
	int k=0;
	// Dout( dag.print(cout) );		
	for(int i=0; i<dag.size(); ++i ){
		// Get the code for this node. 
		dag[i]->accept(*this);
		bool_node* node = rvalue;
		if( dag[i] != node ){
			Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
			dag.replace(i, node);
		}
	}
	dag.addNewNodes(newnodes);
	dag.addNewNodes( tnbuilder.store );
	tnbuilder.reset();
	newnodes.clear();
	dag.removeNullNodes();
	Dout( cout<<" AFTER PROCESS "<<endl );
	//Dout( dag.print(cout) );	
	Dout(cout<<" end ElimFun "<<endl);
}



void DagFunctionToAssertion::visit( UFUN_node& node ){	
	string& name = node.get_ufname();
	if( functionMap.find(name) != functionMap.end() ){
		cout<<" terminating inlining "<<name<<endl;
				
		t_node tn(NULL);
		
		int szz1 = tnbuilder.store.size();
		timerclass timer("         tnbuild  ");
		timer.start();	
		tnbuilder.ivisit = 0;
		tnbuilder.tvisited.clear();
		tnbuilder.tn_build(&node, NULL, &tn);
		
		timer.stop();
		int szz2 = tnbuilder.store.size();
		cout<<" added "<<(szz2-szz1)<<" nodes visited "<<tnbuilder.ivisit<<endl;		
		timer.print();				
					
		
		Assert( tn.children.size() > 0, " This function should still be inlined !!! "<<node.get_name() );	
		
		bool_node* cur = tn.childDisjunct(newnodes); 
		
		NOT_node* nn = new NOT_node();
		nn->mother = cur;
		nn->addToParents();
		newnodes.push_back(nn);
			
		ASSERT_node* asn = new ASSERT_node();
		asn->mother = nn;
		asn->addToParents();
		
		string msg = name;
		msg += ": didn't inline enough";
		asn->setMsg(msg);
		
		newnodes.push_back(asn);	
		
		
		rvalue = new CONST_node(0);
		newnodes.push_back(rvalue);
	}else{
		rvalue = &node;
	}
}
