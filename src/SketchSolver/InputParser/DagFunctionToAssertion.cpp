#include "DagFunctionToAssertion.h"



DagFunctionToAssertion::DagFunctionToAssertion(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap):
dag(p_dag), DagOptim(p_dag),functionMap(p_functionMap) {
	
}

DagFunctionToAssertion::~DagFunctionToAssertion()
{
}

void DagFunctionToAssertion::visit( UFUN_node& node ){	
	string& name = node.get_ufname();
	if( functionMap.find(name) != functionMap.end() ){
		cout<<" terminating inlining "<<name<<endl;
				
		t_node tn(NULL);
		
		int szz1 = newnodes.size();
		timerclass timer("         tnbuild  ");
		timer.start();	
		tnbuilder.ivisit = 0;
		tnbuilder.tvisited.clear();
		tnbuilder.tn_build(&node, NULL, &tn, *this);
		
		timer.stop();
		int szz2 = newnodes.size();
		cout<<" added "<<(szz2-szz1)<<" nodes visited "<<tnbuilder.ivisit<<endl;		
		timer.print();				
					
		
		Assert( tn.children.size() > 0, " This function should still be inlined !!! "<<node.get_name() );	
		
		bool_node* cur = tn.childDisjunct(*this); 
		
		NOT_node* nn = new NOT_node();
		nn->mother = cur;
		nn->addToParents();
		newnodes.push_back(nn);
			
		ASSERT_node* asn = new ASSERT_node();
		asn->mother = nn;
		string msg = "function was not inlined enough ";
		msg += node.get_name();
		asn->setMsg(msg);
		asn->addToParents();				
		newnodes.push_back(asn);	
		
		rvalue = new CONST_node(0);
		newnodes.push_back(rvalue);
	}else{
		rvalue = &node;
	}
}


/*
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
*/
