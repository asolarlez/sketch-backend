#include "DagFunctionToAssertion.h"



DagFunctionToAssertion::DagFunctionToAssertion(BooleanDAG& p_dag, const map<string, BooleanDAG *> &p_functionMap, FloatManager& fm):
dag(p_dag), DagOptim(p_dag, fm),functionMap(p_functionMap) {
	
}

DagFunctionToAssertion::~DagFunctionToAssertion()
{
}

void DagFunctionToAssertion::visit( UFUN_node& node ){	
	const string& name = node.get_ufun_name();
	bool isUninterp = (functionMap.find(name) == functionMap.end());
	if(!node.ignoreAsserts && !isUninterp ){
		Dout(cout<<" terminating inlining "<<name<<endl);
		
		bool_node* cur = node.mother();
		
		NOT_node* nn = NOT_node::create();
		nn->mother() = cur;
		nn->addToParents();
		addNode(nn);

			
		ASSERT_node* asn = ASSERT_node::create();
		asn->mother() = nn;
		string msg = "function was not inlined enough ";
		msg += node.get_ufun_name();
		asn->setMsg(msg);
		asn->addToParents();
		addNode(asn);
		node.add(getDllnode(asn));
		node.remove();
		rvalue = this->getCnode(0);		
	}else{
		if(isUninterp){
			rvalue = &node;
		}else{
			rvalue = getCnode(0);
		}
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
