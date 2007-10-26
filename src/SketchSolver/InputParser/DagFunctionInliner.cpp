#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"


DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap):
dag(p_dag), 
DagOptim(p_dag), 
functionMap(p_functionMap),
replTime(" replacement "),
replTime2(" replacement internal"),
tnbuildTime(" tnbuilding "),
optimTime(" optim "),
ufunAll(" ufun all"),
optAll(" opt all ")

{
	somethingChanged = false;
}

DagFunctionInliner::~DagFunctionInliner()
{
}





void DagFunctionInliner::visit( UFUN_node& node ){	
	ufunAll.restart();
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
			replTime.restart();
			fun->replace(formal->id, actual, replTime2);
			replTime.stop();
		}
		

		vector<bool_node*> controls  = fun->getNodesByType(bool_node::CTRL);
		
		for(int i=0; i<controls.size(); ++i){			
			bool_node* formal = controls[i];
			bool_node* actual = dag.unchecked_get_node( formal->name );		
			if(actual != NULL){
				Assert( (*fun)[formal->id] == formal, "ID is incorrect");	
				replTime.restart();
				fun->replace(formal->id, actual, replTime2);
				replTime.stop();
			}
		}
		
		
		vector<bool_node*>& outputs  = fun->getNodesByType(bool_node::DST);
		
		Assert( outputs.size() == 1, "The outputs are of the wrong size "<< outputs.size()<<"  "<< name);
				
		//rvalue = outputs[0]->mother;
		
		
		bool_node* tn = NULL;
				
		
		int hasBuiltTN = false;
		
		
		bool_node* output = NULL;

		for(int i=0; i<fun->size(); ++i){
			bool_node* n = (*fun)[i];
			if( n != NULL &&  n->type != bool_node::DST ){			
				if(n->type != bool_node::ASSERT){
					if(typeid(*n) != typeid(UFUN_node)){
						optimTime.restart();
						bool_node* nnode = this->computeOptim(n);
						optimTime.stop();

						if(nnode == n){
							this->addNode(n);
						}else{
							replTime.restart();
							this->dag.neighbor_replace(n, nnode, replTime2);
							replTime.stop();
						}
					}else{						
						bool_node* nnode = cse.computeCSE(n);
						if(nnode == n){
							cout<<"    added fun "<<n->get_name()<<endl;
							this->addNode(n);
						}else{
							replTime.restart();
							this->dag.neighbor_replace(n, nnode, replTime2);
							replTime.stop();
						}
					}
				}else{
					if(!hasBuiltTN){	
						hasBuiltTN = true;					
						int szz1 = newnodes.size();

						tnbuildTime.start();		
						tn = tnbuilder.get_exe_cond(&node, *this);
						tnbuildTime.stop();
						int szz2 = newnodes.size();
							
					}
					
					bool_node* cur = n->mother;
					if(tn != NULL){		// !!! This should be OR NOT.
						bool_node* nnode = new NOT_node();
						nnode->mother = tn;
						{
							optimTime.restart();
							bool_node* nnodep = this->computeOptim(nnode);
							optimTime.stop();

							if(nnodep == nnode){
								nnode->addToParents();
								this->addNode(nnode);							
							}
							nnode = nnodep;
						}

						bool_node* ornode = new OR_node();
						ornode->mother = cur;
						ornode->father = nnode;
						{
							optimTime.restart();
							bool_node* ornodep = this->computeOptim(ornode);
							optimTime.stop();

							if(ornodep == ornode){
								this->addNode(ornode);
								ornode->addToParents();
							}	
							ornode = ornodep;
						}
						cur = ornode;				
					}
					ASSERT_node* asn = new ASSERT_node();
					asn->mother = cur;
					asn->setMsg( dynamic_cast<ASSERT_node*>(n)->getMsg() );
					asn->addToParents();
					this->addNode(asn);
					
					n->dislodge();
					n->id = -22;
					delete n;
				}
			}else{				
				if( n!= NULL){
					output = n->mother;
					n->dislodge();
					n->id = -22;
					delete n;
				}
			}
		}		
		rvalue = output;
		somethingChanged = true;
	}else{
		rvalue = &node;
	}
	ufunAll.stop();
}

void DagFunctionInliner::immInline(BooleanDAG& dag){

	initialize(dag);

	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				
		optAll.restart();
		bool_node* node = computeOptim(dag[i]);
		optAll.stop();

		if(dag[i] != node){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				replTime.restart();
				dag.replace(i, node, replTime2);
				replTime.stop();

		}
	}
		
	cleanup(dag);
	tnbuilder.reset();

	Dout( cout<<" AFTER PROCESS "<<endl );
	Dout(cout<<" end ElimFun "<<endl);
}

extern map<string, pair<int, int> > sizes;


void DagFunctionInliner::process(BooleanDAG& dag){
	cout<<" funmap has size " << functionMap.size() << endl;
	somethingChanged = true;
	{
		DagOptim optim(dag);
		optim.process(dag);
	}
	
	timerclass everything("everything");

	everything.start();
	int inlin = 0;
	while(somethingChanged && dag.size() < 25000 && inlin < 20){
		somethingChanged = false;
		cout<<inlin<<": inside the loop dag.size()=="<<dag.size()<<endl;
		immInline(dag);	
		//if(inlin==0){( dag.print(cout) );}
		++inlin;
	}
	everything.stop();
	everything.print();
	ufunAll.print();
	optAll.print();
	replTime.print();
	replTime2.print();
	tnbuildTime.print();
	optimTime.print();
	
	for(map<string, pair<int, int> >::iterator it = sizes.begin(); it != sizes.end(); ++it){
		cout<<it->first<<" : "<< (it->second.second / it->second.first)<<"  "<<it->second.second<<"  "<<it->second.first<<endl;
	}


	( cout<<" after all inlining dag.size()=="<<dag.size()<<endl);
	
	
	// dag.print(cout);
	{
		DagFunctionToAssertion makeAssert(dag, functionMap);
		makeAssert.process(dag);
	}
	
	cout<<" After everything: dag.size()=="<<dag.size()<<endl;	
}


