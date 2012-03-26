#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"
#include "CommandLineArgs.h"



//extern CommandLineArgs* PARAMS;

#ifndef  _MSC_VER
static const int MAX_NODES = 1000000;
#else
static const int MAX_NODES = 510000;
#endif

DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, InlineControl* ict):
dag(p_dag), 
DagOptim(p_dag), 
ufunAll(" ufun all"),
functionMap(p_functionMap),
ictrl(ict)
{
	alterARRACS();
}

DagFunctionInliner::~DagFunctionInliner()
{
}


/*
int space[100];
ARRACC_node tmparrac;
UFUN_node tmpufun("NULL");
ARRASS_node tmparrass;
ACTRL_node tmpactrl;
*/


int sizeForNode(bool_node* bn){

	if(typeid(*bn) == typeid(SRC_node)){
		return sizeof(SRC_node);
	}
	if(typeid(*bn) == typeid(CTRL_node)){
		return sizeof(CTRL_node);
	}	
	if(typeid(*bn) == typeid(DST_node)){
		return sizeof(CTRL_node);
	}	
	if(typeid(*bn) == typeid(CONST_node)){
		return sizeof(CONST_node);
	}
	if(typeid(*bn) == typeid(ASSERT_node)){
		return sizeof(CONST_node);
	}
	return sizeof(AND_node);
}







void DagFunctionInliner::visit( UFUN_node& node ){	
	Dllist tmpList;
	const string& name = node.get_ufname();
	map<int, int> oldToNew;

	if(ictrl != NULL && !ictrl->checkInline(node)){
		rvalue = &node;
		return;
	}	


	bool_node* condition = node.mother;
	if(isConst(condition) && !getBval(condition)){
		rvalue = getCnode(0);
		return;
	}

	ufunAll.restart();


	if( functionMap.find(name) != functionMap.end() ){
		if(mpcontroller.count(node.fgid) > 0){
			bool_node* rv = mpcontroller[node.fgid][node.outname];
			rvalue = rv;
			return;
		}


		funsInlined.insert(name);
		if(ictrl != NULL){ ictrl->registerInline(node); }
		//cout<<" inlining "<<name<<endl;
		BooleanDAG& oldFun = *functionMap[name];
		//oldFun->clone_nodes(clones);		
		vector<const bool_node*> nmap;
		nmap.resize( oldFun.size() );		
		{
			vector<bool_node*>& inputs  = oldFun.getNodesByType(bool_node::SRC);
			
			Assert( inputs.size() == node.multi_mother.size() , "Argument missmatch: More formal than actual parameters");
			
			for(int i=0; i<inputs.size(); ++i){			
				
				bool_node* actual = node.multi_mother[i];
				nmap[inputs[i]->id] = actual;
				actual->children.erase((bool_node*)&node);
//				actual->children.insert(inputs[i]->children.begin(), inputs[i]->children.end());				
			}
		}
		//cout<<endl;
		{
			vector<bool_node*>& controls  = oldFun.getNodesByType(bool_node::CTRL);
			
			for(int i=0; i<controls.size(); ++i){	
				CTRL_node* ctrl = dynamic_cast<CTRL_node*>(controls[i]);
				bool_node* actual = dag.unchecked_get_node( ctrl->name );
				if(actual != NULL){
					nmap[controls[i]->id] = actual;
					actual->children.erase((bool_node*)&node);
//					actual->children.insert(controls[i]->children.begin(), controls[i]->children.end());
				}else{
					if(seenControls.count(ctrl->name)>0){						
						nmap[controls[i]->id] = seenControls[ctrl->name];
					}else{
						bool_node* tmp = controls[i]->clone(false);
						addNode( tmp );
						nmap[controls[i]->id] = tmp;
						seenControls[ctrl->name] = tmp;
					}
				}
			}
		}
		

		/*
			The idea is that we have a wavefront moving through the graph as we add more nodes.
			n is going to be in the boundary of this wavefront. The parents of n will be good nodes,
			but their children will still have pointers to oldFun. 

		*/

		
		lnfuns++;
		

		
		bool_node* output = NULL;

		for(int i=0; i<oldFun.size(); ++i){
			
			bool_node::Type t = oldFun[i]->type;
			if(t == bool_node::SRC || t == bool_node::CTRL) continue;
			if(t == bool_node::CONST){
				CONST_node* n =  dynamic_cast<CONST_node*>(oldFun[i]);
				nmap[n->id] = getCnode(n->getVal());
				continue;
			}
			bool_node* n =  oldFun[i]->clone(false);
			//bool_node* of = oldFun[i];
			n->redirectParentPointers(oldFun, nmap, true, oldFun[i]);			
			int nodeId = n->id;
			if( n != NULL &&  n->type != bool_node::DST ){			
				if(n->type != bool_node::ASSERT){
					if(typeid(*n) != typeid(UFUN_node)){

						
						bool_node* nnode = this->computeOptim(n);
						

						if(nnode == n){
							//bool_node* tmp = n->clone();											
							this->addNode( n );
							
							nmap[nodeId] = n;
							//tmp->replace_child_inParents(of, n); // maybe redundant.
							// parentReplace(oldFun[i], tmp);
						}else{
							n->dislodge();
							//n->removeFromParents(of);
							delete n;
							nmap[nodeId] = nnode;
						}
					}else{			
						UFUN_node* ufun = dynamic_cast<UFUN_node*>(n);
						ufun->ignoreAsserts = ufun->ignoreAsserts || node.ignoreAsserts;

						{							
							DllistNode* tt = getDllnode(ufun);
							tmpList.append(tt);
						}	
						{
							if(oldToNew.count(ufun->fgid)>0){
								ufun->fgid = oldToNew[ufun->fgid];
							}else{
								++uidcount;
								oldToNew[ufun->fgid] = uidcount;
								ufun->fgid = uidcount;
							}
						}


						bool_node * oldMother = ufun->mother;						
						ufun->mother->remove_child( n );
						bool_node* andCond = new AND_node();
						andCond->mother = ufun->mother;
						andCond->father = condition;

						{
							
							bool_node* andcondPrime = this->computeOptim(andCond);
							
							if(andcondPrime == andCond){
								
								this->addNode(andCond);
								andCond->addToParents();
							}else{
								delete andCond;
							}
							andCond = andcondPrime;
						}
						ufun->mother = andCond;
						ufun->mother->children.insert(n);
						if(andCond != oldMother){
							for(vector<bool_node*>::iterator it = ufun->multi_mother.begin(); it != ufun->multi_mother.end(); ++it){
								if(*it == oldMother){
  									oldMother->children.insert( n );
									break;
								}
							  }
						}
						DagOptim::visit(*ufun);						
						const bool_node* nnode = rvalue;
						if( !(isConst(oldMother) && this->getIval(oldMother)==0)){
							if(ictrl != NULL){ ictrl->registerCall(node, dynamic_cast<const UFUN_node*>(nnode)); }
						}
						if(nnode == n){
							//bool_node* tmp = n->clone();
							// cse.setCSE(tmp);
							// replaceDummy(n, tmp);
							
							this->addNode( n );
							nmap[nodeId] = n;							
						}else{
							n->dislodge();
							delete n;
							//n->removeFromParents(of);
							nmap[nodeId] = nnode;
						}
					}
				}else{
					n->mother->remove_child( n );
					if((isConst(n->mother) && getIval(n->mother) == 1)|| node.ignoreAsserts ){ continue; }
					bool_node* nnode = new NOT_node();
					nnode->mother = condition;					
					{
						
						bool_node* nnodep = this->computeOptim(nnode);
						
						if(nnodep == nnode){
							nnode->addToParents();
							this->addNode(nnode);	
							
						}else{
							delete nnode;
						}
						nnode = nnodep;
					}

					
					bool_node* cur = n->mother;
					bool_node* ornode = new OR_node();
					ornode->mother = cur;
					ornode->father = nnode;
					{
						
						bool_node* ornodep = this->computeOptim(ornode);
						
						if(ornodep == ornode){
							this->addNode(ornode);
							
							ornode->addToParents();

						}else{
							delete ornode;
						}
						ornode = ornodep;
					}
					cur = ornode;	
					n->mother = cur;		
					{							
						DllistNode* tt = getDllnode(n);
						tmpList.append(tt);
					}
					if(isConst(cur) && getIval(cur) == 1){
						delete n;
						//In this case, the assertion is just ignored.
						// n->dislodge(); Not needed, since we removed from the mother already.
					}else{
						n->mother->children.insert(n);						
						// tmp->replace_child_inParents(of, tmp);

						this->addNode(n);
						
					}
				}
			}else{
				if( n!= NULL){
					DST_node* dn = dynamic_cast<DST_node*>(n);
					bool_node* ttv = n->mother;
					mpcontroller[node.fgid][dn->name] = ttv;
					if(dn->name == node.outname){
						output = ttv;
					}
					n->dislodge();
					delete n;
				}
			}
		}
		node.add(&tmpList);
		node.remove();
		rvalue = output;
		if(rvalue == NULL){
			rvalue = getCnode(0);
		}
		somethingChanged = true;
	}else{
		DagOptim::visit(node);
	}
	ufunAll.stop();	
}




/*
extern map<string, pair<int, int> > sizes;
*/

void DagFunctionInliner::process(BooleanDAG& dag){
	// cout<<" funmap has size " << functionMap.size() << endl;
	initLight(dag);
	funsInlined.clear();
	somethingChanged = false;
	lnfuns = 0;
	uidcount = 0;
	if(ictrl != NULL){
		for(int i=0; i<dag.size() ; ++i ){
			// Get the code for this node.				
			if(dag[i]->type == bool_node::UFUN){
				UFUN_node& uf = *dynamic_cast<UFUN_node*>(dag[i]);
				uidcount = max(uidcount, uf.fgid);
				ictrl->checkInline(uf);
			}
		}
	}
	
	mpcontroller.clear();

	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.						
		
				
		bool_node* node = computeOptim(dag[i]);		
		
		
		if(dag[i] != node){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				dag.replace(i, node);
		}
	}

	// cout<<" added nodes = "<<newnodes.size()<<endl;

	seenControls.clear();	
	cleanup(dag);
}


