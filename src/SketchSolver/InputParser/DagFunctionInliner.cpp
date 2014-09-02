#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"
#include "CommandLineArgs.h"
#include <sstream>


//extern CommandLineArgs* PARAMS;

#ifndef  _MSC_VER
static const int MAX_NODES = 1000000;
#else
static const int MAX_NODES = 510000;
#endif

DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, 	map<string, int>& p_randholes,
	bool p_randomize, InlineControl* ict):
dag(p_dag), 
DagOptim(p_dag), 
ufunAll(" ufun all"),
functionMap(p_functionMap),
ictrl(ict),
randomize(p_randomize),
randholes(p_randholes)
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

bool_node* DagFunctionInliner::optAndAddNoMap(bool_node* nnode){ //assumes node is unnattached
	bool_node* nnodep = this->computeOptim(nnode);						
	if(nnodep == nnode){
		nnode->addToParents();
		this->addNode(nnode);								
	}else{
		delete nnode;
	}
	return nnodep;
}


void DagFunctionInliner::optAndAdd(bool_node* n, vector<const bool_node*>& nmap){//Assumes node has already been attached
	int nodeId = n->id;
	bool_node* nnode = this->computeOptim(n);						
	if(nnode == n){
		this->addNode( n );							
		nmap[nodeId] = n;	
	}else{
		n->dislodge();
		delete n;
		nmap[nodeId] = nnode;
	}
}


void DagFunctionInliner::visit(CTRL_node& node){
	
	
	if(node.get_Pcond()){
		rvalue = this->getCnode(true);
	}else{
		if(randomize){
			rvalue = checkRandHole(&node);
			return;
		}
		DagOptim::visit(node);
	}
}


bool checkParentsInMap(bool_node* node, vector<const bool_node*>& secondarynmap, vector<const bool_node*>& nmap){
	bool chk = false;
	bool_node* tt = node->mother;
	chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);
	tt = node->father;
	chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);
	arith_node* an = NULL;
	if(node->isArith()){
		an = dynamic_cast<arith_node*>(node);
		for(int i=0; i<an->multi_mother.size(); ++i){
			tt = an->multi_mother[i];
			chk = chk || (tt!= NULL && secondarynmap[tt->id] != NULL);			
		}
	}
	if(chk){
		tt = node->mother;
		if(tt!= NULL && secondarynmap[tt->id] == NULL){
			secondarynmap[tt->id] = nmap[tt->id];
		}
		tt = node->father;
		if(tt!= NULL && secondarynmap[tt->id] == NULL){
			secondarynmap[tt->id] = nmap[tt->id];
		}
		if(an != NULL){
			for(int i=0; i<an->multi_mother.size(); ++i){
				tt = an->multi_mother[i];
				if(tt!= NULL && secondarynmap[tt->id] == NULL){
					secondarynmap[tt->id] = nmap[tt->id];
				}
			}
		}
	}
	return chk;
}


void DagFunctionInliner::visit( UFUN_node& node ){	
	Dllist tmpList;
	const string& name = node.get_ufname();
	map<int, int> oldToNew;

	if(ictrl != NULL && !ictrl->checkInline(node)){
		mpcontroller[node.fgid]["__ALL"] = NULL;
		DagOptim::visit(node);			
		return;
	}	


	bool_node* condition = node.mother;
	if(isConst(condition) && !getBval(condition)){
		rvalue = getCnode(0);
		mpcontroller[node.fgid]["__ALL"] = getCnode(0);
		return;
	}

	ufunAll.restart();


	if( functionMap.find(name) != functionMap.end() ){
		if(mpcontroller.count(node.fgid) > 0){
			map<string,bool_node*>::iterator it = mpcontroller[node.fgid].find(node.outname);
			if(it != mpcontroller[node.fgid].end()){
				bool_node* rv = it->second;
				rvalue = rv;
			}else{
				bool_node* rv = mpcontroller[node.fgid]["__ALL"];
				if(rv == NULL){
					DagOptim::visit(node);	
				}else{
					rvalue = rv;
				}
			}			
			return;
		}

				
		BooleanDAG& oldFun = *functionMap[name];

		if(oldFun.isModel && node.children.size() == 0){
			rvalue = getCnode(false);
			return;
		}

		funsInlined.insert(name);
		if(ictrl != NULL){ ictrl->registerInline(node); }


		// cout<<" Inlined "<<node.get_ufname()<<"   mother = "<<node.mother->lprint()<<endl;
		/*
		if(!isConst(node.mother)){
			cout<<" Inlined "<<node.get_ufname()<<"   mother = "<<node.mother->lprint()<<endl;
			stack<bool_node*> bns;
			bns.push(node.mother);
			while(! bns.empty()){
				bool_node* c = bns.top(); 
				bns.pop();
				if(c->mother != NULL && !isConst(c->mother)){					
					bns.push(c->mother);
				}
				if(c->father != NULL && !isConst(c->father)){
					bns.push(c->father);
				}
				if(c->isArith() && c->type != bool_node::UFUN){
					vector<bool_node*>& bn = dynamic_cast<arith_node*>(c)->multi_mother;
					for(int i=0; i<bn.size(); ++i){
						if(!isConst(bn[i])){
							bns.push(bn[i]);
						}
					}
				}
				cout<<"          "<<c->lprint()<<endl;
			}
		}
		*/

		

		//oldFun->clone_nodes(clones);		
		vector<const bool_node*> nmap;
		nmap.resize( oldFun.size() );		
		{
			vector<bool_node*>& inputs  = oldFun.getNodesByType(bool_node::SRC);
			
			Assert( inputs.size() == node.multi_mother.size() , node.get_ufname()<<" argument missmatch: "<<inputs.size()<<" formal parameters, but only got "<<node.multi_mother.size()<<" actuals.\n"<<node.lprint());
			
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
				if(ctrl->get_Pcond()){
					nmap[ctrl->id] = node.mother;
					continue;
				}
				if(randomize){
					bool_node* subst = checkRandHole(ctrl);
					if(subst != ctrl){
						nmap[ctrl->id] = subst;
						continue;
					}
				}

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
		
		lnfuns++;
		
		// cout<<node.get_ufname()<<endl;

		/*
			The idea is that we have a wavefront moving through the graph as we add more nodes.
			n is going to be in the boundary of this wavefront. The parents of n will be good nodes,
			but their children will still have pointers to oldFun. 

		*/

		vector<const bool_node*> secondarynmap;
		secondarynmap.resize( oldFun.size(), NULL );
		map<const UFUN_node*, SRC_node*> ufToSrc;
		
		bool doubleCopies = false;

		
		bool_node* output = NULL;
		bool_node* assertCond = NULL;

		for(int i=0; i<oldFun.size(); ++i){
			
			bool_node::Type t = oldFun[i]->type;


			if(t == bool_node::SRC || t == bool_node::CTRL) continue;
			if(t == bool_node::CONST){
				CONST_node* n =  dynamic_cast<CONST_node*>(oldFun[i]);
				if(n->isFloat()){
					nmap[n->id] = getCnode(n->getFval());
				}else{
					nmap[n->id] = getCnode(n->getVal());
				}
				
				continue;
			}
			bool_node* n =  oldFun[i]->clone(false);
			//bool_node* of = oldFun[i];
			n->redirectParentPointers(oldFun, nmap, true, oldFun[i]);			

			bool_node* nprime = NULL;

			if(doubleCopies){				
				if(checkParentsInMap(oldFun[i], secondarynmap, nmap)){
					nprime = oldFun[i]->clone(false);					
					nprime->redirectParentPointers(oldFun, secondarynmap, true, oldFun[i]);
				}
			}

			int nodeId = n->id;
			if( n != NULL &&  n->type != bool_node::DST ){			
				if(n->type != bool_node::ASSERT){
					if(typeid(*n) != typeid(UFUN_node)){
						
						optAndAdd(n, nmap);
						if(nprime != NULL){
							optAndAdd(nprime, secondarynmap);
						}						

					}else{			
						UFUN_node* ufun = dynamic_cast<UFUN_node*>(n);						
						ufun->ignoreAsserts = ufun->ignoreAsserts || node.ignoreAsserts;

						if(!ufun->ignoreAsserts){													
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
						if(!ufun->ignoreAsserts){	
							/*
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
							*/
						}else{
							if(!isConst(ufun->mother)){
								ufun->mother->remove_child(n);
								ufun->mother = getCnode(1);
								ufun->mother->children.insert(n);
								for(vector<bool_node*>::iterator it = ufun->multi_mother.begin(); it != ufun->multi_mother.end(); ++it){
									if(*it == oldMother){
  										oldMother->children.insert( n );
										break;
									}
								  }
							}
						}
						DagOptim::visit(*ufun);						
						const bool_node* nnode = rvalue;
						if( nnode->type == bool_node::UFUN ){
							//&& !(isConst(oldMother) && this->getIval(oldMother)==0)
							if(ictrl != NULL){ ictrl->registerCall(node, dynamic_cast<const UFUN_node*>(nnode)); }
						}
						if(nnode == n){
							//bool_node* tmp = n->clone();
							// cse.setCSE(tmp);
							// replaceDummy(n, tmp);
							
							this->addNode( n );
							nmap[nodeId] = n;							
							nnode = n;
						}else{
							n->dislodge();
							delete n;
							//n->removeFromParents(of);
							nmap[nodeId] = nnode;
						}
						if(nprime != NULL){
							nprime->dislodge();
							delete nprime;
						}
						if(oldFun.isModel && nnode->type == bool_node::UFUN ){
							const UFUN_node* ufn = dynamic_cast<const UFUN_node*>(nnode);
							//Add an SRC node to stand in place of this ufun.
							string nm = ufn->get_ufname();
							nm += "_";
							nm += ufn->outname;
							SRC_node* sn = new SRC_node(nm);
							// BUGFIX: this is not wide enough! ufn->nbits is either 1 or 2, set by InputParser.cpp
							//sn->set_nbits( ufn->get_nbits() );
							//cout << "DagFunctionInliner: ufn=" << ufn->lprint() << " nbits=" << ufn->get_nbits() << " isArr=" << ufn->isArr() << endl;
							// BUGFIX: nbits must be 1 if original UFUN out is boolean type
							int nbits = ufn->get_nbits();
							if (nbits > 1) { nbits = PARAMS->NANGELICS; }
							sn->set_nbits(nbits);
							// BUGFIX: need to setArr if UFUN out is array. related to Issue #5.
							// TODO xzl: is this correct? is it necessary?
							if (ufn->isArr()) {
								//cout << "sn " << sn->lprint() << " ufn " << ufn->lprint() << endl;
								sn->setArr(PARAMS->angelic_arrsz);
							}
							doubleCopies = true;
							secondarynmap[nodeId] = sn;
							ufToSrc[ufn] = sn;
						}

					}
				}else{
					n->mother->remove_child( n );		
					if(nprime != NULL){
						nprime->mother->remove_child( nprime );
					}
					//Assertion.					


					if((isConst(n->mother) && getIval(n->mother) == 1)|| (!oldFun.isModel && node.ignoreAsserts ) ){ 					
						delete n;
						if(nprime != NULL){							
							delete nprime;
						}
						continue;
					}					

					if(!oldFun.isModel || nprime == NULL){	
						/*
						bool_node* nnode = new NOT_node();
						nnode->mother = condition;
						nnode = optAndAddNoMap(nnode);
					
					
						bool_node* cur = n->mother;
						bool_node* ornode = new OR_node();
						ornode->mother = cur;
						ornode->father = nnode;
						ornode = optAndAddNoMap(ornode);
						n->mother = ornode;
						*/
						
					}else{
						bool_node* cur = n->mother;
						n->mother = nprime->mother;
						delete nprime;
						dynamic_cast<ASSERT_node*>(n)->makeHardAssert();
						if(assertCond == NULL){
							assertCond = cur;							
						}else{			
							bool_node* tt = new AND_node();
							tt->mother = assertCond;
							tt->father = cur;
							tt->addToParents();
							addNode(tt);							
							assertCond = tt;
						}
					}
					

					{							
						DllistNode* tt = getDllnode(n);
						tmpList.append(tt);
					}
					n->mother->children.insert(n);	
					
					this->addNode(n);
					if(!dynamic_cast<ASSERT_node*>(n)->isNormal()){						
						callMap.clear();						
					}
				}
			}else{
				if( n!= NULL){
					DST_node* dn = dynamic_cast<DST_node*>(n);
					bool_node* ttv = n->mother;
					if(oldFun.isModel){
						if(assertCond == NULL){
							UFUN_node* un = dynamic_cast<UFUN_node*>(ttv);												
							Assert(un != NULL, "Only ufun node can be the output of a model. ttv=" << ttv->lprint());
							SRC_node* sc = ufToSrc[un];
							sc->neighbor_replace(ttv);
							sc->children.clear();
						}else{
							CTRL_node* qn = new CTRL_node();
							stringstream st;
							st<<dn->name;
							st<<lnfuns;
						
							qn->name = st.str();
							qn->set_Angelic();
						
							ARRACC_node* mx = new ARRACC_node();
							if(!PARAMS->angelic_model)
								mx->mother = assertCond;
							else{
								//create new src node which should be valued to false in Synth phase
								SRC_node *st = new SRC_node("__rs_node");
								st->set_nbits(1);//just one bit input
								mx->mother = st;
								addNode(st);
								
							}
							//if its all-angelic-model, we need to set mx->mother as new SRC_node to be set to false in Syn phase
							mx->multi_mother.push_back(qn);
							mx->multi_mother.push_back(ttv);
							mx->addToParents();
							addNode(qn);
							addNode(mx);
							UFUN_node* un = dynamic_cast<UFUN_node*>(ttv);
							Assert(un != NULL, "Output of model should be an uninterpreted function.");
							// BUGFIX: nbits must be 1 if original UFUN out is boolean type
							int nbits = un->get_nbits();
							if (nbits > 1) { nbits = PARAMS->NANGELICS; }
							qn->set_nbits(nbits);
							//cout << "DagFunctionInliner: un=" << un->lprint() << " nbits=" << un->get_nbits() << " isArr=" << un->isArr() << endl;
												
							Assert(un != NULL, "Only ufun node can be the output of a model ttv=" << ttv->lprint());
							// BUGFIX xzl: fix Issue #5, when angelic CTRL is an array
							if (un->isArr()) {
								//cout << "qn " << qn->lprint() << " un " << un->lprint() << endl;
								qn->setArr(PARAMS->angelic_arrsz);
							}
							SRC_node* sc = ufToSrc[un];
							sc->neighbor_replace(mx);
							sc->children.clear();
							ttv = mx;
						}
																		
						nprime->dislodge();
						delete nprime;
					}
					mpcontroller[node.fgid][dn->name] = ttv;
					if(dn->name == node.outname){
						output = ttv;
					}										
					n->dislodge();
					delete n;
				}
			}
		}

		for(map<const UFUN_node*, SRC_node*>::iterator it = ufToSrc.begin(); it != ufToSrc.end(); ++it){
			if(it->second->children.size() == 0){
				delete it->second;
			}else{
				Assert(false, "NYI: All the outputs of a function in a model should be outputed by the model.");
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
		DllistNode* lastDln = NULL;
		for(int i=0; i<dag.size() ; ++i ){

			if(isDllnode(dag[i])){
				lastDln = getDllnode(dag[i]);
			}

			// Get the code for this node.				
			if(dag[i]->type == bool_node::UFUN){
				UFUN_node& uf = *dynamic_cast<UFUN_node*>(dag[i]);
				uidcount = max(uidcount, uf.fgid);
				/*
				When the inline controller checks a function and the function is 
				not inlined, the controller makes sure other function with the same path 
				condition are also not inlined. Therefore, it is good to call checkInline
				on all functions first, before we start inlining.
				*/
				ictrl->preCheckInline(uf);
				map<string, BooleanDAG*>::iterator it = functionMap.find(uf.get_ufname());
				if(it != functionMap.end()){
					if(it->second->isModel && uf.ignoreAsserts){
						if(lastDln != NULL){
							lastDln->add(&uf);
						}else{
							dag.assertions.append(&uf);
						}
					}
				}
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


