#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"
#include "timerclass.h"


int FRACTION = 8;


DagFunctionInliner::DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, int p_inlineAmnt, bool p_mergeFunctions):
dag(p_dag), 
DagOptim(p_dag), 
functionMap(p_functionMap),
replTime(" replacement "),
replTime2(" replacement internal"),
tnbuildTime(" tnbuilding "),
optimTime(" optim "),
ufunAll(" ufun all"),
optAll(" opt all "),
cleanupTime("cleanup time"),
unifyTime("unify time"),
clonetime(" clone time"),
inlineAmnt(p_inlineAmnt),
mergeFunctions(p_mergeFunctions),
divFactor(32),
oldNfun(-1)
{
	somethingChanged = false;
	cout<<" Inline amount = p_inlineAmnt"<<endl;
}

DagFunctionInliner::~DagFunctionInliner()
{
}


void DagFunctionInliner::computeSpecialInputs(){
	for(map<string, BooleanDAG*>::iterator it = functionMap.begin(); it != functionMap.end(); ++it){
		BooleanDAG* fun = it->second;
		vector<bool_node*>& inputs  = fun->getNodesByType(bool_node::SRC);
		for(int i=0; i<inputs.size(); ++i){	
			string fn = inputs[i]->get_name();
			if(fn.find("PC") != -1 ){
					specialInputs[i] = 50;
			}
			if(fn.find("count") != -1){
					specialInputs[i] = 10;
			}
		}
	}
}


void DagFunctionInliner::visit( UFUN_node& node ){	
	ufunAll.restart();
	string& name = node.get_ufname();
	if( functionMap.find(name) != functionMap.end() ){
		//cout<<" inlining "<<name<<endl;
		clonetime.restart();
		BooleanDAG* oldFun = functionMap[name];
		oldFun->clone_nodes(clones);
		clonetime.stop();

		{
			vector<bool_node*>& inputs  = oldFun->getNodesByType(bool_node::SRC);
			
			Assert( inputs.size() == node.multi_mother.size() , "Argument missmatch: More formal than actual parameters");
			
			for(int i=0; i<inputs.size(); ++i){			
				bool_node* formal = clones[inputs[i]->id];
				bool_node* actual = node.multi_mother[i];
				string fn = formal->get_name();
				if(specialInputs.count(i) > 0){
					//cout<<i<<". formal : " << fn << " := "<<actual->get_name()<<endl;
					//cout<<"";
				}
				Assert( clones[formal->id] == formal, "ID is incorrect");
				replTime.restart();
				setTimestampChildren(formal);
				formal->neighbor_replace(actual, replTime2);
				clones[formal->id] = NULL;
				delete formal;
				replTime.stop();
			}
		}
		//cout<<endl;
		{
			vector<bool_node*>& controls  = oldFun->getNodesByType(bool_node::CTRL);
			
			for(int i=0; i<controls.size(); ++i){			
				bool_node* formal = clones[controls[i]->id];
				bool_node* actual = dag.unchecked_get_node( formal->name );		
				if(actual != NULL){
					Assert( clones[formal->id] == formal, "ID is incorrect");	
					replTime.restart();
					setTimestampChildren(formal);
					formal->neighbor_replace(actual, replTime2);
					clones[formal->id] = NULL;
					delete formal;
					replTime.stop();
				}
			}
		}
		
		vector<bool_node*>& outputs  = oldFun->getNodesByType(bool_node::DST);
		
		Assert( outputs.size() == 1, "The outputs are of the wrong size "<< outputs.size()<<"  "<< name);
				
		//rvalue = outputs[0]->mother;
		
		
		bool_node* tn = NULL;
				
		
		int hasBuiltTN = false;
		
		
		bool_node* output = NULL;

		for(int i=0; i<clones.size(); ++i){
			bool_node* n = clones[i];
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
							setTimestampChildren(n);
							n->neighbor_replace(nnode, replTime2);
							replTime.stop();
							delete n;
						}
					}else{						
						bool_node* nnode = cse.computeCSE(n);
						if(nnode == n){
							this->addNode(n);
						}else{
							replTime.restart();
							setTimestampChildren(n);
							n->neighbor_replace(nnode, replTime2);
							replTime.stop();
							delete n;
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
								setTimestamp(nnode);
								this->addNode(nnode);							
							}else{
								delete nnode;
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
								setTimestamp(ornode);
							}else{
								delete ornode;
							}
							ornode = ornodep;
						}
						cur = ornode;				
					}
					ASSERT_node* asn = new ASSERT_node();
					asn->mother = cur;
					asn->setMsg( dynamic_cast<ASSERT_node*>(n)->getMsg() );
					asn->addToParents();
					setTimestamp(asn);
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


void collectSet(arith_node* an, set<bool_node*>& s, int lev){
	s.insert(an);
	if(lev <= 0){ return; }
	for(int i=0; i<an->multi_mother.size(); ++i){
		bool_node* bn = an->multi_mother[i];
		if(typeid(*bn) == typeid(ARRACC_node)){
			collectSet(dynamic_cast<arith_node*>(bn), s, lev-1);
		}else{
			s.insert(bn);
		}
	}
}

bool compareSet(arith_node* an, set<bool_node*>& s, int lev){
	if(s.count(an) > 0){
		return true;
	}
	if(lev <= 0){ return false; }
	for(int i=0; i<an->multi_mother.size(); ++i){
		bool_node* bn = an->multi_mother[i];
		if(typeid(*bn) == typeid(ARRACC_node)){
			bool tmp = compareSet(dynamic_cast<arith_node*>(bn), s, lev-1);
			if(!tmp){
				return false;
			}
		}else{
			if(s.count(bn)==0){
				return false;
			}
		}
	}
	return true;
}


int compareDifferent(bool_node* bn1, bool_node* bn2){
		// If they are different, but they are equivalent ARRACCS differing only on their mothers, add 1.
		//If they are just different symbolic values, add 2.
	
	if(typeid(*bn1) == typeid(ARRACC_node) && typeid(*bn2) == typeid(ARRACC_node)){
		arith_node* an1 = dynamic_cast<arith_node*>(bn1);
		arith_node* an2 = dynamic_cast<arith_node*>(bn2);
		set<bool_node*> s;
		collectSet(an1, s, 3);
		bool tmp = compareSet(an2, s, 3);
		if(tmp){
			return 1;
		}
		return 2;
	}
	
	//If one of them is a constant, add 3.	
	
	if(typeid(*bn1) == typeid(CONST_node) || typeid(*bn2) == typeid(CONST_node)){
		return 3;
	}
	
	return 2;
}


int DagFunctionInliner::argsCompare(vector<bool_node*> arg1, vector<bool_node*> arg2){
	int rv = 0;
	Assert(arg1.size() == arg2.size(), "This can't be happening. It's an invariant. Something is very strange");
	for(int i=0; i<arg1.size(); ++i){
		if(specialInputs.count(i) > 0){
			//cout<<i<<"=("<<arg1[i]->get_name()<<", "<<arg2[i]->get_name()<<")   ";
		}
		if(arg1[i]->id != arg2[i]->id){
			if(specialInputs.count(i) > 0){
				rv+= ( compareDifferent(arg1[i], arg2[i]) *  specialInputs[i]  );
			}else{
				rv+= compareDifferent(arg1[i], arg2[i]);
			}
		}
	}
	//cout<<endl;
	return rv;
}



//Merges the function at id first with the function at id second. The resulting function will go to ID first.

void DagFunctionInliner::mergeFuncalls(int first, int second){
	// cout<<" merging "<<first<<" and "<<second<<endl;
	UFUN_node* fun1 = dynamic_cast<UFUN_node*>(dag[first]);
	UFUN_node* fun2 = dynamic_cast<UFUN_node*>(dag[second]);

	vector<bool_node*>& args1 = fun1->multi_mother;
	vector<bool_node*>& args2 = fun2->multi_mother;

	vector<bool_node*> nargs;
	int szz1 = newnodes.size();
	bool_node* ecall2 = tnbuilder.get_exe_cond(fun2, *this);
	int szz2 = newnodes.size();


	for(int i=0; i<args1.size(); ++i){
		if(specialInputs.count(i) > 0){
			//cout<<i<<"=("<<args1[i]->get_name()<<", "<<args2[i]->get_name()<<")   ";
		}
		if(args1[i] == args2[i]){
			nargs.push_back(args1[i]);
		}else{
			ARRACC_node* an = new ARRACC_node();
			an->mother = ecall2;
			an->multi_mother.push_back(args1[i]);
			an->multi_mother.push_back(args2[i]);
			an->addToParents();
			setTimestamp(an);
			this->addNode(an);
			nargs.push_back(an);
		}
	}

	UFUN_node* funnew = new UFUN_node(*fun1);
	funnew->multi_mother = nargs;
	funnew->children.clear();
	funnew->addToParents();
	setTimestamp(funnew);
	setTimestampChildren(fun1);
	setTimestampChildren(fun2);
	dag.replace(fun1->id, funnew);
	dag.replace(fun2->id, funnew);


	dag[first] = funnew;

}

bool checkFunName(string& name){

	return true;
}

void DagFunctionInliner::unify(){
	initLight(dag);
	vector<vector<bool_node*> >  argLists;
	vector<vector<int> >  diffs;
	map<int, int>  locToG;
	int tot;
	map<int, vector<int> >  nfd;
	map<int, int>  closestMatch;
	int maxDif = -1;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
		if(typeid(*dag[i]) == typeid(UFUN_node)){
			UFUN_node* ufun = dynamic_cast<UFUN_node*>(dag[i]);
			string& name = ufun->get_ufname();
			if(checkFunName(name)){
				vector<vector<bool_node*> >& v = argLists;	
				int id = v.size();
				vector<int> difRow;
				locToG[id] = i;
				tot = 0; 
				int lowestDif = 100000;
				int lowestDifID = -1;
				for(int j=0; j<v.size(); j++){
					int dif = argsCompare(v[j], ufun->multi_mother);
					maxDif = dif > maxDif ? dif : maxDif;
					tot += dif;
					if(dif < lowestDif){ 
						lowestDif = dif; 
						lowestDifID = j;
					}
					 //cout<<" ("<<id<<", "<<j<<")  "<<dif<<endl;
				}
				//cout<<"  id = "<<id<<" lowestDif ="<<lowestDif<<"avg dif="<< (id>0 ? tot / id : -1) <<" ldifID = "<<lowestDifID<<endl;
				closestMatch[id] = lowestDifID;
				nfd[lowestDif].push_back(id);
				v.push_back(ufun->multi_mother);	

			}			
		}
	}




    int nmerges = 0;
	cout<<"  expectedNFuns="<<expectedNFuns<<endl;
	int totFuns=0;
	map<int, int> actualMatch;
	for(map<int, vector<int> >::reverse_iterator it = nfd.rbegin(); it != nfd.rend(); ++it){
		vector<int>& ids = it->second;
		cout<<" ldiff = "<<it->first<<" size = "<<ids.size()<<endl;
		if(totFuns >= expectedNFuns){
			//If we are here, we've exhausted our quota of functions, so we'll start merging the remaining ones with their closest match.
			for(int i=0; i<ids.size(); ++i){
				int a = ids[i];
				int b = closestMatch[a];
				cout<<" merging function "<<a<<"("<< locToG[a] <<") with function "<<b<<"("<< locToG[b]<<")"<<endl;
				Assert( b < a, "This is an invariant");
				int bnew = b;
				while(actualMatch.count(bnew) > 0){
					cout<<"function "<<bnew<<"("<< locToG[bnew]<<") had already been merged into "<<actualMatch[bnew]<<endl;
					int tmp = bnew;
					bnew = actualMatch[bnew];
					if(b != tmp){ actualMatch[b] = bnew; }
				}
				mergeFuncalls( locToG[bnew], locToG[a]);
				++nmerges;
				actualMatch[a] = bnew;
			}
		}
		totFuns += ids.size();
	}



	cout<<" merged "<<nmerges<<" calls"<<endl;
	cleanup(dag);
	tnbuilder.reset();
}


void DagFunctionInliner::immInline(BooleanDAG& dag){

	initLight(dag);

	int nfuns = 0;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				
		if(typeid(*dag[i]) == typeid(UFUN_node)){
			nfuns++;
		}
		timerclass& tc = optTimers[typeid(*dag[i]).name()];
		tc.restart();
		optAll.restart();
		bool_node* node = computeOptim(dag[i]);
		optAll.stop();		
		tc.stop();
		if(dag[i] != node){
				Dout(cout<<"replacing "<<dag[i]->get_name()<<" -> "<<node->get_name()<<endl );
				setTimestampChildren(dag[i]);
				replTime.restart();
				dag.replace(i, node, replTime2);
				replTime.stop();

		}
	}

	cout<<" added nodes = "<<newnodes.size()<<endl;


	cleanupTime.restart();
	cleanup(dag);
	cleanupTime.stop();
	tnbuilder.reset();
	cout<<" nfuns = "<<nfuns<<endl;

	if(mergeFunctions){	
		if(oldNfun > 0){
			if( (nfuns - oldNfun) > 3 && divFactor > (FRACTION + 1)){
				divFactor--;
				if( (nfuns - oldNfun) > 12 ){
					divFactor-= 4;					
				}
				if( (nfuns - oldNfun) > 40 ){
					divFactor-= 4;					
				}
				if(divFactor<(FRACTION + 1)){
					divFactor = (FRACTION + 1);
				}
				cout<<"reducing divFactor "<<divFactor<<endl;
			}
		}
		unifyTime.restart();
		unify();
		unifyTime.stop();
		expectedNFuns++;
	}

	oldNfun = nfuns;
	Dout( cout<<" AFTER PROCESS "<<endl );
	Dout(cout<<" end ElimFun "<<endl);
}

/*
extern map<string, pair<int, int> > sizes;
*/

void DagFunctionInliner::process(BooleanDAG& dag){
	cout<<" funmap has size " << functionMap.size() << endl;
	somethingChanged = true;
	{
		DagOptim optim(dag);
		optim.process(dag);
	}
	
	computeSpecialInputs();

	expectedNFuns = 2;
	timerclass everything("everything");

	everything.start();
	int inlin = 0;
	while(somethingChanged && dag.size() < 510000 && inlin < inlineAmnt){
		somethingChanged = false;
		cout<<inlin<<": inside the loop dag.size()=="<<dag.size()<<endl;
		immInline(dag);	
		//if(inlin==0){( dag.print(cout) );}
		++inlin;
	}
	cout<<" final dag.size()=="<<dag.size()<<endl;
	everything.stop();
	everything.print();
	cleanupTime.print();
	unifyTime.print();
	ufunAll.print();
	clonetime.print();
	optAll.print();

	for(map<string, timerclass>::iterator it = optTimers.begin(); it != optTimers.end(); ++it){
		cout<<"            "<<it->first;
		it->second.print();
	}

	replTime.print();
	replTime2.print();
	tnbuildTime.print();
	optimTime.print();
	/*
	for(map<string, pair<int, int> >::iterator it = sizes.begin(); it != sizes.end(); ++it){
		cout<<it->first<<" : "<< (it->second.second / it->second.first)<<"  "<<it->second.second<<"  "<<it->second.first<<endl;
	}
*/

	( cout<<" after all inlining dag.size()=="<<dag.size()<<endl);
	
	
	// dag.print(cout);
	{
		DagFunctionToAssertion makeAssert(dag, functionMap);
		makeAssert.process(dag);
	}
	
	cout<<" After everything: dag.size()=="<<dag.size()<<endl;	
}


