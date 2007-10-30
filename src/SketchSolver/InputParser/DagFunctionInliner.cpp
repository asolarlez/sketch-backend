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





void DagFunctionInliner::visit( UFUN_node& node ){	
	ufunAll.restart();
	string& name = node.get_ufname();
	if( functionMap.find(name) != functionMap.end() ){
		//cout<<" inlining "<<name<<endl;
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

int DagFunctionInliner::argsCompare(vector<bool_node*> arg1, vector<bool_node*> arg2){
	int rv = 0;
	Assert(arg1.size() == arg2.size(), "This can't be happening. It's an invariant. Something is very strange");
	for(int i=0; i<arg1.size(); ++i){
		if(arg1[i]->id != arg2[i]->id){
			rv++;
		}
	}
	return rv;
}



//Merges the function at id first with the function at id second. The resulting function will go to ID first.

void DagFunctionInliner::mergeFuncalls(int first, int second){
	cout<<" merging "<<first<<" and "<<second<<endl;
	UFUN_node* fun1 = dynamic_cast<UFUN_node*>(dag[first]);
	UFUN_node* fun2 = dynamic_cast<UFUN_node*>(dag[second]);

	vector<bool_node*>& args1 = fun1->multi_mother;
	vector<bool_node*>& args2 = fun2->multi_mother;

	vector<bool_node*> nargs;
	int szz1 = newnodes.size();
	bool_node* ecall2 = tnbuilder.get_exe_cond(fun2, *this);
	int szz2 = newnodes.size();


	for(int i=0; i<args1.size(); ++i){
		if(args1[i] == args2[i]){
			nargs.push_back(args1[i]);
		}else{
			ARRACC_node* an = new ARRACC_node();
			an->mother = ecall2;
			an->multi_mother.push_back(args1[i]);
			an->multi_mother.push_back(args2[i]);
			an->addToParents();
			this->addNode(an);
			nargs.push_back(an);
		}
	}

	UFUN_node* funnew = new UFUN_node(*fun1);
	funnew->multi_mother = nargs;
	funnew->children.clear();
	funnew->addToParents();
	dag.replace(fun1->id, funnew);
	dag.replace(fun2->id, funnew);

	


	dag[first] = funnew;

}



void DagFunctionInliner::unify(){
	initialize(dag);
	map<string, vector<vector<bool_node*> > > argLists;
	map<string, vector<vector<int> > > diffs;
	map<string, map<int, int> > lidToDagID;
	map<string, int> tot;
	int maxDif = -1;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
		if(typeid(*dag[i]) == typeid(UFUN_node)){
			UFUN_node* ufun = dynamic_cast<UFUN_node*>(dag[i]);
			string& name = ufun->get_ufname();
			//ufun->accept(cse);
			//cout<<cse.ccode<<endl;
			vector<vector<bool_node*> >& v = argLists[name];

			int id = v.size();
			
			vector<int> difRow;
			lidToDagID[name][id] = i;
			if(id==0){ tot[name] = 0; }
			for(int j=0; j<v.size(); j++){
				int sz = (ufun->multi_mother.size());
				maxDif = sz > maxDif ? ufun->multi_mother.size() : maxDif;
				int dif = argsCompare(v[j], ufun->multi_mother);
				tot[name] += dif;
				difRow.push_back(dif);
				//cout<<" ("<<id<<", "<<j<<")  "<<dif<<endl;
			}
			v.push_back(ufun->multi_mother);
			diffs[name].push_back(difRow);
		}
	}	

    

	for(map<string, map<int, int> >::iterator it = lidToDagID.begin(); it != lidToDagID.end(); ++it){
		string const & name = it->first;
		map<int, int>& locToG = it->second;
		vector<vector<int> >& ldifs = diffs[name];
		vector<vector<bool_node*> > & larglists = argLists[name];
		int sz = larglists.size();
		if(sz <= 1){ continue; }
		int avg = (tot[name]*2 / ((sz)*(sz-1))) + 1;
		cout<<"avg = "<<avg<<"   cutoff "<< ((avg*FRACTION)/divFactor) <<endl;
		vector<set<int> > eqClasses;
		vector<int>  eqClassRepresentative;
		int i=0;
		for(vector<vector<int> >::iterator rowIt = ldifs.begin(); rowIt != ldifs.end(); ++rowIt, ++i){
			vector<int>& row = *rowIt;	

			//In the loop below, we are going to look for the best possible match.
			//The criteria for best match is as follows:
			// 1. A match must have dif <= avg/3.
			// 2. The best match has the lowest dif.
			// 3. With two matches with identical dif, we pick the one that doesn't belong to any other equivalence class.
			// 4. If all matches with lowest diff belong to different equivalence classes, we join those equivalence classes.


			int lowestDif = maxDif;
			set<int> lowestDifRows;
			for(int j=0; j<row.size(); ++j){
				int dif = row[j];
				if(dif < (avg*FRACTION)/divFactor){
					if(dif < lowestDif){
						lowestDif = dif;
						lowestDifRows.clear();
						lowestDifRows.insert(j);
					}
					if(dif == lowestDif){
						lowestDifRows.insert(j);
					}
					cout<<" merged on  "<<dif<<endl;
				}else{
					if(dif <= (avg*9)/10){
						cout<<" almost but not "<<dif<<endl;

					}
				}
			}
			//For now I am taking a shortcut. I just merge with the first lowest 
			//match I see. I'll fix this latter, but I want to test this first. 
			//just to see how it works.
			if(lowestDifRows.size() > 0){
				int j = *lowestDifRows.begin();
				//search for the eqclass to which j belongs.
				//If it belongs to an eq class, merge i with the 
				//current representative of the eq class. 
				//otherwise, merge i with j and make them an equivalence class.
				int classID=-1;
				for(int t=0; t<eqClasses.size(); ++t){
					if(eqClasses[t].count(j)>0){
						classID = t;
						break;
					}
				}
				if(classID == -1){
					set<int> s;
					s.insert(i);
					s.insert(j);
					eqClasses.push_back(s);
					eqClassRepresentative.push_back(locToG[i]);
					mergeFuncalls(locToG[i], locToG[j]);
				}else{
					eqClasses[classID].insert(i);
					mergeFuncalls(eqClassRepresentative[classID], locToG[i]);
				}
				
			}		
		}

	}

	cleanup(dag);
	tnbuilder.reset();
}


void DagFunctionInliner::immInline(BooleanDAG& dag){

	initialize(dag);

	int nfuns = 0;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
				
		if(typeid(*dag[i]) == typeid(UFUN_node)){
			nfuns++;
		}

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
		unify();
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
	
	timerclass everything("everything");

	everything.start();
	int inlin = 0;
	while(somethingChanged && dag.size() < 210000 && inlin < inlineAmnt){
		somethingChanged = false;
		cout<<inlin<<": inside the loop dag.size()=="<<dag.size()<<endl;
		immInline(dag);	
		//if(inlin==0){( dag.print(cout) );}
		++inlin;
	}
	cout<<" final dag.size()=="<<dag.size()<<endl;
	everything.stop();
	everything.print();
	ufunAll.print();
	optAll.print();
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


