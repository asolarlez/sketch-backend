#include "ComplexInliner.h"
#include "DagFunctionInliner.h"
#include "DagFunctionToAssertion.h"

int FRACTION = 8;

ComplexInliner::ComplexInliner(BooleanDAG& p_dag, map<string, const BooleanDAG *> &p_functionMap, int p_inlineAmnt, bool p_mergeFunctions, FloatManager& fm):
dag(p_dag), 
DagOptim(p_dag, fm),
functionMap(p_functionMap),
optimTime(" optim "),
optAll(" opt all "),
unifyTime("unify time"),
clonetime(" clone time"),
inlineAmnt(p_inlineAmnt),
mergeFunctions(p_mergeFunctions),
divFactor(32),
oldNfun(-1)
{
	somethingChanged = false;
}

ComplexInliner::~ComplexInliner(void)
{
}



void collectSet(ARRACC_node* an, set<bool_node*>& s, int lev){
	s.insert(an);
	if(lev <= 0){ return; }
	for(int i=0; i<an->nargs(); ++i){
		bool_node* bn = an->arguments(i);
		if(typeid(*bn) == typeid(ARRACC_node)){
			collectSet(dynamic_cast<ARRACC_node*>(bn), s, lev-1);
		}else{
			s.insert(bn);
		}
	}
}

bool compareSet(ARRACC_node* an, set<bool_node*>& s, int lev){
	if(s.count(an) > 0){
		return true;
	}
	if(lev <= 0){ return false; }
	for(int i=0; i<an->nargs(); ++i){
		bool_node* bn = an->arguments(i);
		if(typeid(*bn) == typeid(ARRACC_node)){
			bool tmp = compareSet(dynamic_cast<ARRACC_node*>(bn), s, lev-1);
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
		ARRACC_node* an1 = dynamic_cast<ARRACC_node*>(bn1);
		ARRACC_node* an2 = dynamic_cast<ARRACC_node*>(bn2);
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


bool checkFunName(const string& name){

	return true;
}



int ComplexInliner::argsCompare(vector<bool_node*> arg1, bool_node::parent_iter arg2_beg, bool_node::parent_iter arg2_end ){
	int rv = 0;
	//Assert(arg1.size() == arg2.size(), "This can't be happening. It's an invariant. Something is very strange");
	bool_node::parent_iter arg2_it = arg2_beg;
	for(int i=0; i<arg1.size(); ++i, ++arg2_it){
		if(specialInputs.count(i) > 0){
			// cout<<i<<"=("<<arg1[i]->get_name()<<", "<<arg2[i]->get_name()<<")   ";
		}
		if(arg1[i]->id != (*arg2_it)->id){
			if(specialInputs.count(i) > 0){
				bool_node* a1 = arg1[i];
				bool_node* a2 = (*arg2_it);
				// cout<<"  "<<a1<<"  "<<a2<<endl;
				int t0 = staticCompare<equal_to<int> >(a1, 0, true);
				int t1 = staticCompare<equal_to<int> >(a2, 0, true);
				AbstractNodeValue& anv1 = anv[arg1[i]];
				AbstractNodeValue& anv2 = anv[(*arg2_it)];
				//anv1.print(cout);
				//anv2.print(cout);
				int dif = anv1.difference(anv2);
				// cout<<"------ dif ="<<dif<<endl;
				rv+= ( dif * 10 * specialInputs[i]  );
			}else{
				rv+= 0; // compareDifferent(arg1[i], arg2[i]);
			}
		}
	}
	//cout<<endl;
	return rv;
}



//Merges the function at id first with the function at id second. The resulting function will go to ID first.

void ComplexInliner::mergeFuncalls(int first, int second){
	// cout<<" merging "<<first<<" and "<<second<<endl;
	UFUN_node* fun1 = dynamic_cast<UFUN_node*>(dag[first]);
	UFUN_node* fun2 = dynamic_cast<UFUN_node*>(dag[second]);

	

	vector<bool_node*> nargs;
	bool_node* ecall2 = fun2->mother();

	auto args2_it = fun2->arg_begin();
	int i = 0;
	for (auto args1_it = fun1->arg_begin(); args1_it != fun1->arg_end(); ++args1_it, ++args2_it, ++i) {
		if(specialInputs.count(i) > 0){
			bool_node* arg1 = (*args1_it);
			bool_node* arg2 = (*args2_it);
			//cout<<"  "<<arg1<<"  "<<arg2<<endl;
			int t1 = staticCompare<equal_to<int> >(arg1, 1, true);
			int t2 = staticCompare<equal_to<int> >(arg2, 1, true);
			anv[arg1].print(cout);
			anv[arg2].print(cout);
			cout<<"------"<<endl;
			//cout<<i<<"=("<<args1[i]->get_name()<<", "<<args2[i]->get_name()<<")   ";
		}
		if((*args1_it) == (*args2_it)){
			nargs.push_back(*args1_it);
		}else{
			ARRACC_node* an = ARRACC_node::create(ecall2, (*args1_it), (*args2_it));
			
			an->addToParents();
			this->addNode(an);
			nargs.push_back(an);
		}
	}

	OR_node* on = OR_node::create();
	on->mother() = fun1->mother();
	on->father() = fun2->mother();
	on->addToParents();
	addNode(on);

	UFUN_node* funnew = UFUN_node::create(*fun1, false);
	if(fun1->dependent()){
		funnew->makeDependent();
	}
	funnew->mother() = on;
	copy(nargs.begin(), nargs.end(), funnew->arg_begin());
	
	funnew->children.clear();
	funnew->addToParents();

	dag.replace(fun1->id, funnew);
	dag.replace(fun2->id, funnew);


	dag[first] = funnew;

}

void ComplexInliner::computeSpecialInputs(){
	for(auto it = functionMap.begin(); it != functionMap.end(); ++it){
		const BooleanDAG* fun = it->second;
		auto inputs  = fun->getNodesByType(bool_node::SRC);
		for(int i=0; i<inputs.size(); ++i){	
			string fn = inputs[i]->get_name();
			if(fn.find("PC") != -1 ){
					specialInputs[i] = 50;
			}
			if(fn.find("count") != -1){
					specialInputs[i] = 5;
			}
		}
	}
}




void ComplexInliner::unify(){
	initLight(dag);
	vector<vector<bool_node*> >  argLists;
	vector<vector<int> >  diffs;
	map<int, int>  locToG;
	int tot = 0;
	int N = 0;
	map<int, vector<int> >  nfd;
	map<int, int>  closestMatch;
	int maxDif = -1;
	for(int i=0; i<dag.size() ; ++i ){
		// Get the code for this node.
        auto dag_i = dag[i];
		if(typeid(*dag_i) == typeid(UFUN_node)){
			UFUN_node* ufun = dynamic_cast<UFUN_node*>(dag[i]);
			const string& name = ufun->get_ufun_name();
			if(checkFunName(name)){
				vector<vector<bool_node*> >& v = argLists;	
				int id = v.size();
				vector<int> difRow;
				locToG[id] = i;
				int lowestDif = 100000;
				int lowestDifID = -1;
				for(int j=0; j<v.size(); j++){
					int dif = argsCompare(v[j], ufun->arg_begin(), ufun->arg_end());
					maxDif = dif > maxDif ? dif : maxDif;
					tot += dif;
					++N;
					if(dif < lowestDif){ 
						lowestDif = dif; 
						lowestDifID = j;
					}
					 // cout<<" ("<<id<<", "<<j<<")  "<<dif<<endl;
				}
				// cout<<"  id = "<<id<<" lowestDif ="<<lowestDif<<"avg dif="<< (id>0 ? tot / id : -1) <<" ldifID = "<<lowestDifID<<endl;
				closestMatch[id] = lowestDifID;
				nfd[lowestDif].push_back(id);
				v.push_back(vector<bool_node*>(ufun->arg_begin(), ufun->arg_end()));	
			}			
		}
	}



    int nmerges = 0;
	if(N == 0){ N = 1; }
	cout<<"  expectedNFuns="<<expectedNFuns<<" average dif = "<< (tot / N) <<endl;
	int totFuns=0;
	map<int, int> actualMatch;
	for(map<int, vector<int> >::reverse_iterator it = nfd.rbegin(); it != nfd.rend(); ++it){
		vector<int>& ids = it->second;
		cout<<" ldiff = "<<it->first<<" size = "<<ids.size()<<endl;
		if(totFuns >= expectedNFuns || it->first < 300){
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
}


void ComplexInliner::immInline(BooleanDAG& dag){
	map<string, map<string, string> > replaceMap;
    map<string, const BooleanDAG*> constfunmap;
    for(const auto& _it : functionMap) {
        constfunmap[_it.first] = _it.second;
    }
	DagFunctionInliner dfi(dag, constfunmap, replaceMap, floats, NULL, set<string>());
	dfi.process(dag);
	
	somethingChanged = dfi.changed();

	if(mergeFunctions){	
		if(oldNfun > 0){
			if( (dfi.nfuns() - oldNfun) > 3 && divFactor > (FRACTION + 1)){
				divFactor--;
				if( (dfi.nfuns() - oldNfun) > 12 ){
					divFactor-= 4;					
				}
				if( (dfi.nfuns() - oldNfun) > 40 ){
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

	//cout<<" after all"<<endl;
	oldNfun = dfi.nfuns();
	Dout( cout<<" AFTER PROCESS "<<endl );
	Dout(cout<<" end ElimFun "<<endl);
}


void ComplexInliner::process(BooleanDAG &dag){
	// cout<<" funmap has size " << function_map.size() << endl;
	somethingChanged = true;
	{
		DagOptim optim(dag, floats);
		optim.process(dag);
	}
	
	computeSpecialInputs();

	expectedNFuns = 2;
	timerclass everything("everything");

	everything.start();
	int inlin = 0;
	while(somethingChanged && dag.size() < 510000 && inlin < inlineAmnt){
		somethingChanged = false;
		if(inlin!=0){ cout<<inlin<<": inside the loop dag.size()=="<<dag.size()<<endl; }

		immInline(dag);	
		//if(inlin==0){( dag.print(cout) );}
		++inlin;
	}
	
	everything.stop();
	if(inlin>1){
		cout<<" final dag.size()=="<<dag.size()<<endl;
		cout<<"inlin = "<<inlin<<endl;
		everything.print();

		unifyTime.print();
		clonetime.print();
		optAll.print();		
		optimTime.print();
	}
	/*
	for(map<string, pair<int, int> >::iterator it = sizes.begin(); it != sizes.end(); ++it){
		cout<<it->first<<" : "<< (it->second.second / it->second.first)<<"  "<<it->second.second<<"  "<<it->second.first<<endl;
	}
*/
	
	// dag.print(cout);
	{
		DagFunctionToAssertion makeAssert(dag, functionMap, floats);
		makeAssert.process(dag);
	}

	if(inlin>1){
		cout<<" After everything: dag.size()=="<<dag.size()<<endl;	
	}
}