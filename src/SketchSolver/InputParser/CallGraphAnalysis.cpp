#include "CallGraphAnalysis.h"
#include "DagFunctionInliner.h"

void CallGraph::directInlining(BooleanDAG* root, map<string, BooleanDAG*>& funmap){
		vector<vector<BooleanDAG*> > scc;
		computeSCC(root, scc);
		vector<string> nocycles;
		vector<string> vlist;
		for(int i=0; i<scc.size(); ++i){
			if(scc[i].size()==1){
				nocycles.push_back(scc[i][0]->get_name());
				vlist.push_back(scc[i][0]->get_name());
			}else{
				string lastName = "";
				for(int j=0; j<scc[i].size(); j++){
					const string& tmpname = scc[i][j]->get_name();
					if(tmpname != lastName){
						vlist.push_back(tmpname);	
					}
					lastName = tmpname;
				}
			}
		}
		for(vector<string>::reverse_iterator it = vlist.rbegin(); it!= vlist.rend(); ++it){
			BooleanDAG* dagToOptim = funmap[*it];
			InclusiveInliner ict;
			map<string, map<string, string> > replaceMap;
			DagFunctionInliner fi(*(dagToOptim), funmap, replaceMap, NULL, false, &ict);
			for(int i=0; i<nocycles.size(); ++i){
				if(nocycles[i] != dagToOptim->get_name()){
					ict.addFunToInline(nocycles[i]);
				}
			}
			{int i=0;
			do{
				++i;
				fi.process(*dagToOptim);
			}while(fi.changed());}
		}
		{
			BooleanDAG* dagToOptim = root;
			InclusiveInliner ict;
			map<string, map<string, string> > replaceMap;
			DagFunctionInliner fi(*(dagToOptim), funmap, replaceMap, NULL, false, &ict);
			for(int i=0; i<nocycles.size(); ++i){
				ict.addFunToInline(nocycles[i]);
			}
			int i=0;
			do{
				++i;
				fi.process(*dagToOptim);
			}while(fi.changed());
		}
	}

CallGraphAnalysis::CallGraphAnalysis(void)
{
}

CallGraphAnalysis::~CallGraphAnalysis(void)
{
}


void CallGraphAnalysis::populateCG(BooleanDAG& dag, map<string, BooleanDAG*>& funMap, CallGraph& cg){
	set<BooleanDAG*> seen;
	stack<BooleanDAG*> tosee;	
	tosee.push(&dag);
	seen.insert(&dag);
	while(!tosee.empty()){		
		BooleanDAG* cur = tosee.top(); tosee.pop();		
		for(BooleanDAG::iterator it = cur->begin(); it != cur->end(); ++it){
			if(typeid(**it) == typeid(UFUN_node)){
				UFUN_node* node = dynamic_cast<UFUN_node*>(*it);
				if(funMap.count(node->get_ufname())>0){
					BooleanDAG* callee=funMap[node->get_ufname()];
					cg.addEdge(cur, callee, node);
					if(seen.count(callee)==0){
						tosee.push(callee);
						seen.insert(callee);
					}
				}
			}
		}
	}
}


void CallGraphAnalysis::printCG(BooleanDAG& dag, map<string, BooleanDAG*>& funMap){
		CallGraph cg;
		populateCG(dag, funMap, cg);
}

void CallGraphAnalysis::process(BooleanDAG& dag, map<string, BooleanDAG*>& funMap){

	map<string, BooleanDAG*>& tmpmap = funMap;
	/*
	for(map<string, BooleanDAG*>::iterator it = funMap.begin(); it != funMap.end(); ++it){
		tmpmap[it->first] = it->second->clone();
	}
	*/
	
	{
		CallGraph cg;
		populateCG(dag, tmpmap, cg);
		cg.directInlining(&dag, tmpmap);
	}
	{
		cout<<"second stage"<<endl;
		CallGraph cg;
		populateCG(dag, tmpmap, cg);
	}
}

