#pragma once

#include "BooleanDAG.h"
#include <stack>
#include <set>
#include <map>
#include <vector>
#include <algorithm>

using namespace std;

class Gnode{
public:
	vector<BooleanDAG*> children;
	vector<BooleanDAG*> parents;
	vector<BooleanDAG*>& getParents(){ return parents; }
	vector<BooleanDAG*>& getChildren(){ return children; }
	bool sucContains(BooleanDAG* d){
		return find(children.begin(), children.end(), d) != children.end();
	}
};

class CallGraph{

	map<BooleanDAG*, Gnode> gnodes;
	
public:
	void addEdge(BooleanDAG* caller, BooleanDAG* callee, UFUN_node* node){
		cout<<caller->get_name()<<" -> "<<callee->get_name()<<endl;
		gnodes[caller].children.push_back(callee);
		gnodes[callee].parents.push_back(caller);
	}

	template<typename T>
	void dfs(Gnode& gn, T nextFun, set<BooleanDAG*>& visited, vector<BooleanDAG*>& result){
		vector<BooleanDAG*>& tmp = (gn.*nextFun)(); 
		for(int i=0; i<tmp.size(); ++i){
			if(visited.count(tmp[i])==0){
				visited.insert(tmp[i]);
				dfs(gnodes[tmp[i]], nextFun, visited, result);
				result.push_back(tmp[i]);
			}
		}
	}

	template<typename T>
	void dfs(BooleanDAG* n, T nextFun, set<BooleanDAG*>& visited, vector<BooleanDAG*>& result){
		visited.insert(n);
		dfs(gnodes[n], nextFun, visited, result);
		result.push_back(n);
	}

	void computeSCC(BooleanDAG* root, vector<vector<BooleanDAG*> >& out ){
		
		vector<BooleanDAG*> result;
		{
			set<BooleanDAG*> visited;
			dfs(root, &Gnode::getChildren, visited, result);
		}
		{
			set<BooleanDAG*> visited;
			for(int i=result.size()-1; i>=0; --i){
				vector<BooleanDAG*>	tmp;
				dfs(result[i], &Gnode::getParents, visited, tmp);
				if(tmp.size()==1){
					if(gnodes[result[i]].sucContains(result[i])){
						tmp.push_back(result[i]);
					}
				}
				out.push_back(tmp);
			}
		}
	}

	void directInlining(BooleanDAG* root, map<string, BooleanDAG*>& funMap);
};

class CallGraphAnalysis
{
public:
	CallGraphAnalysis(void);
	~CallGraphAnalysis(void);

	void process(BooleanDAG& dag, map<string, BooleanDAG*>& funMap);
	void populateCG(BooleanDAG& dag, map<string, BooleanDAG*>& funMap, CallGraph& cg);
	void printCG(BooleanDAG& dag, map<string, BooleanDAG*>& funMap);
};
