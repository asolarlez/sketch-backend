#pragma once
#include "NodeEvaluator.h"

class CounterexampleFinder :
	public NodeEvaluator
{
	vector<set<int> >  influences;
	vector<int> jumpids;
	void computeInfluences(){
		influences.clear();
		influences.resize(bdag.size());
		for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
			bool_node* cur = (*node_it);
			if(cur->type == bool_node::SRC){
				SRC_node* src = dynamic_cast<SRC_node*>(cur);	
				int oid = inputs->getId(src->get_name());
				influences[cur->id].insert( oid );
				if(jumpids.size() <= oid){
					jumpids.resize(oid+1);
				}
				jumpids[oid] = src->id;
			}else{
				if(cur->mother != NULL){
					set<int>& s = influences[cur->mother->id];
					influences[cur->id].insert(s.begin(), s.end());
				}
				if(cur->father != NULL){
					set<int>& s = influences[cur->father->id];
					influences[cur->id].insert(s.begin(), s.end());
				}
				arith_node* an = dynamic_cast<arith_node*>(cur);
				if(an != NULL){
					for(int i=0; i<an->multi_mother.size(); ++i){
						set<int>& s = influences[an->multi_mother[i]->id];
						influences[cur->id].insert(s.begin(), s.end());
					}
				}
			}
		}
	}
public:
	typedef enum {FOUND, NOTFOUND, UNSAT} Result;

	void init(VarStore& vs){
		inputs = &vs;
		computeInfluences();
	}

	Result searchLoop(int maxfails){
		int failcount = 0;
		failedHAssert = false;
		failedAssert = false;
		int bdsz = bdag.size();
		vector<bool_node*>& ctrls = bdag.getNodesByType(bool_node::CTRL);
		for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){		
			(*node_it)->accept(*this);

			if(failedHAssert){
				++failcount;
				if(failcount > maxfails){
					return NOTFOUND;
				}
				failedHAssert = false;
				ASSERT_node* an = dynamic_cast<ASSERT_node*>(*node_it);
				set<int>& inf = influences[an->id];
				set<int>::reverse_iterator it = inf.rbegin();
				if(it == inf.rend()){
					return UNSAT;
				}
				if( (*node_it)->mother->type == bool_node::EQ ){
					bool_node* eq = (*node_it)->mother;
					if(eq->mother->type==bool_node::SRC){
						int fv = this->getValue(eq->father);

						VarStore::objP& op = inputs->getObj(eq->mother->get_name());

						bool inrange = op.setValSafe(fv);
						if(!inrange){
							return UNSAT;
						}
						node_it = bdag.begin() + eq->mother->id;
						(*node_it)->accept(*this);
						continue;
					}
					if(eq->father->type==bool_node::SRC){
						int fv = this->getValue(eq->mother);
						bool inrange = inputs->getObj(eq->father->get_name()).setValSafe(fv);
						if(!inrange){
							return UNSAT;
						}
						node_it = bdag.begin() + eq->father->id;
						(*node_it)->accept(*this);
						continue;
					}
				}
				int jmp = bdsz;
				for(;it != inf.rend(); ++it){
					inputs->getObj(*it).makeRandom();
					int jid = jumpids[*it];
					if(jid < jmp){ jmp = jid; }
				}
				node_it = bdag.begin() + jmp;
				(*node_it)->accept(*this);
			}
		}
		return (failedAssert && !failedHAssert) ? FOUND : NOTFOUND;
	}

	CounterexampleFinder(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p):
	NodeEvaluator(functionMap_p, bdag_p)
	{
	}

	~CounterexampleFinder(void)
	{
	}
};

