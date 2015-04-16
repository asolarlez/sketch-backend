#pragma once
#include "NodeEvaluator.h"
#include "StringHTable.h"


class mybitset{
	int size;
	unsigned buf[];
public:
	mybitset(int sz):size(sz){
		memset(buf, 0, sz*sizeof(unsigned));
	}
	void insert(int v){
		int idx = v >> 5;
		int msk = 1 << (v & 0x1f);
		buf[idx] |= msk;
	}

	void print(ostream& os);

	void insert(mybitset* other){
		if(other==NULL){ return ; }
		Assert(size == other->size, "e;lkhy");
		unsigned* oth = other->buf;
		for(int i=0; i<size; ++i){
			buf[i] |= oth[i];
		}
	}
	friend mybitset* merge(Ostore<unsigned>& store, mybitset* a, mybitset* b);
	int next(int v){
		++v;
		int msz = size << 5;
		while(v < msz){
			int idx = v >> 5;
			int msk = 1 << (v & 0x1f);
			if((buf[idx] & msk) != 0){
				return v;
			}
			++v;
		}
		return -1;
	}
};


inline mybitset* mybitsetcreateLL(Ostore<unsigned>& store, int wsize){		
	mybitset* rv = new(store.newObj(wsize+1 )) mybitset(wsize);
	return rv;
}

inline mybitset* mybitsetcreate(Ostore<unsigned>& store, int sz){
	int wsize = sz > 0 ? (((sz-1)>>5)+1) : 0;	
	mybitset* rv = new(store.newObj(wsize+1 )) mybitset(wsize);
	return rv;
}

inline mybitset* merge(Ostore<unsigned>& store, mybitset* a, mybitset* b){
	if(b==NULL){
		return a;
	}
	if(a==NULL){
		return b;
	}
	Assert(a->size == b->size, "Clekn");
	unsigned* ba = a->buf;
	unsigned* bb = b->buf;
	int sz = a->size;
	for(int i=0; i<sz; ++i){
		if(ba[i] != (ba[i] | bb[i])){
			mybitset* rv = mybitsetcreateLL(store, sz);
			for(int j=0; j<sz; ++j){
				rv->buf[j] = ba[j] | bb[j];
			}
			return rv;
		}
	}
	return a;
}



class CounterexampleFinder :
	public NodeEvaluator
{
	Ostore<unsigned> store;
	vector<mybitset* >  influences;
	vector<int> jumpids;
	void computeInfluences(){
		influences.clear();
		influences.resize(bdag.size());
		int bssize = inputs->getIntsize();
		for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
			bool_node* cur = (*node_it);
			if(cur->type == bool_node::SRC){
				SRC_node* src = dynamic_cast<SRC_node*>(cur);	
				int oid = inputs->getId(src->get_name());
				mybitset* nb = mybitsetcreate(store, bssize);
				nb->insert( oid );
				// nb->print(cout);
				influences[cur->id] = nb;
				if(jumpids.size() <= oid){
					jumpids.resize(oid+1);
				}
				jumpids[oid] = src->id;
			}else{
				mybitset* res = NULL;
				if(cur->mother != NULL){
					res = influences[cur->mother->id];					
				}
				bool isFresh = false;
				if(cur->father != NULL){
					if(res == NULL){
						res = influences[cur->father->id];
					}else{
						mybitset* old = res;
						res = merge(store, res, influences[cur->father->id]);
						if(old != res){
							isFresh = true;
						}
					}
				}
				arith_node* an = dynamic_cast<arith_node*>(cur);
				if(an != NULL){
					for(int i=0; i<an->multi_mother.size(); ++i){
						if(res == NULL){
							res = influences[an->multi_mother[i]->id];
						}else{
							if(!isFresh){
								mybitset* old = res;
								res = merge(store, res, influences[an->multi_mother[i]->id]);
								if(old != res){
									isFresh = true;
								}
							}else{
								res->insert(influences[an->multi_mother[i]->id]);
							}
							
						}
					}
				}
				influences[cur->id] = res;
			}
		}
	}
public:
	typedef enum {FOUND, NOTFOUND, UNSAT} Result;
	const string* message;
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
				mybitset* inf = influences[an->id];
				if(inf == NULL){
					message = &(an->getMsg());
					return UNSAT;
				}
				int it = inf->next(-1);				
				if(it == -1){
					message = &(an->getMsg());
					return UNSAT;
				}
				if( (*node_it)->mother->type == bool_node::EQ ){
					bool_node* eq = (*node_it)->mother;
					if(eq->mother->type==bool_node::SRC){
						int fv = this->getValue(eq->father);

						VarStore::objP& op = inputs->getObj(eq->mother->get_name());

						bool inrange = op.setValSafe(fv);
						if(!inrange){
							message = &(an->getMsg());
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
							message = &(an->getMsg());
							return UNSAT;
						}
						node_it = bdag.begin() + eq->father->id;
						(*node_it)->accept(*this);
						continue;
					}
				}
				int jmp = bdsz;
				for(;it != -1; it = inf->next(it)){
					inputs->getObj(it).makeRandom();
					int jid = jumpids[it];
					if(jid < jmp){ jmp = jid; }
				}
				node_it = bdag.begin() + jmp;
				(*node_it)->accept(*this);
			}
			
			if(failedAssert){
        ASSERT_node* an = dynamic_cast<ASSERT_node*>(*node_it);
        message = &(an->getMsg());
        cout << (*message) << endl;
				//cout<<" FAILED BUT WONT REPORT "<<(*node_it)->lprint()<<endl;
				return FOUND;
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


