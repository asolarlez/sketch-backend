#pragma once
#include "NodeEvaluator.h"
#include "StringHTable.h"

#include "BitSet.h"


class CounterexampleFinder :
	public NodeEvaluator
{
	float sparseArray;
	Ostore<unsigned> store;
	vector<BitSet* >  influences;
	vector<int> jumpids;
	void computeInfluences(){
		influences.clear();
		influences.resize(bdag.size());
		int bssize = inputs->getIntsize();
		map<string, vector<UFUN_node*> > ufmap;
		for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
			bool_node* cur = (*node_it);
			if(cur->type == bool_node::SRC){
				SRC_node* src = dynamic_cast<SRC_node*>(cur);	
				int oid = inputs->getId(src->get_name());
				BitSet* nb = mybitsetcreate(store, bssize);
				nb->insert( oid );
				// nb->print(cout);
				influences[cur->id] = nb;
				if(jumpids.size() <= oid){
					jumpids.resize(oid+1);
				}
				jumpids[oid] = src->id;
			}else if (cur->type == bool_node::UFUN) {
				UFUN_node& node = *((UFUN_node*)cur);
				string uname = node.get_ufname();
				vector<UFUN_node*>& uv = ufmap[uname];

				const string& tuple_name = node.getTupleName();

				Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
				int size = tuple_type->actSize;
				BitSet* nb = mybitsetcreate(store, bssize);
				uv.push_back(&node);

				for (int tt = 0; tt < uv.size(); ++tt) {
					UFUN_node* ufn = uv[tt];
					for (int j = 0; j < size; j++) {
						stringstream sstr;
						sstr << ufn->get_ufname() << "_" << ufn->get_uniquefid() << "_" << j;
						int oid = inputs->getId(sstr.str());
						nb->insert(oid);
						if (ufn == &node) {
							if (jumpids.size() <= oid) {
								jumpids.resize(oid + 1);
							}
							jumpids[oid] = cur->id;
						}						
					}
				}				
				for (int i = 0; i < node.multi_mother.size(); ++i) {
					nb->insert(influences[node.multi_mother[i]->id]);
				}


				influences[cur->id] = nb;				
				
			}else{
				BitSet* res = NULL;
				if(cur->mother != NULL){
					res = influences[cur->mother->id];					
				}
				bool isFresh = false;
				if(cur->father != NULL){
					if(res == NULL){
						res = influences[cur->father->id];
					}else{
						BitSet* old = res;
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
								BitSet* old = res;
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
	typedef enum {FOUND, NOTFOUND, UNSAT, MOREBITS} Result;
	const string* message;
	void init(VarStore& vs){
		inputs = &vs;
		computeInfluences();
	}


	bool parseLine(ifstream& in) {

		auto vsi = inputs->begin();
		VarStore::objP* arrit = NULL;

		char ch;
		in.get(ch);
		int cur=0;
		bool neg = false;
		int depth = 0;
		bool hasCaptured = true;
		bool outOfRange = false;

		auto regval = [&]() {
			if (!hasCaptured) {

				if (depth == 0) {
					//we just finished a number, and we are not inside an array.
					outOfRange = !vsi->setValSafe(neg ? (-cur) : cur);
					++vsi;
				}
				else {
					//we just finished a number, and we are inside an array.
					outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
					arrit = arrit->next;
				}
			}
			hasCaptured = true;
		};
		auto reset = [&]() {
			cur = 0;
			neg = false;
		};

		while (ch != '\n') {
			switch (ch) {
			case '{': {
				regval();
				reset();
				if (depth == 0) {
					arrit = &(*vsi);
				}
				depth++;
				break;
			}
			case '}': {
				regval();
				reset();
				depth--;
				if (depth == 0) {
					while (arrit != NULL) {
						arrit->setValSafe(0);
						arrit = arrit->next;
					}
					++vsi;
				}
				break;
			}
			case ' ': {
				regval();
				reset();
				break;
			}
			case ',': {
				regval();
				reset();
				break;
			}
			case '-': {
				neg = true;
				break;
			}
			default:
				if (ch >= '0' && ch <= '9') {
					hasCaptured = false;
					cur = cur * 10 + (ch - '0');
				}

			}
			if (outOfRange) {				
				return false;
			}
			in.get(ch);
			if (in.eof()) {
				regval();
				return true;
			}
		}
		regval();
		return true;
	}


	Result fromFile(const string& fname) {
		ifstream file(fname);
		bool ok = true;
		while (!file.eof()) {
			ok = parseLine(file);	
			if (!ok) {
				file.close();
				return MOREBITS;
			}
			inputs->printContent(cout);
			bool rv = this->run(*inputs);
			if (rv) {
				return FOUND;
			}
		}
		file.close();
		return UNSAT;
	}


	Result searchLoop(int maxfails){
		int failcount = 0;
		funargs.clear();
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
				BitSet* inf = influences[an->id];
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
					if(sparseArray > 0.000001){
						inputs->getObj(it).makeRandom(sparseArray);
					}else{
						inputs->getObj(it).makeRandom();
					}
					int jid = jumpids[it];
					if(jid < jmp){ jmp = jid; }
				}

				for (auto funit = funargs.begin(); funit != funargs.end(); ++funit) {
					vector<pair<int, vector<int> > >& args = funit->second;
					for (auto argit = args.begin(); argit != args.end(); ++argit) {
						if (argit->first >= jmp) {
							args.resize(argit - args.begin());
							break;
						}
					}
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

	CounterexampleFinder(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, float sparseArray_p, FloatManager& _floats):
	NodeEvaluator(functionMap_p, bdag_p, _floats), sparseArray(sparseArray_p)
	{
	}

	~CounterexampleFinder(void)
	{
		
	}
};


