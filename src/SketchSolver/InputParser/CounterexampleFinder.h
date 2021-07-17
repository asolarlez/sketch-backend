#pragma once
#include <random>

#include "NodeEvaluator.h"
#include "StringHTable.h"

#include "BitSet.h"


//MODIFIES InputStore
void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype);

//MODIFIES InputStore
void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime=false);

void redeclareInputsAndAngelics(VarStore & inputStore, BooleanDAG* dag);

class File: public vector<VarStore*>
{
    static void growInputs(VarStore & inputStore, BooleanDAG* dag){
        dag->growInputIntSizes();
        redeclareInputs(inputStore, dag);
    }

public:
    enum Result {NO_FILE, DONE, MOREBITS};

    File(BooleanDAG* problem, const string& file, FloatManager& floats)
    {
        VarStore input_store;
        redeclareInputsAndAngelics(input_store, problem);
        vector<bool_node*>& inputs = problem->getNodesByType(bool_node::SRC);
        File::Result res = parseFile(file, floats, inputs, input_store);
        while (res == File::MOREBITS) {
            growInputs(input_store, problem);
            res = parseFile(file, floats, inputs, input_store);
        }
    }

    static bool parseLine(ifstream& in, FloatManager& floats, vector<bool_node*>& inputNodes, VarStore* inputs) {

        auto vsi = inputs->begin();
        VarStore::objP* arrit = NULL;
        VarStore::objP* prevArrit = NULL;
        bool inArray = false;

        int inputId = 0;

        char ch;
        in.get(ch);
        string line;
        while (ch == '#') {
            std::getline(in, line);
            in.get(ch);
        }


        int cur=0;
        bool neg = false;
        int depth = 0;
        bool hasCaptured = true;
        bool outOfRange = false;
        bool isFloat = false;
        double floatVal = 0.0;

        auto regval = [&]() {
            if (!hasCaptured) {

                if (isFloat) {
                    cur = floats.getIdx(floatVal);
                }

                if (depth == 0) {
                    //we just finished a number, and we are not inside an array.
                    outOfRange = !vsi->setValSafe(neg ? (-cur) : cur);
                    ++vsi;
                    ++inputId;
                }
                else {
                    if (!inArray) {
                        cerr << "Error parsing the input. Was expecting a line with the following format" << endl;
                        for (auto it = inputs->begin(); it != inputs->end(); ++it) {
                            auto type = it->otype != NULL? it->otype->str() : "scalar";
                            const auto isArr = it->arrSize() > 1;
                            if (isArr) {
                                cerr << "{" << type << " }  ";
                            }
                            else {
                                cerr << type << "  ";
                            }
                        }
                        cerr << endl;
                        cerr << "corresponding to inputs "<<endl;
                        for (auto it = inputs->begin(); it != inputs->end(); ++it) {
                            cerr << it->getName()<<"  ";
                        }
                        cerr << endl;
                        throw BasicError(string("file parsing error"), "name");

                    }
                    if (arrit == NULL) {
                        prevArrit->makeArr(prevArrit->index, prevArrit->index + 2);
                        arrit = prevArrit->next;
                        ((SRC_node*)inputNodes[inputId])->arrSz++;
                    }

                    //we just finished a number, and we are inside an array.
                    outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
                    prevArrit = arrit;
                    arrit = arrit->next;


                }
            }
            hasCaptured = true;
        };
        auto reset = [&]() {
            cur = 0;
            neg = false;
            isFloat = false;
        };

        while (ch != '\n') {
            switch (ch) {
                case '{': {
                    regval();
                    reset();
                    if (depth == 0) {
                        arrit = &(*vsi);
                        inArray = true;
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
                        inArray = false;
                        ++vsi;
                        ++inputId;
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
                        if (isFloat) {
                            floatVal = floatVal + ((double)(ch - '0') / cur);
                            cur = cur * 10;
                        }
                        else {
                            hasCaptured = false;
                            cur = cur * 10 + (ch - '0');
                        }
                    }
                    if (ch =='.') {
                        isFloat = true;
                        floatVal = (double)cur;
                        cur = 10;
                    }

            }
            if (outOfRange) {
                return false;
            }
            in.get(ch);
            if (in.eof()) {
                regval();
                return !outOfRange;
            }
        }
        regval();
        return !outOfRange;
    }

    Result parseFile(const string& fname, FloatManager& floats, vector<bool_node*>& inputNodes, VarStore inputs) {
        clear();
        ifstream file;
        file.open(fname);
        bool ok = true;

        if (!file.is_open() || file.fail()) {
            Assert(false, "File " << fname << " could not be opened!! file.is_open() = " << file.is_open() <<" file.fail() = " << file.fail());
            return NO_FILE;
        }

        while (!file.eof()) {
            VarStore* new_row = inputs.copy();
            try {
                ok = parseLine(file, floats, inputNodes, new_row);
                push_back(new_row);
            }
            catch (BasicError& e) {
                cerr << "Error parsing file " << fname << endl;
                throw e;
            }

            if (!ok) {
                file.close();
                return MOREBITS;
            }
            if (PARAMS->verbosity > 12) {
                new_row->printContent(cout);
            }
        }
        file.close();
        return DONE;
    }

    explicit File(File* to_copy)
    {
        for(int i = 0;i<to_copy->size();i++)
        {
            push_back(to_copy->at(i)->copy());
        }
    }

    File *sample_sub_file(int num_rows) {
        File* new_file = new File(this);
        shuffle(new_file->begin(), new_file->end(), std::mt19937(std::random_device()()));
        while(new_file->size() > num_rows)
        {
            new_file->pop_back();
        }
        return new_file;
    }
};

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
				for (int i = 0; i < node.nargs(); ++i) {
					nb->insert(influences[node.arguments(i)->id]);
				}


				influences[cur->id] = nb;				
				
			}else{
				BitSet* res = NULL;
				bool isFresh = false;

				{
					for(int i=0; i<cur->nparents(); ++i){
						if(res == NULL){
							res = influences[cur->get_parent(i)->id];
						}else{
							if(!isFresh){
								BitSet* old = res;
								res = merge(store, res, influences[cur->get_parent(i)->id]);
								if(old != res){
									isFresh = true;
								}
							}else{
								res->insert(influences[cur->get_parent(i)->id]);
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
/*
	bool parseLine(ifstream& in, FloatManager& floats, vector<bool_node*>& inputNodes) {

		auto vsi = inputs->begin();
		VarStore::objP* arrit = NULL;
		VarStore::objP* prevArrit = NULL;
		bool inArray = false;

		int inputId = 0;

		char ch;
		in.get(ch);
		string line;
		while (ch == '#') {
			std::getline(in, line);
			in.get(ch);
		}


		int cur=0;
		bool neg = false;
		int depth = 0;
		bool hasCaptured = true;
		bool outOfRange = false;
		bool isFloat = false;
		double floatVal = 0.0;

		auto regval = [&]() {
			if (!hasCaptured) {

				if (isFloat) {
					cur = floats.getIdx(floatVal);
				}

				if (depth == 0) {
					//we just finished a number, and we are not inside an array.
					outOfRange = !vsi->setValSafe(neg ? (-cur) : cur);
					++vsi;
					++inputId;
				}
				else {
					if (!inArray) {
						cerr << "Error parsing the input. Was expecting a line with the following format" << endl;
						for (auto it = inputs->begin(); it != inputs->end(); ++it) {
							auto type = it->otype != NULL? it->otype->str() : "scalar";
							const auto isArr = it->arrSize() > 1;
							if (isArr) {
								cerr << "{" << type << " }  ";
							}
							else {
								cerr << type << "  ";
							}
						}
						cerr << endl;
						cerr << "corresponding to inputs "<<endl;
						for (auto it = inputs->begin(); it != inputs->end(); ++it) {
							cerr << it->getName()<<"  ";
						}
						cerr << endl;
						throw BasicError(string("file parsing error"), "name");

					}
					if (arrit == NULL) {
						prevArrit->makeArr(prevArrit->index, prevArrit->index + 2);
						arrit = prevArrit->next;
						((SRC_node*)inputNodes[inputId])->arrSz++;
					}

					//we just finished a number, and we are inside an array.
					outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
					prevArrit = arrit;
					arrit = arrit->next;


				}
			}
			hasCaptured = true;
		};
		auto reset = [&]() {
			cur = 0;
			neg = false;
			isFloat = false;
		};

		while (ch != '\n') {
			switch (ch) {
			case '{': {
				regval();
				reset();
				if (depth == 0) {
					arrit = &(*vsi);
					inArray = true;
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
					inArray = false;
					++vsi;
					++inputId;
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
					if (isFloat) {
						floatVal = floatVal + ((double)(ch - '0') / cur);
						cur = cur * 10;
					}
					else {
						hasCaptured = false;
						cur = cur * 10 + (ch - '0');
					}
				}
				if (ch =='.') {
					isFloat = true;
					floatVal = (double)cur;
					cur = 10;
				}

			}
			if (outOfRange) {
				return false;
			}
			in.get(ch);
			if (in.eof()) {
				regval();
				return !outOfRange;
			}
		}
		regval();
		return !outOfRange;
	}
*/
    bool parseLine(VarStore* var_store)
    {
        *inputs = *var_store;
        return true;
    }
	Result fromFile(File* file, FloatManager& floats, vector<bool_node*>& inputNodes) {
//		ifstream file;
//		file.open(fname);
		bool ok = true;
//
//		if (!file.is_open() || file.fail()) {
//			Assert(false, "File " << fname << " could not be opened!! file.is_open() = " << file.is_open() <<" file.fail() = " << file.fail());
//			return UNSAT;
//		}

		for(int i = 0;i<file->size();i++) {
//			try {
				ok = parseLine(file->at(i));
//			}
//			catch (BasicError& e) {
//				cerr << "Error parsing file " << fname << endl;
//				throw e;
//			}
			
			if (!ok) {
//				file.close();
				return MOREBITS;
			}
			if (PARAMS->verbosity > 12) {
				inputs->printContent(cout);
			}
			bool rv = this->run(*inputs);
			if (rv) {
				return FOUND;
			}
		}
//		file.close();
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
				if( (*node_it)->mother()->type == bool_node::EQ ){
					bool_node* eq = (*node_it)->mother();
					if(eq->mother()->type==bool_node::SRC){
						int fv = this->getValue(eq->father());

						VarStore::objP& op = inputs->getObj(eq->mother()->get_name());

						bool inrange = op.setValSafe(fv);
						if(!inrange){
							message = &(an->getMsg());
							return UNSAT;
						}
						node_it = bdag.begin() + eq->mother()->id;
						(*node_it)->accept(*this);
						continue;
					}
					if(eq->father()->type==bool_node::SRC){
						int fv = this->getValue(eq->mother());
						bool inrange = inputs->getObj(eq->father()->get_name()).setValSafe(fv);
						if(!inrange){
							message = &(an->getMsg());
							return UNSAT;
						}
						node_it = bdag.begin() + eq->father()->id;
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


