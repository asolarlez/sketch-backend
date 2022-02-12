#ifndef VARSTORE_H
#define VARSTORE_H

#include <utility>
#include <vector>
#include <string>
#include <map>
#include "BasicError.h"
#include "SynthInSolver.h"

using namespace std;


template<typename T>
int intFromBV(T& bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){
        int the_bit = bv[start + i];
        assert(the_bit == 0 || the_bit == 1 || the_bit == -1);
		if( the_bit > 0){
			nval += t;	
		}
		t = t*2;
	}
	return nval;
}

class InliningTree;
class BooleanDagUtility;
// VarStore -- Keeps the mapping of node in the DAG vs its value.
class VarStore{
private:
	

public:
	class objP{

        void getArrRec(vector<pair<int, int> >* ret) const
        {
            assert(get_is_array());
            ret->push_back(getIntAndSize());
            if(next != nullptr && next->defined)
            {
                next->getArrRec(ret);
            }
        }
        bool defined = false;
        bool is_array;
        const string original_name;
        string source_dag_name;
        bool_node::Type type = bool_node::NO_TYPE;
	public:
		string name;

		objP* next;
		OutType* otype;
		int index;
		vector<int> vals;
		bool isNeg;

		OutType* getOtype() const
        {
		    return otype;
        }

		int get_size() const
        {
		    return globalSize();
        }

		string getName() const
		{
			return name;
		}

		~objP(){
            clear();
		}

        void clear()
        {
            vals.clear();
		    defined = false;
            if(next != nullptr) {delete next; next = nullptr;};
        }

		objP(string  nm, int size, OutType* _otype, bool_node::Type _type, const string _original_name = "", const string _source_dag_name = ""):
        name(std::move(nm)),vals(size),otype(_otype), type(_type), isNeg(false), index(0), next(nullptr), defined(true), original_name(_original_name), source_dag_name(_source_dag_name){
		    assert(_otype != nullptr);
            if(_otype == OutType::INT_ARR || _otype == OutType::BOOL_ARR || _otype == OutType::FLOAT_ARR) {
                is_array = true;
                assert(_otype->isArr);
            }
            else
            {
                is_array = false;
                assert(!_otype->isArr);
            }
            assert(source_dag_name != "program_lvl0__id26__id38");
		}

        objP(const objP& old):
        vals(old.vals), name(old.name), original_name(old.original_name),
        source_dag_name(old.source_dag_name), otype(old.otype), type(old.type),
        isNeg(old.isNeg), index(old.index), defined(old.defined), is_array(old.is_array){
		    if(old.next != nullptr){
                next=new objP(*old.next);
            }
            else{next=nullptr;}
        }

        const string& get_original_name() const {
            if(type == bool_node::CTRL)
            AssertDebug(!original_name.empty(), "check why this fails and act accordingly.")
            return original_name;
        }

        const string& get_source_dag_name() const {
            if(type == bool_node::CTRL)
            AssertDebug(!source_dag_name.empty(), "check why this fails and act accordingly.")
            return source_dag_name;
        }

		objP operator=(const objP& old){
            return objP(old);
		}

        bool operator == (const objP& other) const
        {
            const bool debug = false;
            if(other.vals.size() != vals.size())
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            for(int i = 0;i<vals.size();i++) {
                if (vals[i] != other.vals[i]) {
                    if(vals[i] == 0 && other.vals[i] == -1) {
                        //TODO: fix this. Artifact from CEGISSolver. -1 interpreted as 0 in int intFromBV(T& bv, int start, int nbits)
                    }
                    else {
                        if (debug) {
                            cout << "return false" << endl;
                            AssertDebug(false, "not eq");
                        }
                        return false;
                    }
                }
            }
            if(name != other.name)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(original_name != other.original_name)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(source_dag_name != other.source_dag_name)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(otype != other.otype)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(type != other.type)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(isNeg!= other.isNeg)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(index != other.index)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(defined != other.defined)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(is_array != other.is_array)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if((next == nullptr) != (other.next == nullptr))
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(next != nullptr)
            {
                bool ret = (*next == *other.next);
                if(debug) {
                    if (!ret) {
                        cout << "return false" << endl;
                        AssertDebug(false, "not eq");
                    }
                    else{
                        cout << "return true" << endl;
                    }
                }
                return ret;
            }
            if(debug) {
                cout << "return true" << endl;
            }
            return true;
        }

		void makeArr(int start, int end){
            assert(is_array);
			Assert(start < end, "Empty arr");
			index = start;
			if(start+1 < end){
				if(next == nullptr){
					next = new objP(name, vals.size(), otype, type);
				}				
				next->makeArr(start+1, end);
			}else{
				if(next != nullptr){
					delete next;
					next = nullptr;
				}
			}
		}
		int arrSize(){ assert(is_array); if(next==nullptr){ return 1; }else{ return next->arrSize() + 1;  } }
		int element_size() const { return vals.size(); }
		int globalSize() const { if(next == nullptr){ return element_size();} return next->globalSize() + element_size(); }
		int resize(int n){ int x=0; vals.resize(n); if(next != nullptr){ x=next->resize(n); } return x+n; }
		objP* setBit(size_t i, int val){ 
			if(i<vals.size()){ 
				vals[i] = val; 
				return this;
			}else{ 
				Assert(next != nullptr, "bad bad");
				return next->setBit(i-vals.size(), val);
			}
		}
		
		int getInt() const{
			int t = intFromBV(vals, 0, vals.size());
			return isNeg? -t : t; 
		}

        pair<int, int> getIntAndSize() const
        {
            return make_pair(getInt(), (int)vals.size());
        }

		int getInt(int idx) const{
            assert(!is_array);
			if(this->index==idx){
				return getInt();
			}else{
				if(next != nullptr){ return next->getInt(idx); }
                else {return -1;}
			}
			Assert(false,"Control shouldn't reach here");
		}
		vector<pair<int, int> >* getArr() const
        {
            assert(is_array);
		    vector<pair<int, int> >* ret = new vector<pair<int, int> >();
		    getArrRec(ret);
		    return ret;
        }

        void setArr(vector<int> *arr) {
            assert(is_array);
            objP* at = this;
            for(int i = 0;i<arr->size();i++)
            {
                assert(at != nullptr && at->defined);
                at->setVal(arr->at(i));
                at = at->next;
            }
        }

		///Return false if objP did not have enough bits to be made equal to v.
		bool setValSafe(int v) {
			if(v<0){
				v = -v;
				isNeg = true;
			}
			{
				size_t t = vals.size();
				vals.clear();				
				while(v != 0){
					vals.push_back(v&1);
					v = v >> 1;
					if(vals.size()==t && v != 0){
						return false;
					}
				}
				if(t > vals.size()){
					vals.resize(t, 0);
				}
				return true;
			}
		}

		void setVal(size_t v){
			if(v<0){
				v = -v;
				isNeg = true;
			}
			{
				size_t t = vals.size();
				vals.clear();
				while(v != 0){
					vals.push_back(v&1);
					v = v >> 1;
				}
				if(t > vals.size()){
					vals.resize(t, 0);
				}
			}
		}
		void printBit(ostream& out) const{			
			for(size_t i=0; i<vals.size(); ++i){
				out<<(vals[i]==1?1:0);
			}
			if(next!= nullptr){ out<<"|"; next->printBit(out); }
		}
		void printContent(ostream& out) const{			
			out << getInt();
			if(next!= nullptr){ out<<"|"; next->printContent(out); }
		}

		bool increment(){
			for(size_t i=0; i<vals.size(); ++i){
				if(vals[i]==-1){
					vals[i] = 1;
					return true;
				}else{
					vals[i] = -1;
				}				
			}
			if(next != nullptr){
				return next->increment();
			}else{
				return false;
			}
		}

		/**
		If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
		*/
		void makeRandom(float sparseDeg, int n=10){
			int P  = 10000;
			int q = P*sparseDeg;

			if(n > 0 || (rand() % P) < q ){
				for(size_t i=0; i<vals.size(); ++i){
					vals[i] = (rand() & 0x3) > 0? -1 : 1;
				}
			}else{
				for(size_t i=0; i<vals.size(); ++i){
					vals[i] = -1 ;
				}
			}
			if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
		}

		void makeRandom(){/* Bias towards zeros */
			for(size_t i=0; i<vals.size(); ++i){
				vals[i] = (rand() & 0x3) > 0? -1 : 1;
			}
			if(next!= nullptr){ next->makeRandom(); }
		}
		void zeroOut(){/* Bias towards zeros */
			for(size_t i=0; i<vals.size(); ++i){
				vals[i] = -1;
			}
			if(next!= nullptr){ next->zeroOut(); }
		}

        bool get_is_array() const {
            return is_array;
        }

        void rename(const string &new_name, const string &subdag_name) {
            name = new_name;
            source_dag_name = subdag_name;
            assert(source_dag_name != "sketch_main__Wrapper__id27__id29__id30__id41");
        }

        bool_node::Type get_type() const {
            assert(type != bool_node::NO_TYPE);
            return type;
        }
    };

private:
	vector<objP> objs;
	map<string, int> index;
    map<string, map<string, string> > var_name_to_dag_name_to_name;
	int bitsize = 0;

    InliningTree* inlining_tree = nullptr;

    void insert_name_in_original_name_to_dag_name_to_name(string name, string original_name, string source_dag_name)
    {
        if(original_name == "declareInput()") {
            original_name += "___"+name;
        } else if(original_name == "to_var_store()") {
            original_name += "___"+name;
        }
        if(var_name_to_dag_name_to_name.find(original_name) == var_name_to_dag_name_to_name.end()) {
            var_name_to_dag_name_to_name[original_name] = map<string, string>();
        }
        else {
            AssertDebug(
                    var_name_to_dag_name_to_name[original_name].find(source_dag_name) ==
                    var_name_to_dag_name_to_name[original_name].end(),
                    "dag name should be unique for every original name.")
        }
        var_name_to_dag_name_to_name[original_name][source_dag_name] = name;
    }


public:
    void rename(const string &original_name, const string& new_source_dag, const string &new_name, InliningTree* inlining_tree);



    map<string, SynthInSolver*> synths;
    map<string, string> synthouts;

    InliningTree* get_inlining_tree() const
    {
        return inlining_tree;
    }

	void clear()
	{
//		for(auto it:objs)
//		{
//            it.clear();
//		}

        objs.clear();
        index.clear();

        Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
        Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

        delete this;
	}

    int size() const
    {
        return objs.size();
    }

    VarStore() = default;

    VarStore(InliningTree* _inlining_tree);

    VarStore(const VarStore& to_copy);

    VarStore operator = (const VarStore& other) const {
        return VarStore(other);
    }

	VarStore* clone() const {
        return new VarStore(*this);
    }

    auto begin()const { return objs.begin();}
	auto end()const{ return objs.end(); }
    auto begin() { return objs.begin();}
    auto end(){ return objs.end(); }

	void makeRandom(float sparseArray){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].makeRandom(sparseArray);
		}
	}
	void makeRandom(){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].makeRandom();
		}
	}
	void zeroOut(){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].zeroOut();
		}
	}
	bool increment(const string& name){
		// cout<<"Upgraded "<<name<<" ";
		int idx = getId(name);
		
		objP& tmp = objs[idx];
		bool rv = tmp.increment();
		// tmp.printContent(cout);
		//cout<<endl;
		return rv;
	}

	void insertObj(const string& name, int idx, const objP obj)
    {
	    AssertDebug(index.find(name) == index.end(), name + " should not be present in index.");
	    AssertDebug(idx == objs.size(), "idx, " + std::to_string(idx) + " should be the same as objs.size() = " + std::to_string(objs.size()) + ".");
	    objs.push_back(obj);
	    index[name] = idx;
        insert_name_in_original_name_to_dag_name_to_name(obj.name, obj.get_original_name(), obj.get_source_dag_name());
    }

	void newArr(const string& name, int nbits, int arrsz, OutType* otype, bool_node::Type type){
		Assert(index.count(name)==0, name<<": This array already existed!!");
		int begidx = objs.size();
		index[name] = begidx;
		objs.emplace_back(name, nbits, otype, type);
		objs[begidx].makeArr(0, arrsz);
		bitsize += nbits*arrsz;
        assert(objs[begidx].get_is_array());
	}

    void setArr(const string& name, vector<int>& arr) {
	    Assert(index.find(name) != index.end(), name + " not found.");
        objs[index[name]].setArr(&arr);
        assert(objs[index[name]].get_is_array());
	}

	void newVar(const string& name, int nbits, OutType* otype, bool_node::Type type, string original_name, string source_dag_name){
        if(contains(name)) {
            auto obj = getObjConst(name);
            assert(obj.getName() == name);
            assert(obj.get_size() == nbits && obj.element_size() == nbits);
            assert(obj.getOtype() == otype);
            assert(obj.get_original_name() == original_name);
            assert(obj.get_source_dag_name() == source_dag_name);
        }
        else {
            Assert(index.count(name) == 0, name << ": This variable already existed!!");
            insert_name_in_original_name_to_dag_name_to_name(name, original_name, source_dag_name);
            int begidx = objs.size();
            objs.emplace_back(objP(name, nbits, otype, type, original_name, source_dag_name));
            index[name] = begidx;
            bitsize += nbits;
        }
        assert(!objs[index[name]].get_is_array());
	}

	void setVarVal(const string& name, int val, OutType* otype, bool_node::Type type){
        AssertDebug(contains(name), "IF THIS FAILS, REWRITE THIS FUNCTION TO USE newVar first.");
		int idx;
		if(index.count(name)!=0){
			idx = getId(name);
		}else{
            AssertDebug(false, "check previous assert.");
			objs.emplace_back(objP(name, 5, otype, type));
			idx = objs.size()-1;
      		index[name] = idx;

		}
        if(otype == OutType::BOOL)
        {
            assert(val == 0 || val == 1);
        }
		objs[idx].setVal(val);
        assert(!objs[idx].get_is_array());
	}

	void resizeVar(const string& name, int size){
		int idx = getId(name);
		{
			objP& tmp = objs[idx];
			bitsize -= tmp.globalSize();
			int x = tmp.resize(size);
			bitsize += x;
		}
	}
	void resizeArr(const string& name, int arrSize){
		int idx = getId(name);
		{
			objP& tmp = objs[idx];
            assert(tmp.get_is_array());
			bitsize -= tmp.globalSize();
			tmp.makeArr(0, arrSize);
            assert(arrSize*tmp.element_size() == tmp.get_size());
			bitsize += arrSize*tmp.element_size();
		}
	}
	int getBitsize() const{
		return bitsize;
	}
	int getIntsize(){
		return objs.size();
	}
	void printBrief(ostream& out) const{
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].printBit(out);
		}
	}
	void finalizeSynthOutputs() {
		synthouts.clear();
		for (auto sit = synths.begin(); sit != synths.end(); ++sit) {
			stringstream ss;
			sit->second->print(ss);
			synthouts[sit->first] = ss.str();
		}
	}
	void printContent(ostream& out) const{
		for(size_t i=0; i<objs.size(); ++i){
			out << objs[i].name << ":";
			objs[i].printContent(out);
			out << endl;
		}
		for (auto sit = synths.begin(); sit != synths.end(); ++sit) {
			//out << sit->first << ":";
			sit->second->print(out);
			//out << endl;
		}
	}

	void parseContent(istream& in){
		while(in){
			string t;
			in>>t;
			if(t.size()==0){ break; }

			int s  = t.find(':');
			string name = t.substr(0, s);
			string val = t.substr(s+1);
			if(index.count(name)==0){
				continue;
			}
			auto oo = this->getObjConst(name);
			oo.setVal(atoi(val.c_str()));
		}

	}


	bool contains(const string& name) const{
		return index.count(name)>0;
	}
	void setFromString(const string& in){
		for(size_t i=0; i<in.size(); ++i){
			setBit(i, in[i]=='1'? 1 : -1);
		}
	}
	vector<int> serialize() const{
		vector<int> out;
		for(size_t t=0; t<objs.size(); ++t){
			const objP& tmp = objs[t];
			out.insert(out.end(), tmp.vals.begin(), tmp.vals.end());			
		}
		return out;
	}
	void setBit(int i, int val){
		bool found = false;
		for(size_t t=0; t<objs.size(); ++t){
			objP& tmp = objs[t];
			int gsz = tmp.globalSize();
			if(i < gsz){
				tmp.setBit(i, val);
				found = true;
				break;
			}else{
				i = i-gsz;
			}
		}
		Assert(found, "This is a bug");
	}
	int operator[](const string& name) const {
        int id = getId(name);
        AssertDebug(!objs[id].get_is_array(), "Can't return array as an int.");
		return objs[getId(name)].getInt();
	}

    const objP& getObjConst(const string& name) const {
        int id = getId(name);
        assert(id < objs.size());
        return objs.at(id);
    }
	objP& _getObj(const string& name) {
        int id = getId(name);
        assert(id < objs.size());
		return objs.at(id);
	}

    bool has(const string& name) const {
        return index.find(name) != index.end();
    }

	int getId(const string& name) const{
		AssertDebug(has(name), "Var " + name + " does't exists in this VarStore.")
		return index.at(name);
	}
    objP& _getObj(int id){
        return objs[id];
    }
	const objP& getObjConst(int id) const{
		return objs[id];
	}
	friend VarStore old_join(const VarStore& v1 , const VarStore& v2);
	friend VarStore* produce_join(const VarStore& v1 , const VarStore& v2);

    void relabel(vector<bool_node*>& inputs){

        Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
        Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

        assert(inputs.size() == objs.size());
        index.clear();
        for(int i = 0;i<objs.size();i++)
        {
            objP* at = &objs[i];
            string prev_name = at->name;
            do {
                assert(at->name == prev_name);
                at->name = inputs[i]->get_name();
                at = at->next;
            }while(at != nullptr);
            index[objs[i].name] = i;
        }
    }

    bool has_original_name_and_source_dag(const string &original_name, const string &source_dag) const;
    bool has_original_name(const string &original_name) const;

    void rename(BooleanDagUtility *new_dag_util);

    const string &get_name(const string& var_name, const string &source_dag_name);

    VarStore *get_sub_var_store(const string& descend_to);

    void descend_to_subname(const string &under_this_name);
};

inline VarStore* produce_join(const VarStore& _v1, const VarStore& v2)
{
	VarStore* ret = _v1.clone();
	for(int i = 0; i < v2.objs.size(); i++) {
		ret->insertObj(v2.objs[i].name, ret->objs.size(), VarStore::objP(v2.objs[i]));
	}
	ret->bitsize += v2.bitsize;
	return ret;
}

inline VarStore old_join(const VarStore& v1 , const VarStore& v2){
	VarStore rv = v1;
	int sz = v1.objs.size();
	rv.objs.insert(rv.objs.end(), v2.objs.begin(), v2.objs.end());
	for(map<string, int>::const_iterator it = v2.index.begin(); it != v2.index.end(); ++it){
		rv.index[it->first] = it->second + sz;
	}
	rv.bitsize = v1.bitsize + v2.bitsize;
	return rv;
}

#endif

