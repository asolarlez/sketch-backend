#ifndef VARSTORE_H
#define VARSTORE_H

#include <utility>
#include <vector>
#include <string>
#include <map>
#include "BasicError.h"
#include "SynthInSolver.h"
#include "objP.h"

using namespace std;

class LightInliningTree;
class TopologyMatcher;
//class InliningTree;
//class BooleanDagUtility;
// VarStore -- Keeps the mapping of node in the DAG vs its value.
class VarStore{
private:
	vector<objP> objs;
	map<string, int> index;
    map<string, map<string, string> > var_name_to_dag_name_to_name;
	int bitsize = 0;

    LightInliningTree* inlining_tree = nullptr;

    void insert_name_in_original_name_to_dag_name_to_name(const string& name, string original_name, string source_dag_name)
    {
        if(original_name == "declareInput()") {
            original_name += "___"+name;
        } else if(original_name == "declareCtrl()") {
            original_name += "___"+name;
        }else if(original_name == "to_var_store()") {
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
    void rename(const string &original_name, const string& new_source_dag, const string &new_name, const TopologyMatcher *new_inlining_tree, string& prev_source_dag_name);
    void rename(const objP& obj, const string &new_name, const string& new_source_dag);

    map<string, SynthInSolver*> synths;
    map<string, string> synthouts;

    const LightInliningTree* get_inlining_tree() const
    {
        return inlining_tree;
    }

	void clear();

    int size() const
    {
        return objs.size();
    }

    VarStore() = default;

    VarStore(const VarStore& to_copy, bool deep_clone = false);
    VarStore(const VarStore& to_copy, LightInliningTree* _inlining_tree);

    bool operator == (const VarStore& other) const
    {
        AssertDebug(other.synths.empty(), "TODO: implement eq logic for synths and synthouths.");
        AssertDebug(other.synthouts.empty(), "TODO: implement eq logic for synths and synthouths.");

        if(objs.size() != other.objs.size()) {
            return false;
        }

        for(int i = 0;i<objs.size();i++) {
            if(!(objs[i] == other.objs[i])) {
                return false;
            }
        }

        assert(bitsize == other.bitsize);

        return true;
    }

    void operator = (const VarStore& to_copy);

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
	    assert(name == obj.get_name());
        objs.push_back(obj);
	    index[name] = idx;
        insert_name_in_original_name_to_dag_name_to_name(obj.get_name(), obj.get_original_name(), obj.get_source_dag_name());
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

    void setArr(const string& name, const vector<int>& arr) {
	    Assert(index.find(name) != index.end(), name + " not found.");
        objs[index[name]].setArr(&arr);
        assert(objs[index[name]].get_is_array());
	}

	void newVar(const string& name, int nbits, const OutType* otype, bool_node::Type type, const string& original_name, const string& source_dag_name);

	void setVarVal(const string& name, int val, const OutType* otype, bool_node::Type type);

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
	int getIntsize() const {
		return objs.size();
	}
	void printBrief(ostream& out) const{
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].printBit(out);
		}
        cout << endl;
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
			out << objs[i].get_name() << ":";
			objs[i].printContent(out);
			out << endl;
		}
		for (auto sit = synths.begin(); sit != synths.end(); ++sit) {
			//out << sit->first << ":";
			sit->second->print(out);
			//out << endl;
		}
	}

	bool contains(const string& name) const{
		return index.count(name)>0;
	}

    bool contains(const objP& obj, vector<string>* path) const;
	void setFromString(const string& in){
		for(size_t i=0; i<in.size(); ++i){
			setBit(i, in[i]=='1'? 1 : -1);
		}
	}
	vector<int> serialize() const{
		vector<int> out;
		for(const auto & obj : objs){
            obj.append_vals(out);
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
    friend void append_join(VarStore& v1 , const VarStore& v2);
//    friend VarStore* concatenate_join(VarStore& v1 , const VarStore& v2);

    void relabel(const vector<bool_node*>& inputs){

        Assert(synths.empty(), "TODO: implement copy logic for synths.");
        Assert(synthouts.empty(), "TODO: implement copy logic for synthouths.");

        assert(inputs.size() == objs.size());
        index.clear();
        for(int i = 0;i<objs.size();i++)
        {
            objs[i].relabel(inputs[i]->get_name());
            index[objs[i].get_name()] = i;
        }
    }

    bool has_original_name_and_source_dag(const string &original_name, const string &source_dag) const;
    bool has_original_name(const string &original_name) const;

//    void rename(BooleanDagUtility *new_dag_util);

    const string &get_name(const string& var_name, const string &source_dag_name);

    const VarStore *get_sub_var_store(const string& descend_to) const;

    bool check_rep() const;

    void set_inlining_tree(LightInliningTree *new_inlining_tree);
//    void set_inlining_tree(LightInliningTree *new_inlining_tree);

    bool check_rep_and_clear();

    void rename_subdag(const string &prev_name, const string &new_name);
    void change_id(const string &prev_name, int new_id);

    void disjoint_join_with(const VarStore& v2)
    {
        append_join(*this, v2);
    }

    map<string, string> to_map_str_str(FloatManager& floats);

    const VarStore *produce_restrict(const vector<string>& subdomain) const;
};

template<bool_node::Type type>
class GenericVarStore: public VarStore {
public:
    explicit GenericVarStore(const VarStore& _to_copy): VarStore(_to_copy) {}
};

typedef GenericVarStore<bool_node::SRC> InputVarStore;
typedef GenericVarStore<bool_node::CTRL> HoleVarStore;

inline VarStore* produce_join(const VarStore& _v1, const VarStore& v2)
{
	VarStore* ret = _v1.clone();
	for(int i = 0; i < v2.objs.size(); i++) {
		ret->insertObj(v2.objs[i].get_name(), ret->objs.size(), objP(v2.objs[i]));
	}
	ret->bitsize += v2.bitsize;
	return ret;
}

inline void append_join(VarStore& _v1, const VarStore& v2)
{
    VarStore* ret = &_v1;
    for(int i = 0; i < v2.objs.size(); i++) {
        ret->insertObj(v2.objs[i].get_name(), ret->objs.size(), objP(v2.objs[i]));
    }
    ret->bitsize += v2.bitsize;
}

//inline void concatenate_join(VarStore& _v1, const VarStore& v2)
//{
//    VarStore* ret = &_v1;
//    for(int i = 0; i < v2.objs.size(); i++) {
//        ret->insertObj(v2.objs[i].get_name(), ret->objs.size(), v2.objs[i]);
//    }
//    ret->bitsize += v2.bitsize;
////    return ret;
//}


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

