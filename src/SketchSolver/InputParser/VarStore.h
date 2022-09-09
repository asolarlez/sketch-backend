#ifndef VARSTORE_H
#define VARSTORE_H

#include "LightVarStore.h"

class LightInliningTree;
class TopologyMatcher;

// VarStore -- Keeps the mapping of node in the DAG vs its value.
class VarStore: public LightVarStore {
private:
    map<string, map<string, string> > var_name_to_dag_name_to_name;
    mutable LightInliningTree* inlining_tree = nullptr;

    void insert_name_in_original_name_to_dag_name_to_name(const string& name, string original_name, const string& source_dag_name) {
        assert(!original_name.empty() && !source_dag_name.empty());
        if(original_name == "declareInput()" || original_name == "declareCtrl()" || original_name == "to_var_store()") {
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

	void clear() const;

    VarStore() = default;

    VarStore(const VarStore& to_copy, bool deep_clone = false);
    VarStore(const VarStore& to_copy, LightInliningTree* _inlining_tree);

    void operator = (const VarStore& to_copy);

	VarStore* clone() const {
        return new VarStore(*this);
    }


    void insertObj(const string& name, int idx, const objP& obj)
    {
        LightVarStore::insertObj(name, idx, obj);
        insert_name_in_original_name_to_dag_name_to_name(obj.get_name(), obj.get_original_name(), obj.get_source_dag_name());
    }

	void newVar(const string& name, int nbits, const OutType* otype, bool_node::Type type, const string& original_name, const string& source_dag_name);

	void setVarVal(const string& name, int val, const OutType* otype, bool_node::Type type);

	bool contains(const string& name) const override {
        return LightVarStore::contains(name);
	}

    bool contains(const objP& obj, vector<string>* path) const;

	friend VarStore old_join(const VarStore& v1 , const VarStore& v2);
	friend VarStore* produce_join(const VarStore& v1 , const VarStore& v2);
    friend void append_join(VarStore& v1 , const VarStore& v2);

    bool has_original_name_and_source_dag(const string &original_name, const string &source_dag) const;
    bool has_original_name(const string &original_name) const;

    int get_id_from_original_name(const string& original_name) const {
        assert(has_original_name(original_name));
        assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
        assert(var_name_to_dag_name_to_name.at(original_name).size() == 1);
        string target_name = (*var_name_to_dag_name_to_name.at(original_name).begin()).second;
        int ret = getId(target_name);
        assert(ret >= 0);
        assert(objs[ret].get_original_name() == original_name);
        return ret;
    }

    const string &get_name(const string& var_name, const string &source_dag_name);

    const VarStore *get_sub_var_store(const string& descend_to) const;

    bool check_rep() const;

    void set_inlining_tree(LightInliningTree *new_inlining_tree);

    bool check_rep_and_clear();

    void rename_subdag(const string &prev_name, const string &new_name);
    void change_id(const string &prev_name, int new_id);

    void disjoint_join_with(const VarStore& v2)
    {
        append_join(*this, v2);
    }

    map<string, string> to_map_str_str(FloatManager& floats) const;

    const VarStore *produce_restrict(const vector<string>& subdomain) const;

    void link_name_and_id(const string &name, int id) const;

    void copy_only_bits(const VarStore *pStore);
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

