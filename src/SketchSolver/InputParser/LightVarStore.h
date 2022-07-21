//
// Created by Kliment Serafimov on 7/17/22.
//

#ifndef SKETCH_LIGHTVARSTORE_H
#define SKETCH_LIGHTVARSTORE_H

#include <utility>
#include <vector>
#include <string>
#include <map>
#include "BasicError.h"
#include "SynthInSolver.h"
#include "objP.h"

using namespace std;

// VarStore -- Keeps the mapping of node in the DAG vs its value.
class LightVarStore {
protected:
    vector<objP> objs;
    mutable map<string, int> index;
    int bitsize = 0;

public:
    void rename(const objP& obj, const string &new_name, const string& new_source_dag);

    map<string, SynthInSolver*> synths;
    map<string, string> synthouts;

    void clear() const;

    int size() const
    {
        return objs.size();
    }

    LightVarStore() = default;

    LightVarStore(const LightVarStore& to_copy, bool deep_clone = false);

    bool operator == (const LightVarStore& other) const
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

    void operator = (const LightVarStore& to_copy);

    LightVarStore* clone() const {
        return new LightVarStore(*this);
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

    void insertObj(const string& name, int idx, const objP& obj)
    {
        AssertDebug(index.find(name) == index.end(), name + " should not be present in index.");
        AssertDebug(idx == objs.size(), "idx, " + std::to_string(idx) + " should be the same as objs.size() = " + std::to_string(objs.size()) + ".");
        assert(name == obj.get_name());
        objs.push_back(obj);
        index[name] = idx;
    }

    void newArr(const string& name, int nbits, int arrsz, OutType* otype, bool_node::Type type, const string& original_name, const string& source_dag_name){
        Assert(index.count(name)==0, name<<": This array already existed!!");
        int begidx = objs.size();
        index[name] = begidx;
        objs.emplace_back(name, nbits, otype, type, original_name, source_dag_name);
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

    virtual bool contains(const string& name) const{
        return index.count(name)>0;
    }

//	void setFromString(const string& in){
//		for(size_t i=0; i<in.size(); ++i){
//			set_bit(i, in[i]=='1'? 1 : -1);
//		}
//	}
    vector<int> serialize() const{
        vector<int> out;
        for(const auto & obj : objs){
            obj.append_vals(out);
        }
        return out;
    }
//	void set_bit(int i, int val){
//		bool found = false;
//		for(size_t t=0; t<objs.size(); ++t){
//			objP& tmp = objs[t];
//			int gsz = tmp.globalSize();
//			if(i < gsz){
//				tmp.set_bit(i, val);
//				found = true;
//				break;
//			}else{
//				i = i-gsz;
//			}
//		}
//		Assert(found, "This is a bug");
//	}
    int operator[](const string& name) const {
        int id = getId(name);
        AssertDebug(!objs[id].get_is_array(), "Can't return array as an int.");
        return objs[id].getInt();
    }

    int operator[](size_t id) const {
        assert(id < objs.size());
        AssertDebug(!objs[id].get_is_array(), "Can't return"
                                              " array as an int.");
        return objs[id].getInt();
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

    int getId(const string& name) const{
        auto ret_it = index.find(name);
        AssertDebug(ret_it != index.end(), "Var " + name + " does't exists in this VarStore.")
        return ret_it->second;
    }
    objP& _getObj(int id){
        return objs[id];
    }
    const objP& getObjConst(int id) const{
        return objs[id];
    }
    friend LightVarStore old_join(const LightVarStore& v1 , const LightVarStore& v2);
    friend LightVarStore* produce_join(const LightVarStore& v1 , const LightVarStore& v2);
    friend void append_join(LightVarStore& v1 , const LightVarStore& v2);

    void relabel(const vector<bool_node*>& inputs) const {

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

    const LightVarStore *get_sub_var_store(const string& descend_to) const;

    bool check_rep() const;

    bool check_rep_and_clear();

    void rename_subdag(const string &prev_name, const string &new_name);
    void change_id(const string &prev_name, int new_id);

    void disjoint_join_with(const LightVarStore& v2)
    {
        append_join(*this, v2);
    }

    map<string, string> to_map_str_str(FloatManager& floats);

    const LightVarStore *produce_restrict(const vector<string>& subdomain) const;

    void link_name_and_id(const string &name, int id) const;
};




#endif //SKETCH_LIGHTVARSTORE_H
