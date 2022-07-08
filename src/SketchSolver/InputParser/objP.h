//
// Created by Kliment Serafimov on 7/8/22.
//

#ifndef SKETCH_OBJP_H
#define SKETCH_OBJP_H

#include "BooleanNodes.h"

#include <utility>
#include <vector>
#include <string>
#include <cassert>
using namespace std;

class FloatManager;
class DagOptim;

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

class VarStoreElementHeader
{
protected:
    bool_node::Type type = bool_node::NO_TYPE;
    string name;
    const string original_name;
    string source_dag_name;
public:
    const OutType* const otype;

    VarStoreElementHeader(
            bool_node::Type _type, string _name,
            const OutType* _otype, string _original_name,
            string _source_dag_name):
        type(_type), name(std::move(_name)), otype(_otype), original_name(std::move(_original_name)), source_dag_name(std::move(_source_dag_name)) {}

    VarStoreElementHeader(const VarStoreElementHeader& old):
            type(old.type), name(old.name), otype(old.otype), original_name(old.original_name), source_dag_name(old.source_dag_name) {}

    const string& get_name() const
    {
        return name;
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

    void rename(const string &new_name, const string &subdag_name) {
        name = new_name;
        source_dag_name = subdag_name;
    }

    bool_node::Type get_type() const {
        assert(type != bool_node::NO_TYPE);
        return type;
    }
};

class VarStoreElementTrait: public VarStoreElementHeader{
private:
    int nbits;
protected:
    VarStoreElementTrait(const string& _name, int _nbits, const OutType* _otype, bool_node::Type _type, const string& _original_name="",
         const string& _source_dag_name=""): nbits(_nbits),
         VarStoreElementHeader(_type, _name, _otype, _original_name, _source_dag_name){}

    VarStoreElementTrait(const VarStoreElementTrait& old): VarStoreElementHeader(old) {}
public:

    virtual int get_index() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual VarStoreElementTrait* get_next() const {
        AssertDebug(false, "not implemented.");
        return nullptr;
    }

    virtual int get_size() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual ~VarStoreElementTrait() = default;

    virtual void clear() {
        AssertDebug(false, "not implemented.");
    }

    virtual void makeArr(int start, int end) {
        AssertDebug(false, "not implemented.");
    }

    virtual int arrSize() {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual int element_size() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual int globalSize() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual int resize(int n) {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual VarStoreElementTrait* setBit(size_t i, int val) {
        AssertDebug(false, "not implemented.");
        return nullptr;
    }

    virtual int getInt() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual int getInt(int idx) const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual void setArr(const vector<int> *arr) {
        AssertDebug(false, "not implemented.");
    }

    ///Return false if VarStoreElementTrait did not have enough bits to be made equal to v.
    virtual bool setValSafe(int v) {
        AssertDebug(false, "not implemented.");
        return false;
    }

    virtual void setVal(size_t v) {
        AssertDebug(false, "not implemented.");
    }

    virtual void printBit(ostream& out) const {
        AssertDebug(false, "not implemented.");
    }

    virtual void printContent(ostream& out) const {
        AssertDebug(false, "not implemented.");
    }

    virtual bool increment() {
        AssertDebug(false, "not implemented.");
        return false;
    }

    // If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
    virtual void makeRandom(float sparseDeg, int n=10) {
        AssertDebug(false, "not implemented.");
    }

    virtual void makeRandom() {
        AssertDebug(false, "not implemented.");
    }

    virtual void zeroOut() {
        AssertDebug(false, "not implemented.");
    }

    virtual bool get_is_array() const {
        AssertDebug(false, "not implemented.");
        return false;
    }

    virtual void relabel(const string new_name) {
        AssertDebug(false, "not implemented.");
    }

    virtual void populate_vec(int *vv, int sz) const{
        AssertDebug(false, "not implemented.");
    }

    virtual void populate_multi_mother_nodeForINode(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const{
        AssertDebug(false, "not implemented.");
    }

    virtual void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const{
        AssertDebug(false, "not implemented.");
    }

    virtual void append_vals(vector<int>& out) const{
        AssertDebug(false, "not implemented.");
    }

    virtual bool operator == (const VarStoreElementTrait& other) const{
        AssertDebug(false, "not implemented.");
        return false;
    }
};


class objP: public VarStoreElementTrait {
    bool is_array;
    objP* next;
    vector<int> vals;
    int index;
    bool isNeg;
public:
    int get_index() const override {
        return index;
    }

    VarStoreElementTrait* get_next() const override {
        return next;
    }

    int get_size() const override
    {
        return globalSize();
    }

private:
    bool in_clear = false;
public:
    ~objP() override{
        clear();
    }

    void clear() override
    {
        assert(!in_clear);
        in_clear = true;
        vals.clear();
        if(next != nullptr) {delete next; next = nullptr; };
    }

    objP(string  _name, int _size, const OutType* _otype,
         bool_node::Type _type, const string& _original_name="", const string& _source_dag_name=""):
         vals(_size), isNeg(false), index(0), next(nullptr),
         VarStoreElementTrait(_name, _size, _otype, _type, _original_name, _source_dag_name)
     {
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
    }

    objP(const objP& old):
            vals(old.vals), isNeg(old.isNeg), index(old.index), is_array(old.is_array),
            VarStoreElementTrait(old){
        if(old.next != nullptr){
            next=new objP(*old.next);
        }
        else{next=nullptr;}
    }

    objP operator=(const objP& old) {
        return objP(old);
    }

    void makeArr(int start, int end) override{
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

    int arrSize() override{
        assert(is_array);
        if(next==nullptr){
            return 1;
        }else{
            return next->arrSize() + 1;
        }
    }

    int element_size() const override{
        return vals.size();
    }

    int globalSize() const override{
        if(next == nullptr){
            return element_size();
        }
        return next->globalSize() + element_size();
    }

    int resize(int n) override{
        int x=0;
        vals.resize(n);
        if(next != nullptr){
            x=next->resize(n);
        } return x+n;
    }

    VarStoreElementTrait* setBit(size_t i, int val) override{
        if(i<vals.size()){
            vals[i] = val;
            return this;
        }else{
            Assert(next != nullptr, "bad bad");
            return next->setBit(i-vals.size(), val);
        }
    }

    int getInt() const override{
        int t = intFromBV(vals, 0, vals.size());
        return isNeg? -t : t;
    }

    int getInt(int idx) const override{
        assert(!is_array);
        if(this->index==idx){
            return getInt();
        }else{
            if(next != nullptr){ return next->getInt(idx); }
            else {return -1;}
        }
        Assert(false,"Control shouldn't reach here");
    }

    void setArr(const vector<int> *arr) override {
        assert(is_array);
        objP* at = this;
        for(int i = 0;i<arr->size();i++)
        {
            assert(at != nullptr);
            at->setVal(arr->at(i));
            at = at->next;
        }
    }

    ///Return false if objP did not have enough bits to be made equal to v.
    bool setValSafe(int v) override {
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

    void setVal(size_t v) override{
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
    void printBit(ostream& out) const override{
        for(size_t i=0; i<vals.size(); ++i){
            out<<(vals[i]==1?1:0);
        }
        if(next!= nullptr){ out<<"|"; next->printBit(out); }
    }
    void printContent(ostream& out) const override{
        out << getInt();
        if(next!= nullptr){ out<<"|"; next->printContent(out); }
    }

    bool increment() override{
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
    void makeRandom(float sparseDeg, int n=10) override{
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

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<vals.size(); ++i){
            vals[i] = (rand() & 0x3) > 0? -1 : 1;
        }
        if(next!= nullptr){ next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<vals.size(); ++i){
            vals[i] = -1;
        }
        if(next!= nullptr){ next->zeroOut(); }
    }

    bool get_is_array() const override {
        return is_array;
    }

    void relabel(const string new_name) override {
        objP* at = this;
        string prev_name = at->name;
        do {
            assert(at->name == prev_name);
            at->name = new_name;
            at = at->next;
        }while(at != nullptr);
    }

    void populate_vec(int *vv, int sz) const override {
        auto op = this;
        while(op != nullptr){
            Assert(op->index < sz, "Out of bounds error in solver ;alkwebbn");
            vv[op->index] = op->getInt();
            op = op->next;
        }
    }

    void populate_multi_mother_nodeForINode(
            vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const override;

    void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const override;

    void append_vals(vector<int>& out) const override;

    bool operator == (const objP& other) const
    {
        const bool debug = true;
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
};


#endif //SKETCH_OBJP_H
