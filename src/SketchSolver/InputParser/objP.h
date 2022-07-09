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
int intFromBV(const T& bv, int start, int nbits){
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

class VarStoreElementTrait {
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

//    virtual ~VarStoreElementTrait() {
//        AssertDebug(false, "not implemented.");
//    };

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

    virtual void setVal(int v) {
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

class SuccinctBitVectorVector
{
    static const int max_num_bits = 30;
    int num_bits_per_vector;
    int total_num_bits;
    vector<int> vector_of_vectors;
public:
    SuccinctBitVectorVector(int _num_vectors, int _num_bits_per_vector):
        num_bits_per_vector(_num_bits_per_vector), total_num_bits(_num_vectors*_num_bits_per_vector) {
        assert(num_bits_per_vector <= max_num_bits);
        vector_of_vectors = vector<int>(_num_vectors, 0);
    }
    explicit SuccinctBitVectorVector(const SuccinctBitVectorVector* to_copy):
            num_bits_per_vector(to_copy->num_bits_per_vector), total_num_bits(to_copy->total_num_bits) {
        vector_of_vectors = to_copy->vector_of_vectors;
    }

    size_t get_num_vectors() {
        return vector_of_vectors.size();
    }

    int get_num_bits_per_vector() const {
        return num_bits_per_vector;
    }

    void resize_num_bits_per_vector(int new_num_bits_per_vector)
    {
        assert(new_num_bits_per_vector >= num_bits_per_vector);
        assert(new_num_bits_per_vector <= max_num_bits);
        num_bits_per_vector = new_num_bits_per_vector;
    }

    int get_total_num_bits() {
        return total_num_bits;
    }

    bool increment()
    {
        const int max_num = (1<<num_bits_per_vector)-1;
        for(int i = 0;i<vector_of_vectors.size();i++)
        {
            if(vector_of_vectors[i] != max_num)
            {
                vector_of_vectors[i]++;
                return true;
            }
        }
        return false;
    }

    int get(size_t idx) const {
        assert(0 <= idx && idx < vector_of_vectors.size());
        return vector_of_vectors[idx];
    }

    void set(int idx, int val) {
        assert(0 <= idx && idx < vector_of_vectors.size());
        vector_of_vectors[idx] = val;
    }

    void setBit(size_t bit_id, int val) {
        assert(val ==  0 || val == 1);
        int word_id = bit_id/num_bits_per_vector;
        int local_bit_id = bit_id - word_id*num_bits_per_vector;
        setBit(word_id, local_bit_id, val);
    }

    void setBit(size_t word_id, size_t local_bit_id, int val) {
        assert(val ==  0 || val == 1);
        if(val == 0) {
            vector_of_vectors[word_id] &= ~((int)1 << local_bit_id);
        }
        else if(val == 1) {
            vector_of_vectors[word_id] |= ((int)1 << local_bit_id);
        }
        else {
            assert(false);
        }
    }

    bool get_bit(size_t idx, size_t bit_id)
    {
        assert(idx < vector_of_vectors.size());
        assert(bit_id < num_bits_per_vector);
        return (vector_of_vectors[idx] & (1<<bit_id)) != 0;
    }

    void push_back() {
        vector_of_vectors.push_back(0);
    }

    auto begin() const {
        return vector_of_vectors.begin();
    }
    auto end() const {
        return vector_of_vectors.end();
    }

    void clear() {
        vector_of_vectors.clear();
        delete this;
    }

    string to_bit_string()
    {
        string ret;
        for(int i = 0;i<vector_of_vectors.size();i++)
        {
            if(i >= 1)
            {
                ret += "|";
            }
            string bit_string;
            for(int j = 0;j<num_bits_per_vector; j++)
            {
                bit_string += (char)((int)'0'+get_bit(i, j));
            }
            reverse(bit_string.begin(), bit_string.end());
            ret += bit_string;
        }
        return ret;
    }

    const vector<int> *get_vector_of_vectors_pointer() {
        return &vector_of_vectors;
    }
};

class objP;

class VarStoreElementIndexView: public VarStoreElementTrait {
    objP* parent = nullptr;
    SuccinctBitVectorVector* array() const;
    bool is_array() const;
    VarStoreElementIndexView* next = nullptr;
    int index = -1;

    //ONLY CALLED FROM SuccinctobjP CONSTRUCTOR TO PUSH A NEXT ELEMENT IN THE ARRAY.
    explicit VarStoreElementIndexView(objP* _parent): parent(_parent) {
        array()->push_back();
    }
protected:
    VarStoreElementIndexView() = default;
public:

    const vector<int>* get_vector_of_vectors_pointer() const
    {
        return array()->get_vector_of_vectors_pointer();
    }

    int get_index() const override {
        return index;
    }

    VarStoreElementTrait* get_next() const override {
        return next;
    }
//
    int get_size() const override
    {
        return globalSize();
    }
//    ~VarStoreElementIndexView() override {
//        if(next != nullptr) {delete next; next = nullptr; };
//    }
//
//    VarStoreElement(string  _name, int _size, const OutType* _otype,
//                    bool_node::Type _type, string _original_name="", string _source_dag_name=""):
//            array(new SuccinctBitVectorVector(1, _size)), is_head(true), index(0), next(nullptr),
//            VarStoreElementHeader(_type, std::move(_name), _otype, std::move(_original_name), std::move(_source_dag_name))
//    {
//        assert(_otype != nullptr);
//        if(_otype == OutType::INT_ARR || _otype == OutType::BOOL_ARR || _otype == OutType::FLOAT_ARR) {
//            is_array = true;
//            assert(_otype->isArr);
//        }
//        else
//        {
//            is_array = false;
//            assert(!_otype->isArr);
//        }
//    }

protected:
    void init_from_old(const VarStoreElementIndexView& old, objP* _parent) {
        assert(parent == nullptr && index == -1);
        index = old.index;
        parent = _parent;
        if(old.next != nullptr){
            next = new VarStoreElementIndexView(*old.next, _parent);
        }
    }
    void init(objP* _parent) {
        assert(parent == nullptr && index == -1);
        index = 0;
        parent = _parent;
    }
public:

    VarStoreElementIndexView(const VarStoreElementIndexView& old, objP* _parent) {
        init_from_old(old, _parent);
    }

    void makeArr(int start, int end) override;

    int arrSize() override;

    int element_size() const override;

    int globalSize() const override;

    int resize(int n) override;

    VarStoreElementTrait* setBit_helper(size_t i, int val, bool is_head = true);

    VarStoreElementTrait* setBit(size_t i, int val) override {
        return setBit_helper(i, val, true);
    }

    int getInt() const override {
        return array()->get(index);
    }

    int getInt(int idx) const override;

    void setArr(const vector<int> *arr) override {
        assert(is_array());
        VarStoreElementIndexView* at = this;
        for(int i = 0;i<arr->size();i++)
        {
            assert(at != nullptr);
            at->setVal(arr->at(i));
            at = at->next;
        }
    }

    ///Return false if SuccinctobjP did not have enough bits to be made equal to v.
    bool setValSafe(int v) override {
        size_t t = array()->get_num_bits_per_vector();
        array()->set(index, v);
        int len = 0;
        while(v != 0){
            len+=1;
            v = v >> 1;
            if(len==t && v != 0){
                return false;
            }
        }
        return true;
    }

    void setVal(int v) override {
        array()->set(index, v);
        int len = 0;
        while(v != 0){
            len+=1;
            v = v >> 1;
        }
        array()->resize_num_bits_per_vector(len);
    }
    void printBit(ostream& out) const override{
        out << array()->to_bit_string();
    }
    void printContent(ostream& out) const override{
        out << getInt();
        if(next!= nullptr){ out<<"|"; next->printContent(out); }
    }

    bool increment() override{
        return array()->increment();
    }

    /**
    If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
    */
    void makeRandom(float sparseDeg, int n=10) override{
        int P  = 10000;
        int q = P*sparseDeg;

        if(n > 0 || (rand() % P) < q ){
            for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
                array()->setBit(index, i, (rand() & 0x3) > 0? -1 : 1);
            }
        }else{
            for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
                array()->setBit(index, i, 0);
            }
        }
        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
    }

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->setBit(index, i, (rand() & 0x3) > 0? 0 : 1);
        }
        if(next!= nullptr){ next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->setBit(index, i, 0);
        }
        if(next!= nullptr){ next->zeroOut(); }
    }

    bool get_is_array() const override {
        return is_array();
    }

//    void relabel(const string new_name) override {
//        VarStoreElementIndexView* at = this;
//        string prev_name = at->name;
//        do {
//            assert(at->name == prev_name);
//            at->name = new_name;
//            at = at->next;
//        }while(at != nullptr);
//    }

    void populate_multi_mother_nodeForINode(
            vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const override;

    void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const override;

    void append_vals(vector<int>& out) const override;

};

class objP: public VarStoreElementIndexView, public VarStoreElementHeader {
    bool is_array;
    //if is_array == true; then array.size() == number of elements in the array and array holds the array.
    //if is_array == false; then array.size() == 1 and array[0] holds the value.
    SuccinctBitVectorVector* array = nullptr;

//    //ONLY CALLED FROM SuccinctobjP CONSTRUCTOR TO PUSH A NEXT ELEMENT IN THE ARRAY.
//    VarStoreElement(string _name, SuccinctBitVectorVector* _array, const OutType* _otype,
//                    bool_node::Type _type, string _original_name="", string _source_dag_name=""):
//            array(_array), is_head(false), index(0), next(nullptr),
//            VarStoreElementHeader(_type, std::move(_name), _otype, std::move(_original_name), std::move(_source_dag_name))
//    {
//        assert(_otype != nullptr);
//        if(_otype == OutType::INT_ARR || _otype == OutType::BOOL_ARR || _otype == OutType::FLOAT_ARR) {
//            is_array = true;
//            assert(_otype->isArr);
//        }
//        else
//        {
//            is_array = false;
//            assert(!_otype->isArr);
//        }
//        array->push_back();
//    }
public:
    SuccinctBitVectorVector* get_array() const {
        return array;
    }

    const vector<int>* get_vector_of_vectors_pointer() const
    {
        return array->get_vector_of_vectors_pointer();
    }

    int get_size() const override
    {
        return globalSize();
    }

private:
    void clear() {
        assert(array != nullptr);
        array->clear();
    }
public:
//    ~objP() override {
//        objP::clear();
//    }

    objP(string  _name, int _size, const OutType* _otype,
         bool_node::Type _type, string _original_name="", string _source_dag_name=""):
            array(new SuccinctBitVectorVector(1, _size)),
            VarStoreElementHeader(_type, std::move(_name), _otype, std::move(_original_name), std::move(_source_dag_name))
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
        init(this);
    }

//    VarStoreElement(const VarStoreElement& old, SuccinctBitVectorVector* array):
//            array(array), is_head(false), is_array(old.is_array),
//            VarStoreElementHeader(old){
//        assert(!old.is_head);
//        if(old.next != nullptr){
//            next=new VarStoreElement(*old.next, array);
//        }
//        else{next=nullptr;}
//    }

    objP(const objP& old):
            array(new SuccinctBitVectorVector(old.array)), is_array(old.is_array),
            VarStoreElementHeader(old){
        init_from_old(old, this);
    }

    objP operator=(const objP& old) {
        return objP(old);
    }

//    int arrSize() override{
//        assert(is_array);
//        if(next==nullptr){
//            return 1;
//        }else{
//            return next->arrSize() + 1;
//        }
//    }

    int element_size() const override{
        return array->get_num_bits_per_vector();
    }

//    int globalSize() const override{
//        if(next == nullptr){
//            return element_size();
//        }
//        return next->globalSize() + element_size();
//    }

    int resize(int n) override{
        array->resize_num_bits_per_vector(n);
        return array->get_total_num_bits();
    }

//    VarStoreElementTrait* setBit(size_t i, int val) override {
//        assert(val == 0 || val == 1);
//        if(is_head) {
//            array->setBit(i, val);
//        }
//
//        if(i<array->get_num_bits_per_vector()){
//            return this;
//        }else{
//            Assert(next != nullptr, "bad bad");
//            return next->setBit(i-array->get_num_bits_per_vector(), val);
//        }
//    }

//    int getInt() const override {
//        return array->get(index);
//    }

//    int getInt(int idx) const override{
//        assert(!is_array);
//        if(this->index==idx){
//            return getInt();
//        }else{
//            if(next != nullptr){ return next->getInt(idx); }
//            else {return -1;}
//        }
//        Assert(false,"Control shouldn't reach here");
//    }

//    void setArr(const vector<int> *arr) override {
//        assert(is_array);
//        VarStoreElement* at = this;
//        for(int i = 0;i<arr->size();i++)
//        {
//            assert(at != nullptr);
//            at->setVal(arr->at(i));
//            at = at->next;
//        }
//    }

//    ///Return false if SuccinctobjP did not have enough bits to be made equal to v.
//    bool setValSafe(int v) override {
//        size_t t = array->get_num_bits_per_vector();
//        array->set(index, v);
//        int len = 0;
//        while(v != 0){
//            len+=1;
//            v = v >> 1;
//            if(len==t && v != 0){
//                return false;
//            }
//        }
//        return true;
//    }

//    void setVal(int v) override {
//        array->set(index, v);
//        int len = 0;
//        while(v != 0){
//            len+=1;
//            v = v >> 1;
//        }
//        array->resize_num_bits_per_vector(len);
//    }

    void printBit(ostream& out) const override{
        out << array->to_bit_string();
    }

//    void printContent(ostream& out) const override{
//        out << getInt();
//        if(next!= nullptr){ out<<"|"; next->printContent(out); }
//    }

    bool increment() override{
        return array->increment();
    }

//    /**
//    If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
//    */
//    void makeRandom(float sparseDeg, int n=10) override{
//        int P  = 10000;
//        int q = P*sparseDeg;
//
//        if(n > 0 || (rand() % P) < q ){
//            for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//                array->setBit(index, i, (rand() & 0x3) > 0? -1 : 1);
//            }
//        }else{
//            for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//                array->setBit(index, i, 0);
//            }
//        }
//        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
//    }
//
//    void makeRandom() override{/* Bias towards zeros */
//        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//            array->setBit(index, i, (rand() & 0x3) > 0? 0 : 1);
//        }
//        if(next!= nullptr){ next->makeRandom(); }
//    }
//    void zeroOut() override{/* Bias towards zeros */
//        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//            array->setBit(index, i, 0);
//        }
//        if(next!= nullptr){ next->zeroOut(); }
//    }

    bool get_is_array() const override {
        return is_array;
    }

    void relabel(string new_name) override {
        name = std::move(new_name);
    }

//    void populate_multi_mother_nodeForINode(
//            vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const override;
//
//    void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const override;
//
//    void append_vals(vector<int>& out) const override;

};


#endif //SKETCH_OBJP_H
