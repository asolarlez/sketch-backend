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

class vector_int: private vector<int>
{
    bool invariant_0(int val) const
    {
        return val == 0 || val == 1;
    }
    bool invariant_neg1(int val) const
    {
        return invariant_0(val) || val == -1;
    }
    int map_neg1_to_0(int val)
    {
        if(val == -1) return 0;
        return val;
    }
public:
    explicit vector_int(size_t n): vector<int>(n){}

    void push_back(int val) {
        assert(invariant_0(val));
        vector<int>::push_back(val);
    }
    void set(size_t idx, int val, bool inv_neg1 = false)
    {
        if(inv_neg1) {
            assert(invariant_neg1(val));
        }
        else {
            assert(invariant_0(val));
        }
//        if(allow_neg1) {
//            val = map_neg1_to_0(val);
//        }
//        assert(invariant_0(val));
        vector<int>::operator[](idx) = val;
    }

    int operator[](size_t idx) const
    {
        int ret = vector<int>::at(idx);
        assert(invariant_0(ret));
        return ret;
    }

    int get(size_t idx, bool inv_neg1 = false) const
    {
        int ret = vector<int>::at(idx);
        if(inv_neg1) {
            assert(invariant_neg1(ret));
        }
        else {
            assert(invariant_0(ret));
        }
        return ret;
    }

    void resize(size_t n, int val = 0)
    {
        assert(invariant_0(val));
        vector<int>::resize(n, val);
    }
    size_t size() const
    {
        return vector<int>::size();
    }
    void clear()
    {
        vector<int>::clear();
    }
    auto begin()const {
        return vector<int>::begin();
    }
    auto end()const {
        return vector<int>::end();
    }
};

int intFromBV(const vector_int& bv, int start, int nbits);

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

class VarStoreElementTrait: public VarStoreElementHeader{
private:
    int nbits;
protected:
    VarStoreElementTrait(string _name, int _nbits, const OutType* _otype, bool_node::Type _type, string _original_name="",
         string _source_dag_name=""): nbits(_nbits),
         VarStoreElementHeader(_type, std::move(_name), _otype, std::move(_original_name), std::move(_source_dag_name)){}

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


//#define USE_objP

#ifdef USE_objP
class objP: public VarStoreElementTrait {
    bool is_array = false;
    objP* next = nullptr;
    vector_int vals;
    int index = 0;
    bool isNeg = false;
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

    void clear()
    {
        assert(!in_clear);
        in_clear = true;
        vals.clear();
        if(next != nullptr) {delete next; next = nullptr; };
    }
public:
    ~objP() override {
        objP::clear();
    }


    objP(string  _name, int _size, const OutType* _otype,
         bool_node::Type _type, string _original_name="", string _source_dag_name=""):
         vals(_size), isNeg(false), index(0), next(nullptr),
         VarStoreElementTrait(std::move(_name), _size, _otype, _type, std::move(_original_name), std::move(_source_dag_name))
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
        assert(val == 0 || val == 1);
        if(i<vals.size()){
            vals.set(i, val);
            return this;
        }else{
            Assert(next != nullptr, "bad bad");
            return next->setBit(i-vals.size(), val);
        }
    }

    int getInt() const override{
        int t = intFromBV(vals, 0, vals.size());
        int ret = isNeg? -t : t;
//        cout << "objP.getInt() = " << ret << endl;
        return ret;
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

    void setVal(int v) override{
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
            out<<(vals.get(i, true)==1?1:0);
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
                vals.set(i, 1);
                return true;
            }else{
                vals.set(i, -1);
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
                vals.set(i, (rand() & 0x3) > 0? -1 : 1);
            }
        }else{
            for(size_t i=0; i<vals.size(); ++i){
                vals.set(i, -1);
            }
        }
        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
    }

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<vals.size(); ++i){
            vals.set(i, (rand() & 0x3) > 0? -1 : 1, true);
        }
        if(next!= nullptr){ next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<vals.size(); ++i){
            vals.set(i, -1);
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
};
#endif

template<typename T>
class VectorTrait
{
public:
    virtual T get(int idx) const = 0;
    virtual void set(int idx, T) = 0;
//    virtual void push_back(T val) = 0;
};


class SuccinctBitVector : public VectorTrait<bool>
{
    typedef uint32_t WORD_TYPE;
    static const int word_size_bits = 5;
    static_assert(sizeof(WORD_TYPE)*8 == 1<<word_size_bits, "WORD_TYPE and word_size_bits are inconsistent.");
    static const int word_size = 1<<word_size_bits;
    static const int low_order_bits_mask = (1<<word_size_bits)-1;
    int num_bits;
    vector<WORD_TYPE> bits;
public:
    SuccinctBitVector() = default;
    explicit SuccinctBitVector(int _num_bits): num_bits(_num_bits)
    {
        bits = vector<WORD_TYPE>((num_bits+word_size-1) >> word_size_bits, 0);
    }

    bool get(int idx) const override {
        bool ret = (bits[idx >> word_size_bits] & ((WORD_TYPE)1 << (idx & low_order_bits_mask))) != 0;
        return ret;
    }
    void set(int idx,  bool val) override {
        int at_word = idx >> word_size_bits;
        WORD_TYPE original_word = bits[at_word];
        if(val) {
            bits[at_word] |= ((WORD_TYPE)1 << (idx & low_order_bits_mask));
        }
        else {
            bits[at_word] &= ~((WORD_TYPE)1 << (idx & low_order_bits_mask));
        }
    }

    size_t size() const {
        return num_bits;
    }

    // Minimum required for range-for loop
    struct Iterator {
        int at_idx;
        const SuccinctBitVector* _this;
        int operator * () const { return _this->get(at_idx); }
        bool operator != (const Iterator& rhs) const {
            return at_idx != rhs.at_idx;
        }
        void operator ++() {
            at_idx++;
        }
    };

    // auto return requires C++14
    auto begin() const {
        int at_idx = 0;
        return Iterator{at_idx, this};
    }
    auto end() const {
        return Iterator{num_bits, this};
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


#ifndef USE_objP
#define SuccinctobjP objP

class SuccinctobjP: public VarStoreElementTrait {
    bool is_array;
    //if is_array == true; then array.size() == number of elements in the array and array holds the array.
    //if is_array == false; then array.size() == 1 and array[0] holds the value.
    SuccinctBitVectorVector* array = nullptr;
    SuccinctobjP* next = nullptr;
    int index = 0;
    bool is_head = true;

    //ONLY CALLED FROM SuccinctobjP CONSTRUCTOR TO PUSH A NEXT ELEMENT IN THE ARRAY.
    SuccinctobjP(string _name, SuccinctBitVectorVector* _array, const OutType* _otype,
                 bool_node::Type _type, string _original_name="", string _source_dag_name=""):
            array(_array), is_head(false), index(0), next(nullptr),
            VarStoreElementTrait(std::move(_name), _array->get_num_bits_per_vector(), _otype, _type, std::move(_original_name), std::move(_source_dag_name))
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
        array->push_back();
    }
public:
    const vector<int>* get_vector_of_vectors_pointer() const
    {
        return array->get_vector_of_vectors_pointer();
    }

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

    void clear()
    {
        assert(!in_clear);
        in_clear = true;
        if(is_head) {
            array->clear();
        }
        if(next != nullptr) {delete next; next = nullptr; };
    }
public:
    ~SuccinctobjP() override{
        SuccinctobjP::clear();
    }

    SuccinctobjP(string  _name, int _size, const OutType* _otype,
         bool_node::Type _type, string _original_name="", string _source_dag_name=""):
            array(new SuccinctBitVectorVector(1, _size)), is_head(true), index(0), next(nullptr),
            VarStoreElementTrait(std::move(_name), _size, _otype, _type, std::move(_original_name), std::move(_source_dag_name))
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

    SuccinctobjP(const SuccinctobjP& old, SuccinctBitVectorVector* array):
            array(array), is_head(false), index(old.index), is_array(old.is_array),
            VarStoreElementTrait(old){
        assert(!old.is_head);
        if(old.next != nullptr){
            next=new SuccinctobjP(*old.next, array);
        }
        else{next=nullptr;}
    }

    SuccinctobjP(const SuccinctobjP& old):
            array(new SuccinctBitVectorVector(old.array)), is_head(true), index(old.index), is_array(old.is_array),
            VarStoreElementTrait(old){
        assert(old.is_head);
        if(old.next != nullptr){
            next=new SuccinctobjP(*old.next, array);
        }
        else{next=nullptr;}
    }

    SuccinctobjP operator=(const SuccinctobjP& old) {
        return SuccinctobjP(old);
    }

    void makeArr(int start, int end) override{
        assert(is_array);
        Assert(start < end, "Empty arr");
        index = start;
        if(start+1 < end){
            if(next == nullptr){
                next = new SuccinctobjP(name, array, otype, type);
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
        return array->get_num_bits_per_vector();
    }

    int globalSize() const override{
        if(next == nullptr){
            return element_size();
        }
        return next->globalSize() + element_size();
    }

    int resize(int n) override{
        assert(is_head);
        array->resize_num_bits_per_vector(n);
        return array->get_total_num_bits();
    }

    VarStoreElementTrait* setBit(size_t i, int val) override {
        assert(val == 0 || val == 1);
        if(is_head) {
            array->setBit(i, val);
        }

        if(i<array->get_num_bits_per_vector()){
            return this;
        }else{
            Assert(next != nullptr, "bad bad");
            return next->setBit(i-array->get_num_bits_per_vector(), val);
        }
    }

    int getInt() const override {
        return array->get(index);
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
        SuccinctobjP* at = this;
        for(int i = 0;i<arr->size();i++)
        {
            assert(at != nullptr);
            at->setVal(arr->at(i));
            at = at->next;
        }
    }

    ///Return false if SuccinctobjP did not have enough bits to be made equal to v.
    bool setValSafe(int v) override {
        size_t t = array->get_num_bits_per_vector();
        array->set(index, v);
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
        array->set(index, v);
        int len = 0;
        while(v != 0){
            len+=1;
            v = v >> 1;
        }
        array->resize_num_bits_per_vector(len);
    }
    void printBit(ostream& out) const override{
        assert(is_head);
        out << array->to_bit_string();
    }
    void printContent(ostream& out) const override{
        out << getInt();
        if(next!= nullptr){ out<<"|"; next->printContent(out); }
    }

    bool increment() override{
        assert(is_head);
        return array->increment();
    }

    /**
    If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
    */
    void makeRandom(float sparseDeg, int n=10) override{
        int P  = 10000;
        int q = P*sparseDeg;

        if(n > 0 || (rand() % P) < q ){
            for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
                array->setBit(index, i, (rand() & 0x3) > 0? -1 : 1);
            }
        }else{
            for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
                array->setBit(index, i, 0);
            }
        }
        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
    }

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
            array->setBit(index, i, (rand() & 0x3) > 0? 0 : 1);
        }
        if(next!= nullptr){ next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
            array->setBit(index, i, 0);
        }
        if(next!= nullptr){ next->zeroOut(); }
    }

    bool get_is_array() const override {
        return is_array;
    }

    void relabel(const string new_name) override {
        SuccinctobjP* at = this;
        string prev_name = at->name;
        do {
            assert(at->name == prev_name);
            at->name = new_name;
            at = at->next;
        }while(at != nullptr);
    }

    void populate_multi_mother_nodeForINode(
            vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const override;

    void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const override;

    void append_vals(vector<int>& out) const override;

};

#endif

#endif //SKETCH_OBJP_H
