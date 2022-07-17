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

    virtual VarStoreElementTrait* set_bit(size_t i, int val) {
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

class BitMetaVectorTrait {
public:
//    SuccinctBitMatrixTrait(int _num_vectors, int _num_bits_per_vector) {
//        AssertDebug(false, "not implemented.");
//    }
//    explicit SuccinctBitMatrixTrait(const SuccinctBitMatrixTrait* to_copy) {
//        AssertDebug(false, "not implemented.");
//    }

    virtual size_t get_num_vectors() const {
        AssertDebug(false, "not implemented.");
    }

    virtual int get_num_bits_per_vector() const {
        AssertDebug(false, "not implemented.");
    }

    virtual void resize_num_bits_per_vector(int new_num_bits_per_vector)
    {
        AssertDebug(false, "not implemented.");
    }

    virtual int get_total_num_bits() {
        AssertDebug(false, "not implemented.");
    }

    virtual bool increment()
    {
        AssertDebug(false, "not implemented.");
    }

    virtual int get_vector_as_int(size_t idx) const {
        AssertDebug(false, "not implemented.");
    }

    virtual void set_vector_from_int(size_t idx, int val) {
        AssertDebug(false, "not implemented.");
    }

//    virtual void set_bit(size_t bit_id, int val) {
//        AssertDebug(false, "not implemented.");
//    }

    virtual void set_bit(size_t word_id, size_t local_bit_id, int val) {
        AssertDebug(false, "not implemented.");
    }

    virtual bool get_bit(size_t idx, size_t bit_id) const {
        AssertDebug(false, "not implemented.");
    }

    virtual void push_back() {
        AssertDebug(false, "not implemented.");
    }

//    auto begin() const {
//        AssertDebug(false, "not implemented.");
//    }
//    auto end() const {
//        AssertDebug(false, "not implemented.");
//    }

    virtual void clear() {
        AssertDebug(false, "not implemented.");
    }

    virtual string to_bit_string()
    {
        AssertDebug(false, "ARE YOU SURE YOU WANT TO USE THIS VERSION?");
        string ret;
        for(int i = 0;i<get_num_vectors();i++)
        {
            if(i >= 1)
            {
                ret += "|";
            }
            string bit_string;
            for(int j = 0;j<get_num_bits_per_vector(); j++)
            {
                bit_string += (char)((int)'0'+get_bit(i, j));
            }
            reverse(bit_string.begin(), bit_string.end());
            ret += bit_string;
        }
        return ret;
    }

    virtual const vector<int> *as_vector_int_pointer() const {
        AssertDebug(false, "not implemented.");
    }
};

class BitMetaVector_rep_VectorInt: public BitMetaVectorTrait
{
    static const int max_num_bits = 30;
    int num_bits_per_vector;
    int total_num_bits;
    vector<int> vector_of_vectors;
public:
    BitMetaVector_rep_VectorInt(int _num_vectors, int _num_bits_per_vector):
            num_bits_per_vector(_num_bits_per_vector), total_num_bits(_num_vectors*_num_bits_per_vector) {
        assert(num_bits_per_vector <= max_num_bits);
        vector_of_vectors = vector<int>(_num_vectors, 0);
    }
    explicit BitMetaVector_rep_VectorInt(const BitMetaVector_rep_VectorInt* to_copy):
            num_bits_per_vector(to_copy->num_bits_per_vector), total_num_bits(to_copy->total_num_bits) {
        vector_of_vectors = to_copy->vector_of_vectors;
    }

    size_t get_num_vectors() const override {
        return vector_of_vectors.size();
    }

    int get_num_bits_per_vector() const override {
        return num_bits_per_vector;
    }

    void resize_num_bits_per_vector(int new_num_bits_per_vector) override
    {
        assert(new_num_bits_per_vector >= num_bits_per_vector);
        assert(new_num_bits_per_vector <= max_num_bits);
        num_bits_per_vector = new_num_bits_per_vector;
    }

    int get_total_num_bits() override {
        return total_num_bits;
    }

    bool increment() override
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

    int get_vector_as_int(size_t idx) const override {
        assert(0 <= idx && idx < vector_of_vectors.size());
        return vector_of_vectors[idx];
    }

    void set_vector_from_int(size_t idx, int val) override {
        assert(0 <= idx && idx < vector_of_vectors.size());
        vector_of_vectors[idx] = val;
    }

//    void set_bit(size_t bit_id, int val) override {
//        assert(val ==  0 || val == 1);
//        int word_id = bit_id/num_bits_per_vector;
//        int local_bit_id = bit_id - word_id*num_bits_per_vector;
//        set_bit(word_id, local_bit_id, val);
//    }

    void set_bit(size_t word_id, size_t local_bit_id, int val) override {
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

    bool get_bit(size_t idx, size_t bit_id) const override
    {
        assert(idx < vector_of_vectors.size());
        assert(bit_id < num_bits_per_vector);
        return (vector_of_vectors[idx] & (1<<bit_id)) != 0;
    }

    void push_back() override {
        vector_of_vectors.push_back(0);
    }

    auto begin() const {
        return vector_of_vectors.begin();
    }
    auto end() const {
        return vector_of_vectors.end();
    }

    void clear() override {
        vector_of_vectors.clear();
        delete this;
    }

    string to_bit_string() override
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

    const vector<int> *as_vector_int_pointer() const override {
        return &vector_of_vectors;
    }
};

template<typename T>
class VectorTrait
{
public:
    virtual T get(size_t idx) const = 0;
    virtual void set(size_t idx, T) = 0;
    virtual size_t get_num_bits() const = 0;
};

class BitVector : public VectorTrait<bool>
{
    typedef uint32_t WORD_TYPE;
    size_t num_bits = 0;
    vector<WORD_TYPE> bits;
public:
    static const int word_size_bits = 5;
    static const int num_bits_in_a_byte = 8;
    static_assert(sizeof(WORD_TYPE)*num_bits_in_a_byte == 1<<word_size_bits, "WORD_TYPE and word_size_bits are inconsistent.");
    static const int word_size = 1<<word_size_bits;
    static const int low_order_bits_mask = (1<<word_size_bits)-1;
    void clear()
    {
        bits.clear();
    }
    BitVector() = default;
    explicit BitVector(size_t _num_bits, int init_val = 0): num_bits(_num_bits)
    {
        if(init_val == 0) {
            bits = vector<WORD_TYPE>((num_bits + word_size - 1) >> word_size_bits, 0);
        } else
        {
            assert(init_val == 1);
            WORD_TYPE all_ones = ~0;
            assert(__builtin_popcount(all_ones) == word_size);
            bits = vector<WORD_TYPE>((num_bits + word_size - 1) >> word_size_bits, ~0);
        }
    }

    void expand(size_t expansion_size) {
        while(bits.size()*word_size < num_bits+expansion_size) {
            bits.push_back(0);
        }
        num_bits+=expansion_size;
    }

    bool operator [](const size_t idx) const
    {
        return get(idx);
    }

    bool get(size_t idx) const override {
        assert(idx < num_bits);
        bool ret = (bits[idx >> word_size_bits] & ((WORD_TYPE)1 << (idx & low_order_bits_mask))) != 0;
        return ret;
    }
    void set(size_t idx,  bool val) override {
        assert(idx < num_bits);
        int at_word = idx >> word_size_bits;
        if(val) {
            bits[at_word] |= ((WORD_TYPE)1 << (idx & low_order_bits_mask));
        }
        else {
            bits[at_word] &= ~((WORD_TYPE)1 << (idx & low_order_bits_mask));
        }
    }

    size_t get_num_bits() const override {
        return num_bits;
    }

    // Minimum required for range-for loop
    struct Iterator {
        size_t at_idx;
        const BitVector* _this;
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
        size_t at_idx = 0;
        return Iterator{at_idx, this};
    }
    auto end() const {
        return Iterator{num_bits, this};
    }
};

class BitMetaVector_rep_BitVector: public BitMetaVectorTrait
{
    size_t num_bits_per_vector = 0;
    size_t total_num_bits = 0;
    size_t num_vectors = 0;
    BitVector vector_of_vectors;
    bool invariant(const bool check_invariant = true) const {
        return !check_invariant ||
               (
                       num_vectors == vector_of_vectors.get_num_bits() / num_bits_per_vector &&
                       total_num_bits == vector_of_vectors.get_num_bits()
               );
    }
public:
    BitMetaVector_rep_BitVector(int _num_vectors, int _num_bits_per_vector):
            num_bits_per_vector(_num_bits_per_vector), total_num_bits(_num_vectors*_num_bits_per_vector), num_vectors(_num_vectors) {
        vector_of_vectors = BitVector(total_num_bits, 0);
        assert(invariant());
    }
    explicit BitMetaVector_rep_BitVector(const BitMetaVector_rep_BitVector* to_copy):
            num_bits_per_vector(to_copy->num_bits_per_vector), total_num_bits(to_copy->total_num_bits), num_vectors(to_copy->num_vectors) {
        vector_of_vectors = to_copy->vector_of_vectors;
        assert(invariant());
    }

public:

    size_t get_num_vectors() const override {
        assert(invariant());
        return num_vectors;
    }

    int get_num_bits_per_vector() const override {
        assert(invariant());
        return num_bits_per_vector;
    }

    void resize_num_bits_per_vector(int new_num_bits_per_vector) override {
        assert(new_num_bits_per_vector > get_num_bits_per_vector());
        BitVector new_vector_of_vectors(get_num_vectors()*new_num_bits_per_vector);
        for(size_t i = 0;i<get_num_vectors();i++) {
            for(size_t j = 0;j<get_num_bits_per_vector();j++) {
                new_vector_of_vectors.set(i*new_num_bits_per_vector + j, get_bit(i, j));
            }
        }

        num_bits_per_vector = new_num_bits_per_vector;

        vector_of_vectors = new_vector_of_vectors;
        num_bits_per_vector = new_num_bits_per_vector;

        total_num_bits = num_vectors * num_bits_per_vector;
        total_num_bits = get_num_vectors()*get_num_bits_per_vector();

        assert(invariant());
    }

    int get_total_num_bits() override {
        assert(invariant());
        return total_num_bits;
    }

    bool increment() override
    {
        AssertDebug(false, "TODO.");
        return false;
    }

    int get_vector_as_int(size_t idx) const override {
        assert(invariant());
        int ret = 0;
        for(size_t i = 0;i<get_num_bits_per_vector();i++)
        {
            ret |= get_bit(idx, i) << i;
        }
        return ret;
    }

    void set_vector_from_int(size_t idx, int val) override {
        assert(invariant());
        assert(idx < get_num_vectors());
        assert(val >= 0);
        assert(get_num_bits_per_vector() < BitVector::word_size);

        for(int i = 0; i < get_num_bits_per_vector(); i++) {
            set_bit(idx, i, (val & (1<<i)) != 0);
        }

        int vector_as_int = get_vector_as_int(idx);
        assert(val == vector_as_int);
        assert(invariant());
    }

//    void set_bit(size_t bit_id, int val) override {
//        assert(val ==  0 || val == 1);
//        size_t word_id = bit_id/num_bits_per_vector;
//        size_t local_bit_id = bit_id - word_id*num_bits_per_vector;
//        set_bit(word_id, local_bit_id, val);
//    }

    void set_bit(size_t word_id, size_t local_bit_id, int val) override {
        assert(invariant());
        assert(word_id < get_num_vectors() && local_bit_id < get_num_bits_per_vector());
        vector_of_vectors.set(word_id*get_num_bits_per_vector() + local_bit_id, val);
        assert(invariant());
    }

    bool get_bit(size_t idx, size_t bit_id) const override
    {
        assert(invariant());
        assert(bit_id < get_num_bits_per_vector());
        return vector_of_vectors.get(idx*get_num_bits_per_vector() + bit_id);
    }

    void push_back() override {
        assert(invariant());
        vector_of_vectors.expand(num_bits_per_vector);
        total_num_bits += num_bits_per_vector;
        num_vectors ++;
        assert(invariant());
    }

    auto begin() const {
        assert(invariant());
        return vector_of_vectors.begin();
    }
    auto end() const {
        assert(invariant());
        return vector_of_vectors.end();
    }

    void clear() override {
        assert(invariant());
        vector_of_vectors.clear();
        delete this;
    }

    string to_bit_string() override
    {
        assert(invariant());
        string ret;
        for(int i = 0;i<get_num_vectors();i++)
        {
            if(i >= 1)
            {
                ret += "|";
            }
            string bit_string;
            for(int j = 0;j<get_num_bits_per_vector(); j++)
            {
                bit_string += (char)((int)'0'+get_bit(i, j));
            }
            reverse(bit_string.begin(), bit_string.end());
            ret += bit_string;
        }
        return ret;
    }

    const vector<int> *as_vector_int_pointer() const override {
        assert(invariant());
        assert(get_num_bits_per_vector() <= BitVector::word_size);

        vector<int>* ret = new vector<int>();

        for(int i = 0; i<get_num_vectors();i++)
        {
            ret->push_back(get_vector_as_int(i));
        }

        return ret;
    }
};

class objP;

class VarStoreElementIndexView: public VarStoreElementTrait {
    objP* parent = nullptr;
    BitMetaVectorTrait* array() const;
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

    const vector<int>* as_vector_int_pointer() const
    {
        return array()->as_vector_int_pointer();
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
//            array(new SuccinctBitMatrix(1, _size)), is_head(true), index(0), next(nullptr),
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

    VarStoreElementTrait* set_bit_helper(size_t i, int val, bool is_head = true);

public:

    VarStoreElementIndexView(const VarStoreElementIndexView& old, objP* _parent) {
        init_from_old(old, _parent);
    }

    void makeArr(int start, int end) override;

    int arrSize() override;

    int element_size() const override;

    int globalSize() const override;

    int resize(int n) override;

    VarStoreElementTrait* set_bit(size_t i, int val) override {
        return set_bit_helper(i, val, true);
    }

    int getInt() const override {
        return array()->get_vector_as_int(index);
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

private:
    bool does_it_have_enough_bits(int v)
    {
        size_t t = array()->get_num_bits_per_vector();
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
    int get_num_bits(int v)
    {
        int len = 0;
        while(v != 0){
            len+=1;
            v = v >> 1;
        }
        return len;
    }
public:

    ///Return false if SuccinctobjP did not have enough bits to be made equal to v.
    bool setValSafe(int v) override {
        size_t t = array()->get_num_bits_per_vector();
        bool it_has_enough_bits = does_it_have_enough_bits(v);
        int lb_num_bits_necessary = array()->get_num_bits_per_vector();
        if(!it_has_enough_bits) {
            lb_num_bits_necessary = get_num_bits(v);
            array()->resize_num_bits_per_vector(lb_num_bits_necessary);
        }
        array()->set_vector_from_int(index, v);
        int len = 0;
        bool ret = true;
        while(v != 0){
            len+=1;
            v = v >> 1;
            if(len==t && v != 0) {
                ret = false;
                break;
            }
        }
        assert(ret == it_has_enough_bits);
        return ret;
    }

    void setVal(int v) override {
        array()->set_vector_from_int(index, v);
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
                array()->set_bit(index, i, (rand() & 0x3) > 0? -1 : 1);
            }
        }else{
            for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
                array()->set_bit(index, i, 0);
            }
        }
        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
    }

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->set_bit(index, i, (rand() & 0x3) > 0? 0 : 1);
        }
        if(next!= nullptr){ next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->set_bit(index, i, 0);
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

    typedef BitMetaVector_rep_BitVector BitMetaVector_rep_CHOOSE;
    bool is_array;
    //if is_array == true; then array.size() == number of elements in the array and array holds the array.
    //if is_array == false; then array.size() == 1 and array[0] holds the value.
    BitMetaVector_rep_CHOOSE* array = nullptr;

//    //ONLY CALLED FROM SuccinctobjP CONSTRUCTOR TO PUSH A NEXT ELEMENT IN THE ARRAY.
//    VarStoreElement(string _name, SuccinctBitMatrix* _array, const OutType* _otype,
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
    BitMetaVectorTrait* get_array() const {
        return (BitMetaVectorTrait*)array;
    }

    const vector<int>* as_vector_int_pointer() const
    {
        return array->as_vector_int_pointer();
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
            array(new BitMetaVector_rep_CHOOSE(1, _size)),
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

//    VarStoreElement(const VarStoreElement& old, SuccinctBitMatrix* array):
//            array(array), is_head(false), is_array(old.is_array),
//            VarStoreElementHeader(old){
//        assert(!old.is_head);
//        if(old.next != nullptr){
//            next=new VarStoreElement(*old.next, array);
//        }
//        else{next=nullptr;}
//    }

    objP(const objP& old):
            array(new BitMetaVector_rep_CHOOSE(old.array)), is_array(old.is_array),
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

//    VarStoreElementTrait* set_bit(size_t i, int val) override {
//        assert(val == 0 || val == 1);
//        if(is_head) {
//            array->set_bit(i, val);
//        }
//
//        if(i<array->get_num_bits_per_vector()){
//            return this;
//        }else{
//            Assert(next != nullptr, "bad bad");
//            return next->set_bit(i-array->get_num_bits_per_vector(), val);
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
//                array->set_bit(index, i, (rand() & 0x3) > 0? -1 : 1);
//            }
//        }else{
//            for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//                array->set_bit(index, i, 0);
//            }
//        }
//        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
//    }
//
//    void makeRandom() override{/* Bias towards zeros */
//        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//            array->set_bit(index, i, (rand() & 0x3) > 0? 0 : 1);
//        }
//        if(next!= nullptr){ next->makeRandom(); }
//    }
//    void zeroOut() override{/* Bias towards zeros */
//        for(size_t i=0; i<array->get_num_bits_per_vector(); ++i){
//            array->set_bit(index, i, 0);
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
