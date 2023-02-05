//
// Created by Kliment Serafimov on 7/8/22.
//

#ifndef SKETCH_OBJP_H
#define SKETCH_OBJP_H

#include "BooleanNodes.h"
#include "string.h"

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
    mutable string name;
    const string original_name;
    string source_dag_name;
public:
    const OutType* const otype;

    VarStoreElementHeader(
            bool_node::Type _type, string _name,
            const OutType* _otype, string _original_name,
            string _source_dag_name):
            type(_type), name(std::move(_name)), otype(_otype), original_name(std::move(_original_name)), source_dag_name(std::move(_source_dag_name)) {
        assert(!original_name.empty() && !source_dag_name.empty());
    }

    VarStoreElementHeader(const VarStoreElementHeader& old) = default;

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

    void override_source_dag_name(const string& _source_dag_name) {
        if(type == bool_node::CTRL)
            AssertDebug(!_source_dag_name.empty(), "check why this fails and act accordingly.")
        source_dag_name = _source_dag_name;
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

    virtual ~VarStoreElementTrait() = default;

    virtual void makeArr(int start, int end) {
        AssertDebug(false, "not implemented.");
    }

    virtual int arrSize() const {
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

    virtual void set_bit(size_t i, int val) {
        AssertDebug(false, "not implemented.");
    }

    virtual inline int getInt() const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual inline int getInt(int idx) const {
        AssertDebug(false, "not implemented.");
        return 0;
    }

    virtual void setArr(const vector<int> *arr) {
        AssertDebug(false, "not implemented.");
    }

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

    virtual void relabel(const string new_name) const {
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

#include "HyperParams.h"

class BitMetaVectorTrait {
protected:
    typedef MetaBaseType WORD_TYPE;
    static const int max_num_bits = 64;
    static_assert(sizeof(WORD_TYPE)*8 == max_num_bits);
public:
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

    virtual WORD_TYPE get_vector_as_int(size_t idx) const {
        AssertDebug(false, "not implemented.");
    }

    virtual void set_vector_from_int(size_t idx, WORD_TYPE val) {
        AssertDebug(false, "not implemented.");
    }

//    virtual void set_bit(size_t bit_id, int val) {
//        AssertDebug(false, "not implemented.");
//    }

    virtual void set_bit(size_t word_id, size_t local_bit_id, bool val) {
        AssertDebug(false, "not implemented.");
    }

    virtual bool get_bit(size_t idx, size_t bit_id) const {
        AssertDebug(false, "not implemented.");
    }

    virtual void push_back() {
        AssertDebug(false, "not implemented.");
    }

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
};

class BitMetaVector_rep_VectorInt: public BitMetaVectorTrait
{
    int num_bits_per_vector;
    int total_num_bits;
    vector<WORD_TYPE> vector_of_vectors;
public:

    const vector<WORD_TYPE>& get_vector_of_ints() const
    {
        return vector_of_vectors;
    }

    void copy_bits(const BitMetaVector_rep_VectorInt *other) {
        assert(num_bits_per_vector == other->num_bits_per_vector);
        assert(total_num_bits == other->total_num_bits);
        vector_of_vectors = other->vector_of_vectors;
    }

    string to_string()
    {
        string ret = "{";
        for(int i = 0;i<vector_of_vectors.size();i++)
        {
            if(i > 0)
            {
                ret += ", ";
            }
            ret += std::to_string(vector_of_vectors[i]);
//            string bitvector;
//            for(int j = 0;j<num_bits_per_vector;j++)
//            {
//                bitvector += (char)((int)'0'+((vector_of_vectors[i] & (1<<j)) != 0));
//            }
//            ret += bitvector;
        }
        ret += "}";
        return ret;
    }
//
    BitMetaVector_rep_VectorInt(int _num_vectors, int _num_bits_per_vector):
            num_bits_per_vector(_num_bits_per_vector), total_num_bits(_num_vectors*_num_bits_per_vector) {
        assert(num_bits_per_vector <= max_num_bits);
        vector_of_vectors = vector<WORD_TYPE>(_num_vectors, 0);
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
        assert(new_num_bits_per_vector <= max_num_bits);
        if(new_num_bits_per_vector > num_bits_per_vector) {
            num_bits_per_vector = new_num_bits_per_vector;
            total_num_bits = num_bits_per_vector * get_num_vectors();
        }
    }

    int get_total_num_bits() override {
        return total_num_bits;
    }

    bool increment() override
    {
        const WORD_TYPE max_num = (1<<num_bits_per_vector)-1;
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

    WORD_TYPE get_vector_as_int(size_t idx) const override {
        return vector_of_vectors[idx];
    }

    void set_vector_from_int(size_t idx, WORD_TYPE val) override {
        vector_of_vectors[idx] = val;
    }

//    void set_bit(size_t bit_id, int val) override {
//        assert(val ==  0 || val == 1);
//        int word_id = bit_id/num_bits_per_vector;
//        int local_bit_id = bit_id - word_id*num_bits_per_vector;
//        set_bit(word_id, local_bit_id, val);
//    }

    void set_bit(size_t word_id, size_t local_bit_id, bool val) override {
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
        return (vector_of_vectors[idx] & (1<<bit_id)) != 0;
    }

    void push_back() override {
        vector_of_vectors.push_back(0);
        total_num_bits = num_bits_per_vector * get_num_vectors();
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
        }
        else
        {
            assert(init_val == 1);
            WORD_TYPE all_ones = ~0;
//            assert(__builtin_popcount(all_ones) == word_size);
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
        bool ret = (bits[idx >> word_size_bits] & ((WORD_TYPE)1 << (idx & low_order_bits_mask))) != 0;
        return ret;
    }
    void set(size_t idx,  bool val) override {
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
    size_t bits_in_capacity_per_vector = 0;
    size_t capacity_per_vector = 0;
    size_t num_bits_per_vector = 0;
    size_t total_num_bits = 0;
    size_t num_vectors = 0;
    BitVector vector_of_vectors;

public:
    BitMetaVector_rep_BitVector(int _num_vectors, int _num_bits_per_vector):
            num_bits_per_vector(_num_bits_per_vector), total_num_bits(_num_vectors*_num_bits_per_vector), num_vectors(_num_vectors), capacity_per_vector(1) {
        vector_of_vectors = BitVector(total_num_bits, 0);
        while(capacity_per_vector < num_bits_per_vector) {
            capacity_per_vector*=2;
            bits_in_capacity_per_vector++;
        }
    }
    explicit BitMetaVector_rep_BitVector(const BitMetaVector_rep_BitVector* to_copy):
            num_bits_per_vector(to_copy->num_bits_per_vector), total_num_bits(to_copy->total_num_bits), num_vectors(to_copy->num_vectors),
            capacity_per_vector(to_copy->capacity_per_vector), bits_in_capacity_per_vector(to_copy->bits_in_capacity_per_vector) {
        vector_of_vectors = to_copy->vector_of_vectors;
    }

public:

    inline size_t get_num_vectors() const override {
        return num_vectors;
    }

    inline int get_num_bits_per_vector() const override {
        return num_bits_per_vector;
    }

    void resize_num_bits_per_vector(int new_num_bits_per_vector) override {
        assert(new_num_bits_per_vector > get_num_bits_per_vector());
        if(capacity_per_vector < new_num_bits_per_vector) {
            while (capacity_per_vector < new_num_bits_per_vector) {
                capacity_per_vector *= 2;
                bits_in_capacity_per_vector++;
            }
            BitVector new_vector_of_vectors(get_num_vectors() * capacity_per_vector);
            for (size_t i = 0; i < get_num_vectors(); i++) {
                for (size_t j = 0; j < get_num_bits_per_vector(); j++) {
                    new_vector_of_vectors.set((i << bits_in_capacity_per_vector) + j, get_bit(i, j));
                }
            }

            vector_of_vectors = new_vector_of_vectors;
        }

        num_bits_per_vector = new_num_bits_per_vector;

        total_num_bits = num_vectors * num_bits_per_vector;
    }

    int get_total_num_bits() override {
        return total_num_bits;
    }

    bool increment() override {
        AssertDebug(false, "TODO.");
        return false;
    }

    WORD_TYPE get_vector_as_int(size_t idx) const override {
        if(num_bits_per_vector == 1) {
            return get_bit(idx, 0);
        }
        int ret = 0;
        for(size_t i = 0;i<num_bits_per_vector;i++)
        {
            ret |= get_bit(idx, i) << i;
        }
        return ret;
    }

    void set_vector_from_int(size_t idx, WORD_TYPE val) override {
        for(int i = 0; i < get_num_bits_per_vector(); i++) {
            set_bit(idx, i, (val & (1<<i)) != 0);
        }
    }

//    void set_bit(size_t bit_id, int val) override {
//        assert(val ==  0 || val == 1);
//        size_t word_id = bit_id/num_bits_per_vector;
//        size_t local_bit_id = bit_id - word_id*num_bits_per_vector;
//        set_bit(word_id, local_bit_id, val);
//    }

    void set_bit(size_t idx, size_t local_bit_id, bool val) override {
        vector_of_vectors.set((idx << bits_in_capacity_per_vector) | local_bit_id, val);
    }

    bool get_bit(size_t idx, size_t local_bit_id) const override {
        return vector_of_vectors.get((idx << bits_in_capacity_per_vector) | local_bit_id);
    }

    void push_back() override {
        vector_of_vectors.expand(num_bits_per_vector);
        total_num_bits += num_bits_per_vector;
        num_vectors++;
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
};

class objP;

class VarStoreElementIndexView: public VarStoreElementTrait {
    objP* parent = nullptr;
    BitMetaVectorTrait* array() const;
    bool is_array() const;
    mutable int __index = -1;
    mutable bool reached_end = false;


protected:
    explicit VarStoreElementIndexView(int _index, objP* _parent): __index(_index), parent(_parent) {
        assert(__index != -1);
    }


public:

    int arrSize() const override {
        return array()->get_num_vectors();
    }

    int element_size() const override  {
        return array()->get_num_bits_per_vector();
    }

    int globalSize() const override {
        return array()->get_total_num_bits();
    }

    int resize(int n) override  {
        array()->resize_num_bits_per_vector(n);
        return array()->get_total_num_bits();
    }

    int _get_pure_index() const
    {
        return __index;
    }

    void _reset_pure_index() const
    {
        __index = 0;
    }

    int get_index() const override {
        if(__index == -1) {
            assert(reached_end);
            reached_end = false;
            __index = 0;
        }
        return __index;
    }

    bool has_next() const
    {
        assert(__index != -1);
        return __index + 1 < array()->get_num_vectors();
    }
private:
    mutable bool in_iter = false;
public:
    VarStoreElementTrait* get_next() const override {
        if(__index == 0)
        {
            in_iter = true;
        }
        __index++;
        if(__index >= array()->get_num_vectors())
        {
            __index = -1;
            reached_end = true;
            in_iter = false;
            return nullptr;
        }
        return (VarStoreElementTrait*)this;
    }

    int get_size() const override
    {
        return globalSize();
    }

protected:
    VarStoreElementIndexView(const VarStoreElementIndexView& old, objP* _parent) {
        assert(parent == nullptr);
        parent = _parent;
        __index = old.get_index();
        __index = 0;
        assert(__index == 0);
    }

public:


    void makeArr(int start, int exclusive_end) override;

    inline int getInt() const override {
        return array()->get_vector_as_int(get_index());
    }
    void set_bit(size_t i, int val) override {
        assert(val == 0 || val == 1);
        int num_bits_per_vector = array()->get_num_bits_per_vector();
        int word_id = i / num_bits_per_vector;
        int local_bit_id = i - word_id * num_bits_per_vector;
        array()->set_bit(word_id, local_bit_id, val);
    }


    inline int getInt(int idx) const override
    {
        assert(0 <= idx);
        if(idx < array()->get_num_vectors()) {
            return array()->get_vector_as_int(idx);
        }
        else {
            return -1;
        }
    }

    void setArr(const vector<int> *arr) override {
        assert(is_array());
        VarStoreElementTrait* at = (VarStoreElementTrait*)this;
        for(int i = 0;i<arr->size();i++)
        {
            assert(at != nullptr);
            at->setVal(arr->at(i));
            at = at->get_next();
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

    ///Return false if objP did not have enough bits to be made equal to v.
    bool setValSafe(int v) override {
        size_t t = array()->get_num_bits_per_vector();
        bool it_has_enough_bits = does_it_have_enough_bits(v);
        int lb_num_bits_necessary = array()->get_num_bits_per_vector();
        if(!it_has_enough_bits) {
            lb_num_bits_necessary = get_num_bits(v);
            array()->resize_num_bits_per_vector(lb_num_bits_necessary);
        }
        array()->set_vector_from_int(get_index(), v);
        return it_has_enough_bits;
    }

    void setVal(int v) override {
        bool is_same = (v == array()->get_vector_as_int(0));
        array()->set_vector_from_int(get_index(), v);
        bool is_same_after = (v == array()->get_vector_as_int(0));
        assert(is_same_after);
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
        auto next = get_next();
        if(next != nullptr){ out<<"|"; next->printContent(out); }
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
                array()->set_bit(get_index(), i, (rand() & 0x3) > 0? -1 : 1);
            }
        }else{
            for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
                array()->set_bit(get_index(), i, 0);
            }
        }
        auto next = get_next();
        if(next!= nullptr){ next->makeRandom(sparseDeg, n-1); }
    }

    void makeRandom() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->set_bit(get_index(), i, (rand() & 0x3) > 0? 0 : 1);
        }
        auto next = get_next();
        if(next!= nullptr){next->makeRandom(); }
    }
    void zeroOut() override{/* Bias towards zeros */
        for(size_t i=0; i<array()->get_num_bits_per_vector(); ++i){
            array()->set_bit(get_index(), i, 0);
        }

        auto next = get_next();
        if(next!= nullptr){ next->zeroOut(); }
    }

    bool get_is_array() const override {
        return is_array();
    }

    void populate_multi_mother_nodeForINode(
            vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits, const FloatManager& floats) const override;

    void populate_multi_mother_nodeForFun(vector<bool_node*>& multi_mother, DagOptim* for_cnodes, int nbits) const override;

    void relabel(const string new_name) const override;

    void append_vals(vector<int>& out) const override
    {
        vector<int> vals;
        for(int i = 0; i<array()->get_num_bits_per_vector(); i++)
        {
            vals.push_back(array()->get_bit(get_index(), i));
        }
        out.insert(out.end(), vals.begin(), vals.end());
    }
};

class objP: public VarStoreElementIndexView, public VarStoreElementHeader {
public:
//    typedef BitMetaVector_rep_BitVector BitMetaVector_rep_CHOOSE;
    typedef BitMetaVector_rep_VectorInt BitMetaVector_rep_CHOOSE;

private:
    bool is_array;
    //if is_array == true; then array.size() == number of elements in the array and array holds the array.
    //if is_array == false; then array.size() == 1 and array[0] holds the value.
    BitMetaVector_rep_CHOOSE* array = nullptr;

public:
    BitMetaVectorTrait* get_array() const {
        return (BitMetaVectorTrait*)array;
    }

    const BitMetaVector_rep_CHOOSE* as_chosen_rep_pointer() const
    {
        return array;
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
    ~objP()
    {
        objP::clear();
    }
    objP(string  _name, int _size, const OutType* _otype,
         bool_node::Type _type, string _original_name="", string _source_dag_name=""):
            array(new BitMetaVector_rep_CHOOSE(1, _size)),
            VarStoreElementHeader(_type, std::move(_name), _otype, std::move(_original_name), std::move(_source_dag_name)),
            VarStoreElementIndexView(0, this) {

        assert(_otype != nullptr);
        if (_otype == OutType::INT_ARR || _otype == OutType::BOOL_ARR || _otype == OutType::FLOAT_ARR) {
            is_array = true;
            assert(_otype->isArr);
        } else {
            is_array = false;
            assert(!_otype->isArr);
        }
    }

    objP(const objP& old):
            array(new BitMetaVector_rep_CHOOSE(old.array)), is_array(old.is_array),
            VarStoreElementHeader(old), VarStoreElementIndexView(old,this){
    }

    objP operator=(const objP& old) {
        return objP(old);
    }

    int element_size() const override{
        return array->get_num_bits_per_vector();
    }

    int resize(int n) override{
        array->resize_num_bits_per_vector(n);
        return array->get_total_num_bits();
    }

    void printBit(ostream& out) const override{
        out << array->to_bit_string();
    }

    bool increment() override{
        return array->increment();
    }

    bool get_is_array() const override {
        return is_array;
    }

    void relabel(string new_name) const override {
        name = std::move(new_name);
    }

    string to_string()
    {
        if(is_array) {
            return array->to_string();
        } else
        {
            return std::to_string(array->get_vector_as_int(0));
        }
    }

     const BitMetaVector_rep_CHOOSE* get_array_as_supertype() {
        return array;
    }

    BitMetaVector_rep_CHOOSE* get_array_as_supertype_nonconst() {
        return array;
    }
};


#endif //SKETCH_OBJP_H
