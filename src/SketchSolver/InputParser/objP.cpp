//
// Created by Kliment Serafimov on 7/8/22.
//

#include "objP.h"
#include "FloatSupport.h"
#include "DagOptim.h"

#ifndef USE_objP
void VarStoreElementIndexView::populate_multi_mother_nodeForINode(vector<bool_node *>& multi_mother, DagOptim *for_cnodes, int nbits, const FloatManager& floats) const {
    const VarStoreElementIndexView* val = this;
    while(val != nullptr){
        bool_node* cnst;
        assert(val->element_size() == nbits);
        if(nbits==1){
            cnst= for_cnodes->getCnode( val->getInt() ==1 );
        }else{
            if (parent->otype == OutType::FLOAT_ARR) {
                cnst = for_cnodes->getCnode( floats.getFloat(val->getInt()) );
            }
            else {
                cnst = for_cnodes->getCnode(val->getInt());
            }
        }
        while(multi_mother.size()< val->get_index()){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = (VarStoreElementIndexView*)val->get_next();
    }
}

void VarStoreElementIndexView::populate_multi_mother_nodeForFun(
        vector<bool_node *> &multi_mother, DagOptim *for_cnodes, int nbits) const
{
    auto val = this;
    while(val != nullptr){
        bool_node* cnst;
        if(nbits==1){
            cnst= for_cnodes->getCnode( val->getInt() ==1 );
        }else{
            cnst= for_cnodes->getCnode( val->getInt() );
        }

        while(multi_mother.size()< val->get_index()){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = (VarStoreElementIndexView*)val->get_next();
    }
}

void VarStoreElementIndexView::append_vals(vector<int>& out) const {
    vector<int> vals;
    assert(get_index() == 0);
    for(int i = 0; i<array()->get_num_bits_per_vector(); i++) {
        vals.push_back(array()->get_bit(get_index(), i));
    }
    out.insert(out.end(), vals.begin(), vals.end());
}

BitMetaVectorTrait *VarStoreElementIndexView::array() const {
    assert(parent != nullptr);
    return parent->get_array();
}

bool VarStoreElementIndexView::is_array() const {
    assert(parent != nullptr);
    return parent->get_is_array();
}

void VarStoreElementIndexView::makeArr(int start, int exclusive_end){
    assert(start >= 0);
    assert(is_array());
    Assert(start < exclusive_end, "Empty arr");
    assert(get_index() == start);
    assert(get_index() >= 0);
    if(array()->get_num_vectors() <= get_index()) {
        AssertDebug(false, "YOU WANT TO BE PRE-EMPTIVE IN USING array()->push_back()");
        array()->push_back();
    }
    if(start+1 < exclusive_end){
        if(!has_next()) {
            array()->push_back();
        }
        assert(has_next());
        get_next()->makeArr(start+1, exclusive_end);
    }
}

int VarStoreElementIndexView::arrSize() {
    return array()->get_num_vectors();
}

int VarStoreElementIndexView::element_size() const {
    return array()->get_num_bits_per_vector();
}

int VarStoreElementIndexView::globalSize() const {
    return array()->get_total_num_bits();
}

int VarStoreElementIndexView::resize(int n) {
    array()->resize_num_bits_per_vector(n);
    return array()->get_total_num_bits();
}

VarStoreElementTrait *VarStoreElementIndexView::set_bit_helper(size_t local_bit_id, int val, bool is_head) {
    assert(val == 0 || val == 1);
    if(is_head) {
        array()->set_bit(get_index(), local_bit_id, val);
    }

    if(local_bit_id<array()->get_num_bits_per_vector()){
        return this;
    }else{
        AssertDebug(false, "REFACTOR THIS TO NOT ITERATE THOUGH THE ARRAY. YOU CAN JUMP TO IMMEDIATELY.");
        AssertDebug(has_next(), "MUST HAVE NEXT, OTHERWISE set_bit_id IS OUT OF BOUNDS!!!");
        return ((VarStoreElementIndexView*)get_next())->set_bit_helper(local_bit_id-array()->get_num_bits_per_vector(), val, false);
    }
}

int VarStoreElementIndexView::getInt(int idx) const {
    return array()->get_vector_as_int(idx);
}

#endif
