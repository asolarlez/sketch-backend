//
// Created by Kliment Serafimov on 7/8/22.
//

#include "objP.h"
#include "FloatSupport.h"
#include "DagOptim.h"

#ifdef USE_objP
void objP::populate_multi_mother_nodeForINode(vector<bool_node *>& multi_mother, DagOptim *for_cnodes, int nbits, const FloatManager& floats) const {
    auto val = this;
    while(val != nullptr){
        bool_node* cnst;
        assert(val->element_size() == nbits);
        if(nbits==1){
            cnst= for_cnodes->getCnode( val->getInt() ==1 );
        }else{
            if (otype == OutType::FLOAT_ARR) {
                cnst = for_cnodes->getCnode( floats.getFloat(val->getInt()) );
            }
            else {
                cnst = for_cnodes->getCnode(val->getInt());
            }
        }
        while(multi_mother.size()< val->index){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = val->next;
    }
}

void objP::populate_multi_mother_nodeForFun(
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

        while(multi_mother.size()< val->index){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = val->next;
    }
}

void objP::append_vals(vector<int>& out) const {
    out.insert(out.end(), vals.begin(), vals.end());
}
#endif

#ifndef USE_objP
void VarStoreElementIndexView::populate_multi_mother_nodeForINode(vector<bool_node *>& multi_mother, DagOptim *for_cnodes, int nbits, const FloatManager& floats) const {
    auto val = this;
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
        while(multi_mother.size()< val->index){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = val->next;
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

        while(multi_mother.size()< val->index){
            multi_mother.push_back( for_cnodes->getCnode(0) );
        }
        multi_mother.push_back( cnst );
        val = val->next;
    }
}

void VarStoreElementIndexView::append_vals(vector<int>& out) const {
    vector<int> vals;
    for(int i = 0; i<array()->get_num_bits_per_vector(); i++)
    {
        vals.push_back(array()->get_bit(index, i));
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
    assert(is_array());
    Assert(start < exclusive_end, "Empty arr");
    assert(index == start);
    index = start;
    if(array()->get_num_vectors() <= index) {
        array()->push_back();
    }
    if(start+1 < exclusive_end){
        if(next == nullptr){
            next = new VarStoreElementIndexView(index+1, parent);
        }
        next->makeArr(start+1, exclusive_end);
    }else{
        AssertDebug(next == nullptr, "MUST NOT CONTRACT ARRAY!!!");
    }
}

int VarStoreElementIndexView::arrSize() {
    assert(is_array());
    if(next==nullptr){
        return 1;
    }else{
        return next->arrSize() + 1;
    }
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
        array()->set_bit(index, local_bit_id, val);
    }

    if(local_bit_id<array()->get_num_bits_per_vector()){
        return this;
    }else{
        AssertDebug(false, "REFACTOR THIS TO NOT ITERATE THOUGH THE ARRAY. YOU CAN JUMP TO IMMEDIATELY.")
        Assert(next != nullptr, "bad bad");
        return next->set_bit_helper(local_bit_id-array()->get_num_bits_per_vector(), val, false);
    }
}

int VarStoreElementIndexView::getInt(int idx) const {
    assert(!is_array());
    if(this->index==idx){
        return getInt();
    }else{
        if(next != nullptr){ return next->getInt(idx); }
        else {return -1;}
    }
    Assert(false,"Control shouldn't reach here");
}

#endif
