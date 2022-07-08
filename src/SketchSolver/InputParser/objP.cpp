//
// Created by Kliment Serafimov on 7/8/22.
//

#include "objP.h"
#include "FloatSupport.h"
#include "DagOptim.h"

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
