//
// Created by kliment on 6/28/21.
//

#include "SkVal.h"


int local_get_nbits(vector<pair<int, int> >* vec)
{
    assert(vec->size() >= 1);
    int ret = vec->at(0).second;
    for(int i= 1;i<vec->size();i++)
    {
        assert(ret == vec->at(i).second);
    }
    return ret;
}


vector<int>* local_get_first(vector<pair<int, int> >* vec)
{
    vector<int>* ret = new vector<int>();
    for(int i= 0;i<vec->size();i++)
    {
        ret->push_back(vec->at(i).first);
    }
    return ret;
}