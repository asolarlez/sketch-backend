//
// Created by kliment on 3/20/22.
//

#include "GenericFile.h"

GenericFile *GenericFile::produce_filter(std::function< bool(string) >& lambda_condition) {
    GenericFile* ret = new GenericFile(generator);
    for(int i = 0;i<size();i++)
    {
        if(lambda_condition(at(i)))
        {
            ret->push_back(at(i));
        }
    }
    return ret;
}
