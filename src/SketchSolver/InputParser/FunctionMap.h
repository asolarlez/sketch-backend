//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAP_H
#define SKETCH_SOURCE_FUNCTIONMAP_H

#include <string>
#include <map>
#include "BooleanDAG.h"
#include "FunctionMapTransformerLanguage.h"

using namespace std;

class SketchFunction;

using namespace FMTL;

class FunctionMap: public map<string, SketchFunction*>, public FunctionMapTransformer
{
public:
    FunctionMap() = default;

    const map<string, BooleanDAG *> * to_boolean_dag_map() const;

    const vector<string>& get_function_names() const
    {
        vector<string>* ret = new vector<string>();
        for(auto it: *this) {
            ret->push_back(it.first);
        }
        return *ret;
    }

    void insert(const string& name, SketchFunction* sketch_function_map);

    SketchFunction* operator[](const string& name)
    {
        auto it = find(name);
        assert(it != end());
        return it->second;
    }

    void erase(const string& name, bool update_transformer = true)
    {
        if(update_transformer) {
            FunctionMapTransformer::erase(name);
        }
        auto it = find(name);
        if(it != end()) {
            map<string, SketchFunction *>::erase(it);
        }
    }
};


#endif //SKETCH_SOURCE_FUNCTIONMAP_H
