//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAP_H
#define SKETCH_SOURCE_FUNCTIONMAP_H

#include <string>
#include <map>
#include "BooleanDAG.h"
#include "FunctionMapTransformerDag.h"

using namespace std;

namespace SL {
    class SketchFunction;
}

using namespace SL;
class ProgramEnvironment;

class FunctionMap: public map<string, SketchFunction*>, public FMTL::FunctionMapTransformer
{
    ProgramEnvironment* __program_environment;
public:
    FunctionMap(ProgramEnvironment* _program_environment);

    ProgramEnvironment* get_env();

    bool empty() const {
        if(map<string, SketchFunction*>::empty()) {
            assert(FMTL::FunctionMapTransformer::empty());
        }
        return map<string, SketchFunction*>::empty();
    }

    void clear_assert_num_shared_ptr_is_0();

    const map<string, BooleanDAG *> * to_boolean_dag_map() const;

    void populate_boolean_dag_map(map<string, BooleanDAG*>& boolean_dag_map) const;

    void check_consistency();

    void print_extras()
    {
        cout << "get_env()->function_map.transformer_size() " << transformer_size() << endl;
        pair<int, int> min_and_max_depth = transformer_min_and_max_depth();
        cout << "min_and_max_depth " << min_and_max_depth.first << " "<< min_and_max_depth.second << endl;
        assert(!has_cycle());
        cout << "no cycles!" << endl;
    }

    const vector<string>& get_function_names() const
    {
        vector<string>* ret = new vector<string>();
        for(const auto& it: *this) {
            ret->push_back(it.first);
        }
        return *ret;
    }

    const FMTL::TransformPrimitive* insert(const string& name, SketchFunction* sketch_function);

    SketchFunction* operator[](const string& name) const
    {
        auto it = find(name);
        assert(it != end());
        return it->second;
    }

    void erase(const string& name);

    SketchFunction *produce_get(const string &from_dag, const string &under_this_var);

    const VarStore *get_var_store_used_to_concretize_underlying_subdag(const string &from_dag, const string &under_this_var);

    void soft_clear_transformer();

    string pretty_print();

    void print();
};


#endif //SKETCH_SOURCE_FUNCTIONMAP_H
