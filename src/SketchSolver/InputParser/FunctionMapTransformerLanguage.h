//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
#define SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H

#include <utility>

#include "VarStore.h"

namespace FMTL {

    class FunctionMapTransformer;

    enum TransformPrimitiveMetaType
    {
        _concretize,
        _replace,
        _clone,
        _init
    };
    class TransformPrimitive
    {
        const string function_name;

        TransformPrimitiveMetaType meta_type;
        set<TransformPrimitive*> parents;
        set<TransformPrimitive*> children;
        bool is_erased = false;

        int min_depth = -1;
        int max_depth = -1;
        bool visited = false;
        int num_children_visited = 0;
    public:
        const string& get_function_name()
        {
            return function_name;
        }
        int get_num_children()
        {
            return children.size();
        }
        TransformPrimitive* erase(FunctionMapTransformer* transformer);

        explicit TransformPrimitive(const string& _function_name, TransformPrimitiveMetaType _meta_type):
        function_name(_function_name), meta_type(_meta_type) {};

        void add_parent(TransformPrimitive *&new_parent) {
            assert(parents.find(new_parent) == parents.end());
            parents.insert(new_parent);
            assert(new_parent->children.find(this) == new_parent->children.end());
            new_parent->children.insert(this);
        }

        void set_not_visited() {
            visited = false;
            num_children_visited = 0;
            min_depth = -1;
            max_depth = -1;
            covered = false;
            assert(!in_path);
        }

        int get_min_depth()
        {
            return min_depth;
        }

        bool get_visited()
        {
            return visited;
        }

        int get_max_depth()
        {
            return max_depth;
        }

        void visit()
        {
            assert(!visited);

            assert(num_children_visited == children.size());

            int min_child_depth = (1 << 31);
            int max_child_depth = 0;

            for (auto child: children) {
                assert(child->get_visited());
                max_child_depth = max(max_child_depth, child->max_depth);
                min_child_depth = min(min_child_depth, child->min_depth);
            }

            max_depth = max_child_depth + 1;
            min_depth = min_child_depth + 1;

            visited = true;

            for (auto parent: parents) {
                parent->num_children_visited += 1;
                assert(parent->num_children_visited <= parent->children.size());
            }

            for (auto parent: parents) {
                if(parent->ready_for_visit()){
                    parent->visit();
                }
            }
        }

        bool ready_for_visit() {
            assert(num_children_visited <= children.size());
            return num_children_visited == children.size() && !get_visited();
        }

    private:
        bool covered = false;
        bool in_path = false;
    public:

        bool has_cycle() {
            if(in_path) {
                return true;
            }
            if(covered) {
                return false;
            }
            in_path = true;
            for(auto parent : parents){
                if(parent->has_cycle()){
                    return true;
                }
            }
            in_path = false;
            covered = true;
            return false;
        }
    };

    class ConcretizePrimitive: public TransformPrimitive{
        const VarStore& var_store;
        const bool_node::Type concretization_type;
        const bool do_deactivate_pcond;
    public:
        ConcretizePrimitive(const string& _function_name, VarStore &_store, bool_node::Type _concretization_type, bool _do_deactivate_pcond):
                var_store(*_store.copy()), concretization_type(_concretization_type),
                do_deactivate_pcond(_do_deactivate_pcond), TransformPrimitive(_function_name, _concretize) {}
    };

    class InitPrimitive: public TransformPrimitive{
    public:
        InitPrimitive(const string& _function_name): TransformPrimitive(_function_name, _init){}
    };

    class ReplacePrimitive: public TransformPrimitive{
        const string replace_this;
        const string with_this;
    public:
        ReplacePrimitive(const string& _function_name, const string& _replace_this, const string& _with_this):
            replace_this(std::move(_replace_this)), with_this(std::move(_with_this)), TransformPrimitive(_function_name, _replace) {

        }
    };

    class ClonePrimitive: public TransformPrimitive {
        const string original_function;
    public:
        ClonePrimitive(const string& _original_function, const string& _new_function):
            original_function(_original_function),
            TransformPrimitive(_new_function, _clone) {}
    };

    class FunctionMapTransformer {

        set<TransformPrimitive*> program;
        map<string, TransformPrimitive*> where_my_kids_at;
    public:
        explicit FunctionMapTransformer() = default;

        void concretize(const string& function_name, VarStore &store, bool_node::Type type, bool do_deactivate_pcond, const vector<string>& sub_functions);

        void replace_label_with_another(const string& function_name, const string &replace_this_str, const string &with_this_str);

        void clone(const string &original_function_name, const string &clone_function_name);

        void insert(const string& new_function_name);

        void erase(const string& to_erase_name);

        size_t transformer_size()
        {
            return program.size();
        }

        void add_parent(TransformPrimitive* new_primitive, const string& parent_name) {
            assert(where_my_kids_at.find(parent_name) != where_my_kids_at.end());
            new_primitive->add_parent(where_my_kids_at[parent_name]);
        }

        bool has_cycle()
        {
            for(auto it:program){
                it->set_not_visited();
            }
            for(auto it:program){
                if(it->has_cycle()) {
                    return true;
                }
            }
            return false;
        }

        pair<int, int> transformer_min_and_max_depth()
        {
            for(auto it:program)
            {
                it->set_not_visited();
            }

            for(auto it: program)
            {
                if(it->ready_for_visit()) {
                    it->visit();
                }
            }

            int min_depth = 0;
            int max_depth = 0;

            for(auto it: program)
            {
                assert(it->get_visited());
                min_depth = max(min_depth, it->get_min_depth());
                max_depth = max(max_depth, it->get_max_depth());
            }

            return make_pair(min_depth, max_depth);
        }

        set<TransformPrimitive *> &get_program();

        map<string, TransformPrimitive *> & get_where_my_kids_at();
    };
}









/**
 * Goal:
 *      After the solver program has finished, you want to return to the user a FUNCTION MAP with HOLE ASSIGNMENT FOR EVERY FUNCTION.
 *
 *      THE SOLVER LANGUAGE PROGRAM RIGHT NOW WORKS ON THE LEVEL OF:
 *
 *      BOTTOM LINE: NEED TO REFACTOR THE FUNCTION MAP TO BE A FUNCTION MAP OF SKETCH FUNCTIONS THAT KEEP TRACK OF THE EVOLUTION OF THE FUNCTIONMAP!!!
 *
 *      RECONSIDER: Need to keep track of changes on a function-map level.
 *
 *      What you are unsure about: intermediate dags, returning all dags?
 *
 *      the bottom line consideration is: the user only cares about 1-2 harnesses. but in getting there, many more might have been created.
 *
 *      So really the user should at the end return a function map; for example, by specifying which function they want to return.
 *
 *      As dags get deleted, but what does that mean?
 *      a dag can get deleted, but if it is used in another dag, in the program that dag ...
 *
 *      just keep all the dags that are used in non-deleted dags.
 *
 *      need to swipe the ufuns to check which dags in particular have been used, and store them.
 *
 */


#endif //SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
