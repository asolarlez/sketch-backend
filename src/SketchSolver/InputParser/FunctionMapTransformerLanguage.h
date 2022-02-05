//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
#define SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H

#include <utility>

#include "VarStore.h"

class SketchFunction;
class FunctionMap;

namespace FMTL {

    enum TransformPrimitiveMetaType
    {
        _concretize,
        _replace,
        _clone,
        _init
    };
    static const string transform_primitive_meta_type_name[4] = {
    "concretize",
    "replace",
    "clone",
    "init"
    };

    class FunctionMapTransformer;

    class TransformPrimitive
    {
    protected:
        bool is_erased = false;
    private:

        bool visited = false;
        int num_children_visited = 0;
        int min_depth = -1;
        int max_depth = -1;

        bool covered = false;
        bool in_path = false;

        FunctionMap* current_local_function_map = nullptr;

    protected:
        const string function_name;
        map<string, TransformPrimitive*> parents;
        set<TransformPrimitive*> children;
        TransformPrimitiveMetaType meta_type;
        TransformPrimitive* main_parent = nullptr;

        virtual VarStore *get_var_store() const {
            return nullptr;
        }
        virtual const map<string, string> *get_assign_map() const {
            return nullptr;
        }
        virtual const bool_node::Type *get_concretization_type() const {
            return nullptr;
        }

        void set_is_erased(bool is_original);

    public:

        bool get_is_erased() {
            return is_erased;
        }

        const string& get_function_name()
        {
            return function_name;
        }

        virtual TransformPrimitive* erase(FunctionMapTransformer* transformer);

        explicit TransformPrimitive(string  _function_name, TransformPrimitiveMetaType _meta_type):
        function_name(std::move(_function_name)), meta_type(_meta_type) {
            assert(!function_name.empty());
        };

        void add_parent(TransformPrimitive *&new_parent) {
            assert(new_parent != nullptr);
            assert(!new_parent->is_erased);
            assert(parents.find(new_parent->function_name) == parents.end());
            parents[new_parent->function_name] = new_parent;
            assert(new_parent->children.find(this) == new_parent->children.end());
            new_parent->children.insert(this);
        }

        void set_main_parent(TransformPrimitive*& _main_parent)
        {
            assert(_main_parent != nullptr);
            assert(main_parent == nullptr);
            assert(parents.find(_main_parent->function_name) != parents.end());
            main_parent = _main_parent;
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

            for (auto it_parent: parents) {
                auto parent = it_parent.second;
                parent->num_children_visited += 1;
                assert(parent->num_children_visited <= parent->children.size());
            }

            for (auto it_parent: parents) {
                auto parent = it_parent.second;
                if(parent->ready_for_visit()){
                    parent->visit();
                }
            }
        }

        bool ready_for_visit() {
            assert(num_children_visited <= children.size());
            return num_children_visited == children.size() && !get_visited();
        }

        bool has_cycle() {
            if(in_path) {
                return true;
            }
            if(covered) {
                return false;
            }
            in_path = true;
            for(auto it_parent : parents){
                auto parent = it_parent.second;
                if(parent->has_cycle()){
                    return true;
                }
            }
            in_path = false;
            covered = true;
            return false;
        }

        TransformPrimitiveMetaType get_primitive_type() const;

        vector<string> get_inlined_fs();

        void clean();

        void check_consistency(FunctionMapTransformer* transformer);

        int num_reachable_nodes();

        static string tab(int t)
        {
            string ret = "";
            for(int i = 0;i<t;i++)
            {
                ret+="  |";
            }
            return ret;
        }

        TransformPrimitive *
        find_underlying_function(const string &var_name, const FunctionMapTransformer *root, const bool print = false, int t = 0) const;

        const VarStore* find_last_var_store_on_the_way_to(const string& to_this_dag, const string& under_this_var, bool& found) const;

        SketchFunction *reconstruct_sketch_function(FunctionMapTransformer *root, FunctionMap *new_function_map);

        SketchFunction * reconstruct_sketch_function(const string &to_this_dag, const string &under_this_var, bool& found, FunctionMapTransformer* root);

        string parents_to_str();

        TransformPrimitiveMetaType get_meta_type();
    };

    class ConcretizePrimitive: public TransformPrimitive{
        VarStore* var_store;
        const bool_node::Type concretization_type;
    public:
        ConcretizePrimitive(const string &_function_name, VarStore &_store, bool_node::Type _concretization_type) :
                var_store(_store.clone()),
                concretization_type(_concretization_type),
                TransformPrimitive(_function_name, _concretize) {
            AssertDebug(concretization_type == bool_node::CTRL, "TODO: add support for concretization_type = bool_node::SRC. This requires careful consideration about what happens when querying the transformer for varstores, since there can be multiple varstores concretizing a dag (one for SRC, one for CTRL). So far only one var_store can concretize a dag, assumes that a ctrl var store doesn't leave holes unconcretized.");
        }

        VarStore *get_var_store() const override
        {
            return var_store;
        }

        const bool_node::Type* get_concretization_type() const override
        {
            return &concretization_type;
        }

    };

    class InitPrimitive: public TransformPrimitive{
        const map<string, string>* assign_map;
    public:
        InitPrimitive(const string& _function_name, const vector<string>& _subfunction_names):
        TransformPrimitive(_function_name, _init){
            auto* tmp_assign_map = new map<string, string>();
            for(const auto& it: _subfunction_names) {
                (*tmp_assign_map)[it] = it;
            }
            assign_map = tmp_assign_map;
        }
        const map<string, string>* get_assign_map() const override
        {
            return assign_map;
        }

    };

    class ReplacePrimitive: public TransformPrimitive{
        const map<string, string>* assign_map;
    public:
        ReplacePrimitive(const string& _function_name, const string& _replace_this, const string& _with_this):
            TransformPrimitive(_function_name, _replace)
            {
                auto* tmp_assign_map = new map<string, string>();
                (*tmp_assign_map)[_replace_this] = _with_this;
                assign_map = tmp_assign_map;
            }
        const map<string, string> *get_assign_map() const override {
            return assign_map;
        }
    };

    class ClonePrimitive: public TransformPrimitive {
    public:
        ClonePrimitive(const string&  _original_function, const string& _new_function):
            TransformPrimitive(_new_function, _clone) {}
    };

    class FunctionMapTransformer {

        set<TransformPrimitive *> program;
        map<string, TransformPrimitive *> root_dag_reps;
        set<string> erased_root_dag_reps;
        FunctionMap *function_map = nullptr;

        int calc_dag_size() {
            for (auto it: program) {
                it->set_not_visited();
            }
            int ret = 0;
            for (auto it: program) {
                if (!it->get_visited()) {
                    ret += it->num_reachable_nodes();
                }
            }
            return ret;
        }

        void add_parent(TransformPrimitive *new_primitive, const string &parent_name, bool is_main_parent = false) {
            assert(root_dag_reps.find(parent_name) != root_dag_reps.end());
            TransformPrimitive *new_parent = root_dag_reps[parent_name];
            new_primitive->add_parent(new_parent);
            if (is_main_parent) {
                new_primitive->set_main_parent(new_parent);
            }
        }


    public:

        bool contains_only_necessary();

        void print_not_erased()
        {
            cout << "print_not_erased(){" << endl;
            for(TransformPrimitive* it: program)
            {
                if(!it->get_is_erased())
                {
                    cout << it->get_function_name() << " | parents: " << it->parents_to_str() << endl;
                }
            }
            cout << "} done print_not_erased;" << endl;
        }


        void check_consistency();

        FunctionMap *get_function_map() {
            assert(function_map != nullptr);
            return function_map;
        }

        explicit FunctionMapTransformer(FunctionMap* _function_map): function_map(_function_map) {function_map != nullptr;}

        void concretize(const string &function_name, VarStore &store, bool_node::Type type,
                        const vector<string> *sub_functions);

        void replace_label_with_another(const string& function_name, const string &replace_this_str, const string &with_this_str);

        void clone(const string &original_function_name, const string &clone_function_name);

        void insert(const string& new_function_name, const vector<string>& subfunction_names);

        void erase(const string& to_erase_name);

        string find_subdag_name(const string& from_dag_name, const string& find_what_dag_this_varname_maps_to) const;

        const VarStore* find_last_var_store_on_the_way_to(const string& from_dag_name, const string& under_this_var, const string& to_this_dag) const;

        SketchFunction* reconstruct_sketch_function(const string& from_dag, const string& under_this_var, const string& underlying_dag);

        size_t transformer_size()
        {
            return program.size();
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

        const map<string, TransformPrimitive *> & get_root_dag_reps() const;
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
