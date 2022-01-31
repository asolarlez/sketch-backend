//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
#define SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H

#include <utility>

#include "VarStore.h"

namespace FMTL {

//    enum VarStoreTreeNodeType {
//        _leaf_node,
//        _internal_node
//    };
//
//    class VarStoreTreeNode
//    {
//        const string function_name;
//        VarStoreTreeNodeType node_type;
//    public:
//        VarStoreTreeNode(const string& _function_name, VarStoreTreeNodeType _node_type): node_type(_node_type){}
//    };
//
//    class VarStoreLeafNode: public VarStoreTreeNode
//    {
//        const VarStore* var_store;
//    public:
//        VarStoreLeafNode(const string& _function_name, const VarStore* _var_store):
//        var_store(_var_store), VarStoreTreeNode(_function_name, _leaf_node){}
//    };
//
//    class VarStoreInternalNode: public VarStoreTreeNode, public map<string, const VarStoreInternalNode*>
//    {
//    public:
//        VarStoreInternalNode(const string& _function_name): VarStoreTreeNode(_function_name, _internal_node) {}
//    };


    class VarStoreTreeNode: public map<string, const VarStoreTreeNode*>
    {
        const string function_name;
        const VarStore* var_store;
        const map<string, string>* assign_map;
    public:

        VarStoreTreeNode(string  _function_name, const VarStore* _var_store, const map<string, string>* _assign_map):
            function_name(std::move(_function_name)), var_store(_var_store), assign_map(_assign_map) {}

        const VarStore* get_var_store() const {
            return var_store;
        }

        const map<string, string>* get_assign_map() const {
            return assign_map;
        }

        string find_underlying_function_name(const string& var_name) const {
            string ret_so_far;
            if(assign_map != nullptr) {
                if(assign_map->find(var_name) != assign_map->end())
                {
                    string tmp_ret = (*assign_map).at(var_name);
                    if(!ret_so_far.empty())
                    {
                        assert(ret_so_far == tmp_ret);
                    }
                    ret_so_far = tmp_ret;
                }
                if(!ret_so_far.empty())
                {
                    return ret_so_far;
                }
            }
            for(const auto& it: *this) {
                string tmp_ret = it.second->find_underlying_function_name(var_name);
                if(!tmp_ret.empty()) {
                    if (!ret_so_far.empty()) {
                        assert(ret_so_far == tmp_ret);
                    }
                    else {
                        ret_so_far = tmp_ret;
                    }
                }
            }
            return ret_so_far;
        }

        const VarStore* find_last_var_store_on_the_way_to(const string& var_name, bool& found) const {
            assert(!found);
            const VarStore* ret_so_far = nullptr;
            if(var_store != nullptr) {
                if(function_name == var_name) {
                    const VarStore* tmp_ret = var_store;
                    if(ret_so_far != nullptr)
                    {
                        assert(ret_so_far == tmp_ret);
                    }
                    ret_so_far = tmp_ret;
                }
                if(ret_so_far != nullptr)
                {
                    found = true;
                    return ret_so_far;
                }
            }
            if(function_name == var_name)
            {
                found = true;
                return nullptr;
            }
            for(const auto& it: *this) {
                bool local_found = false;
                const VarStore* tmp_ret = it.second->find_last_var_store_on_the_way_to(var_name, local_found);
                if(tmp_ret != nullptr) {
                    if (ret_so_far != nullptr) {
                        assert(ret_so_far == tmp_ret);
                    }
                    else {
                        ret_so_far = tmp_ret;
                    }
                }
                if(local_found == true)
                {
                    if(var_store != nullptr)
                    {
                        const VarStore* local_tmp_ret = var_store;
                        if(ret_so_far != nullptr)
                        {
                            assert(ret_so_far == local_tmp_ret);
                        }
                        ret_so_far = local_tmp_ret;
                    }
                    found = true;
                }
            }
            return ret_so_far;
        }

        const string &get_function_name() const {
            return function_name;
        }
    };

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
    private:
        bool is_erased = false;
        bool is_erasing = false;

        bool visited = false;
        int num_children_visited = 0;
        int min_depth = -1;
        int max_depth = -1;

        bool covered = false;
        bool in_path = false;

    protected:
        const string function_name;
        set<TransformPrimitive*> parents;
        set<TransformPrimitive*> children;
        TransformPrimitiveMetaType meta_type;

        bool result_var_store_tree_is_calculated = false;
        const VarStoreTreeNode* _result_var_store_tree = nullptr;

        virtual VarStore *get_var_store() const {
            assert(false);
        }
        virtual const map<string, string> *get_assign_map() const {
            assert(false);
        }

        void set_is_erased(bool is_original);

    public:

        const string& get_function_name()
        {
            return function_name;
        }

        TransformPrimitive* erase(FunctionMapTransformer* transformer);

        explicit TransformPrimitive(string  _function_name, TransformPrimitiveMetaType _meta_type):
        function_name(std::move(_function_name)), meta_type(_meta_type) {};

        void add_parent(TransformPrimitive *&new_parent) {
            assert(!new_parent->is_erased);
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

        const VarStoreTreeNode * compile_var_store_tree();

        TransformPrimitiveMetaType get_primitive_type() const;

        vector<string> get_inlined_fs();

        void clean();

        void check_consistency(FunctionMapTransformer* transformer);
    };

    class ConcretizePrimitive: public TransformPrimitive{
        VarStore* var_store;
        const bool_node::Type concretization_type;
        const bool do_deactivate_pcond;

    public:
        ConcretizePrimitive(const string& _function_name, VarStore &_store, bool_node::Type _concretization_type, bool _do_deactivate_pcond):
                var_store(_store.copy()), concretization_type(_concretization_type),
                do_deactivate_pcond(_do_deactivate_pcond), TransformPrimitive(_function_name, _concretize) {}

        VarStore *get_var_store() const override
        {
            return var_store;
        }
        const map<string, string> *get_assign_map() const override {
            return nullptr;
        }
    };

    class InitPrimitive: public TransformPrimitive{
    public:
        InitPrimitive(const string& _function_name): TransformPrimitive(_function_name, _init){}
        VarStore *get_var_store() const override {
            return nullptr;
        }
        const map<string, string> *get_assign_map() const override {
            return nullptr;
        }
    };

    class ReplacePrimitive: public TransformPrimitive{
        const string replace_this;
        const string with_this;
        const map<string, string>* assign_map;
    public:
        ReplacePrimitive(const string& _function_name, const string& _replace_this, const string& _with_this):
            replace_this(_replace_this), with_this(_with_this), TransformPrimitive(_function_name, _replace)
            {
                auto* tmp_assign_map = new map<string, string>();
                (*tmp_assign_map)[replace_this] = with_this;
                assign_map = tmp_assign_map;
            }
        VarStore *get_var_store() const override {
            return nullptr;
        }
        const map<string, string> *get_assign_map() const override {
            return assign_map;
        }
    };

    class ClonePrimitive: public TransformPrimitive {
        const string original_function;
    public:
        ClonePrimitive(const string&  _original_function, const string& _new_function):
            original_function(_original_function),
            TransformPrimitive(_new_function, _clone) {}
        VarStore *get_var_store() const override {
            return nullptr;
        }
        const map<string, string> *get_assign_map() const override {
            return nullptr;
        }
    };

    class FunctionMapTransformer {

        set<TransformPrimitive*> program;
        map<string, TransformPrimitive*> where_my_kids_at;
    public:
        explicit FunctionMapTransformer() = default;

        void concretize(const string& function_name, VarStore &store, bool_node::Type type, bool do_deactivate_pcond, const vector<string> *sub_functions);

        void replace_label_with_another(const string& function_name, const string &replace_this_str, const string &with_this_str);

        void clone(const string &original_function_name, const string &clone_function_name);

        void insert(const string& new_function_name);

        void erase(const string& to_erase_name);

        void check_consistency();

        const VarStoreTreeNode* compile_var_store_tree(const string& function_name);

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

        void clean_transformer();
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
