//
// Created by kliment on 1/29/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERDAG_H
#define SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERDAG_H

#include <utility>
#include "VarStore.h"

class FunctionMap;
class ProgramEnvironment;
namespace SL {
    class SketchFunction;
}

namespace FMTL {

    enum TransformPrimitiveMetaType {
        _make_executable,
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

    class TransformPrimitive {
    protected:
        mutable bool is_erased = false;
    private:
        int num_children_visited = 0;
        bool superseded = false;

        bool visited = false;
        int min_depth = -1;
        int max_depth = -1;

        bool covered = false;
        bool in_path = false;

        mutable ProgramEnvironment *current_new_env = nullptr;

        void set_is_erased(bool is_original);

    protected:
        const string function_name;
        map<string, TransformPrimitive *> parents;
        set<TransformPrimitive *> children;
        TransformPrimitiveMetaType meta_type;
        TransformPrimitive *main_parent = nullptr;

        virtual VarStore *get_var_store() const {
            return nullptr;
        }

        virtual const map<string, string> *get_assign_map() const {
            return nullptr;
        }

        virtual const bool_node::Type *get_concretization_type() const {
            return nullptr;
        }

        void clear() {
            parents.clear();
            children.clear();

            if (get_var_store() != nullptr) {
                get_var_store()->clear();
            }

            delete this;
        }

    public:

        bool get_is_erased() const {
            return is_erased;
        }

        const string &get_function_name() {
            return function_name;
        }

        virtual TransformPrimitive *erase(FunctionMapTransformer *transformer);

        explicit TransformPrimitive(string _function_name, TransformPrimitiveMetaType _meta_type) :
                function_name(std::move(_function_name)), meta_type(_meta_type) {
            assert(!function_name.empty());
        };

        void add_parent(TransformPrimitive *&new_parent) {
            assert(new_parent != nullptr);
            assert(!new_parent->is_erased);
            if (parents.find(new_parent->function_name) != parents.end()) {
                assert(parents[new_parent->function_name] == new_parent);
                assert(new_parent->children.find(this) != new_parent->children.end());
            } else {
                parents[new_parent->function_name] = new_parent;
                assert(new_parent->children.find(this) == new_parent->children.end());
                new_parent->children.insert(this);
            }
            assert(!new_parent->is_erased);
        }

        void set_main_parent(TransformPrimitive *&_main_parent) {
            assert(_main_parent != nullptr);
            assert(main_parent == nullptr);
            auto it = parents.find(_main_parent->function_name);
            assert(it != parents.end());
            assert(it->second == _main_parent);
            main_parent = _main_parent;

            assert(!is_erased);
            assert(!main_parent->is_erased);
            auto main_parent_parent_it = main_parent->parents.find(main_parent->function_name);
            if (main_parent_parent_it != main_parent->parents.end()) {
                assert(main_parent_parent_it->second->superseded);
            }
            if (main_parent->function_name == function_name) {
                main_parent->superseded = true;
            }
        }

        void set_not_visited() {
            //visited markers
            visited = false;
            num_children_visited = 0;
            //depth markers
            min_depth = -1;
            max_depth = -1;
            //cycle markers
            covered = false;
            assert(!in_path);
        }

        int get_min_depth() const {
            return min_depth;
        }

        bool get_visited() const {
            return visited;
        }

        int get_max_depth() const {
            return max_depth;
        }

        void visit() {
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

            for (const auto &it_parent: parents) {
                auto parent = it_parent.second;
                parent->num_children_visited += 1;
                assert(parent->num_children_visited <= parent->children.size());
            }

            for (const auto &it_parent: parents) {
                auto parent = it_parent.second;
                if (parent->ready_for_visit()) {
                    parent->visit();
                }
            }
        }

        bool ready_for_visit() {
            assert(num_children_visited <= children.size());
            return num_children_visited == children.size() && !get_visited();
        }

        bool has_cycle() {
            if (in_path) {
                return true;
            }
            if (covered) {
                return false;
            }
            in_path = true;
            for (const auto &it_parent: parents) {
                auto parent = it_parent.second;
                if (parent->has_cycle()) {
                    return true;
                }
            }
            in_path = false;
            covered = true;
            return false;
        }

        TransformPrimitiveMetaType get_primitive_type() const;

        vector<string> get_inlined_fs();

        void check_consistency(FunctionMapTransformer *transformer);

        int num_reachable_nodes();

        static string tab(int t) {
            string ret = "";
            for (int i = 0; i < t; i++) {
                ret += "  |";
            }
            return ret;
        }

        TransformPrimitive *
        find_underlying_function(const string &var_name, const FunctionMapTransformer *root, const bool print = false,
                                 int t = 0) const;

        SL::SketchFunction *extract_sketch_function(const string &to_this_dag, const string &under_this_var,
                                                const FunctionMapTransformer *root) const;

        SL::SketchFunction *
        reconstruct_sketch_function(const FunctionMapTransformer *root, ProgramEnvironment *new_env) const;

        SL::SketchFunction *reconstruct_sketch_function(const string &to_this_dag, const string &under_this_var,
                                                    const FunctionMapTransformer *root) const;

        string parents_to_str() const;
        string children_to_str() const;

        TransformPrimitiveMetaType get_meta_type();

        void unerase(const TransformPrimitive *parent = nullptr) const;

        const map<string, TransformPrimitive *> &get_parents() const;

        bool get_is_superseded() const;

        TransformPrimitive *get_main_parent();

        virtual void pretty_print(string& ret, map<string, string>* holes_to_values,
                                  const FunctionMapTransformer &fmt,
                                  map<string, map<string, string> > *running_assignment_map = new map<string, map<string, string> >(),
                                  set<TransformPrimitive *> *visited = new set<TransformPrimitive *>()) const;

    public:

        string pretty_print(const FunctionMapTransformer &fmt, map<string, string>* holes_to_values) const
        {
            string ret;
            pretty_print(ret, holes_to_values, fmt);
            ret += "return " + function_name + ";";
            return ret;
        }

        string to_string();
    };

    class ConcretizePrimitive : public TransformPrimitive {
        VarStore *var_store = nullptr;
        const bool_node::Type concretization_type;
    public:
        ConcretizePrimitive(const string &_function_name, const VarStore *_store,
                            const bool_node::Type _concretization_type) :
                concretization_type(_concretization_type),
                TransformPrimitive(_function_name, _make_executable) {
//            assert(var_store != nullptr);
            if (_store != nullptr) {
                var_store = _store->clone();
            }
            AssertDebug(concretization_type == bool_node::CTRL,
                        "TODO: add support for concretization_type = bool_node::SRC. This requires careful consideration about what happens when querying the transformer for varstores, since there can be multiple varstores concretizing a dag (one for SRC, one for CTRL). So far only one var_store can concretize a dag, assumes that a ctrl var store doesn't leave holes unconcretized.");
        }

        VarStore *get_var_store() const override {
            return var_store;
        }

        const bool_node::Type *get_concretization_type() const override {
            return &concretization_type;
        }

        void pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt, map<string, map<string, string> > *running_assignment_map,
                     set<TransformPrimitive *> *visited) const override;
    };

    class InitPrimitive : public TransformPrimitive {

        const vector<string> hole_names;
        const map<string, string> assign_map;

        inline static map<string, string> init_assign_map(const vector<string> &_subfunction_names) {
            map<string, string> tmp_assign_map = map<string, string>();
            for (const auto &it: _subfunction_names) {
                tmp_assign_map[it] = it;
            }
            return tmp_assign_map;
        }

    public:
        InitPrimitive(const string &_function_name, const vector<string> &_subfunction_names,
                      const vector<string> &_hole_names) :
                TransformPrimitive(_function_name, _init), hole_names(_hole_names),
                assign_map(init_assign_map(_subfunction_names)) {

        }

        const map<string, string> *get_assign_map() const override {
            return &assign_map;
        }

        void pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt, map<string, map<string, string> > *running_assignment_map,
                     set<TransformPrimitive *> *visited) const override;
    };

    class ReplacePrimitive : public TransformPrimitive {
        const map<string, string> assign_map;

        static map<string, string> init_assign_map(const string &_replace_this, const string &_with_this) {
            map<string, string> tmp_assign_map;
            tmp_assign_map[_replace_this] = _with_this;
            return tmp_assign_map;
        }

    public:
        ReplacePrimitive(const string &_function_name, const string &_replace_this, const string &_with_this) :
                TransformPrimitive(_function_name, _replace), assign_map(init_assign_map(_replace_this, _with_this)) {}

        const map<string, string> *get_assign_map() const override {
            return &assign_map;
        }

        void pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt, map<string, map<string, string> > *running_assignment_map,
                     set<TransformPrimitive *> *visited) const override;
    };

    class ClonePrimitive : public TransformPrimitive {
        const map<string, string> hole_renaming_map;
    public:
        ClonePrimitive(
                const string &_original_function, const string &_new_function,
                const map<string, string> &_hole_renaming_map) :
                TransformPrimitive(_new_function, _clone), hole_renaming_map(_hole_renaming_map) {}

        void pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt, map<string, map<string, string> > *running_assignment_map,
                     set<TransformPrimitive *> *visited) const override;
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

        void print() const
        {

            cout << "program:" << endl;
            for(auto it: program) {
                cout << "\t" << it->to_string() << endl;
            }

            cout << "root_dag_reps: " << endl;
            for(auto it: root_dag_reps) {
                cout << "\t" << it.first << " -> (" << it.second->to_string() << ")" <<  endl;
            }

            cout << "|erased_root_dag_reps| = " << erased_root_dag_reps.size() << endl;

        }

        void clear_erased_root_dag_reps() {
            erased_root_dag_reps.clear();
        }

        bool empty() const {
            return program.empty() && root_dag_reps.empty();
        }

        void soft_clear() {
            vector<string> to_clean;
            for (const auto &it: root_dag_reps) {
                to_clean.push_back(it.first);
            }
            for (int i = 0; i < to_clean.size(); i++) {
                erase(to_clean[i], false);
            }
            program.clear();
            root_dag_reps.clear();
            erased_root_dag_reps.clear();
        }

        bool contains_only_necessary();

        int num_not_erased()
        {
            int _num_not_erased = 0;
            for (TransformPrimitive *it: program) {
                if (!it->get_is_erased()) {
                    _num_not_erased+=1;
                }
            }
            return _num_not_erased;
        }

        void print_not_erased() {
            cout << "print_not_erased(){" << endl;
            for (TransformPrimitive *it: program) {
                if (!it->get_is_erased()) {
                    cout << it->get_function_name() << "| is_superseded = " << it->get_is_superseded() << " | parents: "
                         << it->parents_to_str() << endl;
                }
            }
            cout << "} done print_not_erased;" << endl;
        }


        void check_consistency();

        FunctionMap *get_function_map() const {
            assert(function_map != nullptr);
            return function_map;
        }

        explicit FunctionMapTransformer(FunctionMap *_function_map) : function_map(_function_map) {
            assert(function_map != nullptr);
        }

        const TransformPrimitive *concretize(const string &function_name, const VarStore *store, bool_node::Type type,
                                             const vector<string> *sub_functions);

        const TransformPrimitive *
        replace_label_with_another(const string &function_name, const string &replace_this_str,
                                   const string &with_this_str);

        const TransformPrimitive *clone(const string &original_function_name, const string &clone_function_name,
                                        const map<string, string> hole_rename_map);

        const TransformPrimitive *insert(const string &new_function_name, const vector<string> &subfunction_names,
                                         const vector<string> &hole_names);

        void erase(const string &to_erase_name, bool assert_not_in_function_map = true);

        void reinsert(const string &to_reinsert_name);

        string find_subdag_name(const string &from_dag_name, const string &find_what_dag_this_varname_maps_to) const;

        SL::SketchFunction *
        extract_sketch_function(const string &from_dag, const string &under_this_var, const string &to_this_dag) const;

        SL::SketchFunction *
        reconstruct_sketch_function(const string &from_dag, const string &under_this_var, const string &underlying_dag);

        size_t transformer_size() {
            return program.size();
        }

        bool has_cycle() {
            for (auto it: program) {
                it->set_not_visited();
            }
            for (auto it: program) {
                if (it->has_cycle()) {
                    return true;
                }
            }
            return false;
        }

        pair<int, int> transformer_min_and_max_depth() {
            for (auto it: program) {
                it->set_not_visited();
            }

            for (auto it: program) {
                if (it->ready_for_visit()) {
                    it->visit();
                }
            }

            int min_depth = 0;
            int max_depth = 0;

            for (auto it: program) {
                assert(it->get_visited());
                min_depth = max(min_depth, it->get_min_depth());
                max_depth = max(max_depth, it->get_max_depth());
            }

            return make_pair(min_depth, max_depth);
        }

        set<TransformPrimitive *> &get_program();

        const map<string, TransformPrimitive *> &get_root_dag_reps() const;

        map<string, TransformPrimitive *> &get_root_dag_reps_nonconst();

        const set<string> &get_erased() const {
            return erased_root_dag_reps;
        }
    };
}

#endif //SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERDAG_H
