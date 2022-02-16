//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"
#include "BooleanDagUtility.h"

#include <utility>

const bool rename_holes = true;

SketchFunction *SketchFunction::produce_concretization(const VarStore* _var_store, const bool_node::Type var_type, const bool do_clone, const bool do_deep_clone) {

    if(do_clone) {
        assert(do_deep_clone);
        SketchFunction* the_clone = clone();
        the_clone->increment_shared_ptr();
        the_clone->produce_concretization(_var_store, var_type, false, do_deep_clone);
        the_clone->decrement_shared_ptr_wo_clear();
        return the_clone;
    }
    else {

        VarStore* var_store = nullptr;

        if(_var_store != nullptr) {
            assert(_var_store->check_rep());
            var_store = new VarStore(*_var_store);
        }

        if(do_deep_clone) {
            deep_clone_tail();

            if(var_type == bool_node::CTRL)
            {

                InliningTree *tmp_inlining_tree = new InliningTree(this);

                if(var_store != nullptr) {
                    var_store->check_rep();
                    assert(tmp_inlining_tree->match_topology(var_store->get_inlining_tree()));
                    tmp_inlining_tree->rename_var_store(*var_store);
                    var_store->check_rep();
                }

                tmp_inlining_tree->concretize(var_store, var_type);
                tmp_inlining_tree->clear();
            }
        }

        vector<string> *inlined_functions = nullptr;

        bool prev_has_been_concretized = get_has_been_concretized();
        assert(!prev_has_been_concretized);

        concretize_this_dag(var_store, var_type, inlined_functions);

        assert(inlined_functions != nullptr);

        //construct solution
        if (var_type == bool_node::CTRL && get_has_been_concretized()) {
            AssertDebug(solution == nullptr, "you can't concretize a function twice");
            SATSolverResult sat_solver_result = SAT_UNDETERMINED;
            if (get_dag()->get_failed_assert() != nullptr) {
                sat_solver_result = SAT_UNSATISFIABLE;
            } else if (!get_dag()->getNodesByType(bool_node::CTRL).empty()) {
                sat_solver_result = SAT_NOT_FULLY_CONCRETIZED;
            } else if (get_dag()->get_failed_assert() == nullptr) {
                sat_solver_result = SAT_SATISFIABLE;
            } else {
                assert(false);
            }

            SolverLanguagePrimitives::HoleAssignment* compare_solution = nullptr;

            if(var_store == nullptr) {
                compare_solution = new SolverLanguagePrimitives::HoleAssignment(
                        sat_solver_result, get_inlining_tree(), get_env()->floats);
            } else {
                get_inlining_tree()->match_topology(var_store->get_inlining_tree());
                compare_solution = new SolverLanguagePrimitives::HoleAssignment(
                        sat_solver_result, var_store, get_env()->floats);
            }

            if(compare_solution->get_assignment()->get_inlining_tree() == nullptr) {
                compare_solution->get_assignment()->set_inlining_tree(get_inlining_tree());
            }

            if (solution != nullptr) {
                if (!(*solution == *compare_solution)) {
                    cout << solution->to_string() << endl;
                    cout << compare_solution->to_string() << endl;
                    assert(false);
                }
                compare_solution->clear();
                delete compare_solution;
//                get_solution(); //debug code, it's memory leaky
            } else {
                assert(solution == nullptr);
                solution = compare_solution;

                if(solution != nullptr) {
                    assert(solution->get_assignment()->get_inlining_tree()->get_skfunc() == this);
                }
            }

        }

        rep = get_env()->function_map.concretize(
                get_dag()->get_name(), var_store, var_type, inlined_functions);

        if(var_store != nullptr) {
            var_store->clear();
        }

        if (solution->get_sat_solver_result() == SAT_SATISFIABLE) {
            assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
            assert(get_dag()->getNodesByType(bool_node::CTRL).empty());
        }

        delete inlined_functions;

        return this;
    }
}

SketchFunction *SketchFunction::clone(const string& explicit_name) {

    assert(rename_holes);
    BooleanDAG* cloned_dag = get_dag()->clone(explicit_name, rename_holes);

    if(rename_holes)
    for(auto it : cloned_dag->getNodesByType(bool_node::CTRL)) {
        string actual_name = ((CTRL_node*)it)->get_name();
        if(actual_name != "#PC") {
            string var_name = ((CTRL_node *) it)->get_original_name();
            string sub_dag_name = ((CTRL_node *) it)->get_source_dag_name();

            assert(sub_dag_name == cloned_dag->get_name());
        }
    }

    auto new_primitive = get_env()->function_map.clone(get_dag()->get_name(), cloned_dag->get_name());

    const SolverLanguagePrimitives::HoleAssignment* solution_clone = nullptr;

    if(solution != nullptr) {

        for(auto it : get_dag()->getNodesByType(bool_node::CTRL)) {

            string var_name = ((CTRL_node*)it)->get_original_name();
            string actual_name = ((CTRL_node*)it)->get_name();
            string sub_dag_name = ((CTRL_node*)it)->get_source_dag_name();

            assert(sub_dag_name == get_dag_name());

            assert(solution->get_assignment()->get_name(var_name, get_dag_name()) == actual_name);
        }

        solution_clone = solution;

//        solution_clone = new SolverLanguagePrimitives::HoleAssignment(solution);

//        VarStore* var_store = solution->to_var_store();
//        InliningTree* local_inlining_tree = new InliningTree(this);
//        local_inlining_tree->rename_var_store(*var_store);
//        solution_clone = new SolverLanguagePrimitives::HoleAssignment(solution->get_sat_solver_result(), var_store, get_env()->floats);
//        var_store->clear();
//        local_inlining_tree->clear();

//        for(auto it : cloned_dag->getNodesByType(bool_node::CTRL)) {
//            string var_name = ((CTRL_node*)it)->get_original_name();
//            string actual_name = ((CTRL_node*)it)->get_name();
//            string sub_dag_name = ((CTRL_node*)it)->get_source_dag_name();
//            assert(sub_dag_name == cloned_dag->get_name());
//            solution_clone->rename(actual_name, var_name, sub_dag_name, get_dag_name(), solution->get_assignment()->get_name(var_name, get_dag_name()));
//        }
    }
    return new SketchFunction(
            cloned_dag, get_env(), solution_clone, replaced_labels, original_labels, new_primitive, responsibility, get_inlining_tree(false), get_has_been_concretized());
}

void SketchFunction::core_clear(const string& dag_name)
{
    assert(local_clear_id == global_clear_id);
    get_env()->function_map.erase(dag_name);


    for(const auto& sk_it : get_env()->function_map)
    {
        auto ufuns = sk_it.second->get_dag()->getNodesByType(bool_node::UFUN);
        for(auto it_ufun : ufuns)
        {
            string ufname = ((UFUN_node*)it_ufun)->get_ufname();
            assert(get_env()->function_map.find(ufname) != get_env()->function_map.end());
        }
    }

    if (solution != nullptr) {
        assert(solution->get_num_shared_ptr() == 0);
        assert(solution->get_assignment()->get_inlining_tree()->get_skfunc() != this);
        solution->clear_assert_num_shared_ptr_is_0(true, true);
        delete solution;
    }

    for(auto it: responsibility) {
        it.second->_clear();
    }

    delete this;
}

void SketchFunction::_clear()
{
    if(local_clear_id != global_clear_id) {
        local_clear_id = global_clear_id;
    }
    else {
//        AssertDebug(false, "WOAH! A CYCLIC DEPENDENCY!! NICE!! (you can remove this now haha).")
        return;
    }

    string dag_name = get_dag()->get_name();

    int prev_gloval = global_clear_id;
    if(BooleanDagUtility::soft_clear(solution)) {
        assert(prev_gloval == global_clear_id);
        core_clear(dag_name);
    }
    local_clear_id = -1;
}

void SketchFunction::clear(){
    global_clear_id++;
    _clear();
}

void SketchFunction::clear_assert_num_shared_ptr_is_0() {
    global_clear_id++;

    string dag_name = get_dag()->get_name();

    if(BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()) {
        core_clear(dag_name);
    }
}


void SketchFunction::replace(const string replace_this, const string with_this) {
    assert(new_way);

    AssertDebug(!is_inlining_tree_nonnull(), "TODO: when renaming, need to update inlining tree.");

    assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

    if (replaced_labels.find(replace_this) == replaced_labels.end()) {
        AssertDebug(replaced_labels.find(replace_this) == replaced_labels.end(),
                    "If this happens, it means that you are replacing a label that has previously been replaced (used as 'replace_this'). Not yet handled.");

        assert(replaced_labels.find(replace_this) == replaced_labels.end());

        rep = get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);

        assert(replaced_labels.find(replace_this) == replaced_labels.end());

        get_dag()->replace_label_with_another(replace_this, with_this);

        assert(replaced_labels.find(replace_this) == replaced_labels.end());

        auto original_it = original_labels.find(replace_this);
        assert(original_it == original_labels.end());
        original_labels[replace_this] = replace_this;

        assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

    } else {
        assert(replaced_labels.find(replace_this) != replaced_labels.end());

        if(replaced_labels[replace_this] == with_this){
            assert(get_dag_name() == with_this);

            assert(responsibility.find(with_this) != responsibility.end());

            //nothing to do.
            //replacing a label with itself in a self-recursive function.
            return;
        }

        rep = get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);
        get_dag()->replace_label_with_another(replaced_labels[replace_this], with_this);

        assert(replaced_labels.find(replace_this) != replaced_labels.end());

        auto original_it = original_labels.find(replace_this);
        assert(original_it != original_labels.end());
        assert(original_it->second == replace_this);
        string prev_dep_name = replaced_labels[replace_this];

        auto dependency_it = get_env()->function_map.find(prev_dep_name);
        assert(dependency_it != get_env()->function_map.end());
        assert(responsibility.find(prev_dep_name) != responsibility.end());

        assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

        responsibility[prev_dep_name]->clear();
        assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

        responsibility.erase(prev_dep_name);

        if(get_inlining_tree(false) != nullptr)
        {
            AssertDebug(false, "before you couldn't rename after inlining. attend to this. need to edit the inlining tree.");
            assert(get_inlining_tree()->find(prev_dep_name) != nullptr);

        }
        if(solution != nullptr)
        {
            AssertDebug(false, "before you couldn't rename after concretizing. attend to this, need to edit the concretized solution.");
            assert(solution->get_assignment()->get_inlining_tree()->find(prev_dep_name) != nullptr);
        }
    }

    auto dependency_it = get_env()->function_map.find(with_this);
    assert(dependency_it != get_env()->function_map.end());
    assert(responsibility.find(with_this) == responsibility.end());

    if (dependency_it->second != this){
        assert(dependency_it->second->get_dag_name() != get_dag_name());

        responsibility[with_this] = dependency_it->second;
        responsibility[with_this]->increment_shared_ptr();
    }
    else {
        responsibility[with_this] = dependency_it->second;
    }

    replaced_labels[replace_this] = with_this;
}

SketchFunction * SketchFunction::produce_get(const string& get_the_dag_under_this_varname) {
//    cout << "in produce get " << get_dag()->get_name() <<" "<< get_the_dag_under_this_varname << endl;
    return get_env()->function_map.produce_get(get_dag()->get_name(), get_the_dag_under_this_varname);
}

bool SketchFunction::solution_is_null() {
    return solution == nullptr;
}

const SolverLanguagePrimitives::HoleAssignment * SketchFunction::get_same_soluton() {
    return solution;
}

string SketchFunction::get_assignment(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());
    return it->second;
}

void SketchFunction::reset(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());

    auto original_it = original_labels.find(key);
    assert(original_it != original_labels.end());
    assert(original_it->second == key);

    replace(key, key);
}

const map<string, string> &SketchFunction::get_replace_map() const {
    return replaced_labels;
}

void SketchFunction::set_rep(const TransformPrimitive *new_or_existing_primitive) {
    if(rep == nullptr) {
        rep = new_or_existing_primitive;
    }
    else {
        assert(rep == new_or_existing_primitive);
    }
}

const TransformPrimitive * SketchFunction::get_rep() {
    return rep;
}

const TransformPrimitive * SketchFunction::get_mirror_rep() const {
    return mirror_rep;
}

void SketchFunction::set_mirror_rep(const TransformPrimitive *_mirror_rep) {
    mirror_rep = _mirror_rep;
}

void SketchFunction::deep_clone_tail() {

    //first get all inlined functions
    //clone all inlined functions;
    //replace the name inside the inlined function with eachother's

    //assert that all the ufuns are represented in the function map
    for (const auto& it: get_env()->function_map) {
        for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
            assert(f != get_env()->function_map.end());
        }
    }

    set<string>* inlined_functions = get_inlined_functions();

    assert(inlined_functions != nullptr);

    //assert that all that the previous operation hasn't corrupted the function map ufuns invariant
    for (const auto& it: get_env()->function_map) {
        for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
            assert(f != get_env()->function_map.end());
        }
    }

    // assert that all the dags in the inlined functions have all the ufuns also in the inlined functions
    for (const auto &inlined_f_name: *inlined_functions) {
        auto it_f = get_env()->function_map.find(inlined_f_name);
        assert(it_f != get_env()->function_map.end());
        for (auto ufun_it: it_f->second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
            assert(f != get_env()->function_map.end());
            assert(inlined_functions->find(((UFUN_node *) ufun_it)->get_ufname()) != inlined_functions->end());
        }
    }

    bool entered_recursive_case = false;
    map<string, SketchFunction *> to_inline_skfuncs;
    for (const string &inlined_function_name: *inlined_functions) {
        if (inlined_function_name != get_dag()->get_name()) {
            assert(to_inline_skfuncs.find(inlined_function_name) == to_inline_skfuncs.end());

            to_inline_skfuncs[inlined_function_name] = get_env()->function_map[inlined_function_name]->clone();
            assert(get_env()->function_map.find(to_inline_skfuncs[inlined_function_name]->get_dag()->get_name()) == get_env()->function_map.end());
            get_env()->function_map.insert(to_inline_skfuncs[inlined_function_name]->get_dag()->get_name(),
                                           to_inline_skfuncs[inlined_function_name]);
        } else {
            entered_recursive_case = true;
            assert(get_env()->function_map.find(get_dag()->get_name()) != get_env()->function_map.end());
            to_inline_skfuncs[inlined_function_name] = get_env()->function_map[inlined_function_name];
        }
    }


    assert(to_inline_skfuncs.size() == inlined_functions->size());

    for (const auto &it: *inlined_functions) {
        assert(to_inline_skfuncs.find(it) != to_inline_skfuncs.end());
    }

    inlined_functions->clear();
    inlined_functions = nullptr;

    if (to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end()) {
        assert(entered_recursive_case);
        assert(to_inline_skfuncs[get_dag_name()] == this);
    } else {
        assert(!entered_recursive_case);
        to_inline_skfuncs[get_dag_name()] = this;
    }

    //rename all the ufuns
    for (const auto& skfunc_it: to_inline_skfuncs) {
        set<pair<string, string> > ufnames;

        for (auto ufun_it: skfunc_it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            string ufname = ((UFUN_node *) ufun_it)->get_ufname();
            string original = ((UFUN_node *) ufun_it)->get_original_ufname();
            ufnames.insert(make_pair(ufname, original));
        }

        for (const pair<string, string> &ufname_now_original: ufnames) {
            string ufname = ufname_now_original.first;
            string original = ufname_now_original.second;

            assert(to_inline_skfuncs[ufname]->get_env()->function_map.find(to_inline_skfuncs[ufname]->get_dag()->get_name()) != to_inline_skfuncs[ufname]->get_env()->function_map.end());

            if (to_inline_skfuncs.find(ufname) == to_inline_skfuncs.end()) {
                AssertDebug(false, "this should fail here, it was checked before");
            } else {
                SketchFunction* skfunc = skfunc_it.second;
                skfunc->replace(original, to_inline_skfuncs[ufname]->get_dag()->get_name());
            }
        }
    }

}

#include "SolverLanguageLexAndYaccHeader.h"

SL::VarVal* SketchFunctionEvaluator::eval(SketchFunction *sk_func, SolverLanguagePrimitives::InputAssignment *input_assignment) {
    BooleanDAG *the_dag = nullptr;
    bool new_clone = false;

    assert(sk_func->get_has_been_concretized());
    assert(sk_func->get_dag()->getNodesByType(bool_node::CTRL).empty());
    assert(sk_func->get_dag()->getNodesByType(bool_node::UFUN).empty());

    if (sk_func->get_has_been_concretized()) {
        the_dag = sk_func->get_dag();
    } else {
        the_dag = sk_func->get_dag()->clone();
        new_clone = true;
        sk_func->get_env()->doInline(*the_dag);
    }

    VarStore *the_var_store = input_assignment->to_var_store(false);

    const bool assert_num_remaining_holes_is_0 = true;
    if (assert_num_remaining_holes_is_0) {
        int remaining_holes = the_dag->getNodesByType(bool_node::CTRL).size();
        AssertDebug(remaining_holes == 0,
                    "This dag should not havey any holes remaining, but it has " +
                    std::to_string(remaining_holes) + " remaining_holes.");
    }

    NodeEvaluator node_evaluator(*the_dag, sk_func->get_env()->floats);
    bool fails = node_evaluator.run(*the_var_store);

    delete the_var_store;
    the_var_store = nullptr;
    AssertDebug(!fails, "the dag " + the_dag->get_name() + " asserts false on this input.");

    auto after_run_dests = the_dag->getNodesByType(bool_node::DST);
    assert(after_run_dests.size() == 1);
    //SHOULD BE THIS BUT ISN'T
//                      ret = new VarVal(node_evaluator.getValue(after_run_dests[0]));
    int tuple_node_id = node_evaluator.getValue(after_run_dests[0]);
    bool_node *tuple_node = (*the_dag)[tuple_node_id];
    assert(tuple_node->getOtype()->isTuple && tuple_node->type == bool_node::TUPLE_CREATE);
    AssertDebug(tuple_node->nparents() == 1, "NEET TO GENERALIZE THIS.");
    OutType *out_type = tuple_node->get_parent(0)->getOtype();

    int val = node_evaluator.getValue(tuple_node->get_parent(0));
    //THIS IS A HACK BUT WORKS FOR NOW
    SL::VarVal* ret = nullptr;
    if (out_type == OutType::BOOL) {
        ret = new SL::VarVal((bool) val);
    } else if (out_type == OutType::INT) {
        ret = new SL::VarVal((int) val);
    } else {
        AssertDebug(false,
                    "NEED TO GENERALIZE THIS (^). IN GENERAL out_type can be anything. This was put like this because ::BOOL was the only time that was being used for testing.");
    }

    if (new_clone) {
        the_dag->clear();
    }

    assert(ret != nullptr);
    return ret;
}

SL::VarVal *
SketchFunctionEvaluator::passes(const SketchFunction *sk_func, const SolverLanguagePrimitives::InputAssignment *input_assignment)
{
    AssertDebug(false, "USE new_passes instead.");
    if(sk_func->get_dag()->get_failed_assert() != nullptr) {
        return new SL::VarVal(false);
    }
    VarStore* inputs = input_assignment->to_var_store(false);
    BooleanDAG* concretized_dag = sk_func->get_dag()->clone();
    sk_func->get_env()->doInline(*concretized_dag, *inputs, bool_node::SRC);
    bool ret = concretized_dag->get_failed_assert() == nullptr;
    if(!concretized_dag->getNodesByType(bool_node::CTRL).empty()) {
        assert(!ret);
    }

    inputs->clear();
    concretized_dag->clear();

    return new SL::VarVal(ret);
}

SL::VarVal *SketchFunctionEvaluator::new_passes(SketchFunction *sk_func,
                                                SolverLanguagePrimitives::InputAssignment *input_assignment) {

    if(sk_func->get_dag()->get_failed_assert() != nullptr) {
        return new SL::VarVal(false);
    }

    assert(sk_func->get_has_been_concretized());
    assert(sk_func->get_dag()->getNodesByType(bool_node::CTRL).empty());
    assert(sk_func->get_dag()->getNodesByType(bool_node::UFUN).empty());

    BooleanDAG *the_dag = nullptr;
    bool new_clone = false;

    if (sk_func->get_has_been_concretized()) {
        the_dag = sk_func->get_dag();
    } else {
        the_dag = sk_func->get_dag()->clone();
        new_clone = true;
        sk_func->get_env()->doInline(*the_dag);
    }

    VarStore *the_var_store = input_assignment->to_var_store(false);

    const bool assert_num_remaining_holes_is_0 = true;
    if (assert_num_remaining_holes_is_0) {
        int remaining_holes = the_dag->getNodesByType(bool_node::CTRL).size();
        AssertDebug(remaining_holes == 0,
                    "This dag should not havey any holes remaining, but it has " +
                    std::to_string(remaining_holes) + " remaining_holes.");
    }

    NodeEvaluator node_evaluator(*the_dag, sk_func->get_env()->floats);
    bool fails = node_evaluator.run(*the_var_store);

    if(new_clone) {
        the_dag->clear();
    }
    the_var_store->clear();

    return new SL::VarVal(!fails);
}