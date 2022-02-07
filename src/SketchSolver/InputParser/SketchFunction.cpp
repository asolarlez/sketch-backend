//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"

#include <utility>

SketchFunction *SketchFunction::produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_clone) {

    if(do_clone) {
        return clone()->produce_concretization(var_store, var_type, false);
    }
    else {
        vector<string>* inlined_functions = nullptr;

        if(true) {
            //first get all inlined functions
            //clone all inlined functions;
            //replace the name inside the inlined function with eachother's

            for (auto it: get_env()->function_map) {
                for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
                    auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
                    assert(f != get_env()->function_map.end());
                }
            }

            BooleanDagUtility *to_get_inlined_fs = new BooleanDagUtility(get_dag()->clone(), get_env());
            to_get_inlined_fs->increment_shared_ptr();
            to_get_inlined_fs->inline_this_dag(var_store, var_type, inlined_functions);
            if (var_store.size() >= 1) {
                assert(to_get_inlined_fs->get_dag()->getNodesByType(bool_node::UFUN).empty());
                assert(to_get_inlined_fs->get_dag()->getNodesByType(bool_node::CTRL).empty());
            }
            assert(to_get_inlined_fs->get_dag()->getNodesByType(bool_node::UFUN).empty());
            to_get_inlined_fs->clear();

            for (auto it: get_env()->function_map) {
                for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
                    auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
                    assert(f != get_env()->function_map.end());
                }
            }

            set<string> inlined_fs_as_set;
            for (auto it: *inlined_functions) {
                inlined_fs_as_set.insert(it);
            }

            for (auto inlined_f_name: *inlined_functions) {
                auto it_f = get_env()->function_map.find(inlined_f_name);
                assert(it_f != get_env()->function_map.end());

                for (auto ufun_it: it_f->second->get_dag()->getNodesByType(bool_node::UFUN)) {
                    auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufname());
                    assert(f != get_env()->function_map.end());
                    if (inlined_fs_as_set.find(((UFUN_node *) ufun_it)->get_ufname()) == inlined_fs_as_set.end()) {
                        assert(false);
                        inlined_fs_as_set.insert(((UFUN_node *) ufun_it)->get_ufname());
                    }
                }
            }


            cout << "REPLACING WITH COPIES" << endl;
            map<string, SketchFunction *> to_inline_skfuncs;
            for (string inlined_function_name: *inlined_functions) {
                if (inlined_function_name != get_dag()->get_name()) {
                    assert(to_inline_skfuncs.find(inlined_function_name) == to_inline_skfuncs.end());

//                for(auto ufun_it : get_env()->function_map[inlined_function_name]->get_dag()->getNodesByType(bool_node::UFUN))
//                {
//                    string ufname = ((UFUN_node*)ufun_it)->get_ufname();
//                    assert(inlined_fs_as_set.find(ufname) != inlined_fs_as_set.end());
////                    cout << ufname << " ";
//                }

                    to_inline_skfuncs[inlined_function_name] = get_env()->function_map[inlined_function_name]->clone();

//                for(auto ufun_it : to_inline_skfuncs[inlined_function_name]->get_dag()->getNodesByType(bool_node::UFUN))
//                {
//                    string ufname = ((UFUN_node*)ufun_it)->get_ufname();
//                    cout << ufname << " ";
//                }
//                for(auto ufun_it : to_inline_skfuncs[inlined_function_name]->get_dag()->getNodesByType(bool_node::UFUN))
//                {
//                    string ufname = ((UFUN_node*)ufun_it)->get_ufname();
//                    assert(inlined_fs_as_set.find(ufname) != inlined_fs_as_set.end());
////                    cout << ufname << " ";
//                }

//                assert(to_inline_skfuncs[inlined_function_name]->get_dag()->get_name() != "condition__id587");

                    get_env()->function_map.insert(to_inline_skfuncs[inlined_function_name]->get_dag()->get_name(),
                                                   to_inline_skfuncs[inlined_function_name]);
                    cout << "REPLACE " << inlined_function_name << " WITH CLONE "
                         << to_inline_skfuncs[inlined_function_name]->get_dag()->get_name() << " : ";

//                for(auto ufun_it : to_inline_skfuncs[inlined_function_name]->get_dag()->getNodesByType(bool_node::UFUN))
//                {
//                    string ufname = ((UFUN_node*)ufun_it)->get_ufname();
//                    assert(inlined_fs_as_set.find(ufname) != inlined_fs_as_set.end());
//                    cout << ufname << " ";
//                }

                    cout << " ;;; " << endl;
                } else {
                    AssertDebug(false, "TODO recursive case");
                }

//            cout << "STATE" << endl;
//            for(auto skfunc : to_inline_skfuncs) {
//                cout << skfunc.first <<" "<< skfunc.second->get_dag()->get_name() << " :: " << endl;
//                for(auto ufun_it : skfunc.second->get_dag()->getNodesByType(bool_node::UFUN)) {
//                    string ufname = ((UFUN_node*)ufun_it)->get_ufname();
//                    cout << "here: " << ufname << endl;
//                }
//                cout << "---" << endl;
//            }
            }

            assert(to_inline_skfuncs.size() == inlined_functions->size());

            for (auto it: *inlined_functions) {
                assert(to_inline_skfuncs.find(it) != to_inline_skfuncs.end());
            }

            for (auto skfunc: to_inline_skfuncs) {
                cout << skfunc.first << " " << skfunc.second->get_dag()->get_name() << " :: " << endl;

                set<pair<string, string> > ufnames;
                for (auto ufun_it: skfunc.second->get_dag()->getNodesByType(bool_node::UFUN)) {
                    string ufname = ((UFUN_node *) ufun_it)->get_ufname();
                    string original = ((UFUN_node *) ufun_it)->get_original_ufname();
                    ufnames.insert(make_pair(ufname, original));
                }

                for (pair<string, string> ufname_now_original: ufnames) {
                    string ufname = ufname_now_original.first;
                    string original = ufname_now_original.second;

                    if (to_inline_skfuncs.find(ufname) == to_inline_skfuncs.end()) {
                         assert(false);
                    } else {
                        cout << "ufname: " << ufname << endl;
                        cout << "UFUN REPLACING " << ufname << " " << " WITH "
                             << to_inline_skfuncs[ufname]->get_dag()->get_name() << " UNDER_VAR "
                             << original << endl;

                        skfunc.second->replace(original,
                                               to_inline_skfuncs[ufname]->get_dag()->get_name());
                    }
                }
                cout << "---" << endl;
            }

            auto skfunc = make_pair(get_dag()->get_name(), this);
            set<pair<string, string> > ufnames;
            for (auto ufun_it: skfunc.second->get_dag()->getNodesByType(bool_node::UFUN)) {
                string ufname = ((UFUN_node *) ufun_it)->get_ufname();
                string original = ((UFUN_node *) ufun_it)->get_original_ufname();
                ufnames.insert(make_pair(ufname, original));
            }

            for (pair<string, string> ufname_now_original: ufnames) {
                string ufname = ufname_now_original.first;
                string original = ufname_now_original.second;

                if (to_inline_skfuncs.find(ufname) == to_inline_skfuncs.end()) {
                    assert(false);
                } else {
                    cout << "ufname: " << ufname << endl;
                    cout << "UFUN REPLACING " << ufname << " " << " WITH "
                         << to_inline_skfuncs[ufname]->get_dag()->get_name() << " UNDER_VAR "
                         << original << endl;

                    skfunc.second->replace(original,
                                           to_inline_skfuncs[ufname]->get_dag()->get_name());
                }
            }

            inlined_functions->clear();
            inlined_functions = nullptr;
        }

        assert(!get_dag()->get_failed_assert());

        inline_this_dag(var_store, var_type, inlined_functions);



        if(var_type == bool_node::CTRL && var_store.size() >= 1) {
            SATSolverResult sat_solver_result = SAT_UNDETERMINED;
            if(get_dag()->get_failed_assert() != nullptr) {
                sat_solver_result = SAT_UNSATISFIABLE;
            }
            else if(!get_dag()->getNodesByType(bool_node::CTRL).empty())
            {
                sat_solver_result = SAT_NOT_FULLY_CONCRETIZED;
            }
            else if(get_dag()->get_failed_assert() == nullptr) {
                sat_solver_result = SAT_SATISFIABLE;
            }
            else
            {
                assert(false);
            }

            auto compare_solution = new SolverLanguagePrimitives::HoleAssignment(sat_solver_result, &var_store, get_env()->floats);

            if(solution != nullptr) {
                if(!(*solution == *compare_solution))
                {
                    cout << solution->to_string() << endl;
                    cout << compare_solution->to_string() << endl;
                    assert(false);
                }
                compare_solution->clear();
                delete compare_solution;
            }
            else {
                solution = compare_solution;
            }
        }


        rep = get_env()->function_map.concretize(
                get_dag()->get_name(), var_store, var_type, inlined_functions);

        if(!get_dag()->getNodesByType(bool_node::UFUN).empty())
        {
            if(!get_dag()->getNodesByType(bool_node::CTRL).empty()) {
                assert(var_store.size() == 0 || solution->get_sat_solver_result() == SAT_UNSATISFIABLE);
            }
        }

        if(var_store.size() >= 1) {
            if (solution->get_sat_solver_result() == SAT_SATISFIABLE) {
                assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
                if (var_store.size() >= 1) assert(get_dag()->getNodesByType(bool_node::CTRL).empty());
            }
        }
        else {
            assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
        }
        {
            replaced_labels.clear();
            original_labels.clear();
        }

        delete inlined_functions;

        return this;
    }
}

SketchFunction *SketchFunction::clone(const string& explicit_name) {

    BooleanDAG* cloned_dag = get_dag()->clone(explicit_name);

    auto new_primitive = get_env()->function_map.clone(get_dag()->get_name(), cloned_dag->get_name());

    SolverLanguagePrimitives::HoleAssignment* solution_clone = nullptr;

    if(solution != nullptr) {
        solution_clone = new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    return new SketchFunction(
            cloned_dag, get_env(), solution_clone, replaced_labels, original_labels, new_primitive, responsibility);
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

    if(BooleanDagUtility::soft_clear()) {
        get_env()->function_map.erase(dag_name);


        for(auto sk_it : get_env()->function_map)
        {
            auto ufuns = sk_it.second->get_dag()->getNodesByType(bool_node::UFUN);
            for(auto it_ufun : ufuns)
            {
                string ufname = ((UFUN_node*)it_ufun)->get_ufname();
                assert(get_env()->function_map.find(ufname) != get_env()->function_map.end());
            }
        }

        assert(dag_name != "composite_predicate__id104");


        if (solution != nullptr) {
            solution->clear();
            delete solution;
        }
        for(auto it: responsibility) {
            it.second->_clear();
        }
        delete this;
    }
}

void SketchFunction::clear(){
    global_clear_id++;
    _clear();
}

void SketchFunction::replace(const string replace_this, const string with_this) {
    assert(new_way);


    if(replaced_labels.find(replace_this) == replaced_labels.end()) {
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


    } else {

        assert(replaced_labels.find(replace_this) != replaced_labels.end());


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
        responsibility[prev_dep_name]->clear();
        responsibility.erase(prev_dep_name);

    }


    auto dependency_it = get_env()->function_map.find(with_this);
    assert(dependency_it != get_env()->function_map.end());
    assert(responsibility.find(with_this) == responsibility.end());
    responsibility[with_this] = dependency_it->second;
    responsibility[with_this]->increment_shared_ptr();


    replaced_labels[replace_this] = with_this;
}

SketchFunction * SketchFunction::produce_get(const string& get_the_dag_under_this_varname) {
//    cout << "in produce get " << get_dag()->get_name() <<" "<< get_the_dag_under_this_varname << endl;
    return get_env()->function_map.produce_get(get_dag()->get_name(), get_the_dag_under_this_varname);
}

bool SketchFunction::solution_is_null() {
    return solution == nullptr;
}

SolverLanguagePrimitives::HoleAssignment *SketchFunction::get_same_soluton() {
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

    rep = get_env()->function_map.replace_label_with_another(get_dag()->get_name(), key, key);

    replaced_labels.erase(it);
    original_labels.erase(original_it);
}

void SketchFunction::clear_assert_num_shared_ptr_is_0() {
    string dag_name = get_dag()->get_name();

    if(BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()) {
        get_env()->function_map.erase(dag_name);
        if (solution != nullptr) {
            solution->clear();
            delete solution;
        }

        delete this;
    }
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

void BooleanDagUtility::swap_env(ProgramEnvironment *new_env) {
    assert(original_program_env == nullptr);
    original_program_env = env;
    assert(new_env != env);
    env = new_env;
}

void BooleanDagUtility::reset_env_to_original() {
    assert(original_program_env != nullptr);
    env = original_program_env;
    original_program_env = nullptr;
}

bool BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    int prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num - 1 == BooleanDAG::get_allocated().size());
//    root_dag = nullptr;
    return true;
}