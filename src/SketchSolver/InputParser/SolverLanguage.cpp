//
// Created by kliment on 6/22/21.
//

#include "SolverLanguage.h"
#include "HyperSketchPrograms.h"

#include <iostream>
#include <map>
#include <utility>
#include <cassert>

#include "SketchFunction.h"
#include "Frontier.h"
#include "File.h"

#include "SATSolver.h"
#include "VarStore.h"
#include "CounterexampleFinder.h"
#include "SolverLanguageYaccHeader.h"
#include "FunctionMapTransformerLanguage.h"
#include "BenchmarkScore.h"

using namespace std;
using namespace SL;

map<string, string>
SolverLanguage::eval(const string& harness_name, const string &hypersketch_file_path, const string& file_name, FunctionMap &function_map,
                     FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc) {

    int init_num_global_dags = BooleanDAG::get_allocated().size();
    int init_num_global_nodes = bool_node::get_allocated().size();
    int init_function_map_transformer_size = function_map.transformer_size();

    map<string, string> final_hole_values;

    bool run_hsk_program = true;
    bool run_hardcoded_synthesis_strategy = true;

    if(run_hsk_program) {

        HyperSketchState state_abs = HyperSketchState(function_map, harness_name, file_name, floats, _args, _hc);
        HyperSketchState* state = &state_abs;

        assert(!state->function_map.empty());

        cout << "READING, LEXING, AND PARSING SOLVER PROGRAM FROM FILE: " << hypersketch_file_path << endl;

        parse_solver_langauge_program(state, hypersketch_file_path);

        cout << "DONE PARSING SOLVER PROGRAM" << endl;

        SL::VarVal *var_val_ret = state->eval();

        {
            assert(var_val_ret->is_sketch_function());

            SketchFunction *_concretized_function = var_val_ret->get_skfunc(false);

            SketchFunction* local_harness = function_map[harness_name];
//            local_harness->inline_this_dag();

//            if(_concretized_function->get_var_store()->get_inlining_tree() == nullptr) {
//                _concretized_function->get_inlining_tree()->get_var_store()->set_inlining_tree(
//                        _concretized_function->get_inlining_tree_non_const());
//            }

//            auto var_store = _concretized_function->get_inlining_tree_non_const()->get_var_store();
            auto sol = _concretized_function->get_solution();

            auto local_harness_clone = new BooleanDagUtility(
                    ((BooleanDagUtility*)local_harness)->_clone(true)->get_dag__non_const(),
                    local_harness->get_env(), nullptr, false);
            local_harness_clone->inline_this_dag();

//            local_harness->inline_this_dag(true);
            local_harness_clone->get_inlining_tree()->rename_var_store(*sol);
            local_harness->inline_this_dag();
            local_harness->get_inlining_tree()->rename_var_store(*sol);
            local_harness->concretize_this_dag(sol, bool_node::CTRL);
            local_harness->get_inlining_tree()->rename_var_store(*sol);

            auto ret = local_harness->get_var_store()->to_map_str_str(floats);

            cout << "PREDICT final hole->val map" << endl;
            for(const auto& it: ret) {
                cout << it.first << " "<< it.second<< endl;
            }
            cout << "done with hole->val map" << endl;

            return ret;

            {
                File *file = new File(_concretized_function, file_name);

                int num_passing_inputs =
                        _concretized_function->count_passing_inputs(file);

                cout << "HERE " << _concretized_function->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                file->clear();
            }

            //print function_map_transformer_program, parse it, and check that it's the same.

            bool save_and_test_fmtl_program = true;

            const string fmtl_program_file_name = "fmtl_program_file.fmtl";

            if(save_and_test_fmtl_program)
            {
                assert(final_hole_values.empty());
                string fmtl_program_str = _concretized_function->get_rep()->pretty_print(function_map, &final_hole_values);
                if(false) {
                    cout << "pretty_print FMTL program:" << endl;
                    cout << fmtl_program_str << endl;
                    cout << endl;
                }

                ofstream fmtl_program_file(fmtl_program_file_name);

                fmtl_program_file << fmtl_program_str;

                fmtl_program_file.close();

            }

            var_val_ret->clear_assert_0_shared_ptrs();
            state->clear();

            if(save_and_test_fmtl_program) {

                function_map.print();

                FMTL::FunctionMapTransformerState* fmtl_state = nullptr;
                fmtl_state = new FMTL::FunctionMapTransformerState(function_map);

                FMTL::parse_function_map_transformer_program(fmtl_state, fmtl_program_file_name);

                function_map.clear_erased_root_dag_reps();

                SL::VarVal* from_fmtl_var_val = fmtl_state->eval();

                SketchFunction* concretized_function_from_fmtl = from_fmtl_var_val->get_skfunc(false);

                File* file_from_fmtl = new File(concretized_function_from_fmtl, file_name);

                int num_passing_inputs =
                        concretized_function_from_fmtl->count_passing_inputs(file_from_fmtl);

                cout << "FROM FMTL" << endl;
                cout << "HERE " << concretized_function_from_fmtl->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file_from_fmtl->size()
                     << " (" << 100.0 * (float) num_passing_inputs / file_from_fmtl->size() << " %)" << endl;

                file_from_fmtl->clear();

                from_fmtl_var_val->clear_assert_0_shared_ptrs();
                fmtl_state->clear();

            }
        }
    }
    else if(run_hardcoded_synthesis_strategy){
        ofstream console("hypersketch_console.out");
        SketchFunction* _concretized_function =
                HyperSketchHarness(file_name, function_map["sketch_main__Wrapper"], _args.seed, console)
                        .main();
        {
            File *file = new File(_concretized_function, file_name);

            int num_passing_inputs =
                    _concretized_function->count_passing_inputs(file);

            cout << "FINAL SKETCH\t:\t" << _concretized_function->get_dag()->get_name() << endl;
            cout << "    ACCURACY\t:\t" << num_passing_inputs << " / " << file->size() << " ("
                 << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

            file->clear();
        }

        //print function_map_transformer_program, parse it, and check that it's the same.

        bool save_and_test_fmtl_program = true;

        const string fmtl_program_file_name = "fmtl_program_file.fmtl";

        if(save_and_test_fmtl_program)
        {
            assert(final_hole_values.empty());
            string fmtl_program_str = _concretized_function->get_rep()->pretty_print(function_map, &final_hole_values);
            if(false) {
                cout << "pretty_print FMTL program:" << endl;
                cout << fmtl_program_str << endl;
                cout << endl;
            }

            ofstream fmtl_program_file(fmtl_program_file_name);

            fmtl_program_file << fmtl_program_str;

            fmtl_program_file.close();

        }

        _concretized_function->clear();

        if(save_and_test_fmtl_program)
        {

            function_map.print();

            FMTL::FunctionMapTransformerState* fmtl_state = nullptr;
            fmtl_state = new FMTL::FunctionMapTransformerState(function_map);

            FMTL::parse_function_map_transformer_program(fmtl_state, fmtl_program_file_name);

            function_map.clear_erased_root_dag_reps();

            SL::VarVal* from_fmtl_var_val = fmtl_state->eval();

            SketchFunction* concretized_function_from_fmtl = from_fmtl_var_val->get_skfunc(false);

            File* file_from_fmtl = new File(concretized_function_from_fmtl, file_name);

            int num_passing_inputs =
                    concretized_function_from_fmtl->count_passing_inputs(file_from_fmtl);

            cout << "FROM FMTL" << endl;
            cout << "HERE " << concretized_function_from_fmtl->get_dag()->get_name() << endl;
            cout << "count\t" << num_passing_inputs << " / " << file_from_fmtl->size() << " ("
                 << 100.0 * (float) num_passing_inputs / file_from_fmtl->size() << " %)" << endl;

            file_from_fmtl->clear();

            from_fmtl_var_val->clear_assert_0_shared_ptrs();
            fmtl_state->clear();
        }
    }
    else {
        AssertDebug(false, "Specify dev mode.");
    }

    // assert everything has been garbage collected.
    {
        auto all_allocated_dags = BooleanDAG::get_allocated();
        cout << "Total num dags DAGs: " << all_allocated_dags.get_num() << endl;
        cout << "Max observed number of simultaneously allocated DAGs: " << all_allocated_dags.get_max() << endl;
        int dags_diff = all_allocated_dags.size() - init_num_global_dags;
        assert(dags_diff == 0);

        int all_remaining_inlining_trees = LightSkFuncSetter::all_inlining_trees.size();
        if(all_remaining_inlining_trees != 0) {
            cout << "WARNING: THERE ARE " << all_remaining_inlining_trees <<" REMAINING INLINING TREES DANGLING!!!" << endl;
//                    assert(all_remaining_inlining_trees == 0);
        }

        assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

        function_map.check_consistency();
        assert(function_map.contains_only_necessary());

        cout << "LightSkFuncSetter::max_count: " << LightSkFuncSetter::max_count <<endl;
        assert(LightSkFuncSetter::max_count <= 3);

        int transformer_size_diff = function_map.transformer_size() - init_function_map_transformer_size;
        if(transformer_size_diff != 0){
            int fmap_not_erased = function_map.num_not_erased();
            cout << "function_map.num_not_erased() = " << fmap_not_erased << endl;
            cout << "transformer_size_diff = " << transformer_size_diff << endl;
            assert(fmap_not_erased == transformer_size_diff);
//            function_map.print_not_erased();
        }

        function_map.soft_clear_transformer();
    }

    ofstream fout_by_name("hypersketch_perf_report__sorted_by_name.bench");
    fout_by_name << performance_summary_to_string() << endl;
    fout_by_name.flush();
    fout_by_name.close();

    ofstream fout_by_min("hypersketch_perf_report__sorted_by_min.bench");
    fout_by_min << performance_summary_to_string(true) << endl;
    fout_by_min.flush();
    fout_by_min.close();

    cout << "STATUSQUO final hole->val map" << endl;
    for(const auto& it: final_hole_values) {
        cout << it.first << " "<< it.second<< endl;
    }
    cout << "done with hole->val map" << endl;


    return final_hole_values;
}