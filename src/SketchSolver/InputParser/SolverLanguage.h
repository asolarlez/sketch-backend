//
// Created by kliment on 6/22/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGE_H
#define SKETCH_SOURCE_SOLVERLANGUAGE_H



#include <iostream>
#include <string>
#include <map>
#include <utility>
#include <vector>
#include <cassert>
#include <fstream>

#include "SATSolver.h"
#include "VarStore.h"
#include "CEGISFinder.h"
#include "CEGISChecker.h"
#include "CEGISSolver.h"
#include "NodeHardcoder.h"
#include "CounterexampleFinder.h"
#include "SolverLanguageYaccHeader.h"
#include "SketchFunction.h"
#include "FunctionMapTransformerLanguage.h"

#include "File.h"
#include "GenericFile.h"

using namespace std;


class SolverLanguage {
public:
    SolverLanguage() = default;
    void eval(string solver_program_file_name, FunctionMap &function_map, const string& file_name, FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc,
         bool hasGoodEnoughSolution)
    {

        SolverProgramState state_abs =
                SolverProgramState(function_map, file_name, floats, _args, _hc, hasGoodEnoughSolution);

        SolverProgramState* state = &state_abs;

        {

            assert(!state->function_map.empty());

            int init_num_global_dags = BooleanDAG::get_allocated().size();
            int init_num_global_nodes = bool_node::get_allocated().size();

            BooleanDagLightUtility* local_harness = ((BooleanDagUtility*)state->function_map["sketch_main__Wrapper"])->clone();
            local_harness->increment_shared_ptr();

            const bool test_generic_file = false;
            if(test_generic_file) {
                File *ground_truth_file = new File(local_harness, file_name, state->floats, state->args.seed);
                File *predicted_file = new File(local_harness, new GenericFile(file_name, state->args.seed), state->floats, state->args.seed);
                assert(*ground_truth_file == *predicted_file);
                assert(false);
            }

            FunctionMap& function_map = local_harness->get_env()->function_map;

            int init_function_map_transformer_size = function_map.transformer_size();

            parse_solver_langauge_program(state, solver_program_file_name);

            SL::VarVal *var_val_ret = state->eval();

            if(var_val_ret->is_solution_holder()) {
                AssertDebug(false, "TODO: add printing of FMTL program.")
                const HoleVarStore * solution_holder = var_val_ret->get_hole_var_store(false);

                delete var_val_ret;
                state->clear();

                local_harness->concretize_this_dag(solution_holder, bool_node::CTRL);

                File *file = new File(local_harness, file_name, state->floats, state->args.seed);

                int num_passing_inputs =
                        local_harness->count_passing_inputs(file);

                cout << "HERE " << local_harness->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                file->clear();

                local_harness->clear();

                assert(BooleanDAG::get_allocated().size() - init_num_global_dags == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);


                assert(init_function_map_transformer_size == function_map.transformer_size());
            }
            else
            {
                local_harness->clear();

                assert(var_val_ret->is_sketch_function());

                SketchFunction *_concretized_function = var_val_ret->get_skfunc(false);

                {

                    File *file = new File(_concretized_function, file_name, state->floats, state->args.seed);

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
                    string fmtl_program_str = _concretized_function->get_rep()->pretty_print(function_map);
                    cout << "pretty_print FMTL program:" << endl;
                    cout << fmtl_program_str << endl;
                    cout << endl;

                    ofstream fmtl_program_file(fmtl_program_file_name);

                    fmtl_program_file << fmtl_program_str;

                    fmtl_program_file.close();

                }

                var_val_ret->clear_assert_0_shared_ptrs();
                state->clear();

                if(save_and_test_fmtl_program)
                {

                    function_map.print();

                    FMTL::FunctionMapTransformerState* fmtl_state = nullptr;
                    fmtl_state = new FMTL::FunctionMapTransformerState(function_map);

                    FMTL::parse_function_map_transformer_program(fmtl_state, fmtl_program_file_name);

                    function_map.clear_erased_root_dag_reps();


                    SL::VarVal* from_fmtl_var_val = fmtl_state->eval();

                    SketchFunction* concretized_function_from_fmtl = from_fmtl_var_val->get_skfunc(false);

                    File* file_from_fmtl = new File(concretized_function_from_fmtl, file_name, state->floats, state->args.seed);

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

                int dags_diff = BooleanDAG::get_allocated().size() - init_num_global_dags;
                assert(dags_diff == 0);

                int all_remaining_inlining_trees = LightSkFuncSetter::all_inlining_trees.size();
                assert(all_remaining_inlining_trees == 0);

                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

                int transformer_size_diff = function_map.transformer_size() - init_function_map_transformer_size;

                function_map.check_consistency();
                assert(function_map.contains_only_necessary());

                cout << "LightSkFuncSetter::max_count: " << LightSkFuncSetter::max_count <<endl;
                assert(LightSkFuncSetter::max_count <= 3);

//                if(transformer_size_diff != 0){
//                    function_map.print_not_erased();
//                }

                function_map.soft_clear_transformer();
            }
        }
    }
};

#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
