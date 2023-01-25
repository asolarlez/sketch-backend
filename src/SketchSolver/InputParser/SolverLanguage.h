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

#include "SketchFunction.h"
#include "SolverLanguagePrimitives.h"
#include "Frontier.h"
#include "File.h"

using namespace SolverLanguagePrimitives;

class HyperSketchHarness;

class HyperSketchPrograms {
protected:
    template<typename T>
    class vector : public std::vector<T> {
    public:
        void sort() {
            std::sort(std::vector<T>::begin(), std::vector<T>::end());
        }

        void reverse() {
            std::reverse(std::vector<T>::begin(), std::vector<T>::end());
        }
    };

    typedef SL::SketchFunction SketchFunction;

    vector<pair<int, SketchFunction *> > best_effort_programs(
            SketchFunction *harness, File *file, int num_trials, int num_rows, float timeout) {
        auto init_timestamp = chrono::steady_clock::now();

        vector<pair<int, SketchFunction *> > ret_dags;
        for (int trial_id = 0; trial_id < num_trials; trial_id++) {
            File *subset_file = file->produce_subset_file(num_rows);
            SketchFunction *program = harness->deep_clone();
            program->increment_shared_ptr();
            CEGISSolverResult solver_result = solve(program, subset_file, timeout);
            HoleVarStore* solution = solver_result.final_ctrl_var_store;
            program->make_executable(solution);
            int score = program->count_passing_inputs(file);
            program->increment_shared_ptr();
            ret_dags.push_back(pair<int, SketchFunction *>(score, program));
            program->clear();
            assert(program->get_num_shared_ptr() == 1);
        }

        ret_dags.sort();
        ret_dags.reverse();

        timestamp(init_timestamp,
                  "best_effort_programs__num_trials" + std::to_string(num_trials) + "__num_rows_" + std::to_string(num_rows));
        return ret_dags;
    }

    Frontier<int, SketchFunction *> to_frontier(vector<pair<int, SketchFunction *> > scores_and_programs) {
        Frontier<int, SketchFunction *> ret = Frontier<int, SketchFunction *>(2); // program size and accuracy
        for (int i = 0; i < scores_and_programs.size(); i++) {
            int score = scores_and_programs[i].first;
            SketchFunction *program = scores_and_programs[i].second;
            vector<int> point = vector<int>();
            point.push_back(-score);
            point.push_back(program->size());
            cout << "insert " << "#" << i << " : point(" << point[0] << ", " << point[1] << ")" << endl;
            auto was_inserted = ret.insert(point, program);
            if (was_inserted != nullptr) {
                cout << "accepted" << endl;
            } else {
                cout << "rejected" << endl;
            }
        }
        return ret;
    }

    SketchFunction *main__best_effort_programs(SketchFunction* harness, File* file, int num_trials, int num_rows, float timeout) {
        vector<pair<int, SketchFunction *> > ret_dags =
                best_effort_programs(harness, file, num_trials, num_rows, timeout);
        for (int i = 1; i < ret_dags.size(); i++) {
            ret_dags[i].second->clear();
        }
        return ret_dags[0].second;
    }

    SketchFunction *main__best_effort_frontier(SketchFunction* harness, File* file, int num_trials, int num_rows, float timeout) {
        vector<pair<int, SketchFunction *> > ret_dags =
                best_effort_programs(harness, file, num_trials, num_rows, timeout);
        cout << "ret_dags" << endl;
        for (int i = 0; i < ret_dags.size(); i++) {
            cout << ret_dags[i].first << " " << ret_dags[i].second << endl;
        }
        Frontier<int, SketchFunction *> ret_frontier = to_frontier(ret_dags);
        auto ret = ret_frontier[0].second;
        ret->increment_shared_ptr();

        cout << "frontier" << endl;
        for (int i = 0; i < ret_frontier.size(); i++) {
            std::vector<int> score = ret_frontier[i].first;
            for (int j = 0; j < score.size(); j++) {
                cout << score[j] << " ";
            }
            cout << endl;
        }

        for(int i = 0;i<ret_dags.size();i++) {
            ret_dags[i].second->clear();
        }
        for(int i = 0;i<ret_frontier.size();i++) {
            ret_frontier[i].second->clear();
        }
        return ret;
    }

    void add_to_frontier(
            Frontier<int, SketchFunction*>& frontier, int trial_id, int find_step_id,
            const SketchFunction *_program, HoleVarStore* solution, File *file)
    {
        SketchFunction *concretized_program = _program->produce_concretization(solution, bool_node::CTRL);
        concretized_program->increment_shared_ptr();

        int score = concretized_program->count_passing_inputs(file);
        vector<int> point = vector<int>();
        point.push_back(-score);
        point.push_back(concretized_program->size());

        auto was_inserted = frontier.insert(point, concretized_program);
        cout << "insert #" << trial_id << " find_step_id #" << find_step_id << ": point(" << point[0] << ", " << point[1] << ")" << endl;
        if (was_inserted != nullptr) {
            cout << "accepted" << endl;
        } else {
            cout << "rejected" << endl;
        }

        concretized_program->clear();
    }

    Frontier<int, SketchFunction *>
    best_effort_frontier(const SketchFunction *harness, File *file, int num_trials, int num_rows, float timeout) {
        auto init_timestamp = chrono::steady_clock::now();

        Frontier<int, SketchFunction *> ret_frontier = Frontier<int, SketchFunction *>(2);
        for (int trial_id = 0; trial_id < num_trials; trial_id++) {
            File *subset_file = file->produce_subset_file(num_rows);
            SketchFunction *program = harness->deep_clone();
            program->increment_shared_ptr();
            CEGISSolverResult solver_result = solve(program, subset_file, timeout, file);
            HoleVarStore* solution = solver_result.final_ctrl_var_store;
            add_to_frontier(ret_frontier, trial_id, -1, program, solution, file);
            for(int find_step_id = 0;find_step_id<solver_result.intermediate_solutions.size();find_step_id++) {
                add_to_frontier(ret_frontier, trial_id, find_step_id, program, solver_result.intermediate_solutions[find_step_id], file);
            }
            program->clear();
        }

        timestamp(init_timestamp,
                  "best_effort_frontier__num_trials" + std::to_string(num_trials) + "__num_rows_" + std::to_string(num_rows));

        return ret_frontier;
    }

    SketchFunction * main__best_effort_frontier__2(const SketchFunction *harness, File* file, int num_trials, int num_rows, float timeout) {
        Frontier<int, SketchFunction *> ret_frontier =
                best_effort_frontier(
                        harness, file, num_trials, num_rows, timeout);

        cout << "frontier: " << endl;
        for (int i = 0; i < ret_frontier.size(); i++) {
            std::vector<int> score = ret_frontier[i].first;
            for (int j = 0; j < score.size(); j++) {
                cout << score[j] << " ";
            }
            cout << endl;
        }

        SketchFunction* ret_dag = ret_frontier[0].second;
        ret_dag->increment_shared_ptr();
        ret_frontier.clear();
        return ret_dag;
    }

    Frontier<int, SketchFunction *>
    best_effort_frontier__3(const SketchFunction *harness, File *file, float budget) {
        auto init_timestamp = chrono::steady_clock::now();

        Frontier<int, SketchFunction *> ret_frontier = Frontier<int, SketchFunction *>(2);

        // representative CEGIS initial train of thought was here.

        return ret_frontier;
    }

    SketchFunction * main__best_effort_frontier__3(const SketchFunction *harness, File* file, float budget) {
        Frontier<int, SketchFunction *> ret_frontier =
                best_effort_frontier__3(
                        harness, file, budget);

        cout << "frontier: " << endl;
        for (int i = 0; i < ret_frontier.size(); i++) {
            std::vector<int> score = ret_frontier[i].first;
            for (int j = 0; j < score.size(); j++) {
                cout << score[j] << " ";
            }
            cout << endl;
        }

        SketchFunction* ret_dag = ret_frontier[0].second;
        ret_dag->increment_shared_ptr();
        ret_frontier.clear();
        return ret_dag;
    }
};

class HyperSketchHarness: private HyperSketchPrograms
{
    string file_name;
    SketchFunction* sketch_main__Wrapper;

public:

    HyperSketchHarness(string _file_name, SketchFunction *_sketch_main__Wrapper) :
            file_name(std::move(_file_name)), sketch_main__Wrapper(_sketch_main__Wrapper) {}

    SketchFunction* main() {
        {
            SketchFunction *harness = sketch_main__Wrapper;
            File *file = new File(file_name, harness);

            float budget = float(30);

            return main__best_effort_frontier__3(harness, file, budget);
        }

        {
            int num_trials = 6;
            int num_rows = 120;
            float timeout = float(2);

            SketchFunction *harness = sketch_main__Wrapper;
            File *file = new File(file_name, harness);

            return main__best_effort_frontier__2(harness, file, num_trials, num_rows, timeout);
            return main__best_effort_frontier(harness, file, num_trials, num_rows, timeout);
            return main__best_effort_programs(harness, file, num_trials, num_rows, timeout);
        }
    }
};

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
#include "FunctionMapTransformerLanguage.h"

using namespace std;
using namespace SL;

#include "BenchmarkScore.h"


class SolverLanguage {
public:
    SolverLanguage() = default;
    map<string, string> eval(string hypersketch_file_path, FunctionMap &function_map, const string& file_name, FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc,
                             bool hasGoodEnoughSolution)
    {
        int init_num_global_dags = BooleanDAG::get_allocated().size();
        int init_num_global_nodes = bool_node::get_allocated().size();
        int init_function_map_transformer_size = function_map.transformer_size();


        map<string, string> final_hole_values;

        bool run_hsk_program = true;
        bool run_hardcoded_synthesis_strategy = !run_hsk_program;

        if(run_hsk_program)
        {

            HyperSketchState state_abs =
                    HyperSketchState(function_map, file_name, floats, _args, _hc, hasGoodEnoughSolution);
            HyperSketchState* state = &state_abs;

            assert(!state->function_map.empty());

            cout << "READING, LEXING, AND PARSING SOLVER PROGRAM FROM FILE: " << hypersketch_file_path << endl;

            parse_solver_langauge_program(state, hypersketch_file_path);

            cout << "DONE PARSING SOLVER PROGRAM" << endl;

            SL::VarVal *var_val_ret = state->eval();

            {
                assert(var_val_ret->is_sketch_function());

                SketchFunction *_concretized_function = var_val_ret->get_skfunc(false);

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
            SketchFunction*_concretized_function =
                    HyperSketchHarness(file_name, function_map["sketch_main__Wrapper"])
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

//                int transformer_size_diff = function_map.transformer_size() - init_function_map_transformer_size;
//                if(transformer_size_diff != 0){
//                    function_map.print_not_erased();
//                }

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

        return final_hole_values;
    }
};

#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
