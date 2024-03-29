//
// Created by Kliment Serafimov on 8/24/22.
//

#ifndef SKETCH_HYPERSKETCHPROGRAMS_H
#define SKETCH_HYPERSKETCHPROGRAMS_H

#include "Frontier.h"
#include "SketchFunction.h"
#include "SolverLanguagePrimitives.h"

using namespace SolverLanguagePrimitives;

class HyperSketchPrograms {
protected:
    ofstream& console;
    HyperSketchPrograms(ofstream& _console): console(_console) {}
    template<typename T>
    class vector : public std::vector<T> {
    public:
        vector() = default;
        vector(size_t n, const T& init): std::vector<T>(n, init) {}
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
            CEGISSolverResult solver_result = solve(program, subset_file, 1000000*timeout*num_rows, timeout);
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
            console << "insert " << "#" << i << " : point(" << point[0] << ", " << point[1] << ")" << endl;
            auto was_inserted = ret.insert(point, program);
            if (was_inserted != nullptr) {
                console << "accepted" << endl;
            } else {
                console << "rejected" << endl;
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
        console << "ret_dags" << endl;
        for (int i = 0; i < ret_dags.size(); i++) {
            console << ret_dags[i].first << " " << ret_dags[i].second << endl;
        }
        Frontier<int, SketchFunction *> ret_frontier = to_frontier(ret_dags);
        auto ret = ret_frontier[0].second;
        ret->increment_shared_ptr();


        for(int i = 0;i<ret_dags.size();i++) {
            ret_dags[i].second->clear();
        }
        for(int i = 0;i<ret_frontier.size();i++) {
            ret_frontier[i].second->clear();
        }
        return ret;
    }

    void add_to_frontier(
            Frontier<int, SketchFunction*>& frontier, int subset_id, int trial_id, int find_step_id,
            const SketchFunction *_program, HoleVarStore* solution, File *file)
    {
        SketchFunction *concretized_program = _program->produce_concretization(solution, bool_node::CTRL);
        concretized_program->increment_shared_ptr();

        int score = concretized_program->count_passing_inputs(file);
        vector<int> point = vector<int>();
        point.push_back(-score);
        point.push_back(concretized_program->size());

        auto was_inserted = frontier.insert(point, concretized_program);
        console << "insert trial_" << trial_id << " find_step_" << find_step_id << "  subset_" << subset_id << " : point(" << point[0] << ", " << point[1] << ")" << endl;
        if (was_inserted != nullptr) {
            console << "accepted" << endl;
        } else {
            console << "rejected" << endl;
        }

        concretized_program->clear();
    }

    Frontier<int, SketchFunction *>
    best_effort_frontier(const SketchFunction *harness, File *file, int num_trials, int num_rows, float timeout) {
        auto init_timestamp = chrono::steady_clock::now();

        Frontier<int, SketchFunction *> ret_frontier = Frontier<int, SketchFunction *>(2);
        for (int trial_id = 0; trial_id < num_trials; trial_id++) {
            const int subset_id = trial_id;
            File *subset_file = file->produce_subset_file(num_rows);
            SketchFunction *program = harness->deep_clone();
            program->increment_shared_ptr();
            CEGISSolverResult solver_result = solve(program, subset_file, std::numeric_limits<long long>::max(), timeout, file);
            HoleVarStore* solution = solver_result.final_ctrl_var_store;
            add_to_frontier(ret_frontier, subset_id, trial_id, -1, program, solution, file);
            for(int find_step_id = 0;find_step_id<solver_result.intermediate_solutions.size();find_step_id++) {
                add_to_frontier(ret_frontier, subset_id, trial_id, find_step_id, program, solver_result.intermediate_solutions[find_step_id], file);
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

        console << "frontier: " << endl;
        for (int i = 0; i < ret_frontier.size(); i++) {
            std::vector<int> score = ret_frontier[i].first;
            for (int j = 0; j < score.size(); j++) {
                console << score[j] << " ";
            }
            console << endl;
        }

        SketchFunction* ret_dag = ret_frontier[0].second;
        ret_dag->increment_shared_ptr();
        ret_frontier.clear();
        return ret_dag;
    }

    string frontier_to_string(const Frontier<int, SketchFunction *>& frontier, const string& label = "") {
        string ret;
        if(label != "") {
            ret += label + "\n";
        }
        for (int i = 0; i < frontier.size(); i++) {
            std::vector<int> score = frontier[i].first;
            for (int j = 0; j < score.size(); j++) {
                if(j != 0) {
                     ret += ", ";
                }
                ret += std::to_string(score[j]);
            }
            ret += "\n";
        }

        return ret;
    }

    Frontier<int, SketchFunction *> best_effort_frontier__3(
            const SketchFunction *harness, File *file,
            float budget, float timeout_per_find, int num_examples_per_subset, int num_subsets, int _seed) {
        auto init_timestamp = chrono::steady_clock::now();

        Frontier<int, SketchFunction *> global_frontier = Frontier<int, SketchFunction *>(2);
//        vector<vector<int> > subsets;
//        int num_subsets = budget/timeout_per_example_float;
//        for(int i = 0;i<min(num_subsets, (int)file->size());i++)
//        {
//            vector<int> singleton_subset;
//            singleton_subset.push_back(i);
//            subsets.push_back(singleton_subset);
//        }

//        int total_num_counterexamples = budget/timeout_per_find; // counterexample ~ attempt at integration
//        int num_times_example_as_counterexample = total_num_counterexamples/num_examples_per_subset; // example ~ a person; subset ~ a party;
        assert(num_subsets >= 1);

        const float timeout_per_subset_float = budget/num_subsets;
        const long long timeout_per_subset = timeout_per_subset_float * 1000000; // in microseconds (us)

        int successes = 0;
        int total_num_trials = 0;

        vector<Frontier<int, SketchFunction *> > frontier_per_subset =
                vector<Frontier<int, SketchFunction *> >(num_subsets, Frontier<int, SketchFunction *>(2));
        for(int subset_id = 0;subset_id<num_subsets;subset_id++)
        {
//            File* subset_file = new File();
//            for(int j = 0; j < subsets[subset_id].size();j++) {
//                subset_file->push_back(file->at(subsets[subset_id][j]));
//            }
            auto init_subset_timestep = chrono::steady_clock::now();

            vector<int> ids;
            File* subset_file = file->produce_subset_file(num_examples_per_subset, &ids);

            console << "subset_id #" << subset_id << " chosen ids: ";
            for(int i = 0;i<ids.size();i++){
                console << ids[i] << " ";
            }
            console << endl;
            console << "==================================" << endl;

            auto spent_budget = elapsed_time(init_subset_timestep);

            int local_seed = _seed;

            auto& local_frontier = frontier_per_subset[subset_id];

            int trial_id = 0;

            while(spent_budget < timeout_per_subset) {

                auto trial_init_timestamp = chrono::steady_clock::now();
                SketchFunction *program = harness->deep_clone();
                program->increment_shared_ptr();

                cout << "calling solve" << endl;
//                cout << "calling solve(" << program->get_dag_name() << ", " << //todo: complete this debug line
                CEGISSolverResult solver_result = solve(program, subset_file, timeout_per_subset-spent_budget, timeout_per_find, file, &local_seed);
                local_seed++;
                successes += solver_result.success;
                HoleVarStore *solution = solver_result.final_ctrl_var_store;
                console << "success: " << solver_result.success << endl;
                add_to_frontier(local_frontier, subset_id, trial_id, -1, program, solution, file);
                program->clear();
                total_num_trials++;
                console << "success rate: " << 100.0 * successes / total_num_trials << " %" << endl;

                spent_budget = elapsed_time(init_subset_timestep);
                console << "spent_budget " << spent_budget/1000 << " / " << timeout_per_subset/1000 << " (ms)";
                console << " (" << 100*((float)spent_budget/timeout_per_subset) << " %)" << endl;

                timestamp(trial_init_timestamp, "subset_id"+std::to_string(subset_id));

                console <<  endl;

                trial_id++;
            }

            console << "---------------------------------" << endl;
            console << "frontier for subset id " << subset_id << endl;
            console << frontier_to_string(local_frontier);
            console << "---------------------------------" << endl;

            for(int i = 0;i<local_frontier.size();i++) {
                global_frontier.insert(local_frontier[i].first, local_frontier[i].second);
            }

            local_frontier.clear();

            console << "==================================" << endl;

            string subset_run_label = "subset_id_"+std::to_string(subset_id)+"_b"+std::to_string(timeout_per_subset);

            console << elapsed_time_as_str(subset_run_label, init_subset_timestep) << endl;
            timestamp(init_subset_timestep, subset_run_label);
        }

        timestamp(init_timestamp, "best_effort_frontier__3___b" + std::to_string(budget));

        return global_frontier;

    }

    SketchFunction * main__best_effort_frontier__3(
            const SketchFunction *harness, File* file,
            float budget, float timeout_per_find,
            int examples_per_subset, int num_subsets, int seed) {
        Frontier<int, SketchFunction *> ret_frontier =
                              best_effort_frontier__3(
                                      harness, file, budget, timeout_per_find, examples_per_subset, num_subsets, seed);

        console << "ret_frontier:" << endl << frontier_to_string(ret_frontier) << endl;

        SketchFunction* ret_dag = ret_frontier[0].second;
        ret_dag->increment_shared_ptr();
        ret_frontier.clear();

        return ret_dag;
    }

    SketchFunction * main__best_effort_frontier__4(
            const SketchFunction *harness, File* file, float timeout_per_find, int num_examples_per_subset, int num_subsets, int seed)
    {

        vector<Frontier<int, SketchFunction *> > frontiers;
        Frontier<int, SketchFunction*> global_frontier = Frontier<int, SketchFunction*>(2);
        float budget_per_iter = 10.0;
        for(int budget_id = 0; budget_id < 9; budget_id++, budget_per_iter *= 2) {

            auto timestamp_init_run = std::chrono::steady_clock::now();

            console << "START Running best_effort_frontier__3 with budget = " << budget_per_iter << endl;

            frontiers.push_back(best_effort_frontier__3(
                    harness, file, budget_per_iter, timeout_per_find, num_examples_per_subset, num_subsets, seed));

            auto& local_frontier = frontiers[frontiers.size()-1];

            console << frontier_to_string(local_frontier, "local_frontier__b_"+std::to_string(budget_per_iter));

            console << "DONE Running best_effort_frontier__3 with budget = " << budget_per_iter << endl;
            for(int i = 0;i<local_frontier.size();i++) {
                console << "point(" << local_frontier.get_frontier()[i]->score_to_string() << ")" << endl;
                auto was_inserted = global_frontier.insert(local_frontier[i].first, local_frontier[i].second);
                if (was_inserted != nullptr) {
                    console << "accepted" << endl;
                } else {
                    console << "rejected" << endl;
                }
            }

            console << frontier_to_string(global_frontier, "global_frontier__b_"+std::to_string(budget_per_iter));

            string run_label = "main_run_id_"+std::to_string(budget_id)+"_b_"+std::to_string(budget_per_iter);
            console << elapsed_time_as_str(run_label, timestamp_init_run) << endl;
            timestamp(timestamp_init_run, run_label);
        }

        console << "FINAL OUTPUT" << endl;

        for(int i = 0;i<frontiers.size();i++) {
            console << "frontier_i" << i << endl;
            console << frontier_to_string(frontiers[i]) << endl;
            frontiers[i].clear();
        }

        console << "global_frontier" << endl;
        console << frontier_to_string(global_frontier) << endl;

        console << performance_summary_to_string(true) << endl;

        SketchFunction* ret_dag = global_frontier[0].second;
        ret_dag->increment_shared_ptr();
        global_frontier.clear();

        return ret_dag;
    }
};

class HyperSketchHarness: private HyperSketchPrograms
{
    string file_name;
    SketchFunction* sketch_main__Wrapper;
    int seed;

public:

    HyperSketchHarness(string _file_name, SketchFunction *_sketch_main__Wrapper, int _seed, ofstream& _console) :
            file_name(std::move(_file_name)), sketch_main__Wrapper(_sketch_main__Wrapper), seed(_seed),
            HyperSketchPrograms(_console) {}

    SketchFunction* main() {
        {
            SketchFunction *harness = sketch_main__Wrapper;
            File *file = new File(file_name, harness);
            int total_num_subsets = 10;
            int num_examples_per_subset = 1;
            float timeout_per_find = 1.0;
            return main__best_effort_frontier__4(harness, file, timeout_per_find, num_examples_per_subset, total_num_subsets, seed);
        }
        {
            /// 1 example frontier learning.
            SketchFunction *harness = sketch_main__Wrapper;
            File *file = new File(file_name, harness);

            int num_examples = 10;
            float timeout_per_find = 3.0;
            float budget = timeout_per_find * num_examples;

//            float budget = float(1800);
//            frontier:
            //-279 61
            //-274 49
            //-271 28
            //-242 23
            //-240 22
            //-224 19
            //-205 16

//            float budget = float(100);
            //frontier:
            //-264 67
            //-256 64
            //-252 54
            //-246 53
            //-241 44
            //-232 40
            //-231 39
            //-224 19

            return main__best_effort_frontier__3(harness, file, budget, timeout_per_find, 1, num_examples, seed);
        }

        {
            //int num_trials = 1000;
            //int num_rows = 12;
            //float timeout = float(1);
            //
            //frontier:
            //-296 81
            //-293 65
            //-278 59
            //-275 57
            //-274 50
            //-273 45
            //-271 37
            //-270 34
            //-245 33
            //-224 19

            //int num_trials = 2;
            //int num_rows = 120;
            //float timeout = float(100);
            //frontier:
            //-284 91
            //-282 85
            //-273 80
            //-269 78
            //-268 71
            //-249 69
            //-235 55
            //-204 38

            //int num_trials = 10;
            //int num_rows = 120;
            //float timeout = float(5);
            //frontier:
            //-303 90
            //-294 85
            //-284 81
            //-269 78
            //-268 71
            //-260 39
            //-218 33


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


#endif //SKETCH_HYPERSKETCHPROGRAMS_H
