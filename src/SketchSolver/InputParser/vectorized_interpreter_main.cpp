//
// Created by Kliment Serafimov on 7/27/22.
//

#include "vectorized_interpreter_main.h"

#include <utility>
#include "test_cases.h"

namespace VectorizedInterpreter
{
    vector<BaseType> main(
            string dag_string,
            const VectorizedInterpreter::FileForVecInterp &file_for_vecinterp)
    {
        auto local_benchmarking_init = chrono::steady_clock::now();

        const size_t m = file_for_vecinterp.size();
        VectorizedInterpreter::ParserAndCompiler parser_and_compiler =
                VectorizedInterpreter::ParserAndCompiler(std::move(dag_string), m);
        auto executor = parser_and_compiler.parse_and_compile();

        //----- DONE WITH COMMON PIPELINE -----

        auto local_start_chrono = chrono::steady_clock::now();

        vector<VectorizedInterpreter::Buffer> input_buffers = parser_and_compiler.get_input_buffers(file_for_vecinterp);
        auto timestamp_get_input_buffers = timestamp(local_start_chrono, "get_input_buffers[vecinterp]");

        vector<VectorizedInterpreter::Buffer> out_buffers = parser_and_compiler.get_output_buffers();
        auto timestep_get_output_buffers = timestamp(timestamp_get_input_buffers, "get_output_buffers[vecinterp]");

        executor->run(input_buffers, out_buffers);

        int rez = -1;
        int exec_trials = 1;

        auto timestamp_start_exec = timestamp(local_benchmarking_init, "parse+compile+exec[vecinterp]");

        executor->run(input_buffers, out_buffers);

        auto start_calc_score = timestamp(timestamp_start_exec, "exec[vecinterp_main]");

        auto ret = VectorizedInterpreter::calc_score(m, out_buffers, rez);

        auto timestamp_calc_score = timestamp(start_calc_score, "calc_score[vecinterp]");

        bool print_score = false;
        if(print_score) {
            cout << "score: " << rez << " / " << file_for_vecinterp.size() <<" (" << (float)rez/(float)m*100 << "%)" << endl;
        }

        return ret;
    }


    int main_get_passing_input_idx(
            string dag_string,
            const VectorizedInterpreter::FileForVecInterp &_file_for_vecinterp)
    {
        auto local_benchmarking_init = chrono::steady_clock::now();

        const int _num_stages = 1;
        const int stage_size = (_file_for_vecinterp.size()+_num_stages-1)/_num_stages;

        const size_t num_stages = (_file_for_vecinterp.size() + stage_size - 1) / stage_size;
        assert(_num_stages == num_stages);

        const size_t m = stage_size;

        VectorizedInterpreter::ParserAndCompiler parser_and_compiler =
                VectorizedInterpreter::ParserAndCompiler(std::move(dag_string), m);
        auto executor = parser_and_compiler.parse_and_compile();

        //----- DONE WITH COMMON PIPELINE -----

        vector<const VectorizedInterpreter::FileForVecInterp*> slices = _file_for_vecinterp.get_slices(stage_size);

        int add_to_idx = 0;

        for(int slice_id = 0; slice_id < slices.size();slice_id++) {

            auto local_start_chrono = chrono::steady_clock::now();

            vector<VectorizedInterpreter::Buffer> input_buffers =
                    parser_and_compiler.get_input_buffers(*slices[slice_id]);
            auto timestamp_get_input_buffers = timestamp(local_start_chrono, "get_input_buffers[vecinterp_main_get_id]");

            vector<VectorizedInterpreter::Buffer> out_buffers = parser_and_compiler.get_output_buffers();
            auto timestamp_start_exec = timestamp(timestamp_get_input_buffers, "get_output_buffers[vecinterp_main_get_id]");

            executor->run(input_buffers, out_buffers);

            auto start_calc_score = timestamp(timestamp_start_exec, "exec[vecinterp_main_get_id]");

            int rez = -1;

            auto batch_output = VectorizedInterpreter::calc_score(m, out_buffers, rez);

            auto timestamp_calc_score = timestamp(start_calc_score, "calc_score[vecinterp_main_get_id]");

            for (int i = 0; i < batch_output.size(); i++) {
                if (batch_output[i]) {
                    return i+add_to_idx;
                }
            }
            add_to_idx += slices[slice_id]->size();
        }

        return -1;
    }
}