//
// Created by Kliment Serafimov on 7/27/22.
//

#include "vectorized_interpreter_main.h"

#include <utility>
#include "test_cases.h"

namespace VectorizedInterpreter
{
    vector<BaseType> main(string dag_string, const VectorizedInterpreter::FileForVectorizedInterpreter &file_from_vectorized_intepreter)
    {
        auto local_benchmarking_init = chrono::steady_clock::now();

        const size_t m = file_from_vectorized_intepreter.size();
        VectorizedInterpreter::ParserAndCompiler parser_and_compiler = VectorizedInterpreter::ParserAndCompiler(std::move(dag_string), m);
        auto executor = parser_and_compiler.parse_and_compile();

        //----- DONE WITH COMMON PIPELINE -----

        auto local_start_chrono = chrono::steady_clock::now();

        vector<VectorizedInterpreter::Buffer> input_buffers = parser_and_compiler.get_input_buffers(file_from_vectorized_intepreter);
        auto timestamp_get_input_buffers = timestamp(local_start_chrono, "get_input_buffers_vecinterp");

        vector<VectorizedInterpreter::Buffer> out_buffers = parser_and_compiler.get_output_buffers();
        auto timestep_get_output_buffers = timestamp(timestamp_get_input_buffers, "get_output_buffers_vecinterp");

        executor->run(input_buffers, out_buffers);

        int rez = -1;
        int exec_trials = 1;

        auto timestamp_start_exec = timestamp(local_benchmarking_init, "parse+compile+exec_vecinterp");

        executor->run(input_buffers, out_buffers);

        auto start_calc_score = timestamp(timestamp_start_exec, "exec_vecinterp");

        auto ret = VectorizedInterpreter::calc_score(m, out_buffers, rez);

        auto timestamp_calc_score = timestamp(start_calc_score, "calc_score_vecinterp");

//        auto timestep_benchmark = timestamp(timestep_get_output_buffers, std::to_string(exec_trials)+"_execs");

        bool print_score = true;
        if(print_score) {
            cout << "score: " << rez << " / " << file_from_vectorized_intepreter.size() <<" (" << (float)rez/(float)m*100 << "%)" << endl;
        }

        return ret;
    }
}