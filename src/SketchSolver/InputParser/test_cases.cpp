//
// Created by kliment on 7/3/22.
//

#include <cstring>
#include <iostream>
#include <string>
#include <cassert>
#include <chrono>
#include <map>
#include <utility>
#include "FileForVectorizedInterpreter.h"
#include "test_cases.h"
#include "BenchmarkScore.h"

using namespace std;

namespace VectorizedInterpreter {

    Op *LessThanIntOp::instance = new LessThanIntOp();
    Op *EqIntOp::instance = new EqIntOp();
    Op *NotIntOp::instance = new NotIntOp();
    Op *AndIntOp::instance = new AndIntOp();
    Op *OrIntOp::instance = new OrIntOp();
    Op *MultIntOp::instance = new MultIntOp();
    Op *PlusIntOp::instance = new PlusIntOp();
    Op *XorIntOp::instance = new XorIntOp();
    Op *IfIntOp::instance = new IfIntOp();
    Op *BoolArrayReadOp::instance = nullptr;
    Op *BoolArrayWriteOp::instance = nullptr;
    Op *BoolArrayWriteOpFromConst::instance = nullptr;
    vector<BoolArrayCreate *> BoolArrayCreate::instances = vector<BoolArrayCreate *>();

    string read_dag_from_file(const string &file_path) {
        ifstream input_file(file_path);
        assert(input_file.is_open());
        string ret;
        string line;
        while (getline(input_file, line)) {
            ret += line + "\n";
        }
        input_file.close();
        return ret;
    }

    BoolArrayCreate *create_boolean_array_create(int n, vector<vector<IntArrVar*> >& meta_meta_array)
    {
        const int small_constant = 10;

        if (n <= small_constant) {
            while (BoolArrayCreate::instances.size() <= n) {
                BoolArrayCreate::instances.push_back(nullptr);
            }
            BoolArrayCreate::instances[n] = new BoolArrayCreate(n, meta_meta_array);
        } else {
            assert(false);
        }

        assert(n < BoolArrayCreate::instances.size());
        assert(BoolArrayCreate::instances[n] != nullptr);

        return BoolArrayCreate::instances[n];
    }

    vector<BaseType> calc_score(const size_t m, const vector<Buffer> &out_buffers, int &rez) {
//    typedef uint64_t WORD_TYPE;
//    static const int word_size_bits = 6;
//    static_assert(sizeof(WORD_TYPE)*8 == 1<<word_size_bits, "WORD_TYPE and word_size_bits are inconsistent.");
//    static const int word_size = 1<<word_size_bits;
//    vector<WORD_TYPE> acc((m+word_size-1) >> word_size_bits, -1);

        vector<BaseType> acc(m, 1);

        const size_t out_buffer_size = out_buffers.size();

//    int _sum = 0;
//    for(int i = 0;i<m;i++)
//    {
//        bool acc = true;
//        for(int k = 0; k < out_buffer_size; k++)
//        {
//            acc &= ((BaseType *) out_buffers[k].ptr)[i];
//            cout << ((BaseType *) out_buffers[k].ptr)[i] <<" ";
//        }
//        _sum += acc;
//        cout << " = " << acc << " s " << _sum << endl;
//    }

        for (int k = 0; k < out_buffer_size; k++) {
            const BaseType *buffer = ((BaseType *) out_buffers[k].ptr);
            for (int j = 0; j < m; j++) {
                BaseType val = buffer[j];
                assert(acc[j] == 0 || acc[j] == 1);
                assert(val == 0 || val == 1);
                acc[j] &= val;
            }
        }
        int sum = 0;
        for (int j = 0; j < m; j++) {
            assert(acc[j] == 0 || acc[j] == 1);
            sum += acc[j];
//        cout << j <<" "<< acc[j] << " " << sum << endl;
        }
//    assert(sum == _sum);
        if (rez == -1) {
            rez = sum;
        } else {
            assert(rez == sum);
        }
        return acc;
    }

    void benchmark_vectorized_interpreter(const string &dag_as_string, const FileForVectorizedInterpreter &file,
                                          int exec_trials, bool print_score) {

        auto local_benchmarking_init = chrono::steady_clock::now();

        const size_t m = file.size();
        ParserAndCompiler parser_and_compiler = ParserAndCompiler(dag_as_string, m);
        auto executor = parser_and_compiler.parse_and_compile();

        //----- DONE WITH COMMON PIPELINE -----

        auto local_start_chrono = chrono::steady_clock::now();

        vector<Buffer> input_buffers = parser_and_compiler.get_input_buffers(file);
        auto timestamp_get_input_buffers = timestamp(local_start_chrono, "get_input_buffers");

        vector<Buffer> out_buffers = parser_and_compiler.get_output_buffers();
        auto timestep_get_output_buffers = timestamp(timestamp_get_input_buffers, "get_output_buffers");

        executor->run(input_buffers, out_buffers);

        auto timestamp_exec = timestamp(local_benchmarking_init, "parse+compile+exec");

        int rez = -1;

        for (int i = 0; i < exec_trials; i++) {

            auto timestamp_start_exec = chrono::steady_clock::now();

            executor->run(input_buffers, out_buffers);

            auto timestamp_exec = timestamp(timestamp_start_exec, "exec");

            calc_score(m, out_buffers, rez);

            auto timestamp_calc_score = timestamp(timestamp_exec, "calc_score");
        }

        auto timestep_benchmark = timestamp(timestep_get_output_buffers, std::to_string(exec_trials) + "_execs");
        if (print_score) {
            cout << "score: " << rez << " / " << file.size() << " (" << (float) rez / (float) m * 100 << "%)" << endl;
        }
    }

};