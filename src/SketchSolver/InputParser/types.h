#pragma once

#include <vector>
#include <iostream>
#include <cassert>

using namespace std;

namespace VectorizedInterpreter {

    struct Buffer {
        void *ptr = nullptr;
        size_t size = 0;
    };

    enum ToStringType {
        to_broadcasting_in_julia,
        to_vanilla_julia,
    };

    class Op {
    public:
        virtual void run(const vector<Buffer> &inputs,
                         const vector<Buffer> &outputs) = 0;


        template<typename T>
        void assert_num_inputs_and_outputs(const vector<T> &inputs,
                                                  const vector<T> &outputs) {
            assert_num_inputs(inputs);
            assert_num_outputs(outputs);
        }

        template<typename T>
        void assert_vectorized_input_and_single_output_format(const vector<T> &inputs,
                                                                     const vector<T> &outputs);

        template<typename T>
        void assert_vectorized_inputs_format(const vector<T> &input_sizes);

        template<typename T>
        void assert_num_inputs(const vector<T> &output_sizes) {
            assert(output_sizes.size() == num_inputs());
        }

        template<typename T>
        void assert_num_outputs(const vector<T> &output_sizes) {
            assert(output_sizes.size() == num_outputs());
        }

        virtual size_t num_inputs() = 0;

        virtual size_t num_outputs() = 0;

        virtual vector<size_t> compute_output_sizes(const vector<size_t> &input_sizes) = 0;

        virtual string to_string(const vector<string> &input_ids, const vector<string> &output_ids) {
            return to_string(input_ids, output_ids, to_broadcasting_in_julia);
        }

        virtual string
        to_string(const vector<string> &input_ids, const vector<string> &output_ids, ToStringType to_string_type) {
            assert(false);
            return "";
        }
    };


    struct Operation {
        Op *op;
        vector<int> input_ids;
        vector<int> output_ids;

        string to_string() const {
            assert(output_ids.size() == 1);
            vector<int> input_ids_plus_1 = input_ids;
            //julia indexes start at 1.
            for (int i = 0; i < input_ids_plus_1.size(); i++) {
                input_ids_plus_1[i]++;
            }
            vector<int> output_ids_plus_1 = output_ids;
            //julia indexes start at 1.
            for (int i = 0; i < output_ids_plus_1.size(); i++) {
                output_ids_plus_1[i]++;
            }

            vector<string> input_strs;
            for (auto input: input_ids_plus_1) {
                input_strs.push_back(std::to_string(input));
            }

            vector<string> output_strs;
            for (auto output: output_ids_plus_1) {
                output_strs.push_back(std::to_string(output));
            }

            return op->to_string(input_strs, output_strs);
        }
    };

    struct Graph {
        vector<Operation> operations;
        vector<int> input_ids;
        vector<int> input_idxs;
        vector<int> output_ids;
    };
}
