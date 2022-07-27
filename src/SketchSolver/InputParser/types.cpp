//
// Created by kliment on 7/2/22.
//

#include "types.h"

namespace VectorizedInterpreter {

    template<>
    void Op::assert_vectorized_inputs_format<Buffer>(const vector<Buffer> &input_sizes);

    template<>
    void Op::assert_vectorized_input_and_single_output_format<Buffer>(
            const vector<Buffer> &inputs, const vector<Buffer> &outputs);

    template<>
    void Op::assert_vectorized_inputs_format<size_t>(const vector<size_t> &input_sizes);

    template<>
    void Op::assert_vectorized_inputs_format<Buffer>(const vector<Buffer> &input_sizes) {
        assert_num_inputs(input_sizes);
        for (int i = 1; i < input_sizes.size(); i++) {
            assert(input_sizes[i].size == input_sizes[0].size);
        }
    }

    template<>
    void Op::assert_vectorized_input_and_single_output_format<Buffer>(
            const vector<Buffer> &inputs, const vector<Buffer> &outputs) {
        assert_vectorized_inputs_format(inputs);
        assert_num_outputs(outputs);
        assert(outputs.size() == 1);
        if (!inputs.empty()) {
            assert(outputs[0].size == inputs[0].size);
        }
    }

    template<>
    void Op::assert_vectorized_inputs_format<size_t>(const vector<size_t> &input_sizes) {
        assert_num_inputs(input_sizes);
        for (int i = 1; i < input_sizes.size(); i++) {
            assert(input_sizes[i] == input_sizes[0]);
        }
    }
};