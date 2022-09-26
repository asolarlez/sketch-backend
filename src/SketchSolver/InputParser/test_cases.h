//
// Created by kliment on 7/3/22.
//

#ifndef EXAFUNCTIONGRAPHEXECUTOR_TEST_CASES_H
#define EXAFUNCTIONGRAPHEXECUTOR_TEST_CASES_H

#include <string>
#include <chrono>
#include <map>
#include "BenchmarkScore.h"
#include "types.h"
#include "DagLikeProgramInterpreter.h"

namespace VectorizedInterpreter {

    class FileForVecInterp;

    vector<BaseType> calc_score(const size_t m, const vector<Buffer> &out_buffers, int &rez);

    class BinaryOp : public Op {
        size_t num_inputs() override { return 2; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }
    };

    class ConstIntOp : public Op {
        const BaseType the_const;
        const size_t m;
    public:
        explicit ConstIntOp(BaseType _the_const, size_t _m) : the_const(_the_const),
                                                              m(_m) {} // TODO: remove m bc it can be inferred.
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);
            const size_t predicted_m = outputs[0].size / sizeof(BaseType);
            assert(m == predicted_m);
            for (size_t i = 0; i < m; i++) {
                out_ptr[i] = the_const;
            }
        }

        size_t num_inputs() override { return 0; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;

            output_sizes.push_back(m * sizeof(BaseType));
            assert_num_outputs(output_sizes);
            return output_sizes;
        }


        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            switch (to_string_type) {
                case to_broadcasting_in_julia: {
                    assert_num_inputs_and_outputs(input_ids, output_ids);
                    return "buffers[" + output_ids[0] + "] .= fill(" + std::to_string(the_const) + ", m)";
                    break;
                }
                case to_vanilla_julia: {
                    assert(false);
                    break;
                }
                default:
                    assert(false);
            }
            assert(false);
            return "";
        }
    };

    class LessThanIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                out_ptr[i] = in0_ptr[i] < in1_ptr[i];
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .< buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class EqIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                out_ptr[i] = in0_ptr[i] == in1_ptr[i];
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .== buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class NotIntOp : public Op {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                assert(is_bit(in0_ptr[i]));
                out_ptr[i] = 1 - in0_ptr[i];
                assert(is_bit(out_ptr[i]));
            }
        }

        size_t num_inputs() override { return 1; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "iszero.(buffers[" + input_ids[0] + "])";
        }

        static Op *instance;
    };

    class AndIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                assert(is_bit(in0_ptr[i]));
                assert(is_bit(in1_ptr[i]));
                out_ptr[i] = in0_ptr[i] & in1_ptr[i];
                assert(is_bit(out_ptr[i]));
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .& buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class OrIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                out_ptr[i] = (in0_ptr[i]) | (in1_ptr[i]);
                assert(is_bit(out_ptr[i]));
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .| buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class MultIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                out_ptr[i] = (in0_ptr[i]) * (in1_ptr[i]);
                assert(is_bit(out_ptr[i]));
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .* buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class PlusIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                out_ptr[i] = (in0_ptr[i]) + (in1_ptr[i]);
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "buffers[" + input_ids[0] + "] .+ buffers[" + input_ids[1] +
                   "]";
        }

        static Op *instance;
    };


    class XorIntOp : public BinaryOp {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                assert(is_bit(in0_ptr[i]));
                assert(is_bit(in1_ptr[i]));
                out_ptr[i] = (in0_ptr[i]) ^ (in1_ptr[i]);
                assert(is_bit(out_ptr[i]));
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " + "xor.(buffers[" + input_ids[0] + "], buffers[" +
                   input_ids[1] + "])";
        }

        static Op *instance;
    };


    class IfIntOp : public Op {
    public:
        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {
            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *true_branch_ptr = static_cast<BaseType *>(inputs[2].ptr);
            auto *false_branch_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                assert(is_bit(in0_ptr[i]));
                assert(is_bit(true_branch_ptr[i]));
                assert(is_bit(false_branch_ptr[i]));
                out_ptr[i] = (in0_ptr[i] * true_branch_ptr[i]) + ((1 - in0_ptr[i]) * false_branch_ptr[i]);
                assert(is_bit(out_ptr[i]));
            }
        }

        size_t num_inputs() override { return 3; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= " +
                   "(buffers[" + input_ids[0] + "] .* " + "buffers[" + input_ids[2] + "]) .+ " +
                   "((1 .- buffers[" + input_ids[0] + "]) .* " + "buffers[" + input_ids[1] + "])";
        }

        static Op *instance;
    };


    class BoolArrayReadOp : public BinaryOp {
        const vector<vector<IntArrVar *> > &meta_meta_array;
    public:
        explicit BoolArrayReadOp(const vector<vector<IntArrVar *> > &_meta_meta_array) : meta_meta_array(
                _meta_meta_array) {}

        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {

            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *in0_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *in1_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *out_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            for (size_t i = 0; i < local_n; i++) {
                assert(in1_ptr[i] < meta_meta_array.size());
                int idx = in0_ptr[i];
                int meta_arr_idx = in1_ptr[i];
                const vector<IntArrVar *> &meta_array = meta_meta_array[in1_ptr[i]];
                out_ptr[i] = meta_array[i]->get(idx);
                assert(is_bit(out_ptr[i]));
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "buffers[" + output_ids[0] + "] .= arr_r.(1:m, buffers[" + input_ids[0] + "])";
        }

        static Op *instance;
    };

    class BoolArrayWriteOp : public Op {
        vector<vector<IntArrVar *> > &meta_meta_array;
    public:
        explicit BoolArrayWriteOp(vector<vector<IntArrVar *> > &_meta_meta_array) : meta_meta_array(_meta_meta_array) {}

        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {

            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *idxs_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *input_arr_idx_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *new_vals_ptr = static_cast<BaseType *>(inputs[2].ptr);
            auto *output_buff_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            meta_meta_array.push_back(vector<IntArrVar *>(meta_meta_array[meta_meta_array.size() - 1].size(), nullptr));
            int ret_meta_arr_id = meta_meta_array.size() - 1;

            for (size_t i = 0; i < local_n; i++) {
                assert(idxs_ptr[i] < meta_meta_array[ret_meta_arr_id].size());
                int idx = idxs_ptr[i];
                int input_arr_idx = input_arr_idx_ptr[i];
                int val = new_vals_ptr[i];
                assert(is_bit(val));
                assert(input_arr_idx < meta_meta_array.size() - 1);
                assert(i < meta_meta_array[input_arr_idx].size());
                meta_meta_array[ret_meta_arr_id][i] = new IntArrVar(meta_meta_array[input_arr_idx][i]);
                meta_meta_array[ret_meta_arr_id][i]->set(idx, val);
                output_buff_ptr[i] = meta_meta_array.size() - 1;
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "TODO :: BoolArrayWriteOp";
        }

        size_t num_inputs() override { return 3; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }

        static Op *instance;
    };



    class BoolArrayWriteOpFromConst : public Op {
        vector<vector<IntArrVar *> > &meta_meta_array;
    public:

        explicit BoolArrayWriteOpFromConst(vector<vector<IntArrVar *> > &_meta_meta_array) : meta_meta_array(
                _meta_meta_array) {}

        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {

            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *idxs_ptr = static_cast<BaseType *>(inputs[0].ptr);
            auto *default_vals_ptr = static_cast<BaseType *>(inputs[1].ptr);
            auto *new_vals_ptr = static_cast<BaseType *>(inputs[2].ptr);
            auto *output_buff_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_n = inputs[0].size / sizeof(BaseType);

            meta_meta_array.push_back(vector<IntArrVar *>());
            vector<IntArrVar *> &meta_array = meta_meta_array[meta_meta_array.size() - 1];

            for (size_t i = 0; i < local_n; i++) {
                int idx = idxs_ptr[i];
                int default_val = default_vals_ptr[i];
                assert(is_bit(default_val));
                int val = new_vals_ptr[i];
                assert(is_bit(val));
                meta_array.push_back(new IntArrVar(0, default_val));
                meta_array[i]->set(idx, val);
                output_buff_ptr[i] = meta_meta_array.size() - 1;
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "TODO :: BoolArrayWriteOp";
        }

        size_t num_inputs() override { return 3; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }

        static Op *instance;
    };


    class BoolArrayCreate : public Op {
        vector<vector<IntArrVar *> >& meta_meta_array;
        int size = 0;
    public:
        explicit BoolArrayCreate(int _size, vector<vector<IntArrVar *> > &_meta_meta_array) :
        size(_size) , meta_meta_array(_meta_meta_array) {}

        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override {

            assert_vectorized_input_and_single_output_format(inputs, outputs);

            auto *output_buff_ptr = static_cast<BaseType *>(outputs[0].ptr);

            const size_t local_m = inputs[0].size / sizeof(BaseType);

            meta_meta_array.push_back(vector<IntArrVar *>(local_m, new IntArrVar(vector<BaseType>(size))));
            vector<IntArrVar *> &meta_array = meta_meta_array[meta_meta_array.size() - 1];

            for (size_t i = 0; i < local_m; i++) {
                for (int j = 0; j < size; j++) {
                    meta_array[i]->hard_set(j, ((BaseType *) inputs[j].ptr)[i]);
                }
                output_buff_ptr[i] = meta_meta_array.size() - 1;
            }
        }

        string to_string(const vector<string> &input_ids, const vector<string> &output_ids,
                         ToStringType to_string_type) override {
            assert_num_inputs_and_outputs(input_ids, output_ids);
            return "TODO :: BoolArrayWriteOp";
        }

        size_t num_inputs() override { return size; }

        size_t num_outputs() override { return 1; }

        std::vector<size_t> compute_output_sizes(
                const std::vector<size_t> &input_sizes) override {
            assert_vectorized_inputs_format(input_sizes);
            vector<size_t> output_sizes;
            output_sizes.push_back(input_sizes[0]);
            assert_num_outputs(output_sizes);
            return input_sizes;
        }

        static vector<BoolArrayCreate *> instances;
    };



    BoolArrayCreate *create_boolean_array_create(int n, vector<vector<IntArrVar*> >& meta_meta_var);

    using namespace std;

    class Parser {

        enum OpType {
            _const,
            _lt,
            _not,
            _assert,
            _src,
            _arr_r,
            _and,
            _or,
            _arracc,
            _xor,
            _mult,
            _plus,
            _eq,
            _noop,
            _ctrl,
            _arr_w,
            _arr_create
        };

        enum OutType {
            _int, _bool, _bool_arr, _undefined
        };

        static OpType op_str_to_op_type(const string &key) {
            assert(!key.empty());
            if (key[0] <= 'E') {
                if (key == "AND") {
                    return _and;
                } else if (key == "CONST") {
                    return _const;
                } else if (key == "ASSERT") {
                    return _assert;
                } else if (key == "ARR_R") {
                    return _arr_r;
                } else if (key == "ARRACC") {
                    return _arracc;
                } else if (key == "EQ") {
                    return _eq;
                } else if (key == "CTRL") {
                    return _ctrl;
                } else if (key == "ARR_W") {
                    return _arr_w;
                } else if (key == "ARR_CREATE") {
                    return _arr_create;
                } else {
                    assert(false);
                }
            } else {
                if (key == "LT") {
                    return _lt;
                } else if (key == "OR") {
                    return _or;
                } else if (key == "NOT") {
                    return _not;
                } else if (key == "S") {
                    return _src;
                } else if (key == "XOR") {
                    return _xor;
                } else if (key == "TIMES") {
                    return _mult;
                } else if (key == "PLUS") {
                    return _plus;
                } else {
                    assert(false);
                }
            }
            assert(false);
            return _noop;
        }

        static OutType out_type_str_to_out_type(const string &key) {
            if (key == "INT") {
                return _int;
            } else if (key == "BOOL") {
                return _bool;
            } else if (key == "BOOL_ARR") {
                return _bool_arr;
            } else {
                assert(false);
            }
        }

        class InputNodeSpec {
            string name;
            int nbits = 0;
            int input_id = -1;
        public:

            InputNodeSpec() = default;

            InputNodeSpec(string _name, int _nbits, int _input_id) : name(std::move(_name)), nbits(_nbits),
                                                                     input_id(_input_id) {}

            bool operator<(const InputNodeSpec &other) const {
                assert(name != other.name);
                return name < other.name;
            }
        };

        template<typename KeyType, typename ValType>
        class MyMap : private map<KeyType, ValType> {
        public:
            MyMap() = default;

            void set(KeyType key, ValType val) {
                assert((map<KeyType, ValType>::find(key) == map<KeyType, ValType>::end()));
                map<KeyType, ValType>::operator[](key) = val;
            }

            ValType get(KeyType key) {
                assert((map<KeyType, ValType>::find(key) != map<KeyType, ValType>::end()));
                return map<KeyType, ValType>::at(key);
            }
        };

        class OutTypeArrayMap : public MapTrait<OutType> {
            vector<OutType> my_map;
        public:

            OutType operator[](int idx) override {
                assert(contains(idx));
                return my_map[idx];
            }

            OutType get(int idx) {
                return this->operator[](idx);
            }

            void set(int idx, OutType val) override {
                while (idx >= my_map.size()) {
                    my_map.push_back(_undefined);
                }
                assert(val != _undefined);
                assert(idx < my_map.size());
                my_map[idx] = val;
            }

            bool contains(int idx) override {
                return my_map[idx] != _undefined;
            }
        };


        static void parse_tokenized_line(
                const vector<string> &tokens,
                Graph &graph,
                OutTypeArrayMap &id_to_out_type,
                MyMap<string, InputNodeSpec> &src_name_to_input_node_spec,
                const size_t m,
                vector<vector<IntArrVar*> >& meta_meta_array) {
            int out_id = stoi(tokens[0]);
            assert(tokens[1] == "=");
            string op_str = tokens[2];

            OpType op = op_str_to_op_type(op_str);

            if (op == _assert) {

                vector<string> params;
                for (int j = 3; j < tokens.size(); j++) {
                    params.push_back(tokens[j]);
                }

                assert(params.size() == 2);
                int op_id = stoi(params[0]);
                assert(id_to_out_type.get(op_id) == _bool);
                graph.output_ids.push_back(op_id);

            } else {
                string type_str;
                type_str = tokens[3];

                OutType out_type = out_type_str_to_out_type(type_str);

                vector<string> params;
                for (int j = 4; j < tokens.size(); j++) {
                    params.push_back(tokens[j]);
                }

                id_to_out_type.set(out_id, out_type);

                switch (op) {
                    case _const: {
                        assert(out_type == _int || out_type == _bool);
                        assert(params.size() == 1);
                        BaseType the_const = stoi(params[0]);
                        graph.operations.push_back(
                                {.op = new ConstIntOp(the_const, m),
                                        .input_ids = {},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _lt: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _int || id_to_out_type.get(left_op_id) == _bool);
                        assert(id_to_out_type.get(right_op_id) == _int);
                        graph.operations.push_back(
                                {.op = LessThanIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _not: {
                        assert(out_type == _bool);
                        assert(params.size() == 1);
                        int op_id = stoi(params[0]);
                        assert(id_to_out_type.get(op_id) == _bool);
                        graph.operations.push_back(
                                {.op = NotIntOp::instance,
                                        .input_ids = {op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _assert: {
                        //shouldn't enter here
                        assert(false);
                        break;
                    }
                    case _src: {
                        assert(params.size() == 4);
                        assert(params[2] == "|");
                        int input_id = stoi(params[3]);
                        string src_name = params[0];
                        int nbits = stoi(params[1]);
                        src_name_to_input_node_spec.set(
                                src_name, InputNodeSpec(src_name, nbits, input_id));
                        graph.input_ids.push_back(out_id);
                        graph.input_idxs.push_back(input_id);
                        break;
                    }
                    case _ctrl: {
                        assert(params.size() == 4);
                        assert(params[2] == "|");
                        int input_id = stoi(params[3]);
                        string ctrl_name = params[0];
                        int nbits = stoi(params[1]);
//                    cout<< ctrl_name <<" "<< input_id << endl;
                        src_name_to_input_node_spec.set(
                                ctrl_name, InputNodeSpec(ctrl_name, nbits, input_id));
                        graph.input_ids.push_back(out_id);
                        graph.input_idxs.push_back(input_id);
                        break;
                    }
                    case _arr_r: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int idx_id = stoi(params[0]);
                        int arr_id = stoi(params[1]);
                        assert(id_to_out_type.get(idx_id) == _int || id_to_out_type.get(idx_id) == _bool);
                        assert(id_to_out_type.get(arr_id) == _bool_arr);
                        graph.operations.push_back(
                                {.op = BoolArrayReadOp::instance,
                                        .input_ids = {idx_id, arr_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _arr_w: {
                        assert(out_type == _bool_arr);
                        assert(params.size() == 3);
                        int idx_id = stoi(params[0]);
                        int arr_id = stoi(params[1]);
                        int new_val = stoi(params[2]);
                        assert(id_to_out_type.get(idx_id) == _int || id_to_out_type.get(idx_id) == _bool);
                        if (id_to_out_type.get(arr_id) == _bool_arr) {
                            graph.operations.push_back(
                                    {.op = BoolArrayWriteOp::instance,
                                            .input_ids = {idx_id, arr_id, new_val},
                                            .output_ids = {out_id}});
                        } else {
                            assert(id_to_out_type.get(arr_id) == _bool);
                            graph.operations.push_back(
                                    {.op = BoolArrayWriteOpFromConst::instance,
                                            .input_ids = {idx_id, arr_id, new_val},
                                            .output_ids = {out_id}});
                        }
                        break;
                    }
                    case _arr_create: {
                        assert(out_type == _bool_arr);
                        assert(params.size() >= 2);
                        int n = stoi(params[0]);
                        assert(params.size() == n + 2);
                        vector<int> param_ids;
                        for (int local_id = 1; local_id < n + 1; local_id++) {
                            param_ids.push_back(stoi(params[local_id]));
                            assert(id_to_out_type.get(param_ids[param_ids.size() - 1]) == _bool);
                        }
                        assert(params[n + 1] == "(0)");
                        graph.operations.push_back(
                                {.op = create_boolean_array_create(n, meta_meta_array),
                                        .input_ids = param_ids,
                                        .output_ids = {out_id}});

                        break;
                    }
                    case _and: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _bool);
                        assert(id_to_out_type.get(right_op_id) == _bool);
                        graph.operations.push_back(
                                {.op = AndIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _or: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _bool);
                        assert(id_to_out_type.get(right_op_id) == _bool);
                        graph.operations.push_back(
                                {.op = OrIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _arracc: {
                        switch (out_type) {
                            case _bool: {
                                assert(params.size() == 4);
                                int idx_op = stoi(params[0]);
                                int arr_size = stoi(params[1]);
                                assert(arr_size == 2);
                                int at0_id = stoi(params[2]);
                                int at1_id = stoi(params[3]);

                                assert(id_to_out_type.get(idx_op) == _bool);
                                assert(id_to_out_type.get(at0_id) == _bool);
                                assert(id_to_out_type.get(at1_id) == _bool);
                                graph.operations.push_back(
                                        {.op = IfIntOp::instance,
                                                .input_ids = {idx_op, at0_id, at1_id},
                                                .output_ids = {out_id}});
                                break;
                            }
                            case _int: {
                                assert(params.size() == 4);
                                int idx_op = stoi(params[0]);
                                int arr_size = stoi(params[1]);
                                assert(arr_size == 2);
                                int at0_id = stoi(params[2]);
                                int at1_id = stoi(params[3]);

                                assert(id_to_out_type.get(idx_op) == _bool);
                                assert(id_to_out_type.get(at0_id) == _int);
                                assert(id_to_out_type.get(at1_id) == _int);
                                graph.operations.push_back(
                                        {.op = IfIntOp::instance,
                                                .input_ids = {idx_op, at0_id, at1_id},
                                                .output_ids = {out_id}});
                                break;
                            }
                            default: {
                                assert(false);
                            }
                        }
                        break;
                    }
                    case _xor: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _bool);
                        assert(id_to_out_type.get(right_op_id) == _bool);
                        graph.operations.push_back(
                                {.op = XorIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _mult: {
                        assert(out_type == _int);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _int);
                        assert(id_to_out_type.get(right_op_id) == _int);
                        graph.operations.push_back(
                                {.op = MultIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _plus: {
                        assert(out_type == _int);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _int);
                        assert(id_to_out_type.get(right_op_id) == _int);
                        graph.operations.push_back(
                                {.op = PlusIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    case _eq: {
                        assert(out_type == _bool);
                        assert(params.size() == 2);
                        int left_op_id = stoi(params[0]);
                        int right_op_id = stoi(params[1]);
                        assert(id_to_out_type.get(left_op_id) == _int);
                        assert(id_to_out_type.get(right_op_id) == _int);
                        graph.operations.push_back(
                                {.op = EqIntOp::instance,
                                        .input_ids = {left_op_id, right_op_id},
                                        .output_ids = {out_id}});
                        break;
                    }
                    default: {
                        assert(false);
                    }
                }
            }
        }

    public:

        static Graph parse(const string &dag_as_string, const size_t m, vector<vector<IntArrVar*> >& meta_meta_array) {
            Graph graph{
                    .operations ={},
                    .input_ids = {},
                    .output_ids = {},
            };

            OutTypeArrayMap id_to_out_type;
            MyMap<string, InputNodeSpec> src_name_to_input_node_spec;

            vector<string> tokenized_line;
            string running_token;
            bool in_quote = false;
            for (char c: dag_as_string) {
                if (c == '\n') {
                    assert(!in_quote);
                    if (!running_token.empty()) {
                        tokenized_line.push_back(running_token);
                        running_token.clear();
                    }
                    if (!tokenized_line.empty()) {
                        parse_tokenized_line(tokenized_line, graph, id_to_out_type, src_name_to_input_node_spec, m, meta_meta_array);
                        tokenized_line.clear();
                    }
                } else if (c == '"') {
                    if (in_quote) {
                        assert(!running_token.empty());
                        running_token += c;
                        tokenized_line.push_back(running_token);
                        running_token.clear();
                    } else {
                        assert(running_token.empty());
                        running_token += c;
                    }
                    in_quote = !in_quote;
                } else if (!in_quote) {
                    if (c == ' ') {
                        assert(!running_token.empty());
                        tokenized_line.push_back(running_token);
                        running_token.clear();
                    } else {
                        running_token += c;
                    }
                } else {
                    running_token += c;
                }
            }

            return graph;
        }
    };

    class ParserAndCompiler {
        bool parsed = false;
        bool parsed_and_compiled = false;
        string dag_as_string;
        BaseType *meta_input = nullptr;
        BaseType *meta_output = nullptr;
        vector<size_t> input_sizes;
        vector<size_t> output_sizes;
        size_t m;
        size_t n = 0;
        Graph graph;
    public:
        vector<vector<IntArrVar *> > meta_meta_array;

        ParserAndCompiler(string dag_as_string, size_t _m) : m(_m), dag_as_string(std::move(dag_as_string)) {
            BoolArrayReadOp::instance = new BoolArrayReadOp(meta_meta_array);
            BoolArrayWriteOp::instance = new BoolArrayWriteOp(meta_meta_array);
            BoolArrayWriteOpFromConst::instance = new BoolArrayWriteOpFromConst(meta_meta_array);
        }

        std::unique_ptr<DagLikeProgramInterpreterInterface> parse_and_construct() {
            assert(!parsed);
            auto local_start_chrono = chrono::steady_clock::now();

            graph = Parser::parse(dag_as_string, m, meta_meta_array);

            auto timestep_parse = timestamp(local_start_chrono, "parse[VecInterp]");

            auto executor = create_graph_executor(graph);

            auto timestep_create_graph_executor = timestamp(timestep_parse, "construct[VecInterp]");

            parsed = true;
            return executor;
        }

        std::unique_ptr<DagLikeProgramInterpreterInterface> parse_and_compile() {
            assert(!parsed_and_compiled);
            auto local_start_chrono = chrono::steady_clock::now();

            auto executor = parse_and_construct();

            auto timestep_parse = timestamp(local_start_chrono, "parse_and_construct[VecInterp]");

            n = graph.input_ids.size();

            input_sizes = vector<size_t>(n, sizeof(BaseType) * m);

            output_sizes = executor->compile(input_sizes);

            auto timestep_compile = timestamp(timestep_parse, "compile[VecInterp]");

            auto timestep_prepare = timestamp(local_start_chrono, "parse+construct+compile[VecInterp]");

            parsed_and_compiled = true;
            return executor;
        }

        vector<Buffer> get_input_buffers(const FileForVecInterp &file) {
            assert(m == file.size());
            assert(m >= 1);
            assert(parsed_and_compiled);

            meta_input = (BaseType *) malloc(n * m * sizeof(BaseType));

            int meta_array_id = 0;

            vector<Buffer> input_buffers;
            for (int i = 0; i < n; i++) {
                Var::VarType local_var_type = file[0][i]->var_type;
                for (size_t j = 0; j < m; j++) {
                    assert(local_var_type == file[j][i]->var_type);
                }
                if (local_var_type == Var::_int) {
                    for (size_t j = 0; j < m; j++) {
                        meta_input[i * m + j] = ((IntVar *) file[j][i])->get();
                    }
                } else if (local_var_type == Var::_int_arr) {
                    assert(meta_meta_array.size() == meta_array_id);
                    meta_meta_array.push_back(vector<IntArrVar *>());
                    for (size_t j = 0; j < m; j++) {
                        assert(meta_meta_array.size() == 1);
                        assert(meta_meta_array[meta_array_id].size() == j);
                        meta_meta_array[meta_array_id].push_back((IntArrVar *) file[j][i]);
                        meta_input[i * m + j] = meta_array_id; //bools<j>
                    }
                    meta_array_id++;
                } else {
                    assert(false);
                }

                input_buffers.push_back({.ptr = &meta_input[i * m], .size = input_sizes[i]});
            }

            return input_buffers;
        }

        vector<Buffer> get_output_buffers() {
            assert(parsed_and_compiled);

            const size_t num_outputs = graph.output_ids.size();
            meta_output = static_cast<BaseType *>(malloc(num_outputs * m * sizeof(BaseType)));
            vector<Buffer> output_buffers;
            for (size_t i = 0; i < num_outputs; i++) {
                output_buffers.push_back({.ptr = &meta_output[i * m], .size = output_sizes[i]});
            }
            return output_buffers;
        }

        ~ParserAndCompiler() {
            free(meta_input);
            free(meta_output);
        }

    };


    using namespace std;

    void benchmark_vectorized_interpreter(const string &dag_as_string, const FileForVecInterp &file,
                                          int exec_trials = 20, bool print_score = false);

    void benchmark_julia_code(const string &dag_as_string, bool do_print = false);

    string read_dag_from_file(const string &file_path);
}

#endif //EXAFUNCTIONGRAPHEXECUTOR_TEST_CASES_H
