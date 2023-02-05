//
// Created by kliment on 7/2/22.
//

#include <cassert>
#include <set>
#include <map>
#include <memory>

#include <stack>
#include <cstring>
#include <iostream>
#include "HyperParams.h"
#include "DagLikeProgramInterpreter.h"

using namespace std;

namespace VectorizedInterpreter {

    std::unique_ptr<DagLikeProgramInterpreterInterface> create_graph_executor(Graph graph) {
        return std::make_unique<DagLikeProgramInterpreter>(std::move(graph));
    }

    bool is_bit(int b_or_not_b) {
        return b_or_not_b == 0 || b_or_not_b == 1;
    }

    DagLikeProgramInterpreter::DagLikeProgramInterpreter(Graph _graph) {
        graph = move(_graph);

        int max_original_buffer_id = -1;
        for(const auto& op : graph.operations) {
            for(auto input_buffer_id: op.input_ids) {
                assert(input_buffer_id >= 0);
                max_original_buffer_id = max(max_original_buffer_id, input_buffer_id);
            }
            for(auto output_buffer_id: op.output_ids) {
                assert(output_buffer_id >= 0);
                max_original_buffer_id = max(max_original_buffer_id, output_buffer_id);
            }
        }

        for(auto input_id : graph.input_ids) {
            assert(input_id >= 0);
            max_original_buffer_id = max(max_original_buffer_id, input_id);
        }

        for(auto& output_id : graph.output_ids) {
            assert(output_id >= 0);
            max_original_buffer_id = max(max_original_buffer_id, output_id);
        }

        int num_indexes = max_original_buffer_id + 1;
        CompactMask buffer_ids = CompactMask(num_indexes);

        for(const auto& op : graph.operations)
        {
            for(auto input_buffer_id: op.input_ids) {
                assert(input_buffer_id >= 0);
                buffer_ids.insert(input_buffer_id);
            }
            for(auto output_buffer_id: op.output_ids) {
                assert(output_buffer_id >= 0);
                buffer_ids.insert(output_buffer_id);
            }
        }

        for(auto input_id : graph.input_ids) {
            assert(input_id >= 0);
            buffer_ids.insert(input_id);
        }

        for(auto& output_id : graph.output_ids) {
            assert(output_id >= 0);
            buffer_ids.insert(output_id);
        }

        ArrayMap old_buff_id_to_new_buff_id(num_indexes);
        assert(num_buffers == 0);
        for(auto buffer_id: buffer_ids) {
            old_buff_id_to_new_buff_id.set(buffer_id, num_buffers++);
        }

        for(auto& op : graph.operations)
        {
            for(auto& input_buffer_id: op.input_ids) {
                assert(old_buff_id_to_new_buff_id.contains(input_buffer_id));
                input_buffer_id = old_buff_id_to_new_buff_id[input_buffer_id];
            }
            for(auto& output_buffer_id: op.output_ids) {
                assert(old_buff_id_to_new_buff_id.contains(output_buffer_id));
                output_buffer_id = old_buff_id_to_new_buff_id[output_buffer_id];
            }
        }

        for(auto& op : graph.input_ids) {
            assert(old_buff_id_to_new_buff_id.contains(op));
            op = old_buff_id_to_new_buff_id[op];
        }

        for(auto& op : graph.output_ids) {
            assert(old_buff_id_to_new_buff_id.contains(op));
            op = old_buff_id_to_new_buff_id[op];
        }

        assert(num_buffers == buffer_ids.size());

        next_op = vector<vector<int> >(num_buffers+1, vector<int>());

        for(int op_id = 0;op_id < graph.operations.size(); op_id++)
        {
            for(auto input_buffer_id: graph.operations[op_id].input_ids) {
                next_op[input_buffer_id].push_back(op_id);
            }
            if(graph.operations[op_id].input_ids.empty())
            {
                next_op[num_buffers].push_back(op_id);
            }
        }
    }

    void *get_first_free_slot(BaseType buffer[], size_t meta_buffer_size, size_t vector_size) {
        assert((vector_size % sizeof(BaseType)) == 0);
        assert((meta_buffer_size % vector_size) == 0);
//    for(size_t i = 0;i<meta_buffer_size/sizeof(BaseType); i+=vector_size/sizeof(BaseType))
//    {
//        cout << buffer[i] <<" ";
//    }
//    cout << endl;
        for (size_t i = 0; i < meta_buffer_size / sizeof(BaseType); i += vector_size / sizeof(BaseType)) {
            if (buffer[i] == 0) {
                buffer[i] = 1;
//            for(size_t j = 0;j<meta_buffer_size/sizeof(BaseType); j+=vector_size/sizeof(BaseType))
//            {
//                cout << buffer[j] <<" ";
//            }
//            cout << endl << "----------------" << endl;
                return &buffer[i];
            }
        }
        cout << "NO FREE FLOAT" << endl;
        assert(false);
        return nullptr;
    }

    std::vector<size_t> DagLikeProgramInterpreter::compile(
            const std::vector<size_t> &input_sizes) {

        assert(input_sizes.size() == graph.input_ids.size());

        assert(buffer_sizes.empty());
        buffer_sizes = vector<size_t>(num_buffers, 0);

        size_t active_memory = 0;
        size_t max_active_memory = 0;

        {
            stack<int> at_buff_ids;

            CompactMask visited_buff_ids = CompactMask(num_buffers);

            at_buff_ids.push(num_buffers);

            for (int i = 0; i < graph.input_ids.size(); i++) {
                int buff_id = graph.input_ids[i];
                at_buff_ids.push(buff_id);
                assert(buffer_sizes[buff_id] == 0);
                buffer_sizes[buff_id] = input_sizes[i];
                active_memory += input_sizes[i];
            }

            max_active_memory = max(max_active_memory, active_memory);

            vector<int> num_inputs_visited = vector<int>(graph.operations.size(), 0);
            vector<int> num_sinks_visited = vector<int>(num_buffers, 0);
            while (!at_buff_ids.empty()) {
                int at_buff_id = at_buff_ids.top();
                at_buff_ids.pop();

                for (auto next_op_id: next_op[at_buff_id]) {
                    auto op = graph.operations[next_op_id];
                    if (at_buff_id != num_buffers) { // this is special case; if it is not 0-input node
                        assert(num_inputs_visited[next_op_id] < op.input_ids.size());
                        num_inputs_visited[next_op_id] += 1;
                    }
                    if (num_inputs_visited[next_op_id] == op.input_ids.size()) {
                        vector<size_t> local_input_sizes;
                        for (auto local_input_id: op.input_ids) {
                            assert(buffer_sizes[local_input_id] != 0);
                            local_input_sizes.push_back(buffer_sizes[local_input_id]);
                        }
                        vector<size_t> output_sizes = op.op->compute_output_sizes(local_input_sizes);
                        for (int i = 0; i < op.output_ids.size(); i++) {
                            int local_output_id = op.output_ids[i];
                            assert(buffer_sizes[local_output_id] == 0);
                            buffer_sizes[local_output_id] = output_sizes[i];
                        }

                        for (auto output_buff_id: op.output_ids) {
                            if (!visited_buff_ids.contains(output_buff_id)) {
                                at_buff_ids.push(output_buff_id);
                                visited_buff_ids.insert(output_buff_id);
                                active_memory += buffer_sizes[output_buff_id];
                            }
                        }

                        max_active_memory = max(max_active_memory, active_memory);

                        for (int i = 0; i < op.input_ids.size(); i++) {
                            int input_buffer_id = op.input_ids[i];
                            num_sinks_visited[input_buffer_id] += 1;
                            if (num_sinks_visited[input_buffer_id] == next_op[input_buffer_id].size()) {
                                active_memory -= buffer_sizes[input_buffer_id];
                            }
                        }
                    }
                }
            }
        }

        assert(!buffer_sizes.empty());
        vector_size = buffer_sizes[0];
        for (int i = 1; i < buffer_sizes.size(); i++) {
            assert(vector_size == buffer_sizes[i]);
        }

        vector<size_t> ret;

        for (auto output_buffer_id: graph.output_ids) {
            ret.push_back(buffer_sizes[output_buffer_id]);
        }

        //static allocation of memory;
        if (true) {

            buffers = vector<Buffer>(num_buffers, Buffer());
            assert(buffers.size() == buffer_sizes.size());
            for (int i = 0; i < buffers.size(); i++) {
                buffers[i] = {
                        .ptr = malloc(buffer_sizes[i]),
                        .size = buffer_sizes[i]
                };
            }
        }
            //other way to do static allocation
        else if (false) {
            assert(meta_buffer_size == 0);
            for (int i = 0; i < buffer_sizes.size(); i++) {
                meta_buffer_size += buffer_sizes[i];
            }
            meta_buffer = malloc(meta_buffer_size);

            buffers = vector<Buffer>(num_buffers, Buffer());
            assert(buffers.size() == buffer_sizes.size());
            for (int i = 0; i < buffers.size(); i++) {
                if (i == 0) {
                    buffers[i] = {
                            .ptr = meta_buffer,
                            .size = buffer_sizes[i]
                    };
                } else {
                    void *next_ptr = (void *) (((unsigned long long) buffers[i - 1].ptr) + buffer_sizes[i - 1]);
                    buffers[i] = {
                            .ptr = next_ptr,
                            .size = buffer_sizes[i]
                    };
                }
            }
        }
            // allocate only max_active_memory
        else {
            // calc replacement strategy
            {
                assert(meta_buffer_size == 0);
                meta_buffer_size = max_active_memory;

                meta_buffer = malloc(meta_buffer_size);
                memset(meta_buffer, 0, sizeof(meta_buffer));

                assert(meta_buffer_size % sizeof(BaseType) == 0);
                for (int j = 0; j < meta_buffer_size / sizeof(BaseType); j++) {
                    ((BaseType *) meta_buffer)[j] = 0;
                }

                buffers = vector<Buffer>(num_buffers, Buffer());

                size_t new_active_memory = 0;
                size_t new_max_active_memory = 0;
                {
                    stack<int> at_buff_ids;
                    CompactMask visited_buff_ids = CompactMask(num_buffers);

                    at_buff_ids.push(num_buffers);

                    for (int i = 0; i < graph.input_ids.size(); i++) {
                        int buff_id = graph.input_ids[i];
                        at_buff_ids.push(buff_id);
                        assert(buffer_sizes[buff_id] == input_sizes[i]);
                        buffers[buff_id] = {
                                .ptr = get_first_free_slot((BaseType *) meta_buffer, meta_buffer_size, vector_size),
                                .size = buffer_sizes[buff_id]
                        };
                        new_active_memory += input_sizes[i];
                    }

                    new_max_active_memory = max(new_max_active_memory, new_active_memory);

                    vector<int> num_inputs_visited = vector<int>(graph.operations.size(), 0);
                    vector<int> num_sinks_visited = vector<int>(num_buffers, 0);

                    while (!at_buff_ids.empty()) {
                        int at_buff_id = at_buff_ids.top();
                        at_buff_ids.pop();

                        for (auto next_op_id: next_op[at_buff_id]) {
                            auto op = graph.operations[next_op_id];
                            if (at_buff_id != num_buffers) { // this is special case; if it is not 0-input node
                                assert(num_inputs_visited[next_op_id] < op.input_ids.size());
                                num_inputs_visited[next_op_id] += 1;
                            }
                            if (num_inputs_visited[next_op_id] == op.input_ids.size()) {
                                vector<size_t> local_input_sizes;
                                for (auto local_input_id: op.input_ids) {
                                    assert(buffer_sizes[local_input_id] != 0);
                                    local_input_sizes.push_back(buffer_sizes[local_input_id]);
                                }
                                vector<size_t> output_sizes = op.op->compute_output_sizes(local_input_sizes);
                                for (int i = 0; i < op.output_ids.size(); i++) {
                                    int local_output_id = op.output_ids[i];
                                    assert(buffer_sizes[local_output_id] == output_sizes[i]);
                                    buffers[local_output_id] = {
                                            .ptr = get_first_free_slot((BaseType *) meta_buffer, meta_buffer_size,
                                                                       vector_size),
                                            .size = buffer_sizes[local_output_id]
                                    };
                                }

                                for (auto output_buff_id: op.output_ids) {
                                    if (!visited_buff_ids.contains(output_buff_id)) {
                                        at_buff_ids.push(output_buff_id);
                                        visited_buff_ids.insert(output_buff_id);
                                        new_active_memory += buffer_sizes[output_buff_id];
                                    }
                                }

                                new_max_active_memory = max(new_max_active_memory, new_active_memory);

                                for (int i = 0; i < op.input_ids.size(); i++) {
                                    int input_buffer_id = op.input_ids[i];
                                    num_sinks_visited[input_buffer_id] += 1;
                                    if (num_sinks_visited[input_buffer_id] == next_op[input_buffer_id].size()) {
                                        new_active_memory -= buffer_sizes[input_buffer_id];
                                        assert(buffers[input_buffer_id].ptr != nullptr);
                                        assert(*(BaseType *) (buffers[input_buffer_id].ptr) == 1);
                                        *(BaseType *) (buffers[input_buffer_id].ptr) = 0;
//                                for(int j = 0;j<meta_buffer_size/sizeof(BaseType); j+=vector_size/sizeof(BaseType))
//                                {
//                                    cout << ((BaseType*)meta_buffer)[j] <<" ";
//                                }
//                                cout << endl;
//                                cout << "*****" << endl;
                                    }
                                }
                            }
                        }
                    }
                }
                assert(new_max_active_memory == max_active_memory);
            }
        }

        return ret;
    }

    void DagLikeProgramInterpreter::run(const std::vector<Buffer> &inputs,
                                        const std::vector<Buffer> &outputs) {

        assert(inputs.size() == graph.input_ids.size());

        stack<int> at_buff_ids;
        CompactMask visited_buff_ids = CompactMask(num_buffers);

        at_buff_ids.push(num_buffers);

        assert(graph.input_ids.size() == graph.input_idxs.size());

        for (int _i = 0; _i < graph.input_ids.size(); _i++) {
            int input_buff_id = graph.input_ids[_i];
            at_buff_ids.push(input_buff_id);

            int input_idx = graph.input_idxs[_i];
            assert(inputs[input_idx].size == buffers[input_buff_id].size);
            assert(buffers[input_buff_id].size == buffer_sizes[input_buff_id]);
            assert(buffers[input_buff_id].ptr != nullptr);
            buffers[input_buff_id].ptr = inputs[input_idx].ptr;
        }

        for (int i = 0; i < graph.output_ids.size(); i++) {
            int output_buff_id = graph.output_ids[i];
            assert(outputs[i].size == buffers[output_buff_id].size);
            assert(buffers[output_buff_id].size == buffer_sizes[output_buff_id]);
            assert(buffers[output_buff_id].ptr != nullptr);
            buffers[output_buff_id].ptr = outputs[i].ptr;
        }

        vector<int> num_inputs_visited = vector<int>(graph.operations.size(), 0);

        while (!at_buff_ids.empty()) {
            int at_buff_id = at_buff_ids.top();
            at_buff_ids.pop();

            for (auto next_op_id: next_op[at_buff_id]) {
                auto op = graph.operations[next_op_id];
                if (at_buff_id != num_buffers) { // this is special case; if it is not 0-input node
                    assert(num_inputs_visited[next_op_id] < op.input_ids.size());
                    num_inputs_visited[next_op_id] += 1;
                }
                if (num_inputs_visited[next_op_id] == op.input_ids.size()) {
                    vector<Buffer> local_input_buffers;
                    for (auto local_input_id: op.input_ids) {
                        assert(buffers[local_input_id].ptr != nullptr);
                        assert(buffers[local_input_id].size == buffer_sizes[local_input_id]);
                        local_input_buffers.push_back(buffers[local_input_id]);
                    }
                    vector<Buffer> local_output_buffers;
                    for (int i = 0; i < op.output_ids.size(); i++) {
                        int local_output_id = op.output_ids[i];
                        assert(buffers[local_output_id].ptr != nullptr);
                        assert(buffers[local_output_id].size == buffer_sizes[local_output_id]);

                        local_output_buffers.push_back(buffers[local_output_id]);
                    }

                    op.op->run(local_input_buffers, local_output_buffers);

                    for (auto output_buff_id: op.output_ids) {
                        if (!visited_buff_ids.contains(output_buff_id)) {
                            at_buff_ids.push(output_buff_id);
                            visited_buff_ids.insert(output_buff_id);
                        }
                    }
                }
            }
        }
    }

    string DagLikeProgramInterpreter::dag_to_string() {
        string ret;

        stack<int> at_buff_ids;
        CompactMask visited_buff_ids = CompactMask(num_buffers);

        at_buff_ids.push(num_buffers);

        ret += "num_buffers = " + std::to_string(num_buffers) + "\n";
        bool print_inputs = true;
        if (print_inputs) {
            ret += "input_buffer_ids = [";
            for (int input_buff_id: graph.input_ids) {
                ret += std::to_string(input_buff_id + 1) + ", ";
            }
            ret += "]\n";
        }

        ret +=
                "function f(m, buffers, meta_array)\n"
                "\tfunction arr_r(i, b)\n"
                "\t\treturn meta_array[i][b+1]\n"
                "\tend\n";

        for (int input_buff_id: graph.input_ids) {
            at_buff_ids.push(input_buff_id);
        }

        string tab = "\t";

        vector<int> num_inputs_visited = vector<int>(graph.operations.size(), 0);

        while (!at_buff_ids.empty()) {
            int at_buff_id = at_buff_ids.top();
            at_buff_ids.pop();

            for (auto next_op_id: next_op[at_buff_id]) {
                auto op = graph.operations[next_op_id];
                if (at_buff_id != num_buffers) { // this is special case; if it is not 0-input node
                    assert(num_inputs_visited[next_op_id] < op.input_ids.size());
                    num_inputs_visited[next_op_id] += 1;
                }
                if (num_inputs_visited[next_op_id] == op.input_ids.size()) {
                    ret += tab + op.to_string() + "\n";
                    for (auto output_buff_id: op.output_ids) {
                        if (!visited_buff_ids.contains(output_buff_id)) {
                            at_buff_ids.push(output_buff_id);
                            visited_buff_ids.insert(output_buff_id);
                        }
                    }
                }
            }
        }

        ret += tab + "return [";
        for (int output_buff_id: graph.output_ids) {
            ret += std::to_string(output_buff_id + 1) + ", ";
        }
        ret += "]\n";
        ret += "end";
        return ret;
    }

};
