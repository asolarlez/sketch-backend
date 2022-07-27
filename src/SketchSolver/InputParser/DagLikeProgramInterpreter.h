//
// Created by Kliment Serafimov on 7/27/22.
//

#ifndef SKETCH_DAGLIKEPROGRAMINTERPRETER_H
#define SKETCH_DAGLIKEPROGRAMINTERPRETER_H

#endif //SKETCH_DAGLIKEPROGRAMINTERPRETER_H

#include "types.h"

namespace VectorizedInterpreter {

    template<typename T>
    class MapTrait {
        virtual T operator[](int idx) = 0;

        virtual void set(int idx, T val) = 0;

        virtual bool contains(int idx) = 0;
    };


    bool is_bit(int b_or_not_b);

    class ArrayMap : public MapTrait<int> {
        vector<int> my_map;
    public:
        explicit ArrayMap(int num_indexes) : my_map(vector<int>(num_indexes, -1)) {}

        int operator[](int idx) override {
            assert(contains(idx));
            return my_map[idx];
        }

        void set(int idx, int val) override {
            assert(val >= 0);
            assert(idx < my_map.size());
            my_map[idx] = val;
        }

        bool contains(int idx) override {
            assert(my_map[idx] >= -1);
            return my_map[idx] != -1;
        }
    };

    class DagLikeProgramInterpreterInterface {
    public:
        virtual ~DagLikeProgramInterpreterInterface() = default;

        virtual vector <size_t> compile(
                const vector <size_t> &input_sizes) = 0;

        virtual void run(const vector <Buffer> &inputs,
                         const vector <Buffer> &outputs) = 0;

        virtual string dag_to_string() = 0;
    };

    unique_ptr <DagLikeProgramInterpreterInterface> create_graph_executor(Graph graph);

    class SetTrait {
    public:
        virtual bool contains(int idx) const = 0;

        virtual void insert(int idx) = 0;
    };

    class CompactMask : public SetTrait {
        typedef uint32_t WORD_TYPE;
        static const int word_size_bits = 5;
        static_assert(sizeof(WORD_TYPE) * 8 == 1 << word_size_bits, "WORD_TYPE and word_size_bits are inconsistent.");
        static const int word_size = 1 << word_size_bits;
        static const int low_order_bits_mask = (1 << word_size_bits) - 1;
        int num_idxs;
        vector<WORD_TYPE> visited;
        size_t num_elements = 0;
    public:
        explicit CompactMask(int _num_idxs) : num_idxs(_num_idxs) {
            visited = vector<WORD_TYPE>((num_idxs + word_size - 1) >> word_size_bits, 0);
        }

        bool contains(int idx) const override {
            bool ret = (visited[idx >> word_size_bits] & (1 << (idx & low_order_bits_mask))) != 0;
            return ret;
        }

        void insert(int idx) override {
            int at_word = idx >> word_size_bits;
            WORD_TYPE original_word = visited[at_word];
            visited[at_word] |= (1 << (idx & low_order_bits_mask));
            num_elements += original_word != visited[at_word];
        }

        size_t size() const {
            return num_elements;
        }

        // Minimum required for range-for loop
        struct Iterator {
            int at_idx;
            const CompactMask *_this;

            int operator*() const { return at_idx; }

            bool operator!=(const Iterator &rhs) const {
                return at_idx != rhs.at_idx;
            }

            void operator++() {
                at_idx++;
                while (!_this->contains(at_idx) && at_idx < _this->num_idxs) {
                    at_idx++;
                }
            }
        };

        // auto return requires C++14
        auto begin() const {
            int at_idx = 0;
            while (!contains(at_idx) && at_idx < num_idxs) {
                at_idx++;
            }
            return Iterator{at_idx, this};
        }

        auto end() const {
            return Iterator{num_idxs, this};
        }
    };

    class DagLikeProgramInterpreter : public DagLikeProgramInterpreterInterface {
        Graph graph;
        size_t vector_size{};
        vector<vector<int> > next_op;
        vector<size_t> buffer_sizes;
        vector<Buffer> buffers;
        int num_buffers = 0;

        size_t meta_buffer_size = 0;
        void *meta_buffer{};

    public:
        explicit DagLikeProgramInterpreter(Graph _graph);

        std::vector<size_t> compile(const std::vector<size_t> &input_sizes) override;

        void run(const std::vector<Buffer> &inputs,
                 const std::vector<Buffer> &outputs) override;

        string dag_to_string() override;

    };


}