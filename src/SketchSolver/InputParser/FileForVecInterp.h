//
// Created by kliment on 7/2/22.
//

#ifndef EXAFUNCTIONGRAPHEXECUTOR_FILEFORVECTORIZEDINTERPRETER_H
#define EXAFUNCTIONGRAPHEXECUTOR_FILEFORVECTORIZEDINTERPRETER_H

#include <string>
#include <vector>
#include <cassert>
#include <fstream>
#include "HyperParams.h"
#include <iostream>
#include <fstream>

using namespace std;

class File;

namespace VectorizedInterpreter {

    class Var {
    public:
        enum VarType {
            _int, _int_arr, _no_type
        };
        const VarType var_type;

        Var(VarType _var_type) : var_type(_var_type) {}
    };

    class IntVar : public Var {
        BaseType val;
    public:
        explicit IntVar(BaseType _val) : val(_val), Var(Var::_int) {}

        BaseType get() const {
            return val;
        }
    };

    class IntArrVar : public Var {
        int default_val = 0;
        vector<BaseType> arr;
        const IntArrVar *copy = nullptr;

        bool is_set = false;
        int set_idx = -1;
        BaseType set_val;
    public:
        explicit IntArrVar(vector<BaseType> _arr) : arr(std::move(_arr)), Var(Var::_int_arr) {}

        explicit IntArrVar(const IntArrVar *to_copy) : copy(to_copy), Var(Var::_int_arr) {}

        IntArrVar(int size, int _default_val) : Var(Var::_int_arr) {
            assert(size == 0);
            default_val = _default_val;
        }

        void hard_set(int idx, int val) {
            assert(idx >=  0 && idx < arr.size());
            arr[idx] = val;
        }

        void set(int idx, int val) {
            assert(!is_set);
            set_idx = idx;
            set_val = val;
            is_set = true;
//            arr.resize(idx+1, 0);
//            arr[idx] = val;
        }

        BaseType get(BaseType idx) const {
            assert(idx >= 0);
            if(is_set && idx == set_idx) return set_val;
            if(copy == nullptr)
            {
                if (idx < arr.size()) {
                    return arr[idx];
                } else {
                    return default_val;
                }
            }
            else
            {
                assert(arr.empty());
                return copy->get(idx);
            }
        }
    };

//class BoolVar: public Var
//{
//    bool val;
//public:
//    explicit BoolVar(bool _val): val(_val) {}
//};
//
//class BoolArrVar: public Var
//{
//    vector<bool> val;
//public:
//    explicit BoolArrVar(vector<bool> _val): val(std::move(_val)) {}
//};

    class FileForVecInterp : private vector<vector < const Var *> > {

        static void
        register_token(const string &token, vector<const Var *> &ret, bool &in_vec, vector<BaseType> &running_vec) {
            if (token == "{") {
                assert(!in_vec);
                in_vec = true;
            } else if (token == "}") {
                assert(in_vec);
                ret.push_back(new IntArrVar(running_vec));
                running_vec.clear();
                in_vec = false;
            } else if (token != ",") {
                if (in_vec) {
                    running_vec.push_back(stoi(token));
                } else {
                    ret.push_back(new IntVar(stoi(token)));
                }
            }
        }

        static vector<const Var *> tokenize_and_parse(const string &line) {

            vector<const Var *> ret;
            bool in_vec = false;
            vector<BaseType> running_vec;

            string local_token;
            for (char c: line) {
                if (c == ' ' || c == ',' || c == '}' || c == '{') {
                    if (!local_token.empty()) {
                        register_token(local_token, ret, in_vec, running_vec);
                        local_token.clear();
                    }
                    if (c != ' ') {
                        local_token += c;
                        register_token(local_token, ret, in_vec, running_vec);
                        local_token.clear();
                    }
                } else if ('0' <= c && c <= '9') {
                    local_token += c;
                } else {
                    assert(false);
                }
            }
            if (!local_token.empty()) {
                register_token(local_token, ret, in_vec, running_vec);
            }

            assert(!in_vec);
            assert(running_vec.empty());

            return ret;
        }

        bool is_view = false;
        const FileForVecInterp* view_of_file = nullptr;
        int slice_start = 0;
        int slice_width;
        int slice_end;

    public:

        int size() const {
            if(is_view) {
                return slice_width;
            } else {
                return vector<vector < const Var *> >::size();
            }
        }

        int num_inputs_per_row() const
        {
            assert(!empty());
            return at(slice_start).size();
        }

//        FileForVecInterp(const FileForVecInterp* file, int slice_start, int slice_width);

        FileForVecInterp(const FileForVecInterp* _file, int _slice_start, int _slice_width)
        {
            is_view = true;
            view_of_file = _file;
            slice_start = _slice_start;
            slice_width = _slice_width;
            slice_end = min(_file->size(), slice_start+slice_width);
        }

        explicit FileForVecInterp(const File* file);

        explicit FileForVecInterp(const string &file_name) {
            ifstream file(file_name);
            assert(file.is_open());
            string line;
            while (getline(file, line)) {
                vector<const Var *> input = tokenize_and_parse(line);
                push_back(input);
            }
            if (false) {
                const int new_n = 120;
                ofstream fout("uav_kg__as_bools__" + std::to_string(new_n) + ".data");
                const int original_n = 192;
                for (int i = 0; i < size(); i++) {
                    for (int j = 0; j < at(i).size(); j++) {
                        if (j == 0) {
                            assert(((IntVar *) at(i)[j])->get() == original_n);
                            fout << new_n << " ";
                        } else if (j == 1) {
                            fout << "{";
                            for (int k = 0; k < new_n; k++) {
                                if (k >= 1) {
                                    fout << ", ";
                                }
                                fout << ((IntArrVar *) at(i)[j])->get(k % original_n);
                            }
                            fout << "} ";
                        } else {
                            fout << ((IntVar *) at(i)[j])->get() << endl;
                        }
                    }
                }
                fout.close();
                assert(false);
            }
        }

        vector<const FileForVecInterp*> get_slices(int local_slice_width) const
        {
            assert(!is_view);
            vector<const FileForVecInterp*> ret;
            int at_idx = 0;
            int sum = 0;
            while(at_idx < size())
            {
                FileForVecInterp* running_slice = new FileForVecInterp(this, at_idx, local_slice_width);
                assert(running_slice->size() == local_slice_width);
                sum += running_slice->size();
                ret.push_back(running_slice);
                at_idx+=local_slice_width;
                assert(sum == at_idx);
            }
            return ret;
        }

        const vector<const Var*>& operator [] (int idx) const {
            if(is_view) {
                return view_of_file->operator[](idx+slice_start);
            }
            else {
                return vector<vector<const Var*> >::operator[](idx);
            }
        }
    };
}


#endif //EXAFUNCTIONGRAPHEXECUTOR_FILEFORVECTORIZEDINTERPRETER_H
