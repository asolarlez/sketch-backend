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
    public:
        explicit IntArrVar(vector<BaseType> _arr) : arr(std::move(_arr)), Var(Var::_int_arr) {}

        explicit IntArrVar(const IntArrVar *to_copy) : arr(to_copy->arr), Var(Var::_int_arr) {}

        IntArrVar(int size, int _default_val) : Var(Var::_int_arr) {
            assert(size == 0);
            default_val = _default_val;
        }

        void set(int idx, int val) {
            while (arr.size() <= idx) {
                arr.push_back(default_val);
            }
            arr[idx] = val;
        }

        BaseType get(BaseType idx) const {
            assert(idx >= 0);
            if (idx < arr.size()) {
                return arr[idx];
            } else {
                return default_val;
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

    class FileForVectorizedInterpreter : public vector<vector<Var *> > {

        static void
        register_token(const string &token, vector<Var *> &ret, bool &in_vec, vector<BaseType> &running_vec) {
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

        static vector<Var *> tokenize_and_parse(const string &line) {

            vector<Var *> ret;
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

    public:

        explicit FileForVectorizedInterpreter(const File* file);

        explicit FileForVectorizedInterpreter(const string &file_name) {
            ifstream file(file_name);
            assert(file.is_open());
            string line;
            while (getline(file, line)) {
                vector<Var *> input = tokenize_and_parse(line);
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
    };
}


#endif //EXAFUNCTIONGRAPHEXECUTOR_FILEFORVECTORIZEDINTERPRETER_H
