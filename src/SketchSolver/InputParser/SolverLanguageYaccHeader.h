//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include <utility>

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"

class SolverProgramState: public ProgramState
{
public:
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;
    bool hasGoodEnoughSolution;

    const string& file_name;

    ofstream console_output = ofstream("console_output.out");

    SL::Methods* init_root = nullptr;

    void clear(bool conscious_call = true) override
    {
        console_output.close();
        init_root->clear();
        ProgramState::clear(true);
    }

    SolverProgramState(FunctionMap& _function_map, const string& _file_name, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            ProgramState(_function_map), file_name(_file_name),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution) {}

    void add_root(SL::Methods* _init_root) {
        init_root = _init_root;
    }

//    void add_var(SL::Var *var) {
//        assert(!frames.empty());
//        frames.rbegin()->add_var(var);
//    }
//
//    SL::VarVal* get_var_val(SL::Var *var) {
//        assert(!frames.empty());
//        try {
//            return frames.rbegin()->get_var_val_throws(var);
//        }
//        catch (exception& e) {
//            try {
//                return global.get_var_val_throws(var);
//            }
//            catch (exception& e2){
//                assert(false);
//            }
//        }
//    }
//
//    void set_var_val(SL::Var *var, SL::VarVal* var_val) {
//        assert(!frames.empty());
//        assert(var_val_invariant(var, var_val));
//        try {
//            frames.rbegin()->set_var_val_throws(var, var_val);
//        }
//        catch (exception& e)
//        {
//            string var_type_str = var->get_type()->to_string();
//            if(var_type_str == "SketchFunction") {
//                global.set_var_val(var, var_val);
//                string var_name = var->get_name()->to_string();
//                SketchFunction* skfunc = var_val->get_function(false);
//                FunctionMap& _function_map = skfunc->get_env()->function_map;
//                assert(&_function_map == &function_map);
//                auto new_primitive = function_map.insert(var_name, skfunc);
//                skfunc->set_rep(new_primitive);
//            }
//            else if(var_type_str == "int") {
//                assert(var->get_name()->to_string() == "seed");
//                global.set_var_val(var, var_val);
//
//                args.seed = var_val->get_int(false);
//            }
//            else
//            {
//                assert(false);
//            }
//        }
//    }
//
//    void add_and_set_var_val(SL::Var *var, SL::VarVal* var_val) {
//        add_var(var);
//        set_var_val(var, var_val);
//    }
//
//    SL::Var* name_to_var_throws(SL::Identifier* name)
//    {
//        assert(!frames.empty());
//        try {
//            return frames.rbegin()->name_to_var_throws(name);
//        }
//        catch (exception& e1) {
//            try {
//                return global.name_to_var_throws(name);
//            }
//            catch (exception& e2){
//                if(!SL::is_strongly_typed) {
//                    throw NameNotFound;
//                }
//                else {
//                    assert(false);
//                }
//            }
//        }
//    }
//
//    SL::Var* name_to_var(SL::Identifier* name)
//    {
//        assert(!frames.empty());
//        try {
//            return name_to_var_throws(name);
//        }
//        catch (exception& e1) {
//            cout << "ERROR: NAME NOT FOUND: " << name->to_string() << endl;
//            assert(false);
//        }
//    }

    SL::VarVal * eval();

//    SL::VarVal* return_var_val = nullptr;
//
//    bool has_return()
//    {
//        return return_var_val != nullptr;
//    }
//
//    void set_return_var_val(SL::VarVal* var_val) {
//        assert(return_var_val == nullptr);
//        var_val->set_return();
//        return_var_val = var_val;
//    }
//
//    SL::VarVal* get_return_var_val() {
//        if(return_var_val != nullptr) {
//            assert(return_var_val->get_is_return());
//            return_var_val->complete_return();
//        }
//        SL::VarVal* ret = return_var_val;
//        return_var_val = nullptr;
//        if(ret == nullptr)
//        {
//            ret = new SL::VarVal();
//        }
//        return ret;
//    }
//
//    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals, vector<SL::Param *>* meta_vals = nullptr);
//    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::VarVal *> &vals, vector<SL::Param *>* meta_vals = nullptr);
//
//    void new_stack_frame() {
//        frames.emplace_back(NestedFrame());
//        open_subframe();
//    }
//
//    void pop_stack_frame() {
//        assert(!frames.empty());
//        close_subframe();
//        assert(frames.rbegin()->is_empty());
//        frames.pop_back();
//    }
//
//    SL::Var *method_name_to_var(SL::Identifier *name) {
//        SL::Var* ret = global.name_to_var(name);
//        assert(global.get_var_val(ret)->is_method());
//        return ret;
//    }
//
//    void open_subframe();
//
//    void close_subframe();
//
//    void add_to_function_map(const string &skfunc_name, SketchFunction *skfunc);

};


void parse_solver_langauge_program(SolverProgramState* _state, string solver_program_file);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
