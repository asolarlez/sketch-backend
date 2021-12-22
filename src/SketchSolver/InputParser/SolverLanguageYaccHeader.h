//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"


class Frame {
    map<SL::Var, SL::VarVal *> vars_map;
    map<SL::Name, SL::Var *> name_to_var_map;

public:
    Frame() = default;

    void clear()
    {
        vars_map.clear();
        name_to_var_map.clear();
    }

    void add_var_name(SL::Var *var)
    {
        if(name_to_var_map.find(*var->get_name()) == name_to_var_map.end()) {
            //new variable
            name_to_var_map[*var->get_name()] = var;
        }
        else
        {
            //existing variable.
            assert(name_to_var_map[*var->get_name()] == var);
        }
    }

    void add_var(SL::Var *var) {
        assert(vars_map.find(*var) == vars_map.end());
        vars_map[*var] = nullptr;
        add_var_name(var);
    }

    exception NameNotFound;

    SL::Var *name_to_var_throws(SL::Name *name) {
        if(name_to_var_map.find(*name) == name_to_var_map.end())
        {
            throw NameNotFound;
        }
        return name_to_var_map[*name];
    }

    SL::Var *name_to_var(SL::Name *name) {
        try{
            return name_to_var_throws(name);
        } catch(exception& e){
            assert(false);
        }
    }

    exception VarNotFound;

    SL::VarVal *get_var_val_throws(SL::Var *var) {
        if(vars_map.find(*var) == vars_map.end()){
            throw VarNotFound;
        }
        return vars_map[*var];
    }

    SL::VarVal *get_var_val(SL::Var *var) {
        try {
            return get_var_val_throws(var);
        } catch (exception& e){
            assert(false);
        }
    }

    void set_var_val(SL::Var *var, SL::VarVal *var_val) {
        if(vars_map.find(*var) == vars_map.end())
        {
            //new assignment.
        }
        else
        {
            //overwrite.
        }
        vars_map[*var] = var_val;
        add_var_name(var);
    }
};

class SolverProgramState
{
public:
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;
    bool hasGoodEnoughSolution;

    map<string, SketchFunction*>& function_map;

    SketchFunction* harness_ = nullptr;

    SolverProgramState(SketchFunction* _harness, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution, map<string, SketchFunction*>& _function_map):
            harness_(_harness),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution),
             function_map(_function_map) {}
    SolverProgramState(map<string, SketchFunction*>& _function_map, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            function_map(_function_map),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution), harness_(nullptr) {}

    SL::Methods* init_root = nullptr;
    void add_root(SL::Methods* _init_root)
    {
        init_root = _init_root;
    }

//    map<SL::Var, SL::Method*> methods;

    Frame global;
    vector<Frame> frames;

    void add_var(SL::Var *var) {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    SL::VarVal* get_var_val(SL::Var *var) {
        assert(!frames.empty());
        try {
            return frames.rbegin()->get_var_val_throws(var);
        }
        catch (exception& e)
        {
            try {
                return global.get_var_val(var);
            }
            catch (exception& e2){
                assert(false);
            }
        }
    }

    static bool var_val_invariant(SL::Var *var, SL::VarVal* var_val)
    {
        SL::VarValType var_val_type = var_val->get_type();
        string var_type_str = var->get_type()->to_string();
        if(var_type_str == "File"){
            assert(var_val_type == SL::file_val_type);
        }
        else if(var_type_str == "int"){
            assert(var_val_type == SL::int_val_type);
        }
        else if(var_type_str == "string"){
            assert(var_val_type == SL::string_val_type);
        }
        else if(var_type_str == "method"){
            assert(false);
        }
        else if(var_type_str == "SketchFunction"){
            assert(var_val_type == SL::skfunc_val_type);
        }
        else if(var_type_str == "Solution")
        {
            assert(var_val_type == SL::solution_val_type);
        }
        else if(var_type_str == "Program")
        {
            assert(var_val_type == SL::skfunc_val_type);
        }
        else if(var_type_str == "Input")
        {
            assert(var_val_type == SL::input_val_type);
        }
        else if(var_type_str == "bool")
        {
            assert(var_val_type == SL::bool_val_type);
        }
        else{
            assert(false);
        }
        return true;
    }

    SL::VarVal* set_var_val(SL::Var *var, SL::VarVal* var_val) {
        assert(!frames.empty());
        assert(var_val_invariant(var, var_val));
        frames.rbegin()->set_var_val(var, var_val);
    }

    SL::Var* name_to_var(SL::Name* name)
    {
        try {
            return frames.rbegin()->name_to_var_throws(name);
        }
        catch (exception& e1) {
            try {
                return global.name_to_var(name);
            }
            catch (exception& e2){
                assert(false);
            }
        }
    }

    SolverLanguagePrimitives::SolutionHolder* eval();

    SL::VarVal *return_var_val = nullptr;

    void set_return_var_val(SL::VarVal *var_val) {
        assert(return_var_val == nullptr);
        return_var_val = var_val;
    }

    SL::Method *get_method(SL::Var *var) {
        return global.get_var_val(var)->get_method();
    }

    SL::VarVal *get_return_var_val() {
        assert(return_var_val != nullptr);
        SL::VarVal* ret = return_var_val;
        return_var_val = nullptr;
        return ret;
    }

    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals) {
        assert(vars.size() == vals.size());

        vector<SL::VarVal*> var_vals;
        for(int i = 0;i<vals.size();i++)
        {
            var_vals.emplace_back(vals[i]->eval(this));
        }

        frames.emplace_back(Frame());

        for(int i = 0;i < vars.size();i++)
        {
            set_var_val(vars[i]->get_var(), var_vals[i]);
        }
    }

    void new_stack_frame() {
        frames.emplace_back(Frame());
    }

    void pop_stack_frame() {
        frames.rbegin()->clear();
        frames.pop_back();
    }

    SL::Var *method_name_to_var(SL::Name *name) {
        SL::Var* ret = global.name_to_var(name);
        assert(global.get_var_val(ret)->is_method());
        return ret;
    }

    void add_var_name(SL::Var *var) {
        assert(!frames.empty());
        frames.rbegin()->add_var_name(var);
    }
};


void run_solver_langauge_program(SolverProgramState* _state, string solver_program_file);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
