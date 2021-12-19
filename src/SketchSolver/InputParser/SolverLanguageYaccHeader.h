//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"


class Frame {
    map<SL_LY::Var, SL_LY::VarVal *> vars_map;
    map<Name, SL_LY::Var *> name_to_var_map;

public:
    Frame() = default;

    void clear()
    {
        vars_map.clear();
        name_to_var_map.clear();
    }

    void add_var_name(SL_LY::Var *var)
    {
        assert(name_to_var_map.find(*var->get_name()) == name_to_var_map.end());
        name_to_var_map[*var->get_name()] = var;
    }

    void add_var(SL_LY::Var *var) {
        assert(vars_map.find(*var) == vars_map.end());
        vars_map[*var] = nullptr;
        add_var_name(var);
    }

    exception NameNotFound;

    SL_LY::Var *name_to_var_throws(Name *name) {
        if(name_to_var_map.find(*name) == name_to_var_map.end())
        {
            throw NameNotFound;
        }
        return name_to_var_map[*name];
    }

    SL_LY::Var *name_to_var(Name *name) {
        try{
            return name_to_var_throws(name);
        }
        catch(exception& e){
            assert(false);
        }
    }

    exception VarNotFound;

    SL_LY::VarVal *get_var_val_throws(SL_LY::Var *var) {
        if(vars_map.find(*var) == vars_map.end()){
            throw VarNotFound;
        }
        return vars_map[*var];
    }

    SL_LY::VarVal *get_var_val(SL_LY::Var *var) {
        try {
            return get_var_val_throws(var);
        }
        catch (exception& e){
            assert(false);
        }
    }

    void set_var_val(SL_LY::Var *var, VarVal *var_val) {
        assert(vars_map.find(*var) == vars_map.end());
        vars_map[*var] = var_val;
        add_var_name(var);
    }
};

class SolverProgramState
{
public:
    Harness* harness{};
    const string& file_name;
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;
    bool hasGoodEnoughSolution;
    SolverProgramState(Harness* _harness, const string& _file_name, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            harness(_harness), file_name(_file_name), floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution)
    {

    }

    Methods* init_root = nullptr;
    void add_root(Methods* _init_root)
    {
        init_root = _init_root;
    }

//    map<SL_LY::Var, Method*> methods;

    Frame global;
    vector<Frame> frames;

    void add_var(SL_LY::Var *var) {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    VarVal* get_var_val(SL_LY::Var *var) {
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

    VarVal* set_var_val(SL_LY::Var *var, VarVal* var_val) {
        assert(!frames.empty());
        frames.rbegin()->set_var_val(var, var_val);
    }

    SL_LY::Var* name_to_var(Name* name)
    {
        try {
            return frames.rbegin()->name_to_var(name);
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

    void run()
    {
        assert(init_root != nullptr);

        init_root->populate_state(global);

        global.set_var_val(new SL_LY::Var(new Name("Harness"), new Name("harness")), new VarVal(harness));
        global.set_var_val(new SL_LY::Var(new Name("string"), new Name("file_name")), new VarVal(file_name));

        SL_LY::Var* init_f = new SL_LY::Var(new Name("Solution"), new Name("main"));
        auto input_params = vector<Param*>();

        input_params.emplace_back(new Param(new SL_LY::Var(new Name("Harness"), new Name("harness"))));
        input_params.emplace_back(new Param(new SL_LY::Var(new Name("string"), new Name("file_name"))));

        new_stack_frame();
        assert(frames.size() == 1);
        global.get_var_val(init_f)->get_method()->run(this, input_params);
        assert(frames.size() == 1);
        pop_stack_frame();

    }

    VarVal *return_var_val = nullptr;

    void set_return_var_val(VarVal *var_val) {
        assert(return_var_val == nullptr);
        return_var_val = var_val;
        assert(false);
    }

    Method *get_method(SL_LY::Var *var) {
        return global.get_var_val(var)->get_method();
    }

    VarVal *get_return_var_val() {
        assert(return_var_val != nullptr);
        VarVal* ret = return_var_val;
        return_var_val = nullptr;
        return ret;
    }

    void new_stack_frame(vector<Param *> &vars, vector<Param *> &vals) {
        assert(vars.size() == vals.size());

        vector<VarVal*> var_vals;
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

    SL_LY::Var *method_name_to_var(Name *name) {
        SL_LY::Var* ret = global.name_to_var(name);
        assert(global.get_var_val(ret)->is_method());
        return ret;
    }
};


void run_solver_langauge_program(SolverProgramState* _state);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
