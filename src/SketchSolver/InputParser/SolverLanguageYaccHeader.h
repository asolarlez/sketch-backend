//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include <utility>

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"

static exception NameNotFound;

class Frame {
    map<SL::Var, SL::VarVal* > vars_map;
    map<SL::Identifier, SL::Var *> name_to_var_map;
    vector<SL::Var*> all_new_vars;

public:
    Frame() = default;

    void clear(bool is_global = false)
    {
        for (auto it: vars_map) {
            if (it.second != nullptr) {
                if (is_global){
                    if (it.second->is_sketch_function()) {
                        assert(it.second->get_num_shared_ptr() == 1);
                        delete it.second;
                        continue;
                    }
                }
                it.second->decrement_shared_ptr();
            }
        }

        vars_map.clear();
        name_to_var_map.clear();
        for(auto it:all_new_vars){
            it->clear();
        }
        all_new_vars.clear();
    }

    void add_var_name(SL::Var *_var, bool assert_new = true)
    {
        SL::Identifier name = *_var->get_name();
        if(name_to_var_map.find(name) == name_to_var_map.end()) {
            SL::Var* var = new SL::Var(_var);
            all_new_vars.push_back(var);
            //new variable
            name_to_var_map[name] = var;
        }
        else
        {
            SL::Var* var = _var;
            assert(!assert_new);
            //existing variable.
            assert(*name_to_var_map[name] == *var);
        }
    }

    virtual void add_var(SL::Var *_var) {
        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);
        assert(vars_map.find(*var) == vars_map.end());
        vars_map[*var] = nullptr;
        add_var_name(var);
    }

    virtual SL::Var *name_to_var_throws(SL::Identifier *name) {
        if(name_to_var_map.find(*name) == name_to_var_map.end())
        {
            throw NameNotFound;
        }
        return name_to_var_map[*name];
    }

    SL::Var *name_to_var(SL::Identifier *name) {
        try{
            return name_to_var_throws(name);
        } catch(exception& e){
            AssertDebug(false, "ERROR:: IDENTIFIER NOT FOUND: " + name->to_string());
        }
    }

    exception VarNotFound;

    virtual SL::VarVal* get_var_val_throws(SL::Var *var) {
        if(vars_map.find(*var) == vars_map.end()){
            throw VarNotFound;
        }
        return vars_map[*var];
    }

    SL::VarVal* get_var_val(SL::Var *var) {
        try {
            return get_var_val_throws(var);
        } catch (exception& e){
            assert(false);
        }
    }

    virtual void set_var_val_throws(SL::Var *_var, SL::VarVal* var_val) {

        if(vars_map.find(*_var) == vars_map.end())
        {
            //not yet declared.
            throw VarNotFound;
        }
        else
        {
            //overwrite.
        }

        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);
        if(vars_map[*var] != nullptr){
            vars_map[*var]->decrement_shared_ptr();
            if(vars_map[*var]->get_num_shared_ptr() == 0)
            {
                vars_map[*var] = nullptr;
                vars_map.erase(*var);
            }
        }
        if(var_val != nullptr) {
            var_val->increment_shared_ptr();
        }
        vars_map[*var] = var_val;
        add_var_name(var, false);
    }

    void set_var_val(SL::Var *var, SL::VarVal* var_val) {
        try {
            set_var_val_throws(var, var_val);
        }
        catch (exception& e) {
            AssertDebug(false, "NOT YET DECLARED: " + var->to_string());
        }
    }

    void add_var_and_set_var_val_and_clear_var(SL::Var *var, SL::VarVal* var_val) {
        add_var(var);
        set_var_val(var, var_val);
        var->clear();
    }
};

class NestedFrame: private Frame
{
    vector<Frame> frames;

public:

    bool is_empty()
    {
        return frames.empty();
    }

    explicit NestedFrame() {
        assert(frames.empty());
    }

    void open_subframe()
    {
        frames.push_back(Frame());
    }

    void close_subframe()
    {
        assert(!frames.empty());
        frames.rbegin()->clear();
        frames.pop_back();
    }

    void add_var(SL::Var *var) override {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    SL::VarVal* get_var_val_throws(SL::Var *var) override {
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                return frames[at_id].get_var_val_throws(var);
            }
            catch (exception e){
                at_id--;
            }
        }
        throw VarNotFound;
    }

    void set_var_val_throws(SL::Var *var, SL::VarVal* var_val) override{
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                frames[at_id].set_var_val_throws(var, var_val);
                return ;
            }
            catch (exception e){
                at_id--;
            }
        }
        throw VarNotFound;
    }

    SL::Var* name_to_var_throws(SL::Identifier* name) override{
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                return frames[at_id].name_to_var_throws(name);
            }
            catch (exception e){
                at_id--;
            }
        }
        throw NameNotFound;
    }

    void clear()
    {
        assert(frames.size() == 1);
        frames[0].clear();
    }

};

class SolverProgramState
{
public:
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;
    bool hasGoodEnoughSolution;

    FunctionMap& function_map;

    SketchFunction* harness_ = nullptr;

    SolverProgramState(SketchFunction* _harness, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution, FunctionMap& _function_map):
            harness_(_harness),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution),
            function_map(_function_map),
            global() {}
    SolverProgramState(FunctionMap& _function_map, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            function_map(_function_map),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution), harness_(nullptr),
            global() {}

    SL::Methods* init_root = nullptr;
    void add_root(SL::Methods* _init_root)
    {
        init_root = _init_root;
    }

    Frame global;
    vector<NestedFrame> frames;

    void add_var(SL::Var *var) {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    SL::VarVal* get_var_val(SL::Var *var) {
        assert(!frames.empty());
        try {
            return frames.rbegin()->get_var_val_throws(var);
        }
        catch (exception& e) {
            try {
                return global.get_var_val_throws(var);
            }
            catch (exception& e2){
                assert(false);
            }
        }
    }

    void set_var_val(SL::Var *var, SL::VarVal* var_val) {
        assert(!frames.empty());
        assert(var_val_invariant(var, var_val));
        try {
            frames.rbegin()->set_var_val_throws(var, var_val);
        }
        catch (exception& e)
        {
            string var_type_str = var->get_type()->to_string();
            if(var_type_str == "SketchFunction") {
                global.set_var_val(var, var_val);
                string dag_name = var->get_name()->to_string();
                SketchFunction* sk_func = var_val->get_function(false);
                FunctionMap& _function_map = sk_func->get_env()->function_map;
                assert(&_function_map == &function_map);
                function_map.insert(dag_name, sk_func);
            }
            else if(var_type_str == "int") {
                assert(var->get_name()->to_string() == "seed");
                global.set_var_val(var, var_val);

                args.seed = var_val->get_int(false);
            }
            else
            {
                assert(false);
            }
        }
    }

    void add_and_set_var_val(SL::Var *var, SL::VarVal* var_val)
    {
        add_var(var);
        set_var_val(var, var_val);
    }

    SL::Var* name_to_var_throws(SL::Identifier* name)
    {
        assert(!frames.empty());
        try {
            return frames.rbegin()->name_to_var_throws(name);
        }
        catch (exception& e1) {
            try {
                return global.name_to_var_throws(name);
            }
            catch (exception& e2){
                if(!SL::is_strongly_typed) {
                    throw NameNotFound;
                }
                else {
                    assert(false);
                }
            }
        }
    }

    SL::Var* name_to_var(SL::Identifier* name)
    {
        assert(!frames.empty());
        try {
            return name_to_var_throws(name);
        }
        catch (exception& e1) {
            cout << "ERROR: NAME NOT FOUND: " << name->to_string() << endl;
            assert(false);
        }
    }

    SL::VarVal * eval();

    SL::VarVal* return_var_val = nullptr;

    bool has_return()
    {
        return return_var_val != nullptr;
    }

    void set_return_var_val(SL::VarVal* var_val) {
        assert(return_var_val == nullptr);
        var_val->set_return();
        return_var_val = var_val;
    }

    SL::VarVal* get_return_var_val() {
        if(return_var_val != nullptr) {
            assert(return_var_val->get_is_return());
            return_var_val->complete_return();
        }
        SL::VarVal* ret = return_var_val;
        return_var_val = nullptr;
        if(ret == nullptr)
        {
            ret = new SL::VarVal();
        }
        return ret;
    }

    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals, vector<SL::Param *>* meta_vals = nullptr);
    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::VarVal *> &vals, vector<SL::Param *>* meta_vals = nullptr);

    void new_stack_frame() {
        frames.emplace_back(NestedFrame());
        open_subframe();
    }

    void pop_stack_frame() {
        assert(!frames.empty());
        close_subframe();
        assert(frames.rbegin()->is_empty());
        frames.pop_back();
    }

    SL::Var *method_name_to_var(SL::Identifier *name) {
        SL::Var* ret = global.name_to_var(name);
        assert(global.get_var_val(ret)->is_method());
        return ret;
    }

    void open_subframe();

    void close_subframe();

    void add_to_function_map(const string &sk_func_name, SketchFunction *sk_func);

};


void parse_solver_langauge_program(SolverProgramState* _state, string solver_program_file);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
