//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include <utility>

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"


class Frame {
    map<SL::Var, SL::VarVal* > vars_map;
    map<SL::Name, SL::Var *> name_to_var_map;
#define DOCOPY

#ifndef DOCOPY
    //pass
#else
    vector<SL::Var*> all_new_vars;
#endif

public:
    Frame() {};

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
                if (it.second->get_num_shared_ptr() == 0) {
                    cout << "CLEARED " << it.first.to_string() << endl;
                }
            }
        }

        vars_map.clear();
        name_to_var_map.clear();

#ifndef DOCOPY
        //pass
#else
        for(auto it:all_new_vars)
        {
            it->clear();
        }
        all_new_vars.clear();
#endif
    }


    void add_var_name(SL::Var *_var, bool assert_new = true)
    {
        SL::Name name = *_var->get_name();
        if(name_to_var_map.find(name) == name_to_var_map.end()) {
        #ifndef DOCOPY
            SL::Var* var = _var;
        #else
            SL::Var* var = new SL::Var(_var);
            all_new_vars.push_back(var);
        #endif
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

    void add_var(SL::Var *_var) {

#ifndef DOCOPY
        SL::Var* var = _var;
#else
        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);
#endif
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
            AssertDebug(false, "ERROR:: IDENTIFIER NOT FOUND: " + name->to_string());
        }
    }

    exception VarNotFound;

    SL::VarVal* get_var_val_throws(SL::Var *var) {
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

    void set_var_val_throws(SL::Var *_var, SL::VarVal* var_val) {

        if(vars_map.find(*_var) == vars_map.end())
        {
//            delete var;
            //not yet declared.
            throw VarNotFound;
        }
        else
        {
            //overwrite.
        }

#ifndef DOCOPY
        SL::Var* var = _var;
#else
        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);
#endif
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
        try{
            set_var_val_throws(var, var_val);
        }
        catch (exception& e)
        {
            //not yet declared
            assert(false);
        }
    }

    void add_var_and_set_var_val_and_clear_var(SL::Var *var, SL::VarVal* var_val) {
        add_var(var);
        set_var_val(var, var_val);
        var->clear();
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
            function_map(_function_map), global() {}
    SolverProgramState(map<string, SketchFunction*>& _function_map, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            function_map(_function_map),
            floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution), harness_(nullptr),
            global() {}

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
        catch (exception& e) {
            try {
                return global.get_var_val(var);
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
                function_map[var->get_name()->to_string()] = var_val->get_function();
                //!!! HERE FIX THIS
                function_map[var->get_name()->to_string()]->get_env()->functionMap[var->get_name()->to_string()] =
                        function_map[var->get_name()->to_string()]->get_dag();
            }
            else if(var_type_str == "int")
            {
                assert(var->get_name()->to_string() == "seed");
                global.set_var_val(var, var_val);

                args.seed = var_val->get_int();
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

    SL::Var* name_to_var(SL::Name* name)
    {
        try {
            return frames.rbegin()->name_to_var_throws(name);
        }
        catch (exception& e1) {
            try {
                return global.name_to_var_throws(name);
            }
            catch (exception& e2){
                if(!SL::is_strongly_typed) {
                    SL::Name* any_name = new SL::Name("any");
                    SL::SLType* any_type = new SL::SLType(any_name);
                    SL::Name* copy_name = new SL::Name(name->to_string());
                    SL::Var* var = new SL::Var(any_type, copy_name);
                    add_var(var);
                    var->clear();

                    return name_to_var(name);
                }
                else {
                    assert(false);
                }
            }
        }
    }

    SolverLanguagePrimitives::SolutionHolder* eval();

    SL::VarVal* return_var_val = nullptr;

    void set_return_var_val(SL::VarVal* var_val) {
        assert(return_var_val == nullptr);
        var_val->set_return();
        return_var_val = var_val;
    }

    SL::Method *get_method(SL::Var *var) {
        return global.get_var_val(var)->get_method();
    }

    SL::VarVal* get_return_var_val() {
        SL::VarVal* ret = return_var_val;
        return_var_val = nullptr;
        return ret;
    }

    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals);

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

};


void parse_solver_langauge_program(SolverProgramState* _state, string solver_program_file);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
