//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_SKETCHFUNCTION_H
#define SKETCH_SOURCE_SKETCHFUNCTION_H

//#include "BooleanDAG.h"
//#include "ProgramEnvironment.h"

#include "BooleanNodes.h"
#include "VarStore.h"
#include "SkVal.h"
#include "File.h"
#include "ProgramEnvironment.h"

class BooleanDAG;
class SolverProgramState;

namespace SL
{
    class VarVal;
    class FunctionCall;
}

class SketchFunction
{
    //unrolled dag is the the original unrolled dag using prepare miter at before calling assertDAG
    BooleanDAG* original_dag = nullptr;
    //root dag is
    // IF NOT CONCRETIZED: the root harness dag
    // IF CONCRETIZED: a fully unrolled concretized dag using the doInline from env.
    BooleanDAG* root_dag = nullptr;

    //if env == nullptr => original_dag and root_dag ARE NOT concretized
    //if env != nullptr => original_dag and root_dag ARE concretized
    ProgramEnvironment* env = nullptr;

    bool new_way = true;
    bool keep_track_of_original = false;

public:

    ProgramEnvironment* get_env()
    {
        return env;
    }

    SketchFunction(SketchFunction* shallow_copy): original_dag(shallow_copy->original_dag), root_dag(shallow_copy->root_dag), env(shallow_copy->env) {}

    SketchFunction(
            BooleanDAG* _dag_root,
            BooleanDAG* _original_dag = nullptr,
            ProgramEnvironment* _evn = nullptr):
            root_dag(_dag_root), original_dag(_original_dag), env(_evn){
        if(new_way)
        {
            if(!keep_track_of_original)
            {
                original_dag = nullptr;
            }
        }
        else
        {
            if(_original_dag != nullptr)
            {
                root_dag = original_dag;
                original_dag = nullptr;
            }
        }
    }

    SketchFunction* produce_inlined_dag(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, deactivate_pcond, true);
    }

    void do_inline(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        produce_concretization(var_store, var_type, deactivate_pcond, false);
    }


    void concretize(VarStore& var_store, bool_node::Type var_type, bool deactivate_pcond = false)
    {
        produce_concretization(var_store, var_type, deactivate_pcond, false);
    }

    SketchFunction* produce_concretization(VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond = false, bool do_clone = true);

    SketchFunction *clone() ;

    BooleanDAG *get_dag() {
        return root_dag;
    }

    void clear();

private:
    VarStore* ctrl_var_store__solution;
public:
    VarStore* get_ctrl_var_store()
    {
        return ctrl_var_store__solution;
    }
    void set_solution(VarStore* _ctrl_var_store)
    {
        ctrl_var_store__solution = _ctrl_var_store;
    }

    SketchFunction *get_harness() {
        return this;
    }

    FloatManager& get_floats()
    {
        return env->get_floats();
    }

    SkValType bool_node_out_type_to_sk_val_type(OutType* out_type)
    {
        assert(out_type == OutType::INT || out_type == OutType::BOOL || OutType::FLOAT);
        if(out_type == OutType::INT)
        {
            return sk_type_int;
        }
        else if(out_type == OutType::BOOL)
        {
            return sk_type_bool;
        }
        else if(out_type == OutType::FLOAT)
        {
            return sk_type_float;
        }
        else
        {
            assert(false);
        }
    }
    vector<SkHoleSpec>* get_holes()
    {
        SketchFunction* inlined_harness = produce_inlined_dag();
        vector<bool_node*>& ctrl_nodes = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        auto* ret = new vector<SkHoleSpec>();
        for(int i = 0;i<ctrl_nodes.size(); i++)
        {
            ret->push_back(
                    SkHoleSpec(
                            ctrl_nodes[i]->get_name(),
                            bool_node_out_type_to_sk_val_type(ctrl_nodes[i]->getOtype())));
        }
        inlined_harness->clear();
        return ret;
    }

    SketchFunction* produce_with_concretized_holes(SolverLanguagePrimitives::SolutionHolder* solution_holder, bool do_deactivate_pcond = false)
    {
        VarStore* solution = solution_holder->to_var_store();
        SketchFunction* ret = produce_concretization(*solution, bool_node::CTRL, do_deactivate_pcond);
        delete solution;
        ret->add_solution(solution_holder);
        return ret;
    }

private:
    SolverLanguagePrimitives::SolutionHolder* solution = nullptr;
    void add_solution(SolverLanguagePrimitives::SolutionHolder* _solution_holder)
    {
        assert(solution == nullptr);
        solution = new SolverLanguagePrimitives::SolutionHolder(_solution_holder);
    }
public:

    SolverLanguagePrimitives::SolutionHolder* get_solution()
    {
        return new SolverLanguagePrimitives::SolutionHolder(solution);
    }

    void concretize(SolverLanguagePrimitives::SolutionHolder* solution_holder, bool do_deactivate_pcond = false)
    {
        VarStore* solution = solution_holder->to_var_store();
        concretize(*solution, bool_node::CTRL, do_deactivate_pcond);
        delete solution;
        add_solution(solution_holder);
    }

    SketchFunction* produce_with_concretized_inputs(SolverLanguagePrimitives::InputHolder* input_holder)
    {
        VarStore* inputs = input_holder->to_var_store();
        SketchFunction* ret = produce_concretization(*inputs, bool_node::SRC);
        delete inputs;
        return ret;
    }

    int count_passing_inputs(File* file) {
        int ret = 0;
        int num_0s = 0;
        int num_1s = 0;
        for(int i = 0;i<file->size();i++)
        {
            SolverLanguagePrimitives::InputHolder input =
                    SolverLanguagePrimitives::InputHolder(file->at(i), env->get_floats());
            SketchFunction* concretized_function = produce_with_concretized_inputs(&input);
            assert(concretized_function->get_dag()->getNodesByType(bool_node::CTRL).size() == 0);
            assert((concretized_function->get_dag()->size() == 0) == (concretized_function->get_dag()->get_failed_assert() == nullptr));
            if(concretized_function->get_dag()->get_failed_assert() == nullptr)
            {
                ret += 1;
            }
            concretized_function->clear();
        }
        cout << "num_1s " << num_1s <<" num_0s "<< num_0s <<" ret "<< ret << endl;
        return ret;
    }

    SketchFunction *produce_replace(const string &replace_this, const string &with_this);

    void replace(const string& replace_this, const string &with_this);

private:
    map<string, SketchFunction*>* it_is_in_this_function_map = nullptr;
public:
    void set_assert__it_is_in_this_function_map(map<string, SketchFunction *>* map);
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
