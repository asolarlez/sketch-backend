//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_HARNESS_H
#define SKETCH_SOURCE_HARNESS_H

//#include "BooleanDAG.h"
//#include "ProgramEnvironment.h"

#include "BooleanNodes.h"
#include "VarStore.h"

class ProgramEnvironment;
class BooleanDAG;

class Harness
{
    //unrolled dag is the the original unrolled dag using prepare miter at before calling assertDAG
    BooleanDAG* original_dag;
    //root dag is
    // IF NOT CONCRETIZED: the root harness dag
    // IF CONCRETIZED: a fully unrolled concretized dag using the doInline from env.
    BooleanDAG* root_dag;

    //if env == nullptr => original_dag and root_dag ARE NOT concretized
    //if env != nullptr => original_dag and root_dag ARE concretized
    ProgramEnvironment* env;

    bool new_way = true;
    bool keep_track_of_original = false;
public:
    Harness(
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

    Harness* produce_inlined_dag(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, deactivate_pcond, true);
    }

    Harness* do_inline(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, deactivate_pcond, false);
    }


    Harness* concretize(VarStore& var_store, bool_node::Type var_type, bool deactivate_pcond = false)
    {
        return produce_concretization(var_store, var_type, deactivate_pcond, false);
    }

    Harness* produce_concretization(VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond = false, bool do_clone = true);

    Harness *clone() ;

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

private:
    string name;
public:

    void set_name(const string _name) {
        name = _name;
    }
    string get_name()
    {
        return name;
    }
};

#endif //SKETCH_SOURCE_HARNESS_H
