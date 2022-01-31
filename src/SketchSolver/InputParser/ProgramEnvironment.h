//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_PROGRAMENVIRONMENT_H
#define SKETCH_SOURCE_PROGRAMENVIRONMENT_H

#include "DagFunctionInliner.h"
#include "FunctionMap.h"

class SketchFunction;

void findPureFuns(const map<string, BooleanDAG *> &functionMap, set<string> &pureFuns);

class ProgramEnvironment;

FunctionMap& boolean_dag_map_to_function_map(map<string, BooleanDAG*>& boolean_dag_map, ProgramEnvironment* the_env);

class ProgramEnvironment
{
public:
    CommandLineArgs& params;
    FloatManager& floats;
    HoleHardcoder& hardcoder;
    FunctionMap& function_map;
    int num_inlining_steps;
    map<string, map<string, string> > replaceMap;

    ProgramEnvironment(CommandLineArgs& _params, FloatManager& _floats, HoleHardcoder& _hardcoder,
                       map<string, BooleanDAG*>& boolean_dag_map, int _steps, map<string, map<string, string> >& _replaceMap):
            params(_params), floats(_floats), hardcoder(_hardcoder), replaceMap(std::move(_replaceMap)),
            function_map(boolean_dag_map_to_function_map(boolean_dag_map, this)), num_inlining_steps(_steps)
    {

    }

    void toggle_pcond(BooleanDAG* dag, bool val)
    {
       vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
       for(auto bn : ctrls)
       {
           if(((CTRL_node*)bn)->get_Pcond())
           {
               if (val) {
                   ((CTRL_node *) bn)->activate_pcond();
               } else {
                   ((CTRL_node *) bn)->deactivate_pcond(); }
           }
       }
    }

    void activate_pcond(BooleanDAG* dag)
    {
       toggle_pcond(dag, true);
    }
    void deactivate_pcond(BooleanDAG* dag)
    {
       toggle_pcond(dag, false);
    }


    void doInline(
            BooleanDAG& dag, VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond)
    {
        vector<string>* tmp = nullptr;
        doInline(dag, var_store, var_type, do_deactivate_pcond, tmp);
        delete tmp;
    }

    void doInline(
           BooleanDAG& dag, VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond, vector<string>*& inlined_functions){

        assert(inlined_functions == nullptr);

        if(do_deactivate_pcond){
            deactivate_pcond(&dag);
        }

        //OneCallPerCSiteInliner fin;
        // InlineControl* fin = new OneCallPerCSiteInliner(); //new BoundedCountInliner(PARAMS->boundedCount);
        TheBestInliner fin(num_inlining_steps, params.boundmode == CommandLineArgs::CALLSITE);
        /*
        if(PARAMS->boundedCount > 0){
        fin = new BoundedCountInliner(PARAMS->boundedCount);
        }else{
        fin = new OneCallPerCSiteInliner();
        }
        */

        set<string> pureFuns;

        const map<string, BooleanDAG *>& boolean_dag_function_map = *function_map.to_boolean_dag_map();

        findPureFuns(boolean_dag_function_map, pureFuns);

        DagOneStepInlineAndConcretize dfi(
                var_store,
                var_type,
                dag,
                boolean_dag_function_map,
                std::move(replaceMap),
                floats,
                &hardcoder,
                pureFuns,
                params.randomassign,
                &fin,
                params.onlySpRandAssign,
                params.spRandBias);

        int oldSize = -1;
        bool nofuns = false;
        for (int i = 0; i<num_inlining_steps; ++i) {
            int t = 0;
            int ct = 0;
            do {
                if (params.randomassign && params.onlySpRandAssign) {
                    if (ct < 2) {
                        dfi.turnOffRandomization();
                        ct++;
                    } else {
                        dfi.turnOnRandomization();
                    }
                }

                try {

                    dfi.process(dag);

                }
                catch (BadConcretization) {
                    assert(dfi.get_failedAssert() != nullptr);
                    dag.set_failed_assert(dfi.get_failedAssert());

                    if(do_deactivate_pcond){
                        activate_pcond(&dag);
                    }

                    inlined_functions = dfi.get_inlined_functions();
                    delete &boolean_dag_function_map;

                    return ;
                }
                //
                // dag.repOK();
                set<string>& dones = dfi.getFunsInlined();
                if (params.verbosity> 6) { cout << "inlined " << dfi.nfuns() << " new size =" << dag.size() << endl; }
                //dag.lprint(cout);
                if (params.bndDAG > 0 && dag.size() > params.bndDAG) {
                    cout << "WARNING: Preemptively stopping CEGIS because the graph size exceeds the limit: " << params.bndDAG << endl;
                    exit(1);
                }
                if (oldSize > 0) {
                    if(dag.size() > 400000000 && dag.size() > oldSize * 10){
                        i = num_inlining_steps;
                        cout << "WARNING: Preemptively stopping inlining because the graph was growing too big too fast" << endl;
                        break;
                    }
                    if((dag.size() > 400000 && dag.size() > oldSize * 2)|| dag.size() > 1000000){
                        hardcoder.tryHarder();
                    }
                }
                oldSize = dag.size();
                ++t;
            } while (dfi.changed());
            if (params.verbosity> 6) { cout << "END OF STEP " << i << endl; }
            // fin.ctt.printCtree(cout, dag);

            fin.clear();
            if (t == 1 && params.verbosity> 6) { cout << "Bailing out" << endl; break; }
        }
        hardcoder.afterInline();
        {
            DagFunctionToAssertion makeAssert(dag, boolean_dag_function_map, floats);
            makeAssert.process(dag);
        }


       if(do_deactivate_pcond){
           activate_pcond(&dag);
       }

       inlined_functions = dfi.get_inlined_functions();
        delete &boolean_dag_function_map;
    }

    FloatManager &get_floats() {
        return floats;
    }

    HoleHardcoder &get_hardcoder()
    {
        return hardcoder;
    }
};



/**
* Thought Flow:
* 1. Need to use DagOneStepInlineAndConcretize to concretize dags with a counterexample from checker on the go
* 1.1 Benefits: if you have a conterexample, concretizing while inling should reduce memory consumption due to constant propagation and optimization.
* 1.2 Currently the entire inlined dag is being optimized after concretization.
*
* Actionable:
* Instead of inlining before doing CEGIS. Inline whenever you get a new counterexample using the DagOneStepInlineAndConcretize.
*
* Qs:
* DagOneStepInlineAndConcretize accepts as input all the meta-data of a sketch (look at the spec for initializing DagOneStepInlineAndConcretize)
* Q: should all these parameters be passed to the checker?
* Q: should we rather package them as an inliner, and send the inliner as a parameter, and then initialize the DagOneStepInlineAndConcretize with the inliner?
* Inline seems to be have two nested outer loops around inliner (the for and the do-while loops).
* Q: should we run these outer loops also when we do the inlining on the go?
*  How would that work?
*  How should we structure the code in relation to DagConcretier?
*  Would DagOneStepInlineAndConcretize be instead of the DagInliner in these outer loops or should the outer loops be inside the DagOneStepInlineAndConcretize and we have an one-step-concretizer-inliner as a parameter in the DagOneStepInlineAndConcretize rather than having it inherit from DagInliner.
*/

#endif //SKETCH_SOURCE_PROGRAMENVIRONMENT_H
