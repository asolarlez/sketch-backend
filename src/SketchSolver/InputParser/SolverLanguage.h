//
// Created by kliment on 6/22/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGE_H
#define SKETCH_SOURCE_SOLVERLANGUAGE_H



#include <iostream>
#include <string>
#include <map>
#include <utility>
#include <vector>
#include <cassert>
#include <fstream>

#include "SATSolver.h"
#include "VarStore.h"
#include "CEGISFinder.h"
#include "CEGISChecker.h"
#include "CEGISSolver.h"
#include "NodeHardcoder.h"
#include "CounterexampleFinder.h"
#include "SolverLanguageYaccHeader.h"
#include "SketchFunction.h"
#include "FunctionMapTransformerLanguage.h"


using namespace std;

namespace SolverLanguagePrimitives
{
    class ProblemAE
    {
        File* file;
        string file_name;
        BooleanDagLightUtility *skfunc = nullptr;
    public:
        explicit ProblemAE(BooleanDagLightUtility* _function, File* _file = nullptr):
                skfunc(_function), file(_file){}

        File* get_file()
        {
            return file;
        }

        auto get_harness()
        {
            return skfunc;
        }


        BooleanDAG *get_dag() {
            return skfunc->get_dag();
        }

        virtual string to_string()
        {
            cout << "TODO: ProblemAE::to_string" << endl;
            assert(false);
        }

        const string &get_file_name() {
            return file_name;
        }
    };

    class Solver_AE
    {
    public:
        Solver_AE()= default;

        virtual SL::HoleVarStore * solve(ProblemAE* problem)
        { assert(false); }
        virtual string to_string()
        { assert(false); }
    };

    class WrapperAssertDAG: public Solver_AE
    {
        CommandLineArgs& params;
        FloatManager& floats;
        HoleHardcoder& hardcoder;
        SolverHelper* finder;
        bool hasGoodEnoughSolution;

        ::CEGISSolver* solver;

        ::SATSolver* _pfind;
    public:

        void clear()
        {
            solver->clear();
            delete solver;
            delete _pfind;
            delete finder;
            delete this;
        }

        ::CEGISSolver* get_solver()
        {
            return solver;
        }

        WrapperAssertDAG(FloatManager& _floats, HoleHardcoder& _hardcoder, CommandLineArgs& _params, bool _hasGoodEnoughSolution):
        params(_params), floats(_floats), hardcoder(_hardcoder), hasGoodEnoughSolution(_hasGoodEnoughSolution)
        {
            _pfind = ::SATSolver::solverCreate(params.synthtype, ::SATSolver::FINDER, "WrapperAssertDAG");
            if (params.outputSat) {
                _pfind->outputSAT();
            }
            finder = new SolverHelper(*_pfind);
            finder->setMemo(params.setMemo && params.synthtype == ::SATSolver::MINI);


            CEGISFinderSpec* cegisfind;
            cegisfind = new CEGISFinder(floats, *finder, finder->getMng(), params);
            solver = new ::CEGISSolver(cegisfind, hardcoder, params, floats, _hardcoder);
        }

        SL::HoleVarStore* recordSolution() {
            return new SL::HoleVarStore(solver->ctrlStore);
        }

        SL::HoleVarStore * solve(ProblemAE* problem) override
        {
//            cout << endl;
//            cout << "ENTERING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;
            solver->addProblem(problem->get_harness(), problem->get_file());

            SATSolverResult ret_result = SAT_UNDETERMINED;
            SL::HoleVarStore* holes_to_sk_val = nullptr;
            //copied from InterpreterEnviroment::assertDAG
            {
                bool ret_result_determined = false;
                int solveCode = 0;
                try {

                    solveCode = solver->solve();
                    if (solveCode || !hasGoodEnoughSolution) {
                        holes_to_sk_val = recordSolution();
                    }
                }
                catch (SolverException *ex) {
//                    needs InterpreterEnviroment::basename
//                    cout << "ERROR " << basename() << ": " << ex->code << "  " << ex->msg << endl;
                    ret_result = ex->code;
                    ret_result_determined = true;
                }
                catch (BasicError &be) {
                    if (!hasGoodEnoughSolution) {
                        holes_to_sk_val = recordSolution();
                    }
//                    needs InterpreterEnviroment::basename
//                    cout << "ERROR: " << basename() << endl;
                    ret_result = SAT_ABORTED;
                    ret_result_determined = true;
                }

                if (!ret_result_determined)
                {
                    if (!solveCode) {
                        ret_result = SAT_UNSATISFIABLE;
                    }
                    else {
                        ret_result = SAT_SATISFIABLE;
                    }
                }
            }

            VarStore* tmp_var_store = new VarStore(*holes_to_sk_val);
            auto tmp_dag = problem->get_harness()->produce_concretization(tmp_var_store, bool_node::CTRL);
            tmp_var_store->clear();
            tmp_dag->increment_shared_ptr();
            int num_passing_inputs = tmp_dag->count_passing_inputs(problem->get_file());
            if(ret_result == SAT_SATISFIABLE)
            {
                assert(num_passing_inputs == problem->get_file()->size());
            }
            else
            {
                assert(num_passing_inputs < problem->get_file()->size());
            }
            tmp_dag->clear();

//            HoleAssignment* ret = new HoleAssignment(ret_result, holes_to_sk_val);
            SL::HoleVarStore * ret = holes_to_sk_val;

//            cout << "EXITING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;
//            cout << "failing assert: " << problem->get_harness()->produce_concretization(*holes_to_sk_val->to_var_store(false), bool_node::CTRL)->get_dag()->get_failed_assert() << endl;
//            cout << "returns " << ret->to_string() << endl << endl;


            assert(problem->get_harness()->get_dag()->getNodesByType(bool_node::UFUN).empty());
            if(!problem->get_harness()->get_dag()->getNodesByType(bool_node::CTRL).empty())
            {
                auto tmp_local_var_store = new VarStore(*ret);//;->to_var_store(false);
                auto tmp = problem->get_harness()->produce_concretization(tmp_local_var_store, bool_node::CTRL);
                tmp_local_var_store->clear();
                assert(tmp->get_dag()->getNodesByType(bool_node::UFUN).empty());
                assert(tmp->get_dag()->getNodesByType(bool_node::CTRL).empty());
                tmp->increment_shared_ptr();
                tmp->clear();
            }

            return ret;
        }
    };
};

class SolverLanguage {
public:
    SolverLanguage()
    {

    }
    void eval(string solver_program_file_name, FunctionMap &function_map, const string& file_name, FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc,
         bool hasGoodEnoughSolution)
    {
        SolverProgramState state_abs =
                SolverProgramState(function_map, file_name, floats, _args, _hc, hasGoodEnoughSolution);

        SolverProgramState* state = &state_abs;

        {

            assert(!state->function_map.empty());

            int init_num_global_dags = BooleanDAG::get_allocated().size();
            int init_num_global_nodes = bool_node::get_allocated().size();

            BooleanDagLightUtility* local_harness = ((BooleanDagUtility*)state->function_map["sketch_main__Wrapper"])->clone();
            local_harness->increment_shared_ptr();

            FunctionMap& function_map = local_harness->get_env()->function_map;

            int init_function_map_transformer_size = function_map.transformer_size();

            parse_solver_langauge_program(state, solver_program_file_name);

            SL::VarVal *var_val_ret = state->eval();

            if(var_val_ret->is_solution_holder()) {
                AssertDebug(false, "TODO: add printing of FMTL program.")
                const SL::HoleVarStore * solution_holder = var_val_ret->get_solution(false);

                delete var_val_ret;
                state->clear();

                local_harness->concretize_this_dag(solution_holder, bool_node::CTRL);

                File *file = new File(local_harness, file_name, state->floats, state->args.seed);

                int num_passing_inputs =
                        local_harness->count_passing_inputs(file);

                cout << "HERE " << local_harness->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                file->clear();

                local_harness->clear();

                assert(BooleanDAG::get_allocated().size() - init_num_global_dags == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);


                assert(init_function_map_transformer_size == function_map.transformer_size());
            }
            else
            {
                local_harness->clear();

                assert(var_val_ret->is_sketch_function());

                SketchFunction* concretized_function = var_val_ret->get_function(false);

                File* file = new File(concretized_function, file_name, state->floats, state->args.seed);

                int num_passing_inputs =
                        concretized_function->count_passing_inputs(file);

                cout << "HERE " << concretized_function->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                file->clear();

                //print function_map_transformer_program, parse it, and check that it's the same.

                FMTL::FunctionMapTransformerState* fmtl_state = nullptr;
                {
                    string fmtl_program_str = concretized_function->get_rep()->pretty_print(function_map);
                    cout << "pretty_print FMTL program:" << endl;
                    cout << fmtl_program_str << endl;
                    cout << endl;

                    const string fmtl_program_file_name = "fmtl_program_file.fmtl";

                    ofstream fmtl_program_file(fmtl_program_file_name);

                    fmtl_program_file << fmtl_program_str;

                    fmtl_program_file.close();

                    fmtl_state = new FMTL::FunctionMapTransformerState();

                    FMTL::parse_function_map_transformer_program(fmtl_state, fmtl_program_file_name);

                }

                var_val_ret->clear_assert_0_shared_ptrs();
                state->clear();

                {

                    function_map.print();

                    function_map.clear_erased_root_dag_reps();

                    fmtl_state->eval(&function_map);

                    assert(false);
                }

                int dags_diff = BooleanDAG::get_allocated().size() - init_num_global_dags;
                int all_remaining_inlining_trees = LightSkFuncSetter::all_inlining_trees.size();
                assert(all_remaining_inlining_trees == 0);

                assert(dags_diff == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

                int transformer_size_diff = function_map.transformer_size() - init_function_map_transformer_size;

                function_map.check_consistency();
                assert(function_map.contains_only_necessary());

                cout << "LightSkFuncSetter::max_count: " << LightSkFuncSetter::max_count <<endl;
                assert(LightSkFuncSetter::max_count <= 3);

//                if(transformer_size_diff != 0){
//                    function_map.print_not_erased();
//                }

                function_map.soft_clear_transformer();
            }
        }
    }
};

#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
