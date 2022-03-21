//
// Created by kliment on 3/20/22.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H
#define SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H

#include <string>
#include "BooleanDagLightUtility.h"
#include "CEGISSolver.h"

class File;

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

        virtual HoleVarStore * solve(ProblemAE* problem)
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

        HoleVarStore* recordSolution() {
            return new HoleVarStore(solver->ctrlStore);
        }

        HoleVarStore * solve(ProblemAE* problem) override
        {
//            cout << endl;
//            cout << "ENTERING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;
            solver->addProblem(problem->get_harness(), problem->get_file());

            SATSolverResult ret_result = SAT_UNDETERMINED;
            HoleVarStore* holes_to_sk_val = nullptr;
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
            HoleVarStore * ret = holes_to_sk_val;

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


#endif //SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H
