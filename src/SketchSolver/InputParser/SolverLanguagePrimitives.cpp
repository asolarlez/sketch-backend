//
// Created by kliment on 3/20/22.
//

#include "SolverLanguagePrimitives.h"
#include "GenericFile.h"
#include "File.h"

//#define CHECK_SOLVE

HoleVarStore *SolverLanguagePrimitives::WrapperAssertDAG::solve(SolverLanguagePrimitives::ProblemAE *problem)
{
//            cout << endl;
//            cout << "ENTERING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;

    File* file = nullptr;

#ifdef USE_GENERIC_FILE
        File *predicted_file = new File(problem->get_harness(), problem->get_generic_file(), floats, params.seed);
        file = predicted_file;
#else
        file = problem->get_file();
#endif

    assert(file != nullptr);

    solver->addProblem(problem->get_harness(), file);

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

#ifdef CHECK_SOLVE
    {
        VarStore *tmp_var_store = new VarStore(*holes_to_sk_val);
        auto tmp_dag =
                problem->get_harness()->produce_concretization(tmp_var_store, bool_node::CTRL);
        tmp_var_store->clear();
        tmp_dag->increment_shared_ptr();

        int num_passing_inputs = tmp_dag->count_passing_inputs(file);
        if (ret_result == SAT_SATISFIABLE) {
            assert(num_passing_inputs == file->size());
        } else {
            assert(num_passing_inputs < file->size());
        }
        tmp_dag->clear();
    }
#endif

    HoleVarStore * ret = holes_to_sk_val;

    assert(problem->get_harness()->get_dag()->getNodesByType(bool_node::UFUN).empty());
    if(!problem->get_harness()->get_dag()->getNodesByType(bool_node::CTRL).empty())
    {
        auto tmp_local_var_store = new VarStore(*ret);
        auto tmp = problem->get_harness()->produce_concretization(tmp_local_var_store, bool_node::CTRL);
        tmp_local_var_store->clear();
        assert(tmp->get_dag()->getNodesByType(bool_node::UFUN).empty());
        assert(tmp->get_dag()->getNodesByType(bool_node::CTRL).empty());
        tmp->increment_shared_ptr();
        tmp->clear();
    }

    return ret;
}