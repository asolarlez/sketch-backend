//
// Created by kliment on 3/20/22.
//

#include "SolverLanguagePrimitives.h"
#include "GenericFile.h"
#include "File.h"
#include "BenchmarkScore.h"

//#define CHECK_SOLVE

CEGISSolverResult SolverLanguagePrimitives::WrapperAssertDAG::solve(
        SolverLanguagePrimitives::ProblemAE *problem, const long long _budget,
        const long long timeout_per_find_in_microseconds)
{
//            cout << endl;
//            cout << "ENTERING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;

    const File* file = nullptr;

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

    bool ret_result_determined = false;
    bool solveCode = false;
    CEGISSolverResult solver_result_object;
    try {
        auto start_solver = std::chrono::steady_clock::now();

        // THIS IS THE KEY LINE !!!
        solver_result_object = solver->solve(_budget, timeout_per_find_in_microseconds, problem->get_validation_file(false));
        solveCode = solver_result_object.success;
//            timestamp(start_solver, "cegis__dag_"+std::to_string(problem->get_dag()->get_dag_id_from_the_user())+"__n"+std::to_string(problem->get_dag()->size()));
        timestamp(start_solver, "cegis__dag__n"+std::to_string(problem->get_dag()->size()));

        holes_to_sk_val = recordSolution();
    }
    catch (SolverException *ex) {
        ret_result = ex->code;
        ret_result_determined = true;
    }
    catch (BasicError &be) {
        holes_to_sk_val = recordSolution();
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

//    assert(problem->get_harness()->get_dag()->getNodesByType(bool_node::UFUN).empty());
    if(!problem->get_harness()->get_dag()->getNodesByType(bool_node::CTRL).empty())
    {
        auto tmp_local_var_store = new VarStore(*ret);
        auto tmp = problem->get_harness()->produce_concretization(tmp_local_var_store, bool_node::CTRL);
        tmp_local_var_store->clear();
//        assert(tmp->get_dag()->getNodesByType(bool_node::UFUN).empty());
        assert(tmp->get_dag()->getNodesByType(bool_node::CTRL).empty());
        tmp->increment_shared_ptr();
        tmp->clear();
    }

    return CEGISSolverResult{solveCode, ret, solver_result_object.intermediate_solutions};
}

#include "SketchFunction.h"

void set_inlining_tree(VarStore* sol, const BooleanDagUtility* harness){
    assert(sol->get_inlining_tree() == nullptr);

    VarStore* append_sol = harness->get_inlining_tree(true)->get_solution();
    LightInliningTree* harness_inlining_tree = new LightInliningTree(harness->get_inlining_tree());
    harness_inlining_tree->set_var_store(sol);
    sol->disjoint_join_with(*append_sol);
    sol->set_inlining_tree(harness_inlining_tree);
}


CEGISSolverResult SolverLanguagePrimitives::WrapperBatchEvaluatorSolver::solve(
        SolverLanguagePrimitives::ProblemAE *problem, const long long _budget, const long long timeout_per_find_in_microseconds)
{
    AssertDebug(false, "integrate _budget");
//            cout << endl;
//            cout << "ENTERING WrapperBatchEvaluatorSolver->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;

    const File* file = nullptr;

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
    CEGISSolverResult solver_result;
    //copied from InterpreterEnviroment::assertDAG

    bool ret_result_determined = false;
    bool solveCode = false;
    try {
        auto start_solver = std::chrono::steady_clock::now();
        solver_result = solver->solve(numeric_limits<long long>::max(), timeout_per_find_in_microseconds);
        solveCode = solver_result.success;
//            timestamp(
//                    start_solver,
//                    "cegiswenum__dag_"+std::to_string(problem->get_dag()->get_dag_id_from_the_user())+"__n"+
//                    std::to_string(problem->get_dag()->size()));
        timestamp(
                start_solver,
                "cegiswenum__dagsize_"+std::to_string(problem->get_dag()->size())
        );

        holes_to_sk_val = recordSolution();
    }
    catch (SolverException *ex) {
//                    needs InterpreterEnviroment::basename
//                    cout << "ERROR " << basename() << ": " << ex->code << "  " << ex->msg << endl;
        ret_result = ex->code;
        ret_result_determined = true;
    }
    catch (BasicError &be) {
        holes_to_sk_val = recordSolution();

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


    holes_to_sk_val->relabel(problem->get_harness()->get_dag()->get_CTRL_nodes());

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

    return CEGISSolverResult{solveCode, ret, solver_result.intermediate_solutions};
}

CEGISSolverResult SolverLanguagePrimitives::solve(
        const SketchFunction *skfunc, const File *training_file,
        const long long _budget, float _timeout_per_find,
        const File *validation_file, const int* seed)
{
    vector<string> prev_holes = skfunc->get_deep_holes();
    SketchFunction* harness = skfunc->deep_exact_clone_and_fresh_function_map();
    harness->increment_shared_ptr();

    assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

    harness->inline_this_dag(false);

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant()); // debug code

    vector<string> after_holes = harness->get_deep_holes();
    sort(prev_holes.begin(), prev_holes.end());
    sort(after_holes.begin(), after_holes.end());
//            assert(prev_holes.size() == after_holes.size());
    assert(after_holes.size() == harness->get_dag()->getNodesByType(bool_node::CTRL).size());

    using namespace SolverLanguagePrimitives;
    auto* solver = new WrapperAssertDAG(skfunc->get_env(), seed);

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

    long long timeout_per_find_in_microseconds = numeric_limits<long long>::max();
    {
        float timeout_per_find_in_seconds = _timeout_per_find;
        AssertDebug(timeout_per_find_in_seconds >= 0,
                    "timeout must be positive float >= 0.000001 (in seconds).");
        const double cant_set_timeout_below = 0.000001;
        if (timeout_per_find_in_seconds >= cant_set_timeout_below) { // if more than a microsecond.
            timeout_per_find_in_microseconds = (long long) (
                    ((double) timeout_per_find_in_seconds) * 1000000.0);
        } else if (timeout_per_find_in_seconds >= 0) {
            AssertDebug(false, "cant set timeout below " + std::to_string(cant_set_timeout_below));
        } else {
            assert(timeout_per_find_in_seconds == -1);
        }
    }

    auto* problem = new ProblemAE(harness, training_file, validation_file);

    CEGISSolverResult result_object = (solver)->solve(problem, _budget, timeout_per_find_in_microseconds);
    HoleVarStore * sol = result_object.final_ctrl_var_store;
    {
        //invariant;
        assert(sol->size() == after_holes.size());
        for (const auto &hole_name: after_holes) {
            assert(sol->contains(hole_name));
        }
        for (const auto &obj: sol->get_objs()) {
            assert(sol->contains(obj.get_name()));
        }
    }

    set_inlining_tree(sol, harness);

    bool concretize_after_solving = true;
    if(concretize_after_solving) {
        //make sure produce_concretize-s
        const SketchFunction *to_test = skfunc->produce_concretization(sol, bool_node::CTRL);
        to_test->increment_shared_ptr();
        to_test->clear();
    }

    for(int i = 0; i < result_object.intermediate_solutions.size();i++)
    {
        HoleVarStore * sol = result_object.intermediate_solutions[i];
        assert(sol->size() == after_holes.size());
        for(const auto& hole_name : after_holes) {
            assert(sol->contains(hole_name));
        }
        for(const auto& obj : sol->get_objs()) {
            assert(sol->contains(obj.get_name()));
        }
        auto tmp_harness = skfunc->deep_clone();
        tmp_harness->increment_shared_ptr();
        auto tmp_dag = tmp_harness->produce_executable();
        tmp_dag->increment_shared_ptr();
        sol->relabel(tmp_dag->get_dag()->get_CTRL_nodes());
        set_inlining_tree(sol, tmp_dag);

        if(concretize_after_solving) {
            //make sure produce_concretize-s
            auto to_test__tmp_harness = skfunc->deep_clone();
            to_test__tmp_harness->increment_shared_ptr();
            SketchFunction *to_test = to_test__tmp_harness->produce_concretization(sol, bool_node::CTRL);
            to_test->increment_shared_ptr();
            to_test->clear();
            to_test__tmp_harness->clear();
        }
        tmp_dag->clear();
        tmp_harness->clear();
    }


    harness->clear();
    solver->clear();
    delete problem;

    return result_object;
}
