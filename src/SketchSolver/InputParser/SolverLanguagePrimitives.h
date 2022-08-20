//
// Created by kliment on 3/20/22.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H
#define SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H

#include <string>
#include "BooleanDagLightUtility.h"
#include "CEGISSolver.h"
#include "CEGISFinderBatchEnumeration.h"

class GenericFile;
class File;

namespace SolverLanguagePrimitives {

    class ProblemAE {
        GenericFile *generic_file = nullptr;
        File *file = nullptr;
        string file_name;
        BooleanDagLightUtility *skfunc = nullptr;
    public:
        explicit ProblemAE(BooleanDagLightUtility *_function, GenericFile *_generic_file = nullptr) :
                skfunc(_function), generic_file(_generic_file) {}

        explicit ProblemAE(BooleanDagLightUtility *_function, File *_file = nullptr) :
                skfunc(_function), file(_file) {}

        GenericFile *get_generic_file() {
            assert(generic_file != nullptr);
            return generic_file;
        }

        File *get_file() {
            assert(file != nullptr);
            return file;
        }

        auto get_harness() {
            return skfunc;
        }


        BooleanDAG *get_dag() {
            return skfunc->get_dag();
        }

        virtual string to_string() {
            cout << "TODO: ProblemAE::to_string" << endl;
            assert(false);
        }

        const string &get_file_name() {
            return file_name;
        }
    };

    class Solver_AE {
    public:
        Solver_AE() = default;

        virtual HoleVarStore *solve(ProblemAE *problem, const unsigned long long find_solve_max_timeout_in_microseconds) { assert(false); }

        virtual string to_string() { assert(false); }
    };

    class WrapperAssertDAG : public Solver_AE {
        ProgramEnvironment* env;

        SolverHelper *finder;

        ::CEGISSolver *solver;

        ::SATSolver *_pfind;
    public:

        void clear() {
            solver->clear();
            delete solver;
            delete _pfind;
            delete finder;
            delete this;
        }

        ::CEGISSolver *get_solver() {
            return solver;
        }

        WrapperAssertDAG(ProgramEnvironment* _env) :
                env(_env){
            _pfind = ::SATSolver::solverCreate(env->params.synthtype, ::SATSolver::FINDER, "WrapperAssertDAG");
            if (env->params.outputSat) {
                _pfind->outputSAT();
            }
            finder = new SolverHelper(*_pfind);
            finder->setMemo(env->params.setMemo && env->params.synthtype == ::SATSolver::MINI);


            CEGISFinderSpec *cegisfind;
            cegisfind = new CEGISFinder(env->floats, *finder, finder->getMng(), env->params);
            solver = new ::CEGISSolver(cegisfind, env->params, env->floats, env->hardcoder);
        }

        HoleVarStore *recordSolution() {
            return new HoleVarStore(solver->ctrlStore);
        }

        HoleVarStore *solve(ProblemAE *problem, const unsigned long long find_solve_max_timeout_in_microseconds) override;
    };

    class WrapperBatchEvaluatorSolver : public Solver_AE {
        CommandLineArgs &params;
        FloatManager &floats;
        HoleHardcoder &hardcoder;

        ::CEGISSolver *solver;

    public:

        void clear() {
            solver->clear();
            delete solver;
            delete this;
        }

        ::CEGISSolver *get_solver() {
            return solver;
        }


        WrapperBatchEvaluatorSolver(FloatManager &_floats, HoleHardcoder &_hardcoder, CommandLineArgs &_params) :
        params (_params), floats(_floats), hardcoder(_hardcoder) {
            CEGISFinderSpec *cegisfind;
            cegisfind = new CEGISFinderBatchEnumeration(floats, params);
            solver = new ::CEGISSolver(cegisfind, params, floats, hardcoder);
        }

        HoleVarStore *recordSolution() {
            return new HoleVarStore(solver->ctrlStore);
        }

        HoleVarStore *solve(ProblemAE *problem, unsigned long long find_solve_max_timeout_in_microseconds) override;
    };

    HoleVarStore* solve(SketchFunction* skfunc, File* subset_file, float timeout);
};


void set_inlining_tree(VarStore* sol, BooleanDagUtility* harness);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEPRIMITIVES_H
