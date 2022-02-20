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

#include "SkVal.h"
#include "BooleanDAG.h"
#include "SATSolver.h"
#include "VarStore.h"
#include "CEGISFinder.h"
#include "CEGISChecker.h"
#include "CEGISSolver.h"
#include "NodeHardcoder.h"
#include "CounterexampleFinder.h"
#include "SolverLanguageYaccHeader.h"
#include "SketchFunction.h"

using namespace std;

namespace SolverLanguagePrimitives
{
    enum ValType {type_state, type_int, type_bool, type_solver_e, type_solver_ae, type_problem_ae, type_problem_e, type_solution_holder, type_input_holder};

    class Val
    {
    private:
        ValType val_type;
    public:
        explicit Val(ValType _val_type): val_type(_val_type){}
        explicit Val(Val* _to_copy): val_type(_to_copy->val_type) {};
        ValType get_type() {
            return val_type;
        }
        virtual string to_string()
        {
            assert(false);
        }
    };

    class ValInt: public Val, public PolyVal<int>{
    public:
        explicit ValInt(int _val): PolyVal<int>(_val), Val(type_int){}
        string to_string() override {return std::to_string(get());}
    };

    class ValBool: public Val, public PolyVal<bool>{
    public:
        explicit ValBool(bool _val): PolyVal<bool>(_val), Val(type_bool){}
        string to_string() override {return std::to_string(get());}
    };

    class InputAssignment;

    class ProblemAE;

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

        auto get_holes()
        {
            return skfunc->get_holes();
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

    class ProblemE: public ProblemAE
    {
//        vector<InputAssignment*> concrete_inputs;
    public:
        explicit ProblemE(SketchFunction* f): ProblemAE(f) {}
        string to_string() override
        {
            cout << "TODO: ProblemE::to_string" << endl;
            assert(false);
            return "ProblemE";
        }

    };

    class ProblemA: public ProblemAE
    {
//        HoleAssignment* solution_holder;
    public:
        string to_string() override
        {
            cout << "TODO: ProblemA::to_string" << endl;
            assert(false);
            return "ProblemE";
        }
    };

    class ValProblemE: public Val, public PolyVal<ProblemE*>
    {
    public:
        explicit ValProblemE(ProblemE* _val): PolyVal<ProblemE*>(_val), Val(type_problem_e){}
        string to_string() override {return get()->to_string();}
    };

    class ValProblemAE: public Val, public PolyVal<ProblemAE*>
    {
    public:
        explicit ValProblemAE(ProblemAE* _val): PolyVal<ProblemAE*>(_val), Val(type_problem_ae){}
        string to_string() override {return get()->to_string();}
    };

    class ValSolutionHolder: public Val, public PolyVal<HoleAssignment*>
    {
    public:
        explicit ValSolutionHolder(HoleAssignment* _val): PolyVal<HoleAssignment*>(_val), Val(type_solution_holder){}
        string to_string() override {return get()->to_string();}
    };

    class ValInputHolder: public Val, public PolyVal<InputAssignment*>
    {
    public:
        explicit ValInputHolder(InputAssignment* _val): PolyVal<InputAssignment*>(_val), Val(type_input_holder){}
        string to_string() override {return get()->to_string();}
    };

    class Solver_E
    {
    public:
        Solver_E()= default;
        virtual HoleAssignment* solve(ProblemE* problem)
        { assert(false); }
        virtual string to_string()
        { assert(false); }
    };

    class Solver_AE
    {
    public:
        Solver_AE()= default;
        virtual HoleAssignment* solve(ProblemAE* problem)
        { assert(false); }
        virtual string to_string()
        { assert(false); }
    };

    class WrapperCEGISFinderSpec: public Solver_E
    {
        CEGISFinderSpec* finder;

        void declareControl(CTRL_node* cnode, VarStore& ctrlStore){
            const string& cname = cnode->get_name();
            int size;

            if(cnode->getOtype() != OutType::FLOAT)
            {
                size = cnode->get_nbits();
            }
            else
            {
                size = 18;
            }
            Dout(cout<<"DECLARING CONTROL "<<cname<<" "<<size<<endl);
            finder->declareControl(cnode);

            ctrlStore.newVar(cname, size, cnode->getOtype(), bool_node::CTRL, cnode->get_original_name(), cnode->get_source_dag_name());

        }
        void add_problem(BooleanDAG* problem, VarStore& ctrlStore)
        {
            {
                auto problemIn = problem->getNodesByType(bool_node::CTRL);
                for(int i=0; i<problemIn.size(); ++i){
                    CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);
                    if(!ctrlnode->get_Angelic() /*&& ctrlnode->getOtype() != OutType::FLOAT*/){
                        declareControl(ctrlnode, ctrlStore);
                    }
                }
            }

            finder->updateCtrlVarStore(ctrlStore);
        }
    public:
        explicit WrapperCEGISFinderSpec(CEGISFinderSpec* _finder): finder(_finder), Solver_E() {}

        HoleAssignment* solve(ProblemE* problem) override
        {
            VarStore ctrlStore;
            add_problem(problem->get_dag(), ctrlStore);
            bool found = finder->find(problem->get_dag(), ctrlStore, true);
            SATSolverResult ret;
            if(found)
            {
                ret = SAT_SATISFIABLE;
                return new HoleAssignment(ret, &ctrlStore, finder->get_floats());
            }
            else
            {
                ret = SAT_UNSATISFIABLE;
                return new HoleAssignment(ret);
            }
        }
        string to_string() override
        {
            return "SATSolver";
        }
    };

    class RandomSolver: public Solver_AE
    {
    public:
        RandomSolver(): Solver_AE() {}
        HoleAssignment* solve(ProblemAE* problem) override
        {
            vector<SkHoleSpec>* hole_names = problem->get_holes();
            auto* ret = new HoleAssignment();

            cout << "TODO: RandomSolver::solve" << endl;
            assert(false);
        }
        string to_string() override
        {
            return "RandomSolver";
        }
    };

    class ConstantSolver: public Solver_AE
    {
    public:
        ConstantSolver(): Solver_AE() {}

        SkVal* get_const_val(SkValType sk_val_type)
        {
            SkVal* ret;
            switch (sk_val_type)
            {
                case sk_type_int:
                    ret = new SkValInt(0, 1);
                    break;
                case sk_type_float:
                    ret = new SkValFloat(0.0, 1);
                    break;
                case sk_type_bool:
                    ret = new SkValBool(false);
                    break;
                default:
                    assert(false);
            }
            return ret;
        }

        HoleAssignment* solve(ProblemAE* problem) override
        {
            vector<SkHoleSpec>* blank_solution = problem->get_holes();
            auto* ret = new Assignment_SkVal(bool_node::CTRL);

            for(int i = 0;i<blank_solution->size();i++)
            {
                SkHoleSpec sk_hole_spec = blank_solution->at(i);
                ret->set(sk_hole_spec.get_name(), get_const_val(sk_hole_spec.get_type()));
            }

            return new HoleAssignment(SAT_SATISFIABLE, ret);
        }
        string to_string() override
        {
            return "ConstantSolver";
        }
    };

    class ValSolverE: public Val, public PolyVal<Solver_E*>
    {
    public:
        explicit ValSolverE(Solver_E* _val): PolyVal<Solver_E*>(_val), Val(type_solver_e){}
        string to_string() override {return get()->to_string();}
    };

    class ValSolverAE: public Val, public PolyVal<Solver_AE*>
    {
    public:
        explicit ValSolverAE(Solver_AE* _val): PolyVal<Solver_AE*>(_val), Val(type_solver_ae){}
        string to_string() override {return get()->to_string();}
    };

    typedef Mapping<Val> State;

    class ValState: public Val, public PolyVal<State*>
    {
    public:
        explicit ValState(State* _val): PolyVal<State*>(_val), Val(type_state){}
        string to_string() override {return get()->to_string();}
    };

    class RetNode
    {
    public:
        explicit RetNode() = default;
        virtual Val* eval(ValState* state){ assert(false); }
    };

    class StateNode: public RetNode
    {
    public:
        explicit StateNode() : RetNode() {}
        ValState* eval(ValState* state) override
        {
            assert(false);
        }
    };

    class StateSequence: public StateNode
    {
        vector<StateNode*> sequence;
    public:
        StateSequence(
                StateNode* i0, StateNode* i1,
                StateNode* i2 = NULL, StateNode* i3 = NULL, StateNode* i4 = NULL,
                StateNode* i5 = NULL, StateNode* i6 = NULL, StateNode* i7 = NULL): StateNode(){
            vector<StateNode*> tmp_sequence;
            tmp_sequence.push_back(i0);
            tmp_sequence.push_back(i1);
            tmp_sequence.push_back(i2);
            tmp_sequence.push_back(i3);
            tmp_sequence.push_back(i4);
            tmp_sequence.push_back(i5);
            tmp_sequence.push_back(i6);
            tmp_sequence.push_back(i7);
            for(int i = 0;i<tmp_sequence.size();i++)
            {
                if(tmp_sequence[i] != NULL)
                {
                    sequence.push_back(tmp_sequence[i]);
                }
            }
        }
        ValState* eval(ValState* state) override
        {
            for(int i = 0;i<sequence.size();i++)
            {
                assert(sequence[i] != NULL);
                state = sequence[i]->eval(state);
            }
            return state;
        }
    };

    class StateAssignment: public StateNode
    {
        string var_name;
        RetNode* val;
    public:
        StateAssignment(string _var_name, RetNode* _val):
                var_name(std::move(_var_name)), val(_val), StateNode(){}
        ValState* eval(ValState* state) override
        {
            state->get()->set(var_name, val->eval(state));
            return state;
        }
    };

    class BoolNode: public RetNode{
    public:
        enum BoolNodeType{bool_val, binary_bool_node, not_node, comparison_node, non_null_node};
    private:
        BoolNodeType bool_node_type;
    public:
        explicit BoolNode(BoolNodeType _bool_node_type):
                bool_node_type(_bool_node_type), RetNode(){}
        ValBool* eval(ValState* state) override { assert(false); }
    };

    class IntNode: public RetNode
    {
    public:
        explicit IntNode(): RetNode(){}
        ValInt* eval(ValState* state) override{ assert(false); }
    };

    class SolverENode: public RetNode
    {
    public:
        explicit SolverENode(): RetNode() {}
        ValSolverE* eval(ValState* state) override { assert(false); }
    };

    class SolverAENode: public RetNode
    {
    public:
        explicit SolverAENode(): RetNode() {}
        ValSolverAE* eval(ValState* state) override { assert(false); }
    };

    class ProblemENode: public RetNode
    {
    public:
        explicit ProblemENode(): RetNode(){}
        ValProblemE* eval(ValState* state) override{ assert(false); }
    };

    class ProblemAENode: public RetNode
    {
    public:
        explicit ProblemAENode(): RetNode(){}
        ValProblemAE* eval(ValState* state) override{ assert(false); }
    };


    class SolutionHolderNode: public RetNode
    {
    public:
        explicit SolutionHolderNode(): RetNode(){}
        ValSolutionHolder* eval(ValState* state) override { assert(false); }
    };

    class SolutionHolderConstructor: public SolutionHolderNode
    {
        ProblemAENode* problem_node;
    public:
        explicit SolutionHolderConstructor(ProblemAENode* _problem_node):
                problem_node(_problem_node), SolutionHolderNode() { }
        ValSolutionHolder* eval(ValState* state) override {
            return new ValSolutionHolder(new HoleAssignment(problem_node->eval(state)->get()));
        }
    };

    class IntBinaryOp: public IntNode
    {
    public:
        enum Op{plus, minus};

    private:
        Op op;
        IntNode* left;
        IntNode* right;
    public:
        IntBinaryOp(IntNode* _left, IntNode* _right, Op _op):
                left(_left), right(_right), op(_op), IntNode() {}
        ValInt* eval(ValState* state) override
        {
            int ret;
            int left_int = left->eval(state)->get();
            int right_int = right->eval(state)->get();
            switch (op) {
                case plus:
                    ret = left_int+right_int;
                    break;
                case minus:
                    ret = left_int-right_int;
                    break;
                default:
                    assert(false);
            }
            return new ValInt(ret);
        }
    };

    class BoolComparison: public BoolNode
    {
    public:
        enum Op{lt, lte, eq, neq, gt, gte};

    private:
        Op op;
        IntNode* left;
        IntNode* right;
    public:
        BoolComparison(IntNode* _left, IntNode* _right, Op _op):
                left(_left), right(_right), op(_op), BoolNode(BoolNode::comparison_node) {}
    public:
        ValBool* eval(ValState* state) override
        {
            bool ret;
            int left_int = left->eval(state)->get();
            int right_int = right->eval(state)->get();
            switch (op) {
                case lt:
                    ret = left_int < right_int;
                    break;
                case lte:
                    ret = left_int <=right_int;
                    break;
                case eq:
                    ret = left_int == right_int;
                    break;
                case neq:
                    ret = left_int != right_int;
                    break;
                case gt:
                    ret = left_int > right_int;
                    break;
                case gte:
                    ret = left_int >= right_int;
                    break;
                default:
                    assert(false);
            }
            return new ValBool(ret);
        }
    };

    class StateWhile: public StateNode
    {
        BoolNode* cond;
        StateNode* body;
    public:
        StateWhile(BoolNode* _cond, StateNode* _body): cond(_cond), body(_body), StateNode() {}
        ValState* eval(ValState* state) override
        {
            while(cond->eval(state)->get())
            {
                state = body->eval(state);
            }
            return state;
        }
    };

    class StateIf: public StateNode
    {
        BoolNode* cond;
        StateNode* if_true_body;
        StateNode* if_false_body = NULL;
    public:
        StateIf(BoolNode* _cond, StateNode* _if_true_body):
                cond(_cond), if_true_body(_if_true_body), StateNode() {}
        ValState* eval(ValState* state) override
        {
            if(cond->eval(state)->get())
            {
                return if_true_body->eval(state);
            }
            else if(if_false_body != NULL)
            {
                return if_false_body->eval(state);
            }
            else
            {
                return state;
            }
        }
    };

    class InputHolderNode: public RetNode
    {
    public:
        InputHolderNode(): RetNode(){}
        ValInputHolder* eval(ValState* state) override
        {
            assert(false);
        }
    };

    template<typename LocalValType, typename ParentNodeType, ValType val_type>
    class VarNode: public ParentNodeType
    {
        string var_name;
    public:
        explicit VarNode(string _var_name):
                var_name(std::move(_var_name)), ParentNodeType() {}
        LocalValType* eval(ValState* state) override
        {
            assert(state->get()->has(var_name));
            auto* ret = (LocalValType*)state->get()->get(var_name);
            assert(ret->get_type() == val_type);
            return ret;
        }
    };

    typedef VarNode<ValSolverE, SolverENode, type_solver_e> SolverEVar;
    typedef VarNode<ValSolverE, SolverENode, type_solver_ae> SolverAEVar;
    typedef VarNode<ValProblemE, ProblemENode, type_problem_e> ProblemEVar;
    typedef VarNode<ValProblemAE, ProblemAENode, type_problem_ae> ProblemAEVar;
    typedef VarNode<ValInt, IntNode, type_int> IntVar;
    typedef VarNode<ValSolutionHolder, SolutionHolderNode, type_solution_holder> SolutionHolderVar;
    typedef VarNode<ValInputHolder, InputHolderNode, type_input_holder> InputHolderVar;

    template<typename LocalValType, typename ParentNodeType, typename BaseValType>
    class ConstNode: public ParentNodeType
    {
        LocalValType* val = NULL;
    public:
        ConstNode(): ParentNodeType() {}
        explicit ConstNode(BaseValType base_val):
                val(new LocalValType(base_val)), ParentNodeType() {}
        LocalValType* eval(ValState* state) override {
            return val;
        }
    };

    typedef ConstNode<ValProblemE, ProblemENode, ProblemE*> ProblemEConst;
    typedef ConstNode<ValProblemAE, ProblemAENode, ProblemAE*> ProblemAEConst;
    typedef ConstNode<ValInt, IntNode, int>  IntConst;
    typedef ConstNode<ValSolverE, SolverENode, Solver_E*> SolverEConst;
    typedef ConstNode<ValSolverAE, SolverAENode, Solver_AE*> SolverAEConst;

    class SolveENode: public SolutionHolderNode
    {
        SolverENode* solver_node;
        ProblemENode* problem_node;
    public:
        SolveENode(SolverENode* _solver_node, ProblemENode* _problem_node):
                solver_node(_solver_node), problem_node(_problem_node), SolutionHolderNode() {}
        ValSolutionHolder* eval(ValState* state) override
        {
            return new ValSolutionHolder(solver_node->eval(state)->get()->solve(problem_node->eval(state)->get()));
        }
    };

    class SolveAENode: public SolutionHolderNode
    {
        SolverAENode* solver_node;
        ProblemAENode* problem_node;
    public:
        SolveAENode(SolverAENode* _solver_node, ProblemAENode* _problem_node):
                solver_node(_solver_node), problem_node(_problem_node), SolutionHolderNode() {}
        ValSolutionHolder* eval(ValState* state) override
        {
            return new ValSolutionHolder(solver_node->eval(state)->get()->solve(problem_node->eval(state)->get()));
        }
    };

    class CheckNode: public InputHolderNode
    {
        ProblemAENode* problem_node;
        SolutionHolderNode* solution_holder_node;

    public:
        CheckNode(ProblemAENode* _problem_node, SolutionHolderNode* _solution_holder_node):
                problem_node(_problem_node), solution_holder_node(_solution_holder_node), InputHolderNode() {}
        ValInputHolder* eval(ValState* state) override
        {
            return new ValInputHolder(check(problem_node->eval(state)->get(), solution_holder_node->eval(state)->get()));
        }
        static InputAssignment* check(ProblemAE* problem, HoleAssignment* solution_holder)
        {
            cout << "TODO: check" << endl;
            assert(false);
        }
    };

    class NonNullNode: public BoolNode
    {
        InputHolderNode* input_holder_node;
    public:
        explicit NonNullNode(InputHolderNode* _input_holder_node): input_holder_node(_input_holder_node), BoolNode(non_null_node) {}
        ValBool* eval(ValState *state) override
        {
            return new ValBool(!input_holder_node->eval(state)->get()->is_null());
        }
    };

    class MergeProblemsENode: public ProblemENode{
        ProblemENode* left_node;
        ProblemENode* right_node;
    public:
        MergeProblemsENode(ProblemENode* _left_node, ProblemENode* _right_node):
                left_node(_left_node), right_node(_right_node), ProblemENode() {}
        ValProblemE* eval(ValState* state) override
        {
            return new ValProblemE(merge_problems(left_node->eval(state)->get(), right_node->eval(state)->get()));
        }
        static ProblemE* merge_problems(ProblemE* left, ProblemE* right)
        {
            cout << "TODO: merge_problems" << endl;
            assert(false);
        }
    };

    class ConcretizeInputsNode: public ProblemENode
    {
        ProblemAENode* problem_node;
        InputHolderNode* input_holder_node;
    public:
        ConcretizeInputsNode(ProblemAENode* _problem_node, InputHolderNode* _input_holder_node):
                problem_node(_problem_node), input_holder_node(_input_holder_node), ProblemENode() {}
        ValProblemE* eval(ValState* state) override
        {
            return new ValProblemE(concretize_inputs(problem_node->eval(state)->get(), input_holder_node->eval(state)->get()));
        }
        static ProblemE* concretize_inputs(ProblemAE* problem, InputAssignment* input_holder)
        {
            cout << "TODO: produce_function_with_concretized_inputs" << endl;
            assert(false);
        }
    };


    inline void basic_while()
    {
        cout << "basic_while" << endl;
        auto* setup = new StateAssignment(
                "x",
                new IntConst(0)
        );
        auto* the_while = new StateWhile(
                new BoolComparison(
                        new IntVar("x"),
                        new IntConst(10),
                        BoolComparison::lt
                ),
                new StateAssignment(
                        "x",
                        new IntBinaryOp(
                                new IntVar("x"),
                                new IntConst(1),
                                IntBinaryOp::plus
                        )
                )
        );
        auto* root = new StateSequence(
                setup,
                the_while
        );
        State* ret = root->eval(new ValState(new State()))->get();
        assert(((ValInt*)ret->get("x"))->get() == 10);
        cout << "ok" << endl;
    }

    inline void target_cegis(CEGISFinderSpec* finder)
    {
        cout << "cegis" << endl;
        map<string, SketchFunction*> function_map;
//    #Solver* solver = SATSolver;
//    auto* solver_assgn =
//            new StateAssignment("solver", new SolverConst(new SATSolver()));
//    Harness harness = new ProblemAE(f);
        auto* harness_assgn =
                new StateAssignment(
                        "harness",
                        new ProblemAEConst(new ProblemAE(function_map["f"])));

//  HoleAssignment solution = RandomSolver.solve(harness);
        auto* rand_solution_assgn =
                new StateAssignment(
                        "solution_holder",
                        new SolveAENode(
                                new SolverAEConst(new RandomSolver()),
                                new ProblemAEVar("harness")));

//    #HoleAssignment solution_holder = new HoleAssignment(harness);
//    StateNode* solution_assgn =
//            new StateAssignment("solution", new SolutionHolderConstructor(new ProblemVar("harness")));
//    #solution_holder.init_rand();

//    InputAssignment* counter_example = check(harness, solution_holder)
        auto* counter_example_assgn =
                new StateAssignment(
                        "counter_example",
                        new CheckNode(
                                new ProblemAEVar("harness"),
                                new SolutionHolderVar("solution_holder")));

//    concretized_problem = new ProblemAE();
        auto* concretized_problem_assgn =
                new StateAssignment("concretized_problem", new ProblemEConst());

//    non_null(conter_example)
        auto* counter_example_is_not_null =
                new NonNullNode(new InputHolderVar("counter_example"));

//  concretized_problem = merge(concretized_problem, harness.produce_function_with_concretized_inputs(counter_example));
        auto* problem_merge_assign =
                new StateAssignment(
                        "concretized_problem",
                        new MergeProblemsENode(
                                new ProblemEVar("concretized_problem"),
                                new ConcretizeInputsNode(
                                        new ProblemAEVar("harness"),
                                        new InputHolderVar("counter_example")
                                )
                        )
                );
//        solution_holder = solver.solve(concretized_problem);
        auto* solution_update_assgn =
                new StateAssignment(
                        "solution_holder",
                        new SolveENode(
                                new SolverEConst(new WrapperCEGISFinderSpec(finder)),
                                new ProblemEVar("concretized_problem")
                        )
                );

//        counter_example = check(harness, solution_holder);
        auto* final_check_assgn =
                new StateAssignment(
                        "counter_example",
                        new CheckNode(
                                new ProblemAEVar("harness"),
                                new SolutionHolderVar("solution_holder")
                        )
                );

//    while(non_null(conter_example))
//    {
//        (DONE) concretized_problem = merge(concretized_problem, harness.produce_function_with_concretized_inputs(counter_example));
//        (DONE) solution_holder = solver.solve(concretized_problem);
//        (DONE) counter_example = check(harness, solution_holder);
//    }

        auto* while_loop =
                new StateWhile(
                        counter_example_is_not_null,
                        new StateSequence(
                                problem_merge_assign,
                                solution_update_assgn,
                                final_check_assgn
                        )
                );

//    return = solution_holder
        auto* return_assgn =
                new StateAssignment(
                        "return",
                        new SolutionHolderVar("solution_holder")
                );

        auto* cegis_ast =
                new StateSequence(
                        harness_assgn,
                        rand_solution_assgn,
                        counter_example_assgn,
                        concretized_problem_assgn,
                        while_loop,
                        return_assgn
                );


        State* ret = cegis_ast->eval(new ValState(new State()))->get();
        assert(false);
        //assert(((ValSolutionHolder*)ret->get("return"))->get()->is_null());
        cout << "ok" << endl;
    }

    class WrapperChecker
    {
        CommandLineArgs& args;
        HoleHardcoder& hc;
        FloatManager& floats;
    public:
        WrapperChecker(CommandLineArgs& _args, HoleHardcoder& _hc, FloatManager& _floats):
        args(_args), hc(_hc), floats(_floats) {}
        ProblemE* get_counter_example_concretized_problem_e(ProblemAE* problem, HoleAssignment* solution_holder)
        {
            auto* checker = new CEGISChecker(args, hc, floats);
            checker->addProblem(problem->get_harness(), nullptr);
            VarStore* controls = solution_holder->to_var_store();
            cout << endl << "HERE: controls->printContent(cout);" << endl;
            controls->printContent(cout);
            cout << "END HERE" << endl << endl;
            return new ProblemE(new SketchFunction(checker->check(*controls)));
        }
    };

    inline HoleAssignment* first_cegis(
            SketchFunction* harness, FloatManager& floats, CommandLineArgs& _args, HoleHardcoder& _hc,
            CEGISFinderSpec* finder)
    {
        ProblemAE* problem_ae = new ProblemAE(harness);
        HoleAssignment* solution_holder = (new ConstantSolver())->solve(problem_ae);

        ProblemE* problem_e = (new WrapperChecker(_args, _hc, floats))->
                get_counter_example_concretized_problem_e(problem_ae, solution_holder);

        while(problem_e->get_dag() != NULL)
        {
            solution_holder->update((new WrapperCEGISFinderSpec(finder))->solve(problem_e));
            if (solution_holder->get_sat_solver_result() != SAT_SATISFIABLE)
            {
                break;
            }
            ProblemE* prev_problem_e = problem_e;
            problem_e = (new WrapperChecker(_args, _hc, floats))->
                    get_counter_example_concretized_problem_e(problem_ae, solution_holder);
            if(problem_e->get_dag() == NULL)
            {
                break;
            }
            prev_problem_e->get_dag()->andDag(problem_e->get_dag());
            problem_e = prev_problem_e;
        }
        return solution_holder;
    }




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

        Assignment_SkVal* recordSolution() {
            auto ret = solver->get_control_map_as_map_str_skval();
            cout << "WrapperAssertDAG::recordSolution VALUES = ";
            for (auto it : ret->get_assignment()) {
                cout << it.first << ": " << it.second->to_string() << ", ";
            }
            cout << endl;
            return ret;
        }

        HoleAssignment* solve(ProblemAE* problem) override
        {
//            cout << endl;
//            cout << "ENTERING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;
            solver->addProblem(problem->get_harness(), problem->get_file());

            SATSolverResult ret_result = SAT_UNDETERMINED;
            Assignment_SkVal* holes_to_sk_val = nullptr;
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

            VarStore* tmp_var_store = holes_to_sk_val->to_var_store(false);
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

            HoleAssignment* ret = new HoleAssignment(ret_result, holes_to_sk_val);
//            cout << "EXITING WrapperAssertDAG->solve(" << problem->get_harness()->get_dag()->get_name() << ")" << endl;
//            cout << "failing assert: " << problem->get_harness()->produce_concretization(*holes_to_sk_val->to_var_store(false), bool_node::CTRL)->get_dag()->get_failed_assert() << endl;
//            cout << "returns " << ret->to_string() << endl << endl;


            assert(problem->get_harness()->get_dag()->getNodesByType(bool_node::UFUN).empty());
            if(!problem->get_harness()->get_dag()->getNodesByType(bool_node::CTRL).empty())
            {
                auto tmp_local_var_store = ret->to_var_store(false);
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

    inline HoleAssignment* wrapper_assert_dag(
            SketchFunction* harness, const string& file_name,
            FloatManager& floats, CommandLineArgs& _args,
            HoleHardcoder& _hc,
            bool hasGoodEnoughSolution)
    {
        return
            (new WrapperAssertDAG(floats, _hc, _args, hasGoodEnoughSolution))->
            solve(new ProblemAE(harness, new File(harness, file_name, floats, _args.seed)));
    }

    inline const HoleAssignment* target_best_effort(SolverProgramState* state, string file_name, bool do_solver_program)
    {
        assert(!file_name.empty());
//        assert(state->harness_ == nullptr);
        if(do_solver_program) {

            string solver_program_file_name;

            assert(!state->function_map.empty());

            solver_program_file_name = "solver_language_program__multi_harness_stun.txt";


            int init_num_global_dags = BooleanDAG::get_allocated().size();
            int init_num_global_nodes = bool_node::get_allocated().size();

            BooleanDagLightUtility* local_harness = ((BooleanDagUtility*)state->function_map["sketch_main__Wrapper"])->clone();
            local_harness->increment_shared_ptr();

            FunctionMap& function_map = local_harness->get_env()->function_map;

            int init_function_map_transformer_size = function_map.transformer_size();

            parse_solver_langauge_program(state, solver_program_file_name);

            SL::VarVal *var_val_ret = state->eval();

            if(var_val_ret->is_solution_holder())
            {

                const SolverLanguagePrimitives::HoleAssignment* solution_holder = var_val_ret->get_solution(false);

                delete var_val_ret;
                state->clear();

                local_harness->concretize_this_dag(solution_holder->to_var_store(), bool_node::CTRL);

                File *file = new File(local_harness, file_name, state->floats, state->args.seed);

                int num_passing_inputs =
                        local_harness->count_passing_inputs(file);

                cout << "HERE " << local_harness->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                file->clear();

                local_harness->clear();

//                solution_holder->set_sat_solver_result(SAT_SATISFIABLE); // requires solution_holder to be non-const
                assert(solution_holder->get_sat_solver_result() == SAT_SATISFIABLE);


//                local_harness->set_solution_ctrl_var_store(solution_holder->to_var_store());

                assert(BooleanDAG::get_allocated().size() - init_num_global_dags == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);


                assert(init_function_map_transformer_size == function_map.transformer_size());

                return solution_holder;
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

                var_val_ret->clear_assert_0_shared_ptrs();
                state->clear();

//                BEST SO FAR: count	1249 / 1743 (71.6581 %)
//                count	1258 / 1743 (72.1744 %)

// with params:X
//                num_trials = 6;
//                num_rows_per_sample = 4;
//                select_best = 3;
//count	1224 / 1743 (70.2238 %)

// with params:
//                num_trials = 20;
//                num_rows_per_sample = 4;
//                select_best = 8;
//count	1288 / 1743 (73.8956 %)

// with params:
//                num_trials = 20;
//                num_rows_per_sample = 6;
//                select_best = 8;
//                count	1186 / 1743 (68.0436 %)



// num_trials = 20;
// num_rows_per_sample = 4;
// select_best = 8;
// with x2 (40, 4, 11 maybe);
// count	1288 / 1743 (73.8956 %)

//                num_trials = 6;
//                num_rows_per_sample = 6;
//                select_best = 3;
//count	1058 / 1743 (60.6999 %)

                int dags_diff = BooleanDAG::get_allocated().size() - init_num_global_dags;
                int all_remaining_inlining_trees = SkFuncSetter::all_inlining_trees.size();

                assert(dags_diff == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

                int transformer_size_diff = function_map.transformer_size() - init_function_map_transformer_size;

                function_map.check_consistency();
                assert(function_map.contains_only_necessary());

                cout << SkFuncSetter::max_count <<endl;
                assert(SkFuncSetter::max_count == 1);

//                if(transformer_size_diff != 0){
//                    function_map.print_not_erased();
//                }

                function_map.soft_clear_transformer();

                //TODO: return function / relevant solution
                HoleAssignment* ret = new HoleAssignment(true);
                ret->set_sat_solver_result(SAT_SATISFIABLE);
                return ret;
            }

            assert(false);
        }

        /*
//        expose lightverif
        ofstream fout = ofstream("fixes_old__"+state->harness_->get_dag()->get_name());
        File* all_file = new File(state->harness_, file_name, state->floats, state->args.seed);
        int num_samples = 30;
        int rows_per_sample = 6;
        vector<pair<int, HoleAssignment*> > solutions;
        for(int i = 0;i<num_samples;i++)
        {
            File* sub_file = all_file->sample_sub_file(rows_per_sample, fout);
            WrapperAssertDAG* solver =
                    new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);
            HoleAssignment* solution_holder = (solver)->
                    solve(new ProblemAE(state->harness_, sub_file));
            BooleanDagLightUtility* concretized_function = ((BooleanDagUtility*)state->harness_)->produce_concretization(
                    solution_holder->to_var_store(), bool_node::CTRL);
            int num_passing_inputs = concretized_function->count_passing_inputs(all_file);
            concretized_function->clear();
            solution_holder->set_sat_solver_result(SAT_SATISFIABLE);
            solutions.emplace_back(num_passing_inputs, solution_holder);

            cout << endl;
            cout << "######################################################" << endl;
            cout << "DONE WITH SAMPLE " << i << endl;
            cout << "SCORE " << num_passing_inputs <<" %: "<< 100.0*(float)num_passing_inputs/all_file->size() << endl;
            cout << solver->get_solver()->get_last_elapsed_time().to_string() << endl;
            cout << "######################################################" << endl<< endl;


            fout << endl;
            fout << "######################################################" << endl;
            fout << "DONE WITH SAMPLE " << i << endl;
            fout << "SCORE " << num_passing_inputs <<" %: "<< 100.0*(float)num_passing_inputs/all_file->size() << endl;
            fout << solver->get_solver()->get_last_elapsed_time().to_string() << endl;
            fout << "######################################################" << endl<< endl;
        }
        sort(solutions.begin(), solutions.end());
        reverse(solutions.begin(), solutions.end());

        cout << "Solution count: " << endl;
        for(int i = 0;i<solutions.size();i++)
        {
            cout << "count\t" << solutions[i].first << " / " << all_file->size() <<" ("<< 100.0*(float)solutions[i].first/all_file->size() << " %)" << endl;
        }

        fout << "Solution count: " << endl;
        for(int i = 0;i<solutions.size();i++)
        {
            fout << "count\t" << solutions[i].first << " / " << all_file->size() <<" ("<< 100.0*(float)solutions[i].first/all_file->size() << " %)" << endl;
        }

//        state->harness_->set_solution_ctrl_var_store(solutions[0].second->to_var_store());

        if(solutions[0].second->get_sat_solver_result() == SAT_SATISFIABLE)
        {
            cout << "return SAT_SATISFIABLE" << endl;
        }
        else
        {
            cout << "return NOT SAT_SATISFIABLE" << endl;
        }

        return solutions[0].second;*/
    }

    /**
 *
package ALL{
int g(x){
     return x * ??;
}

bit P(){
  ...;
}

int f(x){
       if(P(x)){
     return g1(x);
       }else{
     return g2(x);
       }
}
}

STUN(file):
  A' = ALL[ g1 with fresh g, g2 with fresh g];
  (sol) = BestEffort(A', file)
  while(){
   A' = ALL[ g1 with (sol->f@A'), g2 with fresh g];
   (sol) = BestEffort(A', file)
  }
*/

};

/**
 * removed hardcoder from CEGISFinder.
 *      Refactored setCurrentHarness by moving it to SolverHelper, along with all relevant fields.
 *      Moved DepTracker to BooleanToCNF.
 * exposed failedAssert in BooleanDAG; it is set after hardcoding in hardCodeINode.
 * completed refactoring of reading files ahead of time;
 *      moved redeclareInputs, declareInputs from CEGISChecker to CounterExampleFinder.
 *      created redeclareInputsAndAngelics
 * added get_size to SkVal
 * removed cpt (checkpointer).
 */

/**
 * TODO: VarStore refactor.
 * Q for 16th July 2021
 *
 * Next big step: moving up from assertDag to doallpairs - wrap prepare miter;
 *
 * TODO: Check refactoring of if(!hcoder.get_globalSat()->setCurrentHarness(tmpPid))
 * Why do we need the hardcoder in;
 *          need bc of rabbit-hole due to concretizing,
 *          moving on to next harness, hardcoding,
 *          but then not checking fully;
 *          so you eventually need to check.
 *     Solution:
 *          that logic was moved down to the (internal, but global) SolverHelper.
 *  CEGISFinder line 39 (DONE: removed, replaced with a call to SolverHelper).
 *  CEGISChecker line 543 (TODO: how do we remove this completelly).
 * HoleHardcoder::get_control_map are all of them integers?
 * remove CEGISSolver::normalizeInputStore?
 *
 * (TO TRY) How to have good testing procedure on all tests? seq/makefile, make, make long; test/testrunner.
 * (TO TEST MORE) Check implementation of best effort - checking if a DAG got fully reduced.
 * (DONE) Check refactoring of file reading ahead of time; how VarStore inputs is created; check grow input refactor.
 *
 * (DONE) Siemens project: going to hackaton in 2 weeks, funding for trip?
 * (DONE) book ticket via Armando's assistant
 * (DONE) CEGISFinder:updateCtrlVarStore why otype is NULL? I need it not be null to cast it to a value of appropriate type.
 * (DONE) Reading file ahead of time and storing examples.
 * (DONE) parseLine: where are the inputs actually stored?
 *
 * (DONE) Can we remove checkpointer CEGIS? yes
 * (DONE) Can we remove stopped early? no
 *
 * (FOR LATER) Need to get SkVal from SynthInSolver (instantiated in synths in VarStore); stuck in implementing finalizeSynthOutputs to store SkVals).
 * (DONE) ask about recordSolution: why call record solution twice?
 * (DONE) ask about get_control_map_as_map_str_str: why populate values twice
 * (DONE) Refactor passing an input-concretized dag to finder
 * (DONE) Refactor returning a input-concretized dag from checker
 *
 * Compiling BooleanDAG to pytorch? possible only when inputs to if-node (arracc) are all one-hots;
 */

class SolverLanguage {
public:
    SolverLanguage()
    {


    }

    void sandbox()
    {
        SolverLanguagePrimitives::basic_while();
//        SolverLanguagePrimitives::target_cegis(finder);
    }

//    const SolverLanguagePrimitives::HoleAssignment *
//    eval(SketchFunction *harness, const string &file_name, FloatManager &floats, CommandLineArgs &_args,
//         HoleHardcoder &_hc,
//         bool hasGoodEnoughSolution, FunctionMap &function_map)
//    {
//        SolverProgramState* state =
//                new SolverProgramState(harness, file_name, floats, _args, _hc, hasGoodEnoughSolution, function_map);
//        return SolverLanguagePrimitives::target_best_effort(state, file_name, true);
//    }

    const SolverLanguagePrimitives::HoleAssignment *
    eval(FunctionMap &function_map, const string& file_name, FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc,
         bool hasGoodEnoughSolution)
    {
        SolverProgramState state =
                SolverProgramState(function_map, file_name, floats, _args, _hc, hasGoodEnoughSolution);
        return SolverLanguagePrimitives::target_best_effort(&state, file_name, true);
    }
};

class SolverProgram: public BooleanDAG
{

};

class SolverLanguageParser
{
    SolverProgram program;
public:
    SolverLanguageParser(const string& file_name)
    {
        ifstream file(file_name);
        string line;
        while ( getline (file, line) )
        {
            cout << line << '\n';
        }
    }
};


#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
