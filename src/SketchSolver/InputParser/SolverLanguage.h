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


#include "BooleanDAG.h"
#include "SATSolver.h"

using namespace std;

/**
 *  Grammar:
 *  StateNode :=
 *      StateNode ; StateNode |
 *      While(BoolNode){StateNode} |
 *      If(BoolNode){StateNode}{StateNode} |
 *      StringNode = ValNode |  # assignment
 *  BoolNode :=
 *      BoolVal |
 *      BinaryBoolNode |
 *      not(BoolNode) |
 *      BoolComparison |
 *  BinaryBoolNode :=
 *      BoolNode {|and|or|xor|} BoolNode |
 *  BoolComparison :=
 *      IntNode {|<|<=|==|!=|>=|>|} IntNode
 *  IntNode :=
 *      IntConst |
 *      IntVar |  # lookup
 *      IntNode {|+|-|*|/|%|} IntNode|
 *  ValNode :=
 *      BoolVal |
 *      IntVal |
 *      SolverVal |
 *      AssignmentVal
 *      ProblemVal
 *  BoolValNode :=
 *      true |
 *      false |
 *      BoolVar  # lookup
 *  IntConst :=
 *      0 | 1 | -1 | 2 | -2 ...
 *  AssignmentVal :=
 *      SolverVal(ProblemVal)
 *  SolverVal :=
 *      SAT_Solver |
 *      Numerical
 *  ProblemVal :=
 *      primitive
 */

namespace SolverLanguagePrimitives
{

    template<typename T>
    class PolyVal
    {
        T val;
    public:
        explicit PolyVal(T _val): val(_val){}
        T get() {
            return val;
        }
    };

    enum ValType {type_state, type_int, type_bool, type_solver_e, type_solver_ae, type_problem_ae, type_problem_e, type_solution_holder, type_input_holder};

    class Val
    {
    private:
        ValType val_type;
    public:
        explicit Val(ValType _val_type): val_type(_val_type){}
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

    class Function
    {
        BooleanDAG* dag;
    public:
        explicit Function(BooleanDAG* _dag): dag(_dag){}
        vector<string>* get_holes()
        {
            cout << "TODO: Function::get_holes()" << endl;
            assert(false);
        }
    };

    template<typename ValType>
    class Assignment
    {
        bool null = true;
        map<string, ValType*> assignment;
    public:
        Assignment() = default;
        bool has(const string& name)
        {
            return assignment.find(name) != assignment.end();
        }
        ValType* get(const string& name)
        {
            assert(has(name));
            return assignment[name];
        }
        void set(const string& name, ValType* val)
        {
            null = false;
            assignment[name] = val;
        }
        bool is_null()
        {
            return null;
        }
        string to_string()
        {
            if(null)
            {
                return "null";
            }
            else {
                string ret = "{";
                for (auto p : assignment) {
                    ret += p.first + " <- " + p.second->to_string() + "; ";
                }
                ret += "}";
                return ret;
            }
        }
    };

    class SkVal
    {
    public:
        enum SketchValType {sk_type_int, sk_type_float};
    private:
        SketchValType sketch_val_type;
    public:
        explicit SkVal(SketchValType _sketch_val_type):
                sketch_val_type(_sketch_val_type) {}
        SketchValType get_type() {
            return sketch_val_type;
        }
        virtual string to_string()
        {
            assert(false);
        }
    };
    class SkValInt: public SkVal, public PolyVal<int>
    {
    public:
        explicit SkValInt(int _val): PolyVal<int>(_val), SkVal(SkVal::sk_type_int){}
        string to_string() override {return std::to_string(get());}
    };
    class SkValFloat: public SkVal, public PolyVal<float>
    {
    public:
        explicit SkValFloat(float _val): PolyVal<float>(_val), SkVal(SkVal::sk_type_float){}
        string to_string() override {return std::to_string(get());}
    };

    class InputHolder;

    class ProblemE;

    class ProblemAE
    {
        Function* function;
        string file_name;
        vector<InputHolder*> inputs_from_file;
    public:
        explicit ProblemAE(Function* _function, string  _file_name = ""):
                function(_function), file_name(std::move(_file_name)) {}
        vector<string>* get_holes()
        {
            return function->get_holes();
        }
        virtual string to_string()
        {
            cout << "TODO: ProblemAE::to_string" << endl;
            assert(false);
        }
        ProblemE* concretize_holes()
        {

        }
    };


    class InputHolder: public Assignment<SkVal>
    {
    public :
        InputHolder(): Assignment<SkVal>() {}
        explicit InputHolder(ProblemAE* problem): Assignment<SkVal>() {
            cout << "TODO: SolutionHolder::SolutionHolder" << endl;
            assert(false);
        }
    };

    class ProblemE: public ProblemAE
    {
        vector<InputHolder*> concrete_inputs;
    public:
        string to_string() override
        {
            cout << "TODO: ProblemE::to_string" << endl;
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

    class SolutionHolder: public Assignment<SkVal>
    {
        SATSolver::SATSolverResult sat_solver_result;
    public :
        SolutionHolder(): Assignment<SkVal>() {}
        explicit SolutionHolder(ProblemAE* problem): Assignment<SkVal>() {
            sat_solver_result = SATSolver::ABORTED;
            cout << "TODO: SolutionHolder::SolutionHolder" << endl;
            assert(false);
        }
        SATSolver::SATSolverResult get_sat_solver_result()
        {
            return sat_solver_result;
        }
    };

    class ValSolutionHolder: public Val, public PolyVal<SolutionHolder*>
    {
    public:
        explicit ValSolutionHolder(SolutionHolder* _val): PolyVal<SolutionHolder*>(_val), Val(type_solution_holder){}
        string to_string() override {return get()->to_string();}
    };

    class ValInputHolder: public Val, public PolyVal<InputHolder*>
    {
    public:
        explicit ValInputHolder(InputHolder* _val): PolyVal<InputHolder*>(_val), Val(type_input_holder){}
        string to_string() override {return get()->to_string();}
    };

    class Solver_E
    {
    public:
        Solver_E()= default;
        virtual SolutionHolder* solve(ProblemE* problem)
        { assert(false); }
        virtual string to_string()
        { assert(false); }
    };

    class Solver_AE
    {
    public:
        Solver_AE()= default;
        virtual SolutionHolder* solve(ProblemAE* problem)
        { assert(false); }
        virtual string to_string()
        { assert(false); }
    };

    class SATSolver: public Solver_E
    {
    public:
        SATSolver(): Solver_E() {}
        SolutionHolder* solve(ProblemE* problem) override
        {
            cout << "TODO: SATSolver::solve" << endl;
            //init ctrlStore (as in addProblem)
            //finder->updateCtrlVarStore(ctrlStore);
            //finder->find(getProblem(), inputStore, ctrlStore, hasInputChanged);
            assert(false);
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
        SolutionHolder* solve(ProblemAE* problem) override
        {
            vector<string>* blank_solution = problem->get_holes();
            cout << "TODO: SATSolver::solve" << endl;
            assert(false);
        }
        string to_string() override
        {
            return "RandomSolver";
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

    class State
    {
        map<string, Val*> assignment;
    public:
        State() = default;
        bool has(const string& name)
        {
            return assignment.find(name) != assignment.end();
        }
        Val* get(const string& name)
        {
            assert(has(name));
            return assignment[name];
        }
        void set(const string& name, Val* val)
        {
            assignment[name] = val;
        }
        string to_string()
        {
            string ret = "{";
            for(auto p : assignment)
            {
                ret += p.first + " <- " + p.second->to_string() + "; ";
            }
            ret += "}";
            return ret;
        }
    };

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
        virtual ValState* eval(ValState* state)
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
            return new ValSolutionHolder(new SolutionHolder(problem_node->eval(state)->get()));
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
        static InputHolder* check(ProblemAE* problem, SolutionHolder* solution_holder)
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
        static ProblemE* concretize_inputs(ProblemAE* problem, InputHolder* input_holder)
        {
            cout << "TODO: concretize_inputs" << endl;
            assert(false);
        }
    };


    inline void basic_while()
    {
        cout << "basic_while" << endl;
        auto setup = new StateAssignment(
                "x",
                new IntConst(0)
        );
        auto the_while = new StateWhile(
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
        auto root = new StateSequence(
                setup,
                the_while
        );
        State* ret = root->eval(new ValState(new State()))->get();
        assert(((ValInt*)ret->get("x"))->get() == 10);
        cout << "ok" << endl;
    }

    inline void target_cegis()
    {
        cout << "cegis" << endl;
        map<string, Function*> function_map;
//    #Solver* solver = SATSolver;
//    auto solver_assgn =
//            new StateAssignment("solver", new SolverConst(new SATSolver()));
//    Harness harness = new ProblemAE(f);
        auto harness_assgn =
                new StateAssignment(
                        "harness",
                        new ProblemAEConst(new ProblemAE(function_map["f"])));

//  SolutionHolder solution = RandomSolver.solve(harness);
        auto rand_solution_assgn =
                new StateAssignment(
                        "solution_holder",
                        new SolveAENode(
                                new SolverAEConst(new RandomSolver()),
                                new ProblemAEVar("harness")));

//    #SolutionHolder solution_holder = new SolutionHolder(harness);
//    StateNode* solution_assgn =
//            new StateAssignment("solution", new SolutionHolderConstructor(new ProblemVar("harness")));
//    #solution_holder.init_rand();

//    InputHolder* counter_example = check(harness, solution_holder)
        auto counter_example_assgn =
                new StateAssignment(
                        "counter_example",
                        new CheckNode(
                                new ProblemAEVar("harness"),
                                new SolutionHolderVar("solution_holder")));

//    concretized_problem = new ProblemAE();
        auto concretized_problem_assgn =
                new StateAssignment("concretized_problem", new ProblemEConst());

//    non_null(conter_example)
        auto counter_example_is_not_null =
                new NonNullNode(new InputHolderVar("counter_example"));

//  concretized_problem = merge(concretized_problem, harness.concretize_inputs(counter_example));
        auto problem_merge_assign =
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
        auto solution_update_assgn =
                new StateAssignment(
                        "solution_holder",
                        new SolveENode(
                                new SolverEConst(new SATSolver()),
                                new ProblemEVar("concretized_problem")
                        )
                );

//        counter_example = check(harness, solution_holder);
        auto final_check_assgn =
                new StateAssignment(
                        "counter_example",
                        new CheckNode(
                                new ProblemAEVar("harness"),
                                new SolutionHolderVar("solution_holder")
                        )
                );

//    while(non_null(conter_example))
//    {
//        (DONE) concretized_problem = merge(concretized_problem, harness.concretize_inputs(counter_example));
//        (DONE) solution_holder = solver.solve(concretized_problem);
//        (DONE) counter_example = check(harness, solution_holder);
//    }

        auto while_loop =
                new StateWhile(
                        counter_example_is_not_null,
                        new StateSequence(
                                problem_merge_assign,
                                solution_update_assgn,
                                final_check_assgn
                        )
                );

//    return = solution_holder
        auto return_assgn =
                new StateAssignment(
                        "return",
                        new SolutionHolderVar("solution_holder")
                );

        auto cegis_ast =
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

    inline ValSolutionHolder* first_cegis(BooleanDAG* dag, const string& file)
    {
        auto* problem_ae = new ProblemAE(new Function(dag), file);
        auto* problem_e = problem_ae->concretize_holes();

        return new ValSolutionHolder((new ValSolverE(new SATSolver()))->get()->solve(problem_e));
    }

};



#include "SATSolver.h"

class SolverLanguage {
public:
    SolverLanguage()
    {


    }

    void sandbox()
    {
        SolverLanguagePrimitives::basic_while();
        SolverLanguagePrimitives::target_cegis();
    }

    SolverLanguagePrimitives::SolutionHolder* eval(BooleanDAG* dag, ostream& out, const string& file)
    {
        return SolverLanguagePrimitives::first_cegis(dag, file)->get();
    }
};



#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
