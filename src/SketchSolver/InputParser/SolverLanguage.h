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
#include "DagFunctionInliner.h"
#include "InterpreterEnvironment.h"

#include "SolverLanguageYaccHeader.h"
#include "SketchFunction.h"


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

    class InputHolder;

    class ProblemAE;

    /*
    class Function
    {
        SketchFunction* harness;
        FloatManager& floats;
    public:
        explicit Function(SketchFunction* _harness, FloatManager& _floats): harness(_harness), floats(_floats){}
        explicit Function(Function* function): harness(function->harness), floats(function->floats){}

        void clear()
        {
            harness->clear();
            delete harness;
            harness = NULL;
        }

        SketchFunction *get_harness() {
            return harness;
        }

//        BooleanDAG* get_dag() const
//        {
//            return dag;
//        }

        FloatManager& get_floats()
        {
            return floats;
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
            SketchFunction* inlined_harness = harness->produce_inlined_dag();
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

        Function* produce_function_with_concretized_holes(SolutionHolder* solution_holder)
        {
            return new Function(
                    harness->produce_concretization(*solution_holder->to_var_store(), bool_node::CTRL), floats);
//            return new Function(
//                    hardCodeINode(get_dag(), *solution_holder->to_var_store(), bool_node::CTRL, floats), floats);
        }

        Function* produce_function_with_concretized_inputs(InputHolder* input_holder)
        {
            return new Function(
                    harness->produce_concretization(*input_holder->to_var_store(), bool_node::SRC), floats);
//            return new Function(
//                    hardCodeINode(get_dag(), *input_holder->to_var_store(), bool_node::SRC, floats), floats);
        }

        BooleanDAG* get_dag() {
            return harness->get_dag();
        }

//        BooleanDAG* get_original_dag()
//        {
//            return harness->get_original_dag();
//        }

        int count_passing_inputs(File* file) {
            int ret = 0;
            int num_0s = 0;
            int num_1s = 0;
            for(int i = 0;i<file->size();i++)
            {
                Function* concretized_function = produce_function_with_concretized_inputs(
                        new InputHolder(file->at(i), floats));
                assert((concretized_function->get_dag()->size() == 0) == (concretized_function->get_dag()->get_failed_assert() == NULL));
                if(concretized_function->get_dag()->get_failed_assert() == NULL)
                {
                    ret += 1;
                }
                concretized_function->clear();
            }
            cout << "num_1s " << num_1s <<" num_0s "<< num_0s <<" ret "<< ret << endl;
            return ret;
        }
    };
     */

    class ProblemAE: public SketchFunction
    {
        File* file;
        string file_name;
    public:
        explicit ProblemAE(SketchFunction* _function, File* _file = NULL):
                SketchFunction(_function), file(_file){}

        File* get_file()
        {
            return file;
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
//        vector<InputHolder*> concrete_inputs;
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
//        SolutionHolder* solution_holder;
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

            if(!ctrlStore.contains(cname)){
                ctrlStore.newVar(cname, size, cnode->getOtype());
            }
        }
        void add_problem(BooleanDAG* problem, VarStore& ctrlStore)
        {
            {
                vector<bool_node*>& problemIn = problem->getNodesByType(bool_node::CTRL);
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

        SolutionHolder* solve(ProblemE* problem) override
        {
            VarStore ctrlStore;
            add_problem(problem->get_dag(), ctrlStore);
            bool found = finder->find(problem->get_dag(), ctrlStore, true);
            SATSolver::SATSolverResult ret;
            if(found)
            {
                ret = SATSolver::SATISFIABLE;
                return new SolutionHolder(ret, &ctrlStore, finder->get_floats());
            }
            else
            {
                ret = SATSolver::UNSATISFIABLE;
                return new SolutionHolder(ret);
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
        SolutionHolder* solve(ProblemAE* problem) override
        {
            vector<SkHoleSpec>* hole_names = problem->get_holes();
            auto* ret = new SolutionHolder();
            
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

        SolutionHolder* solve(ProblemAE* problem) override
        {
            vector<SkHoleSpec>* blank_solution = problem->get_holes();
            auto* ret = new Assignment_SkVal();

            for(int i = 0;i<blank_solution->size();i++)
            {
                SkHoleSpec sk_hole_spec = blank_solution->at(i);
                ret->set(sk_hole_spec.get_name(), get_const_val(sk_hole_spec.get_type()));
            }

            return new SolutionHolder(SATSolver::SATISFIABLE, ret);
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

//  SolutionHolder solution = RandomSolver.solve(harness);
        auto* rand_solution_assgn =
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
        ProblemE* get_counter_example_concretized_problem_e(ProblemAE* problem, SolutionHolder* solution_holder)
        {
            auto* checker = new CEGISChecker(args, hc, floats);
            checker->addProblem(problem->get_harness(), nullptr);
            VarStore* controls = solution_holder->get_controls(floats);
            cout << endl << "HERE: controls->printContent(cout);" << endl;
            controls->printContent(cout);
            cout << "END HERE" << endl << endl;
            return new ProblemE(new SketchFunction(checker->check(*controls)));
        }
    };

    inline SolutionHolder* first_cegis(
            SketchFunction* harness, FloatManager& floats, CommandLineArgs& _args, HoleHardcoder& _hc,
            CEGISFinderSpec* finder)
    {
        ProblemAE* problem_ae = new ProblemAE(harness);
        SolutionHolder* solution_holder = (new ConstantSolver())->solve(problem_ae);

        ProblemE* problem_e = (new WrapperChecker(_args, _hc, floats))->
                get_counter_example_concretized_problem_e(problem_ae, solution_holder);

        while(problem_e->get_dag() != NULL)
        {
            solution_holder->update((new WrapperCEGISFinderSpec(finder))->solve(problem_e));
            if (solution_holder->get_sat_solver_result() != SATSolver::SATISFIABLE)
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

        void recordSolution(Assignment_SkVal* holes_to_sk_val)
        {
//            hardcoder vals get recorded in external outer-loop.
//            hardcoder.get_control_map_str_to_skval(holes_to_sk_val);
            solver->get_control_map_as_map_str_skval(holes_to_sk_val);
            cout << "WrapperAssertDAG::recordSolution VALUES = ";
            for (auto it : holes_to_sk_val->get_assignment()) {
                cout << it.first << ": " << it.second->to_string() << ", ";
            }
            cout << endl;
        }

        SolutionHolder* solve(ProblemAE* problem) override
        {
            solver->addProblem(problem->get_harness(), problem->get_file());

            SATSolver::SATSolverResult ret_result = SATSolver::UNDETERMINED;
            auto* holes_to_sk_val = new Assignment_SkVal();
            //copied from InterpreterEnviroment::assertDAG
            {
                bool ret_result_determined = false;
                int solveCode = 0;
                try {

                    solveCode = solver->solve();
                    if (solveCode || !hasGoodEnoughSolution) {
                        recordSolution(holes_to_sk_val);
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
                        recordSolution(holes_to_sk_val);
                    }
//                    needs InterpreterEnviroment::basename
//                    cout << "ERROR: " << basename() << endl;
                    ret_result = SATSolver::ABORTED;
                    ret_result_determined = true;
                }

                if (!ret_result_determined)
                {
                    if (!solveCode) {
                        ret_result = SATSolver::UNSATISFIABLE;
                    }
                    else {
                        ret_result = SATSolver::SATISFIABLE;
                    }
                }
            }
            cout << "exit WrapperAssertDAG->solve(..)" << endl;
            return new SolutionHolder(ret_result, holes_to_sk_val);
        }
    };

    inline SolutionHolder* wrapper_assert_dag(
            SketchFunction* harness, const string& file_name,
            FloatManager& floats, CommandLineArgs& _args,
            HoleHardcoder& _hc,
            bool hasGoodEnoughSolution)
    {
        return
            (new WrapperAssertDAG(floats, _hc, _args, hasGoodEnoughSolution))->
            solve(new ProblemAE(harness, new File(harness, file_name, floats, _args.seed)));
    }


//    class ConcretizationUnit
//    {
//        int harness_id;
//        string function_to_concretize;
//    };
//
//    inline SolutionHolder* STUN(ProgramEnvironment* env, bool hasGoodEnoughSolution)
//    {
//        /**
//         * generator g_pred(x);
//         * function f_pred0(x) {g_pred(x);}
//         * function f_pred1(x) {if(g_pred(x)){f_pred0(x)}{g_ped(x)})
//         * function f_pred2(x) {if(g_pred(x)){f_pred1(x)}{g_pred(x)})
//         * harness h0(x, y) {assert(f_pred0(x) == y));}
//         * harness h1(x, y) {assert(f_pred1(x) == y));}
//         * harness h2(x, y) {assert(f_pred2(x) == y));}
//         */
//
//        //solve h0:
//        //set up h0 as a harness
//        BooleanDAG* bd = prepareMiter(getCopy(spskpairs[i].spec), getCopy(spskpairs[i].sketch), inlineAmnt);
//
//        Harness* local_harness = new Harness(getCopy(env->spskpairs[i].sketch), bd, program_env);
//
//        local_harness = local_harness->produce_concretization(partial_concretization, bool_node::CTRL);
//
//        local_harness->set_name(spskpairs[i].sketch);
//
//        //concretize f_pred0;
//        //replace f_pred0 with the concretized version
//        //solve h1;
//        //
//
//        SolutionHolder* sol = (new WrapperAssertDAG(env->get_floats(), env->get_hardcoder(), env->params, hasGoodEnoughSolution))->
//                solve(new ProblemAE(new Function(harness, floats), sub_file));
//
//    }

    inline SolutionHolder* target_best_effort(SolverProgramState* state, string file_name = "")
    {

        bool do_solver_program = file_name == "";
        if(do_solver_program) {

            if(file_name.empty()) {
//                file_name = "uav_kg_big__as_bools__smaller.data";
                file_name = "uav_kg_big__as_bools.data";
//                file_name = "zig_zag.data";
            }
            string solver_program_file_name;

//            SketchFunction* local_harness = state->function_map["main_lvl1__Wrapper"]->clone();
            SketchFunction* local_harness = state->function_map["sketch_main__Wrapper"]->clone();

            File *file = nullptr;
            if(state->harness_ == nullptr)
            {
                assert(!state->function_map.empty());

                file = new File(local_harness, file_name, state->floats, state->args.seed);
                solver_program_file_name = "solver_language_program__multi_harness_stun.txt";
            }
            else
            {
                assert(state->function_map.empty());
                assert(state->harness_ != nullptr);
                file = new File(state->harness_, file_name, state->floats, state->args.seed);
                solver_program_file_name = "solver_language_program__single_harness_best_effort.txt";
            }

            cout << "BOOLEAN DAGS:" << endl;

            int init_num_global_dags = BooleanDAG::get_allocated().size();

            int init_num_global_nodes = bool_node::get_allocated().size();


            parse_solver_langauge_program(state, solver_program_file_name);

            SL::VarVal *var_val_ret = state->eval();

            if(var_val_ret->is_solution_holder())
            {

                SolverLanguagePrimitives::SolutionHolder* solution_holder = var_val_ret->get_solution(false);

                delete var_val_ret;

                SketchFunction *concretized_function = local_harness->produce_with_concretized_holes(solution_holder);
                int num_passing_inputs =
                        concretized_function->count_passing_inputs(file);

                cout << "HERE " << local_harness->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                concretized_function->clear();

                solution_holder->set_sat_solver_result(SATSolver::SATISFIABLE);

                local_harness->set_solution(solution_holder->to_var_store());


                assert(BooleanDAG::get_allocated().size() - init_num_global_dags == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

                return solution_holder;
            }
            else
            {
                assert(var_val_ret->is_sketch_function());

                SketchFunction* sk_func = var_val_ret->get_function(false);

                SketchFunction *concretized_function = sk_func;

                file = new File(concretized_function, file_name, state->floats, state->args.seed);

                int num_passing_inputs =
                        concretized_function->count_passing_inputs(file);

                cout << "HERE " << local_harness->get_dag()->get_name() << endl;
                cout << "count\t" << num_passing_inputs << " / " << file->size() << " ("
                     << 100.0 * (float) num_passing_inputs / file->size() << " %)" << endl;

                var_val_ret->clear_assert_0_shared_ptrs();

//                BEST SO FAR: count	1249 / 1743 (71.6581 %)
//                count	1258 / 1743 (72.1744 %)

// with params:
//                num_trials = 6;
//                num_rows_per_sample = 4;
//                select_best = 3;
//count	1224 / 1743 (70.2238 %)

// with params:
//                num_trials = 20;
//                num_rows_per_sample = 4;
//                select_best = 8;
//count	1288 / 1743 (73.8956 %)

                int dags_diff = BooleanDAG::get_allocated().size() - init_num_global_dags;
                assert(dags_diff == 0);
                assert(bool_node::get_allocated().size() - init_num_global_nodes == 0);

                //TODO: return function / relevant solution
                assert(false);

            }

            assert(false);
        }

//        expose lightverif
        ofstream fout = ofstream("fixes_old__"+state->harness_->get_dag()->get_name());
        File* all_file = new File(state->harness_, file_name, state->floats, state->args.seed);
        int num_samples = 30;
        int rows_per_sample = 6;
        vector<pair<int, SolutionHolder*> > solutions;
        for(int i = 0;i<num_samples;i++)
        {
            File* sub_file = all_file->sample_sub_file(rows_per_sample);
            WrapperAssertDAG* solver =
                    new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);
            SolutionHolder* sol = (solver)->
                    solve(new ProblemAE(state->harness_, sub_file));
            SketchFunction* concretized_function = state->harness_->produce_with_concretized_holes(
                    sol);
            int num_passing_inputs = concretized_function->count_passing_inputs(all_file);
            concretized_function->clear();
            sol->set_sat_solver_result(SATSolver::SATISFIABLE);
            solutions.emplace_back(num_passing_inputs, sol);

            cout << endl;
            cout << "######################################################" << endl;
            cout << "DONE WITH SAMPLE " << i << endl;
            cout << "SCORE " << num_passing_inputs <<" %: "<< 100.0*(float)num_passing_inputs/all_file->size() << endl;
            cout << solver->get_solver()->get_last_elapsed_time()->to_string() << endl;
            cout << "######################################################" << endl<< endl;


            fout << endl;
            fout << "######################################################" << endl;
            fout << "DONE WITH SAMPLE " << i << endl;
            fout << "SCORE " << num_passing_inputs <<" %: "<< 100.0*(float)num_passing_inputs/all_file->size() << endl;
            fout << solver->get_solver()->get_last_elapsed_time()->to_string() << endl;
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

        state->harness_->set_solution(solutions[0].second->to_var_store());

        if(solutions[0].second->get_sat_solver_result() == SATSolver::SATISFIABLE)
        {
            cout << "return SATSolver::SATISFIABLE" << endl;
        }
        else
        {
            cout << "return NOT SATSolver::SATISFIABLE" << endl;
        }

        return solutions[0].second;
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

    SolverLanguagePrimitives::SolutionHolder *
    eval(SketchFunction *harness, const string &file_name, FloatManager &floats, CommandLineArgs &_args,
         HoleHardcoder &_hc,
         bool hasGoodEnoughSolution, map<string, SketchFunction *> &function_map)
    {
        SolverProgramState* state =
                new SolverProgramState(harness, floats, _args, _hc, hasGoodEnoughSolution, function_map);
        return SolverLanguagePrimitives::target_best_effort(state, file_name);
    }

    SolverLanguagePrimitives::SolutionHolder *
    eval(map<string, SketchFunction *> &function_map, FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc,
         bool hasGoodEnoughSolution)
    {
        SolverProgramState* state =
                new SolverProgramState(function_map, floats, _args, _hc, hasGoodEnoughSolution);
        return SolverLanguagePrimitives::target_best_effort(state);
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
