//
// Created by kliment on 3/20/22.
//

#ifndef SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
#define SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H


#include "SolverLanguageLexAndYaccHeader.h"
#include "FunctionMap.h"

namespace FMTL{

    class FunctionMapTransformerState: public ProgramState
    {
        SL::CodeBlock* root = nullptr;
        FunctionMap& source_function_map;
        ProgramEnvironment* env = nullptr;
        map<SketchFunction*, SketchFunction*>* meta_map_dp = new map<SketchFunction*, SketchFunction*>();
    public:
        explicit FunctionMapTransformerState(FunctionMap& _source_function_map);

        void clear(bool conscious_call = true) override
        {
            root->clear();
            ProgramState::clear(true);
        }

        void add_root(SL::CodeBlock* code_block) {
            root = code_block;
        }

        SL::VarVal* eval() {

//            assert(!function_map.empty());
//            for(const auto& it: function_map){
//                global.add_var_and_set_var_val_and_clear_var(
//                        new SL::Var(new SL::Identifier("SketchFunction"),
//                                    new SL::Identifier(it.first)),
//                        new SL::VarVal(function_map[it.first]));
//            }

            new_stack_frame();
            assert(frames.size() == 1);

            root->run(this);

            SL::VarVal* var_val_ret = get_return_var_val();
            assert(var_val_ret != nullptr);

            assert(frames.size() == 1);
            pop_stack_frame();
            assert(frames.empty());

            global.clear(true);

            return var_val_ret;
        }

        SketchFunction *get_source_skfunc(string name);

        ProgramEnvironment *get_env();

        map<SketchFunction *, SketchFunction *> *get_meta_map_dp();
    };

    void parse_function_map_transformer_program(FunctionMapTransformerState* _state, string solver_program_file);
}

typedef void* yyscan_t;
void yyerror(yyscan_t scanner, FMTL::FunctionMapTransformerState* state, string s);



#endif //SKETCH_SOURCE_FUNCTIONMAPTRANSFORMERLANGUAGE_H
