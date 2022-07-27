//
// Created by Kliment Serafimov on 7/26/22.
//

#ifndef SKETCH_CEGISFINDERBATCHENUMERATION_H
#define SKETCH_CEGISFINDERBATCHENUMERATION_H

#include "BooleanDAG.h"
#include "VarStore.h"
#include "CEGISParams.h"
#include "CEGISFinder.h"



class CEGISFinderBatchEnumeration : public CEGISFinderSpec  {

        bool stoppedEarly;
        CEGISParams params;

        vector<CTRL_node*> ctrl_nodes;

        void addInputsToTestSet(BooleanDAG* problem, VarStore& input);
        void addProblemToTestSet(BooleanDAG* problem);

    public:

        CEGISFinderBatchEnumeration(FloatManager& floats, CommandLineArgs &args) : params(args), CEGISFinderSpec(floats)
        {

        }

        void clear() override
        {
            delete this;
        }

        bool find(BooleanDAG* problem, VarStore& controls, bool hasInputChanged) override;

        bool minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes) override;

        void declareControl(CTRL_node* cnode) override {
            ctrl_nodes.push_back(cnode);
        }

        void updateCtrlVarStore(VarStore& ctrlStore) override {
            for (CTRL_node* it : ctrl_nodes)  {
                AssertDebug(ctrlStore.contains(it->get_name()), "It seems like ctrlStore should already contain all ctrls bc VarStore is a reference, probably initialized before calling this function; not sure why this is necessary. IF THIS ASSERT FAILS examine why are certain holes not declared ahead of time. ");
                ctrlStore.newVar(it->get_name(), it->get_nbits(), it->otype, it->type, it->get_original_name(), it->get_source_dag_name());
            }
        }

        void retractAssumptions() override {

        }
    };



#endif //SKETCH_CEGISFINDERBATCHENUMERATION_H
