//
// Created by kliment on 7/12/21.
//

#include "CounterexampleFinder.h"

void declareInput(VarStore & inputStore, const string& inname, int bitsize, int arrSz, OutType* otype) {
    //Inputs can be redeclared to change their sizes, but not controls.
    if( !inputStore.contains(inname)){
        if(arrSz >= 0){
            inputStore.newArr(inname, bitsize, arrSz, otype, bool_node::SRC);
        }else{
            inputStore.newVar(inname, bitsize, otype, bool_node::SRC, "declareInput()", "declareInput()");
        }
        Dout( cout<<" INPUT "<<inname<<" sz = "<<bitsize<<endl );
    }else{
        // cout<<" RESIZING "<<inname<<" to "<<bitsize<<endl;
        inputStore.resizeVar(inname, bitsize);
        if(arrSz >= 0){
            inputStore.resizeArr(inname, arrSz);
        }
    }
}

void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime){
    //
    {
        auto specIn = dag->getNodesByType(bool_node::SRC);
        for(size_t i=0; i<specIn.size(); ++i){
            SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);
            int nbits = srcnode->get_nbits();
            if(nbits >= 2 || firstTime){
                declareInput(inputStore, specIn[i]->get_name(), nbits, srcnode->getArrSz(), srcnode->getOtype());
            }
        }
    }
    {
        auto ufunin = dag->getNodesByType(bool_node::UFUN);
        int nbits = dag->getIntSize();
        for(size_t i=0; i<ufunin.size(); ++i){
            UFUN_node* ufunnode = dynamic_cast<UFUN_node*>(ufunin[i]);
            string tuple_name = ufunnode->getTupleName();

            Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
            int size = tuple_type->actSize;
            int ASize =  1 << PARAMS->NINPUTS;
            for(int tt = 0; tt<size; ++tt){
                stringstream sstr;
                sstr<<ufunnode->get_ufname()<<"_"<<ufunnode->get_uniquefid()<<"_"<<tt;
                OutType* ttype = tuple_type->entries[tt];
                bool isArr = ttype->isArr ;
                bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
                declareInput(inputStore, sstr.str() , isBool ? 1 : nbits, (isArr ? ASize : -1), ttype );
            }
        }
    }
}

void redeclareInputsAndAngelics(VarStore & input_store, BooleanDAG* problem)
{
    // code copied from addProblem in CEGISChecker (previously was in CEGISSolver::addProblem).
    // declare inputs and angelics
    redeclareInputs(input_store, problem, true);
    auto problemIn = problem->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < problemIn.size(); ++i) {
        CTRL_node *ctrlnode = dynamic_cast<CTRL_node *>(problemIn[i]);
        int nbits = ctrlnode->get_nbits();
        if (ctrlnode->spAngelic) {
            declareInput(input_store, problemIn[i]->get_name() + "_src", nbits, ctrlnode->getArrSz(),
                         ctrlnode->getOtype());
        }
    }
}