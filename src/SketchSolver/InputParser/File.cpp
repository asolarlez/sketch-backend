//
// Created by kliment on 12/20/21.
//

#include "BooleanDagLightUtility.h"
#include "File.h"
#include "BooleanDAG.h"


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


void File::growInputs(VarStore & inputStore, BooleanDAG* dag){
    dag->growInputIntSizes();
    redeclareInputs(inputStore, dag);
}

void File::relabel(BooleanDagLightUtility *harness) {
    BooleanDagLightUtility* cloned_inlined_harness = harness->produce_inlined_dag();
    cloned_inlined_harness->increment_shared_ptr();
    BooleanDAG* problem = cloned_inlined_harness->get_dag();
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    auto inputs = problem->getNodesByType(bool_node::SRC);

    for(int i = 0;i<size();i++)
    {
        at(i)->relabel(inputs);
    }

    cloned_inlined_harness->clear();
}

#include "GenericFile.h"

File::File(BooleanDagLightUtility *harness, const string &file_name, FloatManager &floats, int seed)

#define IMPLEMENT_WITH_GENERIC_FILE

#ifdef IMPLEMENT_WITH_GENERIC_FILE

{
    GenericFile generic_file = GenericFile(file_name, seed);
    init(harness, &generic_file, floats, seed);
}

#else

{
    generator = std::mt19937(seed);
    BooleanDAG* problem = harness->get_dag()->clone();
    harness->get_env()->doInline(*problem);
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    auto inputs = problem->getNodesByType(bool_node::SRC);

    File::Result res = parseFile(file_name, floats, inputs, input_store);
    const int max_num_bits = 64;

    const map<string, BooleanDAG *> * bool_dag_map = harness->get_env()->function_map.to_boolean_dag_map();
    while (res == File::MOREBITS) {
        int at_int_size = problem->getIntSize();
        AssertDebug(at_int_size < max_num_bits, "TOO MANY BITS, CHECK THE INPUT TYPES OF YOUR HARNESS. OTHERWISE PROBABLY WRONG CODE/INPUT/OUTPUT.");
        growInputs(input_store, problem);
        if(true){
            assert(harness->get_dag()->getIntSize() == at_int_size);
            bool harness_in_function_map = false;
            for(auto it: *bool_dag_map) {
                assert(it.second->getIntSize() == at_int_size);
                it.second->growInputIntSizes();
                if(it.second->get_name() == harness->get_dag()->get_name()) {
                    assert(!harness_in_function_map);
                    assert(it.second == harness->get_dag());
                    harness_in_function_map = true;
                }
            }
            if(!harness_in_function_map)
            {
                assert(harness->get_dag()->getIntSize() == at_int_size);
                harness->get_dag()->growInputIntSizes();
            }
            else
            {
                assert(harness->get_dag()->getIntSize() == at_int_size+1);
            }
        }

        res = parseFile(file_name, floats, inputs, input_store);
    }
    assert(res == File::DONE);
#ifdef CHECK_FILE_INVARIANT
    used = vector<int>(size(), 0);
#endif
    delete bool_dag_map;
    bool_dag_map = nullptr;
    problem->clear();
}
#endif

#ifdef CHECK_FILE_INVARIANT
int File::get_used(int i) {
    return used[i];
}


void File::set_used(int i) {
    used[i]++;
    counterexample_ids_over_time.emplace_back(i);
}

#endif

File *File::produce_filter(std::function< bool(VarStore*) >& lambda_condition) {
    File* ret = new File(generator);
    for(int i = 0;i<size();i++)
    {
        if(lambda_condition(at(i)))
        {
            ret->push_back(at(i)->clone());
        }
    }
#ifdef CHECK_FILE_INVARIANT
    ret->used = vector<int>(ret->size(), 0);
#endif
    return ret;
}

File::File(BooleanDagLightUtility *harness, GenericFile *generic_file, FloatManager &floats, int seed)
{
    init(harness, generic_file, floats, seed);
}

void File::init(BooleanDagLightUtility *harness, GenericFile *generic_file, FloatManager &floats, int seed)
{
    generator = std::mt19937(seed);
    BooleanDAG* problem = harness->get_dag()->clone();
    harness->get_env()->doInline(*problem);
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    auto inputs = problem->getNodesByType(bool_node::SRC);

    File::Result res = parseFile(generic_file, floats, inputs, input_store);
    const int max_num_bits = 32;

    const map<string, BooleanDAG *> * bool_dag_map = harness->get_env()->function_map.to_boolean_dag_map();
    while (res == File::MOREBITS) {
        int at_int_size = problem->getIntSize();
        AssertDebug(at_int_size <= max_num_bits, "TOO MANY BITS, CHECK THE INPUT TYPES OF YOUR HARNESS. OTHERWISE PROBABLY WRONG CODE/INPUT/OUTPUT.");
        growInputs(input_store, problem);
        if(true){
            assert(harness->get_dag()->getIntSize() == at_int_size);
            bool harness_in_function_map = false;
            for(auto it: *bool_dag_map) {
                if(it.second->getIntSize() != at_int_size) {
                    assert(it.second->getIntSize() > at_int_size);
                }
                else {
                    assert(it.second->getIntSize() == at_int_size);
                    it.second->growInputIntSizes();
                }
                if(it.second->get_name() == harness->get_dag()->get_name()) {
                    assert(!harness_in_function_map);
                    assert(it.second == harness->get_dag());
                    harness_in_function_map = true;
                }
            }
            if(!harness_in_function_map)
            {
                assert(harness->get_dag()->getIntSize() == at_int_size);
                harness->get_dag()->growInputIntSizes();
            }
            else
            {
                assert(harness->get_dag()->getIntSize() == at_int_size+1);
            }
        }

        res = parseFile(generic_file, floats, inputs, input_store);
    }
    assert(res == File::DONE);
#ifdef CHECK_FILE_INVARIANT
    used = vector<int>(size(), 0);
#endif
    delete bool_dag_map;
    bool_dag_map = nullptr;
    problem->clear();
}


File::Result File::parseFile(GenericFile *generic_file, FloatManager &floats, vector<bool_node *> &inputNodes,
                             const VarStore &inputs)  {
    light_clear();

    bool allow_new_iter = true;
    for(int i = 0; i<generic_file->size(); i++){
        assert(allow_new_iter);
        VarStore* new_row = inputs.clone();
        parseLineOut ok;
        try {
            ok = parseLine(generic_file->at(i), floats, inputNodes, new_row);
        }
        catch (BasicError& e) {
            assert(false);
            throw e;
        }



        if (ok == more_bits) {
            new_row->clear();
            return MOREBITS;
        }
        else if(ok == complete_row)
        {
            push_back(new_row);
        }
        else if(ok == end_of_file__empty_row)
        {
            new_row->clear();
            assert(i == generic_file->size()-1);
            allow_new_iter = false;
        }
        else
        {
            Assert(false, "missing case for parseLineOut.")
        }
        if (PARAMS->verbosity > 12) {
            new_row->printContent(cout);
        }
    }
    return DONE;
}


File::File() = default;



