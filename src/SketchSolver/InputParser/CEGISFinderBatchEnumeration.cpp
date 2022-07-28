//
// Created by Kliment Serafimov on 7/26/22.
//

#include "CEGISFinderBatchEnumeration.h"
#include "BasicError.h"
#include "File.h"
#include "BenchmarkScore.h"

class VarStoreShape
{
    class LocalObjP {
        int num_bits_per_vector;
        int num_vectors;
        const OutType* const otype;
        bool_node::Type type = bool_node::NO_TYPE;
    public:
        explicit LocalObjP(const objP& source): num_bits_per_vector(source.element_size()),
            num_vectors( source.arrSize()),
            otype( source.otype),
            type( source.get_type()) {}

        bool operator < (const LocalObjP& other) const
        {
            assert(other.otype == otype);
            assert(other.type == type);
            if(num_bits_per_vector*num_vectors < other.num_bits_per_vector)
            {
                return true;
            }
            else if(num_bits_per_vector > other.num_bits_per_vector)
            {
                return false;
            }
            else
            {
                return false;
            }
        }

        string to_string() const
        {
            return "{" + std::to_string(num_bits_per_vector) + "x" + std::to_string(num_vectors)
                   + "," + std::to_string((long long int) otype) + "," + std::to_string(type) + "}";
        }
    };

    vector<LocalObjP> local_objs;
public:

    string to_string()
    {
        string ret = "[";
        for(int i= 0;i<local_objs.size();i++)
        {
            ret += local_objs[i].to_string() + ", ";
        }
        ret += "]";
        return ret;
    }

    explicit VarStoreShape(const VarStore& var_store)
    {
        for(int i = 0;i<var_store.size();i++)
        {
            local_objs.push_back(LocalObjP(var_store.getObjConst(i)));
        }
    }

    bool operator < (const VarStoreShape& other ) const
    {
        if(local_objs.size() < other.local_objs.size())
        {
            return true;
        }
        else if(local_objs.size() > other.local_objs.size())
        {
            return false;
        }
        else
        {
            for(int i = 0;i<local_objs.size();i++)
            {
                if(local_objs[i] < other.local_objs[i])
                {
                    return true;
                }
                else if (other.local_objs[i] < local_objs[i])
                {
                    return false;
                }
            }
            return false;
        }
    }
};


map<VarStoreShape, File*> var_stores_to_exhaustive_files;


File* exhaustive_file(VarStore& _controls)
{
    VarStoreShape var_store_shape(_controls);
//    cout << "var_store_shape " << var_store_shape.to_string() << endl;
    if(var_stores_to_exhaustive_files.find(var_store_shape) == var_stores_to_exhaustive_files.end()) {
        var_stores_to_exhaustive_files[var_store_shape] = new File(_controls);
    }
    assert(var_stores_to_exhaustive_files.find(var_store_shape) != var_stores_to_exhaustive_files.end());
    return var_stores_to_exhaustive_files[var_store_shape];
}

bool CEGISFinderBatchEnumeration::find(BooleanDAG* problem,
                       VarStore& controls, bool hasInputChanged){

    // the caller expects find to keep track of all the constraints.
    // here dirfind is doing that.

    if(hasInputChanged)
    {
        BooleanDAG* newdag = problem->clone();
        if(allInputsDag == nullptr)
        {
            assert(newdag != nullptr);
            allInputsDag = newdag;
        }
        else
        {
            allInputsDag->andDag(newdag);
            DagOptim dag_optim(*allInputsDag, floats);
            dag_optim.process(*allInputsDag);
        }
    }
    else
    {
        //claim: solution is already optimal.
        return false;
    }

    //Solve

    SATSolverResult result = SAT_UNSATISFIABLE;

    auto start_input_construction = std::chrono::steady_clock::now();

    File* exhaustive_inputs = exhaustive_file(controls);

    auto start_measuring_eval_inptus = timestamp(start_input_construction, "construct_exhaustive_inputs");

    int passing_input_id = -1;
    bool use_evaluate_inputs = false;
    if(use_evaluate_inputs) {
        vector<bool> batch_output = allInputsDag->evaluate_inputs(exhaustive_inputs, floats);
        for (int i = 0; i < batch_output.size(); i++) {
            if (batch_output[i]) {
                passing_input_id = i;
                break;
            }
        }
    }
    else {
        passing_input_id = allInputsDag->get_passing_input_id(exhaustive_inputs, floats);
    }

    if(passing_input_id == -1)
    {
        return false;
    }

    controls = *exhaustive_inputs->at(passing_input_id);
    auto ret = exhaustive_inputs->at(passing_input_id);
    controls.copy_only_bits(ret);
    result = SAT_SATISFIABLE;

//    timestamp(start_measuring_eval_inptus, "BatchEnum_find_n"+std::to_string(allInputsDag->size()));
    timestamp(start_measuring_eval_inptus, "BatchEnum_find");

    return true;

    assert(false);

    if (result != SAT_SATISFIABLE){ 	//If solve is bad, return false.
        if( result != SAT_UNSATISFIABLE){
            switch( result ){
                case SAT_UNDETERMINED: {
                    if (params.lightVerif) {
                        return false;
                    }
                    else {
                        throw new SolverException(result, "SAT_UNDETERMINED"); break;
                    }
                }
                case SAT_TIME_OUT: throw new SolverException(result, "SAT_UNDETERMINED"); break;
                case SAT_MEM_OUT:  throw new SolverException(result, "SAT_MEM_OUT"); break;
                case SAT_ABORTED:  throw new SolverException(result, "SAT_ABORTED"); break;

                default:
                    Assert(false, "MISSING CASE in CEGISFinder::find.");
            }
        }
        return false;
    }

    return true;
}

bool CEGISFinderBatchEnumeration::minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes){
   AssertDebug(false, "NOT IMPLEMENTED");
}
