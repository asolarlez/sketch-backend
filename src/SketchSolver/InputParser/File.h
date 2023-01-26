//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_FILE_H
#define SKETCH_SOURCE_FILE_H


#include <random>
#include "VarStore.h"
#include "FileForVecInterp.h"
#include "BenchmarkScore.h"

//MODIFIES InputStore
void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype);

//MODIFIES InputStore
void redeclareInputs(VarStore & inputStore, const BooleanDAG *dag, bool firstTime= false);

void redeclareInputsAndAngelics(VarStore & inputStore, const BooleanDAG* dag);

class BooleanDagLightUtility;

class GenericFile;

//#define CHECK_FILE_INVARIANT

class File: public vector<VarStore*>
{
    #ifdef CHECK_FILE_INVARIANT
        vector<int> counterexample_ids_over_time;
        vector<int> used;
    #endif

    static void growInputs(VarStore & inputStore, BooleanDAG* dag);
    std::mt19937 generator;

    enum Result {NO_FILE, DONE, MOREBITS};
    void light_clear()
    {
#ifdef CHECK_FILE_INVARIANT
        counterexample_ids_over_time.clear();
        used.clear();
#endif
        for(auto& it : *this)
        {
            it->clear();
            it = nullptr;
        }
        vector<VarStore*>::clear();
    }
public:

    void reset()
    {
#ifdef CHECK_FILE_INVARIANT
        counterexample_ids_over_time.clear();
        for(int i = 0;i<used.size();i++){
            used[i] = 0;
        }
#endif
    }

    bool like_unused() const{
#ifdef CHECK_FILE_INVARIANT
        if(!counterexample_ids_over_time.empty()) {
            return false;
        }
        for(auto it :used) {
            if(it != 0) {
                return false;
            }
        }
#endif
        return true;
    }

    bool operator == (const File& other) const
    {
        if(other.size() != size()) {
            return false;
        }
        for(int i = 0;i<size();i++)
        {
            if(!(*at(i) == *other.at(i))) {
                return false;
            }
        }
        return true;
    }

    explicit File(std::mt19937 _generator): generator(_generator){}

    File(BooleanDagLightUtility *harness, const string& file_name, bool_node::Type var_type = bool_node::SRC);
    File(const string& file_name, BooleanDagLightUtility *harness, bool_node::Type var_type = bool_node::SRC);

    void init(BooleanDagLightUtility *harness, GenericFile* generic_file, bool_node::Type var_type = bool_node::SRC);

    File(BooleanDagLightUtility *harness, GenericFile* generic_file, bool_node::Type var_type = bool_node::SRC);

    void push_back(VarStore* to_insert) {
        vector<VarStore*>::push_back(to_insert);
    }

    explicit File(const VarStore& _controls)
    {
        const int num_bits = _controls.get_bit_size();

        assert(num_bits <= 16); // otherwise too many inputs.
        for(int _i = 0; _i < (1<<num_bits); _i++)
        {
            this->push_back(new VarStore(_controls));

            auto local_controls = this->at(this->size()-1);

            int at_bit_id = 0;

            for(int idx = 0;idx<local_controls->size();idx++)
            {
                objP& obj = local_controls->_getObj(idx);
                for(size_t j = 0;j<obj.get_size();j++, at_bit_id++)
                {
                    obj.set_bit(j, ((_i & (1<<at_bit_id)) != 0));
                }
            }
//            cout << "_i: " << _i << " | " <<local_controls->to_string() << endl;
        }

    }

    void clear() {
        light_clear();
        delete this;
    }

    enum parseLineOut {end_of_file__empty_row, more_bits, incomplete_row, complete_row};

    parseLineOut parseLine(string _line, FloatManager& floats, const vector<bool_node*>& inputNodes, VarStore* inputs) {

        auto vsi = inputs->begin();
        VarStoreElementTrait* arrit = nullptr;
        VarStoreElementTrait* prevArrit = nullptr;
        bool inArray = false;

        int inputId = 0;

//        cout << "anew" << endl;
        int at_ch_id = 0;
        char ch = 0;

        assert(!_line.empty());
        for(auto it: _line)
        {
            AssertDebug(it!='#', "COMMENTS SHOULD HAVE BEEN PRE-PROCESSED OUT");
        }

        ch = _line[at_ch_id++];

        int cur=0;
        bool neg = false;
        int depth = 0;
        bool hasCaptured = true;
        bool outOfRange = false;
        bool isFloat = false;
        double floatVal = 0.0;

        auto regval = [&]() {
            if (!hasCaptured) {

                if (isFloat) {
                    cur = floats.getIdx(floatVal);
                }

                if (depth == 0) {
                    //we just finished a number, and we are not inside an array.
                    outOfRange = !vsi->setValSafe(neg ? (-cur) : cur);
//                    cout << "[" << (int)outOfRange <<"]";
                    ++vsi;
                    ++inputId;
                }
                else {
                    if (!inArray) {
                        cerr << "Error parsing the input. Was expecting a line with the following format" << endl;
                        for (auto it = inputs->begin(); it != inputs->end(); ++it) {
                            auto type = it->otype != nullptr? it->otype->str() : "scalar";
                            const auto isArr = it->arrSize() > 1;
                            if (isArr) {
                                cerr << "{" << type << " }  ";
                            }
                            else {
                                cerr << type << "  ";
                            }
                        }
                        cerr << endl;
                        cerr << "corresponding to inputs "<<endl;
                        for (auto it = inputs->begin(); it != inputs->end(); ++it) {
                            cerr << it->get_name() << "  ";
                        }
                        cerr << endl;
                        throw BasicError(string("file parsing error"), "name");
                    }
                    if (arrit == nullptr) {
                        int prev_arrit_id = prevArrit->get_index();
                        prevArrit->makeArr(prev_arrit_id, prev_arrit_id + 2);
                        arrit = prevArrit->get_next();
                        ((SRC_node*)inputNodes[inputId])->arrSz++;
                    }

                    //we just finished a number, and we are inside an array.
                    outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
//                    cout << "[" << (int)outOfRange <<"]'" << endl;
                    prevArrit = arrit;
//                    cout << "i" << arrit->get_index() << endl;
                    arrit = arrit->get_next();
                }
                if(outOfRange)
                {
                    for(int j = 0; j < inputs->size(); j++)
                    {
                        auto& obj = inputs->_getObj(j);
                        obj._reset_pure_index();
                    }
                }
            }
            hasCaptured = true;
        };
        auto reset = [&]() {
            cur = 0;
            neg = false;
            isFloat = false;
        };

        while (ch != '\n') {
            switch (ch) {
                case '{': {
                    regval();
                    reset();
                    if (depth == 0) {
                        arrit = &(*vsi);
                        inArray = true;
                    }
                    depth++;
                    break;
                }
                case '}': {
                    regval();
                    reset();
                    depth--;
                    if (depth == 0) {
                        while (arrit != nullptr) {
                            arrit->setValSafe(0);
                            arrit = arrit->get_next();
                        }
                        inArray = false;
                        ++vsi;
                        ++inputId;
                    }
                    break;
                }
                case ' ': {
                    regval();
                    reset();
                    break;
                }
                case ',': {
                    regval();
                    reset();
                    break;
                }
                case '-': {
                    neg = true;
                    break;
                }
                default: {
                    if (ch >= '0' && ch <= '9') {
                        if (isFloat) {
                            floatVal = floatVal + ((double) (ch - '0') / cur);
                            cur = cur * 10;
                        } else {
                            hasCaptured = false;
                            cur = cur * 10 + (ch - '0');
//                            cout << "[NUM: " << cur << "]" << endl;
                        }
                    } else if (ch == '.') {
                        isFloat = true;
                        floatVal = (double) cur;
                        cur = 10;
                    } else {
                        Assert(false, "UNKNOWN CHARACTER <" << ch << "> IN LINE: " + _line << "\n");
                    }
                    break;
                }
            }
            if (outOfRange) {
                return more_bits;
            }
            assert(at_ch_id < _line.size());
            ch = _line[at_ch_id++];
//            cout << ch << "_";
        }
        regval();

        if(outOfRange){
            return more_bits;
        }
        else
        {
            if(vsi == inputs->end()){
                return complete_row;
            }
            else{
                AssertDebug(false, "INCOMPLETE LINE: " + _line);
                return incomplete_row;
            }
        }
    }

    Result parseFile(GenericFile* generic_file, FloatManager& floats, const vector<bool_node*>& input_nodes, const VarStore& var_store);

    explicit File(File* to_copy)
    {
        for(int i = 0;i<to_copy->size();i++)
        {
            push_back(to_copy->at(i)->clone());
        }
    }

    File *produce_subset_file(int num_rows, vector<int>* _sample_ids = nullptr) {
        if(_sample_ids != nullptr) {
            assert(_sample_ids->empty());
            vector<int> ids;
            for (int i = 0; i < size(); i++) {
                ids.push_back(i);
            }
            vector<int>& sample_ids = *_sample_ids;
            sample(ids.begin(), ids.end(), back_inserter(sample_ids),
                   num_rows, generator);
            File *new_file = new File(generator);
            for(int i = 0;i<sample_ids.size();i++) {
                new_file->push_back((*this)[sample_ids[i]]->clone());
            }
#ifdef CHECK_FILE_INVARIANT
            new_file->used = vector<int>(new_file->size(), 0);
#endif
            return new_file;
        }
        else {
            vector<const VarStore*> samples;
            sample(begin(), end(), back_inserter(samples),
                   num_rows, generator);
            File* new_file = new File(generator);
            for(int i = 0;i<samples.size();i++) {
                new_file->push_back(samples[i]->clone());
            }
#ifdef CHECK_FILE_INVARIANT
            new_file->used = vector<int>(new_file->size(), 0);
#endif
            return new_file;
        }
    }

    int get_used(int i);

    void set_used(int i);
#ifdef CHECK_FILE_INVARIANT
    const vector<int>& get_counterexample_ids_over_time()
    {
        return counterexample_ids_over_time;
    }
#endif

    File *produce_filter(std::function< bool(const VarStore*) >& lambda_condition);

    void relabel(BooleanDagLightUtility *harness);

    File();

    string to_string() const
    {
        string ret;
        for(int i = 0;i<size();i++)
        {
            ret += at(i)->to_string();
            if(i != size()-1)
            {
                ret += "\n";
            }
        }
        return ret;
    }


private:
    mutable const VectorizedInterpreter::FileForVecInterp* vecinterp_version = nullptr;
public:

    const VectorizedInterpreter::FileForVecInterp* get_file_from_vectorized_interpreter() const {

        if(vecinterp_version == nullptr) {
            auto start_reading_exhausitve_inputs = std::chrono::steady_clock::now();
            vecinterp_version = new VectorizedInterpreter::FileForVecInterp(this);
            timestamp(start_reading_exhausitve_inputs, "reading_exhaustive_inputs");
        }
        return vecinterp_version;
    }


    int num_inputs_per_row() const;

    int count(function<bool(const VarStore *)> function1);
};

VarStore* string_to_var_store(const string& _line, BooleanDagLightUtility *skfunc, bool_node::Type var_type = bool_node::SRC);

#endif //SKETCH_SOURCE_FILE_H
