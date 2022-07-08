//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_FILE_H
#define SKETCH_SOURCE_FILE_H


#include <random>
#include "VarStore.h"

//MODIFIES InputStore
void declareInput(VarStore & inputStore, const string& cname, int size, int arrSz, OutType* otype);

//MODIFIES InputStore
void redeclareInputs(VarStore & inputStore, BooleanDAG* dag, bool firstTime=false);

void redeclareInputsAndAngelics(VarStore & inputStore, BooleanDAG* dag);

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

    File(const BooleanDagLightUtility *harness, const string& file_name, FloatManager& floats, int seed, bool_node::Type var_type = bool_node::SRC);

    void init(const BooleanDagLightUtility *harness, GenericFile* generic_file, FloatManager& floats, int seed, bool_node::Type var_type = bool_node::SRC);

    File(const BooleanDagLightUtility *harness, GenericFile* generic_file, FloatManager& floats, int seed, bool_node::Type var_type = bool_node::SRC);

    void clear() {
        light_clear();
        delete this;
    }

    enum parseLineOut {end_of_file__empty_row, more_bits, incomplete_row, complete_row};

    parseLineOut parseLine(string _line, FloatManager& floats, const vector<bool_node*>& inputNodes, VarStore* inputs) {
//
//        AssertDebug(false, "IF YOU GET TO HERE IT SHOULD MEAN THAT YOU HAVE INTEGRATED THIS FUNCTION IN THE EXPECTED WAY.");

        auto vsi = inputs->begin();
        objP* arrit = nullptr;
        objP* prevArrit = nullptr;
        bool inArray = false;

        int inputId = 0;

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
                        prevArrit->makeArr(prevArrit->get_index(), prevArrit->get_index() + 2);
                        arrit = prevArrit->get_next();
                        ((SRC_node*)inputNodes[inputId])->arrSz++;
                    }

                    //we just finished a number, and we are inside an array.
                    outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
//                    cout << "[" << (int)outOfRange <<"]'" << endl;
                    prevArrit = arrit;
                    arrit = arrit->get_next();
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
                        }
                    } else if (ch == '.') {
                        isFloat = true;
                        floatVal = (double) cur;
                        cur = 10;
                    } else {
                        Assert(false, "UNKNOWN CHARACTER <" << ch
                                                            << "> IN LINE: " + _line << "\n");
                    }
                    break;
                }
            }
            if (outOfRange) {
                return more_bits;
            }
//        assert(!in.eof());
            assert(at_ch_id < _line.size());
            ch = _line[at_ch_id++];
//        in.get(ch);
//            cout << ch << "("<<(int)ch<<")'''";
//        if (in.eof())
//            if(at_ch_id == _line.size())
//            {
//                regval();
//                assert(!outOfRange);
//                if(vsi == inputs->end()){
//                    return complete_row;
//                }
//                else {
//                    AssertDebug(false, "INCOMPLETE LINE: " + _line);
//                    return incomplete_row;
//                }
//            }
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

    File *sample_sub_file(int num_rows) {
        if(false) {
            //DEBUG VERSION TO SEE WHICH IDs ARE CHOSEN
            vector<int> ids;
            for (int i = 0; i < size(); i++) {
                ids.push_back(i);
            }
            vector<int> sample_ids;
            sample(ids.begin(), ids.end(), back_inserter(sample_ids),
                   num_rows, generator);
            File *new_file = new File(generator);

#ifdef CHECK_FILE_INVARIANT
            new_file->used = vector<int>(new_file->size(), 0);
#endif
            return new_file;
        }
        else {
            vector<VarStore*> samples;
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

    File *produce_filter(std::function< bool(VarStore*) >& lambda_condition);

    void relabel(BooleanDagLightUtility *harness);

    File();
};

class SketchFunction;
VarStore* string_to_var_store(const string& _line, const BooleanDagLightUtility *skfunc, bool_node::Type var_type = bool_node::SRC);

#endif //SKETCH_SOURCE_FILE_H
