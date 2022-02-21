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


#define CHECK_FILE_INVARIANT

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

    File(BooleanDagLightUtility *harness, const string& file, FloatManager& floats, int seed);

    void clear() {
        light_clear();
        delete this;
    }

    enum parseLineOut {end_of_file__empty_row, more_bits, incomplete_row, complete_row};

    parseLineOut parseLine(ifstream& in, FloatManager& floats, vector<bool_node*>& inputNodes, VarStore* inputs) {

        auto vsi = inputs->begin();
        VarStore::objP* arrit = NULL;
        VarStore::objP* prevArrit = NULL;
        bool inArray = false;

        int inputId = 0;

        char ch = 0;
        assert(!in.eof());
        in.get(ch);
//        cout << ch << "("<<(int)ch<<")'";
        if(in.eof())
        {
            AssertDebug((int)ch == 0, "ch: " + std::to_string(ch));
            return end_of_file__empty_row;
        }
        string line;
        while (ch == '#') {
            assert(!in.eof());
            std::getline(in, line);
            assert(!in.eof());
            in.get(ch);
//            cout << ch << "("<<(int)ch<<")''";
        }

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
                            auto type = it->otype != NULL? it->otype->str() : "scalar";
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
                            cerr << it->getName()<<"  ";
                        }
                        cerr << endl;
                        throw BasicError(string("file parsing error"), "name");

                    }
                    if (arrit == NULL) {
                        prevArrit->makeArr(prevArrit->index, prevArrit->index + 2);
                        arrit = prevArrit->next;
                        ((SRC_node*)inputNodes[inputId])->arrSz++;
                    }

                    //we just finished a number, and we are inside an array.
                    outOfRange = !arrit->setValSafe(neg ? (-cur) : cur);
//                    cout << "[" << (int)outOfRange <<"]'" << endl;
                    prevArrit = arrit;
                    arrit = arrit->next;
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
                        while (arrit != NULL) {
                            arrit->setValSafe(0);
                            arrit = arrit->next;
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
                        Assert(false, "Unknown character <" << ch
                                                            << "> in file on line " + std::to_string((int) size()) +
                                                               ".");
                    }
                    break;
                }
            }
            if (outOfRange) {
                return more_bits;
            }
            assert(!in.eof());
            in.get(ch);
//            cout << ch << "("<<(int)ch<<")'''";
            if (in.eof()) {
                regval();
                assert(!outOfRange);
                if(vsi == inputs->end()){
                    return complete_row;
                }
                else {
                    AssertDebug(false, "Incomplete row in input file " + std::to_string((int)size()) + ".");
                    return incomplete_row;
                }
            }
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
                AssertDebug(false, "Incomplete row in input file row" + std::to_string((int)size()) + ".");
                return incomplete_row;
            }
        }
    }

    Result parseFile(const string& fname, FloatManager& floats, vector<bool_node*>& inputNodes, const VarStore& inputs) {
        light_clear();
        ifstream file;
        file.open(fname);

        if (!file.is_open() || file.fail()) {
            AssertDebug(false, "File " + fname + " could not be opened!!");
            Assert(false, "File " << fname << " could not be opened!!");
            return NO_FILE;
        }

        bool allow_new_iter = true;
        while (!file.eof()) {
            assert(allow_new_iter);
            VarStore* new_row = inputs.clone();
            parseLineOut ok;
            try {
                ok = parseLine(file, floats, inputNodes, new_row);
            }
            catch (BasicError& e) {
                file.close();
                assert(false);
                throw e;
            }



            if (ok == more_bits) {
                file.close();
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
                assert(file.eof());
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
        file.close();
        return DONE;
    }

    explicit File(File* to_copy)
    {
        for(int i = 0;i<to_copy->size();i++)
        {
            push_back(to_copy->at(i)->clone());
        }
    }

    File(std::mt19937 _generator): generator(_generator){}

    File *sample_sub_file(int num_rows, ofstream& out) {
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
            out << "samples: ";
            for(auto i : sample_ids)
            {
                out << i << " ";
                new_file->push_back(at(i)->clone());
            }
            out << endl;
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


#endif //SKETCH_SOURCE_FILE_H
