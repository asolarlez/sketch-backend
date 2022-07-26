//
// Created by kliment on 3/20/22.
//

#ifndef SKETCH_SOURCE_GENERICFILE_H
#define SKETCH_SOURCE_GENERICFILE_H

#include <string>
#include <fstream>
#include <vector>

#include "BasicError.h"
#include <random>
#include <algorithm>
#include <functional>

using namespace std;

//#define USE_GENERIC_FILE
#ifdef USE_GENERIC_FILE
#define FILE_TYPE GenericFile
#else
#define FILE_TYPE File
#endif

class GenericFile: public vector<string> {
    string file_name;
    std::mt19937 generator;

public:

    GenericFile() = default;

    explicit GenericFile(std::mt19937 _generator): generator(_generator){}

    explicit GenericFile(const string& _file_name, int seed): file_name(_file_name) {
        generator = std::mt19937(seed);
        ifstream file(file_name);
        AssertDebug(file.is_open(), "FILE " + file_name + " WASN'T SUCCESSFULLY OPENED.");
        string line;
        while(getline(file, line)) {
            assert(line[line.size()-1] != '\n');
            line+="\n";
            push_back(line);
        }
        file.close();
    }

    explicit GenericFile(GenericFile* to_copy)
    {
        for(int i = 0;i<to_copy->size();i++)
        {
            push_back(to_copy->at(i));
        }
    }



    const string& get_file_name() {
        return file_name;
    }

    void light_clear() {
        vector<string>::clear();
    }

    void clear() {
        light_clear();
        delete this;
    }

    GenericFile *sample_sub_file(int num_rows) {
        if(false) {
            //DEBUG VERSION TO SEE WHICH IDs ARE CHOSEN
            vector<int> ids;
            for (int i = 0; i < size(); i++) {
                ids.push_back(i);
            }
            vector<int> sample_ids;
            sample(ids.begin(), ids.end(), back_inserter(sample_ids),num_rows, generator);
            GenericFile *new_file = new GenericFile(generator);
            for(int i = 0;i< sample_ids.size();i++) {
                assert(sample_ids[i] < size());
                new_file->push_back(at(sample_ids[i]));
            }

            return new_file;
        }
        else {
            vector<string> samples;
            sample(begin(), end(), back_inserter(samples),
                   num_rows, generator);
            GenericFile* new_file = new GenericFile(generator);
            for(int i = 0;i<samples.size();i++) {
                new_file->push_back(samples[i]);
            }
            return new_file;
        }
    }

    GenericFile *produce_filter(std::function< bool(string) >& lambda_condition);

};


#endif //SKETCH_SOURCE_GENERICFILE_H
