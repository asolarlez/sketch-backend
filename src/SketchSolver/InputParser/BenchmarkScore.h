//
// Created by Kliment Serafimov on 7/10/22.
//

#ifndef EXAFUNCTIONGRAPHEXECUTOR_BENCHMARKSCORE_H
#define EXAFUNCTIONGRAPHEXECUTOR_BENCHMARKSCORE_H

#include <string>
#include <chrono>
#include <map>

using namespace std;

class BenchmarkScore
{
    unsigned long long total_us = 0;
    unsigned long long first_us = -1;
    unsigned long long min_us = numeric_limits<unsigned long long>::max();
    unsigned long long max_us = numeric_limits<unsigned long long>::min();
    int count = 0;
    inline unsigned long long get_avg() const{
        return total_us/count;
    }
public:
    void update(unsigned long long new_us) {
        if(count == 0)
        {
            first_us = new_us;
        }
        total_us += new_us;
        min_us = min(min_us, new_us);
        max_us = max(max_us, new_us);
        count += 1;
    }

    string to_string() const {
        return "avg: " + std::to_string(get_avg()) + "; [min: " + std::to_string(min_us) + "; max: " + std::to_string(max_us) + "]; fst: " + std::to_string(first_us) + "; cnt: " + std::to_string(count) + ";";
    }

    bool operator < (const BenchmarkScore& other) const {
        return get_avg() < other.get_avg();
    }
};

extern map<string, BenchmarkScore> timestamp_counter;

std::chrono::steady_clock::time_point timestamp(std::chrono::steady_clock::time_point  prev_timestamp, const string& name);

void print_performance_summary();

#endif //EXAFUNCTIONGRAPHEXECUTOR_BENCHMARKSCORE_H
