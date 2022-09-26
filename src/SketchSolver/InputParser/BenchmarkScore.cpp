//
// Created by Kliment Serafimov on 7/10/22.
//

#include "BenchmarkScore.h"
#include <iostream>
#include <vector>
#include <iomanip>

map<string, BenchmarkScore> timestamp_counter = map<string, BenchmarkScore>();

std::chrono::steady_clock::time_point timestamp(std::chrono::steady_clock::time_point  prev_timestamp, const string& name)
{
    auto local_end = chrono::steady_clock::now();
    auto elapsed = chrono::duration_cast<chrono::microseconds>(local_end - prev_timestamp).count();
//    cout << "ELAPSED(" << name << "): " << elapsed_tokenize << " (us)" << endl;
    if(timestamp_counter.find(name) == timestamp_counter.end()) {
        timestamp_counter[name] = BenchmarkScore();
    }
    timestamp_counter[name].update(elapsed);
    auto new_timestamp = chrono::steady_clock::now();
    return new_timestamp;
}


//void print_performance_summary() {
//    print_performance_summary(cout);
//}

string spaces(int num_spaces)
{
    string ret = "";
    for(int i = 0;i<num_spaces;i++) {
        ret += " ";
    }
    return ret;
}

string performance_summary_to_string(bool sort_by_min)
{
    string ret;
    ret += "{\n\t--- PERFORMANCE SCORE --- ""\n";

    vector<pair<BenchmarkScore, string> > sorted_benchmarks;

    int max_label_width = 0;

    sorted_benchmarks.reserve(timestamp_counter.size());
    for (const auto &it: timestamp_counter) {
        sorted_benchmarks.emplace_back(it.second, it.first);
        max_label_width = max(max_label_width, (int) it.first.size());
    }

    if(sort_by_min) {
        sort(sorted_benchmarks.begin(), sorted_benchmarks.end());
    }

    for (const auto &it: sorted_benchmarks) {
        ret += "\t" + it.second + spaces(max_label_width - it.second.size()) + " ::\t" + it.first.to_string() + "\n";
    }

    ret += "\t--- PERFORMANCE SCORE ---\n} ""\n";
    return ret;
}

void print_performance_summary(ofstream& fout) {
    fout << performance_summary_to_string();
}
