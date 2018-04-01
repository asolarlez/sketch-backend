#pragma once

class SuggestionGenerator {
public:
    virtual vector<tuple<int, int, int>> getSatSuggestions(const gsl_vector* state) = 0;
    virtual vector<tuple<int, int, int>> getUnsatSuggestions() = 0;
};
