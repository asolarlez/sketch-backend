#pragma once
#include "OptimizationWrapper.h"
class SuggestionGenerator {
public:
    virtual vector<tuple<int, int, int>> getSatSuggestions(const gsl_vector* state) = 0;
    virtual void initUnsatSuggestions(LocalState * state) = 0;
    virtual pair<int, int> getNextUnsatSuggestion() = 0;
};
