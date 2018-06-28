#pragma once
#include "OptimizationWrapper.h"
class SuggestionGenerator {
public:
    virtual IClause* getConflictClause(int level, LocalState * state) = 0;
    virtual pair<int, int> getSuggestion(const gsl_vector* state) = 0;
};
