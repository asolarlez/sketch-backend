#pragma once
#include "OptimizationWrapper.h"
class SuggestionGenerator {
public:
    virtual IClause* getConflictClause(int level, LocalState * state) = 0;
};
