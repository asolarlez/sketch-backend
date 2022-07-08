#pragma once
#include "OptimizationWrapper.h"
#include "BoolBasedSampler.h"
class SuggestionGenerator {
public:
    virtual IClause* getConflictClause(int level, LocalState * state) = 0;
    virtual pair<int, int> getSuggestion(const gsl_vector* state) = 0;
    virtual pair<int, int> getUnsatSuggestion(const gsl_vector* state) = 0;
    virtual SClause* getUnsatClause(const gsl_vector* state, const gsl_vector* initState)  = 0;
    virtual SClause* getUnsatClause(const gsl_vector* state)  = 0;
    virtual ~SuggestionGenerator() {};
};
