#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include <map>
#include <set>

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

class SimpleSuggestionGenerator: public SuggestionGenerator {
    vector<int> nodesToSuggest;
    Interface* interf;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    SimpleEvaluator* seval;
public:
    SimpleSuggestionGenerator(BooleanDAG* _dag, Interface* _interface, map<string, int>& _ctrls): dag(_dag), interf(_interface), ctrls(_ctrls) {
        seval = new SimpleEvaluator(*dag, ctrls);
        seval->setInputs(interf);
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
            bool_node* n = (*dag)[nodeid];
            if (n->type == bool_node::LT) {
                nodesToSuggest.push_back(nodeid);
            }
        }
    }
    
    pair<int, int> getSuggestion(const gsl_vector* state) {
        return make_pair(-1, 0);
    }
    pair<int, int> getUnsatSuggestion(const gsl_vector* state) {
        return make_pair(-1, 0);
    }
    virtual IClause* getConflictClause(int level, LocalState * state) { 
        return NULL;
    }

    virtual SClause* getUnsatClause(const gsl_vector* state) { 
        return NULL;
    }

    virtual SClause* getUnsatClause(const gsl_vector* state, const gsl_vector* prevState) { 
        return NULL;
    }
};
    
    