#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include <map>
#include <set>
#include <gsl/gsl_vector.h>

class SimpleSuggestionGenerator: public SuggestionGenerator {
    vector<int> nodesToSuggest;
    Interface* interface;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    SimpleEvaluator* seval;
public:
    SimpleSuggestionGenerator(BooleanDAG* _dag, Interface* _interface, map<string, int>& _ctrls): dag(_dag), interface(_interface), ctrls(_ctrls) {
        seval = new SimpleEvaluator(*dag, ctrls);
        seval->setInputs(interface);
        for (auto it = interface->varsMapping.begin(); it != interface->varsMapping.end(); it++) {
            int nodeid = it->second->nodeid;
            bool_node* n = (*dag)[nodeid];
            if (Util::hasArraccChild(n)) {
                nodesToSuggest.push_back(nodeid);
            }
        }
    }
    
    virtual vector<tuple<int, int, int>> getSatSuggestions(const gsl_vector* state) {
        vector<tuple<int, int, int>> suggestions;
        
        cout << "state: " << Util::print(state) << endl;
        seval->run(state);
        
        vector<tuple<double, int, int>> s;
        
        for (auto it = interface->varsMapping.begin(); it != interface->varsMapping.end(); it++) {
            int nodeid = it->second->nodeid;
            bool_node* n = (*dag)[nodeid];
            bool hasArraccChild = Util::hasArraccChild(n);
            
            double dist = seval->d(n);
            double cost = abs(dist);
            
            if (hasArraccChild) {
                cost = cost / 1000.0;
            }
            if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
                cout << n->lprint() << " = " <<  dist << ";";
                cost = cost - 5.0;
            }
            s.push_back(make_tuple(cost, nodeid, dist > 0));
            
        }
        cout << endl;
        
        sort(s.begin(), s.end());
        //cout << ((*dag)[imap[get<1>(s[0])]])->lprint() << " " << get<2>(s[0]) << endl;
        reverse(s.begin(), s.end());
        for (int k = 0; k < s.size(); k++) {
            int nodeid = get<1>(s[k]);
            if (!interface->hasValue(nodeid)) {
                suggestions.push_back(make_tuple(0, nodeid, get<2>(s[k])));
            }
        }
        int lastIdx = suggestions.size() - 1;
        if (lastIdx > 0) {
            cout << ((*dag)[get<1>(suggestions[lastIdx])])->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
        }
        return suggestions;
    }
    
    virtual vector<tuple<int, int, int>> getUnsatSuggestions(const gsl_vector* state) {
        vector<tuple<int, int, int>> suggestions;
        if (nodesToSuggest.size() == 0) return suggestions;
        int randIdx = nodesToSuggest[rand() % (nodesToSuggest.size())];
        int randVal = rand() % 2;
        cout << "Suggesting: " << ((*dag)[randIdx])->lprint() << " " << randVal << endl;
        suggestions.push_back(make_tuple(0, randIdx, randVal));
        return suggestions;

    }
};
