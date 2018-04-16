#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include <map>
#include <set>
#include <gsl/gsl_vector.h>

class SmartSuggestionGenerator1: public SuggestionGenerator {
    Interface* interface;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    SimpleEvaluator* seval;
    const vector<vector<int>>& dependentInputs;
public:
    SmartSuggestionGenerator1(BooleanDAG* _dag, Interface* _interface, map<string, int>& _ctrls, const vector<vector<int>>& _dependentInputs): dag(_dag), interface(_interface), ctrls(_ctrls), dependentInputs(_dependentInputs) {
        seval = new SimpleEvaluator(*dag, ctrls);
        seval->setInputs(interface);
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
        cout << "state: " << Util::print(state) << endl;
        seval->run(state);
        
        double minNegDist = GradUtil::MAXVAL;
        int minNegDistId = -1;
        for (int i = 0; i < dag->size(); i++) {
            bool_node* n = (*dag)[i];
            if (n->type == bool_node::ASSERT) {
                double dist = seval->d(n);
                if (dist < 0.0) {
                    if (dist < minNegDist) {
                        minNegDist = dist;
                        minNegDistId = n->id;
                    }
                }
            }
        }
        
        cout << "MinNegNode: " << minNegDistId << " with error " << minNegDist << endl;
        
        const vector<int>& influentialInputs = dependentInputs[minNegDistId];
        cout << influentialInputs.size() << endl;
        int randInput = influentialInputs[rand() % influentialInputs.size()];
        double inputDist = seval->d((*dag)[randInput]);
        int val = inputDist >= 0.0 ? 0 : 1;
        cout << "Suggesting: " << ((*dag)[randInput])->lprint() << " " << val << endl;
        suggestions.push_back(make_tuple(0, randInput, val));
        return suggestions;
        
    }
};
