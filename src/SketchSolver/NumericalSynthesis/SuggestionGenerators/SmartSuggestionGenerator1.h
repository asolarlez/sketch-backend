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

class SmartSuggestionGenerator1: public SuggestionGenerator {
    Interface* interf;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    SimpleEvaluator* seval;
    const vector<vector<int>>& dependentInputs;
    const vector<vector<int>>& dependentCtrls;
    vector<int> inputsToAsserts; // TODO: this is pretty local hack
    int counter;
public:
    SmartSuggestionGenerator1(BooleanDAG* _dag, Interface* _interface, map<string, int>& _ctrls, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls): dag(_dag), interf(_interface), ctrls(_ctrls), dependentInputs(_dependentInputs), dependentCtrls(_dependentCtrls) {
        seval = new SimpleEvaluator(*dag, ctrls);
        seval->setInputs(interf);
        counter = 0;
        inputsToAsserts.resize(dag->size(), -1);
        for (int i = 0; i < dag->size(); i++) {
            bool_node* n = (*dag)[i];
            if (n->type == bool_node::ASSERT) {
                const auto& inputs = dependentInputs[i];
				for (auto it = inputs.begin(); it != inputs.end(); ++it) {
                    inputsToAsserts[*it] = i;
                }
            }
        }
    }
    
    virtual vector<tuple<int, int, int>> getSatSuggestions(const gsl_vector* state) {
        vector<tuple<int, int, int>> suggestions;
        
        cout << "state: " << Util::print(state) << endl;
        seval->run(state);
        
        vector<tuple<double, int, int>> s;
        
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
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
            if (!interf->hasValue(nodeid)) {
                suggestions.push_back(make_tuple(0, nodeid, get<2>(s[k])));
            }
        }
        int lastIdx = suggestions.size() - 1;
        if (lastIdx > 0) {
            cout << ((*dag)[get<1>(suggestions[lastIdx])])->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
        }
        return suggestions;
    }
    
    set<bool_node*> getResponsibleNodes(bool_node* node) {
        Assert(node->type == bool_node::ASSERT, "dsfqhuopwqe");
        set<bool_node*> responsibleNodes;
        vector<bool_node*> nodes; // queue
        nodes.push_back(node);
        
        while(nodes.size() > 0) {
            bool_node* n = nodes.back();
            nodes.pop_back();
            Assert(n->type == bool_node::LT || n->type == bool_node::ASSERT || n->type == bool_node::NOT || n->type == bool_node::AND || n->type == bool_node::OR, "Unexpected node type");
            if (n->type == bool_node::LT) {
                responsibleNodes.insert(n);
            }
            if (n->type == bool_node::ASSERT || n->type == bool_node::NOT) {
                nodes.push_back(n->mother);
            }
            if (n->type == bool_node::AND || n->type == bool_node::OR) {
                double dist = seval->d(n);
                double mdist = seval->d(n->mother);
                double fdist = seval->d(n->father);
                if (dist * mdist > 0.0) {
                    nodes.push_back(n->mother);
                }
                if (dist * fdist > 0.0) {
                    nodes.push_back(n->father);
                }
            }
        }
        return responsibleNodes;
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
        
        cout << "MinNegNode: " << (*dag)[minNegDistId]->lprint() << " with error " << minNegDist << endl;
        
        /*cout << "python test.py " << counter++ << " \"" << Util::print(state) << "\" data.txt";
        string  assertMsg = ((ASSERT_node*)(*dag)[minNegDistId])->getMsg();
        size_t start = assertMsg.find("(");
        if (start != string::npos) {
            size_t end = assertMsg.find(")");
            cout << " \"" << assertMsg.substr(start, end - start + 1) << "\"";
        }
        
        const set<int>& inputConstraints = interface->getInputConstraints();
        cout << " \"";
        for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
            string line = to_string(dependentCtrls[*it][0]);
            string assertMsg = ((ASSERT_node*)(*dag)[inputsToAsserts[*it]])->getMsg();
            string point = "";
            size_t start = assertMsg.find("(");
            if (start != string::npos) {
                size_t end = assertMsg.find(")");
                point = assertMsg.substr(start, end - start + 1);
            }
            cout << line << ":" << point << ":" << interface->getValue(*it) << ";" ;
        }
        cout << "\"" << endl;*/
        
        map<int, int> ctrlsToScore;
        for (int i = 0; i < dag->size(); i++) {
            bool_node* n = (*dag)[i];
            if (n->type == bool_node::ASSERT) {
                bool satisfied = seval->d(n) >= 0.0;
                const set<bool_node*>& responsibleNodes = getResponsibleNodes(n);
                for (auto it = responsibleNodes.begin(); it != responsibleNodes.end(); it++) {
                    const vector<int>& ctrls = dependentCtrls[(*it)->id];
                    int cost = satisfied ? 1 : -1;
                    for (int j = 0; j < ctrls.size(); j++) {
                        if (ctrlsToScore.find(ctrls[j]) == ctrlsToScore.end()) {
                            ctrlsToScore[ctrls[j]] = cost;
                        } else {
                            ctrlsToScore[ctrls[j]] += cost;
                        }
                    }
                }
            }
        }
        
        for (auto it = ctrlsToScore.begin(); it != ctrlsToScore.end(); it++) {
            cout << "(" <<  it->first << "," << it->second << "); ";
        }
        cout << endl;
        
        const set<bool_node*>& influentialInputs = getResponsibleNodes((*dag)[minNegDistId]); // this should be a subset of dependentInputs of minNegDistId
        
        vector<tuple<int, int, int>> s;
        for (auto it = influentialInputs.begin(); it != influentialInputs.end(); it++) {
            int inputId = (*it)->id;
            double inputDist = seval->d(*it);
            int val = inputDist >= 0.0 ? 0 : 1;
            const vector<int>& ctrls = dependentCtrls[inputId];
            int cost = 0;
            for (int j = 0; j < ctrls.size(); j++) {
                if (ctrlsToScore.find(ctrls[j]) != ctrlsToScore.end()) {
                    cost += ctrlsToScore[ctrls[j]];
                }
            }
            s.push_back(make_tuple(cost, inputId, val));
            cout << (*dag)[inputId]->lprint() << " " << seval->d((*dag)[inputId]) << "  [" << Util::print(dependentCtrls[inputId])  << "] " << cost << endl;
        }
        sort(s.begin(), s.end());
        reverse(s.begin(), s.end());
        for (int k = 0; k < s.size(); k++) {
            int nodeid = get<1>(s[k]);
            if (!interf->hasValue(nodeid)) {
                suggestions.push_back(make_tuple(0, nodeid, get<2>(s[k])));
            }
        }
        int lastIdx = suggestions.size() - 1;
        if (lastIdx > 0) {
            cout << "Suggesting " << ((*dag)[get<1>(suggestions[lastIdx])])->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
        }

        return suggestions;
        
    }
};
