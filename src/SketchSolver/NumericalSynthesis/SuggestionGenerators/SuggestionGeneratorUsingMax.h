#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include "OptimizationWrapper.h"
#include <map>
#include <set>

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

class SuggestionGeneratorUsingMax: public SuggestionGenerator {
    vector<int> nodesToSuggest;
    Interface* interf;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    SimpleEvaluator* seval;
    OptimizationWrapper* opt;
    int minimizeNode;
    set<int> assertConstraints;
public:
    SuggestionGeneratorUsingMax(BooleanDAG* _dag, Interface* _interface, map<string, int>& _ctrls, OptimizationWrapper* _opt): dag(_dag), interf(_interface), ctrls(_ctrls), opt(_opt) {
        seval = new SimpleEvaluator(*dag, ctrls);
        seval->setInputs(interf);
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
            bool_node* n = (*dag)[nodeid];
            if (Util::hasArraccChild(n)) {
                nodesToSuggest.push_back(nodeid);
            }
        }

        minimizeNode = -1;

	    for (int i = 0; i < dag->size(); i++) { // TODO: this should also be set by caller class
	    	bool_node* n = (*dag)[i];
	    	if (n->type == bool_node::ASSERT && ((ASSERT_node*)n)->isHard()) {
	    		minimizeNode = i;
	    	} else if (n->type == bool_node::ASSERT || Util::isSqrt(n)) {
	    		assertConstraints.insert(i);
	    	} else if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
	    		assertConstraints.insert(i);
	    	}
	    }
    }
    
    virtual vector<tuple<int, int, int>> getSatSuggestions(const gsl_vector* state) {
        vector<tuple<int, int, int>> suggestions;
        
        //cout << "state: " << Util::print(state) << endl;
        seval->run(state);
        
        vector<tuple<double, int, int>> s;
        
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
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
            int nodeid = get<1>(suggestions[lastIdx]);
            bool_node* n = (*dag)[nodeid];
            cout << n->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
            cout << "val: " << seval->d(n) << endl;
        }
        return suggestions;
    }

    vector<tuple<double, int, int>> getDistances(const gsl_vector* state) {
    	seval->run(state);

		vector<tuple<double, int, int>> s;
                
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
            bool_node* n = (*dag)[nodeid];
            
            double dist = seval->d(n);
            double cost = abs(dist);
            s.push_back(make_tuple(cost, nodeid, dist > 0));
        }

        sort(s.begin(), s.end());
        cout << "Distances: " << endl;
        int end = 5;
        if (end > s.size()) {
        	end = s.size();
        }
        for (int i = 0; i < end; i++) {
        	cout << (*dag)[get<1>(s[i])]->lprint() << " " << get<0>(s[i]) << " " << get<2>(s[i]) << endl;
        }

        return s;
    }
    
    virtual void initUnsatSuggestions(LocalState* state) { }
    virtual pair<int, int> getNextUnsatSuggestion() {
        return make_pair(-1, 0);
    }
    vector<tuple<int, int, int>> getUnsatSuggestions(const gsl_vector* state) {
        vector<tuple<int, int, int>> suggestions;
        const vector<tuple<double, int, int>>& distances = getDistances(state);
        
        vector<tuple<double, int, int>> s;


        bool solved = opt->maximize(interf, state, assertConstraints, minimizeNode, -1, get<1>(distances[0]), !get<2>(distances[0])); 
        gsl_vector* sol = opt->getMinState();
        cout << "Max at " << Util::print(sol) << endl;
        const vector<tuple<double, int, int>>& distances1 = getDistances(sol);



        solved = opt->maximize(interf, state, assertConstraints, minimizeNode, -1, get<1>(distances[1]), !get<2>(distances[1])); 
        sol = opt->getMinState();
        cout << "Max at " << Util::print(sol) << endl;
        const vector<tuple<double, int, int>>& distances2 = getDistances(sol);

        seval->run(state);
        for (int i = 0; i < 5; i++) {
        	double cost = get<0>(distances1[i]);
        	int nodeid = get<1>(distances1[i]);
        	int val = seval->d((*dag)[nodeid]) < 0;
        	s.push_back(make_tuple(cost, nodeid, val));
        }

        for (int i = 0; i < 5; i++) {
        	double cost = get<0>(distances2[i]);
        	int nodeid = get<1>(distances2[i]);
        	int val = seval->d((*dag)[nodeid]) < 0;
        	s.push_back(make_tuple(cost, nodeid, val));
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
            int nodeid = get<1>(suggestions[lastIdx]);
            bool_node* n = (*dag)[nodeid];
            cout << n->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
            cout << "val: " << seval->d(n) << endl;
        }
        return suggestions;

    }
};
