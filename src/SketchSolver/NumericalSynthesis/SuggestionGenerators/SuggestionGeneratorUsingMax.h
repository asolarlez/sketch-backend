#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include "OptimizationWrapper.h"
#include "MaxSolverWrapper.h"
#include "BoolNodeSimplifier.h"
#include <map>
#include <set>

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

class SuggestionGeneratorUsingMax: public SuggestionGenerator {
    Interface* interf;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    FloatManager& fm;

    ActualEvaluators* actualEval;
    SmoothEvaluators* smoothEval;

    OptimizationWrapper* opt;
    MaxSolverWrapper* maxOpt;

    int minimizeNode;
    set<int> assertConstraints;

    vector<gsl_vector*> dirGrads;

public:
    SuggestionGeneratorUsingMax(BooleanDAG* _dag, FloatManager& _fm, Interface* _interface, map<string, int>& _ctrls, OptimizationWrapper* _opt, MaxSolverWrapper* _maxOpt, ActualEvaluators* _actualEval, SmoothEvaluators* _smoothEval, int ncontrols): dag(_dag), fm(_fm), interf(_interface), ctrls(_ctrls), opt(_opt), maxOpt(_maxOpt), actualEval(_actualEval), smoothEval(_smoothEval) {

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

        dirGrads.push_back(gsl_vector_alloc(ncontrols));
        dirGrads.push_back(gsl_vector_alloc(ncontrols));
        dirGrads.push_back(gsl_vector_alloc(ncontrols));
        dirGrads.push_back(gsl_vector_alloc(ncontrols));
        dirGrads.push_back(gsl_vector_alloc(ncontrols));
    }
    ~SuggestionGeneratorUsingMax() {
        for (int i = 0; i < dirGrads.size(); i++) {
            gsl_vector_free(dirGrads[i]);
        }
    }

    pair<int, int> getSuggestion(const gsl_vector* state) {
        actualEval->run(state);
        
        vector<tuple<double, int, int>> s;
        
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
            if (interf->hasValue(nodeid)) continue;

            bool_node* n = (*dag)[nodeid];
            bool hasArraccChild = Util::hasArraccChild(n);
            
            double dist = actualEval->dist(n->id);
            double cost = abs(dist);
            if (hasArraccChild && cost < 0.1) {
                return make_pair(nodeid, dist >= 0.0);
            }
            
            if (hasArraccChild) {
                cost = cost / 1000.0;
            }
            
            s.push_back(make_tuple(cost, nodeid, dist > 0));
            
        }
        cout << endl;
        
        sort(s.begin(), s.end());
        if (s.size() > 0) {
            return make_pair(get<1>(s[0]), get<2>(s[1]));
        } else {
            Assert(false, "weqhqp");
            return make_pair(-1, 0);
        }
         
    }

    
    BooleanDAG* getNewDag(BooleanDAG* origDag, int origNid, const gsl_vector* s) {
        BooleanDAG* newDag = origDag->clone();
        SimpleEvaluator* seval = actualEval->getEvaluator(origDag);
        seval->run(s);
        BoolNodeSimplifier simplifier(*newDag, fm);
        simplifier.process(*newDag, origNid, seval);
        newDag->lprint(cout);
        return newDag;
    }
    
    Predicate* make_pure(Predicate* p, const gsl_vector* s) {
        if (p->isBasic()) {
            BasicPredicate* bp = (BasicPredicate*) p;
            BooleanDAG* newDag = getNewDag(bp->dag, bp->nid, s);
            // register newDag
            actualEval->addEvaluator(newDag);
            smoothEval->addEvaluator(newDag);
            return new BasicPredicate(newDag, newDag->size() - 2);
        } else {
            DiffPredicate* dp = (DiffPredicate*)p;
            Predicate* new_p1;
            Predicate* new_p2;
            if (dp->p1->isPure()) {
                new_p1 = dp->p1;
            } else {
                new_p1 = make_pure(dp->p1, s);
            }
            if (dp->p2->isPure()) {
                new_p2 = dp->p2;
            } else {
                new_p2 = make_pure(dp->p2, s);
            }
            return new DiffPredicate(new_p1, new_p2, max(ctrls.size(), 1));
        }
    }

    int getDirectionsLevel0(const gsl_vector* state, double beta) {
        GradUtil::BETA = beta;
        GradUtil::ALPHA = -beta;
        smoothEval->run(state);

        vector<tuple<double, Predicate*, int>> s;
               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
           
            double dist = smoothEval->dist(p);
            double cost = abs(dist);
            if (cost >= 0.1) {
                s.push_back(make_tuple(cost, p, dist > 0));
            }
        }

        sort(s.begin(), s.end());
        cout << "Distances: " << endl;
        int end = 5;
        if (end > s.size()) {
            end = s.size();
        }
        for (int i = 0; i < end; i++) {
            cout << get<1>(s[i])->print() << " " << get<0>(s[i]) << " " << get<2>(s[i]) << endl;
        }

        int numDirs = 0;
        for (int i = 0; i < end; i++) {
            if (numDirs == 5) break;
            double dist = get<0>(s[i]);
            Predicate* p = get<1>(s[i]);
            int val = !get<2>(s[i]);
            if (dist < 0.1) continue;
            smoothEval->getErrorOnPredicate(p, val, dirGrads[numDirs]);
            cout << "G: " << Util::print(dirGrads[numDirs]) << endl;
            bool newDir = true;
            for (int j = 0; j < numDirs; j++) {
                if (Util::sameDir(dirGrads[j], dirGrads[numDirs])) {
                    newDir = false;
                    cout << "Not a new dir" << endl;
                    break;
                }
            }
            if (newDir) {
                numDirs++;
            }
        }
        Assert(numDirs > 0, "No dirs?");
        return numDirs;
    }

    vector<tuple<double, Predicate*, int>> getDistancesLevel0(const gsl_vector* state) {
        actualEval->run(state);

        vector<tuple<double, Predicate*, int>> s;
               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
           
            double dist = actualEval->dist(p);
            double cost = abs(dist);
            s.push_back(make_tuple(cost, p, dist > 0));
        }

        sort(s.begin(), s.end());
        cout << "Distances: " << endl;
        int end = 5;
        if (end > s.size()) {
            end = s.size();
        }
        for (int i = 0; i < end; i++) {
            cout << get<1>(s[i])->print() << " " << get<0>(s[i]) << " " << get<2>(s[i]) << endl;
        }

        return s;
    }


    void getPredicatesNearToMaxLevel0(int idx,  double beta, gsl_vector* state, gsl_vector* dir, set<Predicate*>& preds) {
        cout << "Doing max along dir: " << Util::print(dir) << endl;
        maxOpt->maximize(interf, state, assertConstraints, minimizeNode, beta, dir, -1, idx); 
        gsl_vector* sol = maxOpt->getMinState();
        cout << "Max at " << Util::print(sol) << endl;
        const vector<tuple<double, Predicate*, int>>& distances = getDistancesLevel0(sol);
        int num_preds = min(4, distances.size());
        preds.insert(get<1>(distances[0]));
        for (int i = 1; i < num_preds; i++) {
            if (get<0>(distances[i]) < 0.5) {
                preds.insert(get<1>(distances[i]));
            }
        } 
    }

   

    int chooseBeta(LocalState* ls) {
       double error1 = ls->errors[0];
       double error2 = ls->errors[1];
       double error3 = ls->errors[2];
       cout << "Errors: " << error1 << " " << error2 << " " << error3 << endl;

       if (error3 <= 0.0) {
            Assert(false, "Zero error?");
       } 
       if (error3 < 1.0 || error2 <= 0.01) {
            return 2;
       }
       if (error2 < 5.0 || error1 <= 0.01) {
            return 1;
       } else {
            return 0;
       }
    }

    virtual IClause* getConflictClause(int level, LocalState * ls) { 
        Assert(level == -1, "Other levels is not yet supported");
        int betaIdx = chooseBeta(ls);
        double beta;
        gsl_vector* state;
        if (betaIdx == 0) {
            beta = -1;
            state = ls->localSols[0];
        } else if (betaIdx == 1) {
            beta = -10;
            state = ls->localSols[1];
        } else if (betaIdx == 2) {
            beta = -50;
            state = ls->localSols[2];
        }
        cout << "Choosing beta = " << beta << endl;
        int numDirs = getDirectionsLevel0(state, beta);
        cout << "Num dirs: " << numDirs << endl;
        set<Predicate*> preds;
        for (int i = 0; i < numDirs; i++) {
            getPredicatesNearToMaxLevel0(i, beta, state, dirGrads[i], preds);
        }

        actualEval->run(state);

        IClause* c = new IClause(GradUtil::counter);
        for (auto it = preds.begin(); it != preds.end(); it++) {
            int val = actualEval->dist(*it) < 0;
            if (!(*it)->isPure()) {
                c->add(make_pure(*it, state), val);
            } else {
                c->add(*it, val);
            }
        }
        if (c->size() == 0) {
            Assert(false, "Empty clause?");
        }
        return c;
    }

    
    
};
