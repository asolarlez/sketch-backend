#pragma once

#include "SimpleEvaluator.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SuggestionGenerator.h"
#include "OptimizationWrapper.h"
#include "MaxOptimizationWrapper.h"
#include "BoolNodeSimplifier.h"
#include "NumDebugger.h"
#include <map>
#include <set>

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

#include "BoolBasedSampler.h"

class SuggestionGeneratorUsingMax: public SuggestionGenerator {
    Interface* interf;
    BooleanDAG* dag;
    map<string, int>& ctrls;
    FloatManager& fm;

    ActualEvaluators* actualEval;
    SmoothEvaluators* smoothEval;

    OptimizationWrapper* opt;
    MaxOptimizationWrapper* maxOpt;

    int minimizeNode;
    set<int> assertConstraints;

    vector<gsl_vector*> maxes;
    gsl_vector* tmp;

    int NUM_MAXES = 5;

    NumDebugger* debugger;
    const vector<vector<int>>& dependentInputs;
    

public:
    vector<gsl_vector*> dirGrads;
    SuggestionGeneratorUsingMax(BooleanDAG* _dag, FloatManager& _fm, Interface* _interface, map<string, int>& _ctrls, OptimizationWrapper* _opt, MaxOptimizationWrapper* _maxOpt, ActualEvaluators* _actualEval, SmoothEvaluators* _smoothEval, int ncontrols, const vector<vector<int>>& _dependentInputs, NumDebugger* _debugger): dag(_dag), fm(_fm), interf(_interface), ctrls(_ctrls), opt(_opt), maxOpt(_maxOpt), actualEval(_actualEval), smoothEval(_smoothEval), dependentInputs(_dependentInputs), debugger(_debugger) {

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

        for (int i = 0; i < 20; i++) {
            dirGrads.push_back(gsl_vector_alloc(ncontrols));
        }
        

        for (int i = 0; i < NUM_MAXES; i++) {
            maxes.push_back(gsl_vector_alloc(ncontrols));
        }
        tmp = gsl_vector_alloc(ncontrols);
    }
    ~SuggestionGeneratorUsingMax() {
        for (int i = 0; i < dirGrads.size(); i++) {
            gsl_vector_free(dirGrads[i]);
        }

        for (int i = 0; i < maxes.size(); i++) {
            gsl_vector_free(maxes[i]);
        }
        gsl_vector_free(tmp);
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

    pair<int, int> getUnsatSuggestion(const gsl_vector* state) {
        actualEval->run(state);
        double dassert;
        double d;
        vector<tuple<int, int>> conflict; 
        for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
            //dassert = actualEval->getErrorOnConstraint(*it);
            if (true) {
                // TODO: this is currently hacky
                const vector<int>& inputs = dependentInputs[*it];
                int minid = -1;
                double minerror  = 1e30;
                for (auto it1 = inputs.begin(); it1 != inputs.end(); it1++) {
                    if (interf->hasValue(*it1)) continue;
                    d = actualEval->dist(*it1);
                    //if (dassert < 0.0) {
                        cout << *it << " " << *it1 << " " << d << " " << Util::print(actualEval->grad(*it1)) << endl;
                    //}
                    if (d < minerror) {
                        minerror = d;
                        minid = *it1;
                    } 
                }
                //Assert(minerror > 0.0, "something is wrong");
                if (minid != -1) {
                    cout << "Min " << *it << " " << minid << " " << minerror << endl;
                    conflict.push_back(make_tuple(*it, minid));
                }

                if ((*dag)[*it]->mother()->type == bool_node::NOT &&  minerror >= 0.2) {
                    break;
                }
            }
            
        }
        sort(conflict.begin(), conflict.end());
        reverse(conflict.begin(), conflict.end());
        Assert(conflict.size() > 0, "No failed asserts???");
        return make_pair(get<1>(conflict[0]), 1);

    }

    SClause* getUnsatClause(const gsl_vector* state, const gsl_vector* initState) {  
        actualEval->run(state);
        double dassert;
        double d;
        vector<int> conflictingAsserts;
        for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
            dassert = actualEval->getErrorOnConstraint(*it);
            if ( (*dag)[*it]->mother()->type != bool_node::NOT) {
                continue;
            }
            if (true) {
                if (dassert < 0.01) {
                    conflictingAsserts.push_back(*it);
                }
                if (dassert < -0.2) {
                    break;
                }
            }
            
        }

        SClause* clause = new SClause();

        actualEval->run(initState);
        for (auto it = conflictingAsserts.begin(); it != conflictingAsserts.end(); it++) {
                // TODO: this is currently hacky
                const vector<int>& inputs = dependentInputs[*it];
                int minid = -1;
                double minerror  = 1e30;
                for (auto it1 = inputs.begin(); it1 != inputs.end(); it1++) {
                    d = actualEval->dist(*it1);
                    cout << *it << " " << *it1 << " " << d << " " << Util::print(actualEval->grad(*it1)) << endl;
                    if (d < minerror) {
                        minerror = d;
                        minid = *it1;
                    } 
                }
                //Assert(minerror > 0.0, "something is wrong");
                if (minid != -1 ) {
                    clause->add(*it, minid);
                }
        }

        
        return clause;

    }

    SClause* getUnsatClause(const gsl_vector* state) {  
        actualEval->run(state);
        double dassert;
        double d;
        SClause* clause = new SClause();
        for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
            dassert = actualEval->getErrorOnConstraint(*it);
            if (true) {
                // TODO: this is currently hacky
                const vector<int>& inputs = dependentInputs[*it];
                int minid = -1;
                double minerror  = 1e30;
                for (auto it1 = inputs.begin(); it1 != inputs.end(); it1++) {
                    d = actualEval->dist(*it1);
                    if (dassert < 0.0) {
                        cout << *it << " " << *it1 << " " << d << " " << Util::print(actualEval->grad(*it1)) << endl;
                    }
                    if (d < minerror) {
                        minerror = d;
                        minid = *it1;
                    } 
                }
                //Assert(minerror > 0.0, "something is wrong");
                if (minid != -1 && (*dag)[*it]->mother()->type == bool_node::NOT &&  minerror >= -0.01) {
                    clause->add(*it, minid);
                }

                if ((*dag)[*it]->mother()->type == bool_node::NOT &&  minerror >= 0.2) {
                    break;
                }
            }
            
        }
        
        return clause;

    }

    
    BooleanDAG* getNewDag(BooleanDAG* origDag, int origNid, const gsl_vector* s) {
        BooleanDAG* newDag = origDag->clone();
        SimpleGradEvaluator* seval = actualEval->getEvaluator(origDag);
        seval->run(s);
        BoolNodeSimplifier simplifier(*newDag, fm);
        simplifier.process(*newDag, origNid, seval);
        DagOptim cse(*newDag, fm);
        cse.process(*newDag);
        newDag->cleanup();
        //newDag->lprint(cout);
        return newDag;
    }
    
    Predicate* make_pure(Predicate* p, const gsl_vector* s) {
        cout << "Making pure: " << p->print() << " at " << Util::print(s) << endl;
        if (p->isBasic()) {
            BasicPredicate* bp = (BasicPredicate*) p;
            BooleanDAG* newDag = getNewDag(bp->dag, bp->nid, s);
            // register newDag
            actualEval->addEvaluator(newDag);
            smoothEval->addEvaluator(newDag);
            int cid = -1;
            for (int i = newDag->size() - 1; i >= 0; i--) {
                bool_node* n = (*newDag)[i];
                if (n->type == bool_node::ASSERT) {
                    cid = n->mother()->id;
                    break;
                }
            }
            Assert(cid != -1, "No assert in new dag");
            return new BasicPredicate(newDag, cid, bp);
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
            int ncontrols = ctrls.size();
            if (ncontrols == 0) {
                ncontrols = 1;
            }
            return new DiffPredicate(new_p1, new_p2, ncontrols);
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

    void updateState(gsl_vector* state, int idx, double dx) {
        gsl_vector_set(state, idx, gsl_vector_get(state, idx) + dx);
    }

    void randomize(gsl_vector* tmp) {
        for (int i = 0; i < tmp->size; i++) {
            double r = -1.0 + (rand() % 20)/10.0;
            gsl_vector_set(tmp, i, r);
        }
    }

    int getMaxes(gsl_vector* state, double beta) {
        int numMaxes = 0;
        for (int i = 0; i < NUM_MAXES; i++) {
            randomize(tmp);
            gsl_vector_scale(tmp, 1.0/gsl_blas_dnrm2(tmp));
            gsl_vector_add(state, tmp);
            cout << Util::print(state) << endl;
            maxOpt->maximize(interf, state, tmp, assertConstraints, minimizeNode, beta, -1, i);
            gsl_vector_memcpy(maxes[i], maxOpt->getMinState());
            gsl_vector_sub(state, tmp);
            cout << Util::print(state) << endl;
            numMaxes++;
        }
        return numMaxes; 
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

    /*virtual IClause* getConflictClause(int level, LocalState* ls) {
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

        vector<tuple<double, Predicate*, int>> preds;
        actualEval->run(state);               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
            cout << p->print() << " ";
            double dist = actualEval->dist(p);
            cout << dist << " ";
            cout << Util::print(actualEval->grad(p)) << endl;
            double cost = abs(dist);
            double norm = actualEval->grad_norm(p);
            if (norm < 0.01) continue;
            if (cost/norm < 0.01) continue;
            preds.push_back(make_tuple(cost/norm, p, dist >= 0));
        }
        sort(preds.begin(), preds.end());

        int numPreds = min(20, preds.size());
        IClause* c = new IClause(GradUtil::counter);
        for (int i = 0; i < numPreds; i++) {
            Predicate* p = get<1>(preds[i]);
            cout << p->print() << " ";
            if (!p->isPure()) {
                p = make_pure(p, state);
            }
            c->add(p, !get<2>(preds[i]));
        }
        cout << endl;

        if (c->size() == 0) {
            Assert(false, "Empty clause?");
        }
        return c;
    }*/

    virtual IClause* getConflictClause(int level, LocalState* ls) {
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
        int numMaxes = getMaxes(state, beta);
        cout << "Num maxes: " << numMaxes << endl;

        map<int, vector<tuple<double, Predicate*, int>>> maxesToPredicates;
        map<Predicate*, vector<tuple<double, int, int>>> predicatesToMaxes;

        map<Predicate*, tuple<double, int>> lsPreds;

        actualEval->run(state);
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
            double dist = actualEval->dist(p);
            double cost = abs(dist);
            double norm = actualEval->grad_norm(p);
            if (norm < 0.01) {
                cost = 1000.0;
            } else {
                cost = cost/norm;
            }
            lsPreds[p] = make_tuple(cost, dist >= 0);
        }

        for (int i = 0; i < numMaxes; i++) {
            actualEval->run(maxes[i]);
            vector<tuple<double, Predicate*, int>> preds;
            for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
                Predicate* p = *it;
                double dist = actualEval->dist(p);
                double cost = abs(dist);
                double norm = actualEval->grad_norm(p);
                if (norm < 0.01) {
                    cost = 1000.0;
                } else {
                    cost = cost/norm;
                }
                auto& lsTup = lsPreds[p];
                bool same = false;
                if (get<1>(lsTup) == (dist > 0)) {
                    same = true;
                }
                if (get<0>(lsTup) <= 0.1) {
                    same = true;
                }
                if (cost <= 0.1) {
                    same = true;
                }
                if (!same) {
                    preds.push_back(make_tuple(cost, p, dist >= 0));
                    if (predicatesToMaxes.find(p) == predicatesToMaxes.end()) {
                        vector<tuple<double, int, int>> maxstates;
                        predicatesToMaxes[p] = maxstates;
                    } 
                    predicatesToMaxes[p].push_back(make_tuple(cost, i, dist>=0));
                }

            }
            sort(preds.begin(), preds.end());
            maxesToPredicates[i] = (preds);
        }

        set<int> selected;

        IClause* c = new IClause(GradUtil::counter);

        for (int i = 0; i < numMaxes; i++) {
            if (selected.find(i) != selected.end()) continue;
            auto& l = maxesToPredicates[i];
            if (l.size() == 0) continue;
            //Assert(l.size() != 0, "No preds to eliminate max");
            auto& tup = l[0];
            Predicate* p = get<1>(tup);
            auto& m = predicatesToMaxes[p];
            selected.insert(i);
            for (int k = 0; k < m.size(); k++) {
                selected.insert(get<1>(m[k]));
            }
            if (!p->isPure()) {
                p = make_pure(p, state);
            }
            c->add(p, get<2>(tup));
        }

        if (c->size() == 0) {
            Assert(false, "Empty clause?");
        }

        // add predicates that satisfy all maxes and the current local state
        vector<tuple<double, Predicate*>> preds;
        actualEval->run(state);               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
            auto& m = predicatesToMaxes[p];
            if (m.size() != 0) {
                continue;
            }
            double dist = actualEval->dist(p);
            double cost = abs(dist);
            double norm = actualEval->grad_norm(p);
            if (norm < 0.01) continue;
            preds.push_back(make_tuple(cost/norm, p));
        }
        sort(preds.begin(), preds.end());
        int numDirs = 0;
        vector<double> dists;
        dists.resize(dirGrads.size());
        for (int i = 0; i < preds.size(); i++) {
            if (numDirs >= dirGrads.size()) break;
            Predicate* p = get<1>(preds[i]);
            dists[numDirs] = actualEval->dist(p);
            gsl_vector_memcpy(dirGrads[numDirs], actualEval->grad(p));
            bool newDir = true;
            for (int j = 0; j < numDirs; j++) {
                if (Util::sameDir(dirGrads[j], dirGrads[numDirs])) {
                    if (dists[j] * dists[numDirs] >= 0.0) {
                        newDir = false;
                        //cout << "Not a new dir" << endl;
                        break;
                    }
                }
            }
            if (newDir) {
                cout << "New dir from ls: " << p->print() << " " << actualEval->dist(p) << " " << Util::print(actualEval->grad(p)) << endl;
                if (!p->isPure()) {
                    p = make_pure(p, state);
                }

                c->add(p, dists[numDirs] < 0);

                numDirs++;
            }
        }
        return c;


    }

    /*virtual IClause* getConflictClause(int level, LocalState* ls) {
        gsl_vector* state = ls->localSols[2];

        vector<tuple<double, Predicate*, int>> preds;
        actualEval->run(state);               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
            double dist = actualEval->dist(p);
            double cost = abs(dist);
            double norm = actualEval->grad_norm(p);
            if (norm < 0.01) continue;
            if (cost/norm < 0.1) continue;
            preds.push_back(make_tuple(cost/norm, p, dist >= 0));
        }

        int numPreds = min(5, preds.size());
        IClause* c = new IClause(GradUtil::counter);
        for (int i = 0; i < numPreds; i++) {
            int ridx = rand() % preds.size();
            Predicate* p = get<1>(preds[ridx]);
            cout << p->print() << " " << actualEval->dist(p) << " " << Util::print(actualEval->grad(p)) << endl;
            if (!p->isPure()) {
                p = make_pure(p, state);
            }
            c->add(p, !get<2>(preds[ridx]));
        }

        if (c->size() == 0) {
            Assert(false, "Empty clause?");
        }
        return c;
        
    }*/

    /*virtual IClause* getConflictClause(int level, LocalState * ls) { 
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
        int numMaxes = getMaxes(state, beta);
        cout << "Num maxes: " << numMaxes << endl;
        
        // collect close preds 
        set<Predicate*> close_preds;

        vector<tuple<double, Predicate*>> preds;
        actualEval->run(state);               
        for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            Predicate* p = *it;
            double dist = actualEval->dist(p);
            double cost = abs(dist);
            double norm = actualEval->grad_norm(p);
            if (norm < 0.01) continue;
            preds.push_back(make_tuple(cost/norm, p));
        }
        sort(preds.begin(), preds.end());
        int numDirs = 0;
        vector<double> dists;
        dists.resize(dirGrads.size());
        for (int i = 0; i < preds.size(); i++) {
            if (numDirs >= dirGrads.size()) break;
            Predicate* p = get<1>(preds[i]);
            dists[numDirs] = actualEval->dist(p);
            gsl_vector_memcpy(dirGrads[numDirs], actualEval->grad(p));
            bool newDir = true;
            for (int j = 0; j < numDirs; j++) {
                if (Util::sameDir(dirGrads[j], dirGrads[numDirs])) {
                    if (dists[j] * dists[numDirs] >= 0.0) {
                        newDir = false;
                        //cout << "Not a new dir" << endl;
                        break;
                    }
                }
            }
            if (newDir) {
                cout << "New dir from ls: " << p->print() << " " << actualEval->dist(p) << " " << Util::print(actualEval->grad(p)) << endl;
                numDirs++;
                if (p->isPure()) {
                    close_preds.insert(p);
                } else {
                    p = make_pure(p, state);
                    close_preds.insert(p);
                }
            }
        }

        for (int k = 0; k < numMaxes; k++) {
            preds.clear();
            actualEval->run(maxes[k]);
            for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
                Predicate* p = *it;

                double dist = actualEval->dist(p);
                double cost = abs(dist);
                double norm = actualEval->grad_norm(p);
                if (norm < 0.01) continue;
                preds.push_back(make_tuple(cost/norm, p));
            }
            sort(preds.begin(), preds.end());
            int numDirs = 0;
            for (int i = 0; i < preds.size(); i++) {
                if (numDirs >= 20) break;
                Predicate* p = get<1>(preds[i]);
                dists[numDirs] = actualEval->dist(p);
                gsl_vector_memcpy(dirGrads[numDirs], actualEval->grad(p));
                bool newDir = true;
                for (int j = 0; j < numDirs; j++) {
                    if (Util::sameDir(dirGrads[j], dirGrads[numDirs])) {
                        if (dists[j] * dists[numDirs] >= 0.0) {
                            newDir = false;
                            //cout << "Not a new dir" << endl;
                            break;
                        }
                    }
                }
                if (newDir) {
                    cout << "New dir from max: " << p->print() << " " << actualEval->dist(p) << " " << Util::print(actualEval->grad(p)) << endl;
                    numDirs++;
                    if (p->isPure()) {
                        close_preds.insert(p);
                    } else {
                        p = make_pure(p, maxes[k]);
                        close_preds.insert(p);
                    }
                }
            }
        }   
        // collect common conditions satisfied by state, maxes
        map<Predicate*, vector<tuple<double, int>>> common_preds;

        actualEval->run(state);               
        for (auto it = close_preds.begin(); it != close_preds.end(); it++) {
            Predicate* p = *it;
            cout << "P: " << p->print() << " ";
            double dist = actualEval->dist(p);
            cout << dist << " ";
            cout << Util::print(actualEval->grad(p)) << endl;
            double cost = abs(dist);
            int val = dist >= 0;
            vector<tuple<double, int>> costValPairs;
            costValPairs.push_back(make_tuple(cost, val));
            common_preds[p] = costValPairs;
        }
 
        for (int i = 0; i < numMaxes; i++) {
            actualEval->run(maxes[i]);
            for (auto it = close_preds.begin(); it != close_preds.end(); it++) {
                Predicate* p = *it;

                double dist = actualEval->dist(p);
                double cost = abs(dist);
                int val = dist >= 0;
                common_preds[p].push_back(make_tuple(cost, val));
            }
        }

        vector<tuple<double, Predicate*, int>> ss;

        for (auto it = common_preds.begin(); it != common_preds.end(); it++) {
            Predicate* p = it->first;

            vector<tuple<double, int>>& pairs = common_preds[p];
            int val = -1;
            double cost = 1e10;

            cout << "Predicate " << p->print() << " " ;

            for (int i = 0; i < pairs.size(); i++) {
                auto& tup = pairs[i];
                double tcost = get<0>(tup);
                double tval = get<1>(tup);
                cout << "(" << tval << "," << tcost << ")" << " ";
                if (tcost < cost) {
                    cost = tcost;
                }
                if (tcost > 0.01) {
                    if (val == -1) {
                        val = tval;
                    } else if (val != tval) {
                        val = -2;
                        break;
                    }
                }
            }
            cout << "Final: " << val << "," << cost << endl;
            if (val == 0 || val == 1) {
                ss.push_back(make_tuple(cost, p, val));
            }
        }
        sort(ss.begin(), ss.end());

        cout << "Num common preds: " << ss.size() << endl;

        int numPreds = min(20, ss.size());
        IClause* c = new IClause(GradUtil::counter);
        for (int i = 0; i < numPreds; i++) {
            c->add(get<1>(ss[i]), !get<2>(ss[i]));
        }

        if (c->size() == 0) {
            Assert(false, "Empty clause?");
        }
        return c;
    }*/

    
    
};
