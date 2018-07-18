#pragma once 

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif

#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "FloatSupport.h"
#include "BooleanDAG.h"
#include "Interface.h"
#include "SnoptWrapper.h"
#include "OptimizationWrapper.h"
#include "ConflictGenerator.h"
#include "SuggestionGenerator.h"
#include "SimpleEvaluator.h"
#include "CommandLineArgs.h"
#include "SymbolicEvaluator.h"

#include <unordered_map>


struct pair_hash {
	
	std::size_t operator () (const std::pair<int, int> &p) const {
		return  std::hash<size_t >{}((size_t)p.first << 31 | (unsigned int)p.second);
	}
};



class SetSet {
public:
	typedef int set;
private:
	vector<pair<set, set> > store;
	unordered_map<pair<set, set>, set, pair_hash> index;
	vector<vector<int > > values;
	vector<int> emptyS;
	vector<int> single;
public:
	typedef vector<int>::iterator iterator;

	set singleton(int id) {
		Assert(id >= 0, "You can only store positive ints");
		return id;
	}

	set empty() {
		return -1;
	}

	//return false if b is a subset of a.
	bool merge(vector<int>& a, vector<int>& b, vector<int>& out) {
		bool rv = false;
		auto ia = a.begin();
		auto ib = b.begin();
		while (ia < a.end() || ib < b.end()) {
			if (ia < a.end() && ib < b.end() &&  *ia < *ib) {
				out.push_back(*ia); 
				++ia;
				continue;
			}
			if (ia < a.end() && ib < b.end() && *ib < *ia) {
				out.push_back(*ib);
				++ib;
				rv = true;
				continue;
			}
			if (ia < a.end() && ib < b.end() && *ib == *ia) {
				out.push_back(*ib);
				++ib;
				++ia;
				continue;
			}
			if (ia < a.end()) {
				out.push_back(*ia); 
				++ia;
			}
			if (ib < b.end()) {
				out.push_back(*ib);
				++ib;
				rv = true;
			}
		}
		return rv;
	}

	bool insert(vector<int>& a, int ii, vector<int>& out) {
		bool added = false;
		bool rv = false;
		for (auto ia = a.begin(); ia < a.end(); ++ia) {			
			if (ii < *ia && !added) {
				out.push_back(ii);
				added = true;
				rv = true;
			}
			if (ii == *ia) {
				added = true;								
			}	
			out.push_back(*ia);
		}
		if (!added) {
			out.push_back(ii);
			rv = true;
		}
		return rv;
	}

	bool merge(set s1, set s2, vector<int>& out) {
		if (s1 < 0) {
			if (s2 < 0) {
				return merge(values[-s1 - 2], values[-s2 - 2], out);
				
			}
			else {
				return insert(values[-s1 - 2], s2, out);
			}
		}
		else {
			//s1 > 0
			if (s2 > 0) {
				out.push_back(min(s1, s2));
				out.push_back(max(s1, s2));
				return true;
			}
			else {
				return insert(values[-s2 - 2], s1, out);
			}
		}
	}

	set s_union(set s1, set s2) {
		if (s1 == -1) {
			return s2;
		}
		if (s2 == -1) {
			return s1;
		}
		if (s1 == s2) {
			return s1;
		}
		auto p = make_pair(min(s1, s2), max(s1, s2));
		auto it = index.find(p);
		if (it == index.end()) {
			auto sz = store.size();
			index[p] = -2 - sz;
			store.push_back(p);
			values.push_back(vector<int>());
			if (!merge(s1, s2, values[sz])) {
				// This means one of them was a singleton, and was contained in the other.
				values.pop_back();
				store.pop_back();
				if (s1 >= 0) {
					index[p] = s2;
					return s2;
				}
				else {
					index[p] = s1;
					return s1;
				}
			}

			return -2-sz;
		}
		else {
			return it->second;
		}
	}

	iterator begin(set id) {
		if (id == -1) {
			return emptyS.begin();
		}
		if (id >= 0) {
			single.resize(1);
			single[0] = id;
			return single.begin();
		}
		return values[-id - 2].begin();
	}

	iterator end(set id) {
		if (id == -1) {
			return emptyS.end();
		}
		if (id >= 0) {			
			return single.end();
		}
		return values[-id - 2].end();
	}

	void print(set id) {
		auto it = begin(id);
		cout << "[";
		while (it != end(id)) {
			cout << *it << ", ";
			it++;
		}
		cout << "]";
	}

};




using namespace std;

class NumericalSolver {
protected:
	BooleanDAG* dag;
	Interface* interf;
    
    int ncontrols;
    gsl_vector* state;
    
    bool previousSAT;
    bool fullSAT;
    int numConflictsAfterSAT;
    bool inputConflict;
    
    int CONFLICT_CUTOFF = PARAMS->conflictCutoff;

    
    map<string, int>& ctrls;
    SymbolicEvaluator* eval;
    OptimizationWrapper* opt;
    ConflictGenerator* cg;
    SuggestionGenerator* sg;
    
    SimpleEvaluator* seval;
    
    set<int> assertConstraints;
    int minimizeNode;
    int counter;
	SetSet& inputsetset;
	SetSet& ctrlsetset;

    
    const vector<SetSet::set>& dependentInputs;
    const vector<SetSet::set>& dependentCtrls;
    vector<int> inputsToAsserts; // TODO: this is pretty local hack
	
	// class for picking the part of the numerical problem to handle
	// class to do symbolic evaluation
	// class for computing error function and gradients - I think this should be part of the optimization wrapper
	// class to perform optimization
	// class to generate suggestions
	// class to generate conflicts
	
public:
    NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt, ConflictGenerator* _cg, SuggestionGenerator* _sg, const vector<SetSet::set>& _dependentInputs, const vector<SetSet::set>& _dependentCtrls, SetSet& _inputsetset, SetSet& _ctrlsetset);
    ~NumericalSolver(void);
    
	// Called by the NumericalSolver
    bool checkSAT();
	bool ignoreConflict();
	vector<tuple<int, int, int>> collectSatSuggestions();
    vector<tuple<int, int, int>> collectUnsatSuggestions();
	void getConflicts(vector<pair<int, int>>& conflicts);
	void getControls(map<string, double>& ctrlVals);
    void setState(gsl_vector* state);
    
    // helper functions
    bool checkInputs();
    bool checkCurrentSol();
    bool checkFullSAT();
    bool initializeState(bool suppressPrint);
    void printControls();
    
    void printGraphCmd(string prefix);

};
