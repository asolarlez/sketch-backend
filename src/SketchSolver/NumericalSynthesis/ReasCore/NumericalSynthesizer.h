#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>


#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
//#include "BasicSolver.h"
//#include "InequalitySolver.h"
//#include "BoolApproxSolver.h"
//#include "IteApproxSolver.h"
//#include "SmoothSatSolver.h"
#include "NumericalSolver.h"
#include "Interface.h"
#include "BoolAutoDiff.h"
#include "SnoptWrapper.h"
#include "SimpleConflictGenerator.h"
#include "SimpleSuggestionGenerator.h"


class NumericalSynthesizer : public Synthesizer {
	BooleanDAG* dag;
	Interface* interface;
    map<string, double> ctrlVals; // maps ctrl names to values found by the numerical solver
	NumericalSolver* solver;
    vector<pair<int, int>> conflicts; // (nodeid, val) pairs
    timerclass timer;
	
    map<string, int> ctrls;
	
public:
    NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface, Lit _softConflictLit);
	
	virtual bool synthesis(vec<Lit>& suggestions);
	virtual void newInstance() {}
	virtual void finalize() {}
	virtual void backtrack(int level) {}
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params);
    
	virtual void print(ostream& out) {
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			out << it->first << ":" << it->second << endl;
		}
	}
	virtual void getControls(map<string, string>& values) {
		for (auto it = ctrlVals.begin(); it != ctrlVals.end(); it++) {
			stringstream str;
			str << it->second;
			values[it->first] = str.str();
		}
	}
	
	void convertSuggestions(const vector<tuple<int, int, int>>& s, vec<Lit>& suggestions);

    virtual void set_inout(int ninputs, int noutputs) {
        // do nothing - using interface as input output matrix
    }
    
    virtual int newInputOutputInstance(vector<Tvalue>& inputs, vector<Tvalue>& outputs) {
        Assert(false, "This should not be called");
    }
    
    virtual void backtrackInputs(int level) {
        interface->backtrack(level);
    }
    
    virtual void pushInput(int instance, int inputid, int val, int dlevel, vec<Lit>& conf) {
        int nodeid = interface->getNodeId(inputid);
        Assert(instance == 0, "Multiple instances is not yet supported");
        int oldval = interface->getValue(nodeid);
        if (oldval != EMPTY){
            //writing over non EMPTY values
            Lit l1 = interface->getLit(nodeid, val);
            Lit l2 = interface->getLit(nodeid, oldval);
            conf.push(~l1);
            conf.push(~l2);
        } else {
            interface->pushInput(inputid, val, dlevel);
        }
    }
    
    virtual void getConflictLits(vec<Lit>& conf) {
        for (int i = 0; i < conflicts.size(); ++i) {
            int instance = conflicts[i].first;
            int nodeid = conflicts[i].second;
            Assert(instance == 0, "Multiple instances is not yet supported");
            int val = interface->getValue(nodeid);
            Lit l = interface->getLit(nodeid, val);
            conf.push(~l);
        }
        if (softConflict) {
            conf.push(~softConflictLit);
        }
    }



};

