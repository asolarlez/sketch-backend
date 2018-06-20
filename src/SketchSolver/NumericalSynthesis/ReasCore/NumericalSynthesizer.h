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
#include "GradientDescentWrapper.h"
#include "SimpleConflictGenerator.h"
#include "SimpleSuggestionGenerator.h"
#include "SmartSuggestionGenerator1.h"
#include "SuggestionGeneratorUsingMax.h"

#include "NumDebugger.h"


class NumericalSynthesizer : public Synthesizer {
	BooleanDAG* dag;
	Interface* interf;
    map<string, double> ctrlVals; // maps ctrl names to values found by the numerical solver
	NumericalSolver* solver;
    vector<pair<int, int>> conflicts; // (nodeid, val) pairs
    timerclass timer;
	
    map<string, int> ctrls; // maps ctrl names to index in the state vector
    vector<vector<int>> dependentCtrls; // maps nodes to dependent ctrls
    vector<vector<int>> dependentInputs; // maps nodes to dependent input nodes

    bool initialized;

    NumDebugger* debugger;
	
public:
    NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface, Lit _softConflictLit);
    void init();
	virtual bool synthesis(vec<Lit>& suggestions);
	virtual void newInstance() {}
	virtual void finalize() {}
	virtual void backtrack(int level) {}
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params);
    virtual void initSuggestions(vec<Lit>& suggestions);
    
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
        interf->backtrack(level);
    }
    
    virtual void pushInput(int instance, int inputid, int val, int dlevel, vec<Lit>& conf) {
        int nodeid = interf->getNodeId(inputid);
        Assert(instance == 0, "Multiple instances is not yet supported");
        int oldval = interf->getValue(nodeid);
        if (oldval != EMPTY){
            //writing over non EMPTY values - create conflict
            Lit l1 = interf->getLit(nodeid, val);
            Lit l2 = interf->getLit(nodeid, oldval);
            conf.push(~l1);
            conf.push(~l2);
        } else {
            cout << "Setting: " << (*dag)[nodeid]->lprint() << " to " << val << endl;
            //cout << "[" << Util::print(dependentCtrls[nodeid]) << "]" << endl;
            interf->pushInput(inputid, val, dlevel);
        }
    }
    
    virtual void getConflictLits(vec<Lit>& conf) {
        for (int i = 0; i < conflicts.size(); ++i) {
            int instance = conflicts[i].first;
            int nodeid = conflicts[i].second;
            Assert(instance == 0, "Multiple instances is not yet supported");
            int val = interf->getValue(nodeid);
            Lit l = interf->getLit(nodeid, val);
            conf.push(~l);
        }
        if (true) { //(softConflict) {
            conf.push(~softConflictLit);
        }
    }



};

