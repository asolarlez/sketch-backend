#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>


#include "BooleanDAG.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "Interface.h"
#include "BoolAutoDiff.h"
#include "SnoptWrapper.h"
#include "GradientDescentWrapper.h"


class NumDebugger {
	BooleanDAG* dag;
	Interface* interf;

	int ncontrols;

	map<string, int>& ctrls;
	SymbolicEvaluator* eval;
	OptimizationWrapper* opt;
	GradientDescentWrapper* maxOpt;

	SimpleEvaluator* seval;

	set<int> assertConstraints;
	int minimizeNode;

public:
	NumDebugger(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt): dag(_dag), ctrls(_ctrls), interf(_interface), eval(_eval), opt(_opt) {
		ncontrols = ctrls.size();
    	// if ncontrols = 0, make it 1 just so numerical opt does not break
		if (ncontrols == 0) {
			ncontrols = 1;
		}

		cout << "NControls: " << ncontrols << endl;

		seval = new SimpleEvaluator(*dag, ctrls);
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
	void doOpt() {
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		double arr1[1] = {6.75847};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}

		bool sat = opt->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL); 
	}
	void doMaxOpt() {
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		double arr1[1] = {0.85};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}

		opt->maximize(interf, s, assertConstraints, -1, -1, -1, 0); 
	}

	void plotConditions() { 
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		double arr1[1] = {0.88};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(state, i, arr1[i]);
		}

		seval->run(state);

		vector<pair<double, int>> s;
                
        for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->first;
            bool_node* n = (*dag)[nodeid];
            
            double dist = seval->d(n);
            double cost = abs(dist);
            s.push_back(make_pair(cost, nodeid));
        }

        sort(s.begin(), s.end());
        cout << "Suggestions: " << endl;
        for (int i = 0; i < s.size(); i++) {
        	cout << (*dag)[s[i].second]->lprint() << " " << s[i].first << endl;
        }
        /*for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
            int nodeid = it->second->nodeid;
            bool_node* n = (*dag)[nodeid];
        	getGraphForCondition(n);
        }*/

	}

	void getGraphForCondition(bool_node* n) {
		cout << n->id << endl;
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/cond_" + to_string(n->id) + ".txt");
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		Assert(ncontrols == 1, "dahfa");
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, 0, i);
				seval->run(state);
				double error = seval->d(n);
			    file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();


	}
	void getAsserts(int iterationId) {
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		double arr1[1] = {6.80647};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}

		eval->setInputs(interf); 

		for (int i = 0; i < ncontrols; i++) {
			for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
				genSmoothData(s, i, 1, *it, iterationId);
				gsl_vector_set(s, i, arr1[i]);
			}
		}
	}
	void genSmoothData(gsl_vector* state, int idx, float beta, int assertId, int iterationId) {
		GradUtil::BETA = -beta;
		GradUtil::ALPHA = beta;

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/assert_" + to_string(iterationId) + "_" + to_string(assertId) + "_" + to_string(int(beta)) + ".txt");
		const set<int>& inputConstraints = interf->getInputConstraints();
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				eval->run(state);
				double error = 0.0;
				double e;
				
				e = eval->getErrorOnConstraint(assertId);
        		if (e < 0.0) { // TODO: magic numbers
        			error += -e;
        		}
        		
        		for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        			e = eval->getErrorOnConstraint(*it);
        			if (e < 0.0) {
        				error = -2.0;
        				break;
        			}
        		}
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void getGraphs(int iterationId) { 
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		double arr1[2] = {1.2934,12};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}

		eval->setInputs(interf); 
		seval->setInputs(interf);

		for (int i = 0; i < ncontrols; i++) {
			genSmoothData(s, i, 1, iterationId);
			genSmoothData(s, i, 10, iterationId);
			genSmoothData(s, i, 50, iterationId);
			gsl_vector_set(s, i, arr1[i]);
		}
		for (int i = 0; i < ncontrols; i++) {
			genActualData(s, i, iterationId);
			gsl_vector_set(s, i, arr1[i]);
		}
		//getAsserts(iterationId);
	}

	void genSmoothData(gsl_vector* state, int idx, float beta, int iterationId) {
		GradUtil::BETA = -beta;
		GradUtil::ALPHA = beta;

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/data_smooth_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		const set<int>& inputConstraints = interf->getInputConstraints();
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				eval->run(state);
				double error = 0.0;
				double e;
				if (minimizeNode >= 0) {
					e = eval->getErrorOnConstraint(minimizeNode);
					error += e;
				}
				for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
					e = eval->getErrorOnConstraint(*it);
        			if (e < 0.0) { // TODO: magic numbers
        				error += -e;
        			}
        		}
        		for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        			e = eval->getErrorOnConstraint(*it);
        			if (e < 0.0) {
        				error = -2.0;
        				break;
        			}
        		}
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genActualData(gsl_vector* state, int idx, int iterationId) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/data_actual_" + to_string(iterationId) + "_" + to_string(idx) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		const set<int>& inputConstraints = interf->getInputConstraints();
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				seval->run(state);
				double error = 0.0;
				double e;
				if (minimizeNode >= 0) {
					e = seval->getErrorOnConstraint(minimizeNode);
					error += e;
				}
				for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
					e = seval->getErrorOnConstraint(*it);
        			if (e < 0.0) { // TODO: magic numbers
        				error += -e;
        			}
        		}
        		for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        			e = seval->getErrorOnConstraint(*it);
        			if (e < 0.0) {
        				error = -2.0;
        				break;
        			}
        		}
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();
	}
};