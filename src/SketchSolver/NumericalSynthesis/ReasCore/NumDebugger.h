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


class NumDebugger {
	BooleanDAG* dag;
	Interface* interf;

	int ncontrols;

	map<string, int>& ctrls;
	SmoothEvaluators* smoothEval;
	ActualEvaluators* actualEval;
	OptimizationWrapper* opt;

	set<int> assertConstraints;
	int minimizeNode;

public:
	NumDebugger(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SmoothEvaluators* _smoothEval, ActualEvaluators* _actualEval, OptimizationWrapper* _opt): dag(_dag), ctrls(_ctrls), interf(_interface), smoothEval(_smoothEval), actualEval(_actualEval), opt(_opt) {
		ncontrols = ctrls.size();
    	// if ncontrols = 0, make it 1 just so numerical opt does not break
		if (ncontrols == 0) {
			ncontrols = 1;
		}

		cout << "NControls: " << ncontrols << endl;

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

	void getPredicatesGraphs() {
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
			if (ncontrols == 1) {
				plotPredicate1d(*it, s);
			} else if (ncontrols == 2) {
				plotPredicate2d(*it, s);
			}
		}
	}
	void plotPredicate1d(Predicate* p, gsl_vector* state) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/pred_" + p->print() + ".txt");
		{
			double i = -10.0;
			while (i < 10.0) {
				gsl_vector_set(state, 0, i);
				actualEval->run(state);
				double error = actualEval->dist(p);

        		//cout << i << " " << error << endl;
        		file << i  << "," << error << endl;
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void plotPredicate2d(Predicate* p, gsl_vector* state) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/pred_" + p->print() + ".txt");
		{
			double i = -10.0;
			double j = -10.0;
			while (i < 10.0) {
				j = -10.0;
				while (j < 10.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				actualEval->run(state);
				double error = actualEval->dist(p);

        		//cout << i << " " << error << endl;
        		file << i << "," << j << "," << error << endl;
        		j += 0.1;
        		}
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}
	
	void getGraphs(int level, int iterationId) { 
		gsl_vector* s = gsl_vector_alloc(ncontrols);
		double arr1[2] = {1.2934,12};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}

		if (s->size == 1) { 
			for (int i = 0; i < ncontrols; i++) {
				genSmoothData(s, i, 1, iterationId, level);
				genSmoothData(s, i, 10, iterationId, level);
				genSmoothData(s, i, 50, iterationId, level);
				gsl_vector_set(s, i, arr1[i]);
			}
			for (int i = 0; i < ncontrols; i++) {
				genActualData(s, i, iterationId, level);
				gsl_vector_set(s, i, arr1[i]);
			}
		}
		if (s->size == 2) {
			for (int i = 0; i < ncontrols-1; i++) {
				genSmoothData2D(s, i, 1, iterationId, level);
				genSmoothData2D(s, i, 10, iterationId, level);
				genSmoothData2D(s, i, 50, iterationId, level);
				gsl_vector_set(s, i, arr1[i]);
				gsl_vector_set(s, i+1, arr1[i+1]);
			}
			for (int i = 0; i < ncontrols-1; i++) {
				genActualData2D(s, i, iterationId, level);
				gsl_vector_set(s, i, arr1[i]);
				gsl_vector_set(s, i+1, arr1[i+1]);
			}
		}
		//getAsserts(iterationId);
	}

	double getError(int level) {
		double error = 0.0;
		double e;
		if (level == -1) {
			if (minimizeNode >= 0) {
				e = smoothEval->getErrorOnConstraint(minimizeNode);
				error += e;
			}
			for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
				e = smoothEval->getErrorOnConstraint(*it);
        		if (e < 0.0) { // TODO: magic numbers
        			error += -e;
        		}
        	}	
        	const set<int>& inputConstraints = interf->getInputConstraints();
        	for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        		e = smoothEval->getErrorOnConstraint(*it);
        		if (e < 0.0) {
        			error += -e;
        		}
        	}
        }
        if (level >= 0) {
        	for (auto it = interf->clauseLevels[level].begin(); it != interf->clauseLevels[level].end(); it++) {
        		e = smoothEval->getErrorOnClause(*it);
        		if (e < 0.0) {
        			error += -e;
        		}
        	}
        }
        for (int i = level+1; i < interf->numLevels(); i++) {
        	if (i < 0) continue;
        	for (auto it = interf->clauseLevels[i].begin(); it != interf->clauseLevels[i].end(); it++) {
        		e = smoothEval->getErrorOnClause(*it);
        		if (e < 0.0) {
        			error = -2.0;
        			break;
        		}
        	}
        }
        return error;
	}

	double getActualError(int level) {
		double error = 0.0;
		double e;
		if (level == -1) {
			if (minimizeNode >= 0) {
				e = actualEval->getErrorOnConstraint(minimizeNode);
				error += e;
			}
			for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
				e = actualEval->getErrorOnConstraint(*it);
        		if (e < 0.0) { // TODO: magic numbers
        			error += -e;
        		}
        	}
        	const set<int>& inputConstraints = interf->getInputConstraints();
        	for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        		e = actualEval->getErrorOnConstraint(*it);
        		if (e < 0.0) {
        			error += -e;
        		}
        	}	
        }
        if (level >= 0) {
        	for (auto it = interf->clauseLevels[level].begin(); it != interf->clauseLevels[level].end(); it++) {
        		e = actualEval->getErrorOnClause(*it);
        		if (e < 0.0) {
        			error += -e;
        		}
        	}
        }
        for (int i = level+1; i < interf->numLevels(); i++) {
        	for (auto it = interf->clauseLevels[i].begin(); it != interf->clauseLevels[i].end(); it++) {
        		e = actualEval->getErrorOnClause(*it);
        		if (e < 0.0) {
        			error = -2.0;
        			break;
        		}
        	}
        }
        return error;
	}

	void genSmoothData(gsl_vector* state, int idx, float beta, int iterationId, int level) {
		GradUtil::BETA = -beta;
		GradUtil::ALPHA = beta;

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/" + to_string(level+1) + "_smooth_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = 0.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				smoothEval->run(state);
				double error = getError(level);
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genActualData(gsl_vector* state, int idx, int iterationId, int level) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/" + to_string(level + 1) + "_actual_" + to_string(iterationId) + "_" + to_string(idx) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = 0.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				actualEval->run(state);
				double error = getActualError(level);
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.01;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genSmoothData2D(gsl_vector* state, int idx, float beta, int iterationId, int level) {
		GradUtil::BETA = -beta;
		GradUtil::ALPHA = beta;

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/" + to_string(level + 1) + "_smooth_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		const set<int>& inputConstraints = interf->getInputConstraints();
		{
			double i = -10.0;
			double j = -10.0;
			while (i < 10.0) {
				j = -10.0;
				while (j < 10.0) {
				gsl_vector_set(state, idx, i);
				gsl_vector_set(state, idx + 1, j);
				smoothEval->run(state);
				double error = getError(level);

        		//cout << i << " " << error << endl;
        		file << i << "," << j << "," << error << endl;
        		j += 0.1;
        		}
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genActualData2D(gsl_vector* state, int idx, int iterationId, int level) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/" + to_string(level + 1) + "_actual_" + to_string(iterationId) + "_" + to_string(idx) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = -10.0;
			double j = -10.0;
			while (i < 10.0) {
				j = -10.0;
				while (j < 10.0) {
				gsl_vector_set(state, idx, i);
				gsl_vector_set(state, idx + 1, j);
				actualEval->run(state);
				double error = getActualError(level);

        		//cout << i << " " << error << endl;
        		file << i << "," << j << "," << error << endl;
        		j += 0.1;
        		}
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}
};