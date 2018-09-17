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
	gsl_vector* tmp_state;

	vector<vector<double>> sols;

public:
	NumDebugger(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SmoothEvaluators* _smoothEval, ActualEvaluators* _actualEval, OptimizationWrapper* _opt): dag(_dag), ctrls(_ctrls), interf(_interface), smoothEval(_smoothEval), actualEval(_actualEval), opt(_opt){
		ncontrols = ctrls.size();
    	// if ncontrols = 0, make it 1 just so numerical opt does not break
		if (ncontrols == 0) {
			ncontrols = 1;
		}

		cout << "NControls: " << ncontrols << endl;
		tmp_state = gsl_vector_alloc(ncontrols);

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

	    if (false) {
	    	ifstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/experiments/benchmarks/outputs/" + Util::benchName() + ".txt"); 
	    	string line;
	    	while (getline(file, line)) {
	    		cout << line << endl;
	    		const vector<string>& s = Util::split(line, ";");
	    		Assert(s.size() == ncontrols + 1, "Wrong sols");
	    		vector<double> vals;
	    		for (int i = 0; i < ncontrols; i++) {
	    			vals.push_back(stod(s[i]));
	    		}
	    		sols.push_back(vals);
	    	}

	    	checkSols();
	    }
	}

	void plotGraphs() {
		float arr[4] = {12, -11, -15.5, 4.9};
        for (int j = 0; j < ncontrols; j++) {
            gsl_vector_set(tmp_state, j, arr[j]);
        }
		for (int i = 0; i < ncontrols; i++) {
			genActualData(tmp_state, i, 2, -1);
			gsl_vector_set(tmp_state, i, arr[i]);
			genSmoothData(tmp_state, i, 1, 2, -1);
			gsl_vector_set(tmp_state, i, arr[i]);
			genSmoothData(tmp_state, i, 10, 2, -1);
			gsl_vector_set(tmp_state, i, arr[i]);
			genSmoothData(tmp_state, i, 50, 2, -1);
			gsl_vector_set(tmp_state, i, arr[i]);
		}
		
	}
	void optDebug() {
		float arr[10] = {11.2, 9.1, 11.2, 11.2, 2.6, -0.6, -1.4, 3.9, 10.3, 14.1};
		for (int j = 0; j < ncontrols; j++) {
			gsl_vector_set(tmp_state, j, arr[j]);
		}

		opt->optimize(interf, tmp_state, assertConstraints, minimizeNode, false, 1, NULL, -1);

		gsl_vector_memcpy(tmp_state, opt->getMinState());
		smoothEval->run(tmp_state);

		double error;
    	if (minimizeNode >= 0) {
        	error = smoothEval->getErrorOnConstraint(minimizeNode, tmp_state);
        	if (error > 0.01) {
            	cout << (*dag)[minimizeNode]->lprint() << endl;
            	cout << Util::print(tmp_state) << endl;
        	}
    	}
    	for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
        	error = smoothEval->getErrorOnConstraint(*it, tmp_state);
        	if (error < -0.01) { // TODO: magic number
            	cout << (*dag)[*it]->lprint() << endl;
            	cout << Util::print(tmp_state) << endl;
        	}
    	}
	}

	void getPredicatesGraphs() {
		if (ncontrols == 2) {
			plotPredicate2d(tmp_state);
		} else if (ncontrols == 1) {
			for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
				plotPredicate1d(*it, tmp_state, 2);
			}
		}
		
	}

	void plotLinePredicate2d() {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/2Ddata/" + Util::benchName() + "_linepred" + ".txt");
		
		gsl_vector_set(tmp_state, 0, 0.0);
		gsl_vector_set(tmp_state, 1, 0.0);
		actualEval->run(tmp_state);
		for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
			file << (*it)->print() << ";" << actualEval->dist(*it) << ";" << Util::print(actualEval->grad(*it)) << endl;
		}
    	file.close();
	}
	void plotPredicate1d(Predicate* p, gsl_vector* state, int val) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/1Dfull/" + Util::benchName() +  "_pred_" + p->print() + "_" + to_string(val) + ".txt");
		{
			double i = -20.0;
			while (i < 20.0) {
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

	void plotPredicate2d(gsl_vector* state) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/2Ddata/" + Util::benchName() + "_pred_0" + ".txt");
		{
		
			double i = -20.0;
			double j = -20.0;
			while (i < 20.0) {
				j = -20.0;
				while (j < 20.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				cout << i << " " << j << endl;
				actualEval->run(state);
				for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
					double error = actualEval->dist(*it);
					if (abs(error) < 1.0) {
        				//cout << i << " " << error << endl;
        				file << i << "," << j << "," << error << "," << (*it)->print() << endl;
        			}
        		}
        		j += 0.1;
        		}
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void checkSmoothing() {
		gsl_vector* s = tmp_state;
		double arr1[1] = {6.0};
		for (int i = 0; i < ncontrols; i++) {
			gsl_vector_set(s, i, arr1[i]);
		}
		GradUtil::BETA = -1;
		GradUtil::ALPHA = 1;
		actualEval->run(s);
		smoothEval->run(s);
		exit(0);
	}
	
	void runOptimization(OptimizationWrapper* gd, OptimizationWrapper* snopt, OptimizationWrapper* custom) {
		gsl_vector* s = tmp_state;
		
		for (int i = 0; i < 5; i++) {
			double r = -20 + (rand() % 400)/10.0;
            gsl_vector_set(s, 0, r);
			GradUtil::counter = i;
			gd->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL, -1);
			snopt->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL, -1);
			custom->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL, -1);
		}
	}

	void runOptimization2D() {
		gsl_vector* s = tmp_state;
		
		for (int i = 0; i < 4; i++) {
			double r1 = -20 + (rand() % 400)/10.0;
			double r2 = -20 + (rand() % 400)/10.0;
            gsl_vector_set(s, 0, r1);
            gsl_vector_set(s, 1, r2);
			GradUtil::counter = i;
			opt->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL, -1);
		}
	}

	void getGraphForClause(IClause* c, bool printPreds = false) {
		if (ncontrols == 1) {
			getGraphForClause1d(c, printPreds);
		} else if (ncontrols == 2) {
			getGraphForClause2d(c);
		}
	}
	void getGraphForClause1d(IClause* c, bool printPreds = false) {
		gsl_vector* state = tmp_state;

		if (printPreds) {
			//for (int i = 0; i < c->size(); i++) {
			//	plotPredicate1d(c->getPredicate(i), state, c->getVal(i));
			//}
		}


		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/1Dfull/" + Util::benchName() + "_clause_" + to_string(GradUtil::counter) + ".txt");
		double i = -20.0;
		while (i < 20.0) {
			gsl_vector_set(state, 0, i);
			actualEval->run(state);
			double error = 0.0;
			double e = actualEval->getErrorOnClause(c);
        	if (e < 0.0) {
        		error += -e;
        	}
        	file << i << "," << error << endl;
        	i += 0.1;
       	}
    	file << endl;
    	file.close();
	}

	void getGraphForClause2d(IClause* c) {
		gsl_vector* state = tmp_state;
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/2Dfull/" + Util::benchName() + "_clause_" + to_string(GradUtil::counter) + ".txt");
		double i = -20.0;
		double j = -20.0;
		while (i < 20.0) {
			j = -20.0;
			while (j < 20.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				actualEval->run(state);
				double error = 0.0;
				double e = actualEval->getErrorOnClause(c);
        		if (e < 0.0) {
        			error += -e;
        		}
        		file << i << "," << j << "," << error << endl;
        		j += 0.5;
        	}
        	i += 0.5;
       	}
    	file << endl;
    	file.close();
	}

	void runMaxAnalysis(MaxOptimizationWrapper* maxOpt) {
		gsl_vector* s = tmp_state;

		gsl_vector* m1 = gsl_vector_alloc(ncontrols);
		gsl_vector* m2 = gsl_vector_alloc(ncontrols);
		gsl_vector* m3 = gsl_vector_alloc(ncontrols);
		gsl_vector* m4 = gsl_vector_alloc(ncontrols);

		for (int t = 0; t < 4; t++) {
			double r1 = -20 + (rand()%400)/10.0;
			double r2 = -20 + (rand()%400)/10.0;
			gsl_vector_set(s, 0, r1);
			gsl_vector_set(s, 1, r2);

			GradUtil::counter = t;
			opt->optimize(interf, s, assertConstraints, minimizeNode, false, 1, NULL, -1);
			gsl_vector_memcpy(s, opt->getMinState());
			
			// dir 1 (+1, 0)
			gsl_vector_set(s, 0, gsl_vector_get(s, 0) + 0.5);
        	maxOpt->maximize(interf, s, s, assertConstraints, minimizeNode, -1, -1, 0); 
        	gsl_vector_memcpy(m1, maxOpt->getMinState());
        	gsl_vector_set(s, 0, gsl_vector_get(s, 0) - 0.5);

        	// dir 2 (-1, 0)
        	gsl_vector_set(s, 0, gsl_vector_get(s, 0) - 0.5);
        	maxOpt->maximize(interf, s,s, assertConstraints, minimizeNode, -1, -1, 1);
        	gsl_vector_memcpy(m2, maxOpt->getMinState());
        	gsl_vector_set(s, 0, gsl_vector_get(s, 0) + 0.5);

        	// dir 3 (0, +1)
        	gsl_vector_set(s, 1, gsl_vector_get(s, 1) + 0.5);
        	maxOpt->maximize(interf, s,s, assertConstraints, minimizeNode, -1, -1, 2);
        	gsl_vector_memcpy(m3, maxOpt->getMinState());
        	gsl_vector_set(s, 1, gsl_vector_get(s, 1) - 0.5);

        	// dir 4 (0, -1)
        	gsl_vector_set(s, 1, gsl_vector_get(s, 1) - 0.5);
        	maxOpt->maximize(interf, s,s, assertConstraints, minimizeNode, -1, -1, 3);
        	gsl_vector_memcpy(m4, maxOpt->getMinState());
        	gsl_vector_set(s, 1, gsl_vector_get(s, 1) + 0.5);

        	// collect common conditions satisfied by s, m1, m2, m3, m4
        	map<Predicate*, tuple<double, int>> common_preds;

        	actualEval->run(s);               
        	for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
            	Predicate* p = *it;

            	double dist = actualEval->dist(p);
            	double cost = abs(dist);
            	int val = dist >= 0;

            	common_preds[p] = make_tuple(cost, val);
    		}

    		vector<gsl_vector*> maxes;
    		maxes.push_back(m1);
    		maxes.push_back(m2);
    		maxes.push_back(m3);
    		maxes.push_back(m4);
    		
    		for (int i = 0; i < maxes.size(); i++) {
    			actualEval->run(maxes[i]);
    			for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
    				Predicate* p = *it;

    				double dist = actualEval->dist(p);
    				double cost = abs(dist);
    				int val = dist >= 0;

    				auto& tup = common_preds[p];
    				double& tcost = get<0>(tup);
    				int& tval = get<1>(tup);
    				if (tval == val) {
    					tcost = min(tcost, cost);
    				} else {
    					if (tcost < 0.01) {
    						tval = val;
    						tcost =  cost; 
    					} else {
    						tval = -1;
    					}
    				}
    			}
    		}

    		vector<tuple<double, Predicate*, int>> ss;

    		for (auto it = common_preds.begin(); it != common_preds.end(); it++) {
    			Predicate* p = it->first;
    			auto& tup = it->second;
    			if (get<1>(tup) != -1) {
    				ss.push_back(make_tuple(get<0>(tup), p, get<1>(tup)));
    			}
    		}
    		sort(ss.begin(), ss.end());

    		cout << "Num common preds: " << ss.size() << endl;

    		IClause* c = new IClause(0);
        	for (auto it = ss.begin(); it != ss.end(); it++) {
            	c->add(get<1>(*it), !get<2>(*it));
        	}

        	getGraphForClause2d(c);
        }
	}

	

	void getGraphs(int level, int iterationId) { 
		gsl_vector* s = tmp_state;
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
				//genSmoothData2D(s, i, 10, iterationId, level);
				//genSmoothData2D(s, i, 50, iterationId, level);
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

	double getError(int level, gsl_vector* grad) {
		double error = 0.0;
		double e;
		gsl_vector* t = gsl_vector_alloc(grad->size);
		GradUtil::default_grad(grad);
		if (level == -1) {
			if (minimizeNode >= 0) {
				e = smoothEval->getErrorOnConstraint(minimizeNode, t);
				if (e > -900) {
					error += e;
					gsl_vector_add(grad, t);
				}
			}
			for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
				e = smoothEval->getErrorOnConstraint(*it, t);
        		if (e < 0.0) { // TODO: magic numbers
        			error += -e;
        			gsl_vector_scale(t, -1);
        			gsl_vector_add(grad, t);
        		}
        	}	
        	const set<int>& inputConstraints = interf->getInputConstraints();
        	for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
        		e = smoothEval->getErrorOnConstraint(*it, t);
        		if (e < 0.0) {
        			error += -e;
        			gsl_vector_scale(t, -1);
        			gsl_vector_add(grad, t);
        		}
        	}
        }
        if (level >= 0) {
        	for (auto it = interf->clauseLevels[level].begin(); it != interf->clauseLevels[level].end(); it++) {
        		e = smoothEval->getErrorOnClause(*it, t);
        		if (e < 0.0) {
        			error += -e;
        			gsl_vector_scale(t, -1);
        			gsl_vector_add(grad, t);
        		}
        	}
        }
        for (int i = level+1; i < interf->numLevels(); i++) {
        	if (i < 0) continue;
        	for (auto it = interf->clauseLevels[i].begin(); it != interf->clauseLevels[i].end(); it++) {
        		e = smoothEval->getErrorOnClause(*it);
        		if (e < 0.0) {
        			error = -2.0;
        			GradUtil::default_grad(grad);
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
				if (e > -900) {
					error += e;
				}
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
		cout << "Beta: " << beta << endl;
		gsl_vector* grad = gsl_vector_alloc(state->size);

		string suffix = "";
		if (PARAMS->smoothingMode == 1) {
			suffix = "conc_";
		}
		if (PARAMS->smoothingMode == 2) {
			suffix = "nodeMerge_";
		}

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/data/" + Util::benchName() + "_" + to_string(level+1) + "_" + suffix + "smooth_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		ofstream gfile("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/data/" + Util::benchName() + "_" + to_string(level+1) + "_" + suffix + "smooth_grad_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				smoothEval->run(state);
				double error = getError(level, grad);
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		gfile << i << "," << gsl_vector_get(grad, 0) << endl;
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genActualData(gsl_vector* state, int idx, int iterationId, int level) {
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/data/" + Util::benchName() + "_" + to_string(level + 1) + "_actual_" + to_string(iterationId) + "_" + to_string(idx) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = -20.0;
			while (i < 20.0) {
				gsl_vector_set(state, idx, i);
				actualEval->run(state);
				double error = getActualError(level);
        		//cout << i << " " << error << endl;
        		file << i << "," << error << endl;
        		i += 0.1;
       		}
    	}
    	file << endl;
    	file.close();
	}

	void genSmoothData2D(gsl_vector* state, int idx, float beta, int iterationId, int level) {
		GradUtil::BETA = -beta;
		GradUtil::ALPHA = beta;

		string suffix = "";
		if (PARAMS->smoothingMode == 1) {
			suffix = "conc_";
		}
		if (PARAMS->smoothingMode == 2) {
			suffix = "nodeMerge_";
		}

		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/2Ddata/" + Util::benchName() + "_" + to_string(level + 1) + "_" + suffix + "smooth_" + to_string(iterationId) + "_" + to_string(idx) + "_" + to_string(int(beta)) + ".txt");
		file << "OPT:" << gsl_vector_get(state, idx) << endl;
		const set<int>& inputConstraints = interf->getInputConstraints();
		{
			double i = -20.0;
			double j = -20.0;
			while (i < 20.0) {
				j = -20.0;
				while (j < 20.0) {
				gsl_vector_set(state, idx, i);
				gsl_vector_set(state, idx + 1, j);
				smoothEval->run(state);
				double error = getError(level);

        		cout << i << "," << j << "," << error << endl;
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
		ofstream file("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/2Ddata/" + Util::benchName() + "_" + to_string(level + 1) + "_actual_" + to_string(iterationId) + "_" + to_string(idx) + ".txt");
		//file << "OPT:" << gsl_vector_get(state, idx) << endl;
		{
			double i = -20.0;
			double j = -20.0;
			while (i < 20.0) {
				j = -20.0;
				while (j < 20.0) {
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

	void getSol(int i, gsl_vector* tmp) {
		for (int j = 0; j < ncontrols; j++) {
			gsl_vector_set(tmp, j, sols[i][j]);
		}
	}

	void checkSols() {
		for (int i = 0; i < sols.size(); i++) {
			for (int j = 0; j < ncontrols; j++) {
				gsl_vector_set(tmp_state, j, sols[i][j]);
			}
			cout << Util::print(tmp_state) << endl;
			actualEval->run(tmp_state);
			double actualError = getActualError(-1);
			GradUtil::BETA = -50;
			GradUtil::ALPHA = 50;
			smoothEval->run(tmp_state);
			double smoothError = getError(-1);
			cout << smoothError << " " << actualError << endl;
		}
	}

	void distToSols(const gsl_vector* state) {
		vector<double> dists;
		for (int i = 0; i < sols.size(); i++) {
			double dist = 0.0;
			for (int j = 0; j < ncontrols; j++) {
				dist += pow(gsl_vector_get(state, j) - sols[i][j], 2);
			}
			dists.push_back(dist);
		}
		cout << "Dist to sols: [" << Util::getMin(dists) << "] " << Util::print(dists) << endl;
	}

	void checkWithValidSolutions(IClause* c) {
		for (int i = 0; i < sols.size(); i++) {
			cout << "(" << i << ": ";
			for (int j = 0; j < ncontrols; j++) {
				gsl_vector_set(tmp_state, j, sols[i][j]);
			}
			actualEval->run(tmp_state);
			double e = actualEval->getErrorOnClause(c);
			cout << e << ") " << endl;
		}
	}
};