class SimulatedAnnealing {
	double T = 10;
	double coolingRate = 0.1;
	int NUM_STEPS = 100;
	double acceptanceProb(double e, double e1, double T) {
		if (e1 < e) {
			return 1.0;
		}
		return exp((e - e1)/T);
	}
	
	void getNeighboringState(const gsl_vector* curState, gsl_vector* newState) {
		for (int i = 0; i < curState->size; i++) {
			double c = 0;
			while(true) {
				c = gsl_vector_get(curState, i) - 5.0 + (rand() %100)/10.0; // Randomly permutate cur value with +/- 5 (TODO: magic numbers)
				if (c >= 0.0 && c <= 32.0) break;
			}
			gsl_vector_set(newState, i, c);
		}
	}
public:
	double optimize(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, const gsl_vector* curState);
};

double SimulatedAnnealing::optimize(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, const gsl_vector* curState) {
	double curError = ns->evalState(curState, allInputs, allOutputs);
	
	for (int i = 0; i < NUM_STEPS; i++) {
		//cout << "state: " << curState[0] << " error: " << curError << endl;
		if (curError == 0.0) break;
		gsl_vector* nextState = gsl_vector_alloc(curState->size);
		getNeighboringState(curState, nextState);
		double nextError = ns->evalState(nextState, allInputs, allOutputs);
		//cout << "next state: " << nextState[0] << " error: " << nextError << endl;
		double prob = acceptanceProb(curError, nextError, T);
		double randflip = (rand()%10)/10.0;
		//cout << "prob: " << prob << " randflip: " << randflip << endl;
		if (prob >= randflip) {
			//cout << "Transitioning to next state" << endl;
			curState = nextState;
			curError = nextError;
		}
		T = T*(1 - coolingRate);
	}
	return curError;
}
