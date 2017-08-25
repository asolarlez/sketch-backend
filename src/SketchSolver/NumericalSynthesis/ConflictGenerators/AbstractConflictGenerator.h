#pragma once 

class AbstractConflictGenerator {
public:
	virtual vector<pair<int, int>> getConflicts(gsl_vector* state, vector<vector<int>>& allInputs, vector<int>& instanceIds, int rowid, int colid) = 0;
};
