#pragma once

#include "AbstractConflictGenerator.h"

// Generates the trivial conflict i.e. all set assignments
class SimpleConflictGenerator: public AbstractConflictGenerator {
public:
	SimpleConflictGenerator() {}
	
	virtual vector<pair<int, int>> getConflicts(gsl_vector* state, vector<vector<int>>& allInputs, vector<int>& instanceIds, int rowid, int colid) {
		vector<pair<int, int>> conflicts;
		
		for (int i = 0; i < allInputs.size(); i++) {
			for (int j = 0; j < allInputs[i].size(); j++) {
				if (allInputs[i][j] == 0 || allInputs[i][j] == 1) {
					conflicts.push_back(make_pair(instanceIds[i], j));
				}
			}
		}
		return conflicts;
	}
};
