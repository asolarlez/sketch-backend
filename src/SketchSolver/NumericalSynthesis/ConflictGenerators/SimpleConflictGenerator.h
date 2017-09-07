#pragma once

#include "AbstractConflictGenerator.h"

// Generates the trivial conflict i.e. all set assignments
class SimpleConflictGenerator: public AbstractConflictGenerator {
	map<int, int>& imap;
	set<int>& boolNodes;
public:
	SimpleConflictGenerator(map<int, int>& imap_, set<int>& boolNodes_):imap(imap_), boolNodes(boolNodes_) {}
	
	virtual vector<pair<int, int>> getConflicts(gsl_vector* state, vector<vector<int>>& allInputs, vector<int>& instanceIds, int rowid, int colid) {
		vector<pair<int, int>> conflicts;
		cout << "Conflict clause ";
		for (int i = 0; i < allInputs.size(); i++) {
			for (int j = 0; j < allInputs[i].size(); j++) {
				if (boolNodes.find(imap[j]) != boolNodes.end()) {
					if (allInputs[i][j] == 0 || allInputs[i][j] == 1) {
						cout << j << ", ";
						conflicts.push_back(make_pair(instanceIds[i], j));
					}
				}
			}
		}
		cout << endl;
		return conflicts;
	}
};
