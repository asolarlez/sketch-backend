#pragma once

#include "ConflictGenerator.h"
#include "CommandLineArgs.h"

// Generates the trivial conflict i.e. all set assignments
class SimpleConflictGenerator: public ConflictGenerator {
    Interface* interf;
public:
	SimpleConflictGenerator(Interface* _interface): interf(_interface){}
	
	virtual void getConflicts(vector<pair<int, int>>& conflicts) {
        if (PARAMS->verbosity > 7) {
            cout << "Conflict clause ";
        }
        const set<int>& inputNodes = interf->getInputConstraints();
        for (auto it = inputNodes.begin(); it != inputNodes.end(); it++) {
            if (PARAMS->verbosity > 7) {
                cout << (*it) << ", ";
            }
            conflicts.push_back(make_pair(0, (*it)));
        }
        if (PARAMS->verbosity > 7) {
            cout << endl;
        }
	}
};
