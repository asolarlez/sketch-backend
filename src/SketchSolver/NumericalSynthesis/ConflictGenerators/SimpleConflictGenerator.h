#pragma once

#include "ConflictGenerator.h"
#include "CommandLineArgs.h"

// Generates the trivial conflict i.e. all set assignments
class SimpleConflictGenerator: public ConflictGenerator {
	Interface* interface
public:
	SimpleConflictGenerator(Interface* _interface): interface(_interface){}
	
	virtual void getConflicts(vector<pair<int, int>>& conflicts) {
        if (PARAMS->verbosity > 7) {
            cout << "Conflict clause ";
        }
        set<int>& inputNodes = interface->getInputConstraints();
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
