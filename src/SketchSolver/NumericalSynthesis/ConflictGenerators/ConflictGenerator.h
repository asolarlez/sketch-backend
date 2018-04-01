#pragma once 

class ConflictGenerator {
public:
	virtual void getConflicts(vector<pair<int, int>>& conflicts) = 0;
};
