#pragma once 


#ifndef _NOGSL
#else
#include "CustomSolver.h"
#endif


class ConflictGenerator {
public:
	virtual void getConflicts(vector<pair<int, int>>& conflicts) = 0;
};
