
#ifndef CHECKPOINTER_H_
#define CHECKPOINTER_H_

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

class Checkpointer{
	ofstream cptfile;
	bool doCheckpoint;
	public:
	virtual void checkpoint(char nm, vector<int>& ar);
	virtual void setCheckpoint(const string& filename);
	virtual void resizeInput(const string& name, int newsz);
	Checkpointer(){
		doCheckpoint = false;
	}
};

#endif

