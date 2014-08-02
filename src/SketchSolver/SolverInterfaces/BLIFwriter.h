#pragma once
#include "SATSolver.h"
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

class BLIFwriter :
	public SATSolver
{
	int varNum;
	int tmpID;

	

	string outvar(){
		string tmp = "tmp";
		tmp += int2str(tmpID);
		tmpID++;
		return tmp;
	}
	vector<int> finalOr;
	bool off;
public:
	int vid(int id){
		return id > 0 ? id : -id;
	}

	int sgn(int id){
		return id > 0? 1 : 0;
	}

	int nsgn(int id){
		return id > 0? 0 : 1;
	}

	string nm(int id){
		string nm = "nm";
		nm += int2str(vid(id));
		return nm;
	}

	void turnOff(){
		off = true;
	}

	void turnOn(){
		off = false;
	}
	ofstream output;
	BLIFwriter(const string& name_p, SolverMode smode);
    virtual void annotate(const string& msg);
    virtual void annotateInput(const string& name, int i, int sz);
    virtual void addChoiceClause(int x, int a, int b, int c);
    virtual void addXorClause(int x, int a, int b);
    // virtual void addOrClause(int x, int a, int b);
    virtual void addBigOrClause(int* a, int size);
    virtual void addAndClause(int x, int a, int b);
    virtual void addEqualsClause(int x, int a);
    virtual void addEquateClause(int x, int a);
    virtual void setVarClause(int x);
    virtual void assertVarClause(int x);
	virtual void assumeVarClause(int x);
	virtual void hardAssertVarClause(int x);
	virtual void addHelperClause(int c[], int size);
    virtual int getVarVal(int id);
    virtual int newVar();
	virtual void addCountingHelperClause(int c[], int sz);
    virtual int newInVar();
    virtual void disableVarBranch(int i);
	virtual void outputSAT(){ }
	virtual void addExPairConstraint(int* pairs, int npairs, int out){}
    virtual bool ignoreOld();

    virtual int solve();

    virtual void reset();
    
	virtual void retractableAssertClause(int x);
	virtual void retractAssumptions();
	virtual void markInput(int){  }
    virtual void clean();	
    virtual void printDiagnostics(char c);	
public:
	~BLIFwriter(void);
};
