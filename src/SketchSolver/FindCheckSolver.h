#ifndef FINDCHECKSOLVER_H_
#define FINDCHECKSOLVER_H_

#include "BooleanToCNF.h"

class FindCheckSolver{
	private:
	int controlSize;
	SAT_Manager mngFind;
	varDir dirFind;	
	
	SAT_Manager mngCheck;
	varDir dirCheck;
	
	map<string, int> controlVars;
	
	protected:
	
	//Reserved variable names.
	const string IN;
	const string OUT;
	const string SOUT;
	
	
	
	virtual void defineSketch(SAT_Manager mng, varDir& dir)=0;
	virtual void defineSpec(SAT_Manager mng, varDir& dir)=0;
	
	virtual void addEqualsClauses(SAT_Manager mng, varDir& dir);
	virtual void addDiffersClauses(SAT_Manager mng, varDir& dir);

	
	virtual void setupCheck();		
	virtual bool check(int controls[], int ctrlsize, int input[]);
		
	virtual void setupFind();
	virtual bool find(int input[], int insize, int controls[]);
	
	public:
	FindCheckSolver();
	
	virtual void declareControl(const string& ctrl, int size);	
	
	virtual int getInSize();	
	virtual int getCtrlSize();
	
	virtual void solve();	
	virtual void setup();
};

#endif /*FINDCHECKSOLVER_H_*/
