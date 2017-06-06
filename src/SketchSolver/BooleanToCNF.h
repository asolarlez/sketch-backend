#ifndef BOOLEANTOCNF_H_
#define BOOLEANTOCNF_H_



#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <map>
#include <string>
#include <set>
#include <vector>
#include <algorithm>
//#include <dirent.h>

#include "SATSolver.h"
#include "guardedVal.h"
#include "StringHTable.h"
#include "Tvalue.h"
#include "SynthInSolver.h"
#include "BooleanDAG.h"



#define Dout(msg) /* msg */

using namespace std;

class Tvalue;
class CTRL_node;




class varRange{
	public:
	int varID;
	int range;	
	varRange(int vid, int r): varID(vid), range(r){};
	varRange(const varRange& vr):varID(vr.varID), range(vr.range){};
	varRange& operator=(const varRange& vr){varID=vr.varID; range=vr.range; return *this;};
};



class SolverHelper {
	StringHTable2<int> memoizer;

	StringHTable2<int> intmemo;

	bool doMemoization;
    map<string, int> varmap;
    map<string, int> arrsize;
	map<string, Tvalue> controls;
	map<string, SynthInSolver*> sins;
	map<int, int> bitToInt;
	map<int, int> iToBit;
    int varCnt;
	int lastVar;
    SATSolver& mng;
	vector<char> tmpbuf;
  map<string, BooleanDAG*> numericalAbsMap;

	/*
	This function is in charge of instantiating new synthesizers.
	*/
	Synthesizer* newSynthesizer(const string& name, FloatManager& _fm);


	int freshBoolIntVar(int boolVar);

	int setStr(int id1, char op, int id2){
		id1 = id1>0 ? (id1<<1) : ((-id1)<<1 | 0x1);
		id2 = id2>0 ? (id2<<1) : ((-id2)<<1 | 0x1);
		int p = 0;
		char* tch = &tmpbuf[0];
		writeInt(tch, id1, p);
		tch[p] = op; p++;
		writeInt(tch, id2, p);
		tch[p] = 0;		
		return p;
	}

	int setStrTV(Tvalue& tv);

	int setStriMux(iVar cond, iVar* a, int last){
		int p = 0;
		if(last*10 > tmpbuf.size()){ tmpbuf.resize(last * 11); }
		char* tch = &tmpbuf[0];
		{
			int tt = cond;
			tt = tt>0 ? (tt<<1) : ((-tt)<<1 | 0x1);
			writeInt(tch, tt, p);
			tch[p] = ':'; p++;
		}
		for(int ol = 0; ol < last; ++ol){
			int tt = a[ol];
			tt = tt>0 ? (tt<<1) : ((-tt)<<1 | 0x1);
			writeInt(tch, tt, p);
			tch[p] = ','; p++;
		}
		tch[p-1] = 0;
		return p-1;
	}
	void addGV(char* tch, int& p, const guardedVal& gv);

	int setStrBO(int* a, int last, char separator = '|', int ofst = 1){
		int p = 0;
		if(last*10 > tmpbuf.size()){ tmpbuf.resize(last * 11); }
		char* tch = &tmpbuf[0];
		for(int ol = 0; ol < last; ++ol){
			int tt = a[ol+ofst];
			tt = tt>0 ? (tt<<1) : ((-tt)<<1 | 0x1);
			writeInt(tch, tt, p);
			tch[p] = separator; p++;
		}
		tch[p-1] = 0;
		return p-1;
	}

	int setStrChoice(int a, int b, int c){
		a = a>0 ? (a<<1) : ((-a)<<1 | 0x1);
		b = b>0 ? (b<<1) : ((-b)<<1 | 0x1);
		c = c>0 ? (c<<1) : ((-c)<<1 | 0x1);
		int p = 0;
		char* tch = &tmpbuf[0];
		writeInt(tch, a, p);
		tch[p] = '?'; p++;
		writeInt(tch, b, p);
		tch[p] = ':'; p++;
		writeInt(tch, c, p);
		tch[p] = 0;		
		return p;
	}

	void regIclause(int id, Tvalue& tv);
public:
    int YES;
	string lastErrMsg;
	int getLastVar(){ return lastVar; }
	map<string, int>::const_iterator arrsize_begin(){
		return arrsize.begin();
	}

	iVar bitToI(int bitid){
		if (bitid == YES) {
			return getIntConst(1);
		}
		if (bitid == -YES) {
			return getIntConst(0);
		}
		return bitToInt[bitid];
	}
	const map<string, SynthInSolver*>& get_sins() {
		return sins;
	}


	int intToBit(int id);
	void addSynthSolver(const string& name, const string& syntype, vector<Tvalue>& inputs, vector<Tvalue>& outputs, FloatManager& _fm);


	void writeDIMACS(ofstream& dimacs_file);


	bool isOK(){
		return mng.isOK();
	}

	map<string, int>::const_iterator arrsize_end(){
		return arrsize.end();
	}

	int sval(int var){
		int t = 0;
		if(!doMemoization){ return var; }
		if(var != YES && var != -YES){
			if(var>0){ t = mng.isValKnown(var); }
			else{ t = -mng.isValKnown(-var); }
			if(t != 0){ 
				t = YES*t; 
			}else{ t = var; }
		}else{
			t = var;
		}		 
	    return t;
	}
  
  
  SolverHelper(SATSolver& mng_p):mng(mng_p), tmpbuf(1000) {
	varCnt = 1;
	YES = 0;
	lastVar = 0;
	doMemoization = true;
    }
	void addHelperC(Tvalue& tv);
	int newYES(){
		if(YES == 0 || !doMemoization){
			YES = newAnonymousVar();		
			mng.setVarClause(YES);
		}else{
			Dout(cout<<"Repeating yes = "<<YES<<endl);
		}
		return YES;
	}

	void setMemo(bool b){
		doMemoization = b;
	}

	SATSolver& getMng(){
		return mng;
	}

	void nextIteration(){
		memoizer.nextIter();
	}

	void getStats(){
		memoizer.stats(cout);
	}
	void printAllVars(){
		cout<<"@#@#@#@#@#@#     Outputing Var Map"<<endl;				
		for(int i=1; i<=lastVar; ++i){
			cout<<" var id = "<<i<<"\t "<<mng.getVarVal(i)<<endl;
		}
	}

    void reset() {
		memoizer.clear();
		intmemo.clear();
		varmap.clear();
		arrsize.clear();
		lastVar = 0;
		varCnt = 1;
		YES = 0;
    };

    void print() {
	for(map<string, int>::iterator it= varmap.begin(); it != varmap.end(); ++it) {
	    if( arrsize.find( (it)->first ) !=  arrsize.end() ) {
		int start = (it)->second;
		int size = arrsize[(it)->first];
		int vv = 0;
		int cc = 1;
		for(int i=0; i<size; ++i) {			
			if(mng.getVarVal(start+i) == 1){
				vv = vv + cc;
			}
			cc = cc*2;
		}
		cout<<(it)->first<<"["<<start<<", "<<(start+size-1)<<"]=   "<<vv<<endl;
	    } else {
		cout<<(it)->second<<"\t"<<(it)->first<<"=   "<<mng.getVarVal((it)->second)<<endl;
	    }
	}
    }

	bool checkVar(const string& arName){
		return arrsize.find(arName) != arrsize.end();
	}
    int getVarCnt() { return varCnt; }

    void declareVar(const string& vname) {
	int idx = mng.newVar();
	lastVar = idx;
	Dout( cout<<"declare "<<vname<<"  "<<idx<<endl );
	varmap[vname]= idx;
	++varCnt;
    }


	void outputVarMap(ostream& out){
		cout<<" Outputing a map of size "<<varmap.size()<<endl;
		for( map<string, int>::iterator it = varmap.begin(); it != varmap.end(); ++it){
			const string& arName = it->first;
			int frst = it->second;
			int size = arrsize[arName]; 
			out<<"* declare "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl;
		}		
	}

    void declareInArr(const string& arName, int size) {
		map<string, int>::iterator fit = arrsize.find(arName);
		if(fit != arrsize.end()){
			Assert(fit->second == size, "You declared the same array with a different size earlier!");
		}else{
			int i = 0;
			int idx;
			int frst = -1;
			if( size > 0 ) {
				idx =  mng.newInVar();
				i++;
				frst = idx;
			}
			for(; i<size; ++i) {
				idx =  mng.newInVar();
			}
			lastVar = idx;
			Dout( cout<<"declareIn "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl );
			mng.annotateInput(arName, frst, size);
			varmap[arName] = frst;
			arrsize[arName] = size;
			Assert(size==0 ||  idx == (frst+size-1) , "This is bad, idx != (frst+size-1)");
			varCnt += size;	
		}	
    }

	Tvalue& declareControl(CTRL_node* ctrlnode);
	Tvalue& getControl(CTRL_node* ctrlnode);
	Tvalue& getControl(const string& name);

    void makeArrNoBranch(const string& arName) {
	int var = varmap[arName];
	int sz = arrsize[arName];
	for(int i=0; i<sz; ++i) mng.disableVarBranch(var+i);
    }

    int getVar(const string& vname) {
	return varmap[vname];
    }

    int getArr(const string& arName, int idx) {
    	Assert( varmap.find(arName) != varmap.end(), "var "<<arName<<" not found");
		return varmap[arName] + idx;
    }

    int getArrSize(const string& arName) {
    	Assert( arrsize.find(arName) != arrsize.end(), "var "<<arName<<" not found");
		return arrsize[arName];	
    } 	
    /* Allocate a block of variables (default is one), return first id. */
    int newAnonymousVar (int n = 1) {
	Assert (n >= 1, "must allocate at least one variable");

	int ret = -1;
	do {
	    int tmp = mng.newVar ();
		Assert(!doMemoization || tmp == lastVar +1 , "Oh no, what have I done!! lastVar="<<lastVar<<" tmp="<<tmp);
		lastVar = tmp;
	    if (ret < 0)
		ret = tmp;
	    mng.disableVarBranch(tmp);
	} while (--n);

	return ret;
    }
    void setYes(int yes) {
	YES = yes;
    }

	void setYes() {
		YES = newAnonymousVar();
		mng.setVarClause(YES);
    }


	int intClause(Tvalue& tv);

	iVar getIntConst(int val);

	int plus(int x, int y);
	int minus(int x, int y);
	int times(int x, int y);

	int mod(int x, int y);

	int div(int x, int y);

	int mux(iVar cond, int len, iVar* choices);

	int inteq(iVar x, iVar y);

	int intlt(int x, int y);

    int addEqualsClause (int a, int x = 0);
    int addChoiceClause (int a, int b, int c, int x = 0);
    int addXorClause (int a, int b, int x = 0);
    int addOrClause (int a, int b, int x = 0);
    int addBigOrClause (int* a, int last);
    int addAndClause (int a, int b, int x = 0);	

	int addExPairConstraint(int* pairs, int npairs);

    void addEquateClause (int a, int b);
    void addAssertClause (int a);

	///Return true if it was possible, false if it was not.
	bool assertIfPossible(int a){
		return mng.assertIfPossible(a);
	}
	virtual bool tryAssignment(int a){
		return mng.tryAssignment(a);
	}
	void addHardAssertClause (int a);
	void addAssumeClause (int a);
	void addRetractableAssertClause (int a);
	bool ignoreOld(){
		return mng.ignoreOld();
	}
    int assertVectorsDiffer (int v1, int v2, int size);
    // int select(int choices[], int control, int nchoices, int bitsPerChoice);
    // int selectMinGood(int choices[], int control, int nchoices, int bitsPerChoice);
    // int arbitraryPerm(int input, int insize, int controls[], int ncontrols, int csize);
    void getSwitchVars (vector<int>& switchID, int amtsize, gvvec& output);


	void getSwitchVarsBig(vector<int>& switchID, int amtsize, gvvec& output);

	void addHelperC(int l1, int l2);


	Range getRange(iVar id) {
		return mng.getRange(id);
	}
  
  void setNumericalAbsMap(map<string, BooleanDAG*> numericalAbsMap_p) {
    numericalAbsMap = numericalAbsMap_p;
  }
};

/*
 * Methods for adding various clauses to the solver.
 *
 * Note the deployment of obvious minimization tricks in cases of fixed
 * (true/false) arguments.
 */
inline int
SolverHelper::addEqualsClause (int a, int x)
{
    Assert (a != 0, "input id cannot be zero in addEqualsClause");

    /* No left-value, return right-value. */
    if (x == 0)
	return a;

    /* Add clause. */
    mng.addEqualsClause (x, a);

    return x;
}

inline int
SolverHelper::addChoiceClause (int a, int b, int c, int x)
{
    Assert (a != 0 && b != 0 && c != 0, "input ids cannot be zero (addChoiceClause)");
	a = sval(a); b = sval(b); c = sval(c);
    /* Check for shortcut cases. */
    if (a == YES || b == c)
	return addEqualsClause (b, x);
    if (a == -YES)
	return addEqualsClause (c, x);
    if (b == -c)
	return addXorClause (a, -b, x);

	if(b == YES)
	return addOrClause(a, c, x);
	if(b == -YES)
	return addAndClause(-a, c, x);

	if(c == YES)
	return addOrClause(-a, b, x);
	if(c == -YES)
	return addAndClause(a, b, x);

    /* Allocate fresh result variable as necessary. */
	if (x == 0){
		if(doMemoization){
			int l = this->setStrChoice(a,b,c);
			int rv;
			int tt = lastVar+1;
			if(this->memoizer.condAdd(&tmpbuf[0], l, tt, rv)){
				int xx = mng.isValKnown(rv);
				if(xx != 0){  return xx*YES; }
				return rv;
			}		
		}
		x = newAnonymousVar ();
		// Assert(tt == x, "This is an invariant that shouldn't be violated");
	}

    /* Add clause. */
    mng.addChoiceClause (x, a, b, c);

    return x;
}

inline int
SolverHelper::addXorClause (int a, int b, int x)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero (addXorClause)");
	a = sval(a); b = sval(b); 
    /* Check for shortcut cases (prefer fixed results first). */
    if (a == b)
	return addEqualsClause (-YES, x);
    if (a == -b)
	return addEqualsClause (YES, x);
    if (b == YES)
	return addEqualsClause (-a, x);
    if (b == -YES)
	return addEqualsClause (a, x);
    if (a == YES)
	return addEqualsClause (-b, x);
    if (a == -YES)
	return addEqualsClause (b, x);

    /* Allocate fresh result variable as necessary. */
   	if (x == 0){
		if(doMemoization){
			int l = this->setStr(min(a,b), '^' ,max(a,b));
			int rv;
			int tt = lastVar+1;
			if(this->memoizer.condAdd(&tmpbuf[0], l, tt, rv)){
				int xx = mng.isValKnown(rv);
				if(xx != 0){  return xx*YES; }
				return rv;
			}		
		}
		x = newAnonymousVar ();
		//Assert(tt == x, "This is an invariant that shouldn't be violated");
	}

    /* Add clause. */
    mng.addXorClause (x, a, b);

    return x;
}

inline int
SolverHelper::addOrClause (int a, int b, int x)
{
	if(x==0){
		return -addAndClause(-a, -b, x);
	}else{
		return addAndClause(-a, -b, -x);
	}	
}

inline
int SolverHelper::addExPairConstraint(int* pairs, int npairs){
	int cnt = 0;
	for(int i=0; i<npairs; ++i){
		int x = sval(pairs[2*i]);
		int y = sval(pairs[2*i+1]);
		if(x==-YES || y ==-YES){
			continue;
		}
		if(x==YES){
			return y;
		}
		if(y==YES){
			return x;
		}
		if(x==-y){
			continue;
		}
		pairs[2*cnt] = min(x, y);
		pairs[2*cnt+1] = max(x,y);
		++cnt;
	}
	if(cnt == 0){
		return -YES;
	}
	if(cnt == 1){
		return addAndClause(pairs[0], pairs[1]);
	}
	if(cnt==2){
		if(
			(pairs[0] == -pairs[2] && pairs[1] == -pairs[3])  ||
			(pairs[0] == -pairs[3] && pairs[1] == -pairs[2])
			){
			return addXorClause(pairs[0], -pairs[1]);
		}

	}

	if(doMemoization){
		int l = this->setStrBO(pairs, cnt*2, '/', 0);
		int rv;
		int tt = lastVar+1;
		if(this->memoizer.condAdd(&tmpbuf[0], l, tt, rv)){
			int xx = mng.isValKnown(rv);
			if(xx != 0){  rv = xx*YES; }
			return rv;
		}		
	}
	int out = newAnonymousVar ();
	mng.addExPairConstraint(pairs, cnt, out);
	return out;
}


/*
 * This encodes a[0] == (a[1] OR a[2] OR ...  OR a[last]).
 */
// TODO xzl: when a[0]==0, it generates a new variable a[0] = OR(a[1..last])?
// TODO xzl: any case, it overwrites a[0]?
inline int
SolverHelper::addBigOrClause (int *a, int last)
{
    Assert (a, "array of input ids cannot be null");
    
	int nw = 1;
	int ol = 1;
	for(;ol <= last; ++ol){
		int sva = sval(a[ol]); 
		if(sva == YES){
			return (a[0] = addEqualsClause (YES, a[0]));
		}
		if(sva != -YES){
			a[nw] = a[ol];
			++nw;
		}
	}
	last = nw-1;


    /* Check for shortcut cases. */
    if (last == 0)
	return (a[0] = addEqualsClause (-YES, a[0]));
    else if (last == 1)
	return (a[0] = addEqualsClause (a[1], a[0]));
    else if (last == 2)
	return (a[0] = addOrClause (a[1], a[2], a[0]));

    /* Allocate fresh result variable as necessary. */

	if (a[0] == 0){
		if(doMemoization){
			int l = this->setStrBO(a, last);
			int rv;
			int tt = lastVar+1;
			if(this->memoizer.condAdd(&tmpbuf[0], l, tt, rv)){
				int xx = mng.isValKnown(rv);
				if(xx != 0){  rv = xx*YES; }
				return (a[0] = rv);
			}		
		}
		a[0] = newAnonymousVar ();		
	}

    /* Store output variable. */
    int o = a[0];

    /* Add clause. */
    mng.addBigOrClause (a, last);

    /* Restore and return output variable. */
    return (a[0] = o);
}

inline int
SolverHelper::addAndClause (int a, int b, int x)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero (addAndClause)");
	a = sval(a); b = sval(b);
    /* Check for shortcut cases (prefer fixed results first). */
    if (a == -YES || b == -YES || a == -b)
	return addEqualsClause (-YES, x);
    if (b == YES || a == b)
	return addEqualsClause (a, x);
    if (a == YES)
	return addEqualsClause (b, x);

    /* Allocate fresh result variable as necessary. */
	if (x == 0){
		if(doMemoization){
			int l = this->setStr(min(a,b), '&' ,max(a,b));
			int rv;
			if(this->memoizer.condAdd(&tmpbuf[0], l, lastVar+1, rv)){
				int xx = mng.isValKnown(rv);
				if(xx != 0){  return xx*YES; }
				return rv;
			}
		}
		x = newAnonymousVar ();

	}

    /* Add clause. */
    mng.addAndClause (x, a, b);

    return x;
}

inline void
SolverHelper::addEquateClause (int a, int b)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero (addEquateClause)");

    /* Vacuously true. */
    if (a == b)
	return;

    /* Vacuously false. */
    if (a == -b) {
	cout << "Equality cannot be satisfied, aborting solver" << endl;
	exit (1);  /* FIXME arbitrary termination behavior, double check! */
    }

    /* Identify informed cases, or add an explicit equality clause. */
    if (a == YES)
	mng.assertVarClause (b);
    else if (a == -YES)
	mng.assertVarClause (-b);
    else if (b == YES)
	mng.assertVarClause (a);
    else if (b == -YES)
	mng.assertVarClause (-a);
    else
	mng.addEquateClause (a, b);
}

inline void
SolverHelper::addAssertClause (int a)
{
    Assert (a != 0, "input id cannot be zero in addAssertClause");

     /* Vacuously true. */ 
    if (a == YES)
        return; 
    

    dout ("asserting " << a);

    /* Otherwise, assertion clause necessary. */
    mng.assertVarClause (a);
}


inline void
SolverHelper::addAssumeClause (int a)
{
    Assert (a != 0, "input id cannot be zero in addHardAssertClause");

     /* Vacuously true. */ 
    if (a == YES)
        return; 

    dout ("asserting " << a);

    /* Otherwise, assertion clause necessary. */
	mng.assumeVarClause(a);
}

inline void
SolverHelper::addHardAssertClause (int a)
{
    Assert (a != 0, "input id cannot be zero in addHardAssertClause");

     /* Vacuously true. */ 
    if (a == YES)
        return; 

    dout ("asserting " << a);

    /* Otherwise, assertion clause necessary. */
	mng.hardAssertVarClause(a);
}

inline void
SolverHelper::addRetractableAssertClause (int a)
{
    Assert (a != 0, "input id cannot be zero in addRetractableAssertClause");

     /* Vacuously true. */ 
    if (a == YES)
        return; 

    /* Vacuously false. */ 
    if (a == -YES) {
    	Assert(false, "Assertion cannot be valid, aborting solver");
    }

    dout ("asserting " << a);

    /* Otherwise, assertion clause necessary. */
	mng.retractableAssertClause(a);    
}





inline void
SolverHelper::getSwitchVarsBig(vector<int>& switchID, int amtsize, gvvec& output) {
	int sz = amtsize;
	Assert(switchID.size() == amtsize, "This should never happen");
	Assert(amtsize > 0, "This doesn't make sense with amtsize==0."); //TODO: Actually, it does, but for now, this assertion will help me find a bug. Need to implement support for amtsize=0.
	int val = 0;
	output.clear();
	vec<Lit> vl;

	if (doMemoization) {
		int l = this->setStrBO(&switchID[0], sz, '!', 0);
		int rv;
		int tt = lastVar + 1;
		if (this->memoizer.condAdd(&tmpbuf[0], l, tt, rv)) {
			while (((val >> amtsize) & (0x1)) == 0) {
				int x = rv + val;
				int xx = mng.isValKnown(x);
				if (xx != 0) { x = xx*YES; }
				output.push_back(guardedVal(x, val));
				++val;
			}
			return;
		}
	}

	while (((val >> amtsize) & (0x1)) == 0) {
		int x = newAnonymousVar();
		for (int i = 0; i < sz; ++i) {
			if (((val >> i) & (0x1)) == 0) {
				mng.addHelper2Clause(-x, -switchID[i]);
			} else {
				mng.addHelper2Clause(-x, switchID[i]);
			}
		}
		output.push_back(guardedVal(x, val));
		++val;
		vl.push(lfromInt(x));
	}
	mng.addHelperClause(vl);
}


inline void
SolverHelper::getSwitchVars (vector<int>& switchID, int amtsize,  gvvec& output )
{
	Assert(switchID.size() == amtsize, "This should never happen");
	Assert( amtsize > 0, "This doesn't make sense with amtsize==0."); //TODO: Actually, it does, but for now, this assertion will help me find a bug. Need to implement support for amtsize=0.
	int amtrange = 1;
	Assert(amtsize <= 16, "Casting an bit-vector to an integer is only supported for up to 16 bits.");
	for(int i=0; i<amtsize && i<16; ++i) amtrange *= 2;
	//////////////////////////////////////////////////////
	gvvec tmpVect(amtrange);
	int lastsize = 1;
	//int lastRoundVars = getVarCnt();
	gvvec vals(1);	
	if( (-switchID[amtsize-1]) == YES || switchID[amtsize-1]==YES){
		vals[0].guard = YES;
		if( switchID[amtsize-1] > 0 ){
			vals[0].value = 1;
		}else{
			vals[0].value = 0;
		}
		lastsize = 1;
	}else{
		vals.resize(2);
		vals[0].value = 0;
		vals[0].guard = -(switchID[amtsize-1]);
		vals[1].value = 1;
		vals[1].guard = (switchID[amtsize-1]);
		lastsize = 2;
	}
	
	
	for(int i=1; i<amtsize; ++i){		
		int curval = switchID[amtsize-1-i]; 
		if( (-curval) == YES || curval == YES){
			int v = (curval > 0)? 1:0;
			for(int j=0; j<lastsize; ++j){
				tmpVect[j].value = vals[j].value*2 + v;
				tmpVect[j].guard = vals[j].guard;
				Assert(j<amtrange, "This is out of range amtsize = "<<amtsize );
			}
		}else{			
			for(int j=0; j<lastsize; ++j){
				Assert((2*j+1)<amtrange, "This is out of range");
				tmpVect[2*j].value = vals[j].value*2;
				tmpVect[2*j].guard = this->addAndClause(vals[j].guard, -curval);
				tmpVect[2*j+1].value= vals[j].value*2 + 1;
				tmpVect[2*j+1].guard = this->addAndClause(vals[j].guard, curval);
			}
			lastsize = lastsize*2;	
		}
		vals.resize(lastsize);
		for(int j=0; j<lastsize; ++j){
			vals[j] = tmpVect[j];
		}
	}
	Assert( lastsize <= amtrange, "Sizes don't match: (lastsize > amtrange) ls="<<lastsize<<", ar="<<amtrange<<", as="<<amtsize);	
	output.clear();
	for(gvvec::iterator it = vals.begin(); it != vals.end(); ++it){
		if(it->guard != -YES){
			output.push_back(*it);
		}
		if(it->guard == YES){
			output.clear();
			output.push_back(*it);
			return;
		}
	}	
	return ;
	//////////////////////////////////////////////////////
}


typedef enum{LEFT, RIGHT} direction;



class bitVector{
	int size;
	unsigned* data;
	public:
	bitVector():size(-1){}
	void init(int s){
		size = s;
		int sz = s/32 + ((s%32)==0?0:1);
		data = new unsigned[sz];
		for(int i=0; i<sz; ++i){
			data[i] = 0;
		}
	}
	bitVector(int s):size(s){
		int sz = s/32 + ((s%32)==0?0:1);
		data = new unsigned[sz];
		for(int i=0; i<sz; ++i){
			data[i] = 0;
		}
	}
	bitVector(const bitVector& bv):size(bv.size){
		int sz = size/32 + ((size%32)==0?0:1);
		data = new unsigned[sz];
		for(int i=0; i<sz; ++i){
			data[i] = bv.data[i];	
		}
	}
	inline void set(int s){
		data[s/32] |= (0x1 << (s%32));
	}
	inline void set(int* s, int len){
		int sz = len/32 + ((len%32)==0?0:1);
		int jj=0;
		for(int i=0; i<sz; ++i){
			unsigned tmp=0;
			for(int j=0; j<32; ++j, ++jj){
				 tmp |= ((s[jj]==1)? 1:0)<<j;	
			}
			data[i] = tmp;
		}
	}
	
	inline bool get(int s){
		return (data[s/32]>> (s%32) )>0;
	}
	virtual ~bitVector(){
		if(size>=0){
			delete [] data;	
		}
	}
};

#endif /*BOOLEANTOCNF_H_*/
