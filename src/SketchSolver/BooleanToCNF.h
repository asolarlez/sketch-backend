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
//#include <dirent.h>

#include "SATSolver.h"



using namespace std;


class varRange{
	public:
	int varID;
	int range;	
	varRange(int vid, int r): varID(vid), range(r){};
	varRange(const varRange& vr):varID(vr.varID), range(vr.range){};
	varRange& operator=(const varRange& vr){varID=vr.varID; range=vr.range; return *this;};
};



class SolverHelper {
    map<string, int> varmap;
    map<string, int> arrsize;
    int varCnt;
    SATSolver& mng;
public:
    int YES;
    SolverHelper(SATSolver& mng_p):mng(mng_p) {
	varCnt = 1;
	YES = 0;
    }

    void reset() {
	varmap.clear();
	arrsize.clear();
	varCnt = 1;
	YES = 0;
    };

    void print() {
	for(map<string, int>::iterator it= varmap.begin(); it != varmap.end(); ++it) {
	    if( arrsize.find( (it)->first ) !=  arrsize.end() ) {
		int start = (it)->second;
		int size = arrsize[(it)->first];
		for(int i=0; i<size; ++i) {
		    cout<<start+i<<"\t"<<(it)->first<<"["<<i<<"]=   "<<mng.getVarVal(start+i)<<endl;
		}
	    } else {
		cout<<(it)->second<<"\t"<<(it)->first<<"=   "<<mng.getVarVal((it)->second)<<endl;
	    }
	}
    }


    int getVarCnt() { return varCnt; }

    void declareVar(const string& vname) {
	int idx = mng.newVar();
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

    void declareArr(const string& arName, int size) {		
	int i = 0;
	int idx;
	int frst = -1;
	if( size > 0 ) {
	    idx =  mng.newVar();
	    i++;
	    frst = idx;
	}
	for(; i<size; ++i) {
	    idx =  mng.newVar();
	}
	Dout( cout<<"declare "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl );
	mng.annotateInput(arName, frst, size);
	varmap[arName] = frst;
	arrsize[arName] = size;
	Assert( size==0 || idx == (frst+size-1) , "This is bad, idx != (frst+size-1)");
	varCnt += size;
    }


    void declareInArr(const string& arName, int size) {
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
	Dout( cout<<"declareIn "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl );
	mng.annotateInput(arName, frst, size);
	varmap[arName] = frst;
	arrsize[arName] = size;
	Assert(size==0 ||  idx == (frst+size-1) , "This is bad, idx != (frst+size-1)");
	varCnt += size;
    }

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

    int addEqualsClause (int a, int x = 0);
    int addChoiceClause (int a, int b, int c, int x = 0);
    int addXorClause (int a, int b, int x = 0);
    int addOrClause (int a, int b, int x = 0);
    int addBigOrClause (int* a, int last);
    int addAndClause (int a, int b, int x = 0);	
    void addEquateClause (int a, int b);
    void addAssertClause (int a);

	bool ignoreOld(){
		return mng.ignoreOld();
	}
    int assertVectorsDiffer (int v1, int v2, int size);
    int select(int choices[], int control, int nchoices, int bitsPerChoice);
    int selectMinGood(int choices[], int control, int nchoices, int bitsPerChoice);
    int arbitraryPerm(int input, int insize, int controls[], int ncontrols, int csize);
    varRange getSwitchVars (vector<int>& switchID, int amtsize, vector<int>& vals);
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
    Assert (a != 0, "input id cannot be zero");

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
    Assert (a != 0 && b != 0 && c != 0, "input ids cannot be zero");

    /* Check for shortcut cases. */
    if (a == YES || b == c)
	return addEqualsClause (b, x);
    if (a == -YES)
	return addEqualsClause (c, x);
    if (b == -c)
	return addXorClause (a, -b, x);

    /* Allocate fresh result variable as necessary. */
    if (x == 0)
	x = newAnonymousVar ();

    /* Add clause. */
    mng.addChoiceClause (x, a, b, c);

    return x;
}

inline int
SolverHelper::addXorClause (int a, int b, int x)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero");

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
    if (x == 0)
	x = newAnonymousVar ();

    /* Add clause. */
    mng.addXorClause (x, a, b);

    return x;
}

inline int
SolverHelper::addOrClause (int a, int b, int x)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero");

    /* Check for shortcut cases (prefer fixed results first). */
    if (a == YES || b == YES || a == -b)
	return addEqualsClause (YES, x);
    if (b == -YES || a == b)
	return addEqualsClause (a, x);
    if (a == -YES)
	return addEqualsClause (b, x);

    /* Allocate fresh result variable as necessary. */
    if (x == 0)
	x = newAnonymousVar ();

    /* Add clause. */
    mng.addOrClause (x, a, b);

    return x;
}

/*
 * This encodes a[0] == (a[1] OR a[2] OR ...  OR a[last]).
 */
inline int
SolverHelper::addBigOrClause (int *a, int last)
{
    Assert (a, "array of input ids cannot be null");
    
	int nw = 1;
	int ol = 1;
	for(;ol <= last; ++ol){
		if(a[ol] == YES){
			return (a[0] = addEqualsClause (YES, a[0]));
		}
		if(a[ol] != -YES){
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

	if (a[0] == 0)
	a[0] = newAnonymousVar ();

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
    Assert (a != 0 && b != 0, "input ids cannot be zero");

    /* Check for shortcut cases (prefer fixed results first). */
    if (a == -YES || b == -YES || a == -b)
	return addEqualsClause (-YES, x);
    if (b == YES || a == b)
	return addEqualsClause (a, x);
    if (a == YES)
	return addEqualsClause (b, x);

    /* Allocate fresh result variable as necessary. */
    if (x == 0)
	x = newAnonymousVar ();

    /* Add clause. */
    mng.addAndClause (x, a, b);

    return x;
}

inline void
SolverHelper::addEquateClause (int a, int b)
{
    Assert (a != 0 && b != 0, "input ids cannot be zero");

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
    Assert (a != 0, "input id cannot be zero");

     /* Vacuously true. */ 
    if (a == YES)
        return; 

    /* Vacuously false. */ 
    if (a == -YES) {
    	Assert(false, "Assertion cannot be valid, aborting solver");
    }

    dout ("asserting " << a);

    /* Otherwise, assertion clause necessary. */
    mng.assertVarClause (a);
}




inline varRange
SolverHelper::getSwitchVars (vector<int>& switchID, int amtsize, vector<int>& vals)
{
	Assert(switchID.size() == amtsize, "This should never happen");
	Assert( amtsize > 0, "This doesn't make sense with amtsize==0."); //TODO: Actually, it does, but for now, this assertion will help me find a bug. Need to implement support for amtsize=0.
	int amtrange = 1;
	for(int i=0; i<amtsize && i<12; ++i) amtrange *= 2;
	//////////////////////////////////////////////////////
	vector<int> tmpVect(amtrange);
	int lastsize = 1;
	int lastRoundVars = getVarCnt();
	vals.resize(1);
	if( (-switchID[amtsize-1]) == YES || switchID[amtsize-1]==YES){
		lastRoundVars = YES;
		if( switchID[amtsize-1] > 0 ){
			vals[0] = 1;
		}else{
			vals[0] = 0;
		}
		lastsize = 1;
	}else{
		vals.resize(2);
		lastRoundVars = newAnonymousVar();
		int tmp = newAnonymousVar();
		Assert( tmp == lastRoundVars+1, "BooleanToCNF1: This is bad");
		mng.addEqualsClause(lastRoundVars, -(switchID[amtsize-1]));
		mng.addEqualsClause(tmp,  (switchID[amtsize-1]));
		vals[0] = 0;
		vals[1] = 1;
		lastsize = 2;
	}
	
	
	for(int i=1; i<amtsize; ++i){		
		int curval = switchID[amtsize-1-i]; 
		if( (-curval) == YES || curval == YES){
			int v = (curval > 0)? 1:0;
			for(int j=0; j<lastsize; ++j){
				tmpVect[j] = vals[j]*2 + v;
				Assert(j<amtrange, "This is out of range amtsize = "<<amtsize );
			}
		}else{
			int roundVars = lastRoundVars;
			lastRoundVars = newAnonymousVar();
			newAnonymousVar();			
			for(int j=1; j<lastsize; ++j){
				newAnonymousVar();
				newAnonymousVar();
			}
			for(int j=0; j<lastsize; ++j){
				int cvar = roundVars + j;
				mng.addAndClause(lastRoundVars + j*2, cvar, -(curval));
				mng.addAndClause(lastRoundVars + j*2 + 1, cvar, (curval));
				tmpVect[2*j] = vals[j]*2;
				tmpVect[2*j+1] = vals[j]*2 + 1;
				Assert((2*j+1)<amtrange, "This is out of range");
			}
			lastsize = lastsize*2;	
		}
		vals.resize(lastsize);
		for(int j=0; j<lastsize; ++j){
			vals[j] = tmpVect[j];	
		}		
	}	
	Assert( lastsize <= amtrange, "Sizes don't match: (lastsize > amtrange) ls="<<lastsize<<", ar="<<amtrange<<", as="<<amtsize);
	int roundVars = lastRoundVars;

	return varRange(roundVars, lastsize);
	//////////////////////////////////////////////////////
}


typedef enum{LEFT, RIGHT} direction;



class bitVector{
	int size;
	unsigned* data;
	public:
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
		delete [] data;	
	}
};

#endif /*BOOLEANTOCNF_H_*/
