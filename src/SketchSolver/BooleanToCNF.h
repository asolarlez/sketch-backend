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
#include <dirent.h>

#include "SATSolver.h"



using namespace std;


class varDir{
	map<string, int> varmap;
	map<string, int> arrsize;
	int varCnt;
	SATSolver& mng;
public:
	int YES;
	varDir(SATSolver& mng_p):mng(mng_p){
		varCnt = 1;
		YES = 0;
	}
	
	void reset(){
			varmap.clear();
			arrsize.clear();
			varCnt = 1;
			YES = 0;
	};
	
	void print(){
		for(map<string, int>::iterator it= varmap.begin(); it != varmap.end(); ++it){
			if( arrsize.find( (it)->first ) !=  arrsize.end() ){
				int start = (it)->second;
				int size = arrsize[(it)->first];
				for(int i=0; i<size; ++i){
					cout<<start+i<<"\t"<<(it)->first<<"["<<i<<"]=   "<<mng.getVarVal(start+i)<<endl;
				}
			}else{
				cout<<(it)->second<<"\t"<<(it)->first<<"=   "<<mng.getVarVal((it)->second)<<endl;
			}
		}
	}
	
	
	int getVarCnt(){ return varCnt; }
	
	void declareVar(const string& vname){
		int idx = mng.newVar();
		Dout( cout<<"declare "<<vname<<"  "<<idx<<endl );
		varmap[vname]= idx;
		++varCnt;
	}
	
	void declareArr(const string& arName, int size){		
		int i = 0;
		int idx;
		int frst = -1;
		if( size > 0 ){
			idx =  mng.newVar();
			i++;
			frst = idx;
		}
		for(; i<size; ++i){
			idx =  mng.newVar();
		}
		Dout( cout<<"declare "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl );
		mng.annotateInput(arName, frst, size);
		varmap[arName] = frst;
		arrsize[arName] = size;
		Assert( size==0 || idx == (frst+size-1) , "This is bad, idx != (frst+size-1)");
		varCnt += size;
	}
	
	
	void declareInArr(const string& arName, int size){
		int i = 0;
		int idx;
		int frst = -1;
		if( size > 0 ){
			idx =  mng.newInVar();
			i++;
			frst = idx;
		}
		for(; i<size; ++i){
			idx =  mng.newInVar();
		}
		Dout( cout<<"declareIn "<<arName<<"["<<size<<"] "<<frst<<"-"<<(frst+size-1)<<endl );
		mng.annotateInput(arName, frst, size);
		varmap[arName] = frst;
		arrsize[arName] = size;
		Assert(size==0 ||  idx == (frst+size-1) , "This is bad, idx != (frst+size-1)");
		varCnt += size;
	}
	
	void makeArrNoBranch(const string& arName){
		int var = varmap[arName];
		int sz = arrsize[arName];
		for(int i=0; i<sz; ++i) mng.disableVarBranch(var+i);
	}
	
	int getVar(const string& vname){
		return varmap[vname];
	}
	
	int getArr(const string& arName, int idx){
		return varmap[arName] + idx;
	}
	
	int getArrSize(const string& arName){
		return arrsize[arName];	
	}	
	int newAnonymousVar(){
		int idx = mng.newVar();
		mng.disableVarBranch(varCnt);
		return idx;
	}
	void setYes(int yes){
		YES = yes;
	}
	int addChoiceClause(int a, int b, int c, int gid=0);
 	int addXorClause(int a, int b, int gid=0);
	int addOrClause(int a, int b, int gid=0);
	int addBigOrClause(int* a, int size, int gid=0);
	int addAndClause(int a, int b, int gid=0);	
	void addEquateClause(int x, int a, int gid=0);
};

inline int varDir::addChoiceClause(int a, int b, int c, int gid){
	if( a == YES ){
		return b;
	}	
	if( a == -YES ){
		return c;
	}
	if( b == c){
		return b;	
	}
	if( b == -c){
		return addXorClause(a, -b);
	}
	int x = newAnonymousVar();
	mng.addChoiceClause(x, a, b, c, gid);
	return x;
}
inline int varDir::addXorClause(int a, int b, int gid){
	if( b == YES ){
		return -a;
	}	
	if( b == -YES ){
		return a;
	}
	if( a == YES ){
		return -b;
	}	
	if( a == -YES ){
		return b;
	}
	if( a == b){
		return -YES;	
	}
	if( a == -b){
		return YES;
	}
	int x = newAnonymousVar();	
	mng.addXorClause(x, a, b, gid);
	return x;
}
inline int varDir::addOrClause(int a, int b, int gid){
	if( b == YES ){
		return YES;
	}	
	if( b == -YES ){
		return a;
	}
	if( a == YES ){
		return YES;
	}	
	if( a == -YES ){
		return b;
	}
	if( a == b){
		return a;	
	}
	if( a == -b){
		return YES;
	}
	int x = newAnonymousVar();
	mng.addOrClause(x, a, b, gid);
	return x;
}
//This function encodes a[0] == a[1] or a[2] or ... a[size];
// so a[0] is assumed to be empty initially.
inline int varDir::addBigOrClause(int* a, int size, int gid){
	int x = newAnonymousVar();
	a[0] = x;
	mng.addBigOrClause(a , size, gid);
	return x;
}

inline int varDir::addAndClause(int a, int b, int gid){
	if( b == YES ){
		Dout( cout<<" "<<a<<"= "<<a<<" and "<<b<<"; "<<endl );
		return a;
	}	
	if( b == -YES ){
		Dout( cout<<" "<<-YES<<"= "<<a<<" and "<<b<<"; "<<endl );
		return -YES;
	}
	if( a == YES ){
		Dout( cout<<" "<<b<<"= "<<a<<" and "<<b<<"; "<<endl );
		return b;
	}	
	if( a == -YES ){
		Dout( cout<<" "<<-YES<<"= "<<a<<" and "<<b<<"; "<<endl );
		return -YES;
	}
	if( a == b){
		Dout( cout<<" "<<a<<"= "<<a<<" and "<<b<<"; "<<endl );
		return a;	
	}
	if( a == -b){
		Dout( cout<<" "<<-YES<<"= "<<a<<" and "<<b<<"; "<<endl );
		return -YES;	
	}	
	int x = newAnonymousVar();
	mng.addAndClause(x, a, b, gid);
	return x;
}


inline void varDir::addEquateClause(int x, int a, int gid){	
	if( x == a){
		return;	
	}
	if( x == YES){
		mng.assertVarClause(a);
		return;
	}
	if( x == -YES){
		mng.assertVarClause(-a);
		return;
	}
	if( a == YES){
		mng.assertVarClause(x);
		return;
	}
	if( a == -YES){
		mng.assertVarClause(-x);
		return;
	}
	mng.addEquateClause(x, a, gid);	
}


class varRange{
	public:
	int varID;
	int range;	
	varRange(int vid, int r): varID(vid), range(r){};
	varRange(const varRange& vr):varID(vr.varID), range(vr.range){};
	varRange& operator=(const varRange& vr){varID=vr.varID; range=vr.range; return *this;};
};



int assertVectorsDiffer(SATSolver& mng, varDir& dir, int v1, int v2, int size);

int select(SATSolver& mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice);

int selectMinGood(SATSolver& mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice);

int arbitraryPerm(SATSolver& mng, varDir& dir, int input, int insize, int controls[], int ncontrols, int csize);

inline varRange getSwitchVars(SATSolver& mng, varDir& dir, vector<int>& switchID, int amtsize, vector<int>& vals, int YES){
	Assert(switchID.size() == amtsize, "This should never happen");
	int amtrange = 1;
	for(int i=0; i<amtsize; ++i) amtrange *= 2;
	//////////////////////////////////////////////////////
	vector<int> tmp(amtrange);
	int lastsize = 1;
	int lastRoundVars = dir.getVarCnt();	
	int valssz = 0;
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
		lastRoundVars = dir.newAnonymousVar();
		int tmp = dir.newAnonymousVar();
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
				tmp[j] = vals[j]*2 + v;
			}
		}else{
			int roundVars = lastRoundVars;
			lastRoundVars = dir.newAnonymousVar();
			dir.newAnonymousVar();			
			for(int j=1; j<lastsize; ++j){
				dir.newAnonymousVar();
				dir.newAnonymousVar();
			}
			for(int j=0; j<lastsize; ++j){
				int cvar = roundVars + j;
				mng.addAndClause(lastRoundVars + j*2, cvar, -(curval));
				mng.addAndClause(lastRoundVars + j*2 + 1, cvar, (curval));
				tmp[2*j] = vals[j]*2;
				tmp[2*j+1] = vals[j]*2 + 1;
			}
			lastsize = lastsize*2;	
		}
		vals.resize(lastsize);
		for(int j=0; j<lastsize; ++j){
			vals[j] = tmp[j];	
		}		
	}	
	Assert( lastsize <= amtrange, "Sizes don't match: (lastsize > amtrange) "<<lastsize<<", "<<amtrange);
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
