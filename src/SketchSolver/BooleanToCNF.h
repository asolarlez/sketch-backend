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
	
	varDir(SATSolver& mng_p):mng(mng_p){
		varCnt = 1;
	}
	
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
		Dout( cout<<"declare "<<vname<<"  "<<varCnt<<endl );
		varmap[vname]= varCnt;
		++varCnt;
		mng.newVar();
	}
	
	void declareArr(const string& arName, int size){
		Dout( cout<<"declare "<<arName<<"["<<size<<"] "<<varCnt<<"-"<<(varCnt+size-1)<<endl );
		varmap[arName] = varCnt;
		arrsize[arName] = size;
		varCnt += size;
		for(int i=0; i<size; ++i) mng.newVar();
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
		mng.newVar();
		mng.disableVarBranch(varCnt);
		return varCnt++;
	}
};


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

inline varRange getSwitchVars(SATSolver& mng, varDir& dir, int switchID, int amtsize){
	int amtrange = 1;
	for(int i=0; i<amtsize; ++i) amtrange *= 2;
	//////////////////////////////////////////////////////
	int lastsize = 1;
	int lastRoundVars = dir.getVarCnt();
	mng.addEqualsClause(dir.newAnonymousVar(), -(switchID+amtsize-1));
	mng.addEqualsClause(dir.newAnonymousVar(),  (switchID+ amtsize-1));
	for(int i=1; i<amtsize; ++i){
		lastsize = lastsize*2;
		int roundVars = lastRoundVars;
		lastRoundVars = dir.getVarCnt();
		for(int j=0; j<lastsize; ++j){
			int cvar = roundVars + j;
			mng.addAndClause(dir.newAnonymousVar(), cvar, -(switchID + amtsize-1-i));
			mng.addAndClause(dir.newAnonymousVar(), cvar, (switchID + amtsize-1-i));
		}
	}
	lastsize = lastsize*2;
	Assert( lastsize == amtrange, "Sizes don't match: (lastsize != amtrange) "<<lastsize<<", "<<amtrange);			
	int roundVars = lastRoundVars;
	return varRange(roundVars, amtrange);
	//////////////////////////////////////////////////////					
}

inline varRange getSwitchVars(SATSolver& mng, varDir& dir, vector<int> switchID, int amtsize){
	Assert(switchID.size() == amtsize, "This should never happen");
	int amtrange = 1;
	for(int i=0; i<amtsize; ++i) amtrange *= 2;
	//////////////////////////////////////////////////////
	int lastsize = 1;
	int lastRoundVars = dir.getVarCnt();
	mng.addEqualsClause(dir.newAnonymousVar(), -(switchID[amtsize-1]));
	mng.addEqualsClause(dir.newAnonymousVar(),  (switchID[amtsize-1]));
	for(int i=1; i<amtsize; ++i){
		lastsize = lastsize*2;
		int roundVars = lastRoundVars;
		lastRoundVars = dir.getVarCnt();
		for(int j=0; j<lastsize; ++j){
			int cvar = roundVars + j;
			mng.addAndClause(dir.newAnonymousVar(), cvar, -(switchID[amtsize-1-i]));
			mng.addAndClause(dir.newAnonymousVar(), cvar, (switchID[amtsize-1-i]));
		}
	}
	lastsize = lastsize*2;
	Assert( lastsize == amtrange, "Sizes don't match: (lastsize != amtrange) "<<lastsize<<", "<<amtrange);			
	int roundVars = lastRoundVars;
	return varRange(roundVars, amtrange);
	//////////////////////////////////////////////////////					
}

inline varRange getSwitchVars(SATSolver& mng, varDir& dir, const string& switchvar){
	return getSwitchVars(mng, dir, dir.getArr(switchvar, 0), dir.getArrSize(switchvar));
}



typedef enum{LEFT, RIGHT} direction;

inline void shiftArrNdet(SATSolver& mng, varDir& dir, const string& dest, const string& source, const string& shamt, direction shift){
	int amtsize = dir.getArrSize(shamt);
	Dout( cout<<" "<<dest<<"= "<<source<<(shift == LEFT?" << ":" >> ")<<shamt<<"("<<amtsize<<")"<<endl);
		
	Assert(dir.getArrSize(source) == dir.getArrSize(dest)
			," Sizes don't match "<<dir.getArrSize(source)<<", "<<dir.getArrSize(dest));
	int arsize = dir.getArrSize(source);
	varRange vr = getSwitchVars(mng, dir, shamt);
	int switchVars = vr.varID;
	int amtrange = vr.range;

	for(int aridx=0; aridx<arsize; ++aridx){
		mng.setVarClause(-(dir.newAnonymousVar()));
		for(int j=0; j<amtrange; ++j){
			if( shift == LEFT && j+aridx<arsize){
				int cvar = dir.newAnonymousVar();
				mng.addAndClause(cvar, switchVars+j, dir.getArr(source, j+ aridx));
				int cvar2 = dir.newAnonymousVar();
				mng.addOrClause(cvar2, cvar, cvar-1);
			}			
			if( shift == RIGHT && aridx - j>=0){
				int cvar = dir.newAnonymousVar();
				mng.addAndClause(cvar, switchVars+j, dir.getArr(source, aridx-j));
				int cvar2 = dir.newAnonymousVar();
				mng.addOrClause(cvar2, cvar, cvar-1);
			}
		}
		mng.addEqualsClause(dir.getArr(dest, aridx), dir.getVarCnt()-1);
	}
}


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
