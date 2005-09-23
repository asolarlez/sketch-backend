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
#include "SAT.h"

#define Assert( in, msg) if(!(in)){cout<<msg<<endl;}
#define Dout( out ) out


using namespace std;
//This function encodes x == a xor b;
inline void addXorClause(SAT_Manager mng, int x, int a, int b, int gid=0){
	Dout( cout<<" "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
	{ int tmp[] = { -(x), -(a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { -(x), (a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), -(a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), (a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}

//This function encodes x == a or b;
inline void addOrClause(SAT_Manager mng, int x, int a, int b, int gid=0){
	Dout( cout<<" "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (a), (b)}; SAT_AddClauseSigned(mng, tmp, 3, gid);}	
}

//This function encodes x == a and b;
inline void addAndClause(SAT_Manager mng, int x, int a, int b, int gid=0){
	Dout( cout<<" "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a), -(b)}; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}

//This function encodes x == a;
inline void addEqualsClause(SAT_Manager mng, int x, int a, int gid=0){
	Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
}

inline void setVarClause(SAT_Manager mng, int x, int gid=0){
	Dout( cout<<" set "<<x<<";"<<endl );
	{ int tmp[] = { (x)}; SAT_AddClauseSigned(mng, tmp, 1, gid);}
}



class varDir{
	map<string, int> varmap;
	map<string, int> arrsize;
	int varCnt;
	SAT_Manager mng;
public:
	
	varDir(SAT_Manager mng_p){
		varCnt = 1;
		mng = mng_p;
	}
	
	void setMng(SAT_Manager mng_p){	
		mng = mng_p;
	}
	
	
	void print(){
		for(map<string, int>::iterator it= varmap.begin(); it != varmap.end(); ++it){
			if( arrsize.find( (it)->first ) !=  arrsize.end() ){
				int start = (it)->second;
				int size = arrsize[(it)->first];
				for(int i=0; i<size; ++i){
					cout<<start+i<<"\t"<<(it)->first<<"["<<i<<"]=   "<<SAT_GetVarAsgnment(mng, start+i)<<endl;
				}
			}else{
				cout<<(it)->second<<"\t"<<(it)->first<<"=   "<<SAT_GetVarAsgnment(mng, (it)->second)<<endl;
			}
		}
	}
	
	
	int getVarCnt(){ return varCnt; }
	
	void declareVar(const string& vname){
		Dout( cout<<"declare "<<vname<<endl );
		varmap[vname]= varCnt;
		++varCnt;
		SAT_AddVariable(mng);
	}
	
	void declareArr(const string& arName, int size){
		Dout( cout<<"declare "<<arName<<"["<<size<<"]"<<endl );
		varmap[arName] = varCnt;
		arrsize[arName] = size;
		varCnt += size;
		for(int i=0; i<size; ++i) SAT_AddVariable(mng);
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
		SAT_AddVariable(mng);
		return varCnt++;
	}
};

typedef enum{LEFT, RIGHT} direction;

inline void shiftArrNdet(SAT_Manager mng, varDir& dir, const string& dest, const string& source, const string& shamt, direction shift){
		int amtsize = dir.getArrSize(shamt);
		Dout( cout<<" "<<dest<<"= "<<source<<(shift == LEFT?" << ":" >> ")<<shamt<<"("<<amtsize<<")"<<endl);
		int amtrange = 1;
		for(int i=0; i<amtsize; ++i) amtrange *= 2;
		
		Assert(dir.getArrSize(source) == dir.getArrSize(dest)
				," Sizes don't match "<<dir.getArrSize(source)<<", "<<dir.getArrSize(dest));
		int arsize = dir.getArrSize(source);
		int switchVars;
		{
			//////////////////////////////////////////////////////
			int lastsize = 1;
			int lastRoundVars = dir.getVarCnt();
			addEqualsClause(mng,  dir.newAnonymousVar(), -dir.getArr(shamt, amtsize-1));
			addEqualsClause(mng,  dir.newAnonymousVar(), dir.getArr(shamt, amtsize-1));
			for(int i=1; i<amtsize; ++i){
				lastsize = lastsize*2;
				int roundVars = lastRoundVars;
				lastRoundVars = dir.getVarCnt();
				for(int j=0; j<lastsize; ++j){
					int cvar = roundVars + j;
					addAndClause(mng, dir.newAnonymousVar(), cvar, -dir.getArr(shamt, amtsize-1-i));
					addAndClause(mng, dir.newAnonymousVar(), cvar, dir.getArr(shamt, amtsize-1-i));
				}
			}
			lastsize = lastsize*2;
			Assert( lastsize == amtrange, "Sizes don't match: (lastsize != amtrange) "<<lastsize<<", "<<amtrange);			
			int roundVars = lastRoundVars;
			switchVars = roundVars;
			//////////////////////////////////////////////////////				
		}

		for(int aridx=0; aridx<arsize; ++aridx){
			setVarClause(mng, -(dir.newAnonymousVar()));
			for(int j=0; j<amtrange; ++j){
				if( shift == LEFT && j+aridx<arsize){
					int cvar = dir.newAnonymousVar();
					addAndClause(mng, cvar, switchVars+j, dir.getArr(source, j+ aridx));
					int cvar2 = dir.newAnonymousVar();
					addOrClause(mng, cvar2, cvar, cvar-1);
				}
				
				if( shift == RIGHT && aridx - j>=0){
					int cvar = dir.newAnonymousVar();
					addAndClause(mng, cvar, switchVars+j, dir.getArr(source, aridx-j));
					int cvar2 = dir.newAnonymousVar();
					addOrClause(mng, cvar2, cvar, cvar-1);
				}
			}
			addEqualsClause(mng, dir.getArr(dest, aridx), dir.getVarCnt()-1);
		}
}

#endif /*BOOLEANTOCNF_H_*/
