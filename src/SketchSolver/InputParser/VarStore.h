#ifndef VARSTORE_H
#define VARSTORE_H

#include <vector>
#include <string>
#include <map>
#include "BasicError.h"

using namespace std;


template<typename T>
int intFromBV(T& bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){		
		if( bv[start + i] > 0){
			nval += t;	
		}
		t = t*2;
	}
	return nval;
}


// VarStore -- Keeps the mapping of node in the DAG vs its value.
class VarStore{
private:


public:
	class objP{
	public:
		string name;
		vector<int> vals;
		bool isNeg;
		objP(const string& nm, int size):name(nm),vals(size), isNeg(false){	}
		objP(const objP& old):vals(old.vals), name(old.name), isNeg(old.isNeg){}
		objP& operator=(const objP& old){ vals=old.vals; name=old.name; isNeg = old.isNeg; return *this;}
		int size(){ return vals.size(); }
		int resize(int n){ vals.resize(n); return n; }
		void setBit(int i, int val){ vals[i] = val; }
		int getInt() const{
			int t = intFromBV(vals, 0, vals.size());
			return isNeg? -t : t; 
		}
		
		void setVal(int v){
			if(v<0){
				v = -v;
				isNeg = true;
			}
			{
				int t = vals.size();
				vals.clear();
				while(v != 0){
					vals.push_back(v&1);
					v = v >> 1;
				}
				if(t > vals.size()){
					vals.resize(t, 0);
				}
			}
		}
		void printBit(ostream& out) const{
			for(int i=0; i<vals.size(); ++i){
				out<<(vals[i]==1?1:0);
			}
		}
		void makeRandom(){/* Bias towards zeros */
			for(int i=0; i<vals.size(); ++i){
				vals[i] = (rand() & 0x3) > 0? -1 : 1;
			}
		}
		void zeroOut(){/* Bias towards zeros */
			for(int i=0; i<vals.size(); ++i){
				vals[i] = -1;
			}
		}
	};

private:
	vector<objP> objs;
	map<string, int> index;
	int bitsize;
	objP& getObj(const string& name){ 
		return objs[index[name]];
	}
public:
	VarStore(){
		bitsize=0;
	}	
	
	typedef vector<objP>::iterator iterator;
	iterator begin(){ return objs.begin(); }
	iterator end(){ return objs.end(); }
	void makeRandom(){ 
		for(int i=0; i<objs.size(); ++i){
			objs[i].makeRandom();
		}
	}
	void zeroOut(){ 
		for(int i=0; i<objs.size(); ++i){
			objs[i].zeroOut();
		}
	}
	void newVar(const string& name, int size){
		Assert(index.count(name)==0, "This variable already existed!!");
		objs.push_back(objP(name, size));
		index[name] = objs.size()-1;
		bitsize += size;
	}

	void setVarVal(const string& name, int val){
		int idx;
		if(index.count(name)!=0){
			idx = index[name];
		}else{
			objs.push_back(objP(name, 5));
			idx = objs.size()-1;
		}
		objs[idx].setVal(val);
	}

	void resizeVar(const string& name, int size){
		objP& tmp = getObj(name);
		bitsize -= tmp.size();
		tmp.resize(size);
		bitsize += size;		
	}
	int getBitsize() const{ 
		return bitsize;
	}
	int getIntsize(){
		return objs.size();
	}
	void printBrief(ostream& out) const{
		for(int i=0; i<objs.size(); ++i){
			objs[i].printBit(out);
		}
	}
	bool contains(const string& name) const{
		return index.count(name)>0;
	}
	void setFromString(const string& in){
		for(int i=0; i<in.size(); ++i){
			setBit(i, in[i]=='1'? 1 : -1);
		}
	}
	vector<int> serialize() const{
		vector<int> out;
		for(int t=0; t<objs.size(); ++t){
			const objP& tmp = objs[t];
			out.insert(out.end(), tmp.vals.begin(), tmp.vals.end());			
		}
		return out;
	}
	void setBit(int i, int val){
		bool found = false;
		for(int t=0; t<objs.size(); ++t){
			objP& tmp = objs[t];
			if(i < tmp.size()){
				tmp.setBit(i, val);
				found = true;
				break;
			}else{
				i = i-tmp.size();
			}
		}
		Assert(found, "This is a bug");
	}
	int operator[](const string& name) {
		return objs[index[name]].getInt();
	}
	friend VarStore join(const VarStore& v1 , const VarStore& v2);
};

inline VarStore join(const VarStore& v1 , const VarStore& v2){
	VarStore rv = v1;
	int sz = v1.objs.size();
	rv.objs.insert(rv.objs.end(), v2.objs.begin(), v2.objs.end());
	for(map<string, int>::const_iterator it = v2.index.begin(); it != v2.index.end(); ++it){
		rv.index[it->first] = it->second + sz;
	}
	rv.bitsize = v1.bitsize + v2.bitsize;
	return rv;
}

#endif

