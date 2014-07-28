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
		objP* next;
		int index;
		string name;
		vector<int> vals;
		bool isNeg;
		virtual ~objP(){
			if(next != NULL){ delete next; }
		}
		objP(const string& nm, int size):name(nm),vals(size), isNeg(false), index(0), next(NULL){	}
		objP(const objP& old):vals(old.vals), name(old.name), isNeg(old.isNeg), index(old.index){if(old.next!= NULL){next=new objP(*old.next);}else{next=NULL;} }
		objP& operator=(const objP& old){ 
			vals=old.vals; name=old.name; isNeg = old.isNeg; index=old.index; 
			if(old.next!=NULL){ 
				if(next!=NULL){
					(*next)=*old.next;  
				}else{
					next = new objP(*old.next);
				}
			}else{
				if(next!=NULL){ delete next; next=NULL;}
			}
			return *this;
		}
		void makeArr(int start, int end){			
			Assert(start < end, "Empty arr");
			index = start;
			if(start+1 < end){
				if(next == NULL){
					next = new objP(name, vals.size());
				}				
				next->makeArr(start+1, end);
			}else{
				if(next != NULL){
					delete next;
					next = NULL;
				}
			}
		}
		int size(){ return vals.size(); }
		int globalSize(){ if(next == NULL){ return size();} return next->globalSize() + size(); }
		int resize(int n){ int x=0; vals.resize(n); if(next != NULL){ x=next->resize(n); } return x+n; }
		void setBit(int i, int val){ 
			if(i<vals.size()){ vals[i] = val; }
			else{ 
				Assert(next != NULL, "bad bad"); 
				next->setBit(i-vals.size(), val);
			}
		}
		int getInt() const{
			int t = intFromBV(vals, 0, vals.size());
			return isNeg? -t : t; 
		}
		int getInt(int idx) const{
			if(this->index==idx){
				return getInt();
			}else{
				if(next != NULL){ next->getInt(idx); }
			}
		}
		void setVal(int idx, int v){
			if(this->index==idx){
				setVal(v);
			}else{
				if(next != NULL){ next->setVal(idx, v); }
			}
		}
		///Return false if objP did not have enough bits to be made equal to v.
		bool setValSafe(int v){
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
					if(vals.size()==t && v != 0){
						return false;
					}
				}
				if(t > vals.size()){
					vals.resize(t, 0);
				}
				return true;
			}
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
			if(next!= NULL){ out<<"|"; next->printBit(out); }
		}
		void printContent(ostream& out) const{			
			out << getInt();
			if(next!= NULL){ out<<"|"; next->printContent(out); }
		}

		bool increment(){
			for(int i=0; i<vals.size(); ++i){
				if(vals[i]==-1){
					vals[i] = 1;
					return true;
				}else{
					vals[i] = -1;
				}				
			}
			if(next != NULL){
				return next->increment();
			}else{
				return false;
			}
		}


		void makeRandom(){/* Bias towards zeros */
			for(int i=0; i<vals.size(); ++i){
				vals[i] = (rand() & 0x3) > 0? -1 : 1;
			}
			if(next!= NULL){ next->makeRandom(); }
		}
		void zeroOut(){/* Bias towards zeros */
			for(int i=0; i<vals.size(); ++i){
				vals[i] = -1;
			}
			if(next!= NULL){ next->zeroOut(); }
		}
	};

private:
	vector<objP> objs;
	map<string, int> index;
	int bitsize;
		
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
	bool increment(const string& name){
		// cout<<"Upgraded "<<name<<" ";
		int idx = index[name];
		
		objP& tmp = objs[idx];
		bool rv = tmp.increment();
		// tmp.printContent(cout);
		//cout<<endl;
		return rv;
	}


	void newArr(const string& name, int nbits, int arrsz){
		Assert(index.count(name)==0, name<<": This array already existed!!");
		int begidx = objs.size();
		index[name] = begidx;
		objs.push_back(objP(name, nbits));		
		objs[begidx].makeArr(0, arrsz);
		bitsize += nbits*arrsz;
	}

	void newVar(const string& name, int size){
		Assert(index.count(name)==0, name<<": This variable already existed!!");
		int begidx = objs.size();
		objs.push_back(objP(name, size));
		index[name] = begidx;
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
		int idx = index[name];
		{
			objP& tmp = objs[idx];
			bitsize -= tmp.globalSize();
			int x = tmp.resize(size);
			bitsize += x;
		}
	}
	void resizeArr(const string& name, int arrSize){
		int idx = index[name];
		{
			objP& tmp = objs[idx];
			bitsize -= tmp.globalSize();
			tmp.makeArr(0, arrSize);
			bitsize += arrSize*tmp.size();
		}
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
	void printContent(ostream& out) const{
		for(int i=0; i<objs.size(); ++i){
			out << objs[i].name << ":";
			objs[i].printContent(out);
			cout << endl;
		}
	}

	void parseContent(istream& in){
		while(in){
			string t;
			in>>t;
			if(t.size()==0){ break; }

			int s  = t.find(':');
			string name= t.substr(0, s);
			string val = t.substr(s+1);
			if(index.count(name)==0){
				continue;
			}
			objP& oo = this->getObj(name);
			oo.setVal(atoi(val.c_str()));
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
			int gsz = tmp.globalSize();
			if(i < gsz){
				tmp.setBit(i, val);
				found = true;
				break;
			}else{
				i = i-gsz;
			}
		}
		Assert(found, "This is a bug");
	}
	int operator[](const string& name) {
		return objs[index[name]].getInt();
	}
	objP& getObj(const string& name){ 
		return objs[index[name]];
	}
	int getId(const string& name){
		return index[name];
	}
	objP& getObj(int id){ 
		return objs[id];
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

