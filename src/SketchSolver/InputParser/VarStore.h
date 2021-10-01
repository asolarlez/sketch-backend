#ifndef VARSTORE_H
#define VARSTORE_H

#include <utility>
#include <vector>
#include <string>
#include <map>
#include "BasicError.h"
#include "SynthInSolver.h"

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


        void getArrRec(vector<int>* ret) const
        {
            ret->push_back(getInt());
            if(next != nullptr && next->defined)
            {
                next->getArrRec(ret);
            }
        }
        bool defined = false;
	public:
		string name;
		objP* next;
		OutType* otype;
		int index;
		vector<int> vals;
		bool isNeg;

		OutType* getOtype() const
        {
		    return otype;
        }

		int get_size() const
        {
		    return globalSize();
        }

		string getName()
		{
			return name;
		}

		virtual ~objP(){
		    defined = false;
			if(next != NULL){ delete next; }
		}
		objP(string  nm, int size, OutType* _otype):name(std::move(nm)),vals(size),otype(_otype), isNeg(false), index(0), next(NULL), defined(true){
		    assert(_otype != NULL);
		}
		objP(const objP& old):vals(old.vals), name(old.name), otype(old.otype), isNeg(old.isNeg), index(old.index), defined(old.defined){
		    if(old.next != NULL){next=new objP(*old.next);}else{next=NULL;} }
		objP& operator=(const objP& old){ 
			vals = old.vals; name = old.name; isNeg = old.isNeg; index = old.index;  otype = old.otype; defined = old.defined;
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
					next = new objP(name, vals.size(), otype);
				}				
				next->makeArr(start+1, end);
			}else{
				if(next != NULL){
					delete next;
					next = NULL;
				}
			}
		}
		int arrSize(){ if(next==NULL){ return 1; }else{ return next->arrSize() + 1;  } }
		int size() const { return vals.size(); }
		int globalSize() const { if(next == NULL){ return size();} return next->globalSize() + size(); }
		int resize(int n){ int x=0; vals.resize(n); if(next != NULL){ x=next->resize(n); } return x+n; }
		objP* setBit(size_t i, int val){ 
			if(i<vals.size()){ 
				vals[i] = val; 
				return this;
			}else{ 
				Assert(next != NULL, "bad bad"); 
				return next->setBit(i-vals.size(), val);
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
				if(next != NULL){ return next->getInt(idx); }
                else {return -1;}
			}
			Assert(false,"Control shouldn't reach here");
		}
		vector<int>* getArr()
        {
		    vector<int>* ret = new vector<int>();
		    getArrRec(ret);
		    return ret;
        }


        void setArr(vector<int> *arr) {
            objP* at = this;
            for(int i = 0;i<arr->size();i++)
            {
                assert(at != NULL && at->defined);
                at->setVal(arr->at(i));
                at = at->next;
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
				size_t t = vals.size();
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

		void setVal(size_t v){
			if(v<0){
				v = -v;
				isNeg = true;
			}
			{
				size_t t = vals.size();
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
			for(size_t i=0; i<vals.size(); ++i){
				out<<(vals[i]==1?1:0);
			}
			if(next!= NULL){ out<<"|"; next->printBit(out); }
		}
		void printContent(ostream& out) const{			
			out << getInt();
			if(next!= NULL){ out<<"|"; next->printContent(out); }
		}

		bool increment(){
			for(size_t i=0; i<vals.size(); ++i){
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

		/**
		If it is an array, then after the first N elements, we set to zero with probability 1-sparseDeg
		*/
		void makeRandom(float sparseDeg, int n=10){
			int P  = 10000;
			int q = P*sparseDeg;

			if(n > 0 || (rand() % P) < q ){
				for(size_t i=0; i<vals.size(); ++i){
					vals[i] = (rand() & 0x3) > 0? -1 : 1;
				}
			}else{
				for(size_t i=0; i<vals.size(); ++i){
					vals[i] = -1 ;
				}
			}
			if(next!= NULL){ next->makeRandom(sparseDeg, n-1); }
		}

		void makeRandom(){/* Bias towards zeros */
			for(size_t i=0; i<vals.size(); ++i){
				vals[i] = (rand() & 0x3) > 0? -1 : 1;
			}
			if(next!= NULL){ next->makeRandom(); }
		}
		void zeroOut(){/* Bias towards zeros */
			for(size_t i=0; i<vals.size(); ++i){
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

    map<string, SynthInSolver*> synths;
    map<string, string> synthouts;

	VarStore(){
		bitsize=0;
	}

	VarStore* copy()
    {

	    Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
	    Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

	    VarStore* ret = new VarStore();
	    ret->bitsize = bitsize;

	    vector<pair<int, string> > index_as_vec;

        for(auto it : index)
        {
            index_as_vec.emplace_back(it.second, it.first);
        }

        sort(index_as_vec.begin(), index_as_vec.end());

	    for(const auto& it : index_as_vec)
        {
            ret->insertObj(it.second, it.first, new objP(objs[it.first]));
        }
	    return ret;
    }
	
	typedef vector<objP>::iterator iterator;
	iterator begin(){ return objs.begin(); }
	iterator end(){ return objs.end(); }
	void makeRandom(float sparseArray){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].makeRandom(sparseArray);
		}
	}
	void makeRandom(){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].makeRandom();
		}
	}
	void zeroOut(){ 
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].zeroOut();
		}
	}
	bool increment(const string& name){
		// cout<<"Upgraded "<<name<<" ";
		int idx = getId(name);
		
		objP& tmp = objs[idx];
		bool rv = tmp.increment();
		// tmp.printContent(cout);
		//cout<<endl;
		return rv;
	}

	void insertObj(const string& name, int idx, objP* obj)
    {
	    Assert(index.find(name) == index.end(), name + " should not be present in index.");
	    AssertDebug(idx == objs.size(), "idx, " + std::to_string(idx) + " should be the same as objs.size().");
	    objs.push_back(*obj);
	    index[name] = idx;
    }

	void newArr(const string& name, int nbits, int arrsz, OutType* otype){
		Assert(index.count(name)==0, name<<": This array already existed!!");
		int begidx = objs.size();
		index[name] = begidx;
		objs.emplace_back(name, nbits, otype);
		objs[begidx].makeArr(0, arrsz);
		bitsize += nbits*arrsz;
	}

    void setArr(const string& name, vector<int>* arr) {
	    Assert(index.find(name) != index.end(), name + " not found.");
        objs[index[name]].setArr(arr);
	}

	void newVar(const string& name, int size, OutType* otype){
		Assert(index.count(name)==0, name<<": This variable already existed!!");
		int begidx = objs.size();
		objs.emplace_back(name, size, otype);
		index[name] = begidx;
		bitsize += size;
	}

	void setVarVal(const string& name, int val, OutType* otype){
		int idx;
		if(index.count(name)!=0){
			idx = getId(name);
		}else{
			objs.push_back(objP(name, 5, otype));
			idx = objs.size()-1;
      		index[name] = idx;
		}
		objs[idx].setVal(val);
	}

	void resizeVar(const string& name, int size){
		int idx = getId(name);
		{
			objP& tmp = objs[idx];
			bitsize -= tmp.globalSize();
			int x = tmp.resize(size);
			bitsize += x;
		}
	}
	void resizeArr(const string& name, int arrSize){
		int idx = getId(name);
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
		for(size_t i=0; i<objs.size(); ++i){
			objs[i].printBit(out);
		}
	}
	void finalizeSynthOutputs() {
		synthouts.clear();
		for (auto sit = synths.begin(); sit != synths.end(); ++sit) {
			stringstream ss;
			sit->second->print(ss);
			synthouts[sit->first] = ss.str();
		}
	}
	void printContent(ostream& out) const{
		for(size_t i=0; i<objs.size(); ++i){
			out << objs[i].name << ":";
			objs[i].printContent(out);
			out << endl;
		}
		for (auto sit = synths.begin(); sit != synths.end(); ++sit) {
			//out << sit->first << ":";
			sit->second->print(out);
			//out << endl;
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
		for(size_t i=0; i<in.size(); ++i){
			setBit(i, in[i]=='1'? 1 : -1);
		}
	}
	vector<int> serialize() const{
		vector<int> out;
		for(size_t t=0; t<objs.size(); ++t){
			const objP& tmp = objs[t];
			out.insert(out.end(), tmp.vals.begin(), tmp.vals.end());			
		}
		return out;
	}
	void setBit(int i, int val){
		bool found = false;
		for(size_t t=0; t<objs.size(); ++t){
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
		return objs[getId(name)].getInt();
	}
	objP& getObj(const string& name){ 
		return objs[getId(name)];
	}
	int getId(const string& name){
		AssertDebug(index.find(name) != index.end(), "Var " + name + " does't exists in this VarStore.")
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

