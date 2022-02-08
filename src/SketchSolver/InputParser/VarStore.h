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
        int the_bit = bv[start + i];
        assert(the_bit == 0 || the_bit == 1 || the_bit == -1);
		if( the_bit > 0){
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

        void getArrRec(vector<pair<int, int> >* ret) const
        {
            assert(get_is_array());
            ret->push_back(getIntAndSize());
            if(next != nullptr && next->defined)
            {
                next->getArrRec(ret);
            }
        }
        bool defined = false;
        bool is_array;
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

		string getName() const
		{
			return name;
		}

		~objP(){
            clear();
		}

        void clear()
        {
            name.clear();
            vals.clear();
		    defined = false;
            if(next != nullptr) {delete next; next = nullptr;};
        }

		objP(string  nm, int size, OutType* _otype):
        name(std::move(nm)),vals(size),otype(_otype), isNeg(false), index(0), next(NULL), defined(true){
		    assert(_otype != NULL);
            if(_otype == OutType::INT_ARR || _otype == OutType::BOOL_ARR || _otype == OutType::FLOAT_ARR) {
                is_array = true;
                assert(_otype->isArr);
            }
            else
            {
                is_array = false;
                assert(!_otype->isArr);
            }
		}

        objP(const objP& old):
        vals(old.vals), name(old.name), otype(old.otype), isNeg(old.isNeg), index(old.index), defined(old.defined), is_array(old.is_array){
		    if(old.next != NULL){
                next=new objP(*old.next);
            }
            else{next=NULL;}
        }

		objP& operator=(const objP& old){ 
			vals = old.vals; name = old.name; isNeg = old.isNeg; index = old.index;  otype = old.otype; defined = old.defined;
            is_array = old.is_array;
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

        bool operator == (const objP& other) const
        {
            const bool debug = false;
            if(other.vals.size() != vals.size())
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            for(int i = 0;i<vals.size();i++) {
                if (vals[i] != other.vals[i]) {
                    if(vals[i] == 0 && other.vals[i] == -1)
                    {
                        //TODO: fix this. Artifact from CEGISSolver. -1 interpreted as 0 in int intFromBV(T& bv, int start, int nbits)
                    }
                    else {
                        if (debug) {
                            cout << "return false" << endl;
                            AssertDebug(false, "not eq");
                        }
                        return false;
                    }
                }
            }
            if(name != other.name)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(otype != other.otype)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(isNeg!= other.isNeg)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(index != other.index)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(defined != other.defined)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(is_array != other.is_array)
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if((next == nullptr) != (other.next == nullptr))
            {
                if(debug) {
                    cout << "return false" << endl;
                    AssertDebug(false, "not eq");
                }
                return false;
            }
            if(next != nullptr)
            {
                bool ret = (*next == *other.next);
                if(debug) {
                    if (!ret) {
                        cout << "return false" << endl;
                        AssertDebug(false, "not eq");
                    }
                    else{
                        cout << "return true" << endl;
                    }
                }
                return ret;
            }
            if(debug) {
                cout << "return true" << endl;
            }
            return true;
        }

		void makeArr(int start, int end){
            assert(is_array);
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
		int arrSize(){ assert(is_array); if(next==NULL){ return 1; }else{ return next->arrSize() + 1;  } }
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

        pair<int, int> getIntAndSize() const
        {
            return make_pair(getInt(), (int)vals.size());
        }

		int getInt(int idx) const{
            assert(!is_array);
			if(this->index==idx){
				return getInt();
			}else{
				if(next != NULL){ return next->getInt(idx); }
                else {return -1;}
			}
			Assert(false,"Control shouldn't reach here");
		}
		vector<pair<int, int> >* getArr() const
        {
            assert(is_array);
		    vector<pair<int, int> >* ret = new vector<pair<int, int> >();
		    getArrRec(ret);
		    return ret;
        }

        void setArr(vector<int> *arr) {
            assert(is_array);
            objP* at = this;
            for(int i = 0;i<arr->size();i++)
            {
                assert(at != NULL && at->defined);
                at->setVal(arr->at(i));
                at = at->next;
            }
        }

		///Return false if objP did not have enough bits to be made equal to v.
		bool setValSafe(int v) {
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

        bool get_is_array() const {
            return is_array;
        }
    };

private:
	vector<objP> objs;
	map<string, int> index;
	int bitsize = 0;
		
public:

    map<string, SynthInSolver*> synths;
    map<string, string> synthouts;

	void clear()
	{
//		for(auto it:objs)
//		{
//            it.clear();
//		}

        objs.clear();
        index.clear();

        Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
        Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

        delete this;
	}

    int size() const
    {
        return objs.size();
    }

	VarStore * clone() const {

	    Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
	    Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

	    VarStore* ret = new VarStore();
	    ret->bitsize = bitsize;

	    vector<pair<int, string> > index_as_vec;

        for(const auto& it : index)
        {
            index_as_vec.emplace_back(it.second, it.first);
        }

        sort(index_as_vec.begin(), index_as_vec.end());


        for(int i = 0;i<index_as_vec.size(); i++)
        {
            pair<int, string> it = index_as_vec[i];
            assert(it.first == i);
            ret->insertObj(it.second, it.first, objP(objs[it.first]));
        }

//	    for(const auto& it : index_as_vec)
//        {
//            ret->insertObj(it.second, it.first, new objP(objs[it.first]));
//        }

	    return ret;
    }

    auto begin()const { return objs.begin();}
	auto end()const{ return objs.end(); }
    auto begin() { return objs.begin();}
    auto end(){ return objs.end(); }

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

	void insertObj(const string& name, int idx, const objP obj)
    {
	    AssertDebug(index.find(name) == index.end(), name + " should not be present in index.");
	    AssertDebug(idx == objs.size(), "idx, " + std::to_string(idx) + " should be the same as objs.size() = " + std::to_string(objs.size()) + ".");
	    objs.push_back(obj);
	    index[name] = idx;
    }

	void newArr(const string& name, int nbits, int arrsz, OutType* otype){
		Assert(index.count(name)==0, name<<": This array already existed!!");
		int begidx = objs.size();
		index[name] = begidx;
		objs.emplace_back(name, nbits, otype);
		objs[begidx].makeArr(0, arrsz);
		bitsize += nbits*arrsz;
        assert(objs[begidx].get_is_array());
	}

    void setArr(const string& name, vector<int>& arr) {
	    Assert(index.find(name) != index.end(), name + " not found.");
        objs[index[name]].setArr(&arr);
        assert(objs[index[name]].get_is_array());
	}

	void newVar(const string& name, int nbits, OutType* otype){
		Assert(index.count(name)==0, name<<": This variable already existed!!");
		int begidx = objs.size();
		objs.emplace_back(objP(name, nbits, otype));
		index[name] = begidx;
		bitsize += nbits;
        assert(!objs[index[name]].get_is_array());
	}

	void setVarVal(const string& name, int val, OutType* otype){
		int idx;
		if(index.count(name)!=0){
			idx = getId(name);
		}else{
			objs.emplace_back(objP(name, 5, otype));
			idx = objs.size()-1;
      		index[name] = idx;
		}
        if(otype == OutType::BOOL)
        {
            assert(val == 0 || val == 1);
        }
		objs[idx].setVal(val);
        assert(!objs[idx].get_is_array());
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
            assert(tmp.get_is_array());
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
			auto oo = this->getObjConst(name);
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
        int id = getId(name);
        AssertDebug(!objs[id].get_is_array(), "Can't return array as an int.");
		return objs[getId(name)].getInt();
	}

    const objP& getObjConst(const string& name) const {
        int id = getId(name);
        assert(id < objs.size());
        return objs.at(id);
    }
	objP& _getObj(const string& name) {
        int id = getId(name);
        assert(id < objs.size());
		return objs.at(id);
	}

    bool has(const string& name) const
    {
        return index.find(name) != index.end();
    }

	int getId(const string& name) const{
		AssertDebug(has(name), "Var " + name + " does't exists in this VarStore.")
		return index.at(name);
	}
    objP& _getObj(int id){
        return objs[id];
    }
	const objP& getObjConst(int id) const{
		return objs[id];
	}
	friend VarStore old_join(const VarStore& v1 , const VarStore& v2);
	friend VarStore* produce_join(const VarStore& v1 , const VarStore& v2);

    void relabel(vector<bool_node*>& inputs){

        Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
        Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

        assert(inputs.size() == objs.size());
        index.clear();
        for(int i = 0;i<objs.size();i++)
        {
            objP* at = &objs[i];
            string prev_name = at->name;
            do {
                assert(at->name == prev_name);
                at->name = inputs[i]->get_name();
                at = at->next;
            }while(at != nullptr);
            index[objs[i].name] = i;
        }
    }
};

inline VarStore* produce_join(const VarStore& _v1, const VarStore& v2)
{
	VarStore* ret = _v1.clone();
	for(int i = 0; i < v2.objs.size(); i++) {
		ret->insertObj(v2.objs[i].name, ret->objs.size(), VarStore::objP(v2.objs[i]));
	}
	ret->bitsize += v2.bitsize;
	return ret;
}

inline VarStore old_join(const VarStore& v1 , const VarStore& v2){
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

