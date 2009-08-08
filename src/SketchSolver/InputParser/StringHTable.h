#pragma once

#include <string.h>

#include <map>
#include <vector>

using namespace std;

template<typename T>
class Ostore{
	int PAGESIZE;
	vector<T*> stringStore;
	int pos;
public:
	Ostore(){
		PAGESIZE= 1000; 
		pos = PAGESIZE;
	}
	Ostore(int sz){
		PAGESIZE= sz;
		pos = PAGESIZE;
	}
	~Ostore(){
		for(typename vector<T*>::iterator it = stringStore.begin(); it != stringStore.end(); ++it){
			delete[] *it;
		}
	}
	T* newObj(int size=1){
		size = size;
		Assert(size > 0, "Neg size doesn't make sense; ");
		if(size > PAGESIZE){
			T* rv = new T[size];
			if(stringStore.size()>0){
				T* tt = stringStore.back();
				stringStore[stringStore.size()-1] = rv;
				stringStore.push_back(tt);
			}else{
				stringStore.push_back(rv);
			}
			return rv;
		}else{
			if(pos + size > PAGESIZE){
				stringStore.push_back( new T[PAGESIZE] );
				pos = 0;
			}	
		}
		T* rv = stringStore.back();
		rv = rv + pos;
		pos = pos + size;
		return rv;		
	}
	void clear(){
		for(typename vector<T*>::iterator it = stringStore.begin(); it != stringStore.end(); ++it){
			delete[] *it;
		}
		pos = PAGESIZE;
		stringStore.clear();
	}
};


template<typename T>
class bucket{
public:
	int count;
	unsigned hash;
	char* label;
	bucket* left;
	bucket* right;
	T value;
	bucket(unsigned h, char* l, T val):value(val), left(NULL), right(NULL), label(l), hash(h){		
	}
	bucket(){};
};

template<typename T>
class StringHTable2
{	
	Ostore<char> store;
	Ostore<bucket<T> > bstore;
	vector<bucket<T>*> table;	
	unsigned mask;
	bool compare_eq(const char* s1, const char* s2){
		return strcmp(s1, s2)==0;
	}
public:

	unsigned hash(const char* k, int len){
		unsigned h = 5381;
		for(int i=0; i<len; ++i){		
			unsigned t = k[i];
			h = ((h<<5)+h)+(t*719);
		}
		/*
		unsigned h = 0;
		for(int i=0; i<len; ++i){		
			unsigned t = k[i];
			h = h ^ (t*(h+(i*733)+7829));
			h = (h>>5) | h<<(32-5);
			h = ((h>>1) & ~0xaaaaaaaau) | ((h<<1) & 0xaaaaaaaau);
		}
		*/
		return h;
	}

	StringHTable2(void):table(1024, NULL),mask(0x3FF),store(5000){
		
	}
	~StringHTable2(void){
		clear();
	}



	void add(const char* key, int len, T val){
		Assert(false, "NYI");
	}

	bool get(const char* key, int len, T& out)const{
		Assert(false, "NYI");
	}


	bool condAdd(const char* key, int len, T val, T& out){
		unsigned idx = hash(key, len);
		int tidx = (idx)& mask;
		bucket<T>*& t = table[tidx];
		if(t==NULL){
			char* nkey = store.newObj(len+1);
			strcpy(nkey, key);
			t = new(bstore.newObj()) bucket<T>(idx, nkey, val);
			out = val;
			return false;
		}else{
			bucket<T>* tt = t;
			while(tt != NULL){
				if(tt->hash < idx){
					if(tt->left == NULL){
						char* nkey = store.newObj(len+1);
						strcpy(nkey, key);
						tt->left = new(bstore.newObj()) bucket<T>(idx, nkey, val);
						out = val;						
						return false;
					}else{
						tt = tt->left;
					}
				}else{
					if(tt->hash == idx && compare_eq(key, tt->label)){						
						out = tt->value;
						return true;
					}else{
						if(tt->right == NULL){
							char* nkey = store.newObj(len+1);
							strcpy(nkey, key);
							tt->right = new(bstore.newObj()) bucket<T>(idx, nkey, val);
							out = val;
							return false;
						}else{
							tt = tt->right;
						}
					}
				}
			}
		}
	}

	void clear(){
		for(typename vector<bucket<T>*>::iterator it = table.begin(); it != table.end(); ++it){
			*it = NULL;
		}
		store.clear();
		bstore.clear();

	}
};








template<typename T>
class StringHTable
{
	map<string, T> table;
	mutable T lastNode;
	mutable string last;
public:
	StringHTable(void){}
	~StringHTable(void){}
	void add(const char* key, int len, T val){
		last = key;
		table[last] = val;
		lastNode = val;
	}

	bool get(const char* key, int len, T& out)const{
		string t(key);
		if(t==last){
			out= lastNode;
			return true;
		}
		typename map<string, T>::const_iterator it = table.find(t);
		if(it != table.end()){
			swap(last, t);
			lastNode = it->second;
			out = lastNode;
			return true;
		}else{
			return false;
		}
	}

	bool condAdd(const char* key, int len, T val, T& out){
		
			string t(key);
			if(t==last){
				out = lastNode;
				return true;
			}
			typename map<string, T>::iterator it = table.find(t);
			swap(last, t);
		
		if(it != table.end()){
			lastNode = it->second;
			out = lastNode;
			return true;
		}else{
			lastNode = val;
			out = lastNode;
			table.insert(it, make_pair(last, val));
			return false;
		}
	}

	void clear(){
		table.clear();
		last = "";
	}
};

template<typename T>
class StringHTableCheck{
	StringHTable2<T> ht2;
	StringHTable<T> ht1;
public:

	bool condAdd(const char* key, int len, T val, T& out){
		T o2;
		bool bo1 = ht1.condAdd(key, len, val, out);
		bool bo2 = ht2.condAdd(key, len, val, o2);
		Assert(bo1 == bo2, "outputs should be the same");
		Assert(out == o2, "outputs should be the same");
		return bo1;
	}
	void clear(){
		ht1.clear();
		ht2.clear();
	}
};
