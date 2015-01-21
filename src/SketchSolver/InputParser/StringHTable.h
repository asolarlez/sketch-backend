#pragma once

#include <map>
#include <vector>
#include <cstring>
#include <utility>

using namespace std;



extern char tbl[64];

inline void writeInt(char* buf, unsigned val, int& p){
	while(val != 0){
		unsigned t = val & 0x3f;
		buf[p] = tbl[t];
		p = p+1;
		val = val>>6;
	}
}



template<typename T>
class Ostore{
	int pagesize;
	vector<T*> stringStore;
	int pos;
	int newobjs;
public:
	int newObjs(){ return newobjs; }
	int pages(){ return stringStore.size(); }
	Ostore(){
		pagesize = 1000; 
		pos = pagesize;
		newobjs = 0;
	}
	Ostore(int sz){
		pagesize = sz;
		pos = pagesize;
		newobjs = 0;
	}
	~Ostore(){
		for(typename vector<T*>::iterator it = stringStore.begin(); it != stringStore.end(); ++it){
			delete[] *it;
		}
	}
	T* newObj(int size=1){
		++newobjs;
		Assert(size > 0, "Neg size doesn't make sense; ");
		if(size > pagesize){
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
			if(pos + size > pagesize){
				stringStore.push_back( new T[pagesize] );
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
		pos = pagesize;
		stringStore.clear();
	}
	void swap(Ostore<T>& t1){
		std::swap(pagesize, t1.pagesize);
		stringStore.swap(t1.stringStore);
		std::swap(pos, t1.pos);
		std::swap(newobjs, t1.newobjs);
	}
};

template<typename T>
void swap(Ostore<T>& t1, Ostore<T>& t2){
	t1.swap(t2);
}



template<typename T>
class bucket{
public:
	int count;
	unsigned hash;
	char* label;
	bucket* left;
	bucket* right;
	T value;
	bucket(unsigned h, char* l, T val):value(val), left(NULL), right(NULL), label(l), hash(h),count(0){
	}
	bucket(){};
};

#define SHTSIZE  8192

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
	int hits;
	int phase;
public:
	void nextIter(){
		if(phase % 3 == 2){
			gc(); 
		}
		++phase;	
		//cout<<" hits = "<<hits<<"\t bstoreObjs="<<bstore.newObjs()<<"\t sstorePages="<<store.pages()<<endl;	
	}
	void stats(ostream& out){
		out<<" hits = "<<hits<<"\t bstoreObjs="<<bstore.newObjs()<<"\t sstorePages="<<store.pages()<<endl;	
		deepStats(out);
	}
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

	StringHTable2(void):table(1024, (bucket<T>*)NULL),mask(0x3FF),store(SHTSIZE){
		hits = 0;
		phase=0;
	}
	~StringHTable2(void){
		clear();
	}

	void deepStats(ostream& out){
		map<int, int> /*hitcount -> count*/ hitcountHisto;
		map<int, int> /*depth -> count*/ nodesPerDepth;
		map<int, int> /*depth -> cumulative hitcount*/ hitcountPerDepth;
		map<int, int> /*hitcount -> cumdepth*/ hitcountCumDepth;
		map<int, int> /*depth -> bucketcount*/ bucketsPerDepth;

		for(int i=0; i<table.size(); ++i){
			if(table[i] != NULL){
				int jj = statComputer(table[i], hitcountHisto, 
					nodesPerDepth, hitcountPerDepth, hitcountCumDepth, 0);
				bucketsPerDepth[jj]++;
			}
		}
		map<int, int>::iterator in2 = hitcountCumDepth.begin();
		for(map<int, int>::iterator it = hitcountHisto.begin(); it != hitcountHisto.end(); ++it, ++in2){
			out<<"hitcount \t"<<it->first<<"\t"<<it->second<<"\t"<<( in2->second / it->second )<<endl;
		}
		for(map<int, int>::iterator it = bucketsPerDepth.begin(); it != bucketsPerDepth.end(); ++it){
			out<<"bucketsPerDepth \t"<<it->first<<"\t"<<it->second<<endl;
		}
	}


	int statComputer(bucket<T>* b,map<int, int>& hitcountHisto,map<int, int>& nodesPerDepth,map<int, int>& hitcountPerDepth,map<int, int>& hitcountCumDepth, int depth){
		hitcountHisto[b->count]++;
		nodesPerDepth[depth]++;
		hitcountPerDepth[depth]+= b->count;
		hitcountCumDepth[b->count]+= depth;
		int x1 = depth;
		int x2 = depth;
		if(b->left != NULL){
			x1 = statComputer(b->left, hitcountHisto, 
					nodesPerDepth, hitcountPerDepth, hitcountCumDepth, depth+1);
		}
		if(b->right != NULL){
			x2 = statComputer(b->right, hitcountHisto, 
					nodesPerDepth, hitcountPerDepth, hitcountCumDepth, depth+1);
		}
		return x1 > x2 ? x1 : x2;
	}

	void add(const char* key, int len, T val){
		Assert(false, "NYI; a;lkyyyyuj");
	}

	bool get(const char* key, int len, T& out){
		unsigned idx = hash(key, len);
		int tidx = (idx)& mask;
		bucket<T>*& t = table[tidx];
		if(t==NULL){			
			return false;
		}else{
			bucket<T>* tt = t;
			while(tt != NULL){
				if(tt->hash < idx){
					if(tt->left == NULL){										
						return false;
					}else{
						tt = tt->left;
					}
				}else{
					if(tt->hash == idx && compare_eq(key, tt->label)){						
						out = tt->value;
						++(tt->count);
						++hits;
						return true;
					}else{
						if(tt->right == NULL){							
							return false;
						}else{
							tt = tt->right;
						}
					}
				}
			}
		}
		Assert(false, "NYI; nmjkyut");
		return false;
	}

	void gchelperA(bucket<T>* b, vector<bucket<T>* >& v){
		if(b != NULL){
			if(b->left != NULL){
				gchelperA(b->left, v);
			}
			if(b->count > 0){  v.push_back(b); }
			if(b->right != NULL){
				gchelperA(b->right, v);
			}
		}
	}

	bucket<T>* gchelperB(vector<bucket<T>* >& v, int a, int b, Ostore<char>& cst,Ostore<bucket<T> >& bst){
		if(a == b){ return NULL; }
		Assert( a < b , "This is an invariant");
		int part = (a + b) / 2;
		bucket<T>* left = gchelperB(v, a, part, cst, bst);
		bucket<T>* right = gchelperB(v, part + 1, b, cst, bst);
		bucket<T>* told = v[part];
		char* nkey = cst.newObj(strlen(told->label)+1);
		strcpy(nkey, told->label);
		bucket<T>* tt = new(bst.newObj()) bucket<T>(told->hash, nkey, told->value);
		tt->left = left;
		tt->right = right;
		tt->count = told->count;
		return tt;
	}

	void gc(){
		vector<bucket<T>* > buf;
		Ostore<char> cst(SHTSIZE);
		Ostore<bucket<T> > bst;
		for(int i = 0; i<table.size(); ++i){
			buf.resize(0);
			gchelperA(table[i], buf);
			table[i] = gchelperB(buf, 0, buf.size(), cst, bst);
		}
		swap(store, cst);
		swap(bstore, bst);
	}

	/**
	Return true if the value was already there and false if you had to add it.
	*/
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
						++(tt->count);
						++hits;
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
		Assert(false, "You shouldn't reach here");
		return false;
	}

	void clear(){
		for(typename vector<bucket<T>*>::iterator it = table.begin(); it != table.end(); ++it){
			*it = NULL;
		}
		store.clear();
		bstore.clear();

	}
};





