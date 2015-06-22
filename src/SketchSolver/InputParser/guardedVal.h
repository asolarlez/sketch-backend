#ifndef __GUARDEDVAL_H
#define __GUARDEDVAL_H

#include <cstring>
#include <ostream>
#include "BasicError.h"

using namespace std;

class guardedVal{
public:
	int value;
	int guard;
	int idx;
	guardedVal():value(0), guard(0), idx(-1){}
	guardedVal(int g, int v):value(v), guard(g), idx(-1){}
	guardedVal(int g, int v, int i):value(v), guard(g), idx(i){}
	guardedVal(const guardedVal& gv): value(gv.value), guard(gv.guard), idx(gv.idx){}
	inline bool operator==(const guardedVal& gv) const{ return gv.value == value && gv.guard == guard && gv.idx == idx; }
	inline bool operator!=(const guardedVal& gv) const{ return gv.value != value || gv.guard != guard || gv.idx != idx; }
	inline guardedVal& operator=(const guardedVal& gv){ value = gv.value; guard=gv.guard; idx = gv.idx; return *this; }
	friend ostream &operator<<(ostream& out, const guardedVal& gv){
		out<<"("<<gv.guard<<", "<<gv.value;
		if(gv.idx>=0){
			out<<", "<<gv.idx<<")";
		}else{
			out<<")";
		}
		return out;
	}
};


extern int TOTBUFFERS;

class gvvec;
class tvstore{
	friend class gvvec;
private:
	guardedVal* store;
	int refc;
	int sz;
	int largest;
public:
	tvstore(int size){
		++TOTBUFFERS;
		refc = 0;
		sz = size;
		largest = 0;
		store = new guardedVal[size];
	}
	tvstore* clone(){
		tvstore* rv = new tvstore(sz);
		std::memcpy(rv->store, store, sz*sizeof(guardedVal));
		return rv;
	}
	tvstore* clone(int n){
		Assert(n >= sz, "");
		tvstore* rv = new tvstore(n);
		std::memcpy(rv->store, store, sz*sizeof(guardedVal));
		return rv;
	}

	void grow(int size){
		guardedVal* temp = new guardedVal[size];
		std::memcpy(temp, store, sz*sizeof(guardedVal));
		delete[] store;
		store = temp;
		sz = size;
	}

	void doubleSize(){
		guardedVal* temp = new guardedVal[sz*2];
		std::memcpy(temp, store, sz*sizeof(guardedVal));
		delete[] store;
		store = temp;
		sz = sz*2;
	}
	void doubleSize(guardedVal& front){
		guardedVal* temp = new guardedVal[sz*2];
		std::memcpy(temp+1, store, sz*sizeof(guardedVal));
		temp[0] = front;
		delete[] store;
		store = temp;
	}
	tvstore* clone(guardedVal& front){
		tvstore* rv = new tvstore(sz+1);
		std::memcpy(rv->store+1, store, sz*sizeof(guardedVal));
		rv->store[0] = front;
		return rv;
	}
	~tvstore(){
		--TOTBUFFERS;
		delete[] store;
	}
};

class gvvec{	
	int sz;
	tvstore* store;
	
public:
	typedef guardedVal* iterator;
	typedef guardedVal const* const_iterator;
	iterator begin(){
		Assert(store!=NULL && store->refc==1, "SHARED REF");
		return store->store;
	}
	iterator end(){
		return store->store + sz;
	}

	const_iterator begin()const{
		return store->store;
	}
	const_iterator end()const{
		return store->store + sz;
	}

	gvvec(int _sz){ 
		store = new tvstore(_sz<10 ? _sz*2 : _sz); 
		this->sz = _sz;
		store->refc++;
		store->largest=_sz;
	}
	gvvec& operator=(const gvvec& other){
		if(store != NULL){ 
			if(store->refc>1){
				store->refc--;
			}else{
				cout<<"NULLING OUT BEFORE REASSIGNING oldSize"<<store->sz<<" newsz ="<<other.sz<<endl;
				delete store;
			}
		}		
		store = other.store;
		if(store != NULL){ store->refc++; }
		sz = other.sz;		
		return *this;
	}
	gvvec(){ store=NULL; sz=0; }
	gvvec(const gvvec& other){ 
		store = other.store; 
		if(store != NULL){ store->refc++; }
		sz = other.sz;
	}
	void reserve(int n){
		if(store==NULL){ 
			store = new tvstore(n); store->refc++; store->largest=0; 
		}
		if(n<=store->sz){ 
			return; 
		}
		store->grow(n);
	}
	void clear(){
		if(store!=NULL && store->refc==1){
			store->largest = 0;
		}
		sz = 0;
	}
	int size() const{ return sz; }

	void resize(int n) {
		if(store==NULL){
			store = new tvstore(n);
			store->refc++;
			sz = n;
			store->largest = n;
			return;
		}
		Assert(store->refc>=1, "NONONO");
		if(store->sz > n){
			if(store->largest < n){
				store->largest = n;				
			}
			sz = n;
		}else{			
			if(store->refc==1){
				store->grow(n);
				store->largest = n;
				sz = n;
			}else{
				store->refc--;
				store = store->clone(n);
				store->refc++;
				store->largest=n;
				sz = n;
			}
		}
	}
	
	guardedVal& operator[](int idx){
		Assert(idx < sz, "Out of bounds");
		Assert(store!=NULL && store->refc==1, "SHARED REF");
		return store->store[idx];
	}

	void adjustInt(int adj){
		Assert(store != NULL, "NOT NULL");
		if(store->refc > 1){
			store->refc--;
			store = store->clone();
			store->refc++;
			store->largest = sz;
		}
		for(int i=0; i<sz; ++i){
			store->store[i].value *= adj;
		}

	}

	
	const guardedVal& operator[](int idx) const{
		Assert(idx < sz, "Out of bounds");
		return store->store[idx];
	}

	void push_front(guardedVal gv){
		if(store==NULL){
			push_back(gv);
			return;
		}
		if(store->refc==1){
			if(store->sz <= sz){									
				Assert(store->sz == sz, "WTF!!");
				store->doubleSize(gv);					
			}else{
				guardedVal* dst = store->store;
				guardedVal tmp = *dst;
				*dst = gv;
				++dst;
				gv = tmp;
				for(int i=0; i<sz; ++i){
					tmp = *dst;
					*dst = gv;
					++dst;					
					gv = tmp;
				}
			}
		}else{
			store->refc--;
			store = store->clone(gv);
			store->refc++;
			store->largest = sz;
		}		
		++sz;
		store->largest++;
		Assert(store->largest == sz && store->refc==1, "qpeuypkuuk");
	}

	void push_back(const guardedVal& gv){
		if(store==NULL){
			store = new tvstore(5);
			store->refc++;
			store->largest = 1;
			store->store[0] = gv;
			sz = 1;
		}else{
			if(store->largest == sz){
				if(store->sz <= sz){									
					Assert(store->sz == sz, "WTF!!");
					store->doubleSize();					
				}
				store->store[sz] = gv;
				++sz;
				store->largest++;
			}else{
				//someone else is aware of the extra space. Need to make my own copy now.
				if(store->refc<=1){
					if(store->sz <= sz){									
						Assert(false, "should never reach here because store->sz <=store->largest<=sz");
					}
				}else{
					//If we are here, largest>sz, which means store->sz > sz; so no need to grow after cloning.
					store->refc--;
					store = store->clone();
					store->refc++;
				}				
				Assert(store->sz > sz && store->refc==1, "WTF!!");
				store->store[sz] = gv;
				++sz;
				store->largest=sz;
			}
		}
	}

	~gvvec(){
		if(store==NULL){ return; }
		Assert(store->largest >= sz, "NOT GOOD");
		if(store->refc<=1){
			delete store;
		}else{
			store->refc--;
		}
	}
};



#endif 
