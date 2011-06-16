#pragma once

#include <vector>
#include <cmath>
#include "BasicError.h"

using namespace std;

template<typename T>
class fsiter{
	typedef typename vector<T*>::iterator iter;
	unsigned vers;
	iter cur;	
	iter end;	
public:
	fsiter(int v):vers(v){

	}

	fsiter<T>& operator=(fsiter<T>& f){
		cur = f.cur;
		end = f.end;
		vers = f.vers;
		return *this;
	}

	fsiter(iter p_cur, iter p_end, int v ): cur(p_cur), end(p_end), vers(v){
		
	}	
	template<typename U> friend class FastSet;

	bool checkIter(int v){
		return (v == vers);
	}

	bool operator!=(const fsiter& ot){
		return this->cur != ot.cur;
	}

	bool operator==(const fsiter& ot){
		return this->cur == ot.cur;
	}

	fsiter& operator++(){
		do{
		++cur;
		}while(cur != end && *cur == NULL);
		
		return *this;
	}

	T* operator*(){
		return *this->cur;
	}

};


template<typename T>
struct FastSetTraits
{
    static inline unsigned 
    hash (T* in, int sz) 
    {
		/*
		double fo = (((unsigned)in)>>2);
		fo = sin(fo)*10000000;
		unsigned tmp = fo;
		unsigned m = 1 << sz;
		m = m-1;
		return (tmp & m)<<2;
		*/

		unsigned tmp = (unsigned) in>>2;
		tmp = tmp * (tmp + 4297);
		tmp = tmp + (tmp >> 2) + (tmp >> 5) + (tmp >> 21) + (tmp >> 28);		
		
		unsigned m = 1 << sz;
		m = m-1;
		return (tmp & m)<<2;
    }
};





#define CNST 4
template<typename T>
class FastSet
{
    typedef FastSetTraits<T> traits;

vector<T*> store;
int sz;
int _size;
fsiter<T> _end;
unsigned vers;


#if 0
	unsigned hash(T* in){
		/*
		double fo = (((unsigned)in)>>2);
		fo = sin(fo)*10000000;
		unsigned tmp = fo;
		unsigned m = 1 << sz;
		m = m-1;
		return (tmp & m)<<2;
		*/

		unsigned tmp = (unsigned) in>>2;
		tmp = tmp * (tmp + 4297);
		tmp = tmp + (tmp >> 2) + (tmp >> 5) + (tmp >> 21) + (tmp >> 28);		
		
		unsigned m = 1 << sz;
		m = m-1;
		return (tmp & m)<<2;
		
	}
#endif



public:	
	typedef fsiter<T> iterator;

	int size() const{
		return _size;
	}
	void recompEnd(){
		fsiter<T> tmp( ((FastSet<T>*) this)->store.end(), ((FastSet<T>*) this)->store.end(), vers);
		_end = tmp;
	}

	FastSet(void):_end(0)
	{
		vers = 0;
		sz = 1;
		unsigned tmp = (CNST<<sz)+2;
		store.resize( tmp, NULL);		
		_size = 0;
		recompEnd();
	}

	FastSet(const FastSet& fs): sz(fs.sz), store(fs.store), _size(fs._size), _end(0){
		recompEnd();
		vers = 0;
	}


	FastSet& operator=(const FastSet& fs){
		sz = fs.sz;
		store = fs.store;
		_size = fs._size;
		recompEnd();
		vers = 0;
		return *this;
	}

	iterator end() const{
		return _end;
	}
	
	bool checkIter(iterator& it){
		bool tmp = it.checkIter(vers);
		return tmp;
	}

	iterator begin() const{
		if(_size == 0){ return end(); }
		typename vector<T*>::iterator it = (( FastSet<T>*) this)->store.begin();
		while(*it == NULL){ ++it; }
		return fsiter<T>(it, ((FastSet<T>*) this)->store.end(), vers);
	}

	void resize(){
		++vers;
		++this->sz;
		// if(( store.size() / _size ) > 2){  cout<<" ration on resize "<<store.size()<<"/"<<_size<<"  "<<( store.size() / _size )<<endl; }
		store.resize((CNST<<sz)+2, NULL);		
		unsigned m = 3;
		m = ~m;

		unsigned sz = (store.size()/2) + 2;  // XXX/cgjones: probably not the best name ...
		for(unsigned i=0; i<sz; ++i){
			T* tmp = store[i];
			if(tmp == NULL){ continue; }
			//unsigned loc = hash( tmp );
                        unsigned loc = traits::hash (tmp, this->sz);
			if(loc != (i & m)){ 
				store[i]= NULL; 
				int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
				
				if(store[lp2] == NULL){
					store[lp2] = tmp; continue;
				}
				if(store[lp3] == NULL){
					store[lp3] = tmp; continue;
				}

				if(store[loc] == NULL){
					store[loc] = tmp; continue;
				}
				if(store[lp1] == NULL){
					store[lp1] = tmp; continue;
				}

				if(store[lp4] == NULL){
					store[lp4] = tmp; continue;
				}
				if(store[lp5] == NULL){
					store[lp5] = tmp; continue;
				}
				store[i] = tmp;
				resize();
				break;
			}
		}		
		recompEnd();
	}

	int count(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		bool t0 = store[l] == val;
		bool t1 = store[lp1] == val;
		bool t2 = store[lp2] == val;
		bool t3 = store[lp3] == val;
		bool t4 = store[lp4] == val;
		bool t5 = store[lp5] == val;
		return (t0 || t1 || t2 || t3 || t4 || t5) ? 1 : 0;
	}

	void erase(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		bool t0 = store[l] == val;
		bool t1 = store[lp1] == val;
		bool t2 = store[lp2] == val;
		bool t3 = store[lp3] == val;
		bool t4 = store[lp4] == val;
		bool t5 = store[lp5] == val;

		if(t0){
			store[loc] = NULL; --_size; return;
		}
		if(t1){
			store[lp1] = NULL; --_size; return;
		}
		if(t2){
			store[lp2] = NULL; --_size; return;
		}
		if(t3){
			store[lp3] = NULL; --_size; return;
		}
		if(t4){
			store[lp4] = NULL; --_size; return;
		}
		if(t5){
			store[lp5] = NULL; --_size; return;
		}
	}

	void erase(iterator& it){
		if(it.cur != store.end()){
			--_size;
			*it.cur = NULL;
		}
	}

	iterator find(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		bool t0 = store[l] == val;
		bool t1 = store[lp1] == val;
		bool t2 = store[lp2] == val;
		bool t3 = store[lp3] == val;
		bool t4 = store[lp4] == val;
		bool t5 = store[lp5] == val;

		if(t0){
			return fsiter<T>(store.begin() + loc, store.end(), vers);
		}
		if(t1){
			return fsiter<T>( store.begin() + lp1, store.end(), vers);
		}
		if(t2){
			return fsiter<T>(store.begin() + lp2, store.end(), vers);
		}
		if(t3){
			return fsiter<T>(store.begin() + lp3, store.end(), vers);
		}
		if(t4){
			return fsiter<T>(store.begin() + lp4, store.end(), vers);
		}
		if(t5){
			return fsiter<T>(store.begin() + lp5, store.end(), vers);
		}
		return fsiter<T>(store.end(), store.end(), vers);
	}



	template<typename IT>
	void insert(IT beg, IT end){
		for(IT it = beg; it != end; ++it){
			insert(*it);
		}
	}


	void clear(){
		sz = 1;
		unsigned tmp = (CNST<<sz)+2;
		store.resize( tmp, NULL);
		for(int i=0; i<tmp; ++i){ store[i] = NULL; }
		_size = 0;
		++vers;
		recompEnd();
	}


	void insert(T* val){
		//unsigned loc = hash(val);
       unsigned loc = traits::hash(val, sz);
	   
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		T*& sl = store[l];
		T*& slp1 = store[lp1];
		T*& slp2 = store[lp2];
		T*& slp3 = store[lp3];
		T*& slp4 = store[lp4];
		T*& slp5 = store[lp5];
	   
		bool t0 = sl == val;
		bool t1 = slp1 == val;
		bool t2 = slp2 == val;
		bool t3 = slp3 == val;
		bool t4 = slp4 == val;
		bool t5 = slp5 == val;

		if(!(t0 || t1 || t2 || t3 || t4 || t5)){


			if(slp2 == NULL){
				++_size;
				slp2 = val; return;
			}
			if(slp3 == NULL){
				++_size;
				slp3 = val; return;
			}

			if(sl == NULL){
				++_size;
				sl = val; return;
			}
			if(slp1 == NULL){
				++_size;
				slp1 = val; return;
			}
			
			if(slp4 == NULL){
				++_size;
				slp4 = val; return;
			}
			if(slp5 == NULL){
				++_size;
				slp5 = val; return;
			}
			resize();
			insert(val);
		}		
	}

	void swap(FastSet<T>& t2){
		std::swap(store, t2.store);
		std::swap(sz, t2.sz);
		std::swap(_size, t2._size);
		recompEnd();
		t2.recompEnd();
		++vers;
		++t2.vers;
	}
public:

	~FastSet(void)
	{
	}
};

template<typename T>
void swap(FastSet<T>& t1, FastSet<T>& t2){
	t1.swap(t2);
}
