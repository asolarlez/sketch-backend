#pragma once

#include <vector>
#include <cmath>
#include "BasicError.h"
#include <cstring>

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

	fsiter<T>& operator=(const fsiter<T>& f){
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
		_end = fsiter<T>( ((FastSet<T>*) this)->store.end(), ((FastSet<T>*) this)->store.end(), vers);
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




template<typename T, typename M>
class fmiter{
	typedef pair<T*, M>* iter;
	unsigned vers;
	iter cur;	
	iter end;	
public:
	fmiter(int v):vers(v){

	}

	fmiter<T ,M>& operator=(const fmiter<T, M>& f){
		cur = f.cur;
		end = f.end;
		vers = f.vers;
		return *this;
	}

	fmiter(iter p_cur, iter p_end, int v ): cur(p_cur), end(p_end), vers(v){
#ifdef _DEBUG_B
	Assert(p_cur <= p_end, "What??");
#endif		
	}	
	template<typename U, typename P> friend class FastMap;

	bool checkIter(int v){
		return (v == vers);
	}

	bool operator!=(const fmiter& ot){
		return this->cur != ot.cur;
	}

	bool operator==(const fmiter& ot){
		return this->cur == ot.cur;
	}

	fmiter& operator++(){
		do{
		++cur;
		}while(cur != end && cur->first == NULL);
#ifdef _DEBUG_B
	Assert(cur <= end, "What??");
#endif				
		return *this;
	}

	pair<T*, M>& operator*(){
#ifdef _DEBUG_B
	Assert(cur < end, "What??");
#endif	
		return *(this->cur);
	}

	pair<T*, M>* operator->(){
#ifdef _DEBUG_B
	Assert(cur < end, "What??");
#endif	
		return this->cur;
	}

};





template<class T, class M>
class FastMap
{
    typedef FastSetTraits<T> traits;

pair<T*, M>* store;
pair<T*, M>* store_end;
int sz;
int _size;
fmiter<T, M> _end;
unsigned vers;

public:	
	typedef fmiter<T, M>iterator;

	int size() const{
		return _size;
	}
	void recompEnd(){		
		_end = fmiter<T, M>( ((FastMap<T,M>*) this)->store_end, ((FastMap<T,M>*) this)->store_end, vers);
	}

	FastMap(int initsize):_end(0){
		vers = 0;
		sz = initsize;
		unsigned tmp = (CNST<<sz)+2;
		store = new pair<T*, M>[tmp];		
		store_end = store + tmp;
		memset(store, 0, tmp*sizeof(pair<T*, M>));
		_size = 0;
		recompEnd();
	}

	FastMap(void):_end(0)
	{
		vers = 0;
		sz = 1;
		unsigned tmp = (CNST<<sz)+2;
		store = new pair<T*, M>[tmp];		
		store_end = store + tmp;
		memset(store, 0, tmp*sizeof(pair<T*, M>));
		_size = 0;
		recompEnd();
	}

	FastMap(const FastMap& fs): sz(fs.sz), _size(fs._size), _end(0){
		int tmp = fs.store_end - fs.store;
		store = new pair<T*, M>[tmp];		
		store_end = store + tmp;
		memcpy(store, fs.store, tmp*sizeof(pair<T*, M>));
		recompEnd();
		vers = 0;
	}


	FastMap& operator=(const FastMap& fs){
		sz = fs.sz;
		int tmp = fs.store_end - fs.store;
		store = new pair<T*, M>[tmp];		
		store_end = store + tmp;
		memcpy(store, fs.store, tmp*sizeof(pair<T*, M>));
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
		pair<T*, M> * it = store;
		while(it->first == NULL){ ++it; } //This is safe because the map has at least one non-null element.
		return fmiter<T,M>(it, store_end, vers);
	}

	void resize(){
		++vers;
		++this->sz;
		// if(( store.size() / _size ) > 2){  cout<<" ration on resize "<<store.size()<<"/"<<_size<<"  "<<( store.size() / _size )<<endl; }
		unsigned lsz = store_end - store;
#ifdef _DEBUG_B
	set<pair<T*, M> > vp;
	for(iterator it = begin(); it != end(); ++it){
		vp.insert(*it);
	}
	Assert(vp.size() == _size, "Not working");
#endif
		int store_size = (CNST<<sz)+2;
		pair<T*, M> * tbuf = new pair<T*, M>[store_size];	
		memset(tbuf, 0, store_size*sizeof(pair<T*,M>));
		unsigned m = 3;
		m = ~m;				
		for(unsigned i=0; i<lsz; ++i){
			pair<T*, M> tmp = store[i];
			if(tmp.first == NULL){ continue; }
			//unsigned loc = hash( tmp );
                        unsigned loc = traits::hash (tmp.first, this->sz);
			if(loc != (i & m)){ 
				int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
#ifdef _DEBUG_B
	Assert(lp5 < store_size, "What??");
#endif

				if(tbuf[lp2].first == NULL){
					tbuf[lp2] = tmp; continue;
				}
				if(tbuf[lp3].first == NULL){
					tbuf[lp3] = tmp; continue;
				}

				if(tbuf[loc].first == NULL){
					tbuf[loc] = tmp; continue;
				}
				if(tbuf[lp1].first == NULL){
					tbuf[lp1] = tmp; continue;
				}

				if(tbuf[lp4].first == NULL){
					tbuf[lp4] = tmp; continue;
				}
				if(tbuf[lp5].first == NULL){
					tbuf[lp5] = tmp; continue;
				}
				int j=i;
				while(i<lsz){
					while(tbuf[j].first != NULL){	++j; }
#ifdef _DEBUG_B
					Assert(tbuf[j].first == NULL, "??llken;kiy");
#endif
					tbuf[j] = store[i];
					++i;
					++j;
				}
				delete[] store;
				store = tbuf;
				store_end = tbuf + store_size;
#ifdef _DEBUG_B
				recompEnd();
#endif
				resize();
				return;
			}else{
				tbuf[i] = tmp;
			}
		}
		delete[] store;
		store = tbuf;
		store_end = tbuf + store_size;
		recompEnd();
#ifdef _DEBUG_B
		int ii=0;
	for(iterator it = begin(); it != end(); ++it, ++ii){
		Assert(vp.count(*it)>0, "Not working");
	}
	Assert(ii == _size, "Something fishy");
	Assert(vp.size() == _size, "Not working");
#endif
	}

	int count(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
#ifdef _DEBUG_B
	Assert(store+lp5 < store_end, "What??");
#endif
		bool t0 = store[l].first == val;
		bool t1 = store[lp1].first == val;
		bool t2 = store[lp2].first == val;
		bool t3 = store[lp3].first == val;
		bool t4 = store[lp4].first == val;
		bool t5 = store[lp5].first == val;
		return (t0 || t1 || t2 || t3 || t4 || t5) ? 1 : 0;
	}

	void erase(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
#ifdef _DEBUG_B
	Assert(store+lp5 < store_end, "What??");
#endif
		bool t0 = store[l]->first == val;
		bool t1 = store[lp1]->first == val;
		bool t2 = store[lp2]->first == val;
		bool t3 = store[lp3]->first == val;
		bool t4 = store[lp4]->first == val;
		bool t5 = store[lp5]->first == val;

		if(t0){
			store[loc]->first = NULL; --_size; return;
		}
		if(t1){
			store[lp1]->first = NULL; --_size; return;
		}
		if(t2){
			store[lp2]->first = NULL; --_size; return;
		}
		if(t3){
			store[lp3]->first = NULL; --_size; return;
		}
		if(t4){
			store[lp4]->first = NULL; --_size; return;
		}
		if(t5){
			store[lp5]->first = NULL; --_size; return;
		}
#ifdef _DEBUG_B
	set<pair<T*, M> > vp;
	for(iterator it = begin(); it != end(); ++it){
		vp.insert(*it);
	}
	Assert(vp.count(val)==0, "It didn't get removed!!");
#endif
	}

	void erase(iterator& it){
#ifdef _DEBUG_B
	Assert(it.cur <= store_end, "What??");
#endif
		if(it.cur != store_end){
			--_size;
			it.cur->first = NULL;
		}
	}


	M* findBis(T* val){
		unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		bool t0 = store[l].first == val;
		bool t1 = store[lp1].first == val;
		bool t2 = store[lp2].first == val;
		bool t3 = store[lp3].first == val;
		bool t4 = store[lp4].first == val;
		bool t5 = store[lp5].first == val;
#ifdef _DEBUG_B
	Assert(store+lp5 < store_end, "What??");
#endif
		if(t0){
			return &store[l].second;
		}
		if(t1){
			return &store[lp1].second;
		}
		if(t2){
			return &store[lp2].second;
		}
		if(t3){
			return &store[lp3].second;
		}
		if(t4){
			return &store[lp4].second;
		}
		if(t5){
			return &store[lp5].second;
		}
		return NULL;
	}

	iterator find(T* val){
		//unsigned loc = hash(val);
            unsigned loc = traits::hash(val, sz);
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		bool t0 = store[l].first == val;
		bool t1 = store[lp1].first == val;
		bool t2 = store[lp2].first == val;
		bool t3 = store[lp3].first == val;
		bool t4 = store[lp4].first == val;
		bool t5 = store[lp5].first == val;
#ifdef _DEBUG_B
	Assert(store+lp5 < store_end, "What??");
#endif
		if(t0){
			return fmiter<T, M>(store + loc, store_end, vers);
		}
		if(t1){
			return fmiter<T, M>( store + lp1, store_end, vers);
		}
		if(t2){
			return fmiter<T, M>(store + lp2, store_end, vers);
		}
		if(t3){
			return fmiter<T, M>(store + lp3, store_end, vers);
		}
		if(t4){
			return fmiter<T, M>(store + lp4, store_end, vers);
		}
		if(t5){
			return fmiter<T, M>(store + lp5, store_end, vers);
		}
		return fmiter<T, M>(store_end, store_end, vers);
	}



	template<typename IT>
	void insert(IT beg, IT end){
		for(IT it = beg; it != end; ++it){
			insert(*it);
		}
	}


	void insertIntersection(const FastMap& fm1, const FastMap& fm2){
		pair<T*, M>* beg = fm1.store;
		pair<T*, M>* end = fm1.store_end;

		while(beg < end){
			if(beg->first == NULL){ ++beg; continue; }			
			M* it = ((FastMap&)fm2).findBis(beg->first);
			if(it != NULL && *it == beg->second){
				insert(*beg);
			}
			++beg;
		}
	}


	void insertIntersectionBis(const FastMap& fm1, const FastMap& fm2){
		for(iterator it = fm1.begin(); it!= fm1.end(); ++it){
			iterator it2 = ((FastMap&)fm2).find(it->first);
			if(it2 != fm2.end() && it2->second == it->second){
				insert(*it);
			}			
		}
	}

	void clear(){
		sz = 1;
		delete[] store;
		unsigned tmp = (CNST<<sz)+2;		
		store = new pair<T*, M>[tmp];		
		store_end = store + tmp;
		memset(store, 0, tmp*sizeof(pair<T*, M>));
		_size = 0;
		++vers;
		recompEnd();
	}



	void insert(const pair<T*, M>& val){
#ifdef _DEBUG_B
	set<pair<T*, M> > vof;
	for(iterator it = begin(); it != end(); ++it){
		vof.insert(*it);
	}
	Assert(vof.size()==_size, "It didn't get inserted!!");
#endif
		//unsigned loc = hash(val);
       unsigned loc = traits::hash(val.first, sz);
	   
		int l = loc, lp1 = loc+1, lp2 = loc+2, lp3 = loc+3, lp4 = loc+4, lp5 = loc+5;
		pair<T*, M>& sl = store[l];
		pair<T*, M>& slp1 = store[lp1];
		pair<T*, M>& slp2 = store[lp2];
		pair<T*, M>& slp3 = store[lp3];
		pair<T*, M>& slp4 = store[lp4];
		pair<T*, M>& slp5 = store[lp5];
#ifdef _DEBUG_B
	Assert(store+lp5 < store_end, "What??");
#endif	   		
		{
			T* vf = val.first;
			T* slf = sl.first, *slp2f = slp2.first, *slp3f = slp3.first, *slp1f = slp1.first, *slp4f = slp4.first, *slp5f = slp5.first;
			if(slp2f == vf){
				slp2.second = val.second; return;
			}
			if(slp3f == vf){
				slp3.second = val.second; return;
			}	
			if(slf == vf){
				sl.second = val.second; return;
			}
			if(slp1f == vf){
				slp1.second = val.second; return;
			}					
			if(slp4f == vf){
				slp4.second = val.second; return;
			}
			if(slp5f == vf){
				slp5.second = val.second; return;
			}	
			
			if(slp2f == NULL){
				++_size;
				slp2 = val; return;
			}
			if(slp3f == NULL){
				++_size;
				slp3 = val; return;
			}											
			if(slf == NULL){
				++_size;
				sl = val; return;
			}
			if(slp1f == NULL){
				++_size;
				slp1 = val; return;
			}
			if(slp4f == NULL){
				++_size;
				slp4 = val; return;
			}
			if(slp5f == NULL){
				++_size;
				slp5 = val; return;
			}
			resize();
			insert(val);
		}
		#ifdef _DEBUG_B
			set<pair<T*, M> > vp;
			for(iterator it = begin(); it != end(); ++it){
				vp.insert(*it);
			}
			Assert(vp.count(val)>0, "It didn't get inserted!!");
			Assert(vp.size()==_size, "It didn't get inserted!!");
		#endif
	}

	void insert(T* f, M s){
		insert(make_pair(f, s));
	}
	void swap(FastMap<T, M>& t2){
		std::swap(store, t2.store);
		std::swap(store_end, t2.store_end);
		std::swap(sz, t2.sz);
		std::swap(_size, t2._size);
		recompEnd();
		t2.recompEnd();
		++vers;
		++t2.vers;
	}
public:

	~FastMap(void)
	{
		delete[] store;
	}
};

template<typename T, typename M>
void swap(FastMap<T, M>& t1, FastMap<T, M>& t2){
	t1.swap(t2);
}
