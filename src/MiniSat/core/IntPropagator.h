
#ifndef INTPROP_H
#define INTPROP_H

#include "Tvalue.h"
#include "SolverTypes.h"
#include "Vec.h"
#include <cstddef>
#include <algorithm>
#include <climits>

namespace MSsolverNS{


/*!
Value of an integer variable; includes a flag to tell if the value has been set or not.
*/
class Val{
	int val;
	bool def;
public:
	Val():def(false){ }
	Val(int v):val(v),def(true){}
	bool isDef(){ return def; }
	int v(){ return val; }
	void set(int v){ val=v; def=true; }
	void clear(){ def=false; }
};


class mpair{
public:
	iVar var;
	int level;
	int val;	
	mpair(int v, int lv, int value):var(v),level(lv), val(value){}
	mpair(){}
};

class interpair{
public:
	Lit l;
	int tlevel;
	interpair(int trail_level, Lit lit):tlevel(trail_level),l(lit){}
};

typedef enum { PLUS, MINUS, TIMES, EXACTDIV, LT, EQ, BMUX, MOD, DIV , CONF, ARRC, ARRR, ARRW } ctype;




class Intclause{
protected:	
	ctype type;
	uint32_t size_etc;
	float act;
	iVar data[0];
public:
	Intclause(ctype tp, int sz, iVar a1, iVar a2, iVar a3):type(tp), size_etc(sz<<3), act(0){ data[0]=a1; data[1]=a2; data[2]=a3; }	
	Intclause(ctype tp, int sz, iVar a1, iVar a2, iVar a3, iVar a4) :type(tp), size_etc(sz << 3), act(0) { 
		data[0] = a1; data[1] = a2; data[2] = a3; data[3] = a4; 
	}
	Intclause(ctype tp, int sz,  iVar a1, iVar a2, iVar* ch):type(tp), size_etc(sz<<3), act(0){
		data[0]=a1; data[1]=a2; 
		for(int i=2; i<sz; ++i){ data[i] = ch[i-2]; }		
	}

	Intclause(ctype tp, int sz, iVar a1, iVar* ch) :type(tp), size_etc(sz << 3), act(0) {
		data[0] = a1; 
		for (int i = 1; i<sz; ++i) { data[i] = ch[i - 1]; }
	}

	Intclause(ctype tp, int sz, iVar* ch):type(tp), size_etc(sz<<3), act(0){		
		for(int i=0; i<sz; ++i){ data[i] = ch[i]; }		
	}
	ctype tp(){ return type; }
	void retype(ctype tp){ type = tp; }
	iVar& operator[](int idx){
		return data[idx];
	}
	int size(){
		return size_etc >> 3;
	}
	double activity(){ return act; }
	void decay(float f){ act = act * f; }
	//Increment activity measure for the clause.
	void moreAct(){ act = act + 1.0; }
	void print();
};

class WrappedIC{
	public:
	iVar watched;
	Intclause cc;
	WrappedIC(ctype tp, int sz,  iVar a1, iVar a2, iVar* ch):cc(tp, sz, a1, a2, ch){}
};


//Represents x=a+b;
inline Intclause* PlusClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(PLUS, 3, x, a, b);
}

//Represents x=a-b;
inline Intclause* MinusClause(iVar a, iVar b, iVar x ){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(MINUS, 3, x, a, b);
}

//Represents x=a*b;
inline Intclause* TimesClause(iVar a, iVar b, iVar x ){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(TIMES, 3, x, a, b);
}

//Represents x=a%b;
inline Intclause* ModClause(iVar a, iVar b, iVar x ){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(MOD, 3, x, a, b);
}


//Represents x=a%b;
inline Intclause* DivClause(iVar a, iVar b, iVar x ){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(DIV, 3, x, a, b);
}


//Represents x=a<b;
inline Intclause* LtClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(LT, 3, x, a, b);
}

//Represents x=a==b;
inline Intclause* EqClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(EQ, 3, x, a, b);
}

inline Intclause* BMuxClause(iVar cond, int len, iVar* choices, iVar x){
	void* mem = malloc(sizeof(WrappedIC)+sizeof(uint32_t)*(len+2));
	WrappedIC* tmp = new (mem)WrappedIC(BMUX, len+2, x, cond, choices); 
	return &(tmp->cc);
} 


inline Intclause* ArrCreate(int n, iVar* vals, iVar x) {
	void* mem = malloc(sizeof(Intclause) + sizeof(uint32_t)*(n + 1));
	return new (mem)Intclause(ARRC,n+1, x, vals);
}

inline Intclause* ArrRead(iVar x, iVar idx, iVar arr) {
	void* mem = malloc(sizeof(Intclause) + sizeof(uint32_t)*(3));
	return new (mem)Intclause(ARRR,3, x, idx, arr);
}

inline Intclause* ArrWrite(iVar x, iVar idx, iVar arr, iVar val) {
	void* mem = malloc(sizeof(Intclause) + sizeof(uint32_t)*(4));
	return new (mem)Intclause(ARRR, 4, x, idx, arr, val);
}



inline Intclause* ConfClause(int len, iVar* choices){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*(len*2));
	Intclause* tmp = new (mem)Intclause(CONF, len, choices); 
	for(int i=0; i<len; ++i){
		(*tmp)[len+i] = choices[len+i];
	}
	return tmp;
} 

inline int setConfVal(Intclause& c, int j, int val){
	c[c.size()+j] = val;
	return val;
}

inline void confSwap(Intclause& c, int i, int j){
	iVar a = c[i];
	int n = c.size();
	c[i] = c[j];
	c[j] = a;
	a = c[n+i];
	c[n+i] = c[n+j];
	c[n+j] = a;	
}

inline int confVal(Intclause& c, int j){
	iVar val = c[c.size()+j];
	long long int vv = val;
	if(vv > INT_MAX){
		vv = vv - UINT_MAX;
		vv = vv - 1;
	}
	return vv;
}

inline iVar& watchedIdx(Intclause& ic){
	return *(iVar*)((((char*)(&ic))-offsetof(WrappedIC,cc))+offsetof(WrappedIC, watched));
}

inline int bmuxChildren(Intclause& ic){
	return ic.size()-2;
}



class tentry {
public:

	 Range range;
	 int prior;
	 iVar var;
	 Intclause* rson;
	tentry(const Range& _r, int _prior, iVar _v, Intclause* _rson) :range(_r), prior(_prior), var(_v), rson(_rson) {

	}
	tentry(const tentry& other):range(other.range), prior(other.prior), var(other.var), rson(other.rson) {

	}
	tentry& operator=(const tentry& other) {
		range = other.range;
		prior = other.prior;
		var = other.var;
		rson = other.rson;
		return *this;
	}
};

class RangeTracker {
	//Stores all the Ranges in the order in which they are added.
	//maps idx to a range and the prior index for the range of that variable.
	vec<tentry> ranges;
	//Maps every variable to the latest index within ranges.
	vec<int> varmap;
	// Works as a stack. Every time a new level is seen, this stack records the id 
	// level (first) and the idx where Ranges for that level start (second).
	// since levels increase monotonically, they will increase monotonically in this array.
	vec<pair<int, int> > levelmap;
	int highestlevel;
public:
	RangeTracker() {
		highestlevel = 0;
	}
	void addVar() {
		varmap.push(-1);
	}

	typedef tentry*  rangewalker;


	rangewalker rbegin(iVar vr) {
		int idx = varmap[vr];

		tentry* tent = ranges.begin() + idx;
		return tent;
	}

	rangewalker rbegin(iVar vr, int badVal) {
		int idx = varmap[vr];

		tentry* tent = ranges.begin() + idx;
		while (true) {
			int prev = tent->prior;
			Assert(prev >= 0, "Can't happen");
			tentry* ptent = ranges.begin() + prev;
			if (ptent->range.contains(badVal)) {
				return tent;
			}
			tent = ptent;
		}		
		return NULL;
	}

	rangewalker rbegin() {
		return &ranges.last();
	}

	rangewalker rend() {
		return ranges.begin()-1;
	}

	void updateRange(iVar varid, int level, const Range&  newrange, Intclause* rson);
	void popRanges(int level);
	const Range& getRange(iVar varid);
	const Range& getRange(iVar varid, int& maxidx);
	void dump() {
		for (int i = 0; i < varmap.size(); ++i) {
			cout <<i<< getRange(i).print() << endl;
		}
	}

	int maxrindex() { return ranges.size();  }

	int getLevel(iVar varid) {
		int idx = varmap[varid];
		if (idx < 0) { return -1; }
		for (int i = levelmap.size() - 1; i >= 0; --i) {
			if (levelmap[i].second <= idx) {
				return levelmap[i].first;
			}
		}
		return 0;
	}

	Intclause* getCause(iVar varid, int& pos) {
		if (pos < 0) { pos = ranges.size(); }
		int idx = varmap[varid];		
		while(true){
			if (idx < 0) { return NULL; }
			if (idx < pos) {
				pos = idx;
				return ranges[idx].rson;
			}
			idx = ranges[idx].prior;
		}		
	}

};

inline void RangeTracker::updateRange(iVar varid, int level, const Range&  newrange, Intclause* rson) {
	int newid = ranges.size();
	if (level != highestlevel) {
		Assert(level > highestlevel, "WTF");
		highestlevel = level;
		levelmap.push(make_pair(level, newid));
	}
	int oldid = varmap[varid];
	ranges.push(tentry(newrange, oldid, varid, rson));
	varmap[varid] = newid;
}

inline const Range& RangeTracker::getRange(iVar varid) {
	int idx = varmap[varid];
	if (idx < 0) { return TOP_RANGE;  }
	return ranges[idx].range;
}

inline const Range& RangeTracker::getRange(iVar varid, int& maxidx) {
	if (maxidx < 0) { maxidx = ranges.size();  }
	int idx = varmap[varid];
	while (true) {
		if (idx < 0) { return TOP_RANGE; }
		if (idx < maxidx) {
			maxidx = idx;
			return ranges[idx].range;
		}
		idx = ranges[idx].prior;
	}	
}


inline void RangeTracker::popRanges(int level) {	
	if (level <= highestlevel) {		
		int lastidx = ranges.size();
		int nelems = 0;
		for (int i = levelmap.size() - 1; i >= 0; --i) {	
			int lvv = levelmap[i].first;
			highestlevel = lvv;
			if (lvv <= level) {
				break;
			}
			nelems++;
			lastidx = levelmap[i].second;
		}		
		levelmap.shrink(nelems);
		if (levelmap.size() == 0) {
			highestlevel = 0;
		}
		for (int i = ranges.size() - 1; i >= lastidx; --i) {
			tentry& te = ranges[i];
			varmap[te.var] = te.prior;
		}
		ranges.shrink(ranges.size() - lastidx);			
	}
}


class BaseArray {
public:
	virtual Val val(int idx) = 0;
};

class ArrayCr : public BaseArray{
	vector<Val> vals;
public:
	Val val(int idx) {
		return vals[idx];
	}
};

class ArrayUpdate : public BaseArray {
	BaseArray& parent;
	Val idx;
	Val value;
public:
	Val val(int _idx) {
		if (idx.isDef()) {
			if (idx.v() == _idx) {
				return value;
			}
			else {
				parent.val(_idx);
			}
		}
		else {
			return Val();
		}		
	}
};


class ArrayPropagator {
	vector<BaseArray*> avals;



};




class IntPropagator{
	// Map from variable id to values.
	vec<Val> vals;


	//Like the trail in the sat solver; this is a queue that contains all the pending assignments. 
	//each entry is a triple that includes the variable, its value, and the level.
	vec<mpair> trail;

	//tpos is a mapping from variabels to their position in the trail. If the variable is not in the
	//trail, it will return -1.
	vec<int> tpos;

	//For variables that interface between the SAT solver and the IntPropagator, 
	//mappings stores the TValue associated with that variable. This is used to 
	//get the literal that corresponds to a particular integer assignment to this variable.
	vector<Tvalue> mappings;

	// This is a stack where for every interface variable that gets set we 
	// push the position in the trail where that variable was set and the SAT 
	// literal that corresponds to the value of that variable. This is later used 
	// for conflict analysis in order to produce a conflict clause in terms of the SAT variables.
	vec<interpair> interf;

	// A temporary buffer used to construct conflict clauses.
	vec<Lit> explain;

	// Another temporary buffer used to construct conflict clauses. This one tracks whether a given variable.
	// has been seen as part of conflict clause construction.
	vec<char> seen;

	// Mapping from a variable to the clause that forced that variable to be set.
	vec<Intclause*> reason;

	// points to next position in the trail.
	int qhead;

	// Clause database.
	vec<Intclause*> clauses;

	// Conflict clause database.
	vec<Intclause*> conflicts;

	// Like watches in SAT.
	vec<vec<Intclause*> >  watches;

	// mapping to var to whether that var is a boolean or not.
	vec<char> isBool;

	RangeTracker ranges;
public:
	IntPropagator(){
		qhead = 0;
	}
	~IntPropagator(){
		for(int i=0; i<clauses.size(); ++i){
			if(clauses[i]->tp()!= BMUX){
				free(clauses[i]);
			}else{
				Intclause* tmp = clauses[i];
				free( ((char*)(tmp))-offsetof(WrappedIC,cc) );
			}
		}
	}

	const Val& getVal(int id){
		return vals[id];
	}
	int addVar();

	//Number of interface variables that have been set up to this point.
	int interflen(){
		return interf.size();
	}


	Lit interfLit(int i){
		return interf[i].l;
	}

	int addVar(Tvalue& tv);

	void addMapping(int id, Tvalue& tv);

	bool propagateToInterf(iVar vr, int val) {
		Lit tmplit;
		if (!checkLegal(vr, val, tmplit)) {
			return false;
		}
		else {
			if (var(tmplit) != var_Undef) {
				interf.push(interpair(trail.size(), tmplit));
			}
		}
		return true;
	}

	bool uncheckedSetVal(iVar vr, int val, int level, Intclause* rson){

		if (!propagateToInterf(vr, val)) {
			return false;
		}
		
		Val& v(vals[vr]);
		v.set(val);
		reason[vr] = rson;
		tpos[vr] = trail.size();
		trail.push(mpair(vr, level, val));	
		ranges.updateRange(vr, level, Range(val, val), rson);
		return true;
	}



	/*
	This function looks at the dependencies for vr at the time when the trail was at maxpos and the
	ranges were at maxrpos. 
	*/
	void superhelper(iVar vr, int maxrpos) {
		int newmaxrpos = maxrpos;
		Intclause* ic = ranges.getCause(vr, newmaxrpos);
		if (ic == NULL) {
			//either this was fixed externally or we got to top.
			++newmaxrpos;
			Range r = ranges.getRange(vr, newmaxrpos);
			if (r.isSingle()) {
				Lit tmp;
				if (checkLegal(vr, r.getLo(), tmp)) {
					if (var(tmp) != var_Undef) {
						// cout<<", "<<vr<<"("<<r.getLo()<<")";
						explain.push(~tmp);
					}
				}
			}
		}else{
			//There is a reason. 
			for (int i = 0; i<ic->size(); ++i) {
				iVar iv = (*ic)[i];
				int newlev = newmaxrpos;
				ranges.getRange(iv, newlev);
				if (seen[newlev] ==0) {
					seen[newlev] = 1;
					superhelper(iv, newmaxrpos);
				}
			}
		}
	}


	vec<Lit>& getSummary(iVar vr, Intclause* ic, int badVal, bool useBadVal) {
		explain.clear();
		seen.growTo(nvars());
		for (int i = 0; i< seen.size(); ++i) {
			seen[i] = 0;
		}
		auto it = ranges.rbegin();
		int scount = 0;
		if (ic == NULL) {			
			seen[vr] = 1;			
			scount++;
			if (useBadVal) {
				it = ranges.rbegin(vr, badVal);
			}
			else {
				it = ranges.rbegin(vr);
			}
		} else {
			ic->moreAct();
			for (int i = 0; i<ic->size(); ++i) {				
				iVar iv = (*ic)[i];
				if (seen[iv]==0) {
					seen[iv] = 1;
					++scount;
				}
			}
		}

		
		for (; it != ranges.rend(); --it) {
			iVar iv = it->var;
			if (seen[iv] == 1) {
				seen[iv] = 0;
				--scount;
				Intclause* ic = it->rson;
				
				if (ic == NULL) {
					Range& r = it->range;
					if (r.isSingle()) {
						Lit tmp;
						if (checkLegal(iv, r.getLo(), tmp)) {
							if (var(tmp) != var_Undef) {
								
								explain.push(~tmp);
							}
						}
					}
				} else {
					ic->moreAct();
					int tp = tpos[(*ic)[1]];
					if (ic->tp() == BMUX
						&& iv == (*ic)[0]
						&& tp > 0
						&& tp < tpos[iv]
						&& it->range.isSingle()
						) {

						seen[iv] = 1;
						++scount;
						
						iVar ivv = (*ic)[1];
						if (seen[ivv] == 0) {
							seen[ivv] = 1;
							++scount;
						}
						//cout << " tpos[0]=" << tpos[(*ic)[0]] << 
						//	" tpos[1]=" << tpos[(*ic)[1]] << " tpos[2]=" << tpos[(*ic)[2]]
						//	<< " tpos[3]="<< tpos[(*ic)[3]];
						//cout << "  ivv = " << ivv<<"  ";
						int val = trail[tp].val;
						//cout << "  val = " << val << "  ";
						if (val >= 0 && 2 + val < ic->size()) {
							ivv = (*ic)[2 + val];
							//cout << "  ivv = " << ivv;
							if (seen[ivv] == 0) {
								seen[ivv] = 1;
								++scount;
							}
						}
						//cout << endl;
					}
					else{
						for (int i = 0; i<ic->size(); ++i) {
							iVar iv = (*ic)[i];
							if (seen[iv] == 0) {
								seen[iv] = 1;
								++scount;
							}
						}
					}					
				}
			}
			
			if (scount == 0) {
				break;
			}
		}
		//cout << endl;
		return explain;
	}


	/**
	if(ic==null){
	look at the root causes that led to vr to be set to its current value.
	the return vector contains a list of literals such that if any of those had been true, vr would not have its current value.
	}else{
	it is assumed that ic is a conflict that was returned by propagate.
	we will return a list of literals such that if any of those literals
	had ben true, that clause would not have failed in the same way.
	}
	*/
	vec<Lit>& getSummaryA(iVar vr, Intclause* ic, int lev = -1) {
		explain.clear();
		seen.growTo(ranges.maxrindex());
		for (int i = 0; i< seen.size(); ++i) {
			seen[i] = 0;
		}		
		if (ic == NULL) {
			if (lev == -1) {
				ranges.getRange(vr, lev);
				++lev;
				Assert(lev > 0, "WTF!! 33");
			}
			seen[lev] = 1;
			superhelper(vr, lev);			
		}
		else {
			ic->moreAct();
			for (int i = 0; i<ic->size(); ++i) {
				int newlev=lev;				
				iVar iv = (*ic)[i];
				ranges.getRange(iv, newlev);
				if (seen[newlev] ==0 ) {
					seen[newlev] = 1;
					superhelper(iv, lev);
				}
			}
		}
		//cout<<endl;
		return explain;
	}


	/*
	Walks up the dependency chain to find the root causes for the assignment to vr. 
	Any assignment after maxtpos canot be a reason and should be ignored.
	Modifies seen and explain.
	*/
	void helper(iVar vr, int maxtpos){
		/*
		Note about helper:
		The reasons returned by getSummary should only include variables that come before me in the
		trial. However, there are situations when a clause forced an assignment without being fully
		assigned (i.e. a mux can force an assignment even if some of its choices are not set), but then
		some of those unassigned entries got assigned. so that when I call getSummary, they get returned
		as part of the reason even though they came afterward!!!.
		This is fixed by checking that vrlev  <maxtpos.

		*/
		Intclause* ic = reason[vr];
		if(ic==NULL){
			//either interface or unset.
			Lit tmp;
			Val& vv = vals[vr];
			int vrlev = tpos[vr];
			if(vv.isDef() &&  vrlev >= 0 && (maxtpos<0 || vrlev < maxtpos) && checkLegal(vr, vv.v(), tmp)){
				if(var(tmp)!=var_Undef){
					//cout<<", "<<vr<<"("<<vv.v()<<")";
					explain.push(~tmp);
				}
			} else {
				Assert(false, "NNN");
				int l=0;
				ic = ranges.getCause(vr, l);
				if (ic != NULL) {
					for (int i = 0; i<ic->size(); ++i) {
						iVar iv = (*ic)[i];
						int parpos = tpos[iv];
						if (seen[iv] == 0) {
							seen[iv] = 1;
							helper(iv, maxtpos);
						}
					}
				}
			}
		}		else {
			int vrlev = tpos[vr];
			ic->moreAct();
			if(ic->tp()==BMUX){
				if(vr==(*ic)[0]){
					iVar cond = (*ic)[1];
					int condpos = tpos[cond];
					Val& vc = vals[cond];
					if(vc.isDef() && condpos < vrlev){
						int vcv = vc.v();
						if(seen[cond]==0){
							seen[cond]=1;
							helper(cond, maxtpos);
						}
						if(vcv >=0 && vcv+2 <= ic->size()){
							iVar ivv = (*ic)[vcv+2];
							if(seen[ivv]==0){
								seen[ivv] = 1;
								helper(ivv, maxtpos);
							}
						}
						return;
					}
				}
			}
			

			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				int parpos = tpos[iv];
				if( parpos < vrlev && seen[iv]==0){
					seen[iv] = 1;
					helper(iv, maxtpos);
				}
			}
		}
	}

	/*
	look at the root causes that led to lit to be set to its current value. 
    the return vector contains a list of literals such that if any of those had been true, lit would not have its current value.
	in general, this will not contain lit unless lit was set from scratch.
	*/
	vec<Lit>& getSummary(Lit& l){
		
		for(int i=interf.size()-1; i>=0; i--){
			interpair& ip = interf[i];
			if(ip.l == l){	
				int lev = ip.tlevel;
				int iv = trail[lev].var;							
				return getSummary(iv, NULL, 0, false);
			}
		}
		Assert(false, "WTF");
	}

	

	void innerhelper(Intclause* ic, iVar vr, vec<pair<iVar, int> >& ppp, int trailLev){		
		if(ic==NULL){			
			ppp.push(make_pair(vr, vals[vr].v()));
		}else{
			ic->moreAct();
			if(ic->tp()==BMUX && seen[(*ic)[0]]){
				int xpos = tpos[(*ic)[0]];
				//int xlev = trail[xpos].level;
				iVar cond = (*ic)[1];
				int condpos = tpos[cond];
				if(condpos < xpos){										
					Val& vc = vals[cond];
					if(vc.isDef()){
						int vcv = vc.v();
						if(seen[cond]==0){
							seen[cond]=1;
							int parpos = tpos[cond];
							int tlev = trail[parpos].level;
							if(tlev==trailLev){
								Intclause* nc = reason[cond];
								innerhelper(nc, cond, ppp, trailLev);
							}else{
								if(tlev > 0){
									ppp.push(make_pair(cond, vals[cond].v()));
								}
							}
						}
						if(vcv >=0 && vcv+2 <= ic->size()){
							iVar ivv = (*ic)[vcv+2];
							if(seen[ivv]==0){
								seen[ivv] = 1;
								int parpos = tpos[ivv];
								int tlev = trail[parpos].level;
								if(tlev==trailLev){
									Intclause* nc = reason[ivv];
									innerhelper(nc, ivv, ppp, trailLev);
								}else{
									if(tlev > 0){
										ppp.push(make_pair(ivv, vals[ivv].v()));
									}
								}
							}
						}
						return;
					}
				}
			}

			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				if(seen[iv]==0){
					seen[iv]=1;
					int parpos = tpos[iv];
					if(parpos > 0){
						int tlev = trail[parpos].level;
						if(tlev==trailLev){
							Intclause* nc = reason[iv];
							innerhelper(nc, iv, ppp, trailLev);
						}else{
							if(tlev > 0){
								ppp.push(make_pair(iv, vals[iv].v()));
							}
						}
					} else {
						Intclause* ic= ranges.getCause(iv, trailLev);
						if (ic != NULL) {
							innerhelper(ic, iv, ppp, trailLev);
						}
					}

				}
			}
		}
	}

	void generateInnerConflict(Intclause* ic, int trailLev, int szbnd){	
		if (szbnd <= 1) { return;  }
		vec<pair<iVar, int> > ppp;
		for(int i=0; i< seen.size(); ++i){
			seen[i] = 0;
		}
		innerhelper(ic, 0, ppp, trailLev);

		vec<iVar> vi;
		vi.growTo(ppp.size()*2);
		int sz = ppp.size();
		for(int i=0; i<ppp.size(); ++i){
			vi[i] = ppp[i].first;
			vi[sz+i] = ppp[i].second;
		}
		if(ppp.size() > 1 && ppp.size()< szbnd){
			Intclause* conf = ConfClause(ppp.size(), &vi[0]);			
			addConf(conf);
		}
	}



	bool isSet(iVar var){
		Val& v(vals[var]);
		return v.isDef();
	}

	bool setVal(iVar var, int val, int level){
		Val& v(vals[var]);
		if(v.isDef()){
			return v.v()==val;
		}else{
			Range r = ranges.getRange(var);
			if (!r.contains(val)) {
				return false;
			}
			v.set(val);
			tpos[var] = trail.size();
			trail.push(mpair(var, level, val));		
			ranges.updateRange(var, level, Range(val, val), NULL);
			return true;
		}
	}
	void addMinus(iVar a, iVar b, iVar x){
		Intclause* c = PlusClause(a, b, x);
		Range out = ranges.getRange(a) - ranges.getRange(b);
		if (out.isSingle()) {
			uncheckedSetVal(x, out.getLo(), 0, NULL);
			auto worked = propagate();
			Assert(worked == NULL, "Not worked!!");
			return;
		}
		else {
			ranges.updateRange(x, 0, out, NULL);
		}		
		c->retype(MINUS);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
		watches[x].push(c);
	}


	void addMod(iVar a, iVar b, iVar x){
		Range out = ranges.getRange(a) % ranges.getRange(b);
		if (out.isSingle()) {
			uncheckedSetVal(x, out.getLo(), 0, NULL);
			auto worked = propagate();
			Assert(worked == NULL, "Not worked!!");
			return;
		}
		else {
			ranges.updateRange(x, 0, out, NULL);
		}
		Intclause* c = ModClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}

	void addDiv(iVar a, iVar b, iVar x){
		

		Range out = ranges.getRange(a) / ranges.getRange(b);
		if (out.isSingle()) {
			uncheckedSetVal(x, out.getLo(), 0, NULL);
			auto worked = propagate();
			Assert(worked == NULL, "Not worked!!");
			return;
		}
		else {
			ranges.updateRange(x, 0, out, NULL);
		}
		Intclause* c = DivClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}

	void addPlus(iVar a, iVar b, iVar x){
		Intclause* c = PlusClause(a, b, x);
		clauses.push(c);
		Assert(! (isSet(a) && isSet(b)), "This should have been constant propagated");
		Range out = ranges.getRange(a) + ranges.getRange(b);
		if (out.isSingle()) {
			uncheckedSetVal(x, out.getLo(), 0, NULL);
			auto worked = propagate();
			Assert(worked == NULL, "Not worked!!");
			return;
		}
		else {
			ranges.updateRange(x, 0, out, NULL);
		}
				
		if(isSet(a)){
			//we can't watch a and b because they a is already set. must watch b,c.
			//for that, we need to retype to EXACTDIV.
			c->retype(MINUS); // a = x-b;
			(*c)[0]=a;
			(*c)[1]=x;
			(*c)[2]=b;
			watches[x].push(c);
			watches[b].push(c);
			watches[a].push(c);
			return;
		}
		if(isSet(b)){
			c->retype(MINUS); // b = x-a;
			(*c)[0]=b;
			(*c)[1]=x;
			(*c)[2]=a;
			watches[x].push(c);
			watches[a].push(c);
			watches[b].push(c);
			return;
		}
		watches[a].push(c);
		watches[b].push(c);
		watches[x].push(c);
	}
	void addTimes(iVar a, iVar b, iVar x){
		Intclause* c = TimesClause(a, b, x);
		clauses.push(c);
		Assert(! (isSet(a) && isSet(b)), "This should have been constant propagated");
		if (a == b) {
			ranges.updateRange(x, 0, square(ranges.getRange(a)), c);
		}else{
			ranges.updateRange(x, 0, ranges.getRange(a) * ranges.getRange(b), c);
		}		
		if(isSet(a)){
			//we can't watch a and b because they a is already set. must watch b,c.
			//for that, we need to retype to EXACTDIV.
			c->retype(EXACTDIV); // a = x/b;
			(*c)[0]=a;
			(*c)[1]=x;
			(*c)[2]=b;
			watches[x].push(c);
			watches[b].push(c);
			return;
		}
		if(isSet(b)){
			c->retype(EXACTDIV); // b = x/a;
			(*c)[0]=b;
			(*c)[1]=x;
			(*c)[2]=a;
			watches[x].push(c);
			watches[a].push(c);
			return;
		}
		watches[a].push(c);
		watches[b].push(c);
	}
	void addLt(iVar a, iVar b, iVar x){
		Intclause* c = LtClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
		watches[x].push(c);
	}


	void addConf(Intclause* conf){	
		conf->moreAct();
		conflicts.push(conf);
		int n = conf->size();
		int highLev = -1;
		int highLev2 = -1;
		int pos = -1;
		int pos2 = -1;
		Assert(n >= 2, "qoeioo");
		for(int i=0; i<n; ++i){
			int v = (*conf)[i];
			int tp = tpos[v];
			if(tp > highLev){
				highLev2 = highLev;
				pos2 = pos;
				highLev = tp;
				pos = i;
			}else{
				if(tp > highLev2){
					highLev2 = tp;
					pos2 = i;
				}
			}
		}
		Assert(pos >=0 && pos2 >=0 && pos2 != pos, "Wqek;ji");
		confSwap(*conf, 0, pos);
		if(pos2==0){ pos2 = pos; } // because what used to be at zero is now at pos.
		confSwap(*conf, 1, pos2);		
		watches[(*conf)[0]].push(conf);
		watches[(*conf)[1]].push(conf);		
	}
	
	void addEq(iVar a, iVar b, iVar x){
		//Eq is watched by all three variables.
		Intclause* c = EqClause(a, b, x);
		clauses.push(c);
		watches[x].push(c);
		watches[a].push(c);
		watches[b].push(c);
	}

	const Range& getRange(iVar id) {
		return ranges.getRange(id);
	}

	void addBMux(iVar cond, int len, iVar* choices, iVar x){
		Assert(len > 1, "Can't have len <=1");

		Intclause* c = BMuxClause(cond, len, choices, x);
		clauses.push(c);
		

		Assert(!isSet(cond), "This would have been const propagated!");

		Range out = joinChoices(ranges.getRange(cond), len, choices);
		if (out.isSingle()) {
			uncheckedSetVal(x, out.getLo(), 0, NULL);
			auto worked = propagate();
			Assert(worked == NULL, "Not worked!!");
			return;
		} else {
			ranges.updateRange(x, 0, out, NULL);
		}
		
		watches[cond].push(c);
		watches[x].push(c);
		

		if(isSet(choices[0])){
			for(int i=1; i<len; ++i){
				if(!isSet(choices[i])){
					watches[choices[i]].push(c);
					watchedIdx(*c)=2+i;
					return;
				}
			}
			//All choices are already set. 
			//if cond is also set, then we should just propagate, but we are not set up to propagate, so we just bail out.
			Assert(!isSet(cond), "Hope this doesn't happen");
			watches[choices[0]].push(c);
			watchedIdx(*c)=2;
		}else{
			watches[choices[0]].push(c);
			watchedIdx(*c)=2;
		}
	}


	void removeConf(Intclause& ic){
		Assert(ic.tp()==CONF, "Only conflicts can be removed for now");		
		remove(watches[ic[0]], &ic );
		remove(watches[ic[1]], &ic );		
	}


	void cleanupConfs(){
		Intclause** rd, **wr, **end;
		if (conflicts.size() == 0) return;
		rd = &conflicts[0];
		wr = rd;
		end = rd + conflicts.size();
		while(rd < end){
			double act = (*rd)->activity();
			if(act > 0.01){
				(*rd)->decay( act> 100 ? 0.99 : act > 10 ? 0.9 : 0.6 );
				*wr++ = *rd++;
			}else{
				removeConf(**rd);
				free(*rd);
				++rd;
			}
		}
		//cout<<" Conflicts shrunk by "<<(rd-wr);
		conflicts.shrink(rd-wr);
		//cout<<" became "<<conflicts.size()<<endl;
	}


	bool checkLegal(iVar var, int val, Lit& out);

private:

	typedef enum{CONFL, ADV, REPL} action;

	action plusHelper(iVar x, const mpair& me, iVar other, Intclause* c){
		Val vx = vals[x];
		Val voth = vals[other];
		if(vx.isDef()){ 
			if(voth.isDef()){
				if(me.val + voth.v() != vx.v()){
					return CONFL;									
					// conflict!!!
				}else{
					//clause is already satisfied.
					return ADV;
				}								
			}
			else{					
				//vx is defined, but voth is not. That means we need to set other.
				if(uncheckedSetVal(other, vx.v()-me.val, me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(uncheckedSetVal(x, me.val+voth.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}else{				
				return REPL;
			}
		}
	}
	// x = me * other;
	action timesHelper(iVar x, const mpair& me, iVar other, Intclause* c){
		Val vx = vals[x];
		Val voth = vals[other];
		if(vx.isDef()){ 
			if(voth.isDef()){
				if(me.val * voth.v() != vx.v()){
					return CONFL;									
					// conflict!!!
				}else{
					//clause is already satisfied.
					return ADV;
				}								
			}
			else{					
				//vx is defined, but voth is not. That means we need to set other.
				//but we first need to check if the values of vx and me are compatible. 
				//Recall we are dealing with ints, so if vx is not a multiple of me, we cannot set voth.
				if(me.val == 0){
					if(vx.v()==0){
						return ADV;
					}else{
						return CONFL;
					}
				}
				if(vx.v() % me.val != 0){
					return CONFL;
				}
				if(uncheckedSetVal(other, vx.v()/me.val, me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(uncheckedSetVal(x, me.val*voth.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}else{				
				return REPL;
			}
		}
	}

	

	// represents x = me / other  although more accurately, it's x*other = me so other must divide me.
	action exactdivHelper(iVar x, const mpair& me, iVar other, Intclause* c){
		Val vx = vals[x];
		Val voth = vals[other];
		if(vx.isDef()){ 
			if(voth.isDef()){
				if(me.val != vx.v()*voth.v()){
					return CONFL;									
					// conflict!!!
				}else{
					//clause is already satisfied.
					return ADV;
				}								
			}
			else{			
				//vx is defined, but voth is not. That means we need to set other.
				//we also need to check that me is divisible by x.
				if(vx.v()==0){
					if(me.val==0){
						return ADV;
					}else{
						return CONFL;
					}
				}
				if(me.val % vx.v() != 0){ 
					return CONFL;
				}
				if(uncheckedSetVal(other, me.val/vx.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
				
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(voth.v()==0){
					if(me.val==0){
						return ADV;
					}else{
						return CONFL;
					}
				}

				if(me.val % voth.v() != 0){ 
					return CONFL;
				}
				if(uncheckedSetVal(x, me.val/voth.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}				
			}else{				
				return REPL;
			}
		}
	}

	// represents x = other / me  although more accurately, it's x*me = other so other must divide me.
	action exactdivHelper2(iVar x, const mpair& me, iVar other, Intclause* c){
		Val vx = vals[x];
		Val voth = vals[other];
		if(vx.isDef()){ 
			if(voth.isDef()){
				if(me.val*vx.v() != voth.v()){
					return CONFL;									
					// conflict!!!
				}else{
					//clause is already satisfied.
					return ADV;
				}								
			}
			else{
				//vx is defined, but voth is not. That means we need to set other.				
				if(uncheckedSetVal(other, me.val*vx.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
				
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(me.val==0){
					if(voth.v()==0){
						return ADV;
					}else{
						return CONFL;
					}
				}

				if(voth.v() % me.val != 0){ 
					return CONFL;
				}
				if(uncheckedSetVal(x, voth.v() / me.val, me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}				
			}else{				
				return REPL;
			}
		}
	}



	action minusHelper(iVar x, const mpair& me, iVar other, Intclause* c){
		Val vx = vals[x];
		Val voth = vals[other];
		if(vx.isDef()){ 
			if(voth.isDef()){
				if(me.val - voth.v() != vx.v()){
					return CONFL;									
					// conflict!!!
				}else{
					//clause is already satisfied.
					return ADV;
				}								
			}
			else{			
				//vx is defined, but voth is not. That means we need to set other.
				if(uncheckedSetVal(other, me.val-vx.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
				
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(uncheckedSetVal(x, me.val-voth.v(), me.level, c)){
					return ADV;
				}else{
					return CONFL;
				}				
			}else{				
				return REPL;
			}
		}
	}

	action eqHelper(iVar x, iVar a, iVar b, Val vx, Val va, Val vb, int level, Intclause* c){
		if(vx.isDef() && va.isDef() && vb.isDef()){
			bool eqv = (va.v()==vb.v());
			if( (vx.v()==1?eqv:!eqv) ){
				return ADV;
			}else{
				return CONFL;
			}
		}		
		if(vx.isDef() && va.isDef()){
			if(vx.v()==1){
				if(uncheckedSetVal(b, va.v(), level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}
		if(vx.isDef() && vb.isDef()){
			if(vx.v()==1){
				if(uncheckedSetVal(a, vb.v(), level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}

		if(va.isDef() && vb.isDef()){
			if(uncheckedSetVal(x, va.v()==vb.v()?1:0, level, c)){
				return ADV;
			}else{
				return CONFL;
			}
		}
		return ADV;
	}


	action ltHelper(iVar x, iVar a, iVar b, Val vx, Val va, Val vb, int level, Intclause* c){
		if(vx.isDef() && va.isDef() && vb.isDef()){
			bool eqv = (va.v()<vb.v());
			if( (vx.v()==1?eqv:!eqv) ){
				return ADV;
			}else{
				return CONFL;
			}
		}		
		if(va.isDef() && vb.isDef()){
			if(uncheckedSetVal(x, va.v()<vb.v()?1:0, level, c)){
				return ADV;
			}else{
				return CONFL;
			}
		}

		const Range& ra = ranges.getRange(a);
		const Range& rb = ranges.getRange(b);
		if (vx.isDef()) {
			if (vx.v() == 1) {				
				if (rb.getHi() <= ra.getLo()) {
					return CONFL; // We have a conflict, because v.x should be 0 according to ranges.
				}
								
				// Assert(ra.getLo() < rb.getHi(), "Impossible");
				if (!vb.isDef() && rb.getLo() <= ra.getLo()) {
					Range rbnew = Range(ra.getLo()+1, rb.getHi());
					if (rbnew.isSingle()) {
						//Range was collapsed to a single value; set that value.
						uncheckedSetVal(b, rbnew.getLo(), level, c);
					} else {
						//Still has a range of values, but if it narrowed, we update.
						updateIfBetter(b, level, rbnew, *c);
					}
				}
				if (!va.isDef() &&  ra.getHi() >= rb.getHi()) {
					Range ranew = Range(ra.getLo(), rb.getHi() - 1);
					if (ranew.isSingle()) {
						uncheckedSetVal(a, ranew.getLo(), level, c);
					} else {
						updateIfBetter(a, level, ranew, *c);
					}
				}

			} else { // vx.v() == 0 so a >= b
				if (ra.getHi() < rb.getLo()) {
					return CONFL; // We have a conflict, because v.x should be 0 according to ranges.
				}
				// Assert(ra.getHi() >= rb.getLo(), "Impossible");
				if (!va.isDef() && ra.getLo() < rb.getLo()) {
					Range ranew = Range(rb.getLo(), ra.getHi());
					if (ranew.isSingle()) {
						uncheckedSetVal(a, ranew.getLo(), level, c);
					}
					else {
						updateIfBetter(a, level, ranew, *c);
					}
				}
				if (rb.getHi() > ra.getHi()) {
					Range rbnew = Range(rb.getLo(), ra.getHi());
					if (rbnew.isSingle()) {
						//Range was collapsed to a single value; set that value.
						uncheckedSetVal(b, rbnew.getLo(), level, c);
					}
					else {
						//Still has a range of values, but if it narrowed, we update.
						updateIfBetter(b, level, rbnew, *c);
					}
				}

			}
		} else {
			if (ra.getHi() < rb.getLo()) {
				if (uncheckedSetVal(x, 1, level, c)) {
					return ADV;
				} else {
					return CONFL;
				}
			}
			if (rb.getHi() <= ra.getLo()) {
				if (uncheckedSetVal(x, 0, level, c)) {
					return ADV;
				} else {
					return CONFL;
				}
			}
		}

		return ADV;
	}


	bool propagateLt(Intclause& c, const mpair& p, Intclause**& i, Intclause**& j){
		iVar x = c[0];
		iVar a = c[1];
		iVar b = c[2];		
		action act = ltHelper(x, a, b, vals[x], vals[a], vals[b], p.level, &c);
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}

	bool propagateEq(Intclause& c, const mpair& p, Intclause**& i, Intclause**& j){
		iVar x = c[0];
		iVar a = c[1];
		iVar b = c[2];		
		action act = eqHelper(x, a, b, vals[x], vals[a], vals[b], p.level, &c);
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}


	bool propagateMinus(Intclause& c, const mpair& p, Intclause**& i, Intclause**& j){
		if (p.var != c[1] && p.var != c[2]) { *j++ = &c; return true; }
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a-b
			act = minusHelper(c[0], p, c[2], &c);
			if(act==REPL){
				//neither vx nor vb are defined. So we need to stop watching
				//c[1] (a) and start watching c[0] (vx). We can't do this for a PLUS
				//clause because for PLUS we always watch a and b, but we will accomplish
				//the same thing by turning this into a MINUS clause.
				//So we want to say that a = x+b
				c.retype(PLUS);
				iVar a = p.var;
				iVar x = c[0];
				iVar b = c[2];
				c[0] = a;
				c[1] = x;
				// c[2]  is already b.
				//it is already in watches for b, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				*j++ = &c;
				return true;
			}
		}else{ //We just set b in x=a-b
			mpair q(p.var, p.level, -p.val);
			act=plusHelper(c[0], q, c[1], &c);
			if(act==REPL){
				//neither vx nor va are defined. So we need to stop watching
				//c[2] (b) and start watching c[0] (vx). 
				//So we want to say that b = a-x
				c.retype(MINUS);
				iVar b = p.var;
				iVar x = c[0];
				iVar a = c[1];
				c[0] = b;
				// c[1] already equal to a
				c[2] = x;
				//it is already in watches for a, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				*j++ = &c;
				return true;
			}
		}
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}


	bool propagateExactDiv(Intclause& c, const mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a/b
			act = exactdivHelper(c[0], p, c[2], &c);
			if(act==REPL){
				//neither vx nor vb are defined. So we need to stop watching
				//c[1] (a) and start watching c[0] (vx). We can't do this for a EXACTDIV
				//clause because for EXACTDIV we always watch a and b, but we will accomplish
				//the same thing by turning this into a TIMES clause.
				//So we want to say that a = x*b
				c.retype(TIMES);
				iVar a = p.var;
				iVar x = c[0];
				iVar b = c[2];
				c[0] = a;
				c[1] = x;
				// c[2]  is already b.
				//it is already in watches for b, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				watches[x].push(&c);
				return true;
			}
		}else{ //We just set b in x=a/b			
			act=exactdivHelper2(c[0], p, c[1], &c);
			if(act==REPL){
				//neither vx nor va are defined. So we need to stop watching
				//c[2] (b) and start watching c[0] (vx). 
				//So we want to say that b = a/x
				c.retype(EXACTDIV);
				iVar b = p.var;
				iVar x = c[0];
				iVar a = c[1];
				c[0] = b;
				// c[1] already equal to a
				c[2] = x;
				//it is already in watches for a, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				watches[x].push(&c);
				return true;
			}
		}
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}
	action bmuxHelperCond(iVar x, const mpair& cond,  Intclause* c){
		int sz = c->size();
		Val vx = vals[x];
		if(cond.val + 2 >= sz || cond.val < 0){
			if(vx.isDef()){	
				if(0 != vx.v()){
					return CONFL;
				}else{
					return ADV;
				}
			}else{
				if(uncheckedSetVal(x, 0, cond.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}

		iVar in = (*c)[cond.val + 2];
		Val vin = vals[in];		
		if(vin.isDef()){
			if(vx.isDef()){				
				if(vin.v() != vx.v()){
					return CONFL;
				}else{
					return ADV;
				}
			}else{
				if(uncheckedSetVal(x, vin.v(), cond.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}else{
			if(vx.isDef()){
				if(uncheckedSetVal(in, vx.v(), cond.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}else{
				return REPL;
			}
		}
	}

	action bmuxHelperWatched(const mpair& watched, iVar x, iVar cond, int& towatch,  Intclause* c){
		Val vcond = vals[cond];
		Val vx = vals[x];
		if(vcond.isDef()){
			int sz = c->size();			

			if(vcond.v() + 2 >= sz || vcond.v() < 0){
				if(vx.isDef()){
					if(0 != vx.v()){
						return CONFL;
					}else{
						return ADV;
					}
				}else{
					if(uncheckedSetVal(x, 0, watched.level, c)){
						return ADV;
					}else{
						return CONFL;
					}
				}
			}


			iVar in = (*c)[vcond.v() + 2];
			if(in == watched.var){
				if(vx.isDef()){
					if(watched.val != vx.v()){
						return CONFL;
					}else{
						return ADV;
					}
				}else{
					if(uncheckedSetVal(x, watched.val, watched.level, c)){
						return ADV;
					}else{
						return CONFL;
					}
				}
			}else{
				Val vin = vals[in]; // read that input from the clause.
				if(vin.isDef()){
					if(vx.isDef()){
						if(vin.v() != vx.v()){
							return CONFL;
						}else{
							return ADV;
						}
					}else{
						if(uncheckedSetVal(x, vin.v(), watched.level, c)){
							return ADV;
						}else{
							return CONFL;
						}
					}
				}else{
					//vcond has a value, but I am not watching the right thing, and vin is not set.
					//we should just watch the right thing.
					towatch = vcond.v() + 2;
					return REPL;
				}
			}									
		}else{
			//vcond is not set. check to see if all are the same so far.
			int idx = watchedIdx(*c);
			iVar wvar = (*c)[idx];
			Assert(wvar == watched.var, "WWWW???");
			int sz = c->size();
			towatch = -1;
			for(int i=1; i<sz-2; ++i){
				int cidx = 2+((idx-2 + i)%(sz-2));
				iVar nxt = (*c)[cidx];
				Val vnxt = vals[nxt];
				if(vnxt.isDef() && vnxt.v() != watched.val){
					//another choice is also set and has a different value, so they are not all the same.
					//but if sz==4, then it means all choices are now set, so we can propagate to cond if vx is set.
					//but that only works if vx.v() != 0, because if vx is zero, then it could be that the idx is just out of bounds.
					if(sz == 4 && vx.isDef() && vx.v() != 0){
						 if(vnxt.v()==vx.v()){
							if(uncheckedSetVal(cond, cidx-2, watched.level, c)){								
								return ADV;
						 	}else{								
								return CONFL;
							}
						 }
						 if(watched.val == vx.v()){
							if(uncheckedSetVal(cond, idx-2, watched.level, c)){
								return ADV;
						 	}else{
								return CONFL;
							}
						 }
						 return CONFL;
					}
					return ADV;
				}
				if(!vnxt.isDef()){
					towatch = cidx;
				}
			}
			if(towatch == -1){
				//They are all defined and they are all the same.
				auto crange = ranges.getRange(cond);
				if (crange.getLo() < 0 || crange.getHi() > 1) {
					//if there is a posibility that the condition is out of range, there is not much we can do.
					//but if it is guaranteed to be within range, then we know the output must equal watched.val.
					return ADV;
				}
				if(vx.isDef()){
					if(vx.v() != watched.val){
						return CONFL;
					}else{
						return ADV;
					}
				}else{
					if(uncheckedSetVal(x, watched.val, watched.level, c)){
						return ADV;
					}else{
						return CONFL;
					}
				}
			}else{
				if (sz == 4 && vx.isDef() && watched.val != vx.v()) {
					auto crange = ranges.getRange(cond);
					if ((vx.v() != 0 || (crange.getLo() >= 0 && crange.getHi() <= 1))) {
						if (uncheckedSetVal((*c)[towatch], vx.v(), watched.level, c)) {
							if (uncheckedSetVal(cond, towatch - 2, watched.level, c)) {
								return ADV;
							}
							else {
								return CONFL;
							}
						}
						else {
							return CONFL;
						}
					}
				}
				//Not all defined, but all the defined ones are the same.
				//Watch one of the remaining ones.
				return REPL;
			}
		}
	}

	action bmuxHelperX(const mpair& x, iVar cond,  Intclause* c){
		Val vcond = vals[cond];
		if(vcond.isDef()){
			//We know what the condition is.
			int sz = c->size();			

			if(vcond.v()+2 >= sz || vcond.v() < 0){
				if(0 != x.val){
					return CONFL;
				}else{
					return ADV;
				}
			}


			iVar in = (*c)[vcond.v() + 2];
			Val vin = vals[in]; // read that input from the clause.
			if(vin.isDef()){
				if(vin.v() != x.val){
					return CONFL;
				}else{
					return ADV;
				}
			}else{
				// condition is defined and the output is defined, but the input at that condition is not, set it.
				if(uncheckedSetVal(in, x.val, x.level, c)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}else{
			// if vcond is not defined, we can't do anything with the knowledge that x has a given value.
			//we could try to check if all inputs have been checked, then we would have to find out whether 
			//the value of cond matches one of those inputs and if not return a confl, but for now, we will be lazy.
			return ADV;
		}
	}
	     
	bool propagateBMux(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		action act;
		//we just set p. So p must be one of the watched vars.
		//it can either be c[0] (x) c[1] (cond) or c[watchedIdx(c)];
		if(p.var == c[0]){
			
			act = bmuxHelperX(p, c[1]  , &c);
		}else if(p.var == c[1]){
			
			act = bmuxHelperCond(c[0], p  , &c);
			if(act==REPL){
				//cond has been set, but the output has not. We want to make sure we are observing the input
				int idx = watchedIdx(c);
				if(p.val +2 == idx){
					//already watching the right thing.
				}else{
					iVar in = c[p.val + 2];
					iVar old = c[idx];
					Assert(in != p.var && old != p.var, "NONONO");
					watches[in].push(&c);
					watches[old].removeFirst(&c);
					watchedIdx(c) = p.val + 2;
				}
				act=ADV;
			}
		}else{
			
			// in this case p.var must equal c[watchedIdx(c)];
			int newidx;
			act = bmuxHelperWatched(p, c[0], c[1], newidx, &c);
			if(act==REPL){
				watchedIdx(c) = newidx;
				iVar in = c[newidx];
				Assert(in != p.var, "NONONO");
				watches[in].push(&c);
				return true;
			}
		}
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}

	// c[0] = c[1] % c[2];
	// \E x. c[2]*x + c[0] = c[1]
	// propagations: 
	// c[0] >= c[2] ==> reject.
	// c[1] % c[2] ==> c[0]
	// c[2]=0 ==> c[0]=0
	// c[1]=0 ==> c[0]=0
	// c[0] c[1] => c[1]-c[0] if(c[1]-c[0]) is prime and c[0] <  c[1]-c[0] 
	
	bool propagateMod(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		iVar x = c[0];
		iVar a = c[1];
		iVar b = c[2];
		Val vx = vals[c[0]];
		Val va = vals[c[1]];
		Val vb = vals[c[2]];
		if(vx.isDef()){
			if(va.isDef()){
				if(vb.isDef()){
					if((vx.v() == 0 && vb.v()==0) ||
						(vb.v()!=0 && vx.v() == va.v() % vb.v())){
							act = ADV;
					}else{
						act = CONFL;
					}
				}else{
					int t = va.v() - vx.v();
					if(va.v()==0 && vx.v() != 0){
						act=CONFL;
					}else{
						if(t==1 || t==2 || t==3 || t==5 || t==7 || t==11 || t==13 || t== 17 || t== 19 || t== 23){
							if(vx.v() < t && uncheckedSetVal(b, t, p.level, &c)){
								act= ADV;
							}else{
								act = CONFL;
							}
						}else{
							act = ADV;
						}
					}
				}
			}else{// vx and !va def.
				if(vb.isDef()){
					if(vx.v() >= vb.v()){
						act = CONFL;
					}else{
						if((vb.v()==1 || vb.v()==0) && vx.v()!= 0){
							act = CONFL;
						}else{
							act = ADV;
						}
					}
				}else{
					act = ADV;
				}
			}
		}else{ // !vx.
			if(va.isDef()){
				if(vb.isDef()){
					if(uncheckedSetVal(x, vb.v()==0? 0 : (va.v() % vb.v()), p.level, &c)){
						act = ADV;
					}else{
						act = CONFL;
					}
				}else{
					if(va.v()==0){
						if(uncheckedSetVal(x, 0, p.level, &c)){
							act = ADV;
						}else{
							act = CONFL;
						}
					}else{
						act = ADV;
					}
				}
			}else{
				if(vb.isDef()){
					if(vb.v()==0){
						if(uncheckedSetVal(x, 0, p.level, &c)){
							act = ADV;
						}else{
							act = CONFL;
						}
					}else{
						act = ADV;
					}
				}else{
					act = ADV;
				}
			}
		}

		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouldn't be here");
	}



	// c[0] = c[1] / c[2];
	// c[1] = x*b + c  where c < b   3 = 39/10, so it is not true that c[2]= c[1]/c[0]

	// propagations: 	
	// c[1] / c[2] ==> c[0]
	// c[2]=0 ==> c[0]=0
	// c[1]=0 ==> c[0]=0	
	bool propagateDiv(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		iVar x = c[0];
		iVar a = c[1];
		iVar b = c[2];
		Val vx = vals[c[0]];
		Val va = vals[c[1]];
		Val vb = vals[c[2]];
		if(vx.isDef()){
			if(va.isDef()){
				if(vb.isDef()){
					if((vx.v() == 0 && vb.v()==0) ||
						(vb.v()!=0 && vx.v() == va.v() / vb.v())){
							act = ADV;
					}else{
						act = CONFL;
					}
				}else{
					act=ADV;
				}
			}else{// vx and !va def.
				if(vb.isDef()){
					if(vb.v()==1){
						if(uncheckedSetVal(a, vx.v() , p.level, &c)){
							act = ADV;
						}else{
							act = CONFL;
						}
					}else{
						if(vb.v()==0 && vx.v()!=0){
							act = CONFL;
						}else{
							act = ADV;
						}
					}
				}else{
					act = ADV;
				}
			}
		}else{ // !vx.
			if(va.isDef()){
				if(vb.isDef()){
					if(uncheckedSetVal(x, vb.v()==0? 0 : (va.v() / vb.v()), p.level, &c)){
						act = ADV;
					}else{
						act = CONFL;
					}
				}else{
					if(va.v()==0){
						if(uncheckedSetVal(x, 0, p.level, &c)){
							act = ADV;
						}else{
							act = CONFL;
						}
					}else{
						act = ADV;
					}
				}
			}else{
				if(vb.isDef()){
					if(vb.v()==0){
						if(uncheckedSetVal(x, 0, p.level, &c)){
							act = ADV;
						}else{
							act = CONFL;
						}
					}else{
						act = ADV;
					}
				}else{
					act = ADV;
				}
			}
		}

		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouldn't be here");
	}



	typedef enum { ALLFAILED, ONEUNSET, NOTREADY} confstates;

	bool propagateConf(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		confstates state = ALLFAILED;
		int goodi = -1;
		int undef1 = -1;
		int undef2 = -1;
		for(int i=0; i<c.size(); ++i){
			iVar vari = c[i];
			Val vv = vals[vari];
			if(!vv.isDef()){
				if(state==ONEUNSET){
					state = NOTREADY;
					if(undef2>=0){ break; };
				}
				if(state==ALLFAILED){
					state = ONEUNSET;
					goodi = i;
				}		
				if(undef1==-1){
					undef1 = i;
				}else{
					if(undef2==-1){
						undef2 = i;
					}
				}
			}else{
				if(vv.v() != confVal(c, i)){
					state = NOTREADY;				
					if(undef2>=0){ break; }
				}
			}
		}
		if(state==NOTREADY){			
			if(undef2 <= 2){
				act = ADV;
			}else{
				if(undef1 == 1){
					// 1 is undefined but zero is now defined, so we have to swap 0 with undef2.
					Assert(p.var == c[0], "zero is what we just set");
					confSwap(c, 0, undef2);
					watches[c[0]].push(&c);
					return true;
				}else{
					//undef1 is either 0 or >1; either way, c[1] must be set.
					if(p.var == c[0]){
						//we just set c[0], but c[1] is also set; this means it's probably in the stack, so just wait for its turn to swap it.
						Assert(undef1>1, "wewewwe");//If we just set c[0] and c[1] must be set, that means undef1>1.
						confSwap(c, 0, undef1);
						watches[c[0]].push(&c);
						return true;
					}else{
						Assert(p.var==c[1], "told you");
						confSwap(c, 1, undef2);					
						watches[c[1]].push(&c);
						return true;
					}

				}
			}
		}else if (state==ALLFAILED){
			act = CONFL;
		}else if (state==ONEUNSET){
			iVar vari = c[goodi];
			if(isBool[vari]==1){
				int val = confVal(c, goodi);			
				if(uncheckedSetVal(vari, 1-val, p.level, &c)){
					act = ADV;
				}else{
					act = CONFL;
				}
			}else{
				act = ADV;
			}
		}

		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouldn't be here");
	}


	typedef enum{ PROPAGATED, NOTPROPAGATED, CONFLICT} RangePropStatus;

	/*!
	Update range for variable vr with outnew only if outnew is better than the old one. 
	*/
	RangePropStatus updateIfBetter(iVar vr, int level, const Range& outnew, Intclause& c) {
		const Range& outold = ranges.getRange(vr);
		Range tmp  = intersect(outold, outnew);
		if (tmp.isEmpty()) {
			return CONFLICT;
		} else {
			if (!(tmp == outold)) {
				if (tmp.isSingle()) {
					uncheckedSetVal(vr, tmp.getLo(), level, &c);
				} else {
					trail.push(mpair(vr, level, 0));
					ranges.updateRange(vr, level, tmp, &c);
				}				
				return PROPAGATED;
			}
			return NOTPROPAGATED;
		}
		/*
		const Range& outold = ranges.getRange(vr);
		if (!(outnew == outold) && outnew <= outold) {
			trail.push(mpair(vr, level, 0));
			ranges.updateRange(vr, level, outnew, &c);
			return PROPAGATED;
		} else {
			if (intersect(outnew, outold).isEmpty()) {
				return CONFLICT;
			} else { 
				return NOTPROPAGATED; 
			}
		}
		
		*/
				
	}

	RangePropStatus propagateModInterval(iVar vr, iVar a, iVar b, Intclause& c, int level) {		
		if (a == b) {
			Range outnew = Range(0, 0);
			return updateIfBetter(vr, level, outnew, c);
		}
		else {
			const Range& ina = ranges.getRange(a);
			const Range& inb = ranges.getRange(b);
			Range outnew = ina % inb;
			return updateIfBetter(vr, level, outnew, c);
		}
	}


	RangePropStatus propagateDivInterval(iVar vr, iVar a, iVar b, Intclause& c, int level) {
		const Range& ina = ranges.getRange(a);
		if (a == b) {
			Range outnew = Range(1, 1);
			return updateIfBetter(vr, level, outnew, c);
		}
		else {
			const Range& inb = ranges.getRange(b);
			Range outnew = ina / inb;
			return updateIfBetter(vr, level, outnew, c);
		}
	}

	RangePropStatus propagateTimesInterval(iVar vr, iVar a, iVar b, Intclause& c, int level) {
		const Range& ina = ranges.getRange(a);		
		if (a == b) {
			Range outnew = square(ina);
			return updateIfBetter(vr, level, outnew, c);			
		} else {
			const Range& inb = ranges.getRange(b);
			Range outnew = ina * inb;
			return updateIfBetter(vr, level, outnew, c);
		}
	}

	RangePropStatus propagatePlusInterval(iVar vr, iVar a, iVar b, Intclause& c, int level) {
		const Range& ina = ranges.getRange(a);
		const Range& inb = ranges.getRange(b);
		
		Range outnew = ina + inb;
		return updateIfBetter(vr, level, outnew, c);
	}

	RangePropStatus propagateBMuxInterval(Intclause& c, int level) {
		iVar vr = c[0];
		Val vcond = vals[c[1]];
		
		if (vcond.isDef()) {
			Val vx = vals[vr];
			if (!vx.isDef()) {
				int v = vcond.v();
				Range outnew(0,0);
				if (v >= 0 && v + 2 < c.size()) {
					outnew =  ranges.getRange(c[vcond.v() + 2]);
				}
				return updateIfBetter(vr, level, outnew, c);
			}			
		} else {
			Range cond = ranges.getRange(c[1]);
			return updateIfBetter(vr, level, joinChoices(cond, c.size()-2, &c[2]), c);
		}
		return NOTPROPAGATED;
	}

	Range joinChoices(const Range& cond, int len, iVar* choices) {
		Range iter = intersect(cond, Range(0, len-1));
		if (iter.isEmpty()) {
			return Range(0, 0);
		}
		int i = iter.getLo();
		Range outnew = ranges.getRange(choices[i]);
		for (++i; i <= iter.getHi(); ++i) {
			outnew = join(outnew, ranges.getRange(choices[i]));
		}
		if (cond.getHi() >= len || cond.getLo() < 0) {
			outnew = join(outnew, Range(0, 0));
		}
		return outnew;
	}

	RangePropStatus propagateMinusInterval(iVar vr, iVar a, iVar b, Intclause& c, int level) {
		
		const Range& ina = ranges.getRange(a);
		const Range& inb = ranges.getRange(b);
		
		Range outnew = ina - inb;
		return updateIfBetter(vr, level, outnew, c);
	}

	bool propagatePlus(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		if (p.var != c[1] && p.var != c[2]) { *j++ = &c; return true; }
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a+b
			act = plusHelper(c[0], p, c[2], &c);
			if(act==REPL){
				//neither vx nor vb are defined. So we need to stop watching
				//c[1] (a) and start watching c[0] (vx). We can't do this for a PLUS
				//clause because for PLUS we always watch a and b, but we will accomplish
				//the same thing by turning this into a MINUS clause.
				//So we want to say that a = x-b
				c.retype(MINUS);
				iVar a = p.var;
				iVar x = c[0];
				c[0] = a;
				c[1] = x;
				// c[2]  is already b.
				//it is already in watches for b, so we add to watches for x.
				Assert(x != p.var , "NONONO");
				*j++ = &c;
				return true;
			}
		}else{ // p.var == c[2]
			act=plusHelper(c[0], p, c[1], &c);
			if(act==REPL){
				//neither vx nor va are defined. So we need to stop watching
				//c[2] (b) and start watching c[0] (vx). 
				//So we want to say that b = x-a
				c.retype(MINUS);
				iVar b = p.var;
				iVar x = c[0];
				iVar a = c[1];
				c[0] = b;
				c[1] = x;
				c[2] = a;
				//it is already in watches for a, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				*j++ = &c;
				return true;
			}
		}
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouldn't be here");
	}


	
	bool propagateTimes(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a*b
			act = timesHelper(c[0], p, c[2], &c);
			if(act==REPL){
				//neither vx nor vb are defined. So we need to stop watching
				//c[1] (a) and start watching c[0] (vx). We can't do this for a TIMES
				//clause because for TIMES we always watch a and b, but we will accomplish
				//the same thing by turning this into a EXACTDIV clause.
				//So we want to say that a = x/b but because it's EXACTDIV we enforce that x % b = 0;
				c.retype(EXACTDIV);
				iVar a = p.var;
				iVar x = c[0];
				c[0] = a;
				c[1] = x;
				// c[2]  is already b.
				//it is already in watches for b, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				watches[x].push(&c);
				return true;
			}
		}else{ // p.var == c[2]
			act=timesHelper(c[0], p, c[1], &c);
			if(act==REPL){
				//neither vx nor va are defined. So we need to stop watching
				//c[2] (b) and start watching c[0] (vx). 
				//So we want to say that b = x/a because it's EXACTDIV we enforce that x % a = 0;
				c.retype(EXACTDIV);
				iVar b = p.var;
				iVar x = c[0];
				iVar a = c[1];
				c[0] = b;
				c[1] = x;
				c[2] = a;
				//it is already in watches for a, so we add to watches for x.
				Assert(x != p.var, "NONONO");
				watches[x].push(&c);
				return true;
			}
		}
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
		Assert(false, "Shouln't be here");
	}

	bool propagateTimesExactDivInterval(mpair& p, Intclause& c) {
		iVar vr = c[0];
		iVar a = c[1];
		iVar b = c[2];
		if (c.tp() == EXACTDIV) {
			// vr = a // b; ,whereas we want 
			vr = c[1];
			a = c[0];
			b = c[2];
		}
		propagateTimesInterval(vr, a, b, c, p.level);
		return true;
	}
	

	bool propagatePlusMinusInterval(mpair& p, Intclause& c) {
		iVar vr = c[0];
		iVar a = c[1];
		iVar b = c[2];
		if (c.tp() == MINUS) {
			// vr = a - b; ,whereas we want 
			vr = c[1];
			a = c[0];
			b = c[2];
		}
		//At this point, vr = a + b no matter what.
		if (p.var == vr) {
			// so either b = vr - a; or a = vr - b
			RangePropStatus stat = propagateMinusInterval(a, vr, b, c, p.level);
			if (stat==NOTPROPAGATED) {
				stat = propagateMinusInterval(b, vr, a, c, p.level);
			}
			return stat != CONFLICT;
		}
		else {
			if (p.var == a) {
				// so either vr = a + b; or b = vr - a;
				RangePropStatus stat = propagatePlusInterval(vr, a, b, c, p.level);
				if (stat == NOTPROPAGATED) {
					stat = propagateMinusInterval(b, vr, a, c, p.level);
				}
				return stat != CONFLICT;
			}
			else { // p.var == b;
				   // so either vr = a + b; or a = vr - b;
				RangePropStatus stat = propagatePlusInterval(vr, a, b, c, p.level);
				if (stat == NOTPROPAGATED) {
					stat = propagateMinusInterval(a, vr, b, c, p.level);
				}
				return stat != CONFLICT;
			}
		}
	}

public:

	Intclause* propagate(){
		Intclause* confl=NULL;
		while(qhead < trail.size()){
			mpair p = trail[qhead++];
			vec<Intclause*>& ws = watches[p.var];
			Val vp = vals[p.var];
			if (ws.size() == 0) { continue; }
			Intclause  **i, **j, **end;
			for (i = j = &ws[0], end = i + ws.size();  i != end;){
				Intclause& c = **i++;
				//unlike sat, we do not reorder vars. too much trouble.
				bool goodsofar = false;
				if (vp.isDef()) {		
					//begin hack
					if (p.val != vp.v()) {
						//This means p was added because of range computation, but then it got a concrete value, so we need to update p.val
						p.val = vp.v();

					}
					//end hack
					if(c.tp()==PLUS){
						iVar vr = c[0];
						iVar a = c[1];
						iVar b = c[2];
						goodsofar = propagatePlus(c,p,i,j);	
						if(goodsofar){ 
							goodsofar = propagatePlusMinusInterval(p, c);
						}
					}else		
					if(c.tp()==MINUS){
						iVar vr = c[0];
						iVar a = c[1];
						iVar b = c[2];
						goodsofar = propagateMinus(c,p,i,j);
						if (goodsofar) { 
							goodsofar = propagatePlusMinusInterval(p, c);
						}
					}else
					if(c.tp()==TIMES){
						goodsofar = propagateTimes(c,p,i,j);	
						if (goodsofar) {
							goodsofar = propagateTimesExactDivInterval(p, c);
						}
					}else
					if(c.tp()==EXACTDIV){
						goodsofar = propagateExactDiv(c,p,i,j);	
						if (goodsofar) {
							goodsofar = propagateTimesExactDivInterval(p, c);
						}
					}else
					if(c.tp()==EQ){
						goodsofar = propagateEq(c,p,i,j);
					}else
					if(c.tp()==LT){
						goodsofar = propagateLt(c,p,i,j);
					}else
					if(c.tp()==BMUX){
						goodsofar = propagateBMux(c,p,i,j);
						if(goodsofar){ propagateBMuxInterval(c, p.level); }
					}else
					if(c.tp()==MOD){
						goodsofar = propagateMod(c,p,i,j);
						if (goodsofar) { propagateModInterval(c[0], c[1], c[2], c, p.level); }
					}else
					if(c.tp()==DIV){
						goodsofar = propagateDiv(c,p,i,j);
						if (goodsofar) { propagateDivInterval(c[0], c[1], c[2], c, p.level ); }
					}else
					if(c.tp()==CONF){
						goodsofar = propagateConf(c,p,i,j);
					}
				} else {
					goodsofar = true;
					if (c.tp() == LT) {
						goodsofar = propagateLt(c, p, i, j);
					} else{
						*j++ = &c;
						if (c.tp() == PLUS || c.tp() == MINUS) {
							goodsofar = propagatePlusMinusInterval(p, c);
						}else
						if(c.tp()==TIMES || c.tp() == EXACTDIV){
							goodsofar = propagateTimesExactDivInterval(p, c);
						}else
						if(c.tp()==EQ){
						
						}else					
						if(c.tp()==BMUX){
							propagateBMuxInterval(c,p.level);
						}else
						if(c.tp()==MOD){
							goodsofar = (propagateModInterval(c[0], c[1], c[2], c, p.level) != CONFLICT);
						
						}else
						if(c.tp()==DIV){
							goodsofar = (propagateDivInterval(c[0], c[1], c[2], c, p.level) != CONFLICT);
						}
					}
				}

				if(!goodsofar){
					//cout<<"CONFLICTING "; c.print(); cout<<" "<<p.var<<endl;
					confl = &c;
					qhead = trail.size();
					while(i<end)
						*j++ = *i++;
				}
			}
			ws.shrink(i - j);	
		}
		return confl;
	}

	void cancelUntil(int level){
		int pos = trail.size()-1;
		while(pos>0 && trail[pos].level > level){
			mpair p = trail[pos];
			vals[p.var].clear();
			reason[p.var]=NULL;
			tpos[p.var] = -1;
			pos--;
			trail.pop();
		}
		while(interf.size() > 0 && interf[interf.size()-1].tlevel >= trail.size()){
			interf.pop();
		}
		qhead = trail.size();
		ranges.popRanges(level);
	}

	Lit existingLit(iVar vr);
	void dump();
	void dumpConfs();

	int nvars(){
		return vals.size();
	}

};





}



#endif
