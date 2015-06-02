
#ifndef INTPROP_H
#define INTPROP_H

#include "Tvalue.h"
#include "SolverTypes.h"
#include "Vec.h"
#include <cstddef>

namespace MSsolverNS{

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

typedef enum { PLUS, MINUS, TIMES, EXACTDIV, LT, EQ, BMUX, MOD, DIV  } ctype;




class Intclause{
protected:	
	ctype type;
	uint32_t size_etc;	
	iVar data[0];
public:
	Intclause(ctype tp, int sz, iVar a1, iVar a2, iVar a3):type(tp), size_etc(sz<<3){ data[0]=a1; data[1]=a2; data[2]=a3; }	
	Intclause(ctype tp, int sz,  iVar a1, iVar a2, iVar* ch):type(tp), size_etc(sz<<3){
		data[0]=a1; data[1]=a2; 
		for(int i=2; i<sz; ++i){ data[i] = ch[i-2]; }		
	}
	ctype tp(){ return type; }
	void retype(ctype tp){ type = tp; }
	iVar& operator[](int idx){
		return data[idx];
	}
	int size(){
		return size_etc >> 3;
	}
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

inline iVar& watchedIdx(Intclause& ic){
	return *(iVar*)((((char*)(&ic))-offsetof(WrappedIC,cc))+offsetof(WrappedIC, watched));
}

inline int bmuxChildren(Intclause& ic){
	return ic.size()-2;
}


class IntPropagator{
	// Map from 
	vec<Val> vals;
	vec<mpair> trail;
	vec<int> tpos;
	vector<Tvalue> mappings;
	vec<interpair> interf;
	vec<Lit> explain;
	vec<char> seen;
	vec<Intclause*> reason;
	int qhead;
	vec<Intclause*> clauses;
	vec<vec<Intclause*> >  watches;
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

	int interflen(){
		return interf.size();
	}


	Lit interfLit(int i){
		return interf[i].l;
	}

	int addVar(Tvalue& tv){
		int rv = vals.size();
		vals.push();
		watches.push();
		mappings.push_back(tv);
		seen.push();
		reason.push();
		tpos.push(-1);
		return rv;
	}

	void addMapping(int id, Tvalue& tv);

	bool uncheckedSetVal(iVar vr, int val, int level, Intclause* rson){

		Lit tmplit;
		if(!checkLegal(vr, val, tmplit)){
			return false;
		}else{
			if(var(tmplit)!= var_Undef){
				interf.push(interpair(trail.size(), tmplit));
			}
		}
		Val& v(vals[vr]);
		v.set(val);
		reason[vr] = rson;
		tpos[vr] = trail.size();
		trail.push(mpair(vr, level, val));	
		return true;
	}

	/*
	Note about helper: 
	The reasons returned by getSummary should only include variables that come before me in the 
	trial. However, there are situations when a clause forced an assignment without being fully 
	assigned (i.e. a mux can force an assignment even if some of its choices are not set), but then 
	some of those unassigned entries got assigned. so that when I call getSummary, they get returned 
	as part of the reason even though they came afterward!!!. 
	This is fixed by checking that vrlev  <maxtpos.
		
	*/
	void helper(iVar vr, int maxtpos){
		Intclause* ic = reason[vr];
		if(ic==NULL){
			//either interface or unset.
			Lit tmp;
			Val& vv = vals[vr];
			int vrlev = tpos[vr];
			if(vv.isDef() &&  vrlev >= 0 && (maxtpos<0 || vrlev < maxtpos) && checkLegal(vr, vv.v(), tmp)){
				if(var(tmp)!=var_Undef){
					explain.push(~tmp);
				}
			}
		}else{
			int vrlev = tpos[vr];
			/*
			if(ic->tp()==BMUX){
				if(vr==(*ic)[0]){
					iVar cond = (*ic)[1];
					int condpos = tpos[cond];
					Val& vc = vals[cond];
					if(vc.isDef() && condpos < vrlev){
						int vcv = vc.v();
						if(seen[cond]==0){
							seen[cond]==1;
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
			*/

			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				int parpos = tpos[iv];
				if(/* parpos < vrlev &&*/ seen[iv]==0){
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
				return getSummary(trail[lev].var, NULL, lev);
			}
		}
		Assert(false, "WTF");
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
	vec<Lit>& getSummary(iVar vr, Intclause* ic, int lev = -1){		
		explain.clear(); 
		for(int i=0; i< seen.size(); ++i){
			seen[i] = 0;
		}
		if(ic == NULL){
			seen[vr] = 1;
			helper(vr, lev);	
		}else{
			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				if(seen[iv]==0){
					seen[iv] = 1;
					helper(iv, lev);
				}
			}
		}		
		return explain;
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
			v.set(val);
			tpos[var] = trail.size();
			trail.push(mpair(var, level, val));					
			return true;
		}
	}
	void addMinus(iVar a, iVar b, iVar x){
		Intclause* c = PlusClause(a, b, x);
		c->retype(MINUS);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}

	void addPlus(iVar a, iVar b, iVar x){
		Intclause* c = PlusClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}


	void addMod(iVar a, iVar b, iVar x){
		Intclause* c = ModClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}

	void addDiv(iVar a, iVar b, iVar x){
		Intclause* c = DivClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}


	void addTimes(iVar a, iVar b, iVar x){
		Intclause* c = TimesClause(a, b, x);
		clauses.push(c);
		Assert(! (isSet(a) && isSet(b)), "This should have been constant propagated");
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
	}
	
	void addEq(iVar a, iVar b, iVar x){
		//Eq is watched by all three variables.
		Intclause* c = EqClause(a, b, x);
		clauses.push(c);
		watches[x].push(c);
		watches[a].push(c);
		watches[b].push(c);
	}
	void addBMux(iVar cond, int len, iVar* choices, iVar x){
		Assert(len > 1, "Can't have len <=1");

		Intclause* c = BMuxClause(cond, len, choices, x);
		clauses.push(c);
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
				watches[x].push(&c);
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
					if(sz == 4 && vx.isDef()){
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
				if(sz == 4 && vx.isDef() && watched.val != vx.v()){
					if(uncheckedSetVal((*c)[towatch], vx.v(), watched.level, c)){
						if(uncheckedSetVal(cond, towatch-2, watched.level, c)){
							return ADV;
						}else{
							return CONFL;
						}
					}else{
						return CONFL;
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

	bool propagatePlus(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
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
				watches[x].push(&c);
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

public:

	Intclause* propagate(){
		Intclause* confl=NULL;
		while(qhead < trail.size()){
			mpair p = trail[qhead++];
			vec<Intclause*>& ws = watches[p.var];
			Intclause  **i, **j, **end;
			for (i = j = &ws[0], end = i + ws.size();  i != end;){
				Intclause& c = **i++;
				//unlike sat, we do not reorder vars. too much trouble.
				
				bool goodsofar=false;
				if(c.tp()==PLUS){
					goodsofar = propagatePlus(c,p,i,j);					
				}else		
				if(c.tp()==MINUS){
					goodsofar = propagateMinus(c,p,i,j);
				}else
				if(c.tp()==TIMES){
					goodsofar = propagateTimes(c,p,i,j);					
				}else
				if(c.tp()==EXACTDIV){
					goodsofar = propagateExactDiv(c,p,i,j);					
				}else
				if(c.tp()==EQ){
					goodsofar = propagateEq(c,p,i,j);
				}else
				if(c.tp()==LT){
					goodsofar = propagateLt(c,p,i,j);
				}else
				if(c.tp()==BMUX){
					goodsofar = propagateBMux(c,p,i,j);
				}else
				if(c.tp()==MOD){
					goodsofar = propagateMod(c,p,i,j);
				}else
				if(c.tp()==DIV){
					goodsofar = propagateDiv(c,p,i,j);
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
	}

	Lit existingLit(iVar vr);
	void dump();

	int nvars(){
		return vals.size();
	}

};





}



#endif