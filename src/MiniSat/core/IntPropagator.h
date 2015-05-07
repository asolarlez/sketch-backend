
#ifndef INTPROP_H
#define INTPROP_H

#include "Tvalue.h"
#include "SolverTypes.h"
#include "Vec.h"

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

typedef enum { PLUS, MINUS, TIMES, EXACTDIV, LT, EQ, BMUX  } ctype;


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
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*(len+3));
	return new (mem)Intclause(BMUX, len+2, x, cond, choices); // the 4 is not an error, there are 4 real things, but then at the end there is a slot to write which child is being watched.
} 

inline iVar& watchedIdx(Intclause& ic){
	return ic[4];
}

inline int bmuxChildren(Intclause& ic){
	return ic.size()-2;
}


class IntPropagator{
	// Map from 
	vec<Val> vals;
	vec<mpair> trail;
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
			free(clauses[i]);
		}
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
		return rv;
	}



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
		trail.push(mpair(vr, level, val));	
		return true;
	}

	void helper(iVar vr){
		Intclause* ic = reason[vr];
		if(ic==NULL){
			//either interface or unset.
			Lit tmp;
			Val& vv = vals[vr];
			if(vv.isDef() && checkLegal(vr, vv.v(), tmp)){
				if(var(tmp)!=var_Undef){
					explain.push(tmp);
				}
			}
		}else{
			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				if(seen[iv]==0){
					seen[iv] = 1;
					helper(iv);
				}
			}
		}
	}

	vec<Lit>& getSummary(Lit& l){
		for(int i=interf.size()-1; i>=0; i--){
			interpair& ip = interf[i];
			if(ip.l == l){
				return getSummary(trail[ip.tlevel].var, NULL);
			}
		}
		Assert(false, "WTF");
	}

	vec<Lit>& getSummary(iVar vr, Intclause* ic){		
		explain.clear();
		for(int i=0; i< seen.size(); ++i){
			seen[i] = 0;
		}
		if(ic == NULL){
			seen[vr] = 1;
			helper(vr);	
		}else{
			for(int i=0; i<ic->size(); ++i){
				iVar iv = (*ic)[i];
				if(seen[iv]==0){
					seen[iv] = 1;
					helper(iv);
				}
			}
		}		
		return explain;
	}


	bool setVal(iVar var, int val, int level){
		Val& v(vals[var]);
		if(v.isDef()){
			return v.v()==val;
		}else{
			v.set(val);
			trail.push(mpair(var, level, val));			
			return true;
		}
	}
	void addPlus(iVar a, iVar b, iVar x){
		Intclause* c = PlusClause(a, b, x);
		clauses.push(c);
		watches[a].push(c);
		watches[b].push(c);
	}
	void addTimes(iVar a, iVar b, iVar x){
		Intclause* c = TimesClause(a, b, x);
		clauses.push(c);
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
		Intclause* c = BMuxClause(cond, len, choices, x);
		clauses.push(c);
		watches[cond].push(c);
		watches[x].push(c);
		watches[choices[0]].push(c);
		watchedIdx(*c)=2;
	}

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

	bool checkLegal(iVar var, int val, Lit& out);

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
		iVar in = (*c)[cond.val + 2];
		Val vin = vals[in];
		Val vx = vals[x];
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
			iVar in = (*c)[vcond.v() + 2];
			if(vcond.v()+2 == watched.var){
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
					watches[in].push(&c);
					watches[idx].remove(&c);
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
				if(c.tp()==BMUX){
					goodsofar = propagateBMux(c,p,i,j);
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
			pos--;
			trail.pop();
		}
		while(interf.size() > 0 && interf[interf.size()-1].tlevel >= trail.size()){
			interf.pop();
		}
		qhead = trail.size();
	}

	
	void dump();

};





}



#endif