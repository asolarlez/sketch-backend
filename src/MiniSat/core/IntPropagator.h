
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
	void set(int v){ val=v; }
};

typedef uint32_t iVar;


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

typedef enum { PLUS, MINUS, LT, EQ, BMUX  } ctype;


class Intclause{
protected:	
	ctype type;
	uint32_t size_etc;	
	iVar data[0];
public:
	Intclause(ctype tp, int sz, iVar a, iVar b, iVar x):type(tp), size_etc(sz<<3){ data[0]=a; data[1]=b; data[2]=x; }	
	ctype tp(){ return type; }
	void retype(ctype tp){ type = tp; }
	iVar& operator[](int idx){
		return data[idx];
	}
	int size(){
		return size_etc >> 3;
	}
};

//Represents x=a+b;
inline Intclause* PlusClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(PLUS, 3, a, b, x);
}

//Represents x=a-b;
inline Intclause* MinusClause(iVar a, iVar b, iVar x ){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(PLUS, 3, a, b, x);
}


//Represents x=a<b;
inline Intclause* LtClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(LT, 3, a, b, x);
}

//Represents x=a==b;
inline Intclause* EqClause(iVar a, iVar b, iVar x){
	void* mem = malloc(sizeof(Intclause)+sizeof(uint32_t)*3);
	return new (mem)Intclause(EQ, 3, a, b, x);
}

class IntPropagator{
	// Map from 
	vec<Val> vals;
	vec<mpair> trail;
	vec<Tvalue> mappings;
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
	int addVar(){
		int rv = vals.size();
		vals.push();
		watches.push();
		mappings.push();
		seen.push();
		reason.push();
		return rv;
	}

	int interflen(){
		return interf.size();
	}


	Lit interfLit(int i){
		return interf[i].l;
	}

	int addVar(Tvalue& tv){
		vals.push();
		watches.push();
		mappings.push(tv);
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
	}

	void helper(iVar vr){
		Intclause* ic = reason[vr];
		if(ic==NULL){
			//either interface or unset.
			Lit tmp;
			Val& vv = vals[vr];
			if(vv.isDef() && checkLegal(vr, vv.v, tmp)){
				explain.push(tmp);
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

	vec<Lit>& getSummary(iVar vr){
		explain.clear();
		for(int i=0; i< seen.size(); ++i){
			seen[i] = 0;
		}
		seen[vr] = 1;
		helper(vr);
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

private:

	typedef enum{CONFL, ADV, REPL} action;

	action plusHelper(iVar x, const mpair& me, iVar other){
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
				if(uncheckedSetVal(other, vx.v()-me.val, me.level)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(uncheckedSetVal(x, me.val+voth.v(), me.level)){
					return ADV;
				}else{
					return CONFL;
				}
			}else{				
				return REPL;
			}
		}
	}

	bool checkLegal(iVar var, int val, Lit& out){
		Tvalue& tv = mappings[var];
		if(tv.isSparse()){
			gvvec& nrange = tv.num_ranges;
			for(gvvec::iterator it=nrange.begin(); it < nrange.end(); ++it){
				if(it->idx == val){
					out = it->guard>0?Lit(it->guard):Lit(-it->guard,true);
					return true;
				}
			}
			return false;
		}
		return true;
	}

	action minusHelper(iVar x, const mpair& me, iVar other){
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
				if(uncheckedSetVal(other, me.val-vx.v(), me.level)){
					return ADV;
				}else{
					return CONFL;
				}
				
			}
		}else{// vx is not defined. need to check voth.
			if(voth.isDef()){
				//voth is defined, but vx is not. We need to set a
				if(uncheckedSetVal(x, me.val-voth.v(), me.level)){
					return ADV;
				}else{
					return CONFL;
				}				
			}else{				
				return REPL;
			}
		}
	}

	action eqHelper(iVar x, iVar a, iVar b, Val vx, Val va, Val vb){
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
				if(uncheckedSetVal(b, va.v(), me.level)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}
		if(vx.isDef() && vb.isDef()){
			if(vx.v()==1){
				if(uncheckedSetVal(a, vb.v(), me.level)){
					return ADV;
				}else{
					return CONFL;
				}
			}
		}

		if(va.isDef() && vb.isDef()){
			if(uncheckedSetVal(x, va.v()==vb.v()?1:0, me.level)){
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
		eqHelper(x, a, b, vals[x], vals[a], vals[b]);
		if(act==ADV){
			*j++ = &c;
			return true;
		}else if(act==CONFL){
			*j++ = &c;
			return false;
		}
	}


	bool propagateMinus(Intclause& c, const mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a-b
			act = minusHelper(c[0], p, c[2]);
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
			act=plusHelper(c[0], q, c[1]);
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
	}



	bool propagatePlus(Intclause& c, mpair& p, Intclause**& i, Intclause**& j){
		//p.var can only be c[1] or c[2]
		action act;
		if(p.var == c[1]){//We just set a in x=a+b
			act = plusHelper(c[0], p, c[2]);
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
			act=plusHelper(c[0], p, c[1]);
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
	}


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
				}
				if(c.tp()==MINUS){
					goodsofar = propagateMinus(c,p,i,j);
				}
				if(c.tp()==EQ){
					goodsofar = propagateEq(c,p,i,j);
				}
				if(!goodsofar){
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
	

};





}



#endif