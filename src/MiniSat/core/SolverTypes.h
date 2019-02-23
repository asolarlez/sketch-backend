/***********************************************************************************[SolverTypes.h]
MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/


#ifndef SolverTypes_h
#define SolverTypes_h

#include <cassert>
#include "Alg.h"
#include <algorithm>
#include <string>
#include "StringHTable.h"


//=================================================================================================
// Variables, literals, lifted booleans, clauses:
/*
#include "inttypes.h"

*/
#include "inttypes.h"


namespace MSsolverNS{


// NOTE! Variables are just integers. No abstraction here. They should be chosen from 0..N,
// so that they can be used as array indices.

typedef int Var;
#define var_Undef (-1)

typedef uint32_t iVar;

class Lit {
    int     x;
 public:
    Lit() : x(2*var_Undef)                                              { }   // (lit_Undef)
    explicit Lit(Var var, bool sign = false) : x((var+var) + (int)sign) { }

    // Don't use these for constructing/deconstructing literals. Use the normal constructors instead.
    friend int  toInt       (Lit p);  // Guarantees small, positive integers suitable for array indexing.
    friend Lit  toLit       (int i);  // Inverse of 'toInt()'
	friend Lit  lfromInt    (int i);
    friend Lit  operator   ~(Lit p);
    friend bool sign        (Lit p);
    friend int  var         (Lit p);
    friend Lit  unsign      (Lit p);
    friend Lit  id          (Lit p, bool sgn);

    bool operator == (Lit p) const { return x == p.x; }
    bool operator != (Lit p) const { return x != p.x; }
    bool operator <  (Lit p) const { return x < p.x;  } // '<' guarantees that p, ~p are adjacent in the ordering.
};

inline  Lit  lfromInt    (int i){  Lit p; p.x = i>0? i+i : (-(i+i)) + 1; return p;  }
inline  int  toInt       (Lit p)           { return p.x; }
inline  Lit  toLit       (int i)           { Lit p; p.x = i; return p; }
inline  Lit  operator   ~(Lit p)           { Lit q; q.x = p.x ^ 1; return q; }
inline  bool sign        (Lit p)           { return p.x & 1; }
inline  int  var         (Lit p)           { return p.x >> 1; }
inline  Lit  unsign      (Lit p)           { Lit q; q.x = p.x & ~1; return q; }
inline  Lit  id          (Lit p, bool sgn) { Lit q; q.x = p.x ^ (int)sgn; return q; }

const Lit lit_Undef(var_Undef, false);  // }- Useful special constants.
const Lit lit_Error(var_Undef, true );  // }


//=================================================================================================
// Lifted booleans:


class lbool {
    char     value;
    explicit lbool(int v) : value(v) { }

public:
    lbool()       : value(0) { }
    lbool(bool x) : value((int)x*2-1) { }
    int toInt(void) const { return value; }

    bool  operator == (lbool b) const { return value == b.value; }
    bool  operator != (lbool b) const { return value != b.value; }
    lbool operator ^ (bool b) const { return b ? lbool(-value) : lbool(value); }

    friend int   toInt  (lbool l);
    friend lbool toLbool(int   v);
};
inline int   toInt  (lbool l) { return l.toInt(); }
inline lbool toLbool(int   v) { return lbool(v);  }

const lbool l_True  = toLbool( 1);
const lbool l_False = toLbool(-1);
const lbool l_Undef = toLbool( 0);

//=================================================================================================
// Clause -- a simple class for representing a clause:

extern uint32_t SINGLESET;


class Fake{
	Lit data[0];
public:
	int size() const { return 2; }
	Lit&         operator [] (int i)         { return data[i]; }
    Lit          operator [] (int i) const   { return data[i]; }
};

extern int DEBUGCOUNT;

class Clause {
#ifdef _DEBUG
	int clauseid;
#endif
    uint32_t size_etc;
    union { float act; uint32_t abst; } extra;
    Lit     data[0];

public:
    void calcAbstraction() {
        uint32_t abstraction = 0;
        for (int i = 0; i < size(); i++)
            abstraction |= 1 << (var(data[i]) & 31);
        extra.abst = abstraction;  }

	Clause(Clause* other) {
#ifdef _DEBUG
		clauseid = DEBUGCOUNT++;
#endif
		size_etc = other->size_etc;
		extra = other->extra;
		int sz = size();
		for (int i = 0; i < sz; i++) data[i] = other->data[i];
	}


    // NOTE: This constructor cannot be used directly (doesn't allocate enough memory).
    template<class V>
    Clause(const V& ps, bool learnt) {
        size_etc = (ps.size() << 3) | (uint32_t)learnt;
        for (int i = 0; i < ps.size(); i++) data[i] = ps[i];
        if (learnt) extra.act = 0; else calcAbstraction(); 
#ifdef _DEBUG
		clauseid = DEBUGCOUNT++;
#endif

	}


	static size_t clauseFootprint(int nlits) {
		return sizeof(Clause) + sizeof(uint32_t)*(nlits);
	}

    // -- use this function instead:
    template<class V>
    static Clause* Clause_new(const V& ps, bool learnt = false, Ostore<char>* os=NULL) {
        assert(sizeof(Lit)      == sizeof(uint32_t));
        assert(sizeof(float)    == sizeof(uint32_t));
		size_t sz = clauseFootprint(ps.size());

		void* mem;
		if (os == NULL) {
			mem = malloc(sz);
		}
		else {
			mem = (void*) os->newObj(sz);
		}
		
        Clause* c = new (mem) Clause(ps, learnt); 
//		cout << mem << " -- " << (void*)((char*)mem + (sizeof(Clause) + sizeof(uint32_t)*(ps.size()))) << endl;
		return c;
    }

    int          size        ()      const   { return size_etc >> 3; }
    void         shrink      (int i)         { assert(i <= size()); size_etc = (((size_etc >> 3) - i) << 3) | (size_etc & 7); }
    void         pop         ()              { shrink(1); }
    bool         learnt      ()      const   { return size_etc & 1; }
    uint32_t     mark        ()      const   { return size_etc & 6; }
    void         mark        (uint32_t m)    { size_etc = (size_etc & ~6) | ((m & 6)); }
    const Lit&   last        ()      const   { return data[size()-1]; }

    // NOTE: somewhat unsafe to change the clause in-place! Must manually call 'calcAbstraction' afterwards for
    //       subsumption operations to behave correctly.
    Lit&         operator [] (int i)         { return data[i]; }
    Lit          operator [] (int i) const   { return data[i]; }
    operator const Lit* (void) const         { return data; }

    float&       activity    ()              { return extra.act; }
    uint32_t     abstraction () const { return extra.abst; }

    Lit          subsumes    (const Clause& other) const;
    void         strengthen  (Lit p);
	void print();
};


inline int intcLen(Clause& c){
	return c.size()/2;
}

inline int intcIntVar(Clause& c){
	return toInt(c[0]);
}

inline int intcVal(Clause& c, int i){
	return toInt(c[i*2+2]);
}

inline Lit& intcLit(Clause& c, int i){
	return c[i*2+1];
}





/*_________________________________________________________________________________________________
|
|  subsumes : (other : const Clause&)  ->  Lit
|  
|  Description:
|       Checks if clause subsumes 'other', and at the same time, if it can be used to simplify 'other'
|       by subsumption resolution.
|  
|    Result:
|       lit_Error  - No subsumption or simplification
|       lit_Undef  - Clause subsumes 'other'
|       p          - The literal p can be deleted from 'other'
|________________________________________________________________________________________________@*/
inline Lit Clause::subsumes(const Clause& other) const
{
    if (other.size() < size() || (extra.abst & ~other.extra.abst) != 0)
        return lit_Error;

    Lit        ret = lit_Undef;
    const Lit* c  = (const Lit*)(*this);
    const Lit* d  = (const Lit*)other;

    for (int i = 0; i < size(); i++) {
        // search for c[i] or ~c[i]
        for (int j = 0; j < other.size(); j++)
            if (c[i] == d[j])
                goto ok;
            else if (ret == lit_Undef && c[i] == ~d[j]){
                ret = c[i];
                goto ok;
            }

        // did not find it
        return lit_Error;
    ok:;
    }

    return ret;
}


inline void Clause::strengthen(Lit p)
{
    MSsolverNS::remove(*this, p);
    calcAbstraction();
}

using std::min;
using std::max;

class Range {
	int lo;
	int hi;
public:
	Range(int _lo, int _hi) : lo(_lo), hi(_hi) {}
	int getLo() const { return lo; }
	int getHi() const { return hi; }
	bool contains(int x) {
		return lo <= x && x <= hi;
	}
	bool isSingle() const {
		return lo == hi;
	}
	bool isEmpty() const {
		return (lo > hi);
	}
	std::string print() const;
	friend Range operator+(const Range& a, const Range& b);
	friend Range operator*(const Range& a, const Range& b);
	friend Range operator/(const Range& a, const Range& b);
	friend Range operator%(const Range& a, const Range& b);
	friend Range operator-(const Range& a, const Range& b);
	friend bool operator==(const Range& a, const Range& b);
	friend bool operator<=(const Range& a, const Range& b);
	friend Range join(const Range& a, const Range& b);
	friend Range intersect(const Range& a, const Range& b);
	friend Range square(const Range& a);
};

inline Range square(const Range& ina) {
	int losq = ina.getLo()*ina.getLo();
	int hisq = ina.getHi()*ina.getHi();
	if (ina.getLo() < 0 && ina.getHi() >= 0) {
		return Range(0, max(losq, hisq));
	}
	else {
		return Range(min(losq, hisq), max(losq, hisq));
	}
}

extern Range TOP_RANGE;

inline Range operator+(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }
	return Range(a.lo + b.lo, a.hi + b.hi);
}

inline Range operator-(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }
	return Range(a.lo - b.hi, a.hi - b.lo);
}

inline Range join(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }
	return Range(min(a.lo, b.lo), max(a.hi, b.hi));
}
inline Range intersect(const Range& a, const Range& b) {
	if (a == TOP_RANGE) { return b; }
	if (b == TOP_RANGE) { return a; }
	return Range(max(a.lo, b.lo), min(a.hi, b.hi));
}

inline Range operator*(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }
	int t1 = a.lo * b.lo;
	int t2 = a.hi * b.hi;
	int t3 = a.lo * b.hi;
	int t4 = a.hi * b.hi;
	return Range(min(min(t1, t2), min(t3, t4)), max(max(t1, t2), max(t3, t4)));
}

inline Range operator/(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }

	if (a.lo >= 0 && b.lo > 0) {
		return Range(a.lo/b.hi, a.hi/b.lo);
	}
	if (a.hi <= 0 && b.hi < 0) {
		return Range(a.hi / b.lo, a.lo / b.hi);
	}
	if (a.hi <= 0 && b.lo > 0) {
		return Range(a.lo / b.lo, a.hi / b.hi);
	}
	if (a.lo >= 0 && b.hi < 0) {
		return Range(a.hi / b.hi, a.lo / b.lo);
	}

	if (-a.lo >= a.hi) {
		return Range(a.lo, -a.lo);
	}
	return Range(-a.hi, a.hi);

}


inline Range operator%(const Range& a, const Range& b) {
	if (a == TOP_RANGE || b == TOP_RANGE) { return TOP_RANGE; }

	if (a.lo >= 0 ) {
		return Range(0, max(abs(b.hi), abs(b.lo)));
	}
	if (a.hi <= 0 ) {
		return Range(-max(abs(b.hi), abs(b.lo)),0);
	}
	return Range(-max(abs(b.hi), abs(b.lo)), max(abs(b.hi), abs(b.lo)));

}


inline bool operator==(const Range& a, const Range& b) {
	return a.lo == b.lo && a.hi == b.hi;
}
inline bool operator<=(const Range& a, const Range& b) {
	return (b.lo <= a.lo && a.hi <= b.hi);
}



}
#endif
