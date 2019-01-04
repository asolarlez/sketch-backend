#ifndef __TVALUE_H
#define __TVALUE_H

#include <vector>
#include <cassert>
#include "guardedVal.h"
#include "SATSolver.h"
#include "FloatSupport.h"
#include <climits>

using namespace std;

extern int POISON;

/*
 * The T-value holds either a bit-vector (unsigned or signed) or
 * a sparse integer representation.  In both cases, the 'size' attribute
 * stands for the number of bits (bit-vector) or the number of integers
 * stored (sparse), respectively. Both representations use 'size'
 * variables named 'id' to 'id + size - 1'.
 * 
 */

typedef enum {
    TVAL_BIT, TVAL_NATIVE, TVAL_SPARSE
} valtype_t;

class SolverHelper;

class SolverHelper;

class Tvalue {
    valtype_t type;
    int id;
	bool _isArray;
public:
    /* FIXME this member needs to be made private with proper mutators, thus
     * we can guarantee sanity of a manipulated value object. */
    gvvec num_ranges;	


    /*
     * Accessors.
     */
public:
	
	bool isInt() const{
		return type == TVAL_NATIVE;
	}

	void makeSuperIntArr() {
		type = TVAL_NATIVE;
		_isArray = true;	
	}

	void makeSuperInt(int var){
		id = var;
		type = TVAL_NATIVE;
		_isArray = false;		
		num_ranges.clear();
		num_ranges.push_back(guardedVal(id, -1));
	}

    inline valtype_t getType (void) const { return type; }

    inline int getId (int idx = 0) const {
		if(isInt()){
			return id;
		}
		if(isSparse()){
			return num_ranges[idx].guard;
		}else{			
			int ret =  id > 0? (id + idx) : (id - idx);
			return ret;
		}
    }


	void assignSubrange(const gvvec& inr, int beg, int end) {
		type = TVAL_SPARSE;
		_isArray = false;
		num_ranges.clear();
		for (int i = beg; i < end; ++i) {
			num_ranges.push_back(inr[i]);
		}
	}

    inline int size (void) const { return num_ranges.size(); }

    inline bool isNull (void) const { return id == 0; }

    inline bool isBvect (void) const { return type == TVAL_BIT; }

    inline bool isSparse (void) const { return type == TVAL_SPARSE; }

	inline bool isArray (void) const { return _isArray; }

	Lit litForValue(int v) const {
		if (type == TVAL_SPARSE) {			
			for (gvvec::const_iterator it = num_ranges.begin(); it != num_ranges.end(); ++it) {
				if (it->value == v) {
					return lfromInt(it->guard);
				}
			}
			return Lit();
		}
		if (type == TVAL_BIT) {
			if (v == 0) {
				return lfromInt(-getId());
			}
			if (v == 1) {
				return lfromInt(getId());
			}
		}
		return Lit();
	}
    /*
     * Mutators.
     */

    /* FIXME not sure that these mutators should be exposed as they could
     * lead to inconsistent values (e.g. id / size not correlated with actual
     * allocated variables, etc). */
    inline void setId (int a_id) {
	/* FIXME assertion disabled, this method seems to be used for
	 * "unintialization" of a value as well. */

		id = a_id;	
	
	}




    /*
     * Initializers.
     */
private:
    inline void init (valtype_t a_type, int a_id)
    {
		type = a_type;
	

		/* Set variable id / polarity. */
		setId (a_id);

		/* Initialize container. */
		num_ranges.clear ();
		_isArray=false;
    }

    inline void init (int a_id) { init (TVAL_BIT, a_id); }

    inline void init (void) { init (TVAL_BIT, 0); }


    /*
     * Operators.
     */
public:
    bool operator== (const Tvalue &tv) const {
		if (tv.id != id || tv.num_ranges.size() != num_ranges.size())
			return false;	
		for (int i = 0; i < num_ranges.size(); i++)
			if (num_ranges[i] != tv.num_ranges[i])
			return false;

		return true;
    }

    bool operator!= (const Tvalue &tv) const {
		return ! (*this == tv);
    }



    inline Tvalue &operator= (int a_id) {
		init (a_id);
		return *this;
    }

    /* Return the i-th value represented by a sparse. */
    inline int operator[] (int idx) const {
		Assert (isSparse (), "value type must be sparse (operator[])");
		return num_ranges[idx].value;	
    }


    /*
     * Constructors.
     */
public:
    Tvalue (valtype_t a_type, int a_id) {
		init (a_type, a_id);
		_isArray=false;
    }

    inline Tvalue &operator= (const Tvalue &a_tv) {

		init (a_tv.type, a_tv.id);
		_isArray = a_tv._isArray; 
		num_ranges = a_tv.num_ranges;
		return *this;
    }

    Tvalue (void) {
		init (TVAL_BIT, 0);
		_isArray = false;
    }

    Tvalue (int a_id) { 
		*this = a_id; 
		_isArray = false;
	}

    Tvalue (const Tvalue &a_tv) { *this = a_tv;  }


    /*
     * Friends.
     */
    friend ostream &operator<< (ostream &out, const Tvalue &tv) {
		out << "{" << (tv.id);

		if (!tv.isBvect ()){
			out << " [ ";
			for (int i = 0; i < tv.size(); i++)
				out << tv.num_ranges[i] << ", ";
			out << " ] ";
		} else
			out << " size=" << tv.size();

		out << "}";

		return out;
    }

	void printFloats(ostream& out, FloatManager& fm) const {
		out << "{" << id;
		out << " [ ";
		for (int i = 0; i < size(); i++)  
			out << num_ranges[i].guard<<":"<< fm.getFloat(num_ranges[i].value)<<"("<< num_ranges[i].value<<"), ";
		out << " ] ";
		out << "}";

	}

	int eval(SATSolver* solv)const{
		if ( isSparse () ){
			int tq = 0;
			bool found = false;
			for (int i = 0; i < size(); i++){
				int g;
				g = solv->getVarVal(num_ranges[i].guard);				
				if(g > 0){
					tq = num_ranges[i].value;
					found = true;
				}
			}			
			Assert(found, "What !!??");
			return tq;
		} else{
			int tt = solv->getVarVal(id);
			return (tt==-1 ? 0 : 1);
		}
	}


	void print(ostream &out, SATSolver* solv)const{
		
		if ( isSparse () || isArray()){
			
			int tq = 0;
			bool found = false;
			int ti = -1;
			int lastidx = -1;
			int lastvisitedidx = -1;			
			for (int i = 0; i < size(); i++){
				if(isArray() && lastidx !=  num_ranges[i].idx && lastidx != lastvisitedidx){
					out<<" missing idx = "<<lastidx;
				}
				int g;
				g = solv->getVarVal(num_ranges[i].guard);				
				if(g > 0){
					tq = num_ranges[i].value;
					ti = num_ranges[i].idx;
					lastvisitedidx = ti;
					found = true;
					if(isArray()){ out << "{"<< num_ranges[i].guard <<":("<< tq  <<", "<<ti<<")}"; }else{out << "{"<< num_ranges[i].guard <<":("<< tq  <<")}";}
				}
				lastidx = num_ranges[i].idx;
			}	
			if(isArray() && lastidx != lastvisitedidx){
				out<<" missing idx = "<<lastidx;
			}
			Assert(found, "What !!??");
		} else{
			int tt = solv->getVarVal(id);
			out << "{" <<id<<":("<< (tt==-1 ? 0 : 1)<<")}";
		}
	}



    /*
     * Methods.
     */
    

	inline void markInput(SolverHelper& dir){
/* This functionality is experimental
		if(isSparse()){
			SATSolver& sol = dir.getMng();
			for(int i=0; i<num_ranges.size(); ++i){
				sol.markInput(num_ranges[i].guard);
			}
		}else{
			dir.getMng().markInput(id);
		} */
	}

    /* When we create a sparse value we first populate num_ranges and then we call this method */
    void sparsify (SolverHelper& sh);

    inline void arrayify (void) {
		_isArray = true;
		type = TVAL_SPARSE;
		id = 0;
    }

	inline void intarrayify(void) {
		arrayify();
		type = TVAL_NATIVE;
	}


    /* Negate a bit-vector.
     * FIXME we can generally only negate (=adjust?) a single-bit unsigned vector.
     * However I suspect that this might be called with *signed* bitvectors as well... */
    inline void invert() {
	Assert (isBvect (), "value type must be bitvector");
		id = -id;
    }

	void makeIntVal(int yes, int x){
		Assert(num_ranges.size()==0, "NOGOOD");
		num_ranges.push_back(guardedVal(yes,x)); 
		type = TVAL_SPARSE;		
		id = 0;
		_isArray = false;
	}

	void makeBitVal(int yes, bool bit){		
		setId( bit ?  yes : -yes);
	}

    /* Multiply a sparse by some integer factor. */
    inline void intAdjust (int adj) {
		Assert (isSparse (), "value type must be sparse");
		if (adj != 1){	    
			num_ranges.adjustInt(adj);
		}
    }

private:
    

public:
    


	void makeArray (SolverHelper &dir, int nbits, int arrsz, float sparseArray=-1.0);

	void makeSparse(SolverHelper &dir) {
		makeSparse(dir, 1, 1);
	}

	Tvalue toComplement(SolverHelper &dir) const {
		Tvalue tv(*this);
		if (isBvect()) {
			tv.makeSparse(dir, 1, -1);
			return tv;
		}
		else {
			tv.intAdjust(-1);
			return tv;
		}
	}

    void makeSparse (SolverHelper &dir, int nbits, int adj = 1);

    Tvalue toSparse (SolverHelper &dir, int adj = 1) const;

    inline void addArrDefault(int g, int v) {
	    if (isArray()) {
		    if (num_ranges.size()==0 || num_ranges[0].idx != -1) {
				num_ranges.push_front(guardedVal(g, v, -1));				
		    }
	    }
    }

    /* This method is no longer necessary, since we now explicitly negate Boolean
     * values with a dedicated gate, and get the right kind of default value
     * (i.e. sparse/int) in case of a null argument; instead, use (1) makeSparse()
     * to cast the value to a sparse representation, then (2) intAdjust to apply
     * an integer coefficient. */
#if 0
    void makeSparseCondAdjust (bool cond, int quant, varDir &dir) {
	if (! isSparse ()) {
	    if (cond) {
		Assert (quant == 1 || quant == 0,
			"If cond, then quant must either be 1 or 0");
		bitAdjust (quant == 1 );	
		makeSparse (dir);
	    } else {
		makeSparse (dir);
		if (quant != 1)
		    intAdjust (quant);	
	    }
	} else if (quant != 1)
	    intAdjust (quant);	

	Dout (cout << "quant =" << quant << " new val =" << *this << endl);
    }
#endif
};

using namespace MSsolverNS;



#endif  /* __TVALUE_H */

