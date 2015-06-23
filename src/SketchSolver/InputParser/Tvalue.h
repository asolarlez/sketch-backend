#ifndef __TVALUE_H
#define __TVALUE_H

#include <vector>
#include <cassert>
#include "guardedVal.h"
#include "SATSolver.h"
using namespace std;



/*
 * The T-value holds either a bit-vector (unsigned or signed) or
 * a sparse integer representation.  In both cases, the 'size' attribute
 * stands for the number of bits (bit-vector) or the number of integers
 * stored (sparse), respectively. Both representations use 'size'
 * variables named 'id' to 'id + size - 1'.
 * 
 */

typedef enum {
    TVAL_BVECT, TVAL_BVECT_SIGNED, TVAL_SPARSE, TVAL_ARRAY
} valtype_t;


class SolverHelper;

class Tvalue {
    valtype_t type;
    int id;
    int size;
    bool neg;  /* True means the variable(s) is negated. */

public:
    /* FIXME this member needs to be made private with proper mutators, thus
     * we can guarantee sanity of a manipulated value object. */
    gvvec num_ranges;	


    /*
     * Accessors.
     */
public:
	
    inline valtype_t getType (void) const { return type; }

    inline int getId (int idx = 0) const {
		if(isSparse()){
			return num_ranges[idx].guard;
		}else{
			Assert (id >= 0, "id must be initialized");
			int ret = id + idx;
			return (neg ? -ret : ret);
		}
    }

    inline int getSize (void) const { return size; }

    inline bool isNull (void) const { return id == 0; }

    inline bool isBvect (void) const { return type == TVAL_BVECT; }

    inline bool isBvectSigned (void) const { return type == TVAL_BVECT_SIGNED; }

    inline bool isSparse (void) const { return type == TVAL_SPARSE; }

	inline bool isArray (void) const { return type == TVAL_ARRAY; }


    /*
     * Mutators.
     */

    /* FIXME not sure that these mutators should be exposed as they could
     * lead to inconsistent values (e.g. id / size not correlated with actual
     * allocated variables, etc). */
    inline void setId (int a_id, bool a_neg = false) {
	/* FIXME assertion disabled, this method seems to be used for
	 * "unintialization" of a value as well. */
#if 0
	Assert (a_id != 0, "id must be non-zero");
#endif

	id = a_id;	
	neg = a_neg;

	/* Invert negated variable id / sign. */
	if (id < 0) {
	    neg = ! neg;
	    id = -id;
	}

	/* Assert that (1) id is positive, and (2) value is non-negated, unless
	 * it is an (unsigned) bit-vector of size 1 (i.e. a single bit). */
	Assert (id >= 0 && (! neg || isBvect () && size == 1),
		"only single-bit vectors can be negated size=" << size << "  and neg="
		<< neg << "  isBvect()=" << isBvect() << "  this=" << *this);
    }

    /* FIXME same as above. */
    inline int setSize (int a_size) {
		Assert (isBvect (), "value type must be bitvector");
		size = a_size;
		return size;
    }


    /*
     * Initializers.
     */
private:
    inline void init (valtype_t a_type, int a_id, int a_size, bool a_neg = false)
    {
	type = a_type;
	size = a_size;

	/* Set variable id / polarity. */
	setId (a_id, a_neg);

	/* Initialize container. */
	num_ranges.clear ();
    }

    inline void init (int a_id) { init (TVAL_BVECT, a_id, 1); }

    inline void init (void) { init (TVAL_BVECT, 0, -1); }


    /*
     * Operators.
     */
public:
    bool operator== (const Tvalue &tv) const {
	if (tv.id != id || tv.neg != neg || tv.size != size || tv.num_ranges.size() != num_ranges.size())
	    return false;	
	for (int i = 0; i < num_ranges.size(); i++)
	    if (num_ranges[i] != tv.num_ranges[i])
		return false;

	return true;
    }

    bool operator!= (const Tvalue &tv) const {
	return ! (*this == tv);
    }

    inline Tvalue &operator= (const Tvalue &a_tv) {
	/* FIXME this assertion probably should hold, however it is violated by some use. */
#if 0
	Assert (a_tv.id > 0, "id must be positive, instead it is " << a_tv.id << " (operator=)");
#endif

	init (a_tv.type, a_tv.id, a_tv.size, a_tv.neg);
	num_ranges = a_tv.num_ranges;
	return *this;
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
    Tvalue (valtype_t a_type, int a_id, int a_size, bool a_neg = false) {
	init (a_type, a_id, a_size, a_neg);
    }

    Tvalue (void) {
	init (TVAL_BVECT, 0, -1);
    }

    Tvalue (int a_id) { *this = a_id; }

    Tvalue (const Tvalue &a_tv) { *this = a_tv; }


    /*
     * Friends.
     */
    friend ostream &operator<< (ostream &out, const Tvalue &tv) {
	out << "{" << (tv.neg ? -tv.id : tv.id);

	if (!tv.isBvect ()){
	    out << " [ ";
	    for (int i = 0; i < tv.size; i++)
			out << tv.num_ranges[i] << ", ";
	    out << " ] ";
	} else
	    out << " size=" << tv.size;

	out << "}";

	return out;
    }

	int eval(SATSolver* solv){
		if ( isSparse () ){
			int tq = 0;
			bool found = false;
			for (int i = 0; i < size; i++){
				int g;
				if(num_ranges[i].guard > 0){
					g = solv->getVarVal(num_ranges[i].guard);
				}else{
					g = (-solv->getVarVal(-num_ranges[i].guard));
				}
				if(g > 0){
					tq = num_ranges[i].value;
					found = true;
				}
			}			
			Assert(found, "What !!??");
			return tq;
		} else{
			int tt = ((neg? (-1):1 ) * solv->getVarVal(id));
			return (tt==-1 ? 0 : 1);
		}
	}


	void print(ostream &out, SATSolver* solv){
		
		if ( isSparse () || isArray()){
			
			int tq = 0;
			bool found = false;
			int ti = -1;
			int lastidx = -1;
			int lastvisitedidx = -1;			
			for (int i = 0; i < size; i++){
				if(isArray() && lastidx !=  num_ranges[i].idx && lastidx != lastvisitedidx){
					out<<" missing idx = "<<lastidx;
				}
				int g;
				if(num_ranges[i].guard > 0){
					g = solv->getVarVal(num_ranges[i].guard);
				}else{
					g = (-solv->getVarVal(-num_ranges[i].guard));
				}
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
			int tt = ((neg? (-1):1 ) * solv->getVarVal(id));
			out << "{" << (neg ? -id : id)<<":("<< (tt==-1 ? 0 : 1)<<")}";
		}
	}



    /*
     * Methods.
     */
    /* Get variable corresponding to the sign bit of a bit-vector. */
    int getSignId (SolverHelper &dir) const;


    /* When we create a sparse value we first populate num_ranges and then we call this method */
    void sparsify (SolverHelper& sh);

    inline void arrayify (void) {
	type = TVAL_ARRAY;
	size = num_ranges.size ();
	id = 0;
	neg = false;
    }


    /* Negate a bit-vector.
     * FIXME we can generally only negate (=adjust?) a single-bit unsigned vector.
     * However I suspect that this might be called with *signed* bitvectors as well... */
    inline void bitAdjust (bool bit) {
	Assert (isBvect (), "value type must be bitvector");
	if (! bit)
	    neg = ! neg;
    }

	void makeIntVal(int yes, int x){
		Assert(num_ranges.size()==0, "NOGOOD");
		num_ranges.push_back(guardedVal(yes,x)); 
		type = TVAL_SPARSE;
		size = 1;
		id = 0;
		neg = false;
	}

	void makeBitVal(int yes, bool bit){
		size=1;
		setId(yes, !bit);
	}

    /* Multiply a sparse by some integer factor. */
    inline void intAdjust (int adj) {
		Assert (isSparse (), "value type must be sparse");
		if (adj != 1){	    
			num_ranges.adjustInt(adj);
		}
    }

    /* Invert an integer value. */
    Tvalue toComplement (SolverHelper &dir) const;

private:
    /* Convert a sparse into unsigned / signed bit-vector, including padding bits. */
    Tvalue sparseToBvectAny (SolverHelper &dir, unsigned padding,
	       			    bool toSigned) const;

public:
    Tvalue toBvect (SolverHelper &dir, unsigned padding = 0) const;

    Tvalue toBvectSigned (SolverHelper &dir, unsigned padding = 0) const;

    void makeBvectSigned (SolverHelper &dir, unsigned padding = 0);

	void makeArray (SolverHelper &dir, int nbits, int arrsz);

    void makeSparse (SolverHelper &dir, int adj = 1);

    Tvalue toSparse (SolverHelper &dir, int adj = 1) const;

    inline void addArrDefault(int g, int v) {
	    if (isArray()) {
		    if (num_ranges.size()==0 || num_ranges[0].idx != -1) {
				num_ranges.push_front(guardedVal(g, v, -1));			    
				size = num_ranges.size();
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

#endif  /* __TVALUE_H */

