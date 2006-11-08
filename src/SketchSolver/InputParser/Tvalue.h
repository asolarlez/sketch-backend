#ifndef __TVALUE_H
#define __TVALUE_H

#include <vector>
using namespace std;

/*
 * The T-value holds either a bit-vector (unsigned or signed) or
 * a sparse integer representation.  In both cases, the 'size' attribute
 * stands for the number of bits (bit-vector) or the number of integers
 * stored (sparse), respectively. Both representations use 'size - 1'
 * variables named 'id' to 'id + size - 1'.
 * 
 */

typedef enum {
    TVAL_BVECT, TVAL_BVECT_SIGNED, TVAL_SPARSE
} valtype_t;

class Tvalue {
    valtype_t type;
    int id;
    int size;
    bool neg;  /* True means the variable(s) is negated. */

public:
    /* FIXME this member needs to be made private with proper mutators, thus
     * we can guarantee sanity of a manipulated value object. */
    vector<int> num_ranges;	


    /*
     * Accessors.
     */
public:
    inline valtype_t getType (void) const { return type; }

    inline int getId (int idx = 0) const {
	Assert (id >= 0, "id must be initialized");
	int ret = id + idx;
	return (neg ? -ret : ret);
    }

    inline int getSize (void) const { return size; }


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
	Assert (id >= 0 && (! neg || type == TVAL_BVECT && size == 1),
		"only single-bit vectors can be negated");
    }

    /* FIXME same as above. */
    inline int setSize (int a_size) {
	Assert (type == TVAL_BVECT, "value type must be bitvector");
	size = a_size;
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
	if (tv.id != id || tv.neg != neg || tv.size != size)
	    return false;	

	for (int i = 0; i < size; i++)
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
	Assert (type == TVAL_SPARSE, "value type must be sparse (operator[])");
	return num_ranges[idx];	
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

	if (tv.type == TVAL_SPARSE ){
	    out << " [ ";
	    for (int i = 0; i < tv.size; i++)
		out << tv.num_ranges[i] << ", ";
	    out << " ] ";
	} else
	    out << " size=" << tv.size;

	out << "}";

	return out;
    }



    /*
     * Methods.
     */
    inline bool isNull (void) const { return id == 0; }

    inline bool isBvect (void) const { return type == TVAL_BVECT; }

    inline bool isBvectSigned (void) const { return type == TVAL_BVECT_SIGNED; }

    inline bool isSparse (void) const { return type == TVAL_SPARSE; }

    /* Sparsify a value.
     * FIXME seems like a inconsistency pronating method... */
    inline void sparsify (void) {
	type = TVAL_SPARSE;
	size = num_ranges.size ();	
    }

    /* Negate a bit-vector.
     * FIXME we can generally only negate (=adjust?) a single-bit unsigned vector.
     * However I suspect that this might be called with *signed* bitvectors as well... */
    inline void bitAdjust (bool bit) {
	Assert (type == TVAL_BVECT, "value type must be bitvector");
	if (! bit)
	    neg = ! neg;
    }

    /* Multiply a sparse by some integer factor. */
    inline void intAdjust (int adj) {
	Assert (type == TVAL_SPARSE, "value type must be sparse");

	if (adj != 1)
	    for (int i = 0; i < num_ranges.size (); i++)
		num_ranges[i] = num_ranges[i] * adj;
    }

    Tvalue toBvect (varDir &dir) const {
	Assert (id > 0, "id must be positive, instead it is " << id << " (toBvect)");

	if (type == TVAL_BVECT) {
	    Dout (cout << "Converting from BitVector to BitVector (no-op)" << endl);
	} else if (type == TVAL_BVECT_SIGNED) {
	    Dout (cout << "Converting from BitVectorSigned to BitVector" << endl);

	    /* This conversion is not allowed at the moment (requires dynamic assertion). */
	    Assert (0, "cannot convert from unsigned to signed bitvector");
	} else if (type == TVAL_SPARSE) {
	    Dout (cout << "Converting from Sparse to BitVector" << endl);

	    /* Construct bit-vector disjuncts by repeated iteration. */
	    vector<vector<int> > bit;
	    vector<int> nr (num_ranges);
	    bool more;
	    do {
		bit.push_back (vector<int> ());
		vector<int> &current = bit[bit.size () - 1];
		current.push_back (0);
		Dout (cout << " new vals=");

		/* Iterate over sparse values. */
		more = false;
		for (int i = 0; i < num_ranges.size (); i++) {
		    int &val = nr[i];
		    Dout (cout << ", " << val);

		    Assert (val >= 0, "cannot convert negative integer to unsigned bitvector");

		    /* Skip if current value is zero. */
		    if (val == 0)
			continue;

		    /* Set flag for further iteration. */
		    more = true;

		    /* If LSB is one, record corresponding variable. */
		    if ((val & 1) != 0)
			current.push_back (getId (i));

		    /* Shift right the current value. */
		    val = val >> 1;
		}

		Dout (cout << endl);
	    } while (more);

	    /* Last element inserted is all zeros and can be disposed. */
	    Assert (bit.size () > 0, "result must contain at least one zero bit");
	    bit.pop_back ();

	    /* An empty result is coalesced with "false". */
	    if (bit.size () == 0)
		return Tvalue (TVAL_BVECT, dir.YES, 1, false);

	    /* Otherwise, generate a bitvector of fresh variables. */
	    Tvalue tv (TVAL_BVECT, dir.newAnonymousVar (bit.size ()), bit.size ());

	    /* Associate new bit variables with conjunction of their respective
	     * sets of sparse variables. */
	    for (int i = 0; i < bit.size (); i++) {
		vector<int> &current = bit[i];
		current[0] = tv.id + i;
		dir.mng.addBigOrClause (&current[0], current.size () - 1);
	    }

	    return tv;
	} else
	    assert (0);  /* Can't get here. */

	return *this;
    }

    Tvalue toBvectSigned (varDir &dir) const {
	Assert (id > 0, "id must be positive, instead it is " << id << " (toBvectSigned)");

	if (type == TVAL_BVECT) {
	    Dout (cout << "Converting from BitVector to BitVectorSigned" << endl);

	    /* Allocate new variables sufficient for value bits + additional sign bit. */
	    Tvalue tv (TVAL_BVECT_SIGNED, dir.newAnonymousVar (size + 1), size + 1);

	    /* Unify value bits with previous ones. */
	    int newId = tv.getId ();
	    for (int i = 0; i < size; i++)
		dir.mng.addEqualsClause (id + i, newId + i);

	    /* Unify sign bit with "false". */
	    dir.mng.addEqualsClause (newId + size, -dir.YES);

	    return tv;
	} else if (type == TVAL_BVECT_SIGNED) {
	    Dout (cout << "Converting from BitVectorSigned to BitVectorSigned (no-op)" << endl);
	} else if (type == TVAL_SPARSE) {
	    Dout (cout << "Converting from Sparse to BitVector" << endl);

	    /* Construct bit-vector disjuncts by repeated iteration. */
	    vector<vector<int> > bit;
	    vector<int> nr (num_ranges);
	    bool more;
	    do {
		bit.push_back (vector<int> ());
		vector<int> &current = bit[bit.size () - 1];
		current.push_back (0);
		Dout (cout << " new vals=");

		/* Iterate over sparse values. */
		more = false;
		for (int i = 0; i < num_ranges.size (); i++) {
		    int &val = nr[i];
		    Dout (cout << ", " << val);

		    /* Current value is non-zero. */
		    if (val > 0) {
			/* Set flag for further iteration. */
			more = true;

			/* If LSB is one, record corresponding variable. */
			if ((val & 1) != 0)
			    current.push_back (getId (i));

			/* Shift right the current value. */
			val = val >> 1;
		    }
		}

		Dout (cout << endl);
	    } while (more);

	    /* Last element inserted is all zeros and can be disposed. */
	    Assert (bit.size () > 0, "result must contain at least one zero bit");
	    bit.pop_back ();

	    /* An empty result is coalesced with "false". */
	    if (bit.size () == 0)
		return Tvalue (TVAL_BVECT, dir.YES, 1, false);

	    /* Otherwise, generate a bitvector of fresh variables. */
	    Tvalue tv (TVAL_BVECT, dir.newAnonymousVar (bit.size ()), bit.size ());

	    /* Associate new bit variables with conjunction of their respective
	     * sets of sparse variables. */
	    for (int i = 0; i < bit.size (); i++) {
		vector<int> &current = bit[i];
		current[0] = tv.id + i;
		dir.mng.addBigOrClause (&current[0], current.size () - 1);
	    }

	    return tv;
	} else
	    assert (0);  /* Can't get here. */

	return *this;
    }

    void makeSparse (varDir &dir, int adj = 1) {
	Assert (id > 0, "id must be positive, instead it is" << id << " (makeSparse)");

	if (type == TVAL_BVECT || type == TVAL_BVECT_SIGNED) {
	    if (size == 1 && type == TVAL_BVECT) {
		/* Argument has a single bit (unsigned). */
		Dout (cout << "Converting " << *this << " from Bit to Sparse" << endl); 

		if (id == dir.YES ){
		    /* Bit is aliases with "true" or "false". */
		    num_ranges.push_back (neg ? 0 : 1);
		} else {
		    /* Generate values for assertion / negation of single id. */
		    num_ranges.push_back (0);
		    num_ranges.push_back (1);
		    int tmp = dir.newAnonymousVar (2);
		    dir.mng.addEqualsClause (tmp, -id);
		    dir.mng.addEqualsClause (tmp + 1, id);
		    id = tmp;
		    size = 2;
		}
	    } else {
		/* More than one bit. */
		Dout (cout << "Converting from BitVector" <<
		      (type == TVAL_BVECT_SIGNED ? "Signed" : "") << " to Sparse" << endl); 

		vector<int> &tmp = num_ranges;
		vector<int> ids (size);
		for (int i = 0; i < size; i++)
		    ids[i] = getId (i);
		varRange vr = getSwitchVars (dir.mng, dir, ids, size, tmp, dir.YES);
		id = vr.varID;
		int oldsize = size;  /* save previous size (number of bits). */
		size = vr.range;

		Assert (size == num_ranges.size (),
			"number of variables mismatches number of sparse values");

		/* If we generated values from a signed bitvector, we must adjust
		 * them to properly represent full int-sized signed values. */
		if (type == TVAL_BVECT_SIGNED) {
		    /* Compute padding / testing bitmap. */
		    unsigned int mask = ~((1 << oldsize) - 1);

		    /* Pad most significant bits of resulting numbers with the bit
		     * corresponding to each value's "signed bit". */
		    for (int i = 0; i < size; i++) {
			int &x = num_ranges[i];
			if (x & mask)
			    x |= mask;
		    }
		}
	    }
	    neg = false;
	    type = TVAL_SPARSE;
	} else if (type == TVAL_SPARSE) {
	    Dout (cout << "Converting from Sparse to Sparse (no-op)" << endl);
	} else
	    assert (0);  /* Can't get here. */

	/* Adjust using given coefficient. */
	intAdjust (adj);
    }

    inline Tvalue toSparse (varDir &dir) const {
	Tvalue tv (*this);
	tv.toSparse (dir);
	return tv;
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

