
#include "Tvalue.h"
#include "BooleanToCNF.h"

 int Tvalue::getSignId (SolverHelper &dir) const {
	Assert (isBvect () || isBvectSigned (), "no sign bit for sparse");
	return (isBvect () ? -dir.YES : id + size - 1);
}

void Tvalue::sparsify (SolverHelper& sh) {
	type = TVAL_SPARSE;
	size = num_ranges.size ();
	if(size==2 && num_ranges[0].guard != -num_ranges[1].guard){
		//cout<<"XXX = "<<*this;
		int tmp = num_ranges[1].guard;
		num_ranges[1].guard = -num_ranges[0].guard;
		sh.addHelperC(-num_ranges[0].guard, -tmp);
		sh.addHelperC(num_ranges[0].guard, tmp);
		//cout<<"  -->  "<<*this<<endl;
	}	
	id = 0;
	neg = false;
}



void Tvalue::makeArray (SolverHelper &dir, int nbits, int arrsz) {
	Assert (id > 0, "id must be positive, instead it is " << id << " (makeArray)");
	Assert (this->size == nbits*arrsz, "size=" << size << " != " << nbits << "*" << arrsz);
	num_ranges.clear();
	for(int arrid=0; arrid<arrsz; ++arrid){
		vector<int> ids (nbits);
		gvvec vg;
		for (int i = 0; i < nbits; i++)
			ids[i] = getId (arrid*nbits+i);
		dir.getSwitchVars (ids, nbits, vg);
		for(int t=0; t<vg.size(); ++t){
			num_ranges.push_back(guardedVal(vg[t].guard , vg[t].value ,arrid));
		}
	}
	id = num_ranges[0].guard;
	if(id < 0){ id = -id; }
	int oldsize = size;  /* save previous size (number of bits). */
	size = num_ranges.size();

	neg = false;
	type = TVAL_ARRAY;
}



void Tvalue::makeSparse (SolverHelper &dir, int adj) {
	Assert (isSparse() || id > 0, "id must be positive, instead it is " << id << " (makeSparse)");

	if (isBvect () || isBvectSigned ()) {
	    if (size == 1 && isBvect ()) {
		/* Argument has a single bit (unsigned). */
		Dout (cout << "Converting " << *this << " from Bit to Sparse" << endl); 

		if (id == dir.YES ){
		    /* Bit is aliases with "true" or "false". */
		    num_ranges.push_back (guardedVal(id, neg ? 0 : 1));
		} else {
		    /* Generate values for assertion / negation of single id. */
		    num_ranges.push_back (guardedVal(-getId(), 0));
		    num_ranges.push_back (guardedVal(getId(), 1));		    
		    size = 2;
		}
	    } else {
		/* More than one bit. */
		Dout (cout << "Converting from BitVector" <<
		      (isBvectSigned () ? "Signed" : "") << " to Sparse" << endl); 

		
		vector<int> ids (size);
		for (int i = 0; i < size; i++)
		    ids[i] = getId (i);
		dir.getSwitchVars (ids, size, num_ranges);
		id = num_ranges[0].guard;
		if(id < 0){ id = -id; }
		int oldsize = size;  /* save previous size (number of bits). */
		size = num_ranges.size();


		/* If we generated values from a signed bitvector, we must adjust
		 * them to properly represent full int-sized signed values. */
		if (isBvectSigned ()) {
		    /* Compute padding / testing bitmap. */
		    unsigned int mask = ~((1 << oldsize) - 1);

		    /* Pad most significant bits of resulting numbers with the bit
		     * corresponding to each value's "signed bit". */
		    for (int i = 0; i < size; i++) {
			int &x = num_ranges[i].value;
			if (x & mask)
			    x |= mask;
		    }
		}
	    }
	    neg = false;
	    type = TVAL_SPARSE;
	} else if (isSparse ()) {
	    Dout (cout << "Converting from Sparse to Sparse (no-op)" << endl);
	} else
	    assert (0);  /* Can't get here. */

	/* Adjust using given coefficient. */
	intAdjust (adj);
}


Tvalue Tvalue::toSparse (SolverHelper &dir, int adj) const {
	Tvalue tv (*this);
	tv.makeSparse (dir, adj);
	return tv;
}


Tvalue Tvalue::toComplement (SolverHelper &dir) const {
	Assert (isSparse() || id > 0, "id must be positive, instead it is " << id << " (complement)");

	if (isBvect ()) {	    
	    Tvalue tv (*this);
	    tv.makeSparse(dir, -1);
	    return tv;
	} else if (isBvectSigned ()) {
	    Dout (cout << "toComplement: generating complement BitVectorSigned" << endl);

	    /* Compute the two's complement. */
	    Tvalue tv (TVAL_BVECT_SIGNED, dir.newAnonymousVar (size), size);
	    int newId = tv.getId ();
	    int oneSeen = -dir.YES;
	    for (int i = 0; i < size; i++) {
		/* Bit is inverted if 1 encountered in some earlier bit. */
		int curId = id + i;
		dir.addXorClause (curId, oneSeen, newId++);

		/* Track possibility of current bit being 1. */
		if (i < size - 1) {
		    int nextOneSeen;

		    if (i > 0) {
			nextOneSeen = dir.newAnonymousVar ();
			dir.addOrClause (curId, oneSeen, nextOneSeen);
		    } else
			nextOneSeen = curId;

		    oneSeen = nextOneSeen;
		}
	    }

	    Dout (cout << "toComplement: done" << endl);
	    return tv;
	} else if (isSparse ()) {
	    Dout (cout << "toComplement: generating inverted Sparse" << endl);

	    /* Multiply by -1. */
	    Tvalue tv (*this);
	    tv.intAdjust (-1);
	    return tv;
	} else
	    assert (0);  /* Can't get here. */

	return *this;
}


Tvalue Tvalue::sparseToBvectAny (SolverHelper &dir, unsigned padding,
	       			    bool toSigned) const {
	Assert (isSparse (), "input invariant violated");

	/* Construct bit-vector disjuncts by repeated iteration. */
	vector<vector<int> > bit;
	gvvec nr (num_ranges);
	bool more;
	do {
	    bit.push_back (vector<int> ());
	    vector<int> &current = bit[bit.size () - 1];
	    current.push_back (0);
	    Dout (cout << " new vals=");

	    /* Iterate over sparse values. */
	    more = false;
	    for (int i = 0; i < num_ranges.size (); i++) {
		int &val = nr[i].value;
		Dout (cout << (i ? "," : "") << val);

		Assert (toSigned || val >= 0,
			"cannot convert negative integer to unsigned bitvector");

		/* Skip if current value is zero (no effect on output). */
		if (val == 0)
		    continue;

		/* Require further iteration, only if not a -1 (signed shift fixpoint). */
		if (val != -1)
		    more = true;

		/* If LSB is one, record corresponding variable. */
		if (val & 0x1)
		    current.push_back (getId (i));

		/* Shift right the current value (note: this is a signed shift). */
		val = val >> 1;
	    }

	    Dout (cout << endl);
	} while (more);

	/* Last element inserted holds variables affecting the sign bit. */
	Assert (bit.size () > 0, "result must contain at least a sign bit");

	/* If converting to unsigned, dispose of sign bit. */
	bool lastpad = true;
	if (! (toSigned || padding > 0)) {
	    bit.pop_back ();
	    lastpad = false;

	    /* An empty result implies a constant zero (special case). */
	    if (bit.size () == 0)
		return Tvalue (TVAL_BVECT, -dir.YES, 1);
	} else if (! toSigned)
	    padding--;

	/* Allocate bitvector of fresh variables for both value and sign. */
	int paddedSize = bit.size () + padding;
	Tvalue tv (TVAL_BVECT_SIGNED, dir.newAnonymousVar (paddedSize), paddedSize);

	/* Assign value/sign bits with conjunction of their corresponding
	 * sparse variables. */
	int idx = 0;
	for (int i = 0; i < paddedSize; i++) {
	    vector<int> &current = bit[idx];
	    current[0] = tv.getId(i);
	    dir.addBigOrClause (&current[0], current.size () - 1);

	    /* Advance to next (actual) bit if last one not reached. */
	    if (idx < bit.size () - 1)
		idx++;
	}

	return tv;
    }


Tvalue Tvalue::toBvectSigned (SolverHelper &dir, unsigned padding) const {
	Assert (id > 0, "id must be positive, instead it is " << id << " (toBvectSigned)");

	if (isBvect () || isBvectSigned ()) {
	    Dout (cout << "toBvectSigned: converting from BitVector"
		  << (isBvectSigned () ? "Signed" : "")
		  << " to BitVectorSigned (padding=" << padding << ")" << endl);

	    /* Allocate new variables sufficient for value bits + additional sign bit + padding. */
	    int valueSize = size - (isBvectSigned () ? 1 : 0);
	    int newSize = valueSize + padding + 1;
	    Dout (cout << "toBvectSigned: allocating " << newSize << " variables" << endl);
	    Tvalue tv (TVAL_BVECT_SIGNED, dir.newAnonymousVar (newSize), newSize);

	    /* Find sign bit. */
	    int signId = getSignId (dir);

	    /* Unify value bits with previous ones. */
	    int newId = tv.getId ();
	    Dout (cout << "toBvectSigned: unifying new value bits (" << newId << "-"
		  << newId + valueSize - 1 << ") with previous ones (" << id << "-"
		  << id + valueSize - 1 << ")" << endl);
	    for (int i = 0; i < valueSize; i++)
		dir.addEqualsClause (id + i, newId++);

	    /* Unify sign and padding bits with "false". */
	    Dout (cout << "toBvectSigned: unifying sign + padding bits" << endl);
	    do {
		dir.addEqualsClause (signId, newId++);
	    } while (padding--);

	    Dout (cout << "toBvectSigned: done" << endl);
	    return tv;
	} else if (isSparse ()) {
	    Dout (cout << "Converting from Sparse to BitVectorSigned (padding="
		  << padding << ")" << endl);
	    return sparseToBvectAny (dir, padding, true);
	} else
	    assert (0);  /* Can't get here. */

	return *this;
    }

Tvalue Tvalue::toBvect (SolverHelper &dir, unsigned padding) const {
	Assert (id > 0, "id must be positive, instead it is " << id << " (toBvect)");

	if (isBvect ()) {
	    Dout (cout << "Converting from BitVector to BitVector (padding="
		  << padding << ")" << endl);

	    /* TODO handle padding. */
	    Assert (padding == 0, "padding not yet implemented for this conversion");
	} else if (isBvectSigned ()) {
	    Dout (cout << "Converting from BitVectorSigned to BitVector (padding="
		  << padding << ")" << endl);

	    /* This conversion is not allowed at the moment (requires dynamic assertion). */
	    Assert (0, "cannot convert from unsigned to signed bitvector");
	} else if (isSparse ()) {
	    Dout (cout << "Converting from Sparse to BitVector (padding="
		  << padding << ")" << endl);
	    return sparseToBvectAny (dir, padding, false);
	} else
	    assert (0);  /* Can't get here. */

	return *this;
}


void Tvalue::makeBvectSigned (SolverHelper &dir, unsigned padding) {
	Tvalue tmp = toBvectSigned (dir, padding);
	*this = tmp;
}