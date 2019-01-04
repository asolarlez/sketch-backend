
#include "Tvalue.h"
#include "BooleanToCNF.h"

int POISON = INT_MAX;

void Tvalue::sparsify (SolverHelper& sh) {
	type = TVAL_SPARSE;
	if(size()==2 && num_ranges[0].guard != -num_ranges[1].guard){
		//cout<<"XXX = "<<*this;
		int tmp = num_ranges[1].guard;
		num_ranges[1].guard = -num_ranges[0].guard;
		sh.addHelperC(-num_ranges[0].guard, -tmp);
		sh.addHelperC(num_ranges[0].guard, tmp);
		//cout<<"  -->  "<<*this<<endl;
	}
	id = 0;
}



void Tvalue::makeArray (SolverHelper &dir, int nbits, int arrsz, float sparseArray) {
	Assert (id > 0, "id must be positive, instead it is " << id << " (makeArray)");
	num_ranges.clear();
	int P = 10000;
	int q = P*sparseArray;
	for(int arrid=0; arrid<arrsz; ++arrid){
		vector<int> ids (nbits);
		gvvec vg;
		for (int i = 0; i < nbits; i++)
			ids[i] = getId (arrid*nbits+i);

		if(arrid > 10 && sparseArray > 0.0 && (rand() % P) >= q){
			for (int i = 0; i < nbits; i++){
				dir.addHardAssertClause(-ids[i]);			
			}
		}

		dir.getSwitchVars (ids, nbits, vg);
		for(int t=0; t<vg.size(); ++t){
			num_ranges.push_back(guardedVal(vg[t].guard , vg[t].value ,arrid));
		}
	}
	id = num_ranges[0].guard;
	if(id < 0){ id = -id; }
	_isArray = true;
	type = TVAL_SPARSE;
}



void Tvalue::makeSparse(SolverHelper &dir, int nbits, int adj) {
	if (isBvect()) {
		if (nbits == 1 && isBvect()) {
			/* Argument has a single bit (unsigned). */
			Dout(cout << "Converting " << *this << " from Bit to Sparse" << endl);

			if (id == dir.YES || id == -dir.YES) {
				/* Bit is aliases with "true" or "false". */
				int lid = abs(id);
				num_ranges.push_back(guardedVal(lid, id < 0 ? 0 : 1));
			}
			else {
				/* Generate values for assertion / negation of single id. */
				num_ranges.push_back(guardedVal(-getId(), 0));
				num_ranges.push_back(guardedVal(getId(), 1));
			}
		}
		else {
			/* More than one bit. */
			Dout(cout << "Converting from BitVector" <<
				(isBvectSigned() ? "Signed" : "") << " to Sparse" << endl);


			vector<int> ids(nbits);
			for (int i = 0; i < nbits; i++)
				ids[i] = getId(i);
			dir.getSwitchVarsBig(ids, nbits, num_ranges);
			id = num_ranges[0].guard;
			if (id < 0) { id = -id; }
			int oldsize = nbits;  /* save previous size (number of bits). */

		}

		type = TVAL_SPARSE;
	}
	else if (isSparse()) {
		Dout(cout << "Converting from Sparse to Sparse (no-op)" << endl);
	}
	else
		assert(0);  /* Can't get here. */

	/* Adjust using given coefficient. */
	intAdjust(adj);
}


Tvalue Tvalue::toSparse (SolverHelper &dir, int adj) const {
	Tvalue tv (*this);
	tv.makeSparse (dir, adj);
	return tv;
}


