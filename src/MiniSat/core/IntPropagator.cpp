#include "IntPropagator.h"
#include "Tvalue.h"

namespace MSsolverNS {

	Range TOP_RANGE(INT_MIN, INT_MAX);

	string Range::print() const {
		if (*this == TOP_RANGE) { return "TOP"; }
		stringstream str;
		str << "(" << lo << ", " << hi << ")";
		return str.str();
	}

	int IntPropagator::addVar() {
		int rv = vals.size();
		vals.push();
		watches.push();
		mappings.push_back(Tvalue());
		seen.push();
		reason.push();
		tpos.push(-1);
		isBool.push(0);
		ranges.addVar();
		return rv;
	}

	int IntPropagator::addVar(Tvalue& tv) {
		int rv = vals.size();
		vals.push();
		watches.push();
		mappings.push_back(tv);
		const gvvec& gv = tv.num_ranges;
		if (gv.size() == 2 && gv[0].value == 0 && gv[1].value == 1) {
			isBool.push(1);
		}
		else {
			isBool.push(0);
		}
		int lo = gv[0].value;
		int hi = gv[0].value;
		for (auto it = gv.begin(); it != gv.end(); ++it) {
			if (it->value < lo) { lo = it->value; }
			if (it->value > hi) { hi = it->value; }
		}

		seen.push();
		reason.push();
		tpos.push(-1);
		ranges.addVar();
		ranges.updateRange(rv, 0, Range(lo, hi), NULL);
		return rv;
	}


	Lit IntPropagator::existingLit(iVar vr) {
		Tvalue& tv = mappings[vr];
		if (tv.isSparse()) {
			const gvvec& nrange = tv.num_ranges;
			int val = vals[vr].v();
			for (gvvec::const_iterator it = nrange.begin(); it < nrange.end(); ++it) {
				if (it->value == val) {
					return it->guard > 0 ? Lit(it->guard) : Lit(-it->guard, true);
				}
			}
		}
		return lit_Undef;
	}


	bool IntPropagator::checkLegal(iVar var, int val, Lit& out) {
		Tvalue& tv = mappings[var];
		if (tv.isSparse()) {
			const gvvec& nrange = tv.num_ranges;
			for (gvvec::const_iterator it = nrange.begin(); it < nrange.end(); ++it) {
				if (it->value == val) {
					out = it->guard > 0 ? Lit(it->guard) : Lit(-it->guard, true);
					return true;
				}
			}
			return false;
		}
		return true;
	}




	void IntPropagator::addMapping(int id, Tvalue& tv) {
		Assert(!mappings[id].isSparse(), "NONONO");
		const gvvec& gv = tv.num_ranges;
		if (gv.size() == 2 && gv[0].value == 0 && gv[1].value == 1) {
			isBool[id] = (1);
		}
		else {
			isBool[id] = (0);
		}
		mappings[id] = tv;
	}

	void Intclause::print() {
		Intclause& c = *this;
		switch (c.tp()) {
		case PLUS: cout << "PLUS"; break;
		case MINUS: cout << "MINUS"; break;
		case TIMES: cout << "TIMES"; break;
		case EXACTDIV: cout << "EXACTDIV"; break;
		case LT: cout << "LT"; break;
		case EQ: cout << "EQ"; break;
		case BMUX: cout << "BMUX"; break;
		case MOD: cout << "MOD"; break;
		case DIV: cout << "DIV"; break;
		case CONF: cout << "CONF";
			for (int j = 0; j < c.size(); ++j) {
				iVar val = c[c.size() + j];
				long long int vv = val;
				if (vv > INT_MAX) {
					vv = vv - UINT_MAX;
					vv = vv - 1;
				}
				cout << ", " << c[j] << "=" << vv;
			}

			cout << "       " << c.activity();
			cout << endl;
			return;
		}
		for (int j = 0; j < c.size(); ++j) {
			cout << ", " << c[j];
		}
		cout << endl;
	}

	void IntPropagator::dump() {
		cout << "CLAUSES: " << endl;
		for (int i = 0; i < clauses.size(); ++i) {
			clauses[i]->print();

		}
		cout << "TRAIL" << endl;
		for (int i = 0; i < trail.size(); ++i) {
			if (i == qhead) { cout << " qhead" << endl; }

			Val vp = vals[trail[i].var];
			if (vp.isDef()) {
				cout << "var=" << trail[i].var << ", val=" << trail[i].val << "  (" << trail[i].level << ")" << endl;
			}

		}
		if (trail.size() == qhead) { cout << " qhead" << endl; }
		cout << "RANGES" << endl;
		ranges.dump();
	}


	void IntPropagator::dumpConfs() {
		cout << "CONFLICTS: " << endl;
		for (int i = 0; i < conflicts.size(); ++i) {
			conflicts[i]->print();
		}
	}

}