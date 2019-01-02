#pragma once


#include "BasicError.h"
#include <vector>
#include "BooleanDAG.h"
#include "DagOptim.h"

using namespace std;


class Term {
public:
	int var;
	int exponent;
	Term(int _var, int _exponent) :var(_var), exponent(_exponent) {}
	bool operator==(const Term& t) const {
		return var == t.var && exponent == t.exponent;
	}
};

class Monomial {
public:
	bool_node* coef;
	vector<Term> varpow;
	Monomial(bool_node* _coef) :coef(_coef) {}
	Monomial(const Monomial& m2) : coef(m2.coef), varpow(m2.varpow) {}
	Monomial(int id, DagOptim& dopt) {
		coef = dopt.getCnode(1.0);
		varpow.push_back(Term(id, 1));
	}
	void add(Monomial& other, DagOptim& dopt) {
		coef = PLUS_node::create(coef, other.coef);
		coef->addToParents();
		coef = dopt.optAdd(coef);
	}
	bool hasCoef() {
		return coef != NULL;
	}
	bool hasVar(int var) {
		for (auto it = varpow.begin(); it != varpow.end(); ++it) {
			if (it->var == var) {				
				return true;
			}
		}
		return false;
	}
	void addCoef(int var, int coef) {
		for (auto it = varpow.begin(); it != varpow.end(); ++it) {
			if (it->var == var) {
				it->exponent += coef;
				return;
			}
		}
		varpow.push_back(Term(var, coef));
	}
	void lprint(ostream& out) {
		out << "(" << coef->id << ")";
		for (auto it = varpow.begin(); it != varpow.end(); ++it) {
			out << "x_" << it->var << "^" << it->exponent;
		}
	}
};




inline Monomial times(Monomial& m1, Monomial& m2, DagOptim& dopt) {
	Monomial rv(m1);
	if (m2.hasCoef()) {
		if (m1.hasCoef()) {
			rv.coef = TIMES_node::create(m1.coef, m2.coef);
			rv.coef->addToParents();
			rv.coef = dopt.optAdd(rv.coef);
		}
		else {
			rv.coef = m2.coef;
		}
	}

	for (auto it1 = m2.varpow.begin(); it1 != m2.varpow.end(); ++it1) {
		rv.addCoef(it1->var, it1->exponent);
	}
	return rv;
}

inline Monomial times(Monomial& m1, bool_node* m2, DagOptim& dopt) {
	Monomial rv(m1);
	rv.coef = TIMES_node::create(m1.coef, m2);
	rv.coef->addToParents();
	rv.coef = dopt.optAdd(rv.coef);
	return rv;
}

inline bool coefCompare(vector<Term>& left, vector<Term>& right) {
	if (left.size() != right.size()) {
		return false;
	}
	int matches = 0;
	for (auto leftit = left.begin(); leftit != left.end(); ++leftit) {
		for (auto rightit = right.begin(); rightit != right.end(); ++rightit) {
			if ((*leftit) == (*rightit)) {
				matches++;
			}
		}
	}
	return matches == left.size();
}


class Polynomial;

/**
We need an index from variables to partial solutions, but we also need to remember the order in which we discovered these
partial solutions, because when doing replacements, we must also make them in order.
*/
class PartialSolIndex {
	map<int, Polynomial*> idxmap;
	vector<int> order;
	map<int, bool_node*> sols;
public:
	class iterator {
	public:
		vector<int>::iterator it;
		PartialSolIndex& psi;
		iterator(vector<int>::iterator _it, PartialSolIndex& _psi) :it(_it), psi(_psi) {}
		iterator& operator++() {
			++it;
			return *this;
		}
		pair<int, Polynomial*> operator*() {
			auto pit = psi.idxmap.find(*it);
			return make_pair(pit->first, pit->second);
		}
		bool operator!=(const iterator& other) {
			return it != other.it;
		}
		map<int, Polynomial*>::iterator operator->() {
			auto pit = psi.idxmap.find(*it);
			return pit;
		}
	};

	PartialSolIndex() {

	}

	PartialSolIndex(const PartialSolIndex& other) :idxmap(other.idxmap), order(other.order), sols(other.sols) {

	}


	void print();


	map<int, bool_node*>::iterator findConst(int id) {
		return sols.find(id);
	}

	map<int, bool_node*>::iterator const_end() {
		return sols.end();
	}


	Polynomial* find(int var) {
		auto it = idxmap.find(var);
		if (it == idxmap.end()) { return NULL; }
		return it->second;
	}

	void push_back(int var, Polynomial* p) {
		order.push_back(var);
		idxmap[var] = p;
	}

	void push_back(int var, bool_node* bn, DagOptim& dopt);

	Polynomial* operator[](int var) {
		return idxmap[var];
	}

	bool_node* get(int var) {
		return sols[var];
	}

	iterator begin() {
		return iterator(order.begin(), *this);
	}

	iterator end() {
		return iterator(order.end(), *this);
	}

};

Polynomial* ntimes(bool_node* node, Polynomial* left, Polynomial* right, DagOptim& dopt, Ostore<Polynomial>& store);
Polynomial* nplus(bool_node* node, Polynomial* left, Polynomial* right, DagOptim& dopt, Ostore<Polynomial>& store);

struct varstatus {
	bool byItself;
	int highestExp;
	varstatus() :byItself(true), highestExp(-1) {

	}
};


class Polynomial {
public:
	bool_node* rest; //constant term.
	vector<Monomial> prods;
	Polynomial() {}	
	Polynomial(Monomial& mono) {
		//Polynomial cannot have a mono with empty varpow.
		if (mono.varpow.size() == 0) {
			rest = mono.coef;
			return;
		}
		prods.push_back(mono);
		rest = NULL;
	}
	Polynomial(int id, DagOptim& dopt) {
		prods.push_back(Monomial(id, dopt));
		rest = NULL;
	}

	Polynomial(bool_node* novars) {
		rest = novars;
	}
	Polynomial* clone(Ostore<Polynomial>& store) {
		Polynomial* cl = new(store.newObj())Polynomial();
		cl->rest = this->rest;
		cl->prods = this->prods;
		return cl;
	}

	bool hasVar(int var) {
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			if (it->hasVar(var)) {
				return true;
			}
		}
		return false;
	}
	bool isConstant() {
		return rest != NULL && prods.size() == 0;
	}
	bool hasConstant() {
		return rest != NULL;
	}
	bool hasMonomials() {
		return prods.size() > 0;
	}

	bool singleVar() {
		int var = -1;
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			if (it->varpow.size() > 1) { return false; }
			if (var == -1) {
				var = it->varpow[0].var;
			}
			else {
				if (var != it->varpow[0].var) { return false; }
			}
		}
		return true;
	}



	bool_node* sqrt(bool_node* bn, DagOptim& dopt) {

		Tuple* ufout = dynamic_cast<Tuple*>( OutType::getTuple("_sqrt_RET") );
		if (ufout->entries.size() == 0) {
			vector<OutType*> ot;
			ot.push_back(OutType::FLOAT);
			ufout = dynamic_cast<Tuple*>(OutType::makeTuple("_sqrt_RET", ot, -1));
		}

		UFUN_node* uf = UFUN_node::create("sqrt_math", 1);
		uf->arguments(0) =  bn;
		uf->mother() = dopt.getCnode(1);
		uf->set_tupleName("_sqrt_RET");
		uf->set_nbits(0);
		uf->addToParents();

		TUPLE_R_node* rv = TUPLE_R_node::create();
		rv->mother() = dopt.optAdd(uf);
		rv->idx = 0;
		rv->addToParents();
		return dopt.optAdd(rv);
	}

	bool_node* minus(bool_node* bn, DagOptim& dopt) {
		bool_node* rv = NEG_node::create();
		rv->mother() = bn;
		rv->addToParents();
		return dopt.optAdd(rv);
	}
	bool_node* over(bool_node* a, bool_node* b, DagOptim& dopt) {
		bool_node* rv = DIV_node::create();
		rv->mother() = a;
		rv->father() = b;
		rv->addToParents();
		return dopt.optAdd(rv);
	}

	bool_node* times(bool_node* a, bool_node* b, DagOptim& dopt) {
		bool_node* rv = TIMES_node::create();
		rv->mother() = a;
		rv->father() = b;
		rv->addToParents();
		return dopt.optAdd(rv);
	}
	bool_node* plus(bool_node* a, bool_node* b, DagOptim& dopt) {
		bool_node* rv = PLUS_node::create();
		rv->mother() = a;
		rv->father() = b;
		rv->addToParents();
		return dopt.optAdd(rv);
	}

	bool_node* band(bool_node* a, bool_node* b, DagOptim& dopt) {
		bool_node* rv = AND_node::create();
		rv->mother() = a;
		rv->father() = b;
		rv->addToParents();
		return dopt.optAdd(rv);
	}

	bool_node* neq(bool_node* a, bool_node* b, DagOptim& dopt) {
		bool_node* rv = EQ_node::create();
		rv->mother() = a;
		rv->father() = b;
		rv->addToParents();
		rv = dopt.optAdd(rv);

		bool_node* rv2 = NOT_node::create();
		rv2->mother() = rv;
		rv2->addToParents();
		return dopt.optAdd(rv2);
	}

	void simplifyWConstants(PartialSolIndex& partialSols, DagOptim& dopt) {
		for (auto it = prods.begin(); it != prods.end(); ) {
			Monomial& mm = (*it);
			for (auto prod = mm.varpow.begin(); prod != mm.varpow.end(); ) {
				auto found = partialSols.findConst(prod->var);
				if (found != partialSols.const_end()) {
					if (prod->exponent == 1) {
						mm.coef = times(found->second, mm.coef, dopt);
					}
					else {
						Assert(prod->exponent == 2, "qpoiuyghjkol");
						mm.coef = times(mm.coef, times(found->second, found->second, dopt), dopt);
					}
					prod = mm.varpow.erase(prod);
				}
				else {
					++prod;
				}
			}
			if (mm.varpow.size() == 0) {
				this->rest = plus(rest, mm.coef, dopt);
				it = prods.erase(it);
			}
			else {
				bool match = false;
				for (auto other = prods.begin(); other != it; ++other) {
					if (other->varpow == mm.varpow) {
						other->coef = plus(other->coef, mm.coef, dopt);
						match = true;
						it = prods.erase(it);
						break;
					}
				}
				if (!match) {
					++it;
				}
			}
		}
	}

	void solveVar(PartialSolIndex& partialSols, DagOptim& dopt) {
		int var = -1;
		int deg = -1;
		bool_node* C[3];
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			if (var == -1) {
				var = it->varpow[0].var;
				deg = it->varpow[0].exponent;
				Assert(deg < 3, ";qouytyo7yu WTF?");
				C[deg] = it->coef;
			}
			else {
				int nd = it->varpow[0].exponent;
				if (deg < nd) {
					deg = nd;
				}
				Assert(nd < 3, ";qouytyo7yu WTF?");
				C[nd] = it->coef;
			}
		}
		C[0] = rest;
		if (deg == 1) {
			bool_node* bn = over(minus(rest, dopt), C[1], dopt);
			partialSols.push_back(var, bn, dopt);			
		}
		if (deg == 2) {
			bool_node* discr = plus(times(C[1], C[1], dopt), minus(times(dopt.getCnode(4.0), times(C[2], C[0], dopt), dopt), dopt), dopt);
			bool_node* bn = over(plus(minus(C[1], dopt), sqrt(discr, dopt), dopt), plus(C[2], C[2], dopt), dopt);
			partialSols.push_back(var, bn, dopt);
		}
	}

	void cleanup(DagOptim& dopt) {
		for (auto it = prods.begin(); it != prods.end(); ) {
			if (dopt.isZero(it->coef)) {
				it = prods.erase(it);
			}
			else {
				++it;
			}
		}
	}

	Polynomial* neg(bool_node* node, DagOptim& dopt, Ostore<Polynomial>& store) {
		if (isConstant() && node != NULL) {
			return new (store.newObj()) Polynomial(node);
		}
		Polynomial* rv = new Polynomial(NULL);
		if (rest != NULL) {
			rv->rest = NEG_node::create();
			rv->rest->mother() = rest;
			rv->rest->addToParents();
			rv->rest = dopt.optAdd(rv->rest);
		}
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			Monomial mm(*it);
			bool_node* tmp = mm.coef;
			mm.coef = NEG_node::create();
			mm.coef->mother() = tmp;
			mm.coef->addToParents();
			mm.coef = dopt.optAdd(mm.coef);
			rv->prods.push_back(mm);
		}
		return rv;
	}

	void lprint(ostream& out) {
		bool t = false;
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			if (t) {
				out << " + ";
			}
			t = true;
			it->lprint(out);
		}
		if (rest != NULL) {
			if (t) {
				out << " + ";
			}
			out << "(" << rest->id << ")";
		}
		out << endl;
	}


	void simplify(PartialSolIndex& partialSols, DagOptim& dopt, Ostore<Polynomial>& store) {

		vector<Polynomial*> newpols;
		for (auto it = prods.begin(); it != prods.end(); ) {
			Monomial& mm = *it;
			vector<Polynomial*> replacements;
			Monomial leftover(mm.coef);
			for (auto elems = mm.varpow.begin(); elems != mm.varpow.end(); ++elems) {
				Polynomial* oth = partialSols.find(elems->var);
				if (oth != NULL) {
					for (int i = 0; i < elems->exponent; ++i) {
						replacements.push_back(oth);
					}
				}
				else {
					leftover.varpow.push_back(*elems);
				}
			}
			if (replacements.size() == 0) {
				//just keep around the old monomial.				
				++it;
			}
			else {
				//multiply together all the replacements, multiply them times the leftovers, and add them to newm.
				auto replit = replacements.begin();
				Polynomial * newpol = *replit;
				for (++replit; replit != replacements.end(); ++replit) {
					newpol = ntimes(NULL, newpol, *replit, dopt, store);
				}
				Polynomial tmp(leftover);
				newpol = ntimes(NULL, newpol, &tmp, dopt, store);
				newpols.push_back(newpol);
				it = prods.erase(it);
			}
		}

		if (newpols.size() > 0) {
			auto it = newpols.begin();
			Polynomial* newpol = nplus(NULL, this, *it, dopt, store);
			for (++it; it != newpols.end(); ++it) {
				newpol = nplus(NULL, newpol, *it, dopt, store);
			}
			swap(this->prods, newpol->prods);
			swap(this->rest, newpol->rest);
		}
	}

	map<int, varstatus> findBestVars() {
		//We need to chose the best variable to isolate. 
		//A variable that only occurs by itself is better than a variable 
		//that sometimes occurs with other variables.
		//among vars that only occur by themselves, a var with exponent 1 is better
		//than one with exponent greater than one.			
		map<int, varstatus> vars;
		for (auto it = prods.begin(); it != prods.end(); ++it) {
			Monomial& mm = *it;
			if (mm.varpow.size() == 1) {
				varstatus& vs = vars[mm.varpow[0].var];
				if (vs.highestExp < 0) {
					//first time seeing this variable.
					vs.highestExp = mm.varpow[0].exponent;
					vs.byItself = true;
				}
				else {
					//I've seen this before. update the exponent.
					vs.highestExp = max(vs.highestExp, mm.varpow[0].exponent);
				}
			}
			else {
				for (auto rit = mm.varpow.begin(); rit != mm.varpow.end(); ++rit) {
					varstatus& vs = vars[rit->var];
					vs.highestExp = max(vs.highestExp, vs.highestExp);
					vs.byItself = false;
				}
			}
		}
		return vars;
		/*
		auto it = vars.begin();
		auto bestvar = it;
		for (++it; it != vars.end(); ++it) {
			if (it->second.byItself && !bestvar->second.byItself) {
				//it is better than bestvar because it is byitself and bestvar is not.
				bestvar = it;
			}
			else {
				//either it is not byitself, or bestvar is already byitself.
				if (bestvar->second.byItself && it->second.byItself && bestvar->second.highestExp > it->second.highestExp) {
					//both it and bestvar are byitself, but it has a smallest exponent.
					bestvar = it;
				}
				else
					if (!bestvar->second.byItself && !it->second.byItself && bestvar->second.highestExp > it->second.highestExp) {
						//both it and bestvar are not byitself, but it has a smallest exponent.
						bestvar = it;
					}
			}
		}
		return make_pair(bestvar->first, bestvar->second);
		*/
	}


};





inline Polynomial* nplus(bool_node* node, Polynomial* left, Polynomial* right, DagOptim& dopt, Ostore<Polynomial>& store) {
	if (left->isConstant() && right->isConstant()) {
		return new (store.newObj()) Polynomial(node);
	}

	Polynomial* rv;


	if (left->hasConstant()) {
		if (right->hasConstant()) {
			bool_node * nn = PLUS_node::create(left->rest, right->rest);
			nn->addToParents();
			nn = dopt.optAdd(nn);
			rv = new (store.newObj()) Polynomial(nn);
		}
		else {
			rv = new (store.newObj()) Polynomial(left->rest);
		}
	}
	else {
		rv = new (store.newObj()) Polynomial(right->rest);
	}
	rv->prods = left->prods;

	for (auto rightit = right->prods.begin(); rightit != right->prods.end(); ++rightit) {
		bool matched = false;
		for (auto rvit = rv->prods.begin(); rvit != rv->prods.end(); ++rvit) {
			if (coefCompare(rightit->varpow, rvit->varpow)) {
				// same var, so just add up the coefficients and continue.
				matched = true;
				rvit->add(*rightit, dopt);
				break;
			}
		}
		if (!matched) { // this is a new monomial, so we just add it.
			rv->prods.push_back(*rightit);
		}
	}
	rv->lprint(cout);
	return rv;
}

inline Polynomial* ntimes(bool_node* node, Polynomial* left, Polynomial* right, DagOptim& dopt, Ostore<Polynomial>& store) {
	if (left->isConstant() && right->isConstant()) {
		return new (store.newObj()) Polynomial(node);
	}

	Polynomial* rv;


	if (left->hasConstant()) {
		if (right->hasConstant()) {
			bool_node * nn = TIMES_node::create(left->rest, right->rest);
			nn->addToParents();
			nn = dopt.optAdd(nn);
			rv = new (store.newObj()) Polynomial(nn);
		}
		else {
			rv = new (store.newObj()) Polynomial((bool_node*)NULL);
		}
		for (auto rightit = right->prods.begin(); rightit != right->prods.end(); ++rightit) {
			rv->prods.push_back(times(*rightit, left->rest, dopt));
		}
	}
	else {
		rv = new (store.newObj()) Polynomial((bool_node*)NULL);
	}

	for (auto leftit = left->prods.begin(); leftit != left->prods.end(); ++leftit) {
		for (auto rightit = right->prods.begin(); rightit != right->prods.end(); ++rightit) {
			Monomial mm = times(*rightit, *leftit, dopt);
			bool matched = false;
			for (auto rvit = rv->prods.begin(); rvit != rv->prods.end(); ++rvit) {
				if (coefCompare(mm.varpow, rvit->varpow)) {
					// same var, so just add up the coefficients and continue.
					matched = true;
					rvit->add(mm, dopt);
					break;
				}
			}
			if (!matched) { // this is a new monomial, so we just add it.
				rv->prods.push_back(mm);
			}
		}
		if (right->hasConstant()) {
			Monomial mm = times(*leftit, right->rest, dopt);
			bool matched = false;
			for (auto rvit = rv->prods.begin(); rvit != rv->prods.end(); ++rvit) {
				if (coefCompare(mm.varpow, rvit->varpow)) {
					// same var, so just add up the coefficients and continue.
					matched = true;
					rvit->add(mm, dopt);
					break;
				}
			}
			if (!matched) { // this is a new monomial, so we just add it.
				rv->prods.push_back(mm);
			}
		}
	}
	return rv;
}




inline void PartialSolIndex::print() {
	for (auto it = sols.begin(); it != sols.end(); ++it) {
		cout << "[" << it->first << "] = " << it->second->lprint() << endl;
	}
	for (auto it = idxmap.begin(); it != idxmap.end(); ++it) {
		cout << "[" << it->first << "] = ";
		it->second->lprint(cout);
	}
}

inline void PartialSolIndex::push_back(int var, bool_node* bn, DagOptim& dopt) {
	sols[var] = bn;
	vector<pair<int, bool_node*> > more;
	for (auto it = order.begin(); it != order.end(); ) {
		auto p = idxmap[*it];
		if (p->hasVar(var)) {
			p->simplifyWConstants(*this, dopt);
			if (!p->hasMonomials()) {
				more.push_back(make_pair(*it, p->rest));
				idxmap.erase(*it);
				it = order.erase(it);
				continue;
			}
		}
		++it;
	}
	for (auto it = more.begin(); it != more.end(); ++it) {
		push_back(it->first, it->second, dopt);
	}
}