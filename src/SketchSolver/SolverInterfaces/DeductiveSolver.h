#pragma once

#include "DeductiveSolverHelpers.h"
#include "SynthInSolver.h"

class DeductiveSolution : public Synthesizer {
	BooleanDAG* solution;
	string name;

	DeductiveSolution(BooleanDAG* _solution, string _name, FloatManager& _fm) :solution(_solution), name(_name), Synthesizer(_fm)
	{

	}

	bool synthesis(vec<Lit>& suggestions) {
		return true;
	}

	/*
	This is here just in case you need to do something when a new instance gets created.
	*/
	void newInstance() {

	}


	/*
	Finished solving, get ready to generate.
	*/
	virtual void finalize() {

	}


	/*
	params are the inputs to the generator, and the function should use the DagOptim to add
	the necessary nodes to the dag.
	*/
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {

	}

	/**
	This outputs the expression for the frontend.
	*/
	virtual void print(ostream& out) {

	}

};

class DeductiveSolver {

	BooleanDAG* dag;
	DagOptim dopt;
	Ostore<Polynomial> store;
	PartialSolIndex partialSols;

	vector<Polynomial*> equations;

	void print() {
		dag->lprint(cout);
		dopt.printNewNodes();
		for (auto it = equations.begin(); it != equations.end(); ++it) {
			(*it)->cleanup(dopt);
			cout << " EQ 0 == ";
			(*it)->lprint(cout);
		}

		partialSols.print();
	}



	void solveEquations() {
		print();		

		//First, solve all the easy stuff. Find all variables that can be
		// set directly from one equation.
		bool changed;
		do {
			changed = false;
			for (auto it = equations.begin(); it != equations.end();) {
				if ((*it)->singleVar()) {
					(*it)->solveVar(partialSols, dopt);
					it = equations.erase(it);
					changed = true;
				}
				else {
					(*it)->simplifyWConstants(partialSols, dopt);
					++it;
				}
			}
		} while (changed);

		
		for (auto it = equations.begin(); it != equations.end(); ++it) {
			(*it)->cleanup(dopt);			
		}

		print();
		
		{
			for (auto it = equations.begin(); it != equations.end(); ++it) {
				(*it)->elimNextVar(partialSols, dopt, store);

				dopt.printNewNodes();
				partialSols.print();
			}
		}

	}

public:

	vector<DeductiveSolution> output;
	vector<bool_node*> unknowns;


	DeductiveSolver(BooleanDAG* _dag, FloatManager& fm) :dopt(*_dag, fm), dag(_dag) {

	}

	void loadSols() {	
		Assert(dag->getNodesByType(bool_node::DST).size() == 0, "NO NO NO!");
		for (auto it = unknowns.begin(); it != unknowns.end(); ++it) {
			bool_node* expr = partialSols.get((*it)->id);
			UFUN_node* uf = (UFUN_node*)(*it)->mother;
			string name = uf->get_ufname();
			DST_node* dst = new DST_node();
			dst->name = name;
			dst->mother = expr;
			dst->addToParents();
			dopt.optAdd(dst);
		}
		dag->registerOutputs();
		dopt.cleanup(*dag);
	}

	void symbolicSolve() {

		vector<Polynomial*> pernode(dag->size());		
		vector<vector<bool_node*> > args;
		
		for (auto node = dag->begin(); node != dag->end(); ++node) {
			bool_node* bn = dopt.computeOptim(*node);
			switch (bn->type) {
			case bool_node::PLUS: {
				pernode[bn->id] = nplus(bn, pernode[bn->mother->id], pernode[bn->father->id], dopt, store);
			}
								  continue;
			case bool_node::TIMES: {
				pernode[bn->id] = ntimes(bn, pernode[bn->mother->id], pernode[bn->father->id], dopt, store);
			}
								   continue;
			case bool_node::NEG: {
				pernode[bn->id] = pernode[bn->mother->id]->neg(bn, dopt, store);
			}
								 continue;
			case bool_node::EQ: {
				pernode[bn->id] = nplus(bn, pernode[bn->mother->id], pernode[bn->father->id]->neg(NULL, dopt, store), dopt, store);
			}
								continue;
			case bool_node::ASSERT: {
				equations.push_back(pernode[bn->mother->id]);
			}
									continue;
			case bool_node::TUPLE_R: {
				//this is a fresh unknown.
				UFUN_node* ufn = (UFUN_node*)bn->mother;
				if (ufn->isSynNode()) {
					unknowns.push_back(bn);
					pernode[bn->id] = new (store.newObj()) Polynomial(bn->id, dopt);
					Assert(ufn->multi_mother.size() == 2, "Gen function should have two params, and the second one must be an array constructor.");
					bool_node* second = ufn->multi_mother[1];
					Assert(second->type == bool_node::ARR_CREATE, "Gen function should have two params, and the second one must be an array constructor.");
					ARR_CREATE_node* acn = (ARR_CREATE_node*)second;
					args.push_back(acn->multi_mother);
				}
				else {
					pernode[bn->id] = new (store.newObj()) Polynomial(bn);
				}
			}
			continue;
			default: {
				pernode[bn->id] = new (store.newObj()) Polynomial(bn);
			}
			}

		}

		solveEquations();

		loadSols();

	}
};


