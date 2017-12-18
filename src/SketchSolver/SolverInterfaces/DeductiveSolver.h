#pragma once

#include "DeductiveSolverHelpers.h"
#include "SynthInSolver.h"
#include "BooleanToCNF.h"

class DeductiveSolution : public Synthesizer {
	BooleanDAG* solution;
	string name;
public:

	DeductiveSolution(const string& _name, FloatManager& _fm) :name(_name), Synthesizer(_fm)
	{

	}

	void setSolution(BooleanDAG* sol) {
		auto funs = sol->getNodesByType(bool_node::UFUN);
		for (auto it = funs.begin(); it != funs.end(); ++it) {
			if ((*it)->getOtype()->str() == "_GEN_Solver") {
				UFUN_node* uf = (UFUN_node*)(*it);
				Assert(uf->multi_mother.size() == 2, "p;ytruutfyghjk");
				bool_node* multi = uf->multi_mother[1];
				Assert(multi->type == bool_node::ARR_CREATE, "ugrd4ertvbhj");
				arith_node* an = (arith_node*)multi;
				int ii = 0;
				vector<bool_node*> mmcopy = an->multi_mother;
				for (auto newins = mmcopy.begin(); newins != mmcopy.end(); ++newins, ++ii) {
					stringstream name;
					name << "INPUT_" << ii;
					bool_node* in = sol->create_inputs(0, OutType::FLOAT, name.str());
					sol->replace((*newins)->id, in);
				}				
				sol->remove(uf->id);
				sol->removeNullNodes();
				sol->cleanup();
				break;
			}
		}		
		solution = sol;
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
		return NULL;
	}

	/**
	This outputs the expression for the frontend.
	*/

	void outParent(ostream& out, bool_node* parent) {
		if (parent->type == bool_node::NEG) {
			out << "(-";
			outParent(out, parent->mother);
			out << ")";
		}
		else {
			if (parent->type == bool_node::SRC) {
				SRC_node* sn = (SRC_node*)parent;
				out << sn->get_name();
			}
			else {
				if (parent->type == bool_node::CONST) {
					CONST_node* cn = (CONST_node*)parent;
					if (cn->isFloat()) {
						out << cn->getFval();
					}
					else {
						out << cn->getVal();
					}
				}
				else {

					out << "t" << parent->id;
				}
			}			
		}
	}

	void printBinop(ostream& out, bool_node* node) {
		out << "float t" << node->id << " = ";
		outParent(out, node->mother);
		switch (node->type) {
		case bool_node::PLUS: out << " + "; break;
		case bool_node::TIMES: out << " * "; break;
		case bool_node::DIV: out << " / "; break;
		default:
			Assert(false, "NYI");
		}
		outParent(out, node->father);
		out << ";" << endl;
	}

	string nameTrans(string name) {
		if (name == "sin_math") {
			return "sin";
		}
		if (name == "cos_math") {
			return "cos";
		}
		if (name == "tan_math") {
			return "tan";
		}
		if (name == "sqrt_math") {
			return "sqrt";
		}
		return name;
	}

	void printTupleR(ostream& out, TUPLE_R_node* node) {
		Assert(node->idx == 0, "NYI");
		Assert(node->mother->type == bool_node::UFUN, "UFUN mother required!");
		UFUN_node* un = (UFUN_node*) node->mother;
		out << "float t" << node->id << ";"<<endl;
		out << nameTrans(un->get_ufname()) << "(";
		
		for (auto it = un->multi_mother.begin(); it != un->multi_mother.end(); ++it) {
			outParent(out, *it);			
			out << ", ";
		}
		out << "t" << node->id << "); " << endl;
		
	}
	virtual void print(ostream& out) {
		out << "{" << endl;
		for (auto nodeit = solution->begin(); nodeit != solution->end(); ++nodeit) {
			auto node = *nodeit;
			//out << "// " << node->lprint()<<endl;
			switch (node->type) {
			case bool_node::PLUS:
			case bool_node::TIMES:
			case bool_node::DIV:
				printBinop(out, node); break;
			case bool_node::TUPLE_R:
				printTupleR(out, (TUPLE_R_node*)node); break;
			case bool_node::DST:
				out << "return ";
				outParent(out, node->mother);
				out << ";" << endl;
				break;
			}

		}
		out << "}";		
	}

};

class DeductiveSolver {

	BooleanDAG* dag;
	DagOptim dopt;
	Ostore<Polynomial> store;
	PartialSolIndex partialSols;
	FloatManager& floats;

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


	DeductiveSolver(BooleanDAG* _dag, FloatManager& fm) :dopt(*_dag, fm), dag(_dag), floats(fm) {

	}

	void addInputs(vector<map<int, int> >& inputs, vector<bool_node*>& multi) {
		inputs.push_back(map<int, int>());
		map<int, int>& inmap = inputs[inputs.size() - 1];
		int ii = 0;
		for (auto it = multi.begin(); it != multi.end(); ++it, ++ii) {
			inmap[(*it)->globalId] = ii;
		}
	}

	void loadSols(SolverHelper& slv) {
		Assert(dag->getNodesByType(bool_node::DST).size() == 0, "NO NO NO!");
		vector<map<int, int> > inputs;
		for (auto it = unknowns.begin(); it != unknowns.end(); ++it) {
			bool_node* expr = partialSols.get((*it)->id);
			UFUN_node* uf = (UFUN_node*)(*it)->mother;			
			string name = uf->get_ufname();
			DST_node* dst = new DST_node();
			dst->name = name;
			dst->mother = expr;
			dst->addToParents();
			dag->assertions.append(dst);
			dopt.optAdd(dst);
		}		
		dopt.cleanup(*dag);
		auto outputs = dag->getNodesByType(bool_node::DST);
		auto ukn = unknowns.begin();
		for (auto it = outputs.begin(); it != outputs.end(); ++it, ++ukn) {
			DST_node* dst = (DST_node*)(*it);
			vector<Tvalue> x;
			vector<Tvalue> y;
			SynthInSolver* syn = slv.addSynthSolver(dst->get_name(), "_GEN_Algebraic", x, y, floats); 
			DeductiveSolution* ded = (DeductiveSolution*) syn->getSynth();
			vector<bool_node*> bb;
			bb.push_back((*ukn)->mother);
			BooleanDAG* bd = dag->slice(bb.begin(), bb.end() , dst->id);
			ded->setSolution(bd->clone());
			delete bd;
		}
	}

	void symbolicSolve(SolverHelper& slv) {

		vector<Polynomial*> pernode(dag->size());		
		vector<vector<bool_node*> > args;
		
		{
			DagOptim dd(*dag, floats);
			dd.process(*dag);
		}


		for (auto node = dag->begin(); node != dag->end(); ++node) {
			bool_node* bn = dopt.computeCSE(*node);
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

		loadSols(slv);
		

	}	
};


