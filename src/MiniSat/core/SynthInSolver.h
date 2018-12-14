#pragma once

#include "BooleanNodes.h"
#include "SolverTypes.h"
#include "FloatSupport.h"
#include "Vec.h"
#include "Tvalue.h"
#include "MSolver.h"

class DagOptim;
class bool_node;

namespace MSsolverNS {
	
	const int EMPTY=INT32_MIN;
	
	class mstate {
	public:
		int id;
		int level;
		mstate(int _id, int _level) :id(_id), level(_level) {}
	};
	
	
	/**
	 
	 This is a table that keeps track of the values of inputs and outputs.
	 */
	class InputMatrix {
		int ninputs;
		int nfuns;
		vector<Tvalue> tvs;
		vec<int> grid;
		vec<mstate> stack;
		
	public:
		
		void print() {
			for (int i = 0; i < ninputs; ++i) {
				for (int j = 0; j < nfuns; ++j) {
					if (grid[valueid(j, i)] == EMPTY) {
						cout << "E|\t";
					} else {
						cout << grid[valueid(j, i)]<<"|\t";
					}
				}
				cout << endl;
			}
		}
		
		InputMatrix(int inputs, int outputs)
		:ninputs(inputs + outputs),
		nfuns(0){
			
		}
		int valueid(int instance, int inputid) {
			return instance*ninputs + inputid;
		}
		void pushInput(int instance, int inputid, int val, int level) {
			int id = valueid(instance, inputid);
			grid[id] = val;
			stack.push(mstate(id, level));
		}
		void backtrack(int level) {
			int i;
			int j = 0;
			for (i = stack.size() - 1; i >= 0; --i) {
				mstate& ms = stack[i];
				if (ms.level > level) {
					grid[ms.id] = EMPTY;
					++j;
				} else {
					break;
				}
			}
			stack.shrink(j);
		}
		int newInstance(vector<Tvalue>& inputs, vector<Tvalue>& outputs) {
			int sz = inputs.size() + outputs.size();
			tvs.insert(tvs.end(), inputs.begin(), inputs.end());
			tvs.insert(tvs.end(), outputs.begin(), outputs.end());
			grid.growTo(grid.size() + sz, EMPTY);
			return nfuns++;
		}
		Tvalue& getTval(int id) {
			return tvs[id];
		}
		Tvalue& getTval(int nfun, int input) {
			return tvs[valueid(nfun, input)];
		}
		int getVal(int id) {
			return grid[id];
		}
		int getVal(int nfun, int input) {
			return grid[valueid(nfun, input)];
		}
		int getNumInstances() {
			return nfuns;
		}
	};
	
	class Synthesizer {
	protected:
		/// This matrix records the values of inputs and outputs to all instances of the function you are trying to synthesize.
		InputMatrix* inout;
		
		/// FlotatManager is only relevant if you are using floating point values.
		FloatManager& fm;
		
		Solver* solver;
		
	public:
		/**
		 The conflict contains the ids of all the inputs/outputs for all function instances
		 that were involved in making the problem unsatisfiable.
		 */
		vec<Lit> conflict;
		
		Synthesizer(FloatManager& _fm) :fm(_fm) {
			
		}

		virtual ~Synthesizer() {

		}
		
		void set_inout(InputMatrix* _inout) {
			inout = _inout;
		}
		
		void set_solver(Solver* _solver) {
			solver = _solver;
		}
		
		/*
		 Return true if synthesis succeeds. If false, the conflict tells you what went wrong.
		 */
		virtual bool synthesis(vec<Lit>& suggestions)=0;
		
		/*
		 This is here just in case you need to do something when a new instance gets created.
		 */
		virtual void newInstance() = 0;
		
		
		/*
		 Finished solving, get ready to generate.
		 */
		virtual void finalize() = 0;
		
		
		/*
		 params are the inputs to the generator, and the function should use the DagOptim to add
		 the necessary nodes to the dag.
		 */
		virtual bool_node* getExpression(DagOptim* dopt, bool_node::parent_iter params_begin, bool_node::parent_iter params_end)=0;
		
		/**
		 This outputs the expression for the frontend.
		 */
		virtual void print(ostream& out) = 0;
		
		Lit getLit(int inputid, int val) {
			Tvalue& tv = inout->getTval(inputid);
			return tv.litForValue(val);
		}
		
		Lit getLit(int inputid) {
			Tvalue& tv = inout->getTval(inputid);
			int val = inout->getVal(inputid);
			return tv.litForValue(val);
		}
	};
	
	extern int ID;
	
	class SynthInSolver {
		vec<int> tmpbuf;
		InputMatrix inputOutputs;
		Synthesizer* s;
		int maxlevel;
		int id;
		Solver* solver;
	public:
		int solverIdx; // Stores the index of this object in the sins vector in the miniSAT solver.
		
		SynthInSolver(Synthesizer* syn, int inputs, int outputs, int idx, Solver* slv) :s(syn), inputOutputs(inputs, outputs), solverIdx(idx), solver(slv) {
			syn->set_inout(&inputOutputs);
			syn->set_solver(solver);
			maxlevel = 0;
			id = ++ID;
		}


		Synthesizer* getSynth() {
			return s;
		}

		~SynthInSolver() {
			delete s;
		}
		
		bool_node* getExpression(DagOptim* dopt, bool_node::parent_iter params_begin, bool_node::parent_iter params_end) {
			return s->getExpression(dopt, params_begin, params_end);
			
		}
		
		void print(ostream& out) {
			s->print(out);
		}
		
		int newInstance(vector<Tvalue>& inputs, vector<Tvalue>& outputs) {
			s->newInstance();
			return inputOutputs.newInstance(inputs, outputs);
		}
		
		
		void finalize() {
			s->finalize();
		}
		
		void backtrack(int level, vec<Lit>& suggestions) {
			if (maxlevel <= level) { return;  }
			inputOutputs.backtrack(level);
			//suggestions.clear(); // TODO: not sure if this is necessary
		}
		
		/* Returns the stack level of the input.
		 */
		Clause* pushInput(int instance, int inputid, int val, int dlevel, vec<Lit>& suggestions) {
			//id = valueid(instance,inputid)
			//write only over EMPTY values
			//cout << "ID=" << id << endl;
			int idInGrid = inputOutputs.valueid(instance,inputid);
			if (inputOutputs.getVal(idInGrid) != EMPTY){
				//writing over EMPTY values
				vec<Lit> conf;
				Tvalue& tv = inputOutputs.getTval(idInGrid);
				Lit l1 = tv.litForValue(inputOutputs.getVal(idInGrid));
				conf.push(~l1);
				Lit l2 = tv.litForValue(val);
				conf.push(~l2);
				int sz = sizeof(Clause) + sizeof(uint32_t)*(conf.size());
				tmpbuf.growTo((sz / sizeof(int)) + 1);
				void* mem = (void*)&tmpbuf[0];
				return new (mem) Clause(conf, false);
			}
			maxlevel = dlevel;
			inputOutputs.pushInput(instance, inputid, val, dlevel);
			if (!s->synthesis(suggestions)) {
				vec<Lit> conf;
				for (int i = 0; i < s->conflict.size(); ++i) {
					conf.push(~(s->conflict[i]));
				}
				
				int sz = sizeof(Clause) + sizeof(uint32_t)*(conf.size());
				tmpbuf.growTo((sz / sizeof(int)) + 1);
				void* mem = (void*)&tmpbuf[0];
				return new (mem) Clause(conf, false);
			}
			return NULL;
		}
		
	};
	
}
