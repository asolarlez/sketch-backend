/****************************************************************************************[Solver.C]
MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

#include "MSolver.h"
#include "Sort.h"
#include <cmath>
#include <algorithm>
#include "SynthInSolver.h"

using namespace std;

namespace MSsolverNS{



//=================================================================================================
// Constructor/Destructor:

uint32_t SINGLESET = 6; // lsb of mask must be zero
uint32_t INTSPECIAL = 4; // lsb of mask must be zero
uint32_t SPECIALFUN = 2; // lsb of mask must be zero
/*
uint32_t UFUNCLAUSE = 2;
uint32_t SYNCLAUSE = 1;
*/
char UFUNKIND = 'u';
char SYNKIND = 's';

class UfunSpecialStruct {
public:
	char kind;
	SynthInSolver* s;
	UfunSummary* uf;
	int instance;
	int inputid;
	int value;
	Lit lit;
};

/*
class SynClauseStruct {
public:
	SynthInSolver* s;
	int instance;
	int inputid;
	int value;
};
*/


void Clause::print(){
	for (int i = 0; i < size(); ++i) {
		std::cout << "," << toInt((*this)[i]);
	}
	std::cout << std::endl;
}

Solver::Solver() :

    // Parameters: (formerly in 'SearchParams')
    var_decay(1 / 0.95), clause_decay(1 / 0.999), random_var_freq(0.02)
  , restart_first(100), restart_inc(1.5), learntsize_factor((double)1/(double)3), learntsize_inc(1.1)

    // More parameters:
    //
  , expensive_ccmin  (true)
  , polarity_mode    (polarity_false)
  , verbosity        (0)

    // Statistics: (formerly in 'SolverStats')
    //
  , starts(0), decisions(0), rnd_decisions(0), propagations(0), conflicts(0)
  , clauses_literals(0), learnts_literals(0), max_literals(0), tot_literals(0)

  , ok               (true)
  , cla_inc          (1)
  , var_inc          (1)
  , qhead            (0)
  , simpDB_assigns   (-1)
  , simpDB_props     (0)
  , order_heap       (VarOrderLt(activity))
  , random_seed      (91648253)
  , progress_estimate(0)
  , remove_satisfied (true)
  , incompletenessCutoff(-1)
  , intsolve(new IntPropagator())
  , clauseStore(4000)
{}


Solver::~Solver()
{
    for (int i = 0; i < learnts.size(); i++) free(learnts[i]);
	for (int i = 0; i < binaryLearnts.size(); i++) free(binaryLearnts[i]);
//    for (int i = 0; i < clauses.size(); i++) free(clauses[i]);
	delete intsolve; 
	for (int i = 0; i < sins.size(); ++i) {delete(sins[i]); }
	for (int i = 0; i < allufuns.size(); ++i) { free(allufuns[i]); }
}


//=================================================================================================
// Minor methods:


// Creates a new SAT variable in the solver. If 'decision_var' is cleared, variable will not be
// used as a decision variable (NOTE! This has effects on the meaning of a SATISFIABLE result).
//
Var Solver::newVar(bool sign, bool dvar)
{
    int v = nVars();
    watches   .push();          // (list for positive literal)
    watches   .push();          // (list for negative literal)
    reason    .push(NULL);
    assigns   .push(toInt(l_Undef));
    level     .push(-1);
    activity  .push(0);
    seen      .push(0);

    polarity    .push((char)sign);
    decision_var.push((char)dvar);

    insertVarOrder(v);
    return v;
}

class ToVec{
public:
	Lit i; 
	Lit j;
	ToVec(Lit _i, Lit _j):i(_i),j(_j){}
	inline int size() const{ return 2; }
	inline Lit operator[](int idx) const{ if(idx==0){ return i;}else{return j;} }
};


bool Solver::addCNFBinary(Lit j, Lit i){
	if (!ok) return false;
    if(i == ~j) return true;
	lbool vi = value(i);
	lbool vj = value(j);
	if(vi==l_True || vj==l_True){ return true; }
	if(vi == l_False){
		if(vj==l_False){
			return ok = false;
		}else{
			uncheckedEnqueue(j);
		}
	}else{
		if(vj==l_False){
			uncheckedEnqueue(i);
		}else{			
			vec<Clause*>& wi = watches[toInt(~i)];
			vec<Clause*>& wj = watches[toInt(~j)];
			int szi  =wi.size();
			int szj = wj.size();
			
			if(szi <= szj){
				for(int t=0; t<szi; ++t){
					Clause* ct = wi[t];
					if(ct->size()==2){
						if( (*ct)[0]==j || (*ct)[1]==j ){
							// this clause is already here. Nothing to do.
							return true;
						}
					}
				}
			}else{
				for(int t=0; t<szj; ++t){
					Clause* ct = wj[t];
					if(ct->size()==2){
						if( (*ct)[0]==i || (*ct)[1]==i ){
							// this clause is already here. Nothing to do.
							return true;
						}
					}
				}
			}
			ToVec tv(i,j);
			Clause* c = Clause::Clause_new(tv, false, &clauseStore);
			 wi.push(c);
			 wj.push(c);
			 clauses.push(c);
			 clauses_literals += c->size(); 
		}
	}
	return true;
}




void Solver::addSynSolvClause(SynthInSolver* s, int instid, int inputid, int val, Lit lit) {

	if (value(lit) == l_True) {
		s->pushInput(instid, inputid, val, level[var(lit)], suggestions[s->solverIdx]);
		return;
	}
	if (value(lit) == l_False) {
		return;
	}


	int sz = sizeof(UfunSpecialStruct) / sizeof(Lit);
	vec<Lit> pp(sz+1, Lit(0));
	
	Clause* cc = Clause::Clause_new(pp, false, &clauseStore);
	cc->mark(SPECIALFUN);
	clauses.push(cc);
	UfunSpecialStruct* scs = (UfunSpecialStruct*) &((*cc)[0]);
	scs->kind = SYNKIND;
	scs->s = s;
	scs->instance = instid;
	scs->inputid = inputid;
	scs->value = val;
	scs->lit = lit;

	watches[toInt(lit)].push(cc);

}

bool Solver::addClause(vec<Lit>& ps, uint32_t kind)
{
    assert(decisionLevel() == 0);

	if(kind==INTSPECIAL){
		Clause* c = Clause::Clause_new(ps, false, &clauseStore);
        c->mark(kind);
		attachClause(*c);
		clauses.push(c);
		return ok;
	}


    if (false && kind == SINGLESET) {
		cout << "addClause " << ps.size();
		for (int i=0; i<ps.size(); i++) {
			cout << " " << var(ps[i]);
		}
		cout << endl;
	}
    if (!ok)
        return false;
    else{
        // Check if clause is satisfied and remove false/duplicate literals:
		if(kind != SINGLESET){
			sort(ps);
			Lit p; int i, j;
			for (i = j = 0, p = lit_Undef; i < ps.size(); i++)
				if (value(ps[i]) == l_True || ps[i] == ~p){					
					return true;
				}
				else if (value(ps[i]) != l_False && ps[i] != p)
					ps[j++] = p = ps[i];
			ps.shrink(i - j);
		}
		if(kind == SINGLESET){
			sort(ps);
			Lit p; int i, j;
			for (i = j = 0, p = lit_Undef; i < ps.size(); i++){
				lbool cv = value(ps[i]);
				if (cv == l_True) {
					continue;
				}
				if (cv == l_False) {
					int k;
					for (k = 0; k < j - 1; k++) {
						lbool ck = value(ps[k]);
						if (ck == l_False) {
							return ok = false;
						}
						if (ck == l_Undef) {
							uncheckedEnqueue(ps[k]);
						}
					}
					for (k = i + 1; k < ps.size(); k++) {
						lbool ck = value(ps[k]);
						if (ck == l_False) {
							return ok = false;
						}
						if (ck == l_Undef) {
							uncheckedEnqueue(ps[k]);
						}
					}
					return true;
				}
					 
				if(ps[i] == p){
                    uncheckedEnqueue(p);// no need to check if p is already set because it would have been caught earlier.
					assert(ps[j - 1] == p); 
                    j--;
                    
				} else if (ps[i] == ~ p) {
                    int k;
                    for (k = 0; k < j-1; k++) { // ps[j-1] is p, so we don't want to set that.
						lbool ck = value(ps[k]);
						if (ck == l_False) {
							return ok = false;
						}
						if(ck == l_Undef){
							uncheckedEnqueue(ps[k]);
						}                        
                    }
                    for (k = i+1; k < ps.size(); k++) {
						lbool ck = value(ps[k]);
						if (ck == l_False) {
							return ok = false;
						}
						if (ck == l_Undef) {
							uncheckedEnqueue(ps[k]);
						}
                    }
                    return true;
                }
				ps[j++] = p = ps[i];
			}
			ps.shrink(i - j);
		}
    }
    if (false && kind == SINGLESET) {
		cout << "addClause after shrink " << ps.size();
		for (int i=0; i<ps.size(); i++) {
			cout << " " << var(ps[i]);
		}
		cout << endl;
	}

    if (ps.size() == 0)
        return ok = false;
    else if (ps.size() == 1){
        assert(value(ps[0]) == l_Undef);		
        uncheckedEnqueue(ps[0]);
        return ok = (propagate() == NULL);
    }else{
        Clause* c = Clause::Clause_new(ps, false, &clauseStore);
        // bugfix: SINGLESET is only useful when ps.size()>2
        if (kind != SINGLESET || ps.size()>2) { c->mark(kind); }		
		clauses.push(c);		        
        attachClause(*c);
    }

    return true;
}

// int TOTSINGLESET=0;



SynthInSolver* Solver::addSynth(int inputs, int outputs, Synthesizer* s) {
  int idx = sins.size();
	SynthInSolver* syn = new SynthInSolver(s, inputs, outputs, idx, this);
	sins.push(syn);
  suggestions.push();
	return syn;
}



void Solver::addUfun(int funid, UfunSummary* ufs){
		allufuns.push(ufs);
		if(ufunByID.size() > funid){
			if(ufunByID[funid] != NULL){
				UfunSummary* fst = ufunByID[funid];
				UfunSummary* cur = fst;
				while(cur->next != fst){
					cur = cur->next;
				}
				cur->next = ufs;
				ufs->next = fst;
				ufunByID[funid] = ufs; // ufs will be ordered from biggest id to smallest id.
			}else{
				ufunByID[funid] = ufs;
				ufs->next = ufs;
			}
		}else{
			ufunByID.growTo(funid + 1 , NULL);
			ufunByID[funid] = ufs;
			ufs->next = ufs;
		}
		int sz = sizeof(UfunSpecialStruct) / sizeof(Lit);
		vec<Lit> pp(sz + 1, Lit(0));
		Clause* cc = Clause::Clause_new(pp, false, &clauseStore);
		cc->mark(SPECIALFUN);
		clauses.push(cc);
		UfunSpecialStruct* scs = (UfunSpecialStruct*) &((*cc)[0]);
		scs->kind = UFUNKIND;
		scs->uf = ufs;

		attachUFUNClause(cc);

	}


void Solver::attachUFUNClause(Clause* cc) {
	UfunSpecialStruct* scs = (UfunSpecialStruct*) &((*cc)[0]);
	UfunSummary* ufs = scs->uf;

	for (int i = 0; i<ufs->id; ++i) {
		watches[toInt(ufs->equivs[i])].push(cc);
	}
	OutSummary* ofs = ufs->output;
	for (int i = 0; i<ofs->nouts; ++i) {
		watches[toInt(ofs->lits[i])].push(cc);
		watches[toInt(~ofs->lits[i])].push(cc);
	}
 }



void Solver::attachClause(Clause& c) {
    assert(c.size() > 1);
	uint32_t mark = c.mark();


	if (mark == SPECIALFUN) {
		UfunSpecialStruct* scs = (UfunSpecialStruct*) &(c[0]);
		if (scs->kind == UFUNKIND) {
			attachUFUNClause(&c);
			return;
		}
		if (scs->kind == SYNKIND) {
			watches[toInt(scs->lit)].push(&c);
			return;
		}
		Assert(false, "NYI");
	}

	if(mark==INTSPECIAL){
		int ln = intcLen(c);
		for(int i=0; i<ln; ++i){
			Lit l = intcLit(c, i);
			watches[toInt(l)].push(&c);
		}
		return;
	}


    watches[toInt(~c[0])].push(&c);
    watches[toInt(~c[1])].push(&c);
	if(mark==SINGLESET){
		// TOTSINGLESET += c.size();
		// cout<<"RATIO = "<<TOTSINGLESET<<"/"<<this->nVars()<<"="<<(TOTSINGLESET/(double)this->nVars())<<endl;
		//singleset clauses are watched by all the literals except the last one.
		assert(c.size()>2);
		for(int i=2; i<c.size()-1; ++i){
			watches[toInt(~c[i])].push(&c);
		}
	}
    if (c.learnt()) learnts_literals += c.size();
    else            clauses_literals += c.size(); }


void Solver::detachClause(Clause& c) {
    assert(c.size() > 1);

	
	if(c.mark()==SINGLESET){
		// cout<<"REMOVING SINGLESET"<<endl;
		for(int ii=0; ii<c.size()-1; ++ii){
			remove(watches[toInt(~c[ii])], &c);
		}
		clauses_literals -= c.size();
		return;
	}

	assert(c.mark()==0);
    assert(find(watches[toInt(~c[0])], &c));
    assert(find(watches[toInt(~c[1])], &c));
    remove(watches[toInt(~c[0])], &c);
    remove(watches[toInt(~c[1])], &c);
    if (c.learnt()) learnts_literals -= c.size();
    else            clauses_literals -= c.size(); }


void Solver::removeClause(Clause& c) {
    detachClause(c);
	if (c.learnt()) {
		free(&c);
	}
}


bool Solver::satisfied(const Clause& c) {

	if(c.mark() == SPECIALFUN){
		return false;
	}	

	if (c.mark() == INTSPECIAL) {
		return false;
	}

	if(c.mark() == SINGLESET){ 
		for (int i = 0; i < c.size(); i++){
			lbool cv = value(c[i]);
			if(cv == l_Undef){
				return false;
			}			   
		}
		// cout<<"Removing SINGLESET"<<endl;
		return true; 
	}
	
    for (int i = 0; i < c.size(); i++)
        if (value(c[i]) == l_True)
            return true;
    return false; }


// Revert to the state at given level (keeping all assignment at 'level' but not beyond).
//
void Solver::cancelUntil(int level) {
    if (decisionLevel() > level){
        for (int c = trail.size()-1; c >= trail_lim[level]; c--){
            Var     x  = var(trail[c]);
            assigns[x] = toInt(l_Undef);
            insertVarOrder(x); }
        qhead = trail_lim[level];
        trail.shrink(trail.size() - trail_lim[level]);
        trail_lim.shrink(trail_lim.size() - level);
		for (int i = 0; i < sins.size(); ++i) {
			sins[i]->backtrack(level, suggestions[sins[i]->solverIdx]);
		}
    }	
    intsolve->cancelUntil(level);

}


//=================================================================================================
// Major methods:


Lit Solver::pickBranchLit(int polarity_mode, double random_var_freq)
{
    Var next = var_Undef;


	
    // Random decision:
    if (drand(random_seed) < random_var_freq && !order_heap.empty()){
        next = order_heap[irand(random_seed,order_heap.size())];
        if (toLbool(assigns[next]) == l_Undef && decision_var[next])
            rnd_decisions++; }

    // Activity based decision:
    while (next == var_Undef || toLbool(assigns[next]) != l_Undef || !decision_var[next])
        if (order_heap.empty()){
            next = var_Undef;
            break;
        }else
            next = order_heap.removeMin();

    bool sign = false;
    switch (polarity_mode){
    case polarity_true:  sign = false; break;
    case polarity_false: sign = true;  break;
	case polarity_user: sign = next >= 0? polarity[next] : true; break;
    case polarity_rnd:   sign = irand(random_seed, 2); break;
    default: assert(false); }

    return next == var_Undef ? lit_Undef : Lit(next, sign);
}



/*_________________________________________________________________________________________________
|
|  analyze : (confl : Clause*) (out_learnt : vec<Lit>&) (out_btlevel : int&)  ->  [void]
|  
|  Description:
|    Analyze conflict and produce a reason clause.
|  
|    Pre-conditions:
|      * 'out_learnt' is assumed to be cleared.
|      * Current decision level must be greater than root level.
|  
|    Post-conditions:
|      * 'out_learnt[0]' is the asserting literal at level 'out_btlevel'.
|  
|  Effect:
|    Will undo part of the trail, upto but not beyond the assumption of the current decision level.
|________________________________________________________________________________________________@*/
void Solver::analyze(Clause* confl, vec<Lit>& out_learnt, int& out_btlevel)
{
    int pathC = 0;
    Lit p     = lit_Undef;

    // Generate conflict clause:
    //
    out_learnt.push();      // (leave room for the asserting literal)
    int index   = trail.size() - 1;
    out_btlevel = 0;
	//set<int> awaiting;
	//set<int> visited;
    do{
        assert(confl != NULL);          // (otherwise should be UIP)
        Clause& c = *confl;		
		
		if(c.mark()==INTSPECIAL){	
			//cout << "INTSPEC: ";
			vec<Lit>& summary = intsolve->getSummary(p);					
			for(int i=0; i<summary.size(); ++i){
				Lit q = summary[i];
				int qlev = level[var(q)];				
				if (!seen[var(q)] && qlev > 0){
					varBumpActivity(var(q));
					seen[var(q)] = 1;
					if (qlev >= decisionLevel()) {
						//cout << "," << toInt(q);
						//awaiting.insert(toInt(q)/2);
						//if (visited.count(toInt(q) / 2) > 0) { 
						//	cout << "ERROR:A " << toInt(q) << "already in" << endl; 
						//}
						pathC++;
					}
					else {
						out_learnt.push(q);
						if (level[var(q)] > out_btlevel)
							out_btlevel = level[var(q)];
					}
				}
			}
			//cout << endl;
		}else{
			if (c.learnt())
				claBumpActivity(c);
			
			//cout << "NORMAL:";			
			//cout << "[";
			for (int j = (p == lit_Undef) ? 0 : 1; j < c.size(); j++){
				Lit q = c[j];

				if (!seen[var(q)] && level[var(q)] > 0){
					varBumpActivity(var(q));
					seen[var(q)] = 1;
					if (level[var(q)] >= decisionLevel()) {
						//cout << ", " << toInt(q);
						//awaiting.insert(toInt(q) / 2);
						//if (visited.count(toInt(q) / 2) > 0) { cout << "ERROR:B " << toInt(q) << "already in" << endl; }
						pathC++;
					}
					else {
						out_learnt.push(q);
						if (level[var(q)] > out_btlevel)
							out_btlevel = level[var(q)];
					}
				}
			}
		}
		//cout << "]" << endl;
        // Select next clause to look at:
        while (!seen[var(trail[index--])]);
        p     = trail[index+1];
		//cout << "NEXT:" << toInt(p) << endl;
		//if (awaiting.count(toInt(p) / 2) == 0) { cout << "ERROR:" << toInt(p) << " not in" << endl; }
		//awaiting.erase(toInt(p) / 2);
		//visited.insert(toInt(p) / 2);
        confl = reason[var(p)];
		//if (confl == NULL) {
		//	cout << "AWAITING: ";
		//	for (auto it = awaiting.begin(); it != awaiting.end(); ++it) {
		//		cout << ", " << ((*it)*2);
		//	}
		//	cout << endl;
		//	cout << "VISITED: ";
		//	for (auto it = visited.begin(); it != visited.end(); ++it) {
		//		cout << ", " << ((*it)*2);
		//	}
		//	cout << endl;
		//}
        seen[var(p)] = 0;
        pathC--;

    }while (pathC > 0);
    out_learnt[0] = ~p;

    // Simplify conflict clause:
    //
    int i, j;
    if (expensive_ccmin){
        uint32_t abstract_level = 0;
        for (i = 1; i < out_learnt.size(); i++)
            abstract_level |= abstractLevel(var(out_learnt[i])); // (maintain an abstraction of levels involved in conflict)

        out_learnt.copyTo(analyze_toclear);
        for (i = j = 1; i < out_learnt.size(); i++)
            if (reason[var(out_learnt[i])] == NULL || !litRedundant(out_learnt[i], abstract_level))
                out_learnt[j++] = out_learnt[i];
    }else{		
        out_learnt.copyTo(analyze_toclear);
        for (i = j = 1; i < out_learnt.size(); i++){
            Clause& c = *reason[var(out_learnt[i])];
			Assert( c.mark() != INTSPECIAL, "NOT HERE");
            for (int k = 1; k < c.size(); k++)
                if (!seen[var(c[k])] && level[var(c[k])] > 0){
                    out_learnt[j++] = out_learnt[i];
                    break; }
        }
    }
    max_literals += out_learnt.size();
    out_learnt.shrink(i - j);
    tot_literals += out_learnt.size();

    // Find correct backtrack level:
    //
    if (out_learnt.size() == 1)
        out_btlevel = 0;
    else{
        int max_i = 1;
        for (int i = 2; i < out_learnt.size(); i++)
            if (level[var(out_learnt[i])] > level[var(out_learnt[max_i])])
                max_i = i;
        Lit p             = out_learnt[max_i];
        out_learnt[max_i] = out_learnt[1];
        out_learnt[1]     = p;
        out_btlevel       = level[var(p)];
    }


    for (int j = 0; j < analyze_toclear.size(); j++) seen[var(analyze_toclear[j])] = 0;    // ('seen[]' is now cleared)
}


// Check if 'p' can be removed. 'abstract_levels' is used to abort early if the algorithm is
// visiting literals at levels that cannot be removed later.
bool Solver::litRedundant(Lit p, uint32_t abstract_levels)
{
    analyze_stack.clear(); analyze_stack.push(p);
    int top = analyze_toclear.size();
    while (analyze_stack.size() > 0){
        assert(reason[var(analyze_stack.last())] != NULL);
        Clause& c = *reason[var(analyze_stack.last())]; analyze_stack.pop();
		if (c.mark() == INTSPECIAL) {
			for (int j = top; j < analyze_toclear.size(); j++)
				seen[var(analyze_toclear[j])] = 0;
			analyze_toclear.shrink(analyze_toclear.size() - top);
			return false;
		}
        for (int i = 1; i < c.size(); i++){
            Lit p  = c[i];
            if (!seen[var(p)] && level[var(p)] > 0){
                if (reason[var(p)] != NULL && (abstractLevel(var(p)) & abstract_levels) != 0){
                    seen[var(p)] = 1;
                    analyze_stack.push(p);
                    analyze_toclear.push(p);
                }else{
                    for (int j = top; j < analyze_toclear.size(); j++)
                        seen[var(analyze_toclear[j])] = 0;
                    analyze_toclear.shrink(analyze_toclear.size() - top);
                    return false;
                }
            }
        }
    }

    return true;
}


/*_________________________________________________________________________________________________
|
|  analyzeFinal : (p : Lit)  ->  [void]
|  
|  Description:
|    Specialized analysis procedure to express the final conflict in terms of assumptions.
|    Calculates the (possibly empty) set of assumptions that led to the assignment of 'p', and
|    stores the result in 'out_conflict'.
|________________________________________________________________________________________________@*/
void Solver::analyzeFinal(Lit p, vec<Lit>& out_conflict)
{
    out_conflict.clear();
    out_conflict.push(p);

    if (decisionLevel() == 0)
        return;

    seen[var(p)] = 1;

    for (int i = trail.size()-1; i >= trail_lim[0]; i--){
        Var x = var(trail[i]);
        if (seen[x]){
            if (reason[x] == NULL){
                assert(level[x] > 0);
                out_conflict.push(~trail[i]);
            }else{
                Clause& c = *reason[x];
                for (int j = 1; j < c.size(); j++)
                    if (level[var(c[j])] > 0)
                        seen[var(c[j])] = 1;
            }
            seen[x] = 0;
        }
    }

    seen[var(p)] = 0;
}


void Solver::uncheckedEnqueue(Lit p, Clause* from)
{		
	// cout<<"           "<<(sign(p)?"-":" ")<<var(p)<<endl;
    assert(value(p) == l_Undef);
    assigns [var(p)] = toInt(lbool(!sign(p)));  // <<== abstract but not uttermost effecient
    level   [var(p)] = decisionLevel();
    reason  [var(p)] = from;
    trail.push(p);
}

vec<char> tc(sizeof(Clause) + sizeof(uint32_t)*(3));


Clause* Solver::newTempClause(vec<Lit>& po , Lit q, Clause**& ii, Clause**& jj, Clause**& end){
	vec<Lit> ps(po.size());
	for(int i=0; i<po.size(); ++i){
		ps[i] = po[i];
	}
	Lit fst = ps[0];
	int fstidx = -1;
	sort(ps);	
	Lit p; int i, j;
	for (i = j = 0, p = lit_Undef; i < ps.size(); i++){
		if (ps[i] != p){
			ps[j++] = p = ps[i];
			if(p==fst){
				fstidx = j-1;
			}
		}
	}
	ps.shrink(i - j);
	ps[fstidx] = ps[0];
	ps[0] = fst;
	if(ps[1]==~q){ // This is very important because we are currently watching on q. we will be adding to the 
		// negation of the first two locations in ps, so we want to avoid those locations being ~q.
		if(ps.size() > 2){
			ps[1] = ps[2];
			ps[2] = ~q;
		}else {
			// if ps.size() is not > 2, that's a problem, requires some complexity. 
			assert(ps.size() > 1);//this is impossible.
			//if ps.size()==2, then we need to add that clause to the current ws, but that means we may need to grow it.
			Clause* c = Clause::Clause_new(ps, true, NULL);
			vec<Clause*>& ws = watches[toInt(q)];
			int ipos = ii - ws.begin();
			int jpos = jj- ws.begin();
			attachClause(*c);
			ii = ws.begin() + ipos;
			jj = ws.begin() + jpos;
			end = ws.begin() + ws.size();
			learnts.push(c);	
			return c;
		}		
	}
	assert(fst != ~q);
	Clause* c = Clause::Clause_new(ps, true, NULL);
	attachClause(*c);
	learnts.push(c);
	return c;
}


void Solver::printUfunState(UfunSummary* afs){
	UfunSummary* ufs = afs;
	cout<<"-----------------------------------"<<endl;
	do{
		cout << ufs->id <<"{ [";

		for(int ii=0; ii<ufs->id; ++ii){
			cout << toInt(ufs->equivs[ii]) << "|" << (toInt(value(ufs->equivs[ii]))) << ", ";
		}
		cout << "]";
		OutSummary* os = ufs->output;
		for(int ii=0; ii<os->nouts; ++ii){		
			lbool v1 = value(os->lits[ii]);		
			cout<<"("<<toInt(os->lits[ii])<<"="<<ii<<"|"<< (toInt(value(os->lits[ii])))<<")";
		}
		ufs = ufs->next;
		cout<<endl;
	}while(ufs != afs);
}


Lit getEqLit(UfunSummary* other, UfunSummary* ufs) {
	if (other->id < ufs->id) {
		return ufs->equivs[other->id];
	}
	else {
		return other->equivs[ufs->id];
	}
}


bool Solver::backpropagateUfun(Lit p, UfunSummary* ufs, Clause& c, Clause**& i, Clause**& j, Clause**& end, Clause*& confl){	
	vec<int> setidx;
	OutSummary* os = ufs->output;
	for(int ii=0; ii<os->nouts; ++ii){		
		lbool v1 = value(os->lits[ii]);		
		if(v1==l_True){
			setidx.push(ii);
		}
	}
	if (setidx.size() == 0) {
		return true;
	}

	vec<Lit> plits(3);
	//If there is a conflict, it will be
	// (out1 ^ -out2) => -eqlit   ----> -out1 v out2 v -eqlit	
	UfunSummary* other = ufs->next;
	while(other != ufs){
		bool takeAction  = false;
		OutSummary* os2 = other->output;

		for(int ii=0; ii<setidx.size(); ++ii){		
			int vidx = setidx[ii];
			lbool v1 = value(os2->lits[vidx]);
			if(v1 == l_False){
				//ufs claims this output should be true; this claims the output should be false. That means their inputs can't be equal.
				takeAction = true;
				plits[1] = ~os->lits[vidx];
				plits[2] =  os2->lits[vidx];
				break;
			}			
		}
		if(takeAction){
			Lit eqlit = getEqLit(other, ufs);
			
			if (value(eqlit) == l_Undef) {
				plits[0] = ~eqlit; // this is not a bug.
				uncheckedEnqueue(~eqlit, newTempClause(plits, p, i, j, end));
			}
			else {
							
				if (value(eqlit) == l_True) {
					//We have a conflict. 	
					//cout << "!!!!!!!!!!!!!!!!!!BACKTRACK GOOD!!!!!!!!!!!!!!!!!!!" << endl;
					plits[0] = ~eqlit;
					int targetsize = sizeof(Clause) + sizeof(uint32_t)*plits.size();
					if (tc.size() < targetsize) {
						tc.growTo(targetsize);
					}
					Clause* cnew = new(&tc[0]) Clause(plits, true);
					*j++ = &c;
					while (i < end)
						*j++ = *i++;
					confl = cnew;
					qhead = trail.size();
					return false;
				}
				assert(value(eqlit) != l_True); 
			}
		}
		other = other->next;
	}


	return true;
}


vec<char> tempstore;

bool Solver::propagateUfun(Lit p, UfunSummary* ufs, Clause& c, Clause**& i, Clause**& j, Clause**& end, Clause*& confl){

				// printUfunState(ufs);
				vec<int> alleqs;
				vec<Lit> plits(3);				
				UfunSummary* pufun = NULL;

				//If we are here, it means all parameters are set. Now we need to check if other
				//instances of the same ufun also have all their parameters set.
				UfunSummary* other = ufs->next;
				while(other != ufs){					
					Lit eqlit;
					eqlit = getEqLit(other, ufs);
					
					if (value(eqlit) == l_True){
						if (eqlit == p) {
							pufun = other;
						}

						plits[2] = ~eqlit;
						alleqs.push(other->id);
						// other also has all its parameters set to the same thing as ufs.
						// This means that their outputs should match.
						OutSummary* osum1 = ufs->output;
						OutSummary* osum2 = other->output;
						for(int ii=0; ii<osum2->nouts; ++ii){
							lbool v1 = value(osum1->lits[ii]);
							lbool v2 = value(osum2->lits[ii]);
							if( v1 != v2){
								if(v1 == l_Undef){
									if(v2 == l_True){
										plits[1] = ~(osum2->lits[ii]);
										plits[0] = (osum1->lits[ii]);
																				
										uncheckedEnqueue(osum1->lits[ii], newTempClause( plits, p, i, j, end ) );
									}else{ // v2 == l_False;
										plits[1] = (osum2->lits[ii]);
										plits[0] = ~(osum1->lits[ii]);
										
										uncheckedEnqueue(~osum1->lits[ii], newTempClause( plits, p, i, j, end ) );
									}									
								}else if(v2 == l_Undef){
									if(v1 == l_True){
										plits[1] = ~(osum1->lits[ii]);
										plits[0] = (osum2->lits[ii]);
										
										uncheckedEnqueue(osum2->lits[ii], newTempClause( plits, p, i, j, end ) );
									}else{
										plits[1] = (osum1->lits[ii]);
										plits[0] = ~(osum2->lits[ii]);
										
										uncheckedEnqueue(~osum2->lits[ii], newTempClause( plits, p, i, j, end ));
									}														
								}else{
									//We have a conflict.
									if(v1 == l_True){
										plits[1] = ~(osum1->lits[ii]);
										plits[0] = (osum2->lits[ii]);
									}else{
										plits[1] = ~(osum2->lits[ii]);
										plits[0] = (osum1->lits[ii]);
									}
									int targetsize = sizeof(Clause) + sizeof(uint32_t)*plits.size();
									if(tc.size() < targetsize){
										tc.growTo(targetsize);
									}
									Clause* cnew = new(&tc[0]) Clause(plits, true);																		
									*j++ = &c;
									while (i < end)
											*j++ = *i++;
									confl = cnew;			
									qhead = trail.size();
									return true;
								}

							}							
						}						
					}
					other = other->next;
				}

				if (alleqs.size() > 1) {
					int sz = alleqs.size();
					UfunSummary* ufsni = ufs->next;					
					for (int ii = 0; ii < sz; ++ii) {
						while (ufsni->id != alleqs[ii]) { ufsni = ufsni->next;  }
						UfunSummary* ufsnj = ufsni->next;
						for (int jj = ii + 1; jj < sz; ++jj) {
							//ufs == ufs->next^(i+1)   and ufs == ufs->next^(j+1), so we need to set ufs->next^(i+1)==ufs->next^(j+1)
							while (ufsnj->id != alleqs[jj]) { ufsnj = ufsnj->next; }
							if (addTransEqClause(ufsnj, ufsni, p, ufs, plits, c, i, j, end, confl)) {
								return true;
							}
							ufsnj = ufsnj->next;
						}
						ufsni = ufsni->next;
					}
				}
				if (pufun != NULL){
					other = pufun->next;
					while (other != pufun) {
						if (other != ufs) {
							Lit loth = getEqLit(pufun, other);
							if (value(loth) == l_True) {
								if (addTransEqClause(other, ufs, p, pufun, plits, c, i, j, end, confl)) {
									return true;
								}
							}
						}
						other = other->next;
					}
				}



				if (backpropagateUfun(p, ufs, c, i, j, end, confl)) {
					*j++ = &c;
				}
				return true;
}

bool Solver::addTransEqClause(UfunSummary* ufsnj, UfunSummary* ufsni, Lit p, UfunSummary* ufs, vec<Lit>& plits, Clause& c, Clause**& i, Clause**& j, Clause**& end, Clause*& confl) {
	Lit ieqj = getEqLit(ufsnj, ufsni);
	lbool ieqjval = value(ieqj);
	if (ieqjval == l_Undef) {
		plits[2] = ~getEqLit(ufs, ufsni);
		plits[1] = ~getEqLit(ufs, ufsnj);
		plits[0] = ieqj;
		uncheckedEnqueue(ieqj, newTempClause(plits, p, i, j, end));
	}
	else if (ieqjval == l_False) {
		plits[2] = ~getEqLit(ufs, ufsni);
		plits[1] = ~getEqLit(ufs, ufsnj);
		plits[0] = ieqj;
		//Conflict!!!
		int targetsize = sizeof(Clause) + sizeof(uint32_t)*plits.size();
		if (tc.size() < targetsize) {
			tc.growTo(targetsize);
		}
		Clause* cnew = new(&tc[0]) Clause(plits, true);
		*j++ = &c;
		while (i < end)
			*j++ = *i++;
		confl = cnew;
		qhead = trail.size();
		return true;
	}
	return false;
}




int DEBUGCOUNT = 0;
/*_________________________________________________________________________________________________
|
|  propagate : [void]  ->  [Clause*]
|  
|  Description:
|    Propagates all enqueued facts. If a conflict arises, the conflicting clause is returned,
|    otherwise NULL.
|  
|    Post-conditions:
|      * the propagation queue is empty, even if there was a conflict.
|________________________________________________________________________________________________@*/

Clause* Solver::propagate()
{
    Clause* confl     = NULL;
    int     num_props = 0;
    volatile int avoidGccWeirdness=0;
	int dlevel = decisionLevel();

	bool isLevelZero = (dlevel==0);

    while (qhead < trail.size()){
        Lit            p   = trail[qhead++];     // 'p' is enqueued fact to propagate.
		// std::cout<<(sign(p)?var(p):-var(p))<<std::endl;
        vec<Clause*>&  ws  = watches[toInt(p)];
        Clause         **i, **j, **end;
        num_props++;
        //NOTE xzl: This type cast is rather too bold, it might cause trouble with moderner cc
        //for (i = j = (Clause**)ws, end = i + ws.size();  i != end;){
        for (i = j = ws.begin(), end = i + ws.size();  i != end;){
            Clause& c = **i++;
#ifdef _DEBUG
			++DEBUGCOUNT;
#endif
			uint32_t mrk = c.mark();
			Lit false_lit = ~p;			
			if (mrk == INTSPECIAL) {
				/*
				An INTSPECIAL clause corresponds to a sparse TVALUE. intCLen is the number of guarded
				values in the tvalue, and for each of them you can get the lit and the value. You can 
				also get the intid for the whole thing.
				*/
				int ln = intcLen(c);
				for (int ii = 0; ii<ln; ++ii) {
					Lit l = intcLit(c, ii);
					if (l == p) {// We first need to identify which guardedVal was set.
						int vr = intcIntVar(c);
						int val = intcVal(c, ii);
						int ilen = intsolve->interflen();
						bool goodsofar = intsolve->setVal(vr, val, decisionLevel()); //We set the variable to the corresponding value, making sure it is not already set.
						Intclause* iconf = NULL;
						//						cout<<"WHERE: "<<(num_props+propagations)<<endl;	
						//						intsolve->dump();
						//cout << "vr = " << vr << " val= " << val << endl;
						if (goodsofar) {
							iconf = intsolve->propagate();
							goodsofar = (iconf == NULL);
						}
						else {
							//trying to set vr to two different values (it already has one).
							if (intsolve->isSet(vr)) {
								Lit oth = intsolve->existingLit(vr);
								vec<Lit> ps;
								ps.push(~oth); ps.push(~p);
								tempstore.growTo(sizeof(Clause) + sizeof(uint32_t)*(ps.size()));
								confl = new (&tempstore[0]) Clause(ps, true);
								*j++ = &c;
								while (i < end)
									*j++ = *i++;

								qhead = trail.size();
								goto FoundWatch;
							} else {
								
								vec<Lit>& ps = intsolve->getSummary(vr, NULL, val, true);

								

								Assert(ps.size() > 0, "NOT BIG");
								Lit t = ps[0];
								ps.push(t);
								ps[0] = false_lit;
								tempstore.growTo(sizeof(Clause) + sizeof(uint32_t)*(ps.size()));
								confl = new (&tempstore[0]) Clause(ps, true);
								*j++ = &c;
								while (i < end)
									*j++ = *i++;

								qhead = trail.size();
								goto FoundWatch;
							}
							
						}

						if (goodsofar) {
							//In this case, the assignment did not cause any conflicts within the integer logic, but it forced
							//the values of some interface variables, so those values need to be propagated to the SAT solver.
							int nilen = intsolve->interflen();
							for (int jjj = ilen; jjj<nilen; ++jjj) {
								Lit ilit = intsolve->interfLit(jjj);
								lbool vvv = value(ilit);
								if (vvv == l_False) {
									// If the literal was already false, that's a problem, we need to get the conflict clause.
									vec<Lit>& ps = intsolve->getSummary(ilit);
									// in this case, getSummary returns the causes that led to ilit to have the bad value, but by themselves, they do not
									//constitute a bad assignment. However, those causes force ilit to be true. Also, by convention, if ilit is the problematic
									//variable, the solver expects it to be the first variable in the clause.
									Lit t = ps[0];
									ps.push(t);
									ps[0] = ilit;
									tempstore.growTo(sizeof(Clause) + sizeof(uint32_t)*(ps.size()));
									confl = new (&tempstore[0]) Clause(ps, true);
									*j++ = &c;
									while (i < end)
										*j++ = *i++;

									qhead = trail.size();
									goto FoundWatch;
								}
								if (vvv == l_Undef) {
									uncheckedEnqueue(ilit, &c);
								}
							}
							*j++ = &c;
							goto FoundWatch;
						}
						else {
							//If we are here, there was a conflict. 
							//if iconf is not null, then it means there was a contradiction inside intsolve, 
							//so the root causes for the contradiction constitute a bad assignment. 
							//on the other hand, if iconf is null, it means the value we are setting contradicts
							// something the solver already had for that value, but if that were the case, 
							//the solver would have told us? 

							Assert(iconf != NULL, "Maybe?");
							//vec<Lit> old;
							//intsolve->getSummaryA(vr, iconf).copyTo(old);
							vec<Lit>& ps = intsolve->getSummary(vr, iconf, 0, false);

							
							//intsolve->generateInnerConflict(iconf, decisionLevel(), ps.size() / 2);
							tempstore.growTo(sizeof(Clause) + sizeof(uint32_t)*(ps.size()));
							confl = new (&tempstore[0]) Clause(ps, true);
							*j++ = &c;
							while (i < end)
								*j++ = *i++;

							qhead = trail.size();
							goto FoundWatch;
						}
						goto FoundWatch;
					}
				}
				Assert(false, "should never reach here");

			}

			
			if (mrk == SPECIALFUN) {
				UfunSpecialStruct* syn = (UfunSpecialStruct*)&c[0];
				if (syn->kind == UFUNKIND) {
					UfunSummary* ufs = syn->uf;
					if (propagateUfun(p, ufs, c, i, j, end, confl)) {
						goto FoundWatch;
					}
				} else {
					// syn->kind == SYNKIND;
					confl = syn->s->pushInput(syn->instance, syn->inputid, syn->value, dlevel, suggestions[syn->s->solverIdx]);
					if (confl != NULL) {
						qhead = trail.size();
						// Copy the remaining watches:
						*j++ = &c;
						while (i < end)
							*j++ = *i++;
					} else {
						*j++ = &c;
					}
					goto FoundWatch;
				}
			}

			if(mrk == SINGLESET){				
				// std::cout<<" false_lit = "<<var(false_lit)<<std::endl;
				// std::cout<<" SINGLESET clause of size "<<c.size()<<std::endl;
				int last = c.size()-1;

				if(isLevelZero){
					for(int k=0; k<c.size(); ++k){
						if(c[k] == false_lit){
							// assert(value(c[k])==l_False);							
						}else{
							lbool cv = value(c[k]);
							if (cv == l_False){
								// A conflict in level zero. This means we are done.
								// Copy the remaining watches:
								while (i < end)
									*j++ = *i++;
								qhead = trail.size();		
								confl = &c;
								goto FoundWatch;
							}else
							if (cv == l_Undef){
								// cout<<"Eagerly setting litteral "<<endl;
								uncheckedEnqueue(c[k], &c);
							}
							//If it's true, then there is nothing to do.
						}
					}
					*j++ = &c;
					goto FoundWatch;
				}

				// At this point, the solver just set false_lit so if some other lit in the clause is also false, we have a conflict.
				for(int k=0; k<c.size(); ++k){
					// std::cout<<" c["<<k<<"] = "<<var(c[k])<<std::endl;
					if(c[k] == false_lit){
						if (false && k == last) {
							std::cout<<" false_lit = "<<var(false_lit)<<std::endl;
							std::cout<<" SINGLESET clause of size "<<c.size()<<std::endl;
							std::cout<<" c["<<k<<"] = "<<var(c[k])<<std::endl;
							std::cout<<" c["<<k-1<<"] = "<<var(c[k-1])<<std::endl;
							std::cout << "k,last= " << k << " " << last << endl;
						}
						assert (k != last);
						c[k] = c[0];
						c[0] = false_lit;
					}
					//If k is greater than the position of the original false_lit, the original false_lit is now at zero.
					if (value(c[k]) == l_False && c[k] != false_lit){
						//I know p is false; that's why I am here. 
						//If a c[k] different from p also became false, then we have violated the constraint.
						//If this happens after false_lit, then false_lit is now at zero. Otherwise, false_lit is ahead.
						//
						{
						Lit tt = c[k];
						c[k] = c[1];
						c[1] = tt;	
						}						
						//The violated entry has now been moved to position 1.
						if(c[0] != false_lit){
							//In case we hadn't reached false_lit, we search for false_lit and move it to pos 0.
							for(int tt = k+1; tt < c.size(); ++tt){
								if(c[tt] == false_lit){
									c[tt] = c[0];
									c[0] = false_lit;
									break;
								}
							}
						}
						//So at this point c[0] = false_lit and c[1] equals a violated lit.
						// std::cout<<"WIN "<<&c<<"  k= "<<k<<" var = "<<(sign(c[0])?"-":" ")<<var(c[0])<<std::endl;
						qhead = trail.size();		
						Fake* f = new (&c[0]) Fake();
						Clause* nc = new(&tc[0]) Clause(*f, true);
						// std::cout<<"     clause ["<<(sign(c[0])?"-":" ")<<var(c[0])<<", "<<(sign(c[1])?"-":" ")<<var(c[1])<<"]"<<std::endl;

						//*j is part of the watches for false_lit which is c[0].
						//XXX *j++ = nc;
						//XXX learnts.push(nc);
						//Important invariant: This singleset clause is in the watches of all its literals except for the last one.
						//XXX watches[toInt(~c[1])].push(nc);	
						{
							//c0 is swapped with c[last], so now false_lit is going to be the not-watched.
							Lit tt = c[last];
							c[last] = c[0];
							c[0] = tt;	
						}
						if(k!=last){
							//In this case, last is the unwatched literal, so we move it to pos 0,
							// and the old c[0] (now c[last]) is now the unwatched literal.
							
							//Instead of adding the clause to the current watch list, we swap the current with last and add it to the watch list of what used to be last.
							watches[toInt(~c[0])].push(&c);
						}else{
							//In this case, the old last got swapped with c[1] earlier, so c[1] is the unwatched literal.
							//so we need to swap c[0] with last, and watch c[1].
							
							watches[toInt(~c[1])].push(&c);
						}
						
						// Copy the remaining watches:
						while (i < end)
	                        *j++ = *i++;

						confl = nc;

						goto FoundWatch;
					}
				}
				
				{
					Lit tt = c[last];
					c[last] = c[0];
					c[0] = tt;	
				}
				
				//Instead of adding the clause to the current watch list, we swap the current with last and add it to the watch list of what used to be last.
				watches[toInt(~c[0])].push(&c);
				
				goto FoundWatch;
			}
						
            // Make sure the false literal is data[1]:
            
            if (c[0] == false_lit) {
                c[0] = c[1];
                c[1] = false_lit;
                //avoidGccWeirdness++; // Without this, a bug in some versions of gcc caused the code to break.
            }
#ifdef _DEBUG
            assert(c[1] == false_lit);
#endif
            // If 0th watch is true, then clause is already satisfied.
			{
				Lit first = c[0];
				const lbool valfirst = value(first);
				if (valfirst == l_True){
					*j++ = &c;
				} else {
					// Look for new watch:
					const int csize = c.size();
					for (int k = 2; k < csize; k++)
						if (value(c[k]) != l_False){
							c[1] = c[k]; c[k] = false_lit;
							watches[toInt(~c[1])].push(&c);
							goto FoundWatch; }

					// Did not find watch -- clause is unit under assignment:
					*j++ = &c;
					if (valfirst == l_False){
						confl = &c;
						qhead = trail.size();
						// Copy the remaining watches:
						while (i < end)
							*j++ = *i++;
					}else
						uncheckedEnqueue(first, &c);
				}
			}
        FoundWatch:;
        }
        ws.shrink(i - j);		
    }
    propagations += num_props;
    simpDB_props -= num_props;	
    return confl;
}

/*_________________________________________________________________________________________________
|
|  reduceDB : ()  ->  [void]
|  
|  Description:
|    Remove half of the learnt clauses, minus the clauses locked by the current assignment. Locked
|    clauses are clauses that are reason to some assignment. Binary clauses are never removed.
|________________________________________________________________________________________________@*/
struct reduceDB_lt { bool operator () (Clause* x, Clause* y) { return x->size() > 2 && (y->size() == 2 || x->activity() < y->activity()); } };
void Solver::reduceDB()
{
    int     i, j;
    double  extra_lim = cla_inc / learnts.size();    // Remove any clause below this activity

    sort(learnts, reduceDB_lt());
    for (i = j = 0; i < learnts.size() / 2; i++){
        if (learnts[i]->size() > 2 && !locked(*learnts[i]))
            removeClause(*learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    for (; i < learnts.size(); i++){
        if (learnts[i]->size() > 2 && !locked(*learnts[i]) && learnts[i]->activity() < extra_lim)
            removeClause(*learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    learnts.shrink(i - j);
	intsolve->cleanupConfs();
}


void Solver::removeSatisfied(vec<Clause*>& cs)
{
    int i,j;
    for (i = j = 0; i < cs.size(); i++){
        if (satisfied(*cs[i]))
            removeClause(*cs[i]);
        else
            cs[j++] = cs[i];
    }
    cs.shrink(i - j);
}


/*_________________________________________________________________________________________________
|
|  simplify : [void]  ->  [bool]
|  
|  Description:
|    Simplify the clause database according to the current top-level assigment. Currently, the only
|    thing done here is the removal of satisfied clauses, but more things can be put here.
|________________________________________________________________________________________________@*/
bool Solver::simplify()
{
    assert(decisionLevel() == 0);
	
	
    if (!ok || propagate() != NULL)
        return ok = false;

    if (nAssigns() == simpDB_assigns || (simpDB_props > 0))
        return true;

    // Remove satisfied clauses:
    removeSatisfied(learnts);
	removeSatisfied(binaryLearnts);
	if (remove_satisfied) {       // Can be turned off.
		auto bef = clauses.size();
		removeSatisfied(clauses);
		cout << " Removed " << (bef - clauses.size()) << " clauses"<< endl;
	}
    // Remove fixed variables from the variable heap:
    order_heap.filter(VarFilter(*this));

    simpDB_assigns = nAssigns();
    simpDB_props   = clauses_literals + learnts_literals;   // (shouldn't depend on stats really, but it will do for now)

    return true;
}


bool Solver::simplifyAndCompact() {
	assert(decisionLevel() == 0);


	if (!ok || propagate() != NULL)
		return ok = false;

	int tmpcl = clauses_literals;
	int tmpll = learnts_literals;

	if (nAssigns() == simpDB_assigns || (simpDB_props > 0))
		return true;

	for (size_t it = 0; it != watches.size(); ++it) {
		watches[it].fastclear();
	}

	auto regIfUnSatisfied = [&](vec<Clause*>& cs)
	{
		int i, j;
		for (i = j = 0; i < cs.size(); i++) {
			if (satisfied(*cs[i])) {
				free(cs[i]);
			} 
			else {
				attachClause(*cs[i]);
				cs[j++] = cs[i];
			}
				
		}
		cs.shrink(i - j);
	};
	regIfUnSatisfied(learnts);
	regIfUnSatisfied(binaryLearnts);

	

	{
		auto alloc = clauseStore.clearToAllocator();

		auto reallocate = [&](Clause* oldaddr) {
			auto sz = Clause::clauseFootprint(oldaddr->size());
			void* dest = (void*)clauseStore.newObj(sz, alloc.get());
			return new(dest) Clause(oldaddr);
		};
		
		size_t i, j;
		for (i = j = 0; i < clauses.size(); i++) {
			if (!satisfied(*clauses[i])) {
				Clause* oldaddr = clauses[i];
				Clause* newaddr = reallocate(oldaddr);				
				attachClause(*newaddr);
				clauses[j++] = newaddr;
			}

		}
		cout << " Removed " << (i-j) << " clauses" << endl;
		clauses.shrink(i - j);
	}
	// Remove fixed variables from the variable heap:
	order_heap.filter(VarFilter(*this));

	clauses_literals = tmpcl;
	learnts_literals = tmpll;

	simpDB_assigns = nAssigns();
	simpDB_props = clauses_literals + learnts_literals;   // (shouldn't depend on stats really, but it will do for now)

	return true;
}




/*_________________________________________________________________________________________________
|
|  search : (nof_conflicts : int) (nof_learnts : int) (params : const SearchParams&)  ->  [lbool]
|  
|  Description:
|    Search for a model the specified number of conflicts, keeping the number of learnt clauses
|    below the provided limit. NOTE! Use negative value for 'nof_conflicts' or 'nof_learnts' to
|    indicate infinity.
|  
|  Output:
|    'l_True' if a partial assigment that is consistent with respect to the clauseset is found. If
|    all variables are decision variables, this means that the clause set is satisfiable. 'l_False'
|    if the clause set is unsatisfiable. 'l_Undef' if the bound on number of conflicts is reached.
|________________________________________________________________________________________________@*/


lbool Solver::search(int nof_conflicts, int nof_learnts)
{
    assert(ok);
    int         backtrack_level;
    int         conflictC = 0;
    vec<Lit>    learnt_clause;

    starts++;

    bool first = true;

    for (;;){
        Clause* confl = propagate();
        if (confl != NULL){
			// cout<<" FCNT ="<<FCNT<<" SCNT = "<<SCNT<<" UCNT = "<<UCNT<<endl;
			// cout<<"CONFLICT"<<endl;
            // CONFLICT
			firstTry = false;
            conflicts++; conflictC++;
            if (decisionLevel() == 0) return l_False;

            first = false;

            learnt_clause.clear();
			
			
			
			analyze(confl, learnt_clause, backtrack_level);
			
			          
            cancelUntil(backtrack_level);
            assert(value(learnt_clause[0]) == l_Undef);

            if (learnt_clause.size() == 1){
                uncheckedEnqueue(learnt_clause[0]);
            }else{
                Clause* c = Clause::Clause_new(learnt_clause, true, NULL);
				if (c->size() > 2) {
					learnts.push(c);
				}
				else {
					binaryLearnts.push(c);
				}                
                attachClause(*c);
                claBumpActivity(*c);
                uncheckedEnqueue(learnt_clause[0], c);
            }

            varDecayActivity();
            claDecayActivity();

        }else{
            // NO CONFLICT

            if (nof_conflicts >= 0 && conflictC >= nof_conflicts){
                // Reached bound on number of conflicts:
                progress_estimate = progressEstimate();
                cancelUntil(0);
                return l_Undef; }

            // Simplify the set of problem clauses:
            if (decisionLevel() == 0 && !simplifyAndCompact())
                return l_False;
            

            Lit next = lit_Undef;
            while (decisionLevel() < assumptions.size()){
                // Perform user provided assumption:
                Lit p = assumptions[decisionLevel()];
                if (value(p) == l_True){
                    // Dummy decision level:
                    newDecisionLevel();
                }else if (value(p) == l_False){
                    analyzeFinal(~p, conflict);
                    return l_False;
                }else{
                    next = p;
                    break;
                }
            }
          
            // Use suggestions from custom solver before picking a random lit
            for (int i = 0; i < sins.size(); ++i) {
              if (next != lit_Undef) {
                break;
              }
              vec<Lit>& s = suggestions[sins[i]->solverIdx];
              while(s.size() > 0) {
                Lit p = s.last();
                s.pop();
                if (value(p) == l_Undef) {
                  next = p;
                  decisions++;
                  //cout << "Using suggested literal " << toInt(p) << endl;
                  break;
                }
              }
            }

            if (next == lit_Undef){

				if (nof_learnts >= 0 && learnts.size() - nAssigns() >= nof_learnts)
					// Reduce the set of learnt clauses:
					reduceDB();

                // New variable decision:
                decisions++;
                next = pickBranchLit(polarity_mode, random_var_freq);

                if (next == lit_Undef)
                    // Model found:
                    return l_True;
            }			
            // Increase decision level and enqueue 'next'
            assert(value(next) == l_Undef);
            newDecisionLevel();
			// cout<<(sign(next)?"-":" ")<<var(next)<<endl;
            uncheckedEnqueue(next);
        }
    }
}


void Solver::popCheckIfPossible(int lv) {	
	cancelUntil(lv);
}

bool Solver::checkIfPossible(Lit aa, int& outlv) {
	

	auto lightTry = [=](Lit a) {
		if (!ok) {
			return false;
		}
		lbool lv = value(a);
		//if it already has a value, we just check that it's compatible.
		if (lv == l_True) {
			return true;
		}
		if (lv == l_False) {
			return false;
		}
		newDecisionLevel();
		uncheckedEnqueue(a);
		Clause* confl = propagate();
		if (confl == NULL) {
			return true;
		}
		else {
			return false;
		}
	};

	int lv = this->decisionLevel();
	bool tmp = lightTry(aa);
	if (!tmp) {
		cancelUntil(lv);//go back to the level where we started;
		return false;
	}
	else {		
		outlv = lv;
		return true;
	}
}


bool Solver::tryAssignment(Lit a){
	if (!ok) {
		return false;
	}
	lbool lv = value(a);
	//if it already has a value, we just check that it's compatible.
	if(lv==l_True){
		return true;
	}
	if(lv==l_False){
		return false;
	}
	newDecisionLevel();
	uncheckedEnqueue(a);
	bool rv = true;
	while(true){
		Clause* confl = propagate();
		if(confl==NULL){
			return rv;
		}else{
			if (decisionLevel() == 0){
				ok = false;
				return false;
			}
			vec<Lit>    learnt_clause;
			int         backtrack_level;
			analyze(confl, learnt_clause, backtrack_level);
			cancelUntil(backtrack_level);
			if (learnt_clause.size() == 1){
				uncheckedEnqueue(learnt_clause[0]);
			}else{
				Clause* c = Clause::Clause_new(learnt_clause, true, NULL);
				if (c->size() > 2) {
					learnts.push(c);
				}
				else {
					binaryLearnts.push(c);
				}
				attachClause(*c);        
				uncheckedEnqueue(learnt_clause[0], c);				
			}
			rv = false;
		}
	}
}

bool Solver::assertIfPossible(Lit a){
	lbool lv = value(a);
	//if it already has a value, we just check that it's compatible.
	if(lv==l_True){
		return true;
	}
	if(lv==l_False){
		return false;
	}
	//if it doesn't have a value, try it out to see if it leads to a contradiction.
	assert(decisionLevel()==0); //this only works at decision level 0.
	newDecisionLevel();
	uncheckedEnqueue(a);
	Clause* confl = propagate();
	if(confl == NULL){
		// no conflict arises. it's ok to have this value, so we should make this be level 0 instead of level 1.
		for (int c = trail.size()-1; c >= trail_lim[0]; c--){
            Var     x  = var(trail[c]);
			level[x] = 0;            
		}	
		trail_lim.shrink(1);
		return true;
	}else{
		// conflict arises; this means a cannot be true. 
		cancelUntil(0);
		uncheckedEnqueue(~a);
		ok = (propagate()==NULL);
		return false;
	}
	return false;
}


double Solver::progressEstimate() const
{
    double  progress = 0;
    double  F = 1.0 / nVars();

    for (int i = 0; i <= decisionLevel(); i++){
        int beg = i == 0 ? 0 : trail_lim[i - 1];
        int end = i == decisionLevel() ? trail.size() : trail_lim[i];
        progress += pow(F, i) * (end - beg);
    }

    return progress / nVars();
}


lbool Solver::solve(const vec<Lit>& assumps)
{
    model.clear();
    conflict.clear();
	firstTry = true;
    if (!ok) return l_False;

    assumps.copyTo(assumptions);

    double  nof_conflicts = restart_first;
    double  nof_learnts   = max(nClauses() * learntsize_factor, 1000.0);
    lbool   status        = l_Undef;
	uint64_t decisionsStart = decisions;
	cout << "DECISIONS START = " << decisionsStart << endl;
    if (verbosity >= 1){
        printf("============================[ Search Statistics ]==============================\n");
        printf("| Conflicts |          ORIGINAL         |          LEARNT          | Progress |\n");
        printf("|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |\n");
        printf("===============================================================================\n");
    }
    // Search:
    while (status == l_Undef){
        if (verbosity >= 1)
            printf("| %9d | %7d %8d %8d | %8d %8d %6.0f | %6.3f %% |\n", (int)conflicts, order_heap.size(), nClauses(), (int)clauses_literals, (int)nof_learnts, nLearnts(), (double)learnts_literals/nLearnts(), progress_estimate*100), fflush(stdout);
        status = search((int)nof_conflicts, (int)nof_learnts);
        nof_conflicts *= restart_inc;
        nof_learnts   *= learntsize_inc;		
		if(incompletenessCutoff > 0 && (decisions-decisionsStart) > incompletenessCutoff){
			printf("WARNING: You are running with the -lightverif flag, so I am bailing\n");
			printf("out and assuming the problem is UNSAT even though I don't know for sure.\n");
			cancelUntil(0);
			return l_Undef;
		}
    }

	
    if (verbosity >= 1)
        printf("===============================================================================\n");


    if (status == l_True){
        // Extend & copy model:
        model.growTo(nVars());
		
		for (int i = 0; i < nVars(); i++) { 
			auto val = value(i);
			model[i] = val;
			polarity[i] = (char)(val == l_False);
		}		
		polarity_mode = polarity_user;
#ifdef _DEBUG
        verifyModel();
#endif
    }else{
        assert(status == l_False);
        if (conflict.size() == 0)
            ok = false;
    }

	for (int i = 0; i < sins.size(); ++i) {
		sins[i]->finalize();
	}

    cancelUntil(0);
    return status;
}

//=================================================================================================
// Debug methods:


void Solver::verifyModel()
{
    bool failed = false;

	
    for (int i = 0; i < clauses.size(); i++){
		if(clauses[i]->mark()==0){
			Clause& c = *clauses[i];
			for (int j = 0; j < c.size(); j++)
				if (modelValue(c[j]) == l_True)
					goto next;

			printf("unsatisfied clause: ");
			printClause(*clauses[i]);
			printf("\n");
			failed = true;
		}
		if(clauses[i]->mark()==SINGLESET){
			Clause& c = *clauses[i];
			int cnt = 0;
			for (int j = 0; j < c.size(); j++)
				if (modelValue(c[j]) == l_False)
					++cnt;
			if(cnt > 2){
				printf("unsatisfied clause: ");
				printClause(*clauses[i]);
				printf("\n");
				failed = true;
			}
		}
    next:;
    }

    assert(!failed);

    printf("Verified %d original clauses.\n", clauses.size());
}


void Solver::checkLiteralCount()
{
    // Check that sizes are calculated correctly:
    int cnt = 0;
    for (int i = 0; i < clauses.size(); i++)
        if (clauses[i]->mark() == 0)
            cnt += clauses[i]->size();

    if ((int)clauses_literals != cnt){
        fprintf(stderr, "literal count: %d, real value = %d\n", (int)clauses_literals, cnt);
        assert((int)clauses_literals == cnt);
    }
}


void Solver::writeDIMACS(ofstream& dimacs_file)
{	
    //ofstream dimacs_file(filename);
    if (dimacs_file.is_open()) {
		int clausecount = 0;
		 for (int i = 0; i < clauses.size(); i++) {
            Clause* c = clauses[i];
			if(c->mark() == SINGLESET){
				int n = c->size();
				clausecount += ((n*n)-n)/2;
			}else{
				clausecount++;
			}
		 }
		dimacs_file<<"c "<<clauses.size()<<" original clauses expand to "<<clausecount<<" real clauses plus "<<trail.size()<<" unary clauses"<<endl;
		 clausecount += trail.size();		 
        dimacs_file << "p cnf " << nVars() << " " << clausecount << "\n";
		for(int i=0; i<trail.size(); ++i){			 
			 const char* neg = sign(trail[i]) ? "-" : "";
			 dimacs_file << neg << var(trail[i]) + 1 << " 0"<<endl;
		 }
        for (int i = 0; i < clauses.size(); i++) {
            Clause* c = clauses[i];
			if(c->mark() == SINGLESET){
				dimacs_file<<"c START SINGLESET CLAUSE"<<endl;
				for(int i=0; i<c->size(); ++i){
					for(int j=i+1; j<c->size(); ++j){
						const char* negi = sign((*c)[i]) ? "-" : "";
						const char* negj = sign((*c)[j]) ? "-" : "";
						dimacs_file << negi << var((*c)[i]) + 1 << " ";
						dimacs_file << negj << var((*c)[j]) + 1 << " ";
						dimacs_file << "0\n";
					}
				}
				dimacs_file<<"c END SINGLESET CLAUSE"<<endl;
				continue;
			}
            for (int j = 0; j < c->size(); j++) {
                const char* neg = sign((*c)[j]) ? "-" : "";
                dimacs_file << neg << var((*c)[j]) + 1 << " ";
            }
            dimacs_file << "0\n";
        }
		dimacs_file.flush();
        dimacs_file.close();
    } else {
        cout << "Unable to open file";
    }
}


void Solver::getShareable(set<int>& single, set<pair<int, int> >& dble, set<pair<int, int> >& baseline){
	for(int i=0; i<trail.size(); ++i){
		single.insert(toInt(trail[i]));
	}
	for(int i=0; i<clauses.size(); ++i){		
		Clause* c = clauses[i];
		if(c->size() == 2){
			int x = toInt((*c)[0]);
			int y = toInt((*c)[1]);
			pair<int, int> p = make_pair( min(x,y) , max(x,y) );
			if(baseline.count(p)==0){
				dble.insert(p);
			}
		}
	}
}


void Solver::dump(){
	int tl = 0;
	cout<<"TRAIL: ";
	for(int i=0; i<trail.size(); ++i){
		if(tl > trail_lim.size()){
			if(trail_lim[tl]==i){
				tl++;
				cout<<" || ";
			}
		}
		cout<<", "<<toInt(trail[i]);		
	}
	cout<<endl;
	if(ok){
		cout<<"ALLOK"<<endl;
	}else{
		cout<<"NOTOK"<<endl;
	}
}



}
