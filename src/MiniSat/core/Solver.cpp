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

using namespace std;

namespace MSsolverNS{


//=================================================================================================
// Constructor/Destructor:

uint32_t SINGLESET = 3;
uint32_t LAZYOR = 1;

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
{}


Solver::~Solver()
{
    for (int i = 0; i < learnts.size(); i++) free(learnts[i]);
    for (int i = 0; i < clauses.size(); i++) free(clauses[i]);
	for (int i = 0; i < lazyors.size(); i++) free(lazyors[i]);
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
			Clause* c = Clause::Clause_new(tv, false);
			 wi.push(c);
			 wj.push(c);
			 clauses_literals += c->size(); 
		}
	}
	return true;
}


bool Solver::addClause(vec<Lit>& ps, uint32_t kind)
{
    assert(decisionLevel() == 0);

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
		if(kind != LAZYOR && kind != SINGLESET){
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
				if(ps[i] == p){
                    uncheckedEnqueue(p);
                    if (ps[j-1] == p) {
                        j--;
                    }
				} else if (ps[i] == ~ p) {
                    int k;
                    for (k = 0; k < j; k++) {
                        uncheckedEnqueue(ps[k]);
                    }
                    for (k = i+1; k < ps.size(); k++) {
                        uncheckedEnqueue(ps[k]);
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
        Clause* c = Clause::Clause_new(ps, false);
        // bugfix: SINGLESET is only useful when ps.size()>2
        if (kind != SINGLESET || ps.size()>2) { c->mark(kind); }
		if(kind == LAZYOR){
			lazyors.push(c);
		}else{
			clauses.push(c);
		}        
        attachClause(*c);
    }

    return true;
}

// int TOTSINGLESET=0;

void Solver::attachClause(Clause& c) {
    assert(c.size() > 1);
	uint32_t mark = c.mark();
	if(mark==LAZYOR){
		/*
		Lazyor clause has the following structure 
		c[0] = c[1] or c[2] c[3] ...c[n-1]
		So we want the following rules for i>0
		c[i] => if(c[0] == undef){ c[0] = true }
		        if(c[0] == false){ conflict (c[0], -c[i]) }
		
		*/		
		assert(c.size()>2);
		for(int i=1; i<c.size(); ++i){
			watches[toInt(c[i])].push(&c);
		}
		clauses_literals += c.size();
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

	if(c.mark() == LAZYOR){
		// cout<<"REMOVING LAZYOR"<<endl;
		for(int ii=1; ii<c.size(); ++ii){
			maybeRemove(watches[toInt(c[ii])], &c);
		}

		clauses_literals -= c.size();
		return;
	}
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
    free(&c); }


bool Solver::satisfied(const Clause& c) {
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
	if(c.mark()== LAZYOR){
		if(value(c[0])==l_True){
			return true;
		}
		if(value(c[0])==l_False){
			for (int i = 0; i < c.size(); i++){
				lbool cv = value(c[i]);
				if(cv == l_Undef){
					uncheckedEnqueue(~c[i], NULL);
					// cout<<"Propagating from constant Lazyor"<<endl;
				}	
				if(cv == l_True){
					return false; 
					//We just discovered an inconsistency
					//It probably just arose from the unchecked enqueue of some other 
					//lazyor clause. We leave this clause in place so that
					//the later propagate will discover the problem.
				}
				//if cv == l_False already, there is nothing to do.
			}
			return true;			
		}
		return false;
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
    } }


//=================================================================================================
// Major methods:


Lit Solver::pickBranchLit(int polarity_mode, double random_var_freq)
{
    Var next = var_Undef;

	if(false && /*firstTry &&*/ inputvars.size() > 0){
		next = inputvars.last(); 
		inputvars.pop();
		while(toLbool(assigns[next]) != l_Undef && inputvars.size() > 0){
			next = inputvars.last(); 
			inputvars.pop();
		}
		if(toLbool(assigns[next]) == l_Undef){
			return Lit(next, irand(random_seed, 2));
		}
	}

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
    case polarity_user:  sign = polarity[next]; break;
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

    do{
        assert(confl != NULL);          // (otherwise should be UIP)
        Clause& c = *confl;

        if (c.learnt())
            claBumpActivity(c);

        for (int j = (p == lit_Undef) ? 0 : 1; j < c.size(); j++){
            Lit q = c[j];

            if (!seen[var(q)] && level[var(q)] > 0){
                varBumpActivity(var(q));
                seen[var(q)] = 1;
                if (level[var(q)] >= decisionLevel())
                    pathC++;
                else{
                    out_learnt.push(q);
                    if (level[var(q)] > out_btlevel)
                        out_btlevel = level[var(q)];
                }
            }
        }

        // Select next clause to look at:
        while (!seen[var(trail[index--])]);
        p     = trail[index+1];
        confl = reason[var(p)];
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

	bool isLevelZero = (decisionLevel()==0);

    while (qhead < trail.size()){
        Lit            p   = trail[qhead++];     // 'p' is enqueued fact to propagate.
		// std::cout<<(sign(p)?var(p):-var(p))<<std::endl;
        vec<Clause*>&  ws  = watches[toInt(p)];
        Clause         **i, **j, **end;
        num_props++;
		
        //NOTE xzl: This type cast is rather too bold, it might cause trouble with moderner cc
        //for (i = j = (Clause**)ws, end = i + ws.size();  i != end;){
        for (i = j = &ws[0], end = i + ws.size();  i != end;){
            Clause& c = **i++;

			Lit false_lit = ~p;


			if(c.mark() == LAZYOR){
				/*
				Lazyor clause has the following structure 
				c[0] = c[1] or c[2] c[3] ...c[n-1]
				So we want the following rules for i>0
				c[i] => if(c[0] == true){ nothing to do }
						if(c[0] == undef){ c[0] = true }
						if(c[0] == false){ conflict (c[0], -c[i]) }
		
				*/	
				/*
				std::cout<<"LAZYOR "<<(sign(p)?"-":" ")<<var(p)<<": ";
				for(int ii=0; ii<c.size(); ++ii){
					std::cout<<" "<<(sign(c[ii])?"-":" ")<<var(c[ii])<<", ";
				}
				std::cout<<endl;*/

				Lit first = c[0];
				if (value(first) == l_True){
					// std::cout<<"SKIP"<<endl;
					*j++ = &c;
					goto FoundWatch;
				}
				if (value(first) == l_False){
					// std::cout<<"CONFL"<<endl;
					int ttf[2];
					
					Fake* f = new (ttf) Fake();
					(*f)[0] = first;
					(*f)[1] = ~p;					
					Clause* nc = Clause::Clause_new(*(f), true);
					*j++ = nc;
					// We add nc to the watches list, but that means it is no longer
					// necessary to add c, since it is redundant with this new clause.
					watches[toInt(~first)].push(nc);	
					//Since we are no longer watching this on the LAZYOR clause, 
					//we need to add this as a real clause.
					clauses.push(nc);
					while (i < end)
	                        *j++ = *i++;
					confl = nc;			
					qhead = trail.size();
					goto FoundWatch;
				}else{
					// std::cout<<"ADD"<<endl;
					int ttf[2];
					Fake* f = new (ttf) Fake();
					(*f)[0] = first;
					(*f)[1] = ~p;					
					Clause* nc = Clause::Clause_new(*(f), true);
					watches[toInt(~first)].push(nc);
					clauses.push(nc);
					*j++ = nc;
					// We add nc to the watches list, but that means it is no longer
					// necessary to add c, since it is redundant with this new clause.
					uncheckedEnqueue(first, nc);
					goto FoundWatch;
				}
			}


			if(c.mark() == SINGLESET){
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
					if (value(c[k]) == l_False && c[k] != false_lit){
						//I know p is false; that's why I am here. 
						//If a c[k] different from p also became false, then we have violated the constraint.
						//
						{
						Lit tt = c[k];
						c[k] = c[1];
						c[1] = tt;	
						}

						if(c[0] != false_lit){
							for(int tt = k+1; tt < c.size(); ++tt){
								if(c[tt] == false_lit){
									c[tt] = c[0];
									c[0] = false_lit;
									break;
								}
							}
						}
						// std::cout<<"WIN "<<&c<<"  k= "<<k<<" var = "<<(sign(c[0])?"-":" ")<<var(c[0])<<std::endl;
						qhead = trail.size();		
						Fake* f = new (&c[0]) Fake();
						Clause* nc = Clause::Clause_new(*(f), true);
						// std::cout<<"     clause ["<<(sign(c[0])?"-":" ")<<var(c[0])<<", "<<(sign(c[1])?"-":" ")<<var(c[1])<<"]"<<std::endl;
						*j++ = nc;
						learnts.push(nc);
						//Important invariant: This singleset clause is in the watches of all its literals except for the last one.
						watches[toInt(~c[1])].push(nc);	
						{
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
                avoidGccWeirdness++; // Without this, a bug in some versions of gcc caused the code to break.
            }

            assert(c[1] == false_lit);

            // If 0th watch is true, then clause is already satisfied.
			{
            Lit first = c[0];
				if (value(first) == l_True){
					*j++ = &c;
				} else {
					// Look for new watch:
					for (int k = 2; k < c.size(); k++)
						if (value(c[k]) != l_False){
							c[1] = c[k]; c[k] = false_lit;
							watches[toInt(~c[1])].push(&c);
							goto FoundWatch; }

					// Did not find watch -- clause is unit under assignment:
					*j++ = &c;
					if (value(first) == l_False){
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
	
	removeSatisfied(lazyors);
    if (!ok || propagate() != NULL)
        return ok = false;

    if (nAssigns() == simpDB_assigns || (simpDB_props > 0))
        return true;

    // Remove satisfied clauses:
    removeSatisfied(learnts);
    if (remove_satisfied)        // Can be turned off.
        removeSatisfied(clauses);

    // Remove fixed variables from the variable heap:
    order_heap.filter(VarFilter(*this));

    simpDB_assigns = nAssigns();
    simpDB_props   = clauses_literals + learnts_literals;   // (shouldn't depend on stats really, but it will do for now)

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
                Clause* c = Clause::Clause_new(learnt_clause, true);
                learnts.push(c);
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
            if (decisionLevel() == 0 && !simplify())
                return l_False;

            if (nof_learnts >= 0 && learnts.size()-nAssigns() >= nof_learnts)
                // Reduce the set of learnt clauses:
                reduceDB();

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

            if (next == lit_Undef){
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


bool Solver::tryAssignment(Lit a){
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
				Clause* c = Clause::Clause_new(learnt_clause, true);
				learnts.push(c);
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
    double  nof_learnts   = nClauses() * learntsize_factor;
    lbool   status        = l_Undef;

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
		if(incompletenessCutoff > 0 && decisions > incompletenessCutoff){
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
        for (int i = 0; i < nVars(); i++) model[i] = value(i);
#ifdef _DEBUG
        verifyModel();
#endif
    }else{
        assert(status == l_False);
        if (conflict.size() == 0)
            ok = false;
    }

    cancelUntil(0);
    return status;
}

//=================================================================================================
// Debug methods:


void Solver::verifyModel()
{
    bool failed = false;

	for(int i=0; i<lazyors.size(); ++i){
		Clause& c = *lazyors[i];
		for (int j = 1; j < c.size(); j++){
				if (modelValue(c[j]) == l_True && modelValue(c[0]) == l_True)
					goto nextB;
				if (modelValue(c[j]) == l_True && modelValue(c[0]) == l_False){
					printf("unsatisfied LAZYOR clause: ");
					printClause(*lazyors[i]);
					printf("\n");
					failed = true;
					goto nextB;
				}					
		}
		if(modelValue(c[0]) == l_True){
			//All the operands were false, but it was true;
			//This does not violate the rules of the clause itself, but it is bad anyway.
			//This is because every lazyor clause should be paired with a normal clause
			//that enforces that if all c[i] are false, c[0] is false.
			printf("badness of LAZYOR clause: ");
					printClause(*lazyors[i]);
					printf("\n");
					failed = true;
		}
		nextB:;
	}


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

    printf("Verified %d original clauses and %d lazyor clauses.\n", clauses.size(), lazyors.size());
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
