/**************************************************************************************************

Solver.C -- (C) Niklas Een, Niklas S�rensson, 2004

A simple Chaff-like SAT-solver with support for incremental SAT.

**************************************************************************************************/
#include <cmath>

#include <iostream>

#include "MSolver.h"
#include "Sort.h"


namespace MSsolverNS{
//=================================================================================================
// Debug:


// For derivation output (verbosity level 2)
#define L_IND    "%-*d"
#define L_ind    decisionLevel()*3+3,decisionLevel()
#define L_LIT    "%sx%d"
#define L_lit(p) sign(p)?"~":"", var(p)

// Just like 'assert()' but expression will be evaluated in the release version as well.
inline void check(bool expr) { assert(expr); }



void Solver::printSmallLearnts(){
	for(int i=0; i< learnts.size(); ++i){
		Clause* cc = learnts[i];

		if(cc->size() < 3){
			std::cout<<cc->size();
			for(int t=0; t<cc->size(); ++t){
				std::cout<<(sign((*cc)[t])?"-":"+")<< var((*cc)[t])  <<", ";
			}
			std::cout<<std::endl;
		}
	}
}







//=================================================================================================
// Helpers:


void swapWatch(vec<LitClauseUnion>& ws, LitClauseUnion elem, LitClauseUnion newelem)
{
    int j = 0;
    for (; ws[j] != elem  ; j++) assert(j < ws.size());
	if(j < ws.size()){ ws[j] = newelem; }    
}

void removeWatch(vec<LitClauseUnion>& ws, LitClauseUnion elem)
{
    int j = 0;
    for (; ws[j] != elem  ; j++) assert(j < ws.size());
    for (; j < ws.size()-1; j++) ws[j] = ws[j+1];
    ws.pop();
}
void Solver::printKnownAssigns(){
	for(int i=0; i<assigns.size(); ++i){
		if(toLbool(assigns[i]) != l_Undef){
			std::cout<<"var "<<i<<"="<<(toLbool(assigns[i])==l_True? 't' : 'f')<<std::endl;
		}
	}
}

//=================================================================================================
// Operations on clauses:

// Returns FALSE if top-level conflict detected (must be handled); TRUE otherwise.
// 'out_clause' may be set to NULL if clause is already satisfied by the top-level assignment.
//
bool Solver::newClause(const vec<Lit>& ps_, bool learnt, Clause*& out_clause)
{
    vec<Lit>    qs;
    if (&out_clause != NULL) out_clause = NULL;

    if (!learnt){
        assert(decisionLevel() == 0);
        ps_.copyTo(qs);             // Make a copy of the input vector.

        // Remove false literals:
        for (int i = 0; i < qs.size();){
            if (value(qs[i]) != l_Undef){
                if (value(qs[i]) == l_True)
                    return true;    // Clause always true -- don't add anything.
                else
                    qs[i] = qs.last(),
                    qs.pop();
            }else
                i++;
        }

        // Remove duplicates:
        sortUnique(qs);
        for (int i = 0; i < qs.size()-1; i++){
            if (qs[i] == ~qs[i+1])
                return true;        // Clause always true -- don't add anything.
        }
    }
    const vec<Lit>& ps = learnt ? ps_ : qs;     // 'ps' is now the (possibly) reduced vector of literals.

    if (ps.size() == 0)
        return false;
    else if (ps.size() == 1)
        return enqueue(ps[0]);
    else{
        // Allocate clause:
        assert(sizeof(Lit)   == sizeof(unsigned));
        assert(sizeof(float) == sizeof(unsigned));
        void*   mem = xmalloc<char>(sizeof(Clause) + sizeof(unsigned)*(ps.size() + (int)learnt));
        Clause* c   = new (mem) Clause(learnt,ps);

        // For learnt clauses only:
        if (learnt){
            // Put the second watch on the literal with highest decision level:
            int     max_i = 1;
            int     max   = level[var(ps[1])];
            for (int i = 2; i < ps.size(); i++)
                if (level[var(ps[i])] > max)
                    max   = level[var(ps[i])],
                    max_i = i;
            (*c)[1]     = ps[max_i];
            (*c)[max_i] = ps[1];

            // Bumping:
            claBumpActivity(c); // (newly learnt clauses should be considered active)
            for (int i = 0; i < ps.size(); i++)
                varBumpActivity(ps[i]);

            stats.learnts++;
            stats.learnts_literals += c->size();
        }else{
            stats.clauses++;
            stats.clauses_literals += c->size();
        }

        // Store clause:
        if (c->size() == 2){
            watches[index(~(*c)[0])].push(makeLit((*c)[1]));
            watches[index(~(*c)[1])].push(makeLit((*c)[0]));
        }else{
            watches[index(~(*c)[0])].push(makeClause(c));
            watches[index(~(*c)[1])].push(makeClause(c));
        }
        if (&out_clause != NULL) out_clause = c;

        return true;
    }
}


void Solver::remove(Clause* c, bool just_dealloc)
{
    if (!just_dealloc)
        if (c->size() == 2){
            removeWatch(watches[index(~(*c)[0])], makeLit((*c)[1]));
            removeWatch(watches[index(~(*c)[1])], makeLit((*c)[0]));
        }else{
            removeWatch(watches[index(~(*c)[0])], makeClause(c));
            removeWatch(watches[index(~(*c)[1])], makeClause(c));
        }

    if (c->learnt()){
        stats.learnts--;
        stats.learnts_literals -= c->size();
    }else{
        stats.clauses--;
        stats.clauses_literals -= c->size();
    }

    xfree(c);
}


// Can assume everything has been propagated! (esp. the first two literals are != l_False, unless
// the clause is binary and satisfied, in which case the first literal is true)
// Returns True if clause is satisfied (will be removed), False otherwise.
//
bool Solver::simplify(Clause* c) 
{
    assert(decisionLevel() == 0);
	int oldsz = c->size();
	Lit* ll = &(*c)[0];
	Lit* end = ll + c->size();
	Lit c0 = ~(*c)[0];
	Lit c1 = ~(*c)[1];
	for(Lit* tt = ll ; tt < end; ){
		if (value(*tt) == l_True)
			return true;
		if (value(*tt) != l_False){			
			*ll = *tt;
			++ll;
		}
		++tt;
	}
    int sz = ll - &(*c)[0];
	assert(sz >= 2);
	if(sz != oldsz){
		c->resize(sz);
		if(sz == 2 && oldsz != 2){
			swapWatch(watches[index(c0)], makeClause(c), makeLit((*c)[1]));
			swapWatch(watches[index(c1)], makeClause(c), makeLit((*c)[0]));

		}
	}
	return false;
}



//=================================================================================================
// Minor methods:


// Creates a new SAT variable in the solver. If 'decision_var' is cleared, variable will not be
// used as a decision variable (NOTE! This has effects on the meaning of a SATISFIABLE result).
//
Var Solver::newVar(void)
{
    int     index;
    index = nVars();
    watches .push();          // (list for positive literal)
    watches .push();          // (list for negative literal)
    reason  .push(makeClause(NULL));
    assigns .push(toInt(l_Undef));
	lastassigns .push(toInt(l_Undef));
    level   .push(-1);
    activity.push(0);
    order   .newVar();
    analyze_seen.push(0);
    trail.capacity(index+1);
    return index;
}


// Returns FALSE if immediate conflict.
bool Solver::assume(Lit p) {
    //if (verbosity >= 2)
    //printf(L_IND"assume("L_LIT") %g\n", L_ind, L_lit(p), activity[var(p)]);
    trail_lim.push(trail.size());
    return enqueue(p); }


// Revert to the state at given level.
//
void Solver::cancelUntil(int level) {
    if (decisionLevel() == 0)
        return;

    for (int c = trail.size()-1; c >= trail_lim[level]; c--) {
        Var     x  = var(trail[c]);
        assigns[x] = toInt(l_Undef);
        reason [x] = makeClause(NULL);
    }

    for (int c = trail.size()-1; c >= trail_lim[level]; c--)
        order.undo(var(trail[c]));

    trail.shrink_(trail.size() - trail_lim[level]);
    trail_lim.shrink_(trail_lim.size() - level);

    qhead = trail.size();
}

// Record a clause and drive backtracking. 'clause[0]' must contain the asserting literal.
//
void Solver::record(const vec<Lit>& clause)
{
    assert(clause.size() != 0);
    Clause* c;
    check(newClause(clause, true, c));
    check(ok);
    check(enqueue(clause[0], makeClause(c)));
    if (c != NULL) learnts.push(c);
}


//=================================================================================================
// Major methods:


#define ANALYZE_LIT(p) \
            if (!seen[var(q)] && level[var(q)] > 0){                \
                varBumpActivity(q);                                 \
                seen[var(q)] = 1;                                   \
                if (level[var(q)] == decisionLevel())               \
                    pathC++;                                        \
                else{                                               \
                    out_learnt.push(q);                             \
                    out_btlevel = max(out_btlevel, level[var(q)]);  \
                }                                                   \
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
void Solver::analyze(Clause* _confl, vec<Lit>& out_learnt, int& out_btlevel)
{
    LitClauseUnion confl = makeClause(_confl);
    vec<char>&     seen  = analyze_seen;
    int            pathC = 0;
    Lit            p     = lit_Undef;

    // Generate conflict clause:
    //
    out_learnt.push();      // (leave room for the asserting literal)
    out_btlevel = 0;
    int index = trail.size()-1;
    do{
        assert(confl != makeClause(NULL));          // (otherwise should be UIP)

        if (confl.isLit()){
            Lit q = confl.getLit();
            //ANALYZE_LIT(p);
			if (!seen[var(q)] && level[var(q)] > 0){                
                varBumpActivity(q);                                 
                seen[var(q)] = 1;                                   
                if (level[var(q)] == decisionLevel())               
                    pathC++;                                        
                else{                                               
                    out_learnt.push(q);                             
                    out_btlevel = max(out_btlevel, level[var(q)]);  
                }                                                   
            }
        }else{
            Clause& c = *confl.getClause();
            if (c.learnt())
                claBumpActivity(&c);

            for (int j = p == lit_Undef ? 0 : 1; j < c.size(); j++){
                Lit q = c[j];
                // ANALYZE_LIT(p);
				if (!seen[var(q)] && level[var(q)] > 0){                
					varBumpActivity(q);                                 
					seen[var(q)] = 1;                                   
					if (level[var(q)] == decisionLevel())               
						pathC++;                                        
					else{                                               
						out_learnt.push(q);                             
						out_btlevel = max(out_btlevel, level[var(q)]);  
					}                                                   
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

  #if 1
    // Simplify conflict clause (a bit more):
    // 
    int i,j;
    unsigned int minl = 0;
    for (i = 1; i < out_learnt.size(); i++)
        minl |= 1 << (level[var(out_learnt[i])] & 31);

    out_learnt.copyTo(toclear);
    for (i = j = 1; i < out_learnt.size(); i++)
        if (reason[var(out_learnt[i])] == makeClause(NULL) || !removable(out_learnt[i],minl))
            out_learnt[j++] = out_learnt[i];
  #else
    // Simplify conflict clause (a little):
    //
    int i,j;
    out_learnt.copyTo(toclear);
    for (i = j = 1; i < out_learnt.size(); i++){
        LitClauseUnion r = reason[var(out_learnt[i])];
        if (r == makeClause(NULL))
            out_learnt[j++] = out_learnt[i];
        else if (r.isLit()){
            Lit q = r.getLit();
            if (!seen[var(q)] && level[var(q)] != 0)
                out_learnt[j++] = out_learnt[i];
        }else{
            Clause& c = *r.getClause();
            for (int k = 1; k < c.size(); k++)
                if (!seen[var(c[k])] && level[var(c[k])] != 0){
                    out_learnt[j++] = out_learnt[i];
                    break; }
        }
    }
  #endif

    stats.max_literals += out_learnt.size();
    out_learnt.shrink(i - j);
    stats.tot_literals += out_learnt.size();

    for (int j = 0; j < toclear.size(); j++) seen[var(toclear[j])] = 0;    // ('seen[]' is now cleared)

    //printf(L_IND"Learnt {", L_ind);
    //for (int i = 0; i < out_learnt.size(); i++) printf(" "L_LIT, L_lit(out_learnt[i]));
    //printf(" } at level %d\n", out_btlevel);
}

/* Document me!
 */
#define REMOVABLE_LIT(p) \
            if (!analyze_seen[var(p)] && level[var(p)] != 0)                                             \
                if (reason[var(p)] != makeClause(NULL) && ((1 << (level[var(p)] & 31)) &  minl) != 0){   \
                    analyze_seen[var(p)] = 1;                                                            \
                    stack.push(p);                                                                       \
                    toclear.push(p);                                                                     \
                }else{                                                                                   \
                    for (int j = top; j < toclear.size(); j++)                                           \
                        analyze_seen[var(toclear[j])] = 0;                                               \
                    toclear.shrink_(toclear.size() - top);                                               \
                    return false;                                                                        \
                }

bool Solver::removable(Lit l, unsigned int minl)
{
    assert(reason[var(l)] != makeClause(NULL));
    stack.clear_(); stack.push(l);
    int top = toclear.size();
    while (stack.size() > 0){
        assert(reason[var(stack.last())] != makeClause(NULL));
        LitClauseUnion r = reason[var(stack.last())]; stack.pop();
        if (r.isLit()){
            Lit q = r.getLit();
            REMOVABLE_LIT(q);
        }else{
            Clause& c = *r.getClause();
            for (int i = 1; i < c.size(); i++)
                REMOVABLE_LIT(c[i]);
        }
    }

    return true;
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
Clause* Solver::propagate(void)
{
    Clause* confl = NULL;
    while (qhead < trail.size()){
        //stats.propagations++;
        Lit                   p  = trail[qhead++];     // 'p' is enqueued fact to propagate.
        vec<LitClauseUnion>&  ws = watches[index(p)];
        LitClauseUnion       *i  = (LitClauseUnion*)ws, *j = i, *end = i + ws.size();

        for (;;) {
        next:
            if (i == end) break;
//			std::cout<<" propagate "<<std::endl;
            if (i->isLit()){
                if (!enqueue(i->getLit(),makeLit(p))){
                    confl = tmp_binary;
                    (*confl)[1] = ~p;
                    (*confl)[0] = i->getLit();

                    qhead = trail.size();
                    // Copy the remaining watches:
                    while (i < end)
                        *j++ = *i++;
                }
                else
                    *j++ = *i++;
            }else{
                Clause& c = *i->getClause(); i++;
                // Make sure the false literal is data[1]:
                Lit false_lit = ~p;
                if (c[0] == false_lit)
                    c[0] = c[1], c[1] = false_lit;

                assert(c[1] == false_lit);

                // If 0th watch is true, then clause is already satisfied.
                Lit   first = c[0];
                lbool val   = value(first);
                if (val == l_True){
                    *j++ = makeClause(&c);
                    goto next;
                }else{
                    // Look for new watch:
                    for (int k = 2; k < c.size(); k++)
                        if (value(c[k]) != l_False){
                            c[1] = c[k]; c[k] = false_lit;
                            watches[index(~c[1])].push(makeClause(&c));
                            goto next; }

                    // Clause is unit under assignment:
                    *j++ = makeClause(&c);
                    if (!enqueue(first, makeClause(&c))){
                        confl = &c;
                        qhead = trail.size();
                        // Copy the remaining watches:
                        while (i < end)
                            *j++ = *i++;
                    }
                }
            }
        }
        //stats.inspects += j - (Clause**)ws;
        ws.shrink_(i - j);
    }

    return confl;
}


/*_________________________________________________________________________________________________
|                                                                                                  
|  reduceDB : ()  ->  [void]                                                                       
|                                                                                                  
|  Description:                                                                                    
|    Remove half of the learnt clauses, minus the clauses locked by the current assignment. Locked 
|    clauses are clauses that are reason to a some assignment.                                     
|________________________________________________________________________________________________@*/
struct reduceDB_lt { bool operator () (Clause* x, Clause* y) { return x->size() > 2 && (y->size() == 2 || x->activity() < y->activity()); } };
void Solver::reduceDB(void)
{
    int     i, j;
    double  extra_lim = cla_inc / learnts.size();    // Remove any clause below this activity

    sort(learnts, reduceDB_lt());
    for (i = j = 0; i < learnts.size() / 2; i++){
        if (learnts[i]->size() > 2 && !locked(learnts[i]))
            remove(learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    for (; i < learnts.size(); i++){
        if (learnts[i]->size() > 2 && !locked(learnts[i]) && learnts[i]->activity() < extra_lim)
            remove(learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    learnts.shrink_(i - j);
}


/*_________________________________________________________________________________________________
|                                                                                                  
|  simplifyDB : [void]  ->  [bool]                                                                 
|                                                                                                  
|  Description:                                                                                    
|    Simplify all constraints according to the current top-level assigment (redundant constraints  
|    may be removed altogether).                                                                   
|________________________________________________________________________________________________@*/
void Solver::simplifyDB(void)
{
    if (!ok) return;    // GUARD (public method)
    assert(decisionLevel() == 0);

    /**/static int C = 0; if (C > 4) return; C++;
    if (propagate() != NULL){
        ok = false;
        return; }
    if (nAssigns() == last_simplify)
        return;

    last_simplify = nAssigns();

    for (int type = 0; type < 2; type++){
        vec<Clause*>& cs = type ? learnts : clauses;

        int     j = 0;
        for (int i = 0; i < cs.size(); i++){
            if (simplify(cs[i]))
                remove(cs[i]);
            else
                cs[j++] = cs[i];
        }
        cs.shrink_(cs.size()-j);
    }
}

/*
Solver::buildMinModel(){


}
*/


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
lbool Solver::search(int nof_conflicts, int nof_learnts, const SearchParams& params, bool& frstConflict)
{
    if (!ok) return l_False;    // GUARD (public method)
    assert(root_level == decisionLevel());
	int varId = 1;
    stats.starts++;
    int     conflictC = 0;
    var_decay = 1 / params.var_decay;
    cla_decay = 1 / params.clause_decay;
    model.clear();
	vec<Lit>    learnt_clause;
    for (;;){
        Clause* confl = propagate();
        if (confl != NULL){
			frstConflict = false;
            // CONFLICT
//			std::cout<<" conflict "<<std::endl;
            //if (verbosity >= 2) printf(L_IND"**CONFLICT**\n", L_ind);
            stats.conflicts++; conflictC++;            
			learnt_clause.shrink_(learnt_clause.size());
            int         backtrack_level;
            if (decisionLevel() == root_level)
                return l_False;
            analyze(confl, learnt_clause, backtrack_level);
            cancelUntil(max(backtrack_level, root_level));
            record(learnt_clause);
            varDecayActivity(); claDecayActivity();

        }else{
            // NO CONFLICT

            if (nof_conflicts >= 0 && conflictC >= nof_conflicts){
                // Reached bound on number of conflicts:
                progress_estimate = progressEstimate();
                cancelUntil(root_level);
                return l_Undef; }

            if (decisionLevel() == 0)
                // Simplify the set of problem clauses:
                simplifyDB(), assert(ok);

            if (nof_learnts >= 0 && learnts.size()-nAssigns() >= nof_learnts)
                // Reduce the set of learnt clauses:
                reduceDB();

            // New variable decision:
            stats.decisions++;
            Var next;
			if(frstConflict){

				next = varId; varId++;
				if(next == assigns.size()){ 
					next = var_Undef; 
				}else{
					while(toLbool(assigns[next]) != l_Undef){
						next = varId; varId++;
						if(next == assigns.size()){ next = var_Undef; break; }
					}
				}
			}else{
				next = order.select(params.random_var_freq);
			}
//			std::cout<<"next = "<<next<<std::endl;
            if (next == var_Undef){
                // Model found:
                model.growTo(nVars());
				for (int i = 0; i < nVars(); i++){ model[i] = value(i); lastassigns[i] = assigns[i]; }
                cancelUntil(root_level);
                return l_True;
            }
			lbool tt = toLbool(lastassigns[next]);
			if(tt == l_Undef){
				check(assume(~Lit(next)));
			}else{
				//std::cout<<next<<" q="<<toInt(tt)<<std::endl;
				check(assume( tt == l_True ? Lit(next) : ~Lit(next)));
			}
        }
    }
}


// Return search-space coverage. Not extremely reliable.
//
double Solver::progressEstimate(void)
{
    double  progress = 0;
    double  F = 1.0 / nVars();
    for (int i = 0; i < nVars(); i++)
        if (value(i) != l_Undef)
            progress += pow(F, level[i]);
    return progress / nVars();
}


// Divide all variable activities by 1e100.
//
void Solver::varRescaleActivity(void)
{
    for (int i = 0; i < nVars(); i++)
        activity[i] *= 1e-100;
    var_inc *= 1e-100;
}


// Divide all constraint activities by 1e100.
//
void Solver::claRescaleActivity(void)
{
    for (int i = 0; i < learnts.size(); i++)
        learnts[i]->activity() *= 1e-20;
    cla_inc *= 1e-20;
}


/*_________________________________________________________________________________________________
|                                                                                                  
|  solve : (assumps : const vec<Lit>&)  ->  [bool]                                                 
|                                                                                                  
|  Description:                                                                                    
|    Top-level solve. If using assumptions (non-empty 'assumps' vector), you must call             
|    'simplifyDB()' first to see that no top-level conflict is present (which would put the solver 
|    in an undefined state).                                                                       
|________________________________________________________________________________________________@*/
bool Solver::solve(const vec<Lit>& assumps)
{
    simplifyDB();
    if (!ok) return false;

    SearchParams    params(0.95, 0.999, 0.02);
    double  nof_conflicts = 100;
    double  nof_learnts   = nClauses() / 3;
    lbool   status        = l_Undef;

    for (int i = 0; i < assumps.size(); i++)
        if (!assume(assumps[i]) || propagate() != NULL){
            cancelUntil(0);
            return false; }
    root_level = decisionLevel();

    if (verbosity >= 1){
        printf("c ==================================[MINISAT]===================================\n");
        printf("c | %-9s | %-16s | %-32s | %-8s |\n", "Conflicts", "Original", "Learnt", "Progress");
        printf("c | %9s | %7s %8s | %7s %7s %8s %7s | %8s |\n","", "Clauses","Literals", "Max", "Clauses", "Literals", "LPC", "");
        printf("c ==============================================================================\n");
    }
	bool fstConflict = true;
    while (status == l_Undef){
        if (verbosity >= 1){
            printf("c | %9d | %7d %8d | %7d %7d %8d %7.1f | %6.3f %% |\n",(int)stats.conflicts,(int)stats.clauses, (int)stats.clauses_literals,(int)nof_learnts, (int)stats.learnts, (int)stats.learnts_literals,(double)stats.learnts_literals / (double)stats.learnts,progress_estimate*100);
            fflush(stdout);
        }
        status = search((int)nof_conflicts, (int)nof_learnts, params, fstConflict);
        nof_conflicts *= 1.5;
        nof_learnts   *= 1.1;
    }
    if (verbosity >= 1)
        printf("c ==============================================================================\n");

    cancelUntil(0);
    return status == l_True;
}

}
