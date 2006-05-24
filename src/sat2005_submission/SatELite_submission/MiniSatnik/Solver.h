/**************************************************************************************************

Solver.h -- (C) Niklas Een, Niklas Sörensson, 2004

A simple Chaff-like SAT-solver with support for incremental SAT.

**************************************************************************************************/

#ifndef Solver_h
#define Solver_h

#include "SolverTypes.h"
//#include "Queue.h"
#include "VarOrder.h"

#define reportf(format, args...) ( printf("c "), printf(format , ## args), fflush(stdout) )


//=================================================================================================
// Clause -- a simple class for representing a clause

class Clause {
    unsigned    size_learnt;
    Lit         data[0];
public:
    // NOTE: cannot be used by normal new !!!
    Clause(bool learnt, const vec<Lit>& ps) {
        size_learnt = (ps.size() << 1) | (int)learnt;
        for (int i = 0; i < ps.size(); i++) data[i] = ps[i];
        if (learnt) activity() = 0; }

    int     size        (void)      const { return size_learnt >> 1; }
    bool    learnt      (void)      const { return size_learnt & 1; }
    Lit     operator [] (int index) const { return data[index]; }
    Lit&    operator [] (int index)       { return data[index]; }
    float&  activity    (void)      const { return *((float*)&data[size()]); }
};


//=================================================================================================
// LitClauseUnion -- a simple union type

class LitClauseUnion {
    void* data;
    LitClauseUnion(void* d) : data(d) {}
 public:
    friend LitClauseUnion makeLit    (Lit l)      { return LitClauseUnion((void*)((index(l)<<1) + 1)); }
    friend LitClauseUnion makeClause (Clause* c)  { assert((unsigned int)c % 2 == 0); return LitClauseUnion((void*)c); }

    bool        isLit             (void)     const { return ((unsigned int)data % 2) == 1; }
    bool        isNull            (void)     const { return data == NULL; }
    Lit         getLit            (void)     const { return toLit(((int)data)>>1); }
    Clause*     getClause         (void)     const { return (Clause*)data; }
    bool        operator==(LitClauseUnion c) const { return data == c.data; }
    bool        operator!=(LitClauseUnion c) const { return data != c.data; }
};

//=================================================================================================
// Solver -- the main class:


struct SolverStats {
    int64   starts, decisions, propagations, inspects, conflicts;
    int64   clauses, clauses_literals, learnts, learnts_literals, max_literals, tot_literals;
    SolverStats(void) : starts(0), decisions(0), propagations(0), inspects(0), conflicts(0)
      , clauses(0), clauses_literals(0), learnts(0), learnts_literals(0), max_literals(0), tot_literals(0) { }
};


struct SearchParams {
    double  var_decay, clause_decay, random_var_freq;    // (reasonable values are: 0.95, 0.999, 0.02)    
    SearchParams(double v = 1, double c = 1, double r = 0) : var_decay(v), clause_decay(c), random_var_freq(r) { }
};


class Solver {
protected:
    bool                ok;             // If FALSE, the constraints are already unsatisfiable. No part of the solver state may be used!
    vec<Clause*>        clauses;        // List of problem constraints.
    vec<Clause*>        learnts;        // List of learnt clauses.
    double              cla_inc;        // Amount to bump next clause with.
    double              cla_decay;      // INVERSE decay factor for clause activity: stores 1/decay.

    vec<double>         activity;       // A heuristic measurement of the activity of a variable.
    double              var_inc;        // Amount to bump next variable with.
    double              var_decay;      // INVERSE decay factor for variable activity: stores 1/decay. Use negative value for static variable order.
    VarOrder            order;          // Keeps track of the decision variable order.

    vec<vec<LitClauseUnion> >
                        watches;        // 'watches[lit]' is a list of constraints watching 'lit' (will go there if literal becomes true).

    vec<char>           assigns;        // The current assignments (lbool:s stored as char:s).
    vec<Lit>            trail;          // List of assignments made.
    vec<int>            trail_lim;      // Separator indices for different decision levels in 'trail'.
    vec<LitClauseUnion> reason;         // 'reason[var]' is the clause that implied the variables current value, or 'NULL' if none.
    vec<int>            level;          // 'level[var]' is the decision level at which assignment was made.
    int                 root_level;     // Level of first proper decision.
    int                 last_simplify;  // Number of top-level assignments at last 'simplifyDB()'.
    int                 qhead;          // head of queue (as index in trail)

    // Temporaries (to reduce allocation overhead):
    //
    vec<char>           analyze_seen;
    vec<Lit>            toclear;
    vec<Lit>            stack;
    Clause*             tmp_binary;

    // Main internal methods:
    //
    bool        assume       (Lit p);
    void        cancelUntil  (int level);
    void        record       (const vec<Lit>& clause);

    void        analyze      (Clause* confl, vec<Lit>& out_learnt, int& out_btlevel); // (bt = backtrack)
    bool        removable    (Lit l, unsigned int minl);
inline    bool        enqueue      (Lit fact, LitClauseUnion from = makeClause(NULL));
    Clause*     propagate    (void);
    void        reduceDB     (void);
    Lit         pickBranchLit(const SearchParams& params);
    lbool       search       (int nof_conflicts, int nof_learnts, const SearchParams& params);
    double      progressEstimate(void);

    // Activity:
    //
    void    varBumpActivity(Lit p) {
        if (var_decay < 0) return;     // (negative decay means static variable order -- don't bump)
        if ( (activity[var(p)] += var_inc) > 1e100 ) varRescaleActivity();
        order.update(var(p)); }
    void    varDecayActivity(void) { if (var_decay >= 0) var_inc *= var_decay; }
    void    varRescaleActivity(void);
    void    claDecayActivity(void) { cla_inc *= cla_decay; }
    void    claRescaleActivity(void);

    // Operations on clauses:
    //
    bool    newClause(const vec<Lit>& ps, bool learnt, Clause*& out_clause);
    void    claBumpActivity (Clause* c) { if ( (c->activity() += cla_inc) > 1e20 ) claRescaleActivity(); }
    void    remove(Clause* c, bool just_dealloc = false);
    bool    locked          (const Clause* c) const { LitClauseUnion r = reason[var((*c)[0])]; return !(r.isLit()) && (r.getClause() == c); }
    bool    simplify        (Clause* c) const;

    int     decisionLevel(void) const { return trail_lim.size(); }

public:
    Solver(void) : ok               (true)
                 , cla_inc          (1)
                 , cla_decay        (1)
                 , var_inc          (1)
                 , var_decay        (1)
                 , order            (assigns, activity)
                 , last_simplify    (-1)
                 , qhead            (0)
                 , progress_estimate(0)
                 , verbosity(0)
                 {
                     vec<Lit> dummy(2,lit_Undef);
                     void*   mem = xmalloc<char>(sizeof(Clause) + sizeof(unsigned)*2);
                     tmp_binary  = new (mem) Clause(false,dummy);
                 }

   ~Solver(void) {
       for (int i = 0; i < learnts.size(); i++) remove(learnts[i], true);
       for (int i = 0; i < clauses.size(); i++) remove(clauses[i], true); }

    // Helpers: (semi-internal)
    //
    lbool   value(Var x) const { return toLbool(assigns[x]); }
    lbool   value(Lit p) const { return sign(p) ? ~toLbool(assigns[var(p)]) : toLbool(assigns[var(p)]); }

    int     nAssigns(void) { return trail.size(); }
    int     nClauses(void) { return clauses.size(); }
    int     nLearnts(void) { return learnts.size(); }

    // Statistics: (read-only member variable)
    //
    SolverStats stats;

    // Problem specification:
    //
    Var     newVar (void);
    int     nVars  (void)  { return assigns.size(); }
    void    addUnit(Lit p) { if (ok) enqueue(p); }

    void    addClause(const vec<Lit>& ps)        { if (ok){ Clause* c; ok = newClause(ps, false, c); if (c != NULL) clauses.push(c); } }

    // Solving:
    //
    bool    okay(void) { return ok; }
    void    simplifyDB(void);
    bool    solve(const vec<Lit>& assumps);
    bool    solve(void) { vec<Lit> tmp; return solve(tmp); }

    double      progress_estimate;  // Set by 'search()'.
    vec<lbool>  model;              // If problem is solved, this vector contains the model (if any).
    int         verbosity;          // Verbosity level. 0=silent, 1=some progress report, 2=everything
};


//=================================================================================================
#endif
