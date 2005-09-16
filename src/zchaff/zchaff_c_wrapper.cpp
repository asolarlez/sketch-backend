// *********************************************************************
// Copyright 2000-2004, Princeton University.  All rights reserved.
// By using this software the USER indicates that he or she has read,
// understood and will comply with the following:
//
// --- Princeton University hereby grants USER nonexclusive permission
// to use, copy and/or modify this software for internal, noncommercial,
// research purposes only. Any distribution, including commercial sale
// or license, of this software, copies of the software, its associated
// documentation and/or modifications of either is strictly prohibited
// without the prior consent of Princeton University.  Title to copyright
// to this software and its associated documentation shall at all times
// remain with Princeton University.  Appropriate copyright notice shall
// be placed on all software copies, and a complete copy of this notice
// shall be included in all copies of the associated documentation.
// No right is  granted to use in advertising, publicity or otherwise
// any trademark,  service mark, or the name of Princeton University.
//
//
// --- This software and any associated documentation is provided "as is"
//
// PRINCETON UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS
// OR IMPLIED, INCLUDING THOSE OF MERCHANTABILITY OR FITNESS FOR A
// PARTICULAR PURPOSE, OR THAT  USE OF THE SOFTWARE, MODIFICATIONS, OR
// ASSOCIATED DOCUMENTATION WILL NOT INFRINGE ANY PATENTS, COPYRIGHTS,
// TRADEMARKS OR OTHER INTELLECTUAL PROPERTY RIGHTS OF A THIRD PARTY.
//
// Princeton University shall not be liable under any circumstances for
// any direct, indirect, special, incidental, or consequential damages
// with respect to any claim by USER or any third party on account of
// or arising from the use, or inability to use, this software or its
// associated documentation, even if Princeton University has been advised
// of the possibility of those damages.
// ********************************************************************

#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
using namespace std;

#include "zchaff_solver.h"
#include "zchaff_clsgen.h"

#ifndef SAT_Manager
#define SAT_Manager void *
#endif

// =====================================================================
// Following are wrapper functions for C/C++ callers.
//
// =====================================================================

extern "C" SAT_Manager SAT_InitManager(void) {
  CSolver * solver = new CSolver;
  return (SAT_Manager)solver;
}

extern "C" char * SAT_Version(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  return solver->version();
}

extern "C" void SAT_SetNumVariables(SAT_Manager mng, int n_var) {
  CSolver * solver = (CSolver*) mng;
  solver->set_variable_number(n_var);
}

extern "C" void SAT_ReleaseManager(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  delete solver;
}

extern "C" int SAT_AddVariable(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int vid = solver->add_variable();
  return vid;
}

extern "C" void  SAT_EnableVarBranch(SAT_Manager mng, int vid) {
  CSolver * solver = (CSolver*) mng;
  solver->mark_var_branchable(vid);
}

extern "C" void SAT_DisableVarBranch(SAT_Manager mng, int vid) {
  CSolver * solver = (CSolver*) mng;
  solver->mark_var_unbranchable(vid);
}

extern "C" void SAT_SetTimeLimit(SAT_Manager mng, float runtime) {
  CSolver * solver = (CSolver*) mng;
  solver->set_time_limit(runtime);
}

extern "C" void SAT_SetMemLimit(SAT_Manager mng, int mem_limit) {
  CSolver * solver = (CSolver*) mng;
  solver->set_mem_limit(mem_limit);
}

extern "C" void SAT_AddClause(SAT_Manager           mng,
                          int *                 clause_lits,
                          int                   num_lits,
                          int                   gid = 0) {
  CSolver * solver = (CSolver*) mng;
  solver->add_orig_clause(clause_lits, num_lits, gid);
}

extern "C" void SAT_DeleteClauseGroup(SAT_Manager   mng,
                                  int           gid) {
  CSolver * solver = (CSolver*) mng;
  solver->delete_clause_group(gid);
}

extern "C" void SAT_Reset(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  solver->reset();
}

extern "C" int SAT_MergeClauseGroup(SAT_Manager     mng,
                                int             gid1,
                                int             gid2) {
  CSolver * solver = (CSolver*) mng;
  int g = solver->merge_clause_group(gid1, gid2);
  return g;
}

extern "C" int SAT_AllocClauseGroupID(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int gid = solver->alloc_gid();
  return gid;
}

extern "C" int SAT_GetGlobalGroupID(SAT_Manager mng) {
  return 0;
}

extern "C" int SAT_GetVolatileGroupID(SAT_Manager mng) {
  return -1;
}

extern "C" int SAT_Solve(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int result = solver->solve();
  return result;
}

extern "C" void SAT_AddHookFun(SAT_Manager          mng,
                           void(*fun)(void *),
                           int                  interval) {
  CSolver * solver = (CSolver*) mng;
  solver->add_hook(fun, interval);
}

extern "C" void SAT_MakeDecision(SAT_Manager        mng,
                             int                vid,
                             int                sign) {
  CSolver * solver = (CSolver*) mng;
  solver->make_decision(vid+vid+sign);
}

extern "C" void SAT_SetRandomness(SAT_Manager        mng,
                              int                n) {
  CSolver * solver = (CSolver*) mng;
  solver->set_randomness(n);
}

extern "C" void SAT_SetRandSeed(SAT_Manager         mng,
                            int                 seed) {
  CSolver * solver = (CSolver*) mng;
  solver->set_random_seed(seed);
}

extern "C" int SAT_GetVarAsgnment(SAT_Manager       mng,
                              int               v_idx) {
  CSolver * solver = (CSolver*) mng;
  assert(v_idx > 0 && v_idx < (int) solver->variables()->size());
  int v = solver->variable(v_idx).value();
  return v;
}

extern "C" int SAT_EstimateMemUsage(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int usage = solver->estimate_mem_usage();
  return usage;
}

extern "C" float SAT_GetElapsedCPUTime(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  float time = solver->elapsed_cpu_time();
  return time;
}

extern "C" float SAT_GetCurrentCPUTime(SAT_Manager mng) {
  float time = get_cpu_time() / 1000.0;
  return time;
}

extern "C" float SAT_GetCPUTime(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  float time = solver->cpu_run_time();
  return time;
}

extern "C" int SAT_NumLiterals(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_literals();
  return n;
}

extern "C" int SAT_NumClauses(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_clauses();
  return n;
}

extern "C" int SAT_NumVariables(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_variables();
  return n;
}

extern "C" int SAT_InitNumLiterals(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->init_num_literals();
  return n;
}

extern "C" int SAT_InitNumClauses(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->init_num_clauses();
  return n;
}

extern "C" long64 SAT_NumAddedLiterals(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  long64 n = solver->num_added_literals();
  return n;
}

extern "C" int SAT_NumAddedClauses(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int  n =  solver->num_added_clauses();
  return n;
}

extern "C" int SAT_NumDeletedClauses(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_deleted_clauses();
  return n;
}

extern "C" int SAT_NumDelOrigCls(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_del_orig_cls();
  return n;
}

extern "C" long64 SAT_NumDeletedLiterals(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  long64 n = solver->num_deleted_literals();
  return n;
}

extern "C" int SAT_NumDecisions(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_decisions();
  return n;
}

extern "C" int SAT_NumDecisionsStackConf(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_decisions_stack_conf();
  return n;
}

extern "C" int SAT_NumDecisionsVsids(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_decisions_vsids();
  return n;
}

extern "C" int SAT_NumDecisionsShrinking(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_decisions_shrinking();
  return n;
}

extern "C" int SAT_NumShrinkings(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->num_shrinkings();
  return n;
}

extern "C" int SAT_Random_Seed(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->random_seed();
  return n;
}

extern "C" long64 SAT_NumImplications(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  long64 n = solver->num_implications();
  return n;
}

extern "C" int SAT_MaxDLevel(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->max_dlevel();
  return n;
}

extern "C" float SAT_AverageBubbleMove(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  float n = ((float) solver->total_bubble_move()) /
    (solver->num_added_literals() - solver->init_num_literals());
  return n;
}

extern "C" int SAT_GetFirstClause(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  for (unsigned i = 0; i < solver->clauses()->size(); ++i)
    if (solver->clause(i).status() != DELETED_CL) {
      return i;
    }
  return -1;
}

extern "C" int SAT_GetClauseType(SAT_Manager mng, int cl_idx) {
  CSolver * solver = (CSolver*) mng;
  int type = solver->clause(cl_idx).status();
  return type;
}

extern "C" int SAT_IsSetClauseGroupID(SAT_Manager mng, int cl_idx, int id) {
  CSolver * solver = (CSolver*) mng;
  int r = solver->clause(cl_idx).gid(id);
  return r;
}

extern "C" void SAT_ClearClauseGroupID(SAT_Manager mng, int cl_idx, int id) {
  CSolver * solver = (CSolver*) mng;
  solver->clause(cl_idx).clear_gid(id);
}

extern "C" void SAT_SetClauseGroupID(SAT_Manager mng, int cl_idx, int id) {
  CSolver * solver = (CSolver*) mng;
  solver->clause(cl_idx).set_gid(id);
}

extern "C" int SAT_GetNextClause(SAT_Manager mng, int cl_idx) {
  CSolver * solver = (CSolver*) mng;
  for (unsigned i = cl_idx + 1; i < solver->clauses()->size(); ++i)
    if (solver->clause(i).status() != DELETED_CL) {
      return i;
    }
  return -1;
}

extern "C" int SAT_GetClauseNumLits(SAT_Manager mng, int cl_idx) {
  CSolver * solver = (CSolver*) mng;
  int n = solver->clause(cl_idx).num_lits();
  return n;
}

extern "C" void SAT_GetClauseLits(SAT_Manager mng, int cl_idx, int * lits) {
  CSolver * solver = (CSolver*) mng;
  for (unsigned i = 0; i < solver->clause(cl_idx).num_lits(); ++i) {
    lits[i] = solver->clause(cl_idx).literal(i).s_var();
  }
}

extern "C" void SAT_EnableConfClsDeletion(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  solver->enable_cls_deletion(true);
}

extern "C" void SAT_DisableConfClsDeletion(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  solver->enable_cls_deletion(false);
}

extern "C" void SAT_CleanUpDatabase(SAT_Manager mng) {
  CSolver * solver = (CSolver*) mng;
  solver->clean_up_dbase();
}

extern "C" void SAT_GenClsAnd2(SAT_Manager          mng,
                           int                  a,
                           int                  b,
                           int                  o,
                           int                  gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.and2(*solver, a, b, o, gid);
}

extern "C" void SAT_GenClsAndN(SAT_Manager          mng,
                           int *                inputs,
                           int                  num_inputs,
                           int                  o,
                           int                  gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.and_n(*solver, inputs, num_inputs, o, gid);
}

extern "C" void SAT_GenClsOr2(SAT_Manager           mng,
                          int                   a,
                          int                   b,
                          int                   o,
                          int                   gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.or2(*solver, a, b, o, gid);
}

extern "C" void SAT_GenClsOrN(SAT_Manager           mng,
                          int *                 inputs,
                          int                   num_inputs,
                          int                   o,
                          int                   gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.or_n(*solver, inputs, num_inputs, o, gid);
}

extern "C" void SAT_GenClsNand2(SAT_Manager         mng,
                            int                 a,
                            int                 b,
                            int                 o,
                            int                 gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.nand2(*solver, a, b, o, gid);
}


extern "C" void SAT_GenClsNandN(SAT_Manager         mng,
                            int *               inputs,
                            int                 num_inputs,
                            int                 o,
                            int                 gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.nand_n(*solver, inputs, num_inputs, o, gid);
}


extern "C" void SAT_GenClsNor2(SAT_Manager          mng,
                           int                  a,
                           int                  b,
                           int                  o,
                           int                  gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.nor2(*solver, a, b, o, gid);
}


extern "C" void SAT_GenClsNorN(SAT_Manager          mng,
                           int *                inputs,
                           int                  num_inputs,
                           int                  o,
                           int                  gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.nor_n(*solver, inputs, num_inputs, o, gid);
}

extern "C" void SAT_GenClsXor(SAT_Manager           mng,
                          int                   a,
                          int                   b,
                          int                   o,
                          int                   gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.xor2(*solver, a, b, o, gid);
}

extern "C" void SAT_GenClsNot(SAT_Manager           mng,
                          int                   a,
                          int                   o,
                          int                   gid = 0) {
  CSolver * solver = (CSolver*) mng;
  CClause_Gen cls_gen;
  cls_gen.not1(*solver, a, o, gid);
}
