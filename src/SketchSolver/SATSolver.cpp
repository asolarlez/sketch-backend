

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <map>
#include <string>
#include <set>
#include <vector>
#include <dirent.h>
#include <queue>
#include "SATSolver.h"
#include "zchaff_solver.h"
#include "zchaff_clsgen.h"


 void SAT_AddClauseSigned(SAT_Manager           mng,
                          int *                 clause_lits,
                          int                   num_lits,
                          int                   gid) {
 
 	
  CSolver * solver = (CSolver*) mng;
  int vars = solver->num_variables();
   for(int i=0; i<num_lits; ++i){   
   	int sign=0;	
   	if( clause_lits[i] < 0){ clause_lits[i] = -clause_lits[i]; sign = 1;}
   	if( clause_lits[i]>vars){
  		cout<<" INCORRECT STUFF "<<vars<<"  "<< clause_lits[i]<<"  "<<i<<endl;	
  	}
  	if( clause_lits[i] == 1048575){
  		cout<<"Now is the time!!"<<endl;
  	}
   	clause_lits[i] = (clause_lits[i] << 1) + sign;
  }

  solver->add_orig_clause(clause_lits, num_lits, gid);
}

