#pragma once

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "Tvalue.h"
#include "VarStore.h"
#include "CommandLineArgs.h"
#include "SolverTypes.h"
#include <stack>
#include <ctime>
#include "FloatSupport.h"
#include "FindCheckSolver.h"
#include "Interface.h"

class REASSolver
{
    FloatManager& floats;
    BooleanDAG* problem;
    
    SolverHelper& dirFind;
    SATSolver& mngFind;
    
    bool stoppedEarly;
    
    vector<Tvalue> node_ids;
    map<bool_node*, int> node_values;
    
    protected:
    void declareControl(CTRL_node* cnode);
    bool solveOptimization();
    bool find(VarStore& controls);
    void createBooleanAbstraction(Interface* interf);
    
    
    public:
    VarStore ctrlStore;
    
    REASSolver(SolverHelper& finder, FloatManager& _floats);
    ~REASSolver(void);
    void addProblem(BooleanDAG* miter);
    
    
    virtual bool solve();
    void print_control_map(ostream& out);
    void printDiagnostics(SATSolver& mng, char c);
    void get_control_map(map<string, string>& values);
};
