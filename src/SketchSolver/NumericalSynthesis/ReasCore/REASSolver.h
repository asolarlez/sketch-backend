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
#include "NumericalSynthesizer.h"

class REASSolver
{
    FloatManager& floats;
    BooleanDAG* problem;
        
    bool stoppedEarly;
    
    vector<Tvalue> node_ids;
    map<bool_node*, int> node_values;

    NumericalSynthesizer* ns;
    
    protected:
    void declareControl(CTRL_node* cnode);
    bool solveOptimization();
    bool find(VarStore& controls);
    void createBooleanAbstraction(Interface* interf);
    
    
    public:
    VarStore ctrlStore;
    
    REASSolver(FloatManager& _floats);
    ~REASSolver(void);
    void addProblem(BooleanDAG* miter);
    
    
    virtual bool solve();
    void print_control_map(ostream& out);
    void printDiagnostics(SATSolver& mng, char c);
    void get_control_map(map<string, string>& values);
    map<string, int>& get_ctrls()
    {
        return ns->get_ctrls();
    }
    gsl_vector* get_result()
    {
        return ns->get_result();
    }
};
