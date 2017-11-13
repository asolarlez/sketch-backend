#pragma once
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include <map>
#include "SymbolicEvaluator.h"
#include "Util.h"

#include <iostream>


using namespace std;

// Goes through the DAG and figures out where the gradient vanishes
class GradientAnalyzer: public NodeVisitor
{
	BooleanDAG& bdag;
    SymbolicEvaluator* eval;
    
    double threshold = 0.01;
    
    double upThreshold = 1000;
    
    bool printVals = true;
    bool printGrads = false;
    bool printGradLoss = false;
    bool printGradBlowup = false;
    bool printSqrt = false;
	
public:
	
    GradientAnalyzer(BooleanDAG& bdag_p, SymbolicEvaluator* eval_p): bdag(bdag_p), eval(eval_p) {}
    ~GradientAnalyzer(void) {}
	
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( TUPLE_R_node& node );
	virtual void visit( ASSERT_node& node );
	
	virtual void run();
    virtual void run(bool_node* n);
	
};
