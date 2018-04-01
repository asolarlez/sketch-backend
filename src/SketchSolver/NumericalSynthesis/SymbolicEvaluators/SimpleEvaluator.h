#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"
#include <tuple>
#include <iostream>
#include <gsl/gsl_vector.h>
#include "Util.h"
#include "Interface.h"

using namespace std;


class SimpleEvaluator: NodeVisitor
{
    BooleanDAG& bdag;
    map<string, int> floatCtrls; // Maps float ctrl names to indices with grad vectors
    map<string, int> boolCtrls; // Maps bool ctrl names to indices with grad vectors
    gsl_vector* ctrls; // ctrl values
    vector<double> distances; // Keeps track of distance metric for boolean nodes
    double MIN_VALUE = 0.001;
    Interface* inputValues;
    int DEFAULT_INP = -32;
    
public:
    SimpleEvaluator(BooleanDAG& bdag_p, const map<string, int>& floatCtrls_p);
    
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
    
    
    void setInputs(const Interface* inputValues_p);
    void run(const gsl_vector* ctrls_p);
    
    
    double getErrorOnConstraint(int nodeid);
    double getSqrtError(bool_node* node);
    double getAssertError(bool_node* node);
        
    void setvalue(bool_node& bn, double d) {
        distances[bn.id] = d;
    }
    
    double d(bool_node& bn) {
        return distances[bn.id];
    }
    
    double d(bool_node* bn) {
        return d(*bn);
    }
    
    void print() {
        for (int i = 0; i < bdag.size(); i++) {
            cout << bdag[i]->lprint() << endl;
            cout << d(bdag[i]) << endl;
        }
    }
    
    bool isFloat(bool_node& bn) {
        return (bn.getOtype() == OutType::FLOAT);
    }
    
    bool isFloat(bool_node* bn) {
        return (bn->getOtype() == OutType::FLOAT);
    }
    
    int getInputValue(bool_node& bn) {
        if (inputValues->hasValue(bn.id)) {
            int val = inputValues->getValue(bn.id);
            return val;
        } else {
            return DEFAULT_INP;
        }
    }
    
    int getInputValue(bool_node* bn) {
        return getInputValue(*bn);
    }

    
    
};
