#include "SimpleEvaluator.h"
#include <algorithm>
#include <limits>
#include <math.h>


SimpleEvaluator::SimpleEvaluator(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
	distances.resize(bdag.size(), NULL);
    int nctrls = floatCtrls.size();
	if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

SimpleEvaluator::~SimpleEvaluator(){
	gsl_vector_free(ctrls); 
}

void SimpleEvaluator::visit( SRC_node& node ) {
  //cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: SimpleEvaluator for src");
}

void SimpleEvaluator::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
	// Ignore
}

void SimpleEvaluator::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
	setvalue(node, d(node.mother));
}

void SimpleEvaluator::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float ctrls should be handled by the numerical solver");
		}
		double val = gsl_vector_get(ctrls, idx);
		setvalue(node, val);
	} else {
		int idx = -1;
        int ival = getInputValue(node);
        if (ival != DEFAULT_INPUT) {
            setvalue(node, ival - 0.5);
        } else if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
			double val = gsl_vector_get(ctrls, idx);
			double dist = val - 0.5;
			setvalue(node, dist);
		} else {
			setvalue(node, 0);
			//Assert(false, "All bool ctrls should be handled here");
		}
		
	}
}

void SimpleEvaluator::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	setvalue(node, d(node.mother) + d(node.father));
}

void SimpleEvaluator::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	setvalue(node, d(node.mother)*d(node.father));
}

void SimpleEvaluator::visit( ARRACC_node& node ) {
  //cout << "Visiting ARRACC node" << endl;
	Assert(node.multi_mother.size() == 2, "NYI: SimpleEvaluator for ARRACC of size > 2");
	double m = d(node.mother);
	int idx = (m >= 0) ? 1 : 0;
	setvalue(node, d(node.multi_mother[idx]));
}

void SimpleEvaluator::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	setvalue(node, d(node.mother)/d(node.father));
}

void SimpleEvaluator::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  Assert(false, "NYI: SimpleEvaluator mod");
}

void SimpleEvaluator::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	setvalue(node, -d(node.mother));
}

void SimpleEvaluator::visit( CONST_node& node ) {
	if (node.isFloat()) {
		setvalue(node, node.getFval());
	} else {
		int val = node.getVal();
		if (node.getOtype() == OutType::BOOL) {
			double dist = (val == 1) ? 1000 : -1000;
			setvalue(node, dist);
		} else {
            setvalue(node, val);
		}
	}
}

void SimpleEvaluator::visit( LT_node& node ) {
	//Assert(isFloat(node.mother) && isFloat(node.father), "NYI: SimpleEvaluator for lt with integer parents");
	double m = d(node.mother);
	double f = d(node.father);
	double d = f - m;
	if (d == 0) d = -MIN_VALUE;
	//cout << node.lprint() << " " << m << " " << f << " " << d << endl;
	setvalue(node, d);
}

void SimpleEvaluator::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: SimpleEvaluator for eq");
}

void SimpleEvaluator::visit( AND_node& node ) {
	double m = d(node.mother);
	double f = d(node.father);
	double d = (m < f) ? m : f;
	setvalue(node, d);
}

void SimpleEvaluator::visit( OR_node& node ) {
	double m = d(node.mother);
	double f = d(node.father);
	double d = (m > f) ? m : f;
	setvalue(node, d);
}

void SimpleEvaluator::visit( NOT_node& node ) {
	setvalue(node, -d(node.mother));
}

void SimpleEvaluator::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  Assert(false, "NYI: SimpleEvaluator for arrass");
}

void SimpleEvaluator::visit( UFUN_node& node ) {
	const string& name = node.get_ufname();
	double m = d(node.multi_mother[0]);
	double d;
	if (name == "_cast_int_float_math") {
		d = m;
    } else if (name == "arctan_math") {
        d = atan(m);
    } else if (name == "sin_math") {
        d = sin(m);
    } else if (name == "cos_math") {
        d = cos(m);
    } else if (name == "tan_math") {
        d = tan(m);
    } else if (name == "sqrt_math") {
        d = sqrt(m);
    } else if (name == "exp_math") {
        d = exp(m);
    } else {
        Assert(false, "NYI");
    }
    
	setvalue(node, d);
}

void SimpleEvaluator::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		setvalue(node, d(node.mother));
	} else {
    Assert(false, "NYI");
  }
}

void SimpleEvaluator::setInputs(Interface* inputValues_p) {
    inputValues = inputValues_p;
}

void SimpleEvaluator::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "SimpleEvaluator ctrls sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    
    for (int i = 0; i < bdag.size(); i++) {
        bdag[i]->accept(*this);
    }
}

double SimpleEvaluator::getErrorOnConstraint(int nodeid) {
    bool_node* node = bdag[nodeid];
    if (Util::isSqrt(node)) {
        return getSqrtError(node);
    } else if (node->type == bool_node::ASSERT) {
        return getAssertError(node);
    } else if (node->type == bool_node::CTRL && node->getOtype() == OutType::BOOL) {
    	return getBoolCtrlError(node);
    } else {
    	cout << node->lprint() << endl;
        Assert(false, "Unknonwn node for computing error in Simple Evaluator");
    }
}

double SimpleEvaluator::getSqrtError(bool_node* node) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    return d(x);
}

double SimpleEvaluator::getAssertError(bool_node* node) {
    return d(node->mother);
}

double SimpleEvaluator::getBoolCtrlError(bool_node* node) {
	// return an error if the bool ctrl is not fixed by the sat solver
	int ival = getInputValue(*node);
    if (ival == DEFAULT_INPUT) {
    	return -1.0;
    }
}




