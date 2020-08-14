#include "SimpleGradEvaluator.h"
#include <algorithm>
#include <limits>
#include <math.h>


SimpleGradEvaluator::SimpleGradEvaluator(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
	values.resize(bdag.size(), NULL);
    nctrls = floatCtrls.size();
    if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

SimpleGradEvaluator::~SimpleGradEvaluator(void) {
	gsl_vector_free(ctrls);
	for (int i = 0; i < values.size(); i++) {
		if (values[i] != NULL) {
			delete values[i];
		}
	}
}

void SimpleGradEvaluator::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: SimpleGradEvaluator for src");
}

void SimpleGradEvaluator::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void SimpleGradEvaluator::visit( ASSERT_node& node ) {
    ValueGrad* vg = v(node);
    GradUtil::default_grad(vg->getGrad());
	//cout << "Visiting ASSERT node" << endl;
}

void SimpleGradEvaluator::visit( CTRL_node& node ) {
	//cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float holes should be handled by numerical solver");
		}
		ValueGrad* val = v(node);
		val->update(gsl_vector_get(ctrls, idx));
		val->set = true;
		GradUtil::default_grad(val->getGrad());
		gsl_vector_set(val->getGrad(), idx, 1.0);
		
	} else {
		Assert(false, "Bool holes");
	}
}

void SimpleGradEvaluator::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother());
	ValueGrad* fval = v(node.father());
	ValueGrad::vg_plus(mval, fval, val);
}

void SimpleGradEvaluator::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother());
	ValueGrad* fval = v(node.father());
	ValueGrad::vg_times(mval, fval, val);
}

void SimpleGradEvaluator::visit(ARRACC_node& node )  {
	Assert(node.nargs() == 2, "NYI: SimpleGradEvaluator ARRACC of size > 2");
	if (node.getOtype() == OutType::BOOL) {
	   Assert(false, "bool arracc");	
    }
	
	if (node.getOtype() == OutType::FLOAT) {
		ValueGrad* val  = v(node);
        ValueGrad* mval = v(node.arguments(0));
        ValueGrad* fval = v(node.arguments(1));
        ValueGrad* cval = v(node.mother());

        if (cval->getVal() >= 0) {
            ValueGrad::vg_copy(fval, val);
        } else  {
            ValueGrad::vg_copy(mval, val);
        } 
    }
}

void SimpleGradEvaluator::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother());
	ValueGrad* fval = v(node.father());
	ValueGrad::vg_div(mval, fval, val);
}

void SimpleGradEvaluator::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: SimpleGradEvaluator mod");
}

void SimpleGradEvaluator::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother());
	ValueGrad::vg_neg(mval, val);
}

void SimpleGradEvaluator::visit( CONST_node& node ) {
	if (node.getOtype() == OutType::FLOAT) {
		ValueGrad* val = v(node);
		val->update(node.getFval());
		val->set = true;
		GradUtil::default_grad(val->getGrad());
	} else {
		int val = node.getVal();
		ValueGrad* vg = v(node);
		if (node.getOtype() == OutType::BOOL) {
			vg->update((val == 1) ? 1000 : -1000);
			GradUtil::default_grad(vg->getGrad());
			vg->set = true;
		} else {
            Assert(false, "Integers are not handled yet");
		}
	}
}

void SimpleGradEvaluator::visit( LT_node& node ) {
	ValueGrad* vg = v(node);
	
	// Comput the value from the parents
	Assert(isFloat(node.mother()) && isFloat(node.father()), "NYI: SimpleGradEvaluator for lt with integer parents");
	ValueGrad* mval = v(node.mother());
	ValueGrad* fval = v(node.father());
	
	ValueGrad::vg_lt(mval, fval, vg);
}

void SimpleGradEvaluator::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: SimpleGradEvaluator for eq");
}

void SimpleGradEvaluator::visit( AND_node& node ) {
	ValueGrad* vg = v(node);
    // Compute the value from the parents
    ValueGrad* mdist = v(node.mother());
    ValueGrad* fdist = v(node.father());
    if (mdist->getVal() < fdist->getVal()) {
        ValueGrad::vg_copy(mdist, vg);
    } else {
        ValueGrad::vg_copy(fdist, vg);
    }
}

void SimpleGradEvaluator::visit( OR_node& node ) {
	ValueGrad* vg = v(node);
    // Compute the value from the parents
    ValueGrad* mdist = v(node.mother());
    ValueGrad* fdist = v(node.father());
    if (mdist->getVal() > fdist->getVal()) {
        ValueGrad::vg_copy(mdist, vg);
    } else {
        ValueGrad::vg_copy(fdist, vg);
    }
}

void SimpleGradEvaluator::visit( NOT_node& node ) {
	ValueGrad* vg = v(node);
    ValueGrad* mdist = v(node.mother());

	ValueGrad::vg_not(mdist, vg);
}

void SimpleGradEvaluator::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: SimpleGradEvaluator for arrass");
}

void SimpleGradEvaluator::visit( UFUN_node& node ) {
    ValueGrad* val = v(node);
	ValueGrad* mval = v(node.arguments(0));
	
	const string& name = node.get_ufname();
	if (name == "_cast_int_float_math") {
		ValueGrad::vg_cast_int_float(mval, val);
	} else if (name == "arctan_math") {
        ValueGrad::vg_arctan(mval, val);
    } else if (name == "sin_math") {
        ValueGrad::vg_sin(mval, val);
    } else if (name == "cos_math") {
        ValueGrad::vg_cos(mval, val);
    } else if (name == "tan_math") {
        ValueGrad::vg_tan(mval, val);
    } else if (name == "sqrt_math") {
        ValueGrad::vg_sqrt(mval, val);
    } else if (name == "exp_math") {
        ValueGrad::vg_exp(mval, val);
    } else {
        Assert(false, "NYI");
    }
}

void SimpleGradEvaluator::visit( TUPLE_R_node& node) {
	if (node.mother()->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother()))->nargs() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		ValueGrad* mval = v(node.mother());
		ValueGrad* val = v(node);
		ValueGrad::vg_copy(mval, val);
	} else {
		Assert(false, "NYI");
	}
}


void SimpleGradEvaluator::setInputs(Interface* inputValues_p) {
    inputValues = inputValues_p;
}


// assumes nodesSubset forms a complete DAG (i.e. no dangling parent pointers) and in ascending order
void SimpleGradEvaluator::run(const gsl_vector* ctrls_p, const set<int>& nodesSubset) {
    Assert(ctrls->size == ctrls_p->size, "SimpleGradEvaluator ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) { // why this copying?
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    
    for (auto it = nodesSubset.begin(); it != nodesSubset.end(); it++) {
        bdag[*it]->accept(*this);
    }
}

void SimpleGradEvaluator::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "SimpleGradEvaluator ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    for (int i = 0; i < bdag.size(); i++) {
        bdag[i]->accept(*this);
    }
    
    /*for (int i = 0; i < bdag.size(); i++) {
        DistanceGrad* dg = d(bdag[i]);
        ValueGrad* vg = v(bdag[i]);
        cout << bdag[i]->lprint();
        if (dg->set && bdag[i]->getOtype() == OutType::BOOL) {
            cout << " " << dg->dist << endl;
        } else {
            cout << " " << vg->getVal() << endl;
        }
    }*/ 
}

double SimpleGradEvaluator::getErrorOnConstraint(int nodeid, gsl_vector* grad) { // Negative means errors TODO: name is confusing
    bool_node* node = bdag[nodeid];
    if (Util::isSqrt(node)) {
        return getSqrtError(node, grad);
    } else if (node->type == bool_node::ASSERT) {
        return getAssertError(node, grad);
    } else if (node->type == bool_node::CTRL && node->getOtype() == OutType::BOOL) {
        return getBoolCtrlError(node, grad);
    } else if (node->getOtype() == OutType::BOOL) {
        return getBoolExprError(node, grad);
    } else {
        Assert(false, "Unknown node for computing error");
    }
}

double SimpleGradEvaluator::getSqrtError(bool_node* node, gsl_vector* grad) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->arguments(0);
    ValueGrad* val = v(x);
    Assert(val->set, "Sqrt node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double SimpleGradEvaluator::getAssertError(bool_node* node, gsl_vector* grad) {
    ValueGrad* vg = v(node->mother());
    Assert(vg->set, "Assert node is not set");
    gsl_vector_memcpy(grad, vg->getGrad());
    return vg->getVal();
}

double SimpleGradEvaluator::getBoolCtrlError(bool_node* node, gsl_vector* grad) {
    string name = node->get_name();
    int idx = -1;
    if (floatCtrls.find(name) != floatCtrls.end()) {
        idx = floatCtrls[name];
    } else {
        Assert(false, "qwe8yqwi");
    }
    GradUtil::default_grad(grad);
    gsl_vector_set(grad, idx, 1.0);
    double dist = gsl_vector_get(ctrls, idx);
    
    gsl_vector_scale(grad, 2*dist - 1);
    return 0.1 - dist*(1-dist); // TODO: magic numbers
}

double SimpleGradEvaluator::getBoolExprError(bool_node* node, gsl_vector* grad) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int val = inputValues->getValue(node->id);
    
    ValueGrad* vg = v(node);
    Assert(vg->set, "Boolean expression distance is not set");
    gsl_vector_memcpy(grad, vg->getGrad());
    if (val == 1) {
        return vg->getVal();
    } else {
        gsl_vector_scale(grad, -1.0);
        return -vg->getVal();
    }
}




double SimpleGradEvaluator::getErrorOnConstraint(int nodeid) {
    bool_node* node = bdag[nodeid];
    if (Util::isSqrt(node)) {
        return getSqrtError(node);
    } else if (node->type == bool_node::ASSERT) {
        return getAssertError(node);
    } else if (node->type == bool_node::CTRL && node->getOtype() == OutType::BOOL) {
        return getBoolCtrlError(node);
    } else if (node->getOtype() == OutType::BOOL) {
        return getBoolExprError(node);
    } else {
        Assert(false, "Unknown node for computing error");
    }
}

double SimpleGradEvaluator::getSqrtError(bool_node* node) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->arguments(0);
    ValueGrad* val = v(x);
    Assert(val->set, "Sqrt node is not set");
    return val->getVal();
}

double SimpleGradEvaluator::getAssertError(bool_node* node) {
    ValueGrad* vg = v(node->mother());
    Assert(vg->set, "Assert node is not set");
    return vg->getVal();
}

double SimpleGradEvaluator::getBoolCtrlError(bool_node* node) {
    string name = node->get_name();
    int idx = -1;
    if (floatCtrls.find(name) != floatCtrls.end()) {
        idx = floatCtrls[name];
    } else {
        Assert(false, "qwe8yqwi");
    }
    double dist = gsl_vector_get(ctrls, idx);
    return 0.1 - dist*(1-dist); // TODO: magic numbers
}

double SimpleGradEvaluator::getBoolExprError(bool_node* node) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int val = inputValues->getValue(node->id);
    
    ValueGrad* vg = v(node);
    Assert(vg->set, "Boolean expression distance is not set");
    if (val == 1) {
        return vg->getVal();
    } else {
        return -vg->getVal();
    }
}



