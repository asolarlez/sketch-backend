#include "BoolNodeSimplifier.h"

void BoolNodeSimplifier::visit( ARRACC_node& node ) {
	Assert(node.mother->getOtype() == OutType::BOOL, "Not supported");
	if (node.mother->type == bool_node::CONST) return;
	double cond = seval->d(node.mother);
	if (cond >= 0.0) {
		rvalue = node.multi_mother[1];
	} else {
		rvalue = node.multi_mother[0];
	}
}

void BoolNodeSimplifier::visit( AND_node& node ) {
	if (node.mother->type == bool_node::CONST) return;
	if (node.father->type == bool_node::CONST) return;
	double d1 = seval->d(node.mother);
    double d2 = seval->d(node.father);
    if (d1 <= d2) {
        rvalue = node.mother;
    } else {
        rvalue = node.father;
    }
}
	
void BoolNodeSimplifier::visit( OR_node& node ) {
	if (node.mother->type == bool_node::CONST) return;
	if (node.father->type == bool_node::CONST) return;
	double d1 = seval->d(node.mother);
    double d2 = seval->d(node.father);
    if (d1 >= d2) {
        rvalue = node.mother;
    } else {
        rvalue = node.father;
    }
}

void BoolNodeSimplifier::visit( ASSERT_node& node) {
	rvalue = getCnode(0);
}

void BoolNodeSimplifier::visit( SRC_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( DST_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( CTRL_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( PLUS_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( TIMES_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( NEG_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( DIV_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( MOD_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( CONST_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( LT_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( EQ_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( NOT_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( ARRASS_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( UFUN_node& node ) {
	rvalue = &node;
}

void BoolNodeSimplifier::visit( TUPLE_R_node& node ) {
	rvalue = &node;
}


void BoolNodeSimplifier::process(BooleanDAG& bdag, int nodeid, SimpleGradEvaluator* seval_) {
	seval = seval_;
	for(int i=0; i<= nodeid ; ++i ){
		if(bdag[i] != NULL){
			//cout << "Before " << bdag[i]->lprint() << endl;
			bdag[i]->accept(*this);
            bool_node* node = rvalue;;
            //cout << "After " << node->lprint() << endl;
			if(bdag[i] != node){			
					bdag.replace(i, node);					
			}
			if (i == nodeid) {
				ASSERT_node* an = new ASSERT_node();
				an->mother = node;
				an->addToParents();
				//cout << an->lprint() << endl;
				addNode(an);
				bdag.getNodesByType(an->type).push_back(an);
				bdag.assertions.append(getDllnode(an));
			}
		}
	}
	for (int i = nodeid + 1; i < bdag.size(); i++) {
		bdag.replace(i, getCnode(0));
	}
	
	cleanup(bdag);
}