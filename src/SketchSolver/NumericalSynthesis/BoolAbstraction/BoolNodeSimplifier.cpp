#include "BoolNodeSimplifier.h"

void BoolNodeSimplifier::visit( ARRACC_node& node ) {
	Assert(node.mother->getOtype() == OutType::BOOL, "Not supported");
	double cond = seval->d(node.mother);
	if (cond >= 0.0) {
		rvalue = node.multi_mother[1];
	} else {
		rvalue = node.multi_mother[0];
	}
}

void BoolNodeSimplifier::visit( AND_node& node ) {
	double d1 = seval->d(node.mother);
    double d2 = seval->d(node.father);
    if (d1 <= d2) {
        rvalue = node.mother;
    } else {
        rvalue = node.father;
    }
}
	
void BoolNodeSimplifier::visit( OR_node& node ) {
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

void BoolNodeSimplifier::process(BooleanDAG& bdag, int nodeid, SimpleEvaluator* seval_) {
	seval = seval_;
	for(int i=0; i<= nodeid ; ++i ){
		if(bdag[i] != NULL){
			//cout << "Before " << bdag[i]->lprint() << endl;
            bool_node* node = computeOptim(bdag[i]);
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