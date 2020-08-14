#include "BoolAbstractor.h"

bool BoolAbstractor::createConstraints(BooleanDAG& dag, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids, FloatManager& floats, Interface* interf){
    //timerclass timer("defineProblem");
    //timer.start();
    bool stoppedEarly;
    int YES = dir.newYES();
    dag.lprint(cout);
    //getProblem()->lprint(cout);
    
    BoolAbstractor nts(dir, "PROBLEM", node_values, node_ids, floats, interf);

    try{
        stoppedEarly =false;
        nts.process(dag);
        if(nts.stoppedPrematurely()){
            dir.lastErrMsg = nts.errorMsg;
            stoppedEarly = true;
        }
    }catch(BasicError& e){
        throw e;
    }
    
    return stoppedEarly;
}

void BoolAbstractor::addToInterface(Tvalue& tv, bool_node& node) {
    interf->add(tv, node, dir);
}


void BoolAbstractor::visit( AND_node& node ){
    NodesToSolver::visit(node);
    if (!Util::hasAssertChild(node) && !Util::hasNotAssertChild(node)) {
        addToInterface(node_ids[node.id], node);
    }
    return;
}


void BoolAbstractor::visit( OR_node& node ){
    NodesToSolver::visit(node);
    if (!Util::hasAssertChild(node) && !Util::hasNotAssertChild(node)) {
        addToInterface(node_ids[node.id], node);
    }
}

void BoolAbstractor::visit( XOR_node& node ){
    NodesToSolver::visit(node);
    if (!Util::hasAssertChild(node) && !Util::hasNotAssertChild(node)) {
        addToInterface(node_ids[node.id], node);
    }
}


void
BoolAbstractor::visit (SRC_node &node)
{
    Assert(false, "Not yet supported");
}



void BoolAbstractor::visit( DST_node& node ){
    NodesToSolver::visit(node);
}

void
BoolAbstractor::visit (NOT_node &node)
{
    NodesToSolver::visit(node);
    //addToInterface(node_ids[node.id], node); // We don't have to add this to the interface because NOT node does not introduce any discontinuities
}

void
BoolAbstractor::visit (CTRL_node &node)
{
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}



void BoolAbstractor::visit( PLUS_node& node ){
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}

void BoolAbstractor::visit( TIMES_node& node ){
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}


void BoolAbstractor::visit(DIV_node& node) {
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}

void BoolAbstractor::visit(MOD_node& node) {
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}

void
BoolAbstractor::visit(NEG_node &node)
{
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}

void
BoolAbstractor::visit(LT_node &node)
{
    if (node.mother()->getOtype() == OutType::FLOAT) {
        // create a new variable as the output
        Tvalue& nvar = node_ids[node.id];
        nvar = dir.newAnonymousVar(1);
        //nvar.setSize(1);
    } else {
        NodesToSolver::visit(node);
    }
    if (!Util::hasAssertChild(node) && !Util::hasNotAssertChild(node)) {
        addToInterface(node_ids[node.id], node);
    }
}

void
BoolAbstractor::visit(EQ_node &node)
{
    if (node.mother()->getOtype() == OutType::FLOAT) {
        // create a new variable as the output
        Tvalue& nvar = node_ids[node.id];
        nvar = dir.newAnonymousVar(1);
        //nvar.setSize(1);
    } else {
        NodesToSolver::visit(node);
    }
    if (!Util::hasAssertChild(node) && !Util::hasNotAssertChild(node)) {
        addToInterface(node_ids[node.id], node);
    }
}

void
BoolAbstractor::visit( UFUN_node& node )
{
    const string& name = node.get_ufname();
    if (name == "_cast_int_float_math" || name == "arctan_math" || name == "sin_math" || name == "cos_math" || name == "tan_math" || name == "sqrt_math" || name == "exp_math") {
        return;
    }
    
    Assert(false, "This should not happen");
}

void
BoolAbstractor::visit( ARRACC_node& node )
{
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    addToInterface(node_ids[node.id], node);
}

void
BoolAbstractor::visit( ARRASS_node& node )
{
    Assert(false, "Not yet supported : BoolAbstractor for ARRASS");
}

void
BoolAbstractor::visit( ACTRL_node& node )
{
    Assert(false, "Not yet supported: BoolAbstractor for ACTRL");
}

void
BoolAbstractor::visit( ARR_R_node &node){
    Assert(false, "Not yet supported: BoolAbstractor for ARR_R");
}

void BoolAbstractor::visit( ARR_W_node &node){
    Assert(false, "Not yet supported: BoolAbstractor for ARR_W");
}

void BoolAbstractor::visit( ARR_CREATE_node &node){
    Assert(false, "Not yet supported: BoolAbstractor for ARR_CREATE");
}

void BoolAbstractor::visit( TUPLE_R_node &node){
    if (node.getOtype() == OutType::FLOAT) {
        return;
    } else {
        Assert(false, "This should not happen");
    }
}

void BoolAbstractor::visit (TUPLE_CREATE_node &node) {
    Assert(false, "Not yet supported: BoolAbstractor for TUPLE_CREATE");
}


void
BoolAbstractor::visit (ASSERT_node &node)
{
    NodesToSolver::visit(node);
    //addToInterface(node_ids[node.id], node);
}


void
BoolAbstractor::visit (CONST_node &node)
{	
    if (node.getOtype() == OutType::FLOAT) {
        return;
    }
    NodesToSolver::visit(node);
    //addToInterface(node_ids[node.id], node);
}


/*
void NumericalSynthesizer::getConstraintsOnInputs(SolverHelper* dir, vector<Tvalue>& inputs) {
    return;
    map<string, multimap<double, int>> ctrlToInputIds;
    
    // First, group inputs based on ctrl names
    for (int i = 0; i < inputs.size(); i++) {
        if (imap[i] < 0) continue;
        bool_node* n = (*dag)[imap[i]];
        //cout << n->lprint() << " " << inputs[i] << endl;
        if (n->type == bool_node::LT) {
            bool_node* m = n->mother;
            bool_node* f = n->father;
            
            if (m->type == bool_node::CTRL && f->type == bool_node::CONST) {
                string name = m->get_name();
                double val = ((CONST_node*) f)->getFval();
                if (ctrlToInputIds.find(name) != ctrlToInputIds.end()) {
                    ctrlToInputIds[name].insert(pair<double, int>(val, i)); // it is not possible to have to two nodes with the
                } else {
                    multimap<double, int> ids;
                    ids.insert(pair<double, int>(val, i));
                    ctrlToInputIds[name] = ids;
                }
            }
            
            if (f->type == bool_node::CTRL && m->type == bool_node::CONST) {
                string name = f->get_name();
                double val = ((CONST_node*) m)->getFval();
                if (ctrlToInputIds.find(name) != ctrlToInputIds.end()) {
                    ctrlToInputIds[name].insert(pair<double, int>(val, i));
                } else {
                    multimap<double, int> ids;
                    ids.insert(pair<double, int>(val, i));
                    ctrlToInputIds[name] = ids;
                }
            }
        }
    }
    //cout << ctrlToInputIds.size() << endl;
    // Next, generate constraints for each pair of inputs that have the same ctrl
    for (auto it = ctrlToInputIds.begin(); it != ctrlToInputIds.end(); it++) {
        auto& ids_map = it->second;
        vector<int> ids;
        for (auto id_it = ids_map.begin(); id_it != ids_map.end(); id_it++) {
            //cout << id_it->first << ", ";
            ids.push_back(id_it->second);
        }
        //cout << endl;
        //cout << ids.size() << endl;
        for (int i = 0; i < ids.size() - 1; i++) {
            double t1;
            bool reverse1; // straight is ctrl < const, reverse is const < ctrl
            int a = inputs[ids[i]].getId();
            bool_node* n1 = (*dag)[imap[ids[i]]];
            if (n1->mother->type == bool_node::CTRL) {
                reverse1 = false;
                t1 = ((CONST_node*)n1->father)->getFval();
            } else {
                reverse1 = true;
                t1 = ((CONST_node*)n1->mother)->getFval();
            }
            
            int j = i+1;
            double t2;
            bool reverse2; // straight is ctrl < const, reverse is const < ctrl
            int b = inputs[ids[j]].getId();
            bool_node* n2 = (*dag)[imap[ids[j]]];
            if (n2->mother->type == bool_node::CTRL) {
                reverse2 = false;
                t2 = ((CONST_node*)n2->father)->getFval();
            } else {
                reverse2 = true;
                t2 = ((CONST_node*)n2->mother)->getFval();
            }
            
            if (!reverse1 && !reverse2) { // x < t1, x < t2
                if (t1 < t2) { // a = T => b = T
                    dir->addHelperC(-a, b);
                }
                if (t1 == t2) { // a = b
                    dir->addEquateClause(a, b);
                }
                if (t1 > t2) { // a = F => b = F
                    dir->addHelperC(a, -b);
                }
            } else if (!reverse1 && reverse2) { // x < t1, x > t2
                if (t1 < t2) { // a = T => b = F
                    dir->addHelperC(-a, -b);
                }
                if (t1 == t2) { // a = -b
                    dir->addEquateClause(a, -b);
                }
                if (t1 > t2) { // a = F => b = T
                    dir->addHelperC(a, b);
                }
            } else if (reverse1 && !reverse2) { // x > t1, x < t2
                if (t1 < t2) { // a = F => b = T
                    dir->addHelperC(a, b);
                }
                if (t1 == t2) { // a = -b
                    dir->addEquateClause(a, -b);
                }
                if (t1 > t2) { // a = T => b = F
                    dir->addHelperC(-a, -b);
                }
            } else { // x > t1, x > t2
                if (t1 < t2) { // a = F => b = F
                    dir->addHelperC(a, -b);
                }
                if (t1 == t2) { // a = b
                    dir->addEquateClause(a, b);
                }
                if (t1 > t2) { // a = T => b = T
                    dir->addHelperC(-a, b);
                }
            }
        }
        
    }
}
*/


