#include "NodeEvaluator.h"
#include <cmath>
#include <iomanip>

// Class for interpreter of BooleanDAG.
NodeEvaluator::NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats):
functionMap(functionMap_p), trackChange(false), failedAssert(false), bdag(bdag_p), floats(_floats)
{
	values.resize(bdag.size());
	changes.resize(bdag.size(), false);
	isset.resize(bdag.size(), false);
	vecvalues.resize(bdag.size(), NULL);
    tuplevalues.resize(bdag.size(), NULL);
	epsilon = floats.epsilon;

}


NodeEvaluator::~NodeEvaluator(void)
{
	for(int i=0; i<vecvalues.size(); ++i){
		if(vecvalues[i] != NULL){
			delete vecvalues[i];
		}
	}
    for(int i=0; i<tuplevalues.size(); ++i){
		if(tuplevalues[i] != NULL){
			delete tuplevalues[i];
		}
	}
}

void NodeEvaluator::visit( ARR_R_node &node){
	cpvec* vv = vecvalues[node.father()->id];
	if(vv==NULL){
		setbn(node, i(*node.father()));
		return;
	}
	int idx = i(*node.mother());
	if(idx < 0){
		setbn(node, 0 );
	}else{
    setbn(node, vv->get(idx,i(*node.father())) );
	}
}

void NodeEvaluator::visit( TUPLE_R_node &node){
  OutType* otp = node.getOtype();
  int itup = i(*node.mother());
  if(itup == -1){
    setbn(node, 0);
    if (otp->isArr) {
      cpvec* cpvt = vecvalues[node.id];
      if (cpvt != NULL) {
        delete cpvt;
      }
      vecvalues[node.id] = NULL;
    }
    return;
  }
  cptuple* cpt = tuplevalues[itup];
	if(cpt==NULL){
    setbn(node, 0);
    if (otp->isArr) {
      cpvec* cpvt = vecvalues[node.id];
      if (cpvt != NULL) {
        delete cpvt;
      }
      vecvalues[node.id] = NULL;
    }
		return;
	}
	int idx = node.idx;
  if(idx < 0 || idx >= cpt->size()){
    setbn(node, 0 );
    if (otp->isArr) {
      cpvec* cpvt = vecvalues[node.id];
      if (cpvt != NULL) {
        delete cpvt;
      }
      vecvalues[node.id] = NULL;
    }
    return;
  }
  
  if(otp==OutType::INT || otp ==OutType::BOOL || otp == OutType::FLOAT || otp->isTuple){
    setbn(node, cpt->vv[idx]);
  } else {
    cpvec* cpvt = vecvalues[node.id];
    if(cpvt != NULL){
      cpvt->update(vecvalues[cpt->vv[idx]], -1, 0);
    }else{
      vecvalues[node.id] = new cpvec(vecvalues[cpt->vv[idx]], -1, 0);
    }
    setbn(node, values[cpt->vv[idx]]);
  }
  
}

void NodeEvaluator::visit( ARR_W_node &node){
	cpvec* vvin = vecvalues[node.getOldArr()->id];	
	int idx = i(*node.mother());
	int x = i(*node.getOldArr());
	setbn(node, x);	
	cpvec* vvo = vecvalues[node.id];
	if(vvo != NULL){
		vvo->update(vvin, idx, i(*node.getNewVal()));
	}else{
		vecvalues[node.id] = new cpvec(vvin, idx, i(*node.getNewVal()));
	}
	if (false && node.id == 197) {
		cout << node.lprint() << ":";
		vecvalues[node.id]->print(cout);
		cout << endl;
		cout << node.getOldArr()->id << "{" << idx << "->" << i(*node.getNewVal()) << "} ";
		vvin->print(cout);
		cout << endl;
	}
}

void NodeEvaluator::visit( ARR_CREATE_node &node){
	
	int sz = node.nargs();
	cpvec* cpv = new cpvec(sz);
	if(vecvalues[node.id] != NULL){
		delete vecvalues[node.id];
	}
	vecvalues[node.id] = cpv;	
	for(int t=0; t<sz; ++t){
		cpv->vv[t] = i(*node.arguments(t));
	}
	// TODO xzl: temporarily disable -333
	
	setbn(node, i(*node.getDfltval()));

	//setbn(node, 0);
}

void NodeEvaluator::visit( TUPLE_CREATE_node &node){
	
	int sz = node.nparents();
	cptuple* cpv = new cptuple(sz);
	if(tuplevalues[node.id] != NULL){
		delete tuplevalues[node.id];
	}
	tuplevalues[node.id] = cpv;
  Tuple* otp = (Tuple*)node.getOtype();
	for(int t=0; t<sz; ++t){
    OutType* e = otp->entries[t];
    if(e==OutType::INT || e==OutType::BOOL || e->isTuple) {
      cpv->vv[t] = i(*node.get_parent(t));
    } else {
      cpv->vv[t] = (node.get_parent(t)->id);
    }
	}
	setbn(node, node.id );
    
}

void NodeEvaluator::visit( AND_node& node ){
	setbn(node, b(*node.mother()) && b(*node.father()));
}
void NodeEvaluator::visit( OR_node& node ){
	setbn(node, b(*node.mother()) || b(*node.father()));
}
void NodeEvaluator::visit( XOR_node& node ){
	setbn(node, b(*node.mother()) ^ b(*node.father()));
}
void NodeEvaluator::visit( SRC_node& node ){
	//cout << "NodeEvaluator SRC " << node.lprint() << " " << node.getArrSz() << " " << node.get_nbits() << endl;
	if(node.arrSz>=0){
		if(vecvalues[node.id] != NULL){
			delete vecvalues[node.id];
		}
		vecvalues[node.id] = new cpvec(node.arrSz, &(inputs->getObj(node.get_name())));
		// for SRC arrays, anything beyond bounds are 0 by default
		setbn(node, 0);
	}else{
		setbn(node, (*inputs)[node.get_name()]);	
	}
}
void NodeEvaluator::visit( DST_node& node ){
	setbn(node, i(*node.mother()));
}
void NodeEvaluator::visit( NOT_node& node ){
	setbn(node, !b(*node.mother()));
}
void NodeEvaluator::visit( CTRL_node& node ){
	// TODO xzl: will we encounter angelic array here?
	//cout << "NodeEvaluator CTRL " << node.lprint() << " " << node.get_Angelic() << " " << node.getArrSz() << " " << node.get_nbits() << endl;
	setbn(node, (*inputs)[node.get_name()]);
}


bool arrayComp(cpvec* mv, cpvec* fv, int mdef, int fdef) {
	int msz = mv == NULL ? 0 : mv->size();
	int fsz = fv == NULL ? 0 : fv->size();
	msz = msz > fsz ? msz : fsz;

	bool tt = (mdef == fdef);
	for (int jj = 0; jj<msz; ++jj) {
		int tm = mv == NULL ? mdef : mv->get(jj, mdef);
		int tf = fv == NULL ? fdef : fv->get(jj, fdef);
		tt = tt && (tm == tf);
		if (!tt) {
			break;
		}
	}
	return tt;
}

bool NodeEvaluator::argcomp(bool_node::parent_iter parents_beg, bool_node::parent_iter parents_end, vector<int>& v1, vector<int>& v2) {
	for (int jj = 0; jj < v1.size(); ++jj) {
		if (parents_beg[jj]->isArrType()) {
			cpvec* vv1 = this->vecvalues[v1[jj]];
			cpvec* vv2 = this->vecvalues[v2[jj]];
			if (!arrayComp(vv1, vv2, values[v1[jj]], values[v2[jj]])) {
				return false;
			}
		}else{
			if (v1[jj] != v2[jj]) {
				return false;
			}
		}
	}
	return true;
}


void NodeEvaluator::builtinRetVal(UFUN_node& node, int idxval) {
	string tuple_name = node.getTupleName();

	Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
	int size = tuple_type->actSize;
	Assert(size == 1, "BAD");
	cptuple* cpv = new cptuple(size);
	if (tuplevalues[node.id] != NULL) {
		delete tuplevalues[node.id];
	}
	cpv->vv[0] = idxval;
	tuplevalues[node.id] = cpv;
	setbn(node, node.id);
}


bool NodeEvaluator::checkKnownFun(UFUN_node& node) {
	const string& name = node.get_ufname();
	if (name == "_cast_int_float_math") {
		int val = i(*node.arguments(0));
		builtinRetVal(node, floats.getIdx((float)val));
		return true;
	}
	if (floats.hasFun(name)) {
		int val = i(*node.arguments(0));
		builtinRetVal(node, floats.getFun(name)(val));
		return true;
	}	
	return false;
}


void NodeEvaluator::visit( UFUN_node& node ){

	if (checkKnownFun(node)) {
		return;
	}


	vector<pair<int, vector<int> > >& args = funargs[node.get_ufname()];
	vector<int> cargs;
	for(int ii=0; ii<node.nargs(); ++ii){
		bool_node* pred = node.arguments(ii);
		if (pred->isArrType()) {
			cargs.push_back(pred->id);
		}else {
			cargs.push_back(i(*pred));
		}		
	}
	for(int jj = 0; jj<args.size(); ++jj){
		if(argcomp(node.arg_begin(), node.arg_end(), args[jj].second, cargs)){
			setbn(node, args[jj].first);
			return;
		}
	}
	

	string tuple_name = node.getTupleName();

	Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
	int size = tuple_type->actSize;

	cptuple* cpv = new cptuple(size);
	if(tuplevalues[node.id] != NULL){
		delete tuplevalues[node.id];
	}
	tuplevalues[node.id] = cpv;
	int arrcnt = 0;

	for (int j = 0; j < size ; j++) {
		stringstream sstr;
		sstr<<node.get_ufname()<<"_"<<node.get_uniquefid()<<"_"<<j;
		OutType* type = tuple_type->entries[j];
		Assert(!type->isTuple, "NYS");
		if(type->isArr){
			if(arrcnt == 0){
				if(vecvalues[node.id] != NULL){
					delete vecvalues[node.id];
				}
				VarStore::objP& op = inputs->getObj(sstr.str());
				
				vecvalues[node.id] = new cpvec(op.arrSize(), &(op));
			}else{
				Assert(false, "Multiple return arrays not yet implemented");
			}
			arrcnt++;
			cpv->vv[j] = node.id;
		}else{
			int val = (*inputs)[sstr.str()];
			cpv->vv[j] = val;
		}
	}

	
	args.push_back(make_pair(node.id, cargs));
	setbn(node, node.id);
	return;	
	/*
	vector<bool_node*>& mm = node.multi_mother;
	visitArith(node);
	VarStore tmp = inputs;
	BooleanDAG* callee = functionMap[node.get_name()];
	vector<bool_node*>& inodes = callee->getNodesByType(bool_node::SRC);
	Assert(inodes.size()== mm.size(), "The sizes don't match!!");
	for(int t=0; t<node.multi_mother.size(); ++t){
		tmp.setVarVal(inodes[t]->get_name(), i(*mm[t]));
	}
	vector<bool_node*>& outnodes = callee->getNodesByType(bool_node::DST);	

	Assert(outnodes.size() == 1, "THis is bad");

	bool_node* outnode = outnodes[0];

	recursives.insert(make_pair(&node, NodeEvaluator(functionMap, inputs)));
	recursives[&node].process(*callee);
	
	map<UFUN_node*, NodeEvaluator>::iterator it = recursives.find(&node);

	setbn(node, it->second.i(*outnode));
	*/
}
void NodeEvaluator::visit( ARRACC_node& node ){
	int idx = i(*node.mother());
	if( idx >= node.nargs() || idx < 0){
		setbn(node, 0);
	}else{
		bool_node* pred = node.arguments(idx);
		OutType* otp = node.getOtype();
		if(otp==OutType::INT || otp ==OutType::BOOL || otp->isTuple){
			setbn(node, i(*pred) );
		}
        else{
			
            cpvec* cpvt = vecvalues[node.id];
            if(cpvt != NULL){
                cpvt->update(vecvalues[pred->id], -1, 0);
            }else{
                vecvalues[node.id] = new cpvec(vecvalues[pred->id], -1, 0);
            }
            setbn(node, i(*pred) );
			
		}
	}	
}

void NodeEvaluator::visit(PLUS_node& node) {
	if (node.getOtype() == OutType::FLOAT) {
		float mval = floats.getFloat(i(*node.mother()));
		float fval = floats.getFloat(i(*node.father()));
		int idx = floats.getIdx(mval + fval);
		setbn(node, idx);
		return;
	}
	setbn(node, i(*node.mother()) + i(*node.father()));
}
void NodeEvaluator::visit(TIMES_node& node) {
	if (node.getOtype() == OutType::FLOAT) {
		float mval = floats.getFloat(i(*node.mother()));
		float fval = floats.getFloat(i(*node.father()));
		int idx = floats.getIdx(mval * fval);
		setbn(node, idx);
		return;
	}
	setbn(node, i(*node.mother()) * i(*node.father()));
}

void NodeEvaluator::visit( DIV_node& node ){	
	if (node.getOtype() == OutType::FLOAT) {
		float mval = floats.getFloat(i(*node.mother()));
		float fval = floats.getFloat(i(*node.father()));
		if (abs(fval) < epsilon) {
			if (fval >= 0.0) {
				fval = epsilon;
			}else {
				fval = -epsilon;
			}
		}
		int idx = floats.getIdx(mval / fval);
		setbn(node, idx);
		return;
	}
	int tt = i(*node.father());
	setbn(node, tt == 0 ? 0 : (i(*node.mother()) / tt));
}
void NodeEvaluator::visit( MOD_node& node ){
	int tt = i(*node.father());
	setbn(node, tt == 0? 0 : (i(*node.mother()) % tt));
}
void NodeEvaluator::visit( NEG_node& node ){
	//No need to check if it is float or not, even if it is float, 
	//negating the index will have the effect of negating the value.
	setbn(node, -i(*node.mother()));
}
void NodeEvaluator::visit( CONST_node& node ){
	if (isset[node.id]) {
		//if it's already set, no need to set it again. it's a constant after all.
		return;
	}

	if (node.isFloat()) {
		setbn(node, floats.getIdx(node.getFval()));
	} else {
		setbn(node, node.getVal());		
	}	
}

void NodeEvaluator::visit( LT_node& node ){
	if (node.mother()->getOtype() == OutType::FLOAT) {
		setbn(node, floats(i(*node.mother())) < floats(i(*node.father())));
	} else {
		setbn(node, i(*node.mother()) < i(*node.father()));
	}	
}





void NodeEvaluator::visit( EQ_node& node ){

	if(node.mother()->getOtype() == OutType::BOOL_ARR 
		|| node.mother()->getOtype() == OutType::INT_ARR
		|| node.father()->getOtype() == OutType::BOOL_ARR
		|| node.father()->getOtype() == OutType::INT_ARR){
			cpvec* mv = vecvalues[node.mother()->id];
			cpvec* fv = vecvalues[node.father()->id];
			bool tt = arrayComp(mv, fv, i(*node.mother()), i(*node.father()));
		setbn(node, tt);
	}else{
		setbn(node, i(*node.mother()) == i(*node.father())); 
	}
}
/*!
    multi-mother[0] = old-value;
    multi-mother[1] = new-value;
    if( mother == quant ) return multi-mother[1]; else return multi-mother[0];		
*/
void NodeEvaluator::visit( ARRASS_node& node ){
	setbn(node, i(*node.mother()) == node.quant ? i(*node.getNewVal()) : i(*node.getOldVal()));
}
void NodeEvaluator::visit( ACTRL_node& node ){
	auto sz = node.nparents();
	vector<int> v(sz);
	for(int t=0; t<sz; ++t){
		v[t] = i(*node.get_parent(t));
	}
	setbn(node, intFromBV(v, 0, v.size()));
}
void NodeEvaluator::visit( ASSERT_node &node){
	bool t = b(*node.mother());

	if(node.isNormal()){
		failedAssert = failedAssert || !t;
	}else{
		failedHAssert = failedHAssert || !t;
	}	
	setbn(node, t );
}	


void NodeEvaluator::printNodeValue(int i){
	cout<<i<<" = ";
	if(vecvalues[i]!= NULL){
		cpvec* vv = vecvalues[i];		
		for(int j=0; j<vv->size(); ++j){
			cout<<vv->get(j, values[j])<<", ";
		}
		cout<<endl;
		if (false && (i == 197 || i==194)) {
			vv->print(cout);
			cout << endl;
		}
	}else{
		cout<<values[i]<<endl;
	}
}


bool NodeEvaluator::run(VarStore& inputs_p){
	funargs.clear();
	inputs = &inputs_p;
	int i=0;
	failedAssert = false;
	failedHAssert = false;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){				
		(*node_it)->accept(*this);
		if(failedAssert){
			return true;
		}
		if(failedHAssert){
			return false;
		}
	}
	return false;
}

void NodeEvaluator::display(ostream& out){
	for(int i=0; i<values.size(); ++i){
		out << "AVALS= [" << bdag[i]->globalId << "]" << bdag[i]->lprint();
		if (bdag[i]->getOtype() == OutType::FLOAT) {
			out<<std::fixed;
			out << std::setprecision(9);
			out << "	v=" << floats.getFloat(values[i]) << endl;
		} else {
			out << "	v=" << values[i] << endl;
		}
	}
}


int NodeEvaluator::scoreNodes(int start /*=0*/){
	int i=start;
	float maxcount = -10.0;
	int highest= -1;
	int nconsts = 0;
	for(vector<bool>::iterator it = changes.begin()+start; it != changes.end(); ++it, ++i){
		bool_node* ni = bdag[i];
		{
			int mx = 0;
			for (auto it = ni->p_begin(); it != ni->p_end(); ++it) {
				int t = (*it)->layer;
				mx = ((t + 1)> mx) ? t + 1 : mx;
			}

			ni->layer = mx;
		}



        if(!*it && this->isset[i] && ni->type != bool_node::CONST && !ni->isArrType() && ni->type != bool_node::ASSERT && !ni->getOtype()->isTuple && ni->getOtype() != OutType::FLOAT ){
            ++nconsts;
			int count = 0;
			for(child_iter cit = ni->children.begin(); cit != ni->children.end(); ++cit){
				if((*cit)->type == bool_node::ASSERT && ((ASSERT_node*)(*cit))->isAssume()){
					break;
				}
				if(!changes[(*cit)->id]){
					++count;
					if((*cit)->mother() == ni || (*cit)->father() == ni){
						++count;
					}
				}
			}

			if((count/(float)ni->layer) > maxcount){
				// cout<<i<<": count = "<<count<<" layer = "<<ni->layer<<" ratio "<< (count/(float)ni->layer) <<"   "<<ni->lprint()<<endl;
				highest = i;
				maxcount = (count/(float)ni->layer);
			}else{
				// if(count>0) cout<<i<<": TRIED count = "<<count<<" layer = "<<ni->layer<<" ratio "<< (count/(float)ni->layer) <<"   "<<ni->lprint()<<endl;
			}
		}
	}
	Dout (cout<<"nconstants  = "<<nconsts<<"max count = "<<maxcount<<" highest = "<<highest<<endl;)
	Dout (cout<<bdag[highest]->lprint()<<endl;)
	/*
	for(child_iter cit = bdag[highest]->children.begin(); cit != bdag[highest]->children.end(); ++cit){
		cout<<(*cit)->lprint()<<endl;
	}*/
	return highest;
}
