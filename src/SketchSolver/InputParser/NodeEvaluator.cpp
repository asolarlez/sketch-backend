#include "NodeEvaluator.h"

// Class for interpreter of BooleanDAG.
NodeEvaluator::NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p):
functionMap(functionMap_p), trackChange(false), failedAssert(false), bdag(bdag_p)
{
	values.resize(bdag.size());
	changes.resize(bdag.size(), false);
	vecvalues.resize(bdag.size(), NULL);
    tuplevalues.resize(bdag.size(), NULL);

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
	cpvec* vv = vecvalues[node.father->id];
	if(vv==NULL){
		setbn(node, i(*node.father));
		return;
	}
	int idx = i(*node.mother);
	if(idx < 0){
		setbn(node, 0 );
	}else{
    setbn(node, vv->get(idx,i(*node.father)) );
	}
}

void NodeEvaluator::visit( TUPLE_R_node &node){
  OutType* otp = node.getOtype();
  int itup = i(*node.mother);
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
  
  if(otp==OutType::INT || otp ==OutType::BOOL || otp->isTuple){
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
	int idx = i(*node.mother);
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
	
	int sz = node.multi_mother.size();
	cpvec* cpv = new cpvec(sz);
	if(vecvalues[node.id] != NULL){
		delete vecvalues[node.id];
	}
	vecvalues[node.id] = cpv;	
	for(int t=0; t<sz; ++t){
		cpv->vv[t] = i(*node.multi_mother[t]);
	}
	// TODO xzl: temporarily disable -333
	setbn(node, node.dfltval);
	//setbn(node, 0);
}

void NodeEvaluator::visit( TUPLE_CREATE_node &node){
	
	int sz = node.multi_mother.size();
	cptuple* cpv = new cptuple(sz);
	if(tuplevalues[node.id] != NULL){
		delete tuplevalues[node.id];
	}
	tuplevalues[node.id] = cpv;
  Tuple* otp = (Tuple*)node.getOtype();
	for(int t=0; t<sz; ++t){
    OutType* e = otp->entries[t];
    if(e==OutType::INT || e==OutType::BOOL || e->isTuple) {
      cpv->vv[t] = i(*node.multi_mother[t]);
    } else {
      cpv->vv[t] = (node.multi_mother[t]->id);
    }
	}
	setbn(node, node.id );
    
}

void NodeEvaluator::visit( AND_node& node ){
	setbn(node, b(*node.mother) && b(*node.father));
}
void NodeEvaluator::visit( OR_node& node ){
	setbn(node, b(*node.mother) || b(*node.father));
}
void NodeEvaluator::visit( XOR_node& node ){
	setbn(node, b(*node.mother) ^ b(*node.father));
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
	setbn(node, i(*node.mother));
}
void NodeEvaluator::visit( NOT_node& node ){
	setbn(node, !b(*node.mother));
}
void NodeEvaluator::visit( CTRL_node& node ){
	// TODO xzl: will we encounter angelic array here?
	//cout << "NodeEvaluator CTRL " << node.lprint() << " " << node.get_Angelic() << " " << node.getArrSz() << " " << node.get_nbits() << endl;
	setbn(node, (*inputs)[node.get_name()]);
}
void NodeEvaluator::visit( PLUS_node& node ){
	setbn(node, i(*node.mother) + i(*node.father));
}
void NodeEvaluator::visit( TIMES_node& node ){
	setbn(node, i(*node.mother) * i(*node.father));
}
void NodeEvaluator::visit( UFUN_node& node ){
	Assert(false, "NYI; jngyttyuy");
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
	int idx = i(*node.mother);
	if( idx >= node.multi_mother.size() || idx < 0){
		setbn(node, 0);
	}else{
		bool_node* pred = node.multi_mother[idx];
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

void NodeEvaluator::visit( DIV_node& node ){
	int tt = i(*node.father);
	setbn(node, tt == 0 ? 0 : (i(*node.mother) / tt));
}
void NodeEvaluator::visit( MOD_node& node ){
	int tt = i(*node.father);
	setbn(node, tt == 0? 0 : (i(*node.mother) % tt));
}
void NodeEvaluator::visit( NEG_node& node ){
	setbn(node, -i(*node.mother));
}
void NodeEvaluator::visit( CONST_node& node ){
	setbn(node, node.getVal());
}

void NodeEvaluator::visit( LT_node& node ){
	setbn(node, i(*node.mother) < i(*node.father));
}

void NodeEvaluator::visit( EQ_node& node ){

	if(node.mother->getOtype() == OutType::BOOL_ARR 
		|| node.mother->getOtype() == OutType::INT_ARR
		|| node.father->getOtype() == OutType::BOOL_ARR
		|| node.father->getOtype() == OutType::INT_ARR){
			cpvec* mv = vecvalues[node.mother->id];
			cpvec* fv = vecvalues[node.father->id];
			int msz = mv==NULL? 0 : mv->size() ;
			int fsz = fv==NULL? 0 : fv->size() ;			
			msz = msz > fsz ? msz : fsz;

			bool tt = (i(*node.mother) == i(*node.father));
			for(int jj=0; jj<msz; ++jj){
				int tm = mv==NULL?  i(*node.mother) :mv->get(jj, i(*node.mother));
				int tf = fv==NULL?  i(*node.father) :fv->get(jj, i(*node.father));
				tt = tt && (tm == tf);
				if(!tt){
					if (false) {
						cout << "not EQ! " << node.mother->lprint() << "[" << jj << "]=" << tm << " vs " << node.father->lprint() << "[" << jj << "]=" << tf << endl;
						cout << "i(): " << i(*node.mother) << " " << i(*node.father) << endl;
						cout << "mv:";
						mv->print(cout);
						cout << " fv:";
						fv->print(cout);
						cout << endl;
					}
					break;
				}
			}
		setbn(node, tt);
	}else{
		setbn(node, i(*node.mother) == i(*node.father)); 
	}
}
/*!
    multi-mother[0] = old-value;
    multi-mother[1] = new-value;
    if( mother == quant ) return multi-mother[1]; else return multi-mother[0];		
*/
void NodeEvaluator::visit( ARRASS_node& node ){
	setbn(node, i(*node.mother) == node.quant ? i(*node.multi_mother[1]) : i(*node.multi_mother[0]));
}
void NodeEvaluator::visit( ACTRL_node& node ){
	vector<int> v(node.multi_mother.size());
	for(int t=0; t<node.multi_mother.size(); ++t){
		v[t] = i(*node.multi_mother[t]);
	}
	setbn(node, intFromBV(v, 0, v.size()));
}
void NodeEvaluator::visit( ASSERT_node &node){
	bool t = b(*node.mother);

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
		cout<<"AVALS= ["<<bdag[i]->globalId<<"]"<<bdag[i]->lprint()<<"	v="<<values[i]<<endl;
	}
}


int NodeEvaluator::scoreNodes(int start /*=0*/){
	int i=start;
	int maxcount = -10;
	int highest= -1;
	int nconsts = 0;
	for(vector<bool>::iterator it = changes.begin()+start; it != changes.end(); ++it, ++i){
		bool_node* ni = bdag[i];
		{
			int mx = 0;
			if(ni->mother != NULL){ mx = ni->mother->layer + 1; }
			if(ni->father != NULL){ int t = ni->father->layer; mx = ((t + 1)> mx) ? t+1 : mx; }
			arith_node* an = dynamic_cast<arith_node*>(ni);
			if(an != NULL){
				for(int i=0; i<an->multi_mother.size(); ++i){
					int t = an->multi_mother[i]->layer; 
					mx = ((t + 1)> mx) ? t+1 : mx;
				}
			}
			ni->layer = mx;
		}



        if(!*it && ni->type != bool_node::CONST && !ni->isArrType() && ni->type != bool_node::ASSERT && ni->type != bool_node::TUPLE_CREATE){
            ++nconsts;
			int count = 0;
			for(child_iter cit = ni->children.begin(); cit != ni->children.end(); ++cit){
				if(!changes[(*cit)->id]){
					++count;
					if((*cit)->mother == ni || (*cit)->father == ni){
						++count;
					}
				}
			}
			if(count > maxcount + 3 && count > (ni->layer / 10)){
				// cout<<i<<": count = "<<count<<" layer = "<<ni->layer<<"   "<<ni->lprint()<<endl;
				highest = i;
				maxcount = count;
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
