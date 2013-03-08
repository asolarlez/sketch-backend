#include "NodeEvaluator.h"

// Class for interpreter of BooleanDAG.
NodeEvaluator::NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p):
functionMap(functionMap_p), trackChange(false), hasValidResult(false), failedAssert(false), bdag(bdag_p)
{
	values.resize(bdag.size());
	changes.resize(bdag.size(), false);
	vecvalues.resize(bdag.size(), NULL);

}


NodeEvaluator::~NodeEvaluator(void)
{
	for(int i=0; i<vecvalues.size(); ++i){
		if(vecvalues[i] != NULL){
			delete vecvalues[i];
		}
	}
}

void NodeEvaluator::visit( ARR_R_node &node){
	cpvec* vv = vecvalues[node.father->id];
	int idx = i(*node.mother);
	if(idx < 0){
		setbn(node, 0 );
	}else{
		setbn(node, vv->get(idx,i(*node.father)) );
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
	setbn(node, -333 );
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
	if(node.arrSz>=0){
		if(vecvalues[node.id] != NULL){
			delete vecvalues[node.id];
		}
		vecvalues[node.id] = new cpvec(node.arrSz, &(inputs->getObj(node.get_name())));		
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
	setbn(node, (*inputs)[node.get_name()]);
}
void NodeEvaluator::visit( PLUS_node& node ){
	setbn(node, i(*node.mother) + i(*node.father));
}
void NodeEvaluator::visit( TIMES_node& node ){
	setbn(node, i(*node.mother) * i(*node.father));
}
void NodeEvaluator::visit( UFUN_node& node ){
	Assert(false, "NYI");
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
		bool_node::OutType otp = node.getOtype();
		if(otp==bool_node::INT || otp ==bool_node::BOOL){
			setbn(node, i(*pred) );
		}else{
			{
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

	if(node.mother->getOtype() == bool_node::BOOL_ARR 
		|| node.mother->getOtype() == bool_node::INT_ARR
		|| node.father->getOtype() == bool_node::BOOL_ARR
		|| node.father->getOtype() == bool_node::INT_ARR){
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
				if(!tt){ break; }
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
	if(node.isHard()){
		failedHAssert = failedHAssert || !t;
	}else{
		failedAssert = failedAssert || !t;
	}	
	setbn(node, t );
}	


void NodeEvaluator::printNodeValue(int i){
	cout<<i<<" = ";
	if(vecvalues[i]!= NULL){
		cpvec* vv = vecvalues[i];		
		for(int i=0; i<vv->size(); ++i){
			cout<<vv->get(i, values[i])<<", ";
		}
		cout<<endl;
	}else{
		cout<<values[i]<<endl;
	}
}


bool NodeEvaluator::run(VarStore& inputs_p){
	inputs = &inputs_p;
	int i=0;
	failedAssert = false;
	failedHAssert = false;
	pendingChanges.clear();
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){				
		(*node_it)->accept(*this);
	}
	if (failedHAssert) {
		return false;
	}
	if (hasValidResult) {
		if (trackChange) {
			for (vector<int>::const_iterator it = pendingChanges.begin(); it != pendingChanges.end(); ++it) {
				changes[*it] = true;
				//cout << "changing " << *it << " " << bdag[*it]->lprint() << endl;
			}
		}
	} else {
		hasValidResult = true;
		validResult = values;
	}
	return failedAssert;
}

void NodeEvaluator::display(ostream& out){
	for(int i=0; i<values.size(); ++i){
		cout<<"AVALS= ["<<bdag[i]->globalId<<"]"<<bdag[i]->lprint()<<"	v="<<values[i]<<endl;
	}
}


int NodeEvaluator::scoreNodes(){
	int i=0;
	int maxcount = -10;
	int highest= -1;
	int nconsts = 0;
	for(vector<bool>::iterator it = changes.begin(); it != changes.end(); ++it, ++i){
		bool_node* ni = bdag[i];
		cout << *it << " " << ni->lprint() << endl;
		if(!*it && ni->type != bool_node::CONST && !ni->isArrType() && ni->type != bool_node::ASSERT){
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
			if(count > maxcount + 3){
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
