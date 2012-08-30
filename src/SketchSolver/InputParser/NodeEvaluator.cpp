#include "NodeEvaluator.h"

// Class for interpreter of BooleanDAG.
NodeEvaluator::NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p):
functionMap(functionMap_p), trackChange(false), failedAssert(false), bdag(bdag_p)
{
	values.resize(bdag.size());
	changes.resize(bdag.size(), false);

}


NodeEvaluator::~NodeEvaluator(void)
{
}

void NodeEvaluator::visit( ARR_R_node &node){
	vector<int>& vv = vecvalues[node.father->id];
	int idx = i(*node.mother);
	if(idx < 0){
		setbn(node, 0 );
	}else{
		setbn(node, idx<vv.size()?vv[idx]:i(*node.father) );
	}
}
void NodeEvaluator::visit( ARR_W_node &node){
	vector<int>& vvout = vecvalues[node.id];
	vector<int>& vvin = vecvalues[node.getOldArr()->id];
	int idx = i(*node.mother);
	vvout = vvin;
	int x = i(*node.getOldArr());
	setbn(node, x);
	if(idx >=0 && idx >= vvout.size()){	
		vvout.resize(idx+1, x );
	}
	if(idx>=0){
		vvout[idx] = i(*node.getNewVal());
	}
}
void NodeEvaluator::visit( ARR_CREATE_node &node){
	vector<int>& vv = vecvalues[node.id];
	int sz = node.multi_mother.size();
	vv.resize(sz);
	for(int t=0; t<sz; ++t){
		vv[t] = i(*node.multi_mother[t]);
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
		vector<int>& vv = vecvalues[node.id];
		vv.resize(node.arrSz);
		VarStore::objP* op = &(inputs->getObj(node.get_name()));
		while(op->next != NULL){
			vv[op->index] = op->getInt();
			op = op->next;
		}

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
				vector<int>& vvout = vecvalues[pred->id];
				vecvalues[node.id] = vvout;
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
			vector<int>& mv = vecvalues[node.mother->id];
			vector<int>& fv = vecvalues[node.father->id];
			int msz = mv.size() > fv.size() ? mv.size() : fv.size();
			bool tt = (i(*node.mother) == i(*node.father));
			for(int jj=0; jj<msz; ++jj){
				int tm = jj < mv.size() ? mv[jj] : i(*node.mother);
				int tf = jj < fv.size() ? fv[jj] : i(*node.father);
				tt = tt && (tm == tf);
				if(!tt){ break; }
			}
		setbn(node, tt);
	}else{
		setbn(node, i(*node.mother) == i(*node.father)); //XXX There is a bug here!!
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
	failedAssert = failedAssert || !t;
	setbn(node, t );
}	


void NodeEvaluator::printNodeValue(int i){
	cout<<i<<" = ";
	if(vecvalues.count(i)>0){
		vector<int>& vv = vecvalues[i];		
		for(int i=0; i<vv.size(); ++i){
			cout<<vv[i]<<", ";
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
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){				
		(*node_it)->accept(*this);
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
	int highest;
	int nconsts = 0;
	for(vector<bool>::iterator it = changes.begin(); it != changes.end(); ++it, ++i){
		bool_node* ni = bdag[i];
		if(!*it && ni->type != bool_node::CONST && !ni->isArrType()){
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
