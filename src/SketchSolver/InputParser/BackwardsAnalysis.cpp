#include "BackwardsAnalysis.h"
#include <typeinfo>


BackwardsAnalysis::BackwardsAnalysis(void)
{
}

BackwardsAnalysis::~BackwardsAnalysis(void)
{
}



CONST_node* BackwardsAnalysis::getCnode(int val){
	if( cnmap.find(val) == cnmap.end() ){
		CONST_node* cnode = new CONST_node(val);
		cnode->id = newnodes.size() + dagsize;		
		newnodes.push_back(cnode);
		cnmap[val] = cnode;
		Dout(cout<<" add "<<cnode->id<<"  "<<cnode->get_name()<<endl);
		return cnode;
	}else{
		return cnmap[val];	
	}
}

CONST_node* BackwardsAnalysis::getCnode(bool c){
	return getCnode( c ? 1 : 0 );
}


void BackwardsAnalysis::visitArith(arith_node& node ){
	Info& t = info[&node];
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	if(node.mother != NULL){
		info[node.mother] += t;
	}

	for(int i=0; i<node.multi_mother.size(); ++i){
		info[node.multi_mother[i]] += t;
	}

	rvalue = &node;
}

void BackwardsAnalysis::visitBool(bool_node& node ){
	Info& t = info[&node];
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	bool_node* fmother = NULL;
	if(node.mother != NULL){
		fmother = modifyNode(node.mother, t);
	}

	bool_node* ffather = NULL;
	if(node.father != NULL){
		ffather = modifyNode(node.father, t);
	}
	
	if(fmother != node.mother || ffather != node.father){
		node.dislodge();
		node.mother = fmother;
		node.father = ffather;
		node.resetId();
		node.addToParents();

	}

	rvalue = &node;
}


void BackwardsAnalysis::visit( DST_node& node ){
	Info& t = info[&node];
	t.makeTop();
	if(node.mother != NULL){
		info[node.mother] += t;
	}
	rvalue = &node;
}

void BackwardsAnalysis::visit( ASSERT_node &node){
	Info& t = info[&node];
	// cout<<"node = "<< node.lprint()<<endl;
	// cout<<t.lprint();
	if(node.mother != NULL){
		info[node.mother] += t;
	}
	rvalue = &node;
}

bool_node* BackwardsAnalysis::localModify(AND_node* node, Info& t){
	int v;

	if(t.getValue(node->mother, v)){	
		if(v==1){
			bool_node* tmp = modifyNode(node->father, t);		
			return tmp;	
		}else{
			return getCnode(0);
			
		}
	}
	if(t.getValue(node->father, v)){		
		if(v==1){
			bool_node* tmp = modifyNode(node->mother, t);		
			return tmp;	
		}else{
			return getCnode(0);
			
		}
	}else{
		return node;
	}
}


void BackwardsAnalysis::visit( AND_node& node ){
	Info& t = info[&node];
	//cout<<"node = "<< node.lprint()<<endl;
	//cout<<t.lprint();
	int v;
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	bool_node* tmp = localModify(&node, t);
	if(tmp != &node){
		rvalue = tmp;
		return ;
	}
	

	t.push(Datum(node.mother));
	bool_node* ffather = modifyNode(node.father, t);
	set<Datum> info;
	t.pop(info);
	
	
	t.push(Datum(node.father));
	t.filter(info);
	bool_node* fmother = modifyNode(node.mother, t);
	t.pop();


	if(fmother != node.mother || ffather != node.father){
		node.dislodge();
		node.mother = fmother;
		node.father = ffather;
		node.resetId();
		node.addToParents();

	}

	rvalue = &node;
	return;
}

/*
bool BackwardsAnalysis::check(Info& t, bool_node* n, int& v){	
	if(!t.getValue(n, v)){
		Datum d(n);
		if(dimp.checkImplications(d.node, d.val==0)){
			vector<pair<bool_node*, int> >& gv = dimp.getimps(d.node, d.val==0);
			for(int i=0; i<gv.size(); ++i){
				if(t.getValue(gv[i].first, v)){
					Assert(gv[i].second == 0, "NYI");
					if(v==1){
						v=0;
						return true;
					}
				}
			}
		}
	}else{
		return true;
	}
	return false;
}
*/

bool_node* BackwardsAnalysis::localModify(OR_node* node, Info& t){
	int v;

	if(t.getValue(node->mother, v)){		
		if(v==1){
			return getCnode(1);
			
		}else{
			bool_node* tmp = modifyNode(node->father, t);		
			return tmp;	
		}
	}

	if(t.getValue(node->father, v)){		
		if(v==1){
			return getCnode(1);
		}else{
			bool_node* tmp = modifyNode(node->mother, t);		
			return tmp;	
			
		}
	}else{
		return node;
	}
}



void BackwardsAnalysis::visit( OR_node& node ){
	Info& t = info[&node];
	//cout<<"node = "<< node.lprint()<<endl;
	//cout<<t.lprint();
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	int v;
	bool_node* tmp = localModify(&node, t);
	if(tmp != &node){
		rvalue = tmp;
		return ;
	}


	t.push(Datum(node.mother, 0));
	bool_node* ffather = modifyNode(node.father, t);
	set<Datum> info;
	t.pop(info);


	t.push(Datum(node.father, 0));
	t.filter(info);
	bool_node* fmother = modifyNode(node.mother, t);
	t.pop();

	if(fmother != node.mother || ffather != node.father){
		node.dislodge();
		node.mother = fmother;
		node.father = ffather;
		node.resetId();
		node.addToParents();

	}


	rvalue = &node;
	return;
}



bool_node* BackwardsAnalysis::localModify(ARRACC_node* node, Info& t){
	int v;

	if(t.getValue(node->mother, v)){
		if(v >= 0 && v < node->multi_mother.size()){
			bool_node* tmp = modifyNode(node->multi_mother[v], t);		
			return tmp;
		}else{
			return getCnode(0);
		}
	}else{
		return node;
	}
}


	
void BackwardsAnalysis::visit( ARRACC_node& node ){
	Info& t = info[&node];
	//cout<<"node = "<< node.lprint()<<endl;
	//cout<<t.lprint();
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	int v;
	bool_node* tmp = localModify(&node, t);
	if(tmp != &node){
		rvalue = tmp;
		return ;
	}

	bool_node* fmother = modifyNode(node.mother, t);
	vector<bool_node*> tmm(node.multi_mother.size());
	bool changed = false;
	for(int i=0; i<node.multi_mother.size(); ++i){
		t.push(Datum(node.mother, i));
		tmm[i] = modifyNode(node.multi_mother[i], t);
		changed = changed || (node.multi_mother[i] != tmm[i]);
		t.pop();
	}

	if(fmother != node.mother || changed){
		node.dislodge();
		node.mother = fmother;
		for(int i=0; i<tmm.size(); ++i){
			node.multi_mother[i] = tmm[i];
		}
		node.resetId();
		node.addToParents();
	}

	rvalue = &node;
	return;
}


bool_node* BackwardsAnalysis::modifyNode(bool_node* node, Info& t){
	bool_node* out = node;
	if(typeid(*node) == typeid(AND_node)){
		out = localModify(dynamic_cast<AND_node*>(node), t);
	}

	if(typeid(*node) == typeid(OR_node)){
		out = localModify(dynamic_cast<OR_node*>(node), t);
	}
	if(typeid(*node) == typeid(ARRACC_node)){
		out = localModify(dynamic_cast<ARRACC_node*>(node), t);
	}
	if(typeid(*node) == typeid(ARRASS_node)){
		out = localModify(dynamic_cast<ARRASS_node*>(node), t);
	}

	if(out == node){
		info[node] += t;
	}
	return out;
}


bool_node* BackwardsAnalysis::localModify(ARRASS_node* node, Info& t){
	int v;

	if(t.getValue(node->mother, v)){
		int idx = ( (v == node->quant) ? 1 : 0 );
		bool_node* tmp = modifyNode(node->multi_mother[idx], t);		
		return tmp;
	}else{
		return node;
	}
}

	
void BackwardsAnalysis::visit( ARRASS_node& node ){
	Info& t = info[&node];
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	int v;
	bool_node* tmp = localModify(&node, t);
	if(tmp != &node){
		rvalue = tmp;
		return ;
	}
	bool_node* fmother = modifyNode(node.mother, t);
	bool_node* fmm0 = modifyNode(node.multi_mother[0], t);
	t.push(Datum(node.mother, node.quant));
	bool_node* fmm1 = modifyNode(node.multi_mother[1], t);
	t.pop();

	if(fmother != node.mother ||
		fmm0 != node.multi_mother[0] ||
		fmm1 != node.multi_mother[1]){
			node.dislodge();
			node.mother = fmother;
			node.multi_mother[0] = fmm0;
			node.multi_mother[1] = fmm1;
			node.resetId();
			node.addToParents();
	}

	rvalue = &node;
	return;
}


void BackwardsAnalysis::process(BooleanDAG& bdag){
	int i=0;
	vector<bool_node*> bn = bdag.getNodesByType(bool_node::ASSERT);
	bool_node* tprev = NULL;
//	dimp.process(bdag);
	for(int i = 0; i<bn.size(); ++i){
		bool_node* cur = bn[i];
		Info& c = info[cur];
		if(tprev != NULL){
			Info& p = info[tprev];
			//cout<<"T = "<<tprev->mother->lprint()<<endl;
			if(tprev->mother->children.size()>1  || tprev->mother->type == bool_node::AND){
//				dimp.checkImplications(tprev->mother);
				p.push(Datum(tprev->mother));			
				c += p;
				p.pop();
			}else{
				c+=p;
			}
		}else{
			c.makeTop();
		}
		tprev = cur;
	}


	for(BooleanDAG::reverse_iterator node_it = bdag.rbegin(); node_it != bdag.rend(); ++node_it, ++i){
		bool_node* node = (*node_it);
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		node->accept(*this);
		bool_node* tmp = rvalue;
		if(tmp != node){
			node->neighbor_replace(tmp);
			//bdag.replace(node->id, tmp);
		}
	}
	bdag.removeNullNodes();
	bdag.addNewNodes(newnodes);	
	newnodes.clear();
	bdag.cleanup();
	//bdag.sort_graph();	
	//bdag.repOK();
	bdag.relabel();
}
