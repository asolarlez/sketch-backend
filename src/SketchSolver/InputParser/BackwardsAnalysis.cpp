#include "BackwardsAnalysis.h"

BackwardsAnalysis::BackwardsAnalysis(void)
{
}

BackwardsAnalysis::~BackwardsAnalysis(void)
{
}



CONST_node* BackwardsAnalysis::getCnode(int val){
	if( cnmap.find(val) == cnmap.end() ){
		CONST_node* cnode = new CONST_node(val);
		setTimestamp(cnode);
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
	if(node.mother != NULL){
		info[node.mother] += t;
	}

	if(node.father != NULL){
		info[node.father] += t;
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
	t.makeTop();
	if(node.mother != NULL){
		info[node.mother] += t;
	}
	rvalue = &node;
}


void BackwardsAnalysis::visit( AND_node& node ){
	Info& t = info[&node];
	int v;
	if(t.getValue(node.mother, v)){
		info[node.father] += t;
		if(v==1){
			rvalue = node.father;
			
		}else{
			rvalue = getCnode(0);
			
		}
		return;
	}

	if(t.getValue(node.father, v)){
		info[node.mother] += t;
		if(v==1){
			rvalue = node.mother;
			
		}else{
			rvalue = getCnode(0);
			
		}
		return;
	}

	t.push(Datum(node.mother));
	info[node.father] += t;
	t.pop();

	t.push(Datum(node.father));
	info[node.mother] += t;
	t.pop();

	rvalue = &node;
	return;
}

void BackwardsAnalysis::visit( OR_node& node ){
	Info& t = info[&node];
	int v;
	if(t.getValue(node.mother, v)){
		info[node.father] += t;
		if(v==1){
			rvalue = getCnode(1);
			
		}else{
			rvalue = node.father;			
			
		}
		return;
	}

	if(t.getValue(node.father, v)){
		info[node.mother] += t;
		if(v==1){
			rvalue = getCnode(1);
			
		}else{
			rvalue = node.mother;
			
		}
		return;
	}

	t.push(Datum(node.mother, 0));
	info[node.father] += t;
	t.pop();

	t.push(Datum(node.father, 0));
	info[node.mother] += t;
	t.pop();

	rvalue = &node;
	return;
}
	
void BackwardsAnalysis::visit( ARRACC_node& node ){
	Info& t = info[&node];
	int v;
	if(t.getValue(node.mother, v)){
		info[node.multi_mother[v]] += t;
		rvalue = node.multi_mother[v];
		
		return;
	}
	
	info[node.mother] += t;
	for(int i=0; i<node.multi_mother.size(); ++i){
		t.push(Datum(node.mother, i));
		info[node.multi_mother[i]] += t;
		t.pop();
	}
	rvalue = &node;
	return;
}
	
void BackwardsAnalysis::visit( ARRASS_node& node ){
	Info& t = info[&node];
	int v;
	if(t.getValue(node.mother, v)){
		int idx = ( (v == node.quant) ? 1 : 0 );
		info[node.multi_mother[idx]] += t;
		rvalue = node.multi_mother[idx];
		
		return;
	}
	
	info[node.mother] += t;
	info[node.multi_mother[0]] += t;
	t.push(Datum(node.mother, node.quant));
	info[node.multi_mother[1]] += t;
	t.pop();
	rvalue = &node;
	return;
}


void BackwardsAnalysis::process(BooleanDAG& bdag){
	int i=0;
	for(BooleanDAG::reverse_iterator node_it = bdag.rbegin(); node_it != bdag.rend(); ++node_it, ++i){
		bool_node* node = (*node_it);
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		node->accept(*this);
		bool_node* tmp = rvalue;
		if(tmp != node){
			
			bdag.replace(node->id, tmp);
		}
	}
	bdag.removeNullNodes();
	bdag.addNewNodes(newnodes);
	// bdag.repOK();
	newnodes.clear();
	bdag.sort_graph();
	bdag.cleanup(false);
	bdag.relabel();
}
