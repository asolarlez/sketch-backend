#include "BackwardsAnalysis.h"
#include <typeinfo>


string Info::lprint(){
	stringstream str;
	for(FastMap<bool_node, int>::iterator it = known.begin(); it != known.end(); ++it){
		str<<it->first->lprint()<<"\t ~> "<<it->second<<", "<<endl;
	}
	str<<"::"<<endl;
	for(FastMap<bool_node, int>::iterator it = temp.begin(); it != temp.end(); ++it){
		str<<it->first->lprint()<<"\t ~> "<<it->second<<", "<<endl;
	}
	return str.str();
}

BackwardsAnalysis::BackwardsAnalysis(void)
{
}

BackwardsAnalysis::~BackwardsAnalysis(void)
{
}


bool valueSearch(bool_node* node,FastMap<bool_node,int>& fm, int&out, int bnd){
	if(bnd < 0){ return false; }
	if(node->type == bool_node::NOT){
		bool tmp = valueSearch(node->mother, fm, out, bnd);
		out = 1-out;
		return tmp;
	}
	FastMap<bool_node,int>::iterator vit =  fm.find(node);
	if(vit!= fm.end()){		
		out = vit->second;
		return true;
	}
	if(node->type == bool_node::AND){
		int tv1;
		bool tmp1 = valueSearch(node->mother, fm, tv1, bnd-1);
		if(tmp1 == true && tv1 == 0){
			out = tv1;
			return true;
		}
		int tv2;
		bool tmp2 = valueSearch(node->father, fm, tv2, bnd-1);
		if(tmp2 == true && tv2 == 0){
			out = tv2;
			return true;
		}
		if(tmp1==true && tmp2==true){
			out = ((tv1==1)&&(tv2==1))?1:0;
			return true;
		}
		return false;
	}
	return false;
}



CONST_node* BackwardsAnalysis::getCnode(int val){
	if( cnmap.find(val) == cnmap.end() ){
		CONST_node* cnode = new CONST_node(val);
		//Make sure dagsize gets initialized!!
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
	//cout << "visitArith" << endl;
	Info& t = info[node.id];
	//cout << "after t=info[node.id]" << endl;
	if(t.isBottom()){
		rvalue = getCnode(0);
		//cout << "after rvalue=getCnode" << endl;
		return;
	}
	/* This seems unprofitable; maybe later we'll discover it's useful.
	int tout;
	if(t.getValue(&node, tout)){
		rvalue = getCnode(tout);
		return;
	}
	*/
	if(node.mother != NULL){
		info[node.mother->id] += t;
		//cout << "after info[mother]+=t" << endl;
	}

	for(int i=0; i<node.multi_mother.size(); ++i){
		info[node.multi_mother[i]->id] += t;
		//cout << "after info[multi_mother]+=t" << endl;
	}

	rvalue = &node;
	//cout << "after rvalue=&node" << endl;
}

void BackwardsAnalysis::visitBool(bool_node& node ){
	Info& t = info[node.id];
	if(t.isBottom()){
		rvalue = getCnode(0);
		return;
	}
	int tout;
	/* This seems unprofitable; maybe later we'll discover it's useful.
	if(t.getValue(&node, tout)){
		rvalue = getCnode(tout);
		return;
	}
	*/
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
	Info& t = info[node.id];
	t.makeTop();
	if(node.mother != NULL){
		info[node.mother->id] += t;
	}
	rvalue = &node;
}

void BackwardsAnalysis::visit( ASSERT_node &node){
	Info& t = info[node.id];
	// cout<<"node = "<< node.lprint()<<endl;
	// cout<<t.lprint();
	if(node.mother != NULL){
		info[node.mother->id] += t;
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
	Info& t = info[node.id];
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
	FastMap<bool_node, int> info;
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
	Info& t = info[node.id];
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
	FastMap<bool_node, int> info;
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
//cout << "ARRACC" << endl;
	if(t.getValue(node->mother, v)){
//cout << "after t.getValue" << endl;
		if(v >= 0 && v < node->multi_mother.size()){
//cout << "before modifyNode" << endl;
			bool_node* tmp = modifyNode(node->multi_mother[v], t);		
//cout << "after modifyNode" << endl;
			return tmp;
		}else{
//cout << "return Cnode(0)" << endl;
			return getCnode(0);
		}
	}else{
//cout << "return node" << endl;
		return node;
	}
}


	
void BackwardsAnalysis::visit( ARRACC_node& node ){
	//cout << "ARRACC_node visit" << endl;
	Info& t = info[node.id];
	//cout<<"node = "<< node.lprint()<<endl;
	//cout<<t.lprint();
	//cout << "after t=info[]" << endl;
	if(t.isBottom()){
		//cout << "rvalue=getCnode(0)" << endl;
		rvalue = getCnode(0);
		return;
	}
	int v;
	//cout << "before localModify" << endl;
	bool_node* tmp = localModify(&node, t);
	//cout << "after localModify" << endl;
	if(tmp != &node){
		rvalue = tmp;
		return ;
	}

	//cout << "before modifyNode(mother)" << endl;
	bool_node* fmother = modifyNode(node.mother, t);
	//cout << "after modifyNode(mother)" << endl;
	vector<bool_node*> tmm(node.multi_mother.size());
	bool changed = false;
	for(int i=0; i<node.multi_mother.size(); ++i){
		t.push(Datum(node.mother, i));
		//cout << "before modifyNode(multi_mother[i])" << endl;
		tmm[i] = modifyNode(node.multi_mother[i], t);
		//cout << "after modifyNode(multi_mother[i])" << endl;
		changed = changed || (node.multi_mother[i] != tmm[i]);
		t.pop();
	}

	if(fmother != node.mother || changed){
		//cout << "before dislodge" << endl;
		node.dislodge();
		//cout << "after dislodge" << endl;
		node.mother = fmother;
		for(int i=0; i<tmm.size(); ++i){
			//cout << "multi_mother[i]=tmm[i]" << endl;
			node.multi_mother[i] = tmm[i];
		}
		//cout << "resetId" << endl;
		node.resetId();
		//cout << "addToParents" << endl;
		node.addToParents();
	}

	rvalue = &node;
	//cout << "return" << endl;
	return;
}


bool_node* BackwardsAnalysis::modifyNode(bool_node* node, Info& t){
	//cout<<" modifying "<<node->lprint()<<" sz = "<<t.getSize()<<endl;
	bool_node* out = node;
	if(node->type == bool_node::NOT){
		bool_node* tmp = modifyNode(node->mother, t);
		if(tmp != node->mother){
			if(tmp->type == bool_node::CONST){
				out = this->getCnode(1-dynamic_cast<CONST_node*>(tmp)->getVal());
			}else{
				NOT_node* nn = new NOT_node();
				nn->mother = tmp;				
				nn->addToParents();
				this->addNode(nn);
				out=nn;
			}
		}
	}
	if(node->type == bool_node::AND){
		out = localModify(dynamic_cast<AND_node*>(node), t);
	}

	if(node->type == bool_node::OR){
		out = localModify(dynamic_cast<OR_node*>(node), t);
	}
	
	if(node->type == bool_node::ARRACC){
		out = localModify(dynamic_cast<ARRACC_node*>(node), t);
	}
	
	if(node->type == bool_node::ARRASS){
		out = localModify(dynamic_cast<ARRASS_node*>(node), t);
	}

	if(out == node){
		info[node->id] += t;
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
	Info& t = info[node.id];
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
	dagsizeSet(bdag.size());
	int i=0;
	
	vector<bool_node*> bn = bdag.getNodesByType(bool_node::ASSERT);
	bool_node* tprev = NULL;
//	dimp.process(bdag);
	
	info.resize(bdag.size());
	//cout << "info.size=" << info.size();
	//cout << "ba: dagsize=" << dagsize << endl;
	for(int i = 0; i<bn.size(); ++i){
		bool_node* cur = bn[i];
		Info& c = info[cur->id];
		if(tprev != NULL){
			Info& p = info[tprev->id];
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

	i=0;
	//cout << "reverse begin" << endl;
	for(BooleanDAG::reverse_iterator node_it = bdag.rbegin(); node_it != bdag.rend(); ++node_it, ++i){
		//cout << "i=" << i;
		bool_node* node = (*node_it);
		//cout << "node: " << node->id << endl;
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		//cout<<node->get_name()<<":"<<node->id<<endl;
		node->accept(*this);
		//cout<<"after accept" << endl;
		bool_node* tmp = rvalue;
		//cout<<"after rvalue" << endl;
		info[node->id].clear();
		//cout<<"after clear" << endl;
		if(tmp != node){
			//cout<<"i will replace " << node->get_name() << " with " << tmp->get_name() << endl;
			node->neighbor_replace(tmp);
			//bdag.replace(node->id, tmp);
		}
		//cout<<"after all" << endl;
	}
	//cout << "reverse end" << endl;
	bdag.removeNullNodes();
	if (false) { for (int i=0; i<newnodes.size(); i++) {
		bool_node * node = newnodes[i];
		Assert(node->type == bool_node::CONST, "node->type=" << node->type);
		int val = dynamic_cast<CONST_node*>(node)->getVal();
		Assert(cnmap[val]==node, "cnmap["<<val<<"]="<<cnmap[val]<<"!="<<node);
		int id = node->id;
		if (id != dagsize+i) {
			cout << "Wrong node->id=" << id << "when i=" << i << "dagsize=" << dagsize << endl;
			exit(1);
		}
	} }
	bdag.addNewNodes(newnodes);	
	newnodes.clear();
	bdag.cleanup();
	//bdag.sort_graph();	
	//bdag.repOK();
	bdag.relabel();
}
