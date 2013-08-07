#include "DagOptimizeCommutAssoc.h"

#include <algorithm>
#include <iterator>

DagOptimizeCommutAssoc::DagOptimizeCommutAssoc(void)
{
}

DagOptimizeCommutAssoc::~DagOptimizeCommutAssoc(void)
{


}



bool_node* CAoptimizer::lnewNode(bool_node* mother, bool_node* father){
	bool_node* bn = newNode(bnType);
	
	bn->mother = mother;
	bn->father = father;

	bn->addToParents();		

	parent->addNode(bn);
	return bn;
}


bool_node* CAoptimizer::mergeInputs(int input1, int input2, map<inputId, inputNode >& inputToInterf, map<int, set<int> >& interfToInput, map<pair<int, int>, int>& distances, bool mngDistances){

	inputNode& in1 = inputToInterf[input1];
	inputNode& in2 = inputToInterf[input2];	
	//First, we create a new boolean node corresponding to the merge of the two old nodes:
	bool_node* node = lnewNode(in1.node, in2.node);	
	//Then we create the new input node.
	inputNode& out = inputToInterf[node->id];
	out.node = node;
	//The set of interf nodes for the new input node is the intersection of the set of interfs of input1 and input2.
	set_intersection(in1.interfs.begin(), in1.interfs.end(), in2.interfs.begin(), in2.interfs.end(), inserter(out.interfs, out.interfs.begin()));
	
	Assert(out.interfs.size()>0, "The intersection can't be empty if we are going to join them");
	//Now, we have to go to all the interf nodes in out.interfs, and replace input1 and input2 with out
	

	
	for(set<int>::iterator it = out.interfs.begin(); it != out.interfs.end(); ++it){
		set<int>& cset = interfToInput[*it];
		cset.erase(input1);
		cset.erase(input2);
		cset.insert(node->id);
	}

	//Finally, we need to remove out.interfs from in1.interfs and from in2.interfs. If any of those sets becomes
	//empty, we must remove the set itself.
	

	for(set<int>::iterator it = out.interfs.begin(); it != out.interfs.end(); ++it){
		in1.interfs.erase(*it);
		in2.interfs.erase(*it);
	}
	bool erin1 = false;
	bool erin2 = false;
	if(in1.interfs.size() == 0){		
		erin1 = true;
		for(map<int, int>::iterator itb = in1.distances.begin(); itb != in1.distances.end(); ++itb){
			distances.erase(make_pair(input1, itb->first));
		}
		inputToInterf.erase(input1);
	}

	if(in2.interfs.size() == 0){		
		erin2 = true;
		for(map<int, int>::iterator itb = in2.distances.begin(); itb != in2.distances.end(); ++itb){
			distances.erase(make_pair(input2, itb->first));
		}
		inputToInterf.erase(input2);
	}

	if(mngDistances){
		for(map<inputId, inputNode >::iterator it= inputToInterf.begin(); it != inputToInterf.end(); ++it){
			inputNode& in = it->second;						

			if(in.distances.count(input1)){
				if(erin1){
					in.distances.erase(input1);
					distances.erase( make_pair(it->first, input1) );
				}else{
					int d = nodeDistance(in1, in);
					if(d != 0){
						in.distances[input1] = d;
						distances[make_pair(it->first, input1)] = d;
					}else{
						in.distances.erase(input1);
						distances.erase( make_pair(it->first, input1) );
					}
				}
			}
			
			if(in.distances.count(input2)){
				if(erin2){
					in.distances.erase(input2);
					distances.erase( make_pair(it->first, input2) );
				}else{
					int d = nodeDistance(in2, in);
					if(d != 0){
						in.distances[input2] = d;
						distances[make_pair(it->first, input2)] = d;
					}else{
						in.distances.erase(input2);
						distances.erase( make_pair(it->first, input2) );
					}
				}
			}

			if(it->first == input1 && !erin1){
				for(map<int, int>::iterator itb = in1.distances.begin(); itb != in1.distances.end(); ++itb){
					if(itb->first != input2){
						inputNode& suc = inputToInterf[itb->first];
						int d = nodeDistance(in1, suc);
						if(d != 0){
							itb->second = d;
							distances[make_pair(input1, itb->first)] = d;
						}else{
							distances.erase( make_pair(input1, itb->first)  );
						}
					}
				}
			}

			
			if(it->first == input2 && !erin2){
				for(map<int, int>::iterator itb = in2.distances.begin(); itb != in2.distances.end(); ++itb){
					if(itb->first != input1){
						inputNode& suc = inputToInterf[itb->first];
						int d = nodeDistance(in2, suc);
						if(d != 0){
							itb->second = d;
							distances[make_pair(input2, itb->first)] = d;
						}else{
							distances.erase( make_pair(input2, itb->first)  );
						}
					}
				}
			}

			if(it->first != node->id){
				Assert(it->first < node->id, "This should be true because the new node gets a bigger ID than all previous nodes");
				int d = nodeDistance(in, out);
				if(d!=0){
					in.distances[node->id] = d;
					distances[make_pair(it->first, node->id)] = d;
				}
			}

		}

	}

	return node;
}


bool_node* CAoptimizer::getNode(int id){
	return parent->getNode(id);
}

bool_node* CAoptimizer::replacement(bool_node* bn){

	if(interfToInput.count(bn->id) > 0){
		set<int>& s = interfToInput[bn->id];
		Assert( s.size() == 1, "This can't be happening");
		return inputToInterf[*s.begin()].node;
	}else{
		return bn;
	}

}


void CAoptimizer::computeCommonSubs(){
	//The first step is to group the interface nodes into equivalence classes.
	
	for(interfIter it = interfaces.begin(); it != interfaces.end(); ++it){		
		pair<int, set<inputId>* > & p = *it;
		//The set in this pair says, for each interface node, which input nodes flow to it.
		interfToInput[p.first] = *p.second;
		for(set<int>::iterator sit = p.second->begin(); sit != p.second->end(); ++sit){
			inputToInterf[*sit].interfs.insert(p.first);
		}
	}

	for(map<inputId, inputNode >::iterator it = inputToInterf.begin(); it != inputToInterf.end(); ++it){
		it->second.node = getNode(it->first);
	}
	map<pair<int, int>, int> distances;	
	//Now, all the input nodes with the same set must be coallesced into a single node.
	
	{
		map<set<int>*, int, setComp> current;
		vector<set<int> > tmp;
		for(map<inputId, inputNode>::iterator it = inputToInterf.begin(); it != inputToInterf.end(); ++it){
			map<set<int>*, int, setComp>::iterator f = current.find(&it->second.interfs);
			if(f != current.end()){
				tmp[f->second].insert(it->first);
			}else{
				current[&it->second.interfs] = tmp.size(); 
				set<int> t;
				t.insert(it->first);
				tmp.push_back(t); 
			}
		}

		for(int i=0; i<tmp.size(); ++i){
			map<set<int>*, int ,setComp>::iterator it = setMap.find( &tmp[i]);
			if(it != setMap.end()){
				int newid = it->second;
				inputNode& ninput = inputToInterf[newid];
				ninput.node = getNode(newid);
				set<int>& affectedInterfs = inputToInterf[*tmp[i].begin()].interfs;
				for(set<int>::iterator v = affectedInterfs.begin(); v != affectedInterfs.end(); ++v){
					set<int>& cset = interfToInput[*v];
					for(set<int>::iterator t = tmp[i].begin(); t != tmp[i].end(); ++t){
						cset.erase(*t);
					}
					cset.insert(newid);
					ninput.interfs.insert(*v);
				}
				for(set<int>::iterator t = tmp[i].begin(); t != tmp[i].end(); ++t){
					inputToInterf.erase(*t);
				}
			}else{
				set<int>::iterator t = tmp[i].begin();
				int prev = *t;
				++t;
				for(; t != tmp[i].end(); ++t){
					prev = mergeInputs( prev, *t, inputToInterf, interfToInput, distances)->id;
				}	
			}
		}
	}
	//So by this point, all the nodes in inputToInterf have distinct sets of interfaces. So now the next step is to start merging
	//until all the interface nodes have a single input node.
	
	//Now, we build the network of weights between pairs of nodes that will help us decide which nodes to merge.
	
	
	for(map<inputId, inputNode >::iterator it= inputToInterf.begin(); it != inputToInterf.end(); ++it){
		
		map<inputId, inputNode >::iterator it2 = it;
		it2++;
		for( ; it2 != inputToInterf.end(); ++it2){
			inputNode& in1 = it->second;
			int d = nodeDistance(in1, it2->second);
			if(d != 0){
				in1.distances[it2->first] = d;
				distances[make_pair( it->first, it2->first)] = d;
			}
		}
	}
	

	while(true){
		pair<int, int> p = selectPairToMerge(distances);
		if(p.first == -1 && p.second == -1){ break; }
		mergeInputs(p.first, p.second, inputToInterf, interfToInput, distances, true);
	}


}



pair<int, int> CAoptimizer::selectPairToMerge(map<pair<int, int>, int>& distances){
	int m = 0;
	pair<int, int> rv(-1, -1);
	for(map<pair<int, int>, int>::iterator it = distances.begin(); it != distances.end(); ++it){
		
		if(it->second > m){
			m = it->second;
			rv = it->first;
		}
	}
	distances.erase(rv);
	return rv;
}









bool_node* CAoptimizer::addGeneral(bool_node& bn, bool isInternal){
	set<int>* cset = NULL;
	cset = new set<int>();
	if(bn.father->type == bn.type){
		set<int>* tmp = getSet(bn.father);
		cset->insert(tmp->begin(), tmp->end());
	}else{		
		cset->insert(bn.father->id);
	}
	if(bn.mother->type == bn.type){
		set<int>* tmp =  getSet(bn.mother);
		cset->insert(tmp->begin(), tmp->end());
	}else{
		cset->insert(bn.mother->id);
	}

	bool_node* tmp = existingNode(cset);
	if(tmp == NULL){
		if(isInternal){
			addInternal(bn, cset);
		}else{
			addInterface(bn, cset);
		}
		return &bn;
	}else{
		delete cset;
		return tmp;
	}
}


bool_node* CAoptimizer::existingNode(set<int>* cset){
	setMapIter it  =setMap.find(cset);
	if(it != setMap.end()){
		return getNode(it->second);
	}else{
		return NULL;
	}
}




set<int>* CAoptimizer::getSet(bool_node* bn){
	return setMapInv[bn->id];
}

void CAoptimizer::addInternal(bool_node& bn, set<int>* cset){
	setMapInv[bn.id] = cset;
	setMap[cset] = bn.id;
}

void CAoptimizer::addInterface(bool_node& bn, set<int>* cset){
	addInternal(bn, cset);
	interfaces.push_back(make_pair(bn.id, cset));
}


bool_node* CAoptimizer::checkNode(bool_node& bn){
	
	//Old: Solitary nodes aren't consider. Only nodes that belong to connected components of similar nodes.
	//New: I am now considering solitary nodes too.
	bool good = false;
	bool isInterface = false;
	for(child_iter it = bn.children.begin(); it != bn.children.end(); ++it){
		if((*it)->type == bn.type){
			good = true;			
		}else{
			isInterface = true;
		}
		if(good && isInterface){ 
			break; 
		}
	}

	//First, check if any of the parents are of the same type.	
	if(bn.father->type == bn.type || bn.mother->type == bn.type){
	//If they are, then we need to see if it is an internal or an interface node.
		return addGeneral(bn, !isInterface);
	}else{
	//otherwise, we need to see if any of the children are of the same type.
		return addGeneral(bn, !isInterface);
		/*
		if(good){
			return addGeneral(bn, !isInterface);
		}else{
			return &bn;
		}
		*/
	}
}

void DagOptimizeCommutAssoc::visitACNode(bool_node& bn){
	optiterator oit = optimizers.find(bn.type);
	if(oit == optimizers.end()){
		CAoptimizer& caop = optimizers[bn.type];
		caop.setDag(dag);
		caop.setType(bn.type);
		caop.setParent(this);
		rvalue = caop.checkNode(bn);
	}else{
		rvalue = oit->second.checkNode(bn);
	}
	
}

void DagOptimizeCommutAssoc::visit( AND_node& node ){
	visitACNode(node);
}

void DagOptimizeCommutAssoc::visit( OR_node& node ){
	visitACNode(node);
}

/*
void DagOptimizeCommutAssoc::visit( XOR_node& node ){
	visitACNode(node);
}

void DagOptimizeCommutAssoc::visit( PLUS_node& node ){
	visitACNode(node);
}

void DagOptimizeCommutAssoc::visit( TIMES_node& node ){
	visitACNode(node);
}
*/


bool_node* DagOptimizeCommutAssoc::getNode(int id){

	if(id < dagsize){
		return (*dag)[id];
	}else{
		return newnodes[id - dagsize];
	}
}

void DagOptimizeCommutAssoc::process(BooleanDAG& bdag){
	

	dagsize = bdag.size();
	dag = &bdag;
	timerclass tc("prepass");
	tc.start();
	for(int i=0; i<bdag.size(); ++i){
		// Get the code for this node. 		
		bdag[i]->accept(*this);
		bool_node* n = rvalue;

		if(n != bdag[i]){
			bdag.replace(i, n);
		}
	}
	tc.stop().print();
	
	timerclass tc2("ccs");
	tc2.start();
	for(map<bool_node::Type, CAoptimizer>::iterator it = optimizers.begin(); it != optimizers.end(); ++it){
		it->second.computeCommonSubs();		
	}
	tc2.stop().print();

	timerclass tc3("replacement");
	tc3.start();
	for(int i=0; i<bdag.size(); ++i){
		if(bdag[i] != NULL){
			bool_node::Type t = bdag[i]->type;
			if(t == bool_node::AND || t == bool_node::OR){
				bool_node* tmp = optimizers[t].replacement(bdag[i]);
				if(tmp != bdag[i]){
					bdag.replace(i, tmp);
				}
			}
		}
	}
	tc3.stop().print();

	bdag.removeNullNodes();
	bdag.addNewNodes(newnodes);
	//bdag.repOK();
	newnodes.clear();
	//bdag.sort_graph();
	bdag.cleanup();
	bdag.relabel();
}
