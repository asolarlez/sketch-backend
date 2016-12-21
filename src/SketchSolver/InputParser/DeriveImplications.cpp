#include "DeriveImplications.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"

DeriveImplications::DeriveImplications(void)
{
}

DeriveImplications::~DeriveImplications(void)
{
}

bool DeriveImplications::checkImplications(bool_node* n, bool rev){
	map<int, vector<pair<bool_node*, int> > >::iterator it = implications.find(rev? -(n->id): n->id);	
	if(it == implications.end()){
		return false;
	}else{
		return true;;
	}
}
vector<pair<bool_node*, int> >& DeriveImplications::getimps(bool_node* n, bool rev){
	return implications[rev? -(n->id): n->id];
}


pair<bool_node*, bool_node*> DeriveImplications::idx(bool_node& n){
	if(n.mother->id < n.father->id){
		return make_pair(n.mother, n.father);
	}else{
		return make_pair(n.father, n.mother);
	}
}
bool DeriveImplications::isequiv(bool_node* n1, bool_node* n2){
	Assert(n1 != n2, "This is bad");
	if(n1->type == bool_node::LT){
		
	}
	return false;
}
int DeriveImplications::getid(bool_node& n){
	if(n.type != bool_node::NOT){
		return n.id;
	}else{
		return -(n.mother->id);
	}
}


void DeriveImplications::visit( LT_node& node ){
	ineqmap[ idx(node) ].push_back(&node);
}

void DeriveImplications::visit( EQ_node& node ){
	ineqmap[ idx(node) ].push_back(&node);
}
void DeriveImplications::visit( ASSERT_node &node){

}


void DeriveImplications::process(BooleanDAG& bdag){
	int i=0;
	tmpdag = &bdag;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		(*node_it)->accept(*this);
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    	}
	}
	for(lmap::iterator it = ineqmap.begin(); it != ineqmap.end(); ++it){		
		vector<bool_node*>& v = it->second;
		if(v.size() > 1){
			for(int i=0; i<v.size(); ++i){
				for(int j=i+1; j<v.size(); ++j){
					bool_node* n1 = v[i];
					bool_node* n2 = v[j];
					if(isequiv(n1, n2)){
						vector<pair<bool_node*, int> > & vt1 = implications[getid(*n1)];
						vt1.push_back(make_pair(n2, 1));
						vector<pair<bool_node*, int> > & vt2 = implications[getid(*n2)];
						vt2.push_back(make_pair(n1, 1));
					}else{
						vector<pair<bool_node*, int> > & vt1 = implications[getid(*n1)];
						vt1.push_back(make_pair(n2, 0));
						vector<pair<bool_node*, int> > & vt2 = implications[getid(*n2)];
						vt2.push_back(make_pair(n1, 0));
					}
				}
			}
		}
	}

}