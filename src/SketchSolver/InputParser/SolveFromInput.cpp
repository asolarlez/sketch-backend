#include "SolveFromInput.h"

#include <map>

void SolveFromInput::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, Nout);
	dir.declareArr(OUT, Nout);
	translator(mng, dir, sketch, SOUT);
}

void SolveFromInput::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );		
	translator(mng, dir, spec, OUT);
}


void SolveFromInput::translator(SAT_Manager mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		map<bool_node*, int> node_ids;
		switch((*node_it)->type){
			case bool_node::AND:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				addAndClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::OR:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				addOrClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);				
				break;
			}
			case bool_node::XOR:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				addXorClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::SRC:{
				int iid = (*node_it)->ion_pos;
				node_ids[*node_it] = dir.getArr(IN, iid);
				cout<<"REGISTERING "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<endl;
				break;
			}
			case bool_node::DST:{
				int oid = (*node_it)->ion_pos;		
				int nvar = dir.getArr(outname, oid);
				int msign = (*node_it)->mother_sgn? 1 : -1;
				cout<<outname<<"["<<oid<<"]="<<(*node_it)->mother->name<<" "<<node_ids[(*node_it)->mother]<<endl;
				addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::PT:{	
				int nvar = dir.newAnonymousVar();
				int msign = (*node_it)->mother_sgn? 1 : -1;
				addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);			
				break;
			}
		}
	}
}