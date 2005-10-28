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
	map<bool_node*, int> node_ids;
	map<bool_node*, vector<int> > num_ranges;
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		switch((*node_it)->type){
			case bool_node::AND:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"AND "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addAndClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::OR:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"OR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addOrClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);				
				break;
			}
			case bool_node::XOR:{
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"XOR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addXorClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::SRC:{
				int iid = (*node_it)->ion_pos;
				node_ids[*node_it] = dir.getArr(IN, iid);
				Dout(cout<<"REGISTERING "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::CTRL:{
				int iid = (*node_it)->ion_pos;
				node_ids[*node_it] = dir.getArr(CTRL, iid);
				Dout(cout<<"CONTROL "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::DST:{
				int oid = (*node_it)->ion_pos;		
				int nvar = dir.getArr(outname, oid);
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<outname<<"["<<oid<<"]="<<(*node_it)->mother->name<<" "<<node_ids[(*node_it)->mother]<<"  "<<(*node_it)->mother<<endl);
				addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::PT:{	
				int nvar = dir.newAnonymousVar();
				int msign = (*node_it)->mother_sgn? 1 : -1;
				addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);			
				break;
			}
			case bool_node::ARITH:{
				processArithNode(mng, dir, dynamic_cast<arith_node*>(*node_it), node_ids, num_ranges);
			}
			
		}
	}
}
template<typename COMP>
void processComparissons(SAT_Manager mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	vector<int> one(1,1);
	bool_node* mother = anode->mother;			
	bool hasMother = (mother != NULL);
	vector<int>& mrange = hasMother? num_ranges[mother] : one;
	int mid = hasMother? node_ids[mother] : -1;
	
	bool_node* father = anode->father;			
	bool hasFather = (father != NULL);
	vector<int>& frange = hasFather? num_ranges[father] : one;
	int fid = hasFather? node_ids[father] : -1;
	
	if(fid == -1){
		fid = dir.newAnonymousVar();
		setVarClause(mng, fid);
	}
	if(mid == -1){
		mid = dir.newAnonymousVar();
		setVarClause(mng, mid);
	}			
	int cvar, cvar2;
	cvar2 = dir.newAnonymousVar();
	setVarClause(mng, -cvar2);
	COMP comp;
	Dout(cout<<"SIZES = "<<mrange.size()<<", "<<frange.size()<<endl);
	for(int i=0; i<mrange.size(); ++i){
		for(int j=0; j<frange.size(); ++j){
			Dout(cout<<"COMPARING "<<anode->mother_quant*mrange[i]<<", "<<anode->father_quant*frange[j]<<endl);
			if(comp(anode->mother_quant*mrange[i], anode->father_quant*frange[j])){
				cvar = dir.newAnonymousVar();
				addAndClause(mng, cvar, mid + i, fid + j);
				cvar2 = dir.newAnonymousVar();
				addOrClause(mng, cvar2, cvar, cvar-1);
			}	
		}
	}
	int result = dir.newAnonymousVar();
	node_ids[anode] = result;
	addEqualsClause(mng, result, cvar2);
	return;	
}


void SolveFromInput::processArithNode(SAT_Manager mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	switch(anode->arith_type){
		case arith_node::GE:{
			processComparissons<greater_equal<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::LT:{
			processComparissons<less<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::LE:{
			processComparissons<less_equal<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::GT:{
			processComparissons<greater<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::PLUS:{
			vector<int>& nrange = num_ranges[anode->mother];
			int id = node_ids[anode->mother];			
			num_ranges[anode].resize(nrange.size());
			vector<int>& tmp = num_ranges[anode];
			Dout(cout<<"ADDING "<<anode->father_quant<<"  MULTIPLYIN  "<<anode->mother_quant<<endl);
			for(int i=0; i<nrange.size(); ++i){
				tmp[i] = anode->mother_quant*nrange[i]+anode->father_quant;
			}
			node_ids[anode] = id;
			return;
		}
		case arith_node::ACTRL:{
			int size = anode->multi_mother.size();
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			int id = node_ids[*it];
			for( ; it != anode->multi_mother.end(); ++it){
				Assert(node_ids[*it]-id>=0 && node_ids[*it]-id<size, "THIS SHOULDN'T HAPPEN");
			}
			varRange vr = getSwitchVars(mng,dir, id, size);
			num_ranges[anode].resize(vr.range);
			vector<int>& tmp = num_ranges[anode];
			for(int i=0; i<vr.range; ++i){
				tmp[i] = i;
			}
			node_ids[anode] = vr.varID;
			return;
		}
		case arith_node::ARRACC:{
			vector<int>& nrange = num_ranges[anode->mother];
			int id = node_ids[anode->mother];
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			vector<int> choices(anode->multi_mother.size());
			for(int i=0; it != anode->multi_mother.end(); ++i, ++it){
				Dout(cout<<"choice "<<i<<" = "<<node_ids[*it]<<endl);
				choices[i] = node_ids[*it];				
			}
			int cvar, cvar2;
			cvar2 = dir.newAnonymousVar();
			setVarClause(mng, -cvar2);
			for(int i=0; i<nrange.size(); ++i){
				if( nrange[i] >= 0 && nrange[i] < choices.size() ){
					cvar = dir.newAnonymousVar();
					addAndClause(mng, cvar, choices[nrange[i]], id + i);
					cvar2 = dir.newAnonymousVar();
					addOrClause(mng, cvar2, cvar, cvar-1);
				}
			}
			int result = dir.newAnonymousVar();
			node_ids[anode] = result;
			addEqualsClause(mng, result, cvar2);
			return;
		}
	}
}

