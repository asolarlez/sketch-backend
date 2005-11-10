#include "SolveFromInput.h"

#include <map>
#ifndef INTEGERBOUND
#define INTEGERBOUND 8192
#endif


vector<int> scratchpad(100);

void SolveFromInput::addInputsToTestSet(int input[], int insize){
	int N = getInSize();
	int k = 0;
	int ctrl = 0;
	int numRepeat = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		if((*node_it)->type == bool_node::SRC){
			int iid = (*node_it)->ion_pos;
			if(input[iid % insize] == last_input[iid]){
				++numRepeat;
				(*node_it)->flag = false;
			}else{				
				(*node_it)->flag = true;
			}
			++k;
		}else{
			if(	(*node_it)->type == bool_node::CTRL ){
				(*node_it)->flag = firstTime;
				++ctrl;
			}
		}
	}
	firstTime = false;
	cout<<"* RECYCLED "<<numRepeat<<" values out of "<<N<<endl;
	Assert(k == N, "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
	Assert(ctrl == getCtrlSize(), "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<ctrl<<" CONTROLS"<<endl);
	k=0;
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it){
		if((*node_it)->type == bool_node::SRC){
			int iid = (*node_it)->ion_pos;
			if(input[iid % insize] == last_input[iid]){
				(*node_it)->flag = false;
			}else{
				last_input[iid] = input[iid % insize];
				(*node_it)->flag = true;
			}
			++k;
		}else{
			Assert( (*node_it)->type != bool_node::CTRL, "Specs don't have unknowns!!");
		}
	}
	Assert(k == N, "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
	FindCheckSolver::addInputsToTestSet(input, insize);
}


SolveFromInput::SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, int NS_p):CTRL("_C"){
	N = spec_p->get_n_inputs();
	Nout = spec_p->get_n_outputs();
	spec = spec_p;
	sketch = sketch_p;
  	sketch->cleanup();
  	spec->cleanup();
    sketch->sort_graph();
    spec->sort_graph();
    Dout( cout<<"sketch->get_n_controls() = "<<sketch->get_n_controls()<<"  "<<sketch<<endl );
	declareControl(CTRL, sketch->get_n_controls());
	nseeds = NS_p;
	cout<<"Random seeds = "<<nseeds<<endl;	
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it){
		(*node_it)->flag = true;
	}
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		(*node_it)->flag = true;
	}
	firstTime=true;
}


void SolveFromInput::setupCheck(){
	FindCheckSolver::setupCheck();
	last_input = new int[getInSize()];
	for(int i=0; i<getInSize(); ++i){ last_input[i] = 0; };
}


void SolveFromInput::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, Nout);
	dir.declareArr(OUT, Nout);
	YES = dir.newAnonymousVar();
	setVarClause(mng, YES);
	translator(mng, dir, sketch, SOUT);
}

void SolveFromInput::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );		
	translator(mng, dir, spec, OUT);
}

void SolveFromInput::output_control_map(ostream& out){
	FindCheckSolver::ctrl_iterator ar = begin();
	Assert( ar != NULL, "THIS CAN't HAPPEN");	
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		if((*node_it)->type == bool_node::CTRL){
			int iid = (*node_it)->ion_pos;
			out<<(*node_it)->name<<"\t"<<ar[iid]<<endl;
		}
	}
}
void SolveFromInput::translator(SAT_Manager mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	node_ids[NULL] = YES;

	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		switch((*node_it)->type){
			case bool_node::AND:{
				if(!checkParentsChanged(*node_it, true)){ break; }
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"AND "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addAndClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::OR:{
				if(!checkParentsChanged(*node_it, true)){ break; }
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"OR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addOrClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);				
				break;
			}
			case bool_node::XOR:{
				if(!checkParentsChanged(*node_it, true)){ break; }			
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
				addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);
				break;
			}
			case bool_node::PT:{	
				if(!checkParentsChanged(*node_it, true)){ break; }
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
	int cvar;
	COMP comp;
	Dout(cout<<"SIZES = "<<mrange.size()<<", "<<frange.size()<<endl);
	int orTerms = 0;
	for(int i=0; i<mrange.size(); ++i){
		for(int j=0; j<frange.size(); ++j){
			Dout(cout<<"COMPARING "<<anode->mother_quant*mrange[i]<<", "<<anode->father_quant*frange[j]<<endl);
			if(comp(anode->mother_quant*mrange[i], anode->father_quant*frange[j])){
				cvar = dir.newAnonymousVar();
				addAndClause(mng, cvar, mid + i, fid + j);
//				cvar2 = dir.newAnonymousVar();
//				addOrClause(mng, cvar2, cvar, cvar-1);
				++orTerms;
				if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
				scratchpad[orTerms] = cvar;
			}	
		}
	}
	int result = dir.newAnonymousVar();
	scratchpad[0] = result;
	addBigOrClause(mng, &scratchpad[0], orTerms);
	node_ids[anode] = result;
	
	return;	
}

template<typename THEOP>
void processArith(SAT_Manager mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	THEOP comp;
			if(anode->father == NULL){
				vector<int>& nrange = num_ranges[anode->mother];
				int id = node_ids[anode->mother];			
				num_ranges[anode].resize(nrange.size());
				vector<int>& tmp = num_ranges[anode];
				Dout(cout<<"ADDING "<<anode->father_quant<<"  MULTIPLYIN  "<<anode->mother_quant<<endl);
				for(int i=0; i<nrange.size(); ++i){
					tmp[i] = comp(anode->mother_quant*nrange[i], anode->father_quant);
				}
				node_ids[anode] = id;
			}else{
				vector<int>& nrange = num_ranges[anode->mother];
				vector<int>& frange = num_ranges[anode->father];
				map<int, int> numbers;								
				int mid = node_ids[anode->mother];			
				int fid = node_ids[anode->father];
				num_ranges[anode].resize(0);
				vector<int>& tmp = num_ranges[anode];
				tmp.reserve(nrange.size()*frange.size());
				Dout(cout<<"ADDING "<<anode->father_quant<<"  MULTIPLYIN  "<<anode->mother_quant<<endl);
				int vals = 0;
				for(int i=0; i<nrange.size(); ++i){
					for(int j=0; j<frange.size(); ++j){
						int quant = comp(anode->mother_quant*nrange[i], anode->father_quant*frange[j]);
						if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
						Dout(cout<<"QUANT = "<<quant<<"          "<<mid+i<<", "<<fid + j<<endl);
						if(numbers.find(quant) != numbers.end()){
							int cvar = dir.newAnonymousVar();		
							addAndClause(mng, cvar, mid+i, fid + j);
							int cvar2 = dir.newAnonymousVar();
							addOrClause(mng, cvar2, cvar, numbers[quant]);
							numbers[quant] = cvar2;
						}else{
							int cvar = dir.newAnonymousVar();		
							addAndClause(mng, cvar, mid+i, fid + j);
							tmp.push_back(quant);
							numbers[quant] = cvar;							
							++vals;	
						}
					}
				}
				Dout(cout<<"tmp size = "<<tmp.size()<<endl);
				int newID = dir.getVarCnt();
				for(int i=0; i<vals; ++i){
					int quant = tmp[i];
					int cvar = dir.newAnonymousVar();
					Dout(cout<<"quant = "<<quant<<endl);
					addEqualsClause(mng, cvar, numbers[quant]);
				}
				node_ids[anode] = newID;
			}
}



void SolveFromInput::processArithNode(SAT_Manager mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	switch(anode->arith_type){
		case arith_node::GE:{
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<greater_equal<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::LT:{
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<less<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::LE:{
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<less_equal<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::GT:{
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<greater<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::PLUS:{
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<plus<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::TIMES:{
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<multiplies<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::ACTRL:{
			int size = anode->multi_mother.size();
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			bool parentSame = true;
			vector<int> ids(anode->multi_mother.size());
			for(int i=0 ; it != anode->multi_mother.end(); ++it, ++i, ++signs){
				ids[i]=((*signs)==1?1:-1)*node_ids[*it];
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}
			if(!checkParentsChanged(anode, parentSame)){ break; }
			varRange vr = getSwitchVars(mng,dir, ids, size);
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
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			vector<int> choices(anode->multi_mother.size());
			bool parentSame = true;
			for(int i=0; it != anode->multi_mother.end(); ++i, ++it, ++signs){				
				choices[i] = ((*signs)==1?1:-1)*node_ids[*it];
				Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}
			
			if(!checkParentsChanged(anode, parentSame)){ break; }
			int cvar;
			int orTerms = 0;
			for(int i=0; i<nrange.size(); ++i){
				if( nrange[i] >= 0 && nrange[i] < choices.size() ){
					cvar = dir.newAnonymousVar();
					addAndClause(mng, cvar, choices[nrange[i]], id + i);
					++orTerms;
					if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms] = cvar;
				}
			}
			int result = dir.newAnonymousVar();
			node_ids[anode] = result;
			scratchpad[0] = result;
			addBigOrClause(mng, &scratchpad[0], orTerms);			
			return;
		}
	}
}

bool SolveFromInput::checkParentsChanged(bool_node* node, bool more){
	if(( node->father== NULL || !node->father->flag ) &&
					( node->mother== NULL || !node->mother->flag )&&
					more
					){ node->flag =false; return false; }else{ node->flag = true; return true;}
}

