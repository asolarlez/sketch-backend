#include "SolveFromInput.h"

#include <map>
#ifndef INTEGERBOUND
#define INTEGERBOUND 8192
#endif

bool GLOfirstTime = true;

vector<int> scratchpad(100);

void SolveFromInput::addInputsToTestSet(int input[], int insize){
	int N = getInSize();
	int k = 0;
	int ctrl = 0;
	int numRepeat = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		if((*node_it)->type == bool_node::SRC){
			int iid = (*node_it)->ion_pos;
			Assert(input[iid % insize] == 1 || input[iid % insize]==-1, "This is bad, really bad");
			node_values[(*node_it)]= input[iid % insize];
			if(input[iid % insize] == last_input[iid]){
				++numRepeat;
				Dout(cout<<"input "<<iid<<" unchanged"<<endl);
				(*node_it)->flag = false;
			}else{				
				Dout(cout<<"input "<<iid<<" changed"<<endl);
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
			node_values[(*node_it)]= input[iid % insize];
			if(input[iid % insize] == last_input[iid]){
				Dout(cout<<"input "<<iid<<" unchanged"<<endl);
				(*node_it)->flag = false;
			}else{
				Dout(cout<<"input "<<iid<<" changed"<<endl);
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
		node_values[(*node_it)]=0;
	}
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		(*node_it)->flag = true;
		node_values[(*node_it)]=0;
	}
	node_values[NULL] = 1;
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
	dir.makeArrNoBranch(SOUT);
	dir.makeArrNoBranch(OUT);
	YES = dir.newAnonymousVar();
	Dout(cout<<"YES = "<<YES<<endl);
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




template<>
bool SolveFromInput::booleanPartialEval<logical_or<bool> >(bool_node* node){
	logical_or<bool> comp;
	int fathval = node_values[node->father];
	int mothval = node_values[node->mother];
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = node_values[node];
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
		node_values[node] = newVal;
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}else{
		if(fathval == 1 || mothval == 1){
			int oldVal = node_values[node];
			int newVal =  1;
			Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
			node_values[node] = newVal;
			node->flag = newVal != oldVal;
			node_ids[node] = newVal*YES;
			return true;
		}
	}
	node_values[node] = 0;
	return false;
}




template<>
bool SolveFromInput::booleanPartialEval<logical_and<bool> >(bool_node* node){
	logical_and<bool> comp;
	int fathval = node_values[node->father];
	int mothval = node_values[node->mother];
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = node_values[node];
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
		node_values[node] = newVal;
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}else{
		if(fathval == -1 || mothval == -1){
			int oldVal = node_values[node];
			int newVal =  -1;
			Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
			node_values[node] = newVal;
			node->flag = newVal != oldVal;
			node_ids[node] = newVal*YES;
			return true;
		}	
	}
	node_values[node] = 0;
	return false;
}



template<typename COMP>
bool SolveFromInput::booleanPartialEval(bool_node* node){
	COMP comp;
	int fathval = node_values[node->father];
	int mothval = node_values[node->mother];
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = node_values[node];
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
		node_values[node] = newVal;
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}
	node_values[node] = 0;
	return false;
}


void SolveFromInput::translator(SAT_Manager mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	node_ids[NULL] = YES;

	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		switch((*node_it)->type){
			case bool_node::AND:{
				int fid = node_ids[(*node_it)->father];
				int mid = node_ids[(*node_it)->mother];
				if( booleanPartialEval<logical_and<bool> >(*node_it) ){ Dout( cout<<"AND "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<fid<<" AND "<<mid<<" unchanged"<<endl  ); break; }
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"AND "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addAndClause(mng, nvar, fsign*fid, msign*mid);
				break;
			}
			case bool_node::OR:{
				if( booleanPartialEval<logical_or<bool> >(*node_it) ){  Dout( cout<<"OR "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<"OR didn't change"<<endl  ); break; }
				int nvar = dir.newAnonymousVar();
				node_ids[*node_it] = nvar;
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				Dout(cout<<"OR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				addOrClause(mng, nvar, fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);				
				break;
			}
			case bool_node::XOR:{
				if( booleanPartialEval<not_equal_to<bool> >(*node_it) ){  Dout( cout<<"XOR "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<"XOR didn't change"<<endl  ); break; }			
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
				if( node_values[(*node_it)] != 0){  
					Dout( cout<< dir.getArr(IN, iid)<<" has value "<<node_values[(*node_it)]<<"  "<<(*node_it)<<endl  ); 
					node_ids[*node_it] = node_values[(*node_it)]*YES;
					break; 
				}	
				node_ids[*node_it] = dir.getArr(IN, iid);
				Dout(cout<<"REGISTERING "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::CTRL:{	
				int iid = (*node_it)->ion_pos;
				if( node_values[(*node_it)] != 0){  					
					node_ids[*node_it] = node_values[(*node_it)]*YES;
					Dout( cout<< dir.getArr(CTRL, iid)<<" has value "<<node_values[(*node_it)]<<"   "<< (*node_it) <<"    "<< node_ids[*node_it] <<endl  ); 
					break; 
				}	
				node_ids[*node_it] = dir.getArr(CTRL, iid);
				Dout(cout<<"CONTROL "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::DST:{				
				int oid = (*node_it)->ion_pos;		
				int nvar = dir.getArr(outname, oid);
				int msign = (*node_it)->mother_sgn? 1 : -1;		
				int nvalue = node_values[(*node_it)->mother];
				if( nvalue != 0){
					Dout(cout<<node_ids[(*node_it)->mother]<<"   "<< (*node_it)->mother <<"  nvalue = "<<nvalue<<"    msign="<<msign<<endl);
					setVarClause(mng, nvalue*msign*nvar);
				}else{	
					addEqualsClause(mng, nvar, msign*node_ids[(*node_it)->mother]);
				}
				break;
			}
			case bool_node::PT:{	
				Assert(false, "The graph should not have this node");
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
	bool diag = anode->get_name() == "TMP_NAME_113______TIMES" ;
	bool tmpbool = !diag && GLOfirstTime;
	diag = diag && !GLOfirstTime;
	GLOfirstTime = tmpbool;
	diag = false;
			if(anode->father == NULL){
				//cout<<" IF "<<endl;
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
				//cout<<" THEN "<<endl;
				vector<int>& nrange = num_ranges[anode->mother];
				vector<int>& frange = num_ranges[anode->father];
				map<int, int> numbers;								
				int mid = node_ids[anode->mother];			
				int fid = node_ids[anode->father];
				num_ranges[anode].resize(0);
				vector<int>& tmp = num_ranges[anode];
				tmp.reserve(nrange.size()*frange.size());
				Dout(cout<<"ADDING "<<anode->father_quant<<"  MULTIPLYIN  "<<anode->mother_quant<<endl);
				Dout(cout<<"ADDING "<<anode->father->get_name()<<"  WITH  "<<anode->mother->get_name()<<endl);
				int vals = 0;
				//cout<<" BEFORE THE LOOPS"<<endl;
				for(int i=0; i<nrange.size(); ++i){
					for(int j=0; j<frange.size(); ++j){
						int quant = comp(anode->mother_quant*nrange[i], anode->father_quant*frange[j]);						
						Dout(cout<<quant<<" = "<<anode->mother_quant*nrange[i]<<" * "<<anode->father_quant*frange[j]<<endl);
						if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
						Dout(cout<<"QUANT = "<<quant<<"          "<<mid+i<<", "<<fid + j<<endl);
						if(numbers.find(quant) != numbers.end()){
							if( diag ) cout<< "YES "<<endl;
							int cvar = dir.newAnonymousVar();	
							if( diag ) cout<<" cvar ="<<cvar<<endl;
							addAndClause(mng, cvar, mid+i, fid + j);
							if( diag ) cout<<" ADDED CLAUSE"<<endl;
							int cvar2 = dir.newAnonymousVar();
							if( diag ) cout<<" cvar2 ="<<cvar2<<endl;
							addOrClause(mng, cvar2, cvar, numbers[quant]);
							if( diag ) cout<<" ADDED OTHER CLAUSE"<<endl;
							numbers[quant] = cvar2;
						}else{
							if( diag ) cout<< "NO "<<endl;
							int cvar = dir.newAnonymousVar();		
							addAndClause(mng, cvar, mid+i, fid + j);
							tmp.push_back(quant);
							numbers[quant] = cvar;							
							++vals;	
						}
						//cout<<" ENDLOOP "<<endl;
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
				int nvalue = node_values[*it];
				if( nvalue != 0 ){
					Dout( cout<<" ACTRL "<<*it<<" nvalue = "<<nvalue<<"  signs = "<<(*signs));
					ids[i]=((*signs)==1?1:-1)*nvalue*YES;
				}else{
					Dout( cout<<" ACTRL "<<*it<<" nodeids = "<<node_ids[*it]<<"  signs = "<<(*signs));
					ids[i]=((*signs)==1?1:-1)*node_ids[*it];
				}
				Dout( cout<<"   ids[i]="<<ids[i]<<endl);
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
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			vector<int> choices(anode->multi_mother.size());
			bool parentSame = true;
			bool isBoolean=true;
			for(int i=0; it != anode->multi_mother.end(); ++i, ++it, ++signs){	
				int nvalue = node_values[*it];
				Dout(cout<<" nval = "<<nvalue<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  ");
				if( (*signs)>1 || (*signs)<0 || (num_ranges.find(*it) != num_ranges.end())){
					isBoolean = false;	
				}
				if( nvalue != 0 ){
					choices[i]=((*signs)==1?1:-1)*nvalue*YES;
				}else{
					choices[i]=((*signs)==1?1:-1)*node_ids[*it];
				}
				Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}			
			if(!checkParentsChanged(anode, parentSame)){ break; }
			if(!isBoolean){
				doNonBoolArrAcc(mng, dir, anode, node_ids, 	num_ranges);
				return;	
			}
			bool_node* mother = anode->mother;
			int id = node_ids[mother];
			Assert( mother != NULL, "This should never happen");
			if( (num_ranges.find(mother) == num_ranges.end()) ){ //mother->type != bool_node::ARITH
				int cvar = dir.newAnonymousVar();
				if(choices.size()>=2){
					addChoiceClause(mng, cvar , id , choices[1], choices[0]);
				}else{
					if(choices.size()>=1){
						addAndClause(mng, cvar , -id , choices[0]);
					}else{
						addEqualsClause(mng, cvar, -YES);
					}
				}
				node_ids[anode] = cvar;
				return;
			}
			vector<int>& nrange = num_ranges[mother];
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

void SolveFromInput::doNonBoolArrAcc(SAT_Manager mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){	
	Dout( cout<<" non boolean array "<<endl );
	list<bool_node*>::iterator it = anode->multi_mother.begin();
	list<int>::iterator signs = anode->multi_mother_sgn.begin();
	int N = anode->multi_mother.size();
	vector<int> choices(N);
	vector<int> factors(N);
	vector<vector<int>* > values(N);
	vector<int> tmprange(2);
	tmprange[0] = 0;
	tmprange[1] = 1;
	for(int i=0; i < N; ++i, ++it, ++signs){	
		int nvalue = node_values[*it];
		choices[i] = node_ids[*it];
		factors[i] = (*signs);
		bool hasRanges = true;
		if( num_ranges.find(*it) == num_ranges.end()){
			hasRanges = false;
			int cvar = dir.newAnonymousVar();
			addEqualsClause(mng, cvar, -choices[i]);
			int cvar2 = dir.newAnonymousVar();
			addEqualsClause(mng, cvar2, choices[i]);
			choices[i] = cvar;
			Dout( cout<<" creating new vec with vars "<< cvar << "  " << cvar2 << endl );
		}
		values[i] = hasRanges ? &num_ranges[*it] : &tmprange;
	}		
			
	bool_node* mother = anode->mother;
	int id = node_ids[mother];
	bool isMulti=true;
	if( num_ranges.find(mother) == num_ranges.end() ){			
		int sgn = anode->mother_sgn? 1: -1;
		isMulti = false;
		int cvar = dir.newAnonymousVar();
		addEqualsClause(mng, cvar, sgn*-id);
		int cvar2 = dir.newAnonymousVar();
		addEqualsClause(mng, cvar2, sgn*id);
		id = cvar;
	}
	vector<int>& nrange = isMulti ? num_ranges[mother] : tmprange ;
	map<int, vector<int> > newVals;
	for(int i=0; i<nrange.size(); ++i){
		int factor = factors[ nrange[i] ];
		vector<int>& cvalues = *values[nrange[i]];
		Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<"  factors[x]="<<factor<<"    "<<cvalues.size()<<endl );
		for(int j=0; j<cvalues.size(); ++j){
			int cvar = dir.newAnonymousVar();
			addAndClause(mng, cvar, id + i, choices[nrange[i]] + j);
			newVals[ cvalues[j] * factor].push_back(cvar);
			Dout( cout<<" cvalues["<<j<<"] = "<<cvalues[j]<<"    x factor="<<cvalues[j] * factor<<endl );
		}
	}
	node_ids[anode] = dir.getVarCnt();
	vector<int>& result = num_ranges[anode];
	result.clear();
	for(map<int, vector<int> >::iterator it = newVals.begin(); it != newVals.end(); ++it){
		vector<int>& vars = it->second;
		int orTerms = 0;
		while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
		for(int i=0; i<vars.size(); ++i){
			++orTerms;
			scratchpad[orTerms] = vars[i];
		}
		int cvar = dir.newAnonymousVar();
		scratchpad[0] = cvar;
		addBigOrClause(mng, &scratchpad[0], orTerms);
		result.push_back(it->first);
	}
}


bool SolveFromInput::checkParentsChanged(bool_node* node, bool more){
	if(( node->father== NULL || !node->father->flag ) &&
			( node->mother== NULL || !node->mother->flag )&&
			more
			){ node->flag =false; return false; }else{ node->flag = true; return true;}
}

