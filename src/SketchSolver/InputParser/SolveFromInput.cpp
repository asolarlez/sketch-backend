#include "SolveFromInput.h"

#include <map>
#ifndef INTEGERBOUND
#define INTEGERBOUND 8192
#endif

bool GLOfirstTime = true;

vector<int> scratchpad(100);
vector<int> tmprange(2);
vector<int> unirange(1);


void SolveFromInput::setNewControls(int controls[], int ctrlsize){
	int idx = 0;	
	node_values.clear();
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		node_ids[(*node_it)] = c_node_ids[idx];
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::CTRL ){
			int iid = (*node_it)->ion_pos;
			Assert(controls[iid % ctrlsize] == 1 || controls[iid % ctrlsize]==-1, "This is bad, really bad");
			node_values[(*node_it)]= controls[iid % ctrlsize];
		}
	}
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		node_ids[(*node_it)] = c_node_ids[idx];
		(*node_it)->flag = true;
	}
	buildChecker();	
}




void SolveFromInput::addInputsToTestSet(int input[], int insize){
	int N = getInSize();
	int k = 0;
	int ctrl = 0;
	int numRepeat = 0;
	node_values.clear();
	int idx = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		c_node_ids[idx] = node_ids[(*node_it)];
		node_ids[(*node_it)] = f_node_ids[idx];		
		(*node_it)->flag = f_flags[idx];
		
		if( f_num_ranges.find((*node_it)) != f_num_ranges.end() ){
			Dout( cout<<(*node_it)->get_name()<<" flaged"<<endl );
			num_ranges[(*node_it)] = f_num_ranges[(*node_it)];
		}

		Dout(cout<<"NODE INIT "<<(*node_it)->name<<"  "<<node_ids[(*node_it)]<<"  "<<(*node_it)<<endl);	
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
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		c_node_ids[idx] = node_ids[(*node_it)];
		node_ids[(*node_it)] = f_node_ids[idx];		
		(*node_it)->flag = f_flags[idx];

		if( f_num_ranges.find((*node_it)) != f_num_ranges.end() ){
			Dout( cout<<(*node_it)->get_name()<<" flaged"<<endl );
			num_ranges[(*node_it)] = f_num_ranges[(*node_it)];
		}

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
	idx = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)];
		f_flags[idx] = (*node_it)->flag;
		if( num_ranges.find((*node_it)) != num_ranges.end() ){
			f_num_ranges[(*node_it)] = num_ranges[(*node_it)];
		}
	}
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)];
		f_flags[idx] = (*node_it)->flag;
		if( num_ranges.find((*node_it)) != num_ranges.end() ){
			f_num_ranges[(*node_it)] = num_ranges[(*node_it)];
		}
	}
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
	
	int totSize = spec->size() + sketch->size();
	c_node_ids.resize( totSize , 0 );
	f_node_ids.resize( totSize , 0 );
	f_flags.resize( totSize , true);
	
	cout<<"Random seeds = "<<nseeds<<endl;	
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it){
		(*node_it)->flag = true;
	}
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		(*node_it)->flag = true;
	}
	
	tmprange[0] = 0;
	tmprange[1] = 1;
	unirange[0] = 1;
	
	node_values[NULL] = 1;
	firstTime=true;
}


void SolveFromInput::setupCheck(){
	FindCheckSolver::setupCheck();
	last_input = new int[getInSize()];
	for(int i=0; i<getInSize(); ++i){ last_input[i] = 0; };
}


void SolveFromInput::defineSketch(SATSolver& mng, varDir& dir){
	dir.declareInArr(IN, N);
	dir.declareArr(SOUT, Nout);
	dir.declareArr(OUT, Nout);
	dir.makeArrNoBranch(SOUT);
	dir.makeArrNoBranch(OUT);
	YES = dir.newAnonymousVar();
	dir.setYes(YES);
	Dout(cout<<"YES = "<<YES<<endl);
	mng.setVarClause(YES);
	translator(mng, dir, sketch, SOUT);
}

void SolveFromInput::defineSpec(SATSolver& mng, varDir& dir){
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
	
	int fid = node_ids[node->father];
	int mid = node_ids[node->mother];
	int yoid = node_ids[node];
	
	
	int fathval = (fid==YES)? 1 : ((fid==-YES)? -1 : 0);
	int mothval = (mid==YES)? 1 : ((mid==-YES)? -1 : 0);
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = (yoid==YES)? 1 : ((yoid==-YES)? -1 : 0);
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}else{
		if(fathval*fsign == 1 || mothval*msign == 1){
			int oldVal = (yoid==YES)? 1 : ((yoid==-YES)? -1 : 0);
			int newVal =  1;
			Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
			node->flag = newVal != oldVal;
			node_ids[node] = newVal*YES;
			return true;
		}
	}
	return false;
}




template<>
bool SolveFromInput::booleanPartialEval<logical_and<bool> >(bool_node* node){
	logical_and<bool> comp;

	int fid = node_ids[node->father];
	int mid = node_ids[node->mother];
	int yoid = node_ids[node];
	
	
	int fathval = (fid==YES)? 1 : ((fid==-YES)? -1 : 0);
	int mothval = (mid==YES)? 1 : ((mid==-YES)? -1 : 0);
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = (yoid==YES)? 1 : ((yoid==-YES)? -1 : 0);
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}else{
		if(fathval*fsign == -1 || mothval*msign == -1){
			int oldVal = (yoid==YES)? 1 : ((yoid==-YES)? -1 : 0);
			int newVal =  -1;
			Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");
			node->flag = newVal != oldVal;
			node_ids[node] = newVal*YES;
			return true;
		}	
	}
	return false;
}



template<typename COMP>
bool SolveFromInput::booleanPartialEval(bool_node* node){
	COMP comp;
	int fid = node_ids[node->father];
	int mid = node_ids[node->mother];
	int yoid = node_ids[node];
		
	int fathval = (fid==YES)? 1 : ((fid==-YES)? -1 : 0);
	int mothval = (mid==YES)? 1 : ((mid==-YES)? -1 : 0);
	
	int fsign = node->father_sgn? 1 : -1;
	int msign = node->mother_sgn? 1 : -1;
	if(fathval != 0 && mothval != 0){
		int oldVal = (yoid==YES)? 1 : ((yoid==-YES)? -1 : 0);
		int newVal = comp(fathval*fsign==1 , mothval*msign==1) ? 1 : -1;
		Dout(cout<<fathval*fsign<<" op "<<mothval*msign<<" -> "<< newVal <<" ");		
		node->flag = newVal != oldVal;
		node_ids[node] = newVal*YES;
		return true;
	}
	return false;
}


void SolveFromInput::translator(SATSolver& mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	node_ids[NULL] = YES;

	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		switch((*node_it)->type){
			case bool_node::AND:{
				int fid = node_ids[(*node_it)->father];
				int mid = node_ids[(*node_it)->mother];
				if( booleanPartialEval<logical_and<bool> >(*node_it) ){ Dout( cout<<"AND "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<fid<<" AND "<<mid<<" unchanged"<<endl  ); break; }
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				int nvar = dir.addAndClause(fsign*fid, msign*mid);
				node_ids[*node_it] = nvar;
				Dout(cout<<"AND "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::OR:{
				if( booleanPartialEval<logical_or<bool> >(*node_it) ){  Dout( cout<<"OR "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<"OR didn't change"<<endl  ); break; }
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				int nvar = dir.addOrClause(fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				node_ids[*node_it] = nvar;
				Dout(cout<<"OR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);
				break;
			}
			case bool_node::XOR:{
				if( booleanPartialEval<not_equal_to<bool> >(*node_it) ){  Dout( cout<<"XOR "<<endl  ); break; }
				if(!checkParentsChanged(*node_it, true)){ Dout( cout<<"XOR didn't change"<<endl  ); break; }			
				int fsign = (*node_it)->father_sgn? 1 : -1;
				int msign = (*node_it)->mother_sgn? 1 : -1;
				int nvar = dir.addXorClause(fsign*node_ids[(*node_it)->father], msign*node_ids[(*node_it)->mother]);
				node_ids[*node_it] = nvar;
				Dout(cout<<"XOR "<<(*node_it)->name<<"  "<<node_ids[*node_it]<<"  "<<*node_it<<endl);				
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
				{	
					mng.addEqualsClause( nvar, msign*node_ids[(*node_it)->mother]);
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
void processComparissons(SATSolver& mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges, int YES){

	bool_node* mother = anode->mother;			
	bool hasMother = (num_ranges.find(mother) != num_ranges.end() );
	bool motherUni = false;
	int mid = node_ids[mother] ;
	int mquant = anode->mother_quant;
	if(!hasMother){
		if(mother != NULL){
			Assert( mquant == 1 || mquant == 0, "If mother is bool, the quant is either true or false");
			mid = mid*(mquant?1:-1);
			mquant = 1;
		}
		if( mid == YES){
			motherUni = true;
		}else{
			int cvar = dir.newAnonymousVar();
			int cvar2 = dir.newAnonymousVar();
			mng.addEqualsClause( cvar, -mid);
			mng.addEqualsClause( cvar2, mid);
			mid = cvar;
		}
	}
	vector<int>& mrange = hasMother? num_ranges[mother] : ( motherUni? unirange :tmprange);
	
	
	bool_node* father = anode->father;			
	bool hasFather = (num_ranges.find(father) != num_ranges.end() );
	bool fatherUni = false;
	int fid = node_ids[father];
	int fquant = anode->father_quant;
	if(!hasFather){
		if(father != NULL){
			Assert( fquant == 1 || fquant == 0, "If father is bool, the quant is either true or false");
			fid = fid*(fquant?1:-1);
			fquant = 1;
		}
		if( fid == YES){
			fatherUni = true;
		}else{
			int cvar = dir.newAnonymousVar();
			int cvar2 = dir.newAnonymousVar();
			mng.addEqualsClause( cvar, -fid);
			mng.addEqualsClause( cvar2, fid);
			fid = cvar;		
		}
	}	
	vector<int>& frange = hasFather? num_ranges[father] : (fatherUni? unirange: tmprange);

	
	int cvar = -YES;
	COMP comp;
	Dout(cout<<"SIZES = "<<mrange.size()<<", "<<frange.size()<<endl);
	int orTerms = 0;
	for(int i=0; i<mrange.size(); ++i){
		for(int j=0; j<frange.size(); ++j){
			Dout(cout<<"COMPARING "<<mquant*mrange[i]<<", "<<fquant*frange[j]<<endl);
			if(comp(mquant*mrange[i], fquant*frange[j])){
				if( mid + i == YES ) {
					cvar = fid + j;
					++orTerms;
					if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms] = cvar;
				}else{
					if( fid + j == YES ){
						cvar = mid + i;
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;
					}else{
						cvar = dir.addAndClause(mid + i, fid + j);						
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;						
					}	
				}
			}	
		}
	}
	if( orTerms < 2 ){
		node_ids[anode] = cvar;
	}else{
		int result = dir.addBigOrClause( &scratchpad[0], orTerms);
		node_ids[anode] = result;
	}	
	return;	
}

template<typename THEOP>
inline int doArithExpr(SATSolver& mng, int quant1, int quant2, int id1, int id2, THEOP comp){
	return comp(quant1, quant2);
}



template<>
inline int doArithExpr<divides<int> >(SATSolver& mng, int quant1, int quant2, int id1, int id2, divides<int> comp){
	if(quant2 == 0){
		mng.assertVarClause(-id2);	
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}


template<>
inline int doArithExpr<modulus<int> >(SATSolver& mng, int quant1, int quant2, int id1, int id2, modulus<int> comp){
	if(quant2 == 0){
		mng.assertVarClause(-id2);
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}


template<typename THEOP>
void processArith(SATSolver& mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	THEOP comp;
			if(anode->father == NULL){
				//cout<<" IF "<<endl;
				bool hasRange = true;
				hasRange = (num_ranges.find(anode->mother) != num_ranges.end() );
				vector<int>& nrange = hasRange? num_ranges[anode->mother] : tmprange;
				int id = node_ids[anode->mother];
				int mquant = anode->mother_quant;
				if(!hasRange){
					int sgn = mquant? 1 : -1;
					int cvar = dir.newAnonymousVar();	
					int cvar2 = dir.newAnonymousVar();	
					mng.addEqualsClause( cvar, -sgn*id);
					mng.addEqualsClause( cvar2, sgn*id);
					id = cvar;
					mquant = 1;
				}
				num_ranges[anode].resize(nrange.size());
				vector<int>& tmp = num_ranges[anode];
				Dout(cout<<"ADDING "<<anode->father_quant<<"  MULTIPLYIN  "<<mquant<<endl);
				for(int i=0; i<nrange.size(); ++i){
					//tmp[i] = comp(mquant*nrange[i], anode->father_quant);
					tmp[i] = doArithExpr(mng, mquant*nrange[i], anode->father_quant, id+i, id+i, comp);
					Dout(cout<<"  "<< mquant*nrange[i]<<" op "<<anode->father_quant<<"= "<<tmp[i]<<endl);
				}
				node_ids[anode] = id;
			}else{
				//cout<<" THEN "<<endl;
				Assert( (num_ranges.find(anode->mother) != num_ranges.end() ), "Mother doesn't have stuff");
				Assert( (num_ranges.find(anode->father) != num_ranges.end() ), "Father doesn't have stuff");
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
						// int quant = comp(anode->mother_quant*nrange[i], anode->father_quant*frange[j]);						
						int quant = doArithExpr(mng, anode->mother_quant*nrange[i], anode->father_quant*frange[j], mid+i, fid+j, comp);
						Dout(cout<<quant<<" = "<<anode->mother_quant*nrange[i]<<" * "<<anode->father_quant*frange[j]<<endl);
						if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
						Dout(cout<<"QUANT = "<<quant<<"          "<<mid+i<<", "<<fid + j<<endl);
						if(numbers.find(quant) != numbers.end()){
							int cvar = dir.addAndClause(mid+i, fid + j);							
							int cvar2 = dir.addOrClause(cvar, numbers[quant]);
							numbers[quant] = cvar2;
						}else{
							int cvar = dir.addAndClause(mid+i, fid + j);
							tmp.push_back(quant);
							numbers[quant] = cvar;	
							++vals;
						}
						//cout<<" ENDLOOP "<<endl;
					}
				}
				Dout(cout<<"tmp size = "<<tmp.size()<<endl);
				Assert( vals > 0, "This should not happen here");
				int newID = dir.newAnonymousVar();
				for(int i=1; i<vals; ++i){ 
					int cvar = dir.newAnonymousVar();
					Assert( cvar == newID + i, "SolveFromInput: bad stuff");
				}
				for(int i=0; i<vals; ++i){
					int quant = tmp[i];
					Dout(cout<<"quant = "<<quant<<endl);
					mng.addEqualsClause(newID + i, numbers[quant]);
				}
				node_ids[anode] = newID;
			}
}



void SolveFromInput::processArithNode(SATSolver& mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	switch(anode->arith_type){
		case arith_node::GE:{
			Dout( cout<<" GE "<<endl );
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<greater_equal<int> >(mng, dir, anode, node_ids, num_ranges, YES);
			return;
		}
		case arith_node::LT:{
			Dout( cout<<" LT "<<endl );
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<less<int> >(mng, dir, anode, node_ids, num_ranges, YES);
			return;
		}
		case arith_node::LE:{
			Dout( cout<<" LE "<<endl );			
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<less_equal<int> >(mng, dir, anode, node_ids, num_ranges, YES);
			return;
		}
		case arith_node::GT:{
			Dout( cout<<" GT "<<endl );
			if(!checkParentsChanged(anode, true)){ break; }	
			processComparissons<greater<int> >(mng, dir, anode, node_ids, num_ranges, YES);
			return;
		}
		case arith_node::PLUS:{
			Dout( cout<<" PLUS "<<endl );			
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<plus<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::TIMES:{
			Dout( cout<<" TIMES "<<endl );			
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<multiplies<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::DIV:{
			Dout( cout<<" DIV "<<endl );			
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<divides<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::MOD:{
			Dout( cout<<" MOD "<<endl );			
			if(!checkParentsChanged(anode, true)){ break; }
			processArith<modulus<int> >(mng, dir, anode, node_ids, num_ranges);
			return;
		}
		case arith_node::ACTRL:{
			int size = anode->multi_mother.size();
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			bool parentSame = true;
			vector<int> ids(anode->multi_mother.size());
			for(int i=0 ; it != anode->multi_mother.end(); ++it, ++i, ++signs){
				{
					Dout( cout<<" ACTRL "<<*it<<" nodeids = "<<node_ids[*it]<<"  signs = "<<(*signs));
					ids[i]=((*signs)==1?1:-1)*node_ids[*it];
				}
				Dout( cout<<"   ids[i]="<<ids[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}
			if(!checkParentsChanged(anode, parentSame)){Dout(cout<<"@ACTRL "<<anode->name<<"  "<<node_ids[anode]<<"  "<<num_ranges[anode].size()<<"   "<<anode<<endl);	 break; }
			vector<int>& tmp = num_ranges[anode];
			varRange vr = getSwitchVars(mng,dir, ids, size, tmp, YES);			
			node_ids[anode] = vr.varID;
			Dout(cout<<"&ACTRL "<<anode->name<<"  "<<node_ids[anode]<<"  "<<tmp.size()<<"   "<<anode<<endl);	
			return;
		}
		
		case arith_node::ARRASS: {
			Dout(cout<<"             ARRASS:"<<endl);
			// mother = index
			// multi-mother[0] = old-value;
			// multi-mother[1] = new-value;
			bool_node* mother = anode->mother;
			int id = node_ids[mother];
			int quant = anode->mother_quant;
			Dout(cout<<" mother = "<<((mother != NULL)?mother->get_name():"NULL")<<"  mid = "<<id<<"  mquant = "<<quant<<endl);
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			Assert( anode->multi_mother.size() == 2 , "THIS SHOULDN't HAPPEN");
			vector<int> choices(2);
			vector<bool_node*> mothers(2);
			vector<int> factors(2);
			bool parentSame = true;
			bool isBoolean=true;
			for(int i=0; it != anode->multi_mother.end(); ++i, ++it, ++signs){	
				
				if( (*signs)>1 || (*signs)<0 || (num_ranges.find(*it) != num_ranges.end())){
					isBoolean = false;	
				}
				mothers[i] = *it;
				factors[i] = *signs;
				Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  factor="<<*signs<<"  ");
				{
					choices[i]=node_ids[*it];
				}
				Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}			
			if(!checkParentsChanged(anode, parentSame)){ break; }			
			int guard;						
			if( (num_ranges.find(mother) == num_ranges.end()) ){
				int sgn = anode->mother_sgn? 1: -1;
				if(quant > 1){
					guard = -YES;	
				}else{
					int tmp = sgn * id;
					Dout(cout<<" sgn = "<<sgn<<"  id="<<id<<"  tmp="<<tmp<<endl);
					guard = dir.addXorClause(tmp,  quant==0?YES:-YES);
				}
			}else{
				guard = -YES;
				vector<int>& nrange = num_ranges[mother];
				for(int i=0; i<nrange.size(); ++i){
					if( nrange[i] == quant){
						guard = id + i;						
						break;						
					}					
				}
			}
			Dout(cout<<" guard = "<<guard<<endl);
			int factor0 = factors[0];
			int factor1 = factors[1];
			if(isBoolean){
				Dout(cout<<" is boolean"<<endl);
				if(guard == YES){
					node_ids[anode] = choices[1]*(factor1==1?1:-1);
					return;
				}
				if(guard == -YES){
					node_ids[anode] = choices[0]*(factor0==1?1:-1);
					return;	
				}
				int cvar = dir.addChoiceClause(guard , choices[1]*(factor1==1?1:-1), choices[0]*(factor0==1?1:-1));
				node_ids[anode] = cvar;
				return;
			}else{
				Dout(cout<<" is not boolean"<<endl);
				int mid0 = choices[0];				
				int mid1 = choices[1];
				bool hasRange ;
				hasRange = (num_ranges.find(mothers[1]) != num_ranges.end());
				vector<int>& nr1 = hasRange? num_ranges[mothers[1]] : tmprange;				
				if(!hasRange){
					int cvar = dir.newAnonymousVar();
					int cvar2 = dir.newAnonymousVar();
					mng.addEqualsClause( cvar, -mid1);
					mng.addEqualsClause( cvar2, mid1);
					mid1 = cvar;
				}
				hasRange = (num_ranges.find(mothers[0]) != num_ranges.end());
				vector<int>& nr0 = hasRange? num_ranges[mothers[0]] : tmprange;
				if(!hasRange){
					int cvar = dir.newAnonymousVar();
					int cvar2 = dir.newAnonymousVar();
					mng.addEqualsClause( cvar, -mid0);
					mng.addEqualsClause( cvar2, mid0);
					mid0 = cvar;
				}
				if(guard == YES){
					node_ids[anode] = mid1;
					num_ranges[anode] = nr1;
					vector<int>& tmp = num_ranges[anode];
					Dout( cout<<"var "<< choices[1] <<"  val = ");
					for(int i=0; i<tmp.size(); ++i){ tmp[i] = tmp[i] * factor1; Dout( cout<<tmp[i]<<", ");}
					Dout(cout<<endl);
					return;
				}
				if(guard == -YES){
					node_ids[anode] = mid0;
					num_ranges[anode] = nr0;
					vector<int>& tmp = num_ranges[anode];
					for(int i=0; i<tmp.size(); ++i){ tmp[i] = tmp[i] * factor0; }
					return;
				}
				int i=0, j=0;
				vector<int> res;
				res.reserve(nr0.size() + nr1.size());
				vector<int>& out = num_ranges[anode];
				out.reserve(nr0.size() + nr1.size());
				while(i < nr0.size() || j< nr1.size()){
					bool avi = i < nr0.size();
					bool avj = j < nr1.size();
					int curri = avi ? nr0[i] * factor0 : -1;
					int currj = avj ? nr1[j] * factor1 : -1;
					if( curri == currj && avi && avj){
						Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
						int cvar1 = dir.addAndClause( mid0+i, -guard);
						int cvar2 = dir.addAndClause( mid1+j, guard);
						int cvar3 = dir.addOrClause( cvar2, cvar1);
						out.push_back(curri);
						res.push_back(cvar3);
						i++;
						j++;
						continue;
					}
					if((curri < currj && avi) || !avj){
						Dout(cout<<" curri = "<<curri<<endl);
						int cvar = dir.addAndClause( mid0+i, -guard);
						out.push_back(curri);
						res.push_back(cvar);
						i++;
						continue;
					}
					if( (currj < curri && avj) || !avi ){
						Dout(cout<<" currj = "<<currj<<endl);
						int cvar = dir.addAndClause( mid1+j, guard );
						out.push_back(currj);
						res.push_back(cvar);
						j++;
						continue;
					}
					Assert(false, "Should never get here");
				}
				out.resize(res.size());
				Assert( res.size() > 0, "This should not happen here2");
				int newID = dir.newAnonymousVar();
				for(int k=1; k<res.size(); ++k){
					int cvar = dir.newAnonymousVar();
					Assert( cvar == newID + k, "SolveFromInput: cvar != newID + k");
				}
				for(int k=0; k<res.size(); ++k){
					int val = res[k];
					mng.addEqualsClause( newID+k, val);
				}
				node_ids[anode] = newID;
				return;
			}
		}
		
		case arith_node::ARRACC:{
			Dout(cout<<" ARRACC "<<endl);
			list<bool_node*>::iterator it = anode->multi_mother.begin();
			list<int>::iterator signs = anode->multi_mother_sgn.begin();
			vector<int> choices(anode->multi_mother.size());
			bool parentSame = true;
			bool isBoolean=true;
			for(int i=0; it != anode->multi_mother.end(); ++i, ++it, ++signs){	
				
				Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  signs = "<<*signs<<"  ");
				if( (*signs)>1 || (*signs)<0 || (num_ranges.find(*it) != num_ranges.end())){
					isBoolean = false;	
				}
				{
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
			Dout(cout<<" is boolean"<<endl);
			bool_node* mother = anode->mother;
			int id = node_ids[mother];
			Dout(cout<<" mother = "<<id<<"  signs = "<<anode->mother_sgn<<"  "<<endl);
			Assert( mother != NULL, "This should never happen");
			if( (num_ranges.find(mother) == num_ranges.end()) ){ //mother->type != bool_node::ARITH
				int sgn = anode->mother_sgn? 1: -1;
				int cvar;
				if(choices.size()>=2){
					Dout( cout<<" replacing with choice "<<sgn * id<<", "<<choices[1]<<", "<<choices[0]<<endl );
					cvar = dir.addChoiceClause(sgn * id , choices[1], choices[0]);
				}else{
					if(choices.size()>=1){
						cvar = dir.addAndClause( sgn * -id , choices[0]);
					}else{
						cvar = -YES;
					}
				}
				node_ids[anode] = cvar;
			Dout(cout<<"ARRACC "<<anode->name<<"  "<<node_ids[anode]<<"   "<<anode<<endl);	
				return;
			}
			vector<int>& nrange = num_ranges[mother];
			int cvar = -YES;
			int orTerms = 0;
			for(int i=0; i<nrange.size(); ++i){
				if( nrange[i] >= 0 && nrange[i] < choices.size() ){
					if( id+i == YES){
						cvar = choices[nrange[i]];
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;
					}else{
						if( id+i != -YES ){
							cvar = dir.addAndClause( choices[nrange[i]], id + i);
							++orTerms;
							if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
							scratchpad[orTerms] = cvar;
						}
					}
				}
			}
			if( orTerms < 2){
				node_ids[anode] = cvar;
			}else{
				int result = dir.addBigOrClause( &scratchpad[0], orTerms);			
				node_ids[anode] = result;
			}
			Dout(cout<<"ARRACC "<<anode->name<<"  "<<node_ids[anode]<<"   "<<anode<<endl);	
			return;
		}
	}
}

void SolveFromInput::doNonBoolArrAcc(SATSolver& mng, varDir& dir,arith_node* anode, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){	
	Dout( cout<<" non boolean array "<<endl );
	list<bool_node*>::iterator it = anode->multi_mother.begin();
	list<int>::iterator signs = anode->multi_mother_sgn.begin();
	int N = anode->multi_mother.size();
	vector<int> choices(N);
	vector<int> factors(N);
	vector<vector<int>* > values(N);

	for(int i=0; i < N; ++i, ++it, ++signs){	
		
		choices[i] = node_ids[*it];
		factors[i] = (*signs);
		bool hasRanges = true;
		if( num_ranges.find(*it) == num_ranges.end()){
			hasRanges = false;
			if(*it != NULL){
				Assert( factors[i] == 1 || factors[i] ==0, "Wait, this is pretty bad");	
				choices[i] = choices[i] * (factors[i]? 1:-1);
				factors[i] = 1;
				int cvar = dir.newAnonymousVar();
				int cvar2 = dir.newAnonymousVar();
				mng.addEqualsClause( cvar, -choices[i]);
				mng.addEqualsClause( cvar2, choices[i]);
				choices[i] = cvar;					
				values[i] = &tmprange;
				Dout( cout<<" creating new vec with vars "<< cvar << "  " << cvar2 << endl );
			}else{
				Assert( choices[i] == YES , "This better be true, or else ...");
				values[i] = &unirange;
			}			
		}else{
			values[i] = &num_ranges[*it];
		}
		
	}		
			
	bool_node* mother = anode->mother;
	Assert( mother != NULL, "This case should be handled by the partial evaluator");
	int id = node_ids[mother];
	bool isMulti=true;
	if( num_ranges.find(mother) == num_ranges.end() ){			
		int sgn = anode->mother_sgn? 1: -1;
		isMulti = false;
		int cvar = dir.newAnonymousVar();
		int cvar2 = dir.newAnonymousVar();
		mng.addEqualsClause( cvar, sgn*-id);
		mng.addEqualsClause( cvar2, sgn*id);
		id = cvar;
	}
	vector<int>& nrange = isMulti ? num_ranges[mother] : tmprange ;
	map<int, vector<int> > newVals;
	int vsize = values.size();
	for(int i=0; i<nrange.size(); ++i){
		int factor = factors[ nrange[i] ];
		if( nrange[i] < vsize && nrange[i] >= 0){
			Assert( values[nrange[i]] != NULL , "This can't happen either");
			vector<int>& cvalues = *values[nrange[i]];
			Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<"  factors[x]="<<factor<<"    "<<cvalues.size()<<endl );
			for(int j=0; j<cvalues.size(); ++j){
				if( (id + i) == YES ){
					newVals[ cvalues[j] * factor].push_back(choices[nrange[i]] + j);
				}else{
					int tmpid = choices[nrange[i]] + j;
					if( tmpid == YES ){
						newVals[ cvalues[j] * factor].push_back(id+i);
					}else{
						int cvar = dir.addAndClause( id + i, choices[nrange[i]] + j);
						newVals[ cvalues[j] * factor].push_back(cvar);
						Dout( cout<<" cvalues["<<j<<"] = "<<cvalues[j]<<"    x factor="<<cvalues[j] * factor<<endl );
					}
				}
			}
		}
	}
	
	vector<int>& result = num_ranges[anode];
	result.clear();
	if(newVals.size() == 1){
		map<int, vector<int> >::iterator it = newVals.begin();
		vector<int>& vars = it->second;
		int orTerms = 0;
		while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
		for(int i=0; i<vars.size(); ++i){
			++orTerms;
			scratchpad[orTerms] = vars[i];
		}
		if( orTerms == 1){
			node_ids[anode] = vars[0];
			result.push_back(it->first);
		}else{
			int cvar = dir.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
			node_ids[anode] = cvar;
		}		
	}else{
		Assert( newVals.size() > 0, "This should not happen here2");
		int newID = dir.newAnonymousVar();
		node_ids[anode] = newID;
		int k=1;
		for(k = 1; k< newVals.size(); ++k){
			int cvar = dir.newAnonymousVar();
			Assert( cvar == newID + k, "SolveFromInput3: cvar != newID + k ");
		}
		k = 0;
		for(map<int, vector<int> >::iterator it = newVals.begin(); it != newVals.end(); ++it, ++k){
			vector<int>& vars = it->second;
			int orTerms = 0;
			while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
			for(int i=0; i<vars.size(); ++i){
				++orTerms;
				scratchpad[orTerms] = vars[i];
			}
			scratchpad[0] = newID + k;
			mng.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
		}
	}
}


bool SolveFromInput::checkParentsChanged(bool_node* node, bool more){
#ifdef ABCSAT	
	return true;
#else	
	if(( node->father== NULL || !node->father->flag ) &&
			( node->mother== NULL || !node->mother->flag )&&
			more
			){ node->flag =false; return false; }else{ node->flag = true; return true;}
#endif			
}

