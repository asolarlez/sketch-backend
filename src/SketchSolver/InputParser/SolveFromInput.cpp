#include "SolveFromInput.h"
#include "NodesToSolver.h"

#include <map>









void SolveFromInput::translator(SATSolver& mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	node_ids[NULL] = YES;
	NodesToSolver nts(mng, dir, outname, node_values, num_ranges, node_ids, YES, IN, CTRL);
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		if( (*node_it)->type == bool_node::ARITH){
			if( dynamic_cast<arith_node*>(*node_it)->arith_type != arith_node::ARRACC 
				&& dynamic_cast<arith_node*>(*node_it)->arith_type != arith_node::ARRASS 
			){
				Assert( (*node_it)->mother_sgn == true , "This is a bug" );	
				Assert( (*node_it)->father_sgn == true , "This is a bug" );	
			}
		}
		(*node_it)->accept(nts);
	}
}




void SolveFromInput::setNewControls(int controls[], int ctrlsize){
	int idx = 0;	
	node_values.clear();
	node_ids.clear();
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		//node_ids[(*node_it)] = c_node_ids[idx];
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::CTRL ){
			int iid = (*node_it)->ion_pos;
			Assert(controls[iid % ctrlsize] == 1 || controls[iid % ctrlsize]==-1, "This is bad, really bad");
			node_values[(*node_it)]= controls[iid % ctrlsize];
		}
	}
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		//node_ids[(*node_it)] = c_node_ids[idx];
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


SolveFromInput::SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker, int NS_p):FindCheckSolver(finder, checker), CTRL("_C"){
	N = spec_p->get_n_inputs();
	Nout = spec_p->get_n_outputs();
	spec = spec_p;
	sketch = sketch_p;
	Dout( cout<<"before cleanup "<<endl);
	Dout( sketch->print(cout) );
  	sketch->cleanup(false);
	Dout( cout<<"after cleanup "<<endl);
	Dout( sketch->print(cout) );
  	spec->cleanup(false);
    sketch->sort_graph();
   	Dout( cout<<"after sort "<<endl);
	Dout( sketch->print(cout) );
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


