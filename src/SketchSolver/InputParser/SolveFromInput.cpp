#include "SolveFromInput.h"
#include "NodesToSolver.h"
#include "timerclass.h"


#include "NodesToEuclid.h"
#include "DagCSE.h"

template<typename T>
int intFromBV(T bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){
		Dout( cout<< bv[start + i] << "  " );
		if( bv[start + i] > 0){
			nval += t;	
		}
		t = t*2;
	}
	return nval;
}



SolveFromInput::SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker, int NS_p):FindCheckSolver(finder, checker){
Dout( cout<<"START CONSTRUCTOR"<<endl );
	int N = spec_p->get_n_inputs();
	Nout = spec_p->get_n_outputs();
	cout<<"  Nout="<<Nout<<"  N="<<N<<endl;
	spec = spec_p;
	sketch = sketch_p;
Dout( cout<<"BEFORE CLEANUP"<<endl );
  	sketch->cleanup(false);
  	spec->cleanup(false);
Dout( cout<<"BEFORE SORT"<<endl );  	  	
    sketch->sort_graph();
    spec->sort_graph();
Dout( cout<<"BEFORE RELABEL"<<endl );
    spec->relabel();
    sketch->relabel();

   	Dout( cout<<"after sort "<<endl);
	//Dout( spec->print(cout) );
	//Dout( sketch->print(cout) );
	
	cout<<"before CSE: SPEC nodes = "<<spec->size()<<"\t SKETCH nodes = "<<sketch->size()<<endl;
	
	{
		DagCSE cse(*spec);	
		cse.eliminateCSE();
	}
	{
		DagCSE cse(*sketch);	
		cse.eliminateCSE();
	}
	
	Dout( cout<<" after removing common subexpressions"<<endl);
	Dout( spec->print(cout) );
	Dout( sketch->print(cout) );
	
	
cout<<"BEFORE DC"<<endl;	
    Dout( cout<<"sketch->get_n_controls() = "<<sketch->get_n_controls()<<"  "<<sketch<<endl );
    {
	    vector<bool_node*>& sketchIn = sketch->getNodesByType(bool_node::CTRL);
	    for(int i=0; i<sketchIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(sketchIn[i]);	
			int nbits = ctrlnode->get_nbits();
			declareControl(sketchIn[i]->get_name(), nbits);
		}
    }
    	
	nseeds = NS_p;
cout<<"BEFORE RES"<<endl;	
	int totSize = spec->size() + sketch->size();
	f_node_ids.resize( totSize , 0 );
	f_flags.resize( totSize , true);
	
	cout<<"Random seeds = "<<nseeds<<endl;	
	cout<<"SPEC nodes = "<<spec->size()<<"\t SKETCH nodes = "<<sketch->size()<<endl;
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it){
		(*node_it)->flag = true;
	}
	int specsize = spec->size();
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		(*node_it)->flag = true;
		(*node_it)->id += specsize;
	}
	
	vector<bool_node*>& specIn = spec->getNodesByType(bool_node::SRC);
	vector<bool_node*>& sketchIn = sketch->getNodesByType(bool_node::SRC);
	Assert(specIn.size() == sketchIn.size(), "The number of inputs in the spec and sketch must match");
	
	for(int i=0; i<specIn.size(); ++i){
		sketchIn[i]->name = specIn[i]->name;	
		SRC_node* srcnode = dynamic_cast<SRC_node*>(sketchIn[i]);	
		int nbits = srcnode->get_nbits();
		declareInput(sketchIn[i]->get_name(), nbits);
	}
	
	
	node_ids.resize( totSize );
	node_values[NULL] = 1;
	firstTime=true;
}



void SolveFromInput::setupCheck(){
	Dout( cout<<"setupCheck()"<<endl );
	//FindCheckSolver::setupCheck();
	
	last_input = new int[getInSize()];
	for(int i=0; i<getInSize(); ++i){ last_input[i] = 0; };
}



void SolveFromInput::translator(SATSolver& mng, varDir& dir, BooleanDAG* bdag, const string& outname){
	//node_ids[NULL] = YES;
//	timerclass timer("translator");
	NodesToSolver nts(mng, dir, outname, node_values, node_ids);
	for(BooleanDAG::iterator node_it = bdag->begin(); node_it != bdag->end(); ++node_it){
		if( (*node_it)->type == bool_node::ARITH){
			if( dynamic_cast<arith_node*>(*node_it)->arith_type != arith_node::ARRACC 
				&& dynamic_cast<arith_node*>(*node_it)->arith_type != arith_node::ARRASS 
			){
				Assert( (*node_it)->mother_sgn == true , "This is a bug" );	
				Assert( (*node_it)->father_sgn == true , "This is a bug" );	
			}
		}
//		timer.restart();
		(*node_it)->accept(nts);
//		timer.stop();
//		if(timer.get_cur_ms() > 20.0){
//			timer.print();
//			cout<<(*node_it)->get_name()<<endl;
//		}
	}
}






void SolveFromInput::setNewControls(vector<int>& controls){
	int idx = 0;	
	node_values.clear();
	node_ids.clear();
	node_ids.resize( sketch->size() + spec->size() );
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::CTRL ){
			int iid = (*node_it)->ion_pos;
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();
			Assert( nbits > 0 , "This can not happen rdu;a");
			Assert( iid+ nbits <= controls.size(), "There should be a control entry for each iid a");		
			Assert(controls[iid ] == 1 || controls[iid]==-1, "This is bad, really bad");
			if( nbits ==1 ){
				node_values[(*node_it)]= controls[iid];
			}else{				
				Dout( cout<<" ctrl["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(controls, iid, nbits);				
				Dout( cout <<" > = "<<nval<< endl ) ;
				node_values[(*node_it)] = nval;
			}
		}
	}
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		(*node_it)->flag = true;
	}
	buildChecker();	
}

void SolveFromInput::addInputsToTestSet(vector<int>& input){
	int N = getInSize();
	int k = 0;
	int ctrl = 0;
	int numRepeat = 0;
	node_values.clear();
	int idx = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		node_ids[(*node_it)->id] = f_node_ids[idx];		
		(*node_it)->flag = f_flags[idx];
		
		//Dout(cout<<"NODE INIT "<<(*node_it)->name<<"  "<<node_ids[(*node_it)->id]<<"  "<<(*node_it)<<endl);	
		if((*node_it)->type == bool_node::SRC){
			int iid = (*node_it)->ion_pos;
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);	
			int nbits = srcnode->get_nbits();	
			
			Assert( nbits > 0 , "This can not happen rdu;a");
			Assert( iid+ nbits <= input.size(), "There should be a control entry for each iid b insize="<<input.size()<<"  iid+nbits="<<iid+nbits );
			Assert(input[iid ] == 1 || input[iid]==-1, "This is bad, really bad");
			
			if( nbits ==1 ){
				node_values[(*node_it)]= input[iid];
			}else{				
				Dout( cout<<" input["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(input, iid, nbits);				
				Dout( cout <<" > = "<<nval<< endl ) ;
				node_values[(*node_it)] = nval;
			}
			
			bool changed = false;
			
			for(int i=0; i<nbits; ++i){
				if(input[iid + i] != last_input[iid + i]){
					changed = true;	
				}
			}
			
			if(!changed){
				++numRepeat;
				Dout(cout<<"input "<<iid<<" unchanged"<<endl);
				(*node_it)->flag = false;
			}else{				
				Dout(cout<<"input "<<iid<<" changed"<<endl);
				(*node_it)->flag = true;
			}
			k+=nbits;
		}else{
			if(	(*node_it)->type == bool_node::CTRL ){
				(*node_it)->flag = firstTime;
				CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
				int nbits = ctrlnode->get_nbits();
				ctrl += nbits;
			}
		}
	}
	firstTime = false;
	cout<<"* RECYCLED "<<numRepeat<<" values out of "<<N<<endl;
	Assert(k == N, "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
	Assert(ctrl == getCtrlSize(), "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<ctrl<<" CONTROLS"<<endl);
	k=0;
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		node_ids[(*node_it)->id] = f_node_ids[idx];		
		(*node_it)->flag = f_flags[idx];


		if((*node_it)->type == bool_node::SRC){
			int iid = (*node_it)->ion_pos;
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);	
			int nbits = srcnode->get_nbits();	
			
			Assert( nbits > 0 , "This can not happen rdu;a");
			Assert( iid+ nbits <= input.size(), "There should be a control entry for each iid c");
			Assert(input[iid] == 1 || input[iid]==-1, "This is bad, really bad");
			
			if( nbits ==1 ){
				node_values[(*node_it)]= input[iid];
			}else{				
				Dout( cout<<" input["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(input, iid, nbits);				
				Dout( cout <<" > = "<<nval<< endl ) ;
				node_values[(*node_it)] = nval;
			}
			
			bool changed = false;
			
			for(int i=0; i<nbits; ++i){
				if(input[iid + i] != last_input[iid + i]){
					changed = true;	
				}
			}
			
			if(!changed){
				++numRepeat;
				Dout(cout<<"input "<<iid<<" unchanged"<<endl);
				(*node_it)->flag = false;
			}else{				
				Dout(cout<<"input "<<iid<<" changed"<<endl);
				(*node_it)->flag = true;
			}
			k+=nbits;
		}else{
			Assert( (*node_it)->type != bool_node::CTRL, "Specs don't have unknowns!!");
		}
	}
	Assert(k == N, "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
	FindCheckSolver::addInputsToTestSet(input);
	idx = 0;
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)->id];
		f_flags[idx] = (*node_it)->flag;
	}
	for(BooleanDAG::iterator node_it = spec->begin(); node_it != spec->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)->id];
		f_flags[idx] = (*node_it)->flag;
	}
}


void SolveFromInput::outputEuclid(ostream& fout){
		cout<<"BEFORE OUTPUTING STATE"<<endl;		
		{
			NodesToEuclid neuc(fout, "SPEC_");
			neuc.process(*spec);
		}
		{
			NodesToEuclid neuc(fout, "SKETCH_");
			neuc.process(*sketch);
		}
	}



void SolveFromInput::defineSketch(SATSolver& mng, varDir& dir){
	timerclass timer("defineSketch");
	timer.start();
	
	YES = dir.newAnonymousVar();
	dir.setYes(YES);
	Dout(cout<<"YES = "<<YES<<endl);
	mng.setVarClause(YES);
	translator(mng, dir, sketch, SOUT);
	timer.stop().print();
}

void SolveFromInput::defineSpec(SATSolver& mng, varDir& dir){
		timerclass timer("defineSpec");
	timer.start();
	Dout( cout<<"defineSpec()"<<endl );		
	translator(mng, dir, spec, OUT);
	timer.stop().print();
}

void SolveFromInput::output_control_map(ostream& out){
	FindCheckSolver::ctrl_iterator ar = begin();	
	for(BooleanDAG::iterator node_it = sketch->begin(); node_it != sketch->end(); ++node_it){
		if((*node_it)->type == bool_node::CTRL){
			int iid = (*node_it)->ion_pos;
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();		
			if( nbits > 1 ){
				for(int i=0; i<nbits; ++i){
					out<<(*node_it)->name<<"_"<< i << "\t"<<ar[iid+i]<<endl;
				}
			}else{
				out<<(*node_it)->name<<"\t"<<ar[iid]<<endl;
			}
		}
	}
}


