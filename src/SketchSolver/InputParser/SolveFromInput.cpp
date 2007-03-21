#include "SolveFromInput.h"
#include "NodesToSolver.h"
#include "timerclass.h"


#include "NodesToEuclid.h"
#include "DagCSE.h"
#include "DagOptim.h"
#include "DagElimUFUN.h"


template<typename T>
int intFromBV(T bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){
		( cout<< bv[start + i] << "  " );
		if( bv[start + i] > 0){
			nval += t;	
		}
		t = t*2;
	}
	return nval;
}



void SolveFromInput::setup2QBF(){
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it){
		(*node_it)->flag = true;
	}
	buildChecker();
	outputCheckVarmap(cout);
}



SolveFromInput::SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker, int NS_p):FindCheckSolver(finder, checker), TIP_NAME("MITER_TIP"){
Dout( cout<<"START CONSTRUCTOR"<<endl );
	int N = spec_p->get_n_inputs();
	Nout = spec_p->get_n_outputs();
	cout<<"  Nout="<<Nout<<"  N="<<N<<endl;
	cout<<"* before  EVERYTHING: SPEC nodes = "<<spec_p->size()<<"\t SKETCH nodes = "<<sketch_p->size()<<endl;	
Dout( cout<<"BEFORE CLEANUP"<<endl );
  	sketch_p->cleanup(false);
  	spec_p->cleanup(false);
Dout( cout<<"BEFORE SORT"<<endl );  	  	
    sketch_p->sort_graph();
    spec_p->sort_graph();
Dout( cout<<"BEFORE RELABEL"<<endl );
    spec_p->relabel();
    sketch_p->relabel();
	
	cout<<"* before  CSE: SPEC nodes = "<<spec_p->size()<<"\t SKETCH nodes = "<<sketch_p->size()<<endl;
	
	{
		Dout( cout<<"BEFORE Matching input names"<<endl );
		vector<bool_node*>& specIn = spec_p->getNodesByType(bool_node::SRC);
		vector<bool_node*>& sketchIn = sketch_p->getNodesByType(bool_node::SRC);
		Assert(specIn.size() == sketchIn.size(), "The number of inputs in the spec_p and sketch must match");	
		for(int i=0; i<specIn.size(); ++i){
			sketch_p->rename(sketchIn[i]->name, specIn[i]->name);
		}
	}
	
	{
		Dout( cout<<"BEFORE Matching output names"<<endl );
		vector<bool_node*>& specDST = spec_p->getNodesByType(bool_node::DST);
		vector<bool_node*>& sketchDST = sketch_p->getNodesByType(bool_node::DST);
		Assert(specDST.size() == sketchDST.size(), "The number of inputs in the spec_p and sketch must match");	
		for(int i=0; i<sketchDST.size(); ++i){
			sketch_p->rename(sketchDST[i]->name, specDST[i]->name);			
		}
	}
	
	
	if( false ){	
		Dout( cout<<"BEFORE MAKING MITER"<<endl );	
		sketch_p->makeMiter(*spec_p, TIP_NAME);
		problem = sketch_p;
	   	Dout( cout<<"after makeMiter "<<endl);
		Dout( problem->print(cout) );	
				
		{
			DagOptim cse(*problem);	
			cse.process(*problem);
		}
		cout<<"after OPTIM: Problem nodes = "<<problem->size()<<endl;	
		Dout( problem->print(cout) );	
		
			
		
		{
			DagElimUFUN eufun;	
			eufun.process(*problem);			
		}	
		problem->cleanup(false);
		problem->sort_graph();
		problem->relabel();	
		cout<<"after Eliminating UFUNs: Problem nodes = "<<problem->size()<<endl;
		
			
		{
			DagOptim cse(*problem);	
			cse.process(*problem);
		}	
		cout<<"* after Second optim: Problem nodes = "<<problem->size()<<endl;
	}else{
		{
			DagElimUFUN eufun;	
			eufun.process(*spec_p);
			eufun.stopProducingFuns();
			eufun.process(*sketch_p);
		}
		cout<<"Done with ElimUFUN "<<endl;
		cout<<"Printing spec and sketch "<<endl;
		Dout( spec_p->print(cout) );	
		Dout( sketch_p->print(cout) );	
		
		spec_p->makeMiter(*sketch_p, TIP_NAME);
		problem = spec_p;
		cout<<"after Eliminating UFUNs: Problem nodes = "<<problem->size()<<endl;
		Dout( problem->print(cout) );				
		{
			DagOptim cse(*problem);	
			cse.process(*problem);
		}
		problem->cleanup(false);
		problem->sort_graph();
		problem->relabel();	
		cout<<"* after OPTIM: Problem nodes = "<<problem->size()<<endl;	
		Dout( problem->print(cout) );
		
	}
	
	
	
	
	
	
	
	{
		Dout( cout<<"BEFORE declaring input names"<<endl );
		vector<bool_node*>& specIn = problem->getNodesByType(bool_node::SRC);	
		for(int i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			declareInput(specIn[i]->get_name(), nbits);
		}
	}
	
	
	Dout( problem->print(cout) );	
	
cout<<"BEFORE DC"<<endl;	
    Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
    {
	    vector<bool_node*>& problemIn = problem->getNodesByType(bool_node::CTRL);
	    for(int i=0; i<problemIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);	
			int nbits = ctrlnode->get_nbits();
			declareControl(problemIn[i]->get_name(), nbits);
		}
    }
    	
	nseeds = NS_p;
cout<<"BEFORE RES"<<endl;	
	int totSize = problem->size();
	f_node_ids.resize( totSize , 0 );
	f_flags.resize( totSize , true);
	
	cout<<"Random seeds = "<<nseeds<<endl;	

	
	
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it){
		(*node_it)->flag = true;
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






void SolveFromInput::setNewControls(vector<int>& controls){
	int idx = 0;	
	node_values.clear();
	node_ids.clear();
	node_ids.resize( problem->size() );
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it, ++idx){
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::CTRL ){
			int iid = getCtrlStart( (*node_it)->get_name() );
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();
			Assert( nbits > 0 , "This can not happen rdu;a");
			Assert( iid+ nbits <= controls.size(), "There should be a control entry for each iid a");		
			Assert(controls[iid ] == 1 || controls[iid]==-1, "This is bad, really bad");
			if( nbits ==1 ){
				node_values[(*node_it)]= controls[iid];
			}else{				
				( cout<<(*node_it)->get_name()<<" ctrl["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(controls, iid, nbits);				
				( cout <<" > = "<<nval<< endl ) ;
				node_values[(*node_it)] = nval;
			}
		}
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
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it, ++idx){
		node_ids[(*node_it)->id] = f_node_ids[idx];		
		(*node_it)->flag = f_flags[idx];
		
		//Dout(cout<<"NODE INIT "<<(*node_it)->name<<"  "<<node_ids[(*node_it)->id]<<"  "<<(*node_it)<<endl);	
		if((*node_it)->type == bool_node::SRC){
			int iid = getInStart( (*node_it)->get_name() );
			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);	
			int nbits = srcnode->get_nbits();	
			
			Assert( nbits == getInSize( (*node_it)->get_name() ) , "Size missmatch for input "<<(*node_it)->get_name() );
			
			Assert( nbits > 0 , "This can not happen rdu;a");
			Assert( iid+ nbits <= input.size(), "There should be a control entry for each iid b insize="<<input.size()<<"  iid+nbits="<<iid+nbits );
			Assert(input[iid ] == 1 || input[iid]==-1, "This is bad, really bad");
			
			Dout( cout<<" input "<<(*node_it)->get_name()<<" starts "<<iid<<"  has value"<<input[iid]<<endl );
			
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
	//FindCheckSolver::addInputsToTestSet(input);
	buildFinder();
	idx = 0;
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)->id];
		f_flags[idx] = (*node_it)->flag;
	}
}


void SolveFromInput::outputEuclid(ostream& fout){
		cout<<"BEFORE OUTPUTING STATE"<<endl;		
		{
			NodesToEuclid neuc(fout, "PROBLEM_");
			neuc.process(*problem);
		}		
	}





void SolveFromInput::defineProblem(SATSolver& mng, varDir& dir){
	{
		timerclass timer("defineProblem");
		timer.start();
		YES = dir.newAnonymousVar();
		dir.setYes(YES);
		Dout(cout<<"YES = "<<YES<<endl);
		mng.setVarClause(YES);
		
		NodesToSolver nts(mng, dir, "PROBLEM", node_values, node_ids);		
		nts.process(*problem);		
		bool_node* miterTip = problem->get_node(TIP_NAME);
		Tvalue tv = node_ids[ miterTip->id ];
		Assert( tv.isBvect() && tv.getSize()==1 , "This is very strange");
		if(tv.getId() == -YES ) {  cout<<" The last assert is statically unsatisfiable "<<endl; }
		dir.addAssertClause(tv.getId());
		timer.stop().print();
	}
}

void SolveFromInput::output_control_map(ostream& out){
	FindCheckSolver::ctrl_iterator ar = begin();	
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it){
		if((*node_it)->type == bool_node::CTRL){
			int iid = getCtrlStart( (*node_it)->get_name() );
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();		
			if( nbits > 1 ){
				for(int i=0; i<nbits; ++i){
					out<<(*node_it)->name<<"_"<< i << "\t"<<ar[iid+i]<<endl;
					cout<<(*node_it)->name<<"_"<< i <<"["<<(iid+i)<<"] = \t"<<ar[iid+i]<<endl;
				}
			}else{
				out<<(*node_it)->name<<"\t"<<ar[iid]<<endl;
			}
		}
	}
}


