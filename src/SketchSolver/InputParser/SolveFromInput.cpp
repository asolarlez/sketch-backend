#include "SolveFromInput.h"
#include "NodesToSolver.h"
#include "timerclass.h"


#include "NodesToEuclid.h"
#include "DagCSE.h"
#include "DagOptim.h"
#include "DagElimUFUN.h"
#include "DagFunctionInliner.h"


template<typename T>
int intFromBV(T bv, int start, int nbits){
	int nval = 0;	
	int t = 1;
	
	for(int i=0; i<nbits; ++i){		
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



SolveFromInput::SolveFromInput(BooleanDAG* spec_p, BooleanDAG* sketch_p, SATSolver& finder, SATSolver& checker, 
map<string, BooleanDAG*>& functionMap, int p_nseeds, int inlineAmnt, int NINPUTS_p, bool mergeFunctions):FindCheckSolver(finder, checker), TIP_NAME("MITER_TIP"), NINPUTS(NINPUTS_p){
Dout( cout<<"START CONSTRUCTOR"<<endl );
	int N = spec_p->get_n_inputs();
	Nout = spec_p->get_n_outputs();
	cout<<"  Nout="<<Nout<<"  N="<<N<<endl;
	cout<<"* NINPUTS = "<<NINPUTS<<endl;
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
		Assert(specIn.size() <= sketchIn.size(), "The number of inputs in the spec_p and sketch must match");	
		for(int i=0; i<specIn.size(); ++i){
			cout<<"Matching inputs spec: "<<sketchIn[i]->name<<" with sketch: "<<specIn[i]->name<<endl;
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
		
		
		for(map<string, BooleanDAG*>::iterator it =  functionMap.begin();
					it != functionMap.end(); ++it){
			int sz1 = it->second->size(); 				
			DagOptim cse(*it->second);
			cse.process(*it->second);	
			int sz2 = it->second->size(); 		
			cout<<" optimizing "<<	it->first <<" went from size "<<sz1<<" to "<<sz2<<endl;
		}
		{
			DagOptim cse(*sketch_p);	
			cse.process(*sketch_p);			
		}
		
		/*
		if(sketch_p->size() > 1000){
			{
			DagOptim cse(*sketch_p);	
			cse.process(*sketch_p);
			}
			{
			DagOptim cse(*spec_p);	
			cse.process(*spec_p);
			}
			cout<<"* AFTER PREPROC SKETCH: SPEC nodes = "<<spec_p->size()<<"\t SKETCH nodes = "<<sketch_p->size()<<endl;	
		}
		*/
		
		{
			{
				cout<<" Inlining functions in the sketch."<<endl;
				DagFunctionInliner cse(*sketch_p, functionMap, inlineAmnt, mergeFunctions );	
				cse.process(*sketch_p);
			}
			{
				cout<<" Inlining functions in the spec."<<endl;
				DagFunctionInliner cse(*spec_p, functionMap, inlineAmnt, mergeFunctions  );	
				cse.process(*spec_p);
			}
			cout<<"* AFTER PREPROC SKETCH: SPEC nodes = "<<spec_p->size()<<"\t SKETCH nodes = "<<sketch_p->size()<<endl;
		}
		
		{
			DagElimUFUN eufun;	
			eufun.process(*spec_p);
			eufun.stopProducingFuns();
			eufun.process(*sketch_p);
		}
		cout<<"Done with ElimUFUN "<<endl;
		//cout<<"Printing spec and sketch "<<endl;
		//Dout( spec_p->print(cout) );	
		//Dout( sketch_p->print(cout) );	
		//spec_p->print(cout);
		//sketch_p->print(cout);
		spec_p->makeMiter(*sketch_p, TIP_NAME);
		problem = spec_p;
		cout<<"after Eliminating UFUNs: Problem nodes = "<<problem->size()<<endl;
		// problem->print(cout);
		//Dout( problem->print(cout) );				
		{
			DagOptim cse(*problem);	
			//cse.alterARRACS();
			cse.process(*problem);
		}		
		cout<<"* after OPTIM: Problem nodes = "<<problem->size()<<endl;		
		{
			DagOptim cse(*problem);	
			//cse.alterARRACS();
			cse.process(*problem);
		}		
		
		cout<<"* after OPTIM2: Problem nodes = "<<problem->size()<<endl;		
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
	
	
    Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
    {
	    vector<bool_node*>& problemIn = problem->getNodesByType(bool_node::CTRL);
	    cout<<"  # OF CONTROLS:    "<< problemIn.size() <<endl;
	    for(int i=0; i<problemIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);	
			int nbits = ctrlnode->get_nbits();
			declareControl(problemIn[i]->get_name(), nbits);
		}
    }
    	
	nseeds = p_nseeds;
	//cout<<"BEFORE RES"<<endl;	
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





bool SolveFromInput::check(vector<int>& controls, vector<int>& input){
	bool rv = FindCheckSolver::check(controls, input);
	int iter = 0;
	BooleanDAG* oriProblem = problem;
	int gnbits = -1;
	while(!rv){
	//this means it wasn't able to find a counterexample.
		cout<<"* growing the inputs"<<endl;
		bool keepGoing = false;
		vector<bool_node*>& specIn = problem->getNodesByType(bool_node::SRC);	
		for(int i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			if(nbits < NINPUTS  && nbits >= 2){
				gnbits = gnbits < (nbits+1) ? (nbits+1) : gnbits;			
				declareInput(specIn[i]->get_name(), nbits+1);
				srcnode->set_nbits(nbits+1);
				cout<<"* growing "<<srcnode->get_name()<<" to "<<srcnode->get_nbits()<<endl;
				keepGoing = true;
			}else{
				Dout(cout<<"* input "<<srcnode->get_name()<<" doesn't need to grow its size is already"<<srcnode->get_nbits()<<endl);	
			}
		}
		if(! keepGoing ){
			cout<<"* Done growing inputs. All integer inputs have reached size "<<NINPUTS<<endl;
			 break; 
		}
		
		cout<<" * iter = "<<iter<<"  gnbits = "<<gnbits<<endl;		
		if( iter > 2 || gnbits > 3){
			if( problem == oriProblem){
				//int* ttt = new int[9000];
				problem = hardCodeControls(controls);
				//ofstream outf("output.ucl");
				//outputEuclid(outf);
				//delete ttt;
			}
		}
		
		rv = FindCheckSolver::check(controls, input);	
		++iter;	
	}
	if( problem != oriProblem){	
		cout<<" * Cleaning up alternative problem"<<endl;	
		problem->clear();
		problem = oriProblem;	
	}
	return rv;	
}



BooleanDAG* SolveFromInput::hardCodeControls(vector<int>& controls){
	BooleanDAG* newdag = problem->clone();
	vector<bool_node*> specCtrl = newdag->getNodesByType(bool_node::CTRL);
		
	cout<<" * Specializing problem for controls"<<endl;
	cout<<" * Before specialization: nodes = "<<newdag->size()<<endl;		
	DagOptim cse(*newdag);			
	cout<<"  # OF CONTROLS:    "<< specCtrl.size() <<endl;
	for(int i=0; i<specCtrl.size(); ++i){
		CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(specCtrl[i]);	
		int iid = getCtrlStart( ctrlnode->get_name() );
		int nbits = ctrlnode->get_nbits();
		cout<<"  CONTROL:    "<< ctrlnode->get_name() <<endl;
		Assert( nbits > 0 , "This can not happen rdu;a");
		Assert( iid+ nbits <= controls.size(), "There should be a control entry for each iid a");		
		Assert(controls[iid ] == 1 || controls[iid]==-1, "This is bad, really bad");
		bool_node * repl=NULL;
		if( nbits ==1 ){
			repl = cse.getCnode( controls[iid] == 1 );						
		}else{
			int nval = intFromBV(controls, iid, nbits);	
			repl = cse.getCnode( nval);							
		}
		
		Assert( (*newdag)[ctrlnode->id] == ctrlnode , "The numbering is wrong!!");
		newdag->replace(ctrlnode->id, repl);
	}
		
	cout<<"  # OF CONTROLS:    "<< specCtrl.size() <<endl;
	
	cout<<" * After replacing nodes "<<endl;
	newdag->removeNullNodes();
	cse.process(*newdag);
	Dout( newdag->print(cout) ); 
	cout<<" * After specialization: nodes = "<<newdag->size()<<endl;
	
	
	
	
	return newdag;
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
				Dout( cout<<(*node_it)->get_name()<<" ctrl["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(controls, iid, nbits);				
				Dout( cout <<" > = "<<nval<< endl ) ;
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
	node_ids.resize(problem->size());
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
	Dout( cout<<"* RECYCLED "<<numRepeat<<" values out of "<<N<<endl );
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


