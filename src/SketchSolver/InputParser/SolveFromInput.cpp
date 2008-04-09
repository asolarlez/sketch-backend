#include "SolveFromInput.h"
#include "NodesToSolver.h"
#include "timerclass.h"


#include "NodesToEuclid.h"
#include "DagCSE.h"
#include "DagOptim.h"
#include "DagElimUFUN.h"
#include "DagFunctionInliner.h"
#include "CommandLineArgs.h"

extern CommandLineArgs* PARAMS;

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


SolveFromInput::~SolveFromInput(){
	if(last_input != NULL){
		delete[] last_input;
		last_input = NULL;
	}
}

void SolveFromInput::setup2QBF(){
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it){
		(*node_it)->flag = true;
	}
	buildChecker();
	outputCheckVarmap(cout);
}



SolveFromInput::SolveFromInput(ostream& out_p, BooleanDAG* miter, SATSolver& finder, SATSolver& checker, int p_nseeds, int NINPUTS_p):
FindCheckSolver(finder, checker), NINPUTS(NINPUTS_p), out(out_p), problem(miter){
	last_input = NULL;
	nseeds = p_nseeds;
	{
		Dout( cout<<"BEFORE declaring input names"<<endl );
		vector<bool_node*>& specIn = problem->getNodesByType(bool_node::SRC);	
		for(int i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			declareInput(specIn[i]->get_name(), nbits);
		}
	}
	
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
		if(PARAMS->verbosity > 2){ cout<<"* growing the inputs"<<endl; }
		bool keepGoing = false;
		vector<bool_node*>& specIn = problem->getNodesByType(bool_node::SRC);	
		for(int i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			if(nbits < NINPUTS  && nbits >= 2){
				gnbits = gnbits < (nbits+1) ? (nbits+1) : gnbits;			
				declareInput(specIn[i]->get_name(), nbits+1);
				srcnode->set_nbits(nbits+1);
				if(problem != oriProblem){
					bool_node* tmpn = oriProblem->get_node(srcnode->name);
					SRC_node* oriSrc = dynamic_cast<SRC_node*>(tmpn);
					oriSrc->set_nbits(nbits+1);
				}
				// cout<<"* growing "<<srcnode->get_name()<<" to "<<srcnode->get_nbits()<<endl;
				keepGoing = true;
			}else{
				Dout(cout<<"* input "<<srcnode->get_name()<<" doesn't need to grow its size is already"<<srcnode->get_nbits()<<endl);	
			}
		}
		if(! keepGoing ){
			if(PARAMS->verbosity > 2){ cout<<"* Done growing inputs. All integer inputs have reached size "<<NINPUTS<<endl; }
			 break; 
		}
		
		if(PARAMS->verbosity > 2){ cout<<" * iter = "<<iter<<"  gnbits = "<<gnbits<<endl;	}	
		if( iter > 2 || gnbits > 3){
			if( problem == oriProblem){
				//int* ttt = new int[9000];
				problem = hardCodeControls(problem, controls);
				//ofstream outf("output.ucl");
				//outputEuclid(outf);
				//delete ttt;
			}
		}
		
		rv = FindCheckSolver::check(controls, input);	
		++iter;	
	}
	if( problem != oriProblem){	
		if(PARAMS->verbosity > 2){ cout<<" * Cleaning up alternative problem"<<endl;}
		problem->clear();
		problem = oriProblem;	
	}
	return rv;	
}



BooleanDAG* SolveFromInput::hardCodeControls(BooleanDAG* dag, vector<int>& controls){
	BooleanDAG* newdag = dag->clone();
	vector<bool_node*> specCtrl = newdag->getNodesByType(bool_node::CTRL);
		
	if(PARAMS->verbosity > 2){ cout<<" * Specializing problem for controls"<<endl; }
	if(PARAMS->verbosity > 2){cout<<" * Before specialization: nodes = "<<newdag->size()<<" Ctrls = "<<  specCtrl.size() <<endl;	}
	DagOptim cse(*newdag);			
	
	for(int i=0; i<specCtrl.size(); ++i){
		CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(specCtrl[i]);	
		int iid = getCtrlStart( ctrlnode->get_name() );
		int nbits = ctrlnode->get_nbits();		
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
			
	newdag->removeNullNodes();
	cse.process(*newdag);
	Dout( newdag->print(cout) ); 
	
	if(PARAMS->verbosity > 2){ cout<<" * After replacing nodes size = "<<newdag->size()<<"CTRLS = "<< specCtrl.size() <<endl; }
	
	
	
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

/// Adds an input to the test set. This is the setup for the synthesis step.
//  From this function we call:
//		buildFinder
//  This function gets called from:
//      find.
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
						
			
			if( nbits ==1 ){
				node_values[(*node_it)]= input[iid];
				char c='U';
				if(input[iid] == -1){ c = '0'; }
				if(input[iid] == 1){ c = '1'; }

				if(PARAMS->showInputs){ cout<<" input "<<(*node_it)->get_name()<<" has value "<< c <<endl; }
			}else{				
				Dout( cout<<" input["<< iid <<"::"<< nbits <<"] = < ");
				int nval = intFromBV(input, iid, nbits);				
				Dout( cout <<" > = "<<nval<< endl ) ;
				node_values[(*node_it)] = nval;
				if(PARAMS->showInputs){ cout<<" input "<<(*node_it)->get_name()<<" has value "<<nval<<endl; }
			}
			
			bool changed = false;
			
			for(int i=0; i<nbits; ++i){
				if(input[iid + i] != last_input[iid + i]){
					changed = true;	
				}
				last_input[iid + i] = input[iid + i];
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
	if(PARAMS->verbosity > 2){ cout<<"* RECYCLED "<<numRepeat<<" values out of "<<N<<endl ; }
	Assert(k == N, "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
	Assert(ctrl == getCtrlSize(), "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<ctrl<<" CONTROLS"<<endl);	
	//FindCheckSolver::addInputsToTestSet(input);
	buildFinder();
	idx = 0;
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it, ++idx){
		f_node_ids[idx] = node_ids[(*node_it)->id];
		f_flags[idx] = (*node_it)->flag==1;
	}
}



void SolveFromInput::declareInput(const string& inname, int size){
	FindCheckSolver::declareInput(inname, size);
	if(last_input != NULL){
		delete[] last_input;
		last_input = new int[getInSize()];
		for(int i=0; i<getInSize(); ++i){ last_input[i] = 0; };
	}
}




void SolveFromInput::outputEuclid(ostream& fout){
		cout<<"BEFORE OUTPUTING STATE"<<endl;		
		{
			NodesToEuclid neuc(fout, "PROBLEM_");
			neuc.process(*problem);
		}		
	}





void SolveFromInput::defineProblem(SATSolver& mng, SolverHelper& dir){
	{
		timerclass timer("defineProblem");
		timer.start();
		YES = dir.newAnonymousVar();
		dir.setYes(YES);
		Dout(cout<<"YES = "<<YES<<endl);
		mng.setVarClause(YES);
		
		NodesToSolver nts(dir, "PROBLEM", node_values, node_ids);		
		nts.process(*problem);		
		timer.stop();
		if(PARAMS->verbosity > 2){ timer.print(); }
	}
}

void SolveFromInput::output_control_map(ostream& out){
	FindCheckSolver::ctrl_iterator ar = begin();	
	vector<bool_node*>& controls = problem->getNodesByType(bool_node::CTRL);
	for(BooleanDAG::iterator node_it = controls.begin(); node_it != controls.end(); ++node_it){		
			int iid = getCtrlStart( (*node_it)->get_name() );
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();		
			if( nbits > 1 ){
				for(int i=0; i<nbits; ++i){
					out<<(*node_it)->name<<"_"<< i << "\t"<<ar[iid+i]<<endl;
					//cout<<(*node_it)->name<<"_"<< i <<"["<<(iid+i)<<"] = \t"<<ar[iid+i]<<endl;
				}
			}else{
				out<<(*node_it)->name<<"\t"<<ar[iid]<<endl;
			}
		
	}
}


