#include "CEGISSolver.h"
#include "timerclass.h"
#include <ctime>
#include <queue>
#include "CommandLineArgs.h"
#include "Tvalue.h"
#include "DagOptim.h"
#include "NodesToSolver.h"
#include "NodesToEuclid.h"
#include "NodeSlicer.h"
#include "BackwardsAnalysis.h"

extern CommandLineArgs* PARAMS;

CEGISSolver::CEGISSolver(BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, int p_nseeds, int NINPUTS_p):
dirFind(finder), 
dirCheck(checker), 
lastFproblem(NULL),
mngFind(finder.getMng()),
mngCheck(checker.getMng()),
printDiag(false),
nseeds(p_nseeds),
NINPUTS(NINPUTS_p),
iterlimit(-1),
randseed(time(NULL))
{
	problemStack.push(miter);
	BooleanDAG* problem = miter;
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
	    if(PARAMS->verbosity > 2){
			cout<<"  # OF CONTROLS:    "<< problemIn.size() <<endl;
		}
		int cints = 0;
		int cbits = 0;
	    for(int i=0; i<problemIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);	
			int nbits = ctrlnode->get_nbits();
			if(ctrlnode->getOtype() == bool_node::BOOL){
				cbits++;
			}else{
				cints++;
			}
			/* cout<<" i ="<<i<<"\t"<<problemIn[i]->get_name()<<endl; */
			declareControl(problemIn[i]->get_name(), nbits);
		}
		if(PARAMS->verbosity > 2){
			cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<<endl;
		}
    }
	setup();
}


void CEGISSolver::setup(){
	BooleanDAG* problem = getProblem();
	int totSize = problem->size();
	
	cout<<"Random seeds = "<<nseeds<<endl;	
	
	for(BooleanDAG::iterator node_it = problem->begin(); node_it != problem->end(); ++node_it){
		(*node_it)->flag = true;
	}	

	firstTime=true;
}

CEGISSolver::~CEGISSolver(void)
{
}


void CEGISSolver::declareControl(const string& cname, int size){
	Dout(cout<<"DECLARING CONTROL "<<cname<<" "<<size<<endl);
	dirFind.declareInArr(cname, size);
	ctrlStore.newVar(cname, size);	
}



void CEGISSolver::declareInput(const string& inname, int size){
	//Inputs can be redeclared to change their sizes, but not controls.
	Dout(cout<<"DECLARING INPUT "<<inname<<" "<<size<<endl);
	cpt.resizeInput(inname, size);
	if( !inputStore.contains(inname)){
		inputStore.newVar(inname, size);		
		Dout( cout<<" INPUT "<<inname<<" sz = "<<size<<endl );
	}else{
		inputStore.resizeVar(inname, size);
	}
}

bool CEGISSolver::solve(){	
	if(PARAMS->verbosity > 1){
		cout<<"inputSize = "<<inputStore.getBitsize()<<"\tctrlSize = "<<ctrlStore.getBitsize()<<endl;
	}
	
	srand(randseed);
	inputStore.makeRandom();
	for(int ns = 0; ns < (nseeds-1); ++ns){			
		cout<<"!%";	inputStore.printBrief(cout); cout<<endl;
                // NOTE - newer gcc (4.4) won't accept T --> T& if a variable isn't assigned
                std::vector<int, std::allocator<int> > instore_serialized =
                    inputStore.serialize();
		cpt.checkpoint('f', instore_serialized);
		addInputsToTestSet(inputStore);	
		inputStore.makeRandom();		
	}
	
	bool succeeded = solveCore();
		
	return succeeded;
}

bool CEGISSolver::solveCore(){	
	int iterations = 0;
	bool fail = false;
 	bool doMore=true;
	timerclass ftimer("* FIND TIME");
	timerclass ctimer("* CHECK TIME");
	timerclass ttimer("* TOTAL TIME");
	ttimer.start();
	while(doMore){
		{// Find
			// cout<<"!%";	for(int i=0; i< input.size(); ++i) cout<<" "<<(input[i]==1?1:0); cout<<endl;
                        std::vector<int, std::allocator<int> > instore_serialized =
                            inputStore.serialize();
			cpt.checkpoint('f', instore_serialized);
			if(PARAMS->verbosity > 1 || PARAMS->showInputs){ cout<<"BEG FIND"<<endl; }
			ftimer.restart(); 
			doMore = find(inputStore, ctrlStore);
			ftimer.stop();
			if(PARAMS->verbosity > 1 || PARAMS->showInputs){  cout<<"END FIND"<<endl; }
			if(!doMore){
				cout<<"******** FAILED ********"<<endl;	
				ftimer.print();	ctimer.print();
				fail = true;
				break;
			}
		}
		print_control_map(cout);
		{ // Check
			if(PARAMS->verbosity > 4){ cout<<"!+ ";ctrlStore.printBrief(cout); cout<<endl;}
                        std::vector<int, std::allocator<int> > ctrlstore_serialized = ctrlStore.serialize();
			cpt.checkpoint('c', ctrlstore_serialized);
			if(PARAMS->verbosity > 1){ cout<<"BEG CHECK"<<endl; }
			ctimer.restart(); 
			doMore = check(ctrlStore, inputStore);
			 ctimer.stop();
			if(PARAMS->verbosity > 1){ cout<<"END CHECK"<<endl; }
			if(doMore && PARAMS->olevel >= 7){ abstractProblem(); }
		}
		if(PARAMS->verbosity > 0){cout<<"********  "<<iterations<<"\tftime= "<<ftimer.get_cur_ms() <<"\tctime= "<<ctimer.get_cur_ms()<<endl; }
		++iterations;
		if( iterlimit > 0 && iterations >= iterlimit){ cout<<" * bailing out due to iter limit"<<endl; fail = true; break; }
	}
	ttimer.stop();
	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;		
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	cout<<" *"<<"FIND TIME "<<ftimer.get_tot_ms()<<" CHECK TIME "<<ctimer.get_tot_ms()<<" TOTAL TIME "<<ttimer.get_tot_ms()<<endl;
	return !fail;
}



void CEGISSolver::addInputsToTestSet(VarStore& input){
	map<bool_node*,  int> node_values;
	bool specialize = PARAMS->olevel >= 7;
	BooleanDAG* tmpproblem = NULL;	
	if(!specialize){
		bool sameProblem = getProblem() == lastFproblem;//indicates whether this is the same problem as last time.
		if(find_node_ids.size() != getProblem()->size()){
			find_node_ids.resize(getProblem()->size());
			Assert(!sameProblem, "How could this be true!?");
		}
		int numRepeat=0;
		int idx = 0;
		int k=0, ctrl=0;
		vector<bool_node*>& srcList = getProblem()->getNodesByType(bool_node::SRC);
		for(BooleanDAG::iterator node_it = srcList.begin(); node_it != srcList.end(); ++node_it, ++idx){			
			//Dout(cout<<"NODE INIT "<<(*node_it)->name<<"  "<<node_ids[(*node_it)->id]<<"  "<<(*node_it)<<endl);	
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);	
			int nbits;
			node_values[(*node_it)] = valueForINode(srcnode, input, nbits);
			string name = srcnode->get_name();
			bool changed = true;
			map<string, int>::iterator fit = last_input.find(name);
			if( fit != last_input.end() &&  fit->second == input[name]){
				changed = false;
			}
			last_input[name] = input[name];
			changed = changed || !sameProblem;

			if(!changed){ ++numRepeat;	}
			Dout(cout<<"input "<<name<<(changed? " changed" :" unchanged")<<endl);

			(*node_it)->flag = changed;

			k+=nbits;
		}
		vector<bool_node*>& ctrList = getProblem()->getNodesByType(bool_node::CTRL);
		for(BooleanDAG::iterator node_it = ctrList.begin(); node_it != ctrList.end(); ++node_it, ++idx){			
			//Dout(cout<<"NODE INIT "<<(*node_it)->name<<"  "<<node_ids[(*node_it)->id]<<"  "<<(*node_it)<<endl);	
			(*node_it)->flag = firstTime || !sameProblem;
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();
			ctrl += nbits;
		}

		firstTime = false;
		if(PARAMS->verbosity > 2){ cout<<"* RECYCLED "<<numRepeat<<" values out of "<<input.getIntsize()<<endl ; }
		Assert(k == input.getBitsize(), "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<k<<" INPUTS"<<endl);
		Assert(ctrl == ctrlStore.getBitsize(), "THIS SHOULDN'T HAPPEN!!! PROCESSED ONLY "<<ctrl<<" CONTROLS"<<endl);	
	}else{
		pushProblem(hardCodeINode(getProblem(), input, bool_node::SRC));
		find_node_ids.resize(getProblem()->size());
	}
	//FindCheckSolver::addInputsToTestSet(input);
	lastFproblem = getProblem();
	defineProblem(mngFind, dirFind, node_values, find_node_ids);
	if(specialize){
		popProblem();
		find_node_ids.clear();
	}
}


BooleanDAG* CEGISSolver::hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type){
	BooleanDAG* newdag = dag->clone();
	vector<bool_node*> inodeList = newdag->getNodesByType(type);
		
	if(PARAMS->verbosity > 2){ cout<<" * Specializing problem for "<<(type == bool_node::CTRL? "controls" : "inputs")<<endl; }
	if(PARAMS->verbosity > 2){cout<<" * Before specialization: nodes = "<<newdag->size()<<" Ctrls = "<<  inodeList.size() <<endl;	}
	{
		DagOptim cse(*newdag);			
		
		for(int i=0; i<inodeList.size(); ++i){
			INTER_node* inode = dynamic_cast<INTER_node*>(inodeList[i]);	
			int nbits;
			int t = valueForINode(inode, values, nbits);
			bool_node * repl=NULL;
			if( nbits ==1 ){
				repl = cse.getCnode( t == 1 );						
			}else{
				repl = cse.getCnode( t);							
			}
			
			Assert( (*newdag)[inode->id] == inode , "The numbering is wrong!!");
			newdag->replace(inode->id, repl);
		}
				
		newdag->removeNullNodes();
		cse.process(*newdag);
	}
	Dout( newdag->print(cout) ); 
	
	if(PARAMS->verbosity > 2){ cout<<" * After replacing nodes size = "<<newdag->size()<<" Ctrls = "<< inodeList.size() <<endl; }	
	if(false){
		BackwardsAnalysis ba;
		ba.process(*newdag);
	}
	if(false){
		DagOptim cse(*newdag);			
		cse.process(*newdag);
	}
	if(PARAMS->verbosity > 2){ cout<<" * And after optims it became = "<<newdag->size()<<endl; }	
	return newdag;
}





void CEGISSolver::defineProblem(SATSolver& mng, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids){
	{
		timerclass timer("defineProblem");
		timer.start();
		int YES = dir.newAnonymousVar();
		dir.setYes(YES);
		Dout(cout<<"YES = "<<YES<<endl);
		mng.setVarClause(YES);
		
		NodesToSolver nts(dir, "PROBLEM", node_values, node_ids);		
		nts.process(*getProblem());		
		timer.stop();
		if(PARAMS->verbosity > 2){ timer.print(); }
	}
}




int CEGISSolver::valueForINode(INTER_node* inode, VarStore& values, int& nbits){
			int retval = 0;
			nbits = inode->get_nbits();		
			if(nbits > 1){
				retval = values[inode->get_name()];
			}else{
				retval = values[inode->get_name()]==1 ? 1 : -1;
			}
			if(PARAMS->showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< retval <<endl; }
			return retval;
}




bool CEGISSolver::find(VarStore& input, VarStore& controls){
		
	timerclass tc("* TIME TO ADD INPUT ");
	tc.start();				
	addInputsToTestSet(input);
	tc.stop();
	if(PARAMS->verbosity > 2){ tc.print(); }
	
//Solve
	int result = mngFind.solve();
	if(printDiag){
	  	cout<<"# FIND DIAGNOSTICS"<<endl;
		printDiagnostics(mngFind, 'f');
	}
    if (result != SATSolver::SATISFIABLE){ 	//If solve is bad, return false.    	
    	if( result != SATSolver::UNSATISFIABLE){
	    	switch( result ){
	    	   	case SATSolver::UNDETERMINED: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::TIME_OUT: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::MEM_OUT:  throw new SolverException(result, "MEM_OUT"); break;
	    		case SATSolver::ABORTED:  throw new SolverException(result, "ABORTED"); break;
	    	}    			
    	}
    	return false;
    }
	Dout( dirFind.print() );
	//dirFind.printAllVars();
//Get the values of the Controls.
	int jj=0;
	for(VarStore::iterator it = controls.begin(); it !=controls.end(); ++it){
		const string& cname = it->name;
		int cnt = dirFind.getArrSize(cname);
		Assert( cnt == it->size(), "SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
		for(int i=0; i<cnt; ++i){
			int val = mngFind.getVarVal(dirFind.getArr(cname, i));
			it->setBit(i, (val==1) ? 1 : 0);			
		}
	}
	mngFind.reset();
	return true;
//Return true.
}

void CEGISSolver::abstractProblem(){
	VarStore tmp = join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;	
	BooleanDAG* dag = getProblem()->clone();
	NodeEvaluator eval(empty, *dag);
	int orisize = dag->size();
	eval.run(tmp);
	vector<bool_node*> asserts = dag->getNodesByType(bool_node::ASSERT);
	bool found = false;
	int failedpos = -1;
	for(BooleanDAG::iterator node_it = asserts.begin(); node_it != asserts.end(); ++node_it){
		if(found){
			dag->remove((*node_it)->id);
		}else{
			if(eval.getValue(**node_it)==0){				
				found = true;
				ASSERT_node* an = dynamic_cast<ASSERT_node*>(*node_it);
				cout<<" candidate failed assertion "<<an->getMsg()<<endl;
			}else{
				failedpos++;
			}
		}
	}
	dag->removeNullNodes();
	dag->cleanup();
	int cutoff = ((asserts.size()*6)/10);
	cout<<" failedpos = "<<failedpos<<"   cutoff = "<<cutoff<<endl;
	if(failedpos<cutoff){		
		pushProblem(dag);
		cout<<"Level "<<problemLevel()<<"Replacing dag of size "<<orisize<<" with size "<<dag->size()<<endl;
	}else{
		dag->clear();
		delete dag;
	}
}


bool CEGISSolver::simulate(VarStore& controls, VarStore& input){
	timerclass tc("simtimer");
	tc.start();	
	int iter = 0;
	VarStore& tmpin = input;
	map<string, BooleanDAG*> empty;
	vector<VarStore> expensive;
	BooleanDAG* dag =getProblem();
	if(dag->getNodesByType(bool_node::ASSERT).size()==0){
		tc.stop().print("no cex");
		return false;
	}
	bool hasCtrls = dag->getNodesByType(bool_node::CTRL).size()!=0;
	dag = dag->clone();
	pushProblem(dag);
	do{
		++iter;		
		NodeEvaluator eval(empty, *dag);		
		for(int i=0; i<40; ++i){
			tmpin.makeRandom();
			bool done = eval.run(hasCtrls? join(tmpin, controls)  : tmpin);
			eval.trackChanges();
			if(done){
				tc.stop().print("found a cex");
				popProblem();
				return true;
			}
		}
		if(expensive.size()>0){
			cout<<"adding expensive ones."<<endl;
			for(int i=0; i<expensive.size(); ++i){
				eval.run(hasCtrls? join(expensive[i], controls) : expensive[i]);
			}
		}
		while(true){
			int h = eval.scoreNodes();
			bool_node* niq = (*dag)[h];
			ASSERT_node* an = new ASSERT_node();
			int am = 0;
			if(niq->getOtype()==bool_node::BOOL){
				if(eval.getValue(niq)==0){
					an->mother = new NOT_node();
					am = 1;
				}
			}else{
				an->mother = new EQ_node();
				an->mother->father = new CONST_node( eval.getValue(niq) );
				am = 2;
			}
			BooleanDAG* tbd = dag->slice(h, an);
			pushProblem(tbd);
			
			timerclass tc("check time");
			tc.start();
			bool rv = baseCheck(controls, tmpin);
			tc.stop().print();
			popProblem();
			if(am>0){
				if(am==2){ delete an->mother->father; }
				delete an->mother; 
			}
			delete an;
			dag->relabel();
			if(rv){
				bool done = eval.run(hasCtrls? join(tmpin, controls)  : tmpin);
				if(done){
					tc.stop().print("found a cex");
					popProblem();
					return true;
				}else{
					expensive.push_back(tmpin);
				}
			}else{
				{
					DagOptim cse(*dag);
					int sz = dag->size();
					dag->replace(h, cse.getCnode(eval.getValue((*dag)[h])));
					dag->removeNullNodes();
					cse.process(*dag);
					cout<<" reduced size from "<<sz<<" to "<<dag->size()<<endl;
					if(dag->getNodesByType(bool_node::ASSERT).size()==0){
						tc.stop().print("no cex");
						popProblem();
						return false;
					}
				}
				break;
			}
		}
	}while(iter < 4);
	tc.stop().print("didn't find a cex");	
	{
		BackwardsAnalysis ba;
		ba.process(*dag);
	}
	{
		DagOptim cse(*dag);			
		cse.process(*dag);
	}
	if(PARAMS->verbosity > 2){ cout<<" * And after optims it became = "<<dag->size()<<endl; }	
	bool tv = baseCheck(controls, input);
	popProblem();
	return tv;
}


void CEGISSolver::normalizeInputStore(){
	VarStore tmp = join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;
	NodeSlicer slicer(empty, tmp, *getProblem());
	slicer.process(*getProblem());
	for(VarStore::iterator it = inputStore.begin(); it != inputStore.end(); ++it){
		if(!slicer.isInfluential(it->name)){
			it->setVal(last_input[it->name]);		
		}
	}
}


bool CEGISSolver::growInputs(BooleanDAG* dag, BooleanDAG* oridag){
	int gnbits = -1;
	if(PARAMS->verbosity > 2){ cout<<"* growing the inputs"<<endl; }
	bool keepGoing = false;
	vector<bool_node*>& specIn = dag->getNodesByType(bool_node::SRC);	
	for(int i=0; i<specIn.size(); ++i){			
		SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
		int nbits = srcnode->get_nbits();
		if(nbits < NINPUTS  && nbits >= 2){
			gnbits = gnbits < (nbits+1) ? (nbits+1) : gnbits;			
			declareInput(specIn[i]->get_name(), nbits+1);
			srcnode->set_nbits(nbits+1);
			if(dag != oridag){
				bool_node* tmpn = oridag->get_node(srcnode->name);
				SRC_node* oriSrc = dynamic_cast<SRC_node*>(tmpn);
				oriSrc->set_nbits(nbits+1);
			}
			// cout<<"* growing "<<srcnode->get_name()<<" to "<<srcnode->get_nbits()<<endl;
			keepGoing = true;
		}else{
			Dout(cout<<"* input "<<srcnode->get_name()<<" doesn't need to grow its size is already"<<srcnode->get_nbits()<<endl);	
		}
	}
	cout<<"* nbits = "<<gnbits;
	return keepGoing;
}


bool CEGISSolver::check(VarStore& controls, VarStore& input){	
	bool rv;
	BooleanDAG* oriProblem = getProblem();
	bool dot = true;
	bool pushedNewP = false;
	/*while(problemLevel() != 1){
		if(PARAMS->verbosity > 2){ 
			cout<<" * Cleaning up alternative problem level "<<problemLevel()<<endl;
		}
		popProblem();
	}*/
	int plevel = problemLevel();
	do{		
		if(PARAMS->olevel >= 7 && dot){
			oriProblem = getProblem();
			pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL));
			//getProblem()->lprint(cout);
			pushedNewP = true;
			dot = false;
		}
		rv = simulate(controls, input);	
		//rv = baseCheck(controls, input);		
		if(!rv){			
			if(plevel == 1){
				//In this case, we are at the top, so if it succeeds we grow.							
				bool keepGoing = growInputs(getProblem(), oriProblem);
				if(! keepGoing ){
					if(pushedNewP){	
						if(PARAMS->verbosity > 2){ cout<<" * Cleaning up alternative problem"<<endl;}
						popProblem();
						dot =true;
					}
					if(PARAMS->verbosity > 2){ cout<<"* Done growing inputs. All integer inputs have reached size "<<NINPUTS<<endl; }
					 return false;
				}				
			}else{
				//In this case, we are not at the top, so if it succeeds we pop.			
				if(pushedNewP){	
					if(PARAMS->verbosity > 2){ 
						cout<<" * Cleaning up alternative problem level "<<problemLevel()<<endl;
					}
					popProblem();
					dot =true;
				}
				cout<<" * Popping simplified problem"<<endl;
				popProblem();
				plevel = problemLevel();
			}
		
		}
	}while(!rv);
	if(pushedNewP){	
		if(PARAMS->verbosity > 2){ cout<<" * Cleaning up alternative problem"<<endl;}
		popProblem();
		dot =true;
	}



	return rv;	
}



bool CEGISSolver::baseCheck(VarStore& controls, VarStore& input){
	Dout( cout<<"check()"<<endl );
	timerclass tc("* TIME TO ADD CONTROLS ");
	tc.start();				
	setNewControls(controls);
	if(PARAMS->verbosity > 2){ tc.stop().print(); }
	
    int result = mngCheck.solve();
	//dirCheck.printAllVars();
    if(printDiag){
	    cout<<"# CHECK DIAGNOSTICS"<<endl;
		printDiagnostics(mngCheck, 'c');
    }
    if (result != SATSolver::SATISFIABLE){
    	mngCheck.reset();
    	if( result != SATSolver::UNSATISFIABLE){
	    	switch( result ){
	    	   	case SATSolver::UNDETERMINED: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::TIME_OUT: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::MEM_OUT:  throw new SolverException(result, "MEM_OUT"); break;
	    		case SATSolver::ABORTED:  throw new SolverException(result, "ABORTED"); break;
	    	}
    	}
    	return false;
    }
    
    
    
	for(VarStore::iterator it = input.begin(); it !=input.end(); ++it){
		const string& cname = it->name;
		if(dirCheck.checkVar(cname)){
			int cnt = dirCheck.getArrSize(cname);
			Assert( cnt == it->size(), "SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
			for(int i=0; i<cnt; ++i){
				
				int val = mngCheck.getVarVal(dirCheck.getArr(cname, i));
				it->setBit(i, (val==1)? 1 : 0);						
			}
			Dout( cout<<" input "<<cname<<"  has value "<<it->getInt()<<endl );
		}
	}
	Dout( dirCheck.print() );
	mngCheck.reset();
	return true;
}





void CEGISSolver::setNewControls(VarStore& controls){
	int idx = 0;	
	map<bool_node*,  int> node_values;
	check_node_ids.clear();
	check_node_ids.resize( getProblem()->size() );
	mngCheck.clean();
	dirCheck.reset();
	for(BooleanDAG::iterator node_it = getProblem()->begin(); node_it != getProblem()->end(); ++node_it, ++idx){
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::CTRL ){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);	
			int nbits = ctrlnode->get_nbits();
			node_values[(*node_it)] = valueForINode(ctrlnode, controls, nbits);
		}
		if(	(*node_it)->type == bool_node::SRC ){
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);	
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits());
		}
	}	
	defineProblem(mngCheck, dirCheck, node_values, check_node_ids);
}



bool CEGISSolver::solveFromCheckpoint(istream& in){
	timerclass ctimer("* CHECK TIME");
	
	int inputSize = inputStore.getBitsize();
	int ctrlSize = ctrlStore.getBitsize();
	
	cout<<"inputSize = "<<inputSize<<"\tctrlSize = "<<ctrlSize<<endl;
	
	srand(randseed);
	int maxSize = (ctrlSize>inputSize? ctrlSize : inputSize)+2;

	char* buff = new char[maxSize];
	char last = 'n';
	bool unaddedInput = false;
	
	queue<pair<string, int> > resizelist;
	
	while(!in.eof() && in.good()){
		//in.getline(buff, maxSize, '\n');
		in>>buff;
		if( buff[0] == '\0' ) continue;
		cout<<" Read "<<buff<<endl;
		if(buff[0] == 'f'){
			if( unaddedInput ){ 
				while(resizelist.size()>0){
					pair<string, int> p = resizelist.front();
					resizelist.pop();
					declareInput(p.first, p.second);	
				}
				addInputsToTestSet(inputStore); }
			for(int i=0; i<inputSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE f "<<i<<" of "<<inputSize<<"  "<<buff[i+1]);
				inputStore.setBit(i, buff[i+1]=='1'? 1 : -1);
			}
			unaddedInput = true;
			last = 'f';
		}else 
		if(buff[0] == 'c'){
			for(int i=0; i<ctrlSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE c "<<i<<" of "<<ctrlSize<<"  "<<buff[i+1]);
				ctrlStore.setBit(i, buff[i+1]=='1'? 1 : -1);
			}
			last = 'c';
		}else
		if(buff[0] == 'r'){
			string sbuf(&buff[1]);
			int brk = sbuf.find(' ', 0);
			string name = sbuf.substr(0, brk);
			int size = atoi(sbuf.substr(brk).c_str());
			resizelist.push(make_pair(name, size));			
		}else Assert(false, "CORRUPTED FILE c "<<buff[0]);
	}
	bool succeeded ;
	if( last == 'f' ){
		Assert( unaddedInput , "This is not possible ");
		while(resizelist.size()>0){
			pair<string, int> p = resizelist.front();
					resizelist.pop();
			declareInput(p.first, p.second);	
		}
		succeeded = solveCore();
	}else if(last == 'c'){
		if( unaddedInput ){ 
			while(resizelist.size()>0){
				pair<string, int> p = resizelist.front();
					resizelist.pop();
				declareInput(p.first, p.second);	
			}
			addInputsToTestSet(inputStore); 
		}
		bool doMore;
		{ // Check
			cout<<"!+";	ctrlStore.printBrief(cout);	cout<<endl;
			cout<<"BEG CHECK"<<endl; ctimer.restart();
			doMore = check(ctrlStore, inputStore);
			ctimer.stop(); cout<<"END CHECK"<<endl;
			ctimer.print();
		}
		
		if(doMore){
			succeeded = solveCore();
		}else{
			cout<<" *GOT THE CORRECT ANSWER IN 0 iterations."<<endl;		
			succeeded = true;
		}
	}else{		
		succeeded = solve();		
	}
	return succeeded;
}




void CEGISSolver::printDiagnostics(){
	if(printDiag){
		cout<<"# STATS FOR FINDER"<<endl;
		printDiagnostics(this->mngFind, 'f');	
		cout<<"# STATS FOR CHECKER"<<endl;
		printDiagnostics(this->mngCheck, 'c');
	}
}


void CEGISSolver::printDiagnostics(SATSolver& mng, char c){
	if(printDiag){
   		mng.printDiagnostics(c);
	}
}

void CEGISSolver::print_control_map(ostream& out){
	map<string, int> values;
	get_control_map(values);
	for(map<string, int>::iterator it = values.begin(); it != values.end(); ++it){
		out<<it->first<<"\t"<<it->second<<endl;
	}
}


void CEGISSolver::get_control_map(map<string, int>& values){
	vector<bool_node*>& controls = getProblem()->getNodesByType(bool_node::CTRL);
	for(BooleanDAG::iterator node_it = controls.begin(); node_it != controls.end(); ++node_it){		
		values[(*node_it)->name] = ctrlStore[(*node_it)->get_name()];
	}
}



void CEGISSolver::setCheckpoint(const string& filename){
	cpt.setCheckpoint(filename);
}


void CEGISSolver::outputEuclid(ostream& fout){
		cout<<"BEFORE OUTPUTING STATE"<<endl;		
		{
			NodesToEuclid neuc(fout, "PROBLEM_");
			neuc.process(*getProblem());
		}		
	}


void CEGISSolver::setup2QBF(){
	for(BooleanDAG::iterator node_it = getProblem()->begin(); node_it != getProblem()->end(); ++node_it){
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::SRC || (*node_it)->type == bool_node::CTRL ){
			INTER_node* srcnode = dynamic_cast<INTER_node*>(*node_it);	
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits());
		}
	}
	mngCheck.clean();
	dirCheck.reset();
	map<bool_node*,  int> node_values;
	defineProblem(mngCheck, dirCheck, node_values, check_node_ids);
	outputCheckVarmap(cout);
}
