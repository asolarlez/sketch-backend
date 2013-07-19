#include "CEGISSolver.h"
#include "timerclass.h"
#include <queue>
#include "CommandLineArgs.h"
#include "Tvalue.h"
#include "DagOptim.h"
#include "NodesToSolver.h"
#include "NodesToEuclid.h"
#include "NodeSlicer.h"
#include "BackwardsAnalysis.h"
#include "MiniSATSolver.h"

//extern CommandLineArgs* PARAMS;

CEGISSolver::CEGISSolver(BooleanDAG* miter, SolverHelper& finder, SolverHelper& checker, CommandLineArgs& args):
dirFind(finder), 
dirCheck(checker), 
lastFproblem(NULL),
mngFind(finder.getMng()),
mngCheck(checker.getMng()),
params(args)
{
	problemStack.push(miter);
	BooleanDAG* problem = miter;
	{
		Dout( cout<<"BEFORE declaring input names"<<endl );
		vector<bool_node*>& specIn = problem->getNodesByType(bool_node::SRC);	
		for(int i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			declareInput(specIn[i]->get_name(), nbits, srcnode->getArrSz());
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
			if(!ctrlnode->get_Angelic()){
				/* cout<<" i ="<<i<<"\t"<<problemIn[i]->get_name()<<endl; */
				declareControl(problemIn[i]->get_name(), nbits);
			}			
		}
		if(PARAMS->verbosity > 2){
			cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<<endl;
		}
    }
	for(map<string, int>::const_iterator it = dirFind.arrsize_begin(); it != dirFind.arrsize_end(); ++it){
		if(!ctrlStore.contains(it->first)){
			ctrlStore.newVar(it->first, it->second);
		}
	}

	setup();
}


void CEGISSolver::setup(){
	BooleanDAG* problem = getProblem();
	int totSize = problem->size();
	
	cout<<"Random seeds = "<<params.nseeds<<endl;	
	
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



void CEGISSolver::declareInput(const string& inname, int bitsize, int arrSz){
	//Inputs can be redeclared to change their sizes, but not controls.
	Dout(cout<<"DECLARING INPUT "<<inname<<" "<<size<<endl);
	cpt.resizeInput(inname, bitsize);
	if( !inputStore.contains(inname)){
		if(arrSz >= 0){
			inputStore.newArr(inname, bitsize, arrSz);	
		}else{
			inputStore.newVar(inname, bitsize);				
		}
		Dout( cout<<" INPUT "<<inname<<" sz = "<<size<<endl );
	}else{
		inputStore.resizeVar(inname, bitsize);
		if(arrSz >= 0){
			inputStore.resizeArr(inname, arrSz);
		}
	}
}

bool CEGISSolver::solve(){	
	if(PARAMS->verbosity > 1){
		cout<<"inputSize = "<<inputStore.getBitsize()<<"\tctrlSize = "<<ctrlStore.getBitsize()<<endl;
	}
	
	srand(params.randseed);
	inputStore.makeRandom();
	for(int ns = 0; ns < (params.nseeds-1); ++ns){			
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
	prevSolutionFound = false;
	bool hasInputChanged = true;

	vector<string> mhnames;
	vector<int> mhsizes;
	if(PARAMS->minvarHole){
		getMinVarHoleNode(mhnames, mhsizes);		
	}

	while(doMore){
		// Synthesizer
		{// Find
			//setting input "__rs_node" to false in Input Store
			if(PARAMS->angelic_model){
				if(!inputStore.contains("__rs_node")){
					inputStore.newVar("__rs_node",1);
				}
				inputStore.setVarVal("__rs_node",0);
			}
			// cout<<"!%";	for(int i=0; i< input.size(); ++i) cout<<" "<<(input[i]==1?1:0); cout<<endl;
			if(PARAMS->verbosity > 4){ cout<<"!% ";inputStore.printBrief(cout); cout<<endl;}
                        std::vector<int, std::allocator<int> > instore_serialized =
                            inputStore.serialize();
			cpt.checkpoint('f', instore_serialized);
			if(params.simplifycex != CEGISparams::NOSIM){ abstractProblem(); }
			if(PARAMS->verbosity > 1 || PARAMS->showInputs){ cout<<"BEG FIND"<<endl; }
			ftimer.restart(); 		
			try{
				doMore = find(inputStore, ctrlStore, hasInputChanged);
			}catch(BasicError& e){
				doMore = false;
			}
			ftimer.stop();
			if(PARAMS->verbosity > 1 || PARAMS->showInputs){  cout<<"END FIND"<<endl; }
			if(!doMore){
				if(PARAMS->minvarHole && prevSolutionFound){
					cout << "Cannot find a solution with lower value, hence taking the previous solution" << endl;
					inputStore = prevInputStore;
					ctrlStore = prevCtrlStore;
					fail = false;
					break;
				}
				else{
					cout<<"******** FAILED ********"<<endl;	
					ftimer.print();	ctimer.print();
					fail = true;
					break;
				}
			}
		}

		// Verifier
		if(PARAMS->showControls){ print_control_map(cout); }
		{ // Check
			if(PARAMS->verbosity > 4){ cout<<"!+ ";ctrlStore.printBrief(cout); cout<<endl;}
                        std::vector<int, std::allocator<int> > ctrlstore_serialized = ctrlStore.serialize();
			cpt.checkpoint('c', ctrlstore_serialized);
			if(PARAMS->verbosity > 1){ cout<<"BEG CHECK"<<endl; }
			ctimer.restart(); 
			doMore = check(ctrlStore, inputStore);
			 ctimer.stop();
			if(PARAMS->verbosity > 1){ cout<<"END CHECK"<<endl; }			
		}
		if(PARAMS->verbosity > 0){cout<<"********  "<<iterations<<"\tftime= "<<ftimer.get_cur_ms() <<"\tctime= "<<ctimer.get_cur_ms()<<endl; }
		++iterations;
		if( params.iterlimit > 0 && iterations >= params.iterlimit){ cout<<" * bailing out due to iter limit"<<endl; fail = true; break; }

		if(doMore) hasInputChanged = true;
		else hasInputChanged = false;

		// Minimization loop-- found a solution, but can i find a better solution?
		if(PARAMS->minvarHole && !doMore && mhsizes.size() != 0){
			// store the current solution
			storePreviousSolution(inputStore, ctrlStore);
			// optimization: only add inputs if the verification fails
			if(minimizeHoleValue(mhnames, mhsizes))
				doMore=true;
			else{
				doMore = false;
				inputStore = prevInputStore;
				ctrlStore = prevCtrlStore;
			}
		}
	}
	ttimer.stop();
	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;		
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	cout<<" *"<<"FIND TIME "<<ftimer.get_tot_ms()<<" CHECK TIME "<<ctimer.get_tot_ms()<<" TOTAL TIME "<<ttimer.get_tot_ms()<<endl;
	dirFind.getMng().retractAssumptions();
	return !fail;
}


void CEGISSolver::getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes){
	BooleanDAG* dag = getProblem();
	vector<bool_node*> nodes = dag->getNodesByType(bool_node::CTRL);
	int nMinVarNodes = 0;
	CTRL_node* result;
	for(std::vector<bool_node*>::iterator node_it = nodes.begin(); node_it != nodes.end(); ++node_it) {
		CTRL_node* cnode = dynamic_cast<CTRL_node*>((*node_it));
		bool isMinimizeNode = (*cnode).get_toMinimize();
		if(isMinimizeNode){
			mhnames.push_back(cnode->get_name());
			mhsizes.push_back(cnode->get_nbits());
		}
	}	
	cout << "Number of minvar nodes = " << mhsizes.size() << endl;
}

void CEGISSolver::storePreviousSolution(VarStore prevInputStore1, VarStore prevCtrlStore1){
	prevInputStore = prevInputStore1;
	prevCtrlStore = prevCtrlStore1;
	prevSolutionFound = true;
}

bool CEGISSolver::minimizeHoleValue(vector<string>& mhnames, vector<int>& mhsizes){	
	cout << "*********INSIDE minimizeHoleValue, current value of ";
	bool isSingleMinHole = (mhsizes.size()==1);
	vector<int> bigor; bigor.push_back(0);
	for(int i=0; i<mhsizes.size(); ++i){
		string& minVarNodeName = mhnames[i];
		int minVarNodeSize = mhsizes[i];
		int H__0_val = ctrlStore.getObj(minVarNodeName).getInt(); 
		int H__0_var_idx = dirFind.getVar(minVarNodeName);
		cout <<minVarNodeName<<"=" << H__0_val << ", ";
		Tvalue tv = H__0_var_idx;
		tv.setSize(minVarNodeSize);
		tv.makeSparse(dirFind);
		try{
			for(int i=0; i<tv.num_ranges.size(); ++i){
				if(tv.num_ranges[i].value > H__0_val){	
					dirFind.addRetractableAssertClause(-tv.num_ranges[i].guard);
				}
				if(isSingleMinHole && tv.num_ranges[i].value == H__0_val){	
					dirFind.addRetractableAssertClause(-tv.num_ranges[i].guard);
				}
				if(tv.num_ranges[i].value < H__0_val){	
					bigor.push_back(tv.num_ranges[i].guard);
				}
			}	
		}
		catch(BasicError& be){
			return false;
		}
	}
	cout<<endl;
	if(!isSingleMinHole){
		dirFind.addBigOrClause(&bigor[0], bigor.size()-1);
		try{
			dirFind.addRetractableAssertClause(bigor[0]);
		}
		catch(BasicError& be){
			return false;
		}
	}
	
	
	return true;
}

void CEGISSolver::addInputsToTestSet(VarStore& input){
	map<bool_node*,  int> node_values;
	bool specialize = PARAMS->olevel >= 6;
	BooleanDAG* tmpproblem = NULL;	
	cout<<"Level "<< this->problemLevel()<<"  intsize = "<<getProblem()->getIntSize()<<endl;
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
		BooleanDAG* newdag = hardCodeINode(getProblem(), input, bool_node::SRC);
		// newdag->lprint(cout);
		BackwardsAnalysis ba;
		ba.process(*newdag);
		DagOptim fa(*newdag);
		fa.process(*newdag);
		pushProblem(newdag);
		//newdag->lprint(cout);
		if(PARAMS->verbosity > 2){ cout<<" * After all optims it became = "<<newdag->size()<<endl; }	
		// find_node_ids store the mapping between node in the DAG (miter) vs
		// the variables in the CNF.
		find_node_ids.resize(getProblem()->size());
		//getProblem()->lprint(cout);
	}
	//FindCheckSolver::addInputsToTestSet(input);
	lastFproblem = getProblem();	
	
	try{
	defineProblem(mngFind, dirFind, node_values, find_node_ids);
	}catch(BasicError& e){
		dirFind.nextIteration();
		if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
		if(specialize){
			popProblem();
			find_node_ids.clear();
		}
		throw e;
	}
	// Keeps the history around for debugging purposes.	
	if( params.superChecks ){ find_history = find_node_ids; }
	dirFind.nextIteration();
	if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
	if(specialize){
		popProblem();
		find_node_ids.clear();
	}
}


bool_node* CEGISSolver::nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse){

	int arrsz = -1;
	if(inode->type== bool_node::SRC){	
		arrsz = dynamic_cast<SRC_node*>(inode)->arrSz;
	}
	if(arrsz>=0){
		VarStore::objP* val = &(values.getObj(inode->get_name()));
		int nbits = inode->get_nbits();
		ARR_CREATE_node* acn = new ARR_CREATE_node();
		while(val != NULL){
			bool_node* cnst;
			if(nbits==1){
				cnst= cse.getCnode( val->getInt() ==1 );
			}else{
				cnst= cse.getCnode( val->getInt() );
			}
			while(acn->multi_mother.size()< val->index){
				acn->multi_mother.push_back( cse.getCnode(0) );
			}
			acn->multi_mother.push_back( cnst );
			val = val->next;
		}
		acn->addToParents();
		cse.addNode(acn);
		if(PARAMS->showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< acn->lprint() <<endl; }
		return acn;
	}else{
		int nbits = inode->get_nbits();		
		bool_node* onode;
		if(nbits==1){
			onode= cse.getCnode( values[inode->get_name()]==1 );
		}else{
			onode= cse.getCnode( values[inode->get_name()] );
		}
		if(PARAMS->showInputs && inode->type == bool_node::SRC){ cout<<" input "<<inode->get_name()<<" has value "<< onode->lprint() <<endl; }
		return onode;
	}
}


BooleanDAG* CEGISSolver::hardCodeINode(BooleanDAG* dag, VarStore& values, bool_node::Type type){
	BooleanDAG* newdag = dag->clone();

	vector<bool_node*> inodeList = newdag->getNodesByType(type);
		
	if(PARAMS->verbosity > 2){ cout<<" * Specializing problem for "<<(type == bool_node::CTRL? "controls" : "inputs")<<endl; }
	if(PARAMS->verbosity > 2){cout<<" * Before specialization: nodes = "<<newdag->size()<<" Ctrls = "<<  inodeList.size() <<endl;	}
	
	{
		DagOptim cse(*newdag);			
		cse.isTopLevel = true;
		for(int i=0; i<inodeList.size(); ++i){
			INTER_node* inode = dynamic_cast<INTER_node*>(inodeList[i]);	
			int nbits;
			if(type == bool_node::CTRL){
				CTRL_node* cn = dynamic_cast<CTRL_node*>(inode);
				if(cn->get_Angelic()){
					if(cn->children.size() != 0){
						Assert(cn->children.size() == 1, "NYI");
						bool_node* bn = *(cn->children.begin());
						Assert(bn->type == bool_node::ARRACC || bn->type == bool_node::ARRASS, "NYI");
						arith_node* an = dynamic_cast<arith_node*>(bn);
						if(an->multi_mother[0]==cn){
							Assert(an->multi_mother[0]==cn, "NYI");
							newdag->replace(cn->id, an->multi_mother[1]);
						}else{
							Assert(an->multi_mother[1]==cn, "NYI");
							newdag->replace(cn->id, an->multi_mother[0]);
						}
						
						continue;
					}else{
						newdag->replace(inode->id, cse.getCnode(0));
						continue;
					}
				}
			}
			
			bool_node * repl= nodeForINode(inode, values, cse);			
			Assert( (*newdag)[inode->id] == inode , "The numbering is wrong!!");
			newdag->replace(inode->id, repl);			
		}
				
		newdag->removeNullNodes();
		cse.process(*newdag);
	}
	Dout( newdag->print(cout) ); 
	
	if(false){
		BackwardsAnalysis ba;
		ba.process(*newdag);
	}
	if(false){
		DagOptim cse(*newdag);			
		cse.process(*newdag);
	}
	if(PARAMS->verbosity > 2){ cout<<" * After optims it became = "<<newdag->size()<<endl; }	
	return newdag;
}




// This function is responsible for encoding the problem
void CEGISSolver::defineProblem(SATSolver& mng, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids){
	{
		//timerclass timer("defineProblem");
		//timer.start();
		int YES = dir.newYES();
		//getProblem()->lprint(cout);
		NodesToSolver nts(dir, "PROBLEM", node_values, node_ids);			
		try{
			nts.process(*getProblem());	
			BooleanDAG& bd = *getProblem();
			/*
			for(int i=0; i<node_ids.size(); ++i){
				cout<< bd[i]->lprint() <<"="<<node_ids[i]<<endl;
			}
			*/
		}catch(BasicError& e){
			if(PARAMS->debug){
				for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it){
					const string& cname = it->name;
					int cnt = dir.getArrSize(cname);
					Assert( cnt == it->size(), "SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
					for(int i=0; i<cnt; ++i){
						int val = nts.lastGoodVal(dir.getArr(cname, i));
						it->setBit(i, (val==1) ? 1 : 0);			
					}
				}
			}
			throw e;
		}

		//timer.stop();
		//if(PARAMS->verbosity > 2){ timer.print(); }
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




bool CEGISSolver::find(VarStore& input, VarStore& controls, bool hasInputChanged){
	
	if(hasInputChanged){
	timerclass tc("* TIME TO ADD INPUT ");
	tc.start();				
	addInputsToTestSet(input);
	tc.stop();
	if(PARAMS->verbosity > 2){ tc.print(); }
	}
//Solve
	int result = mngFind.solve();
	if(params.printDiag){
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
	if(false){ //This is useful code when debugging;		
		for(int i=0; i<find_history.size(); ++i){
			cout<<i<<"=";
			find_history[i].print(cout, &mngFind);
			cout<<endl;
		}
		cout<<"???"<<endl;
	}

	mngFind.reset();
	return true;
//Return true.
}

// Method for cutting the problem for the synthesizer. It first finds the
// failing assertion and then based on its relative position on the DAG decides
// to remove or keep the assertions following the failing assertion.
void CEGISSolver::abstractProblem(){
	if(inputStore.getBitsize() == 0) return;
	VarStore tmp = join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;	
	BooleanDAG* dag = getProblem()->clone();
	NodeEvaluator eval(empty, *dag);
	int orisize = dag->size();
	eval.run(tmp);
	vector<bool_node*> asserts = dag->getNodesByType(bool_node::ASSERT);
	bool found = false;
	bool keepRemoving = false;
	int failedpos = -1;
	int lastAssert = 0;
	int cumDist = 0;
	int avgDist = 0;
	int cutoff = ((asserts.size()*6)/10);
	for(BooleanDAG::iterator node_it = asserts.begin(); node_it != asserts.end(); ++node_it){
		ASSERT_node* an = dynamic_cast<ASSERT_node*>(*node_it);
		if(an->isHard()){
			continue;
		}
		if(found){
			if(keepRemoving){
				dag->remove((*node_it)->id);
			}else{
				if((*node_it)->id - lastAssert <= avgDist){					
					failedpos++;					
				}else{					
					dag->remove((*node_it)->id);
					keepRemoving = true;
				}
			}
		}else{
			if(eval.getValue(**node_it)==0){				
				found = true;
				avgDist = failedpos> 0 ? cumDist / failedpos : (*node_it)->id;
				//cout<<"FOUND at "<<failedpos<<" avgDist = "<<avgDist<<endl;
				if(PARAMS->verbosity > 2){
					cout<<" candidate failed assertion "<<an->getMsg()<<endl;
				}
				if(failedpos > cutoff){
					if(PARAMS->verbosity > 2){ cout<<" failedpos = "<<failedpos<<"   cutoff = "<<cutoff <<"  as = "<<asserts.size() <<" node "<<(*node_it)->id<<" out of "<<orisize<<endl; }
					dag->clear();
					delete dag;
					return;
				}
			}else{
				failedpos++;
				cumDist += (*node_it)->id - lastAssert;				
			}			
		}
		//This is buggy because node_it may have beeen deleted by dag->remove()
		lastAssert = (*node_it)->id;
	}
	dag->removeNullNodes();
	dag->cleanup();
	
	if(PARAMS->verbosity > 2){ cout<<" failedpos = "<<failedpos<<"   cutoff = "<<cutoff <<"  as = "<<asserts.size() <<endl; }
	if(failedpos<cutoff){		
		pushProblem(dag);
		if(PARAMS->verbosity > 2){
			cout<<"Level "<<problemLevel()<<"Replacing dag of size "<<orisize<<" with size "<<dag->size()<<endl;
		}
	}else{
		dag->clear();
		delete dag;
	}
}


bool CEGISSolver::simulate(VarStore& controls, VarStore& input){
	timerclass tc("Simulation");
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
			
			bool done;
			if(hasCtrls){
				VarStore ta(join(tmpin, controls));
				done= eval.run(ta);
			}else{
				done= eval.run(tmpin);
			}
			eval.trackChanges();
			if(done){
				tc.stop().print("found a cex");
				popProblem();
				return true;
			}
		}
		if(expensive.size()>0){
			for(int i=0; i<expensive.size(); ++i){
				if(hasCtrls){
					VarStore ta(join(expensive[i], controls));
					eval.run(ta);
				}else{
					eval.run(expensive[i]);
				}
			}
		}
		int hold = -1;
		while(true){
			if(PARAMS->verbosity > 8){ cout<<" TESTING HYPOTHESIS"<<endl; }
			int h = eval.scoreNodes();
			if(h==-1){
				break;
			}
			if(hold == h){
				Assert(false, "CEGISSolver::simulate: This should not happen");
				cout<<"INFINITE LOOP!"<<endl;
				break;
			}
			hold = h;
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
			if(PARAMS->verbosity > 10 && tbd->size() < 50){
				tbd->lprint(cout);
			}
			pushProblem(tbd);
			
			
			bool rv = baseCheck(controls, tmpin);
			
			popProblem();
			if(am>0){
				if(am==2){ delete an->mother->father; }
				delete an->mother; 
			}
			delete an;
			dag->relabel();
			if(rv){
				bool done;
				if(hasCtrls){
					VarStore ta(join(tmpin, controls));
					done= eval.run(ta);
				}else{
					done= eval.run(tmpin);
				}				
				if(done){
					tc.stop().print("found a cex");
					popProblem();
					return true;
				}else{
					expensive.push_back(tmpin);
				}
			}else{
				{
					bool_node* btoR = (*dag)[h];
					int nval = eval.getValue(btoR);
					if(PARAMS->verbosity > 8){ cout<<" FOUND CONST: "<<niq->lprint()<<" = "<<nval<<endl; }
					DagOptim cse(*dag);
					int sz = dag->size();					
					if(btoR->type == bool_node::EQ && nval == 1){
						if(btoR->mother->type == bool_node::CONST){
							dag->replace(btoR->father->id, btoR->mother);
						}else if(btoR->father->type == bool_node::CONST){
							dag->replace(btoR->mother->id, btoR->father);
						}else if( btoR->mother->id >  btoR->father->id){
							dag->replace(btoR->mother->id, btoR->father);
						}else{
							dag->replace(btoR->father->id, btoR->mother);
						}
					}
					dag->replace(h, cse.getCnode(nval));
					dag->removeNullNodes();
					cse.process(*dag);
					if(PARAMS->verbosity >= 5){ cout<<" reduced size from "<<sz<<" to "<<dag->size()<<endl; }
					if(dag->getNodesByType(bool_node::ASSERT).size()==0){
						tc.stop().print("no cex");
						popProblem();
						return false;
					}
				}
				break;
			}
		}		
	}while(iter < params.simiters && dag->size() > params.simstopsize);
	
	{
		BackwardsAnalysis ba;
		ba.process(*dag);
	}
	{
		DagOptim cse(*dag);			
		cse.process(*dag);
	}
	if(PARAMS->verbosity > 2){ cout<<" * Simulation optimized it to = "<<dag->size()<<endl; }	
	tc.stop().print("didn't find a cex");	
	cout<<"After all optim"<<endl;
	//getProblem()->lprint(std::cout);
	// dag->lprint(cout);
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


void CEGISSolver::redeclareInputs(BooleanDAG* dag){
	vector<bool_node*>& specIn = dag->getNodesByType(bool_node::SRC);	
	for(int i=0; i<specIn.size(); ++i){			
		SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
		int nbits = srcnode->get_nbits();
		if(nbits >= 2){	
			declareInput(specIn[i]->get_name(), nbits, srcnode->getArrSz());
		}
	}
}


void CEGISSolver::growInputs(BooleanDAG* dag, BooleanDAG* oridag){
	int gnbits = -1;
	if(PARAMS->verbosity > 2){
		cout<<"* growing the inputs to size "<< (dag->getIntSize()+1) <<endl;
	}
	dag->growInputIntSizes();
	if(oridag != dag){
		oridag->growInputIntSizes();
	}
	redeclareInputs(dag);
}

class CheckControl{
	
	
	typedef enum {CSOLVE, ADJUST} cstate;
	typedef enum {GROW, POP} whatnext;
	cstate state;
	whatnext wn;
	CEGISparams& params;
public:
	typedef enum {POP_LEVEL, GROW_IN, SOLVE, DONE} decision;
	CheckControl(CEGISparams& p_params):params(p_params),state(CSOLVE), wn(GROW){}
	decision actionDecide(int level, BooleanDAG* dag){
		if(params.simplifycex==CEGISparams::SIMSIM  && level != 1){
			return POP_LEVEL;
		}
		if(state == CSOLVE){
			state = ADJUST;
			return SOLVE;
		}
		if(state == ADJUST){
			state = CSOLVE;
			if((wn==GROW && dag->getIntSize() < params.NINPUTS) || (level == 1)){
				wn = POP;
				if(dag->getIntSize() < params.NINPUTS){
					return GROW_IN;
				}else{
					return DONE;
				}
				
			}else{
				wn = GROW;
				if(level > 1){
					return POP_LEVEL;
				}else{
					return DONE;
				}
			}
		}
	}
};

bool CEGISSolver::check(VarStore& controls, VarStore& input){
	BooleanDAG* oriProblem = getProblem();
	bool hardcode = PARAMS->olevel >= 6;
	bool rv = false;
	int ninputs = -1;
	CheckControl cc(params);
	//cout<<"Before hard code"<<endl;
	//getProblem()->lprint(std::cout);

	

	if(hardcode){
		pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL));		
	}
	//cout<<"After hard code"<<endl;
	//getProblem()->lprint(std::cout);
	do{
		switch(cc.actionDecide(problemLevel() - (hardcode? 1: 0),getProblem())){
			case CheckControl::POP_LEVEL:{
				BooleanDAG* tbd = getProblem();
				popProblem();
				cout<<"CONTROL: Popping to level "<<problemLevel()<<endl;
				if(hardcode){
					tbd = getProblem();
					popProblem();
					oriProblem = getProblem();
					//tbd will be invalid by the time we get here because popProblem deletes the problem.
					if(tbd->getIntSize() != oriProblem->getIntSize()){
						redeclareInputs(oriProblem);						
					}
					pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL));					
				}else{
					oriProblem = getProblem();
					//tbd will be invalid by the time we get here because popProblem deletes the problem.
					if(tbd->getIntSize() != oriProblem->getIntSize()){
						redeclareInputs(oriProblem);						
					}
				}
				continue;
			}
			case CheckControl::DONE:{
				if(hardcode){
					popProblem();
				}
				return false;
			}
			case CheckControl::GROW_IN:{
				cout<<"CONTROL: Growing Input"<<problemLevel()<<endl;
				growInputs(getProblem(), oriProblem);
				continue;
			}
			case CheckControl::SOLVE:{
				if(params.simulate){
					rv = simulate(controls, input);	
				}else{
					rv = baseCheck(controls, input);
				}
				continue;
			}
		}
	}while(!rv);
	if(hardcode){
		popProblem();
	}
	return true;
}

/*
bool CEGISSolver::check(VarStore& controls, VarStore& input){	
	bool rv;
	BooleanDAG* oriProblem = getProblem();
	bool dot = true;
	bool pushedNewP = false;
	if(input.getBitsize()==0){
		cout<<"no cex"<<endl;
		return false;
	}
	if(params.simplifycex==CEGISparams::SIMSIM){
		while(problemLevel() != 1){
			if(PARAMS->verbosity > 2){ 
				cout<<" * Cleaning up alternative problem level "<<problemLevel()<<endl;
			}
			popProblem();
		}
	}
	int plevel = problemLevel();
	do{		
		if(PARAMS->olevel >= 6 && dot){
			oriProblem = getProblem();
			pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL));
			//getProblem()->lprint(cout);
			pushedNewP = true;
			dot = false;
		}
		if(params.simulate){
			rv = simulate(controls, input);	
		}else{
			rv = baseCheck(controls, input);
		}
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
					if(PARAMS->verbosity > 2){ cout<<"* Done growing inputs. All integer inputs have reached size "<<params.NINPUTS<<endl; }
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

*/

bool CEGISSolver::baseCheck(VarStore& controls, VarStore& input){
	Dout( cout<<"check()"<<endl );
	//timerclass tc("* TIME TO ADD CONTROLS ");
	//tc.start();				
	setNewControls(controls);
	//if(PARAMS->verbosity > 2){ tc.stop().print(); }
	
    int result = mngCheck.solve();
	//dirCheck.printAllVars();
    if(params.printDiag){
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
			Assert( cnt == it->globalSize(), "SIZE MISMATCH: "<<cnt<<" != "<<it->globalSize()<<endl);
			for(int i=0; i<cnt; ++i){
				
				int val = mngCheck.getVarVal(dirCheck.getArr(cname, i));
				it->setBit(i, (val==1)? 1 : 0);						
			}
			Dout( cout<<" input "<<cname<<"  has value "<<it->getInt()<<endl );
		}
	}
	Dout( dirCheck.print() );
	if(false){ //This is useful code when debugging;
		cout<<"AFTER CSTAGE"<<endl;
		getProblem()->lprint(cout);
		map<string, BooleanDAG*> empty;
		NodeEvaluator eval(empty, *getProblem());
		eval.run(input);
		cout<<"PRINT EVAL"<<endl;
		for(int i=0; i<check_node_ids.size(); ++i){
			cout<<i<<"=";
			check_node_ids[i].print(cout, &mngCheck);
			cout<<"    "<<(*getProblem())[i]->lprint();
			cout<<endl;
		}
		cout<<"???"<<endl;
	}

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
			int arsz = srcnode->getArrSz();
			if(arsz <0){ arsz = 1; }
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits()*arsz);
		}
	}	
	defineProblem(mngCheck, dirCheck, node_values, check_node_ids);
}



bool CEGISSolver::solveFromCheckpoint(istream& in){
	timerclass ctimer("* CHECK TIME");
	
	int inputSize = inputStore.getBitsize();
	int ctrlSize = ctrlStore.getBitsize();
	
	cout<<"inputSize = "<<inputSize<<"\tctrlSize = "<<ctrlSize<<endl;
	
	srand(params.randseed);
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
					declareInput(p.first, p.second, -1);
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
			declareInput(p.first, p.second, -1);	
		}
		succeeded = solveCore();
	}else if(last == 'c'){
		if( unaddedInput ){ 
			while(resizelist.size()>0){
				pair<string, int> p = resizelist.front();
					resizelist.pop();
				declareInput(p.first, p.second, -1);	
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
		cout<<"# STATS FOR FINDER"<<endl;
		printDiagnostics(this->mngFind, 'f');	
		cout<<"# STATS FOR CHECKER"<<endl;
		printDiagnostics(this->mngCheck, 'c');	
}


void CEGISSolver::printDiagnostics(SATSolver& mng, char c){
   		mng.printDiagnostics(c);
}

void CEGISSolver::print_control_map(ostream& out){
	map<string, int> values;
	get_control_map(values);
	for(map<string, int>::iterator it = values.begin(); it != values.end(); ++it){
		out<<it->first<<"\t"<<it->second<<endl;
	}
}


void CEGISSolver::get_control_map(map<string, int>& values){
	for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it){
		values[it->name] = it->getInt();
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


void CEGISSolver::setup2QBF(ostream& out){
	mngCheck.clean();
	dirCheck.reset();
	for(BooleanDAG::iterator node_it = getProblem()->begin(); node_it != getProblem()->end(); ++node_it){
		(*node_it)->flag = true;
		if(	(*node_it)->type == bool_node::SRC || (*node_it)->type == bool_node::CTRL ){
			INTER_node* srcnode = dynamic_cast<INTER_node*>(*node_it);	
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits());
			int base = dirCheck.getVar(srcnode->get_name());
			int n = dirCheck.getArrSize(srcnode->get_name());
			for(int i=0; i<n; ++i){
				if(	(*node_it)->type == bool_node::SRC ){
					out<<"EV "<<(i+base)<<endl;
				}else{
					out<<"UV "<<(i+base)<<endl;
				}
			}
		}
	}
	check_node_ids.resize(getProblem()->size());
	map<bool_node*,  int> node_values;
	MiniSATSolver& ms = dynamic_cast<MiniSATSolver&>(mngCheck);
	ms.debugout = &out;
	defineProblem(mngCheck, dirCheck, node_values, check_node_ids);
	ms.finish();
	ms.debugout = NULL;
	outputCheckVarmap(cout);
}
