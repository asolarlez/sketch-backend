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
#include "CounterexampleFinder.h"
#include "NodeHardcoder.h"
#include "CEGISFinder.cpp"

//extern CommandLineArgs* PARAMS;




void CEGISSolver::addProblem(BooleanDAG* problem, const string& file){
	curProblem = problems.size();
	problems.push_back(problem);
	if (file != "") {
		files[curProblem] = file;
	}



	{
		Dout( cout<<"BEFORE declaring input names"<<endl );
		redeclareInputs(problem, true);		
	}
	 Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
    {
	    vector<bool_node*>& problemIn = problem->getNodesByType(bool_node::CTRL);
	    if(PARAMS->verbosity > 2){
			cout<<"  # OF CONTROLS:    "<< problemIn.size() <<endl;
		}
		int cints = 0;
		int cbits = 0;
    	int cfloats = 0;
	    for(int i=0; i<problemIn.size(); ++i){
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);	
			int nbits = ctrlnode->get_nbits();
			if(ctrlnode->getOtype() == OutType::BOOL){
				cbits++;
			} else if (ctrlnode->getOtype() == OutType::FLOAT) {
				cfloats++;
			} else{
				cints++;
			}		
			if(!ctrlnode->get_Angelic() /*&& ctrlnode->getOtype() != OutType::FLOAT*/){
				/* cout<<" i ="<<i<<"\t"<<problemIn[i]->get_name()<<endl; */

				declareControl(ctrlnode);
			}
			if (ctrlnode->spAngelic) {
				declareInput(problemIn[i]->get_name() + "_src", nbits, ctrlnode->getArrSz(), ctrlnode->getOtype());
			}
		}
		if(PARAMS->verbosity > 2){
			cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
		}
    }

	finder->updateCtrlVarStore(ctrlStore);	
}


CEGISSolver::CEGISSolver(CEGISFinderSpec* _finder, HoleHardcoder& hc, CommandLineArgs& args, FloatManager& _floats):
finder(_finder), 
params(args),
hcoder(hc),
floats(_floats)
{
//	cout << "miter:" << endl;
//	miter->lprint(cout);
		
}



CEGISSolver::~CEGISSolver(void)
{
	for(int i=0; i<problems.size(); ++i){
		problems[i]->clear();
		delete problems[i];
	}
}


void CEGISSolver::declareControl(CTRL_node* cnode){
	const string& cname = cnode->get_name();
	int size;

	if(cnode->getOtype() != OutType::FLOAT)
	{
		size = cnode->get_nbits();
	}
	else
	{
		size = 18;
	}
	Dout(cout<<"DECLARING CONTROL "<<cname<<" "<<size<<endl);
	finder->declareControl(cnode);

	if(!ctrlStore.contains(cname)){
		ctrlStore.newVar(cname, size, cnode->getOtype());
	}	
}

void declareInput(VarStore & inputStore, const string& inname, int bitsize, int arrSz, OutType* otype) {
	//Inputs can be redeclared to change their sizes, but not controls.
	if( !inputStore.contains(inname)){
    if(arrSz >= 0){
			inputStore.newArr(inname, bitsize, arrSz, otype);	
		}else{
			inputStore.newVar(inname, bitsize, otype);				
		}
		Dout( cout<<" INPUT "<<inname<<" sz = "<<bitsize<<endl );
	}else{
		// cout<<" RESIZING "<<inname<<" to "<<bitsize<<endl;
      inputStore.resizeVar(inname, bitsize);
      if(arrSz >= 0){
        inputStore.resizeArr(inname, arrSz);
      }
	}
}

void CEGISSolver::declareInput(const string& inname, int bitsize, int arrSz, OutType* otype) {
	Dout(cout<<"DECLARING INPUT "<<inname<<" "<<bitsize<<endl);
	cpt.resizeInput(inname, bitsize);
	::declareInput(inputStore, inname, bitsize, arrSz, otype);
}

bool CEGISSolver::solve(){	
	if(problems.size()==0){
		return true;
	}
	Assert(problemStack.size() == 0, "A big bug");
	pushProblem((*problems.rbegin())->clone());

	if(PARAMS->verbosity > 1){
		cout<<"inputSize = "<<inputStore.getBitsize()<<"\tctrlSize = "<<ctrlStore.getBitsize()<<endl;
		cout<<"Random seeds = "<<params.nseeds<<endl;	
	}
	
	
	ctrlStore.makeRandom();
	cout<<"!+";	ctrlStore.printBrief(cout); cout<<endl;
//        std::vector<int, std::allocator<int> > ctrlstore_serialized =
//                    ctrlStore.serialize();
//	cpt.checkpoint('c', ctrlstore_serialized);
//	setNewControls(ctrlStore);	
	
	bool succeeded = solveCore();
	//bool succeeded = solveOptimization();
	popProblem();

	return succeeded;
}

/*
bool CEGISSolver::solveOptimization() {
	timerclass ftimer("* FIND TIME");
	timerclass ttimer("* TOTAL TIME");
	ttimer.start();
	
	if(PARAMS->verbosity > 2 || PARAMS->showInputs){ cout<<"BEG FIND"<<endl; }
	ftimer.restart();
	bool fail = false;
	try{
		fail = !find(inputStore, ctrlStore, true);
	}catch(BasicError& e){
		fail = true;
	}
	
	ttimer.stop();
	if(!fail){
		cout<<" *Reporting the minimum found so far"<<endl;
	}else{
		cout<<" *FAILED"<<endl;
	}
	cout<<" *"<<"FIND TIME "<<ftimer.get_tot_ms()<<" TOTAL TIME "<<ttimer.get_tot_ms()<<endl;
	dirFind.getMng().retractAssumptions();
	return !fail;
}
*/


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
		// Verifier
		if(PARAMS->showControls){ print_control_map(cout); }
		{ // Check
			if(PARAMS->verbosity > 5){ cout<<"!+ ";ctrlStore.printBrief(cout); cout<<endl;}
			if(PARAMS->verbosity > 9){ cout<<"!+ ";ctrlStore.printContent(cout); cout<<endl;}
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
		
		if (!doMore) {//To make sure the control store also fleshes out the expressions from custom Synthesizers before being copied
			ctrlStore.finalizeSynthOutputs();
		}
		
		// Minimization loop-- found a solution, but can i find a better solution?
		if(PARAMS->minvarHole && !doMore && mhsizes.size() != 0){
			// store the current solution
			storePreviousSolution(inputStore, ctrlStore);
			// optimization: only add inputs if the verification fails
			if(finder->minimizeHoleValue(ctrlStore, mhnames, mhsizes))
				doMore=true;
			else{
				doMore = false;
				inputStore = prevInputStore;
				ctrlStore = prevCtrlStore;
			}
		}

		// Synthesizer
		if (doMore) {// Find
			// cout<<"!%";	for(int i=0; i< input.size(); ++i) cout<<" "<<(input[i]==1?1:0); cout<<endl;
			if(PARAMS->angelic_model){
				if(!inputStore.contains("__rs_node")){
					inputStore.newVar("__rs_node",1, NULL);
				}
				inputStore.setVarVal("__rs_node",0, NULL);
			}
			if (hasInputChanged) {
				if(PARAMS->verbosity > 5){ cout<<"!% ";inputStore.printBrief(cout); cout<<endl;}
				if(PARAMS->verbosity > 9){ cout<<"!% ";inputStore.printContent(cout); cout<<endl;}
				std::vector<int, std::allocator<int> > instore_serialized = inputStore.serialize();
			       	cpt.checkpoint('f', instore_serialized);
			       	//if(params.simplifycex != CEGISparams::NOSIM){ abstractProblem(); }
			}
			if(PARAMS->verbosity > 2 || PARAMS->showInputs){ cout<<"BEG FIND"<<endl; }
			ftimer.restart(); 		
			try{
				doMore = find(inputStore, ctrlStore, hasInputChanged);
			}catch(BasicError& e){
				doMore = false;
			}
			ftimer.stop();
			if(PARAMS->verbosity > 2 || PARAMS->showInputs){  cout<<"END FIND"<<endl; }
		}

		if(hasInputChanged && !doMore){
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


	ttimer.stop();
	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;		
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	cout<<" *"<<"FIND TIME "<<ftimer.get_tot_ms()<<" CHECK TIME "<<ctimer.get_tot_ms()<<" TOTAL TIME "<<ttimer.get_tot_ms()<<endl;
	finder->retractAssumptions();
	return !fail;
}




void CEGISSolver::getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes){
	map<string, CTRL_node*> minimizes;
	for(size_t i=0; i<problems.size(); ++i){
		BooleanDAG* dag = problems[i];
		vector<bool_node*> nodes = dag->getNodesByType(bool_node::CTRL);
		for(std::vector<bool_node*>::iterator node_it = nodes.begin(); node_it != nodes.end(); ++node_it) {
			CTRL_node* cnode = dynamic_cast<CTRL_node*>((*node_it));
			bool isMinimizeNode = (*cnode).get_toMinimize();
			if(isMinimizeNode){
				if(minimizes.count(cnode->get_name())== 0){
					minimizes[cnode->get_name()] = cnode;
				}				
			}
		}
	}

	for(map<string, CTRL_node*>::iterator it = minimizes.begin(); it != minimizes.end(); ++it){
		CTRL_node* cnode = (*it).second;
		mhnames.push_back(cnode->get_name());
		mhsizes.push_back(cnode->get_nbits());
	}
	cout << "Number of minvar nodes = " << mhsizes.size() << endl;
}

void CEGISSolver::storePreviousSolution(VarStore prevInputStore1, VarStore prevCtrlStore1){
	prevInputStore = prevInputStore1;
	prevCtrlStore = prevCtrlStore1;
	prevSolutionFound = true;
}




bool_node* CEGISSolver::nodeForINode(INTER_node* inode, VarStore& values, DagOptim& cse){
	int arrsz = -1;
  if (inode->type == bool_node::SRC) {
    SRC_node* src_ = dynamic_cast<SRC_node*>(inode);
    if (src_->isTuple) {
      Assert(false, "Not possible");
    }
  }
	if(inode->type== bool_node::SRC){	
		arrsz = dynamic_cast<SRC_node*>(inode)->arrSz;
	}
	if(arrsz>=0){
		VarStore::objP* val = &(values.getObj(inode->get_name()));
		int nbits = inode->get_nbits();
		vector<bool_node*> multi_mother;
		
		while(val != NULL){
			bool_node* cnst;
			if(nbits==1){
				cnst= cse.getCnode( val->getInt() ==1 );
			}else{
				cnst= cse.getCnode( val->getInt() );
			}
			while(multi_mother.size()< val->index){
				multi_mother.push_back( cse.getCnode(0) );
			}
			multi_mother.push_back( cnst );
			val = val->next;
		}
		ARR_CREATE_node* acn = ARR_CREATE_node::create(multi_mother, cse.getCnode(0));		
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


bool CEGISSolver::find(VarStore& input, VarStore& controls, bool hasInputChanged) {
	return finder->find(getProblem(), input, controls, hasInputChanged);
}




// Method for cutting the problem for the synthesizer. It first finds the
// failing assertion and then based on its relative position on the DAG decides
// to remove or keep the assertions following the failing assertion.
void CEGISSolver::abstractProblem(){
	if(inputStore.getBitsize() == 0) return;
	BooleanDAG* dag = getProblem()->clone();
	int orisize = dag->size();
	
	if(orisize < 200){ return; }
	
	VarStore tmp = join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;	
	
	NodeEvaluator eval(empty, *dag, floats);
	
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
		int save_id = (*node_it)->id;
		// save the id in case node is deleted below
		ASSERT_node* an = dynamic_cast<ASSERT_node*>(*node_it);
		if(!an->isNormal()){
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
					if(PARAMS->verbosity > 5){ cout<<" failedpos = "<<failedpos<<"   cutoff = "<<cutoff <<"  as = "<<asserts.size() <<" node "<<(*node_it)->id<<" out of "<<orisize<<endl; }
					dag->clear();
					delete dag;
					return;
				}
			}else{
				failedpos++;
				cumDist += (*node_it)->id - lastAssert;				
			}			
		}
		lastAssert = save_id; //(*node_it)->id;
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

void filterHasserts(vector<bool_node*> const & asserts, int id, vector<bool_node*> & hasserts) {
	vector<bool_node*>::const_iterator i=asserts.begin();
	vector<bool_node*>::const_iterator e=asserts.end();
	for (; i!=e; ++i) {
		ASSERT_node * a = dynamic_cast<ASSERT_node*>(*i);
		if(a->id > id){
			break;
		}
		if (!a->isNormal()) {
			hasserts.push_back(a);
		}
	}
}

 void BitSet::print(ostream& os){
		int i=next(-1);
		os<<"{";
		while(i != -1){
			os<<", "<<i;
			i = next(i);
		}
		os<<"}"<<endl;

	}


 bool hasHAsserts(const vector<bool_node*>& asserts){
	vector<bool_node*>::const_iterator i=asserts.begin();
	vector<bool_node*>::const_iterator e=asserts.end();
	for (; i!=e; ++i) {
		ASSERT_node * a = dynamic_cast<ASSERT_node*>(*i);
		if (!a->isNormal()) {
			return true;
		}		
	}
	return false;
 }

bool CEGISSolver::simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive){
	timerclass tc("Simulation");
	bool timesim = (PARAMS->verbosity > 5);
	if(timesim){ tc.start(); }
	int iter = 0;
	VarStore& tmpin = input;
	map<string, BooleanDAG*> empty;	
	BooleanDAG* dag =getProblem();
	vector<bool_node *> const & asserts = dag->getNodesByType(bool_node::ASSERT);
	vector<bool_node *> hasserts;
	if(asserts.size()==0){
		if(timesim){ tc.stop().print("no cex"); }
		return false;
	}
	int intSize = dag->getIntSize();

	
	bool hasH = hasHAsserts(asserts);
	

	bool hasCtrls = !dag->getNodesByType(bool_node::CTRL).empty();
	if (hasCtrls) {
		// must be not specialized
		cout << "Simulate: hasCtrls=" << hasCtrls << endl;
	}
	dag = dag->clone();
	// NOTE xzl: we push here, so that when we finished, popProblem will free the memory occupied by the cloned dag. Note that in the process of slicing, dag itself will be smaller and smaller.
	pushProblem(dag);
	bool hasInput = true;
	int hold = -1;
	do{
		++iter;
		CounterexampleFinder eval(empty, *dag, params.sparseArray, floats);	
		eval.init(tmpin);
		for(int i=0; i<40; ++i){
			if(params.sparseArray > 0){
				tmpin.makeRandom(params.sparseArray);
			}else{
				tmpin.makeRandom();
			}
			
			bool done;
			Assert(!hasCtrls, "This can not happen");
			{
				CounterexampleFinder::Result res = eval.searchLoop(20);		
				done = (res == CounterexampleFinder::FOUND);

				if(res == CounterexampleFinder::UNSAT){
					if(PARAMS->verbosity > 5 ){ 
						cout<<" UNSAT ASSUMPTION "<<((eval.message != NULL)? *eval.message : "")<< endl; 
					}
					popProblem();
					return false;
				}
			}
			eval.trackChanges();
			if(done){
				if(timesim){ tc.stop().print("found a cex by random testing"); }
				if (false) {
					// useful code for debugging
					eval.display(cout);
					//dag->lprint(cout);					
					for (int i=0; false && i<dag->size(); i++) {
						bool_node * node = (*dag)[i];
						int val = eval.getValue(node);
						cout << val << " " << node->lprint() << endl;
						eval.printNodeValue(i);
						if (!val && node->type == bool_node::ASSERT) {
							cout << "assertion fail!" << endl;
						}
					}
				}
				popProblem();
				return true;
			}
		}
		if(expensive.size()>0){
			for(int i=0; i<expensive.size(); ++i){
				Assert(!hasCtrls, "This can not happen");
				{
					eval.run(expensive[i]);
				}
			}
		}
		
		int lowerbound = 0;
		while(true){
			if(PARAMS->verbosity > 8){ cout<<" TESTING HYPOTHESIS ITER "<<iter<<endl; }
			int h;
			
			h = eval.scoreNodes(0);
		    if (hasH) {
				hasserts.clear();				
				vector<bool_node *> const & locasserts = dag->getNodesByType(bool_node::ASSERT);
				filterHasserts(locasserts, h, hasserts);
				lowerbound = 0;
				if(hasserts.size() > 0){
					lowerbound = (*hasserts.rbegin())->id + 1;
				}
				if(PARAMS->verbosity > 8){ cout<<"h = "<<h<<"  hasserts.size()= "<<hasserts.size()<<endl; }
			} 
			//cout << "TESTING h=" << h << " hold=" << hold << endl;
			if (h == -1){//If this happens, nothing is gained from iterating further.
				iter = params.simiters;
				break;
			}
			if(hold == h){
				//Assert(false, "CEGISSolver::simulate: This should not happen");
				//cout<<"INFINITE LOOP!"<<endl;
				break;
			}
			
			hold = h;
			bool_node* niq = (*dag)[h];
			ASSERT_node* an = ASSERT_node::create();
			int am = 0;
			if(niq->getOtype()==OutType::BOOL){
				if(eval.getValue(niq)==0){
					an->mother() = NOT_node::create();
					am = 1;
				}
			}else{
				an->mother() = EQ_node::create();
				an->mother()->father() = CONST_node::create( eval.getValue(niq) );
				am = 2;
			}
			BooleanDAG* tbd;
		    if (hasH) {
				vector<bool_node*>::const_iterator begin = hasserts.begin(), end=hasserts.end();
				tbd = dag->slice(begin, end, h, an);
			} else {
				tbd = dag->slice(h, an);
			}
			if(PARAMS->verbosity >= 10 && tbd->size() < 50){
				tbd->lprint(cout);
			}
			if(PARAMS->verbosity > 8){ cout<<"SLICE SIZE = "<< tbd->size() <<endl; }
			pushProblem(tbd);
			
			lbool rv = baseCheck(controls, tmpin);
			
			popProblem();
			//tc.stop().print("Step").restart();
			// BUGFIX xzl: we need to dislodge before delete a node, to make the DAG sane. otherwise some passess like BackwardAnalysis will fail silently.
			// Important to maintain the order: dislodge must happen before a node's parent being deleted.
			an->dislodge();
			if(am>0){
				// Important
				an->mother()->dislodge();
				if(am==2){
					// This one is not necessary because it must be a CONST (no parent), but for safety we put it here anyways.
					an->mother()->father()->dislodge();
					delete an->mother()->father();
				}
				delete an->mother(); 
			}
			delete an;
			dag->relabel();
			if(rv != l_False){
				//We didn't prove it unsatisfiable; so either it's sat or it timed out.
				if(rv==l_True){
					//It found an input that causes things to change.
					vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::SRC);
					for(size_t i=0; i<ctrls.size(); ++i){
						SRC_node* bn = dynamic_cast<SRC_node*>(ctrls[i]);
						if(bn->id > h){						
							tmpin.getObj(bn->get_name()).makeRandom();
						}
					}
					bool done = eval.run(tmpin);								
					if(done){
						if(timesim){ tc.stop().print("found a cex by solver checking"); }
						popProblem();
						return true;
					}else{
						expensive.push_back(tmpin);
					}					
				}else{
					//In this case, it timed out, 
					cout<<"IGNORANCE!!!!"<<endl;					
					break;
				}
			}else{
				{
					bool_node* btoR = (*dag)[h];
					int nval = eval.getValue(btoR);
					if(PARAMS->verbosity > 8){ cout<<" FOUND CONST: "<<niq->lprint()<<" = "<<nval<<endl; }
					DagOptim cse(*dag, floats);
					int sz = dag->size();					
					if(btoR->type == bool_node::EQ && nval == 1){
						if(btoR->mother()->type == bool_node::CONST){
							int id = btoR->father()->id;
							if (id >= lowerbound)  dag->replace(id, btoR->mother());
						}else if(btoR->father()->type == bool_node::CONST){
							int id = btoR->mother()->id;
							if (id >= lowerbound) dag->replace(id, btoR->father());
						}else if( btoR->mother()->id >  btoR->father()->id){
							int id = btoR->mother()->id;
							if (id >= lowerbound) dag->replace(id, btoR->father());
						}else{
							int id = btoR->father()->id;
							if (id >= lowerbound) dag->replace(id, btoR->mother());
						}
					}
					// h must >= lowrbound
					dag->replace(h, cse.getCnode(nval));
					dag->removeNullNodes();
					cse.process(*dag);
					if(PARAMS->verbosity >= 5){ cout<<" reduced size from "<<sz<<" to "<<dag->size()<<endl; }
					
					if(dag->getNodesByType(bool_node::ASSERT).empty()){
						if(timesim){ tc.stop().print("no cex"); }
						popProblem();
						return false;
					}
				}
				hold = -1;
				break;
			}
		}
		//cout << "simiters: " << iter << endl;
	}while(hasInput && iter < params.simiters && dag->size() > params.simstopsize);
	//cout << "after simiters " << iter << endl;

	bool tv;
	if (hasInput) {
		BackwardsAnalysis ba;
		//cout << "do ba, dag->repOK():";
		//dag->repOK();
		//cout << "simulate: ba" << endl;
		//ba.process(*dag);
		//cout << "simulate: after ba" << endl;
		DagOptim cse(*dag, floats);
		//cout << "simulate: cse" << endl;
		cse.process(*dag);
		//cout << "simulate: after cse" << endl;
		if(PARAMS->verbosity > 5){ cout<<" * Simulation optimized it to = "<<dag->size()<<endl; }	
		if(timesim){ tc.stop().print("didn't find a cex");	}
		cout<<"After all optim"<<endl;
		// getProblem()->lprint(std::cout);
		//dag->lprint(cout);
		tv = (baseCheck(controls, input) == l_True); //Undefined means we are doing light verif so we treat it as false.
	} else {
		// when there is no input satisfying the assumption, we cannot find any counter example
		tv = false;
	}
	popProblem();
	return tv;
}


void CEGISSolver::normalizeInputStore(){
	VarStore tmp = join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;
	NodeSlicer slicer(empty, tmp, *getProblem(), floats);
	slicer.process(*getProblem());
	for(VarStore::iterator it = inputStore.begin(); it != inputStore.end(); ++it){
		if(!slicer.isInfluential(it->getName())){
			it->setVal(last_input[it->getName()]);		
		}
	}
}


void CEGISSolver::redeclareInputs(BooleanDAG* dag, bool firstTime){
	{
		vector<bool_node*>& specIn = dag->getNodesByType(bool_node::SRC);	
		for(size_t i=0; i<specIn.size(); ++i){			
			SRC_node* srcnode = dynamic_cast<SRC_node*>(specIn[i]);	
			int nbits = srcnode->get_nbits();
			if(nbits >= 2 || firstTime){	
				declareInput(specIn[i]->get_name(), nbits, srcnode->getArrSz(), srcnode->getOtype());
			}
		}
	}
	{
	
		vector<bool_node*>& ufunin = dag->getNodesByType(bool_node::UFUN);
		int nbits = dag->getIntSize();
		for(size_t i=0; i<ufunin.size(); ++i){
			UFUN_node* ufunnode = dynamic_cast<UFUN_node*>(ufunin[i]);	
			string tuple_name = ufunnode->getTupleName();

			Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
			int size = tuple_type->actSize;
			int ASize =  1 << PARAMS->NINPUTS;
			for(int tt = 0; tt<size; ++tt){
				stringstream sstr;
				sstr<<ufunnode->get_ufname()<<"_"<<ufunnode->get_uniquefid()<<"_"<<tt;
				OutType* ttype = tuple_type->entries[tt];	
				bool isArr = ttype->isArr ;
				bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
				declareInput( sstr.str() , isBool ? 1 : nbits, (isArr ? ASize : -1), ttype );
			}
		}
	}
}


void CEGISSolver::growInputs(BooleanDAG* dag, BooleanDAG* oridag, bool isTop){
	int gnbits = -1;
	dag->growInputIntSizes();
	if(oridag != dag){
		oridag->growInputIntSizes();
	}
	if(isTop){
		problems[this->curProblem]->growInputIntSizes();
	}
	redeclareInputs(oridag);
}

class CheckControl{
	
	
	typedef enum {CSOLVE, ADJUST} cstate;
	typedef enum {GROW, POP} whatnext;
	cstate state;
	whatnext wn;
	CEGISparams& params;
	int toVerify;	
public:
	typedef enum {POP_LEVEL, GROW_IN, SOLVE, DONE, NEXT_PROBLEM} decision;
	CheckControl(CEGISparams& p_params, int p_toVerify):params(p_params),state(CSOLVE), wn(GROW), toVerify(p_toVerify){}
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
					toVerify--;
					if(toVerify==0){
						return DONE;
					}else{						
						return NEXT_PROBLEM;
					}
				}
				
			}else{
				wn = GROW;
				if(level > 1){
					return POP_LEVEL;
				}else{
					toVerify--;
					if(toVerify==0){
						return DONE;
					}else{						
						return NEXT_PROBLEM;
					}
				}
			}
		}
		Assert(false, "No return");
	}	
};

// check verifies that controls satisfies all asserts
// and if there is a counter-example, it will be put to input
bool CEGISSolver::check(VarStore& controls, VarStore& input){
	
	BooleanDAG* oriProblem = getProblem();
	bool hardcode = PARAMS->olevel >= 6;
	bool rv = false;
	int ninputs = -1;
	CheckControl cc(params, problems.size());
	//cout<<"check: Before hard code"<<endl;
	//getProblem()->lprint(std::cout);
	if(hardcode){
		pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL, floats));		
	}
//	cout<<"After hard code"<<endl;
	// getProblem()->lprint(std::cout);
	do{
		switch(cc.actionDecide(problemLevel() - (hardcode? 1: 0),getProblem())){
			case CheckControl::POP_LEVEL:{
				// must save the int size! tbd will be deleted by popProblem()
				int tbdIntSize = getProblem()->getIntSize();
				popProblem();
				cout<<"CONTROL: Popping to level "<<problemLevel()<<endl;
				if(hardcode){
					tbdIntSize = getProblem()->getIntSize();
					// must save the int size! tbd will be deleted by popProblem()
					popProblem();
					oriProblem = getProblem();
					if(tbdIntSize != oriProblem->getIntSize()){
						redeclareInputs(oriProblem);						
					}
					pushProblem(hardCodeINode(getProblem(), controls, bool_node::CTRL, floats));					
				}else{
					oriProblem = getProblem();
					if(tbdIntSize != oriProblem->getIntSize()){
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

				BooleanDAG* dag = getProblem();
				if(PARAMS->verbosity > 5){
					cout<<"CONTROL: growing l="<<problemLevel()<<" inputs to size "<< (dag->getIntSize()+1) <<endl;
				}				
				growInputs(dag, oriProblem, (problemLevel() - (hardcode? 1: 0)) == 1 );
				continue;
			}
			case CheckControl::SOLVE:{

				if (files.count(curProblem) > 0) {
					map<string, BooleanDAG*> empty;
					BooleanDAG* dag = getProblem();
					CounterexampleFinder eval(empty, *dag, params.sparseArray, floats);
					VarStore& tmpin = input;
					eval.init(tmpin);

					vector<bool_node*>& inputs = dag->getNodesByType(bool_node::SRC);
					CounterexampleFinder::Result res = eval.fromFile(files[curProblem], floats,  inputs);
					
					while (res == CounterexampleFinder::MOREBITS) {
						BooleanDAG* dag = getProblem();
						if (PARAMS->verbosity > 5) {
							cout << "CONTROL: growing l=" << problemLevel() << " inputs to size " << (dag->getIntSize() + 1) << endl;
						}
						growInputs(dag, oriProblem, (problemLevel() - (hardcode ? 1 : 0)) == 1);
						res = eval.fromFile(files[curProblem], floats, inputs);
					}					
					if (dag != oriProblem) {
						vector<bool_node*>& oInputs = oriProblem->getNodesByType(bool_node::SRC);
						auto oin = oInputs.begin();
						for (auto in = inputs.begin(); in != inputs.end(); ++in, ++oin) {
							if (((SRC_node*)(*oin))->arrSz != ((SRC_node*)(*in))->arrSz) {
								((SRC_node*)(*oin))->arrSz = ((SRC_node*)(*in))->arrSz;
							}
						}
					}
					rv = (res == CounterexampleFinder::FOUND);
					continue;
				}


				if(params.simulate){
					rv = simulate(controls, input, expensives[ curProblem ]);
				}else{
					rv = (baseCheck(controls, input)==l_True);
				}
				continue;
			}
			case CheckControl::NEXT_PROBLEM:{
				
				int tmpPid = (curProblem + 1) % problems.size() ;				
				if(!hcoder.checkHarnessSwitch(tmpPid)){
					if(PARAMS->verbosity > 5){
						cout<<"Failed from leftover clauses from concretization"<<endl;
					}
					rv = true;
					continue;
				}
				int n = problemLevel();
				for(int i=0; i<n; ++i){ popProblem(); }

				curProblem = tmpPid;
				if(PARAMS->verbosity > 5){
					cout<<"Switching to problem "<<curProblem<<endl;
				}
				pushProblem(problems[curProblem]->clone());				
				if(hardcode){				
					oriProblem = getProblem();
					pushProblem(hardCodeINode(oriProblem, controls, bool_node::CTRL, floats));
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


int getSolverVal(bool_node * node, vector<Tvalue> & node_ids, SATSolver & solver, int * j) {
	if (node->type == bool_node::ASSERT) {
		node = node->mother();
	}
	int i = node->id;
	int sv = node_ids[i].eval(&solver);
	if (j) { *j = i; }
	return sv;
}

lbool CEGISSolver::baseCheck(VarStore& controls, VarStore& input){
	Dout( cout<<"check()"<<endl );
	MiniSATSolver mngCheck("checker", SATSolver::CHECKER);	
	SolverHelper dirCheck(mngCheck);
	dirCheck.setMemo(params.setMemo);
	if(params.lightVerif){		
		mngCheck.lightSolve();
	}
	//timerclass tc("* TIME TO ADD CONTROLS ");
	//tc.start();				
	setNewControls(controls, dirCheck);
	//if(PARAMS->verbosity > 2){ tc.stop().print(); }
	
    
    int result = mngCheck.solve();
    
	//dirCheck.printAllVars();
    if(params.printDiag){	    
		printDiagnostics(mngCheck, 'c');
    }
    if (result != SATSolver::SATISFIABLE){
    	mngCheck.reset();
    	if( result != SATSolver::UNSATISFIABLE){
	    	return l_Undef;
    	}
    	return l_False;
    }
    
    
    
	for(VarStore::iterator it = input.begin(); it !=input.end(); ++it){
		const string& cname = it->getName();
		if(dirCheck.checkVar(cname)){
			int cnt = dirCheck.getArrSize(cname);
			Assert( cnt == it->globalSize(), "baseCheck: SIZE MISMATCH: "<<cnt<<" != "<<it->globalSize()<< " " << cname << endl);
			VarStore::objP* tmp = &(*it);
			for(int i=0; i<cnt; ++i){				
				int val = mngCheck.getVarVal(dirCheck.getArr(cname, i));
				int qq = tmp->size() * tmp->index;
				tmp = tmp->setBit(i-qq, (val==1)? 1 : 0);
			}
			Dout( cout<<" input "<<cname<<"  has value "<<it->getInt()<<endl );
		}
	}
	Dout( dirCheck.print() );
	if(false){ //This is useful code when debugging;
		cout << "counter example:" << endl;
		input.printContent(cout);
		map<string, BooleanDAG*> empty;
		BooleanDAG * prob = getProblem();
		NodeEvaluator eval(empty, *prob, floats);
		eval.run(input);
		cout<<"PRINT EVAL"<<endl;
		for(size_t i=0; i<check_node_ids.size(); ++i){
//			cout<<i<<"=";
//			check_node_ids[i].print(cout, &mngCheck);
//			cout<< " vs ";
//			eval.printNodeValue(i);
			bool_node * node = (*prob)[i];
			cout << node->lprint();
			int ev = eval.getValue(node);
			int j;
			int sv = getSolverVal(node, check_node_ids, mngCheck, &j);
			if (node->getOtype() == OutType::FLOAT) {
				cout << "FLT " << floats.getFloat(sv) << " vs " << floats.getFloat(ev);
			}
			else {
				cout << "INT " << sv << " vs " << ev;
			}			
			cout << endl;
			check_node_ids[j].print(cout, &mngCheck);
			cout<< " NodeEvaluator says:";
			eval.printNodeValue(i);
		}
		cout<<"???"<<endl;
	}

	mngCheck.reset();
	return l_True;
}


void CEGISSolver::setNewControls(VarStore& controls, SolverHelper& dirCheck){
	int idx = 0;	
	map<bool_node*,  int> node_values;
	check_node_ids.clear();
	check_node_ids.resize( getProblem()->size() );	
	size_t nbits = getProblem()->getIntSize();
	for (BooleanDAG::iterator node_it = getProblem()->begin(); node_it != getProblem()->end(); ++node_it, ++idx) {
		(*node_it)->flag = true;
		if ((*node_it)->type == bool_node::CTRL) {
			CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(*node_it);
			int locnbits = ctrlnode->get_nbits();
			node_values[(*node_it)] = valueForINode(ctrlnode, controls, locnbits);
		}
		if ((*node_it)->type == bool_node::SRC) {
			SRC_node* srcnode = dynamic_cast<SRC_node*>(*node_it);
			int arsz = srcnode->getArrSz();
			if (arsz < 0) { arsz = 1; }
			if (srcnode->isTuple) {
				Assert(false, "Not possible");
			}
			else {
				dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits()*arsz);
			}
		}
		if ((*node_it)->type == bool_node::UFUN) {
			UFUN_node* ufunnode = dynamic_cast<UFUN_node*>(*node_it);			
			string tuple_name = ufunnode->getTupleName();

			Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
			int size = tuple_type->actSize;
			int ASize = 1 << PARAMS->NINPUTS;
			for (int tt = 0; tt<size; ++tt) {
				stringstream sstr;
				sstr << ufunnode->get_ufname() << "_" << ufunnode->get_uniquefid() << "_" << tt;
				OutType* ttype = tuple_type->entries[tt];
				bool isArr = ttype->isArr;
				bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
				dirCheck.declareInArr(sstr.str(), (isBool ? 1 : nbits) * (isArr ? ASize : 1));
			}
		}
	}	
	//cout << "setNewControls: problem=";
	//getProblem()->lprint(cout);
	NodesToSolver::createConstraints(*getProblem(), dirCheck, node_values, check_node_ids, floats, params.sparseArray);
}



bool CEGISSolver::solveFromCheckpoint(istream& in){
	Assert(false, "This functionality is no longer supported.");
	
	return false;
}




void printDiagnostics(SATSolver& mng, char c){
   		mng.printDiagnostics(c);
}

void CEGISSolver::print_control_map(ostream& out){
	map<string, string> values;
	get_control_map(values);
	for(auto it = values.begin(); it != values.end(); ++it){
		out<<it->first<<"\t"<<it->second<<endl;
	}
}


void CEGISSolver::get_control_map(map<string, string>& values){
	for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it){
		stringstream str;
		if(it->otype == OutType::FLOAT)
		{
			str << floats.getFloat(it->getInt());
		}
		else
		{
			str << it->getInt();
		}
		values[it->getName()] = str.str();
	}
	for (auto it = ctrlStore.synths.begin(); it != ctrlStore.synths.end(); ++it) {
		//stringstream str;
		Assert(ctrlStore.synthouts.find(it->first) != ctrlStore.synthouts.end(), "Synthouts should have been fleshed out")
			//it->second->print(str);
			values[it->first] = ctrlStore.synthouts[it->first];
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


void CEGISSolver::setup2QBF(ofstream& out){
	MiniSATSolver mngCheck("checker", SATSolver::CHECKER);		
	SolverHelper dirCheck(mngCheck);
	dirCheck.setMemo(params.setMemo);
	BooleanDAG* bd = problems[problems.size()-1];
	out<<"c 2qbf problem of the form. \\forall C \\exists In, temp.  (not Correct(C,in, temp)) "<<endl;
	out<<"c vars listed as UV are the C vars; vars listed as EV are the In vars; vars not listed are temps."<<endl;

	{
		vector<bool_node*>& inter = bd->getNodesByType(bool_node::CTRL);
		for(BooleanDAG::iterator node_it = inter.begin(); node_it != inter.end(); ++node_it){			
			INTER_node* srcnode = dynamic_cast<INTER_node*>(*node_it);										
			dirCheck.declareControl((CTRL_node*)*node_it);							
			int base = dirCheck.getVar(srcnode->get_name());
			int n = dirCheck.getArrSize(srcnode->get_name());
			out<<"c "<<	(*node_it)->get_name()<<endl;
			for(int i=0; i<n; ++i){				
				out<<"UV "<<(i+base)+1<<endl;
			}
			
		}
	}
	{
		vector<bool_node*>& inter = bd->getNodesByType(bool_node::SRC);
		for(BooleanDAG::iterator node_it = inter.begin(); node_it != inter.end(); ++node_it){			
			INTER_node* srcnode = dynamic_cast<INTER_node*>(*node_it);										
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits());			
			int base = dirCheck.getVar(srcnode->get_name());
			int n = dirCheck.getArrSize(srcnode->get_name());
			out<<"c "<<	(*node_it)->get_name()<<endl;
			for(int i=0; i<n; ++i){		
				out<<"EV "<<(i+base)+1<<endl;		
			}
		}
	}
	check_node_ids.resize(bd->size());
	map<bool_node*,  int> node_values;
	
	
	NodesToSolver::createConstraints(*bd, dirCheck, node_values, check_node_ids, floats);
	mngCheck.finish();
	mngCheck.writeDIMACS(out);
	dirCheck.outputVarMap(out);		
}
