#include "CEGISSolver.h"
#include "timerclass.h"
#include <queue>
#include "CommandLineArgs.h"
#include "DagOptim.h"
#include "NodesToSolver.h"
#include "NodesToEuclid.h"
#include "NodeSlicer.h"
#include "MiniSATSolver.h"
#include "CounterexampleFinder.h"
#include "SkVal.h"

//extern CommandLineArgs* PARAMS;




void CEGISSolver::addProblem(Harness *harness, File *file){
    checker->addProblem(harness, file);
	problems.push_back(harness);

    {

        Harness* inlined_harness = harness->produce_inlined_dag();
        vector<bool_node*>& problemIn = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        for(int i=0; i<problemIn.size(); ++i){
            CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);
            if(!ctrlnode->get_Angelic() /*&& ctrlnode->getOtype() != OutType::FLOAT*/){
                declareControl(ctrlnode);
            }
        }
//        inlined_harness->clear();
    }

	finder->updateCtrlVarStore(ctrlStore);	
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

bool CEGISSolver::solve(){
	if(problems.size()==0){
		return true;
	}
	Assert(problemStack_is_empty(), "A big bug");

	//**
	//pushProblem((*problems.rbegin())->clone());

	if(PARAMS->verbosity > 1){
		cout<<"inputSize = "<<checker->get_input_store().getBitsize()<<"\tctrlSize = "<<ctrlStore.getBitsize()<<endl;
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
	
	//**
	//popProblem();

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
		fail = !find(checker->get_input_store(), ctrlStore, true);
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
	return !fail;f
}
*/


bool CEGISSolver::solveCore(){	
	int iterations = 0;
	bool fail = false;
    BooleanDAG* counterexample_concretized_dag;
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
			if(PARAMS->verbosity > 1){ cout<<"BEG CHECK"<<endl; }
			ctimer.restart();

			if(!hc.solvePendingConstraints())
			{
			    return false;
			};

            counterexample_concretized_dag = checker->check(ctrlStore);
			doMore = counterexample_concretized_dag != nullptr;

		 	ctimer.stop();
			if(PARAMS->verbosity > 1){ cout<<"END CHECK"<<endl; }			
		}
		if(PARAMS->verbosity > 0){cout<<"********  "<<iterations<<"\tftime= "<<ftimer.get_cur_ms() <<"\tctime= "<<ctimer.get_cur_ms()<<endl; }
		++iterations;
		if( params.iterlimit > 0 && iterations >= params.iterlimit){ cout<<" * bailing out due to iter limit"<<endl; fail = true; break; }

		// swap
		if(doMore) hasInputChanged = true;
		else hasInputChanged = false;
		
		if (!doMore) {//To make sure the control store also fleshes out the expressions from custom Synthesizers before being copied
			ctrlStore.finalizeSynthOutputs();
		}
		
		// Minimization loop-- found a solution, but can i find a better solution?
		if(PARAMS->minvarHole && !doMore && mhsizes.size() != 0){
			// store the current solution
			storePreviousSolution(checker->get_input_store(), ctrlStore);
			// optimization: only add inputs if the verification fails
			if(finder->minimizeHoleValue(ctrlStore, mhnames, mhsizes))
				doMore=true;
			else{
				doMore = false;
				checker->get_input_store() = prevInputStore;
				ctrlStore = prevCtrlStore;
			}
		}

		// Synthesizer
		if (doMore) {// Find
			// cout<<"!%";	for(int i=0; i< input.size(); ++i) cout<<" "<<(input[i]==1?1:0); cout<<endl;


			if (hasInputChanged) {
				if(PARAMS->verbosity > 5){ cout<<"!% ";checker->get_input_store().printBrief(cout); cout<<endl;}
				if(PARAMS->verbosity > 9){ cout<<"!% ";checker->get_input_store().printContent(cout); cout<<endl;}
				std::vector<int, std::allocator<int> > instore_serialized = checker->get_input_store().serialize();
                //if(params.simplifycex != CEGISparams::NOSIM){ abstractProblem(); }
			}
			if(PARAMS->verbosity > 2 || PARAMS->showInputs){ cout<<"BEG FIND"<<endl; }
			ftimer.restart();
			try{
                doMore = finder->find(counterexample_concretized_dag, ctrlStore, hasInputChanged);
			}catch(BasicError& e){
				doMore = false;
			}
			ftimer.stop();
			if(PARAMS->verbosity > 2 || PARAMS->showInputs){  cout<<"END FIND"<<endl; }
		}

		if(hasInputChanged && !doMore){
			if(PARAMS->minvarHole && prevSolutionFound){
				cout << "Cannot find a solution with lower value, hence taking the previous solution" << endl;
				checker->get_input_store() = prevInputStore;
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
    last_elapsed_time = new ElapsedTime(ftimer.get_tot_ms(), ctimer.get_tot_ms(), ttimer.get_tot_ms());
    cout << "*" << last_elapsed_time->to_string() << endl;
	finder->retractAssumptions();


	return !fail;
}




void CEGISSolver::getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes){
	map<string, CTRL_node*> minimizes;
	for(size_t i=0; i<problems.size(); ++i){
		BooleanDAG* dag = problems[i]->get_dag();
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

 void BitSet::print(ostream& os){
		int i=next(-1);
		os<<"{";
		while(i != -1){
			os<<", "<<i;
			i = next(i);
		}
		os<<"}"<<endl;

	}

void CEGISSolver::normalizeInputStore(){
	VarStore tmp = join(checker->get_input_store(), ctrlStore);
	map<string, BooleanDAG*> empty;
	NodeSlicer slicer(empty, tmp, *getProblem(), floats);
	slicer.process(*getProblem());
	for(VarStore::iterator it = checker->get_input_store().begin(); it != checker->get_input_store().end(); ++it){
		if(!slicer.isInfluential(it->getName())){
			it->setVal(last_input[it->getName()]);		
		}
	}
}

bool CEGISSolver::solveFromCheckpoint(istream& in){
	Assert(false, "This functionality is no longer supported.");
	
	return false;
}

void CEGISSolver::print_control_map(ostream& out){
	map<string, string> values;
    get_control_map_as_map_str_str(values);
	for(auto it = values.begin(); it != values.end(); ++it){
		out<<it->first<<"\t"<<it->second<<endl;
	}
}

void CEGISSolver::get_control_map_as_map_str_skval(Assignment_SkVal *values)
{
    for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it){
        if(it->otype == OutType::FLOAT)
        {
            values->set(it->getName(), new SkValFloat((float) floats.getFloat(it->getInt()), it->get_size()));
        }
        else if (it->otype == OutType::INT)
        {
            values->set(it->getName(), new SkValInt(it->getInt(), it->get_size()));
            assert((( SkValInt*) values->get(it->getName()))->get() == ctrlStore[it->getName()]);
        }
        else if (it->otype == OutType::BOOL)
        {
            assert(it->get_size() == 1);
            values->set(it->getName(), new SkValBool(it->getInt()));
            assert((( SkValBool*) values->get(it->getName()))->get() == ctrlStore[it->getName()]);
        }
        else
        {
            AssertDebug(false, "need to add more cases in CEGISSolver::get_control_map_as_map_str_skval.")
        }
    }
    for (auto it = ctrlStore.synths.begin(); it != ctrlStore.synths.end(); ++it) {

        AssertDebug(false, "need to implement synthouts to map to SkVals, rather than strings");
//
//        //stringstream str;
//        Assert(ctrlStore.synthouts.find(it->first) != ctrlStore.synthouts.end(), "Synthouts should have been fleshed out")
//        //it->second->print(str);
//        values[it->first] = ctrlStore.synthouts[it->first];
    }

    VarStore* test_varstore = values->to_var_store();

    assert(test_varstore->size() == ctrlStore.size());

    for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it) {
        assert(ctrlStore[it->getName()] == (*test_varstore)[it->getName()]);
    }
    for(VarStore::iterator it = (*test_varstore).begin(); it !=(*test_varstore).end(); ++it) {
        assert((*test_varstore)[it->getName()] == ctrlStore[it->getName()]);
    }

}

void CEGISSolver::get_control_map_as_map_str_str(map<string, string>& values){
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



// can remove
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
	BooleanDAG* bd = problems[problems.size()-1]->get_dag();
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
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits(), srcnode->getOtype());
			int base = dirCheck.getVar(srcnode->get_name());
			int n = dirCheck.getArrSize(srcnode->get_name());
			out<<"c "<<	(*node_it)->get_name()<<endl;
			for(int i=0; i<n; ++i){		
				out<<"EV "<<(i+base)+1<<endl;		
			}
		}
	}
	get_check_node_ids().resize(bd->size());
	map<bool_node*,  int> node_values;
	
	
	NodesToSolver::createConstraints(*bd, dirCheck, node_values, get_check_node_ids(), floats);
	mngCheck.finish();
	mngCheck.writeDIMACS(out);
	dirCheck.outputVarMap(out);		
}
