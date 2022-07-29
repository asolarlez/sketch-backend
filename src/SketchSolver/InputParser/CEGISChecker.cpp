#include "NodesToSolver.h"
#include "CEGISChecker.h"
#include "File.h"

int CEGISChecker::valueForINode(INTER_node* inode, VarStore& values, int& nbits){
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

// Method for cutting the problem for the synthesizer. It first finds the
// failing assertion and then based on its relative position on the DAG decides
// to remove or keep the assertions following the failing assertion.
void CEGISChecker::abstractProblem(VarStore & inputStore, VarStore& ctrlStore){
	if(inputStore.getBitsize() == 0) return;
	BooleanDAG* dag = getProblemDag()->clone();
	int orisize = dag->size();
	
	if(orisize < 200){ return; }
	
	VarStore tmp = old_join(inputStore, ctrlStore);
	map<string, BooleanDAG*> empty;	
	
	NodeEvaluator eval(*dag, floats);
	
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
        AssertDebug(false, "SketchFunction here doesn't accept an env, this is not tested. It might work, but it might not work either. env is needed for inlining because it stores the function_map");
		pushProblem(new BooleanDagLightUtility(dag));
		if(PARAMS->verbosity > 2){
			cout<<"Level "<<problemLevel()<<"Replacing dag of size "<<orisize<<" with size "<<dag->size()<<endl;
		}
	}else{
		dag->clear();
		delete dag;
	}
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

bool CEGISChecker::simulate(VarStore& controls, VarStore& input, vector<VarStore>& expensive){
	timerclass tc("Simulation");
	bool timesim = (PARAMS->verbosity > 5);
	if(timesim){ tc.start(); }
	int iter = 0;
	VarStore& tmpin = input;
	map<string, BooleanDAG*> empty;	
	BooleanDAG* dag = getProblemDag();
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
    pushProblem(new BooleanDagLightUtility(dag));
	bool hasInput = true;
	int hold = -1;
	do{
		++iter;
		CounterexampleFinder eval(empty, *dag, params.sparseArray, floats);	
		eval.init(tmpin);
        assert(eval.get_src_name_id_linking_done());
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
                    eval.reset_src_to_input_id();
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
                eval.reset_src_to_input_id();
				popProblem();
				return true;
			}
		}
        assert(eval.get_src_name_id_linking_done());
		if(expensive.size()>0){
			for(int i=0; i<expensive.size(); ++i){
				Assert(!hasCtrls, "This can not happen");
				{
					eval.run(expensive[i], false);
				}
			}
		}
        assert(eval.get_src_name_id_linking_done());
		
		int lowerbound = 0;
		while(true){

            assert(eval.get_src_name_id_linking_done());
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
                assert(eval.get_src_name_id_linking_done());
				break;
			}
			if(hold == h){
				//Assert(false, "CEGISSolver::simulate: This should not happen");
				//cout<<"INFINITE LOOP!"<<endl;
                assert(eval.get_src_name_id_linking_done());
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
            pushProblem(new BooleanDagLightUtility(tbd));
			
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
            assert(eval.get_src_name_id_linking_done());
			if(rv != l_False){
				//We didn't prove it unsatisfiable; so either it's sat or it timed out.
				if(rv==l_True){
					//It found an input that causes things to change.
					auto ctrls = dag->getNodesByType(bool_node::SRC);
					for(size_t i=0; i<ctrls.size(); ++i){
						SRC_node* bn = dynamic_cast<SRC_node*>(ctrls[i]);
						if(bn->id > h){						
							tmpin._getObj(bn->get_name()).makeRandom();
						}
					}
                    assert(eval.get_src_name_id_linking_done());
					bool done = eval.run(tmpin, false);
                    assert(eval.get_src_name_id_linking_done());
					if(done){
						if(timesim){ tc.stop().print("found a cex by solver checking"); }
                        eval.reset_src_to_input_id();
						popProblem();
						return true;
					}else{
						expensive.push_back(tmpin);
					}
                    assert(eval.get_src_name_id_linking_done());
				}
                else{
					//In this case, it timed out, 
					cout<<"IGNORANCE!!!!"<<endl;
                    assert(eval.get_src_name_id_linking_done());
					break;
				}
			}
            else{
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
                        eval.reset_src_to_input_id();
						popProblem();
						return false;
					}
				}
				hold = -1;
                assert(eval.get_src_name_id_linking_done());
				break;
			}
            assert(eval.get_src_name_id_linking_done());
		}
        assert(eval.get_src_name_id_linking_done());
		//cout << "simiters: " << iter << endl;
        eval.reset_src_to_input_id();
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
		// getProblemDag()->lprint(std::cout);
		//dag->lprint(cout);
		tv = (baseCheck(controls, input) == l_True); //Undefined means we are doing light verif so we treat it as false.
	} else {
		// when there is no input satisfying the assumption, we cannot find any counter example
		tv = false;
	}
	popProblem();
	return tv;
}


void CEGISChecker::growInputs(VarStore & inputStore, BooleanDAG* dag, BooleanDAG* oridag, bool isTop){
	int gnbits = -1;
	dag->growInputIntSizes();
	if(oridag != dag){
		oridag->growInputIntSizes();
	}
	if(isTop){
		problems[this->curProblem]->get_dag()->growInputIntSizes();
	}
	redeclareInputs(inputStore, oridag);
}

class CheckControl{
	
	
	typedef enum {CSOLVE, ADJUST} cstate;
	typedef enum {GROW, POP} whatnext;
	cstate state;
	whatnext wn;
	CEGISParams& params;
	int toVerify;	
public:
	typedef enum {POP_LEVEL, GROW_IN, SOLVE, DONE, NEXT_PROBLEM} decision;
	CheckControl(CEGISParams& p_params, int p_toVerify): params(p_params), state(CSOLVE), wn(GROW), toVerify(p_toVerify){}
	decision actionDecide(int level, BooleanDAG* dag, bool from_file){
		if(params.simplifycex == CEGISParams::SIMSIM && level != 1){
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
				if(dag->getIntSize() < params.NINPUTS && !from_file){
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
BooleanDAG* CEGISChecker::check(VarStore& controls, VarStore& input){

    if(problemStack.empty())
	{
		pushProblem((*problems.rbegin())->_clone());
	}	
	BooleanDAG* oriProblem = getProblemDag();
	bool rv = false;
	int ninputs = -1;
	CheckControl cc(params, problems.size());
	//cout<<"check: Before hard code"<<endl;
	//getProblemDag()->lprint(std::cout);


    controls.relabel(getHarness()->get_dag()->getNodesByType(bool_node::CTRL));
	pushProblem(getHarness()->produce_concretization(&controls, bool_node::CTRL), true);

    #ifdef CHECK_FILE_INVARIANT
    {
        if (files.find(curProblem) != files.end()) {
            BooleanDAG *concretized_dag = getHarness()->get_dag();
            map<string, BooleanDAG *> empty;
            CounterexampleFinder eval(empty, *concretized_dag, params.sparseArray, floats);
            VarStore &tmpin = get_input_store();
            eval.init(tmpin);
            auto inputs = concretized_dag->getNodesByType(bool_node::SRC);
            File *file = files[curProblem];
            assert(eval.check_file_invariant(file));
            //cout << "FILE PASSES OK (IN CHECKER) !!" << endl;
        }
    }
    #endif

//	assert(false);
//	OLD: pushProblem(hardCodeINode(getProblemDag(), controls, bool_node::CTRL, floats)));
//    cout<<"After hard code"<<endl;
	// getProblemDag()->lprint(std::cout);
	do{
		switch(cc.actionDecide(problemLevel() - 1, getProblemDag(), files.count(curProblem) > 0)){
			case CheckControl::POP_LEVEL:{
				// must save the int size! tbd will be deleted by popProblem()
				int tbdIntSize = getProblemDag()->getIntSize();
				popProblem();
				cout<<"CONTROL: Popping to level "<<problemLevel()<<endl;
				{
				    tbdIntSize = getProblemDag()->getIntSize();
					// must save the int size! tbd will be deleted by popProblem()
					popProblem();
					oriProblem = getProblemDag();
					if(tbdIntSize != oriProblem->getIntSize()){
						redeclareInputs(input, oriProblem);
					}
					pushProblem(getHarness()->produce_concretization(&controls, bool_node::CTRL));
//					pushProblem(hardCodeINode(getProblemDag(), controls, bool_node::CTRL, floats));
				}

				continue;
			}
			case CheckControl::DONE:{
				clear_problemStack();
				return nullptr; //no counterexample
//				return false; //no counterexample
			}
			case CheckControl::GROW_IN:{

				BooleanDAG* dag = getProblemDag();
				if(PARAMS->verbosity > 5){
					cout<<"CONTROL: growing l="<<problemLevel()<<" inputs to size "<< (dag->getIntSize()+1) <<endl;
				}				
				growInputs(input, dag, oriProblem, (problemLevel() - 1) == 1 );

				continue;
			}
			case CheckControl::SOLVE:{

				if (files.count(curProblem) > 0) {
					map<string, BooleanDAG*> empty;
					BooleanDAG* dag = getProblemDag();
					CounterexampleFinder eval(empty, *dag, params.sparseArray, floats);
					eval.init(input);
					auto inputs = dag->getNodesByType(bool_node::SRC);
					CounterexampleFinder::Result res = eval.fromFile(files[curProblem], floats,  inputs);
                    eval.reset_src_to_input_id();
                    assert(dag != oriProblem);

					{
						auto oInputs = oriProblem->getNodesByType(bool_node::SRC);
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

                hcoder.setCurrentHarness(tmpPid);

				int n = problemLevel();
				for(int i=0; i<n; ++i){ popProblem(); }

				curProblem = tmpPid;
				if(PARAMS->verbosity > 5){
					cout<<"Switching to problem "<<curProblem<<endl;
				}
				pushProblem(problems[curProblem]->_clone());

				pushProblem(getHarness()->produce_concretization(&controls, bool_node::CTRL));
//				pushProblem(hardCodeINode(oriProblem, controls, bool_node::CTRL, floats));

				continue;
			}
            default:
                Assert(false, "MISSING CASE in CEGISChecker::check.");
		}
	}while(!rv);
	popProblem();

    { // angelic var magic xD
        if (PARAMS->angelic_model) {
//            AssertDebug(false, "ASK ARMANDO WHAT ANGELIC HOLES ARE, THEY ARE PROBABLY CTRLS.");
            input.newVar("__rs_node", 1, OutType::BOOL, bool_node::SRC, "__rs_node", "CEGISChecker::check(...)");
            input.setVarVal("__rs_node", 0, OutType::BOOL, bool_node::SRC);
        }
    }
    //Return counter-example concretized dag

    BooleanDAG* ret_dag = getHarness()->produce_concretization(&input, bool_node::SRC)->get_dag();

//    BooleanDAG* ret_dag = getHarness()->get_dag()->clone();
//    getHarness()->get_env()->doInline(*ret_dag, input, bool_node::SRC);

	return ret_dag;
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


lbool CEGISChecker::baseCheck(VarStore& controls, VarStore& input){
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
        mngCheck.printDiagnostics('c');
    }
    if (result != SAT_SATISFIABLE){
    	mngCheck.reset();
    	if( result != SAT_UNSATISFIABLE){
	    	return l_Undef;
    	}
    	return l_False;
    }
    
    
    
	for(auto it = input.begin(); it !=input.end(); ++it){
		const string& cname = it->get_name();
		if(dirCheck.checkVar(cname)){
			int cnt = dirCheck.getArrSize(cname);
			Assert( cnt == it->globalSize(), "baseCheck: SIZE MISMATCH: "<<cnt<<" != "<<it->globalSize()<< " " << cname << endl);
            VarStoreElementTrait* tmp = &(*it);
			for(int i=0; i<cnt; ++i){				
				int val = mngCheck.getVarVal(dirCheck.getArr(cname, i));
				int qq = tmp->element_size() * tmp->get_index();
				tmp->set_bit(i, (val==1)? 1 : 0);
			}
			Dout( cout<<" input "<<cname<<"  has value "<<it->getInt()<<endl );
		}
	}
	Dout( dirCheck.print() );
	if(false){ //This is useful code when debugging;
		cout << "counter example:" << endl;
		input.printContent(cout);
		map<string, BooleanDAG*> empty;
		BooleanDAG * prob = getProblemDag();
		NodeEvaluator eval(*prob, floats);
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


void CEGISChecker::setNewControls(VarStore& controls, SolverHelper& dirCheck){
	int idx = 0;	
	map<bool_node*,  int> node_values;
	check_node_ids.clear();
	check_node_ids.resize(getProblemDag()->size() );
	size_t nbits = getProblemDag()->getIntSize();
	for (BooleanDAG::iterator node_it = getProblemDag()->begin(); node_it != getProblemDag()->end(); ++node_it, ++idx) {
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
				dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits()*arsz,  srcnode->getOtype(), bool_node::SRC, srcnode->get_name(), "setNewControls()#1");
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
				sstr << ufunnode->get_ufun_name() << "_" << ufunnode->get_uniquefid() << "_" << tt;
				OutType* ttype = tuple_type->entries[tt];
				bool isArr = ttype->isArr;
				bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
				dirCheck.declareInArr(sstr.str(), (isBool ? 1 : nbits) * (isArr ? ASize : 1), ttype, bool_node::UFUN, ufunnode->get_original_ufname(), "setNewControls()#2");
			}
		}
	}	
	//cout << "setNewControls: problem=";
	//getProblemDag()->lprint(cout);
	NodesToSolver::createConstraints(*getProblemDag(), dirCheck, node_values, check_node_ids, floats, params.sparseArray);
}

void CEGISChecker::addProblem(BooleanDagLightUtility *harness, File *file)
{
    curProblem = (int) problems.size();
    problems.push_back(harness);
    if (file != nullptr) {
        files[curProblem] = file;
    }

    BooleanDagLightUtility* inlined_harness = harness;
    bool new_clone = false;
    if(new_clone) {
        //BE CAREFUL, THIS RENAMES THE SOURCE DAG OF THE HOLES.
        inlined_harness = inlined_harness->produce_inlined_dag();
        inlined_harness->increment_shared_ptr();
        new_clone = true;
    }
    else
    {
        //ASSERT THAT THE DAG WAS ALREADY INLINED.
        //IF THIS FAILS THE DAG WASN'T INLINED.
        if(!inlined_harness->get_dag()->getNodesByType(bool_node::UFUN).empty()) {
            for(auto it: inlined_harness->get_dag()->getNodesByType(bool_node::UFUN)) {
                string ufname = ((UFUN_node *) it)->get_ufun_name();
                assert(inlined_harness->get_env()->function_map.find(ufname) == inlined_harness->get_env()->function_map.end());
            }
        }
        for(auto it:inlined_harness->get_dag()->getNodesByType(bool_node::CTRL)) {
            assert(it->get_name() != "#PC");
        }
    }

    redeclareInputsAndAngelics(get_input_store(), inlined_harness->get_dag());

    // IS THIS DEBUG CODE? YES
    Dout( cout << "problem->get_n_controls() = " << root_dag->get_n_controls() << "  " << root_dag << endl );
    {
        auto problemIn = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        if(PARAMS->verbosity > 2){
            cout<<"  # OF CONTROLS:    "<< problemIn.size() <<endl;
        }
        int cints = 0;
        int cbits = 0;
        int cfloats = 0;
        for(int i=0; i<problemIn.size(); ++i){
            CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);
            if(ctrlnode->getOtype() == OutType::BOOL){
                cbits++;
            } else if (ctrlnode->getOtype() == OutType::FLOAT) {
                cfloats++;
            } else{
                cints++;
            }
        }
        if(PARAMS->verbosity > 2){
            cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
        }
    }

    if(new_clone) {
        inlined_harness->clear();
    }
}
