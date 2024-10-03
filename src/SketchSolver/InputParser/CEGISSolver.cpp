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
#include "BenchmarkScore.h"

void CEGISSolver::addProblem(BooleanDagLightUtility *harness, const File *file){
    checker->addProblem(harness, file);
	problems.push_back(harness);
    files.push_back(file);

    {
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

        auto problemIn = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        for(int i=0; i<problemIn.size(); ++i){
            CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(problemIn[i]);
            if(!ctrlnode->get_Angelic() /*&& ctrlnode->getOtype() != OutType::FLOAT*/){
                declareControl(ctrlnode);
            }
        }

        if(new_clone) {
            inlined_harness->clear();
        }


    }

	finder->updateCtrlVarStore(ctrl_store);
}

CEGISSolver::~CEGISSolver(void)
{
    //assumption: all problems are already deleted
    problems.clear();
    //this is dead code:
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

    ctrl_store.newVar(cname, size, cnode->getOtype(), bool_node::CTRL, cnode->get_original_name(), cnode->get_source_dag_name());
}

CEGISSolverResult CEGISSolver::solve(
        const long long _budget, const long long find_solve_max_timeout_in_microseconds, const File* validation_file){

    AssertDebug(validation_file == nullptr, "NOT TESTED");
    auto timestep_init_solve = std::chrono::steady_clock::now();

    if(problems.empty()){
        return CEGISSolverResult{true, 0};
    }

    Assert(problem_stack_is_empty(), "A big bug");

    if(PARAMS->verbosity > 1){
        cout << "input_store.get_bit_size() = "<<checker->get_input_store().get_bit_size()<< endl;
        cout << "ctrl_store.get_bits_size() = " << ctrl_store.get_bit_size() << endl;
        cout<<"Random seeds = "<<params.nseeds<<endl;
    }

    ctrl_store.makeRandom();
    if(PARAMS->verbosity >= 1) {
        cout<<"!+";	ctrl_store.printBrief(cout); cout << endl;
    }

    int iterations = 0;
	bool fail = false;
    BooleanDAG* counterexample_concretized_dag = nullptr;
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

#ifdef CHECK_FILE_INVARIANT
    File *file = files[(int)files.size() - 1];
    if(file != nullptr) {
        assert(file->get_counterexample_ids_over_time().size() == 0);
    }
#endif

    int finder_step_id = 0;

    int num_counterexample_concretized_dags = 0;

    vector<HoleVarStore*> intermediate_solutions;
	while(doMore){
        cout << "step_id: " << finder_step_id << endl;
		// Verifier
		if(PARAMS->showControls){ print_control_map(cout); }

        { // Check
			if(PARAMS->verbosity > 5){ cout<<"!+ ";ctrl_store.printBrief(cout); cout << endl;}
			if(PARAMS->verbosity > 9){ cout<<"!+ ";ctrl_store.printContent(cout); cout << endl;}
                        std::vector<int, std::allocator<int> > ctrlstore_serialized = ctrl_store.serialize();
			if(PARAMS->verbosity > 1){ cout<<"BEG CHECK"<<endl; }
			ctimer.restart();

			if(!hc.solvePendingConstraints())
			{
                fail = true;
                break;
			};

            if(counterexample_concretized_dag != nullptr) {
                counterexample_concretized_dag->clear();
                counterexample_concretized_dag = nullptr;
            }

            // save intermediate result
            intermediate_solutions.push_back(new HoleVarStore(ctrl_store));

            // here perform validation test.
            if(validation_file != nullptr) {
                auto start_validation = chrono::steady_clock::now();
                auto dag = checker->get_main_problem()->produce_concretization(&ctrl_store, bool_node::CTRL);
                dag->increment_shared_ptr();
                int num_passing_inputs = dag->count_passing_inputs(validation_file);
                dag->clear();
                double p = (double) num_passing_inputs / (double) validation_file->size();
                cout << "validation_accuracy:\t" << num_passing_inputs << " / " << validation_file->size() << " (" << (int) (p*100) << " %)" << endl;
                cout << "time(validation): " << elapsed_time(start_validation) << " (us)" << endl;
            }

            auto start_check = chrono::steady_clock::now();
            counterexample_concretized_dag = checker->check(ctrl_store);
            cout << "time(check): " << elapsed_time(start_check) << " (us)" << endl;
			doMore = counterexample_concretized_dag != nullptr;
            cout << "doMore = " << doMore << endl;
            if(doMore)
            {
                num_counterexample_concretized_dags+=1;
            }

		 	ctimer.stop();
			if(PARAMS->verbosity > 1){ cout<<"END CHECK"<<endl; }			
		}
		if(PARAMS->verbosity > 2){cout<<"********  "<<iterations<<"\tftime= "<<ftimer.get_cur_ms() <<"\tctime= "<<ctimer.get_cur_ms()<<endl; }
		++iterations;
		if( params.iterlimit > 0 && iterations >= params.iterlimit){ cout<<" * bailing out due to iter limit"<<endl; fail = true; break; }

		// swap
		if(doMore) hasInputChanged = true;
		else hasInputChanged = false;
		
		if (!doMore) {//To make sure the control store also fleshes out the expressions from custom Synthesizers before being copied
			ctrl_store.finalizeSynthOutputs();
		}
		
		// Minimization loop-- found a solution, but can i find a better solution?
		if(PARAMS->minvarHole && !doMore && mhsizes.size() != 0){
			// store the current solution
			storePreviousSolution(checker->get_input_store(), ctrl_store);
			// optimization: only add inputs if the verification fails
			if(finder->minimizeHoleValue(ctrl_store, mhnames, mhsizes)){
				doMore=true;
            }
			else{
				doMore = false;
				checker->get_input_store() = prevInputStore;
                ctrl_store = prevCtrlStore;
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
                int nctrlbs = ctrl_store.get_bit_size();
                auto start_finder = chrono::steady_clock::now();
                if(counterexample_concretized_dag != nullptr) {
                    cout << "counterexample_concretized_dag != nullptr" << endl;
                    int dag_size = counterexample_concretized_dag->size();
                    SATSolverResult sat_solver_result_from_find =
                            finder->find(
                                    counterexample_concretized_dag, ctrl_store, hasInputChanged, find_solve_max_timeout_in_microseconds);
                    cout << "time(find): " << elapsed_time(start_finder) << " (us)" << endl;
                    assert(dag_size == counterexample_concretized_dag->size());
                    timestamp(start_finder,
                              "solve__f_" + std::to_string(counterexample_concretized_dag->get_dag_id_from_the_user()) +
                              "__step_" + std::to_string(finder_step_id) +
                              "__dagsz_" + std::to_string(dag_size) +
                              "__nctrlbs" + std::to_string(nctrlbs));
                    counterexample_concretized_dag->clear();
                    counterexample_concretized_dag = nullptr;

                    switch (sat_solver_result_from_find) {
                        case UNSPECIFIED:
                            assert(false);
                            break;
                        case SAT_UNDETERMINED:
                            assert(false);
                            doMore = false;
                            break;
                        case SAT_UNSATISFIABLE:
                            cout << "SAT_UNSATISFIABLE" << endl;
                            doMore = false;
                            break;
                        case SAT_SATISFIABLE:
                            cout << "SAT_SATISFIABLE" << endl;
                            doMore = true;
                            break;
                        case SAT_TIME_OUT:
                            cout << "SAT_TIME_OUT" << endl;
                            doMore = false; //play with this
                            break;
                        case SAT_MEM_OUT:
                            cout << "SAT_MEM_OUT" << endl;
                            doMore = false;
                            break;
                        case SAT_ABORTED:
                            cout << "SAT_ABORTED" << endl;
                            doMore = false;
                            break;
                        default:
                            assert(false);
                            break;
                    }
                }
                else
                {
                    cout << "counterexample_concretized_dag == nullptr" << endl;
                    SATSolverResult sat_solver_result_from_find =
                            finder->find(
                                    counterexample_concretized_dag, ctrl_store, hasInputChanged, find_solve_max_timeout_in_microseconds);
                    cout << "time(find): " << elapsed_time(start_finder) << " (us)" << endl;
                    timestamp(start_finder,
                              "solve__dag_null__step_" + std::to_string(finder_step_id) +
                              "__nctrlbs" + std::to_string(nctrlbs));

                    switch (sat_solver_result_from_find) {
                        case UNSPECIFIED:
                            assert(false);
                            break;
                        case SAT_UNDETERMINED:
                            assert(false);
                            doMore = false;
                            break;
                        case SAT_UNSATISFIABLE:
                            cout << "SAT_UNSATISFIABLE" << endl;
                            doMore = false;
                            break;
                        case SAT_SATISFIABLE:
                            cout << "SAT_SATISFIABLE" << endl;
                            doMore = true;
                            break;
                        case SAT_TIME_OUT:
                            cout << "SAT_TIME_OUT" << endl;
                            doMore = false; //play with this
                            break;
                        case SAT_MEM_OUT:
                            cout << "SAT_MEM_OUT" << endl;
                            doMore = false;
                            break;
                        case SAT_ABORTED:
                            cout << "SAT_ABORTED" << endl;
                            doMore = false;
                            break;
                        default:
                            assert(false);
                            break;
                    }

                }
                #ifdef CHECK_FILE_INVARIANT
                {
                    File *file = files[(int)files.size() - 1];

                    if (doMore) {

                        ProgramEnvironment *env = checker->getProblem()->get_env();

                        BooleanDAG *to_concretize = finder->get_all_inputs_dag()->clone();
                        env->doInline(*to_concretize, ctrlStore, bool_node::CTRL);

                        assert(to_concretize->get_failed_assert() == nullptr);
                        to_concretize->clear();

                        //check that all inputs used from the file pass on the the checker's harness
                        if (file != nullptr) {
                            BooleanDagLightUtility *concretized_function =
                                    checker->getProblem()->produce_concretization(
                                            &ctrlStore, bool_node::CTRL);
                            concretized_function->increment_shared_ptr();
                            BooleanDAG *concretized_dag = concretized_function->get_dag();
                            assert(concretized_dag->get_failed_assert() == nullptr);
                            map<string, BooleanDAG *> empty;
                            CounterexampleFinder eval(empty, *concretized_dag, params.sparseArray, floats);
                            VarStore &tmpin = checker->get_input_store();
                            eval.init(tmpin);
                            File *file = files[(int) files.size() - 1];
                            assert(eval.check_file_invariant(file));
//                        cout << "FILE PASSES OK (in CEGIS Slver)!!" << endl;
                            concretized_function->clear();
                        }

                    } else {

                        BooleanDagLightUtility *harness = checker->getProblem();

                        BooleanDAG *the_dag = finder->get_all_inputs_dag()->clone();
                        harness->get_env()->doInline(
                                *the_dag, ctrlStore, bool_node::CTRL);


                        assert(the_dag->get_failed_assert() != nullptr);
                        the_dag->clear();

                        if (file != nullptr) {
                            BooleanDagLightUtility *concretized_function =
                                    checker->getProblem()->produce_concretization(&ctrlStore, bool_node::CTRL);
                            concretized_function->increment_shared_ptr();
                            if (concretized_function->get_dag()->get_failed_assert() != nullptr) {
//                            cout << "FILE FAILS OK!! (1)" << endl;
                            } else {
                                BooleanDAG *concretized_dag = concretized_function->get_dag();
                                map<string, BooleanDAG *> empty;
                                CounterexampleFinder eval(empty, *concretized_dag, params.sparseArray, floats);
                                VarStore &tmpin = checker->get_input_store();
                                eval.init(tmpin);
                                File *file = files[(int) files.size() - 1];
                                assert(!eval.check_file_invariant(file));
                                auto tmp_dag = checker->getProblem()->produce_concretization(&ctrlStore,
                                                                                             bool_node::CTRL);
                                tmp_dag->increment_shared_ptr();
                                int num_passing_inputs = tmp_dag->count_passing_inputs(file);
                                tmp_dag->clear();
                                assert(num_passing_inputs < file->size());
//                            cout << "FILE FAILS OK!! (2)" << endl;
                            }
                            concretized_function->clear();
                        }
                    }
                }
                #endif
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
                ctrl_store = prevCtrlStore;
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
    last_elapsed_time = ElapsedTime(ftimer.get_tot_ms(), ctimer.get_tot_ms(), ttimer.get_tot_ms());
    cout << "*" << last_elapsed_time.to_string() << endl;
	finder->retractAssumptions();

    checker->clear_problem_stack();

    assert(checker->problem_stack_is_empty());

    if(counterexample_concretized_dag != nullptr) {
        counterexample_concretized_dag->clear();
        counterexample_concretized_dag = nullptr;
    }

    if(finder->get_all_inputs_dag() != nullptr) {
        finder->get_all_inputs_dag()->clear();
        finder->get_all_inputs_dag() = nullptr;
    }
    else
    {
        assert(iterations == 1);
    }
    cout << "returning from CEGISSolver::solve" << endl;
	return CEGISSolverResult{!fail, new HoleVarStore(ctrl_store), intermediate_solutions};
}

void CEGISSolver::getMinVarHoleNode(vector<string>& mhnames, vector<int>& mhsizes){
	map<string, CTRL_node*> minimizes;
	for(size_t i=0; i<problems.size(); ++i){
		const BooleanDAG* dag = problems[i]->get_dag();
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

void BitSet::print(ostream& os){
    int i=next(-1);
    os<<"{";
    while(i != -1){
        os<<", "<<i;
        i = next(i);
    }
    os<<"}"<<endl;

}

/*void CEGISSolver::normalizeInputStore(){
	VarStore tmp = old_join(checker->get_input_store(), ctrlStore);
	map<string, BooleanDAG*> empty;
	NodeSlicer slicer(empty, tmp, *getProblemDAG(), floats);
	slicer.process(*getProblemDAG());
	for(auto it = checker->get_input_store().begin(); it != checker->get_input_store().end(); ++it){
		if(!slicer.isInfluential(it->get_name())){
			it->setVal(last_input[it->get_name()]);
		}
	}
}*/

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

void CEGISSolver::get_control_map_as_map_str_str(map<string, string>& values){
    values = ctrl_store.to_map_str_str(floats);
}

// can remove
//void CEGISSolver::outputEuclid(ostream& fout){
//		cout<<"BEFORE OUTPUTING STATE"<<endl;
//		{
//			NodesToEuclid neuc(fout, "PROBLEM_");
//			neuc.process(*getProblemDAG());
//		}
//	}

void CEGISSolver::setup2QBF(ofstream& out){
	MiniSATSolver mngCheck("checker", SATSolver::CHECKER);		
	SolverHelper dirCheck(mngCheck);
	dirCheck.setMemo(params.setMemo);
	BooleanDAG* bd = problems[problems.size()-1]->get_dag__non_const();
	out<<"c 2qbf problem of the form. \\forall C \\exists In, temp.  (not Correct(C,in, temp)) "<<endl;
	out<<"c vars listed as UV are the C vars; vars listed as EV are the In vars; vars not listed are temps."<<endl;

	{
		auto inter = bd->getNodesByType(bool_node::CTRL);
		for(auto node_it = inter.begin(); node_it != inter.end(); ++node_it){
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
		auto inter = bd->getNodesByType(bool_node::SRC);
		for(auto node_it = inter.begin(); node_it != inter.end(); ++node_it){
			INTER_node* srcnode = dynamic_cast<INTER_node*>(*node_it);										
			dirCheck.declareInArr(srcnode->get_name(), srcnode->get_nbits(), srcnode->getOtype(), bool_node::SRC, srcnode->get_name(), "setup2QBF()");
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
