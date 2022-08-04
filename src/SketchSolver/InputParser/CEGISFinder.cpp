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
#include "CEGISFinder.h"


int CEGISsolveCount=0;


SATSolverResult CEGISFinder::find(
        BooleanDAG* problem,
        VarStore& controls,
        bool hasInputChanged,
        unsigned long long max_finder_solve_timeout_in_microseconds){
	// the caller expects find to keep track of all the constraints.
	// here dirfind is doing that.

    if(hasInputChanged)
    {
        BooleanDAG* newdag = problem->clone();
        if(allInputsDag == nullptr)
        {
            assert(newdag != nullptr);
            allInputsDag = newdag;
        }
        else
        {
            allInputsDag->andDag(newdag);
            DagOptim dag_optim(*allInputsDag, floats);
            dag_optim.process(*allInputsDag);
        }
    }
    else
    {
        //claim: solution is already optimal.
        AssertDebug(false, "INVESTIGATE");
        return SAT_SATISFIABLE;
    }

	//hasInputChange == is it a new problem;
	if(hasInputChanged){
		timerclass tc("* TIME TO ADD INPUT ");
		tc.start();
        addProblemToTestSet(problem);
//		addInputsToTestSet(problem, input);
		tc.stop();
		if(PARAMS->verbosity > 2){ tc.print(); }
	}

	//Solve
	cout << "in bool CEGISFinder::find(BooleanDAG* problem, VarStore& controls, bool hasInputChanged) in CEGISFinder.cpp" << endl;
    SATSolverResult result = mngFind.solve(max_finder_solve_timeout_in_microseconds);
    cout << "out bool CEGISFinder::find(BooleanDAG* problem, VarStore& controls, bool hasInputChanged)  in CEGISFinder.cpp " << endl;

	if(PARAMS->outputSat){
		++CEGISsolveCount;
		stringstream str;
		str<<"sat_SYN_"<<CEGISsolveCount<<".cnf";
		cout<<"Creating SAT file "<<str.str()<<endl;
		ofstream file( str.str().c_str() );
		this->dirFind.writeDIMACS(file);		
	}

	if(params.printDiag){
        mngFind.printDiagnostics('f');
	}

    if (result != SAT_TIME_OUT && result != SAT_SATISFIABLE){ 	//If solve is bad, return false.
    	if( result != SAT_UNSATISFIABLE){
	    	switch( result ){
			    case SAT_UNDETERMINED: {
					if (params.lightVerif) {
						return result;
					}
					else {
						throw new SolverException(result, "SAT_UNDETERMINED"); break;
					}
				}
	    		case SAT_TIME_OUT:  throw new SolverException(result, "SAT_TIME_OUT"); break;
	    		case SAT_MEM_OUT:  throw new SolverException(result, "SAT_MEM_OUT"); break;
	    		case SAT_ABORTED:  throw new SolverException(result, "SAT_ABORTED"); break;

                default:
                    Assert(false, "MISSING CASE in CEGISFinder::find.");
	    	}
    	}
		if(this->stoppedEarly){
			cout<<dirFind.lastErrMsg<<endl;
		}
    	return result;
    }
	Dout( dirFind.print() );
	//dirFind.printAllVars();
	//Get the values of the Controls.6-

	for(auto it = controls.begin(); it !=controls.end(); ++it){
		const string& cname = it->get_name();
		int cnt = dirFind.getArrSize(cname);
		Assert( cnt == it->element_size(), "find: SIZE MISMATCH: "<<cnt<<" != "<<it->element_size()<<endl);
		for(int i=0; i<cnt; ++i){
			int val = mngFind.getVarVal(dirFind.getArr(cname, i));
			it->set_bit(i, (val==1) ? 1 : 0);
		}
	}

	controls.synths.clear();
	auto end = dirFind.get_sins().end();
	for (auto it = dirFind.get_sins().begin(); it != end; ++it) {
		controls.synths[it->first] = it->second;
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
	return result;
}

void CEGISFinder::addProblemToTestSet(BooleanDAG* newdag)
{
    map<bool_node*,  int> node_values;
    if(PARAMS->verbosity > 2){  cout<<" intsize = "<< newdag->getIntSize()<<endl; }

    if(PARAMS->verbosity > 6){ cout<<" * After all optims it became = "<<newdag->size()<<endl; }
    // find_node_ids store the mapping between node in the DAG (miter) vs
    // the variables in the CNF.
    find_node_ids.resize(newdag->size());

    try{
        stoppedEarly = NodesToSolver::createConstraints(*newdag, dirFind, node_values, find_node_ids, floats);
    }catch(BasicError& e){
        dirFind.nextIteration();
        if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }


        find_node_ids.clear();

        AssertDebug(false, "error");
        throw e;
    }
    // Keeps the history around for debugging purposes.
    if( params.superChecks ){ find_history = find_node_ids; }
    dirFind.nextIteration();
    if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }

    find_node_ids.clear();
}

void
CEGISFinder::
addInputsToTestSet(BooleanDAG* problem, VarStore& input){
	map<bool_node*,  int> node_values;
	bool specialize = PARAMS->olevel >= 6;
	BooleanDAG* tmpproblem = NULL;	
	if(PARAMS->verbosity > 2){  cout<<" intsize = "<< problem->getIntSize()<<endl; }

    AssertDebug(false, "NEED TO USE THE NEW HARDCODER (The 2-in-1 inliner and hardcoder).")
	BooleanDAG* newdag = hardCodeINode(problem, input, bool_node::SRC, floats);
//	BackwardsAnalysis ba;
	// ba.process(*newdag);
	DagOptim fa(*newdag, floats);
	fa.process(*newdag);
		
	//cout << "addInputsToTestSet: newdag=";
	//newdag->lprint(cout);
	if(PARAMS->verbosity > 6){ cout<<" * After all optims it became = "<<newdag->size()<<endl; }	
	// find_node_ids store the mapping between node in the DAG (miter) vs
	// the variables in the CNF.
	find_node_ids.resize(newdag->size());
	//getProblemDag()->lprint(cout);
	
	//FindCheckSolver::addInputsToTestSet(input);
	//lastFproblem = newdag;	
	
	try{
		stoppedEarly = NodesToSolver::createConstraints(*newdag, dirFind, node_values, find_node_ids, floats);
	}catch(BasicError& e){
		dirFind.nextIteration();
		if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
		
		
		find_node_ids.clear();
		
		delete newdag;
		throw e;
	}
	// Keeps the history around for debugging purposes.	
	if( params.superChecks ){ find_history = find_node_ids; }
	dirFind.nextIteration();
	if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
	
		
	find_node_ids.clear();
	delete newdag;
}

bool CEGISFinder::minimizeHoleValue(VarStore& ctrlStore, vector<string>& mhnames, vector<int>& mhsizes){
	cout << "*********INSIDE minimizeHoleValue, " << "mhsize=" << mhsizes.size() << " current value of ";
	dirFind.getMng().retractAssumptions();
	bool isSingleMinHole = (mhsizes.size()==1);
	vector<int> bigor; bigor.push_back(0);
	for(size_t i=0; i<mhsizes.size(); ++i){
		string& locminVarNodeName = mhnames[i];
		int minVarNodeSize = mhsizes[i];
		int H__0_val = ctrlStore.getObjConst(locminVarNodeName).getInt();
		int H__0_var_idx = dirFind.getVar(locminVarNodeName);
		cout <<locminVarNodeName<<"=" << H__0_val << ", " << flush;
		Tvalue tv = H__0_var_idx;		
		tv.makeSparse(dirFind, minVarNodeSize);
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
			if (minVarNodeSize > 5 && isSingleMinHole) {
				Tvalue cnst;
				cnst.makeIntVal(dirFind.YES, H__0_val);
				dirFind.intClause(cnst);
				dirFind.intClause(tv);
				dirFind.addRetractableAssertClause(dirFind.intlt(tv.getId(), cnst.getId()));
			}
		}		
		catch(BasicError& be){
			return false;
		}
	}
	if(!isSingleMinHole){
		dirFind.addBigOrClause(&bigor[0], bigor.size()-1);
		try{
			dirFind.addRetractableAssertClause(bigor[0]);
		}
		catch(BasicError& be){
			return false;
		}
	}
	if (params.lightVerif) {
		dirFind.getMng().lightSolve();
	}
	
	return true; //doMore
}
