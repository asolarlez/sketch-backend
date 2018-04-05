#include "REASSolver.h"
#include "timerclass.h"
#include <queue>
#include "CommandLineArgs.h"
#include "Tvalue.h"
#include "DagOptim.h"
#include "MiniSATSolver.h"
#include "BoolAbstractor.h"


//extern CommandLineArgs* PARAMS;

using namespace MSsolverNS;


void REASSolver::addProblem(BooleanDAG* problem_){
    problem = problem_;
    problem->lprint(cout);
   
    Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
    {
        vector<bool_node*>& ctrls = problem->getNodesByType(bool_node::CTRL);
        if(PARAMS->verbosity > 2){
            cout<<"  # OF CONTROLS:    "<< ctrls.size() <<endl;
        }
        int cints = 0;
        int cbits = 0;
        int cfloats = 0;
        for(int i=0; i<ctrls.size(); ++i){
            CTRL_node* ctrlnode = dynamic_cast<CTRL_node*>(ctrls[i]);
            int nbits = ctrlnode->get_nbits();
            if(ctrlnode->getOtype() == OutType::BOOL){
                cbits++;
            } else if (ctrlnode->getOtype() == OutType::FLOAT) {
                cfloats++;
            } else{
                cints++;
            }
            if(!ctrlnode->get_Angelic() && ctrlnode->getOtype() != OutType::FLOAT){
                /* cout<<" i ="<<i<<"\t"<<problemIn[i]->get_name()<<endl; */
                
                declareControl(ctrlnode);
            }
            if (ctrlnode->spAngelic) {
                Assert(false, "NYI: angelic ctrls not supported in REAS");
            }
            
        }
        if(PARAMS->verbosity > 2){
            cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
        }
    }
    for(map<string, int>::const_iterator it = dirFind.arrsize_begin(); it != dirFind.arrsize_end(); ++it){
        if(!ctrlStore.contains(it->first)){
            ctrlStore.newVar(it->first, it->second);
        }
    }
}


REASSolver::REASSolver(SolverHelper& finder, FloatManager& _floats):
dirFind(finder),
mngFind(finder.getMng()),
floats(_floats)
{
    //	cout << "miter:" << endl;
    //	miter->lprint(cout);
    
}



REASSolver::~REASSolver(void)
{
    delete problem;
}


void REASSolver::declareControl(CTRL_node* cnode){
    const string& cname = cnode->get_name();
    int size = cnode->get_nbits();
    Dout(cout<<"DECLARING CONTROL "<<cname<<" "<<size<<endl);
    dirFind.declareControl(cnode);
    
    if(!ctrlStore.contains(cname)){
        ctrlStore.newVar(cname, size);
    }
}

bool REASSolver::solve(){
    bool succeeded = solveOptimization();
    return succeeded;
}

bool REASSolver::solveOptimization() {
    timerclass ftimer("* FIND TIME");
    timerclass ttimer("* TOTAL TIME");
    ttimer.start();
    
    if(PARAMS->verbosity > 2){ cout<<"BEG FIND"<<endl; }
    ftimer.restart();
    bool fail = false;
    try{
        fail = !find(ctrlStore);
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

bool REASSolver::find(VarStore& controls){
    
   timerclass tc("* TIME TO ADD INPUT ");
    tc.start();
    Interface* interface = new Interface(problem->size());
    dirFind.createNumericalSynthesizer(floats, problem, interface);
    createBooleanAbstraction(interface);
    tc.stop();
    if(PARAMS->verbosity > 2){ tc.print(); }
    //Solve
    
    int result = mngFind.solve();
    
    if(PARAMS->printDiag){
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
        if(this->stoppedEarly){
            cerr<<dirFind.lastErrMsg<<endl;
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
        Assert( cnt == it->size(), "find: SIZE MISMATCH: "<<cnt<<" != "<<it->size()<<endl);
        for(int i=0; i<cnt; ++i){
            int val = mngFind.getVarVal(dirFind.getArr(cname, i));
            it->setBit(i, (val==1) ? 1 : 0);
        }
    }
    
    controls.synths.clear();
    auto end = dirFind.get_sins().end();
    for (auto it = dirFind.get_sins().begin(); it != end; ++it) {
        controls.synths[it->first] = it->second;
    }

    mngFind.reset();
    return true;
}

void REASSolver::createBooleanAbstraction(Interface* interface){
    node_ids.resize(problem->size());
    try{
        stoppedEarly = BoolAbstractor::createConstraints(*problem, dirFind, node_values, node_ids, floats, interface);
    }catch(BasicError& e){
        dirFind.nextIteration();
        if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
        throw e;
    }
    // Keeps the history around for debugging purposes.
    dirFind.nextIteration();
    if(PARAMS->verbosity>7){ cout<<" finder "; dirFind.getStats(); }
    node_ids.clear();
}

void REASSolver::printDiagnostics(SATSolver& mng, char c){
    mng.printDiagnostics(c);
}

void REASSolver::print_control_map(ostream& out){
    map<string, string> values;
    get_control_map(values);
    for(auto it = values.begin(); it != values.end(); ++it){
        out<<it->first<<"\t"<<it->second<<endl;
    }
}


void REASSolver::get_control_map(map<string, string>& values){
    for(VarStore::iterator it = ctrlStore.begin(); it !=ctrlStore.end(); ++it){
        stringstream str;
        str << it->getInt();
        values[it->name] = str.str();
    }
    for (auto it = ctrlStore.synths.begin(); it != ctrlStore.synths.end(); ++it) {
        //stringstream str;
        it->second->getControls(values);
        //values[it->first] = str.str();
    }
}
