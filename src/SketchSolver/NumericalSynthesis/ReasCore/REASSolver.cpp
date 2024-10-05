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
    //problem->lprint(cout);
   
    Dout( cout<<"problem->get_n_controls() = "<<problem->get_n_controls()<<"  "<<problem<<endl );
    {
        auto ctrls = problem->getNodesByType(bool_node::CTRL);
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
            if (ctrlnode->spAngelic) {
                Assert(false, "NYI: angelic ctrls not supported in REAS");
            }
            
        }
        if(PARAMS->verbosity > 2){
            cout<<" control_ints = "<<cints<<" \t control_bits = "<<cbits<< " \t control_floats = " << cfloats <<endl;
        }
    }
}


REASSolver::REASSolver(FloatManager& _floats):
floats(_floats), problem(NULL)
{
    //	cout << "miter:" << endl;
    //	miter->lprint(cout);
    
}



REASSolver::~REASSolver(void)
{
    if (problem != NULL) {
        delete problem;
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
    return !fail;
}

bool REASSolver::find(VarStore& controls){
    
   timerclass tc("* TIME TO ADD INPUT ");
    tc.start();

    Interface* interf = new Interface(problem);
    createBooleanAbstraction(interf);

    ns = new NumericalSynthesizer(floats, problem, interf);
    tc.stop();
    if(PARAMS->verbosity > 2){ tc.print(); }
    //Solve
    
    bool result = ns->solve();
    return result;
}

void REASSolver::createBooleanAbstraction(Interface* interf){
    node_ids.resize(problem->size());
    try{
        stoppedEarly = BoolAbstractor::createConstraints(*problem, *(interf->satSolver), node_values, node_ids, floats, interf);
    }catch(BasicError& e){
        throw e;
    }
    node_ids.clear();
}

void REASSolver::print_control_map(ostream& out){
    map<string, string> values;
    get_control_map(values);
    for(auto it = values.begin(); it != values.end(); ++it){
        out<<it->first<<"\t"<<it->second<<endl;
    }
}


void REASSolver::get_control_map(map<string, string>& values){
    ns->getControls(values);
}

void REASSolver::get_control_map_and_floats(map<string, string>& values, map<string, float>& floats){
    ns->getControls_and_floats(values, floats);
}