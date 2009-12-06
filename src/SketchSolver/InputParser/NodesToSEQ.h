#pragma once
#include "NodesToSolver.h"
#include "BLIFwriter.h"
#include <typeinfo>

class NextState{
public:
	Tvalue cond;
	vector<Tvalue> state;
};




/**
	The purpose of this class is to produce a sequential circuit for Alan. 
	It assumes that it is operating on a circuit that calls itself recursively, 
	and that the circuit doesn't have any output (aside from the assertions), 
	so if there is a DST node, it should be set to constant 1.


*/
class NodesToSEQ :
	public NodesToSolver
{
	vector<NextState> nextStates;
	vector<Tvalue> input;
	ostream& output;
	BLIFwriter& writer;
	BooleanDAG& dag;
public:
	 NodesToSEQ (
		 ostream& p_out,
		 BLIFwriter& p_writer,
	     SolverHelper& p_dir, 
	     const string& p_outname, 
		 map<bool_node*,  int>& p_node_values, 
		 vector<Tvalue>& p_node_ids,
		 BooleanDAG& p_dag
		 ) :NodesToSolver(p_dir, p_outname, p_node_values, p_node_ids), output(p_out), writer(p_writer), dag(p_dag){
	 }
	int intVal(bool_node* n){
		if(typeid(*n) == typeid(CONST_node)){
			return dynamic_cast<CONST_node*>(n)->getVal();
		}else{
			return 0;
			//Assert(false, "This can't happen, we currently only accept constant initial state "<<endl);
		}
	}
public:
	virtual ~NodesToSEQ(void);
	virtual void visit (UFUN_node &node);
	virtual void visit (SRC_node &node);
	virtual void complete(vector<bool_node*>& initState);
};
