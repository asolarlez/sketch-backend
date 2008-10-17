#pragma once

#include "BooleanDAG.h"
#include "DagOptim.h"

class BooleanDAGCreator
{
	map<string, bool_node*> named_nodes;
	map<string, string > aliasmap;
	int new_names;
	int new_namesb;

	BooleanDAG* const dag;
	DagOptim optim;
    void getMotherFather(const string& mother, 
					 const string& father, /*OUT*/bool_node*& mth, /*OUT*/bool_node*& fth);

public:
	
	BooleanDAGCreator(BooleanDAG* p_dag);
public:
	virtual ~BooleanDAGCreator(void);

	
  virtual string create_const(int n);

  bool_node* create_inputs(int n, const string& gen_name=string("INPUT"));
  bool_node* create_controls(int n, const string& gen_name=string("CONTROL"));

  /**
	Creates an N-bit output named gen_name, connected to internal node nodeToOutput.
  */
  bool_node* create_outputs(int n, bool_node* nodeToOutput, const string& gen_name=string("OUTPUT"));
  bool_node* create_outputs(int n, const string& gen_name=string("OUTPUT"));
  

  /** Produces a node called name.
  */
  bool_node* get_node(const string& name);  



  /* Associates the name source to the node associated with the name starg.
  */
  void alias(const string& ssource,  const string& starg);

  /**
	Create a new node from the string names of its parents.
  */
  bool_node* new_node(const string& mother, 
                      const string& father, bool_node::Type t, const string& name);

  /* Used during initialization when creating ARITH nodes and ASSERT nodes. thenode will always be an ARITH node or an ASSERT node.	
      The node thenode should not be used after calls to this function, because this function may optimize it 
	  away and deallocate it; so one must be very very careful to always use the return value instead.
	  Also, all the fields of the arith node should already be initialized. Otherwise, you'll get wrong behavior.
	*/
  bool_node* new_node(const string& mother, 
                      const string& father, bool_node* thenode);

  /*-------------------------------------*/

  bool_node* optimizeAndAdd(bool_node* node);
  
  bool has_alias(const string& s){
    return aliasmap.find(s) != aliasmap.end();
  }

  string get_alias(const string& s){
    return aliasmap[s];
  }
  

  string new_name(){
    stringstream str;
    str<<"t"<<hex<<new_names<<"_";
    ++new_names;
    return str.str();
  }
 string new_name(string& base){
 	if(base.length() < 2){
 		return new_name();	
 	}else{
	    stringstream str;
	    str<<base.substr(0,4)<<hex<<"_"<<new_names<<"__";
	    ++new_names;
	    return str.str();
 	}
  }


 void finalize(){
	 optim.cleanup(*dag);
 }

};