#pragma once

#include "BooleanDAG.h"
#include "DagOptim.h"

class BooleanDAGCreator
{
	StringHTable2<bool_node*> named_nodes;
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

	
  virtual bool_node* create_const(int n);
  virtual bool_node* create_const(double n);

  INTER_node* create_inputs(int n, OutType* type, const string& gen_name=string("INPUT"), int arrSz=-1, int tupDepth = -1);
  INTER_node* create_controls(int n, const string& gen_name=string("CONTROL"), bool toMinimize = false, bool angelic = false, bool spConcretize = false, int max = -1);

  /**
	Creates an N-bit output named gen_name, connected to internal node nodeToOutput.
  */
  INTER_node* create_outputs(int n, bool_node* nodeToOutput, const string& gen_name=string("OUTPUT"));
  INTER_node* create_outputs(int n, const string& gen_name=string("OUTPUT"));

  /** Produces a node called name.
  */
  bool_node* get_node(const string& name);  



  /* Associates the name source to the node  starg.
  */
  void alias(const string& ssource,  bool_node* starg);
  /* Only for tuple_r */
    bool_node* new_node(bool_node* mother,
                        int idx);

  /**
	Create a new node from the string names of its parents.
  */
  bool_node* new_node(bool_node* mother, 
                      bool_node* father, bool_node::Type t);

  /* Used during initialization when creating ARITH nodes and ASSERT nodes. thenode will always be an ARITH node or an ASSERT node.	
      The node thenode should not be used after calls to this function, because this function may optimize it 
	  away and deallocate it; so one must be very very careful to always use the return value instead.
	  Also, all the fields of the arith node should already be initialized. Otherwise, you'll get wrong behavior.
	*/
  bool_node* new_node(bool_node* mother, 
                      bool_node* father, bool_node* thenode);

  /*-------------------------------------*/

  bool_node* optimizeAndAdd(bool_node* node);
  /*
  bool has_alias(const string& s){
	  named_nodes.contai
    return named_nodes.find(s) != named_nodes.end();
  }
  */
  bool_node* get_alias(const string& s){
    return get_node(s);
  }
  

  string new_name(){
	 int p = 0;
	char tmpbuf[200];
	tmpbuf[p] = 't'; p++;
	writeInt(tmpbuf, new_names, p);
	++new_names;
	tmpbuf[p] = 0;
    return tmpbuf;
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


 void finalize();

};
