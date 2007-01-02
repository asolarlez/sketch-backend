// BooleanDAG.h: interface for the BooleanDAG class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)
#define AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include <string>
#include <list>
#include <map>
#include "BasicError.h"


using namespace std;
class NodeVisitor;

class bool_node{
protected:
  bool_node():mother(NULL),  father(NULL), flag(0), id(-1), ion_pos(0), otype(BOTTOM){};	
public:
  string name;
  int layer;
  int id;
  int flag;
  int ion_pos;
  typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL, ARITH, ASSERT} Type;
  typedef enum{INT, BOOL, BOTTOM} OutType;
  Type type;
  OutType otype;
  bool_node* mother;  
  bool_node* father;
  
  vector<bool_node*> children;
  
  
  virtual int do_dfs(int idx);
  virtual int back_dfs(int idx);
  virtual void remove_child(bool_node* bn);
  virtual void dislodge();
  virtual string get_tname(){
    switch(type){
    case AND: return "AND";
    case OR: return "OR";
    case XOR: return "XOR";
    case SRC: return "S";
    case DST: return "D";
    case NOT: return "I";
    case CTRL: return "CTRL";
    case ASSERT: return "ASSERT";
    }
    cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
    throw BasicError("Err", "Err");
  }
  string get_name(){
    stringstream str;
    if(name.size() > 0)
      str<<name<<"__"<<get_tname();
    else{      
      str<<"name_"<<abs(id)<<"_"<<this<<"__"<<get_tname();
      
    }
    return str.str();
  }
  void set_layer();
  virtual void accept(NodeVisitor& visitor)=0;
};





class arith_node: public bool_node{
	protected:
	arith_node():bool_node(){ type = ARITH; };
	public:
    typedef enum {
	PLUS, TIMES, ARRACC, DIV, MOD, NEG, CONST, GT, GE, LT, LE, EQ, ARRASS, ACTRL
    } AType;
		
	AType arith_type;
	vector<bool_node*> multi_mother;
	
    virtual int back_dfs(int idx);
	virtual void dislodge();
	virtual string get_tname(){
		switch(arith_type){
			case PLUS: return "PLUS";
			case TIMES: return "TIMES";
			case DIV: return "DIV";
			case MOD: return "MOD";
			case NEG: return "NEG";
			case CONST: return "CONST";
			case ARRACC: return "ARRACC";
			case GT: return "GT";
			case GE: return "GE";
			case LT: return "LT";
			case LE: return "LE";
			case ACTRL: return "ACTRL";
			case ARRASS: return "ARRASS";
			case EQ: return "EQ";
		}
		return "null";
	}
};


class AND_node;
class OR_node;
class XOR_node;
class SRC_node;
class DST_node;
class NOT_node;
class CTRL_node;
class PLUS_node;
class TIMES_node;
class ARRACC_node;
class DIV_node;
class MOD_node;
class NEG_node;
class CONST_node;
class GT_node;
class GE_node;
class LT_node;
class LE_node;
class EQ_node;
class ARRASS_node;
class ACTRL_node;
class ASSERT_node;
class BooleanDAG;

class NodeVisitor{
	public:
	virtual void visit( AND_node& node )=0;
	virtual void visit( OR_node& node )=0;
	virtual void visit( XOR_node& node )=0;
	virtual void visit( SRC_node& node )=0;
	virtual void visit( DST_node& node )=0;
	virtual void visit( NOT_node& node )=0;
	virtual void visit( CTRL_node& node )=0;
	virtual void visit( PLUS_node& node )=0;
	virtual void visit( TIMES_node& node )=0;
	virtual void visit( ARRACC_node& node )=0;
	virtual void visit( DIV_node& node )=0;
	virtual void visit( MOD_node& node )=0;
	virtual void visit( NEG_node& node )=0;
	virtual void visit( CONST_node& node )=0;
	virtual void visit( GT_node& node )=0;
	virtual void visit( GE_node& node )=0;
	virtual void visit( LT_node& node )=0;
	virtual void visit( LE_node& node )=0;
	virtual void visit( EQ_node& node )=0;
	virtual void visit( ARRASS_node& node )=0;
	virtual void visit( ACTRL_node& node )=0;
	virtual void visit( ASSERT_node &node) = 0;	
	virtual void process(BooleanDAG& bdag);
};



class AND_node: public bool_node{ 	
	public: 
		AND_node(){ type = AND; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	};
class OR_node: public bool_node{	
	public: 
		OR_node(){ type = OR; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	};
class XOR_node: public bool_node{	
	public: 
		XOR_node(){ type = XOR; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	};

class INTER_node: public bool_node{	
	protected: 
		INTER_node(){nbits = 1;}
		int nbits;
	public:	
	int get_nbits() const { return nbits; }
	void set_nbits(int n){ nbits = n; }
};


class SRC_node: public INTER_node{		
	public: SRC_node(){ type = SRC; }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};

class DST_node: public INTER_node{		
	public: 
		DST_node(){ type = DST; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};


class CTRL_node: public INTER_node{
	public: 
	CTRL_node(){ type = CTRL;}  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }	
};

class NOT_node: public bool_node{	
	public: 
	NOT_node(){ type = NOT; }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};

class PLUS_node: public arith_node{	
	public: PLUS_node(){ arith_type = PLUS; } 
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class TIMES_node: public arith_node{	
	public: TIMES_node(){ arith_type = TIMES; }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class ARRACC_node: public arith_node{	
	public: ARRACC_node(){ arith_type = ARRACC; }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class DIV_node: public arith_node{	
	public: 
		DIV_node(){ arith_type = DIV; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class MOD_node: public arith_node{	
	
	public: 
		MOD_node(){ arith_type = MOD; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};


class NEG_node: public arith_node{	
	
	public: 
		NEG_node(){ arith_type = NEG; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};


class CONST_node: public arith_node{
	int val;
	public:
		CONST_node(){ arith_type = CONST; val = -1;}
		CONST_node(int n){ arith_type = CONST; val = n;}
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual void setVal(int v){ val = v; }
		virtual int getVal(){ return val; }
};

class GT_node: public arith_node{	
	public: 
		GT_node(){ arith_type = GT; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class GE_node: public arith_node{	
	public: 
		GE_node(){ arith_type = GE; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class LT_node: public arith_node{	
	public: 
		LT_node(){ arith_type = LT; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class LE_node: public arith_node{	
	public: 
		LE_node(){ arith_type = LE; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class EQ_node: public arith_node{	
	public: 
		EQ_node(){ arith_type = EQ; } 
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }		 
};
class ARRASS_node: public arith_node{		
	public: 
		int quant;
		ARRASS_node(){ arith_type = ARRASS; quant = -1; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
};
class ACTRL_node: public arith_node{	
	public: ACTRL_node(){ arith_type = ACTRL; }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }	
};
class ASSERT_node: public bool_node {
public:
    ASSERT_node () { type = ASSERT; }
    virtual void accept (NodeVisitor &visitor) { visitor.visit (*this); }
};



inline bool_node* newBoolNode( bool_node::Type type){
	switch(type){
		case bool_node::AND: return new AND_node();
		case bool_node::OR: return new OR_node();
		case bool_node::XOR: return new XOR_node();
		case bool_node::SRC: return new SRC_node();
		case bool_node::DST: return new DST_node();
		case bool_node::NOT: return new NOT_node();
		case bool_node::CTRL: return new CTRL_node();
		case bool_node::ARITH: Assert( false, "This should not happen");		
		case bool_node::ASSERT: return new ASSERT_node ();
	}
	return NULL;
}

inline arith_node* newArithNode( arith_node::AType type){
	switch(type){
		case arith_node::PLUS: return new PLUS_node();
		case arith_node::TIMES: return new TIMES_node();
		case arith_node::ARRACC: return new ARRACC_node();
		case arith_node::DIV: return new DIV_node();
		case arith_node::MOD: return new MOD_node();
		case arith_node::NEG: return new  NEG_node();
		case arith_node::CONST: return new CONST_node();
		case arith_node::GT: return new GT_node();
		case arith_node::GE: return new GE_node();
		case arith_node::LT: return new LT_node();
		case arith_node::LE: return new LE_node();
		case arith_node::EQ: return new EQ_node();
		case arith_node::ARRASS: return new ARRASS_node();
		case arith_node::ACTRL: return new ACTRL_node();		
	}
	return NULL;
}


inline bool comp_id(bool_node* n1, bool_node* n2){
  if(n1->id == -1){
    return false;
  }
  if(n2->id == -1){ //& n1->id != -1
    return n1->id != n2->id;
  }
  return n1->id < n2->id;
}


inline bool comp_layer(bool_node* n1, bool_node* n2){
  return n1->layer < n2->layer || ( (n1->layer == n2->layer) && n1->ion_pos<n2->ion_pos );
}


class BooleanDAG  
{
  int n_inputs;
  int n_outputs;
  int n_controls;
  int new_names;

  vector<bool_node*> nodes;
  vector<int> layer_sizes;
  map<string, bool_node*> named_nodes;
  map<string, string > aliasmap;
  map<bool_node::Type, vector<bool_node*> > nodesByType;

  

  bool is_sorted; //The sorted property implies that everyone comes after their parents
  bool is_layered;  //The layered property implies that nodes are sorted by layer. is_layered implies is_sorted.
  bool has_passthrough; //This property implies that NOT nodes have been added.

  void compute_layer_sizes();
  void removeFromChildren(bool_node* parent, bool_node* toremove);
  void remove(int i);
public:
  void print(ostream& out);
  typedef vector<bool_node*>::iterator iterator;
  void removeNullNodes();
  void replace(int i, bool_node* cse);
  
  virtual string create_const(int n);
  void create_inter(int n, const string& gen_name, int& counter,  bool_node::Type type);
  void create_inputs(int n, const string& gen_name=string("INPUT"));
  int create_controls(int n, const string& gen_name=string("CONTROL"));
  void create_outputs(int n, const string& gen_name=string("OUTPUT"));

  bool_node* new_node(const string& mother, 
                      const string& father, bool_node::Type t, const string& name);
  bool_node* new_node(bool_node* mother,  
                      bool_node* father,  bool_node::Type t);

  bool_node* new_node(const string& mother, 
                      const string& father, bool_node::Type t, const string& name, bool_node* thenode);
  bool_node* new_node(bool_node* mother,  
                      bool_node* father,  bool_node::Type t, bool_node* thenode);

  bool_node* set_node(bool_node* tmp, bool_node* mother, 
                      bool_node* father,  bool_node::Type t);

  void change_father(const string& father, const string& son);
  void change_mother(const string& father, const string& son);

  int get_n_inputs()const{ return n_inputs; };
  int get_n_outputs()const{ return n_outputs; };
  int get_n_controls()const{ return n_controls; };
  int get_n_layers()const{ return layer_sizes.size(); };


  vector<bool_node*>& getNodesByType(bool_node::Type t);
  const bool_node* operator[](int idx)const{ return nodes[idx]; };
  bool_node* operator[](int idx){ return nodes[idx]; };
  int size()const {return nodes.size();}
  int get_lsize(int layer){ return layer_sizes[layer];};
  void sort_graph();
  void layer_graph();  
  void relabel();
  void cleanup(bool moveNots=true);
  bool_node* get_node(const string& name);  
  iterator begin(){ return nodes.begin(); }
  iterator end(){ return nodes.end(); }
  void alias(const string& ssource,  const string& starg);

  bool has_alias(const string& s){
    return aliasmap.find(s) != aliasmap.end();
  }

  string get_alias(const string& s){
    return aliasmap[s];
  }

  string new_name(){
    stringstream str;
    str<<"TNM_"<<new_names<<"____";
    ++new_names;
    return str.str();
  }
 string new_name(string& base){
 	if(base.length() < 2){
 		return new_name();	
 	}else{
	    stringstream str;
	    str<<base.substr(0,4)<<new_names<<"____";
	    ++new_names;
	    return str.str();
 	}
  }
  
	BooleanDAG();
	virtual ~BooleanDAG();
};

#endif // !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)

