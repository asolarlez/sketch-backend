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
#include <set>
#include "FastSet.h"
#include "BasicError.h"
#include "timerclass.h"
#include "NodeVisitor.h"


#ifdef CONST
#undef CONST
#endif


using namespace std;

class BooleanDAG;


//typedef set<bool_node*>::iterator child_iter;

/*
typedef set<bool_node*>::const_iterator child_citer;
typedef set<bool_node*>::iterator child_iter;
typedef set<bool_node*> childset;
*/

typedef FastSet<bool_node>::iterator child_citer;
typedef FastSet<bool_node>::iterator child_iter;
typedef FastSet<bool_node> childset;



class bool_node{
protected:
  bool_node():mother(NULL), layer(0), father(NULL), flag(0), id(-1), ion_pos(0), otype(BOTTOM){};
  bool_node(const bool_node& bn, bool copyChildren):mother(bn.mother), layer(bn.layer), 
  								 name(bn.name), father(bn.father), 
  								 flag(bn.flag), id(bn.id), ion_pos(bn.ion_pos), 
								 otype(bn.otype), type(bn.type) { if(copyChildren){ children = bn.children; }  };
public:

  typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL,PLUS, TIMES, DIV, MOD, NEG, CONST, GT, GE, LT, LE, EQ,ARITH, ASSERT} Type;
  typedef enum{BOTTOM, BOOL, INT} OutType;

  string name;
  int layer;
  int id;
  mutable int flag;
  int ion_pos;
  
  Type type;
  mutable OutType otype;
  bool_node* mother;  
  bool_node* father;
  
  childset children;
  

  
  OutType joinOtype(OutType t1, OutType t2) const{
  	if(t1 == BOTTOM){ return t2; }
  	if(t2 == BOTTOM){ return t1; }
  	if( t2 == t1 ){ 
  		return t1; 
  	}else{ 
  		return INT; 
  	}
  }

  virtual int do_dfs(int idx);
  virtual int back_dfs(int idx);
  virtual void remove_child(bool_node* bn);  
  virtual void dislodge();
  virtual void removeFromParents(bool_node* bn);
  virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
  virtual void outDagEntry(ostream& out) const;
  virtual void addToParents();
   
  virtual void redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert);
  virtual void redirectPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag);
  virtual void switchInputs(BooleanDAG& bdag);
  virtual string get_tname() const{
    switch(type){
	case PLUS: return "PLUS";
	case TIMES: return "TIMES";
	case DIV: return "DIV";
	case MOD: return "MOD";
	case NEG: return "NEG";
	case CONST: return "CONST";
    case AND: return "AND";
    case OR: return "OR";
    case XOR: return "XOR";
    case SRC: return "S";
    case DST: return "D";
    case NOT: return "NOT";
    case CTRL: return "CTRL";
	case GT: return "GT";
	case GE: return "GE";
	case LT: return "LT";
	case LE: return "LE";
	case EQ: return "EQ";
    case ASSERT: return "ASSERT";
    }
    cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
    throw BasicError("Err", "Err");
  }
  virtual string get_name() const;
  void set_layer();
  virtual void accept(NodeVisitor& visitor) =0;
  virtual bool_node* clone(bool copyChildren = true)=0;
  virtual void printSubDAG(ostream& out);
  virtual OutType getOtype() const;
  virtual void replace_child_inParents(bool_node* ori, bool_node* replacement);
  void neighbor_replace(bool_node* replacement);
  void replace_child(bool_node* ori, bool_node* replacement);
};



inline void bool_node::remove_child(bool_node* bn){
	children.erase(bn);
}




class arith_node: public bool_node{
	protected:
	arith_node():bool_node(){ type = ARITH; };
	arith_node(const arith_node& an, bool copyChildren):bool_node(an, copyChildren), multi_mother(an.multi_mother), arith_type(an.arith_type){ type = ARITH; };
	public:
    typedef enum {   ARRACC, UFUN, ARRASS, ACTRL  } AType;

		
	AType arith_type;
	vector<bool_node*> multi_mother;	
    virtual int back_dfs(int idx);
	virtual void dislodge();
	virtual void removeFromParents(bool_node* bn);
	virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
	virtual void outDagEntry(ostream& out);
	virtual void addToParents();
	virtual void addToParents(bool_node* only_thisone);
	virtual void redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert);
	virtual void replace_child_inParents(bool_node* ori, bool_node* replacement);
	virtual void switchInputs(BooleanDAG& bdag);
	virtual void printSubDAG(ostream& out);
	virtual string get_tname() const{
		switch(arith_type){			
			case ARRACC: return "ARRACC";
			case UFUN: return "UFUN";			
			case ACTRL: return "ACTRL";
			case ARRASS: return "ARRASS";
		}
		return "null";
	}
	virtual OutType getOtype()const;
};




class AND_node: public bool_node{ 	
	public: 
		AND_node(const AND_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		AND_node(){ type = AND; }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new AND_node(*this, copyChildren);  };
		OutType getOtype() const{
			return BOOL;
		}
	};
class OR_node: public bool_node{	
	public: 
		OR_node(){ type = OR; } 
		OR_node(const OR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }   
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new OR_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
	};
class XOR_node: public bool_node{	
	public: 
		XOR_node(){ type = XOR; }  
		XOR_node(const XOR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new XOR_node(*this, copyChildren);  };
		OutType getOtype() const{
			return BOOL;
		}
	};

class INTER_node: public bool_node{	
	protected: 
	INTER_node(const INTER_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), nbits(bn.nbits){ }  
		INTER_node(){nbits = 1;}
		int nbits;
	public:	
	int get_nbits() const { return nbits; }
	void set_nbits(int n){ nbits = n; }
	
	OutType getOtype() const {
		if(otype != BOTTOM){
			return otype;
		}
		if(nbits>1){
			otype = INT;	
		}else{
			otype = BOOL;
		}		
		return otype;
	}
	
	string get_name() const{
	    stringstream str;
	    if(name.size() > 0)
	      str<<name<<"__"<<get_tname();
	    else{      
	      str<<"name_"<<abs(id)<<"_"<<"__"<<get_tname();
	      
	    }
	    Assert( id != -22, "This is a corpse. It's living gargabe "<<str.str()<<" id ="<<id );
	    return str.str();
	  }
};


class SRC_node: public INTER_node{		
	public: SRC_node(){ type = SRC; }  
	SRC_node(const SRC_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }  
	SRC_node(const string& nm){ 
		type = SRC;
		name = nm;
	}
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new SRC_node(*this, copyChildren);  };
};

class DST_node: public INTER_node{		
	public: 
		DST_node(){ type = DST; }  
		DST_node(const DST_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new DST_node(*this, copyChildren);  };
};


class CTRL_node: public INTER_node{
	public: 
	CTRL_node(){ type = CTRL;} 
	CTRL_node(const CTRL_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }   
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new CTRL_node(*this, copyChildren);  };	
};

class NOT_node: public bool_node{	
	public: 
	NOT_node(){ type = NOT; }  
	NOT_node(const NOT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new NOT_node(*this, copyChildren);  };
	OutType getOtype() const {
			return BOOL;
	}
};

class PLUS_node: public bool_node{	
	public: PLUS_node(){ type = PLUS; } 
	PLUS_node(const PLUS_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new PLUS_node(*this, copyChildren);  };
	OutType getOtype()const {
			return INT;
	}
};
class TIMES_node: public bool_node{	
	public: TIMES_node(){ type = TIMES; }  
	TIMES_node(const TIMES_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }    
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new TIMES_node(*this, copyChildren);  };
	OutType getOtype()const {
		return INT;
	}
};



class UFUN_node: public arith_node{
	int nbits;	
	string ufname;
	map<bool_node*, bool_node*> mothersToReplace;
	public: UFUN_node(const string& p_ufname):ufname(p_ufname){ arith_type = UFUN; nbits=1; } 
	UFUN_node(const UFUN_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), nbits(bn.nbits), ufname(bn.ufname){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out){
    	int i=0;
		for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;	  		
		  	}
		}
	}

	virtual bool_node* clone(bool copyChildren = true){return new UFUN_node(*this, copyChildren);  };
	int get_nbits() const { return nbits; }
	const string& get_ufname() const { return ufname; }
	void set_nbits(int n){ nbits = n; }
	OutType getOtype()const {
		if(otype != BOTTOM){
			return otype;
		}
		if(nbits>1){
			otype = INT;	
		}else{
			otype = BOOL;
		}
		return otype;
	}
};



class ARRACC_node: public arith_node{	
	public: ARRACC_node(){ arith_type = ARRACC; }  
	ARRACC_node(const ARRACC_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out){
		if( mother != NULL){
          out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[label=\"idx\"] ; "<<endl;
    	}		
    	int i=0;
		for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;	  		
		  	}
		}
	}
	OutType getOtype()const {
			if(otype != BOTTOM){
				return otype;
			}
			for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
				otype = joinOtype((*it)->getOtype(), otype);	
			}			
			return otype;
		}
	virtual bool_node* clone(bool copyChildren = true){return new ARRACC_node(*this, copyChildren);  };
};
class DIV_node: public bool_node{	
	public: 
		DIV_node(){ type = DIV; }
		DIV_node(const DIV_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }     
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new DIV_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
};
class MOD_node: public bool_node{	
	
	public: 
		MOD_node(){ type = MOD; }  
		MOD_node(const MOD_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new MOD_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
};


class NEG_node: public bool_node{	
	
	public: 
		NEG_node(){ type = NEG; }
		NEG_node(const NEG_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new NEG_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
};


class CONST_node: public bool_node{
	int val;
	public:
		CONST_node(){  
			type = CONST; val = -1;}
		CONST_node(int n){  
			type = CONST; val = n;}
		CONST_node(const CONST_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), val(bn.val){
			//if(val == 13){ cout<<" surprise"<<endl; }
		}  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual void setVal(int v){ val = v; }
		virtual int getVal() const { return val; }
		string get_name(){
		    stringstream str;
		    str<<name<<"_C";
		    if( val<0){
		    	str<<"m";	
		    }
		    str<<abs(val);		    
		    return str.str();
		}
		virtual bool_node* clone(bool copyChildren = true){return new CONST_node(*this, copyChildren);  };
		OutType getOtype()const {
			if(otype != BOTTOM){
				return otype;
			}
			if(val != 0 && val != 1){
				otype = INT;	
			}else{
				otype = BOOL;
			}		
			return otype;
		}
};

class GT_node: public bool_node{	
	public: 
		GT_node(){  type = GT; }  
		GT_node(const GT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new GT_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};
class GE_node: public bool_node{	
	public: 
		GE_node(){  type = GE; } 
		GE_node(const GE_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new GE_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};
class LT_node: public bool_node{	
	public: 
		LT_node(){  type = LT; }  
		LT_node(const LT_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new LT_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};
class LE_node: public bool_node{	
	public: 
		LE_node(){  type = LE; }  
		LE_node(const LE_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new LE_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};
class EQ_node: public bool_node{	
	public: 
		EQ_node(){  type = EQ; } 
		EQ_node(const EQ_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new EQ_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};

/*!
    multi-mother[0] = old-value;
    multi-mother[1] = new-value;
    if( mother == quant ) return multi-mother[1]; else return multi-mother[0];		
*/
class ARRASS_node: public arith_node{		
	public: 
		int quant;
		ARRASS_node(){ arith_type = ARRASS; quant = -1; }
		ARRASS_node(const ARRASS_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), quant(bn.quant){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new ARRASS_node(*this, copyChildren);  };
		virtual void outDagEntry(ostream& out){
			if( mother != NULL){
			  out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[label=\"="<<quant<<"\"] ; "<<endl;
    		}		    		
			
	  		if(multi_mother[0] != NULL){
	  			out<<" "<<multi_mother[0]->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;	  		
	  		}
			if(multi_mother[1] != NULL){
	  			out<<" "<<multi_mother[1]->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
	  		}
		}
		OutType getOtype()const {
			if(otype != BOTTOM){
				return otype;
			}
			otype = joinOtype(multi_mother[0]->getOtype(), multi_mother[1]->getOtype());
			return otype;
		}
};
class ACTRL_node: public arith_node{	
	public: ACTRL_node(){ arith_type = ACTRL; }  
	ACTRL_node(const ACTRL_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }	
	virtual bool_node* clone(bool copyChildren = true){return new ACTRL_node(*this, copyChildren);  };
	OutType getOtype()const {
		return INT;
	}	
};
class ASSERT_node: public bool_node {
	bool isHardAssert;
	string msg;
public:
    ASSERT_node ():isHardAssert(false) { type = ASSERT; }
    ASSERT_node(const ASSERT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ isHardAssert = bn.isHardAssert;  msg = bn.msg; }  
    virtual void accept (NodeVisitor &visitor)  { visitor.visit (*this); }
    virtual bool_node* clone(bool copyChildren = true) {return new ASSERT_node(*this, copyChildren);  };
    virtual void makeHardAssert(){ isHardAssert = true; }
    virtual bool isHard() const { return isHardAssert ; }
    virtual void setMsg(const string& pmsg){ msg = pmsg; }
    virtual const string& getMsg()const{ return msg; }
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
		case bool_node::PLUS: return new PLUS_node();
		case bool_node::TIMES: return new TIMES_node();
		case bool_node::DIV: return new DIV_node();
		case bool_node::MOD: return new MOD_node();
		case bool_node::NEG: return new  NEG_node();
		case bool_node::CONST: return new CONST_node();
		case bool_node::GT: return new GT_node();
		case bool_node::GE: return new GE_node();
		case bool_node::LT: return new LT_node();
		case bool_node::LE: return new LE_node();
		case bool_node::EQ: return new EQ_node();
	}
	return NULL;
}

inline arith_node* newArithNode( arith_node::AType type){
	switch(type){		
		case arith_node::ARRACC: return new ARRACC_node();
		case arith_node::UFUN: return new UFUN_node("NULL");
		case arith_node::ARRASS: return new ARRASS_node();
		case arith_node::ACTRL: return new ACTRL_node();		
	}
	return NULL;
}


inline bool comp_id(bool_node* n1, bool_node* n2){
  int n1id = n1->id;
  if(n1id == -1){
    return false;
  }
  int n2id = n2->id;
  if(n2id == -1){ //& n1->id != -1
    return n1id!= n2id;
  }
  return n1id < n2id;
}


inline bool comp_layer(bool_node* n1, bool_node* n2){
  return n1->layer < n2->layer || ( (n1->layer == n2->layer) && n1->ion_pos<n2->ion_pos );
}

extern timerclass TTMMPP;

class BooleanDAG  
{
  int n_inputs;
  int n_outputs;
  int n_controls;
  int new_names;
  int new_namesb;

  vector<bool_node*> nodes;
  vector<int> layer_sizes;
  map<string, bool_node*> named_nodes;
  map<string, string > aliasmap;
  map<bool_node::Type, vector<bool_node*> > nodesByType;

  

  bool is_sorted; //The sorted property implies that everyone comes after their parents
  bool is_layered;  //The layered property implies that nodes are sorted by layer. is_layered implies is_sorted.
  bool has_passthrough; //This property implies that NOT nodes have been added.

  void compute_layer_sizes();
  //void removeFromChildren(bool_node* parent, bool_node* toremove);
  void remove(int i);
public:

  BooleanDAG* clone();
  void clone_nodes(vector<bool_node*>& nstore);

  void repOK();
  void print(ostream& out);
  typedef vector<bool_node*>::iterator iterator;
  void removeNullNodes();
  void replace(int original, bool_node* replacement);
  //void replace(int original, bool_node* cse);
  
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
                      const string& father, bool_node* thenode);
  bool_node* new_node(bool_node* mother,  
                      bool_node* father, bool_node* thenode);

 bool_node* new_node(bool_node* mother,  
                      bool_node* father, bool_node* thenode, const string& name);


  bool_node* set_node(bool_node* tmp, bool_node* mother, 
                      bool_node* father,  bool_node::Type t);
                      
                      
  void addNewNode(bool_node* v);
  void addNewNodes(vector<bool_node*>& v);

	void clearBackPointers();

  void change_father(const string& father, const string& son);
  void change_mother(const string& father, const string& son);

  int get_n_inputs()const{ return n_inputs; };
  int get_n_outputs()const{ return n_outputs; };
  int get_n_controls()const{ return n_controls; };
  int get_n_layers()const{ return layer_sizes.size(); };


  inline bool checkNodePosition(bool_node* bn){
	  int bid = bn->id;
	return bid < this->size() && bid >= 0 && bn == nodes[bid];
  }


  vector<bool_node*>& getNodesByType(bool_node::Type t);
  bool_node* const & operator[](int idx)const{ return nodes[idx]; };
  bool_node*& operator[](int idx){ return nodes[idx]; };
  int size()const {return nodes.size();}
  int get_lsize(int layer){ return layer_sizes[layer];};
  void sort_graph();
  void layer_graph();  
  void relabel();
  void cleanup(bool moveNots=true);
  bool_node* get_node(const string& name);  
  bool_node* unchecked_get_node(const string& name);
  iterator begin(){ return nodes.begin(); }
  iterator end(){ return nodes.end(); }
  void alias(const string& ssource,  const string& starg);
  void rename(const string& oldname,  const string& newname);
  void resetBackPointers();
  
  bool has_name(const string& s){
  	return named_nodes.find(s) != named_nodes.end();
  }
  
  void clear();
  
  bool has_alias(const string& s){
    return aliasmap.find(s) != aliasmap.end();
  }

  string get_alias(const string& s){
    return aliasmap[s];
  }

  void moveNNb(){
  	new_namesb++;
  	new_names = 0;
  }

  string new_name(){
    stringstream str;
    str<<"t"<<hex<<new_namesb<<"_"<<new_names<<"_";
    ++new_names;
    return str.str();
  }
 string new_name(string& base){
 	if(base.length() < 2){
 		return new_name();	
 	}else{
	    stringstream str;
	    str<<base.substr(0,4)<<new_namesb<<"_"<<new_names<<"__";
	    ++new_names;
	    return str.str();
 	}
  }
  
	BooleanDAG();
	virtual void makeMiter(BooleanDAG& bdag);
	virtual ~BooleanDAG();
};

#endif // !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)

