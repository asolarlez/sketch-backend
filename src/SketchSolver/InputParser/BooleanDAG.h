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
#include "BasicError.h"
#include "timerclass.h"
#include "NodeVisitor.h"

using namespace std;

class BooleanDAG;


typedef set<bool_node*>::iterator child_iter;

class bool_node{
protected:
  bool_node():mother(NULL), layer(0), father(NULL), flag(0), id(-1), ion_pos(0), otype(BOTTOM){};
  bool_node(const bool_node& bn):mother(bn.mother), layer(bn.layer), 
  								 name(bn.name), father(bn.father), 
  								 flag(bn.flag), id(bn.id), ion_pos(bn.ion_pos), 
  								 otype(bn.otype), type(bn.type), children(bn.children) {};
public:
  string name;
  int layer;
  int id;
  int flag;
  int ion_pos;
  typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL, ARITH, ASSERT} Type;
  typedef enum{BOTTOM, BOOL, INT} OutType;
  OutType joinOtype(OutType t1, OutType t2){
  	if(t1 == BOTTOM){ return t2; }
  	if(t2 == BOTTOM){ return t1; }
  	if( t2 == t1 ){ 
  		return t1; 
  	}else{ 
  		return INT; 
  	}
  }
  Type type;
  OutType otype;
  bool_node* mother;  
  bool_node* father;
  
  set<bool_node*> children;
  
  
  virtual int do_dfs(int idx);
  virtual int back_dfs(int idx);
  virtual void remove_child(bool_node* bn);  
  virtual void dislodge();
  virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
  virtual void outDagEntry(ostream& out);
  virtual void addToParents();
  virtual void redirectPointers(BooleanDAG& oribdag, BooleanDAG& bdag);
  virtual void switchInputs(BooleanDAG& bdag);
  virtual string get_tname(){
    switch(type){
    case AND: return "AND";
    case OR: return "OR";
    case XOR: return "XOR";
    case SRC: return "S";
    case DST: return "D";
    case NOT: return "NOT";
    case CTRL: return "CTRL";
    case ASSERT: return "ASSERT";
    }
    cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
    throw BasicError("Err", "Err");
  }
  virtual string get_name();
  void set_layer();
  virtual void accept(NodeVisitor& visitor)=0;
  virtual bool_node* clone()=0;
  virtual void printSubDAG(ostream& out);
  virtual OutType getOtype();
};



#ifdef CONST
#undef CONST
#endif

class arith_node: public bool_node{
	protected:
	arith_node():bool_node(){ type = ARITH; };
	public:
    typedef enum {  PLUS, TIMES, ARRACC, UFUN, DIV, MOD, NEG, CONST, GT, GE, LT, LE, EQ, ARRASS, ACTRL  } AType;

		
	AType arith_type;
	vector<bool_node*> multi_mother;	
    virtual int back_dfs(int idx);
	virtual void dislodge();
	virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
	virtual void outDagEntry(ostream& out);
	virtual void addToParents();
	virtual void redirectPointers(BooleanDAG& oribdag, BooleanDAG& bdag);
	virtual void switchInputs(BooleanDAG& bdag);
	virtual void printSubDAG(ostream& out);
	virtual string get_tname(){
		switch(arith_type){
			case PLUS: return "PLUS";
			case TIMES: return "TIMES";
			case DIV: return "DIV";
			case MOD: return "MOD";
			case NEG: return "NEG";
			case CONST: return "CONST";
			case ARRACC: return "ARRACC";
			case UFUN: return "UFUN";
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
	virtual OutType getOtype();
};




class AND_node: public bool_node{ 	
	public: 
		AND_node(const AND_node& bn): bool_node(bn){ }  
		AND_node(){ type = AND; }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new AND_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
	};
class OR_node: public bool_node{	
	public: 
		OR_node(){ type = OR; } 
		OR_node(const OR_node& bn): bool_node(bn){ }   
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new OR_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
	};
class XOR_node: public bool_node{	
	public: 
		XOR_node(){ type = XOR; }  
		XOR_node(const XOR_node& bn): bool_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new XOR_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
	};

class INTER_node: public bool_node{	
	protected: 
	INTER_node(const INTER_node& bn): bool_node(bn), nbits(bn.nbits){ }  
		INTER_node(){nbits = 1;}
		int nbits;
	public:	
	int get_nbits() const { return nbits; }
	void set_nbits(int n){ nbits = n; }
	
	OutType getOtype(){
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
	
	string get_name(){
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
	SRC_node(const SRC_node& bn): INTER_node(bn){ }  
	SRC_node(const string& nm){ 
		type = SRC;
		name = nm;
	}
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual bool_node* clone(){return new SRC_node(*this);  };
};

class DST_node: public INTER_node{		
	public: 
		DST_node(){ type = DST; }  
		DST_node(const DST_node& bn): INTER_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new DST_node(*this);  };
};


class CTRL_node: public INTER_node{
	public: 
	CTRL_node(){ type = CTRL;} 
	CTRL_node(const CTRL_node& bn): INTER_node(bn){ }   
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual bool_node* clone(){return new CTRL_node(*this);  };	
};

class NOT_node: public bool_node{	
	public: 
	NOT_node(){ type = NOT; }  
	NOT_node(const NOT_node& bn): bool_node(bn){ }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual bool_node* clone(){return new NOT_node(*this);  };
	OutType getOtype(){
			return BOOL;
	}
};

class PLUS_node: public arith_node{	
	public: PLUS_node(){ arith_type = PLUS; } 
	PLUS_node(const PLUS_node& bn): arith_node(bn){ }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual bool_node* clone(){return new PLUS_node(*this);  };
	OutType getOtype(){
			return INT;
	}
};
class TIMES_node: public arith_node{	
	public: TIMES_node(){ arith_type = TIMES; }  
	TIMES_node(const TIMES_node& bn): arith_node(bn){ }    
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual bool_node* clone(){return new TIMES_node(*this);  };
	OutType getOtype(){
		return INT;
	}
};



class UFUN_node: public arith_node{
	int nbits;	
	string ufname;
	public: UFUN_node(const string& p_ufname):ufname(p_ufname){ arith_type = UFUN; nbits=1; } 
	UFUN_node(const UFUN_node& bn): arith_node(bn), nbits(bn.nbits), ufname(bn.ufname){ }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out){
    	int i=0;
		for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;	  		
		  	}
		}
	}
	virtual bool_node* clone(){return new UFUN_node(*this);  };
	int get_nbits() const { return nbits; }
	string& get_ufname(){ return ufname; }
	void set_nbits(int n){ nbits = n; }
	OutType getOtype(){
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
	ARRACC_node(const ARRACC_node& bn): arith_node(bn){ }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
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
	OutType getOtype(){
			if(otype != BOTTOM){
				return otype;
			}
			for(vector<bool_node*>::iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
				otype = joinOtype((*it)->getOtype(), otype);	
			}			
			return otype;
		}
	virtual bool_node* clone(){return new ARRACC_node(*this);  };
};
class DIV_node: public arith_node{	
	public: 
		DIV_node(){ arith_type = DIV; }
		DIV_node(const DIV_node& bn): arith_node(bn){ }     
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new DIV_node(*this);  };
		OutType getOtype(){
			return INT;
		}
};
class MOD_node: public arith_node{	
	
	public: 
		MOD_node(){ arith_type = MOD; }  
		MOD_node(const MOD_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new MOD_node(*this);  };
		OutType getOtype(){
			return INT;
		}
};


class NEG_node: public arith_node{	
	
	public: 
		NEG_node(){ arith_type = NEG; }
		NEG_node(const NEG_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new NEG_node(*this);  };
		OutType getOtype(){
			return INT;
		}
};


class CONST_node: public arith_node{
	int val;
	public:
		CONST_node(){ arith_type = CONST; val = -1;}
		CONST_node(int n){ arith_type = CONST; val = n;}
		CONST_node(const CONST_node& bn): arith_node(bn), val(bn.val){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual void setVal(int v){ val = v; }
		virtual int getVal(){ return val; }
		string get_name(){
		    stringstream str;
		    str<<name<<"_C";
		    if( val<0){
		    	str<<"m";	
		    }
		    str<<abs(val);		    
		    return str.str();
		}
		virtual bool_node* clone(){return new CONST_node(*this);  };
		OutType getOtype(){
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

class GT_node: public arith_node{	
	public: 
		GT_node(){ arith_type = GT; }  
		GT_node(const GT_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new GT_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
};
class GE_node: public arith_node{	
	public: 
		GE_node(){ arith_type = GE; } 
		GE_node(const GE_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new GE_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
};
class LT_node: public arith_node{	
	public: 
		LT_node(){ arith_type = LT; }  
		LT_node(const LT_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new LT_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
};
class LE_node: public arith_node{	
	public: 
		LE_node(){ arith_type = LE; }  
		LE_node(const LE_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new LE_node(*this);  };
		OutType getOtype(){
			return BOOL;
		}
};
class EQ_node: public arith_node{	
	public: 
		EQ_node(){ arith_type = EQ; } 
		EQ_node(const EQ_node& bn): arith_node(bn){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new EQ_node(*this);  };
		OutType getOtype(){
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
		ARRASS_node(const ARRASS_node& bn): arith_node(bn), quant(bn.quant){ }  
		virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }
		virtual bool_node* clone(){return new ARRASS_node(*this);  };
		virtual void outDagEntry(ostream& out){
			if( mother != NULL){
			  out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[label=\"="<<quant<<"\"] ; "<<endl;
    		}		
    		int i=0;
			
	  		if(multi_mother[0] != NULL){
	  			out<<" "<<multi_mother[0]->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;	  		
	  		}
			if(multi_mother[1] != NULL){
	  			out<<" "<<multi_mother[1]->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
	  		}
		}
		OutType getOtype(){
			if(otype != BOTTOM){
				return otype;
			}
			otype = joinOtype(multi_mother[0]->getOtype(), multi_mother[1]->getOtype());
			return otype;
		}
};
class ACTRL_node: public arith_node{	
	public: ACTRL_node(){ arith_type = ACTRL; }  
	ACTRL_node(const ACTRL_node& bn): arith_node(bn){ }  
	virtual void accept(NodeVisitor& visitor){ visitor.visit( *this ); }	
	virtual bool_node* clone(){return new ACTRL_node(*this);  };
	OutType getOtype(){
		return INT;
	}	
};
class ASSERT_node: public bool_node {
	bool isHardAssert;
	string msg;
public:
    ASSERT_node ():isHardAssert(false) { type = ASSERT; }
    ASSERT_node(const ASSERT_node& bn): bool_node(bn){ isHardAssert = bn.isHardAssert;  msg = bn.msg; }  
    virtual void accept (NodeVisitor &visitor) { visitor.visit (*this); }
    virtual bool_node* clone(){return new ASSERT_node(*this);  };
    virtual void makeHardAssert(){ isHardAssert = true; }
    virtual bool isHard(){ return isHardAssert ; }
    virtual void setMsg(const string& pmsg){ msg = pmsg; }
    virtual string& getMsg(){ return msg; }
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
		case arith_node::UFUN: return new UFUN_node("NULL");
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


  void print(ostream& out);
  typedef vector<bool_node*>::iterator iterator;
  void removeNullNodes();
  void replace(int original, bool_node* replacement, timerclass& replacepar=TTMMPP);
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


  bool checkNodePosition(bool_node* bn){
	return bn->id < this->size() && bn->id >= 0 && bn == (*this)[bn->id];
  }


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
  bool_node* unchecked_get_node(const string& name);
  void neighbor_replace(bool_node* onode, bool_node* replacement, timerclass& replacepar);
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
    str<<"TNM_"<<new_namesb<<"_"<<new_names<<"___";
    ++new_names;
    return str.str();
  }
 string new_name(string& base){
 	if(base.length() < 2){
 		return new_name();	
 	}else{
	    stringstream str;
	    str<<base.substr(0,4)<<new_namesb<<"_"<<new_names<<"____";
	    ++new_names;
	    return str.str();
 	}
  }
  
	BooleanDAG();
	virtual void makeMiter(BooleanDAG& bdag, const string& tip_name );
	virtual ~BooleanDAG();
};

#endif // !defined(AFX_BOOLEANDAG_H__61A0B2EB_5CC4_4D69_AA64_9DC156ED4C8D__INCLUDED_)

