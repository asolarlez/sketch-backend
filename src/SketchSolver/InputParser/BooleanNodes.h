#ifndef BOOLNODES_H_
#define BOOLNODES_H_

#include "FastSet.h"
#include "NodeVisitor.h"
#include "Dllist.h"
#include <map>
#include <set>
#include <cstdlib>
#include <typeinfo>


#ifdef CONST
#undef CONST
#endif



using namespace std;

class BooleanDAG;

#ifdef SET_CHILDREN

typedef set<bool_node*>::const_iterator child_citer;
typedef set<bool_node*>::iterator child_iter;
typedef set<bool_node*> childset;

#else

typedef FastSet<bool_node>::iterator child_citer;
typedef FastSet<bool_node>::iterator child_iter;
typedef FastSet<bool_node> childset;

#endif

//#define SCHECKMEM


class bool_node{

private:
  /** The unique ID to be assigned to the next bool_node created. */
  static int NEXT_GLOBAL_ID;
#ifdef SCHECKMEM
	static set<bool_node*> allocated;
#endif

protected:
  bool_node();
  bool_node(const bool_node& bn, bool copyChildren);

public:

  typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL,PLUS, TIMES, DIV, MOD, NEG, CONST, GT, GE, LT, LE, EQ,ARITH, ASSERT} Type;
  typedef enum{BOTTOM, BOOL, INT} OutType;

  string name;
  int layer;
  /**
   * The globally unique ID of this node.
   *
   * (Warning: limits number of nodes to 4Gi).
   */
  int globalId;
  /** 
   * The unique ID of this node within a DAG, not guaranteed to be globally unique.
   * (Implementation detail: the DAG contains a vector of nodes, and this 'id' is
   * this nodes position within that vector.
   */
  int id;
  mutable int flag;
  int ion_pos;
  
  Type type;
  mutable OutType otype;
  bool_node* mother;  
  bool_node* father;
  
  childset children;
  virtual ~bool_node();

  void resetId(){
	globalId = NEXT_GLOBAL_ID++;
  }
  
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
  virtual void redirectPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, childset& tchild);
  virtual void switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements);
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
  virtual string get_sym() const{
    switch(type){
	case PLUS: return "+";
	case TIMES: return "*";
	case DIV: return "/";
	case MOD: return "%";
	case NEG: return "-";
	case CONST: return "C";
    case AND: return "&";
    case OR: return "|";
    case XOR: return "^";
    case SRC: return "S";
    case DST: return "D";
    case NOT: return "!";
    case CTRL: return "CTRL";
	case GT: return ">";
	case GE: return ">=";
	case LT: return "<";
	case LE: return "<=";
	case EQ: return "==";
    case ASSERT: return "ASSERT";
    }
    cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
    throw BasicError("Err", "Err");
  }
  virtual string lprint(){
		stringstream str;
		str<<id<<"= ";
		if(mother != NULL){
			str<<mother->lid()<<" ";
		}
		str<<get_sym()<<" ";
		if(father != NULL){
			str<<father->lid()<<" ";
		}
		return str.str();
  }
  virtual string lid(){
		stringstream str;
		str<<id;
		return str.str();
  }
  virtual string get_name() const;
  void set_layer(bool isRecursive);
  virtual void accept(NodeVisitor& visitor) =0;
  virtual bool_node* clone(bool copyChildren = true)=0;
  virtual void printSubDAG(ostream& out, set<const bool_node* >& s)const;
  virtual OutType getOtype() const;
  virtual void replace_child_inParents(bool_node* ori, bool_node* replacement);
  void neighbor_replace(bool_node* replacement);
  void replace_child(bool_node* ori, bool_node* replacement);
};




/**
 * Special FastSet hash function that hashes on the
 * bool_node's ID (assumed unique) instead of its memory address.
 */
template<>
struct FastSetTraits<bool_node>
{
    static inline unsigned
    hash (bool_node *bn, int sz)
    {
        unsigned tmp = bn->globalId; // (unsigned) in>>2;  // (generic version)

        tmp = tmp * (tmp + 4297);
        tmp = tmp + (tmp >> 2) + (tmp >> 5) + (tmp >> 21) + (tmp >> 28);		
		
        unsigned m = 1 << sz;
        m = m-1;
        return (tmp & m)<<2;
    }
};


inline void bool_node::remove_child(bool_node* bn){
	children.erase(bn);
}




class arith_node: public bool_node{
	protected:
	arith_node():bool_node(){ type = ARITH; };
	virtual ~arith_node(){}
	arith_node(const arith_node& an, bool copyChildren):bool_node(an, copyChildren), multi_mother(an.multi_mother), arith_type(an.arith_type){ type = ARITH; };
	public:
    typedef enum {   ARRACC, UFUN, ARRASS, ACTRL  } AType;

		
	AType arith_type;
	vector<bool_node*> multi_mother;	
    virtual int back_dfs(int idx);
	virtual void dislodge();
	virtual void removeFromParents(bool_node* bn);
	virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
	virtual void outDagEntry(ostream& out)const;
	void set_layer(bool isRecursive);
	virtual void addToParents();
	virtual void addToParents(bool_node* only_thisone);
	virtual void redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert);
	virtual void replace_child_inParents(bool_node* ori, bool_node* replacement);
	virtual void switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements);
	virtual void printSubDAG(ostream& out, set<const bool_node* >& s)const;
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
	virtual ~INTER_node(){}
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
	virtual string lprint(){
		return name;
	}
	virtual string lid(){
		return name;
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

class DST_node: public INTER_node, public DllistNode{
	public: 
		DST_node(){ type = DST; }  
		DST_node(const DST_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new DST_node(*this, copyChildren);  };
		virtual ~DST_node(){}
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
	virtual string lid(){
		stringstream str;
		str<<"(!"<<mother->lid()<<")";
		return str.str();
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



class UFUN_node: public arith_node, public DllistNode{
	const int callsite;
	static int CALLSITES;
	int nbits;	
	string ufname;	
	public: 
	string outname;
	int fgid;
		
		UFUN_node(const string& p_ufname):ufname(p_ufname), callsite(CALLSITES++){ arith_type = UFUN; nbits=1; } 
		UFUN_node(const UFUN_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), nbits(bn.nbits), ufname(bn.ufname), callsite(bn.callsite), outname(bn.outname), fgid(bn.fgid){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out)const{
    	int i=0;
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;	  		
		  	}
		}
	}

	virtual bool_node* clone(bool copyChildren = true){return new UFUN_node(*this, copyChildren);  };
	int get_callsite()const{ return callsite; }
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
	virtual ~UFUN_node(){ 
	}
	string get_name() const{
	    
		if(ufname.size() > 1){
		  stringstream str;
		  str<<ufname.substr(0, 5)<<id<<"__"<<get_tname();
		  return str.str();
		}else{
			return arith_node::get_name();
	      
	    }
	    
	  }
	virtual string lprint(){
		stringstream str;
		str<<id<<"= "<<ufname.substr(0, 5)<<"["<<mother->lid()<<"](";
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";	  		
		  	}
		}
		str<<")";
		return str.str();
	}
};



class ARRACC_node: public arith_node{	
	public: ARRACC_node(){ arith_type = ARRACC; }  
	ARRACC_node(const ARRACC_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out) const{
		if( mother != NULL){
          out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[label=\"idx\"] ; "<<endl;
    	}		
    	int i=0;
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
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
	virtual string lprint(){
		stringstream str;
		str<<id<<"= "<<"["<<mother->lid()<<"]$";
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";	  		
		  	}
		}
		str<<"$";
		return str.str();
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
		string get_name() const{
		    stringstream str;
		    str<<name<<"_C";
		    if( val<0){
		    	str<<"m";	
		    }
		    str<<abs(val);		    
		    return str.str();
		}
		  virtual string lid(){
				stringstream str;
				str<<"(" << val << ")";
				return str.str();
		  }
		virtual string lprint(){
			stringstream str;
			str<<id<<"= "<<"(" << val << ")";
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
		virtual void outDagEntry(ostream& out) const{
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
		virtual string lprint(){
			stringstream str;
			str<<id<<"= "<<mother->lid()<<"=="<<quant<<" ? "<<multi_mother[1]->lid()<<":"<<multi_mother[0]->lid();
			return str.str();
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
	virtual string lprint(){
		stringstream str;
		str<<id<<"= "<<"$$";
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";	  		
		  	}
		}
		str<<"$$";
		return str.str();
	}
};
class ASSERT_node: public bool_node, virtual public DllistNode{
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
	virtual ~ASSERT_node(){ 
	}
	  virtual string lprint(){
		stringstream str;
		str<<id<<"= ASSERT";
		
		str<<mother->lid()<<" : "<<msg;		
		return str.str();
  }
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





inline bool isDllnode(bool_node* bn){
	return bn->type == bool_node::ASSERT || bn->type==bool_node::DST || typeid(*bn) == typeid(UFUN_node) ;
}

inline bool isUFUN(DllistNode* dn){
	bool_node* t = dynamic_cast<bool_node*>(dn);
	return t->type == bool_node::ARITH;
}

inline DllistNode* getDllnode(bool_node* bn){
	return dynamic_cast<DllistNode*>(bn);
}


#endif
