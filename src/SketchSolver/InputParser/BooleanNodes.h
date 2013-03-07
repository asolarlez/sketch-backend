#ifndef BOOLNODES_H_
#define BOOLNODES_H_

#include "CegisCApi.h"
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

/* @TODO: Probably just keep EQ and LT nodes and get rid of other relational
 * nodes -- it may help in recognizing more common sub-expressions.
 */

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


struct bool_node{

private:
  /** The unique ID to be assigned to the next bool_node created. */
  static int NEXT_GLOBAL_ID;
#ifdef SCHECKMEM
	static set<bool_node*> allocated;
#endif

public:
  typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL,PLUS, TIMES, DIV, MOD, NEG, CONST, GT, GE, LT, LE, EQ, ASSERT, ARRACC, UFUN, ARRASS, ACTRL, ARR_R, ARR_W, ARR_CREATE} Type;
  typedef enum{BOTTOM, BOOL, INT,BOOL_ARR, INT_ARR} OutType;
  const Type type;

protected:
  bool_node(Type t);
  bool_node(const bool_node& bn, bool copyChildren);

public:


  bool isArrType(){
	  OutType ot = getOtype();
	  return ot == INT_ARR || ot==BOOL_ARR;
  }
  bool isArith(){
	  return type == ARRACC || type == UFUN || type == ARRASS || type == ACTRL || type == ARR_W || type == ARR_CREATE;
  }

  bool isInter(){
		return type == CTRL || type==SRC || type==DST;
  }

  // string name;
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
  
  
  mutable OutType otype;
  bool_node* mother;  
  bool_node* father;
  
  childset children;
  virtual ~bool_node();

  void resetId(){
	globalId = NEXT_GLOBAL_ID++;
  }
  /**
              INT_ARR
		      |     \
		  BOOL_ARR  INT
		       \    /
			    BOOL
		      

  */
  OutType joinOtype(OutType t1, OutType t2) const{
  	if(t1 == BOTTOM){ return t2; }
  	if(t2 == BOTTOM){ return t1; }
  	if( t2 == t1 ){ 
  		return t1; 
  	}else{ 
		OutType rv = BOOL;
		if(t1==INT_ARR || t2==INT_ARR){
			return INT_ARR;
		}
		if(t1==INT || t2==INT){
			rv = INT;
			if(t1==BOOL_ARR || t2==BOOL_ARR){
				rv = INT_ARR;
			}
		}else{
			if(t1==BOOL_ARR || t2==BOOL_ARR){
				rv = BOOL_ARR;
			}
		}		
  		return rv; 
  	}
  }

  virtual int do_dfs(int idx);
  virtual int back_dfs(int idx);
  
  
  /// Remove bn from my child list.
  virtual void remove_child(bool_node* bn);  
  
  /// Remove me from the child list of my parents.  
  virtual void dislodge();
  // virtual void removeFromParents(bool_node* bn);
  /// If any of my parents is equal to oldpar, make it equal to newpar instead.
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
		case ARRACC: return "ARRACC";
		case UFUN: return "UFUN";			
		case ACTRL: return "ACTRL";
		case ARRASS: return "ARRASS";
		case ARR_R: return "ARR_R";
		case ARR_W: return "ARR_W";
		case ARR_CREATE: return "ARR_CREATE";
    }
    //cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
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
    //cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
    throw BasicError("Err", "Err");
  }
  virtual string lprint()const{
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
  virtual void printSubDAG(ostream& out)const{
	  set<const bool_node* > s;
	  printSubDAG(out, s);
  }

  virtual void lprintSubDAG(ostream& out, set<const bool_node* >& s)const;
  virtual void lprintSubDAG(ostream& out)const{
	  set<const bool_node* > s;
	  lprintSubDAG(out, s);
  }

  virtual OutType getOtype() const;
  virtual string otypeString() const{
	  OutType ot = getOtype();
	  switch(ot){		
		case BOOL: return "BOOL";
		case INT: return "INT";
		case BOOL_ARR: return "BOOL_ARR";
		case INT_ARR: return "INT_ARR";
		default: return "BOTTOM";
	  }
  }
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id<<" "<<father->id;
		return str.str();
	}
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
	arith_node(Type t):bool_node(t){ };
	virtual ~arith_node(){}

	arith_node(const arith_node& an, bool copyChildren):bool_node(an, copyChildren), multi_mother(an.multi_mother){ };
	public:
    
	vector<bool_node*> multi_mother;	
    virtual int back_dfs(int idx);
	virtual void dislodge();
	// virtual void removeFromParents(bool_node* bn);
	//Note: replace_parent won't remove the node from the children list of the old parent, because most of the time the old parent will just
	//get destroyed anyway; if that's not the case, make sure to remove the node from the children list of the old parent.
	virtual void replace_parent(const bool_node * oldpar, bool_node* newpar);
	virtual void outDagEntry(ostream& out)const;
	void set_layer(bool isRecursive);
	virtual void addToParents();
	virtual void addToParents(bool_node* only_thisone);
	virtual void redirectParentPointers(BooleanDAG& oribdag, const vector<const bool_node*>& bdag, bool setChildrn, bool_node* childToInsert);
	virtual void replace_child_inParents(bool_node* ori, bool_node* replacement);
	virtual void switchInputs(BooleanDAG& bdag, map<bool_node*, bool_node*>& replacements);
	virtual void printSubDAG(ostream& out, set<const bool_node* >& s)const;
	virtual void lprintSubDAG(ostream& out, set<const bool_node* >& s)const;
	
	virtual OutType getOtype()const;
};



class ARR_R_node: public bool_node{
	//mother = index
	//father = inputarr
	public: 
		ARR_R_node(const ARR_R_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		ARR_R_node():bool_node(ARR_R){}  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new ARR_R_node(*this, copyChildren);  };
		OutType getOtype() const{
			if(otype != BOTTOM){
				return otype;
			}
			OutType ot = father->getOtype();
			if(ot == BOTTOM){
				otype = ot;
				return ot;
			}
			if(ot == BOOL_ARR){
				otype = BOOL;
				return BOOL;
			}
			if(ot == INT_ARR){
				otype = INT;
				return INT;			
			}
			otype = ot;
			return otype;			
		}
		virtual string lprint()const{
			stringstream str;
			str<<id<<"= "<<father->lid()<<"["<<mother->lid()<<"]";
			return str.str();
		}
	};


/*!
    Array assignment node.   
 
    multi-mother[0] = old-array;
    multi-mother[1] = new-value;
        
*/
class ARR_W_node:public arith_node{		
	public: 
		ARR_W_node():arith_node(ARR_W){ }
		ARR_W_node(const ARR_W_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
		bool_node*& getOldArr(){
			return multi_mother[0];
		}
		bool_node*& getNewVal(){
			return multi_mother[1];
		}
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new ARR_W_node(*this, copyChildren);  };
		virtual void outDagEntry(ostream& out) const{
			if( mother != NULL){
			  out<<" "<<mother->get_name()<<" -> "<<get_name()<<";" ; 
    		}		    		
			
	  		if(multi_mother[0] != NULL){
	  			out<<" "<<multi_mother[0]->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;	  		
	  		}
			if(multi_mother.size()>1 && multi_mother[1] != NULL){
	  			out<<" "<<multi_mother[1]->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
	  		}
		}
		virtual string lprint()const{
			stringstream str;
			str<<id<<"= "<<multi_mother[0]->lid()<<"{"<<mother->lid()<<"->"<<multi_mother[1]->lid()<<"}";				
			return str.str();
		}
		OutType getOtype()const {
			if(otype != BOTTOM){
				return otype;
			}
			otype = multi_mother[0]->getOtype();
			if(otype==INT){
				otype = INT_ARR;
			}
			if(otype==BOOL){
				otype = BOOL_ARR;
			}
			otype = joinOtype(otype, multi_mother[1]->getOtype());
			return otype;
		}
		virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id<<" "<<multi_mother[0]->id<<" "<<multi_mother[1]->id;
			return str.str();
		}
};

class ARR_CREATE_node:public arith_node{		
	public: 
		ARR_CREATE_node():arith_node(ARR_CREATE){ }
		ARR_CREATE_node(const ARR_CREATE_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new ARR_CREATE_node(*this, copyChildren);  };
		virtual void outDagEntry(ostream& out) const{				    					
	  		if(multi_mother[0] != NULL){
	  			out<<" "<<multi_mother[0]->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;	  		
	  		}
			if(multi_mother.size()>1 && multi_mother[1] != NULL){
	  			out<<" "<<multi_mother[1]->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
	  		}
		}
		virtual string lprint()const{
			stringstream str;
			str<<id<<"= "<<"{";
			for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  		if(*it != NULL){
		  			str<<(*it)->lid()<<", ";	  		
		  		}
			}
			str<<"}";
			return str.str();
		}
		OutType getOtype()const {
			if(otype != BOTTOM){
				return otype;
			}
			for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
				otype = joinOtype((*it)->getOtype(), otype);	
			}	
			if(otype == INT){
				otype = INT_ARR;
			}
			if(otype == BOOL){
				otype = BOOL_ARR;
			}
			return otype;
		}
		virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<multi_mother.size();
			for(int i=0; i<multi_mother.size(); ++i){
				str<<" "<<multi_mother[i]->id;
			}
			return str.str();
		}
};



class AND_node: public bool_node{ 	
	public: 
		AND_node(const AND_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		AND_node():bool_node(AND){}  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new AND_node(*this, copyChildren);  };
		OutType getOtype() const{
			return BOOL;
		}
	};
class OR_node: public bool_node{	
	public: 
		OR_node():bool_node(OR){ } 
		OR_node(const OR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }   
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new OR_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
	};
class XOR_node: public bool_node{	
	public: 
		XOR_node():bool_node(XOR){ }  
		XOR_node(const XOR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new XOR_node(*this, copyChildren);  };
		OutType getOtype() const{
			return BOOL;
		}
	};

/* Interface nodes, it includes input, output and control */
class INTER_node: public bool_node{	
	public:	
	string name;
	protected: 
	
	INTER_node(const INTER_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), name(bn.name), nbits(bn.nbits){ }  
		INTER_node(Type t):bool_node(t){nbits = 1;}
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
	    if(name.size() > 0){
			return name;
	      //str<<name<<"__"<<get_tname();
	}else{      
		  stringstream str;
	      str<<"name_"<<abs(id)<<"_"<<"__"<<get_tname();
		  return str.str();  
	    }
	    
	  }
	virtual string lprint()const{
		return name;
	}
	virtual string lid(){
		return name;
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<name<<" "<<nbits;
		return str.str();
	}
};

/* Input nodes */
class SRC_node: public INTER_node{		
public: SRC_node():INTER_node(SRC){ }  
	int arrSz;
	SRC_node(const SRC_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren), arrSz(bn.arrSz){ }  
	SRC_node(const string& nm):INTER_node(SRC), arrSz(-1){ 
		name = nm;
	}
	int getArrSz()const{
		return arrSz;
	}
	void setArr(int sz){ 
		arrSz = sz; 
		if(sz>=0){
			if(otype == INT){
				otype = INT_ARR;			
			}
			if(otype == BOOL){
				otype = BOOL_ARR;
			}		
		}
	}
	bool isArr() const{
		return arrSz >= 0;
	}
	OutType getOtype() const {
		if(otype != BOTTOM){
			return otype;
		}
		INTER_node::getOtype();
		if(!isArr()){ return otype; }
		if(otype == INT){
			otype = INT_ARR;
			return otype;
		}
		if(otype == BOOL){
			otype = BOOL_ARR;
			return otype;
		}		
	}
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new SRC_node(*this, copyChildren);  };	
};

/* Output Node */
class DST_node: public INTER_node, public DllistNode{
	public: 
		DST_node():INTER_node(DST){ }  
		DST_node(const DST_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new DST_node(*this, copyChildren);  };
		virtual ~DST_node(){}
	virtual string lprint()const{
		string tmp =  name;
		tmp += " = " + mother->lprint();
		return tmp;
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<name<<" "<<mother->id;
		return str.str();
	}
};


class CTRL_node: public INTER_node{
	bool toMinimize;
	public: 
	CTRL_node(bool toMinimize = false):INTER_node(CTRL){  this->toMinimize = toMinimize;} 
	CTRL_node(const CTRL_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ this->toMinimize = bn.toMinimize;}   
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new CTRL_node(*this, copyChildren);  };	
	string get_name() const {
		return name;
	}
	bool get_toMinimize() const {
		return toMinimize;
	}
	void set_toMinimize(bool toMinimize) {
		this->toMinimize = toMinimize;
	}
};

class NOT_node: public bool_node{	
	public: 
	NOT_node():bool_node(NOT){ }  
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
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id;
		return str.str();
	}

};

class PLUS_node: public bool_node{	
	public: PLUS_node():bool_node(PLUS){ } 
	PLUS_node(const PLUS_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new PLUS_node(*this, copyChildren);  };
	OutType getOtype()const {
			return INT;
	}
};
class TIMES_node: public bool_node{	
	public: TIMES_node():bool_node(TIMES){  }  
	TIMES_node(const TIMES_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }    
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new TIMES_node(*this, copyChildren);  };
	OutType getOtype()const {
		return INT;
	}
};


/* This node is used for both real and un-interpreted functions. In the case of
 * un-interpreted functions: 'mother' is the path-condition and 'multi-mother'
 * is the input parameter list.
 */
class UFUN_node: public arith_node, public DllistNode{
	const int callsite;
	static int CALLSITES;
	int nbits;	
	string ufname;
	//string name;
	bool isDependent;
	public: 
	bool ignoreAsserts;
	
	string outname;
	int fgid;
		
		UFUN_node(const string& p_ufname):arith_node(UFUN), ufname(p_ufname), callsite(CALLSITES++), ignoreAsserts(false), isDependent(false){ 
			nbits=1; 
		} 
		UFUN_node(const UFUN_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), nbits(bn.nbits), ufname(bn.ufname), callsite(bn.callsite), outname(bn.outname), fgid(bn.fgid), ignoreAsserts(bn.ignoreAsserts), isDependent(bn.isDependent){ }  
	
		void makeDependent(){
			isDependent = true;
			ignoreAsserts = true;
		}
		bool dependent(){
			return isDependent;
		}

		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out)const{
    	int i=0;
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;	  		
		  	}
		}
		out<<" "<<mother->get_name()<<" -> "<<get_name()<<"[style=dotted] ; "<<endl;	  		
	}

	virtual bool_node* clone(bool copyChildren = true){return new UFUN_node(*this, copyChildren);  };
	int get_callsite()const{ return callsite; }
	int get_nbits() const { return nbits; }
	const string& get_ufname() const { return ufname; }
	void set_nbits(int n){ nbits = n; }
	void makeArr(){ 
		if(nbits>1){
			otype = INT_ARR;
		}else{
			otype = BOOL_ARR;
		}
	}
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
		  str<<ufname.substr(0, 5)<<id<<"__"<<get_tname()<<(ignoreAsserts?"_IA":"");
		  return str.str();
		}else{
			return arith_node::get_name();
	      
	    }
	    
	  }
	virtual string lprint()const{
		stringstream str;
		str<<id<<"= "<<ufname.substr(0, min<int>(25,ufname.length() ))<<"#"<<fgid<<"["<<mother->lid()<<"](";
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";	  		
		  	}
		}
		str<<")";
		return str.str();
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<ufname<<" "<<outname<<" "<<fgid;
		if(this->isDependent){
			str<<" ***";
		}else{
			str<<" "<<multi_mother.size();
			for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
		  		if(*it != NULL){
		  			str<<" "<<(*it)->id;
		  		}
			}
		}
		return str.str();
	}
};


/*mother is an index to the array, multi-mother is the array*/
class ARRACC_node: public arith_node{	
	public: ARRACC_node():arith_node(ARRACC){ }  
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
	virtual string lprint()const{
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
	virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id<<" "<<multi_mother.size();
			for(int i=0; i<multi_mother.size(); ++i){
				str<<" "<<multi_mother[i]->id;
			}
			return str.str();
		}
	virtual bool_node* clone(bool copyChildren = true){return new ARRACC_node(*this, copyChildren);  };
};
class DIV_node: public bool_node{	
	public: 
		DIV_node():bool_node(DIV){ }
		DIV_node(const DIV_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }     
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new DIV_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
};
class MOD_node: public bool_node{	
	
	public: 
		MOD_node():bool_node(MOD){ }  
		MOD_node(const MOD_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new MOD_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
};


class NEG_node: public bool_node{	
	
	public: 
		NEG_node():bool_node(NEG){  }
		NEG_node(const NEG_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new NEG_node(*this, copyChildren);  };
		OutType getOtype()const {
			return INT;
		}
		virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id;			
			return str.str();
		}
};


class CONST_node: public bool_node{
	int val;
	public:
		CONST_node():bool_node(CONST){  
			val = -1;}
		CONST_node(int n):bool_node(CONST){  
			val = n;}
		CONST_node(const CONST_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), val(bn.val){
			//if(val == 13){ cout<<" surprise"<<endl; }
		}  
		virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
		virtual void setVal(int v){ val = v; }
		virtual int getVal() const { return val; }
		string get_name() const{
		    stringstream str;
		    str<<"C";
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
		virtual string lprint()const{
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
		virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<val;			
			return str.str();
		}
};

class LT_node: public bool_node{	
	public: 
		LT_node():bool_node(LT){}  
		LT_node(const LT_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new LT_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};

class EQ_node: public bool_node{	
	public: 
		EQ_node():bool_node(EQ){  } 
		EQ_node(const EQ_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }  
		virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
		virtual bool_node* clone(bool copyChildren = true){return new EQ_node(*this, copyChildren);  };
		OutType getOtype()const {
			return BOOL;
		}
};

/*!
    Array assignment node.   
 
    multi-mother[0] = old-value;
    multi-mother[1] = new-value;
    if( mother == quant ) return multi-mother[1]; else return multi-mother[0];		
   
    e.g. A = (a, b, c)
    A[i] = 6
	'mother' is assigned index i. In this case, there will be three nodes
(quant, multi-mother[0], multi_mother[1]) -- {(0, a, 6), (1, b, 6), (2, c, 6)}.
*/
class ARRASS_node: public arith_node{		
	public: 
		int quant;
		ARRASS_node():arith_node(ARRASS){ quant = -1; }
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
		virtual string lprint()const{
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
		virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<mother->id<<" == "<<quant;
			str<<" "<<multi_mother[0]->id;
			str<<" "<<multi_mother[1]->id;			
			return str.str();
		}
};

/* This node typecasts bit to integers. The only thing is used is 'multi-mother'
 * where it stores all the input bits.
 */
class ACTRL_node: public arith_node{	
	public: ACTRL_node():arith_node(ACTRL){ }  
	ACTRL_node(const ACTRL_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }  
	virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }	
	virtual bool_node* clone(bool copyChildren = true){return new ACTRL_node(*this, copyChildren);  };
	OutType getOtype()const {
		return INT;
	}	
	virtual string lprint()const{
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

	virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<otypeString()<<" "<<multi_mother.size();
			for(int i=0; i<multi_mother.size(); ++i){
				str<<" "<<multi_mother[i]->id;
			}
			return str.str();
		}
};
class ASSERT_node: public bool_node, virtual public DllistNode{
	bool isHardAssert;
	string msg;
public:
    ASSERT_node ():bool_node(ASSERT), isHardAssert(false) { }
    ASSERT_node(const ASSERT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ isHardAssert = bn.isHardAssert;  msg = bn.msg; }  
    virtual void accept (NodeVisitor &visitor)  { visitor.visit (*this); }
    virtual bool_node* clone(bool copyChildren = true) {return new ASSERT_node(*this, copyChildren);  };
    virtual void makeHardAssert(){ isHardAssert = true; }
    virtual bool isHard() const { return isHardAssert ; }
    virtual void setMsg(const string& pmsg){ msg = pmsg; }
    virtual const string& getMsg()const{ return msg; }
	virtual ~ASSERT_node(){ 
	}
	  virtual string lprint()const{
		stringstream str;
		str<<id<<"= ASSERT";
		if (isHard()) {
			str << "hard";
		}
		str << ' ';
		
		str<<mother->lid()<<" : "<<msg;		
		return str.str();
  }
  virtual string mrprint()const{
			stringstream str;
			str<<id<<" = "<<get_tname()<<" "<<mother->id<<" \""<<msg<<"\"";
			return str.str();
  }
};



inline bool_node* newNode( bool_node::Type type){
	switch(type){
		case bool_node::AND: return new AND_node();
		case bool_node::OR: return new OR_node();
		case bool_node::XOR: return new XOR_node();
		case bool_node::SRC: return new SRC_node();
		case bool_node::DST: return new DST_node();
		case bool_node::NOT: return new NOT_node();
		case bool_node::CTRL: return new CTRL_node();
		case bool_node::ASSERT: return new ASSERT_node ();
		case bool_node::PLUS: return new PLUS_node();
		case bool_node::TIMES: return new TIMES_node();
		case bool_node::DIV: return new DIV_node();
		case bool_node::MOD: return new MOD_node();
		case bool_node::NEG: return new  NEG_node();
		case bool_node::CONST: return new CONST_node();		
		case bool_node::LT: return new LT_node();
		case bool_node::EQ: return new EQ_node();
		case bool_node::ARRACC: return new ARRACC_node();
		case bool_node::UFUN: return new UFUN_node("NULL");
		case bool_node::ARRASS: return new ARRASS_node();
		case bool_node::ACTRL: return new ACTRL_node();	
		case bool_node::ARR_R: return new ARR_R_node();	
		case bool_node::ARR_W: return new ARR_W_node();	
		case bool_node::ARR_CREATE: return new ARR_CREATE_node();
	}
	return NULL;
}




inline bool isDllnode(bool_node* bn){
	if(bn->type == bool_node::ASSERT || bn->type==bool_node::DST){
		return true;
	}
	UFUN_node* uf = dynamic_cast<UFUN_node*>(bn);
	if(uf != NULL){
		return !uf->ignoreAsserts;
	}
	return false;
}

inline bool isUFUN(DllistNode* dn){
	bool_node* t = dynamic_cast<bool_node*>(dn);
	return t->type == bool_node::UFUN;
}

inline DllistNode* getDllnode(bool_node* bn){
	return dynamic_cast<DllistNode*>(bn);
}


#endif

