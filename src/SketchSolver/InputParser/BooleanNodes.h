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


extern const int UNINITIALIZED;

//#define SCHECKMEM

class OutType{
    public:
	const bool isArr;
	const bool isTuple;
	static OutType* BOTTOM;
	static OutType* BOOL;
	static OutType* INT;
	static OutType* FLOAT;
	static OutType* BOOL_ARR;
	static OutType* INT_ARR;
	static OutType* FLOAT_ARR;
	static vector<OutType*> store;
    static map<string, OutType*> tupleMap;
	virtual string str() const=0;
	// StringHTable2<OutType*> tuples;
	OutType(bool _isArr, bool _isTuple);
	static OutType* joinOtype(OutType* t1, OutType* t2);
    static OutType* makeTuple(const string&, vector<OutType*>& elems, int actFields);
	static OutType* makeTuple(vector<OutType*>& elems);
    static OutType* getTuple(const string& name);
};


class Bottom: public OutType{public: Bottom(): OutType(false, false){}  string str() const{ return "BOTTOM"; }  };

class Bool: public OutType{public: Bool(): OutType(false, false){} string str() const{ return "BOOL"; }};

class Int: public OutType{public: Int(): OutType(false, false){} string str() const{ return "INT"; }};

class Float: public OutType{ public: Float(): OutType(false, false){} string str() const{ return "FLOAT"; }};

class Arr: public OutType{public:  OutType * atype; int arrSize;
	Arr(OutType* type):OutType(true, false), atype(type){}
	string str() const { return atype->str() + "_ARR"; }
};

class Tuple: public OutType{
    public:
    string name;
	OutType* arr;
  int actSize;
	Tuple():OutType(false, true){ arr = new Arr(this); }
	vector<OutType*> entries;
	string str() const { return name; }
};

inline OutType::OutType(bool _isArr, bool _isTuple): isArr(_isArr), isTuple(_isTuple){
	
}






struct bool_node{
    
    private:
    /** The unique ID to be assigned to the next bool_node created. */
    static int NEXT_GLOBAL_ID;
#ifdef SCHECKMEM
	static set<bool_node*> allocated;
#endif
    
    public:
    typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL,PLUS, TIMES, DIV, MOD, NEG, CONST, LT, EQ, ASSERT, ARRACC, UFUN, ARRASS, ACTRL, ARR_R, ARR_W, ARR_CREATE, TUPLE_CREATE, TUPLE_R} Type;
    
    const Type type;
    int depth;
  
    protected:
    bool_node(Type t);
    bool_node(const bool_node& bn, bool copyChildren);
    
    public:
    
    
    bool isArrType(){
        OutType* ot = getOtype();
        return ot->isArr;
    }
    bool isArith(){
        return type == ARRACC || type == UFUN || type == ARRASS || type == ACTRL || type == ARR_W || type == ARR_CREATE || type == TUPLE_CREATE;
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
    
    
    mutable OutType* otype;
    bool_node* mother;
    bool_node* father;
    
    childset children;
    virtual ~bool_node();
    
    void resetId(){
        globalId = NEXT_GLOBAL_ID++;
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
            case LT: return "LT";
            case EQ: return "EQ";
            case ASSERT: return "ASSERT";
            case ARRACC: return "ARRACC";
            case UFUN: return "UFUN";
            case ACTRL: return "ACTRL";
            case ARRASS: return "ARRASS";
            case ARR_R: return "ARR_R";
            case ARR_W: return "ARR_W";
            case ARR_CREATE: return "ARR_CREATE";
            case TUPLE_CREATE: return "TUPLE_CREATE";
            case TUPLE_R: return "TUPLE_R";
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
            case LT: return "<";
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
    
    virtual void lprintSubDAG(ostream& out, set<const bool_node* >& s, int bnd)const;
    virtual void lprintSubDAG(ostream& out, int bnd)const{
        set<const bool_node* > s;
        lprintSubDAG(out, s, bnd);
    }
    
    virtual OutType* getOtype() const;
    
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id<<" "<<father->id;
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
	virtual void lprintSubDAG(ostream& out, set<const bool_node* >& s, int bnd)const;
	
	virtual OutType* getOtype()const;
};



class ARR_R_node: public bool_node{
	//mother = index
	//father = inputarr
	public:
    ARR_R_node(const ARR_R_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    ARR_R_node():bool_node(ARR_R){}
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new ARR_R_node(*this, copyChildren);  };
    OutType* getOtype() const{
        if(otype != OutType::BOTTOM){
            return otype;
        }
        OutType* ot = father->getOtype();
        if(ot == OutType::BOTTOM){
            otype = ot;
            return ot;
        }
        if(ot->isArr){
            otype = ((Arr*)ot)->atype;
            return otype;
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
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = multi_mother[0]->getOtype();
        if(otype==OutType::INT){
            otype = OutType::INT_ARR;
        }
        if(otype==OutType::BOOL){
            otype = OutType::BOOL_ARR;
        }
        if(otype==OutType::FLOAT){
            otype = OutType::FLOAT_ARR;
        }
        if(otype->isTuple){
            otype = ((Tuple*)otype)->arr;
        }
        otype = OutType::joinOtype(otype, multi_mother[1]->getOtype());
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id<<" "<<multi_mother[0]->id<<" "<<multi_mother[1]->id;
        return str.str();
    }
};

class ARR_CREATE_node:public arith_node{
	
	public:
    int dfltval;
    ARR_CREATE_node():arith_node(ARR_CREATE),dfltval(UNINITIALIZED){ }
    ARR_CREATE_node(const ARR_CREATE_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), dfltval(bn.dfltval){ }
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
        str<<"}("<<dfltval<<")";
        return str.str();
    }
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = OutType::BOTTOM;
        for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
            otype = OutType::joinOtype((*it)->getOtype(), otype);
        }
        if(otype == OutType::INT){
            otype = OutType::INT_ARR;
        }
        if(otype == OutType::BOOL){
            otype = OutType::BOOL_ARR;
        }
        if(otype == OutType::FLOAT){
            otype = OutType::FLOAT_ARR;
        }
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<multi_mother.size();
        for(int i=0; i<multi_mother.size(); ++i){
            str<<" "<<multi_mother[i]->id;
        }
        str<<" "<<dfltval;
        return str.str();
    }
};



class TUPLE_CREATE_node:public arith_node{
    public:
    string name;
	TUPLE_CREATE_node():arith_node(TUPLE_CREATE){ }
    TUPLE_CREATE_node(const TUPLE_CREATE_node& bn, bool copyChildren = true): arith_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){TUPLE_CREATE_node* newNode = new TUPLE_CREATE_node(*this, copyChildren); newNode->setName(name);return newNode;  };
    virtual void outDagEntry(ostream& out) const{
        int i=0;
		for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;
		  	}
		}		
    }
    virtual string lprint()const{
        
        stringstream str;
        str<<id<<"= "<<"<";
        for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
            if(*it != NULL){
                str<<(*it)->lid()<<", ";
            }
        }
        str<<">";
        return str.str();
    }
    void setName(string& n){name = n; }
    OutType* getOtype()const {
      if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = OutType::getTuple(name);
        
        return otype;
       /*vector<OutType*> tv;
        for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
            tv.push_back((*it)->getOtype());
        }
        
        otype = OutType::makeTuple(tv);
        return otype;*/
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<multi_mother.size();
        for(int i=0; i<multi_mother.size(); ++i){
            str<<" "<<multi_mother[i]->id;
        }
        return str.str();
    }
};



class TUPLE_R_node: public bool_node{
	//mother = inputarr
	public:
    int idx;
    TUPLE_R_node(const TUPLE_R_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), idx(bn.idx){ }
    TUPLE_R_node():bool_node(TUPLE_R), idx(-1){}
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new TUPLE_R_node(*this, copyChildren);  };
    OutType* getOtype() const{
       
        if(otype != OutType::BOTTOM){
            
            return otype;
        }
        
       OutType* ot = mother->getOtype();
       if(ot == OutType::BOOL){
            return ot;
        }
       Assert(ot->isTuple && idx >=0, "LWEKY");
       otype = ((Tuple*)ot)->entries[idx];
       Assert(otype != NULL, "dfq");
       return otype;
    }
    virtual string lprint()const{
        stringstream str;
        str<<id<<"= "<<mother->lid()<<"["<<idx<<"]";
        return str.str();
    }
    virtual string mrprint()const{
        stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id<<" "<<idx;
		return str.str();
    }
};





class AND_node: public bool_node{
	public:
    AND_node(const AND_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    AND_node():bool_node(AND){}
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new AND_node(*this, copyChildren);  };
    OutType* getOtype() const{
        return OutType::BOOL;
    }
};
class OR_node: public bool_node{
	public:
    OR_node():bool_node(OR){ }
    OR_node(const OR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new OR_node(*this, copyChildren);  };
    OutType* getOtype()const {
        return OutType::BOOL;
    }
};
class XOR_node: public bool_node{
	public:
    XOR_node():bool_node(XOR){ }
    XOR_node(const XOR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new XOR_node(*this, copyChildren);  };
    OutType* getOtype() const{
        return OutType::BOOL;
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
	
	OutType* getOtype() const {
		if(otype != OutType::BOTTOM){
			return otype;
		}
		if(nbits>1){
			otype = OutType::INT;
		}else{
			otype = OutType::BOOL;
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
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<name<<" "<<nbits;
		return str.str();
	}
};

/* Input nodes */
class SRC_node: public INTER_node{
    public: SRC_node():INTER_node(SRC){isTuple = false; }
    int arrSz;
    bool isTuple;
    string tupleName;
    bool ufun;
	SRC_node(const SRC_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren), arrSz(bn.arrSz), isTuple(bn.isTuple), tupleName(bn.tupleName), ufun(bn.ufun) { }
	SRC_node(const string& nm):INTER_node(SRC), arrSz(-1), ufun(false){
		name = nm;
		isTuple = false;
	}
	int getArrSz()const{
		return arrSz;
	}
	void setArr(int sz){
		arrSz = sz;
		if(sz>=0){
			if(otype == OutType::INT){
				otype = OutType::INT_ARR;
			}
			if(otype == OutType::BOOL){
				otype = OutType::BOOL_ARR;
			}
		}
	}
	bool isArr() const{
		return arrSz >= 0;
	}
   void setTuple (const string& name, bool ufun_ = false) {
        tupleName = name;
        isTuple = true;
        ufun = ufun_;
   }
	OutType* getOtype() const {
        
		if(otype != OutType::BOTTOM){
            
			return otype;
		}
        if (isTuple) {return  OutType::getTuple(tupleName); }
		INTER_node::getOtype();
		if(!isArr()){ return otype; }
		if(otype == OutType::INT){
			otype = OutType::INT_ARR;
			return otype;
		}
		if(otype == OutType::BOOL){
			otype = OutType::BOOL_ARR;
			return otype;
		}
		Assert(false, "NYI;lkkjkjytrdd");
		return otype;
	}
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){
        return new SRC_node(*this, copyChildren);};
};

/* Output Node */
class DST_node: public INTER_node, public DllistNode{
	public:
    DST_node():INTER_node(DST){ }
    DST_node(const DST_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new DST_node(*this, copyChildren); };
    virtual ~DST_node(){}
	virtual string lprint()const{
		string tmp =  name;
		tmp += " = " + mother->lprint();
		return tmp;
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<name<<" "<<mother->id;
		return str.str();
	}
};


class CTRL_node: public INTER_node{
	typedef enum{MINIMIZE=1, ANGELIC=2, PCOND=4} Property;
	unsigned kind;
	int arrSz;
  bool spConcretize;
    
	public:
    bool isTuple;
    string tupleName;
    bool spAngelic;
    int max;
    vector<string> parents;
	
    CTRL_node(bool toMinimize = false):INTER_node(CTRL),kind(0),arrSz(-1),spAngelic(false), spConcretize(false), max(-1){  if(toMinimize){ this->kind = MINIMIZE;}  isTuple = false; }
	CTRL_node(unsigned kind_):INTER_node(CTRL),arrSz(-1),spAngelic(false), spConcretize(false), max(-1) {  this->kind = kind; isTuple = false;}
	CTRL_node(const CTRL_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren), isTuple(bn.isTuple), tupleName(bn.tupleName), spAngelic(bn.spAngelic), spConcretize(bn.spConcretize), max(bn.max) {
		this->kind = bn.kind; this->arrSz = bn.arrSz; 
		
	}
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new CTRL_node(*this, copyChildren);};
	string get_name() const {
		return name;
	}
  void setParents(const vector<string>& parents_) {
    parents = parents_;
  }
  void special_concretize(int max_) {
    spConcretize = true;
    max = max_;
  }
  
  bool is_sp_concretize() {
    return spConcretize;
  }
    void setTuple (const string& name) {
        tupleName = name;
        isTuple = true;
    }
	bool get_toMinimize() const {
		return (kind & MINIMIZE) != 0;
	}
	void set_toMinimize(bool toMinimize) {
		if(toMinimize){ this->kind |= MINIMIZE;}
		else{ this->kind &= ~MINIMIZE; }
	}
    
	bool get_Angelic() const {
		return (kind & ANGELIC) != 0;
	}
	void set_Angelic() {
		this->kind |= ANGELIC;
	}
  void set_Special_Angelic() {
		this->kind |= ANGELIC;
    spAngelic = true;
	}
    
	bool get_Pcond() const {
		return (kind & PCOND) != 0;
	}
	void set_Pcond() {
		this->kind |= PCOND;
	}
    
	int getArrSz()const{
		return arrSz;
	}
	void setArr(int sz){
		arrSz = sz;
		if(sz>=0){
			if(otype == OutType::INT){
				otype = OutType::INT_ARR;
			}
			if(otype == OutType::BOOL){
				otype = OutType::BOOL_ARR;
			}
		}
	}
	bool isArr() const{
		return arrSz >= 0;
	}
	OutType* getOtype() const {
		if(otype != OutType::BOTTOM){
			return otype;
		}
        if (isTuple) {return  OutType::getTuple(tupleName); }
		INTER_node::getOtype();
		if(!isArr()){ return otype; }
		if(otype == OutType::INT){
			otype = OutType::INT_ARR;
			return otype;
		}
		if(otype == OutType::BOOL){
			otype = OutType::BOOL_ARR;
			return otype;
		}
		Assert(false, "NYI; egewajyt");
		return otype;
	}
};

class NOT_node: public bool_node{
	public:
	NOT_node():bool_node(NOT){ }
	NOT_node(const NOT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new NOT_node(*this, copyChildren);  };
	OutType* getOtype() const {
        return OutType::BOOL;
	}
	virtual string lid(){
		stringstream str;
		str<<"(!"<<mother->lid()<<")";
		return str.str();
    }
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id;
		return str.str();
	}
    
};

class PLUS_node: public bool_node{
	public: PLUS_node():bool_node(PLUS){ }
	PLUS_node(const PLUS_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new PLUS_node(*this, copyChildren);  };
	OutType* getOtype()const {
        if(otype == OutType::BOTTOM){
            otype = OutType::joinOtype(OutType::joinOtype(mother->getOtype(), father->getOtype()), OutType::INT);
        }
        return otype;
	}
};
class TIMES_node: public bool_node{
	public: TIMES_node():bool_node(TIMES){  }
	TIMES_node(const TIMES_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new TIMES_node(*this, copyChildren);  };
	OutType* getOtype()const {
		if(otype == OutType::BOTTOM){
            otype = OutType::joinOtype(OutType::joinOtype(mother->getOtype(), father->getOtype()), OutType::INT);
        }
        return otype;
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
    string tupleName;
	//string name;
	bool isDependent;
  bool hardAssert;
	public:
	bool ignoreAsserts;
	string outname;
	int fgid;
  bool replaceFun;
    
    UFUN_node(const string& p_ufname):arith_node(UFUN), ufname(p_ufname), callsite(CALLSITES++), ignoreAsserts(false), hardAssert(false), isDependent(false), replaceFun(true) {
        nbits=1;
    }
    UFUN_node(const UFUN_node& bn, bool copyChildren = true): arith_node(bn, copyChildren), nbits(bn.nbits), ufname(bn.ufname), callsite(bn.callsite), outname(bn.outname), fgid(bn.fgid), ignoreAsserts(bn.ignoreAsserts), hardAssert(bn.hardAssert), isDependent(bn.isDependent), replaceFun(bn.replaceFun){ }
	
    void modify_ufname(string& name) {
      ufname = name;
    }
    void makeDependent(){
        isDependent = true;
        ignoreAsserts = true;
    }
    bool dependent() const{
        return isDependent;
    }
  
    void makeAssertsHard() {
      hardAssert = true;
    }
  
    bool hardAsserts() const {
      return hardAssert;
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
    
	virtual bool_node* clone(bool copyChildren = true){UFUN_node* newNode = new UFUN_node(*this, copyChildren); newNode->set_tupleName(tupleName); return newNode; };
	int get_callsite()const{ return callsite; }
	int get_nbits() const { return nbits; }
	const string& get_ufname() const { return ufname; }
	void set_nbits(int n){ nbits = n; }
    void set_tupleName(string& name){tupleName = name;}
    const string& getTupleName() const {return tupleName;}
	void makeArr(){
        /* todo tuple array */
		if(nbits>1){
			otype = OutType::INT_ARR;
		}else{
			otype = OutType::BOOL_ARR;
		}
	}
	bool isArr() const {
		return otype == OutType::INT_ARR || otype == OutType::BOOL_ARR;
	}
    
	OutType* getOtype()const {
		if(otype != OutType::BOTTOM){
			return otype;
		}
        if(nbits == 0){
            otype = OutType::getTuple(tupleName);
            //Assert(dynamic_cast<Tuple*>(otype)->entries.size()>0, "fdasd");
        }else
		if(nbits>1){
			otype = OutType::INT;
		}else{
			otype = OutType::BOOL;
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
		if(this->ignoreAsserts){
			str<<" $IA";
		}
		return str.str();
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<ufname<<" "<<outname<<" "<<fgid;
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
	public:
  ARRACC_node():arith_node(ARRACC){ }
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
	OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        for(vector<bool_node*>::const_iterator it = multi_mother.begin(); it != multi_mother.end(); ++it){
            otype = OutType::joinOtype((*it)->getOtype(), otype);
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
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id<<" "<<multi_mother.size();
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
    OutType* getOtype()const {
        if(otype == OutType::BOTTOM){
            otype = OutType::joinOtype(OutType::joinOtype(mother->getOtype(), father->getOtype()), OutType::INT);
        }
        return otype;
    }
};
class MOD_node: public bool_node{
	
	public:
    MOD_node():bool_node(MOD){ }
    MOD_node(const MOD_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new MOD_node(*this, copyChildren);  };
    OutType* getOtype()const {
        return OutType::INT;
    }
};


class NEG_node: public bool_node{
	public:
    NEG_node():bool_node(NEG){  }
    NEG_node(const NEG_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new NEG_node(*this, copyChildren);  };
    OutType* getOtype()const {
        if(otype == OutType::BOTTOM){
            otype = OutType::joinOtype(mother->getOtype(), OutType::INT);
        }
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id;
        return str.str();
    }
};

typedef union{
	int val;
	double fval;
} NumType;

class CONST_node: public bool_node{
	NumType	v;
	bool isInt;
	public:
    CONST_node():bool_node(CONST), isInt(true){
        depth = 0;
        v.val = -1;}
    CONST_node(int n):bool_node(CONST), isInt(true){
        depth = 0;
        v.val = n;}
    CONST_node(double d):bool_node(CONST), isInt(false){
        depth = 0;
        v.fval = d;}
    CONST_node(const CONST_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), v(bn.v), isInt(bn.isInt){
        //if(val == 13){ cout<<" surprise"<<endl; }
        depth = 0;
    }
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    void setVal(int n){ v.val = n; }
    int getVal() const { return v.val; }
    bool isFloat() const { return !isInt; }
    double getFval() const { return v.fval; }
    static long long int code(double d){
        if(d<1e-100 && d > -1e-100){
            return 0;
        }
        if(d>0.0){
            double lg = log10(d);
            return (((long long int)(lg*100000000))<<2 & (-1l)) | 1;
        }else{
            double lg = log10(-d);
            return ((long long int)(lg*100000000))<<2 | 3;
        }
    }
    long long int getCode() const {
        if(isInt){
            return v.val <<1;
        }else{
            return code(v.fval);
        }
    }
    
    string get_name() const{
        stringstream str;
        str<<"C";
        if( v.val<0){
            str<<"m";
        }
        str<<abs(v.val);
        return str.str();
    }
    virtual string lid(){
        stringstream str;
        if(isInt){
            str<<"(" << v.val << ")";
        }else{
            str<<"(" << v.fval << ")";
        }
        return str.str();
    }
    virtual string lprint()const{
        stringstream str;
        if(isInt){
            str<<id<<"= "<<"(" << v.val << ")";
        }else{
            str<<id<<"= "<<"(" << v.fval << ")";
        }
        return str.str();
    }
    virtual bool_node* clone(bool copyChildren = true){return new CONST_node(*this, copyChildren);  };
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        if(!isInt){
            otype = OutType::FLOAT;
        }else{
            if(v.val != 0 && v.val != 1){
                otype = OutType::INT;
            }else{
                otype = OutType::BOOL;
            }
        }
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        if(isInt){
            str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<v.val;
        }else{
            str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<v.fval;
        }
        return str.str();
    }
};

class LT_node: public bool_node{
	public:
    LT_node():bool_node(LT){}
    LT_node(const LT_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new LT_node(*this, copyChildren);  };
    OutType* getOtype()const {
        return OutType::BOOL;
    }
};

class EQ_node: public bool_node{
	public:
    EQ_node():bool_node(EQ){  }
    EQ_node(const EQ_node& bn,bool copyChildren = true): bool_node(bn, copyChildren){ }
    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new EQ_node(*this, copyChildren);  };
    OutType* getOtype()const {
        return OutType::BOOL;
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
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = OutType::joinOtype(multi_mother[0]->getOtype(), multi_mother[1]->getOtype());
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother->id<<" == "<<quant;
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
	OutType *getOtype()const {
		return OutType::INT;
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
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<multi_mother.size();
        for(int i=0; i<multi_mother.size(); ++i){
            str<<" "<<multi_mother[i]->id;
        }
        return str.str();
    }
};
class ASSERT_node: public bool_node, virtual public DllistNode{
	typedef enum{Normal, Hard, Assume} AssertType;
	AssertType assertType;
	string msg;
    public:
    ASSERT_node ():bool_node(ASSERT), assertType(Normal) { }
    ASSERT_node(const ASSERT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ assertType = bn.assertType;  msg = bn.msg; }
    virtual void accept (NodeVisitor &visitor)  { visitor.visit (*this); }
    virtual bool_node* clone(bool copyChildren = true) {return new ASSERT_node(*this, copyChildren);  };
    virtual void makeHardAssert(){ assertType = Hard; }
	virtual void makeAssume(){ assertType = Assume; }
    virtual bool isHard() const { return assertType == Hard ; }
	virtual bool isNormal() const { return assertType == Normal ; }
	virtual bool isAssume() const { return assertType == Assume ; }
    virtual void setMsg(const string& pmsg){ msg = pmsg; }
    virtual const string& getMsg()const{ return msg; }
	virtual ~ASSERT_node(){ 
	}
    virtual string lprint()const{
		stringstream str;
		switch(assertType){
			case Normal: str<<id<<"= ASSERT "; break;
			case Hard: str<<id<<"= HASSERT "; break;
			case Assume: str<<id<<"= Assume "; break;
		}		
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
		case bool_node::TUPLE_CREATE: return new TUPLE_CREATE_node();
		case bool_node::TUPLE_R: return new TUPLE_R_node();
	}
	return NULL;
}




inline bool isDllnode(bool_node* bn){
	if(bn->type == bool_node::ASSERT || bn->type==bool_node::DST){
		return true;
	}
	if(bn->type != bool_node::UFUN){
		return false;
	}
	UFUN_node* uf = (UFUN_node*)(bn);
	
	return !uf->ignoreAsserts;
	
}

inline bool isUFUN(DllistNode* dn){
	bool_node* t = dynamic_cast<bool_node*>(dn);
	return t->type == bool_node::UFUN;
}

inline DllistNode* getDllnode(bool_node* bn){
	return dynamic_cast<DllistNode*>(bn);
}


#endif

