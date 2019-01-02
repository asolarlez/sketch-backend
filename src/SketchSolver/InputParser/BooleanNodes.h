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
#include <algorithm>


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






class bool_node{
    
    private:
    /** The unique ID to be assigned to the next bool_node created. */
    static int NEXT_GLOBAL_ID;
#ifdef SCHECKMEM
	static set<bool_node*> allocated;
#endif
	protected:
	bool_node** parents;
	size_t numparents;
	

    public:
    typedef enum{AND, OR, XOR, SRC, DST, NOT, CTRL,PLUS, TIMES, DIV, MOD, NEG, CONST, LT, EQ, ASSERT, ARRACC, UFUN, ARRASS, ACTRL, ARR_R, ARR_W, ARR_CREATE, TUPLE_CREATE, TUPLE_R} Type;
	typedef bool_node** parent_iter;

    const Type type;
    int depth;
  
	inline bool_node* mother() const{
		Assert(numparents >= 1, ";lkhyoyui");
		return parents[0];
	}
	inline bool_node* father() const{
		Assert(numparents >= 2, ";ihjkmm,");
		return parents[1];
	}
	inline bool_node*& mother() {
		Assert(numparents >= 1, ";zmxcnui");
		return parents[0];
	}

	inline bool hasFather() const{
		return numparents >= 2;
	}

	inline bool_node*& father() {
		Assert(numparents >= 2, ";98hujn,m");
		return parents[1];
	}
	inline size_t nparents() const {
		return numparents;
	}
	inline parent_iter p_begin() const{
		return parents;
	}
	inline parent_iter p_end() const{
		return parents + numparents;
	}
	inline void set_parent(int i, bool_node* val) {
		Assert(numparents > i, ";mnip;jadyhj");
		parents[i] = val;
	}
	inline bool_node* get_parent(int i) {
		Assert(numparents > i, ";mnip;jadyhj");
		return parents[i];
	}

	template<typename F>
	inline void forallparents(F f) {
		bool_node** parent = parents;
		for (int i = 0; i < numparents; ++i) {
			f(*parent);
			++parent;
		}
	}


    protected:
    bool_node(Type t, size_t nparents);
    bool_node(const bool_node& bn, bool copyChildren);
    
    public:
    
    
    bool isArrType(){
        OutType* ot = getOtype();
        return ot->isArr;
    }
	/*
    bool isArith(){
        return type == ARRACC || type == UFUN || type == ARRASS || type == ACTRL || type == ARR_W || type == ARR_CREATE || type == TUPLE_CREATE;
    }
	*/
    
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
	virtual void addToParent(bool_node* only_thisone);
  
    
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
	string getSMTnode(OutType* ot_needed){
		//We want to have ot_needed type for this node
		//compare with it's current type and do appropriate type-casting
		//in SMT: ideally this should just return _n<id>
		OutType* ot_current =getOtype();
		string base = " _n" + int2str(id) + " ";
		if(ot_current == ot_needed){
			return base;
		}
		else if(ot_current == OutType::BOOL && ot_needed == OutType::INT){
			return " (ite " +base + " 1 0) ";
		}
		else if(ot_current == OutType::INT && ot_needed == OutType::BOOL){
			return " (ite (> " +base + " 0) true false) ";
		}
		else if(ot_current == OutType::INT && ot_needed == OutType::FLOAT){
			return " (to_real " +base + ") ";
		}
		else if(ot_current == OutType::BOOL && ot_needed == OutType::FLOAT){
			return " (to_real (ite " +base+" 1 0)) ";
		}
		else if(type==bool_node::CONST && ot_needed == OutType::INT_ARR){
			OutType* ot_temp = getOtype();
			otype = OutType::INT;
			string ret = "((as const (Array Int Int)) "+ this->smtletprint() +")";
			otype = ot_temp;
			return ret;
		}
		else if(type==bool_node::CONST && ot_needed == OutType::BOOL_ARR){
			OutType* ot_temp = getOtype();
			otype = OutType::BOOL;
			//string ret = "((as const (Array Int Bool)) "+ this->smtletprint() +")";
			string ret = "((as const (Array Int Int)) "+ this->smtletprint() +")";
			otype = ot_temp;
			return ret;
		}
		else if(type==bool_node::CONST && ot_needed == OutType::FLOAT_ARR){
			OutType* ot_temp = getOtype();
			otype = OutType::FLOAT;
			string ret = "((as const (Array Int Real)) "+ this->smtletprint() +")";
			otype = ot_temp;
			return ret;
		}
		else if(ot_current == OutType::BOOL_ARR && ot_needed == OutType::INT_ARR){
			return base;
		}
		else Assert(false, "Type conversion either not supported or implemented: " + ot_current->str() + " -> " + ot_needed->str());

	}
	virtual string get_smtop() const{
        switch(type){
            case PLUS: return "+";
            case TIMES: return "*";
            case DIV: return "div";
            case MOD: return "mod";
            case NEG: return "-";
            case CONST: return "CONST";
            case AND: return "and";
            case OR: return "or";
            case XOR: return "xor";
            case SRC: return "S";
            case DST: return "D";
            case NOT: return "not";
            case CTRL: return "CTRL";
            case LT: return "<";
            case EQ: return "=";
            case ASSERT: return "assert";
            case ARRACC: return "ite"; //but with different order of m,f
            case UFUN: return "UF";
            case ACTRL: return "ACTRL";
            case ARRASS: return "ite"; //different order of m,f and equality constraint
            case ARR_R: return "select"; //order of m,f swapped
            case ARR_W: return "store"; //order of first two parents swapped
            case ARR_CREATE: return "ARRC";
            case TUPLE_CREATE: return "TUPC";
            case TUPLE_R: return "TUPR";
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
			default: throw BasicError("Err", "Err");
        }
        //cout<<"ABOUT TO ABORT BECAUSE OF "<<name<<"  "<<type<<endl;
        throw BasicError("Err", "Err");
    }
    virtual string lprint()const{
		stringstream str;
		str<<id<<"= ";

		if(mother() != NULL){
			str<<mother()->lid()<<" ";
		}
		str<<get_sym()<<" ";
		if(hasFather()){
			str<<father()->lid()<<" ";
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
	virtual string smtletprint(){
		//Default print for binary ops
		stringstream ss;
		string op = get_smtop();
		ss<<" ("<<op<<" ";
		OutType* otm;
		OutType* otf;
		if(op == "+" || op == "*" || op == "div" || op == "mod" || op =="<"){ otm= OutType::INT; otf = otm; }
		else if(op == "and" || op == "or" || op == "xor") { otm= OutType::BOOL; otf = otm; }
		else if (op == "select"){ 
			otm= OutType::INT; 
			otf = getOtype();
			if(otf==OutType::BOOL) otf = OutType::BOOL_ARR;
			else if(otf==OutType::INT) otf = OutType::INT_ARR;
			else if(otf==OutType::FLOAT) otf = OutType::FLOAT_ARR;
			else Assert(false,"other ARR_R otypes cannot be interpreted as arrays!");
		}
		else if (op == "="){ OutType* ot_temp = OutType::joinOtype(mother()->getOtype(),father()->getOtype()); otm = ot_temp; otf = ot_temp; }
		else Assert(false,"Common smtletprint shouldn't be called for this operation: " + op);
		string msmt = mother()->getSMTnode(otm);
		string fsmt = father()->getSMTnode(otf);
		if(op == "select") {//TODO: Make sure all special nodes have their print functions!
			ss<<" "<<fsmt<<" "<<msmt;
		}
		else ss<<" "<<msmt<<" "<<fsmt;
		return ss.str() + ") ";
	}
	virtual string getSMTOtype(){
		otype=getOtype();
		if(otype == OutType::INT){
			return " Int ";
			
		}
		else if(otype  == OutType::BOOL){
			return " Bool ";
		}
		else if (otype == OutType::BOOL_ARR){
			return " (Array Int Int) ";
			//return " (Array Int Bool) ";
		}
		else if (otype == OutType::INT_ARR){
			return " (Array Int Int) ";
		}
		else if (otype == OutType::FLOAT){
			return " Real ";
		}
		else if (otype == OutType::FLOAT_ARR){
			return " (Array Int Float) ";
		}
		else{
			Assert(false,"OutType not identified!");
			return "";
		}
	}

	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother()->id<<" "<<father()->id;
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




class ARR_R_node: public bool_node{
	//mother = index
	//father = inputarr
	private:
    ARR_R_node(const ARR_R_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    ARR_R_node():bool_node(ARR_R, 2){}

public:
	inline static ARR_R_node* create() {
		return new ARR_R_node();
	}
	inline static ARR_R_node* create(const ARR_R_node& bn, bool copyChildren = true) {
		return new ARR_R_node(bn, copyChildren);
	}


    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new ARR_R_node(*this, copyChildren);  };
    OutType* getOtype() const{
        if(otype != OutType::BOTTOM){
            return otype;
        }
        OutType* ot = father()->getOtype();
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
        str<<id<<"= "<<father()->lid()<<"["<<mother()->lid()<<"]";
        return str.str();
    }
};


/*!
 Array assignment node.
 
 multi-mother[0] = old-array;
 multi-mother[1] = new-value;
 
 */
class ARR_W_node:public bool_node{
private: 
	ARR_W_node() :bool_node(ARR_W, 3) { }
	ARR_W_node(const ARR_W_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }

public:
	inline static ARR_W_node* create(){
		return new ARR_W_node();
	}
	inline static ARR_W_node* create(const ARR_W_node& bn, bool copyChildren = true) {
		return new ARR_W_node(bn, copyChildren);
	}
	inline static ARR_W_node* create(bool_node* mother, bool_node* old_arr, bool_node* new_val) {
		ARR_W_node* rv = new ARR_W_node();
		rv->parents[0] = mother;
		rv->parents[1] = old_arr;
		rv->parents[2] = new_val;
		return rv;
	}

	bool_node*& getOldArr(){
        return parents[1];
    }
    bool_node*& getNewVal(){
        return parents[2];
    }
	bool_node* const& getOldArr() const{
		return parents[1];
	}
	bool_node* const& getNewVal() const{
		return parents[2];
	}

    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new ARR_W_node(*this, copyChildren);  };
    virtual void outDagEntry(ostream& out) const{
        if( mother() != NULL){
            out<<" "<<mother()->get_name()<<" -> "<<get_name()<<";" ;
        }
        
        if(getOldArr() != NULL){
            out<<" "<< getOldArr()->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;
        }
        if(getNewVal() != NULL){
            out<<" "<< getNewVal()->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
        }
    }
    virtual string lprint()const{
        stringstream str;
        str<<id<<"= "<< getOldArr()->lid()<<"{"<<mother()->lid()<<"->"<< getNewVal()->lid()<<"}";
        return str.str();
    }
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = getOldArr()->getOtype();
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
        otype = OutType::joinOtype(otype, getNewVal()->getOtype());
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother()->id<<" "<< getOldArr()->id<<" "<< getNewVal()->id;
        return str.str();
    }
	virtual string smtletprint(){//array index value in SMT, index array value in DAG
		stringstream ss;
		OutType* atype = getOtype();
		OutType* vtype;
		if(atype==OutType::BOOL_ARR) vtype = OutType::BOOL;
		else if(atype==OutType::INT_ARR) vtype = OutType::INT;
		else if(atype==OutType::FLOAT_ARR) vtype = OutType::FLOAT;
		else Assert(false,"other types for ARR_W node cannot be converted to arrays!");

		ss<<" (store "<< getOldArr()->getSMTnode(atype)<<" "<< mother()->getSMTnode(OutType::INT) <<" "<< getNewVal()->getSMTnode(vtype)<<") ";
		return ss.str();
	}
};

class ARR_CREATE_node:public bool_node{
	ARR_CREATE_node(const ARR_CREATE_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
	ARR_CREATE_node(int nvals) :bool_node(ARR_CREATE, nvals + 1) { }

	public:
	inline static ARR_CREATE_node* create(const ARR_CREATE_node& bn, bool copyChildren = true){
		return new ARR_CREATE_node(bn, copyChildren);
	}
	inline static ARR_CREATE_node* create(int nvals){
		return new ARR_CREATE_node(nvals);
	}
	inline static ARR_CREATE_node* create(vector<bool_node*>& multi_mother, bool_node* defval) {
		ARR_CREATE_node* rv= new ARR_CREATE_node(multi_mother.size());
		rv->mother() = defval;
		copy(multi_mother.begin(), multi_mother.end(), rv->arg_begin());
		return rv;
	}
		
		
	bool_node* getDfltval() const {
		Assert(mother() != NULL, "NOT NULL");
			return mother();
	}
	void setDfltval(bool_node* dfltval) {
		parents[0] = dfltval;
	}
    

	bool_node*& arguments(int i) {
		return parents[i + 1];
	}
	int nargs() const{
		return numparents - 1;
	}
	bool_node*const & arguments(int i) const {
		return parents[i + 1];
	}

	parent_iter arg_begin() const {
		return parents + 1;
	}
	parent_iter arg_end() const {
		return p_end();
	}

    
    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new ARR_CREATE_node(*this, copyChildren);  };
    virtual void outDagEntry(ostream& out) const{
        if(arguments(0) != NULL){
            out<<" "<<arguments(0)->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;
        }
        if(nargs()>1 && arguments(1) != NULL){
            out<<" "<< arguments(1)->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
        }
    }

    virtual string lprint()const{
        
        stringstream str;
        str<<id<<"= "<<"{";
		for (int i = 0; i < nargs(); ++i) {
            if(arguments(i) != NULL){
                str<<arguments(i)->lid()<<", ";
            }
        }
        str<<"}("<<getDfltval()->lid()<<")";
        return str.str();
    }
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = OutType::BOTTOM;
		for (int i = 0; i < nargs(); ++i) {
            otype = OutType::joinOtype(arguments(i)->getOtype(), otype);
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
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<< nargs();
        for(size_t i=0; i<nargs(); ++i){
            str<<" "<< arguments(i)->id;
        }
        str<<" "<< getDfltval()->lid();
        return str.str();
    }
	virtual string smtletprint(){
		//just create an array that is dfltval(0) by default and then store all these values!
		if(getOtype() == OutType::BOOL_ARR || getOtype() == OutType::INT_ARR){
			string base = "((as const (Array Int Int)) " + getDfltval()->lid() + ")";
			for(size_t i=0;i<nargs(); i++){
				base = "(store " + base + " " + int2str(i) + " " + arguments(i)->getSMTnode(OutType::INT) + ")";
			}
			return " " + base + " ";
		}
		else Assert(false, "ARR_CREATE SMT without BOOL_ARR or INT_ARR not supported: " + getOtype()->str());
		return "";
	}
};



class TUPLE_CREATE_node:public bool_node{
	TUPLE_CREATE_node(int nargs) :bool_node(TUPLE_CREATE, nargs) { }
	TUPLE_CREATE_node(const TUPLE_CREATE_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }

public:
	string name;

	inline static TUPLE_CREATE_node* create(int nargs){
		return new TUPLE_CREATE_node(nargs);
	}
	inline static TUPLE_CREATE_node* create(const TUPLE_CREATE_node& bn, bool copyChildren = true){
		return new TUPLE_CREATE_node(bn, copyChildren);
	}
	inline static TUPLE_CREATE_node* create(vector<bool_node*>& multi_mother) {
		TUPLE_CREATE_node* rv =  new TUPLE_CREATE_node(multi_mother.size());
		copy(multi_mother.begin(), multi_mother.end(), rv->p_begin());
		return rv;
	}



	virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){TUPLE_CREATE_node* newNode = new TUPLE_CREATE_node(*this, copyChildren); newNode->setName(name);return newNode;  };
    virtual void outDagEntry(ostream& out) const{
        int i=0;
		for(auto it = p_begin(); it != p_end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;
		  	}
		}		
    }
    virtual string lprint()const{
        
        stringstream str;
        str<<id<<"= "<<"<";
        for(auto it = p_begin(); it != p_end(); ++it){
            if(*it != NULL){
                str<<(*it)->lid()<<", ";
            }
        }
        str<<">";
        return str.str();
    }
    void setName(const string& n){name = n; }
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
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<nparents();
        for(size_t i=0; i<nparents(); ++i){
            str<<" "<<parents[i]->id;
        }
        return str.str();
    }
	virtual string smtletprint(){
		Assert(false, "TUPLE_CREATE must have been inlined SMT not supported");
		return "";
	}
};



class TUPLE_R_node: public bool_node{
	//mother = inputarr
	public:
    int idx;

private:
	TUPLE_R_node(const TUPLE_R_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren), idx(bn.idx) { }
	TUPLE_R_node() :bool_node(TUPLE_R, 1), idx(-1) {}
public:

	inline static TUPLE_R_node* create() {
		return new TUPLE_R_node();
	}
	inline static TUPLE_R_node* create(const TUPLE_R_node& bn, bool copyChildren = true) {
		return new TUPLE_R_node(bn, copyChildren);
	}
    
    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new TUPLE_R_node(*this, copyChildren);  };
    OutType* getOtype() const{
       
        if(otype != OutType::BOTTOM){
            
            return otype;
        }
        
       OutType* ot = mother()->getOtype();
       if(ot == OutType::BOOL || ot == OutType::INT){
        return OutType::BOOL;
       }
       Assert(ot->isTuple && idx >=0, "LWEKY");
       otype = ((Tuple*)ot)->entries[idx];
       Assert(otype != NULL, "dfq");
       return otype;
    }
    virtual string lprint()const{
        stringstream str;
        str<<id<<"= "<<mother()->lid()<<"["<<idx<<"]";
        return str.str();
    }
    virtual string mrprint()const{
        stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother()->id<<" "<<idx;
		return str.str();
    }
	virtual string smtletprint(){
		Assert(false, "TUPLE_R must have been inlined SMT not supported");
		return "";
	}
};





class AND_node: public bool_node{
private:
    AND_node(const AND_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
    AND_node():bool_node(AND, 2){}
public:
	inline static AND_node* create(const AND_node& bn, bool copyChildren = true){
		return new AND_node(bn, copyChildren);
	}
	inline static AND_node* create(){
		return new AND_node();
	}

	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new AND_node(*this, copyChildren);  };
    OutType* getOtype() const{
        return OutType::BOOL;
    }
};


class OR_node: public bool_node{
private:
    OR_node():bool_node(OR, 2){ }
    OR_node(const OR_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
public:
	inline static OR_node* create(){
		return new OR_node();
	}
	inline static OR_node* create(const OR_node& bn, bool copyChildren = true) {
		return new OR_node(bn, copyChildren);
	}

	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new OR_node(*this, copyChildren);  };
    OutType* getOtype()const {
        return OutType::BOOL;
    }
};
class XOR_node: public bool_node{
private:
	XOR_node() :bool_node(XOR, 2) { }
	XOR_node(const XOR_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static XOR_node* create(){ 
		return new XOR_node();
	}
	inline static XOR_node* create(const XOR_node& bn, bool copyChildren = true) {
		return new XOR_node(bn, copyChildren);
	}
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new XOR_node(*this, copyChildren);  };
    OutType* getOtype() const{
        return OutType::BOOL;
    }
};


class NOT_node : public bool_node {
private:
	NOT_node() :bool_node(NOT, 1) { }
	NOT_node(const NOT_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static NOT_node* create() {
		return new NOT_node();
	}
	inline static NOT_node* create(const NOT_node& bn, bool copyChildren = true) {
		return new NOT_node(bn, copyChildren);
	}
	
	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new NOT_node(*this, copyChildren); };
	OutType* getOtype() const {
		return OutType::BOOL;
	}
	virtual string lid() {
		stringstream str;
		str << "(!" << mother()->lid() << ")";
		return str.str();
	}
	virtual string mrprint()const {
		stringstream str;
		str << id << " = " << get_tname() << " " << getOtype()->str() << " " << mother()->id;
		return str.str();
	}
	virtual string smtletprint() {
		stringstream ss;
		ss << " (not " << mother()->getSMTnode(OutType::BOOL) << ") ";
		return ss.str();
	}
};

class PLUS_node : public bool_node {
private:
	PLUS_node() :bool_node(PLUS, 2) { }
	PLUS_node(bool_node* mother, bool_node*father) :bool_node(PLUS, 2) {
		this->father() = father;
		this->mother() = mother;
	}
	PLUS_node(const PLUS_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static PLUS_node* create() {
		return new PLUS_node();
	}
	inline static PLUS_node* create(const PLUS_node& bn, bool copyChildren = true) {
		return new PLUS_node(bn, copyChildren);
	}
	inline static PLUS_node* create(bool_node* mother, bool_node*father) {
		return new PLUS_node(mother, father);
	}

	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new PLUS_node(*this, copyChildren); };
	OutType* getOtype()const {
		if (otype == OutType::BOTTOM) {
			otype = OutType::joinOtype(OutType::joinOtype(mother()->getOtype(), father()->getOtype()), OutType::INT);
		}
		return otype;
	}
};
class TIMES_node : public bool_node {
private:
	TIMES_node() :bool_node(TIMES, 2) {  }
	TIMES_node(bool_node* mother, bool_node*father) :bool_node(TIMES, 2) {
		this->father() = father;
		this->mother() = mother;
	}
	TIMES_node(const TIMES_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static TIMES_node* create() {
		return new TIMES_node();
	}
	inline static TIMES_node* create(const TIMES_node& bn, bool copyChildren = true) {
		return new TIMES_node(bn, copyChildren);
	}
	inline static TIMES_node* create(bool_node* mother, bool_node*father) {
		return new TIMES_node(mother, father);
	}
	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new TIMES_node(*this, copyChildren); };
	OutType* getOtype()const {
		if (otype == OutType::BOTTOM) {
			otype = OutType::joinOtype(OutType::joinOtype(mother()->getOtype(), father()->getOtype()), OutType::INT);
		}
		return otype;
	}
};


class DIV_node : public bool_node {
private:
	DIV_node() :bool_node(DIV, 2) { }
	DIV_node(const DIV_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static DIV_node* create() {
		return new DIV_node();
	}
	inline static DIV_node* create(const DIV_node& bn, bool copyChildren = true) {
		return new DIV_node(bn, copyChildren);
	}

	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new DIV_node(*this, copyChildren); };
	OutType* getOtype()const {
		if (otype == OutType::BOTTOM) {
			otype = OutType::joinOtype(OutType::joinOtype(mother()->getOtype(), father()->getOtype()), OutType::INT);
		}
		return otype;
	}
};
class MOD_node : public bool_node {

private:
	MOD_node() :bool_node(MOD, 2) { }
	MOD_node(const MOD_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }

public:
	inline static MOD_node* create() {
		return new MOD_node();
	}
	inline static MOD_node* create(const MOD_node& bn, bool copyChildren = true) {
		return new MOD_node(bn, copyChildren);
	}

	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new MOD_node(*this, copyChildren); };
	OutType* getOtype()const {
		return OutType::INT;
	}
};

class LT_node : public bool_node {
private:
	LT_node() :bool_node(LT, 2) {}
	LT_node(const LT_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static LT_node* create() {
		return new LT_node();
	}
	inline static LT_node* create(const LT_node& bn, bool copyChildren = true) {
		return new LT_node(bn, copyChildren);
	}
	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new LT_node(*this, copyChildren); };
	OutType* getOtype()const {
		return OutType::BOOL;
	}
};

class EQ_node : public bool_node {
private:
	EQ_node() :bool_node(EQ, 2) {  }
	EQ_node(const EQ_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static EQ_node* create() {
		return new EQ_node();
	}
	inline static EQ_node* create(const EQ_node& bn, bool copyChildren = true) {
		return new EQ_node(bn, copyChildren);
	}
	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new EQ_node(*this, copyChildren); };
	OutType* getOtype()const {
		return OutType::BOOL;
	}
};




class NEG_node : public bool_node {
private:
	NEG_node() :bool_node(NEG, 1) {  }
	NEG_node(const NEG_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren) { }
public:
	inline static NEG_node* create() {
		return new NEG_node();
	}
	inline static NEG_node* create(const NEG_node& bn, bool copyChildren = true) {
		return new NEG_node(bn, copyChildren);
	}

	virtual void accept(NodeVisitor& visitor) { visitor.visit(*this); }
	virtual bool_node* clone(bool copyChildren = true) { return new NEG_node(*this, copyChildren); };
	OutType* getOtype()const {
		if (otype == OutType::BOTTOM) {
			otype = OutType::joinOtype(mother()->getOtype(), OutType::INT);
		}
		return otype;
	}
	virtual string mrprint()const {
		stringstream str;
		str << id << " = " << get_tname() << " " << getOtype()->str() << " " << mother()->id;
		return str.str();
	}

	virtual string smtletprint() {
		stringstream ss;
		ss << " (- " << mother()->getSMTnode(OutType::INT) << ")";
		return ss.str();
	}
};



/* Interface nodes, it includes input, output and control */
class INTER_node: public bool_node{
protected:

	INTER_node(const INTER_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren), name(bn.name), nbits(bn.nbits) { }
	INTER_node(Type t) :bool_node(t, 0) { nbits = 1; }
	INTER_node(Type t, int nin) :bool_node(t, nin) { nbits = 1; }
	int nbits;

public:
	string name;

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
public: 
    int arrSz;
    bool isTuple;
    string tupleName;
    bool ufun;

private:
	SRC_node() :INTER_node(SRC) { isTuple = false; }
	SRC_node(const SRC_node& bn, bool copyChildren = true): INTER_node(bn, copyChildren), arrSz(bn.arrSz), isTuple(bn.isTuple), tupleName(bn.tupleName), ufun(bn.ufun) { }
	SRC_node(const string& nm):INTER_node(SRC), arrSz(-1), ufun(false){
		name = nm;
		isTuple = false;
	}

public:
	static inline SRC_node* create(){
		return new SRC_node();
	}
	static inline SRC_node* create(const SRC_node& bn, bool copyChildren = true){
		return new SRC_node(bn, copyChildren);
	}
	static inline SRC_node* create(const string& nm){
		return new SRC_node(nm);
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
   void setTuple (const string& name_, bool ufun_ = false) {
        tupleName = name_;
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
	virtual string smtletprint(){
		return " " + get_name() + " ";
	}
	virtual bool_node* clone(bool copyChildren = true){
        return new SRC_node(*this, copyChildren);};
};

/* Output Node */
class DST_node: public INTER_node, public DllistNode{
private:
	DST_node() :INTER_node(DST, 1) { }
	DST_node(const DST_node& bn, bool copyChildren = true) : INTER_node(bn, copyChildren) { }
public:
	inline static DST_node* create(){
		return new DST_node();
	}
	inline static DST_node* create(const DST_node& bn, bool copyChildren = true){
		return new DST_node(bn, copyChildren);
	}

	
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new DST_node(*this, copyChildren); };
    virtual ~DST_node(){}
	virtual string lprint()const{
		string tmp =  name;
		tmp += " = " + mother()->lprint();
		return tmp;
	}
	virtual string mrprint()const{
		stringstream str;
		str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<name<<" "<<mother()->id;
		return str.str();
	}
	virtual string smtletprint(){
		return ""; //ignored
	}
};


class CTRL_node: public INTER_node{
	typedef enum{MINIMIZE=1, ANGELIC=2, PCOND=4} Property;
	unsigned kind;
	int arrSz;
	bool spConcretize;
public:
	bool isFloat;
	bool isTuple;
	string tupleName;
	bool spAngelic;
	int max;
	vector<string> predecessors;

private:
	CTRL_node(bool toMinimize = false) :INTER_node(CTRL), kind(0), arrSz(-1), spAngelic(false), spConcretize(false), max(-1), isFloat(false) { if (toMinimize) { this->kind = MINIMIZE; } }
	CTRL_node(unsigned kind_) :INTER_node(CTRL), arrSz(-1), spAngelic(false), spConcretize(false), max(-1), isFloat(false), isTuple(false) { this->kind = kind_; }
	CTRL_node(const CTRL_node& bn, bool copyChildren = true) : INTER_node(bn, copyChildren), spAngelic(bn.spAngelic), spConcretize(bn.spConcretize), max(bn.max), isFloat(bn.isFloat) {
		this->kind = bn.kind; this->arrSz = bn.arrSz;

	}
public:
	inline static CTRL_node* create(bool toMinimize = false){
		return new CTRL_node(toMinimize);
	}
	inline static CTRL_node* create(unsigned kind_) {
		return new CTRL_node(kind_);
	}
	inline static CTRL_node* create(const CTRL_node& bn, bool copyChildren = true){
		return new CTRL_node(bn, copyChildren);
	}

    
	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new CTRL_node(*this, copyChildren);};
	string get_name() const {
		return name;
	}
  void setPredecessors(const vector<string>& parents_) {
	  predecessors = parents_;
  }
  void special_concretize(int max_) {
    spConcretize = true;
    max = max_;
  }
  
  bool is_sp_concretize() {
    return spConcretize;
  }
  void setFloat() {
    isFloat = true;
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
    if (isFloat) {
      return OutType::FLOAT; // TODO: float array holes is not yet supported
    }
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
	virtual string smtletprint(){
		//defined the variable, add bounds as constraints
		stringstream ss;
		ss<<" "<<get_name()<<" ";//")) ";
		return ss.str();
	}
};


/* This node is used for both real and un-interpreted functions. In the case of
 * un-interpreted functions: 'mother' is the path-condition and 'multi-mother'
 * is the input parameter list.
 */
class UFUN_node: public bool_node, public DllistNode{
	const int callsite;
	static int CALLSITES;
	static int FGID;
	int nbits;
	string ufname;
    string tupleName;
	//string name;
	bool isDependent;
  bool hardAssert;
  int uniquefid;
	public:
	bool ignoreAsserts;
	int fgid;
	string outname;	
  bool replaceFun;
    
  bool_node*& arguments(int i) {
	  return parents[i + 1];
  }
  int nargs() const {
	  return numparents - 1;
  }

  parent_iter arg_begin() const {
	  return parents + 1;
  }
  parent_iter arg_end() const {
	  return p_end();
  }

  bool_node*const & arguments(int i) const {
	  return parents[i + 1];
  }
private:
	UFUN_node(const string& p_ufname, int n_args) :bool_node(UFUN, n_args + 1), ufname(p_ufname), callsite(CALLSITES++), ignoreAsserts(false), hardAssert(false), isDependent(false), replaceFun(true) {
		nbits = 1;
		uniquefid = FGID++;
	}
	UFUN_node(const UFUN_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren), uniquefid(bn.uniquefid), nbits(bn.nbits), ufname(bn.ufname), callsite(bn.callsite), outname(bn.outname), fgid(bn.fgid), ignoreAsserts(bn.ignoreAsserts), hardAssert(bn.hardAssert), isDependent(bn.isDependent), replaceFun(bn.replaceFun) { }

public:

	inline static UFUN_node* create(const string& p_ufname, int n_args) {
		return new UFUN_node(p_ufname, n_args);
	}

	inline static UFUN_node* create(const string& p_ufname, vector<bool_node*>& _args) {
		UFUN_node* uf = new UFUN_node(p_ufname, _args.size());
		copy(_args.begin(), _args.end(), uf->arg_begin());
		return uf;
	}
	inline static UFUN_node* create(const UFUN_node& bn, bool copyChildren = true) {
		return new UFUN_node(bn, copyChildren);
	}

    

	bool isSynNode() {
		return tupleName.substr(0, 5) == "_GEN_";
	}

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
		for(auto it = arg_begin(); it != arg_end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;
		  	}
		}
		out<<" "<<mother()->get_name()<<" -> "<<get_name()<<"[style=dotted] ; "<<endl;
	}
    
	virtual bool_node* clone(bool copyChildren = true){UFUN_node* newNode = new UFUN_node(*this, copyChildren); newNode->set_tupleName(tupleName); return newNode; };
	int get_callsite()const{ return callsite; }
	int get_uniquefid()const{ return uniquefid; }
	void set_uniquefid(){  uniquefid = ++FGID; }
	int get_nbits() const { 
		return nbits; 
	}
	const string& get_ufname() const { return ufname; }
	void set_nbits(int n){ nbits = n; }
    void set_tupleName(const string& name){tupleName = name;}
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
			return bool_node::get_name();
            
	    }
	    
    }
	virtual string lprint()const{
		stringstream str;
		str<<id<<"= "<<ufname.substr(0, min<int>(25,ufname.length() ))<<"#"<<fgid<<"["<<mother()->lid()<<"](";
		for(auto it = arg_begin(); it != arg_end(); ++it){
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
			str<<" "<<nargs();
			for(auto it = arg_begin(); it != arg_end(); ++it){
		  		if(*it != NULL){
		  			str<<" "<<(*it)->id;
		  		}
			}
		}
		return str.str();
	}
	virtual string smtletprint(){
		Assert(false,"There shouldn't be any UFUNs here");
		return "";
	}
};


/*mother is an index to the array, multi-mother is the array*/
class ARRACC_node: public bool_node{
private:
	ARRACC_node(int n):bool_node(ARRACC, n+1){ }
	ARRACC_node(const ARRACC_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }

public:
	inline static ARRACC_node* create(int n) {
		return new ARRACC_node(n);
	}
	inline static ARRACC_node* create(bool_node* mother, 
		bool_node* false_case, 
		bool_node* true_case
		) {
		ARRACC_node* an= new ARRACC_node(2);
		an->set_parent(0, mother);
		an->set_parent(1, false_case);
		an->set_parent(2, true_case);
		return an;
	}

	inline static ARRACC_node* create(const ARRACC_node& bn, bool copyChildren = true) {
		return new ARRACC_node(bn, copyChildren);
	}


	bool_node*& arguments(int i) {
		return parents[i + 1];
	}
	int nargs() const {
		return numparents - 1;
	}

	parent_iter arg_begin() const {
		return parents + 1;
	}
	parent_iter arg_end() const {
		return p_end();
	}

	bool_node*const & arguments(int i) const {
		return parents[i + 1];
	}


	virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
	virtual void outDagEntry(ostream& out) const{
		if( mother() != NULL){
            out<<" "<<mother()->get_name()<<" -> "<<get_name()<<"[label=\"idx\"] ; "<<endl;
    	}
    	int i=0;
		for(auto it = arg_begin(); it != arg_end(); ++it, ++i){
		  	if(*it != NULL){
		  		out<<" "<<(*it)->get_name()<<" -> "<<get_name()<<"[label=\""<< i <<"\"] ; "<<endl;
		  	}
		}
	}
	OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        for(auto it = arg_begin(); it != arg_end(); ++it){
            otype = OutType::joinOtype((*it)->getOtype(), otype);
        }
        return otype;
    }
	virtual string lprint()const{
		stringstream str;
		str<<id<<"= "<<"["<<mother()->lid()<<"]$";
		for(auto it =arg_begin(); it != arg_end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";
		  	}
		}
		str<<"$";
		return str.str();
	}
	virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother()->id<<" "<<nargs();
        for(size_t i=0; i<nargs(); ++i){
            str<<" "<<arguments(i)->id;
        }
        return str.str();
    }
	virtual bool_node* clone(bool copyChildren = true){return new ARRACC_node(*this, copyChildren);  };
	virtual string smtletprint(){
		stringstream ss;
		Assert(nargs() >= 2, "ARRACC Must have at least 2 choices");
		OutType* ot_join = OutType::joinOtype(arguments(1)->getOtype(), arguments(0)->getOtype());
		for (size_t i=2;i<nargs();i++){
			ot_join = OutType::joinOtype(ot_join, arguments(i)->getOtype());
		}
		if(nargs() == 2){
			ss<<" (ite "<< mother()->getSMTnode(OutType::BOOL) <<" "<< arguments(1)->getSMTnode(ot_join)<<" "<< arguments(0)->getSMTnode(ot_join)<<" ) ";
		}
		else{
			//ite n==0 a[0] else (ite n==1 etc...
			string msmt = mother()->getSMTnode(OutType::INT); 
			for(size_t i=0;i<nargs()-1;i++){
				ss<<" (ite (= " << msmt << " "<<i<<") "<<arguments(i)->getSMTnode(ot_join)<<" ";
			}
			ss<<arguments(nargs()-1)->getSMTnode(ot_join)<<" ";
			for(size_t i=0;i<nargs()-1;i++) ss<<")";
		}
		return ss.str();
	}
};






typedef union{
	int val;
	double fval;
} NumType;

class CONST_node: public bool_node{
	NumType	v;
	bool isInt;
	CONST_node() :bool_node(CONST, 0), isInt(true) {
		depth = 0;
		v.val = -1;
	}
	CONST_node(int n) :bool_node(CONST, 0), isInt(true) {
		depth = 0;
		v.val = n;
	}
	CONST_node(double d) :bool_node(CONST, 0), isInt(false) {
		depth = 0;
		v.fval = d;
	}
	CONST_node(const CONST_node& bn, bool copyChildren = true) : bool_node(bn, copyChildren), v(bn.v), isInt(bn.isInt) {
		//if(val == 13){ cout<<" surprise"<<endl; }
		depth = 0;
	}
public:
	inline static CONST_node* create() {
		return new CONST_node();
	}
	inline static CONST_node* create(int n) {
		return new CONST_node(n);
	}
	inline static CONST_node* create(double d) {
		return new CONST_node(d);
	}
	inline static CONST_node* create(const CONST_node& bn, bool copyChildren = true) {
		return new CONST_node(bn, copyChildren);
	}


    virtual void accept(NodeVisitor& visitor)  { visitor.visit( *this ); }
    void setVal(int n){ v.val = n; }
    int getVal() const { return v.val; }
    bool isFloat() const { return !isInt; }
    double getFval() const { return v.fval; }
    
    
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
	virtual string smtletprint(){
		stringstream ss;
		ss<<" ";
		
		if(getOtype() == OutType::BOOL){
			int x = getVal();
			Assert(x==0 || x==1,"Should be boolean values");
			if(x==1) ss<<"true";
			else ss<<"false";
		}
		else if(getOtype() == OutType::INT){
			int x = getVal();
			if(x>=0){
				ss<<x;
			}
			else{
				ss<<"(- "<<-x<<")";
			}
		}
		else if(getOtype() == OutType::FLOAT){
			double x = getFval();
			if(x>=0){
				ss<<x;
			}
			else{
				ss<<"(- "<<-x<<")";
			}
		}
		else Assert(false,"OutType invalid");
		ss<<" ";
		return ss.str();
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
class ARRASS_node: public bool_node{
	public:
    int quant;
private:
    ARRASS_node():bool_node(ARRASS, 3){ quant = -1; }
    ARRASS_node(const ARRASS_node& bn, bool copyChildren = true): bool_node(bn, copyChildren), quant(bn.quant){ }

public:

	inline static ARRASS_node* create() {
		return new ARRASS_node();
	}

	inline static ARRASS_node* create(bool_node* mother, bool_node* old_val, bool_node* new_val) {
		ARRASS_node* rv = new ARRASS_node();
		rv->parents[0] = mother;
		rv->parents[1] = old_val;
		rv->parents[2] = new_val;
		return rv;
	}

	inline static ARRASS_node* create(const ARRASS_node& bn, bool copyChildren = true) {
		return new ARRASS_node(bn, copyChildren);
	}


	bool_node*& getOldVal() {
		return parents[1];
	}
	bool_node*& getNewVal() {
		return parents[2];
	}
	bool_node* const& getOldVal() const {
		return parents[1];
	}
	bool_node* const& getNewVal() const {
		return parents[2];
	}

    virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
    virtual bool_node* clone(bool copyChildren = true){return new ARRASS_node(*this, copyChildren);  };
    virtual void outDagEntry(ostream& out) const{
        if( mother() != NULL){
            out<<" "<<mother()->get_name()<<" -> "<<get_name()<<"[label=\"="<<quant<<"\"] ; "<<endl;
        }
        
        if(getOldVal() != NULL){
            out<<" "<< getOldVal()->get_name()<<" -> "<<get_name()<<"[label=\"O\"] ; "<<endl;
        }
        if(getNewVal() != NULL){
            out<<" "<< getNewVal()->get_name()<<" -> "<<get_name()<<"[label=\"N\"] ; "<<endl;
        }
    }
    virtual string lprint()const{
        stringstream str;
        str<<id<<"= "<<mother()->lid()<<"=="<<quant<<" ? "<< getNewVal()->lid()<<":"<< getOldVal()->lid();
        return str.str();
    }
    OutType* getOtype()const {
        if(otype != OutType::BOTTOM){
            return otype;
        }
        otype = OutType::joinOtype(getOldVal()->getOtype(), getNewVal()->getOtype());
        return otype;
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<<mother()->id<<" == "<<quant;
        str<<" "<< getOldVal()->id;
        str<<" "<< getNewVal()->id;
        return str.str();
    }
	virtual string smtletprint(){
		stringstream ss;
		OutType* ot_join = OutType::joinOtype(getNewVal()->getOtype(), getOldVal()->getOtype());
		ss<<" (ite (= "<< mother()->getSMTnode(OutType::INT) <<" ";
		if (quant >= 0) ss<<quant;
		else ss<<"(- "<<-quant<<")";
		ss<<") "<< getNewVal()->getSMTnode(ot_join)<<" "<< getOldVal()->getSMTnode(ot_join)<<" ) ";
		return ss.str();
	}

};



template<class T, class F>
void forall_args(T* node, F f) {
	if (node->type == bool_node::ARRACC) {
		ARRACC_node* tt = (ARRACC_node*)node;
		for (auto it = tt->arg_begin(); it != tt->arg_end(); ++it) {
			f(*it);
		}
	}
	if (node->type == bool_node::ARRASS) {
		ARRASS_node* tt = (ARRASS_node*)node;
		f(tt->getOldVal());
		f(tt->getNewVal());
	}
}




/* This node typecasts bit to integers. The only thing is used is 'multi-mother'
 * where it stores all the input bits.
 */
class ACTRL_node: public bool_node{
private: 
	ACTRL_node(int nins):bool_node(ACTRL, nins){ }
	ACTRL_node(const ACTRL_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ }
public:
	inline static ACTRL_node* create(int nins) {
		return new ACTRL_node(nins);
	}
	inline static ACTRL_node* create(const ACTRL_node& bn, bool copyChildren = true) {
		return new ACTRL_node(bn, copyChildren);
	}

	virtual void accept(NodeVisitor& visitor) { visitor.visit( *this ); }
	virtual bool_node* clone(bool copyChildren = true){return new ACTRL_node(*this, copyChildren);  };
	OutType *getOtype()const {
		return OutType::INT;
	}
	virtual string lprint()const{
		stringstream str;
		str<<id<<"= "<<"$$";
		for(auto it = p_begin(); it != p_end(); ++it){
		  	if(*it != NULL){
		  		str<<(*it)->lid()<<", ";
		  	}
		}
		str<<"$$";
		return str.str();
	}
    
	virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<getOtype()->str()<<" "<< nparents();
        for(size_t i=0; i<nparents(); ++i){
            str<<" "<<parents[i]->id;
        }
        return str.str();
    }
	virtual string smtletprint(){
		Assert(false,"ACTRL SMT generation not supported");
		return "";
	}
};
class ASSERT_node: public bool_node, virtual public DllistNode{
private:
	typedef enum{Normal, Hard, Assume} AssertType;
	AssertType assertType;
	string msg;
    
    ASSERT_node ():bool_node(ASSERT, 1), assertType(Normal) { }
    ASSERT_node(const ASSERT_node& bn, bool copyChildren = true): bool_node(bn, copyChildren){ assertType = bn.assertType;  msg = bn.msg; }
public:
	static inline ASSERT_node* create(){
		return new ASSERT_node();
	}
	static inline ASSERT_node* create(const ASSERT_node& bn, bool copyChildren = true){
		return new ASSERT_node(bn, copyChildren);
	}

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
		str<<mother()->lid()<<" : "<<msg;		
		return str.str();
    }
    virtual string mrprint()const{
        stringstream str;
        str<<id<<" = "<<get_tname()<<" "<<mother()->id<<" \""<<msg<<"\"";
        return str.str();
    }
	virtual string smtletprint(){
		Assert(false,"Assert nodes should be by-passed for SMT generation");
		return "";
	}
};



inline bool_node* newNode( bool_node::Type type, int size=0){
	switch(type){
		case bool_node::AND: return AND_node::create();
		case bool_node::OR: return OR_node::create();
		case bool_node::XOR: return XOR_node::create();
		case bool_node::SRC: return SRC_node::create();
		case bool_node::DST: return DST_node::create();
		case bool_node::NOT: return NOT_node::create();
		case bool_node::CTRL: return CTRL_node::create();
		case bool_node::ASSERT: return ASSERT_node::create();
		case bool_node::PLUS: return PLUS_node::create();
		case bool_node::TIMES: return TIMES_node::create();
		case bool_node::DIV: return DIV_node::create();
		case bool_node::MOD: return MOD_node::create();
		case bool_node::NEG: return NEG_node::create();
		case bool_node::CONST: return CONST_node::create();
		case bool_node::LT: return LT_node::create();
		case bool_node::EQ: return EQ_node::create();
		case bool_node::ARRACC: return ARRACC_node::create(size);
		case bool_node::UFUN: return UFUN_node::create("NULL", size);
		case bool_node::ARRASS: return ARRASS_node::create();
		case bool_node::ACTRL: return ACTRL_node::create(size);
		case bool_node::ARR_R: return ARR_R_node::create();
		case bool_node::ARR_W: return ARR_W_node::create();
		case bool_node::ARR_CREATE: return ARR_CREATE_node::create(size);
		case bool_node::TUPLE_CREATE: return TUPLE_CREATE_node::create(size);
		case bool_node::TUPLE_R: return TUPLE_R_node::create();
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

