#ifndef __NODESTOSOLVER_H
#define __NODESTOSOLVER_H

#include "BooleanDAG.h"
#include "BooleanToCNF.h"
#include "Tvalue.h"
#include "FloatSupport.h"
#include "CommandLineArgs.h"

#ifndef INTEGERBOUND
# define INTEGERBOUND  8192*6
#endif

extern CommandLineArgs* PARAMS;

// #define Dout( msg ) msg

class Ufinfo {
public:
	vector<Tvalue> params;
	Ufinfo(vector<Tvalue>& p) :params(p) {}
};

class NormalComp {
public:
		bool operator()(const int x,const int y) const { return x < y; }
};


// Visitor for conversion of DAG to SAT.
class NodesToSolver : public NodeVisitor {
	FloatManager& floats;
    const string &outname;
	float sparseArray;
    map<bool_node *, int> &node_values; // -1=false, 1=true, 0=unknown
	void addToVals(map<pair<int, int>, int>& vals, gvvec::iterator it, int idx, int gval);
  template<typename EVAL>
	int compareRange(const gvvec& mv, int mstart, int mend, bool misInt, const gvvec& fv, int fstart, int fend, bool fisInt, EVAL eval);
  template<typename EVAL>
	void compareArrays (const Tvalue& tmval,  const Tvalue& tfval, Tvalue& out, EVAL eval);
    template<class COMPARE_KEY = NormalComp, typename THEOP> void processArith (bool_node &node, THEOP comp, COMPARE_KEY c = NormalComp());
	template<class COMPARE_KEY = NormalComp, typename THEOP> void processFloatArith(bool_node &node, THEOP comp, COMPARE_KEY c = NormalComp());
    template<typename THEOP> int doArithExpr (int quant1, int quant2,
					      int id1, int id2, THEOP comp);
  template<typename EVAL>
	void processEq(Tvalue& mval, Tvalue& fval, Tvalue& out, EVAL eval);
	template<typename EVAL>
	void processLT (LT_node& node, EVAL& eval);



	vector<int> lgv;
    Tvalue tvYES;
    Tvalue tvOne;
    Tvalue tvOneSigned;
	const int YES;
	bool stopAddingClauses;
	bool shortcut;

	map<string, int> ufunids;
	vector<vector<Ufinfo> > ufinfos;
  template <class COMPARE_KEY = NormalComp> void populateGuardedVals(Tvalue& oval, map<int, int, COMPARE_KEY>& numbers);
	void regTuple(vector<Tvalue>* new_vec, Tvalue& nvar);
  map<int, vector<Tvalue>> ufunVarsMap;
protected:
	SolverHelper &dir;
	vector<Tvalue> &node_ids;
        vector<vector<Tvalue>*> tpl_store;
	StringHTable2<int> tplcache;

    /* Return the value indexed by given node, or a default value (of given type). */
    inline Tvalue &tval_lookup (bool_node *node, valtype_t default_type = TVAL_BIT,
				int quant = 1) {
	if (node){		
	    return node_ids[node->id];
	}

	switch (default_type) {
	case TVAL_BIT:
	    return tvYES;

	case TVAL_SPARSE:
	    return tvOne;

	default:
	    assert (0);  /* Can't get here. */
	}

	return tvYES;
    }

    vector<int> scratchpad;
    vector<int> tmprange;
    vector<int> unirange;

    

	bool checkKnownFun(UFUN_node& node);
	bool NATIVEINTS;
public:
   string errorMsg;

   bool stoppedPrematurely(){
	   return this->stopAddingClauses;
   }

   /*
    * p_mng is the wrapper for the sat solver.  
    * p_dir is a SAT solver wrapper that provides a few additiona services for the SAT solver.
    * p_outname is a name for this translation. It is used, for example, to name any output files produced.
    * p_node_values contains values for either input or output nodes. 
    * 
    */
   
	    
     NodesToSolver (
	     SolverHelper& p_dir, 
	     const string& p_outname, 
		 map<bool_node*,  int>& p_node_values, 
		 vector<Tvalue>& p_node_ids,
		 FloatManager& _floats
	 ) :
	dir(p_dir), outname(p_outname), node_values(p_node_values), 
	node_ids(p_node_ids), YES(p_dir.YES), 
	scratchpad(100),tmprange(2), unirange(1), tvYES( p_dir.YES), tvOne (TVAL_SPARSE, p_dir.YES), floats(_floats)
    {
	tmprange[0] = 0;
	tmprange[1] = 1;
	unirange[0] = 1;

	/* Initialize the default "one" integer. */
	tvOne.num_ranges.push_back (guardedVal(p_dir.YES, 1));
	tpl_store.push_back(NULL);
	NATIVEINTS = PARAMS->nativeInts;
    };
    
	int lastGoodVal(int id){
		if(id > lgv.size()){
			return 0;
		}else{
			return lgv[id-1];
		}
	}
  void createCond(Tvalue mval , Tvalue fval, Tvalue& out);

    virtual void visit (AND_node &node);
    virtual void visit (OR_node &node);
    virtual void visit (XOR_node &node);
    virtual void visit (SRC_node &node);
    virtual void visit (DST_node &node);
    virtual void visit (NOT_node &node);
    virtual void visit (CTRL_node &node);
    virtual void visit (PLUS_node &node);
    virtual void visit (TIMES_node &node);
    virtual void visit (ARRACC_node &node);
    virtual void visit (UFUN_node &node);
    virtual void visit (DIV_node &node);
    virtual void visit (NEG_node &node);
    virtual void visit (MOD_node &node);
    virtual void visit( CONST_node& node );    
    virtual void visit (LT_node &node);    
    virtual void visit (EQ_node &node);
    virtual void visit (ARRASS_node &node);
    virtual void visit (ACTRL_node &node);

	virtual void visit( ARR_R_node &node);
	virtual void visit( ARR_W_node &node);
	virtual void visit( ARR_CREATE_node &node);
    
    virtual void visit( TUPLE_R_node &node);
	virtual void visit( TUPLE_CREATE_node &node);


	void intArrW(Tvalue& index, Tvalue& newval, const Tvalue& inarr, Tvalue& nvar, ARR_W_node& node);
	void arrayConstruct(bool_node::parent_iter v_begin, bool_node::parent_iter v_end, Tvalue& nvar);
	void arrRead(bool_node& node, Tvalue& nvar, Tvalue& index, Tvalue& inarr);

    virtual void visit (ASSERT_node &node);
	void process(BooleanDAG& bdag);
  template<typename EVAL>
	void mergeTvalues(int guard, const gvvec& nr0, int nr0Start, int nr0End, const gvvec& nr1, int nr1Start, int nr1End, gvvec& out, EVAL eval, bool isInt, int idx=-1);
  template<typename EVAL>
  void mergeTvalues(int guard, Tvalue& mid0, Tvalue& mid1, Tvalue& output, int& flag, EVAL eval);
  template<typename EVAL>
	void doArrArrAcc(const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output, EVAL eval);
  template<class COMPARE_KEY = NormalComp, typename EVAL>
 void doNonBoolArrAcc (const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output, EVAL eval, COMPARE_KEY c = NormalComp());
 void muxTValues(ARRACC_node* node, const Tvalue& omv, vector<Tvalue>& choices, Tvalue& out, bool isBoolean, bool isArray, bool isInt, bool isFloat);
 template<typename EVAL>
 void computeMaxOrMin(const gvvec& mv,const gvvec& fv, gvvec& out, bool doMax, EVAL eval);
 template<class COMPARE_KEY = NormalComp>
  void arrRTvalue(bool isBool, const Tvalue& index, const Tvalue& inarr, Tvalue& out, COMPARE_KEY c = NormalComp());
 template<typename EVAL>
 void arrWTvalue(const Tvalue& index, const Tvalue& inarr, const Tvalue& newval, Tvalue& nvar, EVAL eval);

 void newSynthesis(const string& name, const string& synthname, vector<Tvalue>& params, vector<Tvalue>& nvars, SolverHelper& dir);
  void preprocessUfun(UFUN_node& node);
  static bool createConstraints(BooleanDAG& dag, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids, FloatManager& floats, float sparseArray = -1.0);

};


#endif /* __NODESTOSOLVER_H */

