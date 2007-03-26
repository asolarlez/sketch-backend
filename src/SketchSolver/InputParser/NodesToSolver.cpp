#include "NodesToSolver.h"
#include <algorithm>
#include "timerclass.h"

/* 
 * Uncomment this to switch to bit-vector operators / comparators.
 * TODO switch to some other (dynamic) mechanism...
 */
// #define HAVE_BVECTARITH


template<typename COMP> void
NodesToSolver::processComparissons (arith_node& node)
{
    bool_node *mother = node.mother;
    Tvalue mval = tval_lookup (mother, TVAL_SPARSE);
    mval.makeSparse (dir);

    bool_node *father = node.father;
    Tvalue fval = tval_lookup (father, TVAL_SPARSE);
    fval.makeSparse (dir);

    int cvar = -YES;
    COMP comp;
    Dout(cout<<"SIZES = "<<mval.getSize ()<<", "<<fval.getSize ()<<endl);
    int orTerms = 0;
    for(int i=0; i<mval.getSize (); ++i){
		for(int j=0; j<fval.getSize (); ++j){
		    Dout(cout<<"COMPARING "<<mval[i]<<", "<<fval[j]<<endl);
		    if(comp(mval[i], fval[j])){
			cvar = dir.addAndClause(mval.getId (i), fval.getId (j));
			++orTerms;
			if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
			scratchpad[orTerms] = cvar;
		    }
		}
    }
    if( orTerms < 2 ){
	node_ids[node.id] = cvar;
    }else{
    scratchpad[0] = 0;
	int result = dir.addBigOrClause( &scratchpad[0], orTerms);
	node_ids[node.id] = result;
    }
    Dout( cout<<node.get_name()<<" :=  "<<node_ids[node.id]<<endl);
    return;
}


template<typename THEOP>
inline int NodesToSolver::doArithExpr(int quant1, int quant2, int id1, int id2, THEOP comp){
	return comp(quant1, quant2);
}



template<>
inline int NodesToSolver::doArithExpr<divides<int> >(int quant1, int quant2, int id1, int id2, divides<int> comp){
	if(quant2 == 0){
		//mng.assertVarClause(-id2);
		//Armando: Can't have this kind of assertions, because at this level we don't know whether this block
		//will execute or not.
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}

template<>
inline int NodesToSolver::doArithExpr<modulus<int> >(int quant1, int quant2, int id1, int id2, modulus<int> comp){
	if(quant2 == 0){
		//mng.assertVarClause(-id2);
		//Armando: Can't have this kind of assertions, because at this level we don't know whether this block
		//will execute or not.
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}


/*
 * Generate bit-wise addition, no overflow semantics.
 *
 * This implementation handles both signed and unsigned variants.
 * Overflow handling is assumed the responsibility of the user.
 */
Tvalue
NodesToSolver::intBvectComputeSum (Tvalue &lval, Tvalue &rval)
{
    /* Compute result size from arguments' sizes. */
    int lsize = lval.getSize () + (lval.isBvect () ? 1 : 0);
    int rsize = rval.getSize () + (rval.isBvect () ? 1 : 0);
    int osize = (lsize > rsize ? lsize : rsize);

    /* Find sign bit of both arguments. */
    int lsign = lval.getSignId (dir);
    int rsign = rval.getSignId (dir);

    dout ("computing addition: osize=" << osize << " lsign=" << lsign
	  << " rsign=" << rsign);

    /* Initialize a vector of consecutive output bits (SAT variables). */
    int oid = dir.newAnonymousVar (osize);

    /* Compute addition:
     * - initialize first carry bit (zero)
     */
    int c = -dir.YES;

    /* - generate values for output bits
     */
    int lbase = lval.getId ();
    int rbase = rval.getId ();
    lsize--;
    rsize--;
    for (int i = 0; i < osize; i++) {
	/* - get current value bits, or sign bit if exceeded either size */
	int l = (i < lsize ? lbase + i : lsign);
	int r = (i < rsize ? rbase + i : rsign);

	/* - compute current output bit, being l ^ r ^ c */
	dir.addXorClause (dir.addXorClause (l, r), c, oid + i);

	/* - compute next carry bit (if such exists), being (l && r) || (l && c) || (r && c) */
	if (i < osize - 1)
	    c = dir.addOrClause (dir.addOrClause (dir.addAndClause (l, r),
						  dir.addAndClause (l, c)),
				 dir.addAndClause (r, c));
    }

    /* Create result value (signed). */
    Tvalue oval (TVAL_BVECT_SIGNED, oid, osize);

    dout ("done");

    return oval;
}

Tvalue
NodesToSolver::intBvectAdd (Tvalue &lval_arg, int lval_quant,
			    Tvalue &rval_arg, int rval_quant)
{
    /* Apply multipliers to values, transform to padded signed bit-vectors.
     *
     * FIXME this is an ugly way to handle constant values in the circuit,
     * and is currently due to a limitation in the way we generate default
     * values for null parent pointers. This needs to be fixed, and should
     * generally be taken care of prior to getting here.
     */
    Tvalue lval;
    Tvalue rval;

    dout ("extracting arguments as (padded) signed bitvectors");
    if (! (lval_quant == 1 || lval_quant == -1)) {
	/* Value must be "one". */
	Assert (lval_arg.getId () == dir.YES && lval_arg.getSize () == 1,
		"value must be a constant true bit");

	dout ("lval corresponds to constant " << lval_quant);
	lval = lval_arg.toSparse (dir, lval_quant).toBvectSigned (dir, 1);
    } else {
	lval = lval_arg.toBvectSigned (dir, 1);

	if (lval_quant == -1) {
	    dout ("negating lval");
	    lval = lval.toComplement (dir);
	}
    }

    if (! (rval_quant == 1 || rval_quant == -1)) {
	/* Value must be "one". */
	Assert (rval_arg.getId () == dir.YES && rval_arg.getSize () == 1,
		"value must be a constant true bit");

	dout ("rval corresponds to constant " << rval_quant);
	rval = rval_arg.toSparse (dir, rval_quant).toBvectSigned (dir, 1);
    } else {
	rval = rval_arg.toBvectSigned (dir, 1);

	if (rval_quant == -1) {
	    dout ("negating rval");
	    rval = rval.toComplement (dir);
	}
    }

    /* Compute addition. */
    dout ("generating addition");
    return intBvectComputeSum (lval, rval);
}

/*
 * Handle signed / unsigned bit-vector addition / subtraction.
 */
void
NodesToSolver::intBvectPlus (arith_node &node)
{
    /* Compute addition of two parent node values. */
    dout ("generating addition");
    Tvalue oval = intBvectAdd (tval_lookup (node.mother), 1,
			       tval_lookup (node.father), 1);

    /* Set node's value. */
    dout ("storing result");
    node_ids[node.id] = oval;

    dout ("done");
}

/*
 * Handle signed / unsigned bit-vector comparisons.
 */
void
NodesToSolver::intBvectEq (arith_node &node)
{
    /* Compute the difference between operands. */
    dout ("computing (lhs - rhs)");
    Tvalue dval = intBvectAdd (tval_lookup (node.mother), 1,
			       tval_lookup (node.father), -1);

    /* Assert it is (all bits) zero. */
    dout ("asserting result is zero");
    int id = dval.getId ();
    int nvars = dval.getSize () + 1;
    vector<int> dvars(nvars);
    int anybit = dvars[0] = dir.newAnonymousVar ();
    for (int i = 1; i < nvars; i++)
	dvars[i] = id++;
    dir.addBigOrClause (&dvars[0], nvars - 1);
    node_ids[node.id] = -anybit;

    dout ("done");
}

void
NodesToSolver::intBvectLt (arith_node &node)
{
    /* Compute the difference between operands. */
    dout ("computing (lhs - rhs)");
    Tvalue dval = intBvectAdd (tval_lookup (node.mother), 1,
			       tval_lookup (node.father), -1);

    /* Assert it is negative. */
    dout ("asserting result is negative");
    int sign = dval.getSignId (dir);
    node_ids[node.id] = sign;

    dout ("done");
}

void
NodesToSolver::intBvectLe (arith_node &node)
{
    /* Compute the reverse difference between operands. */
    dout ("computing (rhs - lhs)");
    Tvalue dval = intBvectAdd (tval_lookup (node.mother), -1,
			       tval_lookup (node.father), 1);

    /* Assert it is non-negative. */
    dout ("asserting result is non-negative");
    int sign = dval.getSignId (dir);
    node_ids[node.id] = -sign;

    dout ("done");
}

void
NodesToSolver::intBvectGt (arith_node &node)
{
    /* Compute the reverse difference between operands. */
    dout ("computing (rhs - lhs)");
    Tvalue dval = intBvectAdd (tval_lookup (node.mother), -1,
			       tval_lookup (node.father), 1);

    /* Assert it is negative. */
    dout ("asserting result is negative");
    int sign = dval.getSignId (dir);
    node_ids[node.id] = sign;

    dout ("done");
}

void
NodesToSolver::intBvectGe (arith_node &node)
{
    /* Compute the difference between operands. */
    dout ("computing (lhs - rhs)");
    Tvalue dval = intBvectAdd (tval_lookup (node.mother), 1,
			       tval_lookup (node.father), -1);

    /* Assert it is non-negative. */
    dout ("asserting result is non-negative");
    int sign = dval.getSignId (dir);
    node_ids[node.id] = -sign;

    dout ("done");
}


#if 0
void
NodesToSolver::intBvectMult (arith_node &node)
{
    /* Get left and right arguments in bit-vector representation. */
    Tvalue lval = tval_lookup (node.mother).toBvect (dir);
    Tvalue rval = tval_lookup (node.father).toBvect (dir);

    /* For each bit (variable) of right-hand side argument, add the left-hand
     * side argument guarded using that bit to the total sum. */

    /* Set node's value. */

}
#endif

template<typename THEOP> void
NodesToSolver::processArith (arith_node &node)
{
    THEOP comp; // int op int

	bool_node* mother = node.mother;
	Tvalue mval = tval_lookup (mother, TVAL_SPARSE);
	mval.makeSparse (dir);
	bool_node* father = node.father;
	Tvalue fval = tval_lookup (father, TVAL_SPARSE);
	fval.makeSparse (dir);

	map<int, int> numbers;
	Tvalue& oval = node_ids[node.id];
	vector<int>& tmp = oval.num_ranges;
	tmp.clear();
	int ttt = mval.getSize ()*fval.getSize ();
	ttt = ttt > INTEGERBOUND ? INTEGERBOUND : ttt;
	tmp.reserve(ttt);
	Dout(cout<<"ARITHOP "<<mval<<"  OP  "<<fval<<endl);
	Dout(cout<<"OPERATING "<<node.father->get_name()<<"  WITH  "<<node.mother->get_name()<<endl);
	int vals = 0;
	//cout<<" BEFORE THE LOOPS"<<endl;
	//				timerclass atimer("TA");
	//				timerclass btimer("TB");
	//				timerclass ctimer("TC");
	//				timerclass dtimer("TD");
	for(int i=0; i<mval.getSize (); ++i){
	    for(int j=0; j<fval.getSize (); ++j){
		// int quant = comp(node.mother_quant*nrange[i], node.father_quant*frange[j]);
		//						atimer.restart();
		int quant = doArithExpr(mval[i], fval[j], mval.getId (i), fval.getId (j), comp);
		//						atimer.stop();
		Dout(cout<<quant<<" = "<<mval[i]<<" OP "<<fval[j]<<endl);
		if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
		Dout(cout<<"QUANT = "<<quant<<"          "<<mval.getId (i)<<", "<<fval.getId (j)<<endl);
		//						btimer.restart();
		map<int, int>::iterator it = numbers.find(quant);
		//						btimer.stop();
		if( it != numbers.end()){
		    //							ctimer.restart();
		    int cvar = dir.addAndClause(mval.getId (i),fval.getId (j));
		    int cvar2 = dir.addOrClause(cvar, it->second);
		    it->second = cvar2;
		    //							ctimer.stop();
		}else{
		    //							dtimer.restart();
		    int cvar = dir.addAndClause(mval.getId (i), fval.getId (j));
		    tmp.push_back(quant);
		    numbers[quant] = cvar;
		    ++vals;
		    //							dtimer.stop();
		}
		//cout<<" ENDLOOP "<<endl;
	    }
	}
	//				atimer.print();
	//				btimer.print();
	//				ctimer.print();
	//				dtimer.print();


	Dout(cout<<"tmp size = "<<tmp.size ()<<endl);
	Assert( vals > 0, "This should not happen here");
	int newID = -1;
	{
	    int quant = tmp[0];
	    newID = numbers[quant];
	}
	for(int i=1; i<vals; ++i){
	    int quant = tmp[i];
	    if(newID + i !=  numbers[quant]){
		newID = -1;
		break;
	    }
	}

	if( newID != -1){
	    oval.setId(newID);
	    oval.sparsify ();
	    Dout( cout<<" := "<<oval<<endl );
	    return;
	}


	newID = dir.newAnonymousVar();
	for(int i=1; i<vals; ++i){
	    int cvar = dir.newAnonymousVar();
	    Assert( cvar == newID + i, "SolveFromInput: bad stuff");
	}
	for(int i=0; i<vals; ++i){
	    int quant = tmp[i];
	    mng.addEqualsClause(newID + i, numbers[quant]);
	}
	oval.setId(newID);
	oval.sparsify ();
	Dout( cout<<" := "<<oval<<endl );
    
}


/*
 * Update node's value and set flag accordingly.
 */
void
NodesToSolver::boolNodeUpdate (bool_node &node, Tvalue &nvar)
{
    if (nvar != node_ids[node.id]) {
	node_ids[node.id] = nvar;
	node.flag = true;
    } else
	node.flag = false;
}


void NodesToSolver::visit( AND_node& node ){
	const Tvalue& fval = tval_lookup(node.father);
	const Tvalue& mval = tval_lookup(node.mother);
	if(!checkParentsChanged(node, true)){ Dout( cout<<fval<<" AND "<<mval<<" unchanged"<<endl  ); return; }	
	Tvalue oldnvar(node_ids[node.id]);
	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addAndClause(fval.getId (), mval.getId ());
	node.flag = oldnvar != nvar;
	Dout(cout<<"AND "<<node.name<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}


void NodesToSolver::visit( OR_node& node ){
	if(!checkParentsChanged( node, true)){ Dout( cout<<"OR didn't change"<<endl  ); return; }
	const Tvalue& fval = tval_lookup(node.father);
	const Tvalue& mval = tval_lookup(node.mother);
	Tvalue oldnvar(node_ids[node.id]);
	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addOrClause(fval.getId (), mval.getId ());
	node.flag = oldnvar != nvar;
	Dout(cout<<"OR "<<node.name<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}
void NodesToSolver::visit( XOR_node& node ){
	if(!checkParentsChanged( node, true)){ Dout( cout<<"XOR didn't change"<<endl  ); return; }
	const Tvalue& fval = tval_lookup(node.father);
	const Tvalue& mval = tval_lookup(node.mother);
	Tvalue oldnvar(node_ids[node.id]);
	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addXorClause(fval.getId (), mval.getId ());
	node.flag = oldnvar != nvar;
	Dout(cout<<"XOR "<<node.name<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}

void
NodesToSolver::visit (SRC_node &node)
{
    int iid = node.ion_pos;

	Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd" );
		
    if (node_values.find (&node) != node_values.end ()) {
		if (node.get_nbits () > 1) {
		    Tvalue tmp = tvYES;
		    tmp.makeSparse (dir, node_values[(&node)]);
		    /* TODO we currently make a sparse value, then convert it to signed bitvec.
		     * This sounds like bad engineering, since what we really should do
		     * is construct a signed from the integer value, directly. */
#ifdef HAVE_BVECTARITH
		    tmp.makeBvectSigned (dir);
#endif /* HAVE_BVECTARITH */
		    node_ids[node.id] = tmp;
		} else {
		    node_ids[node.id] = node_values[(&node)]*YES;
		}
		cout << " input " << node.get_name () << " = " << node_ids[node.id] << endl;
		Dout (cout << dir.getArr (node.get_name(), 0) << " has value " << node_values[(&node)]
	      << "   " << (&node) << "    " << node_ids[node.id] << endl);
    } else {
		node_ids[node.id] = dir.getArr (node.get_name(), 0);
		//This could be removed. It's ok to setSize when get_nbits==1.		
		if (node.get_nbits () > 1) {
		    node_ids[node.id].setSize (node.get_nbits ());
		    Dout (cout << "setting input nodes" << node.name << endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
		    node_ids[node.id].makeSparse (dir);
#endif /* HAVE_BVECTARITH */
		}
	Dout (cout << "REGISTERING " << node.name << "  " << node_ids[node.id]
	      << "  " << &node << endl);
    }
}



void NodesToSolver::visit( DST_node& node ){
	node_ids[node.id] = tval_lookup(node.mother);
	Dout(cout<<"DST = "<<node_ids[node.id]<<endl);
	/*
	int oid = node.ion_pos;
	int nvar = dir.getArr(outname, oid);	

	Tvalue outv = 
	Dout(cout<<" output "<<outv<<endl);
	if(outv.isSparse()){
		cout<<" output "<<node.get_name()<<" = "<<outv<<endl;
		Dout(cout<<"Making output sparse"<<endl);
		outv = outv.toBvectSigned(dir);
	}

	{
		int szout = node.get_nbits();
		int sztv = outv.getSize ();
		int minsz = szout<sztv?szout : sztv;
		Dout(cout<<"minsz = "<<minsz<<" sztv="<<sztv<<"  szout="<<szout<<endl);
		for(int i=0; i<minsz; ++i){
			mng.addEqualsClause( nvar+i, outv.getId (i));
		}
		for(int i=minsz; i<szout; ++i){
			mng.addEqualsClause( nvar+i, -YES);
		}
		Dout(cout<<"DST = "<<outv<<endl);
	}
	*/
	return;
}

/*
 * NOT node visitor.
 *
 * 
 */
void
NodesToSolver::visit (NOT_node &node)
{
    Assert (node.mother && ! node.father, "NOT node must have exactly one predecessor");	
    if (! checkParentsChanged (node, true)) {
		Dout (cout << "NOT unchanged" << endl);
		return;
    }

    const Tvalue &mval = tval_lookup (node.mother);
    Assert( mval.getType() == TVAL_BVECT && mval.getSize() == 1, "Bad Type for NOT");
    Tvalue nvar = -mval.getId ();
    boolNodeUpdate (node, nvar);

    Dout (cout << "PT " << node.name << " " << nvar << " " << &node << endl);
}


void
NodesToSolver::visit (NEG_node &node)
{
    Assert (node.mother && ! node.father, "NEG node must have exactly one predecessor");	
    if (! checkParentsChanged (node, true)) {
		Dout (cout << "NEG unchanged" << endl);
		return;
    }

    const Tvalue &mval = tval_lookup (node.mother);
    Tvalue nvar = mval.toComplement(dir);
    boolNodeUpdate (node, nvar);

    Dout (cout << "NEG " << node.name << " " << nvar << " " << &node << endl);
}





void
NodesToSolver::visit (CTRL_node &node)
{
    int iid = node.ion_pos;
    Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd" );
    if(  node_values.find(&node) != node_values.end() ){
		if( node.get_nbits() > 1 ){
		    Tvalue tmp = tvYES;
		    tmp.makeSparse (dir, node_values[(&node)]);
		    /* TODO we currently make a sparse value, then convert it to signed bitvec.
		     * This sounds like bad engineering, since what we really should do
		     * is construct a signed from the integer value, directly. */
#ifdef HAVE_BVECTARITH
		    tmp.makeBvectSigned (dir);
#endif /* HAVE_BVECTARITH */
		    node_ids[node.id] = tmp;
		    cout<<" control "<<node.get_name()<<" = "<<node_ids[node.id]<<"    "<<node_values[(&node)]<<endl;
		}else{
		    node_ids[node.id] = node_values[(&node)]*YES;
		}
		Dout( cout<< dir.getArr(node.get_name(), 0)<<" has value "<<node_values[(&node)]<<"   "<< (&node) <<"    "<< node_ids[node.id] <<endl  );
		return;
    }else{
		node_ids[node.id] = dir.getArr(node.get_name(), 0);
		if( node.get_nbits() > 1 ){ //This could be removed. It's ok to setSize when get_nbits==1.
		    node_ids[node.id].setSize( node.get_nbits() );
		    Dout(cout<<"setting control nodes"<<node.name<<endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
		    node_ids[node.id].makeSparse(dir);
#endif /* HAVE_BVECTARITH */
		}
		Dout(cout<<"CONTROL "<<node.name<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
		return;
    }
}



void NodesToSolver::visit( PLUS_node& node ){
	Dout( cout<<" PLUS "<<endl );
	if(!checkParentsChanged( node, true)){ return; }

	/* FIXME hard-wired bit-vector arithmetics. */
#ifdef HAVE_BVECTARITH
	intBvectPlus (node);
#else
	processArith<plus<int> >(node);
#endif /* HAVE_BVECTARITH */

	return;
}
void NodesToSolver::visit( TIMES_node& node ){
	Dout( cout<<" TIMES "<<endl );
	if(!checkParentsChanged( node, true)){ return; }
	processArith<multiplies<int> >(node);
	return;
}

/*
class UFUN_store{
	vector<Tvalue> symvalues;
	vector<vector<Tvalue> > arguments;
	BooleanDAG argComp;
	int nargs;
	public:
	UFUN_store(int p_nargs):nargs(p_nargs){
		bool_node* peq = NULL
		for(int i=0; i<nargs; ++i){
			string ina;
			string inb;
			{
				stringstream str;
				str<<"ina"<<i;
				ina = str.str();
				argComp.create_inputs(nargs, ina);
			}
			{
				stringstream str;
				str<<"inb"<<i;
				inb = str.str();
				argComp.create_inputs(nargs, inb);
			}
			EQ_node* eq = new EQ_node();
			argComp.new_node(ina, inb, bool_node::ARITH, argComp.new_name(), eq);
			if(peq != NULL){
				peq = argComp.new_node(peq, eq , bool_node::AND, argComp.new_name());
			}else{
				peq = eq;
			}
		}
		
		peq = argComp.new_node(peq, NULL , bool_node::DST, "DST");
		argComp.relabel();
		argComp.sort_graph();		
	}
	public Tvalue newCall(Tvalue& symvalue, vector<Tvalue>& args, SATSolver& p_mng, varDir& p_dir,  const string& p_outname){
		Assert( args.size() == nargs, "This is not correct ");
		vector<bool_node>& innodes = argComp.getNodesByType(bool_node::SRC);
		
		 map<bool_node*,  int>& p_node_values, 
		 vector<Tvalue>& p_node_ids
		NodesToSolver nts(p_mng, p_dir, p_outname, 
		for(int i=0; i<nargs; ++i){
				
			
		}
		
	}
	
};

*/


void NodesToSolver::visit( UFUN_node& node ){
	Assert(false, "NYI");
/*
	Tvalue in = xx; // input tvalue.
	
	// map<string, vector<Tvalue> > ufunPrevIns;
	// map<string, vector<vector<Tvalue> > > ufunPrevArgs;
	
	vector<Tvalue>& prevInputs =  ufunPrevIns[node.name];
	vector<vector<Tvalue> >& prevArgs =  ufunPrevArgs[node.name];
	
	Tvalue control;
	// not = YES;
	for(int i=0; i< prevInputs.size(); ++i){
	
		int tmp = equals( prevArgs[i], in);	
		
		// c[i] = tmp & not;
		// not = not & !tmp;
				
	}
		
	*/
	
}









//timerclass aracctimer("ARRACC TIMER");
//timerclass flooptimer("FIRST LOOP TIMER");
//timerclass nonbooltimer("NON BOOL TIMER");
//timerclass elooptimer("FINAL LOOP TIMER");


void
NodesToSolver::visit (ARRACC_node &node)
{
    Dout (cout << "ARRACC" << endl);
    Dout (cout << "arracc: begin " << node.get_name() << endl);

    /* Get index value. */
    Assert (node.mother != NULL, "This should never happen");
    Tvalue &mval = tval_lookup (node.mother);
    Dout (cout << "index=" << node.mother->get_name() << " -> " << mval << endl);

#if 0
    aracctimer.restart();
    flooptimer.restart();
#endif

    /* Check whether anything has changed, otherwise quit. */
    Dout (cout << "checking for changed parents..." << endl);
    bool is_unchanged = true;
    vector<bool_node *> &parents = node.multi_mother;
    if (mval.isSparse()) {
	/* Sparse index, check only affected parent nodes. */
	vector<int> &index_vals = mval.num_ranges;
	int nparent = parents.size();
        for (vector<int>::const_iterator it = index_vals.begin();
             is_unchanged && it < index_vals.end(); it++)
        {
	    int index_val = *it;
            if (index_val >= 0 && index_val < nparent) {
                bool_node *cnode = parents[index_val];
                is_unchanged = is_unchanged && ! (cnode && cnode->flag);
                Dout (cout << "parents[" << index_val << "]="
		      << (cnode ? cnode->get_name() : "NULL") << " -> "
		      << (is_unchanged ? "unchanged" : "changed") << endl);
            }
        }
    } else {
	/* Otherwise, check all parent nodes. */
	int i = 0;
        for (vector<bool_node *>::iterator it = parents.begin();
	     is_unchanged && it != parents.end(); it++, i++)
	{
	    bool_node *cnode = *it;
            is_unchanged = is_unchanged && ! (cnode && cnode->flag);
            Dout (cout << "parents[" << i << "]=" << (cnode ? cnode->get_name() :"NULL")
                  << " -> " << (is_unchanged ? "unchanged" : "changed") << endl);
        }
    }
#if 0
    flooptimer.stop().print();
    COUT << " FIRST LOOP mmsize =" << node.multi_mother.size() << " mval.num_ranges.size() ="
        << mval.num_ranges.size() << endl;
#endif
    if (! checkParentsChanged (node, is_unchanged)) {
        Dout (cout << "parents did not change, nothing to do" << endl);
        // aracctimer.stop().print();
        return;
    }

    /* Extract choices into a sparse/bit-vector value subsets. */
    Dout (cout << "partitioning items to sparse/bvect subsets..." << endl);
    map<int, Tvalue> items_sparse;
    map<int, Tvalue> items_bvect;
    int i = 0;
    for (vector<bool_node *>::iterator it = node.multi_mother.begin();
	 it != node.multi_mother.end(); it++, i++)
    {
	bool_node *cnode = *it;
        const Tvalue &cval = tval_lookup (cnode);
        if (cval.isSparse())
	    items_sparse[i] = cval;
	else
	    items_bvect[i] = cval;

        Dout (cout << "item[" << i << "]=" << (cnode ? cnode->get_name() : "NULL")
	      << " -> " << cval << " (" << (cval.isSparse() ? "sparse" : "bvect")
	      << ")" << endl);
    }

    /* Get choice for sparse/bit-vector subsets separately. */
    Tvalue output_sparse, output_bvect;
    bool is_got_sparse = doArrAccSparse (output_sparse, mval, items_sparse);
    bool is_got_bvect = doArrAccBvect (output_bvect, mval, items_bvect);

    /* Initialize overall output value. */
    Tvalue &output = node_ids[node.id];

    /* Convert sparse output to bvect and combine them.
     * TODO currently converting the sparse to unsigned (arbitrarily).
     * We may need to consider converting to signed already, or at least
     * checking to see whether it may take a negative value. */
    bool is_signed = is_got_bvect && output_bvect.isBvectSigned ();
    if (is_got_bvect && is_got_sparse) {
	/* FIXME temporary: exhaustive check for a negative integer in the set
	 * of possible sparse values, in which case everything needs to be
	 * converted to signed. Ideally, we'll have a constant time flag in
	 * the sparse Tvalue that will tell us just that. */
	if (! is_signed) {
	    vector<int> &output_sparse_vals = output_sparse.num_ranges;
	    for (vector<int>::const_iterator it = output_sparse_vals.begin();
		 it < output_sparse_vals.end(); it++)
	    {
		if (*it < 0) {
		    Dout (cout <<
			  "found negative sparse value, forcing signed output" << endl);
		    output_bvect.makeBvectSigned (dir);
		    is_signed = true;
		    break;
		}
	    }
	}

	Dout (cout << "converting sparse output to "
	      << (is_signed ? "signed" : "unsigned") << " bvect" << endl);
	if (is_signed)
	    output_sparse.makeBvectSigned (dir);
	else
	    output_sparse.makeBvect (dir);

	int pad = output_sparse.getSize() - output_bvect.getSize();
	if (pad > 0) {
	    Dout (cout << "padding bvect output with " << pad << " bits" << endl);
	    if (is_signed)
		output_bvect.makeBvectSigned (dir, pad);
	    else
		output_bvect.makeBvect (dir, pad);
	} else if (pad < 0) {
	    pad = -pad;
	    Dout (cout << "padding sparse output with " << pad << " bits" << endl);
	    if (is_signed)
		output_sparse.makeBvectSigned (dir, pad);
	    else
		output_sparse.makeBvect (dir, pad);
	}

	int output_size = output_bvect.getSize ();
	Dout (cout << "merging outputs into single " << output_size
	      << "-bit output" << endl);
	output = Tvalue ((is_signed ? TVAL_BVECT_SIGNED : TVAL_BVECT),
 			 dir.newAnonymousVar (output_size), output_size);
	for (int i = 0; i < output_size; i++) {
	    dir.addOrClause (output_sparse.getId (i), output_bvect.getId (i),
			     output.getId (i));
	}
    } else if (is_got_bvect) {
	Dout (cout << "using bvect output only (no sparse)" << endl);
	output = output_bvect;
    } else if (is_got_sparse) {
	Dout (cout << "using sparse output only (no bvect)" << endl);
	output = output_sparse;
    } else {
	Dout (cout << "no output, defaulting to false" << endl);
	output = -YES;
    }

    /* Store final output to node. */
    Dout (cout << "output=" << output << endl);

#if 0
    aracctimer.stop().print();
#endif

    Dout (cout << "arracc: done" << endl);
}

bool
NodesToSolver::doArrAccBvect (Tvalue &output, Tvalue &index_orig,
			      map<int, Tvalue> &items)
{
    Dout (cout << "arracc: bvect subset, processing "
	  << items.size() << " item(s)" << endl);

    if (items.size() == 0) {
	Dout (cout << "no items, nothing to do" << endl);
	return false;
    }
    
    /* Check if index is non-sparse. */
    Tvalue index = index_orig;
    if (! index.isSparse()) {
        Assert (index.getSize() > 0, "index value must have at least one bit");

        /* Optimize for single-bit index, two single-bit (present) items. */
        if (index.getSize() == 1
	    && (items.count (0) && items[0].getSize() == 1)
	    && (items.count (1) && items[1].getSize() == 1))
	{
	    Dout (cout << "replacing with choice: " << index << " ? "
		  << items[1] << " : " << items[0] << endl);
	    output = dir.addChoiceClause (index.getId(), items[1].getId(),
		 			  items[0].getId());
	    return true;
	}

        /* Otherwise sparsify.
	 * TODO here as well, we make a fresh sparse copy and not override the
	 * original; this prevents inconsistencies in later computation, but
	 * also has a potential for repeated sparsification in the absence
	 * of memoization. */
	Dout (cout << "sparsifying index" << endl);
        index.makeSparse (dir);
    }

#if 0
    elooptimer.restart();
#endif

    /* Generate disjuncts for output, bit-by-bit. */
    const vector<int> &index_vals = index.num_ranges;
    vector<vector <int> > output_bits;
    bool is_repeat, is_signed = false,
	 is_msb_nonsign_current = false, is_msb_nonsign_last;
    int nbit = 0;
    do {
	/* Reset flags. */
	is_repeat = false;
	is_msb_nonsign_last = is_msb_nonsign_current;
	is_msb_nonsign_current = false;

	Dout (cout << "generating disjuncts for output bit " << nbit << "..." << endl);

	/* Add vector for new bit disjuncts.
	 * TODO currently adding a preamble value (zero) to be able to later
	 * generate big OR-clauses conveniently; this is ugly and should be
	 * changed along with the API. */
	output_bits.push_back (vector<int>());
	vector<int> &current_bit = output_bits.back();
	current_bit.push_back (0);

	/* Add current bit disjuncts for all possibly indexed items. */
	int i = 0;
	for (vector<int>::const_iterator it = index_vals.begin();
	     it != index_vals.end(); it++, i++)
	{
	    const int index_val = *it;
	    Dout (cout << "processing index_vals[" << i << "]=" << index_val << endl);

	    /* Ensure index points to existing value. */
	    if (! (items.count (index_val))) {
		Dout (cout << "no value exists, skipping" << endl);
		continue;
	    }

	    /* Skip false-guarded index value. */
	    int index_id = index.getId (i);
	    if (index_id == -YES)
		continue;

	    /* Extract index-pointed array item. */
	    Tvalue &item = items[index_val];
	    int item_nbit = item.getSize();

	    /* Set flag for possibly signed output. */
	    is_signed = is_signed || item.isBvectSigned();

	    /* Set flag for additional iteration(s). */
	    if (nbit < item_nbit) {
		is_repeat = true;

		/* Set flag for a non-sign bit disjunct. */
		is_msb_nonsign_current =
		    is_msb_nonsign_current || item.isBvect();
	    }

	    /* Add current bit disjunct (if non-false). */
	    int item_bit = item.getIdExt (dir, nbit);
	    int disjunct = dir.addAndClause (item_bit, index_id);
	    Dout (cout << ">>> item_bit=" << item_bit << " index_id=" << index_id
		  << " disjunct=" << disjunct << endl);
	    if (disjunct != -YES)
		current_bit.push_back (disjunct);
	}
	
	/* Advance bit counter. */
	nbit++;
    } while (is_repeat);

    /* Discard MSB when safe. */
    if (! (is_signed && is_msb_nonsign_last)) {
	Dout (cout << "discarding output msb ("
	      << (is_signed ? "signed" : "unsigned") << ") ("
	      << (is_msb_nonsign_last ? "nonsign-msb" : "sign-msb")
	      << ")" << endl);
	output_bits.pop_back();
	nbit--;
    }

    Assert (nbit == output_bits.size(), "bit count must be consistent");
    Dout (cout << "generated disjuncts for " << nbit << " output bit(s) ("
	  << (is_signed ? "signed" : "unsigned") << ")" << endl);

    /* No output bits, nothing to do. */
    if (nbit == 0) {
	Dout (cout << "no output bits, nothing to do" << endl);
	return false;
    }

    /* Generate output. */
    output = -YES;
    if (nbit == 1 && output_bits[0].size() <= 2) {
	/* Special case: single bit with single (or no) disjunct. */
	Dout (cout << "special case: re-using single variable of single output bit" << endl);
	if (output_bits[0].size() == 2)
	    output = output_bits[0][1];  /* else, stay with false. */
    } else {
	/* Initialize output object. */
	Dout (cout << "building output bvect with new variables" << endl);
	output = Tvalue ((is_signed ? TVAL_BVECT : TVAL_BVECT_SIGNED),
			 dir.newAnonymousVar (nbit), nbit);

	/* Iterate over bits, form disjunction.
	 * TODO again, not the extra preambling vector element (see above). */
	int i = 0;
	for (vector<vector<int> >::iterator it = output_bits.begin();
	     it != output_bits.end(); it++, i++)
	{
	    vector<int> &current_bit = *it;

	    current_bit[0] = output.getId (i);
	    dir.addBigOrClause (&current_bit[0], current_bit.size() - 1);
	}
    }

#if 0
    elooptimer.stop();
#endif

    Dout (cout << "arracc: bvect subset complete, " << output << endl);
    return true;
}

bool
NodesToSolver::doArrAccSparse (Tvalue &output, Tvalue &index_orig,
			       map<int, Tvalue> &items)
{
    Dout (cout << "arracc: sparse subset, processing "
	  << items.size() << " item(s)" << endl);

    if (items.size() == 0) {
	Dout (cout << "no items, nothing to do" << endl);
	return false;
    }
    
    /* Sparsify index.
     * TODO we currently create a new value, instead of perpetually replacing
     * the existing value. This is necessary in order to avoid losing the
     * original (bit-vector) value, which might be used for other needs as well.
     * However, it also means that we may re-sparsify again and again. This
     * situation needs to be fixed by "memoizing" the different forms a value
     * have already taken in the evaluation (i.e. bvect, sparse, etc). */
    Tvalue index = index_orig.toSparse(dir);

    /* Accumulate guard variables for all possible output values. */
    Dout (cout << "accumulating guards for all possible output values..." << endl);
    map<int, vector<int> > output_val_vars;
    const vector<int> &index_vals = index.num_ranges;
    int i = 0;
    for (vector<int>::const_iterator it = index_vals.begin();
	 it != index_vals.end(); it++, i++)
    {
	const int index_val = *it;
	Dout (cout << "processing index_vals[" << i << "]=" << index_val << endl);

	/* Ensure index points to existing value. */
	if (! (items.count (index_val))) {
	    Dout (cout << "no value exists, skipping" << endl);
	    continue;
	}

	/* Extract index-pointed array item. */
	Tvalue &item = items[index_val];
	Assert (item.isSparse(), "item must be sparse");
	vector<int> &item_vals = item.num_ranges;

	Dout (cout << "processing " << item_vals.size() << " item value(s)..." << endl);

	/* Accumulate guard conjuncts for all possibly output values. */
	int j = 0;
	for (vector<int>::iterator it = item_vals.begin();
	     it != item_vals.end(); it++, j++)
	{
	    int item_val = *it;
	    int cvar = dir.addAndClause (index.getId (i), item.getId (j));
	    output_val_vars[item_val].push_back (cvar);
	    Dout (cout << "item_vals[" << j << "]=" << item_val << endl);
	}
    }

    /* Check resulting values. */
    int nval = output_val_vars.size();
    Dout (cout << "total " << output_val_vars.size() << " possible output values" << endl);
    if (nval == 0) {
	Dout (cout << "no output values, nothing to do" << endl);
	return false;
    }

    /* Form output, distinguish empty (false) from non-empty. */
    output = -YES;
    vector<int> &output_vals = output.num_ranges;

    const bool is_single_val = (nval == 1);
    bool is_allocated = false;

    /* Compute guard disjuncts for each possible output value. */
    int k = 0;
    for (map<int, vector<int> >::iterator it = output_val_vars.begin();
	 it != output_val_vars.end(); it++, k++)
    {
	const int val = it->first;
	const vector<int> &vars = it->second;
	int nvar = vars.size();

	/* Summon guard variables for disjunction generation. */
	scratchpadRefit (nvar + 1);
	int last_term_idx = 0;
	int last_var;
	for (int i = 0; i < nvar; i++)
	    scratchpad[++last_term_idx] = last_var = vars[i];

	/* Associate guard variable with current output value. */
	if (is_single_val && last_term_idx == 1)
	    output.setId (last_var);
	else {
	    /* Allocate fresh guard variables, if not done so far. */
	    if (! is_allocated) {
		output = Tvalue (TVAL_SPARSE, dir.newAnonymousVar (nval), nval);
		is_allocated = true;
	    }

	    /* Generate disjunction for current value's guard. */
	    scratchpad[0] = output.getId (k);
	    mng.addBigOrClause (&scratchpad[0], last_term_idx);
	}

	/* Store current output value. */
	output_vals.push_back (val);
    }

    /* Sparsify. */
    output.sparsify();

    Dout (cout << "arracc: sparse subset complete, " << output << endl);
    return true;
}

void NodesToSolver::visit( DIV_node& node ){
    Dout( cout<<" DIV "<<endl );
    if(!checkParentsChanged( node, true)){ return; }
    processArith<divides<int> >(node);
    return;
}
void NodesToSolver::visit( MOD_node& node ){
    Dout( cout<<" MOD "<<endl );
    if(!checkParentsChanged( node, true)){ return; }
    processArith<modulus<int> >(node);
    return;
}


void
NodesToSolver::visit (EQ_node &node)
{
    Dout (cout << " EQ " << endl);
    if (checkParentsChanged (node, true))
#ifdef HAVE_BVECTARITH
	intBvectEq (node);
#else
	processComparissons<equal_to<int> > (node);
#endif /* HAVE_BVECTARITH */
}

void
NodesToSolver::visit (LT_node &node)
{
    Dout (cout << " LT " << endl);
    if (checkParentsChanged (node, true))
#ifdef HAVE_BVECTARITH
	intBvectLt (node);
#else
	processComparissons<less<int> > (node);
#endif /* HAVE_BVECTARITH */
}

void
NodesToSolver::visit (LE_node &node)
{
    Dout (cout << " LE " << endl);
    if (checkParentsChanged (node, true))
#ifdef HAVE_BVECTARITH
	intBvectLe (node);
#else
	processComparissons<less_equal<int> > (node);
#endif /* HAVE_BVECTARITH */
}

void
NodesToSolver::visit (GT_node &node)
{
    Dout (cout << " GT " << endl);
    if (checkParentsChanged (node, true))
#ifdef HAVE_BVECTARITH
	intBvectGt (node);
#else
	processComparissons<greater<int> > (node);
#endif /* HAVE_BVECTARITH */
}

void
NodesToSolver::visit (GE_node &node)
{
    Dout (cout << " GE " << endl);
    if (checkParentsChanged (node, true))
#ifdef HAVE_BVECTARITH
	intBvectGe (node);
#else
	processComparissons<greater_equal<int> > (node);
#endif /* HAVE_BVECTARITH */
}

void NodesToSolver::visit( ARRASS_node& node ){
    Dout(cout<<"             ARRASS:"<<endl);
    // mother = index
    // multi-mother[0] = old-value;
    // multi-mother[1] = new-value;
    // if( mother == quant ) return multi-mother[1]; else return multi-mother[0];
    bool_node* mother = node.mother;
    const Tvalue& mval = tval_lookup(mother) ;
    int quant = node.quant;
    Dout(cout<<" mother = "<<((mother != NULL)?mother->get_name():"NULL")<<"  mid = "<<mval<<"  mquant = "<<quant<<endl);
    vector<bool_node*>::iterator it = node.multi_mother.begin();    
    Assert( node.multi_mother.size() == 2 , "THIS SHOULDN't HAPPEN");
    vector<Tvalue> choices(2);
    vector<bool_node*> mothers(2);
    bool parentSame = true;
    bool isBoolean=true;
    for(int i=0; it != node.multi_mother.end(); ++i, ++it){
		const Tvalue& cval = tval_lookup(*it);
		if( cval.isSparse() ){
		    isBoolean = false;
		}
		mothers[i] = *it;
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"   ");
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
    }
    if(!checkParentsChanged( node, parentSame)){ return; }
    int guard;
    if( !mval.isSparse() ){
		if(quant > 1){
		    guard = -YES;
		}else{	    
		    Dout(cout<<" mval = "<<mval<<endl);
		    guard = dir.addXorClause(mval.getId (), quant==0?YES:-YES);
		}
    }else{
		guard = -YES;
		const vector<int>& nrange = mval.num_ranges;
		for(int i=0; i<nrange.size(); ++i){
		    if( nrange[i] == quant){
			guard = mval.getId (i);
			break;
		    }
		}
    }
    Dout(cout<<" guard = "<<guard<<endl);
    if(isBoolean){
		Dout(cout<<" is boolean"<<endl);
		int cvar = dir.addChoiceClause(guard , choices[1].getId (), choices[0].getId ());
		node.flag = node_ids[node.id].isNull() || node_ids[node.id].getId () != cvar;
		if( node.flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES oirga;"<<endl; }
		node_ids[node.id] = cvar;
		return;
    }else{
		Dout(cout<<" is not boolean"<<endl);
		Tvalue& mid0 = choices[0];
		Tvalue& mid1 = choices[1];
		if( !mid0.isSparse() ){
		    mid0.makeSparse(dir);
		}
		if( !mid1.isSparse() ){
		    mid1.makeSparse(dir);
		}
		if(guard == YES){
		    node.flag = node_ids[node.id] != mid1;
		    if( node.flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES asdf"<<endl; }
		    node_ids[node.id] = mid1;
		    Dout( cout<<"var "<< mid1 <<endl);
		    return;
		}
		if(guard == -YES){
		    node.flag = node_ids[node.id] != mid0;
		    if( node.flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES paoiu"<<endl; }
		    node_ids[node.id] = mid0;
		    Dout( cout<<"var "<< mid0 <<endl);
		    return;
		}
		int i=0, j=0;
		vector<int>& nr0 = mid0.num_ranges;
		vector<int>& nr1 = mid1.num_ranges;
		vector<int> res;
		res.reserve(nr0.size() + nr1.size());
		vector<int>& out = node_ids[node.id].num_ranges;
		out.reserve(nr0.size() + nr1.size());
		while(i < nr0.size() || j< nr1.size()){
		    bool avi = i < nr0.size();
		    bool avj = j < nr1.size();
		    int curri = avi ? nr0[i]  : -1;
		    int currj = avj ? nr1[j]  : -1;
		    if( curri == currj && avi && avj){
				Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
				int cvar1 = dir.addAndClause( mid0.getId (i), -guard);
				int cvar2 = dir.addAndClause( mid1.getId (j), guard);
				int cvar3 = dir.addOrClause( cvar2, cvar1);
				out.push_back(curri);
				res.push_back(cvar3);
				i++;
				j++;
				continue;
			}
		    if((curri < currj && avi) || !avj){
				Dout(cout<<" curri = "<<curri<<endl);
				int cvar = dir.addAndClause( mid0.getId (i), -guard);
				out.push_back(curri);
				res.push_back(cvar);
				i++;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				Dout(cout<<" currj = "<<currj<<endl);
				int cvar = dir.addAndClause( mid1.getId (j), guard );
				out.push_back(currj);
				res.push_back(cvar);
				j++;
				continue;
		    }
		    Assert(false, "Should never get here");
		}
		out.resize(res.size ());
		Assert( res.size () > 0, "This should not happen here2");
		int newID = dir.newAnonymousVar();
		for(int k=1; k<res.size(); ++k){
		    int cvar = dir.newAnonymousVar();
		    Assert( cvar == newID + k, "SolveFromInput: cvar != newID + k");
		}
		for(int k=0; k<res.size(); ++k){
		    int val = res[k];
		    mng.addEqualsClause( newID+k, val);
		}
		node_ids[node.id].setId(newID);
		node_ids[node.id].sparsify ();
		return;
    }
}




void NodesToSolver::visit( ACTRL_node& node ){
	int size = node.multi_mother.size();
	vector<bool_node*>::iterator it = node.multi_mother.begin();	
	bool parentSame = true;
	vector<int> ids(node.multi_mother.size());
	for(int i=0 ; it != node.multi_mother.end(); ++it, ++i){
		{
			Dout( cout<<" ACTRL "<<*it<<" nodeids = "<<tval_lookup(*it));
			ids[i]=tval_lookup(*it).getId ();
		}
		Dout( cout<<"   ids[i]="<<ids[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
	}
	if(!checkParentsChanged( node, parentSame)){Dout(cout<<"@ACTRL "<<node.name<<"  "<<node_ids[node.id]<<"   "<<&node<<endl);	 return; }
	vector<int>& tmp = node_ids[node.id].num_ranges;
	varRange vr = dir.getSwitchVars(ids, size, tmp);
	node_ids[node.id].setId(vr.varID);
	node_ids[node.id].sparsify ();
	Dout(cout<<"&ACTRL "<<node.name<<"  "<<node_ids[node.id]<<"  "<<tmp.size()<<"   "<<&node<<endl);
	return;
}

void
NodesToSolver::visit (ASSERT_node &node)
{
	assert (node.mother && ! node.father);

	Tvalue fval = tval_lookup (node.mother);
	if (! checkParentsChanged (node, true)) {
		Dout (cout << "ASSERT " << fval << "unchanged" << endl );
		return;
	}

	if( node.isHard()){
		dir.addHardAssertClause (fval.getId ());
	}else{
		if(fval.getId() == -YES ) {  node.printSubDAG(cout); }
		dir.addAssertClause (fval.getId ());
	}

	Dout (cout << "ASSERT " << node.get_name() << " " << fval
		  << " " << &node << endl);

	return;
}


void
NodesToSolver::visit (CONST_node &node)
{	
	
	if( node.getVal() == 1 ){
		node_ids[node.id] = tvYES;
	}else if( node.getVal() == 0){
		node_ids[node.id] = tvYES;
		node_ids[node.id].bitAdjust(false);
	}else{
		node_ids[node.id] = tvOne;
		node_ids[node.id].intAdjust(node.getVal());
	}
	Dout (cout << "CONST " << node.get_name() << " " << node_ids[node.id]<< endl);
}


void
NodesToSolver::scratchpadRefit (size_t fit)
{
    size_t size = scratchpad.size();
    while (size < fit)
	size *= 2;
    if (size != scratchpad.size())
	scratchpad.resize (size);
}


bool NodesToSolver::checkParentsChanged(bool_node& node, bool more){
	if(( node.father== NULL || !node.father->flag ) &&
			( node.mother== NULL || !node.mother->flag )&&
			more
			){ node.flag =false; return false || mng.ignoreOld(); }else{ node.flag = true; return true;}
}

