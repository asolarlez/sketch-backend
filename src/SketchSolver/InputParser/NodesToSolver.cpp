#include "NodesToSolver.h"
#include <algorithm>
#include "timerclass.h"

/* 
 * Uncomment this to switch to bit-vector operators / comparators.
 * TODO switch to some other (dynamic) mechanism...
 */
// #define HAVE_BVECTARITH

// #define Dout(msg) msg
// #define DebugOut( node, tval )  cout<<" NODE= "<<node.get_name()<<" \t"<<tval<<endl;


/*

void NodesToSolver::process(BooleanDAG& bdag){
	int i=0;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		(*node_it)->accept(*this);
		DebugOut((**node_it), node_ids[(*node_it)->id]);
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    	}
	}
}

*/


template<typename COMP> void
NodesToSolver::processComparissons (bool_node& node)
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
		if(orTerms == mval.getSize() * fval.getSize()){
			node_ids[node.id] = YES;
		}else{
			scratchpad[0] = 0;
			int result = dir.addBigOrClause( &scratchpad[0], orTerms);
			node_ids[node.id] = result;
		}
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
NodesToSolver::processArith (bool_node &node)
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
		//if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
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
			Assert(numbers.size() < INTEGERBOUND, "AN INTEGER GOT REALLY BIG, AND IS NOW BEYOND THE SCOPE OF THE SOLVER");
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
	Assert( vals > 0 && vals == numbers.size(), "This should not happen here");
	int newID = -1;
	tmp.resize(vals);
	map<int, int>::iterator it = numbers.begin();

	{
		int quant = it->first;		
		newID = it->second;
		tmp[0] = quant;
	}
	++it;
	for(int i=1; i<vals; ++i, ++it){
		int quant = it->first;
		tmp[i] = quant;
		if(newID + i !=  it->second){
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
	it = numbers.begin();
	for(int i=0; i<vals; ++i, ++it){
		int quant = it->first;
		tmp[i] = quant;
		dir.addEqualsClause(it->second, newID + i);
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
	Dout(cout<<"AND "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
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
	Dout(cout<<"OR "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
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
	Dout(cout<<"XOR "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}

void
NodesToSolver::visit (SRC_node &node)
{
    int iid = node.ion_pos;

	if( node.children.size() == 0){ return; }


	Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
		
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
		Dout( cout << " input " << node.get_name () << " = " << node_ids[node.id] << endl );
		Dout (cout << dir.getArr (node.get_name(), 0) << " has value " << node_values[(&node)]
	      << "   " << (&node) << "    " << node_ids[node.id] << endl);
    } else {
		node_ids[node.id] = dir.getArr (node.get_name(), 0);
		//This could be removed. It's ok to setSize when get_nbits==1.		
		if (node.get_nbits () > 1) {
		    node_ids[node.id].setSize (node.get_nbits ());
		    Dout (cout << "setting input nodes" << node.get_name() << endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
		    node_ids[node.id].makeSparse (dir);
#endif /* HAVE_BVECTARITH */
		}
	Dout (cout << "REGISTERING " << node.get_name() << "  " << node_ids[node.id]
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
    if(!( mval.getType() == TVAL_BVECT && mval.getSize() == 1 )){
    	cerr<<" BAD NODE "<<endl;
    	node.mother->outDagEntry(cerr);
    	node.outDagEntry(cerr);
		/*
    	for(int i=0; i<node.children.size(); ++i){
    		node.children[i]->outDagEntry(cerr);
    	}
		*/
    }
    Assert( mval.getType() == TVAL_BVECT && mval.getSize() == 1, "Bad Type for NOT "<<mval.getSize()<<" "<<mval.getType());
    Tvalue nvar = -mval.getId ();
    boolNodeUpdate (node, nvar);

    Dout (cout << "PT " << node.get_name() << " " << nvar << " " << &node << endl);
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

    Dout (cout << "NEG " << node.get_name() << " " << nvar << " " << &node << endl);
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
		    Dout( cout<<" control "<<node.get_name()<<" = "<<node_ids[node.id]<<"    "<<node_values[(&node)]<<endl);
		}else{
		    node_ids[node.id] = node_values[(&node)]*YES;
		}
		Dout( cout<< dir.getArr(node.get_name(), 0)<<" has value "<<node_values[(&node)]<<"   "<< (&node) <<"    "<< node_ids[node.id] <<endl  );
		return;
    }else{
		node_ids[node.id] = dir.getArr(node.get_name(), 0);
		if( node.get_nbits() > 1 ){ //This could be removed. It's ok to setSize when get_nbits==1.
		    node_ids[node.id].setSize( node.get_nbits() );
		    Dout(cout<<"setting control nodes"<<node.get_name()<<endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
		    node_ids[node.id].makeSparse(dir);
#endif /* HAVE_BVECTARITH */
		}
		Dout(cout<<"CONTROL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
		return;
    }
}



void NodesToSolver::visit( PLUS_node& node ){
	Dout( cout<<" PLUS: "<<node.get_name()<<endl );
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
	Dout( cout<<" TIMES: "<<node.get_name()<<endl );
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


void NodesToSolver::visit( ARRACC_node& node ){


	Dout(cout<<" ARRACC "<<endl);
	const Tvalue& omv = tval_lookup(node.mother) ;	
	bool isSparse = omv.isSparse();
    Dout(cout<<" mother = "<<node.mother->get_name()<<"  mid = "<<omv<<" "<<endl);
	if( isSparse && omv.getId () == YES ){
		int idx = omv.num_ranges[0];

		if( idx >= node.multi_mother.size()){
			node_ids[node.id] = -YES;
			Dout( cout<<node.get_name()<<" SHORTCUT "<<omv<<" out of range"<<endl );
			return;
		}

		bool_node* choice = node.multi_mother[idx];
		
		if(!checkParentsChanged( node, ( choice== NULL || !choice->flag ))){ Dout(cout<<"Parents did not change "<<endl); return; }
		node_ids[node.id] = tval_lookup(choice);
		Tvalue& cval = node_ids[node.id];
		
		Dout( cout<<node.get_name()<<" Shortcout = "<<cval<<endl );
		return;
	}
	
	vector<bool_node*>::iterator it = node.multi_mother.begin();	
	vector<Tvalue> choices(node.multi_mother.size());
	bool parentSame = true;
	bool parentSameBis = true;
	bool isBoolean=true;
	vector<int>::const_iterator itbeg, itend, itfind;
	itbeg = omv.num_ranges.begin();
	itend = omv.num_ranges.end();

//	aracctimer.restart();
//	flooptimer.restart();
	for(int i=0; it != node.multi_mother.end(); ++i, ++it){
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  ");
		const Tvalue& cval = tval_lookup(*it);
		if( cval.isSparse() ){
			isBoolean = false;
		}
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag);
	}
	if( omv.isSparse() ){
		parentSame = true;
		for( ; itbeg < itend; ++itbeg){
			if( *itbeg < node.multi_mother.size() ){
				bool_node* cnode = node.multi_mother[*itbeg];
				parentSame = parentSame && ( (cnode)== NULL || !cnode->flag);
				Dout(cout<<"Checking parents same "<<*itbeg<<" = "<<parentSame);
			}
		}
	}
//	flooptimer.stop().print();
//	COUT<<" FIRST LOOP mmsize ="<<node.multi_mother.size()<<" omv.num_ranges.size()="<<omv.num_ranges.size()<<endl;
	if(!checkParentsChanged( node, parentSame)){ Dout(cout<<"Parents did not change "<<endl);
												 //aracctimer.stop().print();
												 return; }
	if(!isBoolean){
//		nonbooltimer.restart();
		doNonBoolArrAcc(node, node_ids[node.id]);
//		nonbooltimer.stop().print();
//		aracctimer.stop().print();
		Dout(cout<<node.get_name()<<"  "<<node_ids[node.id]<<endl);
		return;
	}
	Dout(cout<<" is boolean"<<endl);
	bool_node* mother = node.mother;
	const Tvalue& mval = tval_lookup(mother) ;
	Dout(cout<<" mother = "<<mval<<"   "<<endl);
	Assert( mother != NULL, "This should never happen");
	if( !mval.isSparse() ){ //mother->type != bool_node::ARITH		
		int cvar;
		if(choices.size()>=2){
			Dout( cout<<" replacing with choice "<<mval<<", "<<choices[1]<<", "<<choices[0]<<endl );
			cvar = dir.addChoiceClause(mval.getId () , choices[1].getId (), choices[0].getId ());
		}else{
			if(choices.size()>=1){
				cvar = dir.addAndClause( mval.getId () , choices[0].getId ());
			}else{
				cvar = -YES;
			}
		}
		node_ids[node.id] = cvar;
		Dout(cout<<"ARRACC "<<node.get_name()<<"  "<<node_ids[node.id]<<"   "<<&node<<endl);
//		aracctimer.stop().print();
		return;
	}
//	elooptimer.restart();
	const vector<int>& nrange = mval.num_ranges;
	int cvar = -YES;
	int orTerms = 0;
	for(int i=0; i<nrange.size(); ++i){
		if( nrange[i] >= 0 && nrange[i] < choices.size() ){
			if( mval.getId (i) == YES){
				cvar = choices[nrange[i]].getId ();
				++orTerms;
				if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
				scratchpad[orTerms] = cvar;
			}else{
				if( mval.getId (i) != -YES ){
					cvar = dir.addAndClause( choices[nrange[i]].getId (), mval.getId (i) );
					++orTerms;
					if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms] = cvar;
				}
			}
		}
	}
	if( orTerms < 2){
		node_ids[node.id] = cvar;
	}else{
		scratchpad[0] = 0;
		int result = dir.addBigOrClause( &scratchpad[0], orTerms);
		node_ids[node.id] = result;		
	}
	Dout(cout<<"ARRACC "<<node.get_name()<<"  "<<node_ids[node.id]<<"   "<<&node<<endl);
//	elooptimer.stop().print();
//	aracctimer.stop().print();
	return;
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













void NodesToSolver::mergeTvalues(int guard, Tvalue& mid0, Tvalue& mid1, Tvalue& output, int& flag){
		if( !mid0.isSparse() ){
		    mid0.makeSparse(dir);
		}
		if( !mid1.isSparse() ){
		    mid1.makeSparse(dir);
		}
		if(guard == YES){
		    flag = output != mid1;
		    if( flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES asdf"<<endl; }
		    output = mid1;
		    Dout( cout<<"var "<< mid1 <<endl);
		    return;
		}
		if(guard == -YES){
		    flag = output != mid0;
		    if( flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES paoiu"<<endl; }
		    output = mid0;
		    Dout( cout<<"var "<< mid0 <<endl);
		    return;
		}
		int i=0, j=0;
		vector<int>& nr0 = mid0.num_ranges;
		vector<int>& nr1 = mid1.num_ranges;
		vector<int> res;
		res.reserve(nr0.size() + nr1.size());
		vector<int>& out = output.num_ranges;
		out.clear();
		out.reserve(nr0.size() + nr1.size());

		int inci = 1;
		int incj = 1;
		// There is an assumption that the num_ranges are monotonic. 
		// However, they could be monotonically increasing or monotonically decreasing.
		// So we need to check.
		if(nr0.size() > 1 && nr0[0] > nr0[1]){
			inci = -1;
			i = nr0.size() -1;
		}
		
		if(nr1.size() > 1 && nr1[0] > nr1[1]){
			incj = -1;
			j = nr1.size() -1;
		}

		while(i < nr0.size() || j< nr1.size()){
		    bool avi = i < nr0.size() && i >= 0;
		    bool avj = j < nr1.size() && j >= 0;
		    int curri = avi ? nr0[i]  : -1;
		    int currj = avj ? nr1[j]  : -1;
		    if( curri == currj && avi && avj){
				Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
				int cvar1 = dir.addAndClause( mid0.getId (i), -guard);
				int cvar2 = dir.addAndClause( mid1.getId (j), guard);
				int cvar3 = dir.addOrClause( cvar2, cvar1);
				out.push_back(curri);
				res.push_back(cvar3);
				i = i + inci;
				j = j + incj;
				continue;
			}
		    if((curri < currj && avi) || !avj){
				Dout(cout<<" curri = "<<curri<<endl);
				int cvar = dir.addAndClause( mid0.getId (i), -guard);
				out.push_back(curri);
				res.push_back(cvar);
				i = i + inci;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				Dout(cout<<" currj = "<<currj<<endl);
				int cvar = dir.addAndClause( mid1.getId (j), guard );
				out.push_back(currj);
				res.push_back(cvar);
				j = j + incj;
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
		    dir.addEqualsClause( val, newID+k);
		}
		output.setId(newID);
		output.sparsify ();
		return;
}





void NodesToSolver::visit( ARRASS_node& node ){
	Dout(cout<<"             ARRASS: "<<node.get_name()<<endl);
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
		//if( node.flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES oirga;"<<endl; }
		node_ids[node.id] = cvar;
		return;
    }else{
		Dout(cout<<" is not boolean"<<endl);
		Tvalue& mid0 = choices[0];
		Tvalue& mid1 = choices[1];
		mergeTvalues(guard, mid0, mid1, node_ids[node.id], node.flag);
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
	if(!checkParentsChanged( node, parentSame)){Dout(cout<<"@ACTRL "<<node.get_name()<<"  "<<node_ids[node.id]<<"   "<<&node<<endl);	 return; }
	vector<int>& tmp = node_ids[node.id].num_ranges;
	varRange vr = dir.getSwitchVars(ids, size, tmp);
	node_ids[node.id].setId(vr.varID);
	node_ids[node.id].sparsify ();
	Dout(cout<<"&ACTRL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<tmp.size()<<"   "<<&node<<endl);
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

/*	if(node.id == 125){
		set<const bool_node*> s;
			cout<<"digraph G{"<<endl;
			node.printSubDAG(cout, s);
			cout<<"}"<<endl;
			cout<<" slice size = "<<s.size()<<endl;
	}
	*/

	Assert(!node.isHard(), "This functionality is depracted");
	{
		if(fval.getId() == -YES ) {  
			cerr<<"  UNSATISFIABLE ASSERTION "<<node.getMsg()<<endl; 
			/*
			set<const bool_node*> s;
			cout<<"digraph G{"<<endl;
			node.printSubDAG(cout, s);
			cout<<"}"<<endl;
			cout<<" slice size = "<<s.size()<<endl;
			*/
		}
		dir.addAssertClause (fval.getId ());
	}
	//cout<<"|"<<node.getMsg()<<"|"<<endl;
	

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


void NodesToSolver::doNonBoolArrAcc(ARRACC_node& node, Tvalue& output){
	Dout( cout<<" non boolean array "<<endl );
	vector<bool_node*>::iterator it = node.multi_mother.begin();
	
	int N = node.multi_mother.size();
	vector<Tvalue> choices(N);
	for(int i=0; i < N; ++i, ++it){
		choices[i] = tval_lookup (*it, TVAL_SPARSE);
		choices[i].makeSparse (dir);
	}
	bool_node* mother = node.mother;
	Tvalue mval = tval_lookup (mother, TVAL_SPARSE);
	mval.makeSparse (dir);

	map<int, vector<int> > newVals;
	int vsize = N;
	vector<int>& nrange = mval.num_ranges;

	for(int i=0; i<nrange.size(); ++i){
		if( nrange[i] < vsize && nrange[i] >= 0){
			vector<int>& cvalues = choices[nrange[i]].num_ranges;
			Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<"  cvsize="<<cvalues.size()<<endl );
			for(int j=0; j<cvalues.size(); ++j){
				int cvar = dir.addAndClause( mval.getId (i), choices[nrange[i]].getId (j) );
				newVals[ cvalues[j] ].push_back(cvar);
				Dout( cout<<" cvalues["<<j<<"] = "<<cvalues[j]<<endl );
			}
		}else{
			Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<" OUT OF RANGE"<<endl );
			newVals[ 0 ].push_back( mval.getId (i));
			//mng.assertVarClause(-mval.getId (i));
			//Armando: Can't have this kind of assertions, because at this level we don't know whether this block
			//will execute or not.
		}
	}

	vector<int>& result = output.num_ranges;
	result.clear();
	Dout(cout<<" newVals.size() == " << newVals.size()<<endl );
	if(newVals.size() == 1){
		map<int, vector<int> >::iterator it = newVals.begin();
		vector<int>& vars = it->second;
		int orTerms = 0;
		while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
		for(int i=0; i<vars.size(); ++i){
			++orTerms;
			scratchpad[orTerms] = vars[i];
		}
		if( orTerms == 1){
			output.setId(vars[0]);
			result.push_back(it->first);
			output.sparsify ();
		}else{
			scratchpad[0] = 0;
			int cvar = dir.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
			output.setId(cvar);
			output.sparsify ();
		}
	}else{
		if(newVals.size() == 0){			
			output = Tvalue( -YES );			
			Dout(cout<<" after sparsification "<<output<<endl);
			return;
		}
		//Assert( newVals.size() > 0, "This should not happen here3");
		int newID = dir.newAnonymousVar();
		output.setId(newID);
		int k=1;
		for(k = 1; k< newVals.size(); ++k){
			int cvar = dir.newAnonymousVar();
			Assert( cvar == newID + k, "SolveFromInput3: cvar != newID + k ");
		}
		k = 0;
		for(map<int, vector<int> >::iterator it = newVals.begin(); it != newVals.end(); ++it, ++k){
			vector<int>& vars = it->second;
			int orTerms = 0;
			while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
			for(int i=0; i<vars.size(); ++i){
				++orTerms;
				scratchpad[orTerms] = vars[i];
			}
			scratchpad[0] = newID + k;
			dir.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
		}
		output.sparsify ();
	}
}


bool NodesToSolver::checkParentsChanged(bool_node& node, bool more){
	if(( node.father== NULL || !node.father->flag ) &&
			( node.mother== NULL || !node.mother->flag )&&
			more
			){ node.flag =false; return false || dir.ignoreOld(); }else{ node.flag = true; return true;}
}

