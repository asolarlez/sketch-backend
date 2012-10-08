#include "NodesToSolver.h"
#include <algorithm>
#include "timerclass.h"

#include "CommandLineArgs.h"
#include "PrintInteresting.h"

//extern CommandLineArgs* PARAMS;

// #define Dout(msg) msg

/* 
 * Uncomment this to switch to bit-vector operators / comparators.
 * TODO switch to some other (dynamic) mechanism...
 */
// #define HAVE_BVECTARITH

// #define Dout(msg) msg
// #define DebugOut( node, tval )  cout<<" NODE= "<<node.get_name()<<" \t"<<tval<<endl;

//#define Dout(msg) msg
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
// #define Dout( out )      out 


class PrintSource: public PrintInteresting{
	vector<Tvalue> &node_ids;
public:
	PrintSource(vector<Tvalue> &nids):node_ids(nids){}
	virtual void visit( ARRACC_node& node ){
		int sz = node_ids[node.id].getSize();
		for(int i=0; i<node.multi_mother.size(); ++i){
			if(node_ids[node.multi_mother[i]->id].getSize() > (sz/2)-1){
				tovisit[node.multi_mother[i]->id] = true;
			}
		}
	}
	virtual void print(BooleanDAG& bdag, int seed){
		for(int i=0; i<=seed; ++i){
			if(tovisit[i]){
				int sz = node_ids[i].getSize();
				cout<<"sz="<<sz<<"\t"<<bdag[i]->lprint()<<endl;
			}
		}
	}
};

void advanceToEndIdx(int& iend, int cidx, vector<guardedVal>&tv){
	while(iend < tv.size() && tv[iend].idx == cidx){
		++iend;
	}
}

int NodesToSolver::compareRange(vector<guardedVal>& mv, int mstart, int mend, vector<guardedVal>& fv, int fstart, int fend){
	int orTerms = 0;
	int i=mstart, j=fstart;
	int inci = 1;
	int incj = 1;
	// There is an assumption that the num_ranges are monotonic. 
	// However, they could be monotonically increasing or monotonically decreasing.
	// So we need to check.
	if(mend -1 > mstart && mv[mstart].value > mv[mstart+1].value){
		inci = -1;
		i = mend -1;
	}
		
	if(fend -1> fstart && fv[fstart].value > fv[fstart+1].value){
		incj = -1;
		j = fend -1;
	}
	vector<int> noeq;
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;
			int curri = avi ? mv[i].value  : -1;
			int currj = avj ? fv[j].value  : -1;
			if( curri == currj && avi && avj){
				int cvar = dir.addAndClause(mv[i].guard, fv[j].guard);
				++orTerms;
				if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
				scratchpad[orTerms] = cvar;
				i = i + inci;
				j = j + incj;
				continue;
			}
			if((curri < currj && avi) || !avj){
				noeq.push_back(mv[i].guard);
				i = i + inci;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				noeq.push_back(fv[j].guard);
				j = j + incj;
				continue;
		    }
	}
	
	if(orTerms==0){
		return -YES;
	}else{
		int rv;
		if(orTerms==1){
			rv = scratchpad[1];
		}else{
			scratchpad[0] = 0;
			rv = dir.addBigOrClause( &scratchpad[0], orTerms);			
		}
		for(vector<int>::iterator it = noeq.begin(); it < noeq.end(); ++it){
			dir.addHelperC(-(*it), -rv);
		}
		return rv;
	}

}


template<typename COMP> void
NodesToSolver::compareArrays (bool_node& node,  Tvalue& tmval,  Tvalue& tfval){
	vector<guardedVal>& mv = tmval.num_ranges;
	vector<guardedVal>& fv = tfval.num_ranges;

	int midx = mv[0].idx;
	int fidx = fv[0].idx;
	int mistart = 0;
	int fistart = 0;
	int miend = 0;
	int fiend = 0;
	advanceToEndIdx(miend, midx, mv);
	advanceToEndIdx(fiend, fidx, fv);
	Tvalue mdef;
	if(midx==-1){
		for(int i=0; i<miend; ++i){
			mdef.num_ranges.push_back(guardedVal(mv[i].guard, mv[i].value));
		}
	}else{
		mdef = tvOne;
		mdef.num_ranges[0].value = -333;
	}
	Tvalue fdef;
	if(fidx==-1){
		for(int i=0; i<fiend; ++i){
			fdef.num_ranges.push_back(guardedVal(fv[i].guard, fv[i].value));
		}
	}else{
		fdef = tvOne;
		fdef.num_ranges[0].value = -333;
	}
	int cvar = YES;
	bool moreM = true;
	bool moreF = true;
	do{
		if(midx == fidx && moreM && moreF){				
				int rv = compareRange(mv, mistart, miend, fv, fistart, fiend);
				cvar = dir.addAndClause(cvar, rv);	
				if(miend < mv.size()){
					mistart=miend;
					midx = mv[mistart].idx;
					advanceToEndIdx(miend, midx, mv);
				}else{
					moreM=false;
				}
				if(fiend<fv.size()){
					fistart=fiend;
					fidx=fv[fistart].idx;
					advanceToEndIdx(fiend, fidx, fv);
				}else{
					moreF=false;
				}
				continue;
		}
		if((midx < fidx && moreM) || !moreF){
			int rv = compareRange(mv, mistart, miend, fdef.num_ranges, 0, fdef.num_ranges.size());
			cvar = dir.addAndClause(cvar, rv);
			if(miend < mv.size()){
				mistart=miend;
				midx = mv[mistart].idx;
				advanceToEndIdx(miend, midx, mv);
			}else{
				moreM=false;
			}
			continue;
		}else{
			int rv = compareRange(mdef.num_ranges, 0, mdef.num_ranges.size(), fv, fistart, fiend);
			cvar = dir.addAndClause(cvar, rv);
			if(fiend<fv.size()){
				fistart=fiend;
				fidx=fv[fistart].idx;
				advanceToEndIdx(fiend, fidx, fv);
			}else{
				moreF=false;
			}
			continue;
		}
	}while(moreM || moreF);

	node_ids[node.id] = cvar;	
}

template<typename COMP> void
NodesToSolver::processComparissons (bool_node& node, bool revFval)
{
    bool_node *mother = node.mother;
    Tvalue mval = tval_lookup (mother, TVAL_SPARSE);    

    bool_node *father = node.father;
    Tvalue fval = tval_lookup (father, TVAL_SPARSE);
	if(mval.isArray() || fval.isArray()){
		if(mval.isBvect()){
			mval.makeSparse(dir);
		}
		if(fval.isBvect()){
			fval.makeSparse(dir);
		}
		compareArrays<COMP>(node, mval, fval);
		return;
	}

	mval.makeSparse (dir);
    fval.makeSparse (dir);
    int cvar = -YES;
    COMP comp;
    Dout(cout<<"SIZES = "<<mval.getSize ()<<", "<<fval.getSize ()<<endl);
    int orTerms = 0;
	vector<char> mc(mval.getSize(), 'n');
	vector<char> fc(fval.getSize(), 'n');
	int flow = 0;
	int fhigh = fval.getSize ();
	int finc = 1;
	if(revFval){
		flow = fhigh-1;
		fhigh = -1;
		finc = -1;
	}
    for(int i=0; i<mval.getSize (); ++i){
		for(int j=flow; j!=fhigh; j = j+finc){
		    Dout(cout<<"COMPARING "<<mval[i]<<", "<<fval[j]<<endl);
		    if(comp(mval[i], fval[j])){
				cvar = dir.addAndClause(mval.getId (i), fval.getId (j));
				mc[i] = 'y';
				fc[j] = 'y';
				++orTerms;
				if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
				scratchpad[orTerms] = cvar;
			}
		}
    }
    if( orTerms < 2 ){
		for(int i=0; i<mc.size(); ++i){ if(mc[i] =='n'){ dir.addHelperC(-cvar, -mval.getId (i)); }  }
		for(int i=0; i<fc.size(); ++i){ if(fc[i] =='n'){ dir.addHelperC(-cvar, -fval.getId (i)); }  }
		node_ids[node.id] = cvar;
    }else{
		if(orTerms == mval.getSize() * fval.getSize()){
			node_ids[node.id] = YES;
		}else{
			scratchpad[0] = 0;
			int result = dir.addBigOrClause( &scratchpad[0], orTerms);
			for(int i=0; i<mc.size(); ++i){ if(mc[i] =='n'){ dir.addHelperC(-result, -mval.getId (i)); }  }
			for(int i=0; i<fc.size(); ++i){ if(fc[i] =='n'){ dir.addHelperC(-result, -fval.getId (i)); }  }
			node_ids[node.id] = result;
		}
    }
    Dout( cout<<node.get_name()<<" :=  "<<node_ids[node.id]<<endl);
    return;
}


template<typename THEOP>
inline int NodesToSolver::doArithExpr(int quant1, int quant2, int id1, int id2, THEOP comp){
	int tt = comp(quant1, quant2);
	
	if(PARAMS->randBnd > 0 && abs(tt) > PARAMS->randBnd){ 
		//cout<<"WARNING: I am doing some really crazy stuff!!!!"<<endl;
		if(!dir.getMng().isNegated()){
			tt = (rand() % (2*PARAMS->randBnd))-PARAMS->randBnd; 
		}else{
			tt =  PARAMS->randBnd*2;
		}
	}
	return tt;
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
    lsize--;
    rsize--;
    for (int i = 0; i < osize; i++) {
	/* - get current value bits, or sign bit if exceeded either size */
		int l = (i < lsize ? lval.getId(i) : lsign);
		int r = (i < rsize ? rval.getId(i) : rsign);

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
    
    int nvars = dval.getSize () + 1;
    vector<int> dvars(nvars);
    int anybit = dvars[0] = dir.newAnonymousVar ();
    for (int i = 1; i < nvars; i++)
		dvars[i] = dval.getId(i-1);
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
	if( mval.getSize() > 200 ){ 
		cout<<"Sparse representation size = "<<mval.getSize()<<endl;
		//PrintSource ps(node_ids);
		//ps.process(*tmpdag, node.mother->id);								
		//tmpdag->printSlice(mother, cout);
	}
	bool_node* father = node.father;
	Tvalue fval = tval_lookup (father, TVAL_SPARSE);
	fval.makeSparse (dir);
	bool isSum = node.type == bool_node::PLUS || node.type == bool_node::TIMES;
	bool skipZeros = node.type == bool_node::TIMES || node.type == bool_node::DIV || node.type == bool_node::MOD;
	map<int, int> numbers;
	Tvalue& oval = node_ids[node.id];
	vector<guardedVal>& tmp = oval.num_ranges;
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
	if(skipZeros){
		int zem=0, zef=0;
		for(int i=0; i<mval.getSize (); ++i){
			if(mval[i]==0){ 
				zem = mval.getId (i);	
				break;
			}
		}
		for(int j=0; j<fval.getSize (); ++j){
			if(fval[j] == 0){
				zef = fval.getId(j);
				break;
			}
		}
		if(zem == 0 && zef != 0){
			numbers[0] = zef;
			++vals;
		}
		if(zem != 0 && zef == 0){
			numbers[0] = zem;
			++vals;
		}
		if(zem != 0 && zef != 0){
			numbers[0] = dir.addOrClause(zem, zef);
			++vals;
		}
	}
	for(int i=0; i<mval.getSize (); ++i){
		if(skipZeros && mval[i] == 0){ continue; }
	    for(int j=0; j<fval.getSize (); ++j){
			if(skipZeros && fval[j] == 0){ continue; }
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
				if(isSum){
					// Because the Tvalues have all their values unique,
					//and because if i != j then a + i == b + j -> a != b
					// and because only 1 id can be true in each Tvalue
					//This optimization is sound.
					//The same is true for a*i == b*j as long as a and i != 0
					it->second = dir.addChoiceClause(mval.getId (i),fval.getId (j), it->second);
				}else{
				int cvar = dir.addAndClause(mval.getId (i),fval.getId (j));
				int cvar2 = dir.addOrClause(cvar, it->second);
				it->second = cvar2;
				}
				//							ctimer.stop();
			}else{
				//							dtimer.restart();
				int cvar = dir.addAndClause(mval.getId (i), fval.getId (j));				
				if(numbers.size() >= INTEGERBOUND){
					PrintSource ps(node_ids);
					ps.process(*tmpdag, node.id);
					Assert(false, "AN INTEGER GOT REALLY BIG, AND IS NOW BEYOND THE SCOPE OF THE SOLVER");
				}
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


	Dout(cout<<"tmp size = "<<numbers.size ()<<endl);
	Assert( vals > 0 && vals == numbers.size(), "This should not happen here");
	int newID = -1;
	tmp.resize(vals);
	map<int, int>::iterator it = numbers.begin();

	{
		int quant = it->first;		
		newID = it->second;
		tmp[0] = guardedVal(newID, quant);
	}
	++it;
	for(int i=1; i<vals; ++i, ++it){		
		tmp[i] = guardedVal(it->second, it->first);		
	}
	oval.sparsify (dir);
	dir.addHelperC(oval);
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
    

	if( node.children.size() == 0){ return; }


	
		
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
		
    } else {
		int arrSz = node.getArrSz();		
		node_ids[node.id] = dir.getArr (node.get_name(), 0);
		//This could be removed. It's ok to setSize when get_nbits==1.		
		if (node.get_nbits () > 1 || arrSz >=0) {		    
		    Dout (cout << "setting input nodes" << node.get_name() << endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
			if(arrSz<0){
				node_ids[node.id].setSize (node.get_nbits ());
				Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
				node_ids[node.id].makeSparse (dir);
			}else{
				node_ids[node.id].setSize (node.get_nbits ()*arrSz);
				Assert( dir.getArrSize(node.get_name()) == arrSz*node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
				node_ids[node.id].makeArray (dir, node.get_nbits(), arrSz);
			}
#endif /* HAVE_BVECTARITH */
		}
		node_ids[node.id].markInput(dir);
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
		return;
    }else{
		Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd" );
		node_ids[node.id] = dir.getArr(node.get_name(), 0);
		if( node.get_nbits() > 1 ){ //This could be removed. It's ok to setSize when get_nbits==1.
		    node_ids[node.id].setSize( node.get_nbits() );
		    Dout(cout<<"setting control nodes"<<node.get_name()<<endl);
#ifndef HAVE_BVECTARITH
		    // In the future, I may want to make some of these holes not-sparse.
		    node_ids[node.id].makeSparse(dir);
#endif /* HAVE_BVECTARITH */
		}
		node_ids[node.id].markInput(dir);
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
	if( isSparse && omv.getId () == YES && omv.num_ranges.size() == 1){
		int idx = omv.num_ranges[0].value;

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
	bool isArray = false;
//	aracctimer.restart();
//	flooptimer.restart();
	for(int i=0; it != node.multi_mother.end(); ++i, ++it){
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  ");
		const Tvalue& cval = tval_lookup(*it);
		if( cval.isSparse() ){
			isBoolean = false;
		}
		if(cval.isArray()){
			isArray=true;
		}
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag);
	}
	if( omv.isSparse() ){
		vector<guardedVal>::const_iterator itbeg, itend, itfind;
		itbeg = omv.num_ranges.begin();
		itend = omv.num_ranges.end();
		parentSame = true;
		for( ; itbeg < itend; ++itbeg){
			if( itbeg->value < node.multi_mother.size() ){
				bool_node* cnode = node.multi_mother[itbeg->value];
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
	if(isArray){
		doArrArrAcc(node, node_ids[node.id]);
		return;
	}

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
	const vector<guardedVal>& nrange = mval.num_ranges;
	int cvar = -YES;
	int orTerms = 0;
	for(int i=0; i<nrange.size(); ++i){
		if( nrange[i].value >= 0 && nrange[i].value < choices.size() ){
			if( mval.getId (i) == YES){
				cvar = choices[nrange[i].value].getId ();
				++orTerms;
				if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
				scratchpad[orTerms] = cvar;
			}else{
				if( mval.getId (i) != -YES ){
					cvar = dir.addAndClause( choices[nrange[i].value].getId (), mval.getId (i) );
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
	processComparissons<equal_to<int> > (node, false);
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
	processComparissons<less<int> > (node, node.mother->type == bool_node::CONST);
#endif /* HAVE_BVECTARITH */
}













void NodesToSolver::mergeTvalues(int guard, const vector<guardedVal>& nr0, int nr0Start, int nr0End, const vector<guardedVal>& nr1, int nr1Start, int nr1End, vector<guardedVal>& out, int idx){
	int i=nr0Start, j=nr1Start;
	out.reserve(out.size()+ (nr0End-nr0Start) + (nr1End-nr1Start) );
	
		int inci = 1;
		int incj = 1;
		// There is an assumption that the num_ranges are monotonic. 
		// However, they could be monotonically increasing or monotonically decreasing.
		// So we need to check.
		if(nr0End -1 > nr0Start && nr0[nr0Start].value > nr0[nr0Start+1].value){
			inci = -1;
			i = nr0End -1;
		}
		
		if(nr1End -1> nr1Start && nr1[nr1Start].value > nr1[nr1Start+1].value){
			incj = -1;
			j = nr1End -1;
		}
		int added = 0;

		while( (i>=nr0Start && i < nr0End) || (j>=nr1Start && j< nr1End)){
		    bool avi = i < nr0End && i >= nr0Start;
		    bool avj = j < nr1End && j >= nr1Start;
			int curri = avi ? nr0[i].value  : -1;
			int currj = avj ? nr1[j].value  : -1;
		    if( curri == currj && avi && avj){
				Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
				int ni = i + inci;
				int nj = j + incj;
				if(added == 1 && ! ((ni>=nr0Start && ni < nr0End) || (nj>=nr1Start && nj< nr1End)) ){
					int cvar = -out[out.size()-1].guard;
					if(cvar!=-YES){
						out.push_back(guardedVal(cvar, curri, idx));
					}
					break;
				}
				int cvar3 = dir.addChoiceClause(guard, nr1[j].guard,nr0[i].guard);
				if(cvar3!= -YES){ ++added; out.push_back(guardedVal(cvar3, curri, idx));	}
				i = ni;
				j = nj;
				continue;
			}
		    if((curri < currj && avi) || !avj){
				Dout(cout<<" curri = "<<curri<<endl);
				int ni = i + inci;
				if(added == 1 && ! ((ni>=nr0Start && ni < nr0End) || (avj)) ){
					int cvar = -out[out.size()-1].guard;
					if(cvar!=-YES){
						out.push_back(guardedVal(cvar, curri, idx));
						dir.addHelperC(-cvar, -guard);
						dir.addHelperC(-cvar, nr0[i].guard);
					}
					break;
				}
				int cvar = dir.addAndClause( nr0[i].guard, -guard);
				if(cvar!=-YES){++added; out.push_back(guardedVal(cvar, curri, idx)); }
				i = ni;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				Dout(cout<<" currj = "<<currj<<endl);
				int nj = j + incj;
				if(added == 1 && ! ((avi) || (nj>=nr1Start && nj< nr1End)) ){
					int cvar = -out[out.size()-1].guard;
					if(cvar!=-YES){
						out.push_back(guardedVal(cvar, currj, idx));
						dir.addHelperC(-cvar, guard);
						dir.addHelperC(-cvar, nr1[j].guard);
					}
					break;
				}
				int cvar = dir.addAndClause( nr1[j].guard, guard );
				if(cvar!=-YES){++added; out.push_back(guardedVal(cvar, currj, idx)); }
				j = nj;
				continue;
		    }
		    Assert(false, "Should never get here");
		}		
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
		
		vector<guardedVal>& nr0 = mid0.num_ranges;
		vector<guardedVal>& nr1 = mid1.num_ranges;		
		vector<guardedVal>& out = output.num_ranges;
		out.clear();
		
		mergeTvalues(guard, nr0, 0, nr0.size(), nr1, 0, nr1.size(), out);


		Assert( out.size () > 0, "This should not happen here2");		
		output.sparsify (dir);
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
		Assert(!cval.isArray(), "ARRASS doesn't work for arrays");
		mothers[i] = *it;
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"   ");
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
    }
    if(!checkParentsChanged( node, parentSame)){ return; }
    int guard;
    if( !mval.isSparse() ){
		if(quant > 1 || quant < 0){
		    guard = -YES;
		}else{	    
		    Dout(cout<<" mval = "<<mval<<endl);
		    guard = dir.addXorClause(mval.getId (), quant==0?YES:-YES);
		}
    }else{
		guard = -YES;
		const vector<guardedVal>& nrange = mval.num_ranges;
		for(int i=0; i<nrange.size(); ++i){
			if( nrange[i].value == quant){
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
	vector<guardedVal>& tmp = node_ids[node.id].num_ranges;
	dir.getSwitchVars(ids, size, tmp);
	node_ids[node.id].sparsify (dir);
	Dout(cout<<"&ACTRL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<tmp.size()<<"   "<<&node<<endl);
	return;
}

void
NodesToSolver::visit( ARR_R_node &node){
	Tvalue index = tval_lookup(node.mother);	
	Tvalue inarr = tval_lookup(node.father);	
	if(!index.isSparse()){
		index.makeSparse(dir);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir);
	}
	
	vector<guardedVal>& idv = index.num_ranges;
	map<int, int> valToID;
	int idxincr = 1;
	int idxi = 0;
	if(idv.size() - 1 > idxi && idv[0].value > idv[1].value){
		idxincr = -1;
		idxi = idv.size()-1;
	}
	while(idxi < idv.size() && idxi >= 0 && idv[idxi].value < 0){
		//Negative indices in the array map to zero by default.
		map<int, int>::iterator it = valToID.find(0);
		if(it == valToID.end()){
			valToID[0] = idv[idxi].guard;
		}else{
			it->second = dir.addOrClause(idv[idxi].guard, it->second);
		}
		idxi += idxincr;
	}	

	vector<guardedVal>::const_iterator begdef = inarr.num_ranges.begin();
	vector<guardedVal>::const_iterator enddef = begdef;
	while(enddef != inarr.num_ranges.end() && enddef->idx <0){
		++enddef;
	}
	
	vector<guardedVal>::const_iterator inarriter = enddef;
	vector<guardedVal>::const_iterator inarrend = inarr.num_ranges.end();
	
	Tvalue defdef = tvOne; // If the array does not have a default value, we create one.
	defdef.num_ranges[0].value = -333;
	if(begdef == enddef){
		begdef = defdef.num_ranges.begin();
		enddef = defdef.num_ranges.end();
	}
	
	int cidx;
	bool moreIdx = idxi < idv.size() && idxi >= 0;
	bool moreArr=false;
	if(inarriter != inarrend){
		cidx = inarriter->idx;	
		moreArr=true;
	}
	while(moreIdx){
		if(moreIdx && moreArr && cidx == idv[idxi].value){
			while(inarriter != inarrend && inarriter->idx == cidx){
				int iatv = inarriter->value;
				map<int, int>::iterator it = valToID.find(iatv);
				if(it == valToID.end()){
					valToID[iatv] = dir.addAndClause(idv[idxi].guard, inarriter->guard);
				}else{
					int cvar = dir.addAndClause(idv[idxi].guard,inarriter->guard);
					it->second = dir.addOrClause(cvar, it->second);
				}
				++inarriter;
			}
			if(inarriter != inarrend){
				cidx = inarriter->idx;
			}else{
				cidx = -1;
				moreArr = false;
			}
			idxi += idxincr;
			if(!(idxi >= 0 && idxi < idv.size())){
				moreIdx = false;
			}
			continue;
		}
		if(!moreIdx || (moreArr && cidx < idv[idxi].value)){ 
			// array entry not contemplated by index.
			// nothing to do but to increment the array entry.
			while(inarriter != inarrend && inarriter->idx == cidx){				
				++inarriter;
			}
			if(inarriter != inarrend){
				cidx = inarriter->idx;
			}else{
				cidx = -1;
				moreArr = false;
			}
			continue;
		}
		if(!moreArr || (moreIdx && idv[idxi].value < cidx)){
			//The index refers to an entry that doesn't exist in the array.
			//Need to produce the default value.
			for(vector<guardedVal>::const_iterator it = begdef; it < enddef; ++it){
				int iatv = it->value;
				map<int, int>::iterator vit = valToID.find(iatv);
				if(vit == valToID.end()){
					valToID[iatv] = dir.addAndClause(idv[idxi].guard, it->guard);
				}else{
					int cvar = dir.addAndClause(idv[idxi].guard,it->guard);
					vit->second = dir.addOrClause(cvar, vit->second);
				}
			}
			idxi += idxincr;
			if(!(idxi >= 0 && idxi < idv.size())){
				moreIdx = false;
			}
			continue;
		}
	}


	Tvalue& nvar = node_ids[node.id];
	if(node.getOtype() == bool_node::INT){
		vector<guardedVal>& tmp = nvar.num_ranges;
		tmp.clear();
		tmp.reserve(valToID.size());
		map<int, int>::iterator it = valToID.begin();
		for(int i=0; it!=valToID.end(); ++i, ++it){
			if(it->second != -YES){
				tmp.push_back( guardedVal(it->second, it->first) );
			}
		}
		if(tmp.size() == 1){
			tmp[0].guard = YES;
		}
		nvar.sparsify (dir);
	}else{
		map<int, int>::iterator it = valToID.begin();
		for(int i=0; it!=valToID.end(); ++i, ++it){
			if(it->first == 0){
				nvar = Tvalue(-it->second);
				return;
			}
			if(it->first == 1){
				nvar = Tvalue(it->second);
				return;
			}
		}
		nvar = Tvalue(-YES);
	}
}
void NodesToSolver::visit( ARR_W_node &node){	
	Tvalue index = tval_lookup(node.mother);	
	vector<guardedVal>& idxgv = index.num_ranges;
	Tvalue inarr = tval_lookup(node.getOldArr());	
	Tvalue newval = tval_lookup(node.getNewVal());
	Tvalue& nvar = node_ids[node.id];
	vector<guardedVal>& out = nvar.num_ranges;
	out.clear();
	if(!index.isSparse()){
		index.makeSparse(dir);
	}	
	if(!newval.isSparse()){
		newval.makeSparse(dir);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir);
	}
	int lasti = 0;
	int lastidx = -1;
	int idxincr = 1;
	int idxstart = 0;
	if(idxgv.size()>1 && idxgv[0].value > idxgv[1].value){
		idxstart = idxgv.size()-1;
		idxincr = -1;
	}
	while(idxstart >= 0 && idxstart < idxgv.size() && idxgv[idxstart].value < 0){
		idxstart += idxincr;
	}
	int cindex = inarr.num_ranges[0].idx;
	int defstart = 0;
	int defend = 0;
	Tvalue tvdef = tvOne;
	tvdef.num_ranges[0].value = -333;
	if(cindex < 0){
		while(defend < inarr.num_ranges.size() && inarr.num_ranges[defend].idx < 0){
			++defend;
		}
	}
	for(int i=0; i<=inarr.getSize(); ++i){
		bool donow = false;
		if(i==inarr.getSize() || inarr.num_ranges[i].idx != cindex){
			donow = true;
		}

		if(donow){
			while(idxstart >= 0 && idxstart < idxgv.size() && idxgv[idxstart].value < cindex){
				if(defend-defstart > 0){
					mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, defstart, defend, newval.num_ranges, 0, newval.getSize(), out, idxgv[idxstart].value);
				}else{
					mergeTvalues(idxgv[idxstart].guard, tvdef.num_ranges, 0, 1, newval.num_ranges, 0, newval.getSize(), out, idxgv[idxstart].value);
				}
				idxstart += idxincr;
			}
			if(idxstart >= 0 && idxstart < idxgv.size() && idxgv[idxstart].value == cindex){
				mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, lasti, i, newval.num_ranges, 0, newval.getSize(), out, idxgv[idxstart].value);
				idxstart += idxincr;
			}else{
				for(int tt=lasti; tt<i; ++tt){
					out.push_back(inarr.num_ranges[tt]);
				}
			}
			lasti = i;
			if(i!= inarr.getSize()){ cindex = inarr.num_ranges[i].idx; }
		}
	}
	while(idxstart >= 0 && idxstart < idxgv.size()){
		if(defend-defstart > 0){
			mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, defstart, defend, newval.num_ranges, 0, newval.getSize(), out, idxgv[idxstart].value);
		}else{
			mergeTvalues(idxgv[idxstart].guard, tvdef.num_ranges, 0, 1, newval.num_ranges, 0, newval.getSize(), out, idxgv[idxstart].value);
		}
		idxstart += idxincr;
	}
	nvar.arrayify();
}

void


	NodesToSolver::visit( ARR_CREATE_node &node){
	Tvalue& nvar = node_ids[node.id];
	vector<guardedVal>& tmp = nvar.num_ranges;
	tmp.clear();
	vector<bool_node*>::iterator it = node.multi_mother.begin();	
	for(int i=0 ; it != node.multi_mother.end(); ++it, ++i){
		const Tvalue& mval = tval_lookup(*it);
		if(mval.isSparse()){
			for(int t=0; t<mval.getSize(); ++t){
				tmp.push_back(guardedVal(mval.getId(t), mval[t], i));
			}			
		}	
		if(mval.isBvect()){
			int v = mval.getId();
			if(v != -YES){
				tmp.push_back(guardedVal(v, 1, i));
			}
			if(v != YES){
				tmp.push_back(guardedVal(-v, 0, i));
			}
		}
	}	
	nvar.arrayify();
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
			
			set<const bool_node*> s;
			cout<<"digraph G{"<<endl;
			node.printSubDAG(cout, s);
			cout<<"}"<<endl;
			cout<<" slice size = "<<s.size()<<endl;
			
		}
		dir.addAssertClause (fval.getId ());
	}
	//cout<<"|"<<node.getMsg()<<"|"<<endl;
	
	if(PARAMS->debug){
		cout<<"ASSERTING "<<node.getMsg()<<endl;
		int res = dir.getMng().solve();
		Assert(res == SATSolver::SATISFIABLE, "Failed assertion!");
		lgv.clear();
		for(int i=1; i < dir.getVarCnt(); ++i){
			lgv.push_back( dir.getMng().getVarVal(i) );
		}
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


void NodesToSolver::doArrArrAcc(ARRACC_node& node, Tvalue& output){
	vector<bool_node*>::iterator it = node.multi_mother.begin();
	int N = node.multi_mother.size();
	vector<Tvalue> choices(N);
	for(int i=0; i < N; ++i, ++it){
		choices[i] = tval_lookup (*it, TVAL_SPARSE);	
		if(choices[i].isBvect()){
			choices[i].makeSparse(dir);
		}		
	}
	bool_node* mother = node.mother;
	Tvalue mval = tval_lookup (mother, TVAL_SPARSE);
	if(mval.isSparse()){
		Assert(false, "NYI aslkdn;hyp;k");
	}else{
		Assert(choices.size() == 2, "NYI aslkdn;hyp;k");
		map<pair<int, int>, int> vals;
		int gval = mval.getId();
		
		vector<guardedVal>& gvl = choices[0].num_ranges;	
		Tvalue altL;
		for(int i=0; i<gvl.size() && gvl[i].idx<0; ++i){
			altL.num_ranges.push_back(gvl[i]);
		}
		if(altL.num_ranges.size()==0){ altL = tvOne; altL.num_ranges[0].value = -333;}
		else{ altL.sparsify(dir); }

		vector<guardedVal>& gvr = choices[1].num_ranges;	
		Tvalue altR;
		for(int i=0; i<gvr.size() && gvr[i].idx<0; ++i){
			altR.num_ranges.push_back(gvr[i]);
		}
		if(altR.num_ranges.size()==0){ altR = tvOne; altR.num_ranges[0].value = -333;}
		else{ altR.sparsify(dir); }

		int idxl = 0;
		int idxr = 0;
		vector<guardedVal>& out = output.num_ranges;
		out.clear();
		int gvrs = gvr.size();
		int gvls = gvl.size();
		while(idxr< gvrs || idxl < gvls){
			if(idxr< gvrs && idxl < gvls && gvr[idxr].idx == gvl[idxl].idx){
				int idxval = gvr[idxr].idx;
				int pir = idxr;
				int pil = idxl;
				while(idxr < gvrs && gvr[idxr].idx == idxval){ ++idxr; }
				while(idxl < gvls && gvl[idxl].idx == idxval){ ++idxl; }
				mergeTvalues(gval, gvl, pil, idxl, gvr, pir, idxr, out, idxval); 
				continue;
			}
			if(idxr>= gvrs ||  (idxl < gvls && gvr[idxr].idx > gvl[idxl].idx) ){
				int idxval = gvl[idxl].idx;
				int pil = idxl;
				while(idxl < gvls && gvl[idxl].idx == idxval){ ++idxl; }
				mergeTvalues(gval, gvl, pil, idxl, altR.num_ranges, 0, altR.num_ranges.size(), out, idxval); 
				continue;
			}
			if(idxl >= gvls || (idxr< gvrs && gvr[idxr].idx < gvl[idxl].idx) ){
				int idxval = gvr[idxr].idx;
				int pir = idxr;
				while(idxr < gvrs && gvr[idxr].idx == idxval){ ++idxr; }
				mergeTvalues(gval, altL.num_ranges, 0, altL.num_ranges.size(), gvr, pir, idxr, out, idxval); 
				continue;
			}
		}
		output.arrayify();
	}

}

void NodesToSolver::addToVals(map<pair<int, int>, int>& vals, vector<guardedVal>::iterator it, int idx, int gval){
	int vval = dir.addAndClause(it->guard, gval);
	if(vval != -YES){
		map<pair<int, int>, int>::iterator mit = vals.find( make_pair(idx, it->value) );
		if(mit == vals.end()){
			vals[make_pair(idx, it->value)] = vval;
		}else{
			mit->second = dir.addOrClause(mit->second, vval );
		}
	}
}


void NodesToSolver::doNonBoolArrAcc(ARRACC_node& node, Tvalue& output){
	Dout( cout<<" non boolean array "<<endl );
	vector<bool_node*>::iterator it = node.multi_mother.begin();
	
	int N = node.multi_mother.size();
	vector<Tvalue> choices(N);
	for(int i=0; i < N; ++i, ++it){
		choices[i] = tval_lookup (*it, TVAL_SPARSE);
		if(choices[i].isBvect()){
			choices[i].makeSparse (dir);
		}
	}
	bool_node* mother = node.mother;
	Tvalue mval = tval_lookup (mother, TVAL_SPARSE);
	if(mval.isSparse()){
		//mval.makeSparse (dir);
		map<int, vector<int> > newVals;
		int vsize = N;
		vector<guardedVal>& nrange = mval.num_ranges;

		for(int i=0; i<nrange.size(); ++i){
			if( nrange[i].value < vsize && nrange[i].value >= 0){
				Tvalue& curr = choices[nrange[i].value];
				vector<guardedVal>& cvalues = curr.num_ranges;
				Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<"  cvsize="<<cvalues.size()<<endl );
				for(int j=0; j<cvalues.size(); ++j){
					int cvar = dir.addAndClause( mval.getId (i), curr.getId (j) );
					newVals[ cvalues[j].value ].push_back(cvar);
					Dout( cout<<" cvalues["<<j<<"] = "<<cvalues[j]<<endl );
				}
			}else{
				Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<" OUT OF RANGE"<<endl );
				newVals[ 0 ].push_back( mval.getId (i));			
			}
		}

		vector<guardedVal>& result = output.num_ranges;
		result.clear();
		Dout(cout<<" newVals.size() == " << newVals.size()<<endl );
		{
			if(newVals.size() == 0){			
				output = Tvalue( -YES );			
				Dout(cout<<" after sparsification "<<output<<endl);
				return;
			}		
			for(map<int, vector<int> >::iterator it = newVals.begin(); it != newVals.end(); ++it){
				vector<int>& vars = it->second;
				int orTerms = 0;
				while( (vars.size() + 1) >= scratchpad.size() ){ scratchpad.resize(scratchpad.size()*2); }
				for(int i=0; i<vars.size(); ++i){
					++orTerms;
					scratchpad[orTerms] = vars[i];
				}
				scratchpad[0] = 0;
				int cvar = dir.addBigOrClause( &scratchpad[0], orTerms);
				result.push_back(guardedVal(cvar, it->first));
			}
			output.sparsify (dir);
		}
	}else{
		//mval is not sparse; it's a single bit.
		if(choices.size() == 0){
			output = Tvalue( -YES );			
			Dout(cout<<" after sparsification "<<output<<endl);
			return;
		}
		if(choices.size() == 1){
			choices.push_back(Tvalue( -YES ));
		}
		int fl;
		mergeTvalues(mval.getId(), choices[0], choices[1], output, fl);
	}
}


bool NodesToSolver::checkParentsChanged(bool_node& node, bool more){
	if(( node.father== NULL || !node.father->flag ) &&
			( node.mother== NULL || !node.mother->flag )&&
			more
			){ 
				node.flag =false; return false || dir.ignoreOld(); 
	}else{ 
		node.flag = true; return true;
	}
}

/*
void NodesToSolver::process(BooleanDAG& bdag){
	int i=0;
	tmpdag = &bdag;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		(*node_it)->accept(*this);
		cout<<(*node_it)->lprint()<<" -----> "<< node_ids[(*node_it)->id].getSize() <<endl;
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    	}
	}
}
*/