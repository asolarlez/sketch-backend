#include "NodesToSolver.h"
#include <algorithm>
#include <functional>
#include "timerclass.h"
#include "Tvalue.h"
#include "CommandLineArgs.h"
#include "PrintInteresting.h"

int TOTBUFFERS = 0;


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

void advanceToEndIdx(int& iend, int cidx, const gvvec&tv){
	while(iend < tv.size() && tv[iend].idx == cidx){
		++iend;
	}
}



bool NodesToSolver::createConstraints(BooleanDAG& dag, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids){
	//timerclass timer("defineProblem");
		//timer.start();
	bool stoppedEarly;
		int YES = dir.newYES();
		//getProblem()->lprint(cout);
		NodesToSolver nts(dir, "PROBLEM", node_values, node_ids);			
		try{
			stoppedEarly =false;
			nts.process(dag);	
			if(nts.stoppedPrematurely()){
				dir.lastErrMsg = nts.errorMsg;
				stoppedEarly = true;
			}

			/*
			BooleanDAG& bd = *getProblem();
			for(int i=0; i<node_ids.size(); ++i){
				cout<< bd[i]->lprint() <<"="<<node_ids[i]<<endl;
			}
			*/
		}catch(BasicError& e){			
			throw e;
		}

		//timer.stop();
		//if(PARAMS->verbosity > 2){ timer.print(); }
	
	return stoppedEarly;
}




void NodesToSolver::computeMaxOrMin(const gvvec& mv, const gvvec& fv, gvvec& out, bool doMax){	
	int mstart=0; int mend=mv.size();
	int fstart=0; int fend=fv.size();

	int msofar = YES;
	int fsofar = YES;

	int i=mstart, j=fstart;
	int inci = 1;
	int incj = 1;
	// There is an assumption that the num_ranges are monotonic. 
	// However, they could be monotonically increasing or monotonically decreasing.
	// So we need to check. Moreover, if doMax is true, we are computing the max, so we want to go
	// from high value to low value, but if it is false, we want to go from low-value to high-value.

	if(doMax){		
		if(mend -1 > mstart && mv[mstart].value < mv[mstart+1].value){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && fv[fstart].value < fv[fstart+1].value){
			incj = -1;
			j = fend -1;
		}
	}else{
		if(mend -1 > mstart && mv[mstart].value > mv[mstart+1].value){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && fv[fstart].value > fv[fstart+1].value){
			incj = -1;
			j = fend -1;
		}
	}
	
	int mcnt = 0; int msz = mend - mstart;
	int fcnt = 0; int fsz = fend - fstart;
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;
			int curri = avi ? mv[i].value  : -1;
			int currj = avj ? fv[j].value  : -1;
			bool ltmin = curri < currj;
			bool ltmax = curri > currj;
			if((( (ltmin && !doMax) || (ltmax && doMax)  ) && avi) || !avj){
				int tmpv = dir.addAndClause(mv[i].guard, fsofar);
				if(tmpv != -YES){
					out.push_back(guardedVal(tmpv, curri));
				}				
				++mcnt;
				if(mcnt == msz){
					msofar = -YES;
				}else{
					if(mcnt==msz-1){
						msofar = mv[i+inci].guard;
					}else{
						msofar = dir.addAndClause(msofar, -mv[i].guard);
					}
				}
				i = i + inci;
				continue;
		    }
		    if( (( (ltmax && !doMax) || (ltmin && doMax)  ) && avj) || !avi ){
				int tmpv = dir.addAndClause(fv[j].guard, msofar);
				if(tmpv != -YES){
					out.push_back(guardedVal(tmpv, currj));
				}
				++fcnt;
				if(fcnt == fsz){
					fsofar = -YES;
				}else{
					if(fcnt == fsz-1){
						fsofar = fv[j+incj].guard;
					}else{
						fsofar = dir.addAndClause(fsofar, -fv[j].guard);
					}
				}
				j = j + incj;
				continue;
		    }
			if( curri == currj && avi && avj){
				int tmpvm = dir.addAndClause(mv[i].guard, fsofar);
				int tmpvf = dir.addAndClause(fv[j].guard, msofar);
				int tmpor = dir.addOrClause(tmpvm, tmpvf);
				if(tmpor != -YES){
					out.push_back(guardedVal(tmpor, currj));
				}
				++mcnt;
				if(mcnt == msz){
					msofar = -YES;
				}else{
					if(mcnt==msz-1){
						msofar = mv[i+inci].guard;
					}else{
						msofar = dir.addAndClause(msofar, -mv[i].guard);
					}
				}
				++fcnt;
				if(fcnt == fsz){
					fsofar = -YES;
				}else{
					if(fcnt == fsz-1){
						fsofar = fv[j+incj].guard;
					}else{
						fsofar = dir.addAndClause(fsofar, -fv[j].guard);
					}
				}
				i = i + inci;
				j = j + incj;
				continue;
			}
	}
}


int NodesToSolver::compareRange(const gvvec& mv, int mstart, int mend, const gvvec& fv, int fstart, int fend){
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
		    // TODO xzl: why -1? does this value really matter?
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
				// when curri=0 (default value) but j does not have, should not put guard to noeq, instead, it should be AND with the negation of all non default guard of j and put into orTerms
				noeq.push_back(mv[i].guard);
				i = i + inci;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				// when currj=0 (default value) but i does not, should not put guard to noeq, instead, it should be AND with the negation of all non default guard of i and put into orTerms
				noeq.push_back(fv[j].guard);
				j = j + incj;
				continue;
		    }
	}
	
	if(orTerms==0){
		// if one has only default value (0) and the other has no term, should be YES
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
NodesToSolver::compareArrays (bool_node& node,  const Tvalue& tmval,  const Tvalue& tfval){
//	cout << "compareArrays: " << node.lprint() << endl << "tmval= " << tmval << endl << "tfval= " << tfval << endl;
	const gvvec& mv = tmval.num_ranges;
	const gvvec& fv = tfval.num_ranges;

	int midx = mv[0].idx;
	int fidx = fv[0].idx;
	int mistart = 0;
	int fistart = 0;
	int miend = 0;
	int fiend = 0;
	advanceToEndIdx(miend, midx, mv);
	advanceToEndIdx(fiend, fidx, fv);
	Tvalue mdef;
	// in Tvalue, idx==-1 means the default value for array
	// mdef is the default value for mother
	if(midx==-1){
		for(int i=0; i<miend; ++i){
			mdef.num_ranges.push_back(guardedVal(mv[i].guard, mv[i].value));
		}
	}else{
		// when there is no default value, the default is UNINITIALIZED, guarded by YES
		mdef = tvOne;
		
		mdef.num_ranges[0].value = UNINITIALIZED;
		//mdef.num_ranges[0].value = 0;
	}
	Tvalue fdef;
	if(fidx==-1){
		for(int i=0; i<fiend; ++i){
			fdef.num_ranges.push_back(guardedVal(fv[i].guard, fv[i].value));
		}
	}else{
		fdef = tvOne;
		
		fdef.num_ranges[0].value = UNINITIALIZED;
		//fdef.num_ranges[0].value = 0;
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
	//cout << "compareArrays: cvar=" << cvar << endl;
}


void NodesToSolver::processLT (LT_node& node){
	bool_node *mother = node.mother;
    Tvalue mval = tval_lookup (mother, TVAL_SPARSE);    

    bool_node *father = node.father;
    Tvalue fval = tval_lookup (father, TVAL_SPARSE);
	if(mval.isArray() || fval.isArray()){
		Assert(false, "Can't do < on arrays");
	}
	mval.makeSparse (dir);
    fval.makeSparse (dir);
    int cvar = -YES;

	const gvvec& mv = mval.num_ranges;
	const gvvec& fv = fval.num_ranges;

	int mstart=0; int mend=mv.size();
	int fstart=0; int fend=fv.size();

	int msofar = YES;
	int fsofar = YES;

	int i=mstart, j=fstart;
	int inci = 1;
	int incj = 1;
	// There is an assumption that the num_ranges are monotonic. 
	// However, they could be monotonically increasing or monotonically decreasing.
	// So we need to check. 
	{
		if(mend -1 > mstart && mv[mstart].value > mv[mstart+1].value){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && fv[fstart].value > fv[fstart+1].value){
			incj = -1;
			j = fend -1;
		}
	}

	
	int mcnt = 0; int msz = mend - mstart;
	int fcnt = 0; int fsz = fend - fstart;

	//If all the mothers are less than all the fathers, then just return yes.
	// mv[i] has the smallest mother.
	// mv[i+(msz-1)*inci] has the biggest mother.
	// fv[j] has the smallest father.
	// fv[j+(fsz-1)*incj] has the biggest father.
	if(mv[i+(msz-1)*inci].value < fv[j].value){
		// cout<<"!!! Saved with YES, "<<inci<<endl;
		node_ids[node.id] = YES;
		return;
	}
	//If all the mothers are >= all the fathers, just return false.
	if(mv[i].value >= fv[j+(fsz-1)*incj].value){
		// cout<<"!!! Saved with NO, "<<incj<<endl;
		node_ids[node.id] = -YES;
		return;
	}

	vector<char> mc(mval.getSize(), 'n');
	vector<char> fc(fval.getSize(), 'n');
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;
			int curri = avi ? mv[i].value  : -1;
			int currj = avj ? fv[j].value  : -1;
			if(!avi){
				dir.addHelperC(cvar, -fv[j].guard);
				j = j + incj;
				continue;
			}
			if(!avj){
				dir.addHelperC(-cvar, -mv[i].guard);
				i = i + inci;
				continue;
			}
			if((( curri < currj  ) && avi && avj)){
				int tmpv = dir.addAndClause(mv[i].guard, fsofar);
				cvar = dir.addOrClause(cvar, tmpv);												
				i = i + inci;
				continue;
		    }
		    if( (( curri > currj  ) && avj && avi)){				
				++fcnt;
				if(fcnt == fsz){
					fsofar = -YES;
				}else{
					if(fcnt == fsz-1){
						fsofar = fv[j+incj].guard;
					}else{
						fsofar = dir.addAndClause(fsofar, -fv[j].guard);
					}
				}
				j = j + incj;
				continue;
		    }
			if( curri == currj && avi && avj){
				++fcnt;
				if(fcnt == fsz){
					fsofar = -YES;
				}else{
					if(fcnt == fsz-1){
						fsofar = fv[j+incj].guard;
					}else{
						fsofar = dir.addAndClause(fsofar, -fv[j].guard);
					}
				}
				j = j + incj;

				int tmpvm = dir.addAndClause(mv[i].guard, fsofar);
				cvar = dir.addOrClause(cvar, tmpvm);					
				
				i = i + inci;
				
				continue;
			}
	}
	node_ids[node.id] = cvar;

}

template<typename COMP> void
NodesToSolver::processComparissons (bool_node& node, bool revFval)
{
    //cout << "comparing " << node.lprint() << endl; 
    bool_node *mother = node.mother;
    Tvalue mval = tval_lookup (mother, TVAL_SPARSE);    

    bool_node *father = node.father;
    Tvalue fval = tval_lookup (father, TVAL_SPARSE);
    //cout << "comparing " << node.lprint() << " mval=" << mval << " fval=" << fval << endl; 
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

	bool isEq = node.type == bool_node::EQ;

    for(int i=0; i<mval.getSize (); ++i){
		for(int j=flow; j!=fhigh; j = j+finc){
		    Dout(cout<<"COMPARING "<<mval[i]<<", "<<fval[j]<<endl);
		    if(comp(mval[i], fval[j])){
				mc[i] = 'y';
				fc[j] = 'y';
				++orTerms;
				if(isEq){					
					if(2*orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms*2-2] = mval.getId(i);
					scratchpad[orTerms*2-1] = fval.getId(j);
				}else{
					cvar = dir.addAndClause(mval.getId (i), fval.getId (j));									
					if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms] = cvar;
				}
			}
		}
    }
    if( orTerms < 2 ){
		if(isEq){
			cvar = dir.addExPairConstraint(&scratchpad[0], orTerms);
		}
		for(int i=0; i<mc.size(); ++i){ if(mc[i] =='n'){ dir.addHelperC(-cvar, -mval.getId (i)); }  }
		for(int i=0; i<fc.size(); ++i){ if(fc[i] =='n'){ dir.addHelperC(-cvar, -fval.getId (i)); }  }
		node_ids[node.id] = cvar;
    }else{
		if(orTerms == mval.getSize() * fval.getSize()){
			node_ids[node.id] = YES;
		}else{
			int result;
			if(isEq){
				result = dir.addExPairConstraint(&scratchpad[0], orTerms);
			}else{
				scratchpad[0] = 0;
				result = dir.addBigOrClause( &scratchpad[0], orTerms);				
			}
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
	// TODO xzl: temporarily disable sparse warning
	if( false && mval.getSize() > 200 ){ 
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
	map<int, vector<int> > qnumbers;
	Tvalue& oval = node_ids[node.id];
	gvvec& tmp = oval.num_ranges;
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
	if(PARAMS->randBnd > 0 && mval.getSize() * fval.getSize() > 3*PARAMS->randBnd){
		shortcut = true;
	}else{
		shortcut = false;
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

			if(isSum){
				qnumbers[quant].push_back(mval.getId(i));
				qnumbers[quant].push_back(fval.getId(j));				
			}else{
				map<int, int>::iterator it = numbers.find(quant);
				//						btimer.stop();
				if( it != numbers.end()){
						int cvar = dir.addAndClause(mval.getId (i),fval.getId (j));
						int cvar2 = dir.addOrClause(cvar, it->second);
						it->second = cvar2;				
				}else{
						int cvar = dir.addAndClause(mval.getId (i), fval.getId (j));				
						if(numbers.size() >= INTEGERBOUND){
							PrintSource ps(node_ids);
							ps.process(*tmpdag, node.id);
							Assert(false, "AN INTEGER GOT REALLY BIG, AND IS NOW BEYOND THE SCOPE OF THE SOLVER" << " current randBnd =" << PARAMS->randBnd << " try to set a smaller one with --bndwrand in backend, or --bnd-int-range in frontend");
						}
						numbers[quant] = cvar;
						++vals;								
				}
			}
		//cout<<" ENDLOOP "<<endl;
	    }
	}

	if(isSum){
		for(map<int, vector<int> >::iterator it = qnumbers.begin(); it != qnumbers.end(); ++it){
			int id = dir.addExPairConstraint(&(it->second[0]), it->second.size()/2);
			numbers[it->first] = id;
			++vals;
		}
	}

	if(shortcut){
		int refuse = -YES;
		for(map<int, int >::iterator it = numbers.begin(); it != numbers.end(); ){
			int tt = it->first;
			if(PARAMS->randBnd > 0 && abs(tt) > PARAMS->randBnd){ 				
				refuse = dir.addOrClause(refuse, it->second);
				numbers.erase(it++);
			}else{
				++it;
			}
		}		
		if(refuse != -YES){
			if(!dir.getMng().isNegated()){
				int id = rand() % numbers.size();
				map<int, int>::iterator mit = numbers.begin();
				for(int i=0; i<id; ++i){ ++mit; }				
				mit->second = dir.addOrClause(mit->second, refuse);			
			}else{
				Assert(numbers.count(PARAMS->randBnd*2)==0, "Lnliurya;");
				numbers[PARAMS->randBnd*2] = refuse;		
			}			
		}
		vals = numbers.size();
	}

	Dout(cout<<"tmp size = "<<numbers.size ()<<endl);
	Assert( vals > 0 && vals == numbers.size(), "NotesToSolver::processArith: This should not happen here "<<vals<<"  "<<numbers.size());
	
	tmp.resize(vals);
	map<int, int>::iterator it = numbers.begin();

	int i;
	for(i=0; it != numbers.end(); ++it){
		if(it->second == YES){
			tmp.resize(1);
			tmp[0] = guardedVal(it->second, it->first);	
			for(map<int, int>::iterator sit = numbers.begin(); sit != numbers.end(); ++sit){
				if(sit->second != it->second){
					dir.addAssertClause(-sit->second);
				}
			}
			i=1;
			break;
		}
		if(it->second == -YES){
			continue;
		}
		tmp[i] = guardedVal(it->second, it->first);		
		++i;
	}
	tmp.resize(i);
	if(tmp.size() == 1){
		if(tmp[0].guard != YES){
			dir.addAssertClause(tmp[0].guard);
			tmp[0].guard = YES;
		}
	}
	oval.sparsify (dir);
	if(oval.getSize()==0){
		stopAddingClauses = true;
		//This means that the problem has become unsat.
	}else{
		dir.addHelperC(oval);
	}
	Dout( cout<<" := "<<oval<<endl );	    
}



void NodesToSolver::visit( AND_node& node ){
	const Tvalue& fval = tval_lookup(node.father);
	const Tvalue& mval = tval_lookup(node.mother);
	
	Tvalue oldnvar(node_ids[node.id]);
	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addAndClause(fval.getId (), mval.getId ());
	node.flag = oldnvar != nvar;
	Dout(cout<<"AND "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}


void NodesToSolver::visit( OR_node& node ){
	
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
    //cout << "NodesToSolver SRC " << node.lprint() << " nbits=" << node.get_nbits() << endl;

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
      
      if (node.isTuple) {
        Assert(false, "Not possible");
      } else {
      
		int arrSz = node.getArrSz();
		node_ids[node.id] = dir.getArr (node.get_name(), 0);
		//This could be removed. It's ok to setSize when get_nbits==1.		
		if (node.get_nbits () > 1 || arrSz >=0) {
		    Dout (cout << "setting input nodes " << node.get_name() << endl);
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

	Dout(cout << "REGISTERING " << node.get_name() << "  " << node_ids[node.id]
	      << "  " << &node << endl);
    }
    }
    // for input arrays, add the default value (out of bound) to be 0, if not present already
    node_ids[node.id].addArrDefault(YES, 0);
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
	node_ids[node.id] = nvar;    

    Dout (cout << "PT " << node.get_name() << " " << nvar << " " << &node << endl);
}


void
NodesToSolver::visit (NEG_node &node)
{
    Assert (node.mother && ! node.father, "NEG node must have exactly one predecessor");	


    const Tvalue &mval = tval_lookup (node.mother);
    Tvalue nvar = mval.toComplement(dir);
    node_ids[node.id] = nvar;

    Dout (cout << "NEG " << node.get_name() << " " << nvar << " " << &node << endl);
}



void
NodesToSolver::visit (CTRL_node &node)
{
    //cout << "NodesToSolver CTRL " << node.lprint() << endl;
    const int nbits = node.get_nbits();
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
		Tvalue & nvar = node_ids[node.id];
		if(node.get_Angelic()){
      if (node.isTuple) {
        Assert(false, "Not possible");
        
      } else {
        // BUGFIX: Issue #5
        // when node is an array, need to create array
        const int arrSz = node.getArrSz();
        //cout << "Angelic " << node.lprint() << " arrSz=" << arrSz << " nbits=" << nbits << endl;
        if(arrSz<0){
          nvar = dir.newAnonymousVar(nbits);
          nvar.setSize(nbits);
          if (nbits > 1) {
            nvar.makeSparse(dir);
          }
        }else{
          const int totbits = nbits*arrSz;
          nvar = dir.newAnonymousVar(totbits);
          nvar.setSize(totbits);
          nvar.makeArray(dir, nbits, arrSz);
        }
      }
		}else{
			nvar = dir.getControl(&node);			
		}		
		Dout(cout<<"CONTROL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
		return;
    }
}



void NodesToSolver::visit( PLUS_node& node ){
	Dout( cout<<" PLUS: "<<node.get_name()<<endl );

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
	Assert(false, "NYI; ;jlqkweyyyyy");
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



void NodesToSolver::muxTValues(ARRACC_node* pnode, const Tvalue& mval, vector<Tvalue>& choices, Tvalue& out, bool isBoolean, bool isArray){

	
	if(isArray){
		doArrArrAcc(mval, choices, out);
		return;
	}
	if(pnode != NULL){
		ARRACC_node& node = *pnode;
		if(node.mother->type == bool_node::LT){
			if(node.mother->mother == node.multi_mother[0] && 
				node.mother->father == node.multi_mother[1]){
					if(choices[0].isBvect()){choices[0].makeSparse(dir);}
					if(choices[1].isBvect()){choices[1].makeSparse(dir);}
					computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, true);
					out.sparsify(dir);
					return;
			}
			if(node.mother->mother == node.multi_mother[1] && 
				node.mother->father == node.multi_mother[0]){
					if(choices[0].isBvect()){choices[0].makeSparse(dir);}
					if(choices[1].isBvect()){choices[1].makeSparse(dir);}
					computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, false);
					out.sparsify(dir);
					return;
			}
		}
	}
	if(!isBoolean){
//		nonbooltimer.restart();
		doNonBoolArrAcc(mval, choices, out);
//		nonbooltimer.stop().print();
//		aracctimer.stop().print();
		Dout(cout<<node.get_name()<<"  "<<out<<endl);
		return;
	}
	Dout(cout<<" is boolean"<<endl);
	
	Dout(cout<<" mother = "<<mval<<"   "<<endl);
	
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
		out = cvar;
		Dout(cout<<"ARRACC "<<node.get_name()<<"  "<<out<<"   "<<&node<<endl);
//		aracctimer.stop().print();
		return;
	}
//	elooptimer.restart();
	const gvvec& nrange = mval.num_ranges;
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
		out = cvar;
	}else{
		scratchpad[0] = 0;
		int result = dir.addBigOrClause( &scratchpad[0], orTerms);
		out = result;		
	}
	Dout(cout<<"ARRACC "<<node.get_name()<<"  "<<out<<"   "<<&node<<endl);
}





//timerclass aracctimer("ARRACC TIMER");
//timerclass flooptimer("FIRST LOOP TIMER");
//timerclass nonbooltimer("NON BOOL TIMER");
//timerclass elooptimer("FINAL LOOP TIMER");


void NodesToSolver::visit( ARRACC_node& node ){


	Dout(cout<<" ARRACC "<<endl);
	//cout<<"NodesToSolver.visit ARRACC "<< node.lprint() << endl;
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
				
		node_ids[node.id] = tval_lookup(choice);				
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

	muxTValues(&node, omv, choices, node_ids[node.id], isBoolean, isArray);

//	elooptimer.stop().print();
//	aracctimer.stop().print();
	return;
}

void NodesToSolver::visit( DIV_node& node ){
    Dout( cout<<" DIV "<<endl );
    
    processArith<divides<int> >(node);
    return;
}
void NodesToSolver::visit( MOD_node& node ){
    Dout( cout<<" MOD "<<endl );
    
    processArith<modulus<int> >(node);
    return;
}


void
NodesToSolver::visit (EQ_node &node)
{
    Dout (cout << " EQ " << endl);
    
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
#ifdef HAVE_BVECTARITH
	intBvectLt (node);
#else
	processLT(node);
	//processComparissons<less<int> > (node, node.mother->type == bool_node::CONST);
#endif /* HAVE_BVECTARITH */
}













void NodesToSolver::mergeTvalues(int guard, const gvvec& nr0, int nr0Start, int nr0End, const gvvec& nr1, int nr1Start, int nr1End, gvvec& out, int idx){
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
		    // if( flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES asdf"<<endl; }
		    output = mid1;
		    Dout( cout<<"var "<< mid1 <<endl);
		    return;
		}
		if(guard == -YES){
		    flag = output != mid0;
		    // if( flag == false ){ cout << "HURRAY, I JUST SAVED A BUNCH OF CLAUSES paoiu"<<endl; }
		    output = mid0;
		    Dout( cout<<"var "<< mid0 <<endl);
		    return;
		}
		
		gvvec& nr0 = mid0.num_ranges;
		gvvec& nr1 = mid1.num_ranges;		
		gvvec& out = output.num_ranges;
		out.clear();
		
		mergeTvalues(guard, nr0, 0, nr0.size(), nr1, 0, nr1.size(), out);


		Assert( out.size () > 0, "NotesToSolver::mergeTValues: This should not happen here2");		
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
		const gvvec& nrange = mval.num_ranges;
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
	gvvec& tmp = node_ids[node.id].num_ranges;
	dir.getSwitchVars(ids, size, tmp);
	node_ids[node.id].sparsify (dir);
	Dout(cout<<"&ACTRL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<tmp.size()<<"   "<<&node<<endl);
	return;
}


void NodesToSolver::arrRTvalue(bool isBool, const Tvalue& index, const Tvalue& inarr, Tvalue& nvar){
	
	const gvvec& idv = index.num_ranges;
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

	gvvec::const_iterator begdef = inarr.num_ranges.begin();
	gvvec::const_iterator enddef = begdef;
	while(enddef != inarr.num_ranges.end() && enddef->idx <0){
		++enddef;
	}
	
	gvvec::const_iterator inarriter = enddef;
	gvvec::const_iterator inarrend = inarr.num_ranges.end();
	
	Tvalue defdef; // If the array does not have a default value, we create one.
	defdef.makeIntVal(YES, UNINITIALIZED);
	
	//defdef.num_ranges[0].value = 0;
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
				//cout << "found index, val: " << cidx << "," << idv[idxi].guard << " " << iatv << "," << inarriter->guard << endl;
				map<int, int>::iterator it = valToID.find(iatv);
				if(it == valToID.end()){
					valToID[iatv] = dir.addAndClause(idv[idxi].guard, inarriter->guard);
				}else{
					int cvar = dir.addAndClause(idv[idxi].guard,inarriter->guard);
					it->second = dir.addOrClause(cvar, it->second);
				}
				//cout << "update valToID[" << iatv << "]=" << valToID[iatv] << endl;
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
			for(gvvec::const_iterator it = begdef; it < enddef; ++it){
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

	
	// TODO xzl: bug This is wrong.
	// especially when type is BIT
	// need to consider the case when index falls out of bound
	// valToID is NOT sufficient. need to make a special case.

	if(!isBool){
		gvvec& tmp = nvar.num_ranges;
		tmp.clear();
		tmp.reserve(valToID.size());
		map<int, int>::iterator it = valToID.begin();
		for(int i=0; it!=valToID.end(); ++i, ++it){
			//cout << "valToID[" << it->first << "]=" << it->second << endl;
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
//	cout << "ARR_R(inarr,index,nvar)1: " << node.lprint() << endl << inarr << endl << index << endl << nvar << endl;
				return;
			}
			if(it->first == 1){
				nvar = Tvalue(it->second);
//	cout << "ARR_R(inarr,index,nvar)2: " << node.lprint() << endl << inarr << endl << index << endl << nvar << endl;
				return;
			}
		}
		nvar = Tvalue(-YES);
	}

}

void
NodesToSolver::visit( ARR_R_node &node){
	//cout << "NodesToSolver ARR_R " << node.lprint() << endl;
	Tvalue index = tval_lookup(node.mother);
	Tvalue inarr = tval_lookup(node.father);
	if(!index.isSparse()){
		index.makeSparse(dir);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir);
	}
	arrRTvalue(node.getOtype() == OutType::BOOL, index, inarr, node_ids[node.id]);
//	cout << "ARR_R(inarr,index,nvar)3: " << node.lprint() << endl << inarr << endl << index << endl << nvar << endl;
}


void NodesToSolver::arrWTvalue(const Tvalue& index, const Tvalue& inarr, const Tvalue& newval, Tvalue& nvar){

	if(index.getSize()==1){ //Constant index.
		const guardedVal& idxgv = index.num_ranges[0];
		int lastIndex = inarr.num_ranges.size()-1;
		const guardedVal& last = inarr.num_ranges[lastIndex];
		//This means we are appending in a currently empty entry. We can just push back newval 
		//at the end of inarr.
		if(last.idx <= idxgv.value-1){
			nvar.num_ranges = inarr.num_ranges;
			gvvec::const_iterator end = newval.num_ranges.end();
			for(gvvec::const_iterator it = newval.num_ranges.begin(); it < end; ++it){
				guardedVal tmp = *it;
				tmp.idx = idxgv.value;
				nvar.num_ranges.push_back(tmp);
			}
			nvar.arrayify();
			return;
		}

	}



	const gvvec& idxgv = index.num_ranges;
	gvvec& out = nvar.num_ranges;
	out.clear();
	out.reserve(inarr.getSize() + newval.getSize());	
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
	Tvalue tvdef;
	tvdef.makeIntVal(YES, UNINITIALIZED);	
	//tvdef.num_ranges[0].value = 0;
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
				gvvec::const_iterator tit = inarr.num_ranges.begin()+lasti;
				for(int tt=lasti; tt<i; ++tt){					
					out.push_back(*tit);
					++tit;
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

void NodesToSolver::visit( ARR_W_node &node){	
	Tvalue index = tval_lookup(node.mother);		
	Tvalue inarr = tval_lookup(node.getOldArr());	
	Tvalue newval = tval_lookup(node.getNewVal());
	Tvalue& nvar = node_ids[node.id];
	if(!index.isSparse()){
		index.makeSparse(dir);
	}	
	if(!newval.isSparse()){
		newval.makeSparse(dir);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir);
	}
	arrWTvalue(index, inarr, newval, nvar);
//	cout << "ARR_W(inarr,index,newval,nvar): " << node.lprint() << endl << inarr << endl << index << endl << newval << endl << nvar << endl;
}



void NodesToSolver::visit( ARR_CREATE_node &node){
	Tvalue& nvar = node_ids[node.id];
	gvvec& tmp = nvar.num_ranges;
	tmp.clear();
	vector<bool_node*>::iterator it = node.multi_mother.begin();	

	tmp.push_back(guardedVal(YES, node.dfltval, -1)); // default value lives in location -1;

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

void NodesToSolver::createCond(Tvalue mval , Tvalue fval, Tvalue& out) {
  mval.makeSparse(dir);
  fval.makeSparse(dir);
  int cvar = -YES;
  equal_to<int> comp;
  int orTerms = 0;
	vector<char> mc(mval.getSize(), 'n');
	vector<char> fc(fval.getSize(), 'n');
	int flow = 0;
	int fhigh = fval.getSize ();
	int finc = 1;
  
  for(int i=0; i<mval.getSize (); ++i){
		for(int j=flow; j!=fhigh; j = j+finc){
      if(comp(mval[i], fval[j])){
				mc[i] = 'y';
				fc[j] = 'y';
				++orTerms;
				
        if(2*orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
        scratchpad[orTerms*2-2] = mval.getId(i);
        scratchpad[orTerms*2-1] = fval.getId(j);
				
			}
		}
  }
  if( orTerms < 2 ){
		cvar = dir.addExPairConstraint(&scratchpad[0], orTerms);
		for(int i=0; i<mc.size(); ++i){ if(mc[i] =='n'){ dir.addHelperC(-cvar, -mval.getId (i)); }  }
		for(int i=0; i<fc.size(); ++i){ if(fc[i] =='n'){ dir.addHelperC(-cvar, -fval.getId (i)); }  }
		out = cvar;
  }else{
		if(orTerms == mval.getSize() * fval.getSize()){
			out = YES;
		}else{
			int result;
			result = dir.addExPairConstraint(&scratchpad[0], orTerms);
			
			for(int i=0; i<mc.size(); ++i){ if(mc[i] =='n'){ dir.addHelperC(-result, -mval.getId (i)); }  }
			for(int i=0; i<fc.size(); ++i){ if(fc[i] =='n'){ dir.addHelperC(-result, -fval.getId (i)); }  }
			out = result;
		}
  }
  return;
}

void NodesToSolver::visit( TUPLE_R_node &node){
    //cout << "NodesToSolver TUPLE_R " << node.lprint() << endl;
    int index = node.idx;
    const Tvalue tid = tval_lookup(node.mother);
    
    int length = tid.num_ranges.size();
	Tvalue zero = tvYES;
	zero.bitAdjust(false);
    vector<Tvalue> choices(tpl_store.size(), zero);
    
    bool isBoolean = true;
    bool isArray = false;
  
  if (node.getOtype()->isArr) {
    Tvalue prev = zero;
    bool first = true;
    for (int i=0; i < length; ++i) {
      int tsidx = tid.num_ranges[i].value;
      if (tsidx > 0) {
        vector<Tvalue>& tuple = *tpl_store[tsidx];
        if (index < tuple.size()) {
          const Tvalue& cval = tuple[index];
          if( cval.isSparse() ){
            isBoolean = false;
          }
          if(cval.isArray()){
            isArray=true;
          }
          if (first) {
            first = false;
            prev = cval;
            continue;
          }
          vector<Tvalue> binChoice(2, zero);
          binChoice[0] = prev;
          binChoice[1] = cval;
          
          Tvalue rhs = tvOne;
          rhs.intAdjust(tsidx);
          Tvalue cond = tvYES;
          createCond(tid, rhs, cond);
          Tvalue out = tvOne;
          muxTValues(NULL, cond, binChoice, out, isBoolean, isArray);
          prev = out;

        }
      }
    }
    node_ids[node.id] = prev;
    return;
  }
    for (int i=0; i < length; ++i) {
      
        int tsidx = tid.num_ranges[i].value;
        if (tsidx > 0) {
            vector<Tvalue>& tuple = *tpl_store[tsidx];
            if (index < tuple.size()) {
                const Tvalue& cval = tuple[index];
                if( cval.isSparse() ){
                    isBoolean = false;
                }
                if(cval.isArray()){
                    isArray=true;
                }
                choices[tsidx] = cval;
            }
        }
    }
    
    muxTValues(NULL, tid, choices, node_ids[node.id], isBoolean, isArray);
}

void NodesToSolver::visit (TUPLE_CREATE_node &node) {
    Tvalue& nvar = node_ids[node.id];
    vector<Tvalue>* new_vec = new vector<Tvalue>(node.multi_mother.size());
    int id = tpl_store.size();
    tpl_store.push_back(new_vec);
    
    vector<bool_node*>::iterator it = node.multi_mother.begin();
    for(int i=0 ; it != node.multi_mother.end(); ++it, ++i){
		const Tvalue& mval = tval_lookup(*it);
        (*new_vec)[i] = mval;
    }
    
	nvar.makeIntVal(YES, id);    
}

void
NodesToSolver::visit (ASSERT_node &node)
{
	assert (node.mother && ! node.father);

	Tvalue fval = tval_lookup (node.mother);


	{
		if(fval.getId() == -YES ) {
			// We can stop adding clauses after this point.
			//If we are in synthesis, this is the end, but if we are in 
			//verification, this may or may not mean something depending on 
			//whether there are assumptions before this point, or if this
			//assertion itself is an assumption.
			if(!dir.getMng().isNegated()){				
				cout<<"  UNSATISFIABLE ASSERTION "<<node.getMsg()<<endl; 				
				errorMsg = "  UNSATISFIABLE ASSERTION ";
				errorMsg += node.getMsg();
				if(PARAMS->verbosity > 7){
					stringstream cstr;
					set<const bool_node*> s;
					cstr<<"digraph G{"<<endl;
					node.printSubDAG(cstr, s);
					cstr<<"}"<<endl;
					cstr<<" slice size = "<<s.size()<<endl;
					if(s.size() < 10){
						cout<<cstr.str()<<endl;
					}
				}
				stopAddingClauses = true;
			}			
		}
		if(node.isHard()){
			//cout << "add hard assert " << fval.getId() << " " << node.lprint() << endl;
			dir.addHardAssertClause (fval.getId ());
		}else{
			if(node.isAssume()){
				dir.addAssumeClause(fval.getId() );
			}else{
				dir.addAssertClause (fval.getId ());
			}			
		}		
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
	
	if( node.getVal() == 1 || node.getVal() == 0){
		node_ids[node.id].makeBitVal(YES, node.getVal()==1);
	}else{
		node_ids[node.id].makeIntVal(YES, node.getVal());
	}
	Dout (cout << "CONST " << node.get_name() << " " << node_ids[node.id]<< endl);
}


void NodesToSolver::doArrArrAcc(const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output){
	//cout << "NodesToSolver.doArrArrAcc " << node.lprint() << endl;
	
	int N = choices.size();	
	for(int i=0; i < N; ++i){		
		if(choices[i].isBvect()){
			choices[i].makeSparse(dir);
		}		
	}
	
	if(mval.isSparse()){
		Assert(false, "NYI aslkdn;hyp;k");
	}else{
		Assert(choices.size() == 2, "NYI aslkdn;hyp;k");
		map<pair<int, int>, int> vals;
		int gval = mval.getId();
		
		const gvvec& gvl = choices[0].num_ranges;	
		Tvalue altL;
		for(int i=0; i<gvl.size() && gvl[i].idx<0; ++i){
			altL.num_ranges.push_back(gvl[i]);
		}
		
		if(altL.num_ranges.size()==0){ altL.makeIntVal(YES, UNINITIALIZED);}
		//if(altL.num_ranges.size()==0){ altL = tvOne; altL.num_ranges[0].value = 0;}
		else{ altL.sparsify(dir); }

		const gvvec& gvr = choices[1].num_ranges;	
		Tvalue altR;
		for(int i=0; i<gvr.size() && gvr[i].idx<0; ++i){
			altR.num_ranges.push_back(gvr[i]);
		}
		
		if(altR.num_ranges.size()==0){ altR.makeIntVal(YES, UNINITIALIZED);}
		//if(altR.num_ranges.size()==0){ altR = tvOne; altR.num_ranges[0].value = 0;}
		else{ altR.sparsify(dir); }

		int idxl = 0;
		int idxr = 0;
		gvvec& out = output.num_ranges;
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
	//cout << "doArrArrAcc " << node.get_name() << ": " << output << endl;
}

void NodesToSolver::addToVals(map<pair<int, int>, int>& vals, gvvec::iterator it, int idx, int gval){
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


void NodesToSolver::doNonBoolArrAcc(const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output){
	Dout( cout<<" non boolean array "<<endl );
	
	int N = choices.size();
	
	for(int i=0; i < N; ++i){		
		if(choices[i].isBvect()){
			choices[i].makeSparse (dir);
		}
	}
	
	if(mval.isSparse()){
		//mval.makeSparse (dir);
		map<int, vector<int> > newVals;
		int vsize = N;
		const gvvec& nrange = mval.num_ranges;

		for(int i=0; i<nrange.size(); ++i){
			if( nrange[i].value < vsize && nrange[i].value >= 0){
				Tvalue& curr = choices[nrange[i].value];
				const gvvec& cvalues = curr.num_ranges;
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

		gvvec& result = output.num_ranges;
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



void NodesToSolver::process(BooleanDAG& bdag){
	int i=0;
	tmpdag = &bdag;
	stopAddingClauses = false;

	bool isNegated = dir.getMng().isNegated();
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
	//		if ((i>=2423808 && i<=2423808+1024) || i%1024 == 0) cout << "processing " << i << " " << (*node_it)->lprint() << endl;
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		int tmpbufs = TOTBUFFERS;
		(*node_it)->accept(*this);
		if(TOTBUFFERS > tmpbufs + 1){
			cout<<"SOMETHING FISHY HERE!!!!"<<endl;
		}

		//Tvalue& tv = node_ids[(*node_it)->id];
//		 cout<<(*node_it)->lprint()<<"--->"<<tv.getSize()<<endl;
//		 cout<<(*node_it)->lprint()<<" -----> "<<tv<<endl;		
//		if(tv.getSize() > 20 && (*node_it)->getOtype() == bool_node::INT ) {cout<<(*node_it)->lprint()<<" -----> "<< tv.getSize()<<"  "<< tv <<endl;}
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    		}
//		catch (exception e) {
//			cout << "exception" << endl;
//			cout << e.what() << endl;
//			throw e;
//		}
		if(stopAddingClauses && !isNegated){
			break;
		}
	}
}

