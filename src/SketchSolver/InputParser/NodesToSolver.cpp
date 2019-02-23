#include "NodesToSolver.h"
#include <algorithm>
#include <functional>
#include "timerclass.h"
#include "Tvalue.h"
#include "CommandLineArgs.h"
#include "PrintInteresting.h"
#include "MiniSATSolver.h"

int TOTBUFFERS = 0;


//extern CommandLineArgs* PARAMS;

//#define Dout(msg) /* msg */

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
		int sz = node_ids[node.id].size();
		for (auto it = node.arg_begin(); it != node.arg_end(); ++it) {
			if (node_ids[(*it)->id].size() > (sz / 2) - 1) {
				tovisit[(*it)->id] = true;
			}
		}
	}
	virtual void print(BooleanDAG& bdag, int seed){
		for(int i=0; i<=seed; ++i){
			if(tovisit[i]){
				int sz = node_ids[i].size();
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



bool NodesToSolver::createConstraints(BooleanDAG& dag, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids, FloatManager& floats, float sparseArray){
	//timerclass timer("defineProblem");
		//timer.start();
	bool stoppedEarly;
		int YES = dir.newYES();
		//getProblem()->lprint(cout);
		NodesToSolver nts(dir, "PROBLEM", node_values, node_ids, floats);	
		nts.sparseArray = sparseArray;
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



template<typename EVAL>
void NodesToSolver::computeMaxOrMin(const gvvec& mv, const gvvec& fv, gvvec& out, bool doMax, EVAL eval){
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
		if(mend -1 > mstart && eval(mv[mstart].value) < eval(mv[mstart+1].value)){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && eval(fv[fstart].value) < eval(fv[fstart+1].value)){
			incj = -1;
			j = fend -1;
		}
	}else{
		if(mend -1 > mstart && eval(mv[mstart].value) > eval(mv[mstart+1].value)){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && eval(fv[fstart].value) > eval(fv[fstart+1].value)){
			incj = -1;
			j = fend -1;
		}
	}
	
	int mcnt = 0; int msz = mend - mstart;
	int fcnt = 0; int fsz = fend - fstart;
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;
			auto curri = avi ? eval(mv[i].value)  : -1;
			auto currj = avj ? eval(fv[j].value)  : -1;
			bool ltmin = curri < currj;
			bool ltmax = curri > currj;
			if((( (ltmin && !doMax) || (ltmax && doMax)  ) && avi) || !avj){
				int tmpv = dir.addAndClause(mv[i].guard, fsofar);
				if(tmpv != -YES){
					out.push_back(guardedVal(tmpv, mv[i].value));
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
					out.push_back(guardedVal(tmpv, fv[j].value));
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
					out.push_back(guardedVal(tmpor, fv[j].value));
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

template<typename EVAL>
int NodesToSolver::compareRange(const gvvec& mv, int mstart, int mend, bool misInt, const gvvec& fv, int fstart, int fend, bool fisInt, EVAL eval){

	if (misInt) {
		if (fisInt) {
			Assert(mend - mstart == 1 && fend - fstart == 1, "Ypoinly;");
			return dir.inteq(mv[mstart].guard, fv[fstart].guard);
		}
		else {
			Assert(mend - mstart == 1 , "Ypoinly;");
			Tvalue eu;
			eu.assignSubrange(fv, fstart, fend);
			dir.intClause(eu);
			return dir.inteq(mv[mstart].guard, eu.getId());
		}
	} else {
		if (fisInt) {
			Assert(fend - fstart == 1, "Ypoinly;");
			Tvalue eu;
			eu.assignSubrange(mv, mstart, mend);
			dir.intClause(eu);
			return dir.inteq(eu.getId(), fv[fstart].guard);
		}
	}


	int orTerms = 0;
	int i=mstart, j=fstart;
	int inci = 1;
	int incj = 1;
	// There is an assumption that the num_ranges are monotonic. 
	// However, they could be monotonically increasing or monotonically decreasing.
	// So we need to check.
	if(mend -1 > mstart && eval(mv[mstart].value) > eval(mv[mstart+1].value)){
		inci = -1;
		i = mend -1;
	}
		
	if(fend -1> fstart && eval(fv[fstart].value) > eval(fv[fstart+1].value)){
		incj = -1;
		j = fend -1;
	}
	vector<int> noeq;
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;
		    // TODO xzl: why -1? does this value really matter?
			int curri = avi ? eval(mv[i].value)  : -1;
			int currj = avj ? eval(fv[j].value)  : -1;
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

template<typename EVAL>
void
NodesToSolver::compareArrays (const Tvalue& tmval,  const Tvalue& tfval, Tvalue& out, EVAL eval){
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
				int rv = compareRange(mv, mistart, miend, tmval.isInt(), fv, fistart, fiend, tfval.isInt(), eval);
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
			int rv = compareRange(mv, mistart, miend,  tmval.isInt(),  fdef.num_ranges, 0, fdef.num_ranges.size(), tfval.isInt(), eval);
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
			int rv = compareRange(mdef.num_ranges, 0, mdef.num_ranges.size(), tmval.isInt(), fv, fistart, fiend, tfval.isInt(), eval);
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

	out = cvar;
	//cout << "compareArrays: cvar=" << cvar << endl;
}








template<typename EVAL>
void NodesToSolver::processLT (LT_node& node, EVAL& eval ){
	bool_node *mother = node.mother();
    Tvalue mval = tval_lookup (mother, TVAL_SPARSE);    

    bool_node *father = node.father();
    Tvalue fval = tval_lookup (father, TVAL_SPARSE);
	if(mval.isArray() || fval.isArray()){
		Assert(false, "Can't do < on arrays");
	}

	if(mval.isInt() || fval.isInt()){
		
		if(!mval.isInt()){
			dir.intClause(mval);
		}
		
		if(!fval.isInt()){
			dir.intClause(fval);
		}
		
		{
			 node_ids[node.id]= (dir.intlt(mval.getId(), fval.getId()));
			 return;
		}
		Assert(false, "Aqirueop");
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
		if(mend -1 > mstart && eval(mv[mstart].value) > eval(mv[mstart+1].value)){
			inci = -1;
			i = mend -1;
		}
		
		if(fend -1> fstart && eval(fv[fstart].value) > eval(fv[fstart+1].value)){
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
	if(eval(mv[i+(msz-1)*inci].value) < eval(fv[j].value)){
		// cout<<"!!! Saved with YES, "<<inci<<endl;
		node_ids[node.id] = YES;
		return;
	}
	//If all the mothers are >= all the fathers, just return false.
	if(eval(mv[i].value) >= eval(fv[j+(fsz-1)*incj].value)){
		// cout<<"!!! Saved with NO, "<<incj<<endl;
		node_ids[node.id] = -YES;
		return;
	}
	//if the largest mother equals the smallest father, then it will be true except for that one case
	if(eval(mv[i+(msz-1)*inci].value) == eval(fv[j].value) && (fsz==1 || msz==1)){
		// cout<<"!!! Saved with YES, "<<inci<<endl;
		int out = -dir.addAndClause(mv[i+(msz-1)*inci].guard, fv[j].guard) ;
		node_ids[node.id] = out;
		for(int qq = 0; qq<msz; ++qq){
			if(qq != i+(msz-1)*inci){
				dir.addHelperC(out, -mv[qq].guard);
			}
		}
		for(int qq = 0; qq<fsz; ++qq){
			if(qq != j){
				dir.addHelperC(out, -fv[qq].guard);
			}
		}
		return;
	}


	vector<char> mc(mval.size(), 'n');
	vector<char> fc(fval.size(), 'n');
	while( (i>=mstart && i < mend) || (j>=fstart && j< fend)){
		    bool avi = i < mend && i >= mstart;
		    bool avj = j < fend && j >= fstart;			
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
			auto curri = eval(mv[i].value) ;
			auto currj = eval(fv[j].value) ;
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
	Dout(cout << cvar << endl;)
}

template<typename EVAL>
void
NodesToSolver::processEq (Tvalue& mval, Tvalue& fval, Tvalue& out, EVAL eval)
{
    //cout << "comparing " << node.lprint() << endl; 

    //cout << "comparing " << node.lprint() << " mval=" << mval << " fval=" << fval << endl; 
	if(mval.isArray() || fval.isArray()){
		if(mval.isBvect()){
			mval.makeSparse(dir);
		}
		if(fval.isBvect()){
			fval.makeSparse(dir);
		}

		compareArrays(mval, fval, out, eval);
		return;
	}

	if(mval.isInt() || fval.isInt()){
		
		if(!mval.isInt()){
			dir.intClause(mval);
		}
		
		if(!fval.isInt()){
			dir.intClause(fval);
		}		
		
		out = (dir.inteq(mval.getId(), fval.getId()));
		return;
		
		Assert(false, "Aqirueop");
	}

	mval.makeSparse (dir);
    fval.makeSparse (dir);
    int cvar = -YES;
    Dout(cout<<"SIZES = "<<mval.size()<<", "<<fval.size()<<endl);
    int orTerms = 0;
	vector<char> mc(mval.size(), 'n');
	vector<char> fc(fval.size(), 'n');
	int flow = 0;
	int fhigh = fval.size();
	int finc = 1;
	

	

    for(int i=0; i<mval.size(); ++i){
		for(int j=flow; j!=fhigh; j = j+finc){
		    Dout(cout<<"COMPARING "<<mval[i]<<", "<<fval[j]<<endl);
		    if(mval[i] == fval[j]){
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
		if(orTerms == mval.size() * fval.size()){
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



bool tvSimilar(Tvalue& t1, Tvalue& t2) {
	if (t1.size() != t2.size()) { return false; }
	for (int i = 0; i < t1.size(); ++i) {
		if (t1.getId(i) != t2.getId(i)) {
			return false;
		}
	}
	return true;
}


template<class COMPARE_KEY, typename THEOP> void
NodesToSolver::processFloatArith(bool_node &node, THEOP comp, COMPARE_KEY c)
{
	bool_node* mother = node.mother();
	Tvalue mval = tval_lookup(mother, TVAL_SPARSE);

	bool_node* father = node.father();
	Tvalue fval = tval_lookup(father, TVAL_SPARSE);
	mval.makeSparse(dir);
	fval.makeSparse(dir);

	map<int, int, COMPARE_KEY> numbers(c);
	map<int, vector<int> > qnumbers;
	Tvalue& oval = node_ids[node.id];


	Dout(cout << "ARITHOP " << mval << "  OP  " << fval << endl);
	Dout(cout << "OPERATING " << node.father->get_name() << "  WITH  " << node.mother->get_name() << endl);
	int vals = 0;
	//cout<<" BEFORE THE LOOPS"<<endl;
	//				timerclass atimer("TA");
	//				timerclass btimer("TB");
	//				timerclass ctimer("TC");
	//				timerclass dtimer("TD");
	
	
	if (tvSimilar(mval, fval)) {

		for (int i = 0; i < mval.size(); ++i) {
			int j = i;

			int quant = doArithExpr(mval[i], fval[j], mval.getId(i), fval.getId(j), comp);

			qnumbers[quant].push_back(i);
			qnumbers[quant].push_back(j);
		}
	}
	else {
		bool checkFeasible = false;
		if (mval.size() * fval.size() > 256) {
			checkFeasible = true;
		}
		for (int i = 0; i < mval.size(); ++i) {
			int lv = -1;
			if (checkFeasible) {
				if (!dir.checkIfPossible(mval.getId(i), lv)) {
					continue;
				}
			}

			for (int j = 0; j < fval.size(); ++j) {

				if (checkFeasible) {
					int lv;
					if (!dir.checkIfPossible(fval.getId(j), lv)) {
						continue;
					}
					dir.popCheckIfPossible(lv);
				}
				int quant = doArithExpr(mval[i], fval[j], mval.getId(i), fval.getId(j), comp);
				
				qnumbers[quant].push_back(i);
				qnumbers[quant].push_back(j);
			}
			if (checkFeasible) {
				dir.popCheckIfPossible(lv);
			}
		}
	}

	vector<int> fidxs(fval.size(), 0);
	vector<int> midxs(mval.size(), 0);
	for (auto it = qnumbers.begin(); it != qnumbers.end(); ++it) {
		bool multiple = false;
		vector<pair<int, int> > mallsame;
		vector<pair<int, int> > fallsame;
		fidxs.assign(fval.size(), 0);
		midxs.assign(mval.size(), 0);
		for (auto pairit = it->second.begin(); pairit != it->second.end(); pairit+=2) {
			midxs[*pairit] ++;
			fidxs[*(pairit + 1)] ++;
			if (midxs[*pairit] > 1 || fidxs[*(pairit + 1)] > 1) {
				multiple = true;
			}
			if (midxs[*pairit] == fval.size()) {
				mallsame.push_back(make_pair(it->first,*pairit) );
			}
			if(fidxs[*(pairit + 1)] == mval.size()) {
				fallsame.push_back(make_pair(it->first, *(pairit + 1)));
			}

		}
		if (mallsame.size()>0 || fallsame.size()>0) {

			for (auto it = mallsame.begin(); it != mallsame.end(); ++it) {
				auto numit = numbers.find(it->first);
				if (numit == numbers.end()) {
					numbers[it->first] = mval.getId(it->second);
				}
				else {
					numit->second = dir.addOrClause(numit->second, mval.getId(it->second));
				}
			}
			for (auto it = fallsame.begin(); it != fallsame.end(); ++it) {
				auto numit = numbers.find(it->first);
				if (numit == numbers.end()) {
					numbers[it->first] = fval.getId(it->second);
				}
				else {
					numit->second = dir.addOrClause(numit->second, fval.getId(it->second));
				}
			}
			for (auto pairit = it->second.begin(); pairit != it->second.end(); pairit += 2) {
				int mv = *pairit;
				int fv = *(pairit + 1);
				if (midxs[mv] != fval.size() && fidxs[fv] != mval.size()) {
					numbers[it->first] = dir.addOrClause(numbers[it->first], dir.addAndClause(mval.getId(mv), fval.getId(fv)));
				}				
			}
		}else if (!multiple) {
			for (auto pairit = it->second.begin(); pairit != it->second.end(); pairit += 2) {
				*pairit = mval.getId(*pairit);
				*(pairit + 1) = fval.getId(*(pairit + 1));
			}
			int id = dir.addExPairConstraint(&(it->second[0]), it->second.size() / 2);
			numbers[it->first] = id;
			++vals;
		}
		else {
			int cvar2 = -YES;
			for (auto pairit = it->second.begin(); pairit != it->second.end(); pairit += 2) {
				int cvar = dir.addAndClause(mval.getId(*pairit), fval.getId(*(pairit + 1)));
				cvar2 = dir.addOrClause(cvar, cvar2);				
			}
			numbers[it->first] = cvar2;
			++vals;
		}
	}

	

	Dout(cout << "tmp size = " << numbers.size() << endl);
	populateGuardedVals(oval, numbers);

}




template<class COMPARE_KEY, typename THEOP> void
NodesToSolver::processArith (bool_node &node, THEOP comp, COMPARE_KEY c)
{    
	bool_node* mother = node.mother();
	Tvalue mval = tval_lookup (mother, TVAL_SPARSE);

	bool_node* father = node.father();
	Tvalue fval = tval_lookup (father, TVAL_SPARSE);

	if (NATIVEINTS) {
		if (mval.size() > 64 && mval.size() > 64) {
			if (!mval.isInt()) {
				dir.intClause(mval);
			}
			if (!fval.isInt()) {
				dir.intClause(fval);
			}
		}
	}



	if(mval.isInt() || fval.isInt()){
		if(!mval.isInt()){
			dir.intClause(mval);
		}
		if(!fval.isInt()){
			dir.intClause(fval);
		}
		if(node.type== bool_node::PLUS){
			 node_ids[node.id].makeSuperInt(dir.plus(mval.getId(), fval.getId()));
			 return;
		}
		if(node.type== bool_node::TIMES){
			 node_ids[node.id].makeSuperInt(dir.times(mval.getId(), fval.getId()));
			 return;
		}
		if(node.type== bool_node::MOD){
			 node_ids[node.id].makeSuperInt(dir.mod(mval.getId(), fval.getId()));
			 return;
		}
		if(node.type== bool_node::DIV){
			 node_ids[node.id].makeSuperInt(dir.div(mval.getId(), fval.getId()));
			 return;
		}
		Assert(false, "Aqirueop");
	}

	mval.makeSparse (dir);
	
	fval.makeSparse (dir);

	if (node.type == bool_node::TIMES &&  mval.size() * fval.size() > 100000) {
		cout << "WARNING: It looks like this problem may benefit from using the --slv-nativeints flag." << endl;
	}

	bool isSum = node.type == bool_node::PLUS || node.type == bool_node::TIMES;
	bool skipZeros = node.type == bool_node::TIMES || node.type == bool_node::DIV || node.type == bool_node::MOD;
  
	map<int, int, COMPARE_KEY> numbers(c);
	map<int, vector<int> > qnumbers;
	Tvalue& oval = node_ids[node.id];
	
	
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
		for(int i=0; i<mval.size(); ++i){
			if(mval[i]==0){ 
				zem = mval.getId (i);	
				break;
			}
		}
		for(int j=0; j<fval.size(); ++j){
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
	if(PARAMS->randBnd > 0 && mval.size() * fval.size() > 3*PARAMS->randBnd){
		shortcut = true;
	}else{
		shortcut = false;
	}



	for(int i=0; i<mval.size(); ++i){
		if(skipZeros && mval[i] == 0){ continue; }
		
	    for(int j=0; j<fval.size(); ++j){
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

	/*
	When 'sum' is true, we have the property that every value appears only once per column and once per row. 
	This property is exploited by addExPairConstraint. For every output value, qnumbers has the list
	of row-column pairs that can produce that value. 
	
	
	*/


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
				if(numbers.size()>0){
					int id = rand() % numbers.size();
					map<int, int>::iterator mit = numbers.begin();
					for(int i=0; i<id; ++i){ ++mit; }				
					mit->second = dir.addOrClause(mit->second, refuse);			
				}else{
					numbers[ rand() % PARAMS->randBnd] = refuse;
				}
			}else{
				Assert(numbers.count(PARAMS->randBnd*2)==0, "Lnliurya;");
				numbers[PARAMS->randBnd*2] = refuse;		
			}			
		}
		vals = numbers.size();
	}

	Dout(cout<<"tmp size = "<<numbers.size ()<<endl);
	Assert( vals > 0 && vals == numbers.size(), "NotesToSolver::processArith: This should not happen here "<<vals<<"  "<<numbers.size());
	populateGuardedVals(oval, numbers);
}

template <class COMPARE_KEY>
void NodesToSolver::populateGuardedVals(Tvalue& oval, map<int, int, COMPARE_KEY>& numbers ) {
	gvvec& tmp = oval.num_ranges;
	tmp.clear();	
	tmp.resize(numbers.size());
	auto it = numbers.begin();

	int i;
	for (i = 0; it != numbers.end(); ++it) {
		if (it->second == YES) {
			tmp.resize(1);
			tmp[0] = guardedVal(it->second, it->first);
			for (auto sit = numbers.begin(); sit != numbers.end(); ++sit) {
				if (sit->second != it->second) {
					dir.addAssertClause(-sit->second);
				}
			}
			i = 1;
			break;
		}
		if (it->second == -YES) {
			continue;
		}
		tmp[i] = guardedVal(it->second, it->first);
		++i;
	}
	tmp.resize(i);
	if (tmp.size() == 1) {
		if (tmp[0].guard != YES) {
			dir.addAssertClause(tmp[0].guard);
			tmp[0].guard = YES;
		}
	}
	oval.sparsify(dir);
	if (oval.size() == 0) {
		stopAddingClauses = true;
		//This means that the problem has become unsat.
	}
	else {
		dir.addHelperC(oval);
	}
	Dout(cout << " := " << oval << endl);
}



void NodesToSolver::visit( AND_node& node ){
	Tvalue fval = tval_lookup(node.father());
	Tvalue mval = tval_lookup(node.mother());
	
	if(fval.isInt()){
		fval = dir.intToBit(fval.getId());
	}
	if(mval.isInt()){
		mval = dir.intToBit(mval.getId());
	}

	
	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addAndClause(fval.getId (), mval.getId ());
	
	Dout(cout<<"AND "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}


void NodesToSolver::visit( OR_node& node ){
	
	Tvalue fval = tval_lookup(node.father());
	Tvalue mval = tval_lookup(node.mother());
	
	if(fval.isInt()){
		fval = dir.intToBit(fval.getId());
	}
	if(mval.isInt()){
		mval = dir.intToBit(mval.getId());
	}

	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addOrClause(fval.getId (), mval.getId ());
	
	Dout(cout<<"OR "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
	return;
}


void NodesToSolver::visit( XOR_node& node ){
	
	Tvalue fval = tval_lookup(node.father());
	Tvalue mval = tval_lookup(node.mother());
	
	if(fval.isInt()){
		fval = dir.intToBit(fval.getId());
	}
	if(mval.isInt()){
		mval = dir.intToBit(mval.getId());
	}

	Tvalue& nvar = node_ids[node.id];
	nvar = dir.addXorClause(fval.getId (), mval.getId ());
	
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
		    tmp.makeSparse (dir, 1, node_values[(&node)]);
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
		    Dout (cout << "setting input nodes " << node.get_name() << endl);
		    // In the future, I may want to make some of these holes not-sparse.
			if(arrSz<0){				
				Assert( dir.getArrSize(node.get_name()) == node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
				node_ids[node.id].makeSparse (dir, node.get_nbits());
			}else{				
				Assert( dir.getArrSize(node.get_name()) == arrSz*node.get_nbits (), "THIS IS basd nbits = "<<node.get_nbits ()<<"  dir.getArrSize(node.get_name())="<<dir.getArrSize(node.get_name()) );
				node_ids[node.id].makeArray (dir, node.get_nbits(), arrSz, sparseArray);
			}
		}

	Dout(cout << "REGISTERING " << node.get_name() << "  " << node_ids[node.id]
	      << "  " << &node << endl);
    }
    
    // for input arrays, add the default value (out of bound) to be 0, if not present already
    node_ids[node.id].addArrDefault(YES, 0);
}



void NodesToSolver::visit( DST_node& node ){
	node_ids[node.id] = tval_lookup(node.mother());
	Dout(cout<<"DST = "<<node_ids[node.id]<<endl);
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

    const Tvalue &mval = tval_lookup (node.mother());

	if(mval.isInt()){
		Tvalue nvar = -dir.intToBit(mval.getId());
		node_ids[node.id] = nvar;
		return;
	}

    if(!( mval.getType() == TVAL_BIT)){
    	cerr<<" BAD NODE "<<endl;
    	node.mother()->outDagEntry(cerr);
    	node.outDagEntry(cerr);
    }
    Assert( mval.getType() == TVAL_BIT, "Bad Type for NOT "<<mval.size()<<" "<<mval.getType());
    Tvalue nvar = -mval.getId ();
	node_ids[node.id] = nvar;    

    Dout (cout << "PT " << node.get_name() << " " << nvar << " " << &node << endl);
}



void
NodesToSolver::visit(NEG_node &node)
{	
	const Tvalue &mval = tval_lookup(node.mother());

	if (mval.isInt()) {
		Tvalue tv;
		tv.makeIntVal(YES, 0);
		int gid = dir.intClause(tv);
		node_ids[node.id].makeSuperInt(dir.minus(gid, mval.getId()));
		return;

	}

	Tvalue nvar = mval.toComplement(dir);
	node_ids[node.id] = nvar;

	Dout(cout << "NEG " << node.get_name() << " " << nvar << " " << &node << endl);
}


void
NodesToSolver::visit (CTRL_node &node)
{
    //cout << "NodesToSolver CTRL " << node.lprint() << endl;
    const int nbits = node.get_nbits();
    if(  node_values.find(&node) != node_values.end() ){
		if( node.get_nbits() > 1 ){
		    Tvalue tmp = tvYES;
		    tmp.makeSparse (dir, 1, node_values[(&node)]);

		    node_ids[node.id] = tmp;
		    Dout( cout<<" control "<<node.get_name()<<" = "<<node_ids[node.id]<<"    "<<node_values[(&node)]<<endl);
		}else{
		    node_ids[node.id] = node_values[(&node)]*YES;
		}		
		return;
    }else{
		Tvalue & nvar = node_ids[node.id];
		if(node.get_Angelic()){
		  // BUGFIX: Issue #5
		  // when node is an array, need to create array
		  const int arrSz = node.getArrSz();
		  //cout << "Angelic " << node.lprint() << " arrSz=" << arrSz << " nbits=" << nbits << endl;
		  if(arrSz<0){
			nvar = dir.newAnonymousVar(nbits);			
			if (nbits > 1) {
			  nvar.makeSparse(dir, nbits);
			}
		  }else{
			const int totbits = nbits*arrSz;
			nvar = dir.newAnonymousVar(totbits);			
			nvar.makeArray(dir, nbits, arrSz);
		  }
      
		}else{
			nvar = dir.getControl(&node);
			if (NATIVEINTS && nvar.isSparse() && nvar.num_ranges.size() > 64) {
					dir.intClause(nvar);	
			}
		}		
		Dout(cout<<"CONTROL "<<node.get_name()<<"  "<<node_ids[node.id]<<"  "<<&node<<endl);
		return;
    }
}



void NodesToSolver::visit( PLUS_node& node ){
	Dout( cout<<" PLUS: "<<node.get_name()<<endl );


	if (node.getOtype() == OutType::FLOAT) {
		processFloatArith<FloatComp>(node, FloatOp<plus<float> >(floats), FloatComp(floats));
	}else {
		processArith(node, plus<int>());
	}	
}

void NodesToSolver::visit( TIMES_node& node ){
	Dout( cout<<" TIMES: "<<node.get_name()<<endl );	
	if (node.getOtype() == OutType::FLOAT) {
		processFloatArith<FloatComp>(node, FloatOp<multiplies<float> >(floats), FloatComp(floats));
	}
	else {
		processArith(node, multiplies<int>());
	}
}


void NodesToSolver::visit(DIV_node& node) {
	Dout(cout << " DIV " << endl);
	if (node.getOtype() == OutType::FLOAT) {
		processFloatArith<FloatComp>(node, FloatOp<divides<float> >(floats), FloatComp(floats));
	} else {
		processArith(node, divides<int>());
	}
}

void NodesToSolver::visit(MOD_node& node) {
	Dout(cout << " MOD " << endl);

	processArith(node, modulus<int>());
}


int ident(int x) {
	return x;
}

void
NodesToSolver::visit(LT_node &node)
{
	Dout(cout << " LT " << endl);

	if (node.mother()->getOtype()== OutType::FLOAT) {
		processLT(node, floats);
	} else {
		processLT(node, ident);
	}
}



void
NodesToSolver::visit(EQ_node &node)
{
	Dout(cout << " EQ " << endl);


	bool_node *mother = node.mother();
	Tvalue mval = tval_lookup(mother, TVAL_SPARSE);

	bool_node *father = node.father();
	Tvalue fval = tval_lookup(father, TVAL_SPARSE);
	if (mother->getOtype() == OutType::FLOAT || mother->getOtype() == OutType::FLOAT_ARR || father->getOtype() == OutType::FLOAT || father->getOtype() == OutType::FLOAT_ARR) {
		processEq(mval, fval, node_ids[node.id], floats);
	} else {
		processEq(mval, fval, node_ids[node.id], ident);
	}
	Dout(cout << node.get_name() << " :=  " << node_ids[node.id] << endl);
}


bool NodesToSolver::checkKnownFun(UFUN_node& node) {
	const string& name = node.get_ufname();	
	if (floats.hasFun(name) || name == "_cast_int_float_math") {
		Tvalue mval = tval_lookup(node.arguments(0), TVAL_SPARSE);
		mval.makeSparse(dir);
		
		map<int, vector<int> > qnumbers;
    FloatComp c = FloatComp(floats);
		map<int, int, FloatComp> numbers(c);
				
		if (name == "_cast_int_float_math") {
			for (int i = 0; i < mval.size(); ++i) {
				int quant = floats.getIdx((float)mval[i]);
				qnumbers[quant].push_back(mval.getId(i));
			}			
		}else {
			FloatFun f = floats.getFun(name);
			for (int i = 0; i < mval.size(); ++i) {
				int quant = f(mval[i]);
				qnumbers[quant].push_back(mval.getId(i));				
			}
		}
		
		for (auto qnit = qnumbers.begin(); qnit != qnumbers.end(); ++qnit) {
			vector<int>& vv = qnit->second;
			vv.insert(vv.begin(), 0);
			dir.addBigOrClause(&vv[0], vv.size()-1);
			numbers[qnit->first] = vv[0];
		}
		vector<Tvalue>* new_vec = new vector<Tvalue>(1);
		populateGuardedVals((*new_vec)[0], numbers);
		Tvalue& nvar= node_ids[node.id];
		nvar.num_ranges.clear();
		regTuple(new_vec, nvar);
		return true;
	}
	return false;
}


void NodesToSolver::preprocessUfun(UFUN_node& node) {
  bool isVerification = dir.getMng().isNegated();
  
  
  string tuple_name = node.getTupleName();
  Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));
  
  if (tuple_type->entries.size() == 0) {
    Tvalue& outvar = node_ids[node.id];
    outvar = -YES;
    return;
  }
  int nbits = tmpdag->getIntSize();
  if (!isVerification){
    nbits = PARAMS->custIntSize;
  }
  //TODO: SynthInSynth changed this nbits variable
  int nouts = tuple_type->entries.size();
  vector<Tvalue> nvars(nouts);
  int totbits = 0;
  for (int i = 0; i<nouts; ++i) {
    OutType* ttype = tuple_type->entries[i];
    stringstream sstr;
    sstr << node.get_ufname() << "_" << node.get_uniquefid() << "_" << i;
    bool isArr = ttype->isArr;
    bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
    int cbits = isBool ? 1 : nbits;
	  
    if (isVerification) {
      nvars[i] = dir.getArr(sstr.str(), 0);
    } else {
      nvars[i] = dir.newAnonymousVar(cbits);
    }
    
    if (isArr || !isBool) {
      if (isArr) {
        Assert(isVerification, "NONONO");
        int arrsz = dir.getArrSize(sstr.str());        
        nvars[i].makeArray(dir, cbits, arrsz / cbits, sparseArray);
      }
      else {        
        nvars[i].makeSparse(dir, cbits);
      }
      totbits += nvars[i].num_ranges.size();
    }
    else {
      totbits += 1;
    }
  }
  ufunVarsMap[node.id] = nvars;
  Tvalue& outvar = node_ids[node.id];
  vector<Tvalue>* new_vec = new vector<Tvalue>(nouts);
  int outid = tpl_store.size();
  tpl_store.push_back(new_vec);
  for (int i = 0; i<nouts; ++i) {
    (*new_vec)[i] = nvars[i];
  }
  outvar.makeIntVal(YES, outid);
}

void NodesToSolver::visit( UFUN_node& node ){
  
  if (checkKnownFun(node)) {
    return;
  }
  
  bool isVerification = dir.getMng().isNegated();


	string tuple_name = node.getTupleName();
	Tuple* tuple_type = dynamic_cast<Tuple*>(OutType::getTuple(tuple_name));

	if (tuple_type->entries.size() == 0) {
		return;
	}


	vector<Tvalue> params;
	for (int i = 0; i<node.nargs(); ++i) {
		params.push_back(tval_lookup(node.arguments(i), TVAL_SPARSE));
	}
  vector<Tvalue>& nvars = ufunVarsMap[node.id];
  int totbits = 0;
  int nouts = nvars.size();
  for (int i = 0; i<nouts; ++i) {
    OutType* ttype = tuple_type->entries[i];
    bool isArr = ttype->isArr;
    bool isBool = (ttype == OutType::BOOL || ttype == OutType::BOOL_ARR);
    
    if (isArr || !isBool) {
      totbits += nvars[i].num_ranges.size();
    }
    else {
      totbits += 1;
    }
  }

	if(isVerification){
		//This means you are in the checking phase.

		map<string, int>::iterator idit = ufunids.find(node.get_ufname());

		int funid = -1;
		if (idit == ufunids.end()) {
			Assert(ufunids.size() == ufinfos.size(), ";ly7u8AA?");
			int sz = ufunids.size();
			ufunids[node.get_ufname()] = sz;
			funid = sz;
			ufinfos.push_back(vector<Ufinfo>());
		}
		else {
			funid = idit->second;
		}
		
		vector<Ufinfo>& oldparams(ufinfos[funid]);
		vec<Lit> equivs(oldparams.size());
		int nparams = params.size();
		for (int cid = 0; cid < oldparams.size(); ++cid) {
			vector<Tvalue>& other = oldparams[cid].params;
			int vvar = 0;
			for (int i = 0; i<nparams; ++i) {
				Tvalue tmpb;
				processEq(params[i], other[i], tmpb, ident);
				if (vvar == 0) {
					vvar = tmpb.getId();
				}else {
					vvar = dir.addAndClause(vvar, tmpb.getId());
				}
			}
			equivs[cid] = lfromInt(vvar);
		}


		UfunSummary* ufs = newUfun(equivs, nvars, totbits, dir);
		ufinfos[funid].push_back(Ufinfo(params));


		MiniSATSolver* ms = (MiniSATSolver*) (&dir.getMng());
		
		ms->addUfun(funid, ufs);
				
	}else{		
		newSynthesis(node.get_ufname(), node.getTupleName(), params, nvars, dir);
	}


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




void NodesToSolver::newSynthesis(const string& name, const string& synthname, vector<Tvalue>& params, vector<Tvalue>& nvars, SolverHelper& dir) {
	dir.addSynthSolver(name, synthname, params, nvars, floats);
}




void NodesToSolver::muxTValues(ARRACC_node* pnode, const Tvalue& mval, vector<Tvalue>& choices, Tvalue& out, bool isBoolean, bool isArray, bool isInt, bool isFloat){

	
	if(isArray){
    if (isFloat) {
      doArrArrAcc(mval, choices, out, floats);
    } else {
		
		if (isInt) {
			for (int i = 0; i < choices.size(); ++i) {
				Tvalue& tv = choices[i];
				if (!tv.isInt()) {
					dir.intClause(tv);
				}
			}
		}
		
		doArrArrAcc(mval, choices, out, ident);
		
      
    }
		return;
	}

	if(isInt){
		Tvalue mv = mval;
		if(!mv.isInt()){
			dir.intClause(mv);
		}
		vector<iVar> chs;
		for(int i=0; i<choices.size(); ++i){
			Tvalue& tv = choices[i];
			if(!tv.isInt()){
				dir.intClause(tv);
			}
			chs.push_back(tv.getId());
		}
		out.makeSuperInt(dir.mux(mv.getId(), chs.size(), &chs[0]));
		return;
	}

	if(pnode != NULL){
		ARRACC_node& node = *pnode;
		if(node.mother()->type == bool_node::LT){
			if(node.mother()->mother() == node.arguments(0) && 
				node.mother()->father() == node.arguments(1)){
					if(choices[0].isBvect()){choices[0].makeSparse(dir, 1);}
					if(choices[1].isBvect()){choices[1].makeSparse(dir, 1);}
          if (node.mother()->mother()->getOtype() == OutType::FLOAT || node.mother()->father()->getOtype() == OutType::FLOAT) {
            computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, true, floats);
          } else {
            computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, true, ident);
          }
					out.sparsify(dir);
					return;
			}
			if(node.mother()->mother() == node.arguments(1) && 
				node.mother()->father() == node.arguments(0)){
					if(choices[0].isBvect()){choices[0].makeSparse(dir, 1);}
					if(choices[1].isBvect()){choices[1].makeSparse(dir, 1);}
          if (node.mother()->mother()->getOtype() == OutType::FLOAT || node.mother()->father()->getOtype() == OutType::FLOAT) {
            computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, false, floats);
          } else {
            computeMaxOrMin(choices[0].num_ranges, choices[1].num_ranges, out.num_ranges, false, ident);
          }
					out.sparsify(dir);
					return;
			}
		}
	}
	if(!isBoolean){
//		nonbooltimer.restart();
    if (isFloat) {
      doNonBoolArrAcc<FloatComp>(mval, choices, out, floats, FloatComp(floats));
    } else {
      doNonBoolArrAcc(mval, choices, out, ident);
    }
//		nonbooltimer.stop().print();
//		aracctimer.stop().print();		
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
		Dout(cout<<"ARRACC "<<pnode->get_name()<<"  "<<out<<"   "<<pnode<<endl);
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
	Dout(if (pnode != NULL) { cout << "ARRACC " << pnode->get_name() << "  " << out << "   " << pnode << endl; });
}





//timerclass aracctimer("ARRACC TIMER");
//timerclass flooptimer("FIRST LOOP TIMER");
//timerclass nonbooltimer("NON BOOL TIMER");
//timerclass elooptimer("FINAL LOOP TIMER");


void NodesToSolver::visit( ARRACC_node& node ){


	Dout(cout<<" ARRACC "<<endl);
	//cout<<"NodesToSolver.visit ARRACC "<< node.lprint() << endl;
	const Tvalue& omv = tval_lookup(node.mother()) ;	
	bool isSparse = omv.isSparse();
	bool isInt = omv.isInt();
    Dout(cout<<" mother = "<<node.mother->get_name()<<"  mid = "<<omv<<" "<<endl);
	if( isSparse && omv.getId () == YES && omv.num_ranges.size() == 1){
		int idx = omv.num_ranges[0].value;

		if( idx >= node.nargs() || idx < 0){
			node_ids[node.id] = -YES;
			Dout( cout<<node.get_name()<<" SHORTCUT "<<omv<<" out of range"<<endl );
			return;
		}

		bool_node* choice = node.arguments(idx);
				
		node_ids[node.id] = tval_lookup(choice);				
		return;
	}
	
	bool_node::parent_iter it = node.arg_begin();	
	vector<Tvalue> choices(node.nargs());
	bool parentSame = true;
	bool parentSameBis = true;
	bool isBoolean=true;	
	bool isArray = false;
  bool isFloat = false;
//	aracctimer.restart();
//	flooptimer.restart();
	for(int i=0; it != node.arg_end(); ++i, ++it){
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  ");
		const Tvalue& cval = tval_lookup(*it);
		if( cval.isSparse() ){
			isBoolean = false;
		}
		if(cval.isArray()){
			isArray=true;
		}
		if(cval.isInt()){
			isInt = true;
		}
		if (node.arguments(i)->getOtype() == OutType::FLOAT) {
		  isFloat = true;
		}
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag);
	}

	muxTValues(&node, omv, choices, node_ids[node.id], isBoolean, isArray, isInt, isFloat);

//	elooptimer.stop().print();
//	aracctimer.stop().print();
	return;
}










template<typename EVAL>
void NodesToSolver::mergeTvalues(int guard, const gvvec& nr0, int nr0Start, int nr0End, const gvvec& nr1, int nr1Start, int nr1End, gvvec& out,EVAL eval, bool isInt, int idx){


	if (isInt) {
		Tvalue tv = guard;
		dir.intClause(tv);
		iVar cho[2] = { iVar(nr0[nr0Start].guard), iVar(nr1[nr1Start].guard) };
		out.push_back(guardedVal(dir.mux(tv.getId(), 2, cho  ), -1, idx));
		return;
	}

	int i=nr0Start, j=nr1Start;
	out.reserve(out.size()+ (nr0End-nr0Start) + (nr1End-nr1Start) );
	
		int inci = 1;
		int incj = 1;
		// There is an assumption that the num_ranges are monotonic. 
		// However, they could be monotonically increasing or monotonically decreasing.
		// So we need to check.
		if(nr0End -1 > nr0Start && eval(nr0[nr0Start].value) > eval(nr0[nr0Start+1].value)){
			inci = -1;
			i = nr0End -1;
		}
		
		if(nr1End -1> nr1Start && eval(nr1[nr1Start].value) > eval(nr1[nr1Start+1].value)){
			incj = -1;
			j = nr1End -1;
		}
		int added = 0;

		while( (i>=nr0Start && i < nr0End) || (j>=nr1Start && j< nr1End)){
		    bool avi = i < nr0End && i >= nr0Start;
		    bool avj = j < nr1End && j >= nr1Start;
			auto curri = avi ? eval(nr0[i].value)  : -1;
			auto currj = avj ? eval(nr1[j].value)  : -1;
		    if( curri == currj && avi && avj){
				Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
				int ni = i + inci;
				int nj = j + incj;
				if(added == 1 && ! ((ni>=nr0Start && ni < nr0End) || (nj>=nr1Start && nj< nr1End)) ){
					int cvar = -out[out.size()-1].guard;
					if(cvar!=-YES){
						out.push_back(guardedVal(cvar, nr0[i].value, idx));
					}
					break;
				}
				int cvar3 = dir.addChoiceClause(guard, nr1[j].guard,nr0[i].guard);
				if(cvar3!= -YES){ ++added; out.push_back(guardedVal(cvar3, nr0[i].value, idx));	}
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
						out.push_back(guardedVal(cvar, nr0[i].value, idx));
						dir.addHelperC(-cvar, -guard);
						dir.addHelperC(-cvar, nr0[i].guard);
					}
					break;
				}
				int cvar = dir.addAndClause( nr0[i].guard, -guard);
				if(cvar!=-YES){++added; out.push_back(guardedVal(cvar, nr0[i].value, idx)); }
				i = ni;
				continue;
		    }
		    if( (currj < curri && avj) || !avi ){
				Dout(cout<<" currj = "<<currj<<endl);
				int nj = j + incj;
				if(added == 1 && ! ((avi) || (nj>=nr1Start && nj< nr1End)) ){
					int cvar = -out[out.size()-1].guard;
					if(cvar!=-YES){
						out.push_back(guardedVal(cvar, nr1[j].value, idx));
						dir.addHelperC(-cvar, guard);
						dir.addHelperC(-cvar, nr1[j].guard);
					}
					break;
				}
				int cvar = dir.addAndClause( nr1[j].guard, guard );
				if(cvar!=-YES){++added; out.push_back(guardedVal(cvar, nr1[j].value, idx)); }
				j = nj;
				continue;
		    }
		    Assert(false, "Should never get here");
		}		
}


template<typename EVAL>
void NodesToSolver::mergeTvalues(int guard, Tvalue& mid0, Tvalue& mid1, Tvalue& output, int& flag, EVAL eval){
		if( !mid0.isSparse() ){
		    mid0.makeSparse(dir, 1);
		}
		if( !mid1.isSparse() ){
		    mid1.makeSparse(dir, 1);
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
		
		mergeTvalues(guard, nr0, 0, nr0.size(), nr1, 0, nr1.size(), out, eval, false);


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

	bool isInt = false;

    bool_node* mother = node.mother();
    const Tvalue& mval = tval_lookup(mother) ;
	isInt = isInt || mval.isInt();

    int quant = node.quant;
    Dout(cout<<" mother = "<<((mother != NULL)?mother->get_name():"NULL")<<"  mid = "<<mval<<"  mquant = "<<quant<<endl);
    bool_node::parent_iter it = node.p_begin()+1;    
    
    vector<Tvalue> choices(2);
    vector<bool_node*> mothers(2);
    bool parentSame = true;
    bool isBoolean=true;
    for(int i=0; it != node.p_end(); ++i, ++it){
		const Tvalue& cval = tval_lookup(*it);
		if( cval.isSparse() ){
		    isBoolean = false;
		}
		isInt = isInt || cval.isInt();
		Assert(!cval.isArray(), "ARRASS doesn't work for arrays");
		mothers[i] = *it;
		Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"   ");
		choices[i] = cval;
		Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
		parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
    }
    
	if(isInt){
		if(mval.isInt()){			
			Tvalue tv;
			tv.makeIntVal(YES, node.quant);			
			int gid = dir.intClause(tv);			
			int beqid = dir.inteq( gid , mval.getId());
			int eqid;
			if(beqid==YES){
				Tvalue uv; uv.makeIntVal(YES, 1);
				eqid = dir.intClause(uv);
			}else if(beqid==-YES){
				Tvalue uv; uv.makeIntVal(YES, 0);
				eqid = dir.intClause(uv);
			}else{
				eqid = dir.bitToI(beqid);
			}
			vector<iVar> chs;
			for(int i=0; i<choices.size(); ++i){
				chs.push_back(dir.intClause(choices[i]));
			}
			node_ids[node.id].makeSuperInt(dir.mux(eqid, chs.size(), &chs[0]) );
			return;
		}else{
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
			Tvalue tvidx = guard;
			dir.intClause(tvidx);
			vector<iVar> chs;
			for(int i=0; i<choices.size(); ++i){
				chs.push_back(dir.intClause(choices[i]));
			}
			node_ids[node.id].makeSuperInt(dir.mux(tvidx.getId(), chs.size(), &chs[0]) );
			return;
		}
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
    if (node.getOtype() == OutType::FLOAT) {
      mergeTvalues(guard, mid0, mid1, node_ids[node.id], node.flag, floats);
    } else {
      mergeTvalues(guard, mid0, mid1, node_ids[node.id], node.flag, ident);
    }
    }
}




void NodesToSolver::visit( ACTRL_node& node ){
	int size = node.nparents();
	auto it = node.p_begin();	
	bool parentSame = true;
	vector<int> ids(size);
	for(int i=0 ; it != node.p_end(); ++it, ++i){
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

template<class COMPARE_KEY>
void NodesToSolver::arrRTvalue(bool isBool, const Tvalue& index, const Tvalue& inarr, Tvalue& nvar, COMPARE_KEY c){
	
	const gvvec& idv = index.num_ranges;
	map<int, int, COMPARE_KEY> valToID(c);
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
		guardedVal gvtmp = idv[idxi];
		if(moreIdx && moreArr && cidx == gvtmp.value){
			while(inarriter != inarrend && inarriter->idx == cidx){
				int iatv = inarriter->value;
				//cout << "found index, val: " << cidx << "," << idv[idxi].guard << " " << iatv << "," << inarriter->guard << endl;
				map<int, int>::iterator it = valToID.find(iatv);
				if(it == valToID.end()){
					valToID[iatv] = dir.addAndClause(gvtmp.guard, inarriter->guard);
				}else{
					int cvar = dir.addAndClause(gvtmp.guard,inarriter->guard);
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
		if(!moreIdx || (moreArr && cidx < gvtmp.value)){ 
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
		if(!moreArr || (moreIdx && gvtmp.value < cidx)){
			//The index refers to an entry that doesn't exist in the array.
			//Need to produce the default value.
			for(gvvec::const_iterator it = begdef; it < enddef; ++it){
				int iatv = it->value;
				map<int, int>::iterator vit = valToID.find(iatv);
				if(vit == valToID.end()){
					valToID[iatv] = dir.addAndClause(gvtmp.guard, it->guard);
				}else{
					int cvar = dir.addAndClause(gvtmp.guard,it->guard);
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

void NodesToSolver::arrRead(bool_node& node, Tvalue& nvar, Tvalue& index, Tvalue& inarr) {
	if (inarr.isInt()) {
		if (!index.isInt()) {
			dir.intClause(index);
		}
		const gvvec& num_ranges = inarr.num_ranges;

		int sz = num_ranges[inarr.size() - 1].idx + 1;
		Range r = dir.getRange(index.getId());
		if (r.getHi() >= sz) {
			sz = r.getHi() + 1;
		}
		vector<iVar> inv(sz, num_ranges.begin()->guard);
		Assert(num_ranges.begin()->idx == -1, "qpouyp9u??");
		for (auto it = num_ranges.begin(); it != num_ranges.end(); ++it) {
			if (it->idx >= 0) {
				inv[it->idx] = it->guard;
			}
		}
		nvar.makeSuperInt(dir.mux(index.getId(), sz, &inv[0]));
	} else {
		Tvalue newidx;
		int idxid = index.getId();
		Range ir = dir.getRange(idxid);
		int last = -1;
		const gvvec& num_ranges = inarr.num_ranges;
		vector<int> bigor; bigor.push_back(0);
		if (ir.getLo()<0) {
			int ltid = dir.intlt(idxid, dir.getIntConst(0));
			newidx.num_ranges.push_back(guardedVal( ltid , -1));
			bigor.push_back(ltid);
		}
		for (auto it = num_ranges.begin(); it != num_ranges.end(); ++it) {
			if (it->idx > last) {
				if (ir.contains(it->idx)) {
					int eqid = dir.inteq(idxid, dir.getIntConst(it->idx));
					newidx.num_ranges.push_back(guardedVal(eqid, it->idx));
					bigor.push_back(eqid);
				}
				last = it->idx;
			}			
		}
		dir.addBigOrClause(&bigor[0], bigor.size()-1);
		newidx.num_ranges.push_back(guardedVal(-bigor[0], last + 1));		
		newidx.sparsify(dir);
		arrRTvalue(node.getOtype() == OutType::BOOL, newidx, inarr, nvar);
	}
}

void
NodesToSolver::visit( ARR_R_node &node){
	//cout << "NodesToSolver ARR_R " << node.lprint() << endl;
	Tvalue index = tval_lookup(node.mother());
	Tvalue inarr = tval_lookup(node.father());

	if (index.isInt() || inarr.isInt()) {
		arrRead(node, node_ids[node.id], index, inarr);
		return;
	}

	if(!index.isSparse()){
		index.makeSparse(dir, 1);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir, 1);
	}
  if (node.getOtype() == OutType::FLOAT) {
    arrRTvalue<FloatComp>(false, index, inarr, node_ids[node.id], FloatComp(floats));
  } else {
    arrRTvalue(node.getOtype() == OutType::BOOL, index, inarr, node_ids[node.id]);
  }
//	cout << "ARR_R(inarr,index,nvar)3: " << node.lprint() << endl << inarr << endl << index << endl << nvar << endl;
}

template<typename EVAL>
void NodesToSolver::arrWTvalue(const Tvalue& index, const Tvalue& inarr, const Tvalue& newval, Tvalue& nvar, EVAL eval){

	if(index.size()==1){ //Constant index.
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
	out.reserve(inarr.size() + newval.size());
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
	for(int i=0; i<=inarr.size(); ++i){
		bool donow = false;
		if(i==inarr.size() || inarr.num_ranges[i].idx != cindex){
			donow = true;
		}

		if(donow){
			while(idxstart >= 0 && idxstart < idxgv.size() && idxgv[idxstart].value < cindex){
				if(defend-defstart > 0){
					mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, defstart, defend, newval.num_ranges, 0, newval.size(), out, eval, false, idxgv[idxstart].value);
				}else{
					mergeTvalues(idxgv[idxstart].guard, tvdef.num_ranges, 0, 1, newval.num_ranges, 0, newval.size(), out, eval, false, idxgv[idxstart].value);
				}
				idxstart += idxincr;
			}
			if(idxstart >= 0 && idxstart < idxgv.size() && idxgv[idxstart].value == cindex){
				mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, lasti, i, newval.num_ranges, 0, newval.size(), out, eval, false, idxgv[idxstart].value);
				idxstart += idxincr;
			}else{
				gvvec::const_iterator tit = inarr.num_ranges.begin()+lasti;
				for(int tt=lasti; tt<i; ++tt){					
					out.push_back(*tit);
					++tit;
				}				
			}
			lasti = i;
			if(i!= inarr.size()){ cindex = inarr.num_ranges[i].idx; }
		}
	}
	while(idxstart >= 0 && idxstart < idxgv.size()){
		if(defend-defstart > 0){
			mergeTvalues(idxgv[idxstart].guard, inarr.num_ranges, defstart, defend, newval.num_ranges, 0, newval.size(), out, eval, false, idxgv[idxstart].value);
		}else{
			mergeTvalues(idxgv[idxstart].guard, tvdef.num_ranges, 0, 1, newval.num_ranges, 0, newval.size(), out, eval, false, idxgv[idxstart].value);
		}
		idxstart += idxincr;
	}
	nvar.arrayify();
}


void NodesToSolver::intArrW(Tvalue& index, Tvalue& newval, const Tvalue& inarr, Tvalue& nvar, ARR_W_node& node) {
	
	if (index.isInt()) {
		gvvec out;
		const gvvec& innr = inarr.num_ranges;
		Range ir = dir.getRange(index.getId());
		int rangeit = max(ir.getLo(), 0);
		int lastidx = -2;
		int deflt = -100;
		for (auto it = innr.begin(); it != innr.end(); ++it) {
			if (it->idx == -1) {
				deflt = it->guard;
			}
			while (it->idx > rangeit && rangeit <= ir.getHi() && lastidx < rangeit) {
				iVar c = dir.getIntConst(rangeit);
				iVar old = deflt;
				iVar nv = newval.getId();
				iVar ov[2] = { old, nv };
				out.push_back(guardedVal(dir.mux(dir.bitToI(dir.inteq(index.getId(), c)), 2, ov), 0, rangeit));
				++rangeit;
			}

			if (it->idx >= 0) {

				iVar c = dir.getIntConst(it->idx);

				iVar old = it->guard;
				iVar nv = newval.getId();
				iVar ov[2] = { old, nv };

				out.push_back(guardedVal(dir.mux(dir.bitToI(dir.inteq(index.getId(), c)), 2, ov), 0, it->idx));
				rangeit = max(rangeit, it->idx + 1);
				lastidx = it->idx;
			}
			else {
				out.push_back(guardedVal(it->guard, 0, it->idx));
				rangeit = max(rangeit, it->idx + 1);
				lastidx = it->idx;
			}
		}

		while (rangeit <= ir.getHi() && lastidx < rangeit) {
			iVar c = dir.getIntConst(rangeit);			
			iVar old = deflt;
			iVar nv = newval.getId();
			iVar ov[2] = { old, nv };
			out.push_back(guardedVal(dir.mux(dir.bitToI(dir.inteq(index.getId(), c)), 2, ov), 0, rangeit));
			++rangeit;
		}

		nvar.num_ranges = out;
		nvar.intarrayify();
	}
	else {
		const gvvec nr = index.num_ranges;
		dir.intClause(index);						
		if (nr.size() == 1) {
			const guardedVal& idxgv = nr[0];
			int lastIndex = inarr.num_ranges.size() - 1;
			const guardedVal& last = inarr.num_ranges[lastIndex];
			//This means we are appending in a currently empty entry. We can just push back newval 
			//at the end of inarr.
			if (last.idx <= idxgv.value - 1) {
				nvar.num_ranges = inarr.num_ranges;
				nvar.num_ranges.push_back(guardedVal( newval.getId() , -1, idxgv.value));			
				nvar.intarrayify();
				return;
				
			}
		}
		int inc = 1;
		int init = 0;
		int end = nr.size();
		if (nr.size() > 1 && nr[0].value > nr[1].value) {
			inc = -1;
			init = nr.size() - 1;
			end = -1;
		}
		int idx = init;
		int i = 0;
		int j = init;
		gvvec& out = nvar.num_ranges;
		out.clear();
		iVar defaultval;
		const gvvec& inarrnranges = inarr.num_ranges;

		while (true) {
			if (i == inarr.size() && j == end) {
				break;
			}
			if (i < inarr.size() && inarrnranges[i].idx < 0) {
				defaultval = inarrnranges[i].guard;
				out.push_back(guardedVal(inarrnranges[i].guard, -1, -1));
				++i;
				continue;
			}
			if (i == inarr.size() || (j != end && nr[j].value < inarrnranges[i].idx)) {
				if (nr[j].value >= 0) {
					iVar c = dir.getIntConst(nr[j].value);					
					iVar old = defaultval;
					iVar nv = newval.getId();
					iVar ov[2] = { old, nv };
					out.push_back(guardedVal(dir.mux(dir.bitToI(dir.inteq(index.getId(), c)), 2, ov), -1, nr[j].value));
				}
				j+= inc;
				continue;
			}
			if (j == end || nr[j].value > inarrnranges[i].idx) {
				out.push_back(guardedVal(inarrnranges[i].guard, -1, inarrnranges[i].idx));
				++i;
				continue;
			}
			if (nr[j].value == inarrnranges[i].idx) {
				iVar c = dir.getIntConst(nr[j].value);
				iVar old = inarrnranges[i].guard;
				iVar nv = newval.getId();
				iVar ov[2] = { old, nv };
				out.push_back(guardedVal(dir.mux(dir.bitToI(dir.inteq(index.getId(), c)), 2, ov), -1, nr[j].value));
				++i;
				j+=inc;
				continue;
			}				
		}
		nvar.intarrayify();
	}
}

void NodesToSolver::visit( ARR_W_node &node){	
	Tvalue index = tval_lookup(node.mother());		
	Tvalue inarr = tval_lookup(node.getOldArr());	
	Tvalue newval = tval_lookup(node.getNewVal());
	Tvalue& nvar = node_ids[node.id];

	if (index.isInt() || inarr.isInt() || newval.isInt()) {		

		if (!index.isSparse() && !index.isInt()) {
			index.makeSparse(dir);
		}
		if (!newval.isInt()) {
			if (!newval.isSparse()) {
				newval.makeSparse(dir, 1);
			}
			dir.intClause(newval);
		}
		if (!inarr.isInt()) {
			if (inarr.isBvect()) {
				inarr.makeSparse(dir, 1);
			}
			dir.intClause(inarr);
		}

		intArrW(index, newval, inarr, nvar, node);
		return;
	}


	if(!index.isSparse()){
		index.makeSparse(dir);
	}	
	if(!newval.isSparse()){
		newval.makeSparse(dir);
	}
	if(inarr.isBvect()){
		inarr.makeSparse(dir);
	}
  if (node.getOtype() == OutType::FLOAT || node.getOtype() == OutType::FLOAT_ARR) {
    arrWTvalue(index, inarr, newval, nvar, floats);
  } else {
    arrWTvalue(index, inarr, newval, nvar, ident);
  }
//	cout << "ARR_W(inarr,index,newval,nvar): " << node.lprint() << endl << inarr << endl << index << endl << newval << endl << nvar << endl;
}




void NodesToSolver::arrayConstruct(bool_node::parent_iter v_begin, bool_node::parent_iter v_end, Tvalue& nvar) {
	gvvec& tmp = nvar.num_ranges;
	tmp.clear();
	int i=0;
	for (auto it = v_begin; it != v_end; ++it, ++i) {
		Tvalue mval = tval_lookup(*it);
		if (!mval.isInt()) {
			dir.intClause(mval);
		}
		tmp.push_back(guardedVal(mval.getId(), -1, i));
	}
	nvar.arrayify();
	return;
}

void NodesToSolver::visit( ARR_CREATE_node &node){
	Tvalue& nvar = node_ids[node.id];
	gvvec& tmp = nvar.num_ranges;
	tmp.clear();
		

	const Tvalue& dflt = tval_lookup(node.getDfltval());
	
	if (!dflt.isSparse()) {
		Assert(dflt.isBvect(), "WTF");
		int flag = dflt.getId();
		tmp.push_back(guardedVal(YES, flag==YES? 1 : 0, -1));
	}
	else {
		tmp.push_back(guardedVal(YES, dflt.num_ranges[0].value, -1)); // default value lives in location -1;
	}

	auto it = node.arg_begin();
	for(int i=0 ; it != node.arg_end(); ++it, ++i){
		const Tvalue& mval = tval_lookup(*it);

		if (mval.isInt()) {

			arrayConstruct(node.arg_begin(), node.arg_end(), nvar);			
			return;
		}

		if(mval.isSparse()){
			for(int t=0; t<mval.size(); ++t){
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
	vector<char> mc(mval.size(), 'n');
	vector<char> fc(fval.size(), 'n');
	int flow = 0;
	int fhigh = fval.size();
	int finc = 1;
  
  for(int i=0; i<mval.size(); ++i){
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
		if(orTerms == mval.size() * fval.size()){
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
    const Tvalue tid = tval_lookup(node.mother());
    
    int length = tid.num_ranges.size();
	Tvalue zero = tvYES;
	zero.invert();
    vector<Tvalue> choices(tpl_store.size(), zero);
    
    bool isBoolean = true;
    bool isArray = false;
    bool isFloat = node.getOtype() == OutType::FLOAT;
  
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
          muxTValues(NULL, cond, binChoice, out, isBoolean, isArray, false, isFloat);
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
    
    muxTValues(NULL, tid, choices, node_ids[node.id], isBoolean, isArray, false, isFloat);
}

void NodesToSolver::visit (TUPLE_CREATE_node &node) {
    Tvalue& nvar = node_ids[node.id];
    vector<Tvalue>* new_vec = new vector<Tvalue>(node.nparents());
	auto it = node.p_begin();
	for (int i = 0; it != node.p_end(); ++it, ++i) {
		const Tvalue& mval = tval_lookup(*it);
		(*new_vec)[i] = mval;
	}    
    
	regTuple(new_vec, nvar);	
}

void NodesToSolver::regTuple(vector<Tvalue>* new_vec, Tvalue& nvar) {
	int id = tpl_store.size();
	stringstream str;
	for (auto nit = new_vec->begin(); nit != new_vec->end(); ++nit) {		
		str << *nit << "|";
	}
	string ts = str.str();
	int oldid = -1;
	if (tplcache.condAdd(ts.c_str(), ts.size(), id, oldid)) {
		delete new_vec;
		nvar.makeIntVal(YES, oldid);
		//cout << "saved " << ts << endl;
	}
	else {
		tpl_store.push_back(new_vec);
		nvar.makeIntVal(YES, id);
	}
}

void
NodesToSolver::visit (ASSERT_node &node)
{
	
	Tvalue fval = tval_lookup (node.mother());
	
	if(fval.isInt()){
		fval = dir.intToBit(fval.getId());
	}
	


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

	if (!dir.getMng().isOK()) {
		if (!dir.getMng().isNegated()) {
			cout << "  UNSATISFIABLE ASSERTION " << node.getMsg() << endl;
			errorMsg = "  UNSATISFIABLE ASSERTION ";
			errorMsg += node.getMsg();
		}
		stopAddingClauses = true;
		cout << "Problem became UNSAT." << node.lprint()<< endl;
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
	if (node.isFloat()) {
		node_ids[node.id].makeIntVal(YES, floats.getIdx(node.getFval()));
	}
	else {
		if (node.getVal() == 1 || node.getVal() == 0) {
			node_ids[node.id].makeBitVal(YES, node.getVal() == 1);
		}
		else {
			node_ids[node.id].makeIntVal(YES, node.getVal());
		}
	}	
	Dout (cout << "CONST " << node.get_name() << " " << node_ids[node.id]<< endl);
}


template<typename EVAL>
void NodesToSolver::doArrArrAcc(const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output, EVAL eval){
	//cout << "NodesToSolver.doArrArrAcc " << node.lprint() << endl;
	
	int N = choices.size();	
	for(int i=0; i < N; ++i){		
		if(choices[i].isBvect()){
			choices[i].makeSparse(dir);
		}		
	}

	bool isInt = choices[0].isInt();
	
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
				mergeTvalues(gval, gvl, pil, idxl, gvr, pir, idxr, out, eval, isInt, idxval);
				continue;
			}
			if(idxr>= gvrs ||  (idxl < gvls && gvr[idxr].idx > gvl[idxl].idx) ){
				int idxval = gvl[idxl].idx;
				int pil = idxl;
				while(idxl < gvls && gvl[idxl].idx == idxval){ ++idxl; }
				mergeTvalues(gval, gvl, pil, idxl, altR.num_ranges, 0, altR.num_ranges.size(), out, eval, isInt, idxval);
				continue;
			}
			if(idxl >= gvls || (idxr< gvrs && gvr[idxr].idx < gvl[idxl].idx) ){
				int idxval = gvr[idxr].idx;
				int pir = idxr;
				while(idxr < gvrs && gvr[idxr].idx == idxval){ ++idxr; }
				mergeTvalues(gval, altL.num_ranges, 0, altL.num_ranges.size(), gvr, pir, idxr, out, eval, isInt, idxval);
				continue;
			}
		}
		if (isInt) {
			output.intarrayify();
		}
		else {
			output.arrayify();
		}
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

template<class COMPARE_KEY, typename EVAL>
void NodesToSolver::doNonBoolArrAcc(const Tvalue& mval, vector<Tvalue>& choices, Tvalue& output, EVAL eval, COMPARE_KEY c){
	Dout( cout<<" non boolean array "<<endl );
	
	int N = choices.size();
	
	for(int i=0; i < N; ++i){		
		if(choices[i].isBvect()){
			choices[i].makeSparse (dir);
		}
	}
	
	if(mval.isSparse()){
		//mval.makeSparse (dir);
		map<int, vector<int>, COMPARE_KEY > newVals(c);
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
		mergeTvalues(mval.getId(), choices[0], choices[1], output, fl, eval);
	}
}



void NodesToSolver::process(BooleanDAG& bdag){
	int i=0;
		tmpdag = &bdag;
	stopAddingClauses = false;
	bool isNegated = dir.getMng().isNegated();
  // Preprocess synth ufun nodes to create tmp out variables
  for (BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it) {
    bool_node* node = *node_it;
    if (node->type == INTER_node::UFUN) {
      preprocessUfun((*dynamic_cast<UFUN_node*>(node)));
    }
  }
  
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
		try{
	//		if ((i>=2423808 && i<=2423808+1024) || i%1024 == 0) cout << "processing " << i << " " << (*node_it)->lprint() << endl;
		Dout(cout<<(*node_it)->get_name()<<":"<<(*node_it)->id<<endl);
		int tmpbufs = TOTBUFFERS;
		(*node_it)->accept(*this);
		
		//cout << (*node_it)->lprint() << "  ";
		//const Tvalue& tv = node_ids[(*node_it)->id];
  //    if ((*node_it)->getOtype() == OutType::FLOAT && !tv.isBvect()) {
		//  
  //    cout << " [ ";
  //    for (int i = 0; i < tv.getSize(); i++)
  //      cout << floats(tv.num_ranges[i].value) << ", ";
  //    cout << " ] "<<endl;
	 // }
	 // else {
		  /*if (tv.isSparse()) {
			  for (int i = 0; i < tv.getSize(); i++)
				  cout << tv.num_ranges[i].guard<<":"<<(tv.num_ranges[i].value) << ", ";
			  cout << " ] " << endl;	
		  }
		  else {
			  if (tv.isInt()) {
				  cout << "INT=" << tv.getId()<<endl;
			  }
			  else {
				  cout << "BIT=" << tv.getId()<<endl;
			  }
			  
		  }*/
	 // }
	  
	  //cout << endl;
      //		if(tv.getSize() > 20 && (*node_it)->getOtype() == bool_node::INT ) {cout<<(*node_it)->lprint()<<" -----> "<< tv.getSize()<<"  "<< tv <<endl;}
		}catch(BasicError& be){
			throw BasicError((*node_it)->get_name(), "ERROR WAS IN THE FOLLOWING NODE");      		
    		}
//		catch (exception e) {
//			cout << "exception" << endl;
//			cout << e.what() << endl;
//			throw e;
//		}
		if(stopAddingClauses){
			break;
		}
	}
}

