#include "NodesToSolver.h"



template<typename COMP>
void NodesToSolver::processComparissons(SATSolver& mng, varDir& dir,arith_node& node, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges, int YES){

	bool_node* mother = node.mother;			
	bool hasMother = (num_ranges.find(mother) != num_ranges.end() );
	bool motherUni = false;
	int mid = node_ids[mother] ;
	int mquant = node.mother_quant;
	if(!hasMother){
		if(mother != NULL){
			Assert( mquant == 1 || mquant == 0, "If mother is bool, the quant is either true or false");
			mid = mid*(mquant?1:-1);
			mquant = 1;
		}
		if( mid == YES){
			motherUni = true;
		}else{
			int cvar = dir.newAnonymousVar();
			int cvar2 = dir.newAnonymousVar();
			mng.addEqualsClause( cvar, -mid);
			mng.addEqualsClause( cvar2, mid);
			mid = cvar;
		}
	}
	vector<int>& mrange = hasMother? num_ranges[mother] : ( motherUni? unirange :tmprange);
	
	
	bool_node* father = node.father;			
	bool hasFather = (num_ranges.find(father) != num_ranges.end() );
	bool fatherUni = false;
	int fid = node_ids[father];
	int fquant = node.father_quant;
	if(!hasFather){
		if(father != NULL){
			Assert( fquant == 1 || fquant == 0, "If father is bool, the quant is either true or false");
			fid = fid*(fquant?1:-1);
			fquant = 1;
		}
		if( fid == YES){
			fatherUni = true;
		}else{
			int cvar = dir.newAnonymousVar();
			int cvar2 = dir.newAnonymousVar();
			mng.addEqualsClause( cvar, -fid);
			mng.addEqualsClause( cvar2, fid);
			fid = cvar;		
		}
	}	
	vector<int>& frange = hasFather? num_ranges[father] : (fatherUni? unirange: tmprange);

	
	int cvar = -YES;
	COMP comp;
	Dout(cout<<"SIZES = "<<mrange.size()<<", "<<frange.size()<<endl);
	int orTerms = 0;
	for(int i=0; i<mrange.size(); ++i){
		for(int j=0; j<frange.size(); ++j){
			Dout(cout<<"COMPARING "<<mquant*mrange[i]<<", "<<fquant*frange[j]<<endl);
			if(comp(mquant*mrange[i], fquant*frange[j])){
				if( mid + i == YES ) {
					cvar = fid + j;
					++orTerms;
					if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
					scratchpad[orTerms] = cvar;
				}else{
					if( fid + j == YES ){
						cvar = mid + i;
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;
					}else{
						cvar = dir.addAndClause(mid + i, fid + j);						
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;						
					}	
				}
			}	
		}
	}
	if( orTerms < 2 ){
		node_ids[&node] = cvar;
	}else{
		int result = dir.addBigOrClause( &scratchpad[0], orTerms);
		node_ids[&node] = result;
	}	
	return;	
}


template<typename THEOP>
inline int NodesToSolver::doArithExpr(SATSolver& mng, int quant1, int quant2, int id1, int id2, THEOP comp){
	return comp(quant1, quant2);
}



template<>
inline int NodesToSolver::doArithExpr<divides<int> >(SATSolver& mng, int quant1, int quant2, int id1, int id2, divides<int> comp){
	if(quant2 == 0){
		mng.assertVarClause(-id2);	
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}



template<>
inline int NodesToSolver::doArithExpr<modulus<int> >(SATSolver& mng, int quant1, int quant2, int id1, int id2, modulus<int> comp){
	if(quant2 == 0){
		mng.assertVarClause(-id2);
		return 0;
	}else{
		return comp(quant1, quant2);
	}
}


template<typename THEOP>
void NodesToSolver::processArith(SATSolver& mng, varDir& dir,arith_node& node, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){
	THEOP comp;
			if(node.father == NULL){
				//cout<<" IF "<<endl;
				bool hasRange = true;
				hasRange = (num_ranges.find(node.mother) != num_ranges.end() );
				vector<int>& nrange = hasRange? num_ranges[node.mother] : tmprange;
				int id = node_ids[node.mother];
				int mquant = node.mother_quant;
				if(!hasRange){
					int sgn = mquant? 1 : -1;
					int cvar = dir.newAnonymousVar();	
					int cvar2 = dir.newAnonymousVar();	
					mng.addEqualsClause( cvar, -sgn*id);
					mng.addEqualsClause( cvar2, sgn*id);
					id = cvar;
					mquant = 1;
				}
				num_ranges[&node].resize(nrange.size());
				vector<int>& tmp = num_ranges[&node];
				Dout(cout<<"ADDING "<<node.father_quant<<"  MULTIPLYIN  "<<mquant<<endl);
				for(int i=0; i<nrange.size(); ++i){
					//tmp[i] = comp(mquant*nrange[i], node.father_quant);
					tmp[i] = doArithExpr(mng, mquant*nrange[i], node.father_quant, id+i, id+i, comp);
					Dout(cout<<"  "<< mquant*nrange[i]<<" op "<<node.father_quant<<"= "<<tmp[i]<<endl);
				}
				node_ids[&node] = id;
			}else{
				//cout<<" THEN "<<endl;
				Assert( (num_ranges.find(node.mother) != num_ranges.end() ), "Mother doesn't have stuff");
				Assert( (num_ranges.find(node.father) != num_ranges.end() ), "Father doesn't have stuff");
				vector<int>& nrange = num_ranges[node.mother];
				vector<int>& frange = num_ranges[node.father];
				map<int, int> numbers;								
				int mid = node_ids[node.mother];			
				int fid = node_ids[node.father];
				num_ranges[&node].resize(0);
				vector<int>& tmp = num_ranges[&node];
				tmp.reserve(nrange.size()*frange.size());
				Dout(cout<<"ADDING "<<node.father_quant<<"  MULTIPLYIN  "<<node.mother_quant<<endl);
				Dout(cout<<"ADDING "<<node.father->get_name()<<"  WITH  "<<node.mother->get_name()<<endl);
				int vals = 0;
				//cout<<" BEFORE THE LOOPS"<<endl;
				for(int i=0; i<nrange.size(); ++i){
					for(int j=0; j<frange.size(); ++j){
						// int quant = comp(node.mother_quant*nrange[i], node.father_quant*frange[j]);						
						int quant = doArithExpr(mng, node.mother_quant*nrange[i], node.father_quant*frange[j], mid+i, fid+j, comp);
						Dout(cout<<quant<<" = "<<node.mother_quant*nrange[i]<<" * "<<node.father_quant*frange[j]<<endl);
						if(quant > INTEGERBOUND){ quant = INTEGERBOUND; }
						Dout(cout<<"QUANT = "<<quant<<"          "<<mid+i<<", "<<fid + j<<endl);
						if(numbers.find(quant) != numbers.end()){
							int cvar = dir.addAndClause(mid+i, fid + j);							
							int cvar2 = dir.addOrClause(cvar, numbers[quant]);
							numbers[quant] = cvar2;
						}else{
							int cvar = dir.addAndClause(mid+i, fid + j);
							tmp.push_back(quant);
							numbers[quant] = cvar;	
							++vals;
						}
						//cout<<" ENDLOOP "<<endl;
					}
				}
				Dout(cout<<"tmp size = "<<tmp.size()<<endl);
				Assert( vals > 0, "This should not happen here");
				int newID = dir.newAnonymousVar();
				for(int i=1; i<vals; ++i){ 
					int cvar = dir.newAnonymousVar();
					Assert( cvar == newID + i, "SolveFromInput: bad stuff");
				}
				for(int i=0; i<vals; ++i){
					int quant = tmp[i];
					Dout(cout<<"quant = "<<quant<<endl);
					mng.addEqualsClause(newID + i, numbers[quant]);
				}
				node_ids[&node] = newID;
			}
}



void NodesToSolver::visit( AND_node& node ){
			int fid = node_ids[node.father];
			int mid = node_ids[node.mother];
			if(!checkParentsChanged(mng, node, true)){ Dout( cout<<fid<<" AND "<<mid<<" unchanged"<<endl  ); return; }
			int fsign = node.father_sgn? 1 : -1;
			int msign = node.mother_sgn? 1 : -1;
			int nvar = dir.addAndClause(fsign*fid, msign*mid);
			int oldnvar = node_ids[&node];
			node.flag = nvar != oldnvar;
			node_ids[&node] = nvar;
			Dout(cout<<"AND "<<node.name<<"  "<<node_ids[&node]<<"  "<<&node<<endl);
			return;
		}


void NodesToSolver::visit( OR_node& node ){
			if(!checkParentsChanged(mng, node, true)){ Dout( cout<<"OR didn't change"<<endl  ); return; }
			int fsign = node.father_sgn? 1 : -1;
			int msign = node.mother_sgn? 1 : -1;
			int nvar = dir.addOrClause(fsign*node_ids[node.father], msign*node_ids[node.mother]);
			int oldnvar = node_ids[&node];
			node.flag = nvar != oldnvar;
			node_ids[&node] = nvar;
			Dout(cout<<"OR "<<node.name<<"  "<<node_ids[&node]<<"  "<<&node<<endl);
			return;
		}
void NodesToSolver::visit( XOR_node& node ){
			if(!checkParentsChanged(mng, node, true)){ Dout( cout<<"XOR didn't change"<<endl  ); return; }			
			int fsign = node.father_sgn? 1 : -1;
			int msign = node.mother_sgn? 1 : -1;
			Dout( cout<<" xor signs f=("<<node.father_sgn<<", "<<fsign<<")  m=("<<node.mother_sgn<<", "<<msign<<") "<<endl );
			int nvar = dir.addXorClause(fsign*node_ids[node.father], msign*node_ids[node.mother]);
			int oldnvar = node_ids[&node];
			node.flag = nvar != oldnvar;
			node_ids[&node] = nvar;
			Dout(cout<<"XOR "<<node.name<<"  "<<node_ids[&node]<<"  "<<&node<<endl);				
			return;
		}
void NodesToSolver::visit( SRC_node& node ){	
			int iid = node.ion_pos;
			if( node_values[(&node)] != 0){  
				Dout( cout<< dir.getArr(IN, iid)<<" has value "<<node_values[(&node)]<<"  "<<(&node)<<endl  ); 
				node_ids[&node] = node_values[(&node)]*YES;
				return; 
			}	
			node_ids[&node] = dir.getArr(IN, iid);
			Dout(cout<<"REGISTERING "<<node.name<<"  "<<node_ids[&node]<<"  "<<&node<<endl);
				return;
			}
void NodesToSolver::visit( DST_node& node ){				
			int oid = node.ion_pos;		
			int nvar = dir.getArr(outname, oid);
			int msign = node.mother_sgn? 1 : -1;		
			{	
				mng.addEqualsClause( nvar, msign*node_ids[node.mother]);
			}
			return;
}
void NodesToSolver::visit( PT_node& node ){	
				Assert(false, "The graph should not have this node");
				return;
}
void NodesToSolver::visit( CTRL_node& node ){	
			int iid = node.ion_pos;
			if( node_values[(&node)] != 0){  					
				node_ids[&node] = node_values[(&node)]*YES;
				Dout( cout<< dir.getArr(CTRL, iid)<<" has value "<<node_values[(&node)]<<"   "<< (&node) <<"    "<< node_ids[&node] <<endl  ); 
				return; 
			}	
			node_ids[&node] = dir.getArr(CTRL, iid);
			Dout(cout<<"CONTROL "<<node.name<<"  "<<node_ids[&node]<<"  "<<&node<<endl);
			return;
}



void NodesToSolver::visit( PLUS_node& node ){
		Dout( cout<<" PLUS "<<endl );			
		if(!checkParentsChanged(mng, node, true)){ return; }
		processArith<plus<int> >(mng, dir, node, node_ids, num_ranges);
		return;
}
void NodesToSolver::visit( TIMES_node& node ){
		Dout( cout<<" TIMES "<<endl );			
		if(!checkParentsChanged(mng, node, true)){ return; }
		processArith<multiplies<int> >(mng, dir, node, node_ids, num_ranges);
		return;
}
	void NodesToSolver::visit( ARRACC_node& node ){
			Dout(cout<<" ARRACC "<<endl);
			list<bool_node*>::iterator it = node.multi_mother.begin();
			list<int>::iterator signs = node.multi_mother_sgn.begin();
			vector<int> choices(node.multi_mother.size());
			bool parentSame = true;
			bool isBoolean=true;
			for(int i=0; it != node.multi_mother.end(); ++i, ++it, ++signs){	
				
				Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  signs = "<<*signs<<"  ");
				if( (*signs)>1 || (*signs)<0 || (num_ranges.find(*it) != num_ranges.end())){
					isBoolean = false;	
				}
				{
					choices[i]=((*signs)==1?1:-1)*node_ids[*it];
				}
				Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}			
			if(!checkParentsChanged(mng, node, parentSame)){ return; }
			if(!isBoolean){
				doNonBoolArrAcc(mng, dir, node, node_ids, 	num_ranges);
				return;	
			}
			Dout(cout<<" is boolean"<<endl);
			bool_node* mother = node.mother;
			int id = node_ids[mother];
			Dout(cout<<" mother = "<<id<<"  signs = "<<node.mother_sgn<<"  "<<endl);
			Assert( mother != NULL, "This should never happen");
			if( (num_ranges.find(mother) == num_ranges.end()) ){ //mother->type != bool_node::ARITH
				int sgn = node.mother_sgn? 1: -1;
				int cvar;
				if(choices.size()>=2){
					Dout( cout<<" replacing with choice "<<sgn * id<<", "<<choices[1]<<", "<<choices[0]<<endl );
					cvar = dir.addChoiceClause(sgn * id , choices[1], choices[0]);
				}else{
					if(choices.size()>=1){
						cvar = dir.addAndClause( sgn * -id , choices[0]);
					}else{
						cvar = -YES;
					}
				}
				node_ids[&node] = cvar;
			Dout(cout<<"ARRACC "<<node.name<<"  "<<node_ids[&node]<<"   "<<&node<<endl);	
				return;
			}
			vector<int>& nrange = num_ranges[mother];
			int cvar = -YES;
			int orTerms = 0;
			for(int i=0; i<nrange.size(); ++i){
				if( nrange[i] >= 0 && nrange[i] < choices.size() ){
					if( id+i == YES){
						cvar = choices[nrange[i]];
						++orTerms;
						if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
						scratchpad[orTerms] = cvar;
					}else{
						if( id+i != -YES ){
							cvar = dir.addAndClause( choices[nrange[i]], id + i);
							++orTerms;
							if(orTerms>=scratchpad.size()){ scratchpad.resize(scratchpad.size()*2); }
							scratchpad[orTerms] = cvar;
						}
					}
				}
			}
			if( orTerms < 2){
				node_ids[&node] = cvar;
			}else{
				int result = dir.addBigOrClause( &scratchpad[0], orTerms);			
				node_ids[&node] = result;
			}
			Dout(cout<<"ARRACC "<<node.name<<"  "<<node_ids[&node]<<"   "<<&node<<endl);	
			return;
		}
	void NodesToSolver::visit( DIV_node& node ){
			Dout( cout<<" DIV "<<endl );			
			if(!checkParentsChanged(mng, node, true)){ return; }
			processArith<divides<int> >(mng, dir, node, node_ids, num_ranges);
			return;
		}
	void NodesToSolver::visit( MOD_node& node ){
			Dout( cout<<" MOD "<<endl );			
			if(!checkParentsChanged(mng, node, true)){ return; }
			processArith<modulus<int> >(mng, dir, node, node_ids, num_ranges);
			return;
		}
	void NodesToSolver::visit( GT_node& node ){
			Dout( cout<<" GT "<<endl );
			if(!checkParentsChanged(mng, node, true)){ return; }	
			processComparissons<greater<int> >(mng, dir, node, node_ids, num_ranges, YES);
			return;
		}
	void NodesToSolver::visit( GE_node& node ){
			Dout( cout<<" GE "<<endl );
			if(!checkParentsChanged(mng, node, true)){ return; }	
			processComparissons<greater_equal<int> >(mng, dir, node, node_ids, num_ranges, YES);
			return;
		}
	void NodesToSolver::visit( LT_node& node ){
			Dout( cout<<" LT "<<endl );
			if(!checkParentsChanged(mng, node, true)){ return; }	
			processComparissons<less<int> >(mng, dir, node, node_ids, num_ranges, YES);
			return;
		}
	void NodesToSolver::visit( LE_node& node ){
			Dout( cout<<" LE "<<endl );			
			if(!checkParentsChanged(mng, node, true)){ return; }	
			processComparissons<less_equal<int> >(mng, dir, node, node_ids, num_ranges, YES);
			return;
		}

	void NodesToSolver::visit( ARRASS_node& node ){
			Dout(cout<<"             ARRASS:"<<endl);
			// mother = index
			// multi-mother[0] = old-value;
			// multi-mother[1] = new-value;
			// if( mother == quant ) return multi-mother[1]; else return multi-mother[0];
			bool_node* mother = node.mother;
			int id = node_ids[mother];
			int quant = node.mother_quant;
			Dout(cout<<" mother = "<<((mother != NULL)?mother->get_name():"NULL")<<"  mid = "<<id<<"  mquant = "<<quant<<endl);
			list<bool_node*>::iterator it = node.multi_mother.begin();
			list<int>::iterator signs = node.multi_mother_sgn.begin();
			Assert( node.multi_mother.size() == 2 , "THIS SHOULDN't HAPPEN");
			vector<int> choices(2);
			vector<bool_node*> mothers(2);
			vector<int> factors(2);
			bool parentSame = true;
			bool isBoolean=true;
			for(int i=0; it != node.multi_mother.end(); ++i, ++it, ++signs){	
				
				if( (*signs)>1 || (*signs)<0 || (num_ranges.find(*it) != num_ranges.end())){
					isBoolean = false;	
				}
				mothers[i] = *it;
				factors[i] = *signs;
				Dout(cout<<" parent = "<<((*it != NULL)?(*it)->get_name():"NULL")<<"  factor="<<*signs<<"  ");
				{
					choices[i]=node_ids[*it];
				}
				Dout(cout<<"choice "<<i<<" = "<<choices[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}			
			if(!checkParentsChanged(mng, node, parentSame)){ return; }			
			int guard;						
			if( (num_ranges.find(mother) == num_ranges.end()) ){
				int sgn = node.mother_sgn? 1: -1;
				if(quant > 1){
					guard = -YES;
				}else{
					int tmp = sgn * id;
					Dout(cout<<" sgn = "<<sgn<<"  id="<<id<<"  tmp="<<tmp<<endl);
					guard = dir.addXorClause(tmp,  quant==0?YES:-YES);
				}
			}else{
				guard = -YES;
				vector<int>& nrange = num_ranges[mother];
				for(int i=0; i<nrange.size(); ++i){
					if( nrange[i] == quant){
						guard = id + i;						
						break;						
					}					
				}
			}
			Dout(cout<<" guard = "<<guard<<endl);
			int factor0 = factors[0];
			int factor1 = factors[1];
			if(isBoolean){
				Dout(cout<<" is boolean"<<endl);
				if(guard == YES){
					node_ids[&node] = choices[1]*(factor1==1?1:-1);
					return;
				}
				if(guard == -YES){
					node_ids[&node] = choices[0]*(factor0==1?1:-1);
					return;	
				}
				int cvar = dir.addChoiceClause(guard , choices[1]*(factor1==1?1:-1), choices[0]*(factor0==1?1:-1));
				node_ids[&node] = cvar;
				return;
			}else{
				Dout(cout<<" is not boolean"<<endl);
				int mid0 = choices[0];				
				int mid1 = choices[1];
				bool hasRange ;
				hasRange = (num_ranges.find(mothers[1]) != num_ranges.end());
				vector<int>& nr1 = hasRange? num_ranges[mothers[1]] : tmprange;				
				if(!hasRange){
					int cvar = dir.newAnonymousVar();
					int cvar2 = dir.newAnonymousVar();
					mng.addEqualsClause( cvar, -mid1);
					mng.addEqualsClause( cvar2, mid1);
					mid1 = cvar;
				}
				hasRange = (num_ranges.find(mothers[0]) != num_ranges.end());
				vector<int>& nr0 = hasRange? num_ranges[mothers[0]] : tmprange;
				if(!hasRange){
					int cvar = dir.newAnonymousVar();
					int cvar2 = dir.newAnonymousVar();
					mng.addEqualsClause( cvar, -mid0);
					mng.addEqualsClause( cvar2, mid0);
					mid0 = cvar;
				}
				if(guard == YES){
					node_ids[&node] = mid1;
					num_ranges[&node] = nr1;
					vector<int>& tmp = num_ranges[&node];
					Dout( cout<<"var "<< choices[1] <<"  val = ");
					for(int i=0; i<tmp.size(); ++i){ tmp[i] = tmp[i] * factor1; Dout( cout<<tmp[i]<<", ");}
					Dout(cout<<endl);
					return;
				}
				if(guard == -YES){
					node_ids[&node] = mid0;
					num_ranges[&node] = nr0;
					vector<int>& tmp = num_ranges[&node];
					for(int i=0; i<tmp.size(); ++i){ tmp[i] = tmp[i] * factor0; }
					return;
				}
				int i=0, j=0;
				vector<int> res;
				res.reserve(nr0.size() + nr1.size());
				vector<int>& out = num_ranges[&node];
				out.reserve(nr0.size() + nr1.size());
				while(i < nr0.size() || j< nr1.size()){
					bool avi = i < nr0.size();
					bool avj = j < nr1.size();
					int curri = avi ? nr0[i] * factor0 : -1;
					int currj = avj ? nr1[j] * factor1 : -1;
					if( curri == currj && avi && avj){
						Dout(cout<<" curri = "<<curri<<" currj = "<<currj<<endl);
						int cvar1 = dir.addAndClause( mid0+i, -guard);
						int cvar2 = dir.addAndClause( mid1+j, guard);
						int cvar3 = dir.addOrClause( cvar2, cvar1);
						out.push_back(curri);
						res.push_back(cvar3);
						i++;
						j++;
						continue;
					}
					if((curri < currj && avi) || !avj){
						Dout(cout<<" curri = "<<curri<<endl);
						int cvar = dir.addAndClause( mid0+i, -guard);
						out.push_back(curri);
						res.push_back(cvar);
						i++;
						continue;
					}
					if( (currj < curri && avj) || !avi ){
						Dout(cout<<" currj = "<<currj<<endl);
						int cvar = dir.addAndClause( mid1+j, guard );
						out.push_back(currj);
						res.push_back(cvar);
						j++;
						continue;
					}
					Assert(false, "Should never get here");
				}
				out.resize(res.size());
				Assert( res.size() > 0, "This should not happen here2");
				int newID = dir.newAnonymousVar();
				for(int k=1; k<res.size(); ++k){
					int cvar = dir.newAnonymousVar();
					Assert( cvar == newID + k, "SolveFromInput: cvar != newID + k");
				}
				for(int k=0; k<res.size(); ++k){
					int val = res[k];
					mng.addEqualsClause( newID+k, val);
				}
				node_ids[&node] = newID;
				return;
			}
		}
	void NodesToSolver::visit( ACTRL_node& node ){
			int size = node.multi_mother.size();
			list<bool_node*>::iterator it = node.multi_mother.begin();
			list<int>::iterator signs = node.multi_mother_sgn.begin();
			bool parentSame = true;
			vector<int> ids(node.multi_mother.size());
			for(int i=0 ; it != node.multi_mother.end(); ++it, ++i, ++signs){
				{
					Dout( cout<<" ACTRL "<<*it<<" nodeids = "<<node_ids[*it]<<"  signs = "<<(*signs));
					ids[i]=((*signs)==1?1:-1)*node_ids[*it];
				}
				Dout( cout<<"   ids[i]="<<ids[i]<<endl);
				parentSame = parentSame && ( (*it)== NULL || !(*it)->flag );
			}
			if(!checkParentsChanged(mng, node, parentSame)){Dout(cout<<"@ACTRL "<<node.name<<"  "<<node_ids[&node]<<"  "<<num_ranges[&node].size()<<"   "<<&node<<endl);	 return; }
			vector<int>& tmp = num_ranges[&node];
			varRange vr = getSwitchVars(mng,dir, ids, size, tmp, YES);			
			node_ids[&node] = vr.varID;
			Dout(cout<<"&ACTRL "<<node.name<<"  "<<node_ids[&node]<<"  "<<tmp.size()<<"   "<<&node<<endl);	
			return;
		}



void NodesToSolver::doNonBoolArrAcc(SATSolver& mng, varDir& dir,arith_node& node, 	map<bool_node*, int>& node_ids, 	map<bool_node*, vector<int> >& num_ranges){	
	Dout( cout<<" non boolean array "<<endl );
	list<bool_node*>::iterator it = node.multi_mother.begin();
	list<int>::iterator signs = node.multi_mother_sgn.begin();
	int N = node.multi_mother.size();
	vector<int> choices(N);
	vector<int> factors(N);
	vector<vector<int>* > values(N);

	for(int i=0; i < N; ++i, ++it, ++signs){	
		
		choices[i] = node_ids[*it];
		factors[i] = (*signs);
		bool hasRanges = true;
		if( num_ranges.find(*it) == num_ranges.end()){
			hasRanges = false;
			if(*it != NULL){
				Assert( factors[i] == 1 || factors[i] ==0, "Wait, this is pretty bad");	
				choices[i] = choices[i] * (factors[i]? 1:-1);
				factors[i] = 1;
				int cvar = dir.newAnonymousVar();
				int cvar2 = dir.newAnonymousVar();
				mng.addEqualsClause( cvar, -choices[i]);
				mng.addEqualsClause( cvar2, choices[i]);
				choices[i] = cvar;					
				values[i] = &tmprange;
				Dout( cout<<" creating new vec with vars "<< cvar << "  " << cvar2 << endl );
			}else{
				Assert( choices[i] == YES , "This better be true, or else ...");
				values[i] = &unirange;
			}			
		}else{
			values[i] = &num_ranges[*it];
		}
		
	}		
			
	bool_node* mother = node.mother;
	Assert( mother != NULL, "This case should be handled by the partial evaluator");
	int id = node_ids[mother];
	bool isMulti=true;
	if( num_ranges.find(mother) == num_ranges.end() ){			
		int sgn = node.mother_sgn? 1: -1;
		isMulti = false;
		int cvar = dir.newAnonymousVar();
		int cvar2 = dir.newAnonymousVar();
		mng.addEqualsClause( cvar, sgn*-id);
		mng.addEqualsClause( cvar2, sgn*id);
		id = cvar;
	}
	vector<int>& nrange = isMulti ? num_ranges[mother] : tmprange ;
	map<int, vector<int> > newVals;
	int vsize = values.size();
	for(int i=0; i<nrange.size(); ++i){
		int factor = factors[ nrange[i] ];
		if( nrange[i] < vsize && nrange[i] >= 0){
			Assert( values[nrange[i]] != NULL , "This can't happen either");
			vector<int>& cvalues = *values[nrange[i]];
			Dout( cout<<" x=nrange["<<i<<"]="<<nrange[i]<<"  factors[x]="<<factor<<"    "<<cvalues.size()<<endl );
			for(int j=0; j<cvalues.size(); ++j){
				if( (id + i) == YES ){
					newVals[ cvalues[j] * factor].push_back(choices[nrange[i]] + j);
				}else{
					int tmpid = choices[nrange[i]] + j;
					if( tmpid == YES ){
						newVals[ cvalues[j] * factor].push_back(id+i);
					}else{
						int cvar = dir.addAndClause( id + i, choices[nrange[i]] + j);
						newVals[ cvalues[j] * factor].push_back(cvar);
						Dout( cout<<" cvalues["<<j<<"] = "<<cvalues[j]<<"    x factor="<<cvalues[j] * factor<<endl );
					}
				}
			}
		}
	}
	
	vector<int>& result = num_ranges[&node];
	result.clear();
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
			node_ids[&node] = vars[0];
			result.push_back(it->first);
		}else{
			int cvar = dir.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
			node_ids[&node] = cvar;
		}		
	}else{
		if(newVals.size() == 0){
			node_ids[&node] = -YES;
			result.push_back(0);
		}
		//Assert( newVals.size() > 0, "This should not happen here3");
		int newID = dir.newAnonymousVar();
		node_ids[&node] = newID;
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
			mng.addBigOrClause( &scratchpad[0], orTerms);
			result.push_back(it->first);
		}
	}
}


bool NodesToSolver::checkParentsChanged(SATSolver& mng, bool_node& node, bool more){
	if(( node.father== NULL || !node.father->flag ) &&
			( node.mother== NULL || !node.mother->flag )&&
			more
			){ node.flag =false; return false || mng.ignoreOld(); }else{ node.flag = true; return true;}
}

