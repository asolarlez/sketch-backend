
#include "ABCSATSolver.h"

int SATSolver::solve(){
		Dout( cout<<" Entering solve"<<endl );
		Dout( cout<<" There are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);		
	   pOutputNode->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) pNtk->pManFunc, out_cnt, NULL );
       Abc_ObjAddFanin( Abc_NtkCreatePo(pNtk), pOutputNode);              
          		Dout( cout<<" Added fanin"<<endl );
       //////////
       // add names to the PIs/POs
       Abc_Ntk_t * pAig;
       Abc_Obj_t * pObj;
       int i;
       int RetValue;
       vector<int> namemap(Abc_NtkPiNum(pNtk));
       
       {
       	Abc_Obj_t * pNode;	   		
		   		Abc_NtkForEachPi( pNtk, pNode, i ){
		   			namemap[i] = pNode->Id;
		   			cout<<" name[i] = "<<pNode->Id<<endl;
		   		}
       }
       
       
       

		Dout( cout<<" Before Logic Store Name for Pi"<<endl );
		Dout( cout<<" There are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);
		Dout( cout<<" Getting the zeroth pi didn't crash "<<endl);
       Abc_NtkForEachPi( pNtk, pObj, i ){
           Abc_NtkLogicStoreName( pObj, Abc_ObjName(pObj) );
       }
		Dout( cout<<" Before Logic Store Name for Po"<<endl );
       Abc_NtkForEachPo( pNtk, pObj, i )
           Abc_NtkLogicStoreName( pObj, Abc_ObjName(pObj) );
       assert( Abc_NtkLatchNum(pNtk) == 0 );
       assert( Abc_NtkPoNum(pNtk) == 1 );
       
       // make sure everything is okay with the network structure       
       if ( !Abc_NtkDoCheck( pNtk ) )
       {
           printf( "ABC_Check_Integrity: The internal network check has failed.\n" );
           return UNDETERMINED;
       }
       // apply structural hashing to logic network and get an AIG
  		Dout( cout<<" Before Strash"<<endl );
       	pAig = Abc_NtkStrash( pNtk, 0, 1 );
       	Abc_NtkDelete( pNtk );
	   	pNtk = pAig;


   		Dout( cout<<" Did another check."<<endl );
   	   {
	   		Vec_Int_t vec;
	   		int nsize = Abc_NtkPiNum(pNtk);
	   		vector<int> vals(nsize, -1);
	   		vec.nCap = vals.size();
			vec.nSize = nsize;
			vec.pArray = &vals[0];
	   		{
		   		int i;
	           Abc_Obj_t * pNode;	   		
		   		Abc_NtkForEachPi( pNtk, pNode, i ){
					if( results.find(namemap[i]) != results.end() ){
						Dout( cout<<" setting value ["<<namemap[i]<<"] = "<<results[namemap[i]]<<endl);
						vals[i] = results[namemap[i]];
						
					}else{
						Dout( cout<<" setting value b ["<<namemap[i]<<"] = "<<vals[i]<<endl);
					}
		   		}
	   		}
	   		Dout( cout<<" Done Setting vals"<<endl );
	   		pNtk = Abc_NtkMiterCofactor(pNtk, &vec);
	   		Dout( cout<<" Done cofactor fanin"<<endl );
   		}
		Dout( cout<<" There are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);
		if(oldpNtk != NULL){
//			if ( !Abc_NtkDoCheck( oldpNtk ) ){
//			    printf( "ABC_Check_Integrity: The internal network check has failed.\n" );
//			    return UNDETERMINED;
//			}
			Dout( cout<<" Anding with old miter"<<endl );
			Dout( cout<<" Old miter has "<<Abc_NtkPiNum(oldpNtk)<<" Pis"<<endl);
			
			pAig = Abc_NtkMiterAnd(pNtk , oldpNtk);
			Abc_NtkDelete( pNtk );
		   	pNtk = pAig;
			pAig = Abc_NtkStrash( pNtk, 0, 1 );
	       	Abc_NtkDelete( pNtk );
		   	pNtk = pAig;
		}
       	
       
       // at this point, we assume the ABC network is okay

       // perform some synthesis on the AIG
       // This doesn't compile pAig = Abc_NtkRewrite( pAig ); // the same network is returned

       // call brute-force SAT (MiniSat-1.14)
       // have a look at src\base\abci\abcProve.c
   		Dout( cout<<" Before Sat"<<endl );
       RetValue = Abc_NtkMiterSat( pAig, 1000000, 0, 0, 0 );
       if ( RetValue == 0 )
       {
           // satisfiable and counter-example is in pAig->pModel, which gives 0/1 for each PI
           Abc_Obj_t * pNode;
           Abc_NtkForEachPi( pAig, pNode, i ){
           		cout<<" produced result ["<<namemap[i]<<"] = "<<pAig->pModel[i]<<endl;
				results[namemap[i]] = pAig->pModel[i];
           }
			return SATISFIABLE;
       }
       else if ( RetValue == 1 )
       {
           return UNSATISFIABLE; // unsat
       }
       else // it is undecided
       {
       		return UNDETERMINED;
       }

   }
       
       