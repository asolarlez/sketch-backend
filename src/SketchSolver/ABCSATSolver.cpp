
#include "ABCSATSolver.h"
#include "timerclass.h"
#include "fraig.h"

extern "C" {
extern int  Abc_NtkRewrite( Abc_Ntk_t * pNtk, int fUpdateLevel, int fUseZeros, int fVerbose );
extern int  Abc_NtkRefactor( Abc_Ntk_t * pNtk, int nNodeSizeMax, int nConeSizeMax, bool fUpdateLevel, bool fUseZeros, bool fUseDcs, bool fVerbose );
}

Abc_Ntk_t * SATSolver::cofactor(Abc_Ntk_t * network, vector<int>& namemap){
	Abc_Ntk_t * result;
    timerclass timer("Cofactor time 1");
     // apply structural hashing to logic network and get an AIG
	timer.start();
	Vec_Int_t vec;
	int nsize = Abc_NtkPiNum(network);
	vector<int> vals(nsize, -1);
	vec.nCap = vals.size();
	vec.nSize = nsize;
	vec.pArray = &vals[0];
	{
   		int i;
       Abc_Obj_t * pNode;	   		
   		Abc_NtkForEachPi( network, pNode, i ){
			if( results.find(namemap[i]) != results.end() ){
				Dout( cout<<" setting value ["<<namemap[i]<<"] = "<<results[namemap[i]]<<endl);
				vals[i] = results[namemap[i]];
				
			}else{
				Dout( cout<<" setting value b ["<<namemap[i]<<"] = "<<vals[i]<<endl);
			}
   		}
	}
	result = Abc_NtkMiterCofactor(network, &vec);
   	//Abc_NtkDelete( network );	
    timer.stop().print();
    return result;
}

void SATSolver::closeMiter(Abc_Ntk_t * network){
   pOutputNode->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) network->pManFunc, out_cnt, NULL );
   Abc_ObjAddFanin( Abc_NtkCreatePo(network), pOutputNode);              
  char Buffer[100];
  int i;
  Abc_Obj_t * pObj;
   Abc_NtkForEachPo( network, pObj, i ){
   	   sprintf( Buffer, "[%d]", pObj->Id );
       Abc_NtkLogicStoreName( pObj, Buffer );
   }
   
   assert( Abc_NtkLatchNum(network) == 0 );
   assert( Abc_NtkPoNum(network) == 1 );	
}





int SATSolver::solve(){

       //////////
       // add names to the PIs/POs
       Abc_Ntk_t * pAig;
       Abc_Obj_t * pObj;
       int i;
       int RetValue;
       vector<int> namemap(Abc_NtkPiNum(pNtk));
       
       closeMiter(pNtk);
   
	{
	   	Abc_Obj_t * pNode;	   		
		Abc_NtkForEachPi( pNtk, pNode, i ){
			namemap[i] = pNode->Id;
		}
	}       
       
       
     
     
     { 
       timerclass timer("Check time");  
       // make sure everything is okay with the network structure       
       timer.start();
       if ( !Abc_NtkDoCheck( pNtk ) )
       {
           printf( "ABC_Check_Integrity: The internal network check has failed.\n" );
           return UNDETERMINED;
       }
       timer.stop().print();
     }
     
     {
     	 timerclass timer("Strash time 1");
       // apply structural hashing to logic network and get an AIG
        timer.start();
       	pAig = Abc_NtkStrash( pNtk, 0, 1 );
       	Abc_NtkDelete( pNtk );
	   	pNtk = pAig;
        timer.stop().print();
     }


	Abc_NtkPrintStats(stdout, pNtk, 0);
	pNtk = cofactor(pNtk, namemap);


		Dout( cout<<" There are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);
		if(oldpNtk != NULL){
			 timerclass timer("And time 1");
             timer.start();
			pAig = Abc_NtkMiterAnd(pNtk , oldpNtk);
			timer.stop().print();
			Abc_NtkDelete( pNtk );
			cout<<"After del "; timer.stop().print();
		   	pNtk = pAig;
			Abc_NtkPrintStats(stdout, pNtk, 0);			
		}
       	
       
       // at this point, we assume the ABC network is okay

       // perform some synthesis on the AIG
       // This doesn't compile pAig = Abc_NtkRewrite( pAig ); // the same network is returned

       // call brute-force SAT (MiniSat-1.14)
       // have a look at src\base\abci\abcProve.c
       {
		 timerclass timer("SAT time total");
         timer.start();
       Prove_Params_t Params, * pParams = &Params;            
        Prove_ParamsSetDefault( pParams );
	    pParams->nItersMax = 5;
	    pParams->fUseBdds = 0;
	    RetValue = Abc_NtkMiterProve( &pNtk, pParams );                          
        
		timer.stop().print();
       }
       if ( RetValue == 0 )
       {
           // satisfiable and counter-example is in pNtk->pModel, which gives 0/1 for each PI
           if (pNtk->pModel == NULL ){
           		cout<<"The problem was solved through rewriting"<<endl;
		        pNtk->pModel = ALLOC( int, Abc_NtkCiNum(pNtk) );
		        memset( pNtk->pModel, 0, sizeof(int) * Abc_NtkCiNum(pNtk) );
		   }
           
           Abc_Obj_t * pNode;
           Abc_NtkForEachPi( pNtk, pNode, i ){
				results[namemap[i]] = pNtk->pModel[i];
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
       
       