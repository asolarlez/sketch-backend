
#include "ABCSATSolver.h"
#include "timerclass.h"
#include "fraig.h"
#include <sstream>

extern "C" {
extern int  Abc_NtkRewrite( Abc_Ntk_t * pNtk, int fUpdateLevel, int fUseZeros, int fVerbose );
extern int  Abc_NtkRefactor( Abc_Ntk_t * pNtk, int nNodeSizeMax, int nConeSizeMax, bool fUpdateLevel, bool fUseZeros, bool fUseDcs, bool fVerbose );
}

Abc_Ntk_t * ABCSATSolver::cofactor(Abc_Ntk_t * network, vector<int>& namemap){
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

void ABCSATSolver::closeMiter(Abc_Ntk_t * network){
	Assert( Abc_ObjFaninNum(pOutputNode) == out_cnt, "This should never happen!");
	if(  solveNegation ){
		pOutputNode->pData = Abc_SopCreateNand( (Extra_MmFlex_t *) network->pManFunc, out_cnt );
		if( pSuperOutputNode != NULL ){			
			Abc_ObjAddFanin( pSuperOutputNode, pOutputNode );
			Dout(cout<<" Super Fanin = "<<Abc_ObjFaninNum(pSuperOutputNode)<<endl );
			pSuperOutputNode->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) network->pManFunc, Abc_ObjFaninNum(pSuperOutputNode), NULL );
			pOutputNode = pSuperOutputNode;
		}
	}else{
		Assert( pSuperOutputNode == NULL, "This shouldn't happen. This is an invariant");
		pOutputNode->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) network->pManFunc, out_cnt, NULL );		
	}
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


void ABCSATSolver::outputToFile(const string& fname){	
	Abc_Ntk_t * pNetlist;
    pNetlist = Abc_NtkLogicToNetlist(pNtk,0);
    const char* nm = fname.c_str();
    Io_WriteBlifNetlist( pNetlist, (char*) nm , 1 );
    Abc_NtkDelete( pNetlist );		
}


void ABCSATSolver::completeProblemSetup(){
	
	closeMiter(pNtk);
}



int ABCSATSolver::solve(){		
       //////////
       // add names to the PIs/POs
       Abc_Ntk_t * pAig;
       Abc_Obj_t * pObj;
       int i;
       int RetValue;
       vector<int> namemap(Abc_NtkPiNum(pNtk));
       
       completeProblemSetup();
   
	{
	   	Abc_Obj_t * pNode;	   		
		Abc_NtkForEachPi( pNtk, pNode, i ){
			namemap[i] = pNode->Id;
		}
	}       
       
       
     
     
     if(doCheck){ 
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
        timer.stop(); //.print();
     }


	//Abc_NtkPrintStats(stdout, pNtk, 0);
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
			//Abc_NtkPrintStats(stdout, pNtk, 0);			
		}
       	
       
       // at this point, we assume the ABC network is okay

       // perform some synthesis on the AIG
       // This doesn't compile pAig = Abc_NtkRewrite( pAig ); // the same network is returned
		
		if( outputAIG ){
			stringstream str;
			str<<name<<"_";
			str<<solvcnt<<".blif";			
			outputToFile(str.str());	
		}
		
		
		++solvcnt;
		
		
       // call brute-force SAT (MiniSat-1.14)
       // have a look at src\base\abci\abcProve.c
       {
		 timerclass timer("SAT time total");
         timer.start();
         if(strategy == FULL){
         	cout<<"FULL"<<endl;
			Prove_Params_t Params, * pParams = &Params;
         	Prove_ParamsSetDefault( pParams );
			pParams->nItersMax = 5;
    	    RetValue = Abc_NtkMiterProve( &pNtk, pParams );                          
         }else if(strategy == BASICSAT){
         	cout<<"BASIC"<<endl;
         	RetValue = Abc_NtkMiterSat( pNtk, 100000000, 0, 0, 0, NULL, NULL );
         }
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
       
       
       
         int ABCSATSolver::getVarVal(int id){
        		Dout( cout<<"var val for ["<<id<<"] = "<<results[id]<<endl ) ;
               return results[id];
        }

         int ABCSATSolver::newVar(){
	        //////////////
	        Abc_Obj_t * pNode;
	        pNode = Abc_NtkCreateNode( pNtk );
	        return pNode->Id;
	        //////////////
        }
		 bool ABCSATSolver::ignoreOld(){
		 	return true;	
		}
		  int ABCSATSolver::newInVar(){
	 	    //////////////
		    Abc_Obj_t * pNode;
		    char Buffer[100];
		    pNode = Abc_NtkCreatePi( pNtk );
   	    	sprintf( Buffer, "[%d]", pNode->Id );
	        Abc_NtkLogicStoreName( pNode, Buffer );
		    if( oldpNtk != NULL){
		    	Abc_Obj_t * pObj = Abc_NtkCreatePi( oldpNtk );		    	
			    Abc_NtkLogicStoreName( pObj, Buffer );
		    }
		    Dout( cout<<"Creating Pi: there are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);
		    return pNode->Id;
		    //////////////		 	
		 }


         void ABCSATSolver::disableVarBranch(int i){
         // nothing to do anymore.
        }

        void ABCSATSolver::deleteClauseGroup(int i){
       	Assert( false, "This should not happen");
       }


        void ABCSATSolver::reset(){       			
   	           oldpNtk = pNtk;   	           
   	           results.clear();   	        
   	           char* st = pNtk->pName;
       		oldpNtk = pNtk;
       	    pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		    pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );

		    strcpy( pNtk->pName, name.c_str());
		               	
            pOutputNode = Abc_NtkCreateNode( pNtk );
            pSuperOutputNode = NULL;
           	out_cnt = 0;
           	Abc_Obj_t * pNode;
           	int i;
           	Abc_NtkForEachPi( oldpNtk, pNode, i ){
           		Abc_Obj_t * pObj;
			    pObj = Abc_NtkCreatePi( pNtk );
		        Abc_NtkLogicStoreName( pObj, Abc_ObjName(pNode) );
	       	}           	
            Dout( cout<<" reset "<<endl );
            FileOutputABC(output<<"#  ======================================="<<endl);
       }

        void ABCSATSolver::cleanupDatabase(){
			Assert(false, "This doesn't happen");
       }

        void ABCSATSolver::clean(){			
       		results.clear();		
       		char* st = pNtk->pName;
       		oldpNtk = pNtk;
       	    pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		    pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );
		    strcpy( pNtk->pName, st);
            pOutputNode = Abc_NtkCreateNode( pNtk );
            pSuperOutputNode = NULL;
            Abc_NtkDelete( oldpNtk );
            oldpNtk = NULL;
            out_cnt = 0;
       }
       


 void ABCSATSolver::annotate(const string& msg){
       Dout( cout<<msg );
       FileOutputABC(output<<msg<<endl);
}


 void ABCSATSolver::annotateInput(const string& name, int i, int sz){
       Dout( cout<<"x "<<name<<" ");
       FileOutputABC(output<<"x "<<name<<" ");
       for(int t=0; t<sz; ++t){
               Dout( cout<<(i+t)<<" ");
               FileOutputABC(output<<(i+t)<<" ");
       }
       Dout(cout<<endl);
       FileOutputABC(output<<endl);
}


//This function encodes x == a ? b:c;
 void ABCSATSolver::addChoiceClause(int x, int a, int b, int c){
       Dout( cout<<" "<<x<<"= "<<a<<" ? "<<b<<":"<<c<<";"<<endl );
       FileOutputABC( output<<x<<" CHOICE "<<a<<" "<<b<<" "<<c<<endl );
   //////
   Abc_Obj_t * pFanin0, * pFanin1, * pFaninC, * pMux;
   Assert( x > 0, "This shouldn't happen choice");
   pMux = Abc_NtkObj( pNtk, GetIntId(x) );
   pFanin0 = getNode(b);
   pFanin1 = getNode(c);
   pFaninC = getNode(a);
   Abc_ObjAddFanin( pMux, pFaninC );
   Abc_ObjAddFanin( pMux, pFanin0 );
   Abc_ObjAddFanin( pMux, pFanin1 );
   pMux->pData = Abc_SopCreateMux( (Extra_MmFlex_t *) pNtk->pManFunc );
   //////
}


//This function encodes x == a xor b;
 void ABCSATSolver::addXorClause(int x, int a, int b){
       Dout( cout<<" "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
       FileOutputABC( output<<x<<" XOR "<<a<<" "<<b<<endl );
	   Assert( x > 0, "This shouldn't happen xor");
	   Abc_Obj_t * pFanin0, * pFanin1, * pXor;
	   pXor = Abc_NtkObj( pNtk, GetIntId(x) );
	   pFanin0 = getNode(a);
	   pFanin1 = getNode(b);
	   Abc_ObjAddFanin( pXor, pFanin0 );
	   Abc_ObjAddFanin( pXor, pFanin1 );
	   pXor->pData = Abc_SopCreateXor( (Extra_MmFlex_t *) pNtk->pManFunc, 2);
}

//This function encodes x == a or b;
 void ABCSATSolver::addOrClause(int x, int a, int b){
       Dout( cout<<" "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
       FileOutputABC( output<<x<<" OR "<<a<<" "<<b<<endl );
   //////
   Abc_Obj_t * pFanin0, * pFanin1, * pAnd;
   Assert( x > 0, "This shouldn't happen");
   pAnd = Abc_NtkObj( pNtk, GetIntId(x) );
   pFanin0 = getNode(a);
   pFanin1 = getNode(b);
   Abc_ObjAddFanin( pAnd, pFanin0 );
   Abc_ObjAddFanin( pAnd, pFanin1 );
   pAnd->pData = Abc_SopCreateOr( (Extra_MmFlex_t *) pNtk->pManFunc, 2, NULL );
}


//This function encodes a[0] == a[1] or a[2] or ... a[size];
 void ABCSATSolver::addBigOrClause(int* a, int size){
       Dout( cout<<" "<<a[0]<<"= " );
       FileOutputABC( output<<a[0]<<" BOR "<<size<<" " );
   //////
   Abc_Obj_t * pFanin0, * pOr;
   Assert( a[0] > 0, "Bad a[0]");
   pOr = Abc_NtkObj( pNtk, GetIntId(a[0]) );
   for (int i = 0; i < size; i++ )
   {
       Dout(cout<<a[i+1]<<" or ");
       FileOutputABC( output<<a[i+1]<<" " );
       pFanin0 = getNode(a[i+1]);
       Abc_ObjAddFanin( pOr, pFanin0 );
   }
   FileOutputABC( output<<endl );
   Dout(cout<<"; "<<endl);
   pOr->pData = Abc_SopCreateOr( (Extra_MmFlex_t *) pNtk->pManFunc, size, NULL );
   //////

}


//This function encodes x == a and b;
 void ABCSATSolver::addAndClause(int x, int a, int b){
       Dout( cout<<" "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
       FileOutputABC( output<<x<<" AND "<<a<<" "<<b<<endl );
   //////
   Abc_Obj_t * pFanin0, * pFanin1, * pAnd;
   Assert( x > 0, "BAD X");
   pAnd = Abc_NtkObj( pNtk, GetIntId(x) );
   pFanin0 = getNode(a);
   pFanin1 = getNode(b);
   Abc_ObjAddFanin( pAnd, pFanin0 );
   Abc_ObjAddFanin( pAnd, pFanin1 );
   pAnd->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) pNtk->pManFunc, 2, NULL );
   //////
}

//This function encodes x = a;
 void ABCSATSolver::addEqualsClause(int x, int a){
 	//should use buffer.
       Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
       FileOutputABC( output<<x<<" EQ "<<a<<endl );
       Abc_Obj_t * pFanin0, * pAnd;
       pAnd = Abc_NtkObj( pNtk, GetIntId(x) );
       pFanin0 = getNode(a);
       Abc_ObjAddFanin( pAnd, pFanin0 );
       pAnd->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) pNtk->pManFunc, 1, NULL );   
}


//This function encodes x == a;
 void ABCSATSolver::addEquateClause(int x, int a){
       Dout( cout<<" "<<x<<" == "<<a<<"; "<<flush<<endl );
       FileOutputABC( output<<"x OUTXOR "<<x<<" "<<-a<<endl );
       Abc_Obj_t * pFanin0, * pFanin1;
       Abc_Obj_t * pNode;
       pNode = Abc_NtkCreateNode( pNtk );
	   pFanin0 = getNode(x);
	   pFanin1 = getNode(a);
	   Abc_ObjAddFanin( pNode, pFanin0 );
	   Abc_ObjAddFanin( pNode, pFanin1 );
	   pNode->pData = Abc_SopCreateNxor( (Extra_MmFlex_t *) pNtk->pManFunc, 2 );
	   Abc_ObjAddFanin( pOutputNode, pNode );
	   out_cnt++;
}


 void ABCSATSolver::setVarClause(int x){
       Dout( cout<<" set "<<x<<";"<<endl );
       FileOutputABC( output<<"x SET "<<x<<" ;"<<endl );
	   Abc_Obj_t * pNode;       
	   pNode = Abc_NtkObj( pNtk, GetIntId( abs(x) ) );
	   if( Abc_ObjIsPi(pNode)){
		   if( x > 0){
			   	results[pNode->Id] = 1;
		   }else{
			   	results[pNode->Id] = 0;
		   }
		Dout( cout<<"Setting input "<<pNode->Id<<endl);
	   }else{
   			Dout( cout<<"Setting other"<<endl);
   			Assert( Abc_ObjFaninNum(pNode) == 0 , "This is a bug");
		   if( x > 0){
		   	pNode->pData = Abc_SopCreateConst1( (Extra_MmFlex_t *) pNtk->pManFunc);
		   }else{
		   	pNode->pData = Abc_SopCreateConst0( (Extra_MmFlex_t *) pNtk->pManFunc);
		   }
		   Dout( cout<<"Done setting other"<<endl );
	   }
}


 void ABCSATSolver::assertVarClause(int x){	
       Dout( cout<<" assert "<<x<<";"<<endl );
       FileOutputABC( output<<"x OUTASSERT "<<x<<" ;"<<endl );
       Abc_Obj_t * pFanin0;
	   pFanin0 = getNode(x);
	   Abc_ObjAddFanin( pOutputNode, pFanin0 );
	   out_cnt++;
}



 void ABCSATSolver::hardAssertVarClause(int x){	 	
 	if(!solveNegation){
 		assertVarClause(x);	
 	}else{
 		Dout( cout<<"hard assert "<<x<<";"<<endl );
 		if( pSuperOutputNode == NULL){
 			pSuperOutputNode = Abc_NtkCreateNode( pNtk );
 		}
 		Abc_Obj_t * pFanin0;
 		pFanin0 = getNode(x);
	    Abc_ObjAddFanin( pSuperOutputNode, pFanin0 );
 	}
      
}



 void ABCSATSolver::printDiagnostics(char c){
   cout << c << "No diagnostics for now"<<endl;
}


void ABCSATSolver::setOutputAIG(){
	cout<<"Setting up to output AIG"<<endl;
	outputAIG = true;	
}


