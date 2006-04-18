
#ifndef SATSOLVER_H
#define SATSOLVER_H

#include "abc.h"
#include "main.h"
#include <iostream>
#include <fstream>
#include <map>
#include <vector>

using namespace std;
enum SAT_StatusT {
    UNDETERMINED,
    UNSATISFIABLE,
    SATISFIABLE,
    TIME_OUT,
    MEM_OUT,
    ABORTED
};


#define Assert( in, msg) if(!(in)){cout<<msg<<endl; exit(1); }
#define Dout( out )      out 
#define CheckRepeats( AR, N) /* for(int _i=0; _i<N; ++_i){ for(int _j=_i+1; _j<N; ++_j){ Assert( (AR[_i])/2 != (AR[_j])/2, "REPEAT ENTRY IN CLAUSE "<<_i<<"  "<<_j<<"  "<<AR[_i] ); } } */
#define FileOutput( out )  out


inline void SolverStart(){ cout<<" STARTING ABC "<<endl; Abc_Start(); }
inline void SolverEnd(){cout<<" ENDING ABC "<<endl; Abc_Stop(); }


class SATSolver{
protected:       
       string name;
       FileOutput( ofstream output );
       Abc_Ntk_t * pNtk;
       Abc_Ntk_t * oldpNtk;
       Abc_Obj_t * pOutputNode;
       int out_cnt;
		map<int, int> results;
		
	int GetIntId(int val){
		return (val);
	}
	
	Abc_Obj_t * getNode(int v){
		Abc_Obj_t * pFanin0;
		if( v > 0 ){
			pFanin0 = Abc_NtkObj( pNtk, GetIntId(v) );
			return pFanin0;
		}else{
			pFanin0 = Abc_NtkObj( pNtk, GetIntId(-v) );
			Abc_Obj_t * pNode;
	        pNode = Abc_NtkCreateNode( pNtk );
	    	Abc_ObjAddFanin( pNode, pFanin0 );
	    	pNode->pData = Abc_SopCreateInv( (Extra_MmFlex_t *) pNtk->pManFunc );
	    	return pNode;
		}
	}
       
public:
        SATSolver(const string& name_p):name(name_p){
               FileOutput( string nm = name; nm += ".circuit"; );
               FileOutput( output.open(nm.c_str()) );
		       /////////       
		       pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		       pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );
		       strcpy( pNtk->pName, name.c_str() );
   	           pOutputNode = Abc_NtkCreateNode( pNtk );
   	           oldpNtk = NULL;
   	           out_cnt = 0;
		       ////////
        }
        void annotate(const string& msg);
        void annotateInput(const string& name, int i, int sz);
        void addChoiceClause(int x, int a, int b, int c, int gid=0);
        void addXorClause(int x, int a, int b, int gid=0);
        void addOrClause(int x, int a, int b, int gid=0);
        void addBigOrClause(int* a, int size, int gid=0);
        void addAndClause(int x, int a, int b, int gid=0);
        void addEqualsClause(int x, int a, int gid=0);
        void addEquateClause(int x, int a, int gid=0);
        void setVarClause(int x, int gid=0);
    void assertVarClause(int x, int gid=0);

        inline int getVarVal(int id){
        		Dout( cout<<"var val for ["<<id<<"] = "<<results[id]<<endl ) ;
               return results[id];
        }

        inline int newVar(){
	        //////////////
	        Abc_Obj_t * pNode;
	        pNode = Abc_NtkCreateNode( pNtk );
	        return pNode->Id;
	        //////////////
        }

		 inline int newInVar(){
	 	    //////////////
		    Abc_Obj_t * pNode;
		    pNode = Abc_NtkCreatePi( pNtk );
		    Dout( cout<<"Creating Pi: there are "<<Abc_NtkPiNum(pNtk)<<" Pis"<<endl);
		    return pNode->Id;
		    //////////////		 	
		 }


        inline void disableVarBranch(int i){
         // nothing to do anymore.
        }

       inline void deleteClauseGroup(int i){
       	Assert( false, "This should not happen");
       }

       int solve();






       inline void reset(){       			
   	           oldpNtk = pNtk;   	           
   	           results.clear();
   	           Dout( cout<<" reset "<<endl );
               FileOutput(output<<"#  ======================================="<<endl);
       }

       inline void cleanupDatabase(){
			Assert(false, "This doesn't happen");
       }

       inline void clean(){			
       		results.clear();		
       		char* st = pNtk->pName;
       		oldpNtk = pNtk;
       	    pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		    pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );
		    strcpy( pNtk->pName, st);
            pOutputNode = Abc_NtkCreateNode( pNtk );
            Abc_NtkDelete( oldpNtk );
            oldpNtk = NULL;
            out_cnt = 0;
       }

       void printDiagnostics(char c);
};




inline void SATSolver::annotate(const string& msg){
       Dout( cout<<msg );
       FileOutput(output<<msg<<endl);
}


inline void SATSolver::annotateInput(const string& name, int i, int sz){
       Dout( cout<<"x "<<name<<" ");
       FileOutput(output<<"x "<<name<<" ");
       for(int t=0; t<sz; ++t){
               Dout( cout<<(i+t)<<" ");
               FileOutput(output<<(i+t)<<" ");
       }
       Dout(cout<<endl);
       FileOutput(output<<endl);
}


//This function encodes x == a ? b:c;
inline void SATSolver::addChoiceClause(int x, int a, int b, int c, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<" ? "<<b<<":"<<c<<";"<<endl );
       FileOutput( output<<x<<" CHOICE "<<a<<" "<<b<<" "<<c<<endl );
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
inline void SATSolver::addXorClause(int x, int a, int b, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
       FileOutput( output<<x<<" XOR "<<a<<" "<<b<<endl );
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
inline void SATSolver::addOrClause(int x, int a, int b, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
       FileOutput( output<<x<<" OR "<<a<<" "<<b<<endl );
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
inline void SATSolver::addBigOrClause(int* a, int size, int gid){
       Dout( cout<<" "<<a[0]<<"= " );
       FileOutput( output<<a[0]<<" BOR "<<size<<" " );
   //////
   Abc_Obj_t * pFanin0, * pOr;
   Assert( a[0] > 0, "Bad a[0]");
   pOr = Abc_NtkObj( pNtk, GetIntId(a[0]) );
   for (int i = 0; i < size; i++ )
   {
       Dout(cout<<a[i+1]<<" or ");
       FileOutput( output<<a[i+1]<<" " );
       pFanin0 = getNode(a[i+1]);
       Abc_ObjAddFanin( pOr, pFanin0 );
   }
   FileOutput( output<<endl );
   Dout(cout<<"; "<<endl);
   pOr->pData = Abc_SopCreateOr( (Extra_MmFlex_t *) pNtk->pManFunc, size, NULL );
   //////

}


//This function encodes x == a and b;
inline void SATSolver::addAndClause(int x, int a, int b, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
       FileOutput( output<<x<<" AND "<<a<<" "<<b<<endl );
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
inline void SATSolver::addEqualsClause(int x, int a, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
       FileOutput( output<<x<<" EQ "<<a<<endl );
       Abc_Obj_t * pFanin0, * pAnd;
       pAnd = Abc_NtkObj( pNtk, GetIntId(x) );
       pFanin0 = getNode(a);
       Abc_ObjAddFanin( pAnd, pFanin0 );
       pAnd->pData = Abc_SopCreateAnd( (Extra_MmFlex_t *) pNtk->pManFunc, 1, NULL );   
}


//This function encodes x == a;
inline void SATSolver::addEquateClause(int x, int a, int gid){
       Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
       FileOutput( output<<"x OUTXOR "<<x<<" "<<-a<<endl );
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


inline void SATSolver::setVarClause(int x, int gid){
       Dout( cout<<" set "<<x<<";"<<endl );
       FileOutput( output<<"x SET "<<x<<" ;"<<endl );
	   Abc_Obj_t * pNode;       
	   pNode = Abc_NtkObj( pNtk, GetIntId( abs(x) ) );
	   if( Abc_ObjIsPi(pNode)){
		   if( x > 0){
			   	results[pNode->Id] = 1;
		   }else{
			   	results[pNode->Id] = 0;
		   }
		Dout( cout<<"Setting input"<<endl);
	   }else{
   			Dout( cout<<"Setting other"<<endl);
		   if( x > 0){
		   	pNode->pData = Abc_SopCreateConst1( (Extra_MmFlex_t *) pNtk->pManFunc);
		   }else{
		   	pNode->pData = Abc_SopCreateConst0( (Extra_MmFlex_t *) pNtk->pManFunc);
		   }
		   Dout( cout<<"Done setting other"<<endl );
	   }
}


inline void SATSolver::assertVarClause(int x, int gid){	
       Dout( cout<<" assert "<<x<<";"<<endl );
       FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );
       Abc_Obj_t * pFanin0;
	   pFanin0 = getNode(x);
	   Abc_ObjAddFanin( pOutputNode, pFanin0 );
	   out_cnt++;
}

inline void SATSolver::printDiagnostics(char c){
   cout << c << "No diagnostics for now"<<endl;
}


#endif

