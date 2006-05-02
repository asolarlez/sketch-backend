
#ifndef ABCSATSOLVER_H
#define ABCSATSOLVER_H

#include "SATSolver.h"
#include "abc.h"
#include "main.h"
#include <iostream>
#include <fstream>
#include <map>
#include <vector>

using namespace std;

inline void ABCSolverStart(){ cout<<" STARTING ABC "<<endl; Abc_Start(); }
inline void ABCSolverEnd(){cout<<" ENDING ABC "<<endl; Abc_Stop(); }


class ABCSATSolver: public SATSolver{
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
	
	void closeMiter(Abc_Ntk_t * network);
	Abc_Ntk_t * cofactor(Abc_Ntk_t * network, vector<int>& namemap);
	
	
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
        ABCSATSolver(const string& name_p):SATSolver(name_p){
		       /////////       
		       pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		       pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );
		       strcpy( pNtk->pName, name.c_str() );
   	           pOutputNode = Abc_NtkCreateNode( pNtk );
   	           oldpNtk = NULL;
   	           out_cnt = 0;
		       ////////
        }
        virtual void annotate(const string& msg);
        virtual void annotateInput(const string& name, int i, int sz);
        virtual void addChoiceClause(int x, int a, int b, int c, int gid=0);
        virtual void addXorClause(int x, int a, int b, int gid=0);
        virtual void addOrClause(int x, int a, int b, int gid=0);
        virtual void addBigOrClause(int* a, int size, int gid=0);
        virtual void addAndClause(int x, int a, int b, int gid=0);
        virtual void addEqualsClause(int x, int a, int gid=0);
        virtual void addEquateClause(int x, int a, int gid=0);
        virtual void setVarClause(int x, int gid=0);
        virtual void assertVarClause(int x, int gid=0);

        virtual int getVarVal(int id);

        virtual int newVar();
		virtual bool ignoreOld();
		virtual int newInVar();


       virtual void disableVarBranch(int i);
       virtual void deleteClauseGroup(int i);
       virtual int solve();
       virtual void reset();
       virtual void cleanupDatabase();
       virtual void clean();
       virtual void printDiagnostics(char c);
};


#endif

