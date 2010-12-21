
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

inline void ABCSolverStart(){  Abc_Start(); }
inline void ABCSolverEnd(){ Abc_Stop(); }
#define FileOutputABC( out ) /* out */

class ABCSATSolver: public SATSolver{
public:
	typedef enum { BASICSAT, FULL } SolutionStrategy;

protected:
	   int timeout;
       FileOutputABC( ofstream output );
       Abc_Ntk_t * pNtk;
       Abc_Ntk_t * oldpNtk;
       Abc_Obj_t * pOutputNode;
       Abc_Obj_t * pSuperOutputNode;
       int out_cnt;
	   map<int, int> results;
		SolutionStrategy  strategy;
		bool outputAIG;
		int solvcnt;
		bool doCheck;
	int GetIntId(int val){
		return (val);
	}

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

    void closeMiter(Abc_Ntk_t * network);


public:

	virtual void addHelperClause(int c[], int size){}
	void completeProblemSetup();


        ABCSATSolver(const string& name_p, SolutionStrategy strategy_p, SolverMode smode):SATSolver(name_p, smode){
		       /////////
		       strategy = strategy_p;
		       pNtk = Abc_NtkAlloc( ABC_NTK_LOGIC, ABC_FUNC_SOP );
		       pNtk->pName = ALLOC( char, strlen(name.c_str()) + 1 );
		       strcpy( pNtk->pName, name.c_str() );
   	           pOutputNode = Abc_NtkCreateNode( pNtk );
   	           pSuperOutputNode = NULL;
   	           oldpNtk = NULL;
   	           out_cnt = 0;
   	           outputAIG = false;
   	           doCheck = false;
   	           solvcnt = 0;
   	           FileOutputABC( string nm = name; nm += ".circuit"; );
			   FileOutputABC( output.open(nm.c_str()) );
			   timeout = 100000000;
		       ////////
        }
        virtual void annotate(const string& msg);
        virtual void annotateInput(const string& name, int i, int sz);
        virtual void addChoiceClause(int x, int a, int b, int c);
        virtual void addXorClause(int x, int a, int b);
        virtual void addOrClause(int x, int a, int b);
        virtual void addBigOrClause(int* a, int size);
        virtual void addAndClause(int x, int a, int b);
        virtual void addEqualsClause(int x, int a);
        virtual void addEquateClause(int x, int a);
        virtual void setVarClause(int x);
        virtual void assertVarClause(int x);
		virtual void hardAssertVarClause(int x);
		virtual void setTimeout(int to);

        virtual int getVarVal(int id);

        virtual int newVar();
		virtual bool ignoreOld();
		virtual int newInVar();


		virtual void outputToFile(const string& fname);

       virtual void disableVarBranch(int i);
       virtual void deleteClauseGroup(int i);
       virtual int solve();
       virtual void reset();
       virtual void cleanupDatabase();
       virtual void clean();
       virtual void printDiagnostics(char c);
       virtual void setOutputAIG();
	virtual void markInput(int){ }
};


#endif

