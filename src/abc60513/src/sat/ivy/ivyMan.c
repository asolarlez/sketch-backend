/**CFile****************************************************************

  FileName    [ivyMan.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [AIG manager.]

  Author      [Alan Mishchenko]
  
  Affiliation [UC Berkeley]

  Date        [Ver. 1.0. Started - May 11, 2006.]

  Revision    [$Id$]

***********************************************************************/

#include "ivy.h"

////////////////////////////////////////////////////////////////////////
///                        DECLARATIONS                              ///
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
///                     FUNCTION DEFINITIONS                         ///
////////////////////////////////////////////////////////////////////////

/**Function*************************************************************

  Synopsis    [Starts the AIG manager.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Man_t * Ivy_ManStart( int nCis, int nCos, int nNodesMax )
{
    Ivy_Man_t * p;
    Ivy_Obj_t * pObj;
    int i;
    // start the manager
    p = ALLOC( Ivy_Man_t, 1 );
    memset( p, 0, sizeof(Ivy_Man_t) );
    // AIG nodes
    p->nCis = nCis;
    p->nCos = nCos;
    p->nObjs = 1 + nCis + nCos;
    p->ObjIdNext = p->nObjs;
    p->nObjsAlloc = p->nObjs + nNodesMax;
    p->pObjs = ALLOC( Ivy_Obj_t, p->nObjsAlloc + 1 );
    memset( p->pObjs, 0, sizeof(Ivy_Obj_t) * (p->nObjsAlloc + 1) );
    // remember the manager in the first entry
    *((Ivy_Man_t **)p->pObjs) = p; 
    p->pObjs++;
    // create the nodes
    p->pObjs->fPhase = 1; // constant
    Ivy_ManForEachObj( p, pObj, i ) 
        pObj->Id = i;
    Ivy_ManForEachCi( p, pObj, i )
        pObj->Type = IVY_CI;
    Ivy_ManForEachCo( p, pObj, i )
        pObj->Type = IVY_CO;
    // start the table
    p->nTableSize = p->nObjsAlloc*5/2+13;
    p->pTable = ALLOC( Ivy_Obj_t *, p->nTableSize );
    memset( p->pTable, 0, sizeof(Ivy_Obj_t *) * p->nTableSize );
    // do not start fanouts now
    p->nTravIds   =  1;
    return p;
}

/**Function*************************************************************

  Synopsis    [Stops the AIG manager.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_ManStop( Ivy_Man_t * p )
{
    int i;
    for ( i = 0; i < p->nArrays; i++ )
        free( p->vvFanouts[i].pArray );
    free( p->vvFanouts );
    free( p->pObjs - 1 );
    free( p->pTable );
    free( p );
}

/**Function*************************************************************

  Synopsis    [Returns the number of dangling nodes removed.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
int Ivy_ManCleanup( Ivy_Man_t * p )
{
    Ivy_Obj_t * pNode;
    int i, nNodesOld;
    assert( p->vvFanouts );
    nNodesOld = Ivy_ManNodeNum(p);
    Ivy_ManForEachNode( p, pNode, i )
        if ( Ivy_ObjFanoutNum(pNode) == 0 )
            Ivy_NodeDelete_rec( pNode );
    return nNodesOld - Ivy_ManNodeNum(p);
}


/**Function*************************************************************

  Synopsis    [Puts the nodes into the DFS order and reassign their IDs.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
/*
void Abc_NtkReassignIds( Abc_Ntk_t * pNtk )
{
    Vec_Ptr_t * vNodes;
    Vec_Ptr_t * vObjsNew;
    Abc_Obj_t * pNode, * pTemp, * pConst1;
    int i, k;
    assert( Abc_NtkIsStrash(pNtk) );
    // start the array of objects with new IDs
    vObjsNew = Vec_PtrAlloc( pNtk->nObjs );
    // put constant node first
    pConst1 = Abc_NtkConst1(pNtk);
    assert( pConst1->Id == 0 );
    Vec_PtrPush( vObjsNew, pConst1 );
    // put PI nodes next
    Abc_NtkForEachPi( pNtk, pNode, i )
    {
        pNode->Id = Vec_PtrSize( vObjsNew );
        Vec_PtrPush( vObjsNew, pNode );
    }
    // put PO nodes next
    Abc_NtkForEachPo( pNtk, pNode, i )
    {
        pNode->Id = Vec_PtrSize( vObjsNew );
        Vec_PtrPush( vObjsNew, pNode );
    }
    // put assert nodes next
    Abc_NtkForEachAssert( pNtk, pNode, i )
    {
        pNode->Id = Vec_PtrSize( vObjsNew );
        Vec_PtrPush( vObjsNew, pNode );
    }
    // put latches next
    Abc_NtkForEachLatch( pNtk, pNode, i )
    {
        pNode->Id = Vec_PtrSize( vObjsNew );
        Vec_PtrPush( vObjsNew, pNode );
    }
    // finally, internal nodes in the DFS order
    vNodes = Abc_AigDfs( pNtk, 1, 0 );
    Vec_PtrForEachEntry( vNodes, pNode, i )
    {
        if ( pNode == pConst1 )
            continue;
        pNode->Id = Vec_PtrSize( vObjsNew );
        Vec_PtrPush( vObjsNew, pNode );
    }
    Vec_PtrFree( vNodes );
    assert( Vec_PtrSize(vObjsNew) == pNtk->nObjs );

    // update the fanin/fanout arrays
    Abc_NtkForEachObj( pNtk, pNode, i )
    {
        Abc_ObjForEachFanin( pNode, pTemp, k )
            pNode->vFanins.pArray[k] = pTemp->Id;
        Abc_ObjForEachFanout( pNode, pTemp, k )
            pNode->vFanouts.pArray[k] = pTemp->Id;
    }

    // replace the array of objs
    Vec_PtrFree( pNtk->vObjs );
    pNtk->vObjs = vObjsNew;

    // rehash the AIG
    Abc_AigRehash( pNtk->pManFunc );
}
*/

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


