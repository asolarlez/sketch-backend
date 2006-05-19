/**CFile****************************************************************

  FileName    [ivyObj.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [Adding/removing objects.]

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

  Synopsis    [Create the new node assuming it does not exist.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_NodeCreate( Ivy_Obj_t * pFanin0, Ivy_Obj_t * pFanin1, int fExor )
{
    Ivy_Man_t * p = Ivy_ObjMan(pFanin0);
    Ivy_Obj_t * pNode;
    // realloc the node array
    if ( p->ObjIdNext == p->nObjsAlloc )
    {
        printf( "Ivy_NodeCreate(): Reallocing the node array.\n" );
        p->nObjsAlloc = 2 * p->nObjsAlloc;
        p->pObjs = REALLOC( Ivy_Obj_t, p->pObjs - 1, p->nObjsAlloc + 1 ) + 1;
        memset( p->pObjs + p->ObjIdNext, 0, sizeof(Ivy_Obj_t) * p->nObjsAlloc / 2 );
    }
    // create the new node
    pNode = p->pObjs + p->ObjIdNext;
    pNode->Id = p->ObjIdNext++;
    pNode->Type = fExor? IVY_EXOR : IVY_AND;
    Ivy_NodeConnect( pNode, pFanin0, pFanin1 );
    // update node counters of the manager
    p->nObjs++;
    p->nNodes++;
    if ( Ivy_ObjIsAnd(pNode) )
        p->nAnds++;
    else
        p->nExors++;
    return pNode;
}


/**Function*************************************************************

  Synopsis    [Deletes the node.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_NodeDelete( Ivy_Obj_t * pNode )
{
    Ivy_Man_t * p;
    assert( Ivy_ObjFanoutNum(pNode) == 0 );
    assert( Ivy_ObjIsNode(pNode) );
    // remove connections
    Ivy_NodeDisconnect( pNode );
    // update node counters of the manager
    p = Ivy_ObjMan(pNode);
    p->nObjs--;
    p->nNodes--;
    if ( Ivy_ObjIsAnd(pNode) )
        p->nAnds--;
    else
        p->nExors--;
    // clean the node's memory
    memset( pNode, 0, sizeof(Ivy_Obj_t) );
}

/**Function*************************************************************

  Synopsis    [Deletes the MFFC of the node.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_NodeDelete_rec( Ivy_Obj_t * pNode )
{
    Ivy_Obj_t * pFanin0, * pFanin1;
    if ( !Ivy_ObjIsNode(pNode) )
        return;
    pFanin0 = Ivy_ObjFanin0(pNode);
    pFanin1 = Ivy_ObjFanin1(pNode);
    Ivy_NodeDelete( pNode );
    if ( Ivy_ObjFanoutNum(pFanin0) == 0 )
        Ivy_NodeDelete_rec( pFanin0 );
    if ( Ivy_ObjFanoutNum(pFanin1) == 0 )
        Ivy_NodeDelete_rec( pFanin1 );
}


/**Function*************************************************************

  Synopsis    [Connects the CO to its fanin node in the AIG.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_NodeConnectCo( Ivy_Obj_t * pNode, Ivy_Obj_t * pFanin )
{
    assert( Ivy_ObjIsCo(pNode) );
    Ivy_NodeConnect( pFanin, Ivy_ObjConst1(pNode), pNode );
    pNode->Level--;
}

/**Function*************************************************************

  Synopsis    [Connects the node to its fanin nodes in the AIG.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_NodeConnect( Ivy_Obj_t * pNode, Ivy_Obj_t * pFanin0, Ivy_Obj_t * pFanin1 )
{
    assert( !Ivy_IsComplement(pNode) );
    assert( pNode->Fanin1 == 0 || Ivy_ObjIsNode(pNode) );
    assert( !Ivy_ObjIsExor(pNode) || (!Ivy_IsComplement(pFanin0) && !Ivy_IsComplement(pFanin1)) );
    // add the fanins
    pNode->Fanin0 = Ivy_Regular(pFanin0)->Id;
    pNode->Fanin1 = Ivy_Regular(pFanin1)->Id;
    pNode->fComp0 = Ivy_IsComplement(pFanin0);
    pNode->fComp1 = Ivy_IsComplement(pFanin1);
    // add the fanins fanouts
    Ivy_ObjAddFaninFanout( Ivy_Regular(pFanin0), pNode );
    Ivy_ObjAddFaninFanout( Ivy_Regular(pFanin1), pNode );
    // compute the phase (sim value for 000... pattern)
    if ( Ivy_ObjIsExor(pNode) )
        pNode->fPhase = Ivy_Regular(pFanin0)->fPhase ^ Ivy_Regular(pFanin1)->fPhase;
    else
        pNode->fPhase = (Ivy_IsComplement(pFanin0) ^ Ivy_Regular(pFanin0)->fPhase) & 
                        (Ivy_IsComplement(pFanin1) ^ Ivy_Regular(pFanin1)->fPhase);
    // add the node to the structural hash table if it is not a CO
    if ( pNode->Fanin1 > 0 )
    Ivy_TableInsertNode( pNode );
    // compute the level
    pNode->Level = Ivy_ObjLevelNew(pNode);
}

/**Function*************************************************************

  Synopsis    [Disconnects the node from its fanin nodes in the AIG.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_NodeDisconnect( Ivy_Obj_t * pNode )
{
    assert( !Ivy_IsComplement(pNode) );
    assert( Ivy_ObjIsNode(pNode) );
    // remove the fanouts
    Ivy_ObjRemoveFaninFanout( Ivy_ObjFanin0(pNode), pNode );
    Ivy_ObjRemoveFaninFanout( Ivy_ObjFanin1(pNode), pNode );
    // remove the node from the structural hash table
    Ivy_TableDeleteNode( pNode );
    // clean the fanin info
    pNode->Fanin0 = 0;
    pNode->Fanin1 = 0;
    pNode->fComp0 = 0;
    pNode->fComp1 = 0;
}


////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


