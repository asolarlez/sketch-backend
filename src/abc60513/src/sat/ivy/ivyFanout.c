/**CFile****************************************************************

  FileName    [ivyFanout.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [Manipulation of fanouts.]

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

  Synopsis    [Add the fanout to the fanin.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_ObjAddFaninFanout( Ivy_Obj_t * pFanin, Ivy_Obj_t * pFanout )
{
    Ivy_Man_t * p;
    assert( pFanin->nRefs == 255 || Ivy_ObjFanoutNum(pFanin) == (int)pFanin->nRefs );
    if ( Ivy_ObjFanoutNum(pFanin) == 0 )
    {
        pFanin->nRefs = 1;
        pFanin->Fanout = pFanout->Id;
        return;
    }
    assert( Ivy_ObjRefs(pFanin) > 0 );
    Ivy_ObjRefsInc(pFanin);
    if ( pFanin->fArray == 0 )
    {
        p = Ivy_ObjMan(pFanin);
        if ( p->nArrays == p->nArraysAlloc )
        {
            if ( p->nArraysAlloc )
                printf( "Ivy_ObjAddFaninFanout(): Reallocing the fanout array.\n" );
            p->nArraysAlloc = p->nArraysAlloc? 2 * p->nArraysAlloc : p->nObjsAlloc / 3;
            p->vvFanouts = REALLOC( Vec_Int_t, p->vvFanouts, p->nArraysAlloc );
            memset( p->vvFanouts + p->nObjsAlloc / 2, 0, sizeof(Vec_Int_t) * p->nObjsAlloc / 2 );
        }
        pFanin->fArray = 1;
        pFanin->Fanout = p->nArrays++;
    }
    Vec_IntPush( Ivy_ObjFanoutArray(pFanin), pFanout->Id );
}

/**Function*************************************************************

  Synopsis    [Removes the fanout of the fanin.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_ObjRemoveFaninFanout( Ivy_Obj_t * pFanin, Ivy_Obj_t * pFanoutToRemove )
{
    assert( pFanin->nRefs == 255 || Ivy_ObjFanoutNum(pFanin) == (int)pFanin->nRefs );
    assert( pFanin->nRefs > 0 );
    if ( Ivy_ObjFanoutNum(pFanin) == 1 )
    {
        assert( pFanin->nRefs == 1 );
        assert( pFanin->Fanout == pFanoutToRemove->Id );
        pFanin->nRefs = 0;
        pFanin->Fanout = 0;
        return;
    }
    assert( pFanin->nRefs > 1 );
    assert( pFanin->fArray == 1 );
    Ivy_ObjRefsDec(pFanin);
    Vec_IntRemove( Ivy_ObjFanoutArray(pFanin), pFanoutToRemove->Id );
    if ( Ivy_ObjFanoutNum(pFanin) == 1 )
    {
        pFanin->Fanout = Vec_IntEntry(Ivy_ObjFanoutArray(pFanin), 0);
        pFanin->fArray = 0;
    }
}



////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


