/**CFile****************************************************************

  FileName    [ivyOper.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [AIG operations.]

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

  Synopsis    [Performs canonicization step.]

  Description [The argument nodes can be complemented.]
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_And( Ivy_Obj_t * p0, Ivy_Obj_t * p1 )
{
    Ivy_Man_t * pMan = Ivy_ObjMan(p0);
    Ivy_Obj_t * pNode;
    // check for trivial cases
    if ( p0 == p1 )
        return p0;
    if ( p0 == Ivy_Not(p1) )
        return Ivy_Not(Ivy_ManConst1(pMan));
    if ( Ivy_Regular(p0) == Ivy_ManConst1(pMan) )
    {
        if ( p0 == Ivy_ManConst1(pMan) )
            return p1;
        return Ivy_Not(Ivy_ManConst1(pMan));
    }
    if ( Ivy_Regular(p1) == Ivy_ManConst1(pMan) )
    {
        if ( p1 == Ivy_ManConst1(pMan) )
            return p0;
        return Ivy_Not(Ivy_ManConst1(pMan));
    }
    // order the arguments
    if ( Ivy_Regular(p0)->Id > Ivy_Regular(p1)->Id )
    {
        if ( pNode = Ivy_TableLookupNode( p1, p0, 0 ) )
            return pNode;
        return Ivy_NodeCreate( p1, p0, 0 );
    }
    else
    {
        if ( pNode = Ivy_TableLookupNode( p0, p1, 0 ) )
            return pNode;
        return Ivy_NodeCreate( p0, p1, 0 );
    }
}

/**Function*************************************************************

  Synopsis    [Implements Boolean OR.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_Or( Ivy_Obj_t * p0, Ivy_Obj_t * p1 )
{
    return Ivy_Not( Ivy_And( Ivy_Not(p0), Ivy_Not(p1) ) );
}

/**Function*************************************************************

  Synopsis    [Implements Boolean XOR.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_Exor( Ivy_Obj_t * p0, Ivy_Obj_t * p1 )
{
    return Ivy_Or( Ivy_And(p0, Ivy_Not(p1)), Ivy_And(p1, Ivy_Not(p0)) );
}
 
/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_Mux( Ivy_Obj_t * pC, Ivy_Obj_t * p1, Ivy_Obj_t * p0 )
{
    return Ivy_Or( Ivy_And(pC, p1), Ivy_And(Ivy_Not(pC), p0) );
}

/**Function*************************************************************

  Synopsis    [Implements the miter.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_Miter_rec( Ivy_Obj_t ** ppObjs, int nObjs )
{
    Ivy_Obj_t * pObj1, * pObj2;
    if ( nObjs == 1 )
        return ppObjs[0];
    pObj1 = Ivy_Miter_rec( ppObjs,           nObjs/2         );
    pObj2 = Ivy_Miter_rec( ppObjs + nObjs/2, nObjs - nObjs/2 );
    return Ivy_Or( pObj1, pObj2 );
}
 
/**Function*************************************************************

  Synopsis    [Implements the miter.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_Miter( Vec_Ptr_t * vPairs )
{
    int i;
    assert( vPairs->nSize > 0 );
    assert( vPairs->nSize % 2 == 0 );
    // go through the cubes of the node's SOP
    for ( i = 0; i < vPairs->nSize; i += 2 )
        vPairs->pArray[i/2] = Ivy_Exor( vPairs->pArray[i], vPairs->pArray[i+1] );
    vPairs->nSize = vPairs->nSize/2;
    return Ivy_Miter_rec( (Ivy_Obj_t **)vPairs->pArray, vPairs->nSize );
}

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


