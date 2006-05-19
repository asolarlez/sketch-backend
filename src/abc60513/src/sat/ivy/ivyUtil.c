/**CFile****************************************************************

  FileName    [ivyUtil.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [Various procedures.]

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

  Synopsis    [Increments the current traversal ID of the network.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_ManIncrementTravId( Ivy_Man_t * pMan )
{
    Ivy_Obj_t * pObj;
    int i;
    if ( pMan->nTravIds == (1<<24)-1 )
    {
        pMan->nTravIds = 0;
        Ivy_ManForEachObj( pMan, pObj, i )
            pObj->TravId = 0;
    }
    pMan->nTravIds++;
}


/**Function*************************************************************

  Synopsis    [Returns 1 if the node is the root of MUX or EXOR/NEXOR.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
int Ivy_NodeIsMuxType( Ivy_Obj_t * pNode )
{
    Ivy_Obj_t * pNode0, * pNode1;
    // check that the node is regular
    assert( !Ivy_IsComplement(pNode) );
    // if the node is not AND, this is not MUX
    if ( !Ivy_ObjIsAnd(pNode) )
        return 0;
    // if the children are not complemented, this is not MUX
    if ( !Ivy_ObjFaninC0(pNode) || !Ivy_ObjFaninC1(pNode) )
        return 0;
    // get children
    pNode0 = Ivy_ObjFanin0(pNode);
    pNode1 = Ivy_ObjFanin1(pNode);
    // if the children are not ANDs, this is not MUX
    if ( !Ivy_ObjIsAnd(pNode0) || !Ivy_ObjIsAnd(pNode1) )
        return 0;
    // otherwise the node is MUX iff it has a pair of equal grandchildren
    return (Ivy_ObjFaninId0(pNode0) == Ivy_ObjFaninId0(pNode1) && (Ivy_ObjFaninC0(pNode0) ^ Ivy_ObjFaninC0(pNode1))) || 
           (Ivy_ObjFaninId0(pNode0) == Ivy_ObjFaninId1(pNode1) && (Ivy_ObjFaninC0(pNode0) ^ Ivy_ObjFaninC1(pNode1))) ||
           (Ivy_ObjFaninId1(pNode0) == Ivy_ObjFaninId0(pNode1) && (Ivy_ObjFaninC1(pNode0) ^ Ivy_ObjFaninC0(pNode1))) ||
           (Ivy_ObjFaninId1(pNode0) == Ivy_ObjFaninId1(pNode1) && (Ivy_ObjFaninC1(pNode0) ^ Ivy_ObjFaninC1(pNode1)));
}

/**Function*************************************************************

  Synopsis    [Recognizes what nodes are control and data inputs of a MUX.]

  Description [If the node is a MUX, returns the control variable C.
  Assigns nodes T and E to be the then and else variables of the MUX. 
  Node C is never complemented. Nodes T and E can be complemented.
  This function also recognizes EXOR/NEXOR gates as MUXes.]
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_NodeRecognizeMux( Ivy_Obj_t * pNode, Ivy_Obj_t ** ppNodeT, Ivy_Obj_t ** ppNodeE )
{
    Ivy_Obj_t * pNode0, * pNode1;
    assert( !Ivy_IsComplement(pNode) );
    assert( Ivy_NodeIsMuxType(pNode) );
    // get children
    pNode0 = Ivy_ObjFanin0(pNode);
    pNode1 = Ivy_ObjFanin1(pNode);
    // find the control variable
//    if ( pNode1->p1 == Fraig_Not(pNode2->p1) )
    if ( Ivy_ObjFaninId0(pNode0) == Ivy_ObjFaninId0(pNode1) && (Ivy_ObjFaninC0(pNode0) ^ Ivy_ObjFaninC0(pNode1)) )
    {
//        if ( Fraig_IsComplement(pNode1->p1) )
        if ( Ivy_ObjFaninC0(pNode0) )
        { // pNode2->p1 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild1(pNode1));//pNode2->p2);
            *ppNodeE = Ivy_Not(Ivy_ObjChild1(pNode0));//pNode1->p2);
            return Ivy_ObjChild0(pNode1);//pNode2->p1;
        }
        else
        { // pNode1->p1 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild1(pNode0));//pNode1->p2);
            *ppNodeE = Ivy_Not(Ivy_ObjChild1(pNode1));//pNode2->p2);
            return Ivy_ObjChild0(pNode0);//pNode1->p1;
        }
    }
//    else if ( pNode1->p1 == Fraig_Not(pNode2->p2) )
    else if ( Ivy_ObjFaninId0(pNode0) == Ivy_ObjFaninId1(pNode1) && (Ivy_ObjFaninC0(pNode0) ^ Ivy_ObjFaninC1(pNode1)) )
    {
//        if ( Fraig_IsComplement(pNode1->p1) )
        if ( Ivy_ObjFaninC0(pNode0) )
        { // pNode2->p2 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild0(pNode1));//pNode2->p1);
            *ppNodeE = Ivy_Not(Ivy_ObjChild1(pNode0));//pNode1->p2);
            return Ivy_ObjChild1(pNode1);//pNode2->p2;
        }
        else
        { // pNode1->p1 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild1(pNode0));//pNode1->p2);
            *ppNodeE = Ivy_Not(Ivy_ObjChild0(pNode1));//pNode2->p1);
            return Ivy_ObjChild0(pNode0);//pNode1->p1;
        }
    }
//    else if ( pNode1->p2 == Fraig_Not(pNode2->p1) )
    else if ( Ivy_ObjFaninId1(pNode0) == Ivy_ObjFaninId0(pNode1) && (Ivy_ObjFaninC1(pNode0) ^ Ivy_ObjFaninC0(pNode1)) )
    {
//        if ( Fraig_IsComplement(pNode1->p2) )
        if ( Ivy_ObjFaninC1(pNode0) )
        { // pNode2->p1 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild1(pNode1));//pNode2->p2);
            *ppNodeE = Ivy_Not(Ivy_ObjChild0(pNode0));//pNode1->p1);
            return Ivy_ObjChild0(pNode1);//pNode2->p1;
        }
        else
        { // pNode1->p2 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild0(pNode0));//pNode1->p1);
            *ppNodeE = Ivy_Not(Ivy_ObjChild1(pNode1));//pNode2->p2);
            return Ivy_ObjChild1(pNode0);//pNode1->p2;
        }
    }
//    else if ( pNode1->p2 == Fraig_Not(pNode2->p2) )
    else if ( Ivy_ObjFaninId1(pNode0) == Ivy_ObjFaninId1(pNode1) && (Ivy_ObjFaninC1(pNode0) ^ Ivy_ObjFaninC1(pNode1)) )
    {
//        if ( Fraig_IsComplement(pNode1->p2) )
        if ( Ivy_ObjFaninC1(pNode0) )
        { // pNode2->p2 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild0(pNode1));//pNode2->p1);
            *ppNodeE = Ivy_Not(Ivy_ObjChild0(pNode0));//pNode1->p1);
            return Ivy_ObjChild1(pNode1);//pNode2->p2;
        }
        else
        { // pNode1->p2 is positive phase of C
            *ppNodeT = Ivy_Not(Ivy_ObjChild0(pNode0));//pNode1->p1);
            *ppNodeE = Ivy_Not(Ivy_ObjChild0(pNode1));//pNode2->p1);
            return Ivy_ObjChild1(pNode0);//pNode1->p2;
        }
    }
    assert( 0 ); // this is not MUX
    return NULL;
}

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


