/**CFile****************************************************************

  FileName    [ivyTable.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [Structural hashing table.]

  Author      [Alan Mishchenko]
  
  Affiliation [UC Berkeley]

  Date        [Ver. 1.0. Started - May 11, 2006.]

  Revision    [$Id$]

***********************************************************************/

#include "ivy.h"

////////////////////////////////////////////////////////////////////////
///                        DECLARATIONS                              ///
////////////////////////////////////////////////////////////////////////

// hashing for existing node 
static unsigned Ivy_HashOne( Ivy_Obj_t * pNode, int TableSize ) 
{
    unsigned Key = Ivy_ObjIsExor(pNode) * 1699;
    Key ^= Ivy_ObjFaninId0(pNode) * 7937;
    Key ^= Ivy_ObjFaninId1(pNode) * 2971;
    Key ^= Ivy_ObjFaninC0(pNode) * 911;
    Key ^= Ivy_ObjFaninC1(pNode) * 353;
    return Key % TableSize;
}

// hashing for potentially existing node
static unsigned Ivy_HashTwo( Ivy_Obj_t * p0, Ivy_Obj_t * p1, int fExor, int TableSize ) 
{
    unsigned Key = fExor * 1699;
    Key ^= Ivy_Regular(p0)->Id * 7937;
    Key ^= Ivy_Regular(p1)->Id * 2971;
    Key ^= Ivy_IsComplement(p0) * 911;
    Key ^= Ivy_IsComplement(p1) * 353;
    return Key % TableSize;
}

////////////////////////////////////////////////////////////////////////
///                     FUNCTION DEFINITIONS                         ///
////////////////////////////////////////////////////////////////////////

/**Function*************************************************************

  Synopsis    [Checks if such node exists in the hash table.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Ivy_Obj_t * Ivy_TableLookupNode( Ivy_Obj_t * p0, Ivy_Obj_t * p1, int fExor )
{
    Ivy_Man_t * p = Ivy_ObjMan(p0);
    int i;
    for ( i = Ivy_HashTwo(p0, p1, fExor, p->nTableSize); p->pTable[i]; i = (i+1) % p->nTableSize )
        if ( Ivy_ObjIsExor(p->pTable[i]) == fExor && 
             Ivy_ObjChild0(p->pTable[i]) == p0 && 
             Ivy_ObjChild1(p->pTable[i]) == p1 )
            return p->pTable[i];
    return NULL;
}

/**Function*************************************************************

  Synopsis    [Adds the node to the hash table.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_TableInsertNode( Ivy_Obj_t * pNode )
{
    Ivy_Man_t * p = Ivy_ObjMan(pNode);
    int i;
    for ( i = Ivy_HashOne(pNode, p->nTableSize); p->pTable[i]; i = (i+1) % p->nTableSize );
    p->pTable[i] = pNode;
}

/**Function*************************************************************

  Synopsis    [Deletes the node from the hash table.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_TableDeleteNode( Ivy_Obj_t * pNode )
{
    Ivy_Man_t * p = Ivy_ObjMan(pNode);
    Ivy_Obj_t * pObj;
    int i;
    // find this entry
    for ( i = Ivy_HashOne(pNode, p->nTableSize); p->pTable[i]; i = (i+1) % p->nTableSize )
        if ( p->pTable[i] == pNode )
            break;
    assert( p->pTable[i] == NULL ); // node should be in the table
    p->pTable[i] = NULL;
    // rehash the remaining entries
    for ( i++; p->pTable[i]; i = (i+1) % p->nTableSize )
    {
        pObj = p->pTable[i];
        p->pTable[i] = NULL;
        Ivy_TableInsertNode( pObj );
    }
}

/**Function*************************************************************

  Synopsis    [Resizes the table.]

  Description [Typically this procedure should not be called.]
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_TableResize( Ivy_Man_t * p )
{
    Ivy_Obj_t ** pTableNew, * pNode;
    int nTableSizeNew, Counter, e, i, clk;

    assert( 0 );

clk = clock();
    // get the new table size
    nTableSizeNew = 2 * p->nTableSize + 13; 
    // allocate a new array
    pTableNew = ALLOC( Ivy_Obj_t *, nTableSizeNew );
    memset( pTableNew, 0, sizeof(Ivy_Obj_t *) * nTableSizeNew );
    // rehash the entries from the old table
    Counter = 0;
    for ( e = 0; e < p->nTableSize; e++ )
    {
        pNode = p->pTable[e];
        if ( pNode == NULL )
            continue;
        Counter++;

        // hash it by ID
        for ( i = Ivy_HashOne(pNode, p->nTableSize); pTableNew[i]; i = (i+1) % nTableSizeNew )
            if ( pTableNew[i] == pNode )
                assert( 0 );
        assert( pTableNew[i] == NULL );
        pTableNew[i] = pNode;
    }
    assert( Counter == p->nNodes );
//    printf( "Increasing the structural table size from %6d to %6d. ", p->nTableSize, nTableSizeNew );
//    PRT( "Time", clock() - clk );
    // replace the table and the parameters
    free( p->pTable );
    p->pTable = pTableNew;
    p->nTableSize = nTableSizeNew;
}

/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Ivy_TableRehash( Ivy_Man_t * p )
{
/*
    Abc_Obj_t ** pBinsNew;
    Abc_Obj_t * pEnt, * pEnt2;
    int * pArray;
    unsigned Key;
    int Counter, Temp, i;

    // allocate a new array
    pBinsNew = ALLOC( Abc_Obj_t *, p->nBins );
    memset( pBinsNew, 0, sizeof(Abc_Obj_t *) * p->nBins );
    // rehash the entries from the old table
    Counter = 0;
    for ( i = 0; i < p->nBins; i++ )
        Abc_AigBinForEachEntrySafe( p->pBins[i], pEnt, pEnt2 )
        {
            // swap the fanins if needed
            pArray = pEnt->vFanins.pArray;
            if ( pArray[0] > pArray[1] )
            {
                Temp = pArray[0];
                pArray[0] = pArray[1];
                pArray[1] = Temp;
                Temp = pEnt->fCompl0;
                pEnt->fCompl0 = pEnt->fCompl1;
                pEnt->fCompl1 = Temp;
            }
            // rehash the node
            Key = Abc_HashKey2( Abc_ObjChild0(pEnt), Abc_ObjChild1(pEnt), p->nBins );
            pEnt->pNext   = pBinsNew[Key];
            pBinsNew[Key] = pEnt;
            Counter++;
        }
    assert( Counter == p->nEntries );
    // replace the table and the parameters
    free( p->pBins );
    p->pBins = pBinsNew;
*/
}


////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


