/**CFile****************************************************************

  FileName    [ivy.h]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [And-Inverter Graph package.]

  Synopsis    [External declarations.]

  Author      [Alan Mishchenko]
  
  Affiliation [UC Berkeley]

  Date        [Ver. 1.0. Started - May 11, 2006.]

  Revision    [$Id$]

***********************************************************************/

#ifndef __IVY_H__
#define __IVY_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
    AIG is an And-Inv Graph with structural hashing.
    It is always structurally hashed. It means that at any time:
    - for each AND gate, there are no other AND gates with the same children
    - the constants are propagated
    - there is no single-input nodes (inverters/buffers)
    Additionally the following invariants are satisfied:
    - there are no dangling nodes (the nodes without fanout)
    - the level of each AND gate reflects the levels of this fanins
    - the AND nodes are in the topological order
    - the constant 1 node has always number 0 in the object list
    The operations that are performed on AIGs:
    - building new nodes (Ivy_And)
    - performing elementary Boolean operations (Ivy_Or, Ivy_Xor, etc)
    - replacing one node by another (Abc_AigReplace)
    - propagating constants (Abc_AigReplace)
    - deleting dangling nodes (Abc_AigDelete)
    When AIG is duplicated, the new graph is structurally hashed too.
    If this repeated hashing leads to fewer nodes, it means the original
    AIG was not strictly hashed (one of the conditions above is violated).

    Some additional ideas for developing a new AIG package:
    - use compact data structure (6 machine words)
    - use contiguous memory for all nodes
      - cache friendly
      - memory efficient for both 32- and 64-bits
      - avoids memory allocation issues
      - allows for direct pointing of fanins and fanouts
    - lookup structural hash table for nodes with one fanout or more
    - use linear probing as a cache friendly hashing technique
    - hide the pointer to the manager in the 0-th unused entry
*/

////////////////////////////////////////////////////////////////////////
///                          INCLUDES                                ///
////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include "vec.h"

////////////////////////////////////////////////////////////////////////
///                         PARAMETERS                               ///
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
///                         BASIC TYPES                              ///
////////////////////////////////////////////////////////////////////////

typedef struct Ivy_Man_t_            Ivy_Man_t;
typedef struct Ivy_Obj_t_            Ivy_Obj_t;

// network types
typedef enum { 
    IVY_CI,                          // 0: primary input (and constant 1 node)
    IVY_CO,                          // 1: primary output
    IVY_AND,                         // 2: internal AND node
    IVY_EXOR                         // 3: internal EXOR node
} Ivy_ObjType_t;

// the AIG node
struct Ivy_Obj_t_  // 6 words
{
    int              Id;             // integer ID
    int              TravId;         // traversal ID
    int              Fanin0;         // fanin ID
    int              Fanin1;         // fanin ID
    int              Fanout;         // fanout ID (or array ID if more than one fanout)
    unsigned         Type    :  2;   // object type
    unsigned         fPhase  :  1;   // value under 000...0 pattern
    unsigned         fMarkA  :  1;   // multipurpose mask
    unsigned         fMarkB  :  1;   // multipurpose mask
    unsigned         fComp0  :  1;   // complemented attribute
    unsigned         fComp1  :  1;   // complemented attribute
    unsigned         fArray  :  1;   // 1 if direct fanout, 0 if array index
    unsigned         nRefs   :  8;   // reference counter (with saturation)
    unsigned         Level   : 16;   // logic level
};

// the AIG manager
struct Ivy_Man_t_
{
    // AIG nodes
    int              nCis;           // the total number of CIs
    int              nCos;           // the total number of COs
    int              nNodes;         // the total number of two-input nodes
    int              nAnds;          // the total number of ANDs
    int              nExors;         // the total number of EXORs
    int              nObjs;          // the total number of objects
    int              ObjIdNext;      // the next free obj ID to assign
    int              nObjsAlloc;     // the allocated number of nodes
    Ivy_Obj_t *      pObjs;          // the array of all nodes
    // stuctural hash table
    Ivy_Obj_t **     pTable;         // structural hash table
    int              nTableSize;     // structural hash table size
    // fanout representation
    int              nArrays;        // the number of fanout arrays
    int              nArraysAlloc;   // the number of fanout arrays allocated
    Vec_Int_t *      vvFanouts;      // fanout arrays
    // various data members
    int              nTravIds;       // the traversal ID
    int              nLevelMax;      // the maximum level
};


////////////////////////////////////////////////////////////////////////
///                      MACRO DEFINITIONS                           ///
////////////////////////////////////////////////////////////////////////

#define IVY_MIN(a,b)       (((a) < (b))? (a) : (b))
#define IVY_MAX(a,b)       (((a) > (b))? (a) : (b))

static inline int          Ivy_BitWordNum( int nBits )            { return nBits/32 + ((nBits%32) > 0);          }
static inline int          Ivy_InfoHasBit( unsigned * p, int i )  { return (p[(i)>>5] & (1<<((i) & 31))) > 0;    }
static inline void         Ivy_InfoSetBit( unsigned * p, int i )  { p[(i)>>5] |= (1<<((i) & 31));                }
static inline void         Ivy_InfoXorBit( unsigned * p, int i )  { p[(i)>>5] ^= (1<<((i) & 31));                }

static inline Ivy_Obj_t *  Ivy_ManConst1( Ivy_Man_t * pMan )      { return pMan->pObjs;                          }
static inline Ivy_Obj_t *  Ivy_ManObj( Ivy_Man_t * pMan, int i )  { return pMan->pObjs + i;                      }
static inline Ivy_Obj_t *  Ivy_ManCi( Ivy_Man_t * pMan, int i )   { return pMan->pObjs + 1 + i;                  }
static inline Ivy_Obj_t *  Ivy_ManCo( Ivy_Man_t * pMan, int i )   { return pMan->pObjs + 1 + pMan->nCis + i;     }

static inline int          Ivy_ManCiNum( Ivy_Man_t * pMan )       { return pMan->nCis;                           }
static inline int          Ivy_ManCoNum( Ivy_Man_t * pMan )       { return pMan->nCos;                           }
static inline int          Ivy_ManNodeNum( Ivy_Man_t * pMan )     { return pMan->nNodes;                         }
static inline int          Ivy_ManAndNum( Ivy_Man_t * pMan )      { return pMan->nAnds;                          }
static inline int          Ivy_ManExorNum( Ivy_Man_t * pMan )     { return pMan->nExors;                         }
static inline int          Ivy_ManObjNum( Ivy_Man_t * pMan )      { return pMan->nObjs;                          }
static inline int          Ivy_ManObjAllocNum( Ivy_Man_t * pMan ) { return pMan->nObjsAlloc;                     }

static inline Ivy_Obj_t *  Ivy_Regular( Ivy_Obj_t * p )           { return (Ivy_Obj_t *)((unsigned)(p) & ~01);   }
static inline Ivy_Obj_t *  Ivy_Not( Ivy_Obj_t * p )               { return (Ivy_Obj_t *)((unsigned)(p) ^  01);   }
static inline Ivy_Obj_t *  Ivy_NotCond( Ivy_Obj_t * p, int c )    { return (Ivy_Obj_t *)((unsigned)(p) ^ (c));   }
static inline int          Ivy_IsComplement( Ivy_Obj_t * p )      { return (int )(((unsigned)p) & 01);           }

static inline int          Ivy_ObjIsConst( Ivy_Obj_t * pObj )     { return Ivy_Regular(pObj)->Id == 0;           }
static inline int          Ivy_ObjIsCi( Ivy_Obj_t * pObj )        { return Ivy_Regular(pObj)->Type == IVY_CI;    }
static inline int          Ivy_ObjIsCo( Ivy_Obj_t * pObj )        { return Ivy_Regular(pObj)->Type == IVY_CO;    }
static inline int          Ivy_ObjIsNode( Ivy_Obj_t * pObj )      { return Ivy_Regular(pObj)->Type >= IVY_AND;   }
static inline int          Ivy_ObjIsAnd( Ivy_Obj_t * pObj )       { return Ivy_Regular(pObj)->Type == IVY_AND;   }
static inline int          Ivy_ObjIsExor( Ivy_Obj_t * pObj )      { return Ivy_Regular(pObj)->Type == IVY_EXOR;  }

static inline Ivy_Man_t *  Ivy_ObjMan( Ivy_Obj_t * pObj )         { assert( !Ivy_IsComplement(pObj) ); return *((Ivy_Man_t **)(pObj - pObj->Id - 1)); }
static inline Ivy_Obj_t *  Ivy_ObjNode( Ivy_Obj_t * pObj, int n ) { assert( !Ivy_IsComplement(pObj) ); return pObj - pObj->Id + n;                    }
static inline Ivy_Obj_t *  Ivy_ObjConst1( Ivy_Obj_t * pObj )      { assert( !Ivy_IsComplement(pObj) ); return pObj - pObj->Id;                    }
 
static inline void         Ivy_ObjSetTravId( Ivy_Obj_t * pObj, int TravId ) { pObj->TravId = TravId;                                              }
static inline void         Ivy_ObjSetTravIdCurrent( Ivy_Obj_t * pObj )      { pObj->TravId = Ivy_ObjMan(pObj)->nTravIds;                          }
static inline void         Ivy_ObjSetTravIdPrevious( Ivy_Obj_t * pObj )     { pObj->TravId = Ivy_ObjMan(pObj)->nTravIds - 1;                      }
static inline int          Ivy_ObjIsTravIdCurrent( Ivy_Obj_t * pObj )       { return (int )((int)pObj->TravId == Ivy_ObjMan(pObj)->nTravIds);     }
static inline int          Ivy_ObjIsTravIdPrevious( Ivy_Obj_t * pObj )      { return (int )((int)pObj->TravId == Ivy_ObjMan(pObj)->nTravIds - 1); }

static inline int          Ivy_ObjId( Ivy_Obj_t * pObj )          { assert( !Ivy_IsComplement(pObj) ); return pObj->Id;                               }
static inline int          Ivy_ObjPhase( Ivy_Obj_t * pObj )       { assert( !Ivy_IsComplement(pObj) ); return pObj->fPhase;                           }
static inline int          Ivy_ObjRefs( Ivy_Obj_t * pObj )        { assert( !Ivy_IsComplement(pObj) ); return pObj->nRefs;                            }
static inline void         Ivy_ObjRefsInc( Ivy_Obj_t * pObj )     { assert( !Ivy_IsComplement(pObj) ); if ( pObj->nRefs < 255 ) pObj->nRefs++;        }
static inline void         Ivy_ObjRefsDec( Ivy_Obj_t * pObj )     { assert( !Ivy_IsComplement(pObj) ); assert( pObj->nRefs > 0 ); if ( pObj->nRefs < 255 ) pObj->nRefs--;      }
static inline int          Ivy_ObjFaninId0( Ivy_Obj_t * pObj )    { assert( !Ivy_IsComplement(pObj) ); return pObj->Fanin0;                           }
static inline int          Ivy_ObjFaninId1( Ivy_Obj_t * pObj )    { assert( !Ivy_IsComplement(pObj) ); return pObj->Fanin1;                           }
static inline int          Ivy_ObjFaninC0( Ivy_Obj_t * pObj )     { assert( !Ivy_IsComplement(pObj) ); return pObj->fComp0;                           }
static inline int          Ivy_ObjFaninC1( Ivy_Obj_t * pObj )     { assert( !Ivy_IsComplement(pObj) ); return pObj->fComp1;                           }
static inline Ivy_Obj_t *  Ivy_ObjFanin0( Ivy_Obj_t * pObj )      { assert( !Ivy_IsComplement(pObj) ); assert(!Ivy_ObjIsCi(pObj)); return Ivy_ObjNode(pObj, pObj->Fanin0); }
static inline Ivy_Obj_t *  Ivy_ObjFanin1( Ivy_Obj_t * pObj )      { assert( !Ivy_IsComplement(pObj) ); assert(pObj->Fanin1 >  0);  return Ivy_ObjNode(pObj, pObj->Fanin1); }
static inline Ivy_Obj_t *  Ivy_ObjChild0( Ivy_Obj_t * pObj )      { assert( !Ivy_IsComplement(pObj) ); return Ivy_NotCond( Ivy_ObjFanin0(pObj), Ivy_ObjFaninC0(pObj) );   }
static inline Ivy_Obj_t *  Ivy_ObjChild1( Ivy_Obj_t * pObj )      { assert( !Ivy_IsComplement(pObj) ); return Ivy_NotCond( Ivy_ObjFanin1(pObj), Ivy_ObjFaninC1(pObj) );   }
static inline int          Ivy_ObjLevel( Ivy_Obj_t * pObj )       { assert( !Ivy_IsComplement(pObj) ); return pObj->Level;                            }
static inline int          Ivy_ObjLevelNew( Ivy_Obj_t * pObj )    { assert( !Ivy_IsComplement(pObj) ); return 1 + IVY_MAX(Ivy_ObjFanin0(pObj)->Level, Ivy_ObjFanin1(pObj)->Level); }
static inline int          Ivy_ObjWhatFanin( Ivy_Obj_t * pObj, Ivy_Obj_t * pFanin )    
{ 
    if ( Ivy_ObjFaninId0(pObj) == Ivy_ObjId(pFanin) ) return 0; 
    if ( Ivy_ObjFaninId1(pObj) == Ivy_ObjId(pFanin) ) return 1; 
    assert(0); return -1; 
}

// manipulation of fanouts of the node
static inline Vec_Int_t * Ivy_ObjFanoutArray( Ivy_Obj_t * pObj )           
{ 
    return pObj->fArray? &(Ivy_ObjMan(pObj)->vvFanouts[pObj->Fanout]) : NULL; 
}
static inline Ivy_Obj_t * Ivy_ObjFanout( Ivy_Obj_t * pObj, int i )
{
    if ( pObj->fArray )
        return Ivy_ObjNode( pObj, Vec_IntEntry(Ivy_ObjFanoutArray(pObj), i) ); 
    assert( i == 0 );
    return pObj->Fanout? Ivy_ObjNode(pObj, pObj->Fanout) : NULL;
} 
static inline int Ivy_ObjFanoutNum( Ivy_Obj_t * pObj )
{   
    if ( pObj->fArray )
        return Vec_IntSize( Ivy_ObjFanoutArray(pObj) );
    return (pObj->Fanout > 0); 
}

////////////////////////////////////////////////////////////////////////
///                             ITERATORS                            ///
////////////////////////////////////////////////////////////////////////

// iterators over nodes, PIs, POs, ANDs
#define Ivy_ManForEachObj( pMan, pObj, i )                                                  \
    for ( i = 0, pObj = pMan->pObjs; i < pMan->nObjs; i++, pObj++ )
#define Ivy_ManForEachCi( pMan, pObj, i )                                                   \
    for ( i = 0, pObj = pMan->pObjs+1; i < pMan->nCis; i++, pObj++ )
#define Ivy_ManForEachCo( pMan, pObj, i )                                                   \
    for ( i = 0, pObj = pMan->pObjs+1+pMan->nCis; i < pMan->nCos; i++, pObj++ )
#define Ivy_ManForEachNode( pMan, pObj, i )                                                 \
    for ( i = 0, pObj = pMan->pObjs+1+pMan->nCis+pMan->nCos; i < pMan->ObjIdNext; i++, pObj++ )\
        if ( !Ivy_ObjIsNode(pObj) ) {} else
// iterators over fanouts
#define Ivy_ObjForEachFanoutSlow( pObj, pFanout, i )                                        \
    for ( i = 0, pFanout = Ivy_ObjFanout(pObj,0);                                           \
          i < Ivy_ObjFanoutNum(pObj); i++, pFanout = Ivy_ObjFanout(pObj,i) )
#define Ivy_ObjForEachFanout( pObj, vFans, pFanout, i )                                     \
    for ( i = 0, vFans = Ivy_ObjFanoutArray(pObj), pFanout = vFans? Vec_IntEntry(vFans,0) : \
          (pObj->Fanout? Ivy_ObjNode(pObj, pObj->Fanout) : NULL); pFanout;                  \
          i++, pFanout = vFans? Ivy_ObjNode(pObj, Vec_IntEntry(vFans,i)) : NULL )

////////////////////////////////////////////////////////////////////////
///                    FUNCTION DECLARATIONS                         ///
////////////////////////////////////////////////////////////////////////

/*=== aigCheck.c ========================================================*/
extern int             Ivy_ManCheck( Ivy_Man_t * pMan );
/*=== aigFanout.c =======================================================*/
extern void            Ivy_ObjAddFaninFanout( Ivy_Obj_t * pFanin, Ivy_Obj_t * pFanout );
extern void            Ivy_ObjRemoveFaninFanout( Ivy_Obj_t * pFanin, Ivy_Obj_t * pFanoutToRemove );
/*=== aigMan.c ==========================================================*/
extern Ivy_Man_t *     Ivy_ManStart( int nCis, int nCos, int nNodesMax );
extern int             Ivy_ManCleanup( Ivy_Man_t * pMan );
extern void            Ivy_ManStop( Ivy_Man_t * p );
/*=== aigObj.c =========================================================*/
extern Ivy_Obj_t *     Ivy_NodeCreate( Ivy_Obj_t * pFanin0, Ivy_Obj_t * pFanin1, int fExor );
extern void            Ivy_NodeDelete_rec( Ivy_Obj_t * pNode );
extern void            Ivy_NodeDelete( Ivy_Obj_t * pNode );
extern void            Ivy_NodeConnectCo( Ivy_Obj_t * pNode, Ivy_Obj_t * pFanin );
extern void            Ivy_NodeConnect( Ivy_Obj_t * pNode, Ivy_Obj_t * pFanin0, Ivy_Obj_t * pFanin1 );
extern void            Ivy_NodeDisconnect( Ivy_Obj_t * pNode );
/*=== aigOper.c =========================================================*/
extern Ivy_Obj_t *     Ivy_And( Ivy_Obj_t * p0, Ivy_Obj_t * p1 );
extern Ivy_Obj_t *     Ivy_Or( Ivy_Obj_t * p0, Ivy_Obj_t * p1 );
extern Ivy_Obj_t *     Ivy_Exor( Ivy_Obj_t * p0, Ivy_Obj_t * p1 );
extern Ivy_Obj_t *     Ivy_Mux( Ivy_Obj_t * pC, Ivy_Obj_t * p1, Ivy_Obj_t * p0 );
extern Ivy_Obj_t *     Ivy_Miter( Vec_Ptr_t * vPairs );
/*=== aigReplace.c ======================================================*/
extern void            Ivy_ManReplaceNode( Ivy_Man_t * pMan, Ivy_Obj_t * pOld, Ivy_Obj_t * pNew, int fUpdateLevel );
/*=== aigTable.c ========================================================*/
extern Ivy_Obj_t *     Ivy_TableLookupNode( Ivy_Obj_t * p0, Ivy_Obj_t * p1, int fExor );
extern void            Ivy_TableInsertNode( Ivy_Obj_t * pNode );
extern void            Ivy_TableDeleteNode( Ivy_Obj_t * pNode );
extern void            Ivy_TableResize( Ivy_Man_t * pMan );
extern void            Ivy_TableRehash( Ivy_Man_t * pMan );
/*=== aigUtil.c =========================================================*/
extern void            Ivy_ManIncrementTravId( Ivy_Man_t * pMan );
extern int             Ivy_ObjIsMuxType( Ivy_Obj_t * pObj );
extern Ivy_Obj_t *     Ivy_ObjRecognizeMux( Ivy_Obj_t * pObj, Ivy_Obj_t ** ppObjT, Ivy_Obj_t ** ppObjE );
 
#ifdef __cplusplus
}
#endif

#endif

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////

