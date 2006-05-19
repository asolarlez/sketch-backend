/**CFile****************************************************************

  FileName    [abcRewrite.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [Network and node package.]

  Synopsis    [Technology-independent resynthesis of the AIG based on DAG aware rewriting.]

  Author      [Alan Mishchenko]
  
  Affiliation [UC Berkeley]

  Date        [Ver. 1.0. Started - June 20, 2005.]

  Revision    [$Id$]

***********************************************************************/

#include "abc.h"
#include "rwr.h"
#include "dec.h"

/*
    The ideas realized in this package are inspired by the paper:
    Per Bjesse, Arne Boralv, "DAG-aware circuit compression for 
    formal verification", Proc. ICCAD 2004, pp. 42-49.
*/

////////////////////////////////////////////////////////////////////////
///                        DECLARATIONS                              ///
////////////////////////////////////////////////////////////////////////

static Cut_Man_t * Abc_NtkStartCutManForRewrite( Abc_Ntk_t * pNtk );
static void        Abc_NodePrintCuts( Abc_Obj_t * pNode );
static void        Abc_ManShowCutCone( Abc_Obj_t * pNode, Vec_Ptr_t * vLeaves );

////////////////////////////////////////////////////////////////////////
///                     FUNCTION DEFINITIONS                         ///
////////////////////////////////////////////////////////////////////////

/**Function*************************************************************

  Synopsis    [Performs incremental rewriting of the AIG.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
int Abc_NtkRewrite( Abc_Ntk_t * pNtk, int fUpdateLevel, int fUseZeros, int fVerbose )
{
    ProgressBar * pProgress;
    Cut_Man_t * pManCut;
    Rwr_Man_t * pManRwr;
    Abc_Obj_t * pNode;
    int i, nNodes, nGain;
    int clk, clkStart = clock();

    assert( Abc_NtkIsStrash(pNtk) );
    // cleanup the AIG
    Abc_AigCleanup(pNtk->pManFunc);
    // start the rewriting manager
    pManRwr = Rwr_ManStart( 0 );
    if ( pManRwr == NULL )
        return 0;
    // compute the reverse levels if level update is requested
    if ( fUpdateLevel )
        Abc_NtkStartReverseLevels( pNtk );
    // start the cut manager
clk = clock();
    pManCut = Abc_NtkStartCutManForRewrite( pNtk );
Rwr_ManAddTimeCuts( pManRwr, clock() - clk );
    pNtk->pManCut = pManCut;

    // resynthesize each node once
    nNodes = Abc_NtkObjNumMax(pNtk);
    pProgress = Extra_ProgressBarStart( stdout, nNodes );
    Abc_NtkForEachNode( pNtk, pNode, i )
    {
        Extra_ProgressBarUpdate( pProgress, i, NULL );
        // stop if all nodes have been tried once
        if ( i >= nNodes )
            break;
        // skip the constant node
        if ( Abc_NodeIsConst(pNode) )
            continue;
        // skip persistant nodes
        if ( Abc_NodeIsPersistant(pNode) )
            continue;
        // skip the nodes with many fanouts
        if ( Abc_ObjFanoutNum(pNode) > 1000 )
            continue;
        // for each cut, try to resynthesize it
        nGain = Rwr_NodeRewrite( pManRwr, pManCut, pNode, fUpdateLevel, fUseZeros );
        if ( nGain > 0 || nGain == 0 && fUseZeros )
        {
            Dec_Graph_t * pGraph = Rwr_ManReadDecs(pManRwr);
            int fCompl           = Rwr_ManReadCompl(pManRwr);

            
//            if ( nGain > 0 )
//                Abc_ManShowCutCone( pNode, Rwr_ManReadLeaves(pManRwr) );

/*
            if ( nGain > 0 )
            { // print stats on the MFFC
                extern void Abc_NodeMffsConeSuppPrint( Abc_Obj_t * pNode );
                printf( "Node %6d : Gain = %4d  ", pNode->Id, nGain );
                Abc_NodeMffsConeSuppPrint( pNode );
            }
*/
            // complement the FF if needed
            if ( fCompl ) Dec_GraphComplement( pGraph );
clk = clock();
            Dec_GraphUpdateNetwork( pNode, pGraph, fUpdateLevel, nGain );
Rwr_ManAddTimeUpdate( pManRwr, clock() - clk );
            if ( fCompl ) Dec_GraphComplement( pGraph );
//    {
//        extern int s_TotalChanges;
//        s_TotalChanges++;
//    }
        }
    }
    Extra_ProgressBarStop( pProgress );
Rwr_ManAddTimeTotal( pManRwr, clock() - clkStart );
    // print stats
    if ( fVerbose )
        Rwr_ManPrintStats( pManRwr );
//        Rwr_ManPrintStatsFile( pManRwr );
    // delete the managers
    Rwr_ManStop( pManRwr );
    Cut_ManStop( pManCut );
    pNtk->pManCut = NULL;
    // put the nodes into the DFS order and reassign their IDs
    Abc_NtkReassignIds( pNtk );
//    Abc_AigCheckFaninOrder( pNtk->pManFunc );
    // fix the levels
    if ( fUpdateLevel )
        Abc_NtkStopReverseLevels( pNtk );
    else
        Abc_NtkGetLevelNum( pNtk );
    // check
    if ( !Abc_NtkCheck( pNtk ) )
    {
        printf( "Abc_NtkRewrite: The network check has failed.\n" );
        return 0;
    }
    return 1;
}


/**Function*************************************************************

  Synopsis    [Starts the cut manager for rewriting.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
Cut_Man_t * Abc_NtkStartCutManForRewrite( Abc_Ntk_t * pNtk )
{
    static Cut_Params_t Params, * pParams = &Params;
    Cut_Man_t * pManCut;
    Abc_Obj_t * pObj;
    int i;
    // start the cut manager
    memset( pParams, 0, sizeof(Cut_Params_t) );
    pParams->nVarsMax  = 4;     // the max cut size ("k" of the k-feasible cuts)
    pParams->nKeepMax  = 250;   // the max number of cuts kept at a node
    pParams->fTruth    = 1;     // compute truth tables
    pParams->fFilter   = 1;     // filter dominated cuts
    pParams->fSeq      = 0;     // compute sequential cuts
    pParams->fDrop     = 0;     // drop cuts on the fly
    pParams->fVerbose  = 0;     // the verbosiness flag
    pParams->nIdsMax   = Abc_NtkObjNumMax( pNtk );
    pManCut = Cut_ManStart( pParams );
    if ( pParams->fDrop )
        Cut_ManSetFanoutCounts( pManCut, Abc_NtkFanoutCounts(pNtk) );
    // set cuts for PIs
    Abc_NtkForEachCi( pNtk, pObj, i )
        if ( Abc_ObjFanoutNum(pObj) > 0 )
            Cut_NodeSetTriv( pManCut, pObj->Id );
    return pManCut;
}

/**Function*************************************************************

  Synopsis    [Prints the cuts at the nodes.]

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Abc_NodePrintCuts( Abc_Obj_t * pNode )
{
    Vec_Ptr_t * vCuts;
    Cut_Cut_t * pCut;
    int k;

    printf( "\nNode %s\n", Abc_ObjName(pNode) );
    vCuts = (Vec_Ptr_t *)pNode->pCopy;
    Vec_PtrForEachEntry( vCuts, pCut, k )
    {
        Extra_PrintBinary( stdout, (unsigned *)&pCut->uSign, 16 ); 
        printf( "   " );
        Cut_CutPrint( pCut, 0 );   
        printf( "\n" );
    }
}


/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Abc_ManRewritePrintDivs( Vec_Ptr_t * vDivs, int nLeaves )
{
    Abc_Obj_t * pFanin, * pNode, * pRoot;
    int i, k;
    pRoot = Vec_PtrEntryLast(vDivs);
    // print the nodes
    Vec_PtrForEachEntry( vDivs, pNode, i )
    {
        if ( i < nLeaves )
        {
            printf( "%6d : %c\n", pNode->Id, 'a'+i );
            continue;
        }
        printf( "%6d : %2d = ", pNode->Id, i );
        // find the first fanin
        Vec_PtrForEachEntry( vDivs, pFanin, k )
            if ( Abc_ObjFanin0(pNode) == pFanin )
                break;
        if ( k < nLeaves )
            printf( "%c", 'a' + k );
        else
            printf( "%d", k );
        printf( "%s ", Abc_ObjFaninC0(pNode)? "\'" : "" );
        // find the second fanin
        Vec_PtrForEachEntry( vDivs, pFanin, k )
            if ( Abc_ObjFanin1(pNode) == pFanin )
                break;
        if ( k < nLeaves )
            printf( "%c", 'a' + k );
        else
            printf( "%d", k );
        printf( "%s ", Abc_ObjFaninC1(pNode)? "\'" : "" );
        if ( pNode == pRoot )
            printf( " root" );
        printf( "\n" );
    }
    printf( "\n" );
}

/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Abc_ManShowCutCone_rec( Abc_Obj_t * pNode, Vec_Ptr_t * vDivs )
{
    if ( Abc_NodeIsTravIdCurrent(pNode) )
        return;
    Abc_NodeSetTravIdCurrent(pNode);
    Abc_ManShowCutCone_rec( Abc_ObjFanin0(pNode), vDivs );
    Abc_ManShowCutCone_rec( Abc_ObjFanin1(pNode), vDivs );
    Vec_PtrPush( vDivs, pNode );
}

/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Abc_ManShowCutCone( Abc_Obj_t * pNode, Vec_Ptr_t * vLeaves )
{
    Abc_Ntk_t * pNtk = pNode->pNtk;
    Abc_Obj_t * pObj;
    Vec_Ptr_t * vDivs;
    int i;
    vDivs = Vec_PtrAlloc( 100 );
    Abc_NtkIncrementTravId( pNtk );
    Vec_PtrForEachEntry( vLeaves, pObj, i )
    {
        Abc_NodeSetTravIdCurrent( Abc_ObjRegular(pObj) );
        Vec_PtrPush( vDivs, Abc_ObjRegular(pObj) );
    }
    Abc_ManShowCutCone_rec( pNode, vDivs );
    Abc_ManRewritePrintDivs( vDivs, Vec_PtrSize(vLeaves) );
    Vec_PtrFree( vDivs );
}

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////


