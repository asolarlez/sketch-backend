/**CFile****************************************************************

  FileName    [cmdHist.c]

  SystemName  [ABC: Logic synthesis and verification system.]

  PackageName [Command processing package.]

  Synopsis    [Procedures working with history.]

  Author      [Alan Mishchenko]
  
  Affiliation [UC Berkeley]

  Date        [Ver. 1.0. Started - June 20, 2005.]

  Revision    [$Id$]

***********************************************************************/

#include "mainInt.h"
#include "cmd.h"
#include "cmdInt.h"

////////////////////////////////////////////////////////////////////////
///                        DECLARATIONS                              ///
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
///                     FUNCTION DEFINITIONS                         ///
////////////////////////////////////////////////////////////////////////

/**Function*************************************************************

  Synopsis    []

  Description []
               
  SideEffects []

  SeeAlso     []

***********************************************************************/
void Cmd_HistoryAddCommand(	Abc_Frame_t * p, char * command )
{
    char Buffer[500];
    strcpy( Buffer, command );
    if ( command[strlen(command)-1] != '\n' )
        strcat( Buffer, "\n" );
    Vec_PtrPush( p->aHistory, Extra_UtilStrsav(Buffer) );
}

////////////////////////////////////////////////////////////////////////
///                       END OF FILE                                ///
////////////////////////////////////////////////////////////////////////
