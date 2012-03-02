{-# LANGUAGE Arrows,
             BangPatterns,
             ConstraintKinds,
             DeriveDataTypeable,
             EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances, ForeignFunctionInterface,
             FunctionalDependencies,
             GADTs,
             GeneralizedNewtypeDeriving,
             ImpredicativeTypes,
             MultiParamTypeClasses,
             NamedFieldPuns,
             NoMonomorphismRestriction,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             TemplateHaskell,
             TupleSections,
             TypeFamilies,
             TypeOperators,
             TypeSynonymInstances,
             ViewPatterns #-}

module CegisCApi.API (
    -- * Types
    BNType (..),
    BoolNode,
    NodeVector,
    BooleanDAG,
    CommandLineArgs,
    InterpreterEnvironment,
    -- * API
    -- ** Initialize the command line
    cmdline_args,
    cl_get_in_name,
    cl_set_in_name,
    cl_get_out_name,
    cl_set_verbosity,
    cl_set_global_params,
    -- ** Start running the backend
    runDriver,
    getEnvt,
    -- ** Core synthesis commands
    evt_get_copy,
    evt_prepare_miter,
    evt_assert_dag,
    evt_is_ready,
    evt_check_ready,
    evt_print_controls,
    -- ** Manipulating the DAG
    bdag_get_nodes_by_type,
    bn_is_minimize
    ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.List.HT as HT

import Text.Printf

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

{# context lib="cegis" #}
{# pointer *bool_node as BoolNode newtype #}
{# pointer *NodeVector newtype #}
{# pointer *BooleanDAG newtype #}
{# pointer *CommandLineArgs newtype #}
{# pointer *InterpreterEnvironment newtype #}
{# enum BNType {underscoreToCase} deriving (Show, Eq) #}



{- Marshaling functions -}
fromEnum' = fromIntegral . fromEnum
toBool = (/= 0)

cStringArray :: [String] -> IO (Ptr CString)
cStringArray = newArray <=< mapM newCString




-- | You probably want to run this function first.
--   Pass it an array of arguments, like
--
-- > cmdline_args [ "--verbosity", "5", "infile", "outfile" ]
cmdline_args :: [String] -> IO CommandLineArgs
cmdline_args x = do
    res <- cmdline_args_ (length x) =<< cStringArray x
    res <$ cl_set_global_params res

-- helpers
{# fun cmdline_args as cmdline_args_
    { `Int', id `(Ptr CString)' } -> `CommandLineArgs' id #}
{# fun cl_set_global_params
    { id `CommandLineArgs' } -> `()' #}

-- | Get the input filename, as parsed by cegis command line handler.
{# fun cl_get_in_name { id `CommandLineArgs' } -> `String' peekCAString* #}

{# fun cl_set_in_name { id `CommandLineArgs', `String' } -> `()' #}

-- | Get the output filename, as parsed by cegis command line handler.
{# fun cl_get_out_name { id `CommandLineArgs' } -> `String' peekCAString* #}

-- | Set the verbosity; 5 is very verbose, -1 should print nearly nothing
{# fun cl_set_verbosity { id `CommandLineArgs', `Int' } -> `()' #}



-- | Call this after 'cmdline_args'. Reads a program from the input filename,
-- creates a DAG (circuit representation), and starts solving if there are
-- any \"assert ... SKETCHES ...;\" lines present.
{# fun runDriver { } -> `()' #}

-- | Get the C-global environment generated by 'runDriver'
{# fun getEnvt { } -> `InterpreterEnvironment' id #}

-- | Get a copy of a function's DAG
{# fun evt_get_copy { id `InterpreterEnvironment', `String' } -> `BooleanDAG' id #}

-- | DAG representing the assertion \"forall in. spec(in) == sketch(in)\". Arguments:
--
-- > evt_prepare_miter spec sketch
{# fun evt_prepare_miter {
    id `InterpreterEnvironment', id `BooleanDAG', id `BooleanDAG' }
    -> `BooleanDAG' id #}

-- | Solves a DAG, usually the output of 'evt_prepare_miter'
{# fun evt_assert_dag {
    id `InterpreterEnvironment', id `BooleanDAG' } -> `Int' #}

-- | Returns if the sketch has a valid solution (write it out with 'evt_print_controls')
{# fun evt_is_ready { id `InterpreterEnvironment' } -> `Bool' toBool #}

-- | Throws an error if the environment is not ready
evt_check_ready :: InterpreterEnvironment -> IO InterpreterEnvironment
evt_check_ready ie = go <$> evt_is_ready ie where
    go False = error "Could not resolve sketch."
    go True = ie

-- | Write the current solutions to a filename
{# fun evt_print_controls {
    id `InterpreterEnvironment', `String' } -> `()' #}



-- marshalling vectors
unpackNodeVec :: NodeVector -> IO [BoolNode]
unpackNodeVec nv = do
    l <- node_vec_size nv
    forM [0..l - 1] (node_vec_get nv)
{# fun node_vec_size { id `NodeVector' } -> `Int' #}
{# fun node_vec_get { id `NodeVector', `Int' } -> `BoolNode' id #}

-- | Get a list of all nodes of a particular type from the tree
{# fun bdag_get_nodes_by_type
    { id `BooleanDAG', fromEnum' `BNType' } -> `[BoolNode]' unpackNodeVec* #}

-- | Determine whether a control node (star) should be minimized
{# fun bn_is_minimize { id `BoolNode' } -> `Bool' toBool #}

