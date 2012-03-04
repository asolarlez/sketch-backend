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

-- | This module provides an API to control the CEGIS synthesis backend.
module CegisCApi.API (
    -- * Types
    BNType (..),
    NodeVector,
    BoolNodePtr,
    BooleanDAGPtr,
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
    evt_write_controls,
    evt_get_controls,
    -- ** Manipulating the DAG
    bdag_get_nodes_by_type,
    bdag_new,
    bdag_clone,
    bdag_add_new_node,
    bn_is_minimize,
    bn_get_name,
    bn_new,
    bn_clone,
    bn_set_const,

    -- *** Sugar
    -- | Example:
    --
    -- @
    --  withDag dag $ do
    --      y <- e_const v
    --      lt_node <- e_lt x y
    --      e_assert lt_node
    -- @
    bn_new_const,
    bn_assert,
    withDag,

    -- **** Comparators
    e_lt,
    e_eq,

    -- **** Logical connectives
    e_or,
    e_and,
    e_not,

    -- **** Other
    e_const,
    e_add_node,
    e_assert,

    -- * Re-exported stuff
    nullPtr
    ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import qualified Data.Map as Map
import qualified Data.List.HT as HT

import Text.Printf

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

{# context lib="cegis" #}
data BoolNode
data BooleanDAG

-- | Reference to a Boolean DAG node. Its memory is
-- managed by the Boolean DAG
{# pointer *bool_node as BoolNodePtr -> BoolNode #}

{# pointer *NodeVector newtype #}

-- | Reference to a Boolean DAG. Boolean DAG's are self-contained,
-- but their control values are statically stored by an
-- InterpreterEnvironment (I think; Armando: correct me if I'm wrong).
{# pointer *BooleanDAG as BooleanDAGPtr -> BooleanDAG #}

-- | Contains a lot of global settings (verbosity, etc.).
-- Make sure to initialize it (via 'cmdline_args')
-- before you do anything else.
{# pointer *CommandLineArgs newtype #}

-- | Manages the 2QBF synthesis problem.
{# pointer *InterpreterEnvironment newtype #}

{# enum BNType {underscoreToCase} deriving (Show, Eq) #}



{- Marshaling functions -}
fromEnum' = fromIntegral . fromEnum

cStringArray :: [String] -> IO (Ptr CString)
cStringArray = newArray <=< mapM newCString

fromCStringArray :: Integral a => a -> Ptr CString -> IO [String]
fromCStringArray (fromInteger . toInteger -> n) ptr = do
    mapM peekCAString =<< peekArray (fromInteger . toInteger $ n) ptr

fromCIntArray :: Integral a => a -> Ptr CInt -> IO [Int]
fromCIntArray (fromInteger . toInteger -> n) ptr = do
    map (fromInteger . toInteger) <$> peekArray n ptr

-- for some reason, can't type "with" into c2hs blocks
withT = with


-- marshalling vectors
unpackNodeVec :: NodeVector -> IO [BoolNodePtr]
unpackNodeVec nv = do
    l <- node_vec_size nv
    forM [0..l - 1] (node_vec_get nv)
{# fun node_vec_size { id `NodeVector' } -> `Int' #}
{# fun node_vec_get { id `NodeVector', `Int' } -> `BoolNodePtr' id #}



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
{# fun cl_get_in_name {
    id `CommandLineArgs' } -> `String' peekCAString* #}

{# fun cl_set_in_name {
    id `CommandLineArgs',
    `String' } -> `()' #}

-- | Get the output filename, as parsed by cegis command line handler.
{# fun cl_get_out_name {
    id `CommandLineArgs' } -> `String' peekCAString* #}

-- | Set the verbosity; 5 is very verbose, -1 should print nearly nothing
{# fun cl_set_verbosity {
    id `CommandLineArgs',
    `Int' } -> `()' #}



-- | Call this after 'cmdline_args'. Reads a program from the input filename,
-- creates a DAG (circuit representation), and starts solving if there are
-- any \"assert ... SKETCHES ...;\" lines present.
{# fun runDriver { } -> `()' #}

-- | Get the C-global environment generated by 'runDriver'.
{# fun getEnvt { } -> `InterpreterEnvironment' id #}

-- | Get a copy of a function's DAG.
{# fun evt_get_copy {
    id `InterpreterEnvironment',
    `String' } -> `BooleanDAGPtr' id #}

-- | DAG representing the assertion \"forall in. spec(in) == sketch(in)\". Arguments:
--
-- > evt_prepare_miter spec sketch
{# fun evt_prepare_miter {
    id `InterpreterEnvironment',
    id `BooleanDAGPtr',
    id `BooleanDAGPtr' } -> `BooleanDAGPtr' id #}

-- | Solves a DAG. Usually this DAG is the output of 'evt_prepare_miter'.
{# fun evt_assert_dag {
    id `InterpreterEnvironment', id `BooleanDAGPtr' } -> `Int' #}

-- | Returns if the sketch has a valid solution (write it out with 'evt_write_controls').
{# fun evt_is_ready {
    id `InterpreterEnvironment' } -> `Bool' toBool #}

-- | Throws an error if the environment is not ready.
evt_check_ready :: InterpreterEnvironment -> IO InterpreterEnvironment
evt_check_ready ie = go <$> evt_is_ready ie where
    go False = error "Could not resolve sketch."
    go True = ie

-- | Write the current solutions to a file (argument: filename).
{# fun evt_write_controls {
    id `InterpreterEnvironment',
    `String' } -> `()' #}

{# fun evt_get_controls as evt_get_controls_ {
    id `InterpreterEnvironment',
    alloca- `CInt' peek*,
    alloca- `Ptr (CString)' peek*,
    alloca- `Ptr CInt' peek* } -> `()' #}

-- | Get the current solution, as a map from hole names to values.
-- Use Map.toList if you want to print / iterate over all of them.
evt_get_controls
  :: InterpreterEnvironment -> IO (Map.Map String Int)
evt_get_controls evt = do
    (n, keys, val) <- evt_get_controls_ evt
    let n' :: Int = fromInteger . toInteger $ n
    keys' <- fromCStringArray n keys
    val' :: [Int] <- fromCIntArray n val
    return $ Map.fromList [(keys' !! i, val' !! i) | i <- [0..(n' - 1)]]
    -- return Map.fromList [





-- | Get a list of all nodes of a particular type from the tree.
{# fun bdag_get_nodes_by_type
    { id `BooleanDAGPtr', fromEnum' `BNType' } -> `[BoolNodePtr]' unpackNodeVec* #}

-- | Creates a new BooleanDAG.
{# fun bdag_new
    { } -> `BooleanDAGPtr' id #}

-- | Clone a DAG
{# fun bdag_clone {
    id `BooleanDAGPtr' } -> `BooleanDAGPtr' id #}

{# fun bdag_add_new_node {
    id `BooleanDAGPtr', id `BoolNodePtr' } -> `()' #} 

-- | Determine whether a control node (star) should be minimized.
{# fun bn_is_minimize { id `BoolNodePtr' } -> `Bool' toBool #}

-- | Get the name of a node.
{# fun bn_get_name { id `BoolNodePtr' } -> `String' peekCAString* #}

{# fun bn_clone {
    id `BoolNodePtr',
    fromBool `Bool' } -> `BoolNodePtr' id #}

-- | Create a new node. Arguments: DAG (required), mother (can be
-- nullPtr), father (can be nullPtr), and type (required).
{# fun bn_new {
    id `BooleanDAGPtr',
    id `BoolNodePtr',
    id `BoolNodePtr',
    fromEnum' `BNType' } -> `BoolNodePtr' id #}

-- | Set the value of a 'BnConst' node.
{# fun bn_set_const {
    id `BoolNodePtr',
    `Int' } -> `()' #}

-- | Convenience -- runs 'bn_new', then 'bn_set_const'
bn_new_const dag v = do
    n <- bn_new dag nullPtr nullPtr BnConst
    n <$ bn_set_const n v

-- | Convenience -- assert a boolean node
bn_assert dag n = bn_new dag n nullPtr BnAssert

e_const v = do
    dag <- ask
    lift $ bn_new_const dag v

-- | Add a new node, possibly from another DAG
e_add_node n = ask >>= lift . flip bdag_add_new_node n

e_unary t x = do
    dag <- ask
    lift $ bn_new dag x nullPtr t

e_binary t x y = do
    dag <- ask
    lift $ bn_new dag x y t

e_lt = e_binary BnLt
e_eq = e_binary BnEq

e_or = e_binary BnOr
e_and = e_binary BnAnd

e_not = e_unary BnNot

e_assert n = do
    dag <- ask
    lift $ bn_assert dag n

withDag = flip runReaderT
