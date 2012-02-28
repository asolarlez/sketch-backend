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

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class

import Text.Printf

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

{# context lib="cegis" #}
{# pointer *bool_node as BoolNode newtype #}
{# pointer *NodeVector newtype #}
{# pointer *CommandLineArgs newtype #}
{# enum BNType {underscoreToCase} deriving (Show, Eq) #}



{- Marshaling functions -}
fromEnum' = fromIntegral . fromEnum

cStringArray :: [String] -> IO (Ptr CString)
cStringArray = newArray <=< mapM newCString

unpackNodeVec :: NodeVector -> IO [BoolNode]
unpackNodeVec nv = do
    l <- node_vec_size nv
    forM [1..l] (node_vec_get nv)



fromNodeVecHandle (NodeVector v) = castPtr v
{# fun node_vec_size { id `NodeVector' } -> `Int' #}
{# fun node_vec_get { id `NodeVector', `Int' } -> `BoolNode' id #}
{# fun get_nodes_by_type as get_nodes_by_type_
    { fromEnum' `BNType' } -> `NodeVector' id #}
{# fun cmdline_args as cmdline_args_
    { `Int', id `(Ptr CString)' } -> `CommandLineArgs' id #}
{# fun cl_set_global_params
    { id `CommandLineArgs' } -> `()' #}

-- trivial sugar
cmdline_args :: [String] -> IO CommandLineArgs
cmdline_args x = do
    res <- cmdline_args_ (length x) =<< cStringArray x
    res <$ cl_set_global_params res

test = do
    cmdline_args [ "--verbosity", "5",
        "/home/gatoatigrado/.sketch/tmp/miniTest1.sk/input.tmp",
        "/home/gatoatigrado/.sketch/tmp/miniTest1.sk/solution-0" ]
-- {# fun f { } -> `Int' #}
-- import Foreign.Point
