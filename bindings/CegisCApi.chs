{-# LANGUAGE Arrows,
             BangPatterns,
             ConstraintKinds,
             DeriveDataTypeable,
             EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
             ForeignFunctionInterface,
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

import Foreign.Ptr
import Foreign.C.Types

{# context lib="cegis" #}
{# pointer *bool_node as BoolNode newtype #}
{# enum BNType {underscoreToCase} deriving (Show, Eq) #}

fromEnum' = fromIntegral . fromEnum

{# pointer *NodeVector newtype #}
fromNodeVecHandle (NodeVector v) = castPtr v
{# fun node_vec_size { id `NodeVector' } -> `Int' #}
{# fun node_vec_get { id `NodeVector', `Int' } -> `BoolNode' id #}
{# fun get_nodes_by_type { fromEnum' `BNType' } -> `NodeVector' id #}

unpackNodeVec :: NodeVector -> IO [BoolNode]
unpackNodeVec nv = do
    l <- node_vec_size nv
    forM [1..l] (node_vec_get nv)

-- {# fun f { } -> `Int' #}
-- import Foreign.Point
