-- Copyright 2012 gatoatigrado (nicholas tung) [ntung at ntung]
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0 .

-- enable to remove excess imports {-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE Arrows,
             BangPatterns,
             ConstraintKinds,
             DeriveDataTypeable,
             EmptyDataDecls,
             FlexibleContexts,
             FlexibleInstances,
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

module CegisCApi.Helper where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.List.HT as HT

import Text.Regex.TDFA
import Text.Printf

import CegisCApi.API
import CegisCApi.HighLevelConcurrent

-- | Pair containing a sketch and a specification
data SketchSpec = SketchSpec {
    sketch :: String,
    spec :: String }
    deriving (Eq, Show, Ord)

-- | Strip \"assert ... SKETCHES ...\" from the file.
-- Effectively prevents runDriver from doing anything
-- until you call 'evt_prepare_miter' later. Common usage:
--
-- @
--     in_nme <- cl_get_in_name cli
--     (in_nme', sketches) <- strip_sketches_lines in_nme
--     cl_set_in_name cli in_nme' -- use the new name
-- @
strip_sketches_lines fn@((++ ".fcns-only") -> fn') = do
    ls <- lines <$> readFile fn
    let re = "^assert ([a-zA-Z0-9_]+) SKETCHES ([a-zA-Z0-9_]+);$"
        (sketches, ls') = HT.partition (=~ re) ls
    writeFile fn' (unlines ls')
    return (fn', map (untuple . (=~ re)) sketches)
    where untuple [[_, sketch, spec]] = SketchSpec sketch spec
          untuple x = error ("untuple -- unexpected input " ++ show x)

