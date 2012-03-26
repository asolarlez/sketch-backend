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

module HsCegis where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.List as List
import qualified Data.Map as Map

import System.Environment
import System.Exit

import Text.Printf

import CegisCApi.API
import CegisCApi.HighLevelConcurrent
import CegisCApi.Helper

data HsCegisArgs = HsCegisArgs {
    minimize :: Bool,
    num_solutions :: Integer }
defArgs = HsCegisArgs {
    minimize = False,
    num_solutions = 1 }

reparse_args :: [String] -> (HsCegisArgs, [String])
reparse_args l = check $ go l (defArgs, []) where
    go ("--num-solutions":x:xs) = go xs . first (\args -> args { num_solutions = read x })
    go ("--minimize":xs) = go xs . first (\args -> args { minimize = True })
    go (x:xs) = second (x:) . go xs
    go [] = id
    check x@(HsCegisArgs { minimize, num_solutions }, _)
        | minimize && num_solutions /= 1 = error "use either --minimize or --num-solutions"
        | otherwise = x

test_args = [ "--verbosity", "5",
    -- "--minimize",
    "--num-solutions", "2",
    "-o", "/home/gatoatigrado/.sketch/tmp/miniTest210MinRepeat.sk/solution",
    "/home/gatoatigrado/.sketch/tmp/miniTest210MinRepeat.sk/input.tmp" ]

minimize_sketch
  :: InterpreterEnvironment
      -> CommandLineArgs
      -> [(SketchSpec, BooleanDAGPtr)]
      -> String
      -> IO ()
minimize_sketch e cli ss_miters fn = do
    putStrLn "\n\nstarting minimize routine"
    evt_check_ready e
    cl_set_verbosity cli (-1)
    cl_set_global_params cli

    let solve_all_dags = do
            forM ss_miters $ \(_, dag) -> evt_assert_dag e dag
            evt_is_ready e

    -- extract all minimize nodes from all dags
    min_nodes <- forM ss_miters $ \(ss, dag) -> do
        nodes <- filterM bn_is_minimize =<< bdag_get_nodes_by_type dag BnCtrl
        putStrLn $ printf "number of minimize control in dag nodes in sketch %s: %d"
            (sketch ss) (length nodes)
        return (ss, dag, nodes)

    -- minimize
    go min_nodes where
        go [] = return ()
        go ((ss, dag, []):xs) = go xs
        go z@((ss, dag, (n:ns)):xs) = do
            evt_check_ready e
            tid <- myThreadId
            putStrLn $ printf "Everything is SAT on thread '%s', minimizing..." (show tid)
            {---------------------------------------------------
            -- v <- go' ss dag n
            -- if v then do
            --     evt_write_controls e fn
            --     go z -- continue minimizing same hole
            -- else do
            --     putStrLn "failed to minimize value."
            --     set_const dag n -- fix the minimum value
            --     go ((ss, dag, ns):xs) -- minimize a different one
            ----------------------------------------------------}

            fork_if (go' ss dag n)
                (do -- putStrLn "minimized value!"
                    evt_write_controls e fn
                    go z) -- continue minimizing same hole
                (do putStrLn "failed to minimize value."
                    set_const dag n -- fix the minimum value
                    go ((ss, dag, ns):xs)) -- minimize a different one

        -- Try to minimize; return True if succeeded, False otherwise
        go' ss dag n = do
            tid <- myThreadId
            putStrLn $ printf "minimizing on thread '%s'" (show tid)
            v <- get_ctrl_value n
            case v of
                Just x -> do
                    -- putStrLn $ printf "Value for node: %d" x
                    (dag', n') <- get_dag_node_copy dag n
                    evt_assert_dag e =<< get_minimize_dag n' x
                    putStrLn "go' checking value..."
                    val <- evt_is_ready e -- if True, then it succeeded in minimization.
                    putStrLn $ "go' done checking value, got " ++ show val
                    return val
                Nothing -> do
                    -- putStrLn $ printf "ERROR -- control '%s' not found in map!" nme
                    return False

        get_ctrl_value :: BoolNodePtr -> IO (Maybe Int)
        get_ctrl_value n = do
            ctrl_map <- evt_get_controls e
            nme <- bn_get_name n
            return $ Map.lookup nme ctrl_map
        
        -- FIXME: this is inefficient. Copies entire DAG, then finds
        -- the copy of control node _n_ in the copied DAG.
        get_dag_node_copy dag n = do
            dag' <- bdag_clone dag
            nme <- bn_get_name n
            let f x = (== nme) <$> bn_get_name x
            nodes <- filterM f =<< bdag_get_nodes_by_type dag BnCtrl
            return (dag', nodes !! 0)

        -- get a new DAG representing "n < v"
        get_minimize_dag n v = do
            dag <- bdag_new
            withDag dag $ do
                e_add_node n
                vn <- e_const v
                lt <- e_lt n vn
                e_assert lt
            return dag

        set_const dag n = do
            tid <- myThreadId
            putStrLn $ printf "pre-checking DAG on thread '%s'" (show tid)
            evt_assert_dag e dag
            evt_check_ready e
            putStrLn "done pre-checking"

            (Just v) <- get_ctrl_value n
            withDag dag $ do
                v_n <- e_const v
                eq_n <- e_eq n v_n
                e_assert eq_n

            putStrLn "checking DAG..."
            evt_assert_dag e dag
            evt_check_ready e

get_another_solution :: InterpreterEnvironment
    -> CommandLineArgs
    -> [(SketchSpec, BooleanDAGPtr)]
    -> String
    -> IO Bool
get_another_solution e cli ss_miters fn = do
    cl_set_verbosity cli (-1)
    cl_set_global_params cli

    -- lookup values of current controls
    ctrl_names <- List.nub . concat <$>
        (forM ss_miters $ \(ss, dag) -> do
            dag_copy <- bdag_clone dag
            nodes <- bdag_get_nodes_by_type dag_copy BnCtrl
            zip nodes <$> mapM bn_get_name nodes)
    ctrl_map <- evt_get_controls e
    let ctrl_values = map (id &&& flip Map.lookup ctrl_map . snd) ctrl_names 
    seq ctrl_values (return ())

    -- create "h /= value(h)" ∀ holes h
    new_dag <- bdag_new
    neqs <- forM ctrl_values $ \((n, name), Just v) -> do
        withDag new_dag $ do
            e_add_node n
            vn <- e_const v
            e_not =<< e_eq n vn

    if neqs == [] then
        return False
    else do
        print "creating big or..."
        bigor <- foldM (\x y -> withDag new_dag $ e_or x y) (head neqs) (tail neqs)
        withDag new_dag $ e_assert bigor
        print "asserting new dag...."
        evt_assert_dag e new_dag

        evt_is_ready e >>= rv where
            rv False = return False
            rv True = True <$ evt_write_controls e fn

main = do
    (args, backend_args) <- reparse_args <$> getArgs
    cli <- cmdline_args backend_args

    -- strip "assert ... sketches ..." from input,
    -- set input to that new file
    nme <- cl_get_in_name cli
    (nme', sketches) <- strip_sketches_lines nme
    cl_set_in_name cli nme'

    outname <- (\x (n :: Integer) -> printf "%s-%03d" x n) <$>
        cl_get_out_name cli

    -- build DAGs from the input functions
    runDriver
    e <- getEnvt

    -- resolve all of the DAGs, return pairs of miters and sketch-specs
    ss_miters <- forM sketches $ \ss@(SketchSpec { sketch, spec }) -> do
        sk_copy <- evt_get_copy e sketch
        spec_copy <- evt_get_copy e spec
        bd <- evt_prepare_miter e spec_copy sk_copy
        evt_assert_dag e bd
        return (ss, bd)

    -- write the first solution out
    evt_check_ready e
    evt_write_controls e (outname 0)

    if (minimize args) then
        minimize_sketch e cli ss_miters (outname 0)
    else return ()

    forM [1 .. num_solutions args - 1] $ \j -> do
        get_another_solution e cli ss_miters (outname j)

    print "done"
