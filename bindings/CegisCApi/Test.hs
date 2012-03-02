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

module CegisCApi.Test where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class

import CegisCApi.API
import CegisCApi.HighLevelConcurrent
import CegisCApi.Helper
import System.Exit

import Text.Printf

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
    "--minimize",
    -- "--num-solutions", "2",
    "-o", "/home/gatoatigrado/.sketch/tmp/miniTest1.sk/solution",
    "/home/gatoatigrado/.sketch/tmp/miniTest1.sk/input.tmp" ]

minimize_sketch
  :: InterpreterEnvironment -> CommandLineArgs -> [(SketchSpec, BooleanDAG)] -> String -> IO ()
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
    go min_nodes >> throw ThreadKilled where
        go [] = return ()
        go ((ss, dag, []):xs) = go xs
        go z@((ss, dag, (n:ns)):xs) =
            fork_if (go' ss dag n)
                (go z) -- continue minimizing same hole
                (do set_const n -- fix the minimum value
                    go ((ss, dag, ns):xs)) -- minimize a different one
        go' ss dag n = return False
        set_const n = return () -- TBD

    {---------------------------------------------------
    -- forM min_nodes
    --     lastSuccessfulValue = self.envt.currentControls[hole.get_name()]
    --     while(toIterate):
    --         self.writeSolutions(num=0)
    --         print 'lastSuccessfulValue = ', lastSuccessfulValue
    --         newDag = BooleanDAG()
    --         bgProblem = self.bDag.clone()
    --         ctrlNodes = bgProblem.getNodesByType(bool_node.CTRL)
    --         hole_clone = None
    --         for i in xrange(len(ctrlNodes)):
    --             if(ctrlNodes[i].get_name() == hole.get_name()):
    --                 hole_clone = ctrlNodes[i]
    --                 break
    --         newDag.addNewNode(hole_clone)
    --         constNode = newDag.new_node(None, None, bool_node.CONST)
    --         constNode.setVal(lastSuccessfulValue)
    --         ltNode = newDag.new_node(hole_clone, constNode, bool_node.LT)
    --         assertNode = newDag.new_node(ltNode, None, bool_node.ASSERT)
    --         self.envt.assertDAG_wrapper(newDag)
    --         if self.envt.status == InterpreterEnvironment.STATUS.READY:
    --             lastSuccessfulValue = self.envt.currentControls[hole.get_name()]
    --         elif self.envt.status == InterpreterEnvironment.STATUS.UNSAT:
    --             toIterate = False
    --         else:
    --             assert False, "Missing case for InterpreterEnvironment.STATUS enum"     
    --     constNode = self.bDag.new_node(None, None, bool_node.CONST)
    --     constNode.setVal(lastSuccessfulValue)
    --     eqNode = self.bDag.new_node(hole, constNode, bool_node.EQ)
    --     self.bDag.new_node(eqNode, None, bool_node.ASSERT)
    --     print('Minimum value of hole ' + str(hole.get_name()) + ' is ' + str(lastSuccessfulValue))
    ----------------------------------------------------}

    {---------------------------------------------------
    -- assert self.envt.status == InterpreterEnvironment.STATUS.READY, "Can't minimize the buggy sketch"
    -- self.cmdLineArgs.verbosity = -1
    -- self.cmdLineArgs.setPARAMS()
    -- ctrlNodes = self.bDag.getNodesByType(bool_node.CTRL)
    -- toMinimizeNodes = []
    -- for i in xrange(len(ctrlNodes)):
    --     if ctrlNodes[i].get_toMinimize():
    --         toMinimizeNodes.append(ctrlNodes[i])
    -- for i in xrange(len(toMinimizeNodes)):
    --     self.minimizeHole(toMinimizeNodes[i])
    ----------------------------------------------------}



test = do
    let (args, backend_args) = reparse_args test_args
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
    evt_print_controls e (outname 0)

    if (minimize args) then
        minimize_sketch e cli ss_miters (outname 0)
    else return ()

    -- forM [2..num_solutions args] $ \j -> do
        -- ...

    -- print "num solutions"
    -- print (num_solutions args)

    print "done"
