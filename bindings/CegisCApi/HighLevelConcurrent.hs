-- Copyright 2012 gatoatigrado (nicholas tung) [ntung at ntung]
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0 .

module CegisCApi.HighLevelConcurrent (forkos_try, fork_if) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.IORef

import System.Exit
import System.Posix.Process

import Text.Printf

-- | Takes a command x and continuation rest;
-- if x is (Just y), passes (Just y) to the continuation
-- otherwise, rolls back state and passes Nothing to the continuation
forkos_try :: IO (Maybe α) -> (Maybe α -> IO ()) -> IO ()
forkos_try x rest = do
    child_pid <- forkProcess (do
        pid <- getProcessID
        putStrLn $ printf "[forkos_try] running on process '%s' ..." (show pid)
        v <- x
        putStrLn $ printf "[forkos_try] done running on process '%s'..." (show pid)
        case v of
            Nothing -> exitImmediately (ExitFailure 214)
            (Just _) -> rest v >> exitImmediately ExitSuccess)
    c <- getProcessStatus True False child_pid
    case c of
        (Just (Exited (ExitFailure 214))) -> do
            -- putStrLn "child failed, running backup"
            rest Nothing
        (Just (Exited (ExitSuccess))) -> do
            -- putStrLn "child succeeded"
            return ()
        {---------------------------------------------------
        -- (Just (Terminated _)) -> do
        --     -- putStrLn "child succeeded"
        --     return ()
        ----------------------------------------------------}
        (Just (Terminated 11)) -> do
            tid <- myThreadId
            putStrLn $ printf
                "[forkos_try] Segmentation fault on process '%s'"
                (show child_pid)
        _ -> do
            tid <- myThreadId
            putStrLn $ printf
                "[forkos_try] unknown error '%s' on process '%s'"
                (show c) (show child_pid)

-- | Common case usage of 'forkos_try'.
fork_if :: IO Bool -> IO () -> IO () -> IO ()
fork_if c t e = forkos_try (toMaybe <$> c) go where
    toMaybe True = Just ()
    toMaybe False = Nothing
    go (Just ()) = t
    go (Nothing) = e

-- Mock function that does some imperative operation,
-- which may succeed or fail.
decrement :: IORef Int -> IO (Maybe ())
decrement x = do
    xv <- readIORef x
    -- IMPORTANT -- messes up state during calculation
    writeIORef x (-100000)

    if (xv > 0) then do
        writeIORef x (xv - 1)
        return (Just ())
    else
        return Nothing

test = do
    x <- newIORef (3 :: Int)
    -- NOTE: If this looks scary, just mentally remove the "$ \_ -> do" parts,
    -- and de-indent all following lines.
    forkos_try (decrement x) $ \_ -> do
        forkos_try (decrement x) $ \_ -> do
            forkos_try (decrement x) $ \_ -> do
                forkos_try (decrement x) $ \_ -> do
                    mytid <- myThreadId
                    putStrLn $ printf "final thread ID: %s" (show mytid)
                    xv <- readIORef x
                    putStrLn $ printf "value of x: %d" xv
