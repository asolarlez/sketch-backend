module CegisCApi.HighLevelConcurrent (forkos_try) where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.IORef

import Text.Printf

-- | Takes a command x and continuation rest;
-- if x is (Just y), passes (Just y) to the continuation
-- otherwise, rolls back state and passes Nothing to the continuation
forkos_try :: IO (Maybe α) -> (Maybe α -> IO ()) -> IO ()
forkos_try x rest = do
    mv <- newEmptyMVar :: IO (MVar Bool)
    forkOS (do
        v <- x
        case v of
            Nothing -> putMVar mv False >> throw ThreadKilled
            (Just _) -> putMVar mv True >> rest v)
    v <- takeMVar mv
    if v then return () else rest Nothing

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
