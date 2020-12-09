{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module New34 where

import Control.Monad (forM_, when)
import Data.IORef (IORef, readIORef, writeIORef)
import GHC.Conc (TVar, readTVarIO, registerDelay)
import System.IO
  ( Handle,
    SeekMode (AbsoluteSeek),
    hGetLine,
    hPutStr,
    hSeek,
  )

data a :+ b
  = L a
  | R b
  deriving (Show)

data a :* b = a :* b deriving (Show)

cache :: a -> [a] -> [a]
cache a as = a : as

cache2 :: a :+ b -> [a] :* [b] -> [a] :* [b]
cache2 (R b) (cs :* bs) = cs :* (b : bs)
cache2 (L a) (cs :* bs) = (a : cs) :* bs

cache3 ::
  a :+ b :+ c ->
  [a] :* [b] :* [c] ->
  [a] :* [b] :* [c]
cache3 (R c) (as :* bs :* cs) = as :* bs :* (c : cs)
cache3 (L (R b)) (as :* bs :* cs) = as :* (b : bs) :* cs
cache3 (L (L a)) (as :* bs :* cs) = (a : as) :* bs :* cs

cache4 ::
  a :+ b :+ c :+ d ->
  [a] :* [b] :* [c] :* [d] ->
  [a] :* [b] :* [c] :* [d]
cache4 (R d) (as :* bs :* cs :* ds) = as :* bs :* cs :* (d : ds)
cache4 (L (R c)) (as :* bs :* cs :* ds) = as :* bs :* (c : cs) :* ds
cache4 (L (L (R b))) (as :* bs :* cs :* ds) = as :* (b : bs) :* cs :* ds
cache4 (L (L (L a))) (as :* bs :* cs :* ds) = (a : as) :* bs :* cs :* ds

type family IC a where
  IC (a :* b) = (IC a :+ b)
  IC a = a

data State a = State
  { put :: a -> IO (),
    get :: IO a
  }

ioRefBackendState :: IORef a -> State a
ioRefBackendState ref =
  State
    { put = writeIORef ref,
      get = readIORef ref
    }

fileBackendState :: forall a. (Show a, Read a) => Handle -> State a
fileBackendState handle =
  State
    { put = \v -> hSeek handle AbsoluteSeek 0 >> hPutStr handle (show v),
      get = hSeek handle AbsoluteSeek 0 >> hGetLine handle >>= \v -> return (read v)
    }

dbBackendState :: State a
dbBackendState =
  State
    { put = undefined,
      get = undefined
    }

type StateNormalFun input output state =
  input -> state -> IO (output, state)

type CacheFun input cache = input -> cache -> cache

type NormalFun input ouput = input -> IO ouput

type FilterFun input = input -> IO Bool

type ChoiseFun input = input -> IO Int

data Process v where
  Normal :: NormalFun input output -> [Process output] -> Process input
  Choise :: ChoiseFun input -> [Process input] -> Process input
  Filter :: FilterFun input -> [Process input] -> Process input
  StateFun :: State state -> StateNormalFun input output state -> [Process output] -> Process input
  Process ::
    (input ~ IC cache, initCache ~ cache) =>
    IORef (TVar Bool, initCache, State cache, CacheFun input cache, NormalFun cache output, [Process output]) ->
    Process input
  Mutil :: IORef (Process input) -> Process input
  Sink :: NormalFun input () -> Process input

runProcess :: v -> Process v -> IO ()
runProcess v (Normal fun ps) = do
  v' <- fun v
  forM_ ps $ \p -> runProcess v' p
runProcess v (Choise fun ps) = do
  v' <- fun v
  runProcess v (ps !! v')
runProcess v (Filter fun ps) = do
  v' <- fun v
  when v' $ forM_ ps $ \p -> runProcess v p
runProcess v (Mutil ref) = do
  p <- readIORef ref
  runProcess v p
runProcess v (Sink fun) = fun v
runProcess v (StateFun state stateFun ps) = do
  oldState <- get state
  (v', newState) <- stateFun v oldState
  put state newState
  forM_ ps $ \p -> runProcess v' p
runProcess v (Process ref) = do
  (tvar, initCache, cache, cacheFun, fun, ps) <- readIORef ref
  readTVarIO tvar >>= \case
    False -> do
      cacheVal <- get cache
      put cache (cacheFun v cacheVal)
      writeIORef ref (tvar, initCache, cache, cacheFun, fun, ps)
    True -> do
      newtvar <- registerDelay 1000000
      cacheVal <- get cache
      put cache initCache
      writeIORef ref (newtvar, initCache, cache, cacheFun, fun, ps)
      v' <- fun cacheVal
      forM_ ps $ \p -> runProcess v' p
