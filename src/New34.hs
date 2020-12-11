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

import Control.Monad
import qualified Control.Monad.State as S
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Conc (TVar, readTVarIO, registerDelay)
import System.IO
  (openFile,  Handle,
    IOMode(ReadWriteMode),
    SeekMode (AbsoluteSeek),
    hGetLine,
    hPutStr,
    hSeek,
  )
import System.IO.Unsafe
import System.Mem.StableName

data a :+ b
  = L a
  | R b
  deriving (Show, Read)

data a :++ b = a :++ b deriving (Show, Read)

cache :: a -> [a] -> [a]
cache a as = a : as

cache2 :: a :+ b -> [a] :++ [b] -> [a] :++ [b]
cache2 (R b) (cs :++ bs) = cs :++ (b : bs)
cache2 (L a) (cs :++ bs) = (a : cs) :++ bs

cache3 ::
  a :+ b :+ c ->
  [a] :++ [b] :++ [c] ->
  [a] :++ [b] :++ [c]
cache3 (R c) (as :++ bs :++ cs) = as :++ bs :++ (c : cs)
cache3 (L (R b)) (as :++ bs :++ cs) = as :++ (b : bs) :++ cs
cache3 (L (L a)) (as :++ bs :++ cs) = (a : as) :++ bs :++ cs

cache4 ::
  a :+ b :+ c :+ d ->
  [a] :++ [b] :++ [c] :++ [d] ->
  [a] :++ [b] :++ [c] :++ [d]
cache4 (R d) (as :++ bs :++ cs :++ ds) = as :++ bs :++ cs :++ (d : ds)
cache4 (L (R c)) (as :++ bs :++ cs :++ ds) = as :++ bs :++ (c : cs) :++ ds
cache4 (L (L (R b))) (as :++ bs :++ cs :++ ds) = as :++ (b : bs) :++ cs :++ ds
cache4 (L (L (L a))) (as :++ bs :++ cs :++ ds) = (a : as) :++ bs :++ cs :++ ds

l21 :: Monad m => a -> m (a :+ b)
l21 v = return $ L v

l22 :: Monad m => b -> m (a :+ b)
l22 v = return $ R v

l31 :: Monad m => a -> m ((a :+ b1) :+ b2)
l31 v = return $ L $ L v

l32 :: Monad m => b1 -> m ((a :+ b1) :+ b2)
l32 v = return $ L $ R v

l33 :: Monad m => b -> m (a :+ b)
l33 v = return $ R v

l41 :: Monad m => a -> m (((a :+ b1) :+ b2) :+ b3)
l41 v = return $ L $ L $ L v

l42 :: Monad m => b1 -> m (((a :+ b1) :+ b2) :+ b3)
l42 v = return $ L $ L $ R v

l43 :: Monad m => b1 -> m ((a :+ b1) :+ b2)
l43 v = return $ L $ R v

l44 :: Monad m => b -> m (a :+ b)
l44 v = return $ R v

type family IC a where
  IC (a :+ b) = (IC a :++ [b])
  IC a = [a]

data State a = State
  { put :: a -> IO (),
    get :: IO a
  }

type StateNormalFun input output state =
  input -> state -> IO (output, state)

type CacheFun input cache = input -> cache -> cache

type NormalFun input ouput = input -> IO ouput

type FilterFun input = input -> IO Bool

type ChoiseFun input = input -> IO Int

type Source output = IO output

type Sink input = input -> IO ()

data Process v where
  Normal :: NormalFun input output -> [Process output] -> Process input
  Choise :: ChoiseFun input -> [Process input] -> Process input
  Filter :: FilterFun input -> [Process input] -> Process input
  StateFun :: State state -> StateNormalFun input output state -> [Process output] -> Process input
  Process ::
    (IC input ~ cache, initCache ~ cache) =>
    IORef (TVar Bool, initCache, State cache, CacheFun input cache, Int, Process cache) ->
    Process input
  Mutil :: IORef (Process input) -> Process input
  Source :: Source output -> [Process output] -> Process ()
  Sink :: Sink input -> Process input

optimize :: Process v -> Process v
optimize (Normal fun [p]) =
  case p of
    Normal fun1 xs -> optimize $ Normal (fun >=> fun1) xs
    _ -> Normal fun [p]
optimize (Choise fun funs) = Choise fun $ map optimize funs
optimize (Source fun funs) = Source fun $ map optimize funs
optimize v = v

run :: Process () -> IO ()
run (Source fun ps) = fun >>= \v -> forM_ ps (runProcess v)
run _ = error "this is not source"

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
  (tvar, initCache, cache, cacheFun, i, p) <- readIORef ref
  readTVarIO tvar >>= \case
    False -> do
      cacheVal <- get cache
      put cache (cacheFun v cacheVal)
      writeIORef ref (tvar, initCache, cache, cacheFun, i, p)
    True -> do
      newtvar <- registerDelay i
      cacheVal <- get cache
      put cache initCache
      writeIORef ref (newtvar, initCache, cache, cacheFun, i, p)
      runProcess cacheVal p

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

createIORefBackend :: a -> IO (State a)
createIORefBackend initState = do
  ref <- newIORef initState
  return $ ioRefBackendState ref

createFileBackend :: (Show a, Read a) => String -> a -> IO (State a)
createFileBackend name initState = do
  handle <- openFile name ReadWriteMode
  hPutStr handle $ show initState
  return $ fileBackendState handle

dbBackendState :: State a
dbBackendState =
  State
    { put = undefined,
      get = undefined
    }

makeSpace :: Int -> String
makeSpace i = replicate i ' '

render :: Process v -> String
render p = a ++ s
  where
    (a, b) = S.runState (render' "" 0 p) M.empty
    s = "\n\n" ++ concatMap ((++ "\n") . snd) (M.toList b)

isOne :: Int -> [Process v] -> S.State (Map Int String) String
isOne i [p] = render' "" i p >>= \v -> return (" " ++ v)
isOne i ps =
  mapM (render' (makeSpace (i + 4)) (i + 4)) ps
    >>= \v -> return ("\n" ++ concat v)

mkstring :: Monad m => [a] -> [a] -> [a] -> m [a]
mkstring s n v = return $ s ++ n ++ v

render' :: String -> Int -> Process v -> S.State (Map Int String) String
render' s i (Normal _ ps) = isOne i ps >>= mkstring s "Normal ->"
render' s i (Choise _ ps) = isOne i ps >>= mkstring s "Choise ->"
render' s i (Filter _ ps) = isOne i ps >>= mkstring s "Filter ->"
render' s i (StateFun _ _ ps) = isOne i ps >>= mkstring s "StateFun ->"
render' s i (Source _ ps) = isOne i ps >>= mkstring s "Source ->"
render' s i (Sink _) = return $ s ++ "Sink\n"
render' s i (Process ioref) = do
  let index = hashStableName $ unsafePerformIO $ makeStableName ioref
  m <- S.get
  case M.lookup index m of
    Just _ -> return ()
    Nothing -> do
      let (_, _, _, _, _, p) = unsafePerformIO $ readIORef ioref
      rs <- render' "" 0 p
      S.put (M.insert index ("Prosess" ++ show index ++ ": " ++ rs) m)
  return $ s ++ "Process" ++ show index ++ " \n"
render' s i (Mutil ioref) = do
  let index = hashStableName $ unsafePerformIO $ makeStableName ioref
  m <- S.get
  case M.lookup index m of
    Just _ -> return ()
    Nothing -> do
      let p = unsafePerformIO $ readIORef ioref
      rs <- render' "" 0 p
      S.put (M.insert index ("Mutil" ++ show index ++ ": " ++ rs) m)
  return $ s ++ "Mutil" ++ show index ++ "\n"