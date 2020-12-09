module Paxos where

import Control.Concurrent
  ( Chan,
    MVar,
    forkIO,
    newChan,
    readChan,
    swapMVar,
    threadDelay,
    writeChan,
  )
import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Concurrent.Chan
  ( Chan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Concurrent.MVar (MVar, swapMVar)
import Control.Monad
  ( Functor (fmap),
    Monad (return),
    forM_,
    replicateM,
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time (UTCTime)
import System.Random (Random (randomRIO))
import System.Timeout
import Prelude hiding (log)

-- Proposer               Acceptors              Learner
-- Prepare
--                         Promise
-- Propose
--                         Accept

newtype Number = Number Integer deriving (Eq, Ord, Show)

newtype ResN = ResN Number deriving (Eq, Ord, Show)

newtype Value = Value Integer deriving (Eq, Ord, Show)

-- data Proposer = Proposer Integer Integer

-- Prepare
data Prepare = Prepare Number deriving (Eq, Ord, Show)

data Promise
  = Pok Number Value
  | PokNull
  | Perror
  deriving (Eq, Ord, Show)

data Propose = Propose Number Value deriving (Eq, Ord, Show)

data Accept
  = Aok
  | Aerror
  deriving (Eq, Ord, Show)

-------------------------------------------------------

newtype Proposer = Proposer Integer deriving (Eq, Ord, Show)

newtype Accepter = Accepter Integer deriving (Eq, Ord, Show)

data ATOP
  = Apromise Promise
  | Aaccept Accept
  deriving (Eq, Ord, Show)

data PTOA
  = Pprepare Prepare
  | Ppropose Propose
  deriving (Eq, Ord, Show)

data Result = Success | Failed deriving (Eq, Ord, Show)

type LogChan = Chan String

data Message
  = MPTA Proposer Accepter UTCTime
  | MATP Accepter Proposer UTCTime
  deriving (Eq, Ord, Show)

-- Proposer           proposer    value   inc number         receive channel        all Accepter
-- proposerProcess :: Proposer -> Value -> IORef Number -> Chan (Accepter, ATOP) -> Map Accepter (Chan (Proposer, PTOA)) -> IO ()

-- Accepter             Accepter          init stat                receive channel           all Proposer
-- accepterProcess ::  Accepter -> (ResN, Number, Value) -> Chan (Proposer, PTOA) -> Map Proposer (Chan (Accepter, ATOP)) -> IO ()

incNumber :: Proposer -> IORef Number -> IO Number
incNumber (Proposer i) v = do
  Number val <- readIORef v
  writeIORef v (Number $ val + 1)
  return $ Number (val * 100 + i)

log :: LogChan -> String -> IO ()
log chan s = writeChan chan s

proposerProcess :: LogChan -> Proposer -> Value -> IORef Number -> Chan (Accepter, ATOP) -> Map Accepter (Chan (Proposer, PTOA)) -> IO ()
proposerProcess logChan proposer value numberRef recChan accMap = do
  randomRIO (10, 1000000) >>= threadDelay

  n <- incNumber proposer numberRef

  log logChan $ show proposer <> ": boardcast " <> show n <> " to all accepter"
  M.traverseWithKey (\_ a -> writeChan a (proposer, Pprepare $ Prepare n)) accMap

  --   res <- timeout 1000 (loop1 ((0,[],(Number 0, value)),0)) ???
  res <- loop1 ((0, [], (Number 0, value)), 0)

  case res of
    Nothing -> proposerProcess logChan proposer value numberRef recChan accMap
    Just (as, val) -> do
      forM_ as $ \a -> do
        let Just c = M.lookup a accMap

        randomRIO (5, 100) >>= threadDelay

        log logChan $ show proposer <> ": send propose " <> show val <> " to " <> show a
        writeChan c (proposer, Ppropose $ Propose n val)

      res1 <- loop2 (0, 0)
      case res1 of
        Success -> do
          putStrLn $ show proposer <> ": ---> finish value is " <> show val
        -- log logChan $ show proposer <> " finish"
        Failed -> do
          proposerProcess logChan proposer value numberRef recChan accMap
  where
    loop1 ((sccCount, as, (num0, val0)), failCount) = do
      if sccCount >= (M.size accMap) `div` 2 + 1
        then return $ Just (as, val0)
        else
          if failCount >= (M.size accMap) `div` 2 + 1
            then return Nothing
            else do
              (a, v) <- readChan recChan
              case v of
                Apromise (Pok num val) -> loop1 ((sccCount + 1, a : as, if num > num0 then (num, val) else (num0, val0)), failCount)
                Apromise PokNull -> loop1 ((sccCount + 1, a : as, (num0, val0)), failCount)
                _ -> loop1 ((sccCount, as, (num0, val0)), failCount + 1)
    loop2 (sccCount, failCount) = do
      if sccCount >= (M.size accMap) `div` 2 + 1
        then return Success
        else
          if failCount >= (M.size accMap) `div` 2 + 1
            then return Failed
            else do
              (_, v) <- readChan recChan
              case v of
                Aaccept Aok -> loop2 (sccCount + 1, failCount)
                _ -> loop2 (sccCount, failCount + 1)

accepterProcess :: LogChan -> Accepter -> (ResN, Number, Value) -> Chan (Proposer, PTOA) -> Map Proposer (Chan (Accepter, ATOP)) -> IO ()
accepterProcess logChan accepter (ResN resn, number, value) recChan proMap = do
  randomRIO (5, 100) >>= threadDelay
  (proposer, v) <- readChan recChan
  let Just c = M.lookup proposer proMap
  case v of
    Pprepare (Prepare n) ->
      if n <= resn
        then do
          writeChan c (accepter, Apromise Perror)
          log logChan $ show accepter <> ": send perror to " <> show proposer
          accepterProcess logChan accepter (ResN resn, number, value) recChan proMap
        else do
          log logChan $
            show accepter <> ": send promise " <> show (if value == Value 0 then PokNull else Pok number value)
              <> " to "
              <> show proposer
          writeChan c (accepter, Apromise $ if value == Value 0 then PokNull else Pok number value)
          accepterProcess logChan accepter (ResN n, number, value) recChan proMap
    Ppropose sv@(Propose n v) ->
      if n < resn
        then do
          log logChan $ show accepter <> ": send aerror to " <> show proposer
          writeChan c (accepter, Aaccept Aerror)
          accepterProcess logChan accepter (ResN resn, number, value) recChan proMap
        else do
          log logChan $ show accepter <> ": send Aok to " <> show proposer <> " value is " <> show sv
          writeChan c (accepter, Aaccept Aok)
          accepterProcess logChan accepter (ResN resn, n, v) recChan proMap

logHandler :: LogChan -> IO ()
logHandler = loop
  where
    loop logChan = do
      readChan logChan >>= putStrLn
      loop logChan

start :: Int -> Int -> IO ()
start pnumber anumber = do
  incNumber <- newIORef $ Number 1
  logChan <- newChan

  pChans <- fmap (zip $ fmap Proposer [1 ..]) $ replicateM pnumber newChan
  aChans <- fmap (zip $ fmap Accepter [1 ..]) $ replicateM anumber newChan

  -- logHanlder
  forkIO $ logHandler logChan

  -- create accepter
  forM_ aChans $ \(acc@(Accepter i), chan) -> do
    -- log logChan $ "start accepterPorcess: " <> show acc
    forkIO $ accepterProcess logChan acc (ResN $ Number 0, Number 0, Value 0) chan (M.fromList pChans)

  -- create proposer
  forConcurrently_ pChans $ \(prop@(Proposer i), chan) -> do
    -- log logChan $ "start proposerPorcess: " <> show prop
    proposerProcess logChan prop (Value i) incNumber chan (M.fromList aChans)
