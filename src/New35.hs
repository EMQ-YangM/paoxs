{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- {-# ghc -Wno-typed-holes #-}

module New35 where

import Codec.Serialise (Serialise, deserialise, serialise)
import Control.Monad
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Conc
import GHC.Generics (Generic)
import New34
  ( Process (Choise, Mutil, Normal, Process, Sink, Source, StateFun),
    cache2,
    fileBackendState,
    ioRefBackendState,
    l21,
    l22,
    l41,
    l42,
    l43,
    l44,
    optimize,
    render,
    run,
    runProcess,
    type (:++) (..),
  )
import System.IO
  ( Handle,
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek),
    hClose,
    hPutStr,
    hSeek,
    openFile,
    withFile,
  )

data Person = Person
  { name :: String,
    age :: Int,
    pid :: Int
  }
  deriving (Show, Eq, Read)
  deriving stock (Generic)
  deriving anyclass (Serialise)

tval = serialise (Person "yang" 23 1001)

rtval = deserialise (tval <> "bbb") :: Person

data Order = Order
  { order :: Int,
    personId :: Int
  }
  deriving (Show, Eq, Read)
  deriving stock (Generic)
  deriving anyclass (Serialise)

data Result = Result
  { personName :: String,
    personAge :: Int,
    personOrder :: Int
  }
  deriving (Show, Eq, Read)

data Record v = Record String v deriving (Show)

choise :: Record String -> IO Int
choise (Record "person" _) = return 0
choise (Record "order" _) = return 1

decode :: Read a => Record String -> IO a
decode (Record _ v) = return $ read v

initCache :: [Person] :++ [Order]
initCache = [] :++ []

cacheFun = cache2

pollRecord :: IORef Int -> IO (Record String)
pollRecord ref = do
  v <- readIORef ref
  modifyIORef' ref (+ 1)
  if even v
    then return $ Record "person" (show $ Person "yang" 23 10)
    else return $ Record "order" (show $ Order 1001 10)

joinWork a =
  Choise
    choise
    [ Normal (decode @Person) [Normal l21 [Process a]],
      Normal (decode @Order) [Normal l22 [Process a]]
    ]

joinWork1 a =
  Choise
    choise
    [ Normal (decode @Person) [Normal l41 [Process a]],
      Normal (decode @Order) [Normal l42 [Process a]],
      Normal (decode @Result) [Normal l43 [Process a]],
      Normal (decode @Double) [Normal l44 [Process a]]
    ]

joinWork2 a b c =
  Source
    (pollRecord b)
    [ Choise
        choise
        [ Normal (decode @Person) [Normal l21 [Process a]],
          Normal (decode @Order) [Normal l22 [Process a]]
        ],
      Sink print,
      StateFun c (\_ s -> return (s + 1, s + 1)) [Sink print]
    ]

joinFun :: [Person] :++ [Order] -> IO [Result]
joinFun (ps :++ os) = return $ concatMap fun ps
  where
    fun p =
      case filter (\v -> pid p == personId v) os of
        [] -> []
        x : _ -> [Result (name p) (age p) (order x)]

initWork = do
  let time = 100000
  ref <- newIORef initCache
  tbool <- registerDelay time
  newIORef (tbool, initCache, ioRefBackendState ref, cacheFun, time, Normal joinFun [Sink print])

main :: IO ()
main = do
  ref <- initWork
  i <- newIORef 0
  countRef <- newIORef 0
  let work = joinWork2 ref i (ioRefBackendState countRef)
  putStrLn "\n---------- process depend ----------\n\n"
  putStr $ render work
  putStrLn "............ optimize later ............ \n"
  let optWork = optimize work
  putStr $ render optWork
  forM_ [1 .. 200] $ \_ -> do
    threadDelay 10000
    run optWork

main1 :: IO ()
main1 = do
  ref <- initWork
  let work = joinWork ref
  putStrLn $ render work
  putStrLn "............ optimize later ............ \n"
  putStrLn $ render $ optimize work
  forM_
    ( concat $
        replicate
          1
          [ Record "person" (show $ Person "yang" 23 10),
            Record "order" (show $ Order 1001 10)
          ]
    )
    $ \v -> do
      threadDelay 10000
      runProcess v work

initState :: IO (IORef Int)
initState = newIORef 0

testProcess :: Handle -> IORef Int -> IORef (Process Int) -> Process Person
testProcess handle ref pref =
  Normal
    return
    [ -- use IORef backend
      StateFun (ioRefBackendState ref) (\_ s -> return (s + 1, s + 1)) [Mutil pref],
      -- use file backend
      StateFun (fileBackendState handle) (\_ s -> return (s + 1, s + 1)) [Mutil pref]
    ]

main2 :: IO ()
main2 = do
  ref <- newIORef 0
  pref <- newIORef (StateFun (ioRefBackendState ref) (\_ s -> return (s + 1, s + 1)) [Sink print])
  handle <- openFile "test_stateFile.txt" ReadWriteMode
  hSeek handle AbsoluteSeek 0 >> hPutStr handle "       "
  hSeek handle AbsoluteSeek 0 >> hPutStr handle (show 0)
  ref <- initState
  putStrLn $ render (testProcess handle ref pref)
  forM_ (concat $ replicate 3 [Person "wang" 23 2]) $
    \v -> do
      threadDelay 10000
      runProcess v $ testProcess handle ref pref
  putStrLn "close handle"
  hClose handle
