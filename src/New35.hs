{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module New35 where

import Control.Monad
import Data.IORef
import GHC.Conc
import New34
import System.IO
  ( Handle,
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek),
    hClose,
    hPutStr,
    hSeek,
    withFile,
  )

data Person = Person
  { name :: String,
    age :: Int,
    id :: Int
  }
  deriving (Show, Eq, Read)

data Order = Order
  { order :: Int,
    personId :: Int
  }
  deriving (Show, Eq, Read)

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

initCache :: [Person] :* [Order]
initCache = [] :* []

mkl :: v -> IO (v :+ v1)
mkl v = return $ L v

mkr :: v1 -> IO (v :+ v1)
mkr v = return $ R v

cacheFun = cache2

pollRecord :: IORef Int -> IO (Record String)
pollRecord ref = do
  v <- readIORef ref
  if even v
    then return $ Record "person" (show $ Person "yang" 23 10)
    else return $ Record "order" (show $ Order 1001 10)

joinWork :: (IC cache ~ (Person :+ Order)) => IORef (TVar Bool, cache, State cache, CacheFun (Person :+ Order) cache, NormalFun cache output, [Process output]) -> Process (Record String)
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
      Normal (decode @Person) [Normal l44 [Process a]]
    ]

joinWork2 ::
  (IC cache ~ (Person :+ Order)) =>
  IORef (TVar Bool, cache, State cache, CacheFun (Person :+ Order) cache, NormalFun cache output, [Process output]) ->
  IORef Int ->
  Process ()
joinWork2 a b =
  Source
    (pollRecord b)
    [ Choise
        choise
        [ Normal (decode @Person) [Normal l21 [Process a]],
          Normal (decode @Order) [Normal l22 [Process a]]        ],
      Sink print
    ]

initWork = do
  ref <- newIORef initCache
  tbool <- registerDelay 1000000
  -- hSeek handle AbsoluteSeek 0 >> hPutStr handle (show initCache)
  -- newIORef (tbool, initCache, fileBackendState handle, cacheFun, print, [])
  newIORef (tbool, initCache, ioRefBackendState ref, cacheFun, print, [])

main2 :: IO ()
main2 = do 
    ref <- initWork 
    i <- newIORef 0
    let work = joinWork2 ref i 
    putStr $ render work 
    forM_ [1..100] $ \_ -> do 
        threadDelay 10000
        run work

main1 :: IO ()
main1 = do
  ref <- initWork
  let work = joinWork ref
  putStrLn $ render work
  forM_
    ( concat $
        replicate
          100
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
    [ Sink print,
      -- use IORef backend
      StateFun (ioRefBackendState ref) (\_ s -> return (s + 1, s + 1)) [Sink print, Mutil pref],
      -- use file backend
      StateFun (fileBackendState handle) (\_ s -> return (s + 1, s + 1)) [Sink print, Mutil pref]
    ]

main :: IO ()
main = do
  ref <- newIORef 0
  pref <- newIORef (StateFun (ioRefBackendState ref) (\_ s -> return (s + 1, s + 1)) [Sink print])
  withFile "test_stateFile.txt" ReadWriteMode $ \handle -> do
    hSeek handle AbsoluteSeek 0 >> hPutStr handle (show 0)
    ref <- initState
    putStrLn $ render (testProcess handle ref pref)
    forM_ (concat $ replicate 100 [Person "wang" 23 2]) $
      \v -> do
        threadDelay 10000
        runProcess v $ testProcess handle ref pref
    print "close handle"
    hClose handle
