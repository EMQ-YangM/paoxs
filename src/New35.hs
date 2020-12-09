module New35 where

import Control.Monad (forM_)
import Data.IORef (IORef, newIORef)
import GHC.Conc (threadDelay)
import New34
  ( Process (Normal, Sink, StateFun),
    fileBackendState,
    ioRefBackendState,
    runProcess,
  )
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
    order :: Int
  }
  deriving (Show, Eq)

initState :: IO (IORef Int)
initState = newIORef 0

testProcess :: Handle -> IORef Int -> Process Person
testProcess handle ref =
  Normal
    return
    [ Sink print,
      -- use IORef backend
      StateFun (ioRefBackendState ref) (\_ s -> return (s + 1, s + 1)) [Sink print],
      -- use file backend
      StateFun (fileBackendState handle) (\_ s -> return (s + 1, s + 1)) [Sink print]
    ]

main :: IO ()
main = do
  withFile "test_stateFile.txt" ReadWriteMode $ \handle -> do
    hSeek handle AbsoluteSeek 0 >> hPutStr handle (show 0)
    ref <- initState
    forM_ (concat $ replicate 100 [Person "wang" 23 2]) $
      \v -> do
        threadDelay 10000
        runProcess v $ testProcess handle ref
    print "close handle"
    hClose handle
