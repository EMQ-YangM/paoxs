module Timer where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.IORef
import Data.Vector
import Dynamic

-- two thread
--
-- IORef [(Int, String)]
-- IORef Int

-- [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]
-- 1
--          1
--
--

-- t = [(1, "a"), (2, "b"), (3, "c"), (4, "d"), (5, "e")]

timeOut :: (Int, Int) -> Int -> Chan (Int, Dynamic) -> IO ()
timeOut (a, b) i m = do
  forkIO $ sleepThred
  return ()
  where
    sleepThred = do
      threadDelay (10 ^ 6 * i)
      writeChan m (1000, Calc a b)
      sleepThred
