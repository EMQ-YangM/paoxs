{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module New32 where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import New33
import New31 (Fun, Cache, FS(..), runFS, Record(..), Guard(..), JoinFun, (:<))
import Control.Monad
import GHC.Conc
import Data.IORef
import GHC.IO (unsafePerformIO)

dealSink = TE

addFun :: Fun v v1 -> FS v1 -> FS v
addFun f fs = FS f [fs]

addFuns :: Fun v v1 -> [FS v1] -> FS v 
addFuns = FS

addJoin :: IORef (TVar Bool, Cache v v1) -> JoinFun v v1 v2 -> [FS [v2]] -> FS (v :< v1)
addJoin  = TF

{-# NOINLINE creatIORef #-}
creatIORef :: IORef (TVar Bool, Cache v v1)
creatIORef  = unsafePerformIO $ do 
    tvbool <- newTVarIO False
    newIORef  (tvbool, ([],[]))


data R1 = R1
  { r11 :: Int,
    r12 :: Int
  }
  deriving (Show, Eq)

csour _  = return $ Continue $ Record "1" $ R1 1 20

filter1 r@(Record _ (R1 _ b)) = 
    if b > 10 then return $ Continue r  else return Skip

add1 (Record k (R1 a b)) = return $ Continue $ Record k $ R1 a (b + 1000)

sink r = print r >> return Skip

t6 = [q|
  source,        , csour
  filter, source , filter1
  sink  , filter , sink
  add1  , filter , add1
  sink1 , add1   , sink
  sink2 , add1   , sink
  sink3 , add1   , sink
  |]

m1 :: IO ()
m1 = do
  forM_ (concat $ replicate 800 [Record "r1" ""]) $
    \v -> do
      threadDelay 10000
      runFS v t6

