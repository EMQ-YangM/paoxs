{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module New32 where

import Control.Monad
import GHC.Conc
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import New31 (Cache, FS (..), Fun, Guard (..), JoinFun, Record (..), addFun, addFuns, addJoin, creatIORef, dealSink, runFS, (:<))
import New33

data R1 = R1
  { r11 :: Int,
    r12 :: Int
  }
  deriving (Show, Eq)

data R2 = R2
  { r21 :: Int,
    r22 :: Int
  }
  deriving (Show, Eq)

csour _ = return $ Continue $ Record "1" $ R1 1 20

filter1 r@(Record _ (R1 _ b)) =
  if b > 10 then return $ Continue r else return Skip

add1 (Record k (R1 a b)) = return $ Continue $ Record k $ R1 a (b + 1000)

m1 :: IO ()
m1 = do
  let sink r = print r >> return Skip
      t6 =
        [q|
  source,        , csour
  filter, source , filter1
  sink  , filter , sink
  add1  , filter , add1
  sink1 , add1   , sink
  sink2 , add1   , sink
  sink3 , add1   , sink
  |]
  forM_ (concat $ replicate 800 [Record "r1" 1]) $
    \v -> do
      threadDelay 10000
      runFS v t6
