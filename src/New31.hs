{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module New31 where

import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Data
import Data.IORef
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Singletons.TypeError
import GHC.Conc
import GHC.Generics hiding (R1)
import GHC.TypeLits
import System.Random
import Type.Reflection
import Unsafe.Coerce
import GHC.IO (unsafePerformIO)

data R1 = R1
  { r11 :: Int,
    r12 :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data R2 = R2
  { r21 :: Int,
    r22 :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Record v = Record
  { rk :: String,
    rv :: v
  }
  deriving (Show, Eq)

data Guard a
  = Continue a
  | Skip
  deriving (Show)

type Fun v v1 =
  Record v -> IO (Guard (Record v1))

type Choise v =
  Record v -> IO Int

type JoinFun v v1 v2 =
  Cache v v1 -> IO (Guard (Record [v2]))

type Cache v v1 = ([Record v], [Record v1])

data v :< v1
  = L v
  | R v1
  deriving (Show)

class MyLift a b where
  myLift :: a -> b

instance MyLift a (a :< b) where
  myLift a = L a

instance MyLift b (a :< b) where
  myLift b = R b

instance MyLift a a where
  myLift = id

data FS v where
  FS :: MyLift v1 b => Fun v v1 -> [FS b] -> FS v
  TF :: IORef (TVar Bool, Cache v v1) -> JoinFun v v1 v2 -> [FS [v2]] -> FS (v :< v1)
  FC :: Choise v -> [FS v] -> FS v
  TE :: Fun v v1 -> FS v

runFS :: Record v -> FS v -> IO ()
runFS r (FS fun fs) = do
  v1 <- fun r
  case v1 of
    Skip -> return ()
    Continue (Record a v2) ->
      forM_ fs $ \f -> runFS (Record a $ myLift v2) f
runFS (Record k v) (TF ref jfun fs) = do
  (tvbool, cache@(lefts, rights)) <- readIORef ref
  b <- readTVarIO tvbool
  if b
    then do
      tvbool' <- registerDelay 1000000
      writeIORef ref (tvbool', ([], []))
      val <- jfun cache
      case val of
        Skip -> return ()
        Continue val' -> forM_ fs $ \f -> runFS val' f
    else case v of
      L v' -> writeIORef ref (tvbool, (Record k v' : lefts, rights))
      R v' -> writeIORef ref (tvbool, (lefts, Record k v' : rights))
runFS r@(Record k v) (FC cf fs) = do
  i <- cf r
  let e = fs !! i
  runFS (Record k v) e
runFS r (TE f) = do
  f r
  return ()

sour1 :: Fun String R1
sour1 s = do
 -- print s
  r1 <- randomRIO (1, 10)
  r2 <- randomRIO (1, 10)
 -- print (Record "r1" $ R1 r1 r2)
  return $ Continue (Record "r1" $ R1 r1 r2)

sour2 :: Fun String R2
sour2 _ = do
  r1 <- randomRIO (1, 10)
  r2 <- randomRIO (1, 10)
  return $ Continue (Record "r2" $ R2 r1 r2)

--sink :: Show a => Fun a ()
sink r = print r >> return Skip

add1 (R1 a b) = R1 a (b + 1)

choise :: Record String -> IO Int
choise (Record s _) =
  case s of
    "r1" -> return 0
    "r2" -> return 1

pFilter :: Record R1 -> Bool
pFilter (Record _ (R1 _ r2)) = r2 < 8

pfilterProcess f r@(Record _ v) =
  if f v
    then return $ Continue r
    else return Skip

process :: (Show v) => (v -> v1) -> Fun v v1
process f (Record k v) = do
  print v
  return $ Continue $ Record k (f v)

join1 :: ([Record R1], [Record R2]) -> IO (Guard (Record [(Int, Int, Int, Int)]))
join1 (ls, rs) = return $ Continue $ Record "join" $ concatMap findv ls
  where
    findv (Record _ l) =
      case filter (\(Record _ r) -> r21 r == r11 l) rs of
        [] -> []
        (Record _ x) : _ -> [(r11 l, r12 l, r21 x, r22 x)]

testFS ref =
  FC
    choise
    [ FS sour1 [FS (pfilterProcess (\(R1 _ v2) -> v2 > 7)) [FS (process add1) [TF ref join1 [TE sink]]]],
      FS sour2 [TF ref join1 [TE sink]]
    ]
-- >>> m1
m1 :: IO ()
m1 = do
  tvbool <- registerDelay 1000000
  ref <- newIORef (tvbool, ([], []))
  forM_ (concat $ replicate 800 [Record "r1" "", Record "r2" ""]) $
    \v -> do
      threadDelay 10000
      runFS v (testFS ref)


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

