{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module New28 where

import Control.Monad
import Data.IORef
import GHC.Conc
import Data.Data (Typeable, cast)


data R = R
  { temperature :: Int,
    humidity :: Int
  }
  deriving (Show, Eq)

data Record k v = Record
  { rk :: k,
    rv :: v
  }
  deriving (Show, Eq)

data Guard a
  = Continue a
  | Skip
  deriving (Show)

type Fun k v k1 v1 =
  Record k v -> IO (Guard (Record k1 v1))

type JoinFun k v k1 v1 k2 v2 =
  Cache k v k1 v1 -> IO (Guard (Record k2 v2))

type Cache k v k1 v1 = (TVar Bool, [Record k v], [Record k1 v1])

data v :< v1
  = LL v
  | RR v1
  deriving (Show)

data FS k v = forall k1 v1. FS (Fun k v k1 v1)  [FS k1 v1] 

runFS :: Record k v -> FS k v -> IO ()
runFS r (FS fun fs) = do
  v1 <- fun r
  case v1 of
    Skip -> return ()
    Continue v2 -> forM_ fs $ \f -> runFS v2 f

source :: Record k v -> IO (Guard (Record k v))
source = return . Continue

filterProcess :: (Record k v -> Bool) -> Fun k v k v
filterProcess f r = do
  if f r
    then return $ Continue r
    else return Skip

process :: (Record k v -> Record k1 v1) -> Record k v -> IO (Guard (Record k1 v1))
process fun r = return $ Continue $ fun r

pFilter :: (v -> Bool) -> Fun k v k v
pFilter f = filterProcess (\(Record _ v) -> f v)

pProcess :: (v -> v1) -> Fun k v k v1
pProcess f = process (\(Record a v) -> Record a $ f v)

sink :: Show v => Fun k v () ()
sink (Record _ v) = print v >> return Skip

t = Record 1 (R 23 32)

add1 (R a b) = R a (b + 101)

getb (R _ b) = b

-- getC :: R -> String
getC b = show b ++ " b-a-b-a-b-a"

myfs =
  FS
    source
    [ FS
        (pFilter (\(R _ b) -> b > 10))
          [FS (pProcess getb) [FS (pProcess getC) [FS sink []]]]
    ] 

(==>) :: Fun k v k1 v1 -> [FS k1 v1] -> [FS k v]
(==>) f fs = [FS f fs]



--(<=>) :: FS k v -> [FS k v] -> [FS k v]
--(<=>) f f1 = f : f1
--addsub ::  FS k v -> FS k1 v1-> FS k v
--addsub (FS ifun fs) a = FS ifun (fs ++ [a])

infixr 5 ==>

-- class Tclass a b k v where
--     (|>) :: a -> b -> FS k v
-- 
-- infixl 3 |>
-- 
-- instance (Typeable k, Typeable v, Typeable k1, Typeable v1) 
--     => Tclass (Fun k v k1 v1) [FS k1 v1] k v where 
--     a |> b = FS a b
-- 
-- instance (Typeable k, Typeable v, Typeable k1, Typeable v1) 
--     => Tclass (FS k v) [FS k1 v1 ] k v where 
--     (FS fun fs) |> b = 
--         case cast b of 
--             Just b' -> FS fun (fs ++ b')
--             Nothing -> error "......."

-- myfs1 = source ==> pFilter (\(R _ b) -> b > 10) ==> pProcess getb ==> [FS sink []]
              -- |> pFilter (\(R _ b) -> b > 10) ==> pProcess getb ==> [FS sink []]

--infixr 8 <=>

--myfs1 = source
--  --  ==> pFilter (\(R _ b) -> b > 10)
--    ==> (pProcess getb ==> pProcess getC ==> FS sink [])
--          <=> (pProcess getb ==> pProcess getC ==> FS sink [])
--          <=> (pProcess getb ==> pProcess getC ==> FS sink [])
--          <=> [pProcess getb ==> pProcess getC ==> FS sink []]
--
-- ==> pProcess getb ==> pProcess getC ==> FS sink []

-- myf1 = FS (pProcess getC) [FS sink []]
--myf1 = FS sink []
-- ==> pProcess getC
-- ==> pProcess getb
-- ==> (pFilter (\(R _ b) -> b > 10) )
-- ==> source

m1 = runFS t myfs

--m2 = runFS t myfs1
