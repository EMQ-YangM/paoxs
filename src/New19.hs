{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module New19 where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forM_)
import Data.Data (Proxy)
import Data.Dynamic
import Data.Foldable (msum)
import Data.IORef
import Data.Kind
import Data.List (intersperse)
import Data.Map (Map, empty, insert, lookup, toList)
import Data.Proxy
import Data.Void (Void)
import GHC.Base (Symbol)
import GHC.TypeLits
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-))
import New18
import Shelly
import System.IO.Unsafe
import System.Random

data Person = Person
  { id :: Int,
    name :: String,
    age :: Int
  }
  deriving (Show, Ord, Eq)

data Order = Order
  { orderNumber :: Int,
    personId :: Int
  }
  deriving (Show, Ord, Eq)

data Result = Result
  { ln :: String,
    fn :: Int,
    on :: Int
  }
  deriving (Show, Ord, Eq)

tv1 =
  Base :> (IN WB TA :: IN "source_person" '[] Person)
    :> (IN WB TA :: IN "source_order" '[] Order)
    :> (IN WA (Reapet 1) :: IN "join" '["source_person", "source_order"] [Result])
    :> (IN WA TA :: IN "sink" '["join"] [Result])

join :: [Person] -> [Order] -> [Result]
join (p : ps) os = case inis os of
  Just v -> Result (name p) (age p) (orderNumber v) : join ps os
  Nothing -> join ps os
  where
    inis (x : xs) =
      if New19.id p == personId x
        then Just x
        else inis xs
    inis [] = Nothing
join [] _ = []

ff1 =
  func
    tv1
    ( Point
        :<|> (\_ -> randomPerson 1 10)
        :<|> (\_ -> randomOrder 1 10)
        :<|> (\(SList p :< SList e :< E) -> return $ join p e)
        :<|> (\_ -> return [])
    )

test1 :: IO ()
test1 = do
  logChan <- newChan :: IO (Chan String)
  ref <- newIORef Data.Map.empty
  start logChan ref ff1
  handleLog logChan
  where
    handleLog chan = do
      v <- readChan chan
   -- putStrLn v
      handleLog chan

randomString :: IO String
randomString = sequence $ take 5 $ repeat $ randomRIO ('a', 'z')

randomPerson :: Int -> Int -> IO Person
randomPerson lv hv = do
  Person <$> randomRIO (lv, hv)
    <*> randomString
    <*> randomRIO (20, 50)

randomOrder :: Int -> Int -> IO Order
randomOrder lv hv = do
  Order <$> randomRIO (0, 100000)
    <*> randomRIO (lv, hv)

t1 :: Dynamic
t1 = toDyn (1 :: Int)

t2 :: Dynamic
t2 = toDyn (2 :: Int)

t3 = fromDyn t1 1 + fromDyn t2 3 :: Int


