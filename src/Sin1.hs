{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Sin1 where

import Data.Kind
-- import Data.Singletons.Prelude

import Data.Map (Map, empty, insert)
import qualified Data.Map as M
import Data.Singletons
import Data.Singletons.CustomStar
import Data.Singletons.Prelude (MaxSym0)
import Data.Singletons.TypeLits
import GHC.TypeLits (type (+))
import GHC.TypeNats

-- import GHC.TypeLits.Normalise

$( singletons
     [d|
       data DoorState = Opened | Closed | Locked
         deriving (Show, Eq, Ord)

       mergeState :: DoorState -> DoorState -> DoorState
       mergeState = max

       mergeStateList :: [DoorState] -> DoorState
       mergeStateList = foldr mergeState Opened
       --  add :: (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
       --  add (a, b) ((x0, x0s) : xs) = (x0, b : x0s) : xs

       --  ins :: Int -> Int -> Map Int Int -> Map Int Int
       --  ins k v m = insert k v m
       |]
 )

-- data Graph = Graph Int [Graph]

-- data J :: Graph -> Type where
--   J :: J ( 'Graph 0 '[])

-- A :: v -> ss -> J ('Graph (Add k '[]))

-- t :: KnownNat n => m (n * n + 1) s
-- t = undefined

data T m = T

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

-- closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
-- closeSomeOpenedDoor (MkSomeDoor s d) = case s of
--     SOpened -> Just . fromDoor_ $ closeDoor d
--     SClosed -> Nothing
--     SLocked -> Nothing

-- lockAnySomeDoor :: SomeDoor -> SomeDoor
-- lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
  Opened -> fromDoor_ . mkDoor SOpened
  Closed -> fromDoor_ . mkDoor SClosed
  Locked -> fromDoor_ . mkDoor SLocked

l :: [(Integer, [(Integer, Integer)], [(Integer, Integer)])]
l =
  [ (0, [], [(0, 1)]),
    (1, [(0, 1)], [(1, 2), (2, 3)]),
    (2, [(1, 2)], [(2, 4), (2, 6)]),
    (4, [(2, 4), (3, 4)], [(4, 8)])
  ]

data T3 = T3 Nat [(Nat, Nat)] [(Nat, Nat)]

data T2 = T2 [(Nat, Nat)] [(Nat, Nat)]

data Fun :: Nat -> T2 -> Type where
  Fun :: xs -> ys  ->  Fun 0 ( 'T2 '[ '(0, 1)] '[ '(1, 2), '(2, 3)])
  -- Fun :: xs -> ys  ->  Fun 0 ( 'T2 (Length xs) (Length ys))

data J :: T3 -> Type where
  J :: J ( 'T3 1 '[ '(0, 1)] '[ '(1, 2), '(2, 3)])
  J1 :: Fun m ('T2 a b) -> J ('T3 m a b)


k1 :: J ( 'T3 1 '[ '(0, 1)] '[ '(1, 2), '(2, 3)])
k1 = J

