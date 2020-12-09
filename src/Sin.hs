{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Sin where

import Data.Kind

data Foo a = MkFoo

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

-- data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }

data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

-- mkDoor :: DoorState -> String -> Door s
-- mkDoor Opened = UnsafeMkDoor
-- mkDoor Closed = UnsafeMkDoor
-- mkDoor Locked = UnsafeMkDoor

data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

lockAnyDoor0 :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor0 sng door = case sng of
  SOpened -> lockDoor (closeDoor door) -- in this branch, s is 'Opened
  SClosed -> lockDoor door -- in this branch, s is 'Closed
  SLocked -> door -- in this branch, s is 'Locked

lockAnyDoor :: SDoorState s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor -- in this branch, s is 'Opened
  SClosed -> lockDoor -- in this branch, s is 'Closed
  SLocked -> id

doorStatus0 :: SDoorState s -> Door s -> DoorState
doorStatus0 SOpened _ = Opened
doorStatus0 SClosed _ = Closed
doorStatus0 SLocked _ = Locked

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus s _ = fromSDoorState s

class SingDSI s where
  singDS :: SDoorState s

instance SingDSI 'Opened where
  singDS = SOpened

instance SingDSI 'Closed where
  singDS = SClosed

instance SingDSI 'Locked where
  singDS = SLocked

lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS

doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS


mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor



