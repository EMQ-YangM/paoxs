{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module New25 where

import Lens.Micro.TH
import Lens.Micro.Mtl
import Control.Monad.RWS.Class (MonadState)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.State.Strict (StateT(runStateT))

data Config = Config {
    _set1 :: String,
    _set2 :: String
}

data State0 = State0 {
    _value1 :: Maybe Int,
    _value2 :: Float,
    _value3 :: String
} deriving (Show)

data State1 = State1 {
    _time :: Int,
    _state :: State0
} deriving (Show)

makeLenses ''Config
makeLenses ''State0
makeLenses ''State1

ds :: ( MonadReader Config m
      , MonadState State1 m
      , MonadError String m  ) 
    => m ()
ds = do 
    val2 <- view set1
    time .= 10
    state.value3 .= val2
    state.value2 .= 10.0
    state.value1 ?= 10001
                             
    throwError "nice"

run :: IO ()
run = do 
    let v = runReaderT (runStateT (runExceptT ds) (State1 1 (State0 (Just 1) 1 "great"))) (Config "start" "end")
    v' <- v
    print v'
    return ()
















