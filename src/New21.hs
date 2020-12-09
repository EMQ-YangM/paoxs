{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module New21 where

import GHC.TypeLits
import Dynamic


data Source (n :: Nat) ot
data Sink (n :: Nat) (inu :: Nat)
data Immediately (n :: Nat) (inu :: Nat) ot
data Filter (n :: Nat) (inu :: Nat) ot
data Worker (n :: Nat) (ins :: [Nat]) ot 


newtype I s =  I (s -> Dynamic -> IO (s, Dynamic))

connect :: I s0 -> I s1 -> I (s0, s1)
connect (I f0) (I f1) = 
    I $ \(s0, s1) d -> do
        (s0', d') <- f0 s0 d
        (s1', d'') <- f1 s1 d'
        return ((s0', s1'), d'')

connectInit :: Init s0 -> Init s1 -> Init (s0, s1)
connectInit (Init s0) (Init s1) = Init (s0, s1)

newtype Init s = Init s

run :: I s -> Init s -> Dynamic -> IO (s, Dynamic)
run (I fun) (Init s) = fun s 



















