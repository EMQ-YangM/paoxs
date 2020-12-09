{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module New24 where


import Control.Effect.State
import Control.Effect.Reader
import Control.Carrier.State.Strict
import Control.Carrier.Reader (runReader)
import Control.Lens (makeLenses)
import Control.Effect.Lens (use, (.=))

--action1 :: Has (State String) sig m => m ()
--action1 = get >>= \s -> put ("hello, " ++ s )
--
--
--action2 :: (Has (State String) sig m, Has (Reader Int) sig m) => m ()
--action2 = do 
--    i <- ask 
--    put (replicate i '!')
--
--e1 :: (Algebra sig m, Effect sig) => [a] -> m (Int, ())
--e1 list =  runState 0 $ do  
--    i <- get
--    put (i + length list)
--
--e2 :: (Algebra sig m, Effect sig) => m (Int, ())
--e2 = runReader "hello" . runState 0 $ do    
--    list <- ask 
--    put (length (list :: String))
--
--e3 :: (Int, ()) 
--e3 = run . runReader "hello" . runState 0 $ do 
--    list <- ask 
--    put (length (list :: String))
--
--
--
--
--data Contex = Contex
--  { _amount :: Int
--  , _disabled :: Bool
--  } deriving (Eq, Show)
--
--makeLenses ''Contex
--
--
--
--stateTest :: Has (State Contex) sig m => m Int
--stateTest = do
--  initial <- use amount
--  amount .= (initial + 1)
--  disabled .= True
--  use amount
--
