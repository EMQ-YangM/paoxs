{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module New11 where

import GHC.TypeLits
import Data.Text(Text)
import qualified Data.ByteString as Text
import qualified Data.Char as Text

-- data Bar (n :: Nat) = Bar String deriving (Show)

-- bar :: (KnownNat n) => Bar n -> (String, Integer)
-- bar b@(Bar s) = (s, natVal b)

-- main :: IO ()
-- main = do
--   i <- readLn
--   let Just someNat = someNatVal i
--   case someNat of
--     SomeNat (_ :: Proxy n) -> do
--       let a :: Bar n
--           a = Bar "as"
--       print $ bar a


data Bar (n :: Nat) = Bar String deriving Show

bar :: (KnownNat n) => Bar n -> (String, Integer)
bar b@(Bar s) = (s, natVal b)

-- main :: IO ()
-- main = do
--     i <- readLn
--     let Just someNat = someNatVal i
--     case someNat of
--         SomeNat (_ :: Proxy n) -> do
--             let a :: Bar (n + 5)
--                 a = Bar "as"
--             print $ bar a



data Value :: * -> * where
  VStr :: Text -> Value Text
  VInt :: Int  -> Value Int

  
data Op :: * -> * where
  OpEcho    :: Op a
  OpReverse :: Op Text
  OpCaps    :: Op Text
  OpInc     :: Op Int
  OpNeg     :: Op Int

execOp :: Value a -> Op a -> Value a
execOp val        OpEcho    = val
execOp (VStr str) OpReverse = VStr $ undefined
execOp (VStr str) OpCaps    = VStr $ undefined
execOp (VInt i)   OpInc     = VInt $ i + 1
execOp (VInt i)   OpNeg     = VInt $ negate i






data Get (a :: *)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)













