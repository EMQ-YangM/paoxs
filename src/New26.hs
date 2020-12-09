{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module New26 where

import Conduit (MonadIO (liftIO))
import Control.Carrier.Error.Either
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Effect.Lens (use, (.=))
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens hiding (assign, use, view, (.=))
import Data.Kind
import Control.Algebra (send, (:+:)(..))
import Data.Data (Typeable)
import Control.Monad.ST
import Data.STRef
import Data.Traversable (for)
import Data.Typeable (cast)

data Animal = Cat
  { _age :: Int,
    _name :: String,
    _position :: Position
  }
  deriving (Show)

data Position = Position
  { _x :: Double,
    _y :: Double
  }
  deriving (Show)

makeLenses ''Position
makeLenses ''Animal

t = Cat 23 "baba" (Position 1 2)

t1 = t ^. position . x

action1 :: (Has (State String) sig m, Has (Reader Int) sig m) => m ()
action1 = do
  s <- get
  c <- ask @Int
  put ("hello, " ++ s ++ show c)

example1 :: (Algebra sig m, Effect sig) => [a] -> m (Int, ())
example1 list = runState 0 $ do
  i <- get
  put (i + length list)

example2 :: (Algebra sig m, Effect sig) => m (Int, ())
example2 = runReader "hello" . runState 0 $ do
  list <- ask
  put (length (list :: String))

example4 :: IO (Int, ())
example4 = runM . runReader "hello" . runState 0 $ do
  list <- ask
  liftIO (putStrLn list)
  put (length list)

example5 :: IO (Position, (Animal, Either String String))
example5 =
  runM . runState (Position 2 3) . runState (Cat 23 "nice" (Position 1 2)) . runError $ s

stateTest ::
  ( Has (State Animal) sig m,
    Has (Error String) sig m
  ) => m ()
stateTest = do
  val <- use age
  age .= (val + 1)
  position . x .= 10
  position . y .= 10.1

stateTest1 ::
  ( Has (State Position) sig m,
    Has (Error String) sig m
  ) => m String
stateTest1 = do
  x' <- use x
  x .= 1111 + x'
  y .= 3211
  throwError "error"

s :: 
  ( Has (State Animal) sig m,
    Has (State Position) sig m,
    Has (Error String) sig m,
    MonadIO m
  ) => m String
s = do 
    stateTest 
    liftIO $ print "info"
    stateTest1

--data Teletype (m :: Type -> Type) k
--read :: Has Teletype sig m => m String
--write :: Has Teletype sig m => String -> m ()

data Teletype (m :: Type -> Type) k where 
    Read ::            Teletype m String
    Write :: String -> Teletype m ()

read :: Has Teletype sig m => m String
read = send Read

write :: Has Teletype sig m => String -> m ()
write s = send (Write s)

newtype TeletypeIOC m a = TeletypeIOC { runTeletypeIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

--instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
--  alg hdl sig ctx = case sig of
--    L Read      -> (<$ ctx) <$> liftIO getLine
--    L (Write s) -> ctx      <$  liftIO (putStrLn s)
--    R other     -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)

data (f :-: g) e = Inl (f e) | Inr (g e)

--tt :: (Maybe :~: []) Int
--tt =  Inl [1]

data Event = Event

data Response a where
  Accept :: a -> Response a
  Finish :: a -> Response a
  Defer :: Response a

class (Typeable a, Show a) => Responder a where
  respond :: Event -> a -> Response a

instance Responder Int where 
    respond _ i = Accept i

instance Responder Double where 
    respond _ i = Accept i

data SomeResponder = forall a . Responder a => SomeResponder a

newtype Chain = Chain [SomeResponder]

c = Chain [SomeResponder (1 :: Int), SomeResponder (2 :: Double)]

f = propagate Event c

-- Dirt-simple imperative implementation with the ST monad.
-- An implementation with a fold could do this all purely
-- but the accumulator is a little fiddly
propagate :: Event -> Chain -> Chain
propagate evt (Chain c) = runST do
  -- We need a signaling variable in case something in the chain
  -- wants to abort the traversal.
  abort <- newSTRef False
  -- Iterate through the responder chain...
  result <- for c \(SomeResponder item) -> do
    -- attempting to aply the function at each item
    let given = respond evt item
    -- but first checking to see if we've aborted in prior iterations
    done <- readSTRef abort
    -- shortcut for rewrapping and returning a SomeResponder
    let wrap = pure . SomeResponder
    if
      -- A prior Finish result means we no-op
      | done -> wrap item
      -- Return a new value while writing to the signal variable.
      | Finish a <- given -> writeSTRef abort True *> wrap a
      -- Just return the new value.
      | Accept a <- given -> wrap a
      -- No match? Continue onward
      | Defer <- given -> wrap item
  pure (Chain result)

applying :: Responder a => (a -> a) -> Chain -> Chain
applying f (Chain c) = Chain (map go c)
  where
    go (SomeResponder r) = maybe (SomeResponder r) (SomeResponder . f) (cast r)





























