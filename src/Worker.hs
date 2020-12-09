{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Worker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import GHC.Base (Type)
import GHC.TypeLits

-- connect :: Worker -> Worker -> Worker

--                3 4     4 3        3 3
-- mnConnect :: worker -> worker -> worker

--             a
--             b      d
--             c      f

data Config = Config

data State1

data P (s :: Symbol) (ts :: [Type]) where
  HNil :: P s '[]
  (:#) :: t -> P s ts -> P s (t ': ts)

infixr 5 :#

type Pin = P "in"

type Pout = P "out"

data Worker i o r s m = Worker (Pin i -> MaybeT (ReaderT r (StateT s m)) (Pin o))

-- Worker (Pin i) (Pout o) r s m a

sum0 :: Worker '[Int] '[Int] Config Int IO
sum0 = Worker $ \(x :# HNil) -> do
  y <- lift get
  let v = x + y
  put v
  liftIO $ print v
  return (v :# HNil)

add1 :: Worker '[Int] '[Int] Config Int IO
add1 = Worker $ \(x :# HNil) -> do
  return (x + 1 :# HNil)

filter1 :: Monad m => (a -> Bool) -> Worker '[a] '[a] r s m
filter1 f = Worker $ \(x :# HNil) ->
  if f x
    then return (x :# HNil)
    else mzero

sum2 :: Worker '[Int, Int] '[Int] Config Int IO
sum2 = Worker $ \(x :# y :# HNil) -> do
  let v = x + y
  return (v :# HNil)

infixl 1 .|

(.|) ::
  Monad m =>
  Worker i o r s m ->
  Worker o o' r s m ->
  Worker i o' r s m
(.|) (Worker f1) (Worker f2) =
  Worker $ \x -> do
    a <- f1 x
    f2 a

f2to1 ::
  Monad m =>
  Worker '[a] '[b] r s m ->
  Worker '[c] '[d] r s m ->
  Worker '[b, d] '[e] r s m ->
  Worker '[a, c] '[e] r s m
f2to1 (Worker w1) (Worker w2) (Worker w3) =
  Worker $ \(x :# y :# HNil) -> do
    (x1 :# HNil) <- w1 (x :# HNil)
    (y1 :# HNil) <- w2 (y :# HNil)
    w3 (x1 :# y1 :# HNil)

runWorker :: r -> s -> (Pin i) -> Worker i o r s IO -> IO (Maybe (Pin o), s)
runWorker r s pin (Worker w) = runStateT (runReaderT (runMaybeT $ w pin) r) s

t :: Pin i -> Worker i o Config Int IO -> IO (Maybe (Pin o), Int)
t p s = runWorker Config 0 p s

--- Rosetree = all state
--- f1 :: State -> in -> (State, out)
--- f2 :: S1 -> Int -> (S1, Int)
--- f3 :: S2 -> Bool -> (S2, Int)

data Rosetree a = Rosetree a [Rosetree a]

data K (ts :: Rosetree Type) where
  Co :: K x -> K ( 'Rosetree a xs) -> K ( 'Rosetree a (x ': xs))

-- v1 :: K ('Rosetree Int '[])
-- v1 = RL 1
--  level
-- Level Node State Fun (Pin Node) (Pin Node)

-- (a, b) (1,1) (1, 2) (1, 3) (1, 4)

-- (1, 1, Node s f) (1, 2, Node s f )
-- (2, 1, Node s f) (2, 2, Node s f) (2, 3, Node s f)
-- (3, 1, Node s f) (3, 2, Node s f)

-- [[s f, s f], [s f, s f], [s, f, s f]]

--  [[a,b,c],[d,e,f],[h,i,j]]
-- data Ababa

data NL (ts :: [[Type]]) where
  NLL :: NL '[ '[]]
  AddL :: t -> NL (t1 ': ts) -> NL ((t ': t1) ': ts)
  AddN :: NL (xs ': '[]) -> NL ys -> NL (xs ': ys)

-- data KS (ts :: NL) (fs :: [[Type]]) where
--   KLL :: KS '[ '[]]

tt ::
  NL
    '[ '[Int, Int],
       '[String, Int]
     ]
tt = undefined

tt1 ::
  NL
    '[ '[(Pin '[Int, Int], Pin '[Int, Float])],
       '[Pin '[String, Float]]
     ]
tt1 = undefined

-- l0 l1 a b c
-- i     . . .
-- j       .
-- o     .   .

-- l1  l2 f g h
--  a     . . .
--  b     .   .
--  c     . .

-- l2  l3 l m n
-- f      . .
-- g      .   .
-- h      .

-- l3  l4 v s x
-- l      . .
-- m        . .
-- n      .   .

-- [ [ [ ] ]]
-- [ [
--   [a , b, c]
--   [e , f, g]
--   [l , s, m]
-- ]
--
--  ]

kk :: [[Int]]
kk =
  [ [1, 2, 0],
    [3, 4, 5],
    [3, 2, 5]
  ]

kk1 :: [[Int]]
kk1 =
  [ [1, 2, 0],
    [3, 4, 5],
    [3, 2, 5]
  ]

kk2 =
  -- m * n
  [ (100, (1, 3, 3), (0, 2, 0)), --f
    (100, (2, 4, 2), (3, 0, 5)), --g
    (100, (0, 5, 5), (0, 2, 0)) --h
  ]

kk3 =
  -- n * l
  undefined

-- data T3 a b c = T3 (a -> b -> c)

-- data L (ts :: [Type]) (n :: Nat) where
--   Nil :: L '[] 0
--   Con :: a -> (L ts n) -> (L (a ': ts) (n + 1))

-- data F a m n (ts :: (L '[Type] m, L '[Type] n)) where
--   F :: (a -> L xs m -> L ys n) -> F a m n ts

-- data M m n (ts :: [(Type, L '[Type] m, L '[Type] n)]) where
--   Mn :: M m n '[]
--   Cons :: F a m n t0 -> M m n ts -> M m n (t0 ': ts)

-- sum111 :: Int -> L '[Int, Int, String] 3 -> L '[Int, Int, String] 3
-- sum111 = \x (Con w (Con y (Con z Nil))) -> Con x $ Con (x + y + w) $ Con z Nil

-- va = F sum111

-- fs :: M 3 3 '[t1, t2, t3]
-- fs = Cons va $ Cons va $  Cons va $ Mn

---------------------------------------------------------------

-- data L (ts :: [Type]) (n :: Nat) where
--   Nil :: L '[] 0
--   Con :: a -> (L ts n) -> (L (a ': ts) (n + 1))

-- data F m n (ts :: (Type, L '[Type] m, L '[Type] n)) where
--   F :: (a -> L xs m -> L ys n) -> F m n ts

-- data M m n l (ts :: L (Type, L '[Type] m, L '[Type] n) l)
--   Mn :: M m n 0 '[]

-- Cons :: F m n t0 -> M m n ts -> M m n (t0 ': ts)

-- sum21 :: Int -> L '[Int, Int, String] 3 -> L '[Int, Int, String] 3
-- sum21 = \x (Con w (Con y (Con z Nil))) -> Con x $ Con (x + y + w) $ Con z Nil

-- sum31 :: String -> L '[Int, Int, String] 3 -> L '[Int, Int, String] 3
-- sum31 = \x (Con w (Con y (Con z Nil))) -> Con w $ Con (y + w) $ Con z Nil

-- va :: F 3 3 ts
-- va = F sum21

-- vb :: F 3 3 ts
-- vb = F sum31

-- fs :: M 3 3 '[t1, t2, t3, t4]
-- fs = Cons va $ Cons vb $ Cons va $ Cons vb $ Mn

-- fs1 :: M 3 3 '[t1, t2, t3]
-- fs1 = Cons vb $ Cons va $ Cons vb $ Mn

-- conM :: M m n ts -> M n l ks -> M m l gs
-- conM = undefined

---------------------------------------------------------------
---------------------------------------------------------------
data L (ts :: [Type]) (n :: Nat) where
  Nil :: L '[] 0
  Con :: a -> (L ts n) -> (L (a ': ts) (n + 1))

data F m n (ts :: (Type, L '[Type] m, L '[Type] n)) where
  F :: (a -> L xs m -> L ys n) -> F m n ts

data M m n (ts :: [(Type, L '[Type] m, L '[Type] n)]) where
  Mn :: M m n '[]
  Cons :: F m n t0 -> M m n ts -> M m n (t0 ': ts)

sum21 :: Int -> L '[Int, Int, String] 3 -> L '[Int, Int, String] 3
sum21 = \x (Con w (Con y (Con z Nil))) -> Con x $ Con (x + y + w) $ Con z Nil

sum31 :: String -> L '[Int, Int, String] 3 -> L '[Int, Int, String] 3
sum31 = \x (Con w (Con y (Con z Nil))) -> Con w $ Con (y + w) $ Con z Nil

va :: F 3 3 ts
va = F sum21

vb :: F 3 3 ts
vb = F sum31

fs :: M 3 3 '[t1, t2, t3, t4]
fs = Cons va $ Cons vb $ Cons va $ Cons vb $ Mn

fs1 :: M 3 3 '[t1, t2, t3]
fs1 = Cons vb $ Cons va $ Cons vb $ Mn

conM :: M m n ts -> M n l ks -> M m l gs
conM = undefined

---------------------------------------------------------------
---------------------------------------------------------------

-- fs = Cons sum11 $ Mn

-- Cons :: (t :: (Type, L '[Type] m, L '[Type] n)) -> M m n ts -> M m n (t ': ts)

-- data F m n (ts :: T3 Type (L '[Type] m) (L '[Type] n)) where
--   F ::
--     (ts ::  'T3 Type (L '[Type] m) (L '[Type] n ))  ->
--     F m n ts

-- data M m n (ts :: [(Type, L '[Type] m, L '[Type] n)]) where
--   Mn :: M m n '[]
--   Cons :: F m n t0 -> M m n ts -> M m n (t0 ': ts)

-- data L (ts :: [Type]) (n :: Nat) where
--   Nil :: L '[] 0
--   Con :: a -> (L ts n) -> (L (a ': ts) (n + 1))

--  ->
-- (M m n (t ': ts))

-- data M m n (ts :: [(Type, L '[Type] m, L '[Type] n)]) where
--   Mn :: M m n '[]
--   Cons ::
--     (t :: (Type, L '[Type] m, L '[Type] n)) ->
--     (M m n (ts :: [(Type, L '[Type] m, L '[Type] n)])) ->
--     (M m n (t ': ts))

-- (M m n (g ': gs))

-- data M (ts :: [(Type, L Type Nat , L Type Nat)]) where
--   Mn :: M '[]

--    3   2         3   2           3   2
-- s Pin Pout : s1 Pin Pout : s2 : Pin Pout : []

--    2   2         2   2
-- v Pin Pout : v1 Pin Pout : []

kk4 :: NL '[ '[Maybe Int]]
kk4 = undefined

-- coll :: [[Int]] -> [[Int]] -> [[]]

t1 :: NL '[ '[String, Float, Int], '[Int, Int], '[Int, String]]
t1 =
  AddN (AddL "ncie" $ AddL 1 $ AddL 1 $ NLL) $
    AddN (AddL 1 $ AddL 1 NLL) $
      AddL 1 $ (AddL "nice") NLL

at :: (Int, Int) -> [[a]] -> a
at (i, j) xs = (xs !! i) !! j

type W = '[ '[String, Float, Int], '[Int, Int], '[Int, String]]

-- type W12 = 'at (1, 2) W

t2 ::
  NL '[ '[String, Float, Int], '[Int, Int], '[Int, String]] ->
  NL '[ '[String, Float, Int], '[Int, Int], '[Int, String]]
t2 = undefined

data LL a n where
  Nl :: LL a 0
  Lc :: a -> (LL a n) -> LL a (n + 1)

data Lse a m n = Lse (LL a m) (LL a n)

tll :: LL Int 2
tll = Lc 1 (Lc 2 Nl)

tls :: Lse Int 2 2
tls = Lse tll tll

cococ :: Lse a m n -> Lse a n l -> Lse a m l
cococ = undefined
