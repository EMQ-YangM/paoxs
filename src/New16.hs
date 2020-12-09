{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module New16 where

import Control.Concurrent
import Control.Concurrent.Chan
import Dynamic
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-))
import Data.Kind
import Data.Data (Proxy)
import GHC.Base (Symbol)
import Data.Proxy
import GHC.TypeLits
import Data.Void (Void)
import Data.Map (Map, empty, insert, toList)
import Data.List (intersperse)
import Shelly

data V (s :: Symbol) a = V

data Join a = Join a

data Lc :: [(Symbol, Type)] -> Type where
    LNil :: Lc '[]
    (:|:) :: V s a -> Lc xs -> Lc ( '(s, a) ': xs) 


infixr 4 :|:

data IN (s :: Symbol) 
        (i :: [(Symbol, Type)]) 
        (o :: [(Symbol, Type)]) = IN

-- | dag map
-- start node: ("start", '[], '[])
-- test  node: ("test", '[ '("start", @Int@) ])
-- test1 node: ("test1", '[ '("start", @Int@),  '("test", @Int@)])
type SSS = (Symbol, [(Symbol, Type)], [(Symbol, Type)])

data State :: [(Symbol, [(Symbol, Type)], [(Symbol, Type)])] -> Type where
    Base :: State '[ '("sink",'[],'[]), '("source",'[],'[])]
    (:>) ::  forall s i o xs. NotElem s xs =>
        State xs -> IN s i o -> State ( AddNode s i o ( InsIns s o (InsOuts s i xs)) )


infixl 4 :>

--t0 = IN ::  IN "a" '[ '("source", Int) ] '[]
--t1 = IN ::  IN "b" '[ '("source", Int) ] '[]
--t2 = IN ::  IN "c" '[ '("a", Int), '("b", Int)] '[ '("sink", (Int,Int,Int))]
--
--tv = Base :> t0  
--          :> t1 
--          :> t2 
--



t0 = IN ::  IN "a" '[ '("source", Int) ] '[]
t1 = IN ::  IN "b" '[] '[]
t2 = IN ::  IN "c" '[ '("b", Int)] '[]
t3 = IN ::  IN "d" '[ '("b", Int), '("c", Int)] '[ '("sink", Bool)]
t4 = IN ::  IN "e" '[ '("a", Int),'("source", Int)] '[]
t5 = IN ::  IN "f" '[ '("a", Int)] '[ '("b", Bool)]
t6 = IN ::  IN "j" '[ '("d", Int)] '[ '("sink", Bool) ]
t7 = IN ::  IN "k" '[ '("e", Int), '("b", Int) ] '[]
t8 = IN ::  IN "l" '[ '("a", Int)] '[ '("b", Int)]
t9 = IN ::  IN "m" '[ '("l", Int), '("c", Int)] '[ '("d", Float),'("sink", Int)]
t10 = IN :: IN "s" '[ '("e", Int)] '[ '("k", Int)]
t11 = IN :: IN "x" '[ '("c", Int), '("b", Float), '("k", Int)] '[ '("j", Int)]

tv = Base :> t0  
          :> t1 
          :> t2 
          :> t3 
          :> t4 
          :> t5 
          :> t6 
          :> t7 
          :> t8 
          :> t9 
          :> t10 
          :> t11
          :> (IN :: IN "test" '[ '("a", Int)] '[ '("b", Int)  ] )
          :> (IN :: IN "test1" '[ '("a", Int)] '[ '("b", Int)  ] )


func :: (Translate (Reverse ss '[]) ~ v) => State ss -> v -> v
func _ y = y

uv :: Node s i o
uv = undefined

source = Node "source" undefined
sink = Node "sink" undefined

tx = func tv (cNode 
         :<|> cNode  
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode
         :<|> cNode  
         :<|> End )

cNode :: forall s i o. (KnownSymbol s, CreateEmptyList i) => Node s i o 
cNode = Node (symbolVal (Proxy :: Proxy s )) (cEmptyList (Proxy :: Proxy i) )
    
--------------------------------------------

data SList (s :: Symbol) a = SList [a]

instance (KnownSymbol s, Show a) => Show (SList s a) where
    show (SList xs) = " (" ++ symbolVal (Proxy :: Proxy s)  ++ " " ++ show xs ++ ")"

data Sval (s :: Symbol) v = Sval v

inSval :: Sval s v -> SList s v -> SList s v
inSval (Sval x) (SList xs) = SList (x : xs)

data CList :: [(Symbol, Type)] -> Type where
    CNil :: CList '[]
    CAdd :: (KnownSymbol s, Show a) => SList s a -> CList xs -> CList ('(s,a) ': xs)

instance Show (CList xs) where
    show CNil= ""
    show (CAdd x ys) = show x ++ show ys

--------------------------------------------

class InCList a s where
    inCList :: a -> s -> s

instance {-# OVERLAPPABLE #-}
   forall s b v xs. ( InCList (Sval s v) (CList xs), InList s xs ~ ())
    => InCList (Sval s v) (CList ( '(b, v) ': xs )) where 
    inCList x (CAdd y ys) = CAdd y $ inCList x ys

instance {-# OVERLAPPABLE #-}
    forall s v xs. InCList (Sval s v) (CList ( '(s, v) ': xs )) where 
    inCList x (CAdd y ys) = CAdd (inSval x y) ys

instance InCList (Sval s v) (CList '[]) where
    inCList _ CNil = error "strange happened"

--------------------------------------------

class CreateEmptyList s where
    cEmptyList :: Proxy s -> CList s

instance (CreateEmptyList xs, KnownSymbol s, Show a) 
   => CreateEmptyList ( '(s, a) ': xs ) where
    cEmptyList _ = CAdd (SList [] :: SList s a) (cEmptyList (Proxy :: Proxy xs))

instance CreateEmptyList '[] where
    cEmptyList _ = CNil

tf :: CreateEmptyList s => CList s
tf  = cEmptyList (Proxy :: Proxy s)

tt :: CList '[ '("a", Int) ,  '("b", Int) ]
tt = tf 

tt1 = inCList (Sval 1 :: Sval "a" Int) tt

-------------
-- a  b
--
--  c  
-- a  b
-- -  -
-- -  -
-- -  -
-- -  -
--
--

class Create s where
   create :: s -> IO () 

--instance Create a where
--   create a = undefined


class HasState s where
    has :: Proxy s -> Map String [String]

-- >>> cc
cc = cr tv

runCmd :: IO ()
runCmd = shelly $ do
    run_ "dot" ["-Tpng", "-o", "wf.png", "wf.dot" ]
    run_ "open" ["wf.png"]

cr ::(HasState s) => State s -> IO ()
cr s = do 
    let r = fun1 Proxy s
    createDot r 
    runCmd

createDot :: Map String [String] -> IO ()
createDot m = writeFile "wf.dot" res 
    where ls = Data.Map.toList m
          loop [] = ""
          loop ((name, outList) : xs) = name ++ 
                        " -> {" ++ (concat $ intersperse "," outList) ++ "}\n"  ++ loop xs
          res = "digraph test{\n" ++ loop ls ++ "\n}"

fun1 :: (HasState s) => Proxy s -> State s -> Map String [String]
fun1 p _  = has p

instance (H1 x, HasState xs) =>  HasState (x ': xs) where
    has _ = insert k a $ has (Proxy :: Proxy xs)
        where (k,a) = h1 (Proxy :: Proxy x)

instance HasState '[] where
    has _ = empty

class H1 s where
    h1 :: Proxy s -> (String, [String])

instance (KnownSymbol s, H2 ys) => H1 ('(s, xs, ys)) where
    h1 _ =  (symbolVal (Proxy :: Proxy s), h2 (Proxy :: Proxy ys))

class H2 s where
    h2 :: Proxy s -> [String]

instance (KnownSymbol s , H2 xs) => H2 ( '(s, v) ': xs) where
    h2 _ = symbolVal (Proxy :: Proxy s) : (h2 (Proxy :: Proxy xs))

instance H2 '[] where
    h2 _ = []

data a :<|> b = a :<|> b

instance (Show a, Show b) => Show (a :<|> b) where
    show (a :<|> b) = show a  ++ "\n" ++ show b

infixr 4 :<|>

data Node (s :: Symbol) 
          (i :: [(Symbol, Type)]) 
          (o :: [(Symbol, Type)])
    = Node String (CList i) 

instance Show (Node s i o) where
    show (Node s cl) = "Node " ++ s ++ show cl

data End = End

instance Show End where 
    show End = ""


type family Translate xs where
    Translate '[] = End
    Translate ('(v, xs, ys) ': zs) 
     = Node v xs ys :<|> Translate zs

type family Reverse xs ys where
    Reverse '[] y = y
    Reverse (x ': xs) y = Reverse xs (x ': y)

type family AddNode (a :: Symbol) (i :: [(Symbol, Type)]) (o :: [(Symbol, Type)]) (ss :: [SSS]) where
    AddNode s i o ss = '( s, i, o ) ': ss

type family InsOut (s :: Symbol) (i :: (Symbol, Type)) (ss :: [SSS]) where
    InsOut sb '(s, t) ('(v, xs, ys) ': zs) = 
        If (CmpSymbol s v) ( '(v, xs, '(sb, t) ': ys ) ': zs  ) ( '(v, xs, ys)  ': InsOut sb '(s, t) zs)
    InsOut a b '[] =  TypeError ( Text "node " :<>: ShowType a :<>: Text " inputs: " :<>: ShowType b :<>: Text " not found")

type family InsOuts (s :: Symbol) (is :: [(Symbol, Type)]) (ss :: [SSS]) where
    InsOuts _ '[] ss = ss
    InsOuts sb (x ': xs) ss = InsOuts sb xs (InsOut sb x ss)

type family InsIn (s :: Symbol) (i :: (Symbol, Type)) (ss :: [SSS]) where
    InsIn sb '(s, t) ( '(v, xs, ys) ': zs ) = 
        If (CmpSymbol s v) ( '(v, '(sb, t) ': xs, ys) ': zs ) ( '(v, xs, ys) ': InsIn sb '(s, t) zs )
    InsIn a b '[] =  TypeError ( Text "node " :<>: ShowType a :<>: Text " outputs: " :<>: ShowType b :<>: Text " not found")

type family InsIns (s :: Symbol) (is :: [((Symbol), Type)]) (ss :: [SSS]) where 
    InsIns _ '[] ss = ss
    InsIns sb (x ': xs) ss = InsIns sb xs (InsIn sb x ss)

type family NotElem (s :: Symbol) (es :: [SSS] ) :: Constraint where
    NotElem _ '[] = ()
    NotElem s ('(v, _, _) ': xs) =  
     If (CmpSymbol s v) ( TypeError (Text "节点 "  :<>: ShowType v :<>: Text " 出现了两次" )) (NotElem s xs)

type family InList (s :: Symbol) (es :: [(Symbol, Type)]) where
    InList s '[] = TypeError (Text "there is not symbol " :<>: ShowType s :<>: Text " in CList")
    InList s ('(v, _) ': xs) = If (CmpSymbol s v) () (InList s xs)

type family If (a :: Ordering) b c  where
    If EQ b _ = b
    If _  _ c = c

type family Ifa (a :: Ordering) b c  where
    Ifa EQ b _ = b
    Ifa _  _ c = c
