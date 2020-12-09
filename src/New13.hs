{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module New13 where

import Control.Concurrent
import Control.Concurrent.Chan
import Dynamic
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-))
import Data.Kind
import Data.Data (Proxy)
import GHC.Base (Symbol)
import Data.Proxy
import GHC.TypeLits


data V (s :: Symbol) a = V

data Join a = Join a

data Lc :: [(Symbol, Type)] -> Type where
    LNil :: Lc '[]
    (:|:) :: V s a -> Lc xs -> Lc ( '(s, a) ': xs) 

infixr 4 :|:

tin = (V :: V "start" Int) :|: (V :: V "fliter"  String ) :|: LNil

tout = (V :: V "end" Float) :|: (V :: V "test"  Int) :|: LNil


data N (s :: Symbol) (a :: [(Symbol, Type)])  = N

data IN (s :: Symbol) 
        (i :: [(Symbol, Type)]) 
        (o :: [(Symbol, Type)]) = IN

-- | dag map
-- start node: ("start", '[], '[])
-- test  node: ("test", '[ '("start", @Int@) ])
-- test1 node: ("test1", '[ '("start", @Int@),  '("test", @Int@)])
type SSS = (Symbol, [(Symbol, Type)], [(Symbol, Type)])

data State :: [(Symbol, [(Symbol, Type)], [(Symbol, Type)])] -> Type where
    Base :: State '[ '("start"  , '[], '[]) , '("end"  , '[], '[])]
    (:>) ::  forall i s xs. NotElem s xs =>
        State xs -> N s i  -> State ( AddNode s i (InsOuts s i xs)) 
    (:|) ::  forall s i o xs. NotElem s xs =>
        State xs -> IN s i o -> State ( AddNode s i ( InsIns s o (InsOuts s i xs)) )


infixl 4 :>
infixl 4 :|

t1 =  (N :: N "a" '[ '("start", Int)]  )

t2 =  (N :: N "b" '[  '("start", Bool), '("a", String)]  )

t3 =  (N :: N "c" '[ '("b", Int), '("start", Int)  ]  )

t4 =  (N :: N "d" '[ '("c", Int), '("start", Int)  ]  )

t5 =  (N :: N "r" '[ '("b", Int), '("d", Int), '("c", String), '("a", String) ]  )

t3' =  (N :: N "c1" '[ '("b", Int), '("r", Int)  ]  )

t0 = IN :: IN "a" '[ '("start", Int) ] '[ '("end", Bool) ]

t00 = IN :: IN "b" '[ '("start", Int) ] '[ '("end", Bool) ]

tv = Base  :| t0 :| t00


type family AddNode (a :: Symbol) (i :: [(Symbol, Type)]) (ss :: [SSS]) where
    AddNode s i ss = '( s, i, '[] ) ': ss

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
    InsIns sb (x ': xs) ss = InsOuts sb xs (InsOut sb x ss)




type family NotElem (s :: Symbol) (es :: [SSS] ) :: Constraint where
    NotElem _ '[] = ()
    NotElem s ('(v, _, _) ': xs) =  
     If (CmpSymbol s v) ( TypeError (Text "节点 "  :<>: ShowType v :<>: Text " 出现了两次" )) (NotElem s xs)

type family If (a :: Ordering) b c  where
    If EQ c _ = c
    If _  _ b = b

data Node (s :: Symbol) i o
    = Node {
        ninput :: Lc i
      , noutput :: Lc o
      , funct :: Lc i -> Lc o
    } 


