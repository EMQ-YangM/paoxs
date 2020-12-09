{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module New15 where

import Control.Concurrent
import Control.Concurrent.Chan
import Dynamic
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-))
import Data.Kind
import Data.Data (Proxy)
import GHC.Base (Symbol)
import Data.Proxy
import GHC.TypeLits


data N (s :: Symbol) (a :: [(Symbol, Type)])  = N

-- | dag map
-- start node: ("start", '[], '[])
-- test  node: ("test", '[ '("start", @Int@) ])
-- test1 node: ("test1", '[ '("start", @Int@),  '("test", @Int@)])
type SSS = (Symbol, [(Symbol, Type)], [(Symbol, Type)])

data R (a :: [SSS])

--data a :> b

data IN (s :: Symbol) 
        (i :: [(Symbol, Type)]) 
        (o :: [(Symbol, Type)])

type SE = ('[ '("start",'[],'[]) , '("end",'[],'[])])

type V = SE
    :> IN "a" '[ '("start",Int) ] '[]
    :> IN "b" '[ '("a",Int) ] '[]
    :> IN "c" '[ '("b",Int) ] '[]
    :> IN "f" '[ '("a",Int), '("b",Int) ] '[ '("c", Float), '("end", String)  ]

vapi :: Proxy V 
vapi = Proxy

--class HasState s where
--    result :: s -> [String]
--
--instance (HasState xs) => HasState (xs :> (IN s i o)) where
--    result xs  = []
--
--instance HasState (Proxy ( xs :: '[(Symbol, [(Symbol, Type)], [(Symbol, Type)])] ) where
--    result (Proxy :: Proxy ( '(x, a , b) ': xs)) = 
--      let v = Proxy 
--      in []
--

type family (:>) s i where
    xs :> (IN s i o) = 
      If (NotElem s xs) 
        (AddNode s i o ( InsIns s o (InsOuts s i xs))) 
        ( TypeError (Text "节点 " :<>: ShowType s :<>: Text " 出现了两次"))




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

type family NotElem (s :: Symbol) (es :: [SSS] ) where
    NotElem _ '[] = 'EQ
    NotElem s ('(v, _, _) ': xs) =  
     If (CmpSymbol s v) ('LT) (NotElem s xs)

type family If (a :: Ordering) b c  where
    If EQ c _ = c
    If _  _ b = b


